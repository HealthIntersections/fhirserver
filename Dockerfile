FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y wget git unixodbc-dev libgtk2.0-dev xvfb sqlite3

RUN wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.2RC1/fpc-src_3.2.2-210709_amd64.deb/download && \
    wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.2RC1/fpc-laz_3.2.2-210709_amd64.deb/download && \
    wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.2RC1/lazarus-project_2.2.0RC1-0_amd64.deb/download

RUN dpkg -i download && dpkg -i download.1 && dpkg -i download.2


RUN mkdir work && cd work && \
    git clone https://github.com/dezlov/PascalTZ && \
    git clone https://github.com/grahamegrieve/delphi-markdown && \
    git clone https://github.com/mriscoc/extrasyn && \
    git clone https://github.com/grahamegrieve/HtmlViewer && \
    git clone https://github.com/grahamegrieve/lazarus-ide-tester && \
    git clone https://github.com/Xor-el/QRCodeGenLib4Pascal

RUN lazbuild work/PascalTZ/package/pascaltz.lpk && \
    lazbuild work/extrasyn/extrahighlighters.lpk && \
    lazbuild work/extrasyn/extrahighlighters_dsgn.lpk && \
    lazbuild work/lazarus-ide-tester/package/idetester.lpk && \
    lazbuild work/lazarus-ide-tester/ide/idetester_dsgn.lpk && \
    lazbuild work/HtmlViewer/package/FrameViewer09.lpk && \
    lazbuild work/delphi-markdown/packages/markdownengine.lpk && \
    lazbuild work/QRCodeGenLib4Pascal/QRCodeGenLib/src/Packages/FPC/QRCodeGenLib4PascalPackage.lpk

COPY . /work/fhirserver

RUN lazbuild work/fhirserver/packages/fhir.lpk && \
    lazbuild work/fhirserver/packages/fhir2.lpk && \
    lazbuild work/fhirserver/packages/fhir3.lpk && \
    lazbuild work/fhirserver/packages/fhir4.lpk && \
    lazbuild work/fhirserver/packages/fhir5.lpk && \
    lazbuild work/fhirserver/packages/fhir_xver.lpk && \
    lazbuild work/fhirserver/packages/fhir_fsl.lpk && \
    lazbuild work/fhirserver/packages/fhir_fui.lpk && \
    lazbuild work/delphi-markdown/tests/markdowntests.lpk

RUN lazbuild work/fhirserver/server/fhirserver.lpi --build-mode=Linux

RUN find /work/fhirserver/exec/pack \
      -type f \( -iname *.dat -o -iname *.cfg \) \
      -exec cp {} /work/fhirserver/exec/64/ \; && \
    find /work/fhirserver/exec/pack \
      -type f -iname *.so  \
      -exec cp {} /usr/lib \;

ENV DISPLAY :99
ENV PORT 80

RUN printf '#!/bin/bash \n\
Xvfb  :99 -screen 0 1024x768x8 -nolisten tcp & \n\
echo "[web]" > /work/fhirserver/exec/64/web.ini; \n\
echo "http=${PORT}" >> /work/fhirserver/exec/64/web.ini; \n\
/work/fhirserver/exec/64/fhirserver "$@"'> /bin/entrypoint.sh && \
chmod +x /bin/entrypoint.sh

ENTRYPOINT ["/bin/entrypoint.sh"]

CMD ["-cmd",  "exec",  "-cfg", "http://tx.fhir.org/config", "-version",  "4"]
