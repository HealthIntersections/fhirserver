FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y wget git unixodbc-dev libgtk2.0-dev xvfb sqlite3

COPY build/linux-dependencies.sh /work/bootstrap/linux-dependencies.sh

RUN /work/bootstrap/linux-dependencies.sh /work/bootstrap

WORKDIR /work/fhirserver
COPY . /work/fhirserver
RUN ls -l /work
RUN /work/fhirserver/build/linux-fhirserver.sh

ENV DISPLAY :99
ENV PORT 80

RUN printf '#!/bin/bash \n\
Xvfb  :99 -screen 0 1024x768x8 -nolisten tcp & \n\
echo "[web]" > /work/fhirserver/exec/64/web.ini; \n\
echo "http=${PORT}" >> /work/fhirserver/exec/64/web.ini; \n\
/work/fhirserver/server/C\:/work/fhirserver/Exec/64/fhirserver "$@"'> /bin/entrypoint.sh && \
chmod +x /bin/entrypoint.sh

ENTRYPOINT ["/bin/entrypoint.sh"]

CMD ["-cmd",  "exec",  "-cfg", "http://tx.fhir.org/config", "-version",  "4"]
