FROM ubuntu:20.04 as builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y wget git unixodbc-dev libgtk2.0-dev xvfb sqlite3

COPY build/linux-toolchain.sh build/linux-libraries.sh /work/bootstrap/

RUN /work/bootstrap/linux-toolchain.sh /work/bootstrap
RUN /work/bootstrap/linux-libraries.sh /work/bootstrap

RUN wget https://dev.mysql.com/get/Downloads/Connector-ODBC/8.0/mysql-connector-odbc_8.0.26-1ubuntu20.04_amd64.deb && \
    dpkg -i mysql-connector-odbc_8.0.26–1ubuntu20.04_amd64.deb && \
    rm -f mysql-connector-odbc_8.0.26–1ubuntu20.04_amd64.deb

WORKDIR /work/fhirserver
COPY . /work/fhirserver

RUN cp exec/pack/linux/*.so /usr/lib/
RUN /work/fhirserver/build/linux-fhirserver.sh /work/bootstrap

ENV DISPLAY :99
ENV PORT 80
ENV TERMINOLOGY_CACHE /terminology
VOLUME /terminology

RUN printf '#!/bin/bash \n\
Xvfb  :99 -screen 0 1024x768x8 -nolisten tcp & \n\
echo "[web]" > /work/fhirserver/exec/64/web.ini; \n\
echo "http=${PORT}" >> /work/fhirserver/exec/64/web.ini; \n\
/work/fhirserver/exec/64/fhirserver $(eval echo "$@")'> /bin/entrypoint.sh && \
chmod +x /bin/entrypoint.sh

ENTRYPOINT ["/bin/entrypoint.sh"]

CMD ["-cmd",  "exec",  "-cfg", "http://tx.fhir.org/config", "-version",  "4", "-local", "$TERMINOLOGY_CACHE"]
