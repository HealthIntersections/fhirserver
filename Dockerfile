FROM ubuntu:20.04 as builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y wget git unixodbc-dev libgtk2.0-dev xvfb sqlite3 libsqlite3-dev && \
    cd /tmp && \
    wget https://dev.mysql.com/get/Downloads/Connector-ODBC/8.0/mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit.tar.gz && \
    tar -xzvf mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit.tar.gz && \
    cp -r mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit/lib/* /usr/local/lib && \
    cp -r mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit/bin/* /usr/local/bin && \
    rm -rf mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit  && \
    rm -rf mysql-connector-odbc-8.0.26-linux-glibc2.12-x86-64bit.tar.gz && \
    myodbc-installer -a -d -n "MySQL ODBC 8.0 Driver" -t "Driver=/usr/local/lib/libmyodbc8w.so" && \
    myodbc-installer -a -d -n "MySQL ODBC 8.0" -t "Driver=/usr/local/lib/libmyodbc8a.so"

COPY build/linux-toolchain.sh build/unix-libraries.sh /work/bootstrap/
RUN /work/bootstrap/linux-toolchain.sh /work/bootstrap

WORKDIR /work/fhirserver
COPY . /work/fhirserver

RUN /work/bootstrap/unix-libraries.sh /work/bootstrap
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
