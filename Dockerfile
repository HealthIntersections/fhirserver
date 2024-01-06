FROM ubuntu:22.04 as builder

ENV DEBIAN_FRONTEND=noninteractive


RUN apt update && apt install -y tzdata wget git unixodbc-dev libgtk2.0-dev xvfb sqlite3 libsqlite3-dev build-essential

# Download and build OpenSSL 1.1.1w
WORKDIR /tmp
RUN wget https://www.openssl.org/source/openssl-1.1.1w.tar.gz \
    && tar -xf openssl-1.1.1w.tar.gz \
    && cd openssl-1.1.1w \
    && ./config \
    && make \
    && make test \
    && make install

RUN ls -la /usr/local/lib/

# Set the timezone
RUN echo "UTC" > /etc/timezone

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

COPY build/linux-toolchain.sh build/linux-libraries.sh /work/bootstrap/
RUN /work/bootstrap/linux-toolchain.sh /work/bootstrap

WORKDIR /work/fhirserver
COPY . /work/fhirserver

RUN /work/bootstrap/linux-libraries.sh /work/bootstrap
RUN cp /usr/local/lib/*.so* /usr/lib/
RUN /work/fhirserver/build/linux-fhirserver.sh /work/bootstrap
RUN cp exec/pack/*.properties exec/64

# Install curl for the health check
RUN apt-get update && apt-get install -y curl

# Set the health check
HEALTHCHECK --interval=1m --timeout=10s --retries=5 \
  CMD curl -f http://localhost:${PORT}/fhir/metadata || exit 1

# Set the environment variables
ENV DISPLAY :99
ENV PORT 80
ENV TERMINOLOGY_CACHE /terminology
VOLUME /terminology

ENV DEBIAN_FRONTEND=

RUN printf '#!/bin/bash \n\
Xvfb  :99 -screen 0 1024x768x8 -nolisten tcp & \n\
echo "[web]" > /work/fhirserver/exec/64/web.ini; \n\
echo "http=${PORT}" >> /work/fhirserver/exec/64/web.ini; \n\
/work/fhirserver/exec/64/fhirserver $(eval echo "$@")'> /bin/entrypoint.sh && \
chmod +x /bin/entrypoint.sh

ENTRYPOINT ["/bin/entrypoint.sh"]

CMD ["-cmd",  "exec",  "-cfg", "/config/config.ini", "-local", "$TERMINOLOGY_CACHE"]
