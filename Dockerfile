FROM ubuntu:24.04 as builder

ENV DEBIAN_FRONTEND=noninteractive


RUN apt update && apt install -y tzdata wget git unixodbc-dev libgtk2.0-dev xvfb sqlite3 libsqlite3-dev build-essential curl binutils

# Download and build OpenSSL 1.1.1w
WORKDIR /tmp
RUN wget https://www.openssl.org/source/openssl-1.1.1w.tar.gz \
    && tar -xf openssl-1.1.1w.tar.gz \
    && cd openssl-1.1.1w \
    && ./config \
    && make \
    && make test \
    && make install

# RUN ls -la /usr/local/lib/

# Set the timezone
RUN echo "UTC" > /etc/timezone

RUN cd /tmp && \
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

RUN /work/bootstrap/linux-libraries.sh /work/bootstrap && \
    cp /usr/local/lib/*.so* /usr/lib/ && \
    /work/fhirserver/build/linux-fhirserver.sh /work/bootstrap && \
    cp exec/pack/*.properties exec/64
# RUN cp exec/install/* exec/64

RUN mkdir -p /work/fhirserver/exec/install/bin && \
    mkdir -p /work/fhirserver/exec/install/x86_64 && \
    mkdir -p /work/fhirserver/exec/install/content && \
    mkdir -p /work/fhirserver/exec/install/config && \
    mkdir -p /work/fhirserver/exec/install/config/config && \
    mkdir -p /work/fhirserver/exec/install/config/default_config && \
    mkdir -p /work/fhirserver/exec/install/web

RUN cd /work/fhirserver && \
    cp /work/fhirserver/exec/64/fhirserver /work/fhirserver/exec/install/bin && \
    cp /work/fhirserver/exec/64/FHIRToolkit /work/fhirserver/exec/install/bin && \
    cp /work/fhirserver/exec/64/FHIRConsole /work/fhirserver/exec/install/bin && \
    cp /work/fhirserver/exec/pack/linux/*so* /work/fhirserver/exec/install/x86_64

RUN cp /work/fhirserver/exec/pack/linux/start_bare.sh /work/fhirserver/exec/install/bin/start.sh && \
    cp /work/fhirserver/exec/pack/linux/install.sh /work/fhirserver/exec/install && \
    cp /work/fhirserver/exec/pack/linux/get-openssl.sh /work/fhirserver/exec/install && \
    cp /tmp/openssl-1.1.1w/*.so* /work/fhirserver/exec/install/x86_64 && \
    cp /work/fhirserver/exec/pack/*.properties /work/fhirserver/exec/install/content && \
    cp /work/fhirserver/exec/pack/*.dat /work/fhirserver/exec/install/content && \
    cp /work/fhirserver/exec/pack/fhirserver.cfg /work/fhirserver/exec/install/config && \
    cp /work/fhirserver/exec/pack/web.ini /work/fhirserver/exec/install/config && \
    cp /work/fhirserver/config/config.ini       /work/fhirserver/exec/install/config/config && \
    cp /work/fhirserver/config/config_bare.json /work/fhirserver/exec/install/config/config/config.json && \
    cp /work/fhirserver/config/config.ini       /work/fhirserver/exec/install/config/default_config && \
    cp /work/fhirserver/config/config_bare.json /work/fhirserver/exec/install/config/default_config/config.json && \
    mkdir -p /work/fhirserver/exec/install/web && \
    cp -r /work/fhirserver/server/web/* /work/fhirserver/exec/install/web && \
    cd /work/fhirserver/exec && tar -czvf ./install.tgz ./install/  && ls -la /work/fhirserver/exec


# Set the health check
HEALTHCHECK --interval=1m --timeout=10s --retries=5 \ 
  CMD curl -f http://localhost:${PORT}/fhir/metadata || exit 1

# Set the environment variables
ENV DISPLAY :99
ENV PORT 80
ENV TERMINOLOGY_CACHE /terminology
VOLUME /terminology

ENV DEBIAN_FRONTEND=

# ENTRYPOINT ["/bin/entrypoint.sh"]

# CMD ["-cmd",  "exec",  "-cfg", "/config/config.ini", "-local", "$TERMINOLOGY_CACHE"]



# Runtime stage
FROM ubuntu:24.04 as runtime

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC
# Set up environment variables
# ENV HOME=~/
ENV DISPLAY=:99
ENV PORT=80
ENV TERMINOLOGY_CACHE=/var/cache/txcache

# Install runtime dependencies
RUN apt-get update && apt-get install -y wget tzdata xvfb libgtk2.0-0 libsqlite3-dev curl \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p $HOME/fhirserver/config $TERMINOLOGY_CACHE /fhirserver \
    && chmod -R 777 $TERMINOLOGY_CACHE \
    && chmod -R 777 /fhirserver

# Copy necessary files from the builder stage
COPY --from=builder /work/fhirserver/exec/install.tgz /fhirserver/install.tgz

# RUN cd /fhirserver \
#     && tar -xzvf install.tgz \
#     && cd ./install \
#     && ./install.sh > install.log 2>&1

# Assuming /fhirserver is your working directory
WORKDIR /fhirserver

# Extract the contents of the tar file
RUN tar -xzvf install.tgz

# Change working directory to the extracted folder
WORKDIR /fhirserver/install

# Run the installation script
RUN ./install.sh -nodaemon 
# -zero=https://storage.googleapis.com/tx-fhir-org/config.json

# Define entrypoint and command
CMD ["bash", "-c", "cd ~/fhirserver/ && ./start.sh"]

# Expose the necessary port
EXPOSE 80
