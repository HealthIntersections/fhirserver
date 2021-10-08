FROM ubuntu:20.04 as builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y wget git unixodbc-dev libgtk2.0-dev xvfb sqlite3

COPY build/linux-toolchain.sh build/linux-update-dependencies.sh /work/bootstrap/

RUN /work/bootstrap/linux-toolchain.sh /work/bootstrap
RUN /work/bootstrap/linux-update-dependencies.sh /work/bootstrap

WORKDIR /work/fhirserver
COPY . /work/fhirserver

RUN cp exec/pack/linux/*.so /usr/lib/
RUN /work/fhirserver/build/linux-fhirserver.sh /work/bootstrap

# Now the slimmed down execution environment
FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y xvfb libgtk2.0-dev && rm -rf /var/lib/apt/lists/* 
WORKDIR /work/fhirserver
ENV DISPLAY :99
ENV PORT 80

COPY --from=builder /work/fhirserver/exec/64 /work/fhirserver/exec/64
COPY --from=builder /work/fhirserver/exec/pack/linux/libChakraCore.so /usr/lib/libChakraCore.so

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
