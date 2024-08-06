#!/bin/bash
set -e

start_xvfb() {
    Xvfb :99 -screen 0 1024x768x8 -nolisten tcp &
    sleep 2  # Give some time for Xvfb to start
}

# get the default config if the config folder is empty (e.g. when mounted as a volume)
if [ -z "$(ls -A /root/fhirserver/config)" ]; then
  cp /root/fhirserver/default_config/* /root/fhirserver/config/
fi

stop_xvfb() {
   pkill Xvfb || true
}

trap stop_xvfb EXIT

rm -f /tmp/.X99-lock
start_xvfb
export DISPLAY=:99

./fhirserver -cmd exec -cfg $HOME/fhirserver/config/config.ini