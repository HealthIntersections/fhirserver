#!/bin/bash
set -e

start_xvfb() {
    Xvfb :99 -screen 0 1024x768x8 -nolisten tcp &
    sleep 2  # Give some time for Xvfb to start
}

stop_xvfb() {
   pkill Xvfb || true
}

trap stop_xvfb EXIT

rm -f /tmp/.X99-lock
start_xvfb
export DISPLAY=:99

./fhirserver -cmd exec -cfg $HOME/fhirserver/config/config.ini