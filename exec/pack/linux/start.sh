#!/bin/bash
set -e

# Determine the directory where this script is located (this will be $HOME/fhirserver)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SERVICE_FILE="/etc/systemd/system/fhirserver.service"

# Function to start Xvfb
start_xvfb() {
    Xvfb :99 -screen 0 1024x768x8 -nolisten tcp &
    sleep 2  # Give some time for Xvfb to start
}

# Function to stop Xvfb
stop_xvfb() {
   pkill Xvfb || true
}

trap stop_xvfb EXIT

rm -f /tmp/.X99-lock
start_xvfb
export DISPLAY=:99

# Parse command-line arguments
DAEMON_MODE=""

for arg in "$@"; do
    case $arg in
        -daemon)
            DAEMON_MODE="enable"
            ;;
        -nodaemon)
            DAEMON_MODE="disable"
            ;;
    esac
done

# Determine which config.ini to use
if [ -f "$SCRIPT_DIR/config/config.ini" ]; then
    CONFIG_FILE="$SCRIPT_DIR/config/config.ini"
else
    CONFIG_FILE="$SCRIPT_DIR/default_config/config.ini"
fi

if [ ! -f "$SCRIPT_DIR/config/config.json" ]; then
    cp "$SCRIPT_DIR/default_config/config.json" "$SCRIPT_DIR/config/config.json"
    echo "File copied successfully."
fi

# Handle daemon management
if [ "$DAEMON_MODE" == "enable" ]; then
    sudo systemctl enable fhirserver.service
    sudo systemctl start fhirserver.service
elif [ "$DAEMON_MODE" == "disable" ]; then
    sudo systemctl stop fhirserver.service || true
    sudo systemctl disable fhirserver.service || true
fi

# Start the FHIR server using the determined config file
exec "$SCRIPT_DIR/fhirserver" -cmd exec -cfg "$CONFIG_FILE"
