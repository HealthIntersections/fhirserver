#!/bin/bash

# Detect architecture
ARCH=$(uname -m)

INSTALL_PATH=${INSTALL_PATH:-"$HOME/fhirserver"}
CACHE_FOLDER=${CACHE_FOLDER:-"/var/cache/txcache"}
ARM_FILES=./arm_64
X86_64_FILES=./x86_64

set -x

# Function to run commands with sudo only if not root
run_as_root() {
    if [ "$(id -u)" -ne 0 ]; then
        sudo "$@"
    else
        "$@"
    fi
}

INSTALL_AS_DAEMON=1
for arg in "$@"; do
    if [ "$arg" == "-nodaemon" ]; then
        INSTALL_AS_DAEMON=0
        break
    fi
done

run_as_root apt update && run_as_root apt install -y wget tzdata xvfb libgtk2.0-0 libsqlite3-dev

mkdir -p $INSTALL_PATH
mkdir -p $INSTALL_PATH
run_as_root mkdir -p $CACHE_FOLDER
run_as_root chmod 1777 $CACHE_FOLDER

cp bin/* $INSTALL_PATH 
cp content/* $INSTALL_PATH 
cp -r config/* $INSTALL_PATH 
cp -r web $INSTALL_PATH

# Define paths

# Copy files based on architecture
case $ARCH in
    x86_64)
        cp $X86_64_FILES/* $INSTALL_PATH
        ;;
    arm*)
        cp $ARM_FILES/* $INSTALL_PATH
        ;;
    *)
        echo "Unsupported architecture: $ARCH"
        exit 1
        ;;
esac

# Create a symlink to the executable
ln -s $INSTALL_PATH/fhirserver/start.sh /usr/local/bin/fhirserver

# Install as a daemon if not contained
if [ "$INSTALL_AS_DAEMON" -eq 1 ]; then
    # Create a systemd service file or equivalent
    echo "Installing as a daemon..."
    SERVICE_FILE="/etc/systemd/system/fhirserver.service"
    echo "[Unit]
Description=FHIR Server

[Service]
ExecStart=$INSTALL_PATH/fhirserver
# Add other service configurations as needed

[Install]
WantedBy=multi-user.target" > $SERVICE_FILE

    systemctl enable fhirserver.service
    systemctl start fhirserver.service
else
    echo "Skipping daemon installation."
fi

echo "Installation to $INSTALL_PATH completed."


