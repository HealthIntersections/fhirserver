#!/bin/bash
cd "$(dirname "$0")"

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

run_as_root apt update && run_as_root apt install -y wget tzdata xvfb libgtk2.0-0 libsqlite3-dev curl

mkdir -p $INSTALL_PATH
mkdir -p $INSTALL_PATH/config
mkdir -p $INSTALL_PATH/default_config
run_as_root mkdir -p $CACHE_FOLDER
run_as_root chmod 1777 $CACHE_FOLDER

cp bin/* $INSTALL_PATH 
cp content/* $INSTALL_PATH 
# cp -r config/* $INSTALL_PATH 
# cp -r config/config/* $INSTALL_PATH/config 
cp -r config/* $INSTALL_PATH/config 
cp -r default_config/* $INSTALL_PATH/default_config 
cp -r web $INSTALL_PATH

# Files based on architecture
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

# Create link so that the server can be started from anywhere
ln -s $INSTALL_PATH/start.sh /usr/local/bin/fhirserver

# Copy the default configuration file
cp "$INSTALL_PATH/default_config/config.json" "$INSTALL_PATH/default_config/config.json"

# Prepare and install the systemd service file but do not enable or start it
SERVICE_FILE="/etc/systemd/system/fhirserver.service"
echo "[Unit]
Description=FHIR Server

[Service]
ExecStart=$INSTALL_PATH/fhirserver
# Add other service configurations as needed

[Install]
WantedBy=multi-user.target" | run_as_root tee $SERVICE_FILE > /dev/null

echo "Installation to $INSTALL_PATH completed."

cd $INSTALL_PATH