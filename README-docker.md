# FHIR Server Docker Installation

This  Dockerfile and necessary scripts build and run a FHIR Server environment on Ubuntu 24.04. The Dockerfile is split into two stages: a **builder** stage where the FHIR server is built and packaged, and a **runtime** stage where the server is executed. Below are the details of the build and installation process.

## Dockerfile Overview

### 1. **Builder Stage**
In the builder stage, the Dockerfile performs the following tasks:
- Installs the necessary dependencies, including `git`, `curl`, `openssl`, `unixODBC`, and other libraries required for building the server.
- Downloads and compiles **OpenSSL 1.1.1w**.
- Installs **MySQL ODBC Connector 8.0.26**.
- Copies toolchain and library scripts from the host machine (`build/linux-toolchain.sh` and `build/linux-libraries.sh`) and executes them to prepare the build environment.
- Builds the FHIR server in `/work/fhirserver` using the `linux-fhirserver.sh` script.
- Copies all necessary files (binaries, libraries, properties files, etc.) to the installation directory: `/work/fhirserver/exec/install`.
- Packages the installation files into a tarball (`install.tgz`) located in `/work/fhirserver/exec/install.tgz`.

### 2. **Runtime Stage**
In the runtime stage, the Dockerfile:
- Installs necessary runtime dependencies like `xvfb`, `libgtk2.0`, `libsqlite3`, and others required to run the FHIR server.
- Creates directories for configuration, package cache, and terminology cache:
  - Configuration directory: `/root/fhirserver/config`
  - Terminology cache: `/var/cache/txcache`
- Extracts the tarball (`install.tgz`) into `/root/fhirserver/install`.
- Runs the installation script (`install.sh`) to set up the FHIR server environment.
- Exposes port `80` for the FHIR server.

## Paths

### 1. **Installation Paths**
The software is installed by default into the following directories:
- **Binary files** (executables): `/root/fhirserver/install/bin/`
- **Libraries**: `/root/fhirserver/install/x86_64/`
- **Configuration files**: `/root/fhirserver/install/config/`
- **Default configuration files**: `/root/fhirserver/install/default_config/`
- **Content files (such as `.properties` and `.dat` files)**: `/root/fhirserver/install/content/`
- **Web assets**: `/root/fhirserver/install/web/`

### 2. **Cache and Configuration**
- **Terminology cache**: `/var/cache/txcache`
  - This directory is used for caching terminology data during FHIR operations.
- **Configuration**:
  - The main configuration files for the FHIR server are stored in `/root/fhirserver/install/config/`. If configuration files are not available, default configurations from `/root/fhirserver/install/default_config/` will be used.

### 3. **Tarball Location**
The final packaged installation tarball is created in `/root/fhirserver/install.tgz`. This tarball contains all the necessary files to deploy and run the FHIR server.

## Health Check
The Dockerfile includes a health check to verify if the FHIR server is running. It sends a request to `http://localhost:80/fhir/metadata` every minute. If the server fails to respond, the container is considered unhealthy.

```bash
HEALTHCHECK --interval=1m --timeout=10s --retries=5 \
  CMD curl -f http://localhost:${PORT}/fhir/metadata || exit 1
```



## Environment Variables

- `DISPLAY=:99`: Xvfb is used for the graphical environment. This environment variable sets the display.
- `PORT=80`: The default port for the FHIR server.
- `TERMINOLOGY_CACHE=/var/cache/txcache`: The directory used to cache terminology data during runtime.

## Installation Process

The installation is done by running the installer after ensuring the needed permissions are set:

```bash
RUN chmod a+x ./install.sh && ./install.sh
```


## Usage Instructions

### Build the Docker Image

To build the Docker image, use the following command:

```bash
docker build -t fhirserver-image .
```


### Run the Docker Container

#### Default configuration
To run the container with the default configuration:

```bash
docker run -d -p 80:80 --name fhirserver fhirserver-image`
```
This command will run the FHIR server on port 80 of your host machine.

#### Customizing Configuration

You can customize the configuration of the FHIR server by mapping local directory to the respective Docker container directory (`/root/fhirserver/config`). This can be done using Docker volume mapping.

For example, to override the default configuration by mapping a local folder to the FHIR server's configuration folder:
```yaml
version: '3.3'  
services: 
  fhirserver:  
    image: zeora/fhirserver:nightly
  volumes: 
  - ./config:/root/fhirserver/config        
```

You can also map a local terminology cache to `/var/cache/txcache` to persist it between container restarts.


### Access the FHIR Server

Once the container is running, you can access the FHIR server at http://localhost

