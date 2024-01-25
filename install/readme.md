# Installing FHIRServer

The full set of FHIRServer files to install is:

* the FHIRServer executable compiled in release mode + debug variant
* the FHIRConsole executable compiled in release mode
* the files in /exec/pack
* the files in /exec/path/X where X is the platform 

## Windows

In addition, the windows installer performs the following tasks
* add application icons (to FHIRConsole, mainly)
* tell you about software dependencies (slated for removal)
* (optional) install the service (and stop the service before installing if necessary)
* (optional) add the install directory to the path (not necessary for execution, but may be convenient)
* (optional) add FHIRServer to the firewall rules to allow external access
* (optional) run the FHIRConsole at the end of installation to configure the server
 
Other tasks performed by the install builder:
* update the version number in the source
* regenerate the .web file from /server/web
* sign the binaries
* upload to healthintersections.com.au/FhirServer and update the .rss and .inc files

## Linux

On linux, the distribution is just the files in a what?

Todo: what about system services?

## OSX

Who knows?

# Installing the FHIRToolkit

The full set of FHIRToolkit files to install is:

* the FHIRToolkit executable compiled in the build 
* the files in /exec/pack
* the files in /exec/path/X where X is the platform 

