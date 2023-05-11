# Building the code 

The code compiles under either Delphi (windows 32+64) or Lazarus/FPC (windows 32+64/Linux/OSX). 

## System Pre-requisites

### Windows

The build is tested on windows 10. Other versions of windows might work. If you're going to run the tests, 
you need to install mysql and mssql (dev license versions are fine). 

### Linux

The list of packages that must be installed is in the [Dockerfile](../Dockerfile) starting line #5. You might
not need all these packages to do development, but you need them all to pass the test cases.

### OSX

(to be documented)

### FPC / Lazarus

The code compiles using the trunk versions of FPC + Lazarus. In time, it would be good to transit to the stable version, but compiling depends on bug fixes in FPC that are not in stable yet.
You can install Lazarus using any method you want, and then fetch and get the dependencies documented in [the dependency build script](unix-libraries.sh). Alternatively, just run the 
install script for the relevant OS you are using:

* linux.sh
* windows-all.bat
* osx.sh

All of these scripts are run from the root directory of the repository e.g. build/linux.sh, and take a single parameter, which is the root directory to use for temporary files. 
You can nominate any directory except a directory inside the FHIRServer repository (git restriction). Typically, if you are just doing a once off build, you'd choose a temporary
folder or a RAM drive. If you're getting ready to use lazarus to develop the software, the usual practice is to name the folder that contains the repository -  this will set 
up /tools and /source as sibling directories, and leave a fully installed version of lazarus ready to go in {folder}/tools/lazarus.

Then open the file fhirprojects.lpg, and you should be good to go. I recommend installing the following packages into the IDE:
 * anchordocking
 * anchordockingdsgn
 * laz_virtualtree_package
 * laz project groups

### Delphi

The code should in principle compile under Delphi XE3+ (any edition, personal will do). Note that 
in practice, various subtle but breaking changes have been introduced to the runtime library (Streams, 
Indy) that mean that some fiddling with IFDEFs may be necessary. 10.3 and 10.4 are both expected to 
work without modification.

In order to properly compile, you need to fetch the repositories listed in [the dependency build script](unix-libraries.sh)
and update the project paths to point to them. 

And also you need to install https://bitbucket.org/sglienke/testinsight/wiki/Home (all delphi 
users should have this installed!)

Note that the FHIRServer is a *big* compile. You may have problems compiling in 
resource constrained environments.

The delphi compile should work without further manipulation of library paths etc if 
you install using the build script. If you don't, then the paths require that this 
project is in a directory called 'server', with the other dependencies detailed above 
are in a sibling directory called 'source'.

## More about the Build scripts 

The build scripts are found in /build. There are 3 sets of files:

* windows*.bat
* linux*.sh
* osx*.sh

When invoking these, you can provide a directory to use as the working directory for the tool chain and dependencies (see above).

These are broken up into modules:
* x-all.x: Install the entire tool chain
* x-update.x: Just update the libaries and build. Use this after you've run x-all.x once

* x-toolchain.x: installs the actual tool chain
* x-libraries.x: installs/updates the libraries 
* x-fhirserver.x: does the actual build of the 3 applications

In general, the intent is to only use the first two

You can run these without parameters, in which case they'll attempt to use a default location (/tmp/fsbuild for linux, r:/fsbuild - ram drive - for windows).
In that location, all the tools and libraries are installed, including a fully working version of the lazarus IDE which is used for developing
the applications (if not using delphi)

## Test cases 

To run the test cases, execute the fhirserver executable with the parameters ```-tests``` or ```-tests -gui```.
There must be a file test-settings.ini in the same directory as the executable that provides the settings 
for the tests:

```
[locations]
fhirserver= the local folder for the repo
fhir-test-cases= where the fhir test cases are
markdown= the root directory of the markdown repo in the dependencies
snomed= the local location of the file https://storage.googleapis.com/ig-build/snomed.test.cache

; if there's no mssql, delete this section
[mssql]
driver=SQL Server
server=(local)
database=test
username=
password=
; username and password are only required if not using windows authentication

; if there's no mysql, delete this section
[mysql]
driver=MySQL ODBC 8.0
server=${FHIRSERVER_MYSQL_SERVER}
database=
username=
password=

[email]
; Sender MUST be a GMail aaccount; servers are hardcoded
sender=
password=; password for the sender account
destination=

[ssl]
; test ssl cert for testing ssl related features.
cert= {public key cert filename}
key= {private key filename}
cacert= {ca cert filename}
password= {for private key}
```

# Releasing the Applications

Releases are done through GitHub releases. The windows build creates the release, and uploads 
the windows bines (installers for server and toolkit). Then other builds are run on linux and
OSX and upload additional binaries. 

## Windows Release

Windows: execute build\windows-release.bat [version] ([path])

where:
* version is the version that will be released
* path is the optional path (matches what was used when running the batch files above)

In addition to the code checked into git, you need:
* a code signing certificate
* a batch file load-password.bat that has the password for the code signing certificate 
* an install\tools directory that has command line versions of 
  * dcc64 + the delphi release library 
  * 7-zip
  * signtool (from ksign)
  * innosetup 
  * gh (github cli)
  
  
## OSX Release

work in progress

## Linux Release

yet to be  done
