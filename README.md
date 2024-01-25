# Pascal FHIRServer Reference Implementation

Reference Implementation Server + Libraries for the FHIR Specification, written in Pascal

![Build Status](https://img.shields.io/github/actions/workflow/status/HealthIntersections/fhirserver/linux-docker-build.yml?label=Build%20status)

## Basic Information

This is a set of FHIR applications written in Pascal. It is also 
the home of the pascal reference implementation for FHIR. Included 
in this project:
* The Pascal reference implementation for FHIR (/library) + many useful supporting libraries
* The FHIR reference server 
  * supports the entire FHIR functionality, along with OAuth, OpenID.Connect, and SCIM
  * includes v2 and DICOM end points
  * includes set of tests for the server and library 
* The FHIR toolkit - a set of utilities for developers


Also:

* A VCL demo program that shows how to connect to to an argonaut interface (contributed by Wellsoft, thanks)
* The FHIR Notepad++ plug-in - a set of useful utilities for FHIR developers (being phased out)

For binary releases of this content, see http://www.healthintersections.com.au/FhirServer

This project is maintained by the FHIR Project lead (Grahame Grieve). The server runs in 
multiple locations, including http://test.fhir.org, http://tx.fhir.org, and http://packages2.fhir.org

## License

The license is standard BSD-3:

Copyright (c) 2011+, HL7, Inc and Health Intersections Pty Ltd
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* Neither the name of HL7 nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.

## Support

See <https://chat.fhir.org/#narrow/stream/179281-pascal>

## Reference Server

In form, this a middleware tool to help applications exchange data via a repository. 
It also shows how to implement a generic FHIR server against an SQL database. The server 
is specifically written as an reference server - it implements all parts of the 
specification as defined, and is co-developed with the specification. It is not 
well optimised for hosting/supporting very large repositories efficiently. 

## Libraries

The open source FHIR Server includes delphi/pascal implementations of:
* Xml/XPath/XML Patch + XML Digital Signature
* JSON/JSONPointer/Json Patch + JSON Digital Signature (+ JWT/JWK support using openSSL)
* OAuth/openID Connect (including google/facebook clients)
* Turtle format (RDF)
* GraphQL
* HL7 V2
* DICOM
* CDA
* Liquid Templating language
* Javascript and Java invocation libraries
* SCIM
* LOINC/SNOMED/RxNorm/CVX/UCUM
* CQL (Clinical Query Language)
* Graphical Components
  * simple quick graphing library
  * De Novo word processor
  * FHIR LCL Components
  * FHIR FMX Components. Note that FMX support is being phased out
* All of FHIR + FHIRPath + smart all launch + cds-hooks, of course

## Projects

FPC/Lazarus:
* fhirprojects.lpg - a group containing the three projects
* /server/fhirserver.lpi - the FHIR server (win64, win32, linux64, osx64)
* /server/fhirconsole.lpi - management utility for the server  (win64, win32, linux64, osx64)
* /toolkit2/fhirtoolkit.lpi - ToolKit for FHIR developers (win64, win32, linux64, osx64)

Delphi:

* fhir-projects.groupproj - a group containing these projects
* \server\FHIRServer.dproj - the FHIR server (win64 or win32)
* \utilities\vcldemo\FhirVclDemo.dproj - a demonstration of a working standalone client for Cerner and Epic
* \utilities\nppformats\formatUtils.dproj - useful utility extensions for Notepad++ (not FHIR specific)
* \utilities\cde\ClinicalDocumentEditor.dproj - word processor demo + CDA editor. Planned to be moved to FPC/Lazarus 
* \toolkit\FHIRToolkitR3.dproj and \toolkit\FHIRToolkitR4.dproj - win/osx - deprecated for fhirtoolkit.lpi - see above
* \transformer\FHIRTransformer.dproj - Prototype transform tool - deprecated for fhirtoolkit.lpi - see above
* \npp\fhirnpp.dproj - Notepad++ extensions for FHIR - deprecated for fhirtoolkit.lpi - see above
* \utilities\publisher\IGPublisher.dproj - not sure what this is

## Folders

* **library**: the pascal reference implementation with supporting code
* **exec**: files needed by the server at run time

* **.github**: ci-build setup
* **exec**: files required at execution time, and target for the compiled applications (e.g. exec/64)
* **build**: build scripts for windows and linux. Will install the entire pascal toolchain from scratch  - see below
* **dependencies**: external code that isn't in it's own repository (mainly for legacy reasons)
* **doco**: documentation for the product (though most documentation lives in the healthintersections wiki)
* **fixtures**: resources used for running the tests during the ci-build
* **install**: install scripts for the windows versions of the applications (to be reviewed)
* **library**: library code as described above  
* **npp**: FHIR Npp - being phased out
* **packages**: lazarus packages for the libraries
* **resources**: resources used in the test scripts
* **server**: server code (including the server console)
* **testcases**: more resources for test cases 
* **toolkit**: old toolkit being phased out
* **toolkit2**: new toolkit under development
* **transformer**: - being phased out
* **utilities**: - misc other projects as listed above + java code generator for pascal code

## Compiling / Building

See [build/readme.md] for further instructions for building the programs in this repository.

## Building Release

Also see [build/readme.md] for release information
