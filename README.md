# Pascal FHIRServer Reference Implementation

Reference Implementation Server + Libraries for the FHIR Specification, written in Pascal

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
* The FHIR Notepad++ plug-in - a set of useful utilities for FHIR developers 
* A VCL demo program that shows how to connect to to an argonaut interface (contributed by Wellsoft, thanks)

For binary releases of this content, see http://www.healthintersections.com.au/FhirServer

This project is maintained by the FHIR Project lead. The server runs in 
multiple locations, including http://test.fhir.org, http://tx.fhir.org, and http://packages2.fhir.org

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
* JSON/JSONPointer/Json Patch + JSON Digital Signature (+ JWT/JWK support)
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
  * Scintilla Wrapper (per InnoSetup) with XMl/JSON/Javascript syntax highlighters
  * simple quick graphing library
  * De Novo word processor
  * FHIR FMX Components
* All of FHIR + FHIRPath + smart all launch + cds-hooks, of course

## Projects

FPC:
* /server/fhirserver.lpi - the FHIR server (win64, win32, linux64, osx64)
* /server/fhirconsole.lpi - management utility for the server  (win64, win32, linux64, osx64)
* /toolkit2/fhirtoolkit.lpi - ToolKit for FHIR developers (win64, win32, linux64, osx64)

Delphi:

* \server\FHIRServer.dproj - the FHIR server (win64 or win32)
* \utilities\vcldemo\FhirVclDemo.dproj - a demonstration of a working standalone client for Cerner and Epic
* \utilities\nppformats\formatUtils.dproj - useful utility extensions for Notepad++
* \utilities\cde\ClinicalDocumentEditor.dproj - word processor demo + CDA editor. Planned to be moved to FPC/Lazarus 
* \toolkit\FHIRToolkitR3.dproj and \toolkit\FHIRToolkitR4.dproj - win/osx - deprecated for fhirtoolkit.lpi - see above
* \transformer\FHIRTransformer.dproj - Prototype transform tool - deprecated for fhirtoolkit.lpi - see above
* \npp\fhirnpp.dproj - Notepad++ extensions for FHIR - deprecated for fhirtoolkit.lpi - see above
* \utilities\publisher\IGPublisher.dproj - not sure what this is

## Folders

* **library**: the pascal reference implementation with supporting code
* **exec**: files needed by the server at run time

## Compiling 

The code compiles under either Delphi (windows 32+64) or Lazarus/FPC (windows 32+64/Linux/OSX).

### Delphi

The code should in principle compile under Delphi XE3+ (any edition, personal will do). Note that 
in practice, various subtle but breaking changes have been introduced to the runtime library (Streams, 
Indy) that mean that some fiddling with IFDEFs may be necessary. 10.3 and 10.4 are both expected to 
work without modification.

In order to properly compile, the following git repositories must be put in these places:
- https://github.com/grahamegrieve/fhirserver in C:\work\fhirserver
- https://github.com/grahamegrieve/delphi-markdown in C:\work\markdown

And also you need to install https://bitbucket.org/sglienke/testinsight/wiki/Home (all delphi 
users should have this installed!)

For design time support for the GUI applications, you need to install treeview and 
synedit (see in dependencies folders), then open and compile the packages in /packages, 
and install the 2 design time packages.

Note that the FHIRServer is a *big* compile. You may have problems compiling in 
resource constrained environments.

### FPC / Lazarus

The code compiles using FPC 3.3.1 / Lazarus 2.1.0 (or more recent). The code depends on some 
recent bug fixes so older versions are probably not supported.
  
The FhirServer depends on the following other GitHub repositories:
* https://github.com/dezlov/PascalTZ
* https://github.com/grahamegrieve/delphi-markdown

The toolkit also depends on these repositories:
* https://github.com/mriscoc/extrasyn
* https://github.com/BerndGabriel/HtmlViewer

Get a local copy of these, and install their packages. 

## Test Cases

in order to run the tests, you also need the repo https://github.com/FHIR/fhir-test-cases locally, and you'll need to pass this as a parameter to the test cases.

## Building Release

Bulding an actual release requires the following tools
* Innosetup v6 + 
* FinalBuilder v8
