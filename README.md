fhirserver
==========

Reference Implementation Server for the FHIR Specification. 

Basic Information
-----------------

This is a set of FHIR applications written in Delphi. It is also 
the home of the pascal reference implementation for FHIR. Included 
in this project:
* The Delphi reference implementation for FHIR (/reference-platform)
* A set of tests for the delphi reference implementation 
* The FHIR reference server (supports the entire FHIR functionality, along with OAuth, OpenID.Connect, and SCIM)
* The FHIR toolkit - a set of utilities for developers
* The FHIR Notepad++ plug-in - a set of useful utilities for FHIR developers 
* A VCL demo program that shows how to connect to to an argonaut interface (contributed by Wellsoft, thanks)

For binary releases of this content, see http://www.healthintersections.com.au/FhirServer

This project is maintained by the FHIR Project lead. The server runs in 
multiple locations, including http://test.fhir.org, and other places. 

Reference Server
----------------

In form, this a middleware tool to help applications exchange data via a repository. 
It also shows how to implement a generic FHIR server against an SQL database. The server 
is specifically written as an reference server - it implements all parts of the 
specification as defined, and is co-developed with the specification. It is not 
well optimised for hosting/supporting very large repositories efficiently. 

Libraries
---------

The open source FHIR Server includes delphi/pascal implementations of:
* Xml/XPath/XML Patch + XML Digital Signature
* JSON/JSONPointer/Json Patch + JSON Digital Signature (+ JWT/JWK support)
* OAuth/openID Connect (including google/facebook clients)
* Turtle format (RDF)
* GraphQL
* Javascript and Java invocation libraries
* SCIM
* LOINC/SNOMED/RxNorm/CVX/UCUM
* CQL (Clinical Query Language)
* All of FHIR + FHIRPath + smart all launch + cds-hooks, of course

Compiling 
---------

This is pascal code that (in principle) compiles under Delphi XE3+ (any edition, personal will do).
Note that in practice, various subtle but breaking changes have been introduced to the runtime
library (Streams, Indy) that mean that some fiddling with IFDEFs may be necessary.

Support for Free Pascal(/Lazarus) would be good, but requires substantial work to deal with the 
difference between delphi and FPC implementations of pascal. Contributions are welcome

The FhirServer depends on the following other GitHub repositories:
* https://github.com/grahamegrieve/delphi-markdown
* https://github.com/VSoftTechnologies/DUnitX
* + TreeView

You need to get a copy of these and fix the paths for the markdown processor units in the .dprs.
You also need to get a copy of the latest version of the Indy components, since the server
depends on some recent fixes to Indy. note you don't need to install them to delphi - just put them in 
your system path. (Tools...Options)

And also you need to install https://bitbucket.org/sglienke/testinsight/wiki/Home (all delphi 
users should have this installed!)

After that, compiling should be simple: open the file fhirprojects.groupproj in your version of delphi, and compile all projects

Note that the FHIRServer is a *big* compile. You may have problems compiling in resource constrained environments.

Building Release
----------------

Bulding an actual release has a long list of dependencies, including 
several versions of the FHIR specification, and the following tools
* Innosetup + InnoSetup download plug-in
* FinalBuilder
 
Running
-------

* Notepad++ plug-in. Either run the installer, or change the compile location to compile the plug-in to the notepad++ plug-in directory for your systems
* Server: See install\readme.txt, and also, you need a copy of the full specification for the same version as the generated reference implementation code (see the downloads page for that version) 

