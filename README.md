fhirserver
==========

Reference Implementation Server for the FHIR Specification. 

Basic Information
-----------------

This is a FHIR server + utilities written in Delphi. It is also 
the home of the pascal reference implementation. Included in this 
project:
* The Delphi reference implementation for FHIR (/reference-platform)
* A set of tests for the delphi reference implementation 
* The FHIR reference server (supports the entire FHIR functionality, along with OAuth, OpenID.Connect, and SCIM
* The FHIR Notepad++ plug-in - a set of useful utilities for FHIR developers 
* A simple FHIR Value set editor - primarily intended for FHIR specification / implementation guide editors

For binary releases of this content, see http://www.healthintersections.com.au/FhirServer

This project is maintained by the FHIR Project lead. The server runs in 
multiple locations, including http://fhir2.healthintersections.com.au. 
and other places. 

Reference Server
----------------

In form, this a middleware tool to help applications exchange data via a repository. 
It shows how to implement a generic FHIR server against an SQL database. The server 
is specifically written as an reference server - it implements all parts of the 
specification as defined, and is co-developed with the specification. It is not 
well optimised for hosting/supporting very large repositories efficiently. 

Compiling 
---------

This is pascal code that (in principle) compiles under Delphi XE3+ (any edition, personal will do).
Note that in practice, various subtle but breaking changes have been introduced to the runtime
library (Streams, Indy) that mean that some fiddling with IFDEFs may be necessary.

Support for Free Pascal(/Lazarus) would be good, but requires substantial work to deal with the 
difference between delphi and FPC unicode implementations. Contributions are welcome

Some of these projects depend on the MarkdownProcessor at 
https://github.com/grahamegrieve/delphi-markdown. You need to get a copy of 
this and fix the paths for the markdown processor units in the .dprs.

After that, compiling should be simple: open the file fhirprojects.groupproj in your version of delphi, and compile

Running
-------

* ValueSet Editor: just execute 
* Notepad++ plug-in. Either run the installer, or change the compile location to compile the plug-in to the notepad++ plug-in directory for your systems
* Server: See install\readme.txt, and also, you need a copy of the full specification for the same version as the generated reference implementation code (see the downloads page for that version) 

