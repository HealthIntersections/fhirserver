fhirserver
==========

Reference Implementation Server for the FHIR Specification


Basic Information
-----------------
This is the code for the reference FHIR server that runs at http://fhir.healthintersections.com.au and other places. 
It shows how to implement a generic FHIR server against an SQL database.

In form, this a middleware tool to help applications exchange data via a repository. 

The server is specifically written as an reference server - it implements all parts of the specification 
as defined, and is co-developed with the specification. It is not optimised for hosting/supporting very
large repositories efficiently. 


Compiling 
---------

This is pascal code that compoiles under Delphi XE+ (any edition, personal will do).
 
Support for Free Pascal(/Lazarus) would be good, but requires substantial work to deal with the 
difference between delphi and FPC unicode implementations.

Compiling should be simple:
* get a copy of IndySoap (http://sourceforge.net/projects/indysoap/), and add the source directory to your delphi path
* open the file Server\FHIRServer.dproj in your version of delphi, and compile


The Server uses the pascal reference implementation released with the specification. 
It is simply copied into place using the file copy_fhir.bat, and committed with the
server for convenience. 


Running
-------

System Pre-requisites
* you need either MSSQL 2012, or MySQL 5.5+  (err, MySQL is not supported right now because of a lack of open-source licensed connections - the original code used mydac)
* you need a copy of the file fhir-spec.zip from a properly built copy of FHIR. The reference source is http://hl7.org/fhir/fhir-spec.zip, but you should have the one that matches the built code compiled 
* when the executable runs, it takes two parameters, in either order: an optional ini filename, and an optional command
* for ini file documentation, see exec\fhir.ini. If no ini file is nominated, the default is fhir.ini in the same directory as the executable
* if there is no command, nothing will happen - the server will terminate

Commands

- `-help` - this document
- `setup` - install the various required tables in the nominated database
- `install 'name'` - set the fhir server to run as a service against the nominated ini file as the given service name. Default service name is "fhir-server'
- `-start 'name'` - start the named fhir service 
- `-stop 'name'` - stop the name fhir service
- `-debug` - just run the service in debug mode
- `-snomed 'dir'` - update the snomed cache from the nominated directory - either RF1 or RF2 supported
- `-loinc 'file'` - update the loinc cache from the given filename (access database)
- '-mount' - install the database
- '-unmount' - uninstall the database


