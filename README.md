fhirserver
==========

Reference Implementation Server for the FHIR Specification. 


Basic Information
-----------------

This is a Pascal (Delphi) server that implements OAuth, OpenID.Connect, SCIM, and FHIR, 
and is maintained by the author of the FHIR specification. The server runs at 
http://fhir.healthintersections.com.au, http://fhir-dev.healthintersections.com.au
and other places. 

In form, this a middleware tool to help applications exchange data via a repository. 
It shows how to implement a generic FHIR server against an SQL database.

The server is specifically written as an reference server - it implements all parts of the specification 
as defined, and is co-developed with the specification. It is not optimised for hosting/supporting very
large repositories efficiently. 

Installing the Server
---------------------

You have to:
* install this first: http://www.healthintersections.com.au/AltovaXmlCom.msi
* edit the ini file


Compiling 
---------

This is pascal code that compoiles under Delphi XE+ (any edition, personal will do).
 
Support for Free Pascal(/Lazarus) would be good, but requires substantial work to deal with the 
difference between delphi and FPC unicode implementations.

Compiling should be simple:
* get a copy of IndySoap (http://sourceforge.net/projects/indysoap/), and add the source directory to your delphi path
** Note: you don't need to install indysoap, just have it in your source path 
* open the file Server\FHIRServer.dproj (fro the DSTU version) or Server\FHIRServerDev.dproj (for the trunk version of FHIR) in your version of delphi, and compile

The Server uses the pascal reference implementation released with the specification. 
It is simply copied into place (using the file copy_fhir.bat), and committed with the
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
- `-mount` - install the database. You can also get the mount process to load the examples in the fhir specification using an extra parameter: `-load 'filename'` 
- `-unmount` - uninstall the database
- `-remount` - uninstall and then install the database (also uses the -load parameter)
- `-tests` - run the automated tests. Needs a local mssql database called "fhir-test" with trusted access
- `-snomed-rf1 'folder'` - import a SNOMED CT distribution using an RF1 format (can take ~1hour). Nominate the directory that contains the snapshot directly
- `-snomed-rf2 'folder'` - import a SNOMED CT distribution using an RF2 format (can take ~1hour). Nominate the directory that contains the snapshot directly
- `-loinc 'file'` - update the loinc cache from the given filename (access database)
- `-rxstems` - connect to the RxNorm database, and generate the text search index
- '-unii 'file'` - fill the unii database from the unii text distribution format (tab delimited)



