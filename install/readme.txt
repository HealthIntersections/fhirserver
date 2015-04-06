Welcome to the FHIR server install

This program will install the fhir server, and configure it's service settings.

One the installation is complete you must do the following things: 

(1) rename ini file from fhirserver.ini to fhir.dev.ini
(2) edit the ini file to configure the database 
    access (MS SQL SERVER 2012 required) 
server= network name of the database to connect top e.g.
server=.\SQLEXPRESS

(3) create a GUID and add this to the ini file under the scim section [scim] e.g.
[scim]
; SCIM sub-system configuration
salt=f5cc3abf-e9ca-4db8-86e8-ac5ec57e999e

(4)
-Define an admin user in the ini file
change [adminuser] section heading to 
[admin] and add your admin user account and email address
username=admin
email=example@gmail.com

(5) initialise the database: run fhirserver.exe -mount -password yourpassword 
    (If your MS SQL Server instance does not support Full-Text search, use the -no-text-index argument on this command) 

(6) edit the ini file to configure the web set up. 
  You must configure the http port,
  * https
  * the server's formal web address (used in redirects)
(7) Authentication (OAuth) setup
  * FHIRServer needs to reference a file called auth.example.ini to run
  * This file resides in C:\Program Files\FHIRServer
  * Add a reference to auth.example.ini in the fhir.dev.ini file here:
   clients=C:\Program Files\FHIRServer\auth.example.ini