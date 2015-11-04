Welcome to the FHIR server install

This program will install the fhir server, and configure it's service settings.

One the installation is complete you must do the following things: 

(1) edit fhirserver.ini 
  - configure database access 
  - configure the web address settings and ports and certificates
  - create a GUID and set the SCIM salt 
  - Define an admin user in the ini file [adminuser] section
  - generally review other settings
(2) possibly edit auth.ini to configure oauth relationships etc
(3) initialise the database: run fhirserver.exe -mount -password yourpassword 

Note: you do not need to run SSL, but if you want to, you need to use openSSL to create or get valid certificates (public / privsate key in DER format, along with the public key of the CA server)    
   
Server Command Line Parameters:
-------------------------------

commands (these are mutually exclusive):
-install - install the service in the windows service catalogue (note that this is usually done by the installer, and so not necessary. It doesn't create the database - see -mount)
-start - start the service (same as starting through the service manager)
-stop - stop the server (same as stopping through the service manager)
-remove - remove the service from the windows service catalogue
-debug - just run the fhir server directly from the command line
-mount. populate the database starting from an empty state
-unmount. remove all the database content created by -mount
-remount. run -unmount, then -mount
-index. rebuild the indexes on all resources, but leave the database untouched otherwise
-tests. run internal tests (Grahame only; other users don't have all the requirements for this)
-snomed-rf1 - upload snomed CT from RF1 release (this is not maintained anymore)
-snomed-rf2 - upload Snomed CT from an RF2 release (tested: US, AU and AMT releases)
-loinc - upload LOINC from the .csv distrbution file
-rxstems - after using the import rxnorm script, finish processing the content 
-ncistems - after using the import ncimeta script, finish processing the content

if you run the server without any of these comamnds, it will do nothing

options:
-ini [filename]. the name of the ini file for settings. Default is fhirwerver.ini in the same directory as the executable
-name [name]. name of the service, for  -install, -start, -stop, -remove. The default service name is fhirserver. The only reason to change this is run more than one server per system
-title ["display name"]. the title of the service, for -install, -start, -stop, -remove, and also for -debug. Default is FHIR Server (DSTU2)
-unii [filename]. Use with -mnount, -remount, to load the unii codes from unii.txt, when installing (get unii.txt from unii distribution)
-profile [filename]. Not used at this time
-load [filename]. Load the database with a set of resources from a zip file. Usually this would be spec/examples.zip
-sver [string]. Use with -snomed-rf1 or -snomed-rf2 to specify the release URL (required)
-lver [string]. Use with -loinc to specify the version of LOINC being imported (required)
-password [pword]. Use with -mount of -remount to specify the password of the administrator account (required)
-no-text-index. Use to specify not to create a text index (for Azure versions of SQL Server)


