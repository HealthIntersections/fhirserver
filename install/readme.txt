Welcome to the FHIR server install

This program will install the fhir server, and configure it's service settings.

One the installation is complete you must do the following things: 

(1) edit the ini file to configure the database 
    access (MYSQL or MSSQL 2012 required)
(2) initialise the database (run fhirserver.exe -mount)
(3) edit the ini file to configure the web set up. 
  You must configure the http port,
  * https
  * the server's formal web address (used in redirects)
  * OAUth set up
