FHIRServer Security
===================

This page documents the FHIR Server security. 

Principles 
----------

The FHIRServer offers 2 interfaces: secure, and insecure. 
They both have exactly the same functionality, which is 
what is described in the FHIR Specification. However, 
on the secure interface, clients have to authenticate.
Note that there are 2 differment meanings to authenticate:

- identifying the software that is the client
- identifying the user that the software is acting on behalf of

Finally, there is *authorization* - determining what 
actions the user authorizes the software to take on their
behalf. 

Insecure Interface
------------------

This is s a non-SSL interface to the server. it is provided for 
user convenience. The main convenience is that it is is easy to
snoop on the traffic, or use simpler reverse proxies for 
troubleshooting when you're not a professional client developer. 
Also, you don't have to muck around with certificates. 

The non-SSL interface is not intended for production use, 
and so doesn't generally support security. However you can 
configure the server to use OWin-type security. if you do 
the /metadata endpoint is available to anyone, but to access
anything else, clients will have to login using the OWin 
interface - see below.

Secure Interface
----------------

This is an SSL based interface to the server, using OpenSSL.
Implementers can use a self-signed certificate, or a certificate
signed by a CA. Browsers generally require the latter now

By default, the secure interface allows anyone to retrieve
the /metadata endpoint. Any other operations require some form
of authentication. The Secure interface supports 3 kinds of 
security:

* OAuth2 login (actually, the Smart App Launch profile on OAuth2)
* OWin-type login
* Certificates + JWT Bearer Tokens

The interface can support all 3 kinds of security at the same 
time. 

OAuth2 login
------------

This implements the SMART App launch profile, as specified by the 
HL7 specification at http://hl7.org/fhir/smart-app-launch. 
There is, as yet, no dynamic registration of clients. Clients must 
be registered in the authorization file manually

Part of the OAuth login process  includes identifying the
end user. the FHIR Server supports the following methods 
of identifying a user: 
- user login maintained in the FHIR Server using the inbuilt SCIM server
- delegated identify provider to one of Google, Facebook, or HL7.org (other providers can be added)
- user can authenticate to the system administrator directly (e.g. by skype) (note: no one has ever done this...)

These methods and the tokens required to administer them are
also configured in the authorization file

Security summary:
- software is identified by the client_id (and possibly secret)
- user is identified by login or identity provider
- user authorises software to perform a set of actions using Smart on fhir scopes


OWin Authorization
------------------

This is a form of authentication used by the .net OWin framework,
though not actually documented as part of the OWin specification (yet?)

User posts a body of type application/x-www-form-urlencodedto [base]/oauth/token.
The body contains the parameters username and password, and also grant_type.
The server checks the user name and password, and if it is acceptable, 
returns a JSON body:

  {
    "access_token" : "[token}",
    "expires_in" : "{#seconds}",
    "token_type" : "bearer"
  }
  
The client extracts the token, and makes subsequent calls using 
the header

  Authorization: Bearer {token}
  
Security summary:
- software is identified by the username/password in the call
- the user is not identified
- the software is authorised to do anything on the interface

Certificates + JWT
------------------

In this method, the client uses an SSL certificate to connect to the server,
and the server uses it to identify the client. The server will automatically
pick up the client identity from the certificate. In addition, the server can 
be configured to verify that the certificate is on a known list of acceptable 
certificates. 

The certificates are considered to authenticate the client software. Clients 
can also provide a JWT in the authorization header as a bearer token:

  Authorization: Bearer [JWT]

The server will pick up the JWT and (i nthe future) check that it is valid.
If it is, it will treat the JWT as an openID Connect JWT that identifies the 
user of the software

Administrators can also configure what to do when there is no certificate/JWT, 
or unrecognised ones:
- reject the request
- limit the response (see below)
- doesn't make any difference

Security summary:
- software is identified by the SSL certificate
- the user is identified by the JWT
- the software is authorised to do anything on the interface


