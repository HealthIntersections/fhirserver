
const

  ID_SOAP_VERSION = '2.00-000';

{ 29-May 2013   2.00-000 

Indy Soap comes to life again!

This version of IndySoap
- compiles and passes all tests under Delphi XE3 and Delphi 5
   - other versions have not been tested
- fixes many bugs due to test coverage being improved
- has a few bugs that were fixed over a 10 year period of using IndySoap 

}  
{ 18-Mar 2003   1.00

 MAJOR RELEASE

 Many things have been fixed or overhauled.
 highlights:
   * much improved performance
      - loops have been trimmed and optimised
      - custom XML pasrser introduced

   * support for
      - headers
      - polymorphism
      - sets
      - default values on properties
      - compression (if you have zlib.pas and set the ID_SOAP_COMPRESSION define)
      - username and password on WinInet client
      - ergo, MapPoint is now supported

   * Bugs fixed
      - worked around an problem with cookies in some versions of Indy9
      - manual properties are registered with all descendent classes now
      - fixed several character set issues

 Specific changes that may raise backward compatibility issues:
  * ITI wide renaming (no longer constrained to interfaces or methods, you can define renaming for types and names at the ITI level)
  * change parameters of array property registration (split first parameter into 2 strings)
  * the way that WSDL services are mapped into pascal interfaces, and
    the names that they get, has been overhauled. The names are quite likely
    to be different - you will either have to fix them or update your source
    (but their functionality won't have changed)

 Other misc changes
  * allow interface implementation factories to be un-registerd, don't provide wsdl for unregistered interfaces (package support)
  * fix various range check failures

Status
 D4    compiles. DUnit not checked
 D5    compiles, DUnit Passes
 D6    compiles, DUnit Passes
 D7    not checked

 Kylix: This version compiles but does not work. This is under investigation
}

//  ID_SOAP_VERSION = '1.00';

{ 18-Mar 2003   0.07

* Introduce TIdSoapQName
* Introduce TIdSoapRawXML
* Fix leak - var parameters returned as nil by the server used to leak
* introduce DOM based messaging events
* fix IDE problems with asserts in notify events
* remove ability to use any version of OpenXML - too many changes in OpenXML
* relax type checking - types only required on nodes that are interpreted as types now

Status
 D4    compiles. DUnit not checked
 D5    compiles, DUnit Passes
 D6    compiles, DUnit Passes
 D7    not checked
 K1    not checked
 K2    Compiles, DUnit tests crashes X-Windows
 K3    not checked
}
{
  ID_SOAP_VERSION = '0.07';
}

{ 25-Feb 2003   0.06
* various minor fixes for issues reported on indy-soap-public
* introduce IdSoapXml.pas for xml library independence
}

{ 29-Oct 0.05
* introduce new IdSoapSimpleClass for smarter handling of simple
  XML types if desired
* Introduce TIdSoapFault for smarter SOAP exception handling
* New Encoding Option seoUseDefaults. Before this, some types
  allowed defaults and some didn't. Now most types will allow a
  default type with this option.
* fix interoperability issue where parameters where not being written
  in order
* fix bug in names of binding operation messages

}


{ 04-Oct 0.04
  Installer
  Attachment support [DIME] begins. To do:
    * attachment support in marshalling engine
    * attachment support reading and writing WSDL's
    * SwA / Mime
    * SoapBuilders Testing
  TwoWay TCP/IP (Asynchronous) introduced
  Changed Server side implementation objects lifetime handling
}

{0.03         26-Aug 2002  
  Header support improved
  Session Support introduced
  Introduce seoSendCharset, don't send charset as a matter of course
}


{0.02         24-Aug 2002  
  Introduction of doc|lit support
}

{0.01
 until version started to be tracked
}
