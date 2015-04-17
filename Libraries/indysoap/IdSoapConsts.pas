{
IndySOAP: Global Constants
}

Unit IdSoapConsts;

{$I IdSoapDefines.inc}

Interface

{$I IdSoapVersion.inc}

{$IFNDEF VER240}
Type
  TSymbolName = ShortString;
{$ENDIF}

Const
  LF = #10;
  CR = #13;

  EOL_WINDOWS = CR + LF;
  EOL_PLATFORM = EOL_WINDOWS;

  //
  CHAR0 = #0;
  BACKSPACE = #8;

  TAB = #9;
  CHAR32 = ' ';

  {$IFDEF DELPHI5}
  PathDelim = '\';                                                                // do not localise
  {$ENDIF}

  MININT = Integer($80000000); // when no default is set on a property

  // ITI management
  ID_SOAP_CONFIG_FILE_EXT = '.IdSoapCfg';                                         // do not localise

  // Interface specifics
  ID_SOAP_INTERFACE_BASE_NAME = 'IIdSoapInterface';    // do not localise   MUST be the name of the base IndySoap interface

  // ITI related constants
  ID_SOAP_ITI_BIN_STREAM_VERSION_OLDEST = 7;
  ID_SOAP_ITI_BIN_STREAM_VERSION_SOAPACTION = 8;         // When SOAP Action was introduced
  ID_SOAP_ITI_BIN_STREAM_VERSION_NAMES = 9;              // When Name ReDefining was introduced
  ID_SOAP_ITI_BIN_STREAM_VERSION_SOAPOP = 10;            // When Doc|Lit support was introduced
  ID_SOAP_ITI_BIN_STREAM_VERSION_SESSION = 11;           // When Sessional support was introduced
  ID_SOAP_ITI_BIN_STREAM_VERSION_INTF_FIX = 12;          // When interface inheritance was fixed
  ID_SOAP_ITI_BIN_STREAM_VERSION_HEADERS = 13;           // When headers were added
  ID_SOAP_ITI_BIN_STREAM_VERSION_ATTACHMENTS = 14;       // When attachment encoding type was added
  ID_SOAP_ITI_BIN_STREAM_VERSION_ENCODINGOVERRIDE = 15;  // When encoding override type was added
  ID_SOAP_ITI_BIN_STREAM_VERSION_CATEGORY = 16;          // When Interface category was added
  ID_SOAP_ITI_BIN_STREAM_VERSION_SESSION2 = 17;          // Change session from boolean to TIdSoapSessionOption
  ID_SOAP_ITI_BIN_STREAM_VERSION_VISIBILITY = 18;        // When Interface.Visibility was introduced
  ID_SOAP_ITI_BIN_STREAM_VERSION_COMINITMODE = 19;       // When Method.WantCOMInit was introduced

  ID_SOAP_ITI_BIN_STREAM_VERSION = 19;   // SOAP ITI stream version number
  ID_SOAP_ITI_XML_STREAM_VERSION = 7;   // SOAP ITI stream version number

  // ITI XML Node Names
  ID_SOAP_ITI_XML_NODE_NAME = 'Name';                                            // do not localise
  ID_SOAP_ITI_XML_NODE_UNITNAME = 'UnitName';                                    // do not localise
  ID_SOAP_ITI_XML_NODE_GUID = 'GUID';                                            // do not localise
  ID_SOAP_ITI_XML_NODE_ANCESTOR = 'Ancestor';                                    // do not localise
  ID_SOAP_ITI_XML_NODE_METHOD = 'Method';                                        // do not localise
  ID_SOAP_ITI_XML_NODE_CALLINGCONVENTION = 'CallingConvention';                  // do not localise
  ID_SOAP_ITI_XML_NODE_METHODKIND = 'MethodKind';                                // do not localise
  ID_SOAP_ITI_XML_NODE_METHODSESSION = 'Sessional';                              // do not localise
  ID_SOAP_ITI_XML_NODE_METHODSESSION2 = 'Session2';                              // do not localise
  ID_SOAP_ITI_XML_NODE_RESULTTYPE = 'ResultType';                                // do not localise
  ID_SOAP_ITI_XML_NODE_PARAMETER = 'Parameter';                                  // do not localise
  ID_SOAP_ITI_XML_NODE_PARAMFLAG = 'ParamFlag';                                  // do not localise
  ID_SOAP_ITI_XML_NODE_NAMEOFTYPE = 'NameOfType';                                // do not localise
  ID_SOAP_ITI_XML_NODE_VERSION = 'Version';                                      // do not localise
  ID_SOAP_ITI_XML_NODE_ITI = 'ITI';                                              // do not localise
  ID_SOAP_ITI_XML_NODE_INTERFACE = 'Interface';                                  // do not localise
  ID_SOAP_ITI_XML_NODE_DOCUMENTATION = 'Documentation';                          // do not localise
  ID_SOAP_ITI_XML_NODE_REQUEST_NAME = 'RequestMsgName';                          // do not localise
  ID_SOAP_ITI_XML_NODE_RESPONSE_NAME = 'ResponseMsgName';                        // do not localise
  ID_SOAP_ITI_XML_NODE_NAMESPACE = 'Namespace';                                  // do not localise
  ID_SOAP_ITI_XML_NODE_SOAPACTION = 'SoapAction';                                // do not localise
  ID_SOAP_ITI_XML_NODE_SOAPOPTYPE = 'SoapOpType';                                // do not localise
  ID_SOAP_ITI_XML_NODE_INHERITED_METHOD = 'InheritedMethod';                     // do not localise
  ID_SOAP_ITI_XML_NODE_IS_INHERITED = 'IsInherited';                             // do not localise
  ID_SOAP_ITI_XML_NODE_HEADER = 'Header';                                        // do not localise
  ID_SOAP_ITI_XML_NODE_RESPHEADER = 'Respheader';                                // do not localise
  ID_SOAP_ITI_XML_NODE_ATTACHMENTTYPE = 'AttachmentType';                        // do not localise
  ID_SOAP_ITI_XML_NODE_ENCODINGOVERRIDE = 'EncodingOverride';                    // do not localise
  ID_SOAP_ITI_XML_NODE_CATEGORY = 'Category';                                    // do not localise
  ID_SOAP_ITI_XML_NODE_VISIBILITY = 'Visibility';                                // do not localise
  ID_SOAP_ITI_XML_NODE_COMINITMODE = 'ComInitMode';                              // do not localise
  ID_SOAP_ITI_XML_NODE_MANDATORY = 'Mandatory';                                  // do not localise
  // XML encoding support:

  ID_SOAP_DEFAULT_NAMESPACE_CODE = 'ns';                                         // do not localise
  ID_SOAP_DS_DEFAULT_ROOT = 'urn:nevrona.com/indysoap/v1/';                      // do not localise

  ID_SOAP_NS_SOAPENV = 'http://schemas.xmlsoap.org/soap/envelope/';              // do not localise
  ID_SOAP_NS_SOAPENC = 'http://schemas.xmlsoap.org/soap/encoding/';              // do not localise
  ID_SOAP_NS_SCHEMA_1999 = 'http://www.w3.org/1999/XMLSchema';                   // do not localise
  ID_SOAP_NS_SCHEMA_INST_1999 = 'http://www.w3.org/1999/XMLSchema-instance';     // do not localise
  ID_SOAP_NS_SCHEMA_2001 = 'http://www.w3.org/2001/XMLSchema';                   // do not localise
  ID_SOAP_NS_SCHEMA_INST_2001 = 'http://www.w3.org/2001/XMLSchema-instance';     // do not localise
  ID_SOAP_NS_WSDL_SOAP = 'http://schemas.xmlsoap.org/wsdl/soap/';                // do not localise
  ID_SOAP_NS_WSDL_SOAP12 = 'http://schemas.xmlsoap.org/wsdl/soap12/';            // do not localise
  ID_SOAP_NS_SOAP_HTTP = 'http://schemas.xmlsoap.org/soap/http';                 // do not localise
  ID_SOAP_NS_WSDL =  'http://schemas.xmlsoap.org/wsdl/';                         // do not localise
  ID_SOAP_NS_WSDL_DIME = 'http://schemas.xmlsoap.org/ws/2002/04/dime/wsdl/';     // do not localise
  ID_SOAP_NS_WSDL_MIME = 'http://schemas.xmlsoap.org/wsdl/mime/';                // do not localise
  ID_SOAP_NS_XML_CORE = 'http://www.w3.org/XML/1998/namespace';
  ID_SOAP_NS_SOAPENV_CODE = 'soap';                                              // do not localise
  ID_SOAP_NS_SOAPENC_CODE = 'soap-enc';                                          // do not localise
  ID_SOAP_NS_SCHEMA_CODE = 'xsd';                                                // do not localise
  ID_SOAP_NS_SCHEMA_INST_CODE = 'xsi';                                           // do not localise
  ID_SOAP_NS_WSDL_SOAP_CODE = 'soap';                                            // do not localise
  ID_SOAP_NS_WSDL_DIME_CODE = 'dime';                                            // do not localise
  ID_SOAP_NS_WSDL_MIME_CODE = 'mime';                                            // do not localise
  ID_SOAP_NS_WSDL_CODE = 'wsdl';                                                 // do not localise

  ID_SOAP_NS_SCHEMA = ID_SOAP_NS_SCHEMA_2001;                                    // do not localise
  ID_SOAP_NS_SCHEMA_INST = ID_SOAP_NS_SCHEMA_INST_2001;                          // do not localise

  ID_SOAP_XML_LANG = 'lang';

  ID_SOAP_SCHEMA_INCLUDE = 'include';
  ID_SOAP_SCHEMA_QNAME = 'QName';
  ID_SOAP_SCHEMA_MINOCCURS = 'minOccurs';                                        // do not localise
  ID_SOAP_SCHEMA_MAXOCCURS = 'maxOccurs';                                        // do not localise
  ID_SOAP_SCHEMA_IMPORT = 'import';                                              // do not localise
  ID_SOAP_SCHEMA_NAMESPACE = 'namespace';                                        // do not localise
  ID_SOAP_SCHEMA_REF = 'ref';                                                    // do not localise
  ID_SOAP_SCHEMA_ELEMENT = 'element';                                            // do not localise
  ID_SOAP_SCHEMA_LOCATION = 'schemaLocation';
  ID_SOAP_SCHEMA_SCHEMA = 'schema';
  ID_SOAP_SCHEMA_TARGETNS = 'targetNamespace';
  ID_SOAP_SCHEMA_ATTRFORMDEF = 'attributeFormDefault';
  ID_SOAP_SCHEMA_ELEMFORMDEF = 'elementFormDefault';
  ID_SOAP_SCHEMA_ID = 'id';
  ID_SOAP_SCHEMA_VERSION = 'version';
  ID_SOAP_SCHEMA_ATTRIBUTE = 'attribute';
  ID_SOAP_SCHEMA_DEFAULT = 'default';
  ID_SOAP_SCHEMA_FIXED = 'fixed';
  ID_SOAP_SCHEMA_FORM = 'form';
  ID_SOAP_SCHEMA_NAME = 'name';
  ID_SOAP_SCHEMA_TYPE = 'type';
  ID_SOAP_SCHEMA_USE = 'use';
  ID_SOAP_SCHEMA_SUBSTGROUP = 'substitutionGroup';
  ID_SOAP_SCHEMA_ABSTRACT = 'abstract';
  ID_SOAP_SCHEMA_NILLABLE = 'nillable';
  ID_SOAP_SCHEMA_MIXED = 'mixed';
  ID_SOAP_SCHEMA_COMPLEXTYPE = 'complexType';
  ID_SOAP_SCHEMA_SIMPLETYPE = 'simpleType';
  ID_SOAP_SCHEMA_LIST = 'list';
  ID_SOAP_SCHEMA_ITEMTYPE = 'itemType';
  ID_SOAP_SCHEMA_COMPLEXCONTENT = 'complexContent';
  ID_SOAP_SCHEMA_SIMPLECONTENT = 'simpleContent';
  ID_SOAP_SCHEMA_ALL = 'all';
  ID_SOAP_SCHEMA_CHOICE = 'choice';
  ID_SOAP_SCHEMA_SEQUENCE = 'sequence';
  ID_SOAP_SCHEMA_ATTRGROUP = 'attributeGroup';
  ID_SOAP_SCHEMA_EXTENSION = 'extension';
  ID_SOAP_SCHEMA_RESTRICTION = 'restriction';
  ID_SOAP_SCHEMA_BASE = 'base';
  ID_SOAP_SCHEMA_ENUMERATION = 'enumeration';
  ID_SOAP_SCHEMA_VALUE = 'value';
  ID_SOAP_SCHEMA_ANNOTATION = 'annotation';
  ID_SOAP_SCHEMA_APPINFO = 'appinfo';
  ID_SOAP_SCHEMA_PATTERN = 'pattern';

  ID_SOAP_NS_SCHEMATRON = 'http://www.ascc.net/xml/schematron';
  ID_SOAP_SCHEMATRON_PATTERN = 'pattern';
  ID_SOAP_SCHEMATRON_NAME = 'name';
  ID_SOAP_SCHEMATRON_ABSTRACT = 'abstract';
  ID_SOAP_SCHEMATRON_ID = 'id';
  ID_SOAP_SCHEMATRON_REPORT = 'report';
  ID_SOAP_SCHEMATRON_ASSERT = 'assert';
  ID_SOAP_SCHEMATRON_TEST = 'test';
  ID_SOAP_SCHEMATRON_RULE = 'rule';

  ID_SOAP_NAME_ENCODINGSTYLE = 'encodingStyle';                                  // do not localise
  ID_SOAP_NAME_MUSTUNDERSTAND = 'mustUnderstand';                                // do not localise
  ID_SOAP_NAME_FAULT = 'Fault';                                                  // do not localise
  ID_SOAP_NAME_FAULTCODE = 'faultcode';                                          // do not localise
  ID_SOAP_NAME_FAULTACTOR = 'faultactor';                                        // do not localise
  ID_SOAP_NAME_FAULTSTRING = 'faultstring';                                      // do not localise
  ID_SOAP_NAME_FAULTDETAIL = 'detail';                                           // do not localise
  ID_SOAP_NAME_SCHEMA_TYPE = 'type';                                             // do not localise
  ID_SOAP_NAME_ENV = 'Envelope';                                                 // do not localise
  ID_SOAP_NAME_BODY = 'Body';                                                    // do not localise
  ID_SOAP_NAME_HEADER = 'Header';                                                // do not localise
  ID_SOAP_NAME_XML_ID = 'id';                                                    // do not localise
  ID_SOAP_NAME_XML_HREF = 'href';                                                // do not localise
  ID_SOAP_NAME_XML_XMLNS = 'xmlns';                                              // do not localise
  ID_SOAP_NAME_SCHEMA_POSITION = 'position';                                     // do not localise
  ID_SOAP_NAME_SCHEMA_OFFSET = 'offset';                                         // do not localise
  ID_SOAP_NAME_SCHEMA_ITEM = 'item';                                             // do not localise

  ID_SOAP_WSDL_OPEN = '##any';

  ID_SOAP_CHARSET_8 = 'charset=utf-8';
  ID_SOAP_CHARSET_16 = 'charset=utf-16';

  // SOAP NAMES OF SIGNIFICANCE
  ID_SOAP_NAME_RESULT = 'return';
  ID_SOAP_NULL_TYPE = 'NULL'; // this is used for the class type for an null and unnamed class reading a SOAP packet

  // SOAP NAMES OF INSIGNIFICANCE
  ID_SOAP_NULL_NODE_NAME = 'Root';
  ID_SOAP_NULL_NODE_TYPE = 'Null';
  ID_SOAP_NAME_REF_TYPE = '#ref'; // this is the arbitrary type assigned to a reference node

  // Schema Types
  ID_SOAP_XSI_TYPE_ANY = 'anyType';                                              // do not localise
  ID_SOAP_XSI_TYPE_STRING = 'string';                                            // do not localise
  ID_SOAP_XSI_TYPE_INTEGER = 'int';                                              // do not localise
  ID_SOAP_XSI_TYPE_BOOLEAN = 'boolean';                                          // do not localise
  ID_SOAP_XSI_TYPE_BYTE = 'unsignedByte';                                        // do not localise
  ID_SOAP_XSI_TYPE_CARDINAL = 'unsignedInt';                                     // do not localise
  ID_SOAP_XSI_TYPE_UNSIGNEDLONG = 'unsignedLong';                                // do not localise
  ID_SOAP_XSI_TYPE_COMP = 'long';                                                // do not localise
  ID_SOAP_XSI_TYPE_CURRENCY = 'decimal';                                         // do not localise
  ID_SOAP_XSI_TYPE_DATETIME = 'dateTime';                                        // do not localise
  ID_SOAP_XSI_TYPE_DURATION = 'duration';                                        // do not localise
  ID_SOAP_XSI_TYPE_TIMEINSTANT = 'timeInstant';{from 1999 schema but we allow it}// do not localise
  ID_SOAP_XSI_TYPE_DATE = 'date';                                                // do not localise
  ID_SOAP_XSI_TYPE_TIME = 'time';                                                // do not localise
  ID_SOAP_XSI_TYPE_DOUBLE = 'double';                                            // do not localise
  ID_SOAP_XSI_TYPE_EXTENDED = 'double';                                          // do not localise
  ID_SOAP_XSI_TYPE_INT64 = 'long';                                               // do not localise
  ID_SOAP_XSI_TYPE_SHORTINT = 'byte';                                            // do not localise
  ID_SOAP_XSI_TYPE_SINGLE = 'float';                                             // do not localise
  ID_SOAP_XSI_TYPE_SMALLINT = 'short';                                           // do not localise
  ID_SOAP_XSI_TYPE_WORD = 'unsignedShort';                                       // do not localise
  ID_SOAP_XSI_TYPE_BASE64BINARY = 'base64Binary';                                // do not localise
  ID_SOAP_SOAP_TYPE_BASE64BINARY = 'base64';     {Apache error}                  // do not localise
  ID_SOAP_XSI_TYPE_HEXBINARY = 'hexBinary';                                      // do not localise
  ID_SOAP_SOAPENC_ARRAY = 'Array';                                               // do not localise
  ID_SOAP_SOAPENC_ARRAYTYPE = 'arrayType';                                       // do not localise
  ID_SOAP_XSI_TYPE_QNAME = 'QName';                                              // do not localise
  ID_SOAP_XSI_ATTR_NIL = 'nil';                                                  // do not localise
  ID_SOAP_XSI_ATTR_NULL = 'null';                                                // do not localise
  ID_SOAP_XSI_ATTR_NILLABLE = 'nillable';                                        // do not localise
  ID_SOAP_XSI_TYPE_ANYURI = 'anyURI';                                            // do not localise
  ID_SOAP_XSI_TYPE_ID = 'ID';                                                    // do not localise
  ID_SOAP_XSI_TYPE_IDREF = 'IDREF';                                              // do not localise

  // HTTP RPC settings                                                           // do not localise
  ID_SOAP_DEFAULT_SOAP_PATH = '/soap';                                          // do not localise
  ID_SOAP_HTTP_ACTION_HEADER = 'SOAPAction';                                     // do not localise
// V1.2 ID_SOAP_HTTP_SOAP_TYPE = 'application/soap';                             // do not localise
  ID_SOAP_HTTP_SOAP_TYPE = 'text/xml';                                           // do not localise
  ID_SOAP_HTTP_BIN_TYPE = 'application/Octet-Stream';                            // do not localise
  ID_SOAP_HTTP_DIME_TYPE = 'application/dime';                                   // do not localise
  ID_SOAP_HTTP_PARAMS_TYPE = 'application/x-www-form-urlencoded';                // do not localise

  ID_SOAP_DEFAULT_WSDL_PATH = '/wsdl';                                           // do not localise

  ID_SOAP_HTTP_DEFLATE = 'deflate';

  // TCPIP Communications constants
  ID_SOAP_TCPIP_MAGIC_REQUEST : Longint = $49445351; // IDSQ
  ID_SOAP_TCPIP_MAGIC_RESPONSE : Longint = $49445341; // IDSA
  ID_SOAP_TCPIP_MAGIC_FOOTER : Longint = $49445345; // IDSE
  ID_SOAP_MAX_MIMETYPE_LENGTH = 100;              // not allowed to have interface names longer than this in requests (DoS protection)
  ID_SOAP_MAX_PACKET_LENGTH = 100 * 1024 * 1024;  // not allowed to have interface names longer than 100MB in requests (DoS protection) (should be long enough! - needs more work)
  ID_SOAP_TCPIP_TIMEOUT = 60000;


  // client Interface Handling Settings
  ID_SOAP_BUFFER_SIZE = 8192;          // size of SOAP stub buffer (keep it large for efficient allocs)
  ID_SOAP_MAX_STUB_BUFFER_SIZE = 20;   // max size of a SOAP stub
  ID_SOAP_MAX_STRING_PARAMS = 50;      // max num of string OR widestring params allowed in a single method.
  ID_SOAP_INVALID = '|||';
  
  // you may have up to IDSOAP_MAX_STRING_PARAMS AnsiStrings AND
  //                    IDSOAP_MAX_STRING_PARAMS WideStrings but no more
  // This option uses IDSOAP_MAX_STRING_PARAMS * 8 bytes only during


  // server Interface Handling Settings
  ID_SOAP_INIT_MEM_VALUE = #0;        // what to initialize mem to (generally for the OUT param type). Leave this at 0 - anything else will have fatal consequences
  {$IFNDEF CLR}
  ID_SOAP_INVALID_POINTER = pointer($fffffffe);   // just to prevent use of invalid pointers
  {$ENDIF}

  // Binary Stream Constants
  ID_SOAP_BIN_MAGIC = $10EA8F0A;

  ID_SOAP_BIN_PACKET_EXCEPTION = 1;
  ID_SOAP_BIN_PACKET_MESSAGE = 2;

  ID_SOAP_BIN_NODE_STRUCT = 1;
  ID_SOAP_BIN_NODE_ARRAY = 2;
  ID_SOAP_BIN_NODE_REFERENCE = 3;

  ID_SOAP_BIN_NOTVALID = 0;
  ID_SOAP_BIN_TYPE_PARAM = 1;
  ID_SOAP_BIN_TYPE_NODE = 2;

  ID_SOAP_BIN_CLASS_NIL = 0;
  ID_SOAP_BIN_CLASS_NOT_NIL = 1;

  ID_SOAP_BIN_TYPE_BOOLEAN = 2;
  ID_SOAP_BIN_TYPE_BYTE = 3;
  ID_SOAP_BIN_TYPE_CARDINAL = 4;
  ID_SOAP_BIN_TYPE_CHAR = 5;
  ID_SOAP_BIN_TYPE_COMP = 6;
  ID_SOAP_BIN_TYPE_CURRENCY = 7;
  ID_SOAP_BIN_TYPE_DOUBLE = 8;
  ID_SOAP_BIN_TYPE_ENUM = 9;
  ID_SOAP_BIN_TYPE_EXTENDED = 10;
  ID_SOAP_BIN_TYPE_INT64 = 11;
  ID_SOAP_BIN_TYPE_INTEGER = 12;
  ID_SOAP_BIN_TYPE_SHORTINT = 13;
  ID_SOAP_BIN_TYPE_SHORTSTRING = 14;
  ID_SOAP_BIN_TYPE_SINGLE = 15;
  ID_SOAP_BIN_TYPE_SMALLINT = 16;
  ID_SOAP_BIN_TYPE_STRING = 17;
  ID_SOAP_BIN_TYPE_WIDECHAR = 18;
  ID_SOAP_BIN_TYPE_WIDESTRING = 19;
  ID_SOAP_BIN_TYPE_WORD = 20;
  ID_SOAP_BIN_TYPE_SET = 21;
  ID_SOAP_BIN_TYPE_BINARY = 22;
  ID_SOAP_BIN_TYPE_DATETIME = 23;
  ID_SOAP_BIN_TYPE_DATETIME_NULL = 24;
  ID_SOAP_BIN_TYPE_GENERAL = 25;
  ID_SOAP_BIN_TYPE_XML = 26;
  ID_SOAP_BIN_TYPE_ANSISTRING = 27;

  ID_SOAP_WSDL_SUFFIX_SERVICE = 'Service';
  ID_SOAP_WSDL_SUFFIX_PORT = 'Port';
  ID_SOAP_WSDL_SUFFIX_BINDING = 'Binding';

  ID_SOAP_WSDL_DIME_CLOSED = 'http://schemas.xmlsoap.org/ws/2002/04/dime/closed-layout';
  ID_SOAP_CONTENT_TYPE = 'Content-Type';
  ID_SOAP_CONTENT_DISPOSITION = 'Content-Disposition';
  ID_SOAP_MULTIPART_RELATED : AnsiString = 'multipart/related';
  ID_SOAP_MULTIPART_FORMDATA : AnsiString = 'multipart/form-data';
  ID_SOAP_MIME_BOUNDARY : AnsiString = 'boundary';
  ID_SOAP_MIME_START : AnsiString = 'start';
  ID_SOAP_MIME_TYPE : AnsiString = 'type';
  ID_SOAP_MIME_ID = 'Content-ID';
  ID_SOAP_MIME_TRANSFERENCODING = 'Content-Transfer-Encoding';
  ID_SOAP_MIME_DEFAULT_START = 'uuid:{FF461456-FE30-4933-9AF6-F8EB226E1BF7}';
  ID_SOAP_MIME_DEFAULT_BOUNDARY = 'MIME_boundary';


Implementation

End.
