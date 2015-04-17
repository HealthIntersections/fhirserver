{
IndySOAP: Resource strings
}

unit IdSoapResourceStrings;

{$I IdSoapDefines.inc}

interface

resourceString

  // ERR_ means an error
  // OP_ means a description of what it being done
  // MSG_ means a message for the user/programmer
  // NAME_ means a name of something


  RS_NAME_INDYSOAP = 'IndySoap';

  RS_ERR_ENGINE_UNKNOWN_TYPE = 'Unknown / unsupported Type %s';
  RS_ERR_ENGINE_PARAM_TYPE_WRONG = 'The Parameter %s has an unknown or unsupprted Type %s';
  RS_ERR_ENGINE_UNREG_TYPE = 'The Type %s is not registered in the IndySoap Type Registry';
  RS_ERR_ENGINE_TYPE_NS_MISSING = 'Attempt to determine Namespace for %s "%s" with no manually provided Namespace. A Namespace must be provided in the code';
  RS_ERR_ENGINE_BAD_ENUM_TYPE = 'IndySoap does not handle the Enumeration %s due to irregular values';
  RS_ERR_ENGINE_ENUM_OUT_RANGE = 'The Value %s is out of range for the type %s';
  RS_ERR_ENGINE_NOT_ENUM_TYPE = 'The Type %s is not an enumerated type';
  RS_ERR_ENGINE_ENUM_UNIT_WRONG = 'RTTI Error, Unit for Enumeration "%s" is %s';

  RS_ERR_CLIENT_WRONG_MESSAGE = 'Message Name is wrong. Expected/Received = ';
  RS_ERR_CLIENT_UNKNOWN_ENCODING_TYPE = 'Encoding Type could not be determined';
  RS_ERR_CLIENT_NOT_ACTIVE = 'Client is not Active';
  RS_ERR_CLIENT_MIMETYPE = 'The content type received from the server is not supported';

  RS_ERR_SERVER_UNKNOWN_ENCODING_TYPE = 'Encoding Type could not be determined';
  RS_ERR_SERVER_NO_HANDLER = 'No Handler for the Soap Request "%s" in namespace "%s"';
  RS_ERR_SERVER_SESSION_REQUIRED = 'The Soap Request "%s" in namespace "%s" requires a valid session but none was found';
  RS_ERR_SERVER_NO_IMPL = 'No Implementation for the Soap Request "%s" in namespace "%s"';
  RS_ERR_SERVER_NO_METHOD = 'No Servicing Method for the Soap Request "%s" in namespace "%s"';
  RS_ERR_SERVER_BAD_MIMETYPE_SETTING = 'You cannot set the MimeType list to an empty list';
  RS_ERR_SERVER_BAD_CHARTYPE = 'The Character encoding Type "%s" is not supported by IndySoap';
  RS_ERR_SERVER_BAD_MIMETYPE = 'The Mime Type "%s" is not supported by IndySoap';

  RS_ERR_WININET_NO_DLL = 'Unable to Load WinInet DLL';
  RS_ERR_WININET_NO_ROUTINE = 'Unable to find WinInet Entry Point';
  RS_OP_WININET_QUERY = 'WinInet Request Status Query';
  RS_OP_WININET_REQ_OPEN = 'Open Request';
  RS_OP_WININET_REQ_SEND = 'Send Request';
  RS_OP_WININET_READ = 'Read Request Result';
  RS_OP_WININET_CONNECT = 'Connect';

  RS_ERR_DEBUG_LEAKING_OBJECTS = 'Leaking Objects';
  RS_OP_DEBUG_OBJECT_TRACKING = 'Object Tracking';
  RS_MSG_DEBUG_OBJECT_TRACKING_NOT_FOUND = 'No Live Objects found';

  RS_ERR_DATE_INVALID = 'is not a valid Date or Time ';
  RS_ERR_DATE_TOO_SHORT = 'Date is too short';
  RS_ERR_DATE_YEAR_LENGTH = 'Year length could not be determined';
  RS_ERR_DATE_INVALID_YEAR = 'Year is not valid';
  RS_ERR_DATE_INVALID_MONTH = 'Month is not valid';
  RS_ERR_DATE_INVALID_DAY = 'Day is not valid';
  RS_ERR_DATE_INVALID_SEPARATOR = 'Date/Time separator "T" not found';
  RS_ERR_TIME_TOO_SHORT = 'Time is too short';
  RS_ERR_TIME_INVALID_TIME = 'Time is not valid';
  RS_ERR_TIME_INVALID_HOUR = 'Hour is not valid';
  RS_ERR_TIME_INVALID_MIN = 'Minutes is not valid';
  RS_ERR_TIME_INVALID_SEC = 'Seconds is not valid';
  RS_ERR_TIME_INVALID_NSEC = 'Seconds fraction is not valid';
  RS_ERR_TIMEZONE_TOO_SHORT = 'Timezone is too short';
  RS_ERR_TIMEZONE_INVALID = 'Timezone is invalid';
  RS_ERR_TIMEZONE_INVALID_HOUR = 'Timezone Hour is not valid';
  RS_ERR_TIMEZONE_INVALID_MIN = 'Timezone Minutes is not valid';
  RS_ERR_DATE_INVALID_CHAR_END = 'Invalid Character expecting End of Date';

  RS_ERR_ITI_WRONG_VERSION = 'ITI has the wrong version. Expected / Received: ';
  RS_ERR_ITI_GETFILE_MISSING = 'OnGetITIFileName not provided';
  RS_ERR_ITI_FILE_NOT_FOUND = 'File could not be found';
  RS_ERR_ITI_GETSTREAM_MISSING  = 'OnGetITIStream not provided';
  RS_ERR_ITI_GETSTREAM_FAILED  = 'OnGetITIStream did not return a valid stream';
  RS_ERR_ITI_BAD_SOURCE = 'ITI Source unknown';

  RS_MSG_SERVER_ERROR = 'Server';

  RS_NAME_SOAP_ERROR_SOURCE = 'Server'; // identification of error source when a SOAP fault is returned from the server

  RS_NAME_SOAP_PARAMETER = 'Parameter ';
  RS_ERR_SOAP_ENCODINGSTYLE = 'Unknown SOAP encoding type';
//  RS_ERR_SOAP_MISSING_NAMESPACE = 'No Name Space defined For the Namespace';
  RS_ERR_SOAP_UNRESOLVABLE_NAMESPACE = 'The Namespace %s in the entity %s could not be resolved';
  RS_ERR_SOAP_MISSING_METHOD_NAMESPACE = 'No Name Space defined For the Method';
  RS_ERR_SOAP_MISSING_BODY = 'No Body Entity Found decoding the SOAP message';
  RS_ERR_SOAP_MISSING_CONTENTS = 'Nothing found in the SOAP body decoding the SOAP message';
  RS_ERR_SOAP_MISSING_TYPE = 'No type was provided for the element';
  RS_ERR_SOAP_NAMESPACE_MISMATCH = 'Namespace mismatch for the element "%s" of type "%s", Expected %s, received %s';
  RS_ERR_SOAP_ARRAY_MISSING = 'Array "%s" not found in list "%s"';
  RS_ERR_SOAP_ARRAY_DIM_MISSING = 'An Array dimension was blank';
  RS_ERR_SOAP_ARRAY_DIM_MISMATCH1 = 'Array Offset "%s" dimensions do not match Array Base dimensions (%s)';
  RS_ERR_SOAP_ARRAY_DIM_MISMATCH2 = 'Error reading Array %s, Position "%s" has the wrong number of dimensions';
  RS_ERR_SOAP_ARRAY_DIM_MISMATCH3 = 'Error reading Array %s, Position "%s" is out of range';
  RS_ERR_SOAP_ARRAY_DIM_MISMATCH4 = 'Error reading Array %s, Element is out of range';
  RS_ERR_SOAP_ARRAY_ORDER_REQ = 'Cannot accept unorder items into an Array once an positioned item has been found';
  RS_ERR_SOAP_ARRAY_NOT_ARRAY = 'Element "%s" is not an array (%s)';
  RS_ERR_SOAP_ARRAY_ITEMS_MISSING = 'Error reading Array "%s", not all items were found';
  RS_ERR_SOAP_PARAM_MISSING = 'Parameter "%s" not found in list "%s"';
  RS_ERR_SOAP_TYPE_MISMATCH_NS = 'Type Mismatch. For the parameter %s, the type found was "%s" in the namespace "%s", which doesn''t match any of the expected values: %s';
  RS_ERR_SOAP_TYPE_MISMATCH = 'Type Mismatch. For the parameter %s, the Type "%s" was expected but the type found was "%s"';
  RS_ERR_SOAP_TYPE_NS_MISMATCH = 'Type Namespace Mismatch. For the parameter %s, the Type Namespace "%s" was expected but the type namespace found was "%s"';
  RS_ERR_SOAP_TYPE_NOT_BOOLEAN = 'Type Mismatch. For the Parameter "%s", the value "%s" is not a valid Boolean value';
  RS_ERR_SOAP_TYPE_NOT_CHAR = 'Type Mismatch. For the Parameter "%s", the value "%s" is not a valid Char value';
  RS_ERR_SOAP_TYPE_NOT_WIDECHAR = 'Type Mismatch. For the Parameter "%s", the value "%s" is not a valid Widechar value';
  RS_ERR_SOAP_TYPE_NOT_CURRENCY = 'Type Mismatch. For the Parameter "%s", the value "%s" is not a valid Currency value';
  RS_ERR_SOAP_TYPE_NOT_SHORTSTRING = 'Type Mismatch. For the Parameter "%s", the value "%s" is not a valid ShortString (too long)';
  RS_ERR_SOAP_TYPE_NOT_SET = 'Type Mismatch. For the Parameter "%s", the value "%s" is not a valid ShortString (empty)';
  RS_ERR_SOAP_NS_CODE_NOT_DEFINED = 'The Namespace for the abbreviation "%s" could not be found in the list of known namespaces (%s)';
  RS_ERR_SOAP_REFERENCE_MISSING = 'Reference "%s" pointed to an object that was not found (known references: "%s")';
  RS_ERR_SOAP_BAD_CHAR = 'Encoding Problem. Character %s (Value %s) in Parameter %s is not valid (called from %s )';

  RS_ERR_BIN_UNKNOWN_TYPE = 'Object/Parameter Type Flag unknown (%s)';
  RS_ERR_BIN_BAD_PACKET = 'SoapPacket does not appear to encoded in Binary format';
  RS_ERR_BIN_BAD_PACKET_VERSION = 'Binary SoapPacket has the wrong version (was "%s", expected "%s")';

  RS_ERR_WSDL_UNSUPPORTED_TYPE = 'Attempt to describe unsupported type';

  RS_ERR_TCPIP_TOO_LONG = '%s Name Length too long';
  RS_ERR_TCPIP_NO_CONN = 'TCIP Channel "%s" is not connected';


implementation


end.

