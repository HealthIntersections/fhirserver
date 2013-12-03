{ File: OCI.Pas                                                      }
{ Description: ODBC Call-Level Interface                             }
{ Author: Pieter A. Myburgh                                          }
{ Copyright: Korbitec (Pty) Ltd                                      }
{                                                                    }

unit OdbcImplementation;

{! 2 !}
{0.00-000  10 Jul 03 21:45  []       User: Grahame Grieve    File First added to CodeVault}

{.$.ObjExportAll On}

//change by kestral
{$IFNDEF NOT_ODBCDYN}
{$DEFINE ODBCDYN}
{$ENDIF}

interface

uses
  Windows, SysUtils, odbcHeaders;

{$IFDEF ODBCDYN}

type

TSQLAllocHandle = function (
  AHandleType: SQLSMALLINT;
  AInputHandle: SQLHANDLE;
  AOutputHandle: SQLHANDLEPtr): SQLRETURN; stdcall;

TSQLBindCol = function (
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  ATargetType: SQLSMALLINT;
  ATargetValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLBindParameter = function (
  AStatementHandle: SQLHSTMT;
  AParameterNumber: SQLUSMALLINT;
  AInputOutputType: SQLSMALLINT;
  AValueType: SQLSMALLINT;
  AParameterType: SQLSMALLINT;
  AColumnSize: SQLUINTEGER;
  ADecimalDigits: SQLSMALLINT;
  AParameterValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLBrowseConnect = function (
  AConnectionHandle: SQLHDBC;
  AInConnectionString: SQLCHARPtr;
  AStringLength1: SQLSMALLINT;
  AOutConnectionString: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength2: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLBulkOperations = function (
  AStatementHandle: SQLHSTMT;
  AOperation: SQLSMALLINT): SQLRETURN; stdcall;

TSQLCancel = function (
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

TSQLCloseCursor = function (
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

TSQLColAttribute = function (
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  AFieldIdentifier: SQLUSMALLINT;
  ACharacterAttribute: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr;
  ANumericAttribute: SQLPOINTER): SQLRETURN; stdcall;

TSQLColumnPrivileges = function (
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; stdcall;

TSQLColumns = function (
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; stdcall;

TSQLConnect = function (
  AConnectionHandle: SQLHDBC;
  AServerName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  AUserName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AAuthentication: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; stdcall;

TSQLCopyDesc = function (
  ASourceDescHandle: SQLHDESC;
  ATargetDescHandle: SQLHDESC): SQLRETURN; stdcall;

TSQLDataSources = function (
  AEnvironmentHandle: SQLHENV;
  ADirection: SQLUSMALLINT;
  AServerName: SQLCHARPtr;
  ABufferLength1: SQLSMALLINT;
  ANameLength1: SQLSMALLINTPtr;
  ADescription: SQLCHARPtr;
  ABufferLength2: SQLSMALLINT;
  ANameLength2: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLDescribeCol = function (
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  AColumnName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ANameLength: SQLSMALLINTPtr;
  ADataType: SQLSMALLINTPtr;
  AColumnSize: SQLUINTEGERPtr;
  ADecimalDigits: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLDescribeParam = function (
  AStatementHandle: SQLHSTMT;
  AParameterNumber: SQLUSMALLINT;
  ADataType: SQLSMALLINTPtr;
  AParameterSize: SQLUINTEGERPtr;
  ADecimalDigits: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLDisconnect = function (
  AConnectionHandle: SQLHDBC): SQLRETURN; stdcall;

TSQLDriverConnect = function (
  AConnectionHandle: SQLHDBC;
  AWindowHandle: SQLHWND;
  AInConnectionString: SQLCHARPtr;
  AStringLength1: SQLSMALLINT;
  AOutConnectionString: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength2: SQLSMALLINTPtr;
  ADriverCompletion: SQLUSMALLINT): SQLRETURN; stdcall;

TSQLDrivers = function (
  AEnvironmentHandle: SQLHENV;
  ADirection: SQLUSMALLINT;
  ADriverDescription: SQLCHARPtr;
  ABufferLength1: SQLSMALLINT;
  ADescriptionLength: SQLSMALLINTPtr;
  ADriverAttributes: SQLCHARPtr;
  ABufferLength2: SQLSMALLINT;
  AAttributesLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLEndTran = function (
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ACompletionType: SQLSMALLINT): SQLRETURN; stdcall;

TSQLExecDirect = function (
  AStatementHandle: SQLHSTMT;
  AStatementText: SQLCHARPtr;
  ATextLength: SQLINTEGER): SQLRETURN; stdcall;

TSQLExecute = function (
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

TSQLFetch = function (
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

TSQLFetchScroll = function (
  AStatementHandle: SQLHSTMT;
  AFetchOrientation: SQLSMALLINT;
  AFetchOffset: SQLINTEGER): SQLRETURN; stdcall;

TSQLForeignKeys = function (
  AStatementHandle: SQLHSTMT;
  APKCatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  APKSchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  APKTableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AFKCatalogName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT;
  AFKSchemaName: SQLCHARPtr;
  ANameLength5: SQLSMALLINT;
  AFKTableName: SQLCHARPtr;
  ANameLength6: SQLSMALLINT): SQLRETURN; stdcall;

TSQLFreeHandle = function (
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE): SQLRETURN; stdcall;

TSQLFreeStmt = function (
  AStatementHandle: SQLHSTMT;
  AOption: SQLUSMALLINT): SQLRETURN; stdcall;

TSQLGetConnectAttr = function (
  AConnectionHandle: SQLHDBC;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLGetCursorName = function (
  AStatementHandle: SQLHSTMT;
  ACursorName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ANameLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLGetData = function (
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  ATargetType: SQLSMALLINT;
  ATargetValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLGetDescField = function (
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AFieldIdentifier: SQLSMALLINT;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLGetDescRec = function (
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr;
  AType: SQLSMALLINTPtr;
  ASubType: SQLSMALLINTPtr;
  ALength: SQLINTEGERPtr;
  APrecision: SQLSMALLINTPtr;
  AScale: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLGetDiagField = function (
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ARecNumber: SQLSMALLINT;
  ADiagIdentifier: SQLSMALLINT;
  ADiagInfo: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLGetDiagRec = function (
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ARecNumber: SQLSMALLINT;
  ASqlstate: SQLCHARPtr;
  ANativeError: SQLINTEGERPtr;
  AMessageText: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ATextLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLGetEnvAttr = function (
  AEnvironmentHandle: SQLHENV;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLGetFunctions = function (
  AConnectionHandle: SQLHDBC;
  AFunctionId: SQLUSMALLINT;
  ASupported: SQLUSMALLINTPtr): SQLRETURN; stdcall;

TSQLGetInfo = function (
  AConnectionHandle: SQLHDBC;
  AInfoType: SQLUSMALLINT;
  AInfoValue: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLGetStmtAttr = function (
  AStatementHandle: SQLHSTMT;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLGetTypeInfo = function (
  AStatementHandle: SQLHSTMT;
  ADataType: SQLSMALLINT): SQLRETURN; stdcall;

TSQLMoreResults = function (
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

TSQLNativeSql = function (
  AConnectionHandle: SQLHDBC;
  AInStatementText: SQLCHARPtr;
  ATextLength1: SQLINTEGER;
  AOutStatementText: SQLCHARPtr;
  ABufferLength: SQLINTEGER;
  ATextLength2: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLNumParams = function (
  AStatementHandle: SQLHSTMT;
  AParameterCount: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLNumResultCols = function (
  AStatementHandle: SQLHSTMT;
  AColumnCount: SQLSMALLINTPtr): SQLRETURN; stdcall;

TSQLParamData = function (
  AStatementHandle: SQLHSTMT;
  AValue: SQLPOINTERPtr): SQLRETURN; stdcall;

TSQLPrepare = function (
  AStatementHandle: SQLHSTMT;
  AStatementText: SQLCHARPtr;
  ATextLength: SQLINTEGER): SQLRETURN; stdcall;

TSQLPrimaryKeys = function (
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; stdcall;

TSQLProcedureColumns = function (
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AProcName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; stdcall;

TSQLProcedures = function (
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AProcName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; stdcall;

TSQLPutData = function (
  AStatementHandle: SQLHSTMT;
  AData: SQLPOINTER;
  AStrLen_or_Ind: SQLINTEGER): SQLRETURN; stdcall;

TSQLRowCount = function (
  AStatementHandle: SQLHSTMT;
  ARowCount: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLSetConnectAttr = function (
  AConnectionHandle: SQLHDBC;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; stdcall;

TSQLSetCursorName = function (
  AStatementHandle: SQLHSTMT;
  ACursorName: SQLCHARPtr;
  ANameLength: SQLSMALLINT): SQLRETURN; stdcall;

TSQLSetDescField = function (
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AFieldIdentifier: SQLSMALLINT;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER): SQLRETURN; stdcall;

TSQLSetDescRec = function (
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AType: SQLSMALLINT;
  ASubType: SQLSMALLINT;
  ALength: SQLINTEGER;
  APrecision: SQLSMALLINT;
  AScale: SQLSMALLINT;
  AData: SQLPOINTER;
  AStringLength: SQLINTEGERPtr;
  AIndicator: SQLINTEGERPtr): SQLRETURN; stdcall;

TSQLSetEnvAttr = function (
  AEnvironmentHandle: SQLHENV;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; stdcall;

TSQLSetPos = function (
  AStatementHandle: SQLHSTMT;
  ARowNumber: SQLUSMALLINT;
  AOperation: SQLUSMALLINT;
  ALockType: SQLUSMALLINT): SQLRETURN; stdcall;

TSQLSetStmtAttr = function (
  AStatementHandle: SQLHSTMT;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; stdcall;

TSQLSpecialColumns = function (
  AStatementHandle: SQLHSTMT;
  AIdentifierType: SQLUSMALLINT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AScope: SQLUSMALLINT;
  ANullable: SQLUSMALLINT): SQLRETURN; stdcall;

TSQLStatistics = function (
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AUnique: SQLUSMALLINT;
  AReserved: SQLUSMALLINT): SQLRETURN; stdcall;

TSQLTablePrivileges = function (
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; stdcall;

TSQLTables = function (
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  ATableType: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; stdcall;

{ Installer API }

TSQLConfigDataSource = function (
  AWindowParent: SQLHWND;
  ARequest: SQLUSMALLINT;
  ADriver: SQLPOINTER;
  AAttributes: SQLPOINTER): SQLBOOL; stdcall;

TSQLConfigDriver = function (
  AWindowParent: SQLHWND;
  ARequest: SQLUSMALLINT;
  ADriver: SQLPOINTER;
  AArguments: SQLPOINTER;
  AMessage: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AMessageLength: SQLUSMALLINTPtr): SQLBOOL; stdcall;

TSQLCreateDataSource = function (
  AWindowParent: SQLHWND;
  ADataSource: SQLPOINTER): SQLBOOL; stdcall;

TSQLGetConfigMode = function (
  AConfigMode: SQLUSMALLINTPtr): SQLBOOL; stdcall;

TSQLGetInstalledDrivers = function (
  ADescriptions: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  ADescriptionsLength: SQLUSMALLINTPtr): SQLBOOL; stdcall;

TSQLGetPrivateProfileString = function (
  ASection: SQLPOINTER;
  AEntry: SQLPOINTER;
  ADefault: SQLPOINTER;
  AProfileString: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AFilename: SQLPOINTER): SQLINTEGER; stdcall;

TSQLGetTranslator = function (
  AWindowParent: SQLHWND;
  AName: SQLPOINTER;
  ABufferLength1: SQLUSMALLINT;
  ANameLength: SQLUSMALLINTPtr;
  APath: SQLPOINTER;
  ABufferLength2: SQLUSMALLINT;
  APathLength: SQLUSMALLINTPtr;
  AOption: SQLUINTEGERPtr): SQLBOOL; stdcall;

TSQLInstallDriverEx = function (
  ADriver: SQLPOINTER;
  APathIn: SQLPOINTER;
  APathOut: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathOutLength: SQLUSMALLINTPtr;
  ARequest: SQLUSMALLINT;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

TSQLInstallDriverManager = function (
  APath: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathLength: SQLUSMALLINTPtr): SQLBOOL; stdcall;

TSQLInstallerError = function (
  AErrorNumber: SQLUSMALLINT;
  AErrorCode: SQLUINTEGERPtr;
  AErrorMessage: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AErrorMessageLength: SQLUSMALLINTPtr): SQLRETURN; stdcall;

TSQLInstallTranslatorEx = function (
  ATranslator: SQLPOINTER;
  APathIn: SQLPOINTER;
  APathOut: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathOutLength: SQLUSMALLINTPtr;
  ARequest: SQLUSMALLINT;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

TSQLManageDataSources = function (
  AWindowParent: SQLHWND): SQLBOOL; stdcall;

TSQLPostInstallerError = function (
  AErrorCode: SQLUINTEGER;
  AErrorMessage: SQLPOINTER): SQLRETURN; stdcall;

TSQLReadFileDSN = function (
  AFileName: SQLPOINTER;
  AAppName: SQLPOINTER;
  AKeyName: SQLPOINTER;
  AKeyString: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AKeyStringLength: SQLUSMALLINTPtr): SQLBOOL; stdcall;

TSQLRemoveDriver = function (
  ADriver: SQLPOINTER;
  ARemoveDataSources: SQLBOOL;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

TSQLRemoveDriverManager = function (
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

TSQLRemoveDSNFromIni = function (
  ADataSource: SQLPOINTER): SQLBOOL; stdcall;

TSQLRemoveTranslator = function (
  ATranslator: SQLPOINTER;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

TSQLSetConfigMode = function (
  AConfigMode: SQLUSMALLINT): SQLBOOL; stdcall;

TSQLValidDSN = function (
  ADataSource: SQLPOINTER): SQLBOOL; stdcall;

TSQLWriteDSNToIni = function (
  ADataSource: SQLPOINTER;
  ADriver: SQLPOINTER): SQLBOOL; stdcall;

TSQLWriteFileDSN = function (
  AFileName: SQLPOINTER;
  AAppName: SQLPOINTER;
  AKeyName: SQLPOINTER;
  AKeyString: SQLPOINTER): SQLBOOL; stdcall;

TSQLWritePrivateProfileString = function (
  ASection: SQLPOINTER;
  AEntry: SQLPOINTER;
  AProfileString: SQLPOINTER;
  AFilename: SQLPOINTER): SQLBOOL; stdcall;

{$ENDIF}

function SQLAllocHandle(
  AHandleType: SQLSMALLINT;
  AInputHandle: SQLHANDLE;
  AOutputHandle: SQLHANDLEPtr): SQLRETURN; stdcall;

function SQLBindCol(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  ATargetType: SQLSMALLINT;
  ATargetValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLBindParameter(
  AStatementHandle: SQLHSTMT;
  AParameterNumber: SQLUSMALLINT;
  AInputOutputType: SQLSMALLINT;
  AValueType: SQLSMALLINT;
  AParameterType: SQLSMALLINT;
  AColumnSize: SQLUINTEGER;
  ADecimalDigits: SQLSMALLINT;
  AParameterValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLBrowseConnect(
  AConnectionHandle: SQLHDBC;
  AInConnectionString: SQLCHARPtr;
  AStringLength1: SQLSMALLINT;
  AOutConnectionString: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength2: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLBulkOperations(
  AStatementHandle: SQLHSTMT;
  AOperation: SQLSMALLINT): SQLRETURN; stdcall;

function SQLCancel(
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

function SQLCloseCursor(
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

function SQLColAttribute(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  AFieldIdentifier: SQLUSMALLINT;
  ACharacterAttribute: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr;
  ANumericAttribute: SQLPOINTER): SQLRETURN; stdcall;

function SQLColumnPrivileges(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; stdcall;

function SQLColumns(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; stdcall;

function SQLConnect(
  AConnectionHandle: SQLHDBC;
  AServerName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  AUserName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AAuthentication: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; stdcall;

function SQLCopyDesc(
  ASourceDescHandle: SQLHDESC;
  ATargetDescHandle: SQLHDESC): SQLRETURN; stdcall;

function SQLDataSources(
  AEnvironmentHandle: SQLHENV;
  ADirection: SQLUSMALLINT;
  AServerName: SQLCHARPtr;
  ABufferLength1: SQLSMALLINT;
  ANameLength1: SQLSMALLINTPtr;
  ADescription: SQLCHARPtr;
  ABufferLength2: SQLSMALLINT;
  ANameLength2: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLDescribeCol(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  AColumnName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ANameLength: SQLSMALLINTPtr;
  ADataType: SQLSMALLINTPtr;
  AColumnSize: SQLUINTEGERPtr;
  ADecimalDigits: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLDescribeParam(
  AStatementHandle: SQLHSTMT;
  AParameterNumber: SQLUSMALLINT;
  ADataType: SQLSMALLINTPtr;
  AParameterSize: SQLUINTEGERPtr;
  ADecimalDigits: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLDisconnect(
  AConnectionHandle: SQLHDBC): SQLRETURN; stdcall;

function SQLDriverConnect(
  AConnectionHandle: SQLHDBC;
  AWindowHandle: SQLHWND;
  AInConnectionString: SQLCHARPtr;
  AStringLength1: SQLSMALLINT;
  AOutConnectionString: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength2: SQLSMALLINTPtr;
  ADriverCompletion: SQLUSMALLINT): SQLRETURN; stdcall;

function SQLDrivers(
  AEnvironmentHandle: SQLHENV;
  ADirection: SQLUSMALLINT;
  ADriverDescription: SQLCHARPtr;
  ABufferLength1: SQLSMALLINT;
  ADescriptionLength: SQLSMALLINTPtr;
  ADriverAttributes: SQLCHARPtr;
  ABufferLength2: SQLSMALLINT;
  AAttributesLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLEndTran(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ACompletionType: SQLSMALLINT): SQLRETURN; stdcall;

function SQLExecDirect(
  AStatementHandle: SQLHSTMT;
  AStatementText: SQLCHARPtr;
  ATextLength: SQLINTEGER): SQLRETURN; stdcall;

function SQLExecute(
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

function SQLFetch(
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

function SQLFetchScroll(
  AStatementHandle: SQLHSTMT;
  AFetchOrientation: SQLSMALLINT;
  AFetchOffset: SQLINTEGER): SQLRETURN; stdcall;

function SQLForeignKeys(
  AStatementHandle: SQLHSTMT;
  APKCatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  APKSchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  APKTableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AFKCatalogName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT;
  AFKSchemaName: SQLCHARPtr;
  ANameLength5: SQLSMALLINT;
  AFKTableName: SQLCHARPtr;
  ANameLength6: SQLSMALLINT): SQLRETURN; stdcall;

function SQLFreeHandle(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE): SQLRETURN; stdcall;

function SQLFreeStmt(
  AStatementHandle: SQLHSTMT;
  AOption: SQLUSMALLINT): SQLRETURN; stdcall;

function SQLGetConnectAttr(
  AConnectionHandle: SQLHDBC;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLGetCursorName(
  AStatementHandle: SQLHSTMT;
  ACursorName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ANameLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLGetData(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  ATargetType: SQLSMALLINT;
  ATargetValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLGetDescField(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AFieldIdentifier: SQLSMALLINT;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLGetDescRec(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr;
  AType: SQLSMALLINTPtr;
  ASubType: SQLSMALLINTPtr;
  ALength: SQLINTEGERPtr;
  APrecision: SQLSMALLINTPtr;
  AScale: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLGetDiagField(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ARecNumber: SQLSMALLINT;
  ADiagIdentifier: SQLSMALLINT;
  ADiagInfo: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLGetDiagRec(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ARecNumber: SQLSMALLINT;
  ASqlstate: SQLCHARPtr;
  ANativeError: SQLINTEGERPtr;
  AMessageText: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ATextLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLGetEnvAttr(
  AEnvironmentHandle: SQLHENV;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLGetFunctions(
  AConnectionHandle: SQLHDBC;
  AFunctionId: SQLUSMALLINT;
  ASupported: SQLUSMALLINTPtr): SQLRETURN; stdcall;

function SQLGetInfo(
  AConnectionHandle: SQLHDBC;
  AInfoType: SQLUSMALLINT;
  AInfoValue: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLGetStmtAttr(
  AStatementHandle: SQLHSTMT;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLGetTypeInfo(
  AStatementHandle: SQLHSTMT;
  ADataType: SQLSMALLINT): SQLRETURN; stdcall;

function SQLMoreResults(
  AStatementHandle: SQLHSTMT): SQLRETURN; stdcall;

function SQLNativeSql(
  AConnectionHandle: SQLHDBC;
  AInStatementText: SQLCHARPtr;
  ATextLength1: SQLINTEGER;
  AOutStatementText: SQLCHARPtr;
  ABufferLength: SQLINTEGER;
  ATextLength2: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLNumParams(
  AStatementHandle: SQLHSTMT;
  AParameterCount: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLNumResultCols(
  AStatementHandle: SQLHSTMT;
  AColumnCount: SQLSMALLINTPtr): SQLRETURN; stdcall;

function SQLParamData(
  AStatementHandle: SQLHSTMT;
  AValue: SQLPOINTERPtr): SQLRETURN; stdcall;

function SQLPrepare(
  AStatementHandle: SQLHSTMT;
  AStatementText: SQLCHARPtr;
  ATextLength: SQLINTEGER): SQLRETURN; stdcall;

function SQLPrimaryKeys(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; stdcall;

function SQLProcedureColumns(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AProcName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; stdcall;

function SQLProcedures(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AProcName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; stdcall;

function SQLPutData(
  AStatementHandle: SQLHSTMT;
  AData: SQLPOINTER;
  AStrLen_or_Ind: SQLINTEGER): SQLRETURN; stdcall;

function SQLRowCount(
  AStatementHandle: SQLHSTMT;
  ARowCount: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLSetConnectAttr(
  AConnectionHandle: SQLHDBC;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; stdcall;

function SQLSetCursorName(
  AStatementHandle: SQLHSTMT;
  ACursorName: SQLCHARPtr;
  ANameLength: SQLSMALLINT): SQLRETURN; stdcall;

function SQLSetDescField(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AFieldIdentifier: SQLSMALLINT;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER): SQLRETURN; stdcall;

function SQLSetDescRec(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AType: SQLSMALLINT;
  ASubType: SQLSMALLINT;
  ALength: SQLINTEGER;
  APrecision: SQLSMALLINT;
  AScale: SQLSMALLINT;
  AData: SQLPOINTER;
  AStringLength: SQLINTEGERPtr;
  AIndicator: SQLINTEGERPtr): SQLRETURN; stdcall;

function SQLSetEnvAttr(
  AEnvironmentHandle: SQLHENV;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; stdcall;

function SQLSetPos(
  AStatementHandle: SQLHSTMT;
  ARowNumber: SQLUSMALLINT;
  AOperation: SQLUSMALLINT;
  ALockType: SQLUSMALLINT): SQLRETURN; stdcall;

function SQLSetStmtAttr(
  AStatementHandle: SQLHSTMT;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; stdcall;

function SQLSpecialColumns(
  AStatementHandle: SQLHSTMT;
  AIdentifierType: SQLUSMALLINT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AScope: SQLUSMALLINT;
  ANullable: SQLUSMALLINT): SQLRETURN; stdcall;

function SQLStatistics(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AUnique: SQLUSMALLINT;
  AReserved: SQLUSMALLINT): SQLRETURN; stdcall;

function SQLTablePrivileges(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; stdcall;

function SQLTables(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  ATableType: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; stdcall;

{ Installer API }

function SQLConfigDataSource(
  AWindowParent: SQLHWND;
  ARequest: SQLUSMALLINT;
  ADriver: SQLPOINTER;
  AAttributes: SQLPOINTER): SQLBOOL; stdcall;

function SQLConfigDriver(
  AWindowParent: SQLHWND;
  ARequest: SQLUSMALLINT;
  ADriver: SQLPOINTER;
  AArguments: SQLPOINTER;
  AMessage: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AMessageLength: SQLUSMALLINTPtr): SQLBOOL; stdcall;

function SQLCreateDataSource(
  AWindowParent: SQLHWND;
  ADataSource: SQLPOINTER): SQLBOOL; stdcall;

function SQLGetConfigMode(
  AConfigMode: SQLUSMALLINTPtr): SQLBOOL; stdcall;

function SQLGetInstalledDrivers(
  ADescriptions: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  ADescriptionsLength: SQLUSMALLINTPtr): SQLBOOL; stdcall;

function SQLGetPrivateProfileString(
  ASection: SQLPOINTER;
  AEntry: SQLPOINTER;
  ADefault: SQLPOINTER;
  AProfileString: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AFilename: SQLPOINTER): SQLINTEGER; stdcall;

function SQLGetTranslator(
  AWindowParent: SQLHWND;
  AName: SQLPOINTER;
  ABufferLength1: SQLUSMALLINT;
  ANameLength: SQLUSMALLINTPtr;
  APath: SQLPOINTER;
  ABufferLength2: SQLUSMALLINT;
  APathLength: SQLUSMALLINTPtr;
  AOption: SQLUINTEGERPtr): SQLBOOL; stdcall;

function SQLInstallDriverEx(
  ADriver: SQLPOINTER;
  APathIn: SQLPOINTER;
  APathOut: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathOutLength: SQLUSMALLINTPtr;
  ARequest: SQLUSMALLINT;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

function SQLInstallDriverManager(
  APath: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathLength: SQLUSMALLINTPtr): SQLBOOL; stdcall;

function SQLInstallerError(
  AErrorNumber: SQLUSMALLINT;
  AErrorCode: SQLUINTEGERPtr;
  AErrorMessage: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AErrorMessageLength: SQLUSMALLINTPtr): SQLRETURN; stdcall;

function SQLInstallTranslatorEx(
  ATranslator: SQLPOINTER;
  APathIn: SQLPOINTER;
  APathOut: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathOutLength: SQLUSMALLINTPtr;
  ARequest: SQLUSMALLINT;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

function SQLManageDataSources(
  AWindowParent: SQLHWND): SQLBOOL; stdcall;

function SQLPostInstallerError(
  AErrorCode: SQLUINTEGER;
  AErrorMessage: SQLPOINTER): SQLRETURN; stdcall;

function SQLReadFileDSN(
  AFileName: SQLPOINTER;
  AAppName: SQLPOINTER;
  AKeyName: SQLPOINTER;
  AKeyString: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AKeyStringLength: SQLUSMALLINTPtr): SQLBOOL; stdcall;

function SQLRemoveDriver(
  ADriver: SQLPOINTER;
  ARemoveDataSources: SQLBOOL;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

function SQLRemoveDriverManager(
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

function SQLRemoveDSNFromIni(
  ADataSource: SQLPOINTER): SQLBOOL; stdcall;

function SQLRemoveTranslator(
  ATranslator: SQLPOINTER;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; stdcall;

function SQLSetConfigMode(
  AConfigMode: SQLUSMALLINT): SQLBOOL; stdcall;

function SQLValidDSN(
  ADataSource: SQLPOINTER): SQLBOOL; stdcall;

function SQLWriteDSNToIni(
  ADataSource: SQLPOINTER;
  ADriver: SQLPOINTER): SQLBOOL; stdcall;

function SQLWriteFileDSN(
  AFileName: SQLPOINTER;
  AAppName: SQLPOINTER;
  AKeyName: SQLPOINTER;
  AKeyString: SQLPOINTER): SQLBOOL; stdcall;

function SQLWritePrivateProfileString(
  ASection: SQLPOINTER;
  AEntry: SQLPOINTER;
  AProfileString: SQLPOINTER;
  AFilename: SQLPOINTER): SQLBOOL; stdcall;

procedure LoadODBCDLL;
  
implementation

{$IFDEF ODBCDYN}

const
  ErrorLoadingLibrary = 'Error Loading Library: ';
  ErrorLoadingMethod = 'Error Loading Method: ';

var
  hODBCDLL: THandle;
  hODBCINSTDLL: THandle;

  PSQLAllocHandle: TSQLAllocHandle;
  PSQLBindCol: TSQLBindCol;
  PSQLBindParameter: TSQLBindParameter;
  PSQLBrowseConnect: TSQLBrowseConnect;
  PSQLBulkOperations: TSQLBulkOperations;
  PSQLCancel: TSQLCancel;
  PSQLCloseCursor: TSQLCloseCursor;
  PSQLColAttribute: TSQLColAttribute;
  PSQLColumnPrivileges: TSQLColumnPrivileges;
  PSQLColumns: TSQLColumns;
  PSQLConnect: TSQLConnect;
  PSQLCopyDesc: TSQLCopyDesc;
  PSQLDataSources: TSQLDataSources;
  PSQLDescribeCol: TSQLDescribeCol;
  PSQLDescribeParam: TSQLDescribeParam;
  PSQLDisconnect: TSQLDisconnect;
  PSQLDriverConnect: TSQLDriverConnect;
  PSQLDrivers: TSQLDrivers;
  PSQLEndTran: TSQLEndTran;
  PSQLExecDirect: TSQLExecDirect;
  PSQLExecute: TSQLExecute;
  PSQLFetch: TSQLFetch;
  PSQLFetchScroll: TSQLFetchScroll;
  PSQLForeignKeys: TSQLForeignKeys;
  PSQLFreeHandle: TSQLFreeHandle;
  PSQLFreeStmt: TSQLFreeStmt;
  PSQLGetConnectAttr: TSQLGetConnectAttr;
  PSQLGetCursorName: TSQLGetCursorName;
  PSQLGetData: TSQLGetData;
  PSQLGetDescField: TSQLGetDescField;
  PSQLGetDescRec: TSQLGetDescRec;
  PSQLGetDiagField: TSQLGetDiagField;
  PSQLGetDiagRec: TSQLGetDiagRec;
  PSQLGetEnvAttr: TSQLGetEnvAttr;
  PSQLGetFunctions: TSQLGetFunctions;
  PSQLGetInfo: TSQLGetInfo;
  PSQLGetStmtAttr: TSQLGetStmtAttr;
  PSQLGetTypeInfo: TSQLGetTypeInfo;
  PSQLMoreResults: TSQLMoreResults;
  PSQLNativeSql: TSQLNativeSql;
  PSQLNumParams: TSQLNumParams;
  PSQLNumResultCols: TSQLNumResultCols;
  PSQLParamData: TSQLParamData;
  PSQLPrepare: TSQLPrepare;
  PSQLPrimaryKeys: TSQLPrimaryKeys;
  PSQLProcedureColumns: TSQLProcedureColumns;
  PSQLProcedures: TSQLProcedures;
  PSQLPutData: TSQLPutData;
  PSQLRowCount: TSQLRowCount;
  PSQLSetConnectAttr: TSQLSetConnectAttr;
  PSQLSetCursorName: TSQLSetCursorName;
  PSQLSetDescField: TSQLSetDescField;
  PSQLSetDescRec: TSQLSetDescRec;
  PSQLSetEnvAttr: TSQLSetEnvAttr;
  PSQLSetPos: TSQLSetPos;
  PSQLSetStmtAttr: TSQLSetStmtAttr;
  PSQLSpecialColumns: TSQLSpecialColumns;
  PSQLStatistics: TSQLStatistics;
  PSQLTablePrivileges: TSQLTablePrivileges;
  PSQLTables: TSQLTables;

  { Installer API }
  PSQLConfigDataSource: TSQLConfigDataSource;
  PSQLConfigDriver: TSQLConfigDriver;
  PSQLCreateDataSource: TSQLCreateDataSource;
  PSQLGetConfigMode: TSQLGetConfigMode;
  PSQLGetInstalledDrivers: TSQLGetInstalledDrivers;
  PSQLGetPrivateProfileString: TSQLGetPrivateProfileString;
  PSQLGetTranslator: TSQLGetTranslator;
  PSQLInstallDriverEx: TSQLInstallDriverEx;
  PSQLInstallDriverManager: TSQLInstallDriverManager;
  PSQLInstallerError: TSQLInstallerError;
  PSQLInstallTranslatorEx: TSQLInstallTranslatorEx;
  PSQLManageDataSources: TSQLManageDataSources;
  PSQLPostInstallerError: TSQLPostInstallerError;
  PSQLReadFileDSN: TSQLReadFileDSN;
  PSQLRemoveDriver: TSQLRemoveDriver;
  PSQLRemoveDriverManager: TSQLRemoveDriverManager;
  PSQLRemoveDSNFromIni: TSQLRemoveDSNFromIni;
  PSQLRemoveTranslator: TSQLRemoveTranslator;
  PSQLSetConfigMode: TSQLSetConfigMode;
  PSQLValidDSN: TSQLValidDSN;
  PSQLWriteDSNToIni: TSQLWriteDSNToIni;
  PSQLWriteFileDSN: TSQLWriteFileDSN;
  PSQLWritePrivateProfileString: TSQLWritePrivateProfileString;

function GetProcAddressW(hModule: HMODULE; name: String): FARPROC;
begin
  result := GetProcAddress(hModule, pchar(name+'W'));
  if result = nil then
    result := GetProcAddress(hModule, pchar(name));
end;

procedure LoadODBCDLL;
begin
  if hODBCDLL = 0 then
  begin
    hODBCDLL:= LoadLibrary(ODBCDLL);
    if hODBCDLL = 0 then
      raise EODBCExpress.Create(ErrorLoadingLibrary+ODBCDLL);
    ODBCLoaded := true;
    @PSQLAllocHandle:= GetProcAddressW(hODBCDLL, 'SQLAllocHandle');
    @PSQLBindCol:= GetProcAddressW(hODBCDLL, 'SQLBindCol');
    @PSQLBindParameter:= GetProcAddressW(hODBCDLL, 'SQLBindParameter');
    @PSQLBrowseConnect:= GetProcAddressW(hODBCDLL, 'SQLBrowseConnect');
    @PSQLBulkOperations:= GetProcAddressW(hODBCDLL, 'SQLBulkOperations');
    @PSQLCancel:= GetProcAddressW(hODBCDLL, 'SQLCancel');
    @PSQLCloseCursor:= GetProcAddressW(hODBCDLL, 'SQLCloseCursor');
    @PSQLColAttribute:= GetProcAddressW(hODBCDLL, 'SQLColAttribute');
    @PSQLColumnPrivileges:= GetProcAddressW(hODBCDLL, 'SQLColumnPrivileges');
    @PSQLColumns:= GetProcAddressW(hODBCDLL, 'SQLColumns');
    @PSQLConnect:= GetProcAddressW(hODBCDLL, 'SQLConnect');
    @PSQLCopyDesc:= GetProcAddressW(hODBCDLL, 'SQLCopyDesc');
    @PSQLDataSources:= GetProcAddressW(hODBCDLL, 'SQLDataSources');
    @PSQLDescribeCol:= GetProcAddressW(hODBCDLL, 'SQLDescribeCol');
    @PSQLDescribeParam:= GetProcAddressW(hODBCDLL, 'SQLDescribeParam');
    @PSQLDisconnect:= GetProcAddressW(hODBCDLL, 'SQLDisconnect');
    @PSQLDriverConnect:= GetProcAddressW(hODBCDLL, 'SQLDriverConnect');
    @PSQLDrivers:= GetProcAddressW(hODBCDLL, 'SQLDrivers');
    @PSQLEndTran:= GetProcAddressW(hODBCDLL, 'SQLEndTran');
    @PSQLExecDirect:= GetProcAddressW(hODBCDLL, 'SQLExecDirect');
    @PSQLExecute:= GetProcAddressW(hODBCDLL, 'SQLExecute');
    @PSQLFetch:= GetProcAddressW(hODBCDLL, 'SQLFetch');
    @PSQLFetchScroll:= GetProcAddressW(hODBCDLL, 'SQLFetchScroll');
    @PSQLForeignKeys:= GetProcAddressW(hODBCDLL, 'SQLForeignKeys');
    @PSQLFreeHandle:= GetProcAddressW(hODBCDLL, 'SQLFreeHandle');
    @PSQLFreeStmt:= GetProcAddressW(hODBCDLL, 'SQLFreeStmt');
    @PSQLGetConnectAttr:= GetProcAddressW(hODBCDLL, 'SQLGetConnectAttr');
    @PSQLGetCursorName:= GetProcAddressW(hODBCDLL, 'SQLGetCursorName');
    @PSQLGetData:= GetProcAddressW(hODBCDLL, 'SQLGetData');
    @PSQLGetDescField:= GetProcAddressW(hODBCDLL, 'SQLGetDescField');
    @PSQLGetDescRec:= GetProcAddressW(hODBCDLL, 'SQLGetDescRec');
    @PSQLGetDiagField:= GetProcAddressW(hODBCDLL, 'SQLGetDiagField');
    @PSQLGetDiagRec:= GetProcAddressW(hODBCDLL, 'SQLGetDiagRec');
    @PSQLGetEnvAttr:= GetProcAddressW(hODBCDLL, 'SQLGetEnvAttr');
    @PSQLGetFunctions:= GetProcAddressW(hODBCDLL, 'SQLGetFunctions');
    @PSQLGetInfo:= GetProcAddressW(hODBCDLL, 'SQLGetInfo');
    @PSQLGetStmtAttr:= GetProcAddressW(hODBCDLL, 'SQLGetStmtAttr');
    @PSQLGetTypeInfo:= GetProcAddressW(hODBCDLL, 'SQLGetTypeInfo');
    @PSQLMoreResults:= GetProcAddressW(hODBCDLL, 'SQLMoreResults');
    @PSQLNativeSql:= GetProcAddressW(hODBCDLL, 'SQLNativeSql');
    @PSQLNumParams:= GetProcAddressW(hODBCDLL, 'SQLNumParams');
    @PSQLNumResultCols:= GetProcAddressW(hODBCDLL, 'SQLNumResultCols');
    @PSQLParamData:= GetProcAddressW(hODBCDLL, 'SQLParamData');
    @PSQLPrepare:= GetProcAddressW(hODBCDLL, 'SQLPrepare');
    @PSQLPrimaryKeys:= GetProcAddressW(hODBCDLL, 'SQLPrimaryKeys');
    @PSQLProcedureColumns:= GetProcAddressW(hODBCDLL, 'SQLProcedureColumns');
    @PSQLProcedures:= GetProcAddressW(hODBCDLL, 'SQLProcedures');
    @PSQLPutData:= GetProcAddressW(hODBCDLL, 'SQLPutData');
    @PSQLRowCount:= GetProcAddressW(hODBCDLL, 'SQLRowCount');
    @PSQLSetConnectAttr:= GetProcAddressW(hODBCDLL, 'SQLSetConnectAttr');
    @PSQLSetCursorName:= GetProcAddressW(hODBCDLL, 'SQLSetCursorName');
    @PSQLSetDescField:= GetProcAddressW(hODBCDLL, 'SQLSetDescField');
    @PSQLSetDescRec:= GetProcAddressW(hODBCDLL, 'SQLSetDescRec');
    @PSQLSetEnvAttr:= GetProcAddressW(hODBCDLL, 'SQLSetEnvAttr');
    @PSQLSetPos:= GetProcAddressW(hODBCDLL, 'SQLSetPos');
    @PSQLSetStmtAttr:= GetProcAddressW(hODBCDLL, 'SQLSetStmtAttr');
    @PSQLSpecialColumns:= GetProcAddressW(hODBCDLL, 'SQLSpecialColumns');
    @PSQLStatistics:= GetProcAddressW(hODBCDLL, 'SQLStatistics');
    @PSQLTablePrivileges:= GetProcAddressW(hODBCDLL, 'SQLTablePrivileges');
    @PSQLTables:= GetProcAddressW(hODBCDLL, 'SQLTables');
  end;
end;

procedure LoadODBCINSTDLL;
begin
  if hODBCINSTDLL = 0 then
  begin
    hODBCINSTDLL:= LoadLibrary(ODBCINSTDLL);
    if hODBCINSTDLL = 0 then
      raise EODBCExpress.Create(ErrorLoadingLibrary+ODBCINSTDLL);

    @PSQLConfigDataSource:= GetProcAddress(hODBCINSTDLL, 'SQLConfigDataSource');
    @PSQLConfigDriver:= GetProcAddress(hODBCINSTDLL, 'SQLConfigDriver');
    @PSQLCreateDataSource:= GetProcAddress(hODBCINSTDLL, 'SQLCreateDataSource');
    @PSQLGetConfigMode:= GetProcAddress(hODBCINSTDLL, 'SQLGetConfigMode');
    @PSQLGetInstalledDrivers:= GetProcAddress(hODBCINSTDLL, 'SQLGetInstalledDrivers');
    @PSQLGetPrivateProfileString:= GetProcAddress(hODBCINSTDLL, 'SQLGetPrivateProfileString');
    @PSQLGetTranslator:= GetProcAddress(hODBCINSTDLL, 'SQLGetTranslator');
    @PSQLInstallDriverEx:= GetProcAddress(hODBCINSTDLL, 'SQLInstallDriverEx');
    @PSQLInstallDriverManager:= GetProcAddress(hODBCINSTDLL, 'SQLInstallDriverManager');
    @PSQLInstallerError:= GetProcAddress(hODBCINSTDLL, 'SQLInstallerError');
    @PSQLInstallTranslatorEx:= GetProcAddress(hODBCINSTDLL, 'SQLInstallTranslatorEx');
    @PSQLManageDataSources:= GetProcAddress(hODBCINSTDLL, 'SQLManageDataSources');
    @PSQLPostInstallerError:= GetProcAddress(hODBCINSTDLL, 'SQLPostInstallerError');
    @PSQLReadFileDSN:= GetProcAddress(hODBCINSTDLL, 'SQLReadFileDSN');
    @PSQLRemoveDriver:= GetProcAddress(hODBCINSTDLL, 'SQLRemoveDriver');
    @PSQLRemoveDriverManager:= GetProcAddress(hODBCINSTDLL, 'SQLRemoveDriverManager');
    @PSQLRemoveDSNFromIni:= GetProcAddress(hODBCINSTDLL, 'SQLRemoveDSNFromIni');
    @PSQLRemoveTranslator:= GetProcAddress(hODBCINSTDLL, 'SQLRemoveTranslator');
    @PSQLSetConfigMode:= GetProcAddress(hODBCINSTDLL, 'SQLSetConfigMode');
    @PSQLValidDSN:= GetProcAddress(hODBCINSTDLL, 'SQLValidDSN');
    @PSQLWriteDSNToIni:= GetProcAddress(hODBCINSTDLL, 'SQLWriteDSNToIni');
    @PSQLWriteFileDSN:= GetProcAddress(hODBCINSTDLL, 'SQLWriteFileDSN');
    @PSQLWritePrivateProfileString:= GetProcAddress(hODBCINSTDLL, 'SQLWritePrivateProfileString');
  end;
end;

function SQLAllocHandle(
  AHandleType: SQLSMALLINT;
  AInputHandle: SQLHANDLE;
  AOutputHandle: SQLHANDLEPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLAllocHandle <> nil then
    Result:= PSQLAllocHandle(
      AHandleType,
      AInputHandle,
      AOutputHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLBindCol(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  ATargetType: SQLSMALLINT;
  ATargetValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLBindCol <> nil then
    Result:= PSQLBindCol(
      AStatementHandle,
      AColumnNumber,
      ATargetType,
      ATargetValue,
      ABufferLength,
      AStrLen_or_Ind)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLBindParameter(
  AStatementHandle: SQLHSTMT;
  AParameterNumber: SQLUSMALLINT;
  AInputOutputType: SQLSMALLINT;
  AValueType: SQLSMALLINT;
  AParameterType: SQLSMALLINT;
  AColumnSize: SQLUINTEGER;
  ADecimalDigits: SQLSMALLINT;
  AParameterValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLBindParameter <> nil then
    Result:= PSQLBindParameter(
      AStatementHandle,
      AParameterNumber,
      AInputOutputType,
      AValueType,
      AParameterType,
      AColumnSize,
      ADecimalDigits,
      AParameterValue,
      ABufferLength,
      AStrLen_or_Ind)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLBrowseConnect(
  AConnectionHandle: SQLHDBC;
  AInConnectionString: SQLCHARPtr;
  AStringLength1: SQLSMALLINT;
  AOutConnectionString: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength2: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLBrowseConnect <> nil then
    Result:= PSQLBrowseConnect(
      AConnectionHandle,
      AInConnectionString,
      AStringLength1,
      AOutConnectionString,
      ABufferLength,
      AStringLength2)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLBulkOperations(
  AStatementHandle: SQLHSTMT;
  AOperation: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLBulkOperations <> nil then
    Result:= PSQLBulkOperations(
      AStatementHandle,
      AOperation)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLCancel(
  AStatementHandle: SQLHSTMT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLCancel <> nil then
    Result:= PSQLCancel(
      AStatementHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLCloseCursor(
  AStatementHandle: SQLHSTMT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLCloseCursor <> nil then
    Result:= PSQLCloseCursor(
      AStatementHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLColAttribute(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  AFieldIdentifier: SQLUSMALLINT;
  ACharacterAttribute: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr;
  ANumericAttribute: SQLPOINTER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLColAttribute <> nil then
    Result:= PSQLColAttribute(
      AStatementHandle,
      AColumnNumber,
      AFieldIdentifier,
      ACharacterAttribute,
      ABufferLength,
      AStringLength,
      ANumericAttribute)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLColumnPrivileges(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLColumnPrivileges <> nil then
    Result:= PSQLColumnPrivileges(
      AStatementHandle,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      ATableName,
      ANameLength3,
      AColumnName,
      ANameLength4)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLColumns(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLColumns <> nil then
    Result:= PSQLColumns(
      AStatementHandle,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      ATableName,
      ANameLength3,
      AColumnName,
      ANameLength4)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLConnect(
  AConnectionHandle: SQLHDBC;
  AServerName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  AUserName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AAuthentication: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLConnect <> nil then
    Result:= PSQLConnect(
      AConnectionHandle,
      AServerName,
      ANameLength1,
      AUserName,
      ANameLength2,
      AAuthentication,
      ANameLength3)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLCopyDesc(
  ASourceDescHandle: SQLHDESC;
  ATargetDescHandle: SQLHDESC): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLCopyDesc <> nil then
    Result:= PSQLCopyDesc(
      ASourceDescHandle,
      ATargetDescHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLDataSources(
  AEnvironmentHandle: SQLHENV;
  ADirection: SQLUSMALLINT;
  AServerName: SQLCHARPtr;
  ABufferLength1: SQLSMALLINT;
  ANameLength1: SQLSMALLINTPtr;
  ADescription: SQLCHARPtr;
  ABufferLength2: SQLSMALLINT;
  ANameLength2: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLDataSources <> nil then
    Result:= PSQLDataSources(
      AEnvironmentHandle,
      ADirection,
      AServerName,
      ABufferLength1,
      ANameLength1,
      ADescription,
      ABufferLength2,
      ANameLength2)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLDescribeCol(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  AColumnName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ANameLength: SQLSMALLINTPtr;
  ADataType: SQLSMALLINTPtr;
  AColumnSize: SQLUINTEGERPtr;
  ADecimalDigits: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLDescribeCol <> nil then
    Result:= PSQLDescribeCol(
      AStatementHandle,
      AColumnNumber,
      AColumnName,
      ABufferLength,
      ANameLength,
      ADataType,
      AColumnSize,
      ADecimalDigits,
      ANullable)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLDescribeParam(
  AStatementHandle: SQLHSTMT;
  AParameterNumber: SQLUSMALLINT;
  ADataType: SQLSMALLINTPtr;
  AParameterSize: SQLUINTEGERPtr;
  ADecimalDigits: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLDescribeParam <> nil then
    Result:= PSQLDescribeParam(
      AStatementHandle,
      AParameterNumber,
      ADataType,
      AParameterSize,
      ADecimalDigits,
      ANullable)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLDisconnect(
  AConnectionHandle: SQLHDBC): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLDisconnect <> nil then
    Result:= PSQLDisconnect(
      AConnectionHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLDriverConnect(
  AConnectionHandle: SQLHDBC;
  AWindowHandle: SQLHWND;
  AInConnectionString: SQLCHARPtr;
  AStringLength1: SQLSMALLINT;
  AOutConnectionString: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength2: SQLSMALLINTPtr;
  ADriverCompletion: SQLUSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLDriverConnect <> nil then
    Result:= PSQLDriverConnect(
      AConnectionHandle,
      AWindowHandle,
      AInConnectionString,
      AStringLength1,
      AOutConnectionString,
      ABufferLength,
      AStringLength2,
      ADriverCompletion)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLDrivers(
  AEnvironmentHandle: SQLHENV;
  ADirection: SQLUSMALLINT;
  ADriverDescription: SQLCHARPtr;
  ABufferLength1: SQLSMALLINT;
  ADescriptionLength: SQLSMALLINTPtr;
  ADriverAttributes: SQLCHARPtr;
  ABufferLength2: SQLSMALLINT;
  AAttributesLength: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLDrivers <> nil then
    Result:= PSQLDrivers(
      AEnvironmentHandle,
      ADirection,
      ADriverDescription,
      ABufferLength1,
      ADescriptionLength,
      ADriverAttributes,
      ABufferLength2,
      AAttributesLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLEndTran(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ACompletionType: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLEndTran <> nil then
    Result:= PSQLEndTran(
      AHandleType,
      AHandle,
      ACompletionType)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLExecDirect(
  AStatementHandle: SQLHSTMT;
  AStatementText: SQLCHARPtr;
  ATextLength: SQLINTEGER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLExecDirect <> nil then
    Result:= PSQLExecDirect(
      AStatementHandle,
      AStatementText,
      ATextLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLExecute(
  AStatementHandle: SQLHSTMT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLExecute <> nil then
    Result:= PSQLExecute(
      AStatementHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLFetch(
  AStatementHandle: SQLHSTMT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLFetch <> nil then
    Result:= PSQLFetch(
      AStatementHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLFetchScroll(
  AStatementHandle: SQLHSTMT;
  AFetchOrientation: SQLSMALLINT;
  AFetchOffset: SQLINTEGER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLFetchScroll <> nil then
    Result:= PSQLFetchScroll(
      AStatementHandle,
      AFetchOrientation,
      AFetchOffset)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLForeignKeys(
  AStatementHandle: SQLHSTMT;
  APKCatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  APKSchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  APKTableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AFKCatalogName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT;
  AFKSchemaName: SQLCHARPtr;
  ANameLength5: SQLSMALLINT;
  AFKTableName: SQLCHARPtr;
  ANameLength6: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLForeignKeys <> nil then
    Result:= PSQLForeignKeys(
      AStatementHandle,
      APKCatalogName,
      ANameLength1,
      APKSchemaName,
      ANameLength2,
      APKTableName,
      ANameLength3,
      AFKCatalogName,
      ANameLength4,
      AFKSchemaName,
      ANameLength5,
      AFKTableName,
      ANameLength6)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLFreeHandle(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLFreeHandle <> nil then
    Result:= PSQLFreeHandle(
      AHandleType,
      AHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLFreeStmt(
  AStatementHandle: SQLHSTMT;
  AOption: SQLUSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLFreeStmt <> nil then
    Result:= PSQLFreeStmt(
      AStatementHandle,
      AOption)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetConnectAttr(
  AConnectionHandle: SQLHDBC;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetConnectAttr <> nil then
    Result:= PSQLGetConnectAttr(
      AConnectionHandle,
      AAttribute,
      AValue,
      ABufferLength,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetCursorName(
  AStatementHandle: SQLHSTMT;
  ACursorName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ANameLength: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetCursorName <> nil then
    Result:= PSQLGetCursorName(
      AStatementHandle,
      ACursorName,
      ABufferLength,
      ANameLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetData(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  ATargetType: SQLSMALLINT;
  ATargetValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetData <> nil then
    Result:= PSQLGetData(
      AStatementHandle,
      AColumnNumber,
      ATargetType,
      ATargetValue,
      ABufferLength,
      AStrLen_or_Ind)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetDescField(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AFieldIdentifier: SQLSMALLINT;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetDescField <> nil then
    Result:= PSQLGetDescField(
      ADescriptorHandle,
      ARecNumber,
      AFieldIdentifier,
      AValue,
      ABufferLength,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetDescRec(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr;
  AType: SQLSMALLINTPtr;
  ASubType: SQLSMALLINTPtr;
  ALength: SQLINTEGERPtr;
  APrecision: SQLSMALLINTPtr;
  AScale: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetDescRec <> nil then
    Result:= PSQLGetDescRec(
      ADescriptorHandle,
      ARecNumber,
      AName,
      ABufferLength,
      AStringLength,
      AType,
      ASubType,
      ALength,
      APrecision,
      AScale,
      ANullable)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetDiagField(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ARecNumber: SQLSMALLINT;
  ADiagIdentifier: SQLSMALLINT;
  ADiagInfo: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetDiagField <> nil then
    Result:= PSQLGetDiagField(
      AHandleType,
      AHandle,
      ARecNumber,
      ADiagIdentifier,
      ADiagInfo,
      ABufferLength,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetDiagRec(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ARecNumber: SQLSMALLINT;
  ASqlstate: SQLCHARPtr;
  ANativeError: SQLINTEGERPtr;
  AMessageText: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ATextLength: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetDiagRec <> nil then
    Result:= PSQLGetDiagRec(
      AHandleType,
      AHandle,
      ARecNumber,
      ASqlstate,
      ANativeError,
      AMessageText,
      ABufferLength,
      ATextLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetEnvAttr(
  AEnvironmentHandle: SQLHENV;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetEnvAttr <> nil then
    Result:= PSQLGetEnvAttr(
      AEnvironmentHandle,
      AAttribute,
      AValue,
      ABufferLength,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetFunctions(
  AConnectionHandle: SQLHDBC;
  AFunctionId: SQLUSMALLINT;
  ASupported: SQLUSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetFunctions <> nil then
    Result:= PSQLGetFunctions(
      AConnectionHandle,
      AFunctionId,
      ASupported)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetInfo(
  AConnectionHandle: SQLHDBC;
  AInfoType: SQLUSMALLINT;
  AInfoValue: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetInfo <> nil then
    Result:= PSQLGetInfo(
      AConnectionHandle,
      AInfoType,
      AInfoValue,
      ABufferLength,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetStmtAttr(
  AStatementHandle: SQLHSTMT;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetStmtAttr <> nil then
    Result:= PSQLGetStmtAttr(
      AStatementHandle,
      AAttribute,
      AValue,
      ABufferLength,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLGetTypeInfo(
  AStatementHandle: SQLHSTMT;
  ADataType: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLGetTypeInfo <> nil then
    Result:= PSQLGetTypeInfo(
      AStatementHandle,
      ADataType)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLMoreResults(
  AStatementHandle: SQLHSTMT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLMoreResults <> nil then
    Result:= PSQLMoreResults(
      AStatementHandle)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLNativeSql(
  AConnectionHandle: SQLHDBC;
  AInStatementText: SQLCHARPtr;
  ATextLength1: SQLINTEGER;
  AOutStatementText: SQLCHARPtr;
  ABufferLength: SQLINTEGER;
  ATextLength2: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLNativeSql <> nil then
    Result:= PSQLNativeSql(
      AConnectionHandle,
      AInStatementText,
      ATextLength1,
      AOutStatementText,
      ABufferLength,
      ATextLength2)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLNumParams(
  AStatementHandle: SQLHSTMT;
  AParameterCount: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLNumParams <> nil then
    Result:= PSQLNumParams(
      AStatementHandle,
      AParameterCount)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLNumResultCols(
  AStatementHandle: SQLHSTMT;
  AColumnCount: SQLSMALLINTPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLNumResultCols <> nil then
    Result:= PSQLNumResultCols(
      AStatementHandle,
      AColumnCount)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLParamData(
  AStatementHandle: SQLHSTMT;
  AValue: SQLPOINTERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLParamData <> nil then
    Result:= PSQLParamData(
      AStatementHandle,
      AValue)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLPrepare(
  AStatementHandle: SQLHSTMT;
  AStatementText: SQLCHARPtr;
  ATextLength: SQLINTEGER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLPrepare <> nil then
    Result:= PSQLPrepare(
      AStatementHandle,
      AStatementText,
      ATextLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLPrimaryKeys(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLPrimaryKeys <> nil then
    Result:= PSQLPrimaryKeys(
      AStatementHandle,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      ATableName,
      ANameLength3)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLProcedureColumns(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AProcName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLProcedureColumns <> nil then
    Result:= PSQLProcedureColumns(
      AStatementHandle,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      AProcName,
      ANameLength3,
      AColumnName,
      ANameLength4)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLProcedures(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AProcName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLProcedures <> nil then
    Result:= PSQLProcedures(
      AStatementHandle,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      AProcName,
      ANameLength3)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLPutData(
  AStatementHandle: SQLHSTMT;
  AData: SQLPOINTER;
  AStrLen_or_Ind: SQLINTEGER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLPutData <> nil then
    Result:= PSQLPutData(
      AStatementHandle,
      AData,
      AStrLen_or_Ind)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLRowCount(
  AStatementHandle: SQLHSTMT;
  ARowCount: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLRowCount <> nil then
    Result:= PSQLRowCount(
      AStatementHandle,
      ARowCount)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLSetConnectAttr(
  AConnectionHandle: SQLHDBC;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLSetConnectAttr <> nil then
    Result:= PSQLSetConnectAttr(
      AConnectionHandle,
      AAttribute,
      AValue,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLSetCursorName(
  AStatementHandle: SQLHSTMT;
  ACursorName: SQLCHARPtr;
  ANameLength: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLSetCursorName <> nil then
    Result:= PSQLSetCursorName(
      AStatementHandle,
      ACursorName,
      ANameLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLSetDescField(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AFieldIdentifier: SQLSMALLINT;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLSetDescField <> nil then
    Result:= PSQLSetDescField(
      ADescriptorHandle,
      ARecNumber,
      AFieldIdentifier,
      AValue,
      ABufferLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLSetDescRec(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AType: SQLSMALLINT;
  ASubType: SQLSMALLINT;
  ALength: SQLINTEGER;
  APrecision: SQLSMALLINT;
  AScale: SQLSMALLINT;
  AData: SQLPOINTER;
  AStringLength: SQLINTEGERPtr;
  AIndicator: SQLINTEGERPtr): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLSetDescRec <> nil then
    Result:= PSQLSetDescRec(
      ADescriptorHandle,
      ARecNumber,
      AType,
      ASubType,
      ALength,
      APrecision,
      AScale,
      AData,
      AStringLength,
      AIndicator)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLSetEnvAttr(
  AEnvironmentHandle: SQLHENV;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLSetEnvAttr <> nil then
    Result:= PSQLSetEnvAttr(
      AEnvironmentHandle,
      AAttribute,
      AValue,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLSetPos(
  AStatementHandle: SQLHSTMT;
  ARowNumber: SQLUSMALLINT;
  AOperation: SQLUSMALLINT;
  ALockType: SQLUSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLSetPos <> nil then
    Result:= PSQLSetPos(
      AStatementHandle,
      ARowNumber,
      AOperation,
      ALockType)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLSetStmtAttr(
  AStatementHandle: SQLHSTMT;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLSetStmtAttr <> nil then
    Result:= PSQLSetStmtAttr(
      AStatementHandle,
      AAttribute,
      AValue,
      AStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLSpecialColumns(
  AStatementHandle: SQLHSTMT;
  AIdentifierType: SQLUSMALLINT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AScope: SQLUSMALLINT;
  ANullable: SQLUSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLSpecialColumns <> nil then
    Result:= PSQLSpecialColumns(
      AStatementHandle,
      AIdentifierType,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      ATableName,
      ANameLength3,
      AScope,
      ANullable)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLStatistics(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AUnique: SQLUSMALLINT;
  AReserved: SQLUSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLStatistics <> nil then
    Result:= PSQLStatistics(
      AStatementHandle,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      ATableName,
      ANameLength3,
      AUnique,
      AReserved)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLTablePrivileges(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLTablePrivileges <> nil then
    Result:= PSQLTablePrivileges(
      AStatementHandle,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      ATableName,
      ANameLength3)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

function SQLTables(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  ATableType: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN;
begin
  LoadODBCDLL;

  if @PSQLTables <> nil then
    Result:= PSQLTables(
      AStatementHandle,
      ACatalogName,
      ANameLength1,
      ASchemaName,
      ANameLength2,
      ATableName,
      ANameLength3,
      ATableType,
      ANameLength4)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCDLL);
end;

{ Installer API }

function SQLConfigDataSource(
  AWindowParent: SQLHWND;
  ARequest: SQLUSMALLINT;
  ADriver: SQLPOINTER;
  AAttributes: SQLPOINTER): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLConfigDataSource <> nil then
    Result:= PSQLConfigDataSource(
      AWindowParent,
      ARequest,
      ADriver,
      AAttributes)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLConfigDriver(
  AWindowParent: SQLHWND;
  ARequest: SQLUSMALLINT;
  ADriver: SQLPOINTER;
  AArguments: SQLPOINTER;
  AMessage: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AMessageLength: SQLUSMALLINTPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLConfigDriver <> nil then
    Result:= PSQLConfigDriver(
      AWindowParent,
      ARequest,
      ADriver,
      AArguments,
      AMessage,
      ABufferLength,
      AMessageLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLCreateDataSource(
  AWindowParent: SQLHWND;
  ADataSource: SQLPOINTER): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLCreateDataSource <> nil then
    Result:= PSQLCreateDataSource(
      AWindowParent,
      ADataSource)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLGetConfigMode(
  AConfigMode: SQLUSMALLINTPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLGetConfigMode <> nil then
    Result:= PSQLGetConfigMode(
      AConfigMode)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLGetInstalledDrivers(
  ADescriptions: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  ADescriptionsLength: SQLUSMALLINTPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLGetInstalledDrivers <> nil then
    Result:= PSQLGetInstalledDrivers(
      ADescriptions,
      ABufferLength,
      ADescriptionsLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLGetPrivateProfileString(
  ASection: SQLPOINTER;
  AEntry: SQLPOINTER;
  ADefault: SQLPOINTER;
  AProfileString: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AFilename: SQLPOINTER): SQLINTEGER;
begin
  LoadODBCINSTDLL;

  if @PSQLGetPrivateProfileString <> nil then
    Result:= PSQLGetPrivateProfileString(
      ASection,
      AEntry,
      ADefault,
      AProfileString,
      ABufferLength,
      AFilename)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLGetTranslator(
  AWindowParent: SQLHWND;
  AName: SQLPOINTER;
  ABufferLength1: SQLUSMALLINT;
  ANameLength: SQLUSMALLINTPtr;
  APath: SQLPOINTER;
  ABufferLength2: SQLUSMALLINT;
  APathLength: SQLUSMALLINTPtr;
  AOption: SQLUINTEGERPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLGetTranslator <> nil then
    Result:= PSQLGetTranslator(
      AWindowParent,
      AName,
      ABufferLength1,
      ANameLength,
      APath,
      ABufferLength2,
      APathLength,
      AOption)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLInstallDriverEx(
  ADriver: SQLPOINTER;
  APathIn: SQLPOINTER;
  APathOut: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathOutLength: SQLUSMALLINTPtr;
  ARequest: SQLUSMALLINT;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLInstallDriverEx <> nil then
    Result:= PSQLInstallDriverEx(
      ADriver,
      APathIn,
      APathOut,
      ABufferLength,
      APathOutLength,
      ARequest,
      AUsageCount)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLInstallDriverManager(
  APath: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathLength: SQLUSMALLINTPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLInstallDriverManager <> nil then
    Result:= PSQLInstallDriverManager(
      APath,
      ABufferLength,
      APathLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLInstallerError(
  AErrorNumber: SQLUSMALLINT;
  AErrorCode: SQLUINTEGERPtr;
  AErrorMessage: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AErrorMessageLength: SQLUSMALLINTPtr): SQLRETURN;
begin
  LoadODBCINSTDLL;

  if @PSQLInstallerError <> nil then
    Result:= PSQLInstallerError(
      AErrorNumber,
      AErrorCode,
      AErrorMessage,
      ABufferLength,
      AErrorMessageLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLInstallTranslatorEx(
  ATranslator: SQLPOINTER;
  APathIn: SQLPOINTER;
  APathOut: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathOutLength: SQLUSMALLINTPtr;
  ARequest: SQLUSMALLINT;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLInstallTranslatorEx <> nil then
    Result:= PSQLInstallTranslatorEx(
      ATranslator,
      APathIn,
      APathOut,
      ABufferLength,
      APathOutLength,
      ARequest,
      AUsageCount)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLManageDataSources(
  AWindowParent: SQLHWND): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLManageDataSources <> nil then
    Result:= PSQLManageDataSources(
      AWindowParent)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLPostInstallerError(
  AErrorCode: SQLUINTEGER;
  AErrorMessage: SQLPOINTER): SQLRETURN;
begin
  LoadODBCINSTDLL;

  if @PSQLPostInstallerError <> nil then
    Result:= PSQLPostInstallerError(
      AErrorCode,
      AErrorMessage)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLReadFileDSN(
  AFileName: SQLPOINTER;
  AAppName: SQLPOINTER;
  AKeyName: SQLPOINTER;
  AKeyString: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AKeyStringLength: SQLUSMALLINTPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLReadFileDSN <> nil then
    Result:= PSQLReadFileDSN(
      AFileName,
      AAppName,
      AKeyName,
      AKeyString,
      ABufferLength,
      AKeyStringLength)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLRemoveDriver(
  ADriver: SQLPOINTER;
  ARemoveDataSources: SQLBOOL;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLRemoveDriver <> nil then
    Result:= PSQLRemoveDriver(
      ADriver,
      ARemoveDataSources,
      AUsageCount)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLRemoveDriverManager(
  AUsageCount: SQLUINTEGERPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLRemoveDriverManager <> nil then
    Result:= PSQLRemoveDriverManager(
      AUsageCount)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLRemoveDSNFromIni(
  ADataSource: SQLPOINTER): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLRemoveDSNFromIni <> nil then
    Result:= PSQLRemoveDSNFromIni(
      ADataSource)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLRemoveTranslator(
  ATranslator: SQLPOINTER;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLRemoveTranslator <> nil then
    Result:= PSQLRemoveTranslator(
      ATranslator,
      AUsageCount)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLSetConfigMode(
  AConfigMode: SQLUSMALLINT): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLSetConfigMode <> nil then
    Result:= PSQLSetConfigMode(
      AConfigMode)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLValidDSN(
  ADataSource: SQLPOINTER): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLValidDSN <> nil then
    Result:= PSQLValidDSN(
      ADataSource)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLWriteDSNToIni(
  ADataSource: SQLPOINTER;
  ADriver: SQLPOINTER): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLWriteDSNToIni <> nil then
    Result:= PSQLWriteDSNToIni(
      ADataSource,
      ADriver)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLWriteFileDSN(
  AFileName: SQLPOINTER;
  AAppName: SQLPOINTER;
  AKeyName: SQLPOINTER;
  AKeyString: SQLPOINTER): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLWriteFileDSN <> nil then
    Result:= PSQLWriteFileDSN(
      AFileName,
      AAppName,
      AKeyName,
      AKeyString)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

function SQLWritePrivateProfileString(
  ASection: SQLPOINTER;
  AEntry: SQLPOINTER;
  AProfileString: SQLPOINTER;
  AFilename: SQLPOINTER): SQLBOOL;
begin
  LoadODBCINSTDLL;

  if @PSQLWritePrivateProfileString <> nil then
    Result:= PSQLWritePrivateProfileString(
      ASection,
      AEntry,
      AProfileString,
      AFilename)
  else
    raise EODBCExpress.Create(ErrorLoadingMethod+ODBCINSTDLL);
end;

initialization
  hODBCDLL:= 0;
  hODBCINSTDLL:= 0;
finalization
  if hODBCDLL <> 0 then
    FreeLibrary(hODBCDLL);
  if hODBCINSTDLL <> 0 then
    FreeLibrary(hODBCINSTDLL);

{$ELSE}

//function SQLAllocConnect

//function SQLAllocEnv

function SQLAllocHandle(
  AHandleType: SQLSMALLINT;
  AInputHandle: SQLHANDLE;
  AOutputHandle: SQLHANDLEPtr): SQLRETURN; external ODBCDLL;

//function SQLAllocStmt

function SQLBindCol(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  ATargetType: SQLSMALLINT;
  ATargetValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

//function SQLBindParam

function SQLBindParameter(
  AStatementHandle: SQLHSTMT;
  AParameterNumber: SQLUSMALLINT;
  AInputOutputType: SQLSMALLINT;
  AValueType: SQLSMALLINT;
  AParameterType: SQLSMALLINT;
  AColumnSize: SQLUINTEGER;
  ADecimalDigits: SQLSMALLINT;
  AParameterValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

function SQLBrowseConnect(
  AConnectionHandle: SQLHDBC;
  AInConnectionString: SQLCHARPtr;
  AStringLength1: SQLSMALLINT;
  AOutConnectionString: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength2: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLBulkOperations(
  AStatementHandle: SQLHSTMT;
  AOperation: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLCancel(
  AStatementHandle: SQLHSTMT): SQLRETURN; external ODBCDLL;

function SQLCloseCursor(
  AStatementHandle: SQLHSTMT): SQLRETURN; external ODBCDLL;

function SQLColAttribute(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  AFieldIdentifier: SQLUSMALLINT;
  ACharacterAttribute: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr;
  ANumericAttribute: SQLPOINTER): SQLRETURN; external ODBCDLL;

//function SQLColAttributes

function SQLColumnPrivileges(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLColumns(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLConnect(
  AConnectionHandle: SQLHDBC;
  AServerName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  AUserName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AAuthentication: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLCopyDesc(
  ASourceDescHandle: SQLHDESC;
  ATargetDescHandle: SQLHDESC): SQLRETURN; external ODBCDLL;

function SQLDataSources(
  AEnvironmentHandle: SQLHENV;
  ADirection: SQLUSMALLINT;
  AServerName: SQLCHARPtr;
  ABufferLength1: SQLSMALLINT;
  ANameLength1: SQLSMALLINTPtr;
  ADescription: SQLCHARPtr;
  ABufferLength2: SQLSMALLINT;
  ANameLength2: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLDescribeCol(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  AColumnName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ANameLength: SQLSMALLINTPtr;
  ADataType: SQLSMALLINTPtr;
  AColumnSize: SQLUINTEGERPtr;
  ADecimalDigits: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLDescribeParam(
  AStatementHandle: SQLHSTMT;
  AParameterNumber: SQLUSMALLINT;
  ADataType: SQLSMALLINTPtr;
  AParameterSize: SQLUINTEGERPtr;
  ADecimalDigits: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLDisconnect(
  AConnectionHandle: SQLHDBC): SQLRETURN; external ODBCDLL;

function SQLDriverConnect(
  AConnectionHandle: SQLHDBC;
  AWindowHandle: SQLHWND;
  AInConnectionString: SQLCHARPtr;
  AStringLength1: SQLSMALLINT;
  AOutConnectionString: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength2: SQLSMALLINTPtr;
  ADriverCompletion: SQLUSMALLINT): SQLRETURN; external ODBCDLL;

function SQLDrivers(
  AEnvironmentHandle: SQLHENV;
  ADirection: SQLUSMALLINT;
  ADriverDescription: SQLCHARPtr;
  ABufferLength1: SQLSMALLINT;
  ADescriptionLength: SQLSMALLINTPtr;
  ADriverAttributes: SQLCHARPtr;
  ABufferLength2: SQLSMALLINT;
  AAttributesLength: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLEndTran(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ACompletionType: SQLSMALLINT): SQLRETURN; external ODBCDLL;

//function SQLError

function SQLExecDirect(
  AStatementHandle: SQLHSTMT;
  AStatementText: SQLCHARPtr;
  ATextLength: SQLINTEGER): SQLRETURN; external ODBCDLL;

function SQLExecute(
  AStatementHandle: SQLHSTMT): SQLRETURN; external ODBCDLL;

//function SQLExtendedFetch

function SQLFetch(
  AStatementHandle: SQLHSTMT): SQLRETURN; external ODBCDLL;

function SQLFetchScroll(
  AStatementHandle: SQLHSTMT;
  AFetchOrientation: SQLSMALLINT;
  AFetchOffset: SQLINTEGER): SQLRETURN; external ODBCDLL;

function SQLForeignKeys(
  AStatementHandle: SQLHSTMT;
  APKCatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  APKSchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  APKTableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AFKCatalogName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT;
  AFKSchemaName: SQLCHARPtr;
  ANameLength5: SQLSMALLINT;
  AFKTableName: SQLCHARPtr;
  ANameLength6: SQLSMALLINT): SQLRETURN; external ODBCDLL;

//function SQLFreeConnect

//function SQLFreeEnv

function SQLFreeHandle(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE): SQLRETURN; external ODBCDLL;

function SQLFreeStmt(
  AStatementHandle: SQLHSTMT;
  AOption: SQLUSMALLINT): SQLRETURN; external ODBCDLL;

function SQLGetConnectAttr(
  AConnectionHandle: SQLHDBC;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

//function SQLGetConnectOption

function SQLGetCursorName(
  AStatementHandle: SQLHSTMT;
  ACursorName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ANameLength: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLGetData(
  AStatementHandle: SQLHSTMT;
  AColumnNumber: SQLUSMALLINT;
  ATargetType: SQLSMALLINT;
  ATargetValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStrLen_or_Ind: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

function SQLGetDescField(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AFieldIdentifier: SQLSMALLINT;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

function SQLGetDescRec(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AName: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr;
  AType: SQLSMALLINTPtr;
  ASubType: SQLSMALLINTPtr;
  ALength: SQLINTEGERPtr;
  APrecision: SQLSMALLINTPtr;
  AScale: SQLSMALLINTPtr;
  ANullable: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLGetDiagField(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ARecNumber: SQLSMALLINT;
  ADiagIdentifier: SQLSMALLINT;
  ADiagInfo: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLGetDiagRec(
  AHandleType: SQLSMALLINT;
  AHandle: SQLHANDLE;
  ARecNumber: SQLSMALLINT;
  ASqlstate: SQLCHARPtr;
  ANativeError: SQLINTEGERPtr;
  AMessageText: SQLCHARPtr;
  ABufferLength: SQLSMALLINT;
  ATextLength: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLGetEnvAttr(
  AEnvironmentHandle: SQLHENV;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

function SQLGetFunctions(
  AConnectionHandle: SQLHDBC;
  AFunctionId: SQLUSMALLINT;
  ASupported: SQLUSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLGetInfo(
  AConnectionHandle: SQLHDBC;
  AInfoType: SQLUSMALLINT;
  AInfoValue: SQLPOINTER;
  ABufferLength: SQLSMALLINT;
  AStringLength: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLGetStmtAttr(
  AStatementHandle: SQLHSTMT;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AStringLength: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

//function SQLGetStmtOption

function SQLGetTypeInfo(
  AStatementHandle: SQLHSTMT;
  ADataType: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLMoreResults(
  AStatementHandle: SQLHSTMT): SQLRETURN; external ODBCDLL;

function SQLNativeSql(
  AConnectionHandle: SQLHDBC;
  AInStatementText: SQLCHARPtr;
  ATextLength1: SQLINTEGER;
  AOutStatementText: SQLCHARPtr;
  ABufferLength: SQLINTEGER;
  ATextLength2: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

function SQLNumParams(
  AStatementHandle: SQLHSTMT;
  AParameterCount: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLNumResultCols(
  AStatementHandle: SQLHSTMT;
  AColumnCount: SQLSMALLINTPtr): SQLRETURN; external ODBCDLL;

function SQLParamData(
  AStatementHandle: SQLHSTMT;
  AValue: SQLPOINTERPtr): SQLRETURN; external ODBCDLL;

//function SQLParamOptions

function SQLPrepare(
  AStatementHandle: SQLHSTMT;
  AStatementText: SQLCHARPtr;
  ATextLength: SQLINTEGER): SQLRETURN; external ODBCDLL;

function SQLPrimaryKeys(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLProcedureColumns(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AProcName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AColumnName: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLProcedures(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  AProcName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLPutData(
  AStatementHandle: SQLHSTMT;
  AData: SQLPOINTER;
  AStrLen_or_Ind: SQLINTEGER): SQLRETURN; external ODBCDLL;

function SQLRowCount(
  AStatementHandle: SQLHSTMT;
  ARowCount: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

function SQLSetConnectAttr(
  AConnectionHandle: SQLHDBC;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; external ODBCDLL;

//function SQLSetConnectOption

function SQLSetCursorName(
  AStatementHandle: SQLHSTMT;
  ACursorName: SQLCHARPtr;
  ANameLength: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLSetDescField(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AFieldIdentifier: SQLSMALLINT;
  AValue: SQLPOINTER;
  ABufferLength: SQLINTEGER): SQLRETURN; external ODBCDLL;

function SQLSetDescRec(
  ADescriptorHandle: SQLHDESC;
  ARecNumber: SQLSMALLINT;
  AType: SQLSMALLINT;
  ASubType: SQLSMALLINT;
  ALength: SQLINTEGER;
  APrecision: SQLSMALLINT;
  AScale: SQLSMALLINT;
  AData: SQLPOINTER;
  AStringLength: SQLINTEGERPtr;
  AIndicator: SQLINTEGERPtr): SQLRETURN; external ODBCDLL;

function SQLSetEnvAttr(
  AEnvironmentHandle: SQLHENV;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; external ODBCDLL;

//function SQLSetParam

function SQLSetPos(
  AStatementHandle: SQLHSTMT;
  ARowNumber: SQLUSMALLINT;
  AOperation: SQLUSMALLINT;
  ALockType: SQLUSMALLINT): SQLRETURN; external ODBCDLL;

//function SQLSetScrollOptions

function SQLSetStmtAttr(
  AStatementHandle: SQLHSTMT;
  AAttribute: SQLINTEGER;
  AValue: SQLPOINTER;
  AStringLength: SQLINTEGER): SQLRETURN; external ODBCDLL;

//function SQLSetStmtOption

function SQLSpecialColumns(
  AStatementHandle: SQLHSTMT;
  AIdentifierType: SQLUSMALLINT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AScope: SQLUSMALLINT;
  ANullable: SQLUSMALLINT): SQLRETURN; external ODBCDLL;

function SQLStatistics(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  AUnique: SQLUSMALLINT;
  AReserved: SQLUSMALLINT): SQLRETURN; external ODBCDLL;

function SQLTablePrivileges(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT): SQLRETURN; external ODBCDLL;

function SQLTables(
  AStatementHandle: SQLHSTMT;
  ACatalogName: SQLCHARPtr;
  ANameLength1: SQLSMALLINT;
  ASchemaName: SQLCHARPtr;
  ANameLength2: SQLSMALLINT;
  ATableName: SQLCHARPtr;
  ANameLength3: SQLSMALLINT;
  ATableType: SQLCHARPtr;
  ANameLength4: SQLSMALLINT): SQLRETURN; external ODBCDLL;

//function SQLTransact

{ Tracing section }

{#define TRACE_VERSION 1000  // Version of trace API

RETCODE	SQL_API TraceOpenLogFile(LPWSTR,LPWSTR,DWORD);  // open a trace log file
RETCODE	SQL_API TraceCloseLogFile();                    // Request to close a trace log
VOID    SQL_API TraceReturn(RETCODE,RETCODE);           // Processes trace after FN is called
DWORD   SQL_API TraceVersion();                         // Returns trace API version}

{ Installer API }

function SQLConfigDataSource(
  AWindowParent: SQLHWND;
  ARequest: SQLUSMALLINT;
  ADriver: SQLPOINTER;
  AAttributes: SQLPOINTER): SQLBOOL; external ODBCINSTDLL;

function SQLConfigDriver(
  AWindowParent: SQLHWND;
  ARequest: SQLUSMALLINT;
  ADriver: SQLPOINTER;
  AArguments: SQLPOINTER;
  AMessage: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AMessageLength: SQLUSMALLINTPtr): SQLBOOL; external ODBCINSTDLL;

function SQLCreateDataSource(
  AWindowParent: SQLHWND;
  ADataSource: SQLPOINTER): SQLBOOL; external ODBCINSTDLL;

//function SQLGetAvailableDrivers

function SQLGetConfigMode(
  AConfigMode: SQLUSMALLINTPtr): SQLBOOL; external ODBCINSTDLL;

function SQLGetInstalledDrivers(
  ADescriptions: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  ADescriptionsLength: SQLUSMALLINTPtr): SQLBOOL; external ODBCINSTDLL;

function SQLGetPrivateProfileString(
  ASection: SQLPOINTER;
  AEntry: SQLPOINTER;
  ADefault: SQLPOINTER;
  AProfileString: SQLPOINTER;
  ABufferLength: SQLINTEGER;
  AFilename: SQLPOINTER): SQLINTEGER; external ODBCINSTDLL;

function SQLGetTranslator(
  AWindowParent: SQLHWND;
  AName: SQLPOINTER;
  ABufferLength1: SQLUSMALLINT;
  ANameLength: SQLUSMALLINTPtr;
  APath: SQLPOINTER;
  ABufferLength2: SQLUSMALLINT;
  APathLength: SQLUSMALLINTPtr;
  AOption: SQLUINTEGERPtr): SQLBOOL; external ODBCINSTDLL;

//function SQLInstallDriver

function SQLInstallDriverEx(
  ADriver: SQLPOINTER;
  APathIn: SQLPOINTER;
  APathOut: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathOutLength: SQLUSMALLINTPtr;
  ARequest: SQLUSMALLINT;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; external ODBCINSTDLL;

function SQLInstallDriverManager(
  APath: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathLength: SQLUSMALLINTPtr): SQLBOOL; external ODBCINSTDLL;

function SQLInstallerError(
  AErrorNumber: SQLUSMALLINT;
  AErrorCode: SQLUINTEGERPtr;
  AErrorMessage: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AErrorMessageLength: SQLUSMALLINTPtr): SQLRETURN; external ODBCINSTDLL;

//function SQLInstallODBC

//function SQLInstallTranslator

function SQLInstallTranslatorEx(
  ATranslator: SQLPOINTER;
  APathIn: SQLPOINTER;
  APathOut: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  APathOutLength: SQLUSMALLINTPtr;
  ARequest: SQLUSMALLINT;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; external ODBCINSTDLL;

function SQLManageDataSources(
  AWindowParent: SQLHWND): SQLBOOL; external ODBCINSTDLL;

function SQLPostInstallerError(
  AErrorCode: SQLUINTEGER;
  AErrorMessage: SQLPOINTER): SQLRETURN; external ODBCINSTDLL;

function SQLReadFileDSN(
  AFileName: SQLPOINTER;
  AAppName: SQLPOINTER;
  AKeyName: SQLPOINTER;
  AKeyString: SQLPOINTER;
  ABufferLength: SQLUSMALLINT;
  AKeyStringLength: SQLUSMALLINTPtr): SQLBOOL; external ODBCINSTDLL;

//function SQLRemoveDefaultDataSource

function SQLRemoveDriver(
  ADriver: SQLPOINTER;
  ARemoveDataSources: SQLBOOL;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; external ODBCINSTDLL;

function SQLRemoveDriverManager(
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; external ODBCINSTDLL;

function SQLRemoveDSNFromIni(
  ADataSource: SQLPOINTER): SQLBOOL; external ODBCINSTDLL;

function SQLRemoveTranslator(
  ATranslator: SQLPOINTER;
  AUsageCount: SQLUINTEGERPtr): SQLBOOL; external ODBCINSTDLL;

function SQLSetConfigMode(
  AConfigMode: SQLUSMALLINT): SQLBOOL; external ODBCINSTDLL;

function SQLValidDSN(
  ADataSource: SQLPOINTER): SQLBOOL; external ODBCINSTDLL;

function SQLWriteDSNToIni(
  ADataSource: SQLPOINTER;
  ADriver: SQLPOINTER): SQLBOOL; external ODBCINSTDLL;

function SQLWriteFileDSN(
  AFileName: SQLPOINTER;
  AAppName: SQLPOINTER;
  AKeyName: SQLPOINTER;
  AKeyString: SQLPOINTER): SQLBOOL; external ODBCINSTDLL;

function SQLWritePrivateProfileString(
  ASection: SQLPOINTER;
  AEntry: SQLPOINTER;
  AProfileString: SQLPOINTER;
  AFilename: SQLPOINTER): SQLBOOL; external ODBCINSTDLL;

{$ENDIF}

end.

