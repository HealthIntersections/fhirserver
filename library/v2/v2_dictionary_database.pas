unit v2_dictionary_database;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$I fhir.inc}

interface

uses
  fsl_utilities,
  fdb_manager, fdb_dialects, fdb_odbc,
  v2_base, v2_dictionary;

const
  // This minimal data layer is solely used to prevent case sensitivity problems
  // on RDBMS such as Sybase. Field names cross tables - maybe this will be
  // fixed up sometime in the future

  FN_VERSION = 'HL7_Version';
  FN_VERSION_ID = 'version_id';
  FN_DESCRIPTIONP = 'description_as_pub';
  FN_DESCRIPTION = 'description';
  FN_SEGCODE = 'Seg_Code';
  FN_DATAITEM = 'Data_Item';
  FN_REQOPT = 'Req_Opt';
  FN_REPNAL = 'Repetitional';
  FN_REPCOUNT = 'Repetitions';
  FN_OPTNAL = 'Optional';
  FN_FIELDNUM_2 = 'Lfd_Nr';
  FN_FIELDNUM_3 = 'seq_no';
  FN_COMPNUM_2 = 'Comp_Nr';
  FN_COMPNUM_3 = 'Comp_No';
  FN_TABLEID = 'Table_ID';
  FN_DTCODE = 'Data_Type_Code';
  FN_DATASTRUC = 'Data_Structure';
  FN_LENGTH = 'Length';
  FN_LENGTH_OLD = 'Length_Old';        // because of v2.7 changes
  FN_LENGTH_MIN = 'min_length';
  FN_LENGTH_MAX = 'max_length';
  FN_LENGTH_CONF = 'conf_length';
  FN_ELEMENTARY = 'Elementary';
  FN_TABLEVALS = 'Table_Value';
  FN_SORTNUM_2 = 'Sort_Nr';
  FN_SORTNUM_3 = 'Sort_No';
  FN_EVNTCODE = 'Event_Code';
  FN_MSGTYPE = 'message_type';
  FN_USAGE = 'Usage';
  FN_TABLETYPE = 'Table_Typ';
  FN_MSG_STRUCT = 'Message_Structure';
  FN_XMPL_EVENT = 'Example_Event';
  FN_XMPL_MTYPE = 'Example_Msg_Type';
  FN_ACTION = 'Action';        // note: Action is an invalid field name for Interbase
  FN_MSGTYP_SEND = 'Message_Typ_Snd';
  FN_MSGTYP_RETN = 'Message_Typ_Return';
  FN_MSGSTRUC_SEND = 'Message_Structure_Snd';
  FN_MSGSTRUC_RETN = 'Message_Structure_Return';
  FN_GROUPNAME = 'GroupName';               // in German - inconvenient but the system forces us to use Frank Oemig's names

  TN_VERSIONS = 'Versions';
  TN_SEGDATELEM = 'SegmentDataElements';
  TN_COMPLIST = 'Components';
  TN_DATAELEM = 'DataElements';
  TN_DATATYPES = 'DataTypes';
  TN_SEGMENTS = 'Segments';
  TN_DATASTRUCS = 'DataStructures';
  TN_DSCOMPS = 'DataStructureComponents';
  TN_TABLES = 'Tables';
  TN_TABLEVALS = 'TableValues';
  TN_EVENTS = 'Events';
  TN_MSGSTRUCTS = 'MsgStructIDs';
  TN_EVENTMSGTYP = 'EventMessageTypes';
  TN_STRUCTSEGS = 'MsgStructIDSegments';
  TN_EVNTMSGSEGS = 'EventMessageTypeSegments';

type
  THL7V2OdbcDictionary = class(THL7V2Dictionary)
  Private
    function GetVersionSelect(aVersion : THL7V2Version):String;
    procedure FindSegment(oStructure : THL7V2ModelMessageStructure; oStack : THL7V2ModelSegmentGroups; oConn : TFDBConnection);
    procedure SaveSegmentGroup(oConn: TFDBConnection; var iNum : integer; oGroup : THL7V2ModelSegmentGroup);
  Protected
    function FieldNum:String;
    function GetCompNumName : String;
    function GetSortNumName : String;

    function GetDBVersion : String; Virtual;
    function GetConnection(usage: String): TFDBConnection; Virtual;
    procedure YieldConnection(oConn: TFDBConnection); Virtual;
    function NamePrefix: String; Virtual;

    function  VersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean; Override;
    Function ListVersions : THL7V2Versions; Override;

    procedure LoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables); Override;
    procedure LoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents); Override;
    procedure LoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements); Override;
    procedure LoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure LoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment); Override;
    procedure LoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes); Override;
    procedure LoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure LoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents); Override;
    procedure LoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure LoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure LoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Override;
    procedure LoadSegmentMaps(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Override;

    procedure AddVersion(aVersion : THL7V2Version; const sDescription: String); Override;
    procedure AddTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables); Override;
    procedure AddComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents); Override;
    procedure AddDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements); Override;
    procedure AddSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure AddSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure AddDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes); Override;
    procedure AddStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure AddStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure AddEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure AddEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure AddMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Override;
    procedure AddSegmentMaps(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Override;

    procedure DoneLoading(aTransferEvent: TOnDictTransferProgress); Override;
  end;

function FixActionFieldName(platform: TFDBPlatform): String;

type
  // you need this driver: https://www.microsoft.com/en-us/download/details.aspx?id=13255
  THL7V2AccessDictionary = class(THL7V2OdbcDictionary)
  Private
    FManager: TFDBManager;
    FFilename: String;
    FStmt: TFDBConnection;
    FStmtInUse: Boolean;
    FDBVersion : String;
    procedure LookupDBVersion;
    procedure SetFileName(const Value: String);
    procedure Close;
  Protected
    function GetDBVersion : String; override;
    procedure PrepareForLoad(wipe: Boolean); Override;
    function GetConnection(usage: String): TFDBConnection; Override;
    procedure YieldConnection(stmt: TFDBConnection); Override;
    function NamePrefix: String; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; overload; override;
    constructor Create(sHL7Dict: String); overload;
    destructor Destroy; Override;
    function SourceDescription(fulldetails: Boolean): String; Override;
    property FileName : String read FFileName write SetFileName;
  end;

implementation

  { utils }

function FixActionFieldName(platform: TFDBPlatform): String;
begin
  if platform = kdbInterbase then
    Result := 'Item' + FN_ACTION
  else
    Result := FN_ACTION
end;

function SQLWrapString(const AStr: String): String;
var
  i: Integer;
begin
  Result := AStr;
  for i := Length(Result) downto 1 do
    if Result[i] = '''' then
      Insert('''', Result, i);
  // GDG 10/12/2000. This routine used to handle ':' as well, but this is redundant
  // after upgrade to latest odbc version many months before.
end;

{ THL7V2OdbcDictionary }

function THL7V2OdbcDictionary.VersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean;
var
  oConn: TFDBConnection;
begin
  oConn := GetConnection('VersionDefined');
  try
    oConn.sql := 'select ' + FN_DESCRIPTION + ' from ' + NamePrefix + TN_VERSIONS + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      Result := oConn.fetchnext;
      if Result then
        sDesc := oConn.ColStringByName[FN_DESCRIPTION];
    finally
      oConn.terminate;
      end;
  finally
    YieldConnection(oConn);
    end;
end;

Function THL7V2OdbcDictionary.ListVersions : THL7V2Versions;
var
  oConn: TFDBConnection;
begin
  result := [];
  oConn := GetConnection('ListVersions');
  try
    oConn.sql := 'select ' + FN_VERSION + ' from ' + NamePrefix + TN_VERSIONS;
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        include(result, THL7V2Version(StringArrayIndexOfInsensitive(NAMES_HL7V2_VERSION, oConn.ColStringByName[FN_VERSION])));
    finally
      oConn.terminate;
      end;
  finally
    YieldConnection(oConn);
    end;
end;

procedure THL7V2OdbcDictionary.AddVersion(aVersion : THL7V2Version; const sDescription: String);
var
  oConn: TFDBConnection;
  sErrDesc: String;
begin
  oConn := GetConnection('AddVersion');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_VERSIONS + ' (' + FN_VERSION + ', ' + FN_DESCRIPTION + ') values (''' + NAMES_HL7V2_VERSION[aVersion] + ''', ''' + SQLWrapString(sDescription) + ''')';
    oConn.Prepare;
    try
      try
        //      LogFullContents(TN_VERSIONS, description, version);
        oConn.Execute;
      except
        oConn.terminate;
        sErrDesc := sDescription;
        ConvertCharacter(NamePrefix + TN_VERSIONS, '', sErrDesc);
        oConn.sql := 'insert into ' + NamePrefix + TN_VERSIONS + ' (' + FN_VERSION + ', ' + FN_DESCRIPTION+ ') values (''' + NAMES_HL7V2_VERSION[aVersion] + ''', ''' + SQLWrapString(sErrDesc) + ''')';
        oConn.Prepare;
        oConn.Execute;
        end;
    finally
      oConn.terminate;
      end;
  finally
    YieldConnection(oConn);
    end;
end;

procedure THL7V2OdbcDictionary.LoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment);
var
  oConn: TFDBConnection;
  oField : THL7V2ModelField;
begin
  oConn := GetConnection('LoadFields');
  try
    oConn.sql := 'select ' + FN_DATAITEM + ', ' + FN_REQOPT + ', ' + FN_REPNAL + ', ' + FN_REPCOUNT + ', ' + FieldNum + ' from ' +
      NamePrefix + TN_SEGDATELEM + ' where ' + FN_SEGCODE + ' = ''' + oSegment.Code + ''' ' +
      'and ' + GetVersionSelect(aVersion)+' order by ' + FieldNum;
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oField := THL7V2ModelField.create;
        try
          oField.DataElement := oConn.ColIntegerByName[FN_DATAITEM];
          oField.Required := (oConn.ColStringByName[FN_REQOPT] = 'R');
          oField.RepeatCount := oConn.ColIntegerByName[FN_REPCOUNT];
          oField.Repeatable := (oField.RepeatCount > 0) or (oConn.ColStringByName[FN_REPNAL] = 'Y');
          oField.FieldNumber := oConn.ColIntegerByName[FieldNum];
          oSegment.Fields.Add(oField.Link);
        finally
          oField.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
var
  oConn: TFDBConnection;
  oSegment : THL7V2ModelSegment;
  iSegment : Integer;
  oField : THL7V2ModelField;
  iField : Integer;
begin
  oConn := GetConnection('AddFields');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_SEGDATELEM + ' (' + FN_SEGCODE + ', ' + FN_DATAITEM + ', ' + FN_REQOPT + ', ' + FN_REPNAL + ', ' + FN_REPCOUNT + ', ' + FN_VERSION + ', ' + FieldNum + ') ' +
      'values (:code, :id, :req, :rep, :count, :ver, :num)';
    oConn.Prepare;
    try
      for iSegment := 0 to oSegments.Count - 1 do
        begin
        oSegment := oSegments[iSegment];
        for iField := 0 to oSegment.Fields.Count - 1 do
          begin
          oField := oSegment.Fields[iField];
          oConn.BindString('code', oSegment.Code);
          oConn.BindInteger('id', oField.DataElement);
          if oField.Required then
            oConn.BindString('req', 'R')
          else
            oConn.BindString('req', '');
          if oField.Repeatable then
            oConn.BindString('rep', 'Y')
          else
            oConn.BindString('rep', '');
          oConn.BindInteger('count', oField.RepeatCount);
          oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
          oConn.BindInteger('num', oField.FieldNumber);
          oConn.Execute;
          end;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents);
var
  oConn: TFDBConnection;
  oComponent : THL7V2ModelComponent;
begin
  oConn := GetConnection('LoadComponents');
  try
    oConn.sql := 'select ' + GetCompNumName + ', ' + FN_DESCRIPTION+ ', ' + FN_TABLEID + ', ' + FN_DTCODE + ' from ' + NamePrefix + TN_COMPLIST + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oComponent := THL7V2ModelComponent.create;
        try
          oComponent.Name := oConn.ColStringByName[FN_DESCRIPTION];
          oComponent.Table := oConn.ColIntegerByName[FN_TABLEID];
          oComponent.DataType := oConn.ColStringByName[FN_DTCODE];
          oComponent.Number := oConn.ColIntegerByName[GetCompNumName];
          oComponents.Add(oComponent.Link);
        finally
          oComponent.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents);
var
  oConn: TFDBConnection;
  iComponent : Integer;
  oComponent : THL7V2ModelComponent;
begin
  oConn := GetConnection('AddComponents');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_COMPLIST + ' (' + FN_VERSION + ', ' + GetCompNumName + ', ' + FN_DESCRIPTION + ', ' + FN_TABLEID + ', ' + FN_DTCODE + ') values ' +
      '(:ver, :num, :desc, :id, :code)';
    oConn.Prepare;
    try
      for iComponent := 0 to oComponents.Count - 1 do
        begin
        oComponent := oComponents[iComponent];
        oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
        oConn.BindInteger('num', oComponent.Number);
        oConn.BindString('desc', oComponent.Name);
        oConn.BindInteger('id', oComponent.Table);
        oConn.BindString('code', oComponent.DataType);
        oConn.Execute;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements);
var
  oConn: TFDBConnection;
  oDataElement : THL7V2ModelDataElement;
begin
  oConn := GetConnection('LoadDataElements');
  try
    if aVersion >= hv27 then
      oConn.sql := 'select ' + FN_DATAITEM + ', ' + FN_DESCRIPTION + ', ' + FN_DATASTRUC + ', ' + FN_LENGTH_MIN + ', ' + FN_LENGTH_MAX + ', ' + FN_LENGTH_CONF + ', ' + FN_TABLEID + ' from ' + NamePrefix + TN_DATAELEM + ' where ' + GetVersionSelect(aVersion)
    else
      oConn.sql := 'select ' + FN_DATAITEM + ', ' + FN_DESCRIPTION + ', ' + FN_DATASTRUC + ', ' + FN_LENGTH_OLD + ', ' + FN_TABLEID + ' from ' + NamePrefix + TN_DATAELEM + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oDataElement := THL7V2ModelDataElement.create;
        try
          oDataElement.Description := oConn.colStringByName[FN_DESCRIPTION];
          oDataElement.Structure := oConn.ColStringByName[FN_DATASTRUC];
          if aVersion >= hv27 then
          begin
            oDataElement.Length_Min := oConn.ColIntegerByName[FN_LENGTH_MIN];
            oDataElement.Length_Max := oConn.ColIntegerByName[FN_LENGTH_MAX];
            oDataElement.Length_Conf := oConn.ColStringByName[FN_LENGTH_CONF];
          end
          else
            oDataElement.Length_Old := oConn.ColIntegerByName[FN_LENGTH_OLD];
          oDataElement.Table := oConn.ColIntegerByName[FN_TABLEID];
          oDataElement.Id := oConn.ColIntegerByName[FN_DATAITEM];
          oDataElements.Add(oDataElement.Link);
        finally
          oDataElement.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements);
var
  oConn: TFDBConnection;
  iDataElement : Integer;
  oDataElement : THL7V2ModelDataElement;
begin
  oConn := GetConnection('AddDataElements');
  try
    if aVersion >= hv27 then
      oConn.sql := 'insert into ' + NamePrefix + TN_DATAELEM + ' (' + FN_VERSION + ', ' + FN_DATAITEM + ', ' + FN_DESCRIPTION + ', ' + FN_DATASTRUC + ', ' + FN_LENGTH_MIN + ', ' + FN_LENGTH_MAX + ', ' + FN_LENGTH_CONF + ', ' + FN_TABLEID + ') values ' +
      '(:ver, :num, :desc, :struc, :len_min, :len_max, :len_conf, :table)'
    else
      oConn.sql := 'insert into ' + NamePrefix + TN_DATAELEM + ' (' + FN_VERSION + ', ' + FN_DATAITEM + ', ' + FN_DESCRIPTION + ', ' + FN_DATASTRUC + ', ' + FN_LENGTH_OLD + ', ' + FN_TABLEID + ') values ' +
      '(:ver, :num, :desc, :struc, :len, :table)';
    oConn.Prepare;
    try
      for iDataElement := 0 to oDataElements.Count - 1 do
        begin
        oDataElement := oDataElements[iDataElement];
        oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
        oConn.BindInteger('num', oDataElement.Id);
        oConn.BindString('desc', oDataElement.Description);
        oConn.BindString('struc', oDataElement.Structure);
        if aVersion >= hv27 then
        begin
          oConn.BindInteger('len_min', oDataElement.Length_Min);
          oConn.BindInteger('len_max', oDataElement.Length_Max);
          oConn.BindString('len_conf', oDataElement.Length_Conf);
        end
        else
          oConn.BindInteger('len', oDataElement.Length_Old);
        oConn.BindInteger('table', oDataElement.Table);
        oConn.Execute;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes);
var
  oConn: TFDBConnection;
  oDataType : THL7V2ModelDataType;
begin
  oConn := GetConnection('LoadDataTypes');
  try
    oConn.sql := 'select ' + FN_DTCODE + ', ' + FN_DESCRIPTION + ', ' + FN_LENGTH + ' from ' + NamePrefix + TN_DATATYPES + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oDataType := THL7V2ModelDataType.create;
        try
          oDataType.Name := oConn.ColStringByName[FN_DTCODE];
          oDataType.Description := oConn.ColStringByName[FN_DESCRIPTION];
          oDataType.Length := oConn.ColIntegerByName[FN_LENGTH];
          oDataTypes.Add(oDataType.Link);
        finally
          oDataType.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes);
var
  oConn: TFDBConnection;
  iDataType : Integer;
  oDataType : THL7V2ModelDataType;
begin
  oConn := GetConnection('AddDataTypes');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_DATATYPES + ' (' + FN_VERSION + ', ' + FN_DTCODE + ', ' + FN_DESCRIPTION + ', ' + FN_LENGTH + ') values ' +
      '(:ver, :name, :desc, :len)';
    oConn.Prepare;
    try
      for iDataType := 0 to oDataTypes.Count - 1 do
        begin
        oDataType := oDataTypes[iDataType];
        oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
        oConn.BindString('name', oDataType.Name);
        oConn.BindString('desc', oDataType.Description);
        oConn.BindInteger('len', oDataType.Length);
        oConn.Execute;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
var
  oConn: TFDBConnection;
  oSegment : THL7V2ModelSegment;
begin
  oConn := GetConnection('LoadSegments');
  try
    oConn.sql := 'select ' + FN_DESCRIPTION + ', ' + FN_SEGCODE + ' from ' + NamePrefix + TN_SEGMENTS + ' where ' + GetVersionSelect(aVersion)+' order by ' + FN_SEGCODE + ' asc';
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oSegment := THL7V2ModelSegment.create;
        try
          oSegment.Code := oConn.ColStringByName[FN_SEGCODE];
          oSegment.Description := oConn.ColStringByName[FN_DESCRIPTION];
          oSegments.Add(oSegment.Link);
        finally
          oSegment.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
  LoadSegmentFields(aVersion, oSegments);
end;

procedure THL7V2OdbcDictionary.AddSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
var
  oConn: TFDBConnection;
  iSegment : Integer;
  oSegment : THL7V2ModelSegment;
begin
  oConn := GetConnection('AddSegments');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_SEGMENTS + ' (' + FN_VERSION + ', ' + FN_SEGCODE + ', ' + FN_DESCRIPTION + ') values (:ver, :code, :desc)';
    oConn.Prepare;
    try
      for iSegment := 0 to oSegments.Count - 1 do
        begin
        oSegment := oSegments[iSegment];
        oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
        oConn.BindString('code', oSegment.Code);
        oConn.BindString('desc', oSegment.Description);
        oConn.Execute;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
var
  oConn: TFDBConnection;
  oStructure : THL7V2ModelStructure;
begin
  oConn := GetConnection('LoadStructures');
  try
    oConn.sql := 'select ' + FN_DATASTRUC + ', ' + FN_DESCRIPTION + ', ' + FN_DTCODE + ', ' + FN_ELEMENTARY + ' from ' + NamePrefix + TN_DATASTRUCS + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oStructure := THL7V2ModelStructure.create;
        try
          oStructure.Name := oConn.ColStringByName[FN_DATASTRUC];
          oStructure.Description := oConn.ColStringByName[FN_DESCRIPTION];
          oStructure.DataType := oConn.ColStringByName[FN_DTCODE];
          oStructure.ID := oConn.ColIntegerByName[FN_ELEMENTARY];
          oStructures.Add(oStructure.Link);
        finally
          oStructure.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
var
  oConn: TFDBConnection;
  iStructure : Integer;
  oStructure : THL7V2ModelStructure;
begin
  oConn := GetConnection('AddStructures');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_DATASTRUCS + ' (' + FN_VERSION + ', ' + FN_DATASTRUC + ', ' + FN_DESCRIPTION + ', ' + FN_DTCODE + ', ' + FN_ELEMENTARY + ') values ' +
      '(:ver, :struc, :desc, :code, :id)';
    oConn.Prepare;
    try
      for iStructure := 0 to oStructures.Count - 1 do
        begin
        oStructure := oStructures[iStructure];
        oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
        oConn.BindString('struc', oStructure.Name);
        oConn.BindString('desc', oStructure.Description);
        oConn.BindString('code', oStructure.DataType);
        oConn.BindInteger('id', oStructure.ID);
        oConn.Execute;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents);
var
  oConn: TFDBConnection;
  oStructure : THL7V2ModelStructure;
  oComp : THL7V2ModelComponent;
  sLast : String;
begin
  oStructure := nil;
  oConn := GetConnection('LoadStructures');
  try
    sLast := '';
    oConn.sql := 'select ' + FN_DATASTRUC + ', ' + GetCompNumName + ' from ' + NamePrefix + TN_DSCOMPS + ' where ' + GetVersionSelect(aVersion)+' order by ' + FN_DATASTRUC + ', ' + FieldNum;
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        if oConn.ColStringByName[FN_DATASTRUC] <> sLast then
          begin
          sLast := oConn.ColStringByName[FN_DATASTRUC];
          oStructure := oStructures.GetByName(sLast);
          end;
        oComp := oComponents.GetByNumber(oConn.ColIntegerByName[GetCompNumName]);
        oStructure.Components.add(oComp.Link);
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
var
  oConn: TFDBConnection;
  iStructure : Integer;
  iComp : Integer;
  iNum : integer;
  oStructure : THL7V2ModelStructure;
begin
  iNum := 0;
  oConn := GetConnection('AddStructures');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_DSCOMPS + ' (' + FN_VERSION + ', ' + FN_DATASTRUC + ', ' + GetCompNumName + ', '+FieldNum+') values ' +
      '(:ver, :struc, :comp, :num)';
    oConn.Prepare;
    try
      for iStructure := 0 to oStructures.Count - 1 do
        begin
        oStructure := oStructures[iStructure];
        for iComp := 0 to oStructure.Components.Count - 1 do
          begin
          oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
          oConn.BindString('struc', oStructure.Name);
          oConn.BindInteger('comp', oStructure.Components[iComp].Number);
          oConn.BindInteger('num', iNum);
          inc(iNum);
          oConn.Execute;
          end;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables);
var
  oConn: TFDBConnection;
  oTable : THL7V2ModelTable;
  oTableItem : THL7V2ModelTableItem;
begin
  oConn := GetConnection('LoadTables');
  try
    oConn.sql := 'select ' + FN_TABLEID + ', ' + FN_DESCRIPTIONP + ' from ' + NamePrefix + TN_TABLES + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oTable := THL7V2ModelTable.create;
        try
          oTable.Description := oConn.ColStringByName[FN_DESCRIPTIONP];
          oTable.ID := oConn.ColIntegerByName[FN_TABLEID];
          oTables.Add(oTable.Link);
        finally
          oTable.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
    oConn.sql := 'select ' + FN_TABLEID + ', ' + FN_TABLEVALS + ', ' + FN_DESCRIPTIONP + ', ' + GetSortNumName + ' from ' + NamePrefix + TN_TABLEVALS + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oTableItem := THL7V2ModelTableItem.create;
        try
          oTableItem.ID := oConn.ColIntegerByName[GetSortNumName];
          oTableItem.Code := oConn.ColStringByName[FN_TABLEVALS];
          oTableItem.Description := oConn.ColStringByName[FN_DESCRIPTIONP];
          oTables.GetByID(oConn.ColIntegerByName[FN_TABLEID]).Items.Add(oTableItem.Link);
        finally
          oTableItem.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables);
var
  oConn: TFDBConnection;
  iTable : Integer;
  iItem : Integer;
  oTable : THL7V2ModelTable;
  oTableItem : THL7V2ModelTableItem;
begin
  oConn := GetConnection('AddTables');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_TABLES + ' (' + FN_VERSION + ', ' + FN_TABLEID + ', ' + FN_DESCRIPTIONP + ') values (:ver, :id, :desc)';
    oConn.Prepare;
    try
      for iTable := 0 to oTables.Count - 1 do
        begin
        oTable := oTables[iTable];
        oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
        oConn.BindInteger('id', oTable.ID);
        oConn.BindString('desc', oTable.Description);
        oConn.Execute;
        end;
    finally
      oConn.terminate;
    end;
    oConn.sql := 'insert into ' + NamePrefix + TN_TABLEVALS + ' (' + FN_VERSION + ', ' + FN_TABLEID + ', ' + FN_TABLEVALS + ', ' + FN_DESCRIPTIONP + ', ' + GetSortNumName + ') values ' +
                '(:ver, :id, :val, :desc, :num)';
    oConn.Prepare;
    try
      for iTable := 0 to oTables.Count - 1 do
        begin
        oTable := oTables[iTable];
        for iItem := 0 to oTable.Items.count - 1 do
          begin
          oTableItem := oTable.Items[iItem];
          oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
          oConn.BindInteger('id', oTable.ID);
          oConn.BindString('val', oTableItem.Code);
          oConn.BindString('desc', oTableItem.Description);
          oConn.BindInteger('num', oTableItem.ID);
          oConn.Execute;
          end;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
var
  oConn: TFDBConnection;
  oEvent : THL7V2ModelEvent;
begin
  oConn := GetConnection('LoadEvents');
  try
    oConn.sql := 'select ' + FN_EVNTCODE + ', ' + FN_DESCRIPTION + ' from ' + NamePrefix + TN_EVENTS + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oEvent := THL7V2ModelEvent.create;
        try
          oEvent.Name := oConn.ColStringByName[FN_EVNTCODE];
          oEvent.Description := oConn.ColStringByName[FN_DESCRIPTION];
          oEvents.Add(oEvent.Link);
        finally
          oEvent.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
var
  oConn: TFDBConnection;
  iEvent : Integer;
  oEvent : THL7V2ModelEvent;
begin
  oConn := GetConnection('AddEvents');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_EVENTS + ' (' + FN_VERSION + ', ' + FN_EVNTCODE + ', ' + FN_DESCRIPTION + ') values ' +
      '(:ver, :code, :desc)';
    oConn.Prepare;
    try
      for iEvent := 0 to oEvents.Count - 1 do
        begin
        oEvent := oEvents[iEvent];
        oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
        oConn.BindString('code', oEvent.Name);
        oConn.BindString('desc', oEvent.Description);
        oConn.Execute;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
var
  oConn: TFDBConnection;
  oEvent : THL7V2ModelEvent;
  oMessage : THL7V2ModelEventMessage;
begin
  oConn := GetConnection('LoadEventMessages');
  try
    oConn.sql := 'select ' + FN_EVNTCODE + ', ' + FieldNum + ', ' + FN_MSGTYP_SEND + ', ' + FN_MSGTYP_RETN + ', ' + FN_MSGSTRUC_SEND + ', ' + FN_MSGSTRUC_RETN + ', ' + FieldNum + ' from ' + NamePrefix + TN_EVENTMSGTYP + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oEvent := oEvents.GetByName(oConn.ColStringByName[FN_EVNTCODE]);
        oMessage := THL7V2ModelEventMessage.create;
        try
          oMessage.Message := oConn.ColStringByName[FN_MSGTYP_SEND];
          oMessage.Structure := oConn.ColStringByName[FN_MSGSTRUC_SEND];
          oMessage.Reply := oConn.ColStringByName[FN_MSGTYP_RETN];
          oMessage.ReplyStructure := oConn.ColStringByName[FN_MSGSTRUC_RETN];
          oEvent.Messages.add(oMessage.Link);
        finally
          oMessage.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
var
  oConn: TFDBConnection;
  iEvent : Integer;
  oEvent : THL7V2ModelEvent;
  iMessage : Integer;
  oMessage : THL7V2ModelEventMessage;
  iNum : integer;
begin
  iNum := 0;
  oConn := GetConnection('AddEvents');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_EVENTMSGTYP + ' (' + FN_VERSION + ', ' + FN_EVNTCODE + ', ' +
      FieldNum + ', ' + FN_MSGTYP_SEND + ', ' + FN_MSGTYP_RETN + ', ' + FN_MSGSTRUC_SEND + ', ' + FN_MSGSTRUC_RETN + ') values ' +
      '(:ver, :code, :num, :sm, :rm, :ss, :rs)';
    oConn.Prepare;
    try
      for iEvent := 0 to oEvents.Count - 1 do
        begin
        oEvent := oEvents[iEvent];
        for iMessage := 0 to oEvent.Messages.Count - 1 do
          begin
          oMessage := oEvent.Messages[iMessage];
          oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
          oConn.BindInteger('num', iNum);
          oConn.BindString('code', oEvent.Name);
          oConn.BindString('sm', oMessage.Message);
          oConn.BindString('ss', oMessage.Structure);
          oConn.BindString('rm', oMessage.Reply);
          oConn.BindString('rs', oMessage.ReplyStructure);
          oConn.Execute;
          inc(iNum)
          end;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.LoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
var
  oConn: TFDBConnection;
  oMessageStructure : THL7V2ModelMessageStructure;
begin
  oConn := GetConnection('LoadMessageStructures');
  try
    oConn.sql := 'select ' + FN_MSG_STRUCT + ', ' + FN_DESCRIPTION + ', ' + FN_XMPL_EVENT + ', ' + FN_XMPL_MTYPE + ', ' + FixActionFieldName(oConn.Owner.Platform) + ' from ' + NamePrefix + TN_MSGSTRUCTS + ' where ' + GetVersionSelect(aVersion);
    oConn.Prepare;
    try
      oConn.Execute;
      while oConn.FetchNext do
        begin
        oMessageStructure := THL7V2ModelMessageStructure.create;
        try
          oMessageStructure.Name := oConn.ColStringByName[FN_MSG_STRUCT];
          oMessageStructure.Description := oConn.ColStringByName[FN_DESCRIPTION];
          oMessageStructure.ExampleEvent := oConn.ColStringByName[FN_XMPL_EVENT];
          oMessageStructure.ExampleMsgType := oConn.ColStringByName[FN_XMPL_MTYPE];
          oMessageStructure.Action := oConn.ColStringByName[FixActionFieldName(oConn.Owner.Platform)];
          oMessageStructures.Add(oMessageStructure.Link);
        finally
          oMessageStructure.Free;
        end;
        end;
    finally
      oConn.Terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.AddMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
var
  oConn: TFDBConnection;
  iMessageStructure : Integer;
  oMessageStructure : THL7V2ModelMessageStructure;
begin
  oConn := GetConnection('AddMessageStructures');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_MSGSTRUCTS + ' (' + FN_VERSION + ', ' + FN_MSG_STRUCT + ', ' +
      FN_DESCRIPTION + ', ' + FN_XMPL_EVENT + ', ' +
      FN_XMPL_MTYPE + ', ' + FixActionFieldName(oConn.Owner.Platform) + ') values ' +
      '(:ver, :struc, :desc, :xe, :xm, :act)';
    oConn.Prepare;
    try
      for iMessageStructure := 0 to oMessageStructures.Count - 1 do
        begin
        oMessageStructure := oMessageStructures[iMessageStructure];
        oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
        oConn.BindString('struc', oMessageStructure.Name);
        oConn.BindString('desc', oMessageStructure.Description);
        oConn.BindString('xe', oMessageStructure.ExampleEvent);
        oConn.BindString('xm', oMessageStructure.ExampleMsgType);
        oConn.BindString('act', oMessageStructure.Action);
        oConn.Execute;
        end;
    finally
      oConn.terminate;
    end;
  finally
    YieldConnection(oConn);
  end;
end;

procedure THL7V2OdbcDictionary.FindSegment(oStructure : THL7V2ModelMessageStructure; oStack : THL7V2ModelSegmentGroups; oConn : TFDBConnection);
var
  oFocus : THL7V2ModelSegmentGroup;
  oGroup : THL7V2ModelSegmentGroup;
  sName : String;
begin
  oFocus := oStack[0];
  sName := oConn.ColStringByName[FN_SEGCODE];

  if StringArrayExistsSensitive(['{', '<', '[', '{[', '[{'], sName) then
    begin
    if sName = '{' then
      oGroup := THL7V2ModelSegmentGroup.create(oConn.ColStringByName[FN_GROUPNAME], False, True, gtGroup)
    else if sName = '<' then
      oGroup := THL7V2ModelSegmentGroup.create(oConn.ColStringByName[FN_GROUPNAME], False, True, gtChoice)
    else if sName = '[' then
      oGroup := THL7V2ModelSegmentGroup.create(oConn.ColStringByName[FN_GROUPNAME], True, False, gtGroup)
    else // sName = '{[' or = '[{'
      oGroup := THL7V2ModelSegmentGroup.create(oConn.ColStringByName[FN_GROUPNAME], True, True, gtGroup);
    try
      oFocus.Children.add(oGroup.Link);
      oStack.Insert(0, oGroup.Link);
    finally
      oGroup.Free;
    end;
    end
  else if StringArrayExistsSensitive(['}]', ']}', '}', ']', '>'], sName) then
    oStack.DeleteByIndex(0)
  else if sName = '|' then
    assert(oFocus.GroupType = gtChoice, 'Segment | encountered when not in Choice Segment')  // we can safely ignore this on the assumption that there will be only one segment per choice item there is no mandate for this but it is true in all existing choices and there won't be any more
  else
    begin
    oGroup := THL7V2ModelSegmentGroup.create(oConn.ColStringByName[FN_SEGCODE], oConn.ColIntegerByName[FN_OPTNAL] = 1, oConn.ColIntegerByName[FN_REPNAL] = 1, gtSingle);
    try
      oFocus.Children.add(oGroup.Link);
    finally
      oGroup.Free;
    end;
    end;
end;

procedure THL7V2OdbcDictionary.LoadSegmentMaps(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
var
  oConn: TFDBConnection;
  sLastName : String;
  oStructure : THL7V2ModelMessageStructure;
  oStack : THL7V2ModelSegmentGroups;
begin
  oStructure := nil;
  oStack := THL7V2ModelSegmentGroups.create;
  try
    oConn := GetConnection('LoadSegmentMaps');
    try
      oConn.sql := 'select ' + FN_MSG_STRUCT + ', ' + FieldNum + ', ' + FN_SEGCODE + ', ' + FN_GROUPNAME + ', ' + FN_REPNAL + ', ' + FN_OPTNAL +
        ' from ' + NamePrefix + TN_STRUCTSEGS + ' where ' + GetVersionSelect(aVersion)+' order by ' + FN_MSG_STRUCT + ', ' + FieldNum;
      oConn.Prepare;
      try
        sLastName := '';
        oConn.Execute;
        while oConn.FetchNext do
          begin
          if oConn.ColStringByName[FN_MSG_STRUCT] <> sLastName then
            begin
            sLastName := oConn.ColStringByName[FN_MSG_STRUCT];
            oStructure := oMessageStructures.GetByName(sLastName);
            oStructure.SegmentMap := THL7V2ModelSegmentGroup.create;
            oStructure.SegmentMap.Code := sLastName;
            oStructure.SegmentMap.GroupType := gtGroup;
            oStack.Clear;
            oStack.Add(oStructure.SegmentMap.Link);
            end;
          if assigned(oStructure) then
            FindSegment(oStructure, oStack, oConn);
          end;
      finally
        oConn.Terminate;
      end;
      oConn.sql := 'select ' + FN_EVNTCODE + ', ' + FN_MSGTYPE + ', ' + FieldNum + ', ' + FN_SEGCODE + ', ' + FN_GROUPNAME + ', ' + FN_REPNAL + ', ' + FN_OPTNAL +
        ' from ' + NamePrefix + TN_EVNTMSGSEGS + ' where ' + GetVersionSelect(aVersion)+' order by ' + FN_MSGTYPE + ', ' + FN_EVNTCODE + ', ' + FieldNum;
      oConn.Prepare;
      try
        sLastName := '';
        oConn.Execute;
        while oConn.FetchNext do
          begin
          if oConn.ColStringByName[FN_MSGTYPE] + '_' + oConn.ColStringByName[FN_EVNTCODE] <> sLastName then
            begin
            sLastName := oConn.ColStringByName[FN_MSGTYPE] + '_' + oConn.ColStringByName[FN_EVNTCODE];
            oStructure := oMessageStructures.GetByName(sLastName);
            if not assigned(oStructure) then
              begin
              oStructure := THL7V2ModelMessageStructure.create;
              try
                oStructure.Name := sLastName;
                oStructure.Description := '(Implicitly Created by HL7Connect)';
                oMessageStructures.Add(oStructure.Link);
              finally
                oStructure.Free;
              end;
              end;
            oStructure.SegmentMap := THL7V2ModelSegmentGroup.create;
            oStructure.SegmentMap.GroupType := gtGroup;
            oStructure.SegmentMap.Code := sLastName;
            oStack.Clear;
            oStack.Add(oStructure.SegmentMap.Link);
            end;
          if assigned(oStructure) then
            FindSegment(oStructure, oStack, oConn);
          end;
      finally
        oConn.Terminate;
      end;
    finally
      YieldConnection(oConn);
    end;
  finally
    oStack.Free;
  end;
end;

procedure THL7V2OdbcDictionary.SaveSegmentGroup(oConn: TFDBConnection; var iNum : integer; oGroup : THL7V2ModelSegmentGroup);
var
  sCode : String;
  iLoop : integer;
begin
  case oGroup.GroupType of
    gtSingle :
        begin
        inc(iNum);
        oConn.BindString('seg', oGroup.Code);
        oConn.BindString('name', '');
        oConn.BindInteger('num', iNum);
        oConn.BindIntegerFromBoolean('rep', oGroup.Repeating);
        oConn.BindIntegerFromBoolean('opt', oGroup.Optional);
        oConn.Execute;
        end;
    gtGroup :
        begin
        inc(iNum);
        sCode := '.';
        if oGroup.Optional then
          sCode := '[';
        if oGroup.Repeating then
          sCode := sCode + '{';
        oConn.BindString('seg', sCode);
        oConn.BindString('name', oGroup.Code);
        oConn.BindIntegerFromBoolean('rep', False);
        oConn.BindIntegerFromBoolean('opt', False);
        oConn.BindInteger('num', iNum);
        oConn.Execute;
        for iLoop := 0 to oGroup.Children.count - 1 do
          SaveSegmentGroup(oConn, iNum, oGroup.Children[iLoop]);
        inc(iNum);
        sCode := '';
        if oGroup.Repeating then
          sCode := sCode + '}';
        if oGroup.Optional then
          sCode := sCode + ']';
        oConn.BindString('seg', sCode);
        oConn.BindString('name', '');
        oConn.BindIntegerFromBoolean('rep', False);
        oConn.BindIntegerFromBoolean('opt', False);
        oConn.BindInteger('num', iNum);
        oConn.Execute;
        end;
    gtChoice :
        begin
        inc(iNum);
        oConn.BindString('seg', '<');
        oConn.BindString('name', oGroup.Code);
        oConn.BindIntegerFromBoolean('rep', False);
        oConn.BindIntegerFromBoolean('opt', False);
        oConn.BindInteger('num', iNum);
        oConn.Execute;
        for iLoop := 0 to oGroup.Children.count - 1 do
          begin
          SaveSegmentGroup(oConn, iNum, oGroup.Children[iLoop]);
          if iLoop <> oGroup.Children.count - 1 then
            begin
            inc(iNum);
            oConn.BindString('seg', '|');
            oConn.BindString('name', oGroup.Code);
            oConn.BindIntegerFromBoolean('rep', False);
            oConn.BindIntegerFromBoolean('opt', False);
            oConn.BindInteger('num', iNum);
            oConn.Execute;
            end;
          end;
        inc(iNum);
        oConn.BindString('seg', '>');
        oConn.BindString('name', '');
        oConn.BindIntegerFromBoolean('rep', False);
        oConn.BindIntegerFromBoolean('opt', False);
        oConn.BindInteger('num', iNum);
        oConn.Execute;
        end;
  else
    assert(false, 'unknown type for grouptype');
  end;
end;

procedure THL7V2OdbcDictionary.AddSegmentMaps(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
var
  oConn: TFDBConnection;
  oStructure : THL7V2ModelMessageStructure;
  iLoop : integer;
  iNum : integer;
begin
  oConn := GetConnection('AddSegmentMaps');
  try
    oConn.sql := 'insert into ' + NamePrefix + TN_STRUCTSEGS + ' (' + FN_VERSION + ', ' + FN_MSG_STRUCT + ', ' + FieldNum + ', ' + FN_SEGCODE + ', ' + FN_GROUPNAME + ', ' + FN_REPNAL + ', ' + FN_OPTNAL + ') values ' +
                 '(:ver, :struc, :num, :seg, :name, :rep, :opt)';
    oConn.Prepare;
    try
      oConn.BindString('ver', NAMES_HL7V2_VERSION[aVersion]);
      for iLoop := 0 to oMessageStructures.Count - 1 do
        begin
        oStructure := oMessageStructures[iLoop];
        oConn.BindString('struc', oStructure.Name);
        iNum := 0;
        if assigned(oStructure.SegmentMap) then
          SaveSegmentGroup(oConn, iNum, oStructure.SegmentMap);
        end;
    finally
      oConn.terminate;
      end;
  finally
    YieldConnection(oConn);
    end;
end;

function THL7V2OdbcDictionary.GetVersionSelect(aVersion : THL7V2Version): String;
begin
  if GetDBVersion >= '3.0' then
    Result := FN_VERSION_ID+' = (Select '+FN_VERSION_ID+' from  '+NamePrefix+TN_VERSIONS+' where '+FN_VERSION+' = '''+NAMES_HL7V2_VERSION[aVersion]+''')'
  else
    Result := FN_VERSION+' = '''+NAMES_HL7V2_VERSION[aVersion]+''''
end;

function THL7V2OdbcDictionary.FieldNum: String;
begin
  if GetDBVersion >= '3.0' then
    result := FN_FIELDNUM_3
  else
    result := FN_FIELDNUM_2;
end;

function THL7V2OdbcDictionary.GetCompNumName: String;
begin
  if GetDBVersion >= '3.0' then
    result := FN_COMPNUM_3
  else
    result := FN_COMPNUM_2;
end;

function THL7V2OdbcDictionary.GetSortNumName: String;
begin
  if GetDBVersion >= '3.0' then
    result := FN_SORTNUM_3
  else
    result := FN_SORTNUM_2;
end;

procedure THL7V2OdbcDictionary.DoneLoading(aTransferEvent: TOnDictTransferProgress);
begin
  // not interested
end;

function THL7V2OdbcDictionary.GetConnection(usage: String): TFDBConnection;
begin
  result := nil;
  RaiseError('GetConnection', 'Need to override GetConnection in '+ClassName);
end;

function THL7V2OdbcDictionary.GetDBVersion: String;
begin
  result := '';
  RaiseError('GetDBVersion', 'Need to override GetDBVersion in '+ClassName);
end;

function THL7V2OdbcDictionary.NamePrefix: String;
begin
  result := '';
  RaiseError('NamePrefix', 'Need to override NamePrefix in '+ClassName);
end;

procedure THL7V2OdbcDictionary.YieldConnection(oConn: TFDBConnection);
begin
  RaiseError('YieldConnection', 'Need to override YieldConnection in '+ClassName);
end;

{ THL7V2AccessDictionary }

constructor THL7V2AccessDictionary.Create;
begin
  inherited;
end;

constructor THL7V2AccessDictionary.Create(sHL7Dict: String);
begin
  Create;
  FileName := sHL7Dict;
end;

destructor THL7V2AccessDictionary.Destroy;
begin
  Close;
  inherited;
end;

function THL7V2AccessDictionary.GetConnection(usage: String): TFDBConnection;
begin
  if FStmtInUse then
    RaiseError('GetConnection', 'Attempt to reuse stmt');
  FStmtInUse := True;
  Result := FStmt;
end;

function THL7V2AccessDictionary.GetDBVersion: String;
begin
  result := FDBVersion;
end;

procedure THL7V2AccessDictionary.LookupDBVersion;
begin
  FStmt.sql := 'Select * from DBVersion';
  FStmt.prepare;
  try
    FStmt.Execute;
    if FStmt.FetchNext then
      FDBVersion := FStmt.ColStringByName['db_name']
    else
      RaiseError('LookupDBVersion', 'DBVersion is empty');
  finally
    FStmt.Terminate;
  end;
end;

function THL7V2AccessDictionary.NamePrefix: String;
begin
  If FDBVersion >= '3.0' Then
    Result := 'HL7'
  Else
    Result := '';
end;

procedure THL7V2AccessDictionary.PrepareForLoad(wipe: Boolean);
begin
  RaiseError('PrepareForLoad', 'You cannot transfer the Data Dictionary into an MSAccess database');
end;

function THL7V2AccessDictionary.SourceDescription(fulldetails: Boolean): String;
begin
  if fulldetails then
    Result := 'Microsoft Access Database from HL7 at ' + FFilename
  else
    Result := 'HL7 Access';
end;

procedure THL7V2AccessDictionary.SetFileName(const Value: String);
begin
  Close;
  FFileName := Value;
  FManager := TFDBOdbcManager.Create('HL7dict', kdbAccess, 1, 0, 'Microsoft Access Driver (*.mdb, *.accdb)', '', FFileName, '', '');
  FStmt := Fmanager.GetConnection('HL7 Dictionary');
  LookupDBVersion;
  FStmtInUse := False;
end;

procedure THL7V2AccessDictionary.YieldConnection;
begin
  FStmtInUse := False;
end;

procedure THL7V2AccessDictionary.Close;
begin
  if assigned(FStmt) then
    begin
    Fstmt.Terminate;
    Fstmt.Release;
    end;
  if assigned(FManager) then
    FManager.Free;
end;

function THL7V2AccessDictionary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FManager.sizeInBytes);
  inc(result, FStmt.sizeInBytes);
end;

end.
