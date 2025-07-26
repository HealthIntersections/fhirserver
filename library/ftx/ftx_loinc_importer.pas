Unit ftx_loinc_importer;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

Interface

Uses
  SysUtils, Contnrs, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream, fsl_fpc,
  fdb_manager, fdb_sqlite3,
  ftx_loinc_services, ftx_service;


type

  { TCodeInformation }
  TCodeInformation = class (TFslObject)
  private
    FKey : integer;
    FChildren : TKeySet;
  public
    constructor create; override;
    destructor destroy; override;
    function link : TCodeInformation; overload;
  end;

  { TCodeMap }

  TCodeMap = class (TFslMap<TCodeInformation>)
  public
    function addCode(code : String; key : integer; codeList : TFslList<TCodeInformation>) : TCodeInformation;
    function getCode(code : String) : TCodeInformation;
  end;

  { TKeyMap }
  TKeyMap = class (TDictionary<String, Integer>)
  private
    FName : String;
  public
    constructor Create(name : String);

    procedure addKey(code : String; key : integer);
    function getKey(code : String) : integer;
  end;


  { TLoincImporter }

  TLoincImporter = class (TFslObject)
  private
    callback : TInstallerCallback;
    lastmessage : String;
    FStart : TDateTime;
    FFolder : String;
    TotalConcepts : Integer;

    FOutputFile: String;
    FDate: String;
    FVersion: String;

    FStepCount : Integer;
    FLangKey : integer;
    FCodeKey : integer;
    FRelKey : integer;
    FDescKey : integer;
    FPropKey : integer;
    FPropValueKey : integer;
    FPropValues : TDictionary<String, Integer>;

    // working items
    conn, dbCodes, dbRels, dbDesc, dbProps, dbText: TFDBConnection;
    codes : TCodeMap;
    codeList : TFslList<TCodeInformation>;
    rels, langs, statii, dTypes, props : TKeyMap;
    partNames : TDictionary<String, String>;

    procedure Progress(Step : integer; pct : real; msg : String);
    function pv(value : String) : integer;

    procedure CreateTables(step : integer);
    procedure ProcessDescription(codeKey, languageKey, descriptionType : integer; value : String);
    procedure ProcessProperty(codeKey, propertyType : integer; value : String);
    procedure ProcessParts(step : integer);
    procedure ProcessPartItems(items : TStringArray);
    procedure ProcessCodes(step : integer);
    procedure ProcessCodeItems(items : TStringArray);
    procedure ProcessConsumerNames(step : integer);
    procedure ProcessConsumerNameItems(items : TStringArray);
    procedure ProcessLists(step : integer);
    procedure ProcessListItems(items : TStringArray; var list : String);
    procedure ProcessPartLinks(step : integer);
    procedure ProcessPartLinkItems(items : TStringArray);
    procedure ProcessListLinks(step : integer);
    procedure ProcessListLinkItems(items : TStringArray);
    procedure ProcessLanguageVariants(step : integer; list : TStringList);
    procedure ProcessLanguageVariantsItems(items : TStringArray; list : TStringList);
    procedure ProcessLanguage(step : integer;  code : String);
    procedure ProcessLanguageItems(code : String; lk : integer; items : TStringArray);
    procedure ProcessPropertyValues(step: integer);
    procedure ProcessHierarchy(step: integer);
    procedure ProcessHierarchyItems(items : TStringArray);
    procedure StoreClosureTable(step: integer);
  public
    constructor Create; overload;
    destructor Destroy; overload;
    procedure ImportLOINC;

    Property Folder : String read FFolder write FFolder;
    Property OutputFile : String read FOutputFile write FOutputFile;
    Property Version : String read FVersion write FVersion;
    Property Date : String read FDate write FDate;
  end;

function importLoinc(folder, version, date, dest : String; callback : TInstallerCallback = nil) : String;

Implementation


const
  KNOWN_PROPERTY_NAMES : array of String = ['AskAtOrderEntry', 'AssociatedObservations', 'CHANGE_REASON_PUBLIC', 'CHNG_TYPE', 'CLASS', 'CLASSTYPE', 'COMMON_ORDER_RANK', 'COMMON_TEST_RANK', 'COMPONENT', 'CONSUMER_NAME',
      'DefinitionDescription', 'DisplayName', 'EXAMPLE_UCUM_UNITS', 'EXAMPLE_UNITS', 'EXMPL_ANSWERS', 'EXTERNAL_COPYRIGHT_LINK', 'EXTERNAL_COPYRIGHT_NOTICE', 'FORMULA',
      'HL7_ATTACHMENT_STRUCTURE', 'HL7_FIELD_SUBFIELD_ID', 'LONG_COMMON_NAME', 'MAP_TO', 'METHOD_TYP', 'ORDER_OBS', 'PROPERTY', 'PanelType', 'RELATEDNAMES2', 'SCALE_TYP',
      'SHORTNAME', 'STATUS', 'STATUS_REASON', 'STATUS_TEXT', 'SURVEY_QUEST_SRC', 'SURVEY_QUEST_TEXT', 'SYSTEM', 'TIME_ASPCT', 'UNITSREQUIRED', 'ValidHL7AttachmentRequest',
      'VersionFirstReleased', 'VersionLastChanged', 'adjustment', 'analyte', 'analyte-core', 'analyte-divisor', 'analyte-divisor-suffix', 'analyte-gene', 'analyte-numerator',
      'analyte-suffix', 'answer-list', 'answers-for', 'category', 'challenge', 'child', 'count', 'document-kind', 'document-role', 'document-setting', 'document-subject-matter-domain',
      'document-type-of-service', 'parent', 'rad-anatomic-location-imaging-focus', 'rad-anatomic-location-laterality', 'rad-anatomic-location-laterality-presence', 'rad-anatomic-location-region-imaged',
      'rad-guidance-for-action', 'rad-guidance-for-approach', 'rad-guidance-for-object', 'rad-guidance-for-presence', 'rad-maneuver-maneuver-type', 'rad-modality-modality-subtype',
      'rad-modality-modality-type', 'rad-pharmaceutical-route', 'rad-pharmaceutical-substance-given', 'rad-reason-for-exam', 'rad-subject', 'rad-timing', 'rad-view-aggregation',
      'rad-view-view-type', 'search', 'super-system', 'system-core', 'time-core', 'time-modifier',
      'Answer', 'AnswerList'];

function importLoinc(folder, version, date, dest : String; callback : TInstallerCallback = nil) : String;
var
  imp : TLoincImporter;
begin
  if FileExists(dest) then
    DeleteFile(dest);
  imp := TLoincImporter.Create;
  try
    imp.callback := callback;
    imp.progress(0, 0, 'Import LOINC from '+folder);
    imp.Folder := folder;
    imp.Version := version;
    result := dest;
    imp.outputFile := result;
    imp.ImportLOINC;
    imp.progress(16, 0, 'Done '+inttostr(imp.TotalConcepts)+' Concepts');
  finally
    imp.free;
  end;
end;

constructor TLoincImporter.Create;
begin
  inherited create;
  FPropValues := TDictionary<String, Integer>.create;
  partNames := TDictionary<String, String>.create;
  codes := TCodeMap.create;
  statii := TKeyMap.create('Status');
  langs := TKeyMap.create('Language');
  rels := TKeyMap.create('Relationship');
  dTypes := TKeyMap.create('DescriptionType');
  props := TKeyMap.create('PropertyType');
  codeList := TFslList<TCodeInformation>.create;

  FStepCount := 30;
end;

destructor TLoincImporter.Destroy;
begin
  FPropValues.free;
  codeList.free;
  codes.free;
  statii.free;
  langs.free;
  rels.free;
  dTypes.free;
  props.free;
  partNames.free;
  inherited;
end;

procedure TLoincImporter.ImportLOINC;
var
  db : TFDBManager;
  st : TStringList;
  s : string;
  i : integer;
begin
  st := TStringList.create;
  try
    db := TFDBSQLiteManager.create('db', FOutputFile, false, true, 10);
    try
      conn := db.GetConnection('definitions');
      dbCodes := db.GetConnection('codes');
      dbRels := db.GetConnection('relationships');
      dbDesc := db.GetConnection('descriptions');
      dbProps := db.GetConnection('properties');
      dbText := db.GetConnection('text');
      try
        CreateTables(1);

        dbCodes.sql := 'Insert into Codes (CodeKey, Code, Type, RelationshipKey, StatusKey, Description) values (:ck, :c, :t, :rk, :sk, :d)';
        dbCodes.prepare;
        dbRels.sql := 'Insert into Relationships (RelationshipKey, RelationshipTypeKey, SourceKey, TargetKey, StatusKey) values (:rk, :rtk, :sk, :tk, :stk)';
        dbRels.prepare;
        dbDesc.sql := 'Insert into Descriptions (DescriptionKey, CodeKey, LanguageKey, DescriptionTypeKey, Value) values (:dk, :ck, :lk, :tk, :v)';
        dbDesc.prepare;
        dbProps.sql := 'Insert into Properties (PropertyKey, PropertyTypeKey, CodeKey, PropertyValueKey) values (:pk, :ptk, :ck, :v)';
        dbProps.prepare;
        dbText.sql := 'Insert into TextIndex (CodeKey, Type, Lang, Text) values (:ck, :tk, :lk, :t)';
        dbText.prepare;

        ProcessLanguageVariants(2, st);
        FStepCount := 12 + st.count;
        ProcessParts(3);
        ProcessCodes(4);
        ProcessConsumerNames(5);
        ProcessLists(6);
        ProcessPartLinks(7);
        ProcessListLinks(8);
        ProcessHierarchy(9);
        ProcessPropertyValues(10);
        storeClosureTable(11);
        for i := 0 to st.count - 1 do
          ProcessLanguage(12+i, st[i]);

        dbCodes.terminate;
        dbRels.terminate;
        dbDesc.terminate;
        dbProps.terminate;
        dbText.terminate;

        dbCodes.Release;
        dbRels.Release;
        dbDesc.terminate;
        dbProps.terminate;
        dbText.terminate;
        conn.Release;
      except
        on e : Exception do
        begin
          dbCodes.Error(e);
          dbRels.Error(e);
          dbDesc.Error(e);
          dbProps.Error(e);
          conn.Error(e);
          raise;
        end;
      end;
    finally
      db.free;
    end;
  finally
    st.free;
  end;
end;

procedure TLoincImporter.Progress(Step : integer; pct : real; msg : String);
begin
  if (assigned(callback)) then
  begin
    if msg = '' then
      msg := lastmessage;
    pct := ((step / FStepCount) * 100) + (pct * (100 / 16));
    callback(trunc(pct), Msg);
    lastmessage := msg;
  end
  else if (msg <> '') then
  begin
    Writeln('           '+DescribePeriod(now - FStart));
    write('#'+inttostr(step)+' '+msg)
  end
  else
    write('.');
end;

function TLoincImporter.pv(value: String): integer;
begin
  if not FPropValues.TryGetValue(value, result) then
  begin
    inc(FPropValueKey);
    result := FPropValueKey;
    FPropValues.add(value, result);
  end;
end;

function checkPropName(s : String) : String;
begin
  if not StringArrayExists(KNOWN_PROPERTY_NAMES, s) then
    raise EFslException.create('Unknown Property Name: '+s);
  result := s;
end;
               
function adjustPropName(s : String) : String;
begin
  if StringArrayExistsSensitive(KNOWN_PROPERTY_NAMES, s) then
    result := s
  else if (s = 'ADJUSTMENT') then
    result := 'adjustment'
  else if (s = 'CHALLENGE') then
    result := 'challenge'
  else if (s = 'COUNT') then
    result := 'count'
  else if (s = 'DIVISORS') then
    result := 'analyte-divisor'
  else if (s = 'Document.Kind') then
    result := 'document-kind'
  else if (s = 'Document.Role') then
    result := 'document-role'
  else if (s = 'Document.Setting') then
    result := 'document-setting'
  else if (s = 'Document.SubjectMatterDomain') then
    result := 'document-subject-matter-domain'
  else if (s = 'Document.TypeOfService') then
    result := 'document-type-of-service'
  else if (s = 'GENE') then
    result := 'analyte-gene'
  else if (s = 'METHOD') then
    result := 'METHOD_TYP'
  else if (s = 'Rad.Anatomic Location.Imaging Focus') then
    result := 'rad-anatomic-location-imaging-focus'
  else if (s = 'Rad.Anatomic Location.Laterality') then
    result := 'rad-anatomic-location-laterality'
  else if (s = 'Rad.Anatomic Location.Laterality.Presence') then
    result := 'rad-anatomic-location-laterality-presence'
  else if (s = 'Rad.Anatomic Location.Region Imaged') then
    result := 'rad-anatomic-location-region-imaged'
  else if (s = 'Rad.Guidance for.Action') then
    result := 'rad-guidance-for-action'
  else if (s = 'Rad.Guidance for.Approach') then
    result := 'rad-guidance-for-approach'
  else if (s = 'Rad.Guidance for.Object') then
    result := 'rad-guidance-for-object'
  else if (s = 'Rad.Guidance for.Presence') then
    result := 'rad-guidance-for-presence'
  else if (s = 'Rad.Maneuver.Maneuver Type') then
    result := 'rad-maneuver-maneuver-type'
  else if (s = 'Rad.Modality.Modality Subtype') then
    result := 'rad-modality-modality-subtype'
  else if (s = 'Rad.Modality.Modality Type') then
    result := 'rad-modality-modality-type'
  else if (s = 'Rad.Pharmaceutical.Route') then
    result := 'rad-pharmaceutical-route'
  else if (s = 'Rad.Pharmaceutical.Substance Given') then
    result := 'rad-pharmaceutical-substance-given'
  else if (s = 'Rad.Reason for Exam') then
    result := 'rad-reason-for-exam'
  else if (s = 'Rad.Subject') then
    result := 'rad-subject'
  else if (s = 'Rad.Timing') then
    result := 'rad-timing'
  else if (s = 'Rad.View.Aggregation') then
    result := 'rad-view-aggregation'
  else if (s = 'Rad.View.View Type') then
    result := 'rad-view-view-type'
  else if (s = 'SCALE') then
    result := 'SCALE_TYP'
  else if (s = 'SUFFIX') then
    result := 'analyte-suffix'
  else if (s = 'SUPER SYSTEM') then
    result := 'super-system'
  else if (s = 'TIME') then
    result := 'TIME_ASPCT'
  else if (s = 'TIME MODIFIER') then
    result := 'time-modifier'
  else
    raise EFslException.create('Unknown Property Name: '+s);
end;

procedure TLoincImporter.CreateTables(step: integer);
var
  sql : String;
  procedure addEntry(tblname, keyName, valueName : String; map : TKeyMap; code : String; key : integer);
  begin
    conn.ExecSQL('Insert into '+tblName+' ('+keyName+', '+valueName+') values ('''+inttostr(key)+''', '''+code+''')');
    if (map <> nil) then
      map.addKey(code, key);
  end;

begin
  sql := 'CREATE TABLE Config ('+
    '`ConfigKey` int NOT NULL, '+
    '`Value` varchar(15) NOT NULL, '+
    'PRIMARY KEY (`ConfigKey`))';
  conn.ExecSQL(sql);

  sql := 'Insert into Config (ConfigKey, Value) values (1, ''c3c89b66-5930-4aa2-8962-124561a5f8c1'')';
  conn.ExecSQL(sql);
  sql := 'Insert into Config (ConfigKey, Value) values (2, '''+FVersion+''')';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE Types ('+
    '`TypeKey` int NOT NULL, '+
    '`Code` varchar(15) NOT NULL, '+
    'PRIMARY KEY (`TypeKey`))';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE Languages ('+
    '`LanguageKey` int NOT NULL, '+
    '`Code` varchar(15) NOT NULL, '+
    '`Description` varchar(255) NOT NULL, '+
    'PRIMARY KEY (`LanguageKey`))';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE StatusCodes ('+
    '`StatusKey` int NOT NULL, '+
    '`Description` varchar(255) NOT NULL, '+
    'PRIMARY KEY (`StatusKey`))';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE RelationshipTypes ('+
    '`RelationshipTypeKey` int NOT NULL, '+
    '`Description` varchar(15) NOT NULL, '+
    'PRIMARY KEY (`RelationshipTypeKey`))';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE DescriptionTypes ('+
    '`DescriptionTypeKey` int NOT NULL, '+
    '`Description` varchar(15) NOT NULL, '+
    'PRIMARY KEY (`DescriptionTypeKey`))';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE PropertyTypes ('+
    '`PropertyTypeKey` int NOT NULL, '+
    '`Description` varchar(15) NOT NULL, '+
    'PRIMARY KEY (`PropertyTypeKey`))';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE Codes ('+
    '`CodeKey` int NOT NULL, '+
    '`Code` varchar(15) NOT NULL, '+
    '`Type` int NOT NULL, '+
    '`RelationshipKey` int NULL, '+
    '`StatusKey` int NOT NULL, '+
    '`Description` varchar NOT NULL, '+
    'PRIMARY KEY (`CodeKey`))';
  conn.ExecSQL(sql);

  sql := 'CREATE Unique Index CodesCode on Codes (Code)';
  conn.ExecSQL(sql);
  conn.ExecSQL('CREATE INDEX idx_codes_statuskey ON Codes (StatusKey)');
  conn.ExecSQL('CREATE INDEX idx_codes_codekey_statuskey ON Codes (CodeKey, StatusKey)');

  sql := 'CREATE TABLE Relationships ('+
    '`RelationshipKey` int NOT NULL, '+
    '`RelationshipTypeKey` int NOT NULL, '+
    '`SourceKey` int NOT NULL, '+
    '`TargetKey` int NOT NULL, '+
    '`StatusKey` int NOT NULL, '+
    'PRIMARY KEY (`RelationshipKey`))';
  conn.ExecSQL(sql);
  sql := 'CREATE Index RelationshipsSource on Relationships (RelationshipTypeKey, SourceKey)';
  conn.ExecSQL(sql);
  sql := 'CREATE Index RelationshipsTarget on Relationships (RelationshipTypeKey, TargetKey)';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE PropertyValues ('+
    '`PropertyValueKey` int NOT NULL, '+
    '`Value` varchar NOT NULL, '+
    'PRIMARY KEY (`PropertyValueKey`))'; 
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE Properties ('+
    '`PropertyKey` int NOT NULL, '+
    '`PropertyTypeKey` int NOT NULL, '+
    '`CodeKey` int NOT NULL, '+
    '`PropertyValueKey` int NOT NULL, '+
    'PRIMARY KEY (`PropertyKey`))';
  conn.ExecSQL(sql);
  sql := 'CREATE Index PropertiesCode1 on Properties (PropertyTypeKey, CodeKey)';
  conn.ExecSQL(sql);
  sql := 'CREATE Index PropertiesCode2 on Properties (CodeKey, PropertyTypeKey)';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE Descriptions ('+
    '`DescriptionKey` int NOT NULL, '+
    '`CodeKey` int NOT NULL, '+
    '`LanguageKey` int NOT NULL, '+
    '`DescriptionTypeKey` int NOT NULL, '+
    '`Value` varchar NOT NULL, '+
    'PRIMARY KEY (`DescriptionKey`))';
  conn.ExecSQL(sql);
  sql := 'CREATE Index DescriptionsCode on Descriptions (CodeKey, LanguageKey)';
  conn.ExecSQL(sql);

  sql := 'CREATE TABLE Closure ('+
    '`AncestorKey` int NOT NULL, '+
    '`DescendentKey` int NOT NULL, '+
    'PRIMARY KEY (`AncestorKey`, `DescendentKey`))';
  conn.ExecSQL(sql);

  sql := 'CREATE VIRTUAL TABLE TextIndex USING fts5(codekey UNINDEXED, type UNINDEXED, lang UNINDEXED, text)';
  conn.ExecSQL(sql);

  addEntry('Types', 'TypeKey', 'Code', nil, 'Code', 1);
  addEntry('Types', 'TypeKey', 'Code', nil, 'Part', 2);
  addEntry('Types', 'TypeKey', 'Code', nil, 'AnswerList', 3);
  addEntry('Types', 'TypeKey', 'Code', nil, 'Answer', 4);


  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'NotStated', 0);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'ACTIVE', 1);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'DEPRECATED', 2);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'TRIAL', 3);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'DISCOURAGED', 4);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'EXAMPLE', 5);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'PREFERRED', 6);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'Primary', 7);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'DocumentOntology', 8);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'Radiology', 9);
  addEntry('StatusCodes', 'StatusKey', 'Description', statii, 'NORMATIVE', 10);
                                                                                    
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, 'N/A', 0);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('adjustment'), 1);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('challenge'), 2);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('CLASS'), 3);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('COMPONENT'), 4);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('count'), 5);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('analyte-divisor'), 6);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('document-kind'), 7);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('document-role'), 8);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('document-setting'), 9);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('document-subject-matter-domain'),10);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('document-type-of-service'),11);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('analyte-gene'),12);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('METHOD_TYP'),13);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('PROPERTY'),14);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-anatomic-location-imaging-focus'),15);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-guidance-for-action'),16);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-guidance-for-approach'),17);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-guidance-for-object'),18);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-guidance-for-presence'),19);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-maneuver-maneuver-type'),20);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-modality-modality-subtype'),21);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-modality-modality-type'),22);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-pharmaceutical-route'),23);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-pharmaceutical-substance-given'),24);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-reason-for-exam'),25);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-subject'),26);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-timing'),27);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-view-aggregation'),28);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-view-view-type'),29);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('SCALE_TYP'),30);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('analyte-suffix'),31);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('super-system'),32);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('SYSTEM'),33);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('TIME_ASPCT'),34);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('time-modifier'),35);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-anatomic-location-laterality'),36);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-anatomic-location-laterality-presence'),37);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, checkPropName('rad-anatomic-location-region-imaged'),38);

  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, 'AnswerList', 39);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, 'Answer', 40);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, 'answers-for', 41);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, 'parent', 42);
  addEntry('RelationshipTypes', 'RelationshipTypeKey', 'Description', rels, 'child', 43);

  addEntry('DescriptionTypes', 'DescriptionTypeKey', 'Description', dTypes, 'LONG_COMMON_NAME', 1);
  addEntry('DescriptionTypes', 'DescriptionTypeKey', 'Description', dTypes, 'SHORTNAME', 2);
  addEntry('DescriptionTypes', 'DescriptionTypeKey', 'Description', dTypes, 'ConsumerName', 3);
  addEntry('DescriptionTypes', 'DescriptionTypeKey', 'Description', dTypes, 'RELATEDNAMES2', 4);
  addEntry('DescriptionTypes', 'DescriptionTypeKey', 'Description', dTypes, 'DisplayName', 5);
  addEntry('DescriptionTypes', 'DescriptionTypeKey', 'Description', dTypes, 'LinguisticVariantDisplayName', 6);

  addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'CLASSTYPE', 1);
  addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'ORDER_OBS', 2);
  addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'EXAMPLE_UNITS', 3);
  addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'EXAMPLE_UCUM_UNITS', 4);
  addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'PanelType', 5);
  addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'AskAtOrderEntry', 6);
  addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'UNITSREQUIRED', 7);
  //addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'CLASS', 8);
  addEntry('PropertyTypes', 'PropertyTypeKey', 'Description', props, 'Copyright', 9);

  conn.ExecSQL('Insert into Languages (LanguageKey, Code, Description) values (1, ''en-US'', ''English (United States)'')');
  langs.addKey('en-US', 1);
end;

procedure TLoincImporter.ProcessDescription(codeKey, languageKey, descriptionType : integer; value : String);
var
  dk : integer;
begin
  if (value <> '') then
  begin
    inc(FDescKey);
    dk := FDescKey;
    dbDesc.BindInteger('dk', dk);
    dbDesc.BindInteger('ck', codeKey);
    dbDesc.BindInteger('lk', languageKey);
    dbDesc.BindInteger('tk', descriptionType);
    dbDesc.BindString('v', value);
    dbDesc.Execute;

    dbText.BindInteger('ck', codeKey);
    dbText.BindInteger('tk', descriptionType);
    dbDesc.BindInteger('lk', languageKey);
    dbText.BindString('t', value);
    dbText.Execute;
  end;
end;

procedure TLoincImporter.ProcessProperty(codeKey, propertyType: integer; value: String);
var
  pk : integer;
begin
  if (value <> '') then
  begin
    inc(FPropKey);
    pk := FPropKey;
    dbProps.BindInteger('pk', pk);
    dbProps.BindInteger('ck', codeKey);
    dbProps.BindInteger('ptk', propertyType);
    dbProps.BindInteger('v', pv(value));
    dbProps.Execute;
  end;
end;

function readLine(src : String; len : integer; var cursor : integer) : String;
var
  start : integer;
  ch : char;
begin
  start := cursor;
  ch := src[cursor];
  while (cursor <= len) and not CharInSet(src[cursor], [#13, #10]) do
    inc(cursor);
  result := src.subString(start-1, cursor-start);
  inc(cursor); // pass the eoln we found
  if (cursor <= len) and CharInSet(src[cursor], [#13, #10]) and (src[cursor] <> src[cursor-1]) then
    inc(cursor);
end;

function csvSplit(line : String; count : integer) : TStringArray;
var
  inQuoted : boolean;
  ch : char;
  i, b, e, l: integer;
  procedure AddCell;
  begin
    if (l < count) then
    begin
      result[l] := line.subString(b, e-b-1);
      inc(l);
    end;
    b := i;
  end;
begin
  SetLength(result, count);
  inQuoted := false;
  i := 1;
  b := 0;
  e := 0;
  l := 0;
  while (i <= line.length) do
  begin
    ch := line[i];
    if not Inquoted and (ch = ',') then
      addCell
    else if (ch <> '"') then
      e := i
    else if inQuoted then
    begin
      e := i;
      InQuoted := false;
    end
    else
    begin
      InQuoted := true;
      b := i;
    end;
    inc(i);
  end;
  addCell;
end;

procedure TLoincImporter.ProcessParts(step: integer);
var
  src, line : String;
  len, cursor, count : Integer;
begin
  Progress(step, 0,'Importing Parts');
  src := FileToString(FilePath([folder, 'AccessoryFiles', 'PartFile', 'Part.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;
  while (cursor < len) do
  begin
    inc(count);
    if count mod 1000 = 0 then
      Progress(step, cursor / len, 'Importing Parts: '+pct(cursor, len));
    line := readLine(src, len, cursor);
    ProcessPartItems(csvSplit(line, 5));
  end;
end;

procedure TLoincImporter.ProcessPartItems(items: TStringArray);
var
  c, d : String;
  ck, t, rk, sk : integer;
begin
  inc(FCodeKey);
  ck := FCodeKey;
  c := items[0];
  t := 2;
  rk := rels.getKey(adjustPropName(items[1]));
  d := items[2];
  sk := statii.getKey(items[4]);
  dbCodes.bindInteger('ck', ck);
  dbCodes.bindString('c', c);
  dbCodes.bindInteger('t', t);
  dbCodes.bindInteger('rk', rk);
  dbCodes.bindInteger('sk', sk);
  dbCodes.bindString('d', d);
  dbCodes.execute;
  codes.addCode(c, ck, codeList);
  partNames.add(items[1]+'.'+items[2], items[0]);

  ProcessDescription(ck, 1, dTypes.getKey('DisplayName'), items[3]);
end;

procedure TLoincImporter.ProcessCodes(step: integer);
var
  src, line : String;
  len, cursor, count : Integer;
begin
  Progress(step, 0,'Importing Codes');
  src := FileToString(FilePath([folder,  'LoincTable', 'Loinc.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;
  while (cursor < len) do
  begin
    inc(count);
    if count mod 1000 = 0 then
      Progress(step, cursor / len, 'Importing Codes: '+pct(cursor, len));
    line := readLine(src, len, cursor);
    ProcessCodeItems(csvSplit(line, 39));
  end;
end;

function descClassType(s : String) : String;
begin
  if (s = '1') then
    result := 'Laboratory class'
  else if (s = '2') then
    result := 'Clinical class'
  else if (s = '3') then
    result := 'Claims attachment'
  else if (s = '4') then
    result := 'Surveys'
  else
    result := s;
end;

procedure TLoincImporter.ProcessCodeItems(items: TStringArray);
var
  c, d, s, clsCode : String;
  ck, t, rk, sk, rtk, tk, stk  : integer;
begin
  inc(FCodeKey);
  ck := FCodeKey;
  c := RemoveQuotes(items[0]);
  t := 1;
  d := RemoveQuotes(items[25]); // long common name
  sk := statii.getKey(items[11]);
  dbCodes.bindInteger('ck', ck);
  dbCodes.bindString('c', c);
  dbCodes.bindInteger('t', t);
  dbCodes.bindNull('rk');
  dbCodes.bindInteger('sk', sk);
  dbCodes.bindString('d', d);
  dbCodes.execute;
  codes.addCode(c, ck, codeList);

  clsCode := partNames['CLASS.'+items[7]];

  inc(FRelKey);
  rk := FRelKey;
  rtk := rels.getKey(adjustPropName('CLASS'));
  sk := ck;
  tk := codes.getCode(clsCode).FKey;
  stk := 0;

  dbRels.bindInteger('rk', rk);
  dbRels.bindInteger('rtk', rtk);
  dbRels.bindInteger('sk', sk);
  dbRels.bindInteger('tk', tk);
  dbRels.bindInteger('stk', stk);
  dbRels.execute;

  ProcessProperty(ck, props.getKey('CLASSTYPE'), descClassType(items[13]));
  ProcessProperty(ck, props.getKey('ORDER_OBS'), items[21]);
  ProcessProperty(ck, props.getKey('EXAMPLE_UNITS'), items[24]);
  ProcessProperty(ck, props.getKey('EXAMPLE_UCUM_UNITS'), items[26]);
  ProcessProperty(ck, props.getKey('PanelType'), items[34]);
  ProcessProperty(ck, props.getKey('AskAtOrderEntry'), items[35]);
  ProcessProperty(ck, props.getKey('UNITSREQUIRED'), items[18]);
  ProcessProperty(ck, props.getKey('Copyright'), items[23]);


  ProcessDescription(ck, 1, dTypes.getKey('LONG_COMMON_NAME'), d);
  ProcessDescription(ck, 1, dTypes.getKey('ConsumerName'), items[12]);
  ProcessDescription(ck, 1, dTypes.getKey('RELATEDNAMES2'), items[19]);
  ProcessDescription(ck, 1, dTypes.getKey('SHORTNAME'), items[20]);
  ProcessDescription(ck, 1, dTypes.getKey('DisplayName'), items[38]);
end;

procedure TLoincImporter.ProcessConsumerNames(step: integer);
var
  src, line : String;
  len, cursor, count : Integer;
begin
  Progress(step, 0,'Importing Consumer Names');
  src := FileToString(FilePath([folder, 'AccessoryFiles',  'ConsumerName', 'ConsumerName.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;
  while (cursor < len) do
  begin
    inc(count);
    if count mod 1000 = 0 then
      Progress(step, cursor / len, 'Importing Consumer Names: '+pct(cursor, len));
    line := readLine(src, len, cursor);
    ProcessConsumerNameItems(csvSplit(line, 2));
  end;
end;


procedure TLoincImporter.ProcessConsumerNameItems(items: TStringArray);
begin
  ProcessDescription(codes.getCode(items[0]).FKey, 1, dTypes.getKey('ConsumerName'), items[1]);
end;


procedure TLoincImporter.ProcessLists(step: integer);
var
  src, line : String;
  len, cursor, count : Integer; 
  list : String;
begin
  Progress(step, 0,'Importing Answer Lists');
  src := FileToString(FilePath([folder, 'AccessoryFiles',  'AnswerFile', 'AnswerList.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;         
  list := '';
  while (cursor < len) do
  begin
    inc(count);
    if count mod 1000 = 0 then
      Progress(step, cursor / len, 'Importing Answer Lists: '+pct(cursor, len));
    line := readLine(src, len, cursor);                            
    ProcessListItems(csvSplit(line, 11), list);
  end;
end;

procedure TLoincImporter.ProcessListItems(items: TStringArray; var list: String);
var
  c, d : String;
  ck1, ck2, t, sk : integer;
  rk, rtk, stk : integer;
begin
  c := RemoveQuotes(items[0]);
  if (c <> list) then
  begin
    list := c;
    inc(FCodeKey);
    ck1 := FCodeKey;
    t := 3;
    d := RemoveQuotes(items[1]); // long common name
    sk := 0;
    dbCodes.bindInteger('ck', ck1);
    dbCodes.bindString('c', c);
    dbCodes.bindInteger('t', t);
    dbCodes.bindNull('rk');
    dbCodes.bindInteger('sk', sk);
    dbCodes.bindString('d', d);
    dbCodes.execute;
    codes.addCode(c, ck1, codeList);

  end
  else
    ck1 := codes.getCode(c).FKey;

  c := RemoveQuotes(items[6]);
  if codes.ContainsKey(c) then
    ck2 := codes.getCode(c).FKey
  else
  begin
    inc(FCodeKey);
    ck2 := FCodeKey;
    t := 4;
    d := RemoveQuotes(items[10]); // long common name
    sk := 0;
    dbCodes.bindInteger('ck', ck2);
    dbCodes.bindString('c', c);
    dbCodes.bindInteger('t', t);
    dbCodes.bindNull('rk');
    dbCodes.bindInteger('sk', sk);
    dbCodes.bindString('d', d);
    dbCodes.execute;
    codes.addCode(c, ck2, codeList);
  end;

  inc(FRelKey);
  rk := FRelKey;
  rtk := rels.getKey(checkPropName('Answer'));
  stk := 0;
  dbRels.bindInteger('rk', rk);
  dbRels.bindInteger('rtk', rtk);
  dbRels.bindInteger('sk', ck1);
  dbRels.bindInteger('tk', ck2);
  dbRels.bindInteger('stk', stk);
  dbRels.execute;

  // reverse relationship
  inc(FRelKey);
  rk := FRelKey;
  rtk := rels.getKey(checkPropName('AnswerList'));
  dbRels.bindInteger('rk', rk);
  dbRels.bindInteger('rtk', rtk);
  dbRels.bindInteger('sk', ck2);
  dbRels.bindInteger('tk', ck1);
  dbRels.bindInteger('stk', stk);
  dbRels.execute;
end;

procedure TLoincImporter.ProcessPartLinks(step: integer);
var
  src, line : String;
  len, cursor, count : Integer;
begin
  Progress(step, 0,'Importing Part Relationships');
  src := FileToString(FilePath([folder, 'AccessoryFiles',  'PartFile', 'LoincPartLink_Primary.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;
  while (cursor < len) do
  begin
    inc(count);
    if count mod 1000 = 0 then
      Progress(step, cursor / len, 'Importing Part Relationships: '+pct(cursor, len));
    line := readLine(src, len, cursor);
    ProcessPartLinkItems(csvSplit(line, 7));
  end;
end;

procedure TLoincImporter.ProcessPartLinkItems(items: TStringArray);
var
  rk, rtk, sk, tk, stk : integer;
begin
  inc(FRelKey);
  rk := FRelKey;
  rtk := rels.getKey(adjustPropName(items[5]));
  sk := codes.getCode(items[0]).FKey;
  tk := codes.getCode(items[2]).FKey;
  stk := statii.getKey(items[6]);

  dbRels.bindInteger('rk', rk);
  dbRels.bindInteger('rtk', rtk);
  dbRels.bindInteger('sk', sk);
  dbRels.bindInteger('tk', tk);
  dbRels.bindInteger('stk', stk);
  dbRels.execute;
end;

procedure TLoincImporter.ProcessListLinks(step: integer);
var
  src, line : String;
  len, cursor, count : Integer;
begin
  Progress(step, 0,'Importing Answer List Links');
  src := FileToString(FilePath([folder, 'AccessoryFiles',  'AnswerFile', 'LoincAnswerListLink.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;
  while (cursor < len) do
  begin
    inc(count);
    if count mod 1000 = 0 then
      Progress(step, cursor / len, 'Importing Answer List Links: '+pct(cursor, len));
    line := readLine(src, len, cursor);
    ProcessListLinkItems(csvSplit(line, 5));
  end;
end;

procedure TLoincImporter.ProcessListLinkItems(items: TStringArray);
var
  rk, rtk, sk, tk, stk : integer;
begin
  inc(FRelKey);
  rk := FRelKey;
  sk := codes.getCode(items[0]).FKey;
  tk := codes.getCode(items[2]).FKey;
  stk := statii.getKey(items[4]);

  rtk := rels.getKey(checkPropName('AnswerList'));
  dbRels.bindInteger('rk', rk);
  dbRels.bindInteger('rtk', rtk);
  dbRels.bindInteger('sk', sk);
  dbRels.bindInteger('tk', tk);
  dbRels.bindInteger('stk', stk);
  dbRels.execute;

  // reverse relationship
  inc(FRelKey);
  rk := FRelKey;
  rtk := rels.getKey(checkPropName('answers-for'));
  dbRels.bindInteger('rk', rk);
  dbRels.bindInteger('rtk', rtk);
  dbRels.bindInteger('sk', tk);
  dbRels.bindInteger('tk', sk);
  dbRels.bindInteger('stk', stk);
  dbRels.execute;

end;

procedure TLoincImporter.ProcessLanguageVariants(step : integer; list : TStringList);
var
  src, line : String;
  len, cursor, count : Integer;
begin
  conn.SQL := 'Insert into Languages (LanguageKey, Code, Description) values (:k, :c, :d)';
  conn.prepare;
  Progress(step, 0,'Importing Languages');
  src := FileToString(FilePath([folder, 'AccessoryFiles', 'LinguisticVariants', 'LinguisticVariants.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;
  while (cursor < len) do
  begin
    inc(count);
    Progress(step, cursor / len, 'Importing Languages: '+pct(cursor, len));
    line := readLine(src, len, cursor);
    ProcessLanguageVariantsItems(csvSplit(line, 4), list);
  end;
  conn.terminate;
end;

procedure TLoincImporter.ProcessLanguageVariantsItems(items : TStringArray; list : TStringList);
var
  k : integer;
  c, d : String;
begin
  k := strToInt(items[0]);
  c := items[1]+'-'+items[2];
  d := items[3];
  conn.bindInteger('k', k);
  conn.bindString('c', c);
  conn.bindString('d', d);
  conn.Execute;
  langs.add(c, k);
  list.add(c);
end;

procedure TLoincImporter.ProcessLanguage(step: integer; code: String);
var
  src, line : String;
  len, cursor, count : Integer;
  lk : integer;
begin
  Progress(step, 0,'Importing Language '+code);
  lk := langs.getKey(code);
  src := FileToString(FilePath([folder,  'AccessoryFiles', 'LinguisticVariants', code.replace('-', '')+inttostr(lk)+'LinguisticVariant.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;
  while (cursor < len) do
  begin
    inc(count);
    if count mod 1000 = 0 then
      Progress(step, cursor / len, 'Importing Language '+code+': '+pct(cursor, len));
    line := readLine(src, len, cursor);
    ProcessLanguageItems(code, lk, csvSplit(line, 12));
  end;
end;

procedure TLoincImporter.ProcessLanguageItems(code: String; lk: integer; items: TStringArray);
var
  ck : integer;
begin
  ck := codes.getCode(items[0]).FKey;

  ProcessDescription(ck, lk, dTypes.getKey('LONG_COMMON_NAME'), items[9]);
  ProcessDescription(ck, lk, dTypes.getKey('RELATEDNAMES2'), items[10]);
  ProcessDescription(ck, lk, dTypes.getKey('SHORTNAME'), items[8]);
  ProcessDescription(ck, lk, dTypes.getKey('LinguisticVariantDisplayName'), items[11]);
end;

procedure TLoincImporter.ProcessPropertyValues(step: integer);
var
  pp : TPair<String,integer>;
begin
  conn.sql := 'Insert into PropertyValues (PropertyValueKey, Value) values (:pk, :v)';
  conn.prepare;
  for pp in FPropValues do
  begin
    conn.BindInteger('pk', pp.Value);
    conn.bindString('v', pp.Key);
    conn.execute;
  end;
  conn.terminate;
end;

procedure TLoincImporter.ProcessHierarchy(step: integer);
var
  src, line : String;
  len, cursor, count : Integer;
  lk : integer;
begin
  Progress(step, 0,'Processing Hierarchy');
  src := FileToString(FilePath([folder, 'AccessoryFiles', 'ComponentHierarchyBySystem', 'ComponentHierarchyBySystem.csv']), TEncoding.UTF8);
  len := src.length;
  cursor := 1;
  readLine(src, len, cursor);
  count := 0;
  while (cursor < len) do
  begin
    inc(count);
    if count mod 1000 = 0 then
      Progress(step, cursor / len, 'Processing Hierarchy: '+pct(cursor, len));
    line := readLine(src, len, cursor);
    ProcessHierarchyItems(csvSplit(line, 12));
  end;
end;

procedure TLoincImporter.ProcessHierarchyItems(items : TStringArray);
var
  ciC, ciP, ci : TCodeInformation;
  rk, rtk, sk, tk, stk : integer;
  s : string;
  c, d : String;
  ck, t : integer;
begin
  // "LP432695-7.LP29693-6.LP343406-7","1","LP343406-7","LP7819-8","Microbiology"
  if not codes.TryGetValue(items[3], ciC) then
  begin
    inc(FCodeKey);
    ck := FCodeKey;
    c := items[3];
    t := 2;
    rk := 0;
    d := items[4];
    sk := 0;
    dbCodes.bindInteger('ck', ck);
    dbCodes.bindString('c', c);
    dbCodes.bindInteger('t', t);
    dbCodes.bindInteger('rk', rk);
    dbCodes.bindInteger('sk', sk);
    dbCodes.bindString('d', d);
    dbCodes.execute;
    ciC := codes.addCode(c, ck, codeList);
  end;
  if items[2] = '' then
    conn.ExecSQL('Insert into Config (ConfigKey, Value) values (3, '''+items[3]+''')')
  else
  begin
    ciP := codes.GetCode(items[2]);


  //  // we do 3 things with this info - store parent and child relationships and generate parent->child closure table for is-a queries

   // 1: parent

    inc(FRelKey);
    rk := FRelKey;
    rtk := rels.getKey(checkPropName('parent'));
    sk := ciC.FKey;
    tk := ciP.FKey;
    stk := 0;
    dbRels.bindInteger('rk', rk);
    dbRels.bindInteger('rtk', rtk);
    dbRels.bindInteger('sk', sk);
    dbRels.bindInteger('tk', tk);
    dbRels.bindInteger('stk', stk);
    dbRels.execute;

    // 2; child
    inc(FRelKey);
    rk := FRelKey;
    rtk := rels.getKey(checkPropName('child'));
    sk := ciP.FKey;
    tk := ciC.FKey;
    stk := 0;
    dbRels.bindInteger('rk', rk);
    dbRels.bindInteger('rtk', rtk);
    dbRels.bindInteger('sk', sk);
    dbRels.bindInteger('tk', tk);
    dbRels.bindInteger('stk', stk);
    dbRels.execute;

    for s in items[0].split(['.']) do
    begin
      ci := codes.getCode(s);
      if (ci.FChildren = nil) then
        ci.FChildren := TKeySet.create;
      ci.FChildren.addKey(ciC.FKey);
    end;
  end;
end;

procedure TLoincImporter.StoreClosureTable(step: integer);
var
  count, k : integer;
  ci : TCodeInformation;
begin
  Progress(step, 0,'Storing Closure Table');
  conn.sql := 'Insert into Closure (AncestorKey, DescendentKey) values (:a, :d)';
  conn.prepare;
  count := 0;
  for ci in codeList do
  begin
    inc(count);
    if count mod 10 = 0 then
      Progress(step, count / codeList.count, 'Storing Closure Table: '+pct(count, codeList.count));
    if (ci.FChildren <> nil) then
    begin
      conn.BindInteger('a', ci.FKey);
      for k in ci.FChildren.Keys do
      begin
        conn.BindInteger('d', k);
        conn.Execute;
      end;
    end;
  end;
  conn.terminate;
end;

{ TKeyMap }

constructor TKeyMap.Create(name: String);
begin
  inherited create;
  FName := name;
end;

procedure TKeyMap.addKey(code: String; key: integer);
begin
  if ContainsKey(code) then
    raise EFslException.create('Duplicate code '+code+' tried to be added to '+FName+' Table')
  else
    Add(code, key);
end;

function TKeyMap.getKey(code: String): integer;
begin
  if not TryGetValue(code, result) then
    raise EFslException.create(code+' not found in '+FName+' Table');
end;

{ TCodeInformation }

constructor TCodeInformation.create;
begin
  inherited create;
end;

destructor TCodeInformation.destroy;
begin
  FChildren.Free;
  inherited destroy;
end;

function TCodeInformation.link: TCodeInformation;
begin
  result := TCodeInformation(inherited link);
end;

{ TCodeMap }

function TCodeMap.addCode(code: String; key: integer; codeList : TFslList<TCodeInformation>) : TCodeInformation;
begin
  result := TCodeInformation.create;
  add(code, result);
  codeList.add(result.link);
  result.FKey := key;
end;

function TCodeMap.getCode(code: String): TCodeInformation;
begin
  if not TryGetValue(code, result) then
    raise EFslException.create(code+' not found in Code Table');
end;

End.

