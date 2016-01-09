unit MPISearch;

interface

uses
  SysUtils,
  ParseMap, StringSupport, EncodeSupport,
  AdvObjects,
  KDBManager,
  FHIRSupport,
  FHIRIndexManagers, FHIRDataStore;

Type
  TMPICertainty = (mcNull, mcCertain, mcProbable, mcPossible);

  TMPISearchProcessor = class (TAdvObject)
  private
    // inputs
    Fparams: TParseMap;
    Frepository: TFHIRDataStore;
    FcompartmentId: string;
    FConnection: TKDBConnection;
    FbaseURL: string;
    Fcompartments: string;
    Ftypekey: integer;
    Findexes: TFHIRIndexInformation;
    Flink_: string;
    Flang: string;
    Fsession: TFHIRSession;
    FKey : String;

    // parameters we have
    FDateOfBirth : String;
    FGender : String;
    FFirstName : String;
    FFamilyName : String;
    FIdentifier : String;


    procedure SetConnection(const Value: TKDBConnection);
    procedure Setindexes(const Value: TFHIRIndexInformation);
    procedure Setparams(const Value: TParseMap);
    procedure Setrepository(const Value: TFHIRDataStore);
    procedure Setsession(const Value: TFHIRSession);

    function baseSQL(sort, score : String; certainty : TMPICertainty) : String;
    function searchTail : String;
    function Indexkey(indexName : String): String;
    procedure runCertainSearch;
    procedure runProbableSearch1;
    procedure runProbableSearch2;
    procedure runProbableSearch3;
    procedure runPossibleSearch1;
    procedure runPossibleSearch2;
  public
    Destructor Destroy; override;

    property typekey : integer read Ftypekey write Ftypekey;
    property compartmentId : string read FcompartmentId write FcompartmentId;
    property compartments : string read Fcompartments write Fcompartments;
    property baseURL : string read FbaseURL write FbaseURL;
    property lang : string read Flang write Flang;
    property params : TParseMap read Fparams write Setparams;
    property indexes : TFHIRIndexInformation read Findexes write Setindexes;
    property repository : TFHIRDataStore read Frepository write Setrepository;
    property session : TFHIRSession read Fsession write Setsession;
    property Connection : TKDBConnection read FConnection write SetConnection;
    property key : String read FKey write FKey;
    property link_ : string read Flink_;

    procedure execute;
  end;

const
  CODES_TMPICertainty : array [TMPICertainty] of String = ('', 'Certain', 'Probable', 'Possible');

implementation

{ TMPISearchProcessor }

function TMPISearchProcessor.baseSQL(sort, score : String; certainty : TMPICertainty) : String;
begin
  result := 'Insert into SearchEntries '+
   '  Select '+
   '    '+key+', ResourceKey, MostRecent as ResourceVersionKey, '''+sort+'''+Id, '+score+', '+inttostr(ord(certainty))+' '+
   '  from '+
   '    Ids '+
   '  where '+
   '    Deleted = 0 and Ids.MasterResourceKey is null and Ids.ResourceTypeKey = '+inttostr(Ftypekey)+' and '+
   '    not (ResourceKey in (Select ResourceKey from SearchEntries where SearchKey = '+key+')) and ';
end;

destructor TMPISearchProcessor.Destroy;
begin
  FConnection.Free;
  Findexes.Free;
  Frepository.Free;
  Fsession.Free;

  inherited;
end;

procedure TMPISearchProcessor.execute;
begin
  // parameters we care about:
  FDateOfBirth := Fparams.GetVar('birthdate').ToLower;
  FGender := Fparams.GetVar('gender').ToLower;
  FFirstName := FParams.GetVar('given').ToLower;
  FFamilyName := FParams.getVar('family').ToLower;
  FIdentifier := FParams.getVar('identifier').ToLower;

  if (FFirstName = '') or (FFamilyName = '') or (FGender = '') then
    raise Exception.Create('You must provide at least given name, family name, and gender for an MPI search');

  Flink_ := '_query=mpi&family='+EncodeMIME(FFamilyName)+'&given='+EncodeMIME(FFirstName)+'&gender='+EncodeMIME(FGender)+'&birthdate='+EncodeMIME(FDateOfBirth)+'&identifier='+EncodeMIME(FIdentifier);

  if (FDateOfBirth <> '') and (FIdentifier <> '') then
    runCertainSearch;
  if (FIdentifier <> '') then
    runProbableSearch1;
  if (FDateOfBirth <> '') then
    runProbableSearch2;
  runProbableSearch3;
  runPossibleSearch1;
  if (FIdentifier <> '') then
    runPossibleSearch2;
end;

function TMPISearchProcessor.Indexkey(indexName: String): String;
begin
  result := inttostr(FIndexes.GetKeyByName(indexName));
end;

procedure TMPISearchProcessor.runCertainSearch;
var
  sql : String;
  DateOfBirthLower : String;
  DateOfBirthHigher : String;
begin
  DateOfBirthLower := FDateOfBirth.Replace('-', '')+'000000';
  DateOfBirthHigher := FDateOfBirth.Replace('-', '')+'235959';

  // all parameters, exact match
  sql := baseSQL('a', '100', mcCertain)+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('family')+' and Value = '''+SQLWrapString(FFamilyName)+''')) and '+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('given')+' and Value = '''+SQLWrapString(FFirstName)+'''))';
  if FGender <> 'either' then
    sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('gender')+' and Value = '''+SQLWrapString(FGender)+'''))';
  sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('identifier')+' and Value = '''+SQLWrapString(FIdentifier)+'''))';
  sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('birthdate')+' and Value >= '''+SQLWrapString(DateOfBirthLower)+''' and Value <= '''+SQLWrapString(DateOfBirthHigher)+'''))';
  sql := sql + searchTail;
  FConnection.ExecSQL(sql);
end;

procedure TMPISearchProcessor.runProbableSearch1;
var
  sql : String;
begin
  // ignore dob
  sql := baseSQL('b', '95', mcProbable)+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('family')+' and Value = '''+SQLWrapString(FFamilyName)+''')) and '+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('given')+' and Value = '''+SQLWrapString(FFirstName)+'''))';
  if FGender <> 'either' then
    sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('gender')+' and Value = '''+SQLWrapString(FGender)+'''))';
  sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('identifier')+' and Value = '''+SQLWrapString(FIdentifier)+'''))';
  sql := sql + searchTail;
  FConnection.ExecSQL(sql);
end;

procedure TMPISearchProcessor.runProbableSearch2;
var
  sql : String;
  DateOfBirthLower : String;
  DateOfBirthHigher : String;
begin
  // ignore identifier, loose date of birth
  DateOfBirthLower := FDateOfBirth.substring(0, 4) + '0000000000';
  DateOfBirthHigher := FDateOfBirth.substring(0, 4) + '1231235959';

  sql := baseSQL('c', '90', mcProbable)+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('family')+' and Value = '''+SQLWrapString(FFamilyName)+''')) and '+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('given')+' and Value = '''+SQLWrapString(FFirstName)+'''))';
  if FGender <> 'either' then
    sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('gender')+' and Value = '''+SQLWrapString(FGender)+'''))';
  sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('birthdate')+' and Value >= '''+SQLWrapString(DateOfBirthLower)+''' and Value <= '''+SQLWrapString(DateOfBirthHigher)+'''))';
  sql := sql + searchTail;
  FConnection.ExecSQL(sql);
end;

procedure TMPISearchProcessor.runProbableSearch3;
var
  sql : String;
begin
  // just name and gender
  sql := baseSQL('d', '80', mcProbable)+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('family')+' and Value = '''+SQLWrapString(FFamilyName)+''')) and '+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('given')+' and Value = '''+SQLWrapString(FFirstName)+'''))';
  sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('gender')+' and Value = '''+SQLWrapString(FGender)+'''))';
  sql := sql + searchTail;
  FConnection.ExecSQL(sql);
end;

procedure TMPISearchProcessor.runPossibleSearch1;
var
  sql : String;
begin
  // just names by soundex and gender
  sql := baseSQL('e', '50', mcPossible)+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('phonetic')+' and Value like '''+SQLWrapString(EncodeNYSIIS(FFamilyName))+'%'')) and '+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('phonetic')+' and Value like '''+SQLWrapString(EncodeNYSIIS(FFirstName))+'%''))';
  if FGender <> 'either' then
    sql := sql + ' and Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('gender')+' and Value = '''+SQLWrapString(FGender)+'''))';
  sql := sql + searchTail;
  FConnection.ExecSQL(sql);
end;

procedure TMPISearchProcessor.runPossibleSearch2;
var
  sql : String;
begin
  // just identifier
  sql := baseSQL('f', '40', mcPossible)+
    'Ids.ResourceKey in (select ResourceKey from IndexEntries where Flag <> 2 and (IndexKey = '+IndexKey('identifier')+' and Value = '''+SQLWrapString(FIdentifier)+'''))';
  sql := sql + searchTail;
  FConnection.ExecSQL(sql);
end;

function TMPISearchProcessor.searchTail: String;
begin
  result := ' order by ResourceKey DESC';
end;

procedure TMPISearchProcessor.SetConnection(const Value: TKDBConnection);
begin
  FConnection.Free;
  FConnection := Value;
end;

procedure TMPISearchProcessor.Setindexes(const Value: TFHIRIndexInformation);
begin
  Findexes.Free;
  Findexes := Value;
end;

procedure TMPISearchProcessor.Setparams(const Value: TParseMap);
begin
  Fparams := Value;
end;

procedure TMPISearchProcessor.Setrepository(const Value: TFHIRDataStore);
begin
  Frepository.Free;
  Frepository := Value;
end;

procedure TMPISearchProcessor.Setsession(const Value: TFHIRSession);
begin
  Fsession.Free;
  Fsession := Value;
end;

end.
