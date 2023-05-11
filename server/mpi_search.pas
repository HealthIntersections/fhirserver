unit mpi_search;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils,
  fsl_base, fsl_http, fsl_utilities,
  fhir_objects,

  
  fdb_manager,
  session, fhir_indexing,
  indexing;

Type
  TMPICertainty = (mcNull, mcCertain, mcProbable, mcPossible);

  TMPISearchProcessor = class (TFslObject)
  private
    // inputs
    Fparams: THTTPParameters;
    FConnection: TFDBConnection;
    FbaseURL: string;
    Ftypekey: integer;
    Findexes: TFHIRIndexInformation;
    Flink_: string;
    Flang: THTTPLanguages;
    Fsession: TFHIRSession;
    FKey : String;

    // parameters we have
    FDateOfBirth : String;
    FGender : String;
    FFirstName : String;
    FFamilyName : String;
    FIdentifier : String;
    FSessionCompartments: TFslList<TFHIRCompartmentId>;
    FCompartment: TFHIRCompartmentId;


    procedure SetConnection(const Value: TFDBConnection);
    procedure Setindexes(const Value: TFHIRIndexInformation);
    procedure Setparams(const Value: THTTPParameters);
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
    procedure SetCompartment(const Value: TFHIRCompartmentId);
    procedure SetSessionCompartments(const Value: TFslList<TFHIRCompartmentId>);
  public
    destructor Destroy; override;

    property typekey : integer read Ftypekey write Ftypekey;
    property compartment : TFHIRCompartmentId read FCompartment write SetCompartment;
    property sessionCompartments : TFslList<TFHIRCompartmentId> read FSessionCompartments write SetSessionCompartments;
    property baseURL : string read FbaseURL write FbaseURL;
    property lang : THTTPLanguages read Flang write Flang;
    property params : THTTPParameters read Fparams write Setparams;
    property indexes : TFHIRIndexInformation read Findexes write Setindexes;
    property session : TFHIRSession read Fsession write Setsession;
    property Connection : TFDBConnection read FConnection write SetConnection;
    property key : String read FKey write FKey;
    property link_ : string read Flink_;

    procedure execute;
  end;

const
  CODES_TMPICertainty : array [TMPICertainty] of String = ('', 'certain', 'probable', 'possible');

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
  FSessionCompartments.Free;
  FCompartment.Free;
  FConnection.Free;
  Findexes.Free;
  Fsession.Free;

  inherited;
end;

procedure TMPISearchProcessor.execute;
begin
  // parameters we care about:
  FDateOfBirth := Fparams['birthdate'].ToLower;
  FGender := Fparams['gender'].ToLower;
  FFirstName := FParams['given'].ToLower;
  FFamilyName := FParams['family'].ToLower;
  FIdentifier := FParams['identifier'].ToLower;

  if (FFirstName = '') or (FFamilyName = '') or (FGender = '') then
    raise EFHIRException.create('You must provide at least given name, family name, and gender for an MPI search');

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

procedure TMPISearchProcessor.SetCompartment(const Value: TFHIRCompartmentId);
begin
  FCompartment.Free;
  FCompartment := Value;
end;

procedure TMPISearchProcessor.SetConnection(const Value: TFDBConnection);
begin
  FConnection.Free;
  FConnection := Value;
end;

procedure TMPISearchProcessor.Setindexes(const Value: TFHIRIndexInformation);
begin
  Findexes.Free;
  Findexes := Value;
end;

procedure TMPISearchProcessor.Setparams(const Value: THTTPParameters);
begin
  Fparams := Value;
end;


procedure TMPISearchProcessor.Setsession(const Value: TFHIRSession);
begin
  Fsession.Free;
  Fsession := Value;
end;

procedure TMPISearchProcessor.SetSessionCompartments(const Value: TFslList<TFHIRCompartmentId>);
begin
  FSessionCompartments.Free;
  FSessionCompartments := Value;
end;

end.
