unit fhir4_deidentifier;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

this code is not functional at this time. It may be brought back to life in the future.

{$i fhir.inc}

interface

uses
  SysUtils,
  fsl_utilities, fsl_threads, fsl_utilities,
  fsl_base,  AdvCSVExtractors, AdvStringLists, AdvFiles,
  fdb_manager,
  fhir_objects{, FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources};

// fake name generator

type
  TPseudoData = class (TFslObject)
  private
    FName: TFHIRHumanName;
    FTelecom: TFHIRContactPoint;
    FAddress: TFHIRAddress;
    FPhoto: TFhirAttachment;
    procedure SetName(const Value: TFHIRHumanName);
    procedure SetTelecom(const Value: TFHIRContactPoint);
    procedure SetAddress(const Value: TFHIRAddress);
    procedure SetPhoto(const Value: TFhirAttachment);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TPseudoData; overload;

    property name : TFHIRHumanName read FName write SetName;
    property telecom : TFHIRContactPoint read FTelecom write SetTelecom;
    property address : TFHIRAddress read FAddress write SetAddress;
    property photo : TFhirAttachment read FPhoto write SetPhoto;
  end;

  TFakeDataRepository = class (TFslObject)
  private
    FMale : TFslList<TPseudoData>;
    FFemale : TFslList<TPseudoData>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure loadFromFile(filename : String);
    function getRandom(gender : TFhirAdministrativeGenderEnum) : TPseudoData;
  end;

  TFHIRDeIdentifier = class (TFslObject)
  private
    FPractitioners: boolean;
    FPersist: boolean;
    FDatabase : TFDBManager;
    FMakeFakeData: boolean;
    FDataCache : TFslMap<TPseudoData>;
    FLock : TFslLock;
    NextKey : integer;

    function loadPseudoData(conn : TFDBConnection; aType : TFhirResourceType; id : String) : TPseudoData;
    function makePseudoData(gender : TFhirAdministrativeGenderEnum) : TPseudoData;
    procedure SavePseudoData(conn : TFDBConnection; aType : TFhirResourceType; id : String; pd : TPseudoData);
    function fetchData(aType : TFhirResourceType; id : String; gender : TFhirAdministrativeGenderEnum) : TPseudoData;

    function fuzzify(date : TFslDateTime) : TFslDateTime;
    procedure processPatient(res : TFhirPatient);
    procedure processPerson(res : TFhirPerson);
    procedure processPractitioner(res : TFhirPractitioner);
    procedure processPractitionerRole(res : TFhirPractitionerRole);
    procedure processRelatedPerson(res : TFhirRelatedPerson);
    procedure SetDatabase(const Value: TFDBManager);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure load(filename : String);

    procedure DeIdentify(res : TFHIRResource);

    property database : TFDBManager read FDatabase write SetDatabase;
    property persist : boolean read FPersist write FPersist;
    property practitioners : boolean read FPractitioners write FPractitioners;
    property makeFakeData : boolean read FMakeFakeData write FMakeFakeData;
  end;

implementation

{ TFHIRDeIdentifier }

constructor TFHIRDeIdentifier.Create;
begin
  inherited;
  FDataCache := TFslMap<TPseudoData>.create;
  FLock := TFslLock.create('DeIdentifier');
end;

procedure TFHIRDeIdentifier.DeIdentify(res : TFHIRResource);
begin
  case res.ResourceType of
    frtPatient: processPatient(res as TFhirPatient);
    frtPerson: processPerson(res as TFhirPerson);
    frtPractitioner: processPractitioner(res as TFhirPractitioner);
    frtPractitionerRole: processPractitionerRole(res as TFhirPractitionerRole);
    frtRelatedPerson: processRelatedPerson(res as TFhirRelatedPerson);
  end;
end;

destructor TFHIRDeIdentifier.Destroy;
begin
  FDatabase.Free;
  FDataCache.Free;
  FLock.Free;
  inherited;
end;

function TFHIRDeIdentifier.fetchData(aType: TFhirResourceType; id: String; gender : TFhirAdministrativeGenderEnum): TPseudoData;
var
  n : String;
  pd : TPseudoData;
begin
  n := CODES_TFHIRResourceType[aType]+'/'+id;
  FLock.Lock;
  try
    if FDataCache.TryGetValue(n, pd) then
    begin
      result := pd.Link;
      exit;
    end;
  finally
    FLock.Unlock;
  end;
  pd := nil;
  try
    FDatabase.connection('DeIdentification',
      procedure (conn : TFDBConnection)
      begin
        pd := loadPseudoData(conn, aType, id);
        if pd = nil then
        begin
          pd := makePseudoData(gender);
          savePseudoData(conn, aType, id, pd);
        end;
      end
    );
    FLock.Lock;
    try
      FDataCache.AddOrSetValue(n, pd);
    finally
      FLock.Unlock;
    end;
    result := pd.Link;
  finally
    pd.Free;
  end;
end;

function TFHIRDeIdentifier.fuzzify(date: TFslDateTime): TFslDateTime;
var
  diff : TDateTime;
begin
  diff := trunc((now - date.DateTime) * (random / 10)); // up to 10% different based on gap
  if diff < 2 then
    diff := 2;
  if random(2) = 1 then
    result := date.add(diff)
  else
    result := date.subtract(diff);
  if result.DateTime > Now then
    result := date.subtract(diff);
end;

procedure TFHIRDeIdentifier.load(filename: String);
begin

end;

function TFHIRDeIdentifier.loadPseudoData(conn: TFDBConnection; aType: TFhirResourceType; id: String): TPseudoData;
begin
  result := nil;
  conn.SQL := 'select * from PsuedoData where Type = '+inttostr(ord(aType))+' and id = '''+SQLWrapString(id);
  conn.Prepare;
  conn.Execute;
  if conn.FetchNext then
  begin
    result := TPseudoData.Create;
    try
      result.name.family := conn.ColStringByName['Family'];
      result.name.givenList.AddItem(TFhirString.Create(conn.ColStringByName['Given']));
      result.address.lineList.AddItem(TFhirString.Create(conn.ColStringByName['Line']));
      result.address.city := conn.ColStringByName['City'];
      result.address.state := conn.ColStringByName['State'];
      result.address.postalCode := conn.ColStringByName['PostalCode'];
      result.telecom.value := conn.ColStringByName['Telecom'];
      result.telecom.system := ContactPointSystemPhone;
      result.telecom.use := ContactPointUseTemp;
      result.photo.data := conn.ColBlobByName['Photo'];
      result.photo.contentType := 'image/png';
      result.link;
    finally
      result.Free;
    end;
  end;
end;

function TFHIRDeIdentifier.makePseudoData(gender: TFhirAdministrativeGenderEnum): TPseudoData;
begin
  result := TPseudoData.Create;
  try
    result.name.family := 'Smith';
    case gender of
      AdministrativeGenderMale: result.name.givenList.Add(TFhirString.Create('Adam'));
      AdministrativeGenderFemale: result.name.givenList.Add(TFhirString.Create('Annette'));
    else
      result.name.givenList.Add(TFhirString.Create('Anno'));
    end;
    result.address.lineList.AddItem('101 Erewhon Street');
    result.address.city := 'NowhereVille';
    result.address.postalCode := '10101';
    result.photo := nil;
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRDeIdentifier.processPatient(res: TFhirPatient);
var
  nk : TFhirPatientContact;
  name, telecom, address, photo : boolean;
  data : TPseudoData;
begin
  name := res.nameList.count > 0;
  telecom := res.telecomList.count > 0;
  address := res.addressList.count > 0;
  photo := res.photoList.count > 0;

  res.identifierList.Clear;
  res.nameList.clear;
  res.telecomList.clear;
  res.birthDate := fuzzify(res.birthDate);
  if (res.deceased <> nil) and (res.deceased is TFhirDateTime) then
    res.deceased := TFhirDateTime.Create(fuzzify((res.deceased as TFhirDateTime).value));
  res.addressList.clear;
  res.photoList.Clear;
  for nk in res.contactList do
  begin
    nk.name := nil;
    nk.telecomList.Clear;
    nk.address := nil;
  end;
  if makeFakeData then
  begin
    data := fetchData(frtPatient, res.id, res.gender);
    try
      if name then
        res.nameList.Add(data.name.link);
      if telecom then
        res.telecomList.Add(data.telecom.link);
      if address then
        res.addressList.Add(data.address.link);
      if photo then
        res.photoList.Add(data.photo.link);
    finally
      data.free;
    end;
  end;
end;

procedure TFHIRDeIdentifier.processPerson(res: TFhirPerson);
var
  name, telecom, address, photo : boolean;
  data : TPseudoData;
begin
  name := res.nameList.count > 0;
  telecom := res.telecomList.count > 0;
  address := res.addressList.count > 0;
  photo := res.photo <> nil;

  res.identifierList.Clear;
  res.nameList.clear;
  res.telecomList.clear;
  res.birthDate := fuzzify(res.birthDate);
  res.addressList.clear;
  res.photo := nil;
  if makeFakeData then
  begin
    data := fetchData(frtPatient, res.id, res.gender);
    try
      if name then
        res.nameList.Add(data.name.link);
      if telecom then
        res.telecomList.Add(data.telecom.link);
      if address then
        res.addressList.Add(data.address.link);
      if photo then
        res.photo := data.photo.link;
    finally
      data.free;
    end;
  end;
end;

procedure TFHIRDeIdentifier.processPractitioner(res: TFhirPractitioner);
var
  name, telecom, address, photo : boolean;
  data : TPseudoData;
begin
  name := res.nameList.count > 0;
  telecom := res.telecomList.count > 0;
  address := res.addressList.count > 0;
  photo := res.photoList.count > 0;

  if not practitioners then
    exit;
  res.identifierList.Clear;
  res.nameList.clear;
  res.telecomList.clear;
  res.birthDate := fuzzify(res.birthDate);
  res.addressList.clear;
  res.photoList.Clear;
  if makeFakeData then
  begin
    data := fetchData(frtPatient, res.id, res.gender);
    try
      if name then
        res.nameList.Add(data.name.link);
      if telecom then
        res.telecomList.Add(data.telecom.link);
      if address then
        res.addressList.Add(data.address.link);
      if photo then
        res.photoList.Add(data.photo.link);
    finally
      data.free;
    end;
  end;
end;

procedure TFHIRDeIdentifier.processPractitionerRole(res: TFhirPractitionerRole);
begin
  if not practitioners then
    exit;
  res.identifierList.Clear;
  res.telecomList.Clear;
//  if makeFakeData then
end;

procedure TFHIRDeIdentifier.processRelatedPerson(res: TFhirRelatedPerson);
var
  name, telecom, address, photo : boolean;
  data : TPseudoData;
begin
  name := res.nameList.count > 0;
  telecom := res.telecomList.count > 0;
  address := res.addressList.count > 0;
  photo := res.photoList.count > 0;

  res.identifierList.Clear;
  res.nameList.clear;
  res.telecomList.clear;
  res.birthDate := fuzzify(res.birthDate);
  res.addressList.clear;
  res.photoList.Clear;
  if makeFakeData then
  begin
    data := fetchData(frtPatient, res.id, res.gender);
    try
      if name then
        res.nameList.Add(data.name.link);
      if telecom then
        res.telecomList.Add(data.telecom.link);
      if address then
        res.addressList.Add(data.address.link);
      if photo then
        res.photoList.Add(data.photo.link);
    finally
      data.free;
    end;
  end;
end;


procedure TFHIRDeIdentifier.SavePseudoData(conn: TFDBConnection; aType: TFhirResourceType; id: String; pd: TPseudoData);
begin
  FLock.exec(
    procedure
    begin
      if NextKey = 0 then
        nextKey := conn.CountSQL('Select max(PsuedoDataKey) from PsuedoData')+1
      else
        inc(nextKey);
    end
  );

  conn.SQL := 'insert into PsuedoData (Family, Given, Line, City, State, PostalCode, Telecom, Photo) values (:f, :g, :l, :c, :s, :p, :t, :i)';
  conn.Prepare;

  conn.BindString('f', pd.name.family);
  conn.BindString('g', pd.name.givenList[0].value);
  conn.BindString('l', pd.address.lineList[0].value);
  conn.BindString('c', pd.address.city);
  conn.BindString('s', pd.address.state);
  conn.BindString('p', pd.address.postalCode);
  conn.BindString('t', pd.telecom.value);
  if pd.photo <> nil then
    conn.BindBlob('i', pd.photo.data)
  else
    conn.BindNull('i');
  conn.Execute;
  conn.Terminate;
end;

procedure TFHIRDeIdentifier.SetDatabase(const Value: TFDBManager);
begin
  FDatabase := Value;
end;

function TFHIRDeIdentifier.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDatabase.sizeInBytes);
  inc(result, FDataCache.sizeInBytes);
  inc(result, FLock.sizeInBytes);
end;

{ TPseudoData }

constructor TPseudoData.Create;
begin
  inherited;
  FName := TFHIRHumanName.create;
  FTelecom := TFHIRContactPoint.create;
  FAddress := TFHIRAddress.create;
  FPhoto := TFhirAttachment.create;
end;

destructor TPseudoData.Destroy;
begin
  FName.Free;
  FTelecom.Free;
  FAddress.Free;
  FPhoto.Free;
  inherited;
end;

function TPseudoData.link: TPseudoData;
begin
  result := TPseudoData(inherited link);
end;

procedure TPseudoData.SetAddress(const Value: TFHIRAddress);
begin
  FAddress.Free;
  FAddress := Value;
end;

procedure TPseudoData.SetName(const Value: TFHIRHumanName);
begin
  FName.Free;
  FName := Value;
end;

procedure TPseudoData.SetPhoto(const Value: TFhirAttachment);
begin
  FPhoto.Free;
  FPhoto := Value;
end;

procedure TPseudoData.SetTelecom(const Value: TFHIRContactPoint);
begin
  FTelecom.Free;
  FTelecom := Value;
end;

function TPseudoData.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FName.sizeInBytes);
  inc(result, FTelecom.sizeInBytes);
  inc(result, FAddress.sizeInBytes);
  inc(result, FPhoto.sizeInBytes);
end;

{ TFakeDataRepository }

constructor TFakeDataRepository.Create;
begin
  inherited;
  FMale := TFslList<TPseudoData>.create;
  FFemale := TFslList<TPseudoData>.create;
end;

destructor TFakeDataRepository.Destroy;
begin
  FMale.Free;
  FFemale.Free;
  inherited;
end;

procedure TFakeDataRepository.loadFromFile(filename: String);
var
  csv : TFslCSVExtractor;
  line : TFslStringList;
  pd : TPseudoData;
begin
  line := TFslStringList.Create;
  try
    csv := TFslCSVExtractor.Create(TFslFile.Create(filename, fmOpenRead + fmShareDenyWrite));
    try
      csv.ConsumeEntries(line); // headers
      while not csv.EndOfStream do
      begin
        csv.ConsumeEntries(line);
        pd := TPseudoData.Create;
        try
          pd.name.family := line[2];
          pd.name.givenList.AddItem(TFhirString.Create(line[1]));
          pd.address.lineList.AddItem(TFhirString.Create(line[3]));
          pd.address.city := line[4];
          pd.address.state := line[5];
          pd.address.postalCode := line[6];
          pd.telecom.value := line[7];
          pd.telecom.system := ContactPointSystemPhone;
          pd.telecom.use := ContactPointUseTemp;
          pd.photo := nil;
          if line[0] = 'male' then
            FMale.Add(pd.link)
          else
            FFemale.Add(pd.link);
        finally
          pd.Free;
        end;
      end;
    finally
      csv.Free;
    end;
  finally
    line.free;
  end;
end;

function TFakeDataRepository.getRandom(gender : TFhirAdministrativeGenderEnum): TPseudoData;
var
 i : integer;
begin
  case gender of
    AdministrativeGenderMale: result := FMale[random(FMale.Count)].link;
    AdministrativeGenderFemale: result := FFemale[random(FFemale.Count)].link;
  else
    begin
    i := random(FMale.Count + FFemale.Count);
    if i > FMale.Count then
      result := FFemale[i - FMale.Count].link
    else
      result := FMale[i].link;
    end;
  end;
end;

function TFakeDataRepository.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMale.sizeInBytes);
  inc(result, FFemale.sizeInBytes);
end;

end.
