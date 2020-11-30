Unit ftx_ucum_import;

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

Interface

Uses
  Classes,
  AdvStringLists,
  SystemConfigurations,
  SysUtils,
  fsl_utilities,
  IniFiles,
  KProcs,
  fdb_manager,
  Ucum,
  gwDatabaseUtils,
  gwUcum,
  fdb_dialects,
  gwProgressActions,
  IdSoapMsXml,
  MsXmlParser;


Type
  TUcumImportAction = class (TProgressAction)
  private
    FFilename: String;
    FUcum : TUcumModel;
    TotalConcepts : Integer;
    Fname: String;
    FPath: String;

    FDb : TFDBManager;
    FConn : TFDBConnection;
    FStatus: Integer;
    FKey: Integer;
    FOldSource: String;
    FContext : TFslDecimalContext;

    Function ParseDecimal(S,s1 : String):TFslDecimal;
    Function ParsePrefix(oElem : IXMLDOMElement):TUcumPrefix;
    Function ParseBaseUnit(oElem : IXMLDOMElement):TUcumBaseUnit;
    Function ParseUnit(oElem : IXMLDOMElement):TUcumDefinedUnit;
    Function GetPropertyIndex(const sName : String):Integer;
    Procedure LoadCommonUnits(oIni : TIniFile);
  protected
    function sizeInBytesV : cardinal; override;
  public
    procedure Go;                                                          Override;
    function  PreTitle:string;                                             Override;
    Property FileName : String read FFilename write FFilename;
    Property Name : String read Fname write FName;
    Property Path : String read FPath write FPath;
    Property Status : Integer read FStatus Write FStatus;
    Property Key : Integer read FKey write FKey;
    Property OldSource : String read FOldSource write FOldSource;
  end;


Implementation


{ TUcumImportAction }

Var
  GUcum : TUcumServices;

Function MakeSafeFileName(sName : String; newkey : Integer):String;
var
  i : integer;
Begin
  result := '';
  for i := 1 to length(sName) Do
    if (sName[i] in ['a'..'z', '_', '-', 'A'..'Z', '0'..'9']) Then
      result := result + sName[i];
  if result = '' then
    result := inttostr(newkey);
End;


procedure TUcumImportAction.Go;
var
  oParser : TMsXmlParser;
  oXml : IXMLDomDocument2;
  oElem : IXMLDOMElement;
  Reg : TSystemConfiguration;
  bAbort : Boolean;
  iCount : integer;
  oErrors : TFslStringList;
  oIni : TIniFile;
  newKey : integer;
  sFilename : String;
  iStatus : integer;
begin
  FDB := CreateConnectionPoolManager(nil, nil, 'import');
  FContext := TFslDecimalContext.create;
  FConn := FDb.GetConnection('snomed');
  Try
    if Key <> 0 Then
      NewKey := Key
    Else
      newKey := FConn.CountSQL('Select Max(DefinitionKey) from gwDefinitions') + 1;
    sFilename := TSystemConfiguration.ApplicationFolder+'\data\ucum-'+MakeSafeFileName(Name, newkey)+'.cache';

    if (Status < 2) and (FConn.CountSQL('Select count(*) from gwDefinitions where type = 2 and status = 1') = 0) Then
      iStatus := 1 // going to be default unless user said it was inactive
    Else if Status = 2 Then
      iStatus := 3
    Else if Status = 1 then
      iStatus := 1
    Else
      iStatus := 2;



    iCount := 0;
    Reg := TSystemConfiguration.Create;
    Try
      TotalConcepts := StrToIntDef(Reg.GetSetting('\Manager\Import\Ucum', 'Count'), 299);

      oParser := TMsXmlParser.Create;
      try
        oXml := oParser.Parse(FFilename);
      Finally
        oParser.Free;
      End;
      if oXml.documentElement.nodeName <> 'root' Then
        raise ETerminologySetup.create('Invalid ucum essence file');
      GUcum := TUcumServices.Create;
      try

        FUcum := GUcum.Model;
        Fucum.Clear;
        FUcum.Version := TMsXmlParser.GetAttribute(oXml.documentElement, 'version');
        FUcum.RevisionDate := TMsXmlParser.GetAttribute(oXml.documentElement, 'revision-date');
        FUcum.RevisionDate := copy(FUcum.RevisionDate, 8, length(FUcum.RevisionDate)-9);
        oElem := TMsXmlParser.FirstChild(oXml.documentElement);
        while (oElem <> nil) Do
        Begin
          inc(iCount);
          Progress('Import Ucum', TMsXmlParser.GetAttribute(oElem, 'Code'), iCount, TotalConcepts, false, bAbort);
          if bAbort then
            Abort;

         if oElem.NodeName = 'prefix' Then
           FUcum.prefixes.Add(ParsePrefix(oElem))
         Else if oElem.NodeName = 'base-unit' Then
           FUcum.baseUnits.Add(ParseBaseUnit(oElem))
         Else if oElem.NodeName = 'unit' Then
           FUcum.definedUnits.Add(ParseUnit(oElem))
         else
           raise ETerminologySetup.create('unrecognised element '+oElem.nodename);
          oElem := TMsXmlParser.NextSibling(oElem);
        End;
        oErrors := TFslStringList.Create;
        Try
          GUcum.Validate(oErrors);
          if oErrors.Count > 0 then
            raise ETerminologySetup.create(oErrors.asText);
        Finally
          oErrors.Free;
        End;
        oIni := nil;
        If FileExists(AppendSlash(ExtractFilePath(FFilename))+'ucum-extensions.txt') Then
          oIni := TIniFile.Create(AppendSlash(ExtractFilePath(FFilename))+'ucum-extensions.txt')
        Else if FileExists(AppendSlash(TSystemConfiguration.ApplicationFolder)+'data/ucum/ucum-extensions.txt') Then
          oIni := TIniFile.Create(AppendSlash(TSystemConfiguration.ApplicationFolder)+'data/ucum/ucum-extensions.txt');
        if oIni <> nil Then
        Begin
          Try
            LoadCommonUnits(oIni);
          Finally
            oIni.Free;
          End;
        End;
        GUcum.Save(sFilename);
        SetFileReadOnly(sFilename, true);
      Finally
        GUcum.Free;
      End;
      if iStatus = 1 Then
        FConn.ExecSQL('Update gwDefinitions Set Status = 2 where Status = 1 and Type = 2');
      if Key <> 0 Then
        FConn.SQL := 'Update gwDefinitions Set Name = :v, Type = 2, Status = :st, Source = :s, Path = :p, Size = :sz, UpdatedDate = '+DBGetDate(FConn.Owner.Platform)+', UpdatedUser = 0, SourceInfo = :info where DefinitionKey = :k'
      Else
        FConn.SQL := 'insert into gwDefinitions (DefinitionKey, Name, Type, Status, Source, Size, AddedDate, AddedUser, Path, SourceInfo) values (:k, :v, 2, :st, :s, :sz, '+DBGetDate(FConn.Owner.Platform)+', 0, :p, :info)';
      FConn.Prepare;
      Try
        FConn.BindKey('k', newkey);
        FConn.BindString('v', Name);
        FConn.BindString('p', Path);
        FConn.BindInteger('sz', SizeFile(sFilename));
        FConn.BindString('s', sFilename);
        FConn.BindInteger('st', iStatus);
        FConn.BindBlobFromString('info', '[files]'+crlf+'source='+FFilename+crlf);
        FConn.Execute;
      Finally
        FConn.Terminate;
      End;

      if FileExists(OldSource) And (sFilename <> OldSource) Then
      begin
        SetFileReadOnly(OldSource, false);
        DeleteFile(OldSource);
      End;

      Reg.SetSetting('\Manager\Import\Ucum', 'Count', inttostr(iCount));
    Finally
      Reg.Free;
    End;
  Finally
    FConn.Release;
    FContext.free;
    FDb.Free;
  End;
end;

function TUcumImportAction.PreTitle: string;
begin
  result := 'Ucum Import';
end;

Function TUcumImportAction.ParsePrefix(oElem : IXMLDOMElement):TUcumPrefix;
var
  oChild : IXMLDOMElement;
  s : String;
Begin
  result := TUcumPrefix.Create;
  try
    result.code := TMsXmlParser.GetAttribute(oElem, 'Code');
    result.codeUC := TMsXmlParser.GetAttribute(oElem, 'CODE');
    oChild := TMsXmlParser.FirstChild(oElem);
    while oChild <> nil do
    Begin
      if oChild.nodeName = 'name' Then
        result.names.Add(TMsXmlParser.TextContent(oChild, ttAsIs))
      else if oChild.nodeName = 'printSymbol' Then
        result.printSymbol := TMsXmlParser.TextContent(oChild, ttAsIs)
      else if oChild.nodeName = 'value' Then
      begin
        s := TmsXmlParser.GetAttribute(oChild, 'value');
        result.value := ParseDecimal(s, result.Code);
        result.value.Precision := 24; // arbitrarily high. even when an integer, these numbers are precise
        if s[2] = 'e' Then
          result.Text := '10^'+Copy(s, 3, $FF)
        else
          result.Text := s;
      End
      else
        raise ETerminologySetup.create('unknown element in prefix: '+oChild.NodeName);
      oChild := TMsXmlParser.NextSibling(oChild);
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

Function TUcumImportAction.ParseBaseUnit(oElem : IXMLDOMElement):TUcumBaseUnit;
var
  oChild : IXMLDOMElement;
  s : String;
Begin
  result := TUcumBaseUnit.Create;
  try
    result.code := TMsXmlParser.GetAttribute(oElem, 'Code');
    result.codeUC := TMsXmlParser.GetAttribute(oElem, 'CODE');
    s := TMsXmlParser.GetAttribute(oElem, 'dim');
    if s <> '' Then
      result.dim := s[1];
    oChild := TMsXmlParser.FirstChild(oElem);
    while oChild <> nil do
    Begin
      if oChild.nodeName = 'name' Then
        result.names.Add(TMsXmlParser.TextContent(oChild, ttAsIs))
      else if oChild.nodeName = 'printSymbol' Then
        result.printSymbol := TMsXmlParser.TextContent(oChild, ttAsIs)
      else if oChild.nodeName = 'property' Then
        result.PropertyType := GetPropertyIndex(TMsXmlParser.TextContent(oChild, ttAsIs))
      else
        raise ETerminologySetup.create('unknown element in base unit: '+oChild.NodeName);
      oChild := TMsXmlParser.NextSibling(oChild);
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

Function TUcumImportAction.ParseUnit(oElem : IXMLDOMElement):TUcumDefinedUnit;
var
  oChild : IXMLDOMElement;
Begin
  result := TUcumDefinedUnit.Create;
  try
    result.code := TMsXmlParser.GetAttribute(oElem, 'Code');
    result.codeUC := TMsXmlParser.GetAttribute(oElem, 'CODE');
    result.metric := TMsXmlParser.GetAttribute(oElem, 'isMetric') = 'yes';
    result.isSpecial := TMsXmlParser.GetAttribute(oElem, 'isSpecial') = 'yes';
    result.class_ := TMsXmlParser.GetAttribute(oElem, 'class');
    oChild := TMsXmlParser.FirstChild(oElem);
    while oChild <> nil do
    Begin
      if oChild.nodeName = 'name' Then
        result.names.Add(TMsXmlParser.TextContent(oChild, ttAsIs))
      else if oChild.nodeName = 'printSymbol' Then
        result.printSymbol := TMsXmlParser.TextContent(oChild, ttAsIs)
      else if oChild.nodeName = 'property' Then
        result.PropertyType := GetPropertyIndex(TMsXmlParser.TextContent(oChild, ttAsIs))
      else if oChild.nodeName = 'value' Then
      begin
        result.value.unit_ := TMsXmlParser.GetAttribute(oChild, 'Unit');
        result.value.unitUC := TMsXmlParser.GetAttribute(oChild, 'UNIT');
        result.value.value := ParseDecimal(TmsXmlParser.GetAttribute(oChild, 'value'), result.value.unit_);
        result.value.text := TMsXmlParser.TextContent(oChild, ttAsIs);
      End
      else
        raise ETerminologySetup.create('unknown element in unit: '+oChild.NodeName); 
      oChild := TMsXmlParser.NextSibling(oChild);
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;


function TUcumImportAction.ParseDecimal(S,s1: String): TFslDecimal;
begin
  if s = '' then
    result := FContext.One
  Else
    result := FContext.Value(s);
end;


Procedure TUcumImportAction.LoadCommonUnits(oIni : TIniFile);
var
  oList : TStringList;
  oUnits : TFslStringList;
  i, j : integer;
  s : string;
Begin
  oList := TStringList.Create;
  Try
    oIni.ReadSection('common units', oList);
    oUnits := TFslStringList.Create;
    Try
      for i := 0 to oList.Count - 1 Do
      begin
        oUnits.AsCSV := oIni.ReadString('common units', oList[i], '');
        for j := 0 to oUnits.Count - 1 do
        begin
          s := GUcum.Validate(oUnits[j]);
          if s <> '' Then
            raise ETerminologySetup.create('The unit '+oUnits[j]+' on the property '+oList[i]+' in the UCUM extensions is not valid: '+s);
        End;
        FUcum.Properties[GetPropertyIndex(oList[i])].CommonUnits.Assign(oUnits);
      End;
    Finally
      oUnits.Free;
    End;
  Finally
    oList.Free;
  End;
End;


function TUcumImportAction.GetPropertyIndex(const sName: String): Integer;
begin
  result :=  FUcum.Properties.IndexByName(sName);
  if result = -1 then
    result := FUcum.Properties.AddByName(sName);
end;

function TUcumImportAction.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilename.length * sizeof(char)) + 12);
  inc(result, FUcum.sizeInBytes);
  inc(result, (Fname.length * sizeof(char)) + 12);
  inc(result, (FPath.length * sizeof(char)) + 12);
  inc(result, FDb.sizeInBytes);
  inc(result, FConn.sizeInBytes);
  inc(result, (FOldSource.length * sizeof(char)) + 12);
  inc(result, FContext.sizeInBytes);
end;

End.


