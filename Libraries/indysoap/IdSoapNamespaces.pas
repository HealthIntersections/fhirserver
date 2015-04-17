{
IndySOAP: Namespace support for XML reading / writing

This is a workaround for missing namespace support in OpenXML
}

Unit IdSoapNamespaces;

{$I IdSoapDefines.inc}

Interface

Uses
  Classes,
  IdSoapDebug,
  {$IFDEF UNICODE}
  Xml.Xmldom,
  {$ELSE}
  IdSoapOpenXML,
  {$ENDIF}
  IdSoapXML;

Type
  TNMToken = String;

  TQName = Class (TIdBaseObject)
  Private
    FNameSpace: String;
    FName: String;
  Public
    Property NameSpace : String Read FNameSpace Write FNameSpace;
    Property Name : String Read FName Write FName;
    function Equals(AName : TQName):Boolean; reintroduce; virtual;
    Function Clone : TQName;
    Function defined : Boolean;
  End;

Const
  NO_DEF = False;
  DEF_OK = True;

Type
  TIdSoapXmlNamespaceSupport = Class (TIdBaseObject)
  Private
    // name spaces are indexed either way
    FNameSpaces : TStringList;
    FUsedCodes : TStringList;
    FCodeList : TStringList;
    FDefaultNamespaceStack : TStringList;
    FDefaultNamespace : String;
  Public
    Constructor Create;
    destructor Destroy; Override;
    Property NameSpaces : TStringList Read FNameSpaces;

    Function GetNameSpaceCode(Const ANameSpace : String; ADefaultValid : Boolean; ASuggestion : String = '') : String;
    Function DefineNamespace(Const ANamespace : String; Const ASuggestedCode: String): String;
    Procedure DefineDefaultNamespace(Const ANamespace : String; AElement : TdomElement); Overload;
    Procedure DefineDefaultNamespace(Const ANamespace : String; AElement : TIdSoapXmlElement); Overload;
    Procedure UnDefineDefaultNamespace;

    Procedure AddNamespaceDefinitions(AxmlElement : TdomElement); Overload;
    Procedure AddNamespaceDefinitions(AxmlElement : TIdSoapXmlElement); Overload;
  End;

Function HasDefaultNamespace(AElement : TIdSoapXmlElement):Boolean; overload;
Function HasDefaultNamespace(AElement : TdomElement):Boolean; overload;
Function ResolveXMLNamespaceCode(AElement : TdomElement; ANamespace, ALocation : String; AAllowDefault : Boolean = False):String; overload;
Function ResolveXMLNamespaceCode(AElement : TIdSoapXmlElement; ANamespace, ALocation : String; AAllowDefault : Boolean = False):String; overload;
Function LooseFindChildElement(AElement : TdomElement; AName : String):TdomElement; overload;
Function LooseFindChildElement(AElement : TIdSoapXmlElement; AName : String):TIdSoapXmlElement; overload;
Function GetNamespacePrefix(Const AQName : String):String;
Procedure SplitNamespace(Const AQname : String; Var VNamespace, VName : String);

Implementation

Uses
  IdSoapExceptions,
  IdSoapResourceStrings,
  IdSoapUtilities,
  IdSoapConsts,
  SysUtils;

Const
  ASSERT_UNIT = 'IdSoapNamespaces';


Function HasDefaultNamespace(AElement : TIdSoapXmlElement):Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.HasDefaultNamesapce';
Var
  i : Integer;
  ns, name : widestring;
Begin
  Result := False;
  For i := 0 To AElement.AttributeCount Do
    Begin
    AElement.getAttributeName(i, ns, name);
    If ((ns = 'http://www.w3.org/2000/xmlns/') And (name = 'xmlns')) Then
      Begin
      Result := True;
      Break;
      End;
    End;
  If (Not Result) And Assigned(AElement.parentNode) Then
    Result := HasDefaultNamespace(AElement.parentNode);
end;

Function HasDefaultNamespace(AElement : TdomElement):Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.HasDefaultNamesapce';
Var
  i : Integer;
  LAttr : TdomAttr;
Begin
  Result := False;
  For i := 0 To AElement.attributes.Length - 1 Do
    Begin
    LAttr := AElement.Attributes.item[i] As TdomAttr;
    If ((LAttr.namespaceURI = 'http://www.w3.org/2000/xmlns/') And (LAttr.localName = 'xmlns')) Then
      Begin
      Result := True;
      Break;
      End;
    End;
  If (Not Result ) And Assigned(AElement.parentNode) And (AElement.nodeType = ELEMENT_NODE) Then
    Result := HasDefaultNamespace(AElement.parentNode As TdomElement);
End;


Function ResolveXMLNamespaceCode(AElement : TdomElement; ANamespace, ALocation : String; AAllowDefault : Boolean = False):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.ResolveXMLNamespaceCode';
Var
  i : Integer;
  LAttr : TdomAttr;
Begin
  Assert(AAllowDefault Or (ANamespace <> ''), ASSERT_LOCATION+': namespace is blank ('+ALocation+')');
  Assert(ALocation <> '', ASSERT_LOCATION+': Location is blank ('+ALocation+')');

  Result := '';
  For i := 0 To AElement.attributes.Length - 1 Do
    Begin
    LAttr := AElement.Attributes.item[i] As TdomAttr;
    If ( (ANameSpace = '') And (LAttr.namespaceURI = 'http://www.w3.org/2000/xmlns/') And (LAttr.localName = 'xmlns')) Or
       ((LAttr.namespaceURI = 'http://www.w3.org/2000/xmlns/') And
       (LAttr.localName = ANameSpace)) Then
      Begin
      Result := LAttr.value;
      Break;
      End;
    End;
  If Result = '' Then
    Begin
    If Assigned(AElement.parentNode) And (AElement.nodeType = ELEMENT_NODE) Then
      Begin
      Result := ResolveXMLNamespaceCode(AElement.parentNode As TdomElement, ANamespace, ALocation, AAllowDefault);
      End
    Else
      Begin
      Raise EIdSoapNamespaceProblem.CreateFmt(RS_ERR_SOAP_UNRESOLVABLE_NAMESPACE, [ANamespace, ALocation]);
      End;
    End;
End;

Function ResolveXMLNamespaceCode(AElement : TIdSoapXmlElement; ANamespace, ALocation : String; AAllowDefault : Boolean = False):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.ResolveXMLNamespaceCode';
Var
  i : Integer;
  ns, name : widestring;
Begin
  Assert(AAllowDefault Or (ANamespace <> ''), ASSERT_LOCATION+': namespace is blank ('+ALocation+')');
  Assert(ALocation <> '', ASSERT_LOCATION+': Location is blank ('+ALocation+')');

  Result := '';
  For i := 0 To AElement.AttributeCount Do
    Begin
    AElement.getAttributeName(i, ns, name);
    If ( (ANameSpace = '') And (ns = 'http://www.w3.org/2000/xmlns/') And (name = 'xmlns')) Or
       ((ns = 'http://www.w3.org/2000/xmlns/') And
       (name = ANameSpace)) Then
      Begin
      Result := AElement.getAttribute(ns, name);
      Break;
      End;
    End;
  If Result = '' Then
    Begin
    If Assigned(AElement.ParentNode) Then
      Begin
      Result := ResolveXMLNamespaceCode(AElement.ParentNode, ANamespace, ALocation, AAllowDefault);
      End
    Else
      Begin
      Raise EIdSoapNamespaceProblem.CreateFmt(RS_ERR_SOAP_UNRESOLVABLE_NAMESPACE, [ANamespace, ALocation]);
      End;
    End;
End;

Function LooseFindChildElement(AElement : TdomElement; AName : String):TdomElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.LooseFindChildElement';
Var
  LNode : TdomNode;
Begin
  Assert(AName <> '', ASSERT_LOCATION+': name is blank');

  LNode := AElement.firstChild;
  Result := Nil;
  While Assigned(LNode) And Not Assigned(Result) Do
    Begin
    If (LNode.nodeType = ELEMENT_NODE) And ( AnsiSameText(LNode.nodeName, AName) Or AnsiSameText(LNode.localName, AName)) Then
      Begin
      Result := LNode As TdomElement;
      End
    Else
      Begin
      LNode := LNode.nextSibling;
      End;
    End;
End;

Function LooseFindChildElement(AElement : TIdSoapXmlElement; AName : String):TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.LooseFindChildElement';
Var
  LNode : TIdSoapXmlElement;
Begin
  Assert(AName <> '', ASSERT_LOCATION+': name is blank');

  LNode := AElement.firstChild;
  Result := Nil;
  While Assigned(LNode) And Not Assigned(Result) Do
  Begin
    If AnsiSameText(LNode.nodeName, AName) Or AnsiSameText(LNode.Name, AName) Then
      Result := LNode
    Else
      LNode := LNode.NextSibling;
  End;
End;

{ TIdSoapXmlNamespaceSupport }

Constructor TIdSoapXmlNamespaceSupport.Create;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlNamespaceSupport.create';
Begin
  Inherited;
  FNameSpaces := TStringList.Create;
  FCodeList := TStringList.Create;
  FUsedCodes := TStringList.Create;
  FDefaultNamespaceStack := TStringList.Create;
  FDefaultNamespace := '';
End;

Destructor TIdSoapXmlNamespaceSupport.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlNamespaceSupport.destroy';
Begin
  Assert(Self.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FDefaultNamespaceStack);
  FreeAndNil(FCodeList);
  FreeAndNil(FUsedCodes);
  FreeAndNil(FNameSpaces);
  Inherited;
End;

Function TIdSoapXmlNamespaceSupport.GetNameSpaceCode(Const ANameSpace: String; ADefaultValid : Boolean; ASuggestion : String = ''): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlNamespaceSupport.GetNameSpaceCode';
Var
  LIndex : Integer;
Begin
  Assert(Self.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': self is not valid');
  Assert(FNameSpaces <> Nil, ASSERT_LOCATION+': AllNameSpacesByName is not valid');

  If ANamespace = '' Then
    Begin
    Result := ''
    End
  Else If ADefaultValid And (ANamespace = FDefaultNamespace) Then
    Begin
    Result := '';
    End
  Else If ANamespace = '##any' Then
    Begin
    Result := '##any';
    End
  Else
    Begin
    LIndex := FNameSpaces.IndexOfName(ANameSpace);
    If LIndex = -1 Then
      Begin
      DefineNamespace(ANamespace, ASuggestion);
      LIndex := FNameSpaces.IndexOfName(ANameSpace);
      Assert(LIndex <> -1, ASSERT_LOCATION+': Namespace "'+ANameSpace+'" not defined?');
      End;
    Result := Copy(FNameSpaces[Lindex], Length(ANamespace)+2, MaxInt);
    FUsedCodes.Add(Result);
    Result := Result + ':';
    End;
End;

Function TIdSoapXmlNamespaceSupport.DefineNamespace(Const ANamespace : String; Const ASuggestedCode: String): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlNamespaceSupport.DefineNamespace';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Self is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ''''');
  Assert(FNameSpaces <> Nil, ASSERT_LOCATION+': AllNameSpacesByName is not valid');

  If FNameSpaces.IndexOfName(ANamespace) > -1 Then
    Begin
    Result := FNameSpaces.Values[ANamespace]
    End
  Else If ANamespace[1] = '#' Then
    Begin
    Result := ANameSpace;
    End
  Else
    Begin
    If ASuggestedCode = '' Then
      Begin
      Result := ID_SOAP_DEFAULT_NAMESPACE_CODE;
      End
    Else
      Begin
      Result := ASuggestedCode;
      End;
    i := 1;
    While (i < 10) And (FCodeList.IndexOf(Result) > -1) Do
      Begin
      Inc(i);
      if (result[length(result)] >= '0') And (result[length(result)] <= '8') then
        Begin
        Result[Length(Result)] := chr(ord(Result[Length(Result)])+1)
        End
      Else
        Begin
        Result := Result + '1';
        End;
      End;
    FNameSpaces.Values[ANamespace] := Result;
    FCodeList.Add(Result);
    End;
End;

Procedure TIdSoapXmlNamespaceSupport.AddNamespaceDefinitions(AxmlElement: TdomElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlNamespaceSupport.AddNamespaceDefinitions';
Var
  i : Integer;
  n, v : String;
Begin
  Assert(Self.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': self is not valid');
  Assert(IdSoapTestNodeValid(AxmlElement, TdomElement), ASSERT_LOCATION+': xmlElement is not valid');
  Assert(FNameSpaces <> Nil, ASSERT_LOCATION+': AllNameSpacesByName is not valid');
  For i := 0 To FNameSpaces.Count -1 Do
    Begin
    n := FNameSpaces.Names[i];
    v := Copy(FNameSpaces[i], Length(n)+2, MaxInt);
    If FUsedCodes.indexof(v) > -1 Then // check that it was actually used
      Begin
      AxmlElement.setAttribute(ID_SOAP_NAME_XML_XMLNS+':'+v, n);
      End;
    End;
End;

Procedure TIdSoapXmlNamespaceSupport.AddNamespaceDefinitions(AxmlElement: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlNamespaceSupport.AddNamespaceDefinitions';
Var
  i : Integer;
  n, v : String;
Begin
  Assert(Self.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': self is not valid');
  Assert(AxmlElement.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': xmlElement is not valid');
  Assert(FNameSpaces <> Nil, ASSERT_LOCATION+': AllNameSpacesByName is not valid');
  For i := 0 To FNameSpaces.Count -1 Do
    Begin
    n := FNameSpaces.Names[i];
    v := Copy(FNameSpaces[i], Length(n)+2, MaxInt);
    If FUsedCodes.indexof(v) > -1 Then // check that it was actually used
      Begin
      AxmlElement.setAttribute('http://www.w3.org/2000/xmlns/', ID_SOAP_NAME_XML_XMLNS+':'+v, n);
      End;
    End;
End;


Procedure TIdSoapXmlNamespaceSupport.DefineDefaultNamespace(Const ANamespace: String; AElement: TdomElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.DefineDefaultNamespace';
Begin
  Assert(Self.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': self is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': namespace is blank');
  Assert(IdSoapTestNodeValid(AElement, TdomElement), ASSERT_LOCATION+': Element is not valid');
  FDefaultNamespaceStack.Add(FDefaultNamespace);
  FDefaultNamespace := ANamespace;
  AElement.setAttribute(ID_SOAP_NAME_XML_XMLNS, ANamespace);
End;

Procedure TIdSoapXmlNamespaceSupport.DefineDefaultNamespace(Const ANamespace: String; AElement: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.DefineDefaultNamespace';
Begin
  Assert(Self.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': self is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': namespace is blank');
  Assert(AElement.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': Element is not valid');
  FDefaultNamespaceStack.Add(FDefaultNamespace);
  FDefaultNamespace := ANamespace;
  {$IFNDEF UNICODE}
  AElement.setAttribute('http://www.w3.org/2000/xmlns/', ID_SOAP_NAME_XML_XMLNS, ANamespace);
  {$ENDIF}
End;

Procedure TIdSoapXmlNamespaceSupport.UnDefineDefaultNamespace;
Const ASSERT_LOCATION = ASSERT_UNIT+'.UnDefineDefaultNamespace';
Begin
  Assert(Self.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': self is not valid');
  Assert(FDefaultNamespaceStack.count > 0, ASSERT_LOCATION+': UnDefineDefaultNamespace called too many times');
  FDefaultNamespace := FDefaultNamespaceStack[FDefaultNamespaceStack.count - 1];
  FDefaultNamespaceStack.Delete(FDefaultNamespaceStack.count -1 );
End;

Function GetNamespacePrefix(Const AQName : String):String;
Var
  i : Integer;
Begin
  i := Pos(':', AQname);
  If i > 0 Then
    Begin
    Result := Copy(AQname, 1, i-1);
    End
  Else
    Begin
    Result := '';
    End;
End;

Procedure SplitNamespace(Const AQname : String; Var VNamespace, VName : String);
Var
  i : Integer;
Begin
  i := Pos(':', AQname);
  If i > 0 Then
    Begin
    VNamespace := Copy(AQname, 1, i-1);
    VName := Copy(AQname, i+1, $FFFF);
    End
  Else
    Begin
    VNamespace := '';
    VName := AQname;
    End;
End;


{ TQName }

Function TQName.Clone: TQName;
Begin
  If Self = Nil Then
    Begin
    Result := Nil;
    End
  Else
    Begin
    Result := TQName.Create;
    Result.FNameSpace := FNameSpace;
    Result.FName := FName;
    End;
End;

Function TQName.defined: Boolean;
Begin
  Result := FNameSpace <> '';
End;

Function TQName.Equals(AName: TQName): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TQName.Equals';
Begin
  Assert(Self.TestValid(TQName), ASSERT_LOCATION+': self is not valid');
  Assert(AName.TestValid(TQName), ASSERT_LOCATION+': Name is not valid');

  Result := (Assigned(AName)) And (AName.FNamespace = FNameSpace) And (AName.FName = FName);
End;

End.




