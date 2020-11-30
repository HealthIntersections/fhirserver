unit server_java;

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

interface

uses
  SysUtils, Classes,
  JNI, JNIWrapper, JavaRuntime,
  fsl_base, fsl_json,
  fhir_objects, FHIR.Version.Resources, FHIR.Version.Parser, FHIR.Version.Utilities;


type
  TJByteArray = TDshortintArray; //array of jbyte;

  TJavaLibraryWrapper = class (TFslObject)
  private
    Jvm : TJavaRuntime;
    JLibraryClass : TJavaClass;
    JLibrary : TJavaObject;
    JThrowableClass : TJavaClass;
    JStringClass : TJavaClass;
    JGetMessage : TJavaMethod;
    JInit : TJavaMethod;
    JTxConnect : TJavaMethod;
    JStatus : TJavaMethod;
    JSeeResource : TJavaMethod;
    JDropResource : TJavaMethod;
    JValidate : TJavaMethod;
    JConvert : TJavaMethod;
    JUnConvert : TJavaMethod;
    procedure checkException;
    function convertToJByteArray(b : TBytes) : TJByteArray;
    function convertFromJByteArray(b : TJByteArray) : TBytes;
  public
    constructor Create(jarPath : String); virtual;
    destructor Destroy; override;

    procedure Detach;

    procedure init(packPath : String);
    procedure txConnect(txServer : String);
    function status : TJsonObject;

    procedure seeResource(r : TFHIRResource; src : TBytes);
    procedure dropResource(type_, id : String);

    function validateResource(location : String; source : TBytes; format : TFHIRFormat; options : String) : TFHIROperationOutcome; overload;
    function validateResource(location : String; res : TFHIRResource; options : String) : TFHIROperationOutcome; overload;

    function convertResource(source : TBytes; fmt : TFHIRFormat; version : TFHIRVersion) : TBytes;
    function unConvertResource(source : TBytes; fmt : TFHIRFormat; version : TFHIRVersion) : TBytes;
  end;

implementation

const
  JAVA_FORMATS : array [TFHIRFormat] of string = ('', 'XML', 'JSON', 'TURTLE', '', '', '');
  JAVA_VERSIONS : array [TFHIRVersion] of string = ('', 'r1', 'r2', 'r3', 'r4');

{ TJavaLibraryWrapper }

procedure TJavaLibraryWrapper.checkException;
var
  j : jthrowable;
  o : TJavaObject;
  s : String;
begin
  j := Jvm.GetVM.checkException;
  if j <> nil then
  begin
    o := TJavaObject.CreateWithHandle(JThrowableClass, j);
    try
      s := o.toString;
    finally
      o.Free;
    end;
    Jvm.GetVM.clearException;
    raise EFHIRException.create(s);
  end;
end;

function TJavaLibraryWrapper.convertResource(source: TBytes; fmt : TFHIRFormat; version : TFHIRVersion) : TBytes;
var
  p : TJavaParams;
  v : jvalue;
begin
  Jvm.GetVM.attachThread;
  p := TJavaParams.Create;
  try
    p.addByteArray(convertToJByteArray(source));
    p.addString(JAVA_FORMATS[fmt]);
    p.addString(JAVA_VERSIONS[version]);
    v := JConvert.Call(p, JLibrary);
  finally
    p.Free;
  end;
  checkException;
  result := convertFromJByteArray(JbyteArrayToDshortintArray(v.l));
end;

constructor TJavaLibraryWrapper.Create(jarPath: String);
begin
  inherited Create;
  Jvm := TJavaRuntime.GetDefault;
  Jvm.addToClasspath(jarPath); // PUB_HOME+'\org.hl7.fhir.validator.jar';
  JThrowableClass := TJavaClass.Create('java.lang.Throwable');
  JStringClass := TJavaClass.Create('java.lang.String');
  !{$IFDEF FHIR4}
  JLibraryClass := TJavaClass.Create('org.hl7.fhir.r4.validation.NativeHostServices');
  checkException;
  {$ENDIF}
  !{$IFDEF FHIR3}
  JLibraryClass := TJavaClass.Create('org.hl7.fhir.dstu3.validation.NativeHostServices');
  {$ENDIF}
  !{$IFDEF FHIR2}
  raise EFHIRException.create('There is no Java bridge for DSTU2');
  {$ENDIF}

  JLibrary := JLibraryClass.Instantiate(nil);
  JGetMessage := TJavaMethod.Create(JThrowableClass, 'getMessage', nonstatic, AString, []);
  JInit := TJavaMethod.Create(JLibraryClass, 'init', nonstatic, Void, [AString]);
  JTxConnect := TJavaMethod.Create(JLibraryClass, 'connectToTxSvc', nonstatic, Void, [AString]);
  JStatus := TJavaMethod.Create(JLibraryClass, 'status', nonstatic, AString, []);
  JSeeResource := TJavaMethod.Create(JLibraryClass, 'seeResource', nonstatic, Void, [AByteArray]);
  JDropResource := TJavaMethod.Create(JLibraryClass, 'dropResource', nonstatic, Void, [AString, AString]);
  JValidate := TJavaMethod.Create(JLibraryClass, 'validateResource', nonstatic, AByteArray, [AString, AByteArray, AString, AString]);
  JConvert := TJavaMethod.Create(JLibraryClass, 'convertResource', nonstatic, AByteArray, [AByteArray, AString, AString]);
  JUnConvert := TJavaMethod.Create(JLibraryClass, 'unConvertResource', nonstatic, AByteArray, [AByteArray, AString, AString]);
end;

destructor TJavaLibraryWrapper.Destroy;
begin
  JUnConvert.Free;
  JConvert.Free;
  JValidate.Free;
  JDropResource.Free;
  JStringClass.Free;
  JStatus.Free;
  JSeeResource.Free;
  JGetMessage.Free;
  JThrowableClass.Free;
  JTxConnect.Free;
  JInit.Free;
  JLibrary.Free;
  JLibraryClass.Free;
  Jvm.Free;
  inherited;
end;

procedure TJavaLibraryWrapper.Detach;
begin
  Jvm.GetVM.detachThread;
end;

procedure TJavaLibraryWrapper.dropResource(type_, id: String);
var
  p : TJavaParams;
begin
  Jvm.GetVM.attachThread;
  p := TJavaParams.Create;
  try
    p.addString(type_);
    p.addString(id);
    JDropResource.Call(p, JLibrary);
  finally
    p.Free;
  end;
  checkException;
end;

procedure TJavaLibraryWrapper.init(packPath: String);
var
  p : TJavaParams;
  r : jvalue;
begin
  Jvm.GetVM.attachThread;
  p := TJavaParams.Create;
  try
    p.AddString(packPath);
    r := JInit.Call(p, JLibrary);
  finally
    p.Free;
  end;
  checkException;
end;

procedure TJavaLibraryWrapper.txConnect(txServer: String);
var
  p : TJavaParams;
  r : jvalue;
begin
  Jvm.GetVM.attachThread;
  p := TJavaParams.Create;
  try
    p.AddString(txServer);
    r := JTxConnect.Call(p, JLibrary);
  finally
    p.Free;
  end;
  checkException;
end;

function TJavaLibraryWrapper.status: TJsonObject;
var
  v : jvalue;
  o : TJavaObject;
begin
  Jvm.GetVM.attachThread;
  v := JStatus.Call(nil, JLibrary);
  o := TJavaObject.CreateWithHandle(JStringClass, v.l);
  try
    result := TJSONParser.Parse(o.toString);
  finally
    o.Free;
  end;
end;

function TJavaLibraryWrapper.unConvertResource(source: TBytes; fmt : TFHIRFormat; version : TFHIRVersion) : TBytes;
var
  p : TJavaParams;
  v : jvalue;
begin
  Jvm.GetVM.attachThread;
  p := TJavaParams.Create;
  try
    p.addByteArray(convertToJByteArray(source));
    p.addString(JAVA_FORMATS[fmt]);
    p.addString(JAVA_VERSIONS[version]);
    v := JUnConvert.Call(p, JLibrary);
  finally
    p.Free;
  end;
  checkException;
  result := convertFromJByteArray(JbyteArrayToDshortintArray(v.l));
end;

function TJavaLibraryWrapper.validateResource(location: String; res: TFHIRResource; options: String): TFHIROperationOutcome;
begin
  result := validateResource(location, resourceToBytes(res, ffXml), ffXml, options);
end;

function TJavaLibraryWrapper.validateResource(location: String; source: TBytes; format : TFHIRFormat; options: String): TFHIROperationOutcome;
var
  p : TJavaParams;
  b : TBytes;
  jb : TJByteArray;
  v : jvalue;
begin
  Jvm.GetVM.attachThread;
  p := TJavaParams.Create;
  try
    p.addString(location);
    p.addByteArray(convertToJByteArray(source));
    p.addString(JAVA_FORMATS[format]);
    p.addString(options);
    v := JValidate.Call(p, JLibrary);
  finally
    p.Free;
  end;
  checkException;
  jb := JbyteArrayToDshortintArray(v.l);
  b := convertFromJByteArray(jb);
  result := bytesToResource(b) as TFHIROperationOutcome;
end;

procedure TJavaLibraryWrapper.seeResource(r: TFHIRResource; src : TBytes);
var
  xml : TFHIRXmlComposer;
  s : TBytesStream;
  p : TJavaParams;
  jb : TJByteArray;
begin
  Jvm.GetVM.attachThread;
  if length(src) = 0 then
  begin
    s := TBytesStream.Create();
    try
      xml := TFHIRXmlComposer.Create(nil, OutputStyleNormal, THTTPLanguages.create('en'));
      try
        xml.Compose(s, r);
      finally
        xml.Free;
      end;
      src := s.Bytes;
      setLength(src, s.Size);
    finally
      s.Free;
    end;
  end;
  jb := convertToJByteArray(src);
  p := TJavaParams.Create;
  try
    p.addByteArray(jb);
    JSeeResource.Call(p, JLibrary);
  finally
    p.Free;
  end;
  checkException;
end;

//function TJavaLibraryWrapper.unConvertResource(r: TBytes;
//  src: String): TFHIRResource;
//begin
//
//end;
//
//function TJavaLibraryWrapper.validateResource(r: TFHIRResource;
//  version: String): TFHIROperationOutcome;
//begin
//
//end;
//
//function TJavaLibraryWrapper.convertResource(r: TFHIRResource;
//  dst: String): TBytes;
//begin
//
//end;
//
//
//procedure TJavaLibraryWrapper.dropResource(r: TFHIRResource);
//begin
//
//end;


{$R-}

function TJavaLibraryWrapper.convertToJByteArray(b: TBytes): TJByteArray;
var
  i : integer;
begin
  SetLength(result, length(b));
  for i := 0 to length(b) - 1 do
    result[i] := b[i];
end;

function TJavaLibraryWrapper.convertFromJByteArray(b: TJByteArray): TBytes;
var
  i : integer;
begin
  SetLength(result, length(b));
  for i := 0 to length(b) - 1 do
    result[i] := b[i];
end;

end.
