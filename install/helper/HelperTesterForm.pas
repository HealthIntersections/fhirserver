unit HelperTesterForm;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ComCtrls;

type
  TForm10 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Button4: TButton;
    CheckListBox1: TCheckListBox;
    Button5: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    od: TOpenDialog;
    eIni: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    eKey: TEdit;
    Label4: TLabel;
    eValue: TEdit;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    dll : THandle;
    urls : TStringList;
    procedure progress(IntParam: Integer; StrParam: WideString);
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

type
  TInstallerCallback = procedure (IntParam: Integer; StrParam: WideString) of object;
  TMyDllListPackages = Function (Version : PAnsiChar) : PAnsiChar; stdcall;
  TMyDllDownloadPackages = Function (url : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;
  TMyDllInstallDatabase = Function (ExeName, IniFile, Password, version, packages, mode : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;
  TMyDllGetIniValue = Function (IniFile, Key : PAnsiChar) : PAnsiChar; stdcall;
  TMyDllSetIniValue = Procedure (IniFile, Key, value : PAnsiChar); stdcall;

var
  MyDllListPackages : TMyDllListPackages;
  MyDllDownloadPackages : TMyDllDownloadPackages;
  MyDllGetIniValue : TMyDllGetIniValue;
  MyDllSetIniValue : TMyDllSetIniValue;
  MyDllInstallDatabase : TMyDllInstallDatabase;

procedure TForm10.Button1Click(Sender: TObject);
begin
  od.FileName := 'C:\work\fhirserver\install\installer.dll';
  if od.Execute then
  begin
    dll := LoadLibrary(pchar(od.FileName));
    if dll < 32 then
      RaiseLastOSError;
    @MyDllListPackages := GetProcAddress(dll, 'MyDllListPackages');
    if @MyDllListPackages = nil then
      raise Exception.create('package list not found: '+SysErrorMessage(GetLastError));
    @MyDllDownloadPackages := GetProcAddress(dll, 'MyDllDownloadPackages');
    if @MyDllDownloadPackages = nil then
      raise Exception.create('package loader not found');
    @MyDllGetIniValue := GetProcAddress(dll, 'MyDllGetIniValue');
    if @MyDllGetIniValue = nil then
      raise Exception.create('ini get not found');
    @MyDllSetIniValue := GetProcAddress(dll, 'MyDllSetIniValue');
    if @MyDllSetIniValue = nil then
      raise Exception.create('ini setter not found');
    @MyDllInstallDatabase := GetProcAddress(dll, 'MyDllInstallDatabase');
    if @MyDllInstallDatabase = nil then
      raise Exception.create('db installer not found');
  end;
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  FreeLibrary(dll);
end;

procedure TForm10.Button3Click(Sender: TObject);
var
  v, s : AnsiString;
  p : string;
  pl, tl : TArray<String>;
begin
  v := edit1.Text;
  s := MyDllListPackages(PAnsiChar(v));
  p := s;
  p := p.replace(#13#10, '~');
  pl := p.Split(['~']);
  CheckListBox1.Items.Clear;
  urls.clear;
  for p in pl do
  begin
    tl := p.Trim.Split(['|']);
    CheckListBox1.Items.Add(tl[2] + ': '+tl[3]);
    CheckListBox1.Checked[CheckListBox1.Items.count- 1] := tl[1] = '1';
    urls.Add(tl[2]);
  end;
end;


procedure TForm10.Button4Click(Sender: TObject);
var
  msg : PAnsiChar;
begin
  try
    msg := MyDllInstallDatabase(
     'C:\work\fhirserver\Server\win64\Debug\fhirserver.exe', 'C:\Program Files\FHIRServer\fhirserver.ini',
       'g', 'r2', ',hl7.fhir.core#1.0.2,fhir.tx.support#1.0.2', 'open', progress);
    if (msg <> nil) then
      ShowMessage('Error: '+ msg);
    progress(0, '');
  except
    on E: Exception do
      ShowMessage(E.ClassName+ ': '+ E.Message);
  end;
end;

procedure TForm10.Button5Click(Sender: TObject);
var
  s : AnsiString;
  i : integer;
begin
  s := '';
  for i := 0 to urls.Count - 1 do
    if CheckListBox1.Checked[i] then
      s := s + ','+urls[i];

  showMessage(s);
  showMessage(MyDllDownloadPackages(PAnsiChar(s), progress));
end;

function StrToPChar(AStr: AnsiString): PAnsiChar;
begin
  if AStr = '' then
    Result := NIL
  else
    begin
    AStr := AStr + #0;
    GetMem(Result, Length(AStr) + 1);
    Move(AStr[1], Result^, Length(AStr));
    end;
end;


procedure TForm10.Button6Click(Sender: TObject);
begin
  eValue.text := MyDllGetIniValue(strtopchar(eINi.Text), strtopchar(eKey.Text));
end;

procedure TForm10.Button7Click(Sender: TObject);
begin
  MyDllSetIniValue(strtopchar(eINi.Text), strtopchar(eKey.Text), strtopchar(eValue.text));
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  urls := TStringList.create;
end;

procedure TForm10.FormDestroy(Sender: TObject);
begin
  urls.Free;
end;

procedure TForm10.progress(IntParam: Integer; StrParam: WideString);
begin
  ProgressBar1.Position := intParam;
  Label1.Caption := StrParam;
  Application.ProcessMessages;
end;

end.
