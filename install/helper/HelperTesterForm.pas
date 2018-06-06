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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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

var
  MyDllListPackages : TMyDllListPackages;
  MyDllDownloadPackages : TMyDllDownloadPackages;

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
  showmessage('a');
  v := edit1.Text;
  showmessage('b');
  s := MyDllListPackages(PAnsiChar(v));
  showmessage('c');
  p := s;
  p := p.replace(#13#10, '~');
  pl := p.Split(['~']);
  showmessage('d');
  CheckListBox1.Items.Clear;
  urls.clear;
  showmessage('e');
  for p in pl do
  begin
    showmessage('p: '+p);
    tl := p.Trim.Split(['|']);
    showmessage('tl-0: '+tl[0]);
    showmessage('tl-1: '+tl[1]);
    showmessage('tl-2: '+tl[2]);
    showmessage('tl-3: '+tl[3]);
    CheckListBox1.Items.Add(tl[2] + ': '+tl[3]);
    CheckListBox1.Checked[CheckListBox1.Items.count- 1] := tl[1] = '1';
    urls.Add(tl[2]+':'+tl[0]);
  end;
  showmessage('f');
end;

procedure TForm10.Button4Click(Sender: TObject);
begin
(*
procedure TInstallerCallbackHandler.Callback(IntParam: Integer; StrParam: WideString);
begin
  writeln(strParam, ' ', intParam);
end;

var
  dll : THandle;
  funcSCT : TInstallSnomedFunction;
  funcDB : TInstallDatabaseFunction;
  cb : TInstallerCallbackHandler;
  msg : PAnsiChar;
begin
  try
    Writeln('Installation tester.');
    Writeln('');
    cb := TInstallerCallbackHandler.create;
    dll := LoadLibraryA('C:\work\fhirserver\install\installer.dll');
    try
      @funcSCT := GetProcAddress(dll, 'MyDllInstallSnomed');
      @funcDB := GetProcAddress(dll, 'MyDllInstallDatabase');
      msg := funcSCT('C:\work\fhirserver\Server\win64_3\Debug\fhirserver3.exe', 'C:\data\terminologies\sct-au\20160430', 'C:\Program Files\FHIRServer\snomed_32506021000036107_20160531.cache', 'http://snomed.info/sct/32506021000036107/version/20160531', cb.Callback);
      if (msg <> nil) then
        Writeln('Error: ', msg);
      msg := funcDB('C:\work\fhirserver\Server\win64_3\Debug\fhirserver3.exe', 'C:\Program Files\FhirServer\fhirserver.ini', 'g', 'C:\Program Files\FhirServer\load\load.ini', cb.Callback);
      if (msg <> nil) then
        Writeln('Error: ', msg);
    finally
      cb.Free;
      FreeLibrary(dll);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
*)
end;

procedure TForm10.Button5Click(Sender: TObject);
var
  s : AnsiString;
  i : integer;
begin
  s := '';
  for i := 0 to urls.Count - 1 do
    if CheckListBox1.Checked[i] then
      s := s + '|'+urls[i];

  showMessage(MyDllDownloadPackages(PAnsiChar(s), progress));
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
