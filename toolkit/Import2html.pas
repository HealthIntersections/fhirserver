unit Import2html;

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

interface

uses
{$IFDEF OSX}
{$ELSE}
  Winapi.Shellapi, Winapi.Windows, FMX.Platform.Win, //JclSysUtils,
{$ENDIF}
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.DialogService,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.WebBrowser, FMX.ScrollBox, FMX.Memo;

type
  TContentImport = class(TForm)
    Edit1: TEdit;
    Button7: TButton;
    OpenDialog2: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    WebBrowser1: TWebBrowser;
    Button3: TButton;
    procedure Button7Click(Sender: TObject);
    procedure runAndWait(Path, command, parameters: String);
    procedure savePageContent(pagefilename: string);
    procedure Button2Click(Sender: TObject);
    procedure FileReplaceString(const FileName, searchstring, replacestring: string);

  private
    { Private declarations }
  public
    { Public declarations }
    destfile, igrootfolder, igContentFolder, igPublisherFolder, tempfolder, pandocfolder, mediafolder, pagecontentfolder: string;
  end;

var
  ContentImport: TContentImport;

implementation

{$R *.fmx}

uses
  fsl_base;

procedure TContentImport.Button2Click(Sender: TObject);
begin
  savePageContent(Edit1.text);
end;

procedure TContentImport.Button7Click(Sender: TObject);
var
  optionsStr, tempstr, filestr, sCmd, ExecuteFile, ParamString, StartInString: string;
  SL: TStringList;
  sa: TStringDynArray;
  i: integer;
  imageFiles: TStringDynArray;
  imgfile:string;
  currFolder: string;

begin
  if igContentFolder = '' then
    exit;
  if igPublisherFolder = '' then
    exit;

//  tempfolder := igContentFolder + '\temp';
//  pagecontentfolder := igContentFolder + '\pagecontent';
  mediafolder := igContentFolder + '\images';
  pandocfolder := igPublisherFolder + '\framework\pandoc';

  if not(directoryexists(pwidechar(tempfolder))) then
    ForceDirectories(pwidechar(tempfolder));

  currFolder := getcurrentdir;

  OpenDialog2.InitialDir := igrootfolder;
  if OpenDialog2.Execute then
  begin
    // copy doc to tmp.ocx
    tempstr := tempfolder + '\tmp.docx';
    TFile.copy(OpenDialog2.FileName, tempstr, true);
    ///

    // convert to tmp.txt
    optionsStr := '--extract-media ' + tempfolder + ' --from=docx --to=html4 --resource-path=. ' + tempfolder + '\tmp.docx -o ' + tempfolder + '\tmp.txt';
    optionsStr := '--extract-media . --from=docx --to=html4 --resource-path=. ' + tempfolder + '\tmp.docx -o ' + tempfolder + '\tmp.txt';
    runAndWait(pandocfolder, 'pandoc', optionsStr);
    ///

    // delete tmp.docx
    tempstr := tempfolder + '\tmp.docx';
    deletefile(pwidechar(tempstr));
    try
      TFile.delete(tempfolder + '\tmp_display.html');
    except

    end;
    ///


    imagefiles:= TDirectory.GetFiles(pandocfolder + '\media');
    for imgfile in imageFiles do
      TFile.copy(imgfile, tempfolder + '\'+edit1.text+'-'+extractfilename(imgfile));


    // delete media folder

    // convert to tempfolder\tmp.html and preview
    SL := TStringList.Create;
    try
      SL.LoadFromFile(tempfolder + '\tmp.txt');
      begin
        SL.Insert(0,
          '<div xmlns="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> <head> <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> </head>');
        SL.Append('</div>');
        SL.SaveToFile(tempfolder + '\tmp.xml');
      end;
      begin
        SL.Insert(0, '<html><body>');
        SL.Append('</body></html>');
        SL.SaveToFile(tempfolder + '\tmp.html');
      end;
    finally
      SL.Free;
    end;

    TFile.copy(tempfolder + '\tmp.html', tempfolder + '\tmp_display.html');
    FileReplaceString(tempfolder + '\tmp.xml',  './media/', './'+edit1.text+'-');
    FileReplaceString(tempfolder + '\tmp.html', './media/', './'+edit1.text+'-');
    FileReplaceString(tempfolder + '\tmp_display.html', './media/', './'+edit1.text+'-');

    filestr := tempfolder + '\tmp_display.html';
    filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    WebBrowser1.URL := filestr;

    if Edit1.text <> '' then
      try
//        savePageContent(Edit1.text);
      except
      end
    else
      TDialogService.MessageDialog('File not saved - page name is empty.' + #13#10 + 'To save, enter a page name', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOK],
        System.UITypes.TMsgDlgBtn.mbOK, 0,
        procedure(const AResult: TModalResult)
        begin
        end);;



    // to here

    SetCurrentDir(currFolder);

  end;

end;

procedure TContentImport.FileReplaceString(const FileName, searchstring, replacestring: string);
var
  fs: TFileStream;
  strList: TStringList;
  S: string;

begin
  strList := TStringList.Create;
  strList.LoadFromFile(FileName);
  strList.text := stringreplace(strList.text, searchstring, replacestring, [rfReplaceAll, rfIgnoreCase]);
  strList.SaveToFile(FileName);
  strList.Free;

end;

procedure TContentImport.savePageContent(pagefilename: string);
var
  sa: TStringDynArray;
  i: integer;
  srcfile, destfile: string;

begin

  if pagefilename <> '' then
begin
    try
      if directoryexists(tempfolder + '\media') then
      begin
        sa := TDirectory.GetFiles(tempfolder + '\media');
        for i := 0 to Length(sa) - 1 do
        begin
          destfile := mediafolder + '\' + extractfilename(sa[i]);
          TFile.Move(sa[i], destfile);
        end;
      end;
    except
    end;
   try
      srcfile := tempfolder + '\tmp.xml';
      destfile := pagecontentfolder + '\' + ChangeFileExt(pagefilename, '.xml');
      TFile.copy(srcfile, destfile, true);
    except
    end
end

  else
    TDialogService.MessageDialog('File not saved - page name is empty.' + #13#10 + 'To save, enter a page name', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOK],
      System.UITypes.TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
      end);;

  // TDirectory.Delete(mediafolder, True);

  // TO DO: need to copy images

  // TDirectory.Delete(tempfolder, True);

end;

procedure TContentImport.runAndWait(Path, command, parameters: String);
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}

var
  folderstr, filestr: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  sCmd, ExecuteFile, ParamString, StartInString: string;

begin
  folderstr := getcurrentdir;

  begin
    FillChar(SEInfo, SizeOf(SEInfo), 0);
    SEInfo.cbSize := SizeOf(TShellExecuteInfo);
    with SEInfo do
    begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      // Wnd := FmxHandleToHWND(ImplementationGuideEditor.Handle);
      lpFile := PChar(command);
      lpDirectory := PChar(Path);
      lpParameters := PChar(parameters);
      nShow := SW_SHOWNORMAL;
    end;
    if ShellExecuteEx(@SEInfo) then
    begin
      repeat
        // application.ProcessMessages;
        GetExitCodeProcess(SEInfo.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE) or application.Terminated;
    end;
  end;
end;
{$ENDIF}

end.
