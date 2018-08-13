unit Import2html;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Types, System.UITypes, System.Classes, System.Variants,
  shellapi, winapi.windows, FMX.platform.win, FMX.DialogService,

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
  currFolder: string;

begin

  if igContentFolder = '' then
    exit;
  if igPublisherFolder = '' then
    exit;

  // igrootfolder:='C:\ImpGuide';
  // igcontentfolder := igrootfolder + '\content';
  tempfolder := igContentFolder + '\temp';
  pagecontentfolder := igContentFolder + '\pagecontent';
  mediafolder := igContentFolder + '\images';

  // IGPublisherFolder := igrootfolder + '\publish';
  pandocfolder := igPublisherFolder + '\framework\pandoc';

  if not(directoryexists(pwidechar(tempfolder))) then
    ForceDirectories(pwidechar(tempfolder));

  currFolder := getcurrentdir;

  OpenDialog2.InitialDir := igrootfolder;
  if OpenDialog2.Execute then
  begin
    // copy doc to tmp.ocx
    tempstr := tempfolder + '\tmp.docx';
    copyfile(pwidechar(OpenDialog2.filename), pwidechar(tempstr), false);
    ///

    // convert to tmp.txt
    optionsStr := '--extract-media ' + tempfolder + ' --from=docx --to=html4 --resource-path=. ' + tempfolder + '\tmp.docx -o ' + tempfolder + '\tmp.txt';
    runAndWait(pandocfolder, 'pandoc', optionsStr);
    ///

    // delete tmp.docx
    tempstr := tempfolder + '\tmp.docx';
    deletefile(pwidechar(tempstr));
    try
    tfile.delete(tempfolder + '\tmp_display.html');
    except

    end;
    ///


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


    tfile.Copy(tempfolder + '\tmp.html', tempfolder + '\tmp_display.html');
    FileReplaceString(tempfolder + '\tmp.xml',tempfolder + '/media', './');
    FileReplaceString(tempfolder + '\tmp.html',tempfolder + '/media', './');


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
  strList:TstringList;
  S: string;

begin

{  memo1.lines.LoadFromFile(FileName);
  memo1.lines.Text:=StringReplace(memo1.lines.Text, SearchString, replaceString, [rfReplaceAll, rfIgnoreCase]);
    memo1.lines.SaveToFile(FileName);

    exit;
    }
{  fs := TFileStream.Create(FileName, fmOpenread or fmShareDenyNone);
  try
      SetLength(S, fs.Size);
          fs.ReadBuffer(S[1], fs.Size);
            finally
                fs.Free;
                  end;


                    S  := StringReplace(S, SearchString, replaceString, [rfReplaceAll, rfIgnoreCase]);
                      fs := TFileStream.Create(FileName+'2', fmCreate);
                        try
                            fs.WriteBuffer(S[1], Length(S));
                              finally
                                  fs.Free;
                                    end;}

  strList:=TStringList.create;

  strList.LoadFromFile(FileName);
  strList.Text:=StringReplace(strList.Text, SearchString, replaceString, [rfReplaceAll, rfIgnoreCase]);
  strList.SaveToFile(FileName);
  strList.Free;

end;




procedure TContentImport.savePageContent(pagefilename: string);
var
  sa: TStringDynArray;
  i: integer;

begin

  if pagefilename <> '' then
    try
      // move temporary media folder files to actual media
      if directoryexists(tempfolder + '\media') then
      begin
        sa := TDirectory.GetFiles(tempfolder + '\media');
        for i := 0 to Length(sa) - 1 do
        begin
          destfile := mediafolder + '\' + extractfilename(sa[i]);
          movefile(pwidechar(sa[i]), pwidechar(destfile));
        end;
      end;
      ///


      // To do: Delete all files in temp media folder?
      // to do: deletefile(pwidechar(tempfolder + '\tmp.html'));


      // copy to edit1-text to actual pageContentFolder

      copyfile(pwidechar(tempfolder + '\tmp.html'), pwidechar(pagecontentfolder + '\' +  ChangeFileExt(pagefilename, '.xml')), false);
    except
    end
  else
    TDialogService.MessageDialog('File not saved - page name is empty.' + #13#10 + 'To save, enter a page name', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOK],
      System.UITypes.TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
      end);;

  // TDirectory.Delete(mediafolder, True);

  ///

  TDirectory.Delete(tempfolder, True);

end;

procedure TContentImport.runAndWait(Path, command, parameters: String);
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
        application.ProcessMessages;
        GetExitCodeProcess(SEInfo.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE) or application.Terminated;
    end;
  end;

end;

end.
