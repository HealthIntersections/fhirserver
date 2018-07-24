unit IGSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, JclSysUtils, System.zip, System.IOUtils,
  FMX.Dialogs, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Types,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent, FDownloadForm;

type
  TIGSettingsForm = class(TForm)
    btnCheckDependencies: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    Button3: TButton;
    Button4: TButton;
    BStartDownload: TButton;
    EditFileName: TEdit;
    EditURL: TEdit;
    BStopDownload: TButton;
    CheckBox2: TCheckBox;
    GroupBox1: TGroupBox;
    Button2: TButton;
    Button5: TButton;
    Edit2: TEdit;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure btnCheckDependenciesClick(Sender: TObject);
    procedure OnZipProgressEvent (Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure BStartDownloadClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
///////////////////////
  private
    { Private declarations }
    FClient: THTTPClient;
    FGlobalStart: Cardinal;
    FAsyncResult: IAsyncResult;
    FDownloadStream: TStream;
  public
    { Public declarations }
    dependencies:boolean;
    fwInstalled:boolean;
    fwBinFolder:string;
    BaseIGTemplateFolder:String;
  end;

var
  IGSettingsForm: TIGSettingsForm;

implementation

{$R *.fmx}

procedure TIGSettingsForm.BStartDownloadClick(Sender: TObject);
begin
DownloadForm:=TDownloadForm.create(nil);
DownloadForm.url:='https://github.com/costateixeira/ihe_mma/archive/master.zip';
DownloadForm.localFileName:='c:\temp\xxx.zip';
DownloadForm.Show;
DownloadForm.sampleDownload;
DownloadForm.Destroy;

end;

procedure TIGSettingsForm.btnCheckDependenciesClick(Sender: TObject);
var
  str:string;
  myZipFile : TZipFile;

begin
memo1.Lines.Clear;
execute('cmd.exe /C jekyll -v', str,true);

//  try
//    TZipFile.ExtractZipFile('C:\temp\xxx.zip', 'C:\temp\xxxunzipped')
//  except
//  end;

memo1.Lines.Add(str);
end;

procedure TIGSettingsForm.Button1Click(Sender: TObject);
var dir, folder:string;

begin
if SelectDirectory('Select path to IG framework (contains License.md)', '', dir) then begin
  fwBinFolder:=dir;
  edit1.Text:=dir;
end;

end;

procedure TIGSettingsForm.Button2Click(Sender: TObject);
var
url, filename:string;
LStream:TFileStream;
  LHttpClient: THTTPClient;

begin
url:='https://github.com/madhur/PortableJekyll/archive/master.zip';
url:='https://github.com/costateixeira/ihe_mma/archive/master.zip';

fileName:='C:\temp\tempunzipped\xxx.zip';

    LStream := TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyNone);
    try
//      NetHTTPClient1.Get(url,Lstream);
    finally
      LStream.Free;
    end;

end;

procedure TIGSettingsForm.Button3Click(Sender: TObject);
begin
// 1. download and install the IG FW
// 2. copy resource to ig-xxx.xml
// 3. update properties.txt
// 4. copy media\*.* to src\images
// 5. copy pages \*.* to src\pagecontent
// 6. build?





end;

procedure TIGSettingsForm.Button5Click(Sender: TObject);
var dir:string;
begin
if SelectDirectory('Select path to Base IG template', '', dir) then begin
  BaseIGTemplateFolder:=dir;
  edit2.Text:=dir;
end;
end;

procedure TIGSettingsForm.FormShow(Sender: TObject);
begin
edit1.text:=fwbinfolder;
end;

procedure TIGSettingsForm.OnZipProgressEvent(Sender: TObject; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  ProgressBar1.Value := (Position * 100) div Header.UncompressedSize ;
  Application.ProcessMessages;
end;












end.
