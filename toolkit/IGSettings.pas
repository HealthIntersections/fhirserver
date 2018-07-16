unit IGSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, JclSysUtils, System.zip,
  FMX.Dialogs, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Types,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TIGSettingsForm = class(TForm)
    btnCheckDependencies: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    NetHTTPClient1: TNetHTTPClient;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnCheckDependenciesClick(Sender: TObject);
    procedure OnZipProgressEvent (Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    dependencies:boolean;
    fwInstalled:boolean;
    fwBinFolder:string;

  end;

var
  IGSettingsForm: TIGSettingsForm;

implementation

{$R *.fmx}

procedure TIGSettingsForm.btnCheckDependenciesClick(Sender: TObject);
var
  str:string;
  myZipFile : TZipFile;

begin
execute('jekyll -v', str,true);
  myZipFile := TZipFile.Create;
  try
    myZipFile.ExtractZipFile('C:\temp\temp.zip','C:\temp\tempunzipped',OnZipProgressEvent);
    myZipFile.Close;
  finally
    myZipFile.Free;
  end;


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
      NetHTTPClient1.Get(url,Lstream);
    finally
      LStream.Free;
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
