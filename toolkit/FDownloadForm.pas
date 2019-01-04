//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.
//
// This software is the copyrighted property of Embarcadero Technologies, Inc.
// ("Embarcadero") and its licensors. You may only use this software if you
// are an authorized licensee of Delphi, C++Builder or RAD Studio
// (the "Embarcadero Products").  This software is subject to Embarcadero's
// standard software license and support agreement that accompanied your
// purchase of the Embarcadero Products and is considered a Redistributable,
// as such term is defined thereunder. Your use of this software constitutes
// your acknowledgement of your agreement to the foregoing software license
// and support agreement.
//---------------------------------------------------------------------------
unit FDownloadForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, System.Zip,
  IdSSLOpenSSL, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL;

type

  TDownload = class;

  TDownloadForm = class(TForm)
    ProgressBar1: TProgressBar;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    sourceURL:String;
    localFileName:String;
    UnzipLocation:string;
    Unzip:boolean;
    busy:boolean;
    FCancel: boolean;
  end;

  TDownload = class(TThread)
  private
    httpclient: TIdHTTP;
    url: string;
    filename: string;
    unzipLocation:string;
    maxprogressbar: integer;
    progressbarstatus: integer;
    procedure ExtractZip(ZipFile: string; ExtractPath: string);
    procedure idhttp1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure idhttp1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure UpdateProgressBar;
    procedure SetMaxProgressBar;
  protected
    linkedForm:TDownloadForm;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; aurl, afilename, aunzipLocation: string; aLinkedForm:TDownloadForm);
    destructor Destroy; override;
//    property Cancel: boolean read FCancel write FCancel;
  end;

var
  DownloadForm: TDownloadForm;
  DownloadThread: TDownload;

implementation

{$R *.fmx}

constructor TDownload.Create(CreateSuspended: boolean; aurl, afilename, aunzipLocation: string; aLinkedForm:TDownloadForm);
begin
  inherited Create(CreateSuspended);
  linkedForm:=aLinkedForm;
  unzipLocation:= aunzipLocation;
  httpclient := TIdHTTP.Create(nil);
  httpclient.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(httpclient);
  TIdSSLIOHandlerSocketOpenSSL(httpclient.IOHandler).SSLOptions.SSLVersions:=[sslvTLSv1_1,sslvTLSv1_2];

  httpclient.Request.UserAgent := 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; SLCC1';
  httpclient.HandleRedirects := true;
  httpclient.OnWorkBegin := idhttp1WorkBegin;
  httpclient.OnWork := idhttp1Work;
  url := aurl;
  filename := afilename;
end;

procedure TDownload.idhttp1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  progressbarstatus := AWorkCount;

  if linkedForm.FCancel then
  begin
    linkedForm.FCancel := False;
    httpclient.Disconnect;
    linkedForm.busy:=false;
  end;

  Queue(UpdateProgressBar);
end;

procedure TDownload.idhttp1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  maxprogressbar := AWorkCountMax;
  Queue(SetMaxProgressBar);
end;

procedure TDownload.Execute;
var
  Stream: TMemoryStream;
  unzip:Boolean;
begin
unzip:=true;
  Stream := TMemoryStream.Create;
  try
    linkedForm.busy:=true;
    httpclient.Get(linkedForm.sourceURL, Stream);
    Stream.SaveToFile(filename);
    if unzip then begin
        linkedForm.caption := 'Done Downloading. Extracting...';
        ExtractZip(linkedForm.localFileName, UnzipLocation);
    end else begin
        linkedForm.caption := 'Done Downloading';
    end;
  finally
    Stream.Free;
    linkedForm.busy:=false;
  end;
end;

procedure TDownload.UpdateProgressBar;
var
  ZipFile: string;
begin
  linkedForm.ProgressBar1.value := progressbarstatus;
  linkedForm.Button1.text := 'Downloading...';
  linkedform.Label1.Text:= inttostr(progressbarstatus div 1024) + ' of ' + inttostr(maxprogressbar div 1024) ;
end;

procedure TDownload.SetMaxProgressBar;
begin
  linkedForm.ProgressBar1.Max := maxprogressbar;
end;

destructor TDownload.Destroy;
begin
  FreeAndNil(httpclient);
  inherited Destroy;
end;

procedure TDownload.ExtractZip(ZipFile, ExtractPath: string);
begin
  if TZipFile.IsValid(ZipFile) then
    try
      TZipFile.ExtractZipFile(ZipFile, ExtractPath);
      DeleteFile(ZipFile);
      linkedForm.Caption := 'Done.';
      linkedForm.Button1.Enabled := true;
      linkedForm.close;
    except
    end
  else
  begin
    linkedForm.Button1.Text := 'Error Extracting files';

  end;
end;

procedure TDownloadForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TDownloadForm.Button2Click(Sender: TObject);
begin
fcancel:=true;
//busy:=false;
close;
end;

procedure TDownloadForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if busy then canClose:=false;
 if assigned(DownloadThread) then fcancel:=true;
end;

procedure TDownloadForm.FormShow(Sender: TObject);
var
//  DownloadThread: TDownload;
  link: string;
begin
  ProgressBar1.value := 0;
  Button1.Enabled := False;
  Caption := 'Starting Download...';
  link := sourceUrl;
  DownloadThread := TDownload.Create(true, link, localFileName, UnzipLocation, self);
  DownloadThread.FreeOnTerminate := true;
  DownloadThread.Start;
end;

end.

