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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.zip,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.ImageList, FMX.ImgList;

type
  TDownloadForm = class(TForm)
    PanelTop: TPanel;
    PanelCenter: TPanel;
    LabelFile: TLabel;
    EditFileName1: TEdit;
    BStartDownload: TButton;
    Memo1: TMemo;
    ImageList1: TImageList;
    LabelURL: TLabel;
    EditURL1: TEdit;
    LabelGlobalSpeed: TLabel;
    ProgressBarDownload: TProgressBar;
    BStopDownload: TButton;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure BStartDownloadClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure BStartComponentClick(Sender: TObject);
    procedure ReceiveDataEvent(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function GetZipSize(const Filename: string): integer;

  private
    { Private declarations }
    FClient: THTTPClient;
    FGlobalStart: Cardinal;
    FAsyncResult: IAsyncResult;
    FDownloadStream: TStream;
//    ProcessedBytes: cardinal;
    procedure ZipOnProgress(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
    procedure DoEndDownload(const AsyncResult: IAsyncResult);
  public
    { Public declarations }
    url:String;
    localFileName:String;
    UnzipLocation:string;
//    UnzipFile:String;
    Unzip:boolean;
    DownloadComplete:boolean;
    procedure SampleDownload;
    procedure DoUnzip;
  end;

var
  DownloadForm: TDownloadForm;
  zf:TZipFile;
  currsize, processedsofar, totalsize:cardinal;
  PreviousFilename:string;

implementation
{$R *.fmx}

uses
  System.IOUtils;




function TDownloadForm.GetZipSize(const filename: string): integer;
var
  StreamArquivo: TFileStream;
begin
  StreamArquivo := TFileStream.Create(filename, fmOpenRead);
  try
    result := StreamArquivo.Size;
  finally
    StreamArquivo.Free;
  end;
end;

procedure TDownloadForm.BStartDownloadClick(Sender: TObject);
begin
  DownloadComplete:=false;
  SampleDownload;
end;

procedure TDownloadForm.ButtonCancelClick(Sender: TObject);
begin
  (Sender as TButton).Enabled := False;
  FAsyncResult.Cancel;
end;

procedure TDownloadForm.BStartComponentClick(Sender: TObject);
begin
  BStartDownload.Enabled := False;
  SampleDownload;
end;

procedure TDownloadForm.DoEndDownload(const AsyncResult: IAsyncResult);
var
  LAsyncResponse: IHTTPResponse;
begin
  try
    LAsyncResponse := THTTPClient.EndAsyncHTTP(AsyncResult);
    TThread.Synchronize(nil,
      procedure
      begin
        Memo1.Lines.Add('Download Finished!');
        Memo1.Lines.Add(Format('Status: %d - %s', [LAsyncResponse.StatusCode, LAsyncResponse.StatusText]));
      end);
  finally
    LAsyncResponse := nil;
    FreeandNil(FDownloadStream);
    if unzip then try
      doUnzip;
      except
      end;

    BStopDownload.Enabled := False;
    BStartDownload.Enabled := True;
    DownloadComplete:=true;

  end;

end;


procedure TDownloadForm.DoUnzip;
var tx:tarray<tzipheader>;
i:integer;
begin
  totalsize:=0;
  processedsofar:=0;
  zf.OnProgress:=ZipOnProgress;
  PreviousFilename:='';
  if not directoryexists(UnzipLocation) then
    try
      ForceDirectories(UnzipLocation);
    except
    end;
  try
    zf.Open(LocalFileName, zmRead);
    tx:=zf.FileInfos;
    for i:=0 to length(tx)-1 do
      totalsize:=totalsize+tx[i].unCompressedSize;
    zf.close;
    zf.Open(LocalFileName, zmRead);
    zf.ExtractAll(UnzipLocation);
    zf.Close;
  except
  end;
end;


procedure TDownloadForm.ZipOnProgress(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
begin

   if PreviousFilename <> FileName then
     begin
       processedsofar:=processedsofar+currsize;
       PreviousFilename := FileName;
     end
     else
     currsize:=Header.uncompressedSize;

  progressbardownload.Value:= 100 * Position div  Header.uncompressedSize;
  progressbardownload.Value:= 100 * (Position + processedsofar) div  totalsize;
  progressbardownload.repaint;

end;


procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  zf:=TZipFile.create;
  FClient := THTTPClient.Create;
  DownloadComplete:=false;
  FClient.OnReceiveData := ReceiveDataEvent;
end;

procedure TDownloadForm.FormDestroy(Sender: TObject);
begin
  zf.destroy;
  FDownloadStream.Free;
  FClient.Free;
end;

procedure TDownloadForm.ReceiveDataEvent(const Sender: TObject; AContentLength, AReadCount: Int64;
  var Abort: Boolean);
var
  LTime: Cardinal;
  LSpeed: Integer;
begin
  LTime := TThread.GetTickCount - FGlobalStart;
  LSpeed := (AReadCount * 1000) div LTime;
  TThread.Queue(nil,
    procedure
    begin
      ProgressBarDownload.Value := AReadCount;
      LabelGlobalSpeed.Text := Format('Global speed: %d KB/s', [LSpeed div 1024]);
    end);
end;

procedure TDownloadForm.SampleDownload;
var
  LResponse: IHTTPResponse;
  LFileName: string;
  LSize: Int64;
begin
  BStartDownload.Enabled := False;
  LFileName := TPath.Combine(TPath.GetDocumentsPath, LocalFileName);
  LocalFileName := LFileName;

  try
//    URL := URL;
    LResponse := FClient.Head(URL);
    LSize := LResponse.ContentLength;
    Memo1.Lines.Add(Format('Head response: %d - %s', [LResponse.StatusCode, LResponse.StatusText]));
    LResponse := nil;

    ProgressBarDownload.Max := LSize;
    ProgressBarDownload.Min := 0;
    ProgressBarDownload.Value := 0;
    LabelGlobalSpeed.Text := 'Global speed: 0 KB/s';

    Memo1.Lines.Add(Format('Downloading: "%s" (%d Bytes) into "%s"' , [LocalFileName, LSize, LFileName]));

    // Create the file that is going to be dowloaded
    FDownloadStream := TFileStream.Create(LFileName, fmCreate);
    FDownloadStream.Position := 0;

    FGlobalStart := TThread.GetTickCount;

    // Start the download process
    FAsyncResult := FClient.BeginGet(DoEndDownload, URL, FDownloadStream);

  finally
    BStopDownload.Enabled := FAsyncResult <> nil;
    BStartDownload.Enabled := FAsyncResult = nil;
  end;
end;

end.
