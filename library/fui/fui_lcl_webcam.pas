unit fui_lcl_webcam;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls,
  {$IFDEF WINDOWS}
  MfObjects, MfIdl, MfApi;
  {$ENDIF}


type
  { TWebCamManager }

  TWebCamManager = class (TComponent)
  private
    FOnCameraListChange: TNotifyEvent;
  public
    procedure listCameras(list : TStringList);
    property OnCameraListChange : TNotifyEvent read FOnCameraListChange write FOnCameraListChange;
  end;

  { TWebCamViewer }

  TWebCamViewer = class (TCustomPanel)
  private
    FActive: boolean;
    FWebcamName: String;
    procedure SetActive(AValue: boolean);
    procedure SetWebCamName(AValue: String);
  public
    function snapshot : TBitmap;
    property webcamName : String read FWebcamName write SetWebCamName;
    property Active : boolean read FActive write SetActive;
  end;

implementation

{$IFDEF WINDOWS}

{ TWebCamManager }

procedure TWebCamManager.listCameras(list: TStringList);
var
  attr : IMFAttributes;
begin
  MFCreateAttributes(attr, 1);
  attr.SetGUID(@MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE, @MF_DEVSOURCE_ATTRIBUTE_SOURCE_TYPE_VIDCAP_GUID);

 // !
end;

{ TWebCamViewer }

procedure TWebCamViewer.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
end;

procedure TWebCamViewer.SetWebCamName(AValue: String);
begin
  if FWebcamName=AValue then Exit;
  FWebcamName:=AValue;
end;

function TWebCamViewer.snapshot: TBitmap;
begin

end;

{$ENDIF}
end.

