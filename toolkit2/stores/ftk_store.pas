unit ftk_store;

{$i fhir.inc}

interface

uses
  SysUtils, Classes, IniFiles,
  fsl_base;

type

  TLoadedBytes = record
    content : TBytes;
    timestamp : TDateTime;
  end;

  { TStorageService }

   TStorageService = class abstract (TFslObject)
   private
     FHandle : TComponent;
   protected
     FIni : TIniFile;
   public
     constructor create(handle : TComponent; ini : TIniFile);

     function link : TStorageService; overload;

     property Handle : TComponent read FHandle;

     function scheme : String; virtual; abstract;
     function CheckTimes : boolean; virtual; abstract;
     function openDlg(out newName : String) : boolean; virtual; abstract;
     function saveDlg(existing : String; suggestedExtension : String; out newName : String) : boolean; virtual; abstract;

     function CaptionForAddress(address : String) : String; virtual; abstract;
     function describe(address : String) : String; virtual; abstract;
     function MakeFilename(address : String) : String; virtual; abstract;

     function load(address : String) : TLoadedBytes; virtual; abstract;
     function save(address : String; bytes : TBytes) : TDateTime; virtual; abstract;
     procedure delete(address : String); virtual; abstract;
   end;


implementation

{ TStorageService }

constructor TStorageService.create(handle: TComponent; ini : TIniFile);
begin
  inherited create;
  FHandle := handle;
  FIni := ini;
end;

function TStorageService.link: TStorageService;
begin
  result := TStorageService(inherited link);
end;

end.
