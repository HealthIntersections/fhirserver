unit FHIR.Toolkit.TextEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  ComCtrls,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Toolkit.Context;

type

  { TTextEditor }

  TTextEditor = class (TToolkitEditor)
  protected
    function GetBytes: TBytes; override;
    procedure SetBytes(AValue: TBytes); override;
    function GetCanBeSaved: boolean; override;
  public
    procedure newContent; override;
    procedure bindToTab(tab : TTabSheet); override;
    procedure locate(location : TSourceLocation); override;
  end;

implementation

{ TTextEditor }

function TTextEditor.GetBytes: TBytes;
begin

end;

procedure TTextEditor.SetBytes(AValue: TBytes);
begin

end;

function TTextEditor.GetCanBeSaved: boolean;
begin
  result := true;
end;

procedure TTextEditor.newContent;
begin

end;

procedure TTextEditor.bindToTab(tab: TTabSheet);
begin
  // set the tab caption
  //add the tool bar
  //add the syn edit
  //set up the high lighting
  //

end;

procedure TTextEditor.locate(location: TSourceLocation);
begin

end;

end.

