unit mvDataSources;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  mvBase;

type
  { TDataSourceViewManager }

  TDataSourceViewManager = class (TViewManager)
  private
  public
    procedure initialize; override;
  end;

implementation

{ TDataSourceViewManager }

procedure TDataSourceViewManager.initialize;
begin
  inherited initialize;
end;

end.

