unit FHIR.Ui.ColorSB;

interface

uses
  SysUtils, Classes, Messages, Graphics, Types, Controls, Buttons;

type
  TColorSpeedButton = class(TSpeedButton)
  private
    FColor: TColor;
    procedure SetColor(const Value: TColor);
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    Constructor Create(owner : TComponent);
  published
    property Color : TColor read FColor write SetColor;
  end;

Procedure Register;

implementation

Procedure Register;
Begin
  RegisterComponents('Misc', [TColorSpeedButton]);
End;



{ TColorSpeedButton }

constructor TColorSpeedButton.Create(owner: TComponent);
begin
  inherited;
  Color := clBtnFace;
end;

procedure TColorSpeedButton.SetColor(const Value: TColor);
begin
  FColor := Value;
  Invalidate;
end;

procedure TColorSpeedButton.WMPaint(var Message: TWMPaint);
begin
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect(2, 2, Width-2, Height-2));
  inherited;
end;

end.
