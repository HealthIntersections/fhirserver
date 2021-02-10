unit fui_lcl_utilities;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  Controls, Forms;

procedure setForOS(btnOk, btnCancel : TControl);

implementation

procedure setForOS(btnOk, btnCancel : TControl);
{$IFNDEF WINDOWS}
var
  l : integer;
{$ENDIF}
begin
  {$IFNDEF WINDOWS}
  l := btnCancel.left;
  btnCancel.left := btnOk.left;
  btnOk.left := l;
  {$ENDIF}
end;

end.

