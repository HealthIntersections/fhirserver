unit FHIR.Transformer.Utilities;

interface

uses
  WinApi.Windows, SysUtils, Messages, Forms, Classes, Dialogs, Graphics, Controls, StdCtrls, Consts,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream;

type
  TMsgBoxType = (mbInformation, mbConfirmation, mbError, mbCriticalError);


function MsgBoxP(const Text, Caption: PChar; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
function MsgBox(const Text, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
function MsgBoxFmt(const Text: String; const Args: array of const; const Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;

implementation

var
  MessageBoxCaptions: array[TMsgBoxType] of PChar;

function MsgBoxFmt(const Text: String; const Args: array of const; const Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
begin
  Result := MsgBox(Format(Text, Args), Caption, Typ, Buttons);
end;

function MsgBox(const Text, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
begin
  Result := MsgBoxP(PChar(Text), PChar(Caption), Typ, Buttons);
end;

function MsgBoxP(const Text, Caption: PChar; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
const
  IconFlags: array[TMsgBoxType] of Cardinal = (MB_ICONINFORMATION, MB_ICONQUESTION, MB_ICONEXCLAMATION, MB_ICONSTOP);
//  DefaultCaptions: array[TMsgBoxType] of Word = (SMsgDlgInformation, SMsgDlgConfirm, SMsgDlgError, SMsgDlgError);
var
  C: PChar;
  NewCaption: String;
begin
  C := Caption;
  if (C = nil) or (C[0] = #0) then begin
    C := MessageBoxCaptions[Typ];
    if C = nil then begin
//      NewCaption := LoadStr(DefaultCaptions[Typ]);
      C := PChar(NewCaption);
    end;
  end;
//  Result := AppMessageBox(Text, C, Buttons or IconFlags[Typ]);
end;


end.
