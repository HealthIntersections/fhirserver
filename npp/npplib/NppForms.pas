{
    This file is part of DBGP Plugin for Notepad++
    Copyright (C) 2007  Damjan Zobo Cvetko

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit NppForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, FHIR.Npp.Base;

type
  TNppForm = class(TForm)
  private
    { Private declarations }
  protected
    procedure RegisterForm();
    procedure UnregisterForm();
    procedure DoClose(var Action: TCloseAction); override;
  public
    { Public declarations }
    Npp: TNppPlugin;
    DefaultCloseAction: TCloseAction;
    constructor Create(NppParent: TNppPlugin); reintroduce; overload;
    constructor Create(AOwner: TNppForm); reintroduce; overload;
    destructor Destroy; override;
//    procedure Show;
    function WantChildKey(Child: TControl; var Message: TMessage): Boolean; override;
  end;

var
  NppForm: TNppForm;

implementation

{$R *.dfm}

{ TNppForm }

constructor TNppForm.Create(NppParent: TNppPlugin);
begin
  self.Npp := NppParent;
  self.DefaultCloseAction := caNone;
  inherited Create(nil);
  // We figure right now this does more damage than good.
  // So let the main transalte and dispatch do it's thing instead of isdialogmessage
  self.RegisterForm();
end;

constructor TNppForm.Create(AOwner: TNppForm);
begin
  self.Npp := AOwner.Npp;
  self.DefaultCloseAction := caNone;
  inherited Create(Aowner);
  //self.RegisterForm();
end;

destructor TNppForm.Destroy;
begin
  if (self.HandleAllocated) then
  begin
    self.UnregisterForm();
  end;
  inherited;
end;

procedure TNppForm.RegisterForm();
var
  r: Integer;
begin
  r := SendMessage(self.Npp.NppData.NppHandle, NPPM_MODELESSDIALOG, MODELESSDIALOGADD, self.Handle);
{
  if (r = 0) then
  begin
    ShowMessage('Failed reg of form '+form.Name);
    exit;
  end;
}
end;

procedure TNppForm.UnregisterForm();
var
  r: Integer;
begin
  if (not self.HandleAllocated) then exit;
  r := SendMessage(self.Npp.NppData.NppHandle, NPPM_MODELESSDIALOG, MODELESSDIALOGREMOVE, self.Handle);
{
  if (r = 0) then
  begin
    ShowMessage('Failed unreg form '+form.Name);
    exit;
  end;
}
end;

procedure TNppForm.DoClose(var Action: TCloseAction);
begin
  if (self.DefaultCloseAction <> caNone) then Action := self.DefaultCloseAction;
  inherited;
end;

// This is going to help us solve the problems we are having because of N++ handling our messages
function TNppForm.WantChildKey(Child: TControl; var Message: TMessage): Boolean;
begin
  Result := Child.Perform(CN_BASE + Message.Msg, Message.WParam, Message.LParam) <> 0;
end;

end.
