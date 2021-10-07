unit frm_file_changed;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TModifiedFileAction = (dmaSave, dmaDiff, dmaReload, dmaIgnore, dmaNoCheck);

  { TModifiedFileActionForm }

  TModifiedFileActionForm = class(TForm)
    Button3: TButton;
    Button4: TButton;
    ModifiedFileActionForm: TButton;
    Button5: TButton;
    Button6: TButton;
    lblDetails: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ModifiedFileActionFormClick(Sender: TObject);
  private
    FAction : TModifiedFileAction;
  public

  end;

var
  ModifiedFileActionForm: TModifiedFileActionForm;

function checkModifiedFileAction(owner : TComponent; filename : String; saved, loaded : TDateTime) : TModifiedFileAction;

implementation

{$R *.lfm}

function checkModifiedFileAction(owner : TComponent; filename : String; saved, loaded : TDateTime) : TModifiedFileAction;
begin
  ModifiedFileActionForm := TModifiedFileActionForm.create(owner);
  try
    ModifiedFileActionForm.lblDetails.Caption := 'The content at '+filename+' has been modified behind the scenes. '+
      'The loaded time is '+FormatDateTime('c', loaded)+', and the saved time is '+FormatDateTime('c', saved)+'. What do you want to do?';
    ModifiedFileActionForm.FAction := dmaIgnore;
    ModifiedFileActionForm.ShowModal;
    result := ModifiedFileActionForm.FAction;
  finally
    ModifiedFileActionForm.free;
  end;
end;

{ TModifiedFileActionForm }

procedure TModifiedFileActionForm.Button6Click(Sender: TObject);
begin
  FAction := dmaDiff;
  ModalResult := mrOK;
end;

procedure TModifiedFileActionForm.FormCreate(Sender: TObject);
begin
end;

procedure TModifiedFileActionForm.ModifiedFileActionFormClick(Sender: TObject);
begin
  FAction := dmaReload;
  ModalResult := mrOK;
end;

procedure TModifiedFileActionForm.Button5Click(Sender: TObject);
begin
  FAction := dmaSave;
  ModalResult := mrOK;
end;

procedure TModifiedFileActionForm.Button3Click(Sender: TObject);
begin
  FAction := dmaIgnore;
  ModalResult := mrOK;
end;

procedure TModifiedFileActionForm.Button4Click(Sender: TObject);
begin
  FAction := dmaNoCheck;
  ModalResult := mrOK;
end;

end.

