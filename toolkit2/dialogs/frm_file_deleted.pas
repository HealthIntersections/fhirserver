unit frm_file_deleted;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fui_lcl_utilities;

type
  TDeletedFileAction = (dfaSave, dfaSaveAs, dfaDiscard, dfaIgnore, dfaNoCheck);

  { TDeletedFileActionForm }

  TDeletedFileActionForm = class(TForm)
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    lblDetails: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAction : TDeletedFileAction;
  public

  end;

var
  DeletedFileActionForm: TDeletedFileActionForm;

function checkDeletedFileAction(owner : TComponent; filename : String) : TDeletedFileAction;

implementation

{$R *.lfm}

function checkDeletedFileAction(owner : TComponent; filename : String) : TDeletedFileAction;
begin
  DeletedFileActionForm := TDeletedFileActionForm.create(owner);
  try
    DeletedFileActionForm.lblDetails.Caption := 'The file '+filename+' has been deleted from it''s source location. What do you want to do?';
    DeletedFileActionForm.FAction := dfaIgnore;
    DeletedFileActionForm.ShowModal;
    result := DeletedFileActionForm.FAction;
  finally
    DeletedFileActionForm.free;
  end;
end;

{ TDeletedFileActionForm }

procedure TDeletedFileActionForm.Button6Click(Sender: TObject);
begin
  FAction := dfaSave;
  ModalResult := mrOK;
end;

procedure TDeletedFileActionForm.Button7Click(Sender: TObject);
begin
  FAction := dfaNoCheck;
  ModalResult := mrOK;
end;

procedure TDeletedFileActionForm.FormCreate(Sender: TObject);
begin
end;

procedure TDeletedFileActionForm.Button5Click(Sender: TObject);
begin
  FAction := dfaSaveAs;
  ModalResult := mrOK;
end;

procedure TDeletedFileActionForm.Button4Click(Sender: TObject);
begin
  FAction := dfaDiscard;
  ModalResult := mrOK;
end;

procedure TDeletedFileActionForm.Button3Click(Sender: TObject);
begin
  FAction := dfaIgnore;
  ModalResult := mrOK;
end;

end.

