unit frm_file_deleted;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

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

