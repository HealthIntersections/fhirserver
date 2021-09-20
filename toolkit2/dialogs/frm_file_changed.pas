unit frm_file_changed;

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

