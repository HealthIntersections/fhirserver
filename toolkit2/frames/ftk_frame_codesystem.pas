unit ftk_frame_codesystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, EditBtn, Buttons, Types,
  fui_lcl_managers,
  ftk_frame_resource;

type
  TFrame = TResourceEditorFrame;

  { TCodeSystemFrame }

  TCodeSystemFrame = class (TFrame)
    btnAddIdentifier: TButton;
    btnDeleteIdentifier: TButton;
    btnConcepts: TButton;
    chkExperimental: TCheckBox;
    chkCase: TCheckBox;
    chkGrammar: TCheckBox;
    chkNeedsVersion: TCheckBox;
    cbxStatus: TComboBox;
    cbxJurisdiction: TComboBox;
    cbxHeirarchy: TComboBox;
    cbxContent: TComboBox;
    edtDate: TDateEdit;
    edtURL: TEdit;
    edtName: TEdit;
    edtConceptCount: TEdit;
    edtVersion: TEdit;
    editTitle: TEdit;
    edtPublisher: TEdit;
    edtDescription: TEdit;
    edtPurpose: TEdit;
    edtCopyright: TEdit;
    edtValueSet: TEdit;
    edtSupplements: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lvIdentifiers: TListView;
    PageControl1: TPageControl;
    btnName: TSpeedButton;
    btnPublisher: TSpeedButton;
    btnDescription: TSpeedButton;
    btnPurpose: TSpeedButton;
    btnCopyright: TSpeedButton;
    TabSheet1: TTabSheet;
    procedure GroupBox1Click(Sender: TObject);
    procedure GroupBox3Click(Sender: TObject);
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    FManager : TObjectManager;
  public
    procedure initialize; override;

  end;

implementation

{$R *.lfm}

{ TCodeSystemFrame }

procedure TCodeSystemFrame.TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TCodeSystemFrame.initialize;
begin
  FManager := TObjectManager.create;
  //FManager.registerControl('url', edtURL);
  //FManager.registerControl('name', edtName, btnName);
  //FManager.registerControl('version', edtVersion);
  //FManager.registerControl('title', editTitle);
  //
  //FManager.registerControl('status', cbxStatus);
  //FManager.registerControl('experimental', chkExperimental);
  //FManager.registerControl('jurisdiction', cbxJurisdiction);
  //FManager.registerControl('date', edtDate);
  //FManager.registerControl('publisher', edtPublisher, btnPublisher);
  //FManager.registerControl('description', edtDescription, btnDescription);
  //FManager.registerControl('purpose', edtPurpose, btnPurpose);
  //FManager.registerControl('copyright', edtCopyright, btnCopyright);
  //
  //FManager.registerControl('valueSet', edtValueSet);
  //FManager.registerControl('count', edtConceptCount);
  //FManager.registerControl('supplements', edtSupplements);
  //FManager.registerControl('heirarchy', cbxHeirarchy);
  //FManager.registerControl('content', cbxContent);
  //
  //FManager.registerControl('caseSensitive', chkCase);
  //FManager.registerControl('grammar', chkGrammar);
  //FManager.registerControl('needsVersion', chkNeedsVersion);
end;

procedure TCodeSystemFrame.GroupBox1Click(Sender: TObject);
begin

end;

procedure TCodeSystemFrame.GroupBox3Click(Sender: TObject);
begin

end;

end.

