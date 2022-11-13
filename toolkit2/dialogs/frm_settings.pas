unit frm_settings;

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
  ValEdit, ComCtrls, Grids, Buttons,
  fsl_utilities,
  fhir_colour_utils,
  fui_lcl_utilities, fui_lcl_managers,
  ftk_context,
  dlg_txsrvr_props;

type
  { TServerManager }
  TServerManager = class(TListManager<TToolkitContextTerminologyServer>)
  public
    function canSort : boolean; override;
    function manageColWidths : boolean; override;
    function allowedOperations(item : TToolkitContextTerminologyServer) : TNodeOperationSet;override;
    function AskOnDelete(item : TToolkitContextTerminologyServer) : boolean; override;
    function loadList : boolean; override;

    function getCellText(item : TToolkitContextTerminologyServer; col : integer) : String; override;
    function getSummaryText(item : TToolkitContextTerminologyServer) : String; override;
    function compareItem(left, right : TToolkitContextTerminologyServer; col : integer) : integer; override;
    function getCanCopy(item : TToolkitContextTerminologyServer; mode : String) : boolean; override;
    function getCopyValue(item : TToolkitContextTerminologyServer; mode : String) : String; override;

    function addItem(mode : String) : TToolkitContextTerminologyServer; override;
    function editItem(item : TToolkitContextTerminologyServer; mode : String) : boolean; override;
    function deleteItem(item : TToolkitContextTerminologyServer) : boolean; override;
  end;

  { TToolkitSettingsForm }

  TToolkitSettingsForm = class(TForm)
    btnAddServer: TBitBtn;
    btnEditServer: TBitBtn;
    btnDeleteServer: TBitBtn;
    btnClearCache: TButton;
    btnEditorFont: TButton;
    btnLogFont: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    btnViewFont: TButton;
    Button6: TButton;
    chkSideBySide: TCheckBox;
    dlgExe: TOpenDialog;
    dlgFont: TFontDialog;
    edtCache: TEdit;
    edtTxLog: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    lblDiff: TLabel;
    lblEditorFont: TLabel;
    lblLogFont: TLabel;
    lblViewFont: TLabel;
    lvServers: TListView;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure btnClearCacheClick(Sender: TObject);
    procedure btnEditorFontClick(Sender: TObject);
    procedure btnLogFontClick(Sender: TObject);
    procedure btnViewFontClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupBox5Click(Sender: TObject);
    procedure lvServersResize(Sender: TObject);
    procedure vlServersClick(Sender: TObject);
  private
    FDiffTool : String;
    FManager : TServerManager;
    FTx : TToolkitContextTerminologyServers;
  public

    procedure loadServers(tx : TToolkitContextTerminologyServers);
    procedure saveServers(tx : TToolkitContextTerminologyServers);

    property DiffTool : String read FDiffTool write FDiffTool;
  end;

var
  ToolkitSettingsForm: TToolkitSettingsForm;

implementation

{$R *.lfm}

{ TServerManager }

function TServerManager.canSort : boolean;
begin
  result := true;
end;

function TServerManager.manageColWidths: boolean;
begin
  Result := false;
end;

function TServerManager.allowedOperations(item : TToolkitContextTerminologyServer) : TNodeOperationSet;
begin
  result := [opAdd, opDelete, opEdit];
end;

function TServerManager.AskOnDelete(item : TToolkitContextTerminologyServer) : boolean;
begin
  result := true;
end;

function TServerManager.loadList : boolean;
var
  t : TToolkitContextTerminologyServer;
begin
  for t in ToolkitSettingsForm.FTx.List do
    Data.add(t.copy);
end;

function TServerManager.getCellText(item : TToolkitContextTerminologyServer; col : integer) : String;
begin
  case col of
    0:result := item.name;
    1:result := item.address;
    2:if item.default then result := 'true' else result := '';
    3:result := '??';
  end;
end;

function TServerManager.getSummaryText(item : TToolkitContextTerminologyServer) : String;
begin
  result := item.name+': '+item.address;
end;

function TServerManager.compareItem(left, right : TToolkitContextTerminologyServer; col : integer) : integer;
begin
  case col of
    0:result := compareStr(left.name, right.name);
    1:result := compareStr(left.address, right.address);
    2:result := integer(left.default) - integer(right.default);
    3:result := 0;
  end;
end;

function TServerManager.getCanCopy(item : TToolkitContextTerminologyServer; mode : String) : boolean;
begin
  result := true;
end;

function TServerManager.getCopyValue(item : TToolkitContextTerminologyServer; mode : String) : String;
begin
  result := item.address;
end;

function TServerManager.addItem(mode : String) : TToolkitContextTerminologyServer;
var
  t : TToolkitContextTerminologyServer;
begin
  result := nil;
  TxServerPropertiesDialog := TTxServerPropertiesDialog.create(ToolkitSettingsForm);
  try
    if (TxServerPropertiesDialog.showModal = mrOK) then
    begin
      result := TToolkitContextTerminologyServer.create;
      result.name := TxServerPropertiesDialog.edtName.text;
      result.address := TxServerPropertiesDialog.edtAddress.text;
      result.default := TxServerPropertiesDialog.chkDefault.checked;
      if result.default then
      begin
        for t in Data do
          t.Default := false;
        refresh;
      end;
    end;
  finally
    FreeAndNil(TxServerPropertiesDialog);
  end;
end;

function TServerManager.editItem(item : TToolkitContextTerminologyServer; mode : String) : boolean;
var
  t : TToolkitContextTerminologyServer;
begin
  TxServerPropertiesDialog := TTxServerPropertiesDialog.create(ToolkitSettingsForm);
  try
    TxServerPropertiesDialog.edtName.text := item.name;
    TxServerPropertiesDialog.edtAddress.text := item.address;
    TxServerPropertiesDialog.chkDefault.checked := item.default;
    result := TxServerPropertiesDialog.showModal = mrOK;
    if (result) then
    begin
      item.name := TxServerPropertiesDialog.edtName.text;
      item.address := TxServerPropertiesDialog.edtAddress.text;
      item.default := TxServerPropertiesDialog.chkDefault.checked;
      if item.default then
      begin
        for t in Data do
          if (t <> item) then
            t.Default := false;
        refresh;
      end;
    end;
  finally
    FreeAndNil(TxServerPropertiesDialog);
  end;
end;

function TServerManager.deleteItem(item: TToolkitContextTerminologyServer): boolean;
begin
  Result := true;
end;

{ TToolkitSettingsForm }

function describeFont(font : TFont) : String;
begin
  if font.name = '' then
    result := '(default)'
  else
    result := font.name;
  if font.size <> 0 then
    result := result+', '+inttostr(font.size)+'pt';
  if fsBold in font.Style then
    result := result+', bold';
  if fsItalic in font.Style then
      result := result+', italic';
  if fsUnderline in font.Style then
    result := result+', underline';
  if font.Color <> clDefault then
    result := result+'; color = '+ColourToString(font.color);
end;

procedure TToolkitSettingsForm.FormShow(Sender: TObject);
begin
  lblEditorFont.caption := describeFont(lblEditorFont.Font);
  lblLogFont.caption := describeFont(lblLogFont.Font);
  lblViewFont.caption := describeFont(lblViewFont.Font);
  lblDiff.caption := ExtractFileName(FDiffTool);
  lvServersResize(self);
end;

procedure TToolkitSettingsForm.GroupBox5Click(Sender: TObject);
begin

end;

procedure TToolkitSettingsForm.lvServersResize(Sender: TObject);
begin
  lvServers.columns[1].Width := lvServers.ClientWidth - (lvServers.columns[0].Width + lvServers.columns[2].Width + lvServers.columns[3].Width);
end;

procedure TToolkitSettingsForm.vlServersClick(Sender: TObject);
begin

end;

procedure TToolkitSettingsForm.loadServers(tx: TToolkitContextTerminologyServers);
begin
  FTx := tx;
  FManager.DoLoad;
end;

procedure TToolkitSettingsForm.saveServers(tx: TToolkitContextTerminologyServers);
var
  t, i : TToolkitContextTerminologyServer;
begin
  for t in FManager.Data do
  begin
    i := tx.getById(t.id);
    if (i = nil) then
      tx.List.add(t.link)
    else
      i.updateFrom(t);
  end;
end;

procedure TToolkitSettingsForm.btnEditorFontClick(Sender: TObject);
begin
  dlgFont.Font.assign(lblEditorFont.Font);
  if (dlgFont.Execute) then
  begin
    lblEditorFont.Font.assign(dlgFont.Font);
    lblEditorFont.caption := describeFont(lblEditorFont.Font);
  end;
end;

procedure TToolkitSettingsForm.btnClearCacheClick(Sender: TObject);
begin
  DeleteFile(edtCache.text);
end;

procedure TToolkitSettingsForm.btnLogFontClick(Sender: TObject);
begin
  dlgFont.Font.assign(lblLogFont.Font);
  if (dlgFont.Execute) then
  begin
    lblLogFont.Font.assign(dlgFont.Font);
    lblLogFont.caption := describeFont(lblLogFont.Font);
  end;
end;

procedure TToolkitSettingsForm.btnViewFontClick(Sender: TObject);
begin
  dlgFont.Font.assign(lblViewFont.Font);
  if (dlgFont.Execute) then
  begin
    lblViewFont.Font.assign(dlgFont.Font);
    lblViewFont.caption := describeFont(lblViewFont.Font);
  end;
end;

procedure TToolkitSettingsForm.Button6Click(Sender: TObject);
begin
  dlgExe.filename := FDiffTool;
  if dlgExe.Execute then
  begin
    FDiffTool := dlgExe.filename;
    lblDiff.caption := ExtractFileName(FDiffTool);
  end;
end;

procedure TToolkitSettingsForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
  FManager := TServerManager.create;
  FManager.List := lvServers;
  FManager.registerControl(btnAddServer, copAdd);
  FManager.registerControl(btnEditServer, copEdit);
  FManager.registerControl(btnDeleteServer, copDelete);
end;

procedure TToolkitSettingsForm.FormDestroy(Sender: TObject);
begin
  FManager.Free;
end;

end.

