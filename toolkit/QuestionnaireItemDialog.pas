unit QuestionnaireItemDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.ComboEdit, FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.TabControl,
  FMX.ListBox, FMX.Edit, FMX.DialogService,
  StringSupport, DateSupport,
  ToolkitSettings,
  FHIRTypes, FHIRResources, FHIRClient,
  ResourceEditingSupport, BaseFrame;

type
  TQuestionnaireItemForm = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbRequired: TCheckBox;
    cbRepeats: TCheckBox;
    cbReadOnly: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    edtLinkId: TEdit;
    edtDefinition: TEdit;
    edtPrefix: TEdit;
    edtText: TEdit;
    cbxType: TComboBox;
    edtMaxLength: TEdit;
    tbInitial: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    edtDefaultDate: TTabItem;
    TabItem6: TTabItem;
    TabItem7: TTabItem;
    TabItem8: TTabItem;
    TabItem9: TTabItem;
    TabItem10: TTabItem;
    TabItem11: TTabItem;
    TabItem12: TTabItem;
    TabItem13: TTabItem;
    TabItem14: TTabItem;
    tabOptions: TTabControl;
    tabValueSet: TTabItem;
    tabOptionList: TTabItem;
    Label8: TLabel;
    cbDefaultBoolean: TCheckBox;
    edtDefaultDecimal: TEdit;
    edtDefaultInteger: TEdit;
    edtDefaultString: TEdit;
    edtDefaultUrl: TEdit;
    edtDefaultReference: TEdit;
    edtDefaultQuantity: TEdit;
    dedDefaultDate: TDateEdit;
    dedDefaultDateTime: TDateEdit;
    tdtDefaultDateTime: TTimeEdit;
    tdtDefaultTime: TTimeEdit;
    memDefaultText: TMemo;
    cbxDefaultChoice: TComboBox;
    cedDefaultOpenChoice: TComboEdit;
    edtDefaultQuantityUnits: TEdit;
    Label9: TLabel;
    Panel2: TPanel;
    btnOptionAdd: TButton;
    btnOptionUp: TButton;
    btnOptionDown: TButton;
    btnOptionDelete: TButton;
    grdOptions: TGrid;
    cedValueSet: TComboEdit;
    Button1: TButton;
    btnAsChild: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure inputChange(Sender: TObject);
    procedure grdOptionsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure grdOptionsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnOptionAddClick(Sender: TObject);
    procedure btnOptionUpClick(Sender: TObject);
    procedure btnOptionDownClick(Sender: TObject);
    procedure btnOptionDeleteClick(Sender: TObject);
    procedure btnLoadValuesets(Sender: TObject);
    procedure cedValueSetChange(Sender: TObject);
    procedure btnAsChildClick(Sender: TObject);
  private
    FItem: TFhirQuestionnaireItem;
    Loading : boolean;
    FSettings: TFHIRToolkitSettings;
    FOnWork: TWorkEvent;
    procedure SetItem(const Value: TFhirQuestionnaireItem);
    procedure loadInitialValue;
    procedure saveInitialValue;
    procedure loadOptions;
    procedure commit;
    procedure SetSettings(const Value: TFHIRToolkitSettings);
  public
    destructor Destroy; override;
    property item : TFhirQuestionnaireItem read FItem write SetItem;

    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
    property OnWork : TWorkEvent read FOnWork write FOnWork;
  end;

var
  QuestionnaireItemForm: TQuestionnaireItemForm;

implementation

{$R *.fmx}

uses
  FHIRToolkitForm;

{ TQuestionnaireItemForm }

procedure TQuestionnaireItemForm.btnOkClick(Sender: TObject);
begin
  if (edtMaxLength.Text <> '') and not StringIsInteger16(edtMaxLength.Text) then
  begin
    showmessage('Max Length must be an integer');
    ActiveControl := edtMaxLength;
  end
  else
  begin
    commit;
    ModalResult := mrOk;
  end;
end;

procedure TQuestionnaireItemForm.btnOptionAddClick(Sender: TObject);
begin
  grdOptions.BeginUpdate;
  grdOptions.RowCount := grdOptions.RowCount + 1;
  item.optionList.Append;
  grdOptions.EndUpdate;
end;

procedure TQuestionnaireItemForm.btnOptionDeleteClick(Sender: TObject);
var
  i : integer;
begin
  i := grdOptions.Row;
  if (i >= 0) then
  begin
    grdOptions.BeginUpdate;
    item.optionList.DeleteByIndex(i);
    grdOptions.RowCount := grdOptions.RowCount - 1;
    grdOptions.EndUpdate;
    if i > 0 then
      grdOptions.Row := i - 1;
  end;
end;

procedure TQuestionnaireItemForm.btnOptionDownClick(Sender: TObject);
var
  i : integer;
begin
  i := grdOptions.Row;
  if (i < item.optionList.Count - 1) then
  begin
    grdOptions.BeginUpdate;
    item.optionList.Exchange(i, i+1);
    grdOptions.EndUpdate;
    grdOptions.Row := i + 1;
  end;
end;

procedure TQuestionnaireItemForm.btnOptionUpClick(Sender: TObject);
var
  i : integer;
begin
  i := grdOptions.Row;
  if (i > 0) then
  begin
    grdOptions.BeginUpdate;
    item.optionList.Exchange(i, i-1);
    grdOptions.EndUpdate;
    grdOptions.Row := i - 1;
  end;
end;

procedure TQuestionnaireItemForm.btnAsChildClick(Sender: TObject);
begin
  if (edtMaxLength.Text <> '') and not StringIsInteger16(edtMaxLength.Text) then
  begin
    showmessage('Max Length must be an integer');
    ActiveControl := edtMaxLength;
  end
  else
  begin
    commit;
    ModalResult := mrYes;
  end;
end;

procedure TQuestionnaireItemForm.btnLoadValuesets(Sender: TObject);
var
  client : TFHIRClient;
  params : TDictionary<String, String>;
  be : TFhirBundleEntry;
  bundle : TFhirBundle;
  vs : TFHIRValueSet;
  s : String;
begin
  Loading := true;
  try
    bundle := TFhirBundle.create;
    params := TDictionary<String, String>.create;
    try
      client := TFhirThreadedClient.Create(TFhirHTTPClient.Create(nil, FSettings.serverAddress('Terminology', 0), false, FSettings.timeout * 1000, FSettings.proxy), MasterToolsForm.threadMonitorProc);
      try
        params.Add('_summary', 'true');
        OnWork(self, 'Fetching ValueSets', true,
          procedure
          begin
            bundle := client.search(frtValueSet, true, params);
          end);
        try
          cedValueSet.Items.Clear;
          cedValueSet.Items.BeginUpdate;
          if item.options <> nil then
          begin
            if item.options.display <> '' then
              cedValueSet.Items.Add(item.options.reference + ': '+item.options.display)
            else
              cedValueSet.Items.Add(item.options.reference);
          end;
          for be in bundle.entryList do
          begin
            if (be.resource <> nil) and (be.resource is TFhirValueSet) then
            begin
              vs := be.resource as TFhirValueSet;
              s := vs.url;
              cedValueSet.Items.Add(vs.url+': '+vs.name);
            end;
          end;
          cedValueSet.Items.EndUpdate;
        finally
          bundle.Free;
        end;
      finally
        client.Free;
      end;
    finally
      params.free;
    end;
  finally
    Loading := false;
  end;
end;

procedure TQuestionnaireItemForm.cbxTypeChange(Sender: TObject);
var
  nType : TFhirItemTypeEnum;
  ok : boolean;
begin
  if Loading then
    exit;

  nType := TFhirItemTypeEnum(cbxType.ItemIndex+1);
  if (edtMaxLength.Text <> '') and not (nType in [ItemTypeDecimal, ItemTypeInteger, ItemTypeString, ItemTypeText, ItemTypeUrl, ItemTypeOpenChoice]) then
  begin
    TDialogService.MessageDialog('This will clear MaxLength. Continue?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult)
    begin
      ok := AResult = mrYes;
    end);
    if not ok then
    begin
      Loading := true;
      try
        cbxType.ItemIndex := ord(item.type_)-1;
      finally
        Loading := false;
      end;
      exit;
    end;
  end;
  if item.initial <> nil then
  begin
    TDialogService.MessageDialog('This will clear the default value. Continue?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult)
    begin
      ok := AResult = mrYes;
    end);
    if not ok then
    begin
      Loading := true;
      try
        cbxType.ItemIndex := ord(item.type_)-1;
      finally
        Loading := false;
      end;
      exit;
    end;
  end;
  if (item.optionList.Count > 0) and not ((nType in [ItemTypeOpenChoice, ItemTypeChoice]) and (item.type_ in [ItemTypeOpenChoice, ItemTypeChoice]) ) then
  begin
    TDialogService.MessageDialog('This will clear the options list. Continue?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult)
    begin
      ok := AResult = mrYes;
    end);
    if not ok then
    begin
      Loading := true;
      try
        cbxType.ItemIndex := ord(item.type_)-1;
      finally
        Loading := false;
      end;
      exit;
    end
    else
      item.optionList.Clear;
  end;
  item.type_ := TFhirItemTypeEnum(cbxType.ItemIndex+1);
  edtMaxLength.Enabled := item.type_ in [ItemTypeDecimal, ItemTypeInteger, ItemTypeString, ItemTypeText, ItemTypeUrl, ItemTypeOpenChoice];
  if not edtMaxLength.Enabled then
    item.maxLength := '';
  edtMaxLength.Text := item.maxLength;
  loadInitialValue;
  loadOptions;
end;

procedure TQuestionnaireItemForm.cedValueSetChange(Sender: TObject);
var
  l, r : String;
begin
  if Loading then
    exit;
  if cedValueSet.Text = '' then
    item.options := nil
  else
  begin
    if item.options = nil then
      item.options := TFhirReference.Create;
    if cedValueSet.text.IndexOf(':') > 0 then
    begin
      StringSplit(cedValueSet.text, ':', l, r);
      item.options.reference := l.Trim;
      item.options.display := r.Trim;
    end
    else
    begin
      item.options.reference := cedValueSet.Text;
      item.options.display := '';
    end;
  end;
end;

procedure TQuestionnaireItemForm.commit;
begin
  item.required := cbRequired.IsChecked;
  item.repeats := cbRepeats.IsChecked;
  item.readOnly := cbReadOnly.IsChecked;
  item.linkId := edtLinkId.Text;
  item.definition := edtDefinition.Text;
  item.prefix := edtPrefix.Text;
  item.text := edtText.Text;
  item.maxLength := edtMaxLength.Text;
  saveInitialValue;
end;

destructor TQuestionnaireItemForm.Destroy;
begin
  FSettings.Free;
  FItem.Free;
  inherited;
end;

procedure TQuestionnaireItemForm.FormShow(Sender: TObject);
begin
  Loading := true;
  try
    cbRequired.IsChecked := item.required;
    cbRepeats.IsChecked := item.repeats;
    cbReadOnly.IsChecked := item.readOnly;
    edtLinkId.Text := item.linkId;
    edtDefinition.Text := item.definition;
    edtPrefix.Text := item.prefix;
    edtText.Text := item.text;
    cbxType.ItemIndex := ord(item.type_)-1;
    edtMaxLength.Text := item.maxLength;
    edtMaxLength.Enabled := item.type_ in [ItemTypeDecimal, ItemTypeInteger, ItemTypeString, ItemTypeText, ItemTypeUrl, ItemTypeOpenChoice];
    loadInitialValue;
    loadOptions;
  finally
    Loading := false;
  end;
end;

procedure TQuestionnaireItemForm.grdOptionsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  v : TFhirQuestionnaireItemOption;
begin
  v := item.optionList[aRow];
  if v.value = nil then
    Value := ''
  else case item.type_ of
    ItemTypeInteger: value := (v.value as TFHIRInteger).value;
    ItemTypeDate: value := (v.value as TFHIRDate).value.toString('c');
    ItemTypeTime: value := (v.value as TFHIRTime).value;
    ItemTypeString: value := (v.value as TFHIRString).value;
    ItemTypeChoice, ItemTypeOpenChoice: case ACol of
      0: value := (v.value as TFhirCoding).system;
      1: value := (v.value as TFhirCoding).code;
      2: value := (v.value as TFhirCoding).display;
    end;
  end;
end;

procedure TQuestionnaireItemForm.grdOptionsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  v : TFhirQuestionnaireItemOption;
begin
  v := item.optionList[aRow];
  case item.type_ of
    ItemTypeInteger: v.value := TFHIRInteger.Create(value.AsString);
    ItemTypeDate: v.value := TFHIRDate.Create(TDateTimeEx.fromFormat('c', value.AsString));
    ItemTypeTime: v.value := TFHIRTime.Create(value.AsString);
    ItemTypeString: v.value := TFHIRString.Create(value.AsString);
    ItemTypeChoice, ItemTypeOpenChoice: case ACol of
      0: (v.value as TFhirCoding).system := value.AsString;
      1: (v.value as TFhirCoding).code := value.AsString;
      2: (v.value as TFhirCoding).display := value.AsString;
    end;
  end;
end;

procedure TQuestionnaireItemForm.inputChange(Sender: TObject);
begin
  commit;
end;

procedure TQuestionnaireItemForm.loadInitialValue;
begin
  case item.type_  of
    ItemTypeGroup, ItemTypeDisplay :
      begin
      if item.initial <> nil then
        item.initial := nil;
      tbInitial.TabIndex := 0;
      end;
    ItemTypeBoolean :
      begin
      tbInitial.TabIndex := 1;
      if item.initial is TFhirBoolean then
        cbDefaultBoolean.IsChecked := TFhirBoolean(item.initial).value
      else
      begin
        cbDefaultBoolean.IsChecked := false;
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeDecimal :
      begin
      tbInitial.TabIndex := 2;
      if item.initial is TFhirDecimal then
        edtDefaultDecimal.Text := TFhirDecimal(item.initial).value
      else
      begin
        edtDefaultDecimal.Text := '';
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeInteger :
      begin
      tbInitial.TabIndex := 3;
      if item.initial is TFhirInteger then
        edtDefaultInteger.Text := TFhirInteger(item.initial).value
      else
      begin
        edtDefaultInteger.Text := '';
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeDate :
      begin
      tbInitial.TabIndex := 4;
      if item.initial is TFhirDate then
        dedDefaultDate.Date := TFhirDate(item.initial).value.DateTime
      else
      begin
        dedDefaultDate.Text := '';
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeDateTime :
      begin
      tbInitial.TabIndex := 5;
      if item.initial is TFhirDateTime then
      begin
        presentDateTime(item.initial as TFhirDateTime, dedDefaultDateTime, tdtDefaultDateTime);
      end
      else
      begin
        dedDefaultDateTime.Text := '';
        tdtDefaultDateTime.Text := '';
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeTime :
      begin
      tbInitial.TabIndex := 6;
      if item.initial is TFhirTime then
        tdtDefaultTime.Text := TFhirTime(item.initial).value
      else
      begin
        tdtDefaultTime.Text := '';
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeString :
      begin
      tbInitial.TabIndex := 7;
      if item.initial is TFhirString then
        edtDefaultString.Text := TFhirString(item.initial).value
      else
      begin
        edtDefaultString.Text := '';
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeText :
      begin
      tbInitial.TabIndex := 8;
      if item.initial is TFhirString then
        memDefaultText.Text := TFhirString(item.initial).value
      else
      begin
        memDefaultText.Text := '';
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeUrl :
      begin
      tbInitial.TabIndex := 9;
      if item.initial is TFhirUri then
        edtDefaultUrl.Text := TFhirUri(item.initial).value
      else
      begin
        edtDefaultUrl.Text := '';
        if item.initial <> nil then
          item.initial := nil
      end;
      end;
    ItemTypeChoice :
      begin
      tbInitial.TabIndex := 10;
      end;
    ItemTypeOpenChoice :
      begin
      tbInitial.TabIndex := 11;
      end;
    ItemTypeAttachment :
      begin
      tbInitial.TabIndex := 12;
      end;
    ItemTypeReference :
      begin
      tbInitial.TabIndex := 13;
      end;
    ItemTypeQuantity :
      begin
      tbInitial.TabIndex := 14;
      end;
  end;
end;

procedure TQuestionnaireItemForm.loadOptions;
var
  i : integer;
  col : TColumn;
begin
  for i := grdOptions.ColumnCount - 1 downto 0 do
  begin
    col := grdOptions.Columns[i];
    grdOptions.RemoveObject(col);
  end;

  if item.type_ in [ItemTypeChoice, ItemTypeOpenChoice] then
  begin
    if (item.options <> nil) then
    begin
      tabValueSet.Enabled := true;
      tabOptionList.Enabled := false;
      tabOptions.ActiveTab := tabValueSet;
      cedValueSet.Enabled := true;
      if item.options.display <> '' then
        cedValueSet.Text := item.options.reference + ' : '+item.options.display
      else
        cedValueSet.Text := item.options.reference;
      grdOptions.RowCount := 0;
    end
    else if item.optionList.Count > 0 then
    begin
      tabValueSet.Enabled := false;
      tabOptionList.Enabled := true;
      cedValueSet.Enabled := false;
      tabOptions.ActiveTab := tabOptionList;
      cedValueSet.Text := '';
      grdOptions.RowCount := item.optionList.Count;
      col := TStringColumn.Create(self);
      col.Header := 'System';
      grdOptions.AddObject(col);
      col := TStringColumn.Create(self);
      col.Header := 'Code';
      grdOptions.AddObject(col);
      col := TStringColumn.Create(self);
      col.Header := 'Display';
      grdOptions.AddObject(col);
    end
    else
    begin
      tabValueSet.Enabled := true;
      tabOptionList.Enabled := true;
      tabOptions.ActiveTab := tabValueSet;
      cedValueSet.Enabled := true;
      cedValueSet.Text := '';
      grdOptions.RowCount := 0;
      col := TStringColumn.Create(self);
      col.Header := 'System';
      grdOptions.AddObject(col);
      col := TStringColumn.Create(self);
      col.Header := 'Code';
      grdOptions.AddObject(col);
      col := TStringColumn.Create(self);
      col.Header := 'Display';
      grdOptions.AddObject(col);
    end;
  end
  else if item.type_ in [ItemTypeInteger, ItemTypeDate, ItemTypeTime, ItemTypeString] then
  begin
    tabValueSet.Enabled := false;
    tabOptionList.Enabled := true;
    tabOptions.ActiveTab := tabOptionList;
    cedValueSet.Enabled := false;
    cedValueSet.Text := '';
    grdOptions.RowCount := item.optionList.Count;
    col := TStringColumn.Create(self);
    col.Header := 'Value';
    grdOptions.AddObject(col);
  end
  else
  begin
    tabValueSet.Enabled := false;
    tabOptionList.Enabled := false;
    tabOptions.ActiveTab := tabValueSet;
    cedValueSet.Enabled := false;
  end;
end;

procedure TQuestionnaireItemForm.saveInitialValue;
begin
  case item.type_  of
    ItemTypeGroup, ItemTypeDisplay :
      item.initial := nil;
    ItemTypeBoolean : if cbDefaultBoolean.IsChecked    then item.initial := TFhirBoolean.Create(true) else item.initial := nil;
    ItemTypeDecimal : if edtDefaultDecimal.Text <> ''  then item.initial := TFhirDecimal.Create(edtDefaultDecimal.Text) else item.initial := nil;
    ItemTypeInteger : if edtDefaultInteger.Text <> ''  then item.initial := TFhirInteger.Create(edtDefaultDecimal.Text) else item.initial := nil;
    ItemTypeDate :    if dedDefaultDate.Text <> ''     then item.initial := TFhirDate.Create(TDateTimeEx.make(dedDefaultDate.Date, dttzLocal)) else item.initial := nil;
    ItemTypeDateTime :if dedDefaultDateTime.Text <> '' then item.initial := storeDateTime(dedDefaultDateTime, tdtDefaultDateTime) else item.initial := nil;
    ItemTypeTime :    if tdtDefaultTime.Text <> ''     then item.initial := TFhirTime.Create(tdtDefaultTime.Text) else item.initial := nil;
    ItemTypeString :  if edtDefaultString.Text <> ''   then item.initial := TFhirString.Create(edtDefaultString.Text) else item.initial := nil;
    ItemTypeText :    if memDefaultText.Text <> ''     then item.initial := TFhirString.Create(memDefaultText.Text) else item.initial := nil;
    ItemTypeUrl :     if edtDefaultUrl.Text <> ''      then item.initial := TFhirUri.Create(edtDefaultUrl.Text) else item.initial := nil;
    ItemTypeChoice : ;
    ItemTypeOpenChoice : ;
    ItemTypeAttachment : ;
    ItemTypeReference : ;
    ItemTypeQuantity : ;
  end;
end;

procedure TQuestionnaireItemForm.SetItem(const Value: TFhirQuestionnaireItem);
begin
  FItem.Free;
  FItem := Value;
end;

procedure TQuestionnaireItemForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

end.
