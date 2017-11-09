unit ResourceLanguageDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.ComboEdit,
  StringSupport,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities,
  ToolkitUtilities;

type
  TResourceLanguageForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    lblCurrentLanguage: TLabel;
    Panel3: TPanel;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    ComboEdit1: TComboEdit;
    CheckBox2: TCheckBox;
    lblCurrentStatus: TLabel;
    procedure FormShow(Sender: TObject);
  private
    FResource : TFHIRResource;
    procedure SetResource(const Value: TFHIRResource);
  public
    Destructor Destroy; override;
    Property Resource : TFHIRResource read FResource write SetResource;
  end;

var
  ResourceLanguageForm: TResourceLanguageForm;

implementation

{$R *.fmx}

{ TResourceLanguageForm }

destructor TResourceLanguageForm.Destroy;
begin
  FResource.Free;
  inherited;
end;

procedure TResourceLanguageForm.FormShow(Sender: TObject);
var
  translatable, translated : integer;
  st : TStringList;
  s : String;
  function elements(i : integer): string;
  begin
    if i = 1 then
      result := 'element'
    else
      result := 'elements';
  end;
begin
  st := langList;
  try
    if resource.language = '' then
    begin
      lblCurrentLanguage.Text := '(no specified language)';
      for s in st do
        ComboEdit1.Items.Add(langDesc(s));
    end
    else
    begin
      lblCurrentLanguage.Text := langDesc(resource.language);
      ComboEdit1.Items.Add('');
      for s in st do
        if s <> Resource.language then
          ComboEdit1.Items.Add(langDesc(s));
    end;
  finally
    st.Free;
  end;
  translatable := 0;
  translated := 0;
  iterateResource(resource, procedure (node : TFHIRObject)
    begin
      if (node is TFhirString) or (node is TFhirMarkdown) then
      begin
        inc(translatable);
        if (node as TFhirElement).HasExtension('http://hl7.org/fhir/StructureDefinition/translation') then
          inc(translated);
      end;
    end);
  lblCurrentStatus.Text := 'The resource contains '+inttostr(translatable)+' '+elements(translatable)+' that are translatable. '+inttostr(translated)+' '+elements(translated)+' have a translation';
end;

procedure TResourceLanguageForm.SetResource(const Value: TFHIRResource);
begin
  FResource.Free;
  FResource := value;
end;

end.
