unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FHIR.FMX.Ctrls, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FHIR.R4.Resources, FHIR.R4.Types, FHIR.R4.Utilities, FHIR.Base.Objects;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    FHIRStringEdit1: TFHIRStringEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  res:TFHIRExampleScenario;
  ident:TFHIRIdentifier;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
resourcetoFile(res,'c:\temp\sss.xml',ffXML,OutputStylePretty);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 res:=TFHIRExampleScenario(FiletoResource('c:\temp\sss.xml'));

res.nameElement:=FHIRStringEdit1.associate(res.nameElement);

end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
//pat.Destroy;
end;

end.
