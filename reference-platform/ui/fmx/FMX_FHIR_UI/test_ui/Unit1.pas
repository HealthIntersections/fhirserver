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
    procedure FHIRStringEdit1Change(Sender: TObject);
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
  pat:TFHIRPatient;
  ident:TFHIRIdentifier;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
resourcetoFile(pat,'c:\temp\sss.xml',ffXML,OutputStylePretty);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 pat:=TFHIRPatient(FiletoResource('c:\temp\sss.xml'));

end;

procedure TForm1.FHIRStringEdit1Change(Sender: TObject);
var i:integer;
tempIdent:TFHIRIdentifier;
begin
pat.identifierList.Clear;
//tempIdent:=;
for i := 0 to FHIRStringEdit1.FHIRStringList.Count-1 do
begin
//ident.value:=FHIRStringEdit1.FHIRStringList[0];
pat.identifierList.AddItem(TFHIRIdentifier.Create('',FHIRStringEdit1.FHIRStringList[0]));

end;


end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
ident.Destroy;
pat.Destroy;
end;

end.
