unit UITesterFormFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Objects,
  FMX.Controls.Presentation,
  Generics.Collections, Math,
  FHIR.Ui.Graph, GraphTester;

type
  TForm10 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    FGraph : TFGraph; // TFGraph;
    function GetFuncValue(sender : TObject; x : Double) : Double;
  public
  end;

var
  Form10: TForm10;

implementation

{$R *.fmx}

procedure TForm10.Button1Click(Sender: TObject);
begin
  TFGraphTester.addMarks(FGraph);
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  TFGraphTester.addSeries(FGraph);
end;

procedure TForm10.Button3Click(Sender: TObject);
begin
  (FGraph.Series.last.Data as TRandomData).addMore;
end;

procedure TForm10.Button4Click(Sender: TObject);
begin
  FGraph.Functions.Add(FGraph.CreateFunction(GetFuncValue));
end;

procedure TForm10.Button5Click(Sender: TObject);
begin
  TFGraphTester.addBand(FGraph);
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  FGraph := TFGraph.create(self);
  AddObject(FGraph);
  TFGraphTester.configure(FGraph);
  FGraph.Repaint;
end;


function TForm10.GetFuncValue(sender: TObject; x: Double): Double;
begin
  result := 4000 / abs(500 - x);
end;


end.
