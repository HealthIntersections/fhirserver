unit UITesterForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Generics.Collections, Math,
  FHIR.Ui.Graph, FHIR.Ui.GraphDesigner;

type
  TRandomData = class (TFGraphDataProvider)
  private
    FData : TList<TFGraphDataPoint>;
    FXmin : Double;
    FXmax : Double;
    FYmin : Double;
    FYmax : Double;

    function pointFactory(i : integer):TFGraphDataPoint;
  public
    constructor Create; override;
    destructor Destroy; override;

    function name : String; override;
    function HasDuplicateXValues : boolean; override;
    function count : integer; override;
    function getPoint( i : integer) : TFGraphDataPoint; override;
    procedure prepare; override;
    function getMinXValue : Double; override;
    function getMaxXValue : Double; override;
    function getMinYValue : Double; override;
    function getMaxYValue : Double; override;
  end;

  TForm10 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FGraph : TFGraph;
    procedure runDesigner(sender : TObject);
  public
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

procedure TForm10.Button1Click(Sender: TObject);
begin
  FGraph.Annotations.Add(FGraph.createMark(0.4, 0.4, 0.6, 0.6, clRed, 'Test', mtLine, mpUpRight, dtAfter));
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  FGraph.Series.Add(FGraph.createSeries(TRandomData.create));
  FGraph.Series[0].RegressionType := rg_dwls;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  FGraph := TFGraph.create(self);
  FGraph.Parent := Panel1;
  FGraph.align := alClient;
  FGraph.OnWantDesigner := runDesigner;
  FGraph.Appearance.ShowMarks := true;
  FGraph.Appearance.TitleFont.Size := 12;
  FGraph.XAxis.AutoSizing := true;
  FGraph.YAxis1.AutoSizing := true;
end;

procedure TForm10.runDesigner(sender: TObject);
begin
  designGraph(self, FGraph);
end;

{ TRandomData }

function TRandomData.count: integer;
begin
  result := FData.count;
end;

constructor TRandomData.Create;
var
  i : integer;
  p : TFGraphDataPoint;
begin
  inherited;
  FData := TList<TFGraphDataPoint>.create;
  for i := 0 to 1000 do
    FData.Add(pointFactory(i));
  FXmin := 0;
  FYMin := 0;
  FXMax := 1000;
  FYMax := 0;
  for p in FData do
  begin
    FYMin := min(FYMin, p.y);
    FYMax := max(FYMax, p.y);
  end;
end;

destructor TRandomData.Destroy;
begin
  FData.Free;
  inherited;
end;

function TRandomData.getMaxXValue: Double;
begin
  result := FXmax;
end;

function TRandomData.getMaxYValue: Double;
begin
  result := FYmax;
end;

function TRandomData.getMinXValue: Double;
begin
  result := FXmin;
end;

function TRandomData.getMinYValue: Double;
begin
  result := FYmin;
end;

function TRandomData.getPoint(i: integer): TFGraphDataPoint;
begin
  result := FData[i];
end;

function TRandomData.HasDuplicateXValues: boolean;
begin
  result := false;
end;

function TRandomData.name: String;
begin
  result := 'Test Data';
end;

function TRandomData.pointFactory(i: integer): TFGraphDataPoint;
begin
  result.id := i;
  result.x := i;
  result.y := i + random * 100;
end;

procedure TRandomData.prepare;
begin
  // nothing
end;

end.
