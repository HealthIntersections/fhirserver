unit GraphTester;
{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

interface

uses
  System.UITypes, {$IFDEF FMX} FMX.Graphics, FMX.Types, {$ELSE}Graphics, Controls, {$ENDIF}
  Generics.Collections, Math,
  FHIR.Ui.Graph;

type
  TRandomData = class (TFGraphDataProvider)
  private
    FData : TList<TFGraphDataPoint>;
    FXmin : Double;
    FXmax : Double;
    FYmin : Double;
    FYmax : Double;

    FName : String;

    function pointFactory(i : integer):TFGraphDataPoint;
    procedure checkLimits;
  public
    constructor Create(name : String);
    destructor Destroy; override;

    procedure addMore;

    function name : String; override;
    function HasDuplicateXValues : boolean; override;
    function count : integer; override;
    function dataCount : integer; override;
    function getPoint( i : integer) : TFGraphDataPoint; override;
    procedure prepare; override;
    function getMinXValue : Double; override;
    function getMaxXValue : Double; override;
    function getMinYValue : Double; override;
    function getMaxYValue : Double; override;
  end;


  TFGraphTester = {static} class
  public
    class procedure configure(graph : TFGraph);
    class procedure addMarks(graph : TFGraph);
    class procedure addSeries(graph : TFGraph);
    class procedure addBand(graph : TFGraph);
  end;

implementation


{ TRandomData }

procedure TRandomData.addMore;
var
  i, b : integer;
begin
  b := FData.Count;
  for i := 0 to 10 do
    FData.Add(pointFactory(i+b));
  checkLimits;
  change;
end;

procedure TRandomData.checkLimits;
var
  p : TFGraphDataPoint;
begin
  FXmin := 0;
  FXMax := FData.Count + 1;
  FYMin := MaxInt;
  FYMax := -MaxInt;
  for p in FData do
  begin
    if p.error = '' then
    begin
      FYMin := min(FYMin, p.y);
      FYMax := max(FYMax, p.y);
    end;
  end;
end;

function TRandomData.count: integer;
begin
  result := FData.count;
end;

constructor TRandomData.Create(name : String);
var
  i : integer;
begin
  inherited Create;
  FName := name;
  FData := TList<TFGraphDataPoint>.create;
  for i := 0 to 10 do
    FData.Add(pointFactory(i));
  checkLimits;
end;

function TRandomData.datacount: integer;
begin
  result := count - 1;
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
  result := FName;
end;

function TRandomData.pointFactory(i: integer): TFGraphDataPoint;
begin
  result.id := i;
  result.x := i;
  if i = 5 then
    result.error := 'Communication Error'
  else
    result.y := i + random * 4;
end;

procedure TRandomData.prepare;
begin
  // nothing
end;

{ TFGraphTester }

class procedure TFGraphTester.addMarks(graph: TFGraph);
begin
  graph.Annotations.Add(graph.createMark(0.5, 0.5, 0.9, 0.9, clRed, 'Test 1', mtLine, mpUpRight, dtAfter));
  graph.Annotations.Add(graph.createMark(0.5, 0.5, 0.9, 0.1, clGreen, 'Test 2', mtLine, mpUpRight, dtAfter));
  graph.Annotations.Add(graph.createMark(0.5, 0.5, 0.1, 0.9, clBlue, 'Test 3', mtLine, mpUpRight, dtAfter));
  graph.Annotations.Add(graph.createMark(0.5, 0.5, 0.1, 0.1, clGray, 'Test 4', mtLine, mpUpRight, dtAfter));
end;

class procedure TFGraphTester.addSeries(graph: TFGraph);
var
  n : string;
begin
  n := 'Test Data';
  if graph.Series.Count > 0 then
    n := 'Second test Data';

  graph.Series.Add(graph.createSeries(TRandomData.create(n)));
  graph.Series.last.RegressionType := rg_passingBablok;
  graph.Series.last.LegendStatus := lsAll;
  graph.Series.last.DrawPoints := true;
  graph.Series.last.PointShape := ps_Square;
end;

class procedure TFGraphTester.configure(graph: TFGraph);
begin
  graph.Name := 'TestChart';
  {$IFDEF FMX}
  graph.align := TAlignLayout.Client;
  graph.Legend.borderColor := TAlphaColors.Maroon;
  {$ELSE}
  graph.align := alClient;
  graph.Legend.borderColor := clMaroon;
  {$ENDIF}
  graph.Appearance.ShowMarks := true;
  graph.Appearance.TitleFont.Size := 12;
  graph.XAxis.AutoSizing := true;
  graph.YAxis1.AutoSizing := true;
  graph.YAxis2.ShowAxis := true;
  graph.Dimensions.RightMargin := 80;
  graph.Dimensions.TopMargin := 80;
  graph.Dimensions.LeftMargin := 80;
  graph.Dimensions.BottomMargin := 80;
  graph.Legend.visible := true;
  graph.Legend.borderStyle := psSolid;
  graph.Legend.Layout := lsDown;
  graph.Legend.top := 20;
  graph.Legend.left := 20;
  graph.Legend.top := 20;
  graph.Legend.height := 0;
  graph.Legend.width := 0;
end;

class procedure TFGraphTester.addBand(graph : TFGraph);
var
  band : TFGraphBand;
begin
  band := TFGraphBand.Create;
  try
    band.YAxis2 := false;
    band.color := clGreen;
    band.Min := 0.5;
    band.Max := 0.75;
    band.opacity := 0.1;

    graph.Bands.Add(band.link);
  finally
    band.Free;
  end;
end;

end.
