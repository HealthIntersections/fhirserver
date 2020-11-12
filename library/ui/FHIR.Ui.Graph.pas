unit FHIR.Ui.Graph;

{
Copyright (c) 1996+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

// todo:
// https://stackoverflow.com/questions/21562662/how-can-i-manually-scale-a-components-font-with-a-change-in-dpi-in-delphi

interface

uses
  SysUtils, Classes, Generics.Collections, Math,
  {$IFDEF FMX}
  System.Types, System.UITypes, FMX.Graphics, FMX.StdCtrls, System.Math.Vectors, FMX.Types, FMX.Objects,
  {$ELSE}
  WinApi.Windows, Messages, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.Forms, Vcl.ExtCtrls, Vcl.Clipbrd,
  {$ENDIF}
  fsl_base;

const
  tiny = 1.0e-20;  { used to avoid divide by zero errors }
  MaxLogTickCount = 9; {Must equal largest value in AllowedLogTickCounts}
  LogE = 0.4342944818;
 { time constants : Alert users will note that here and elsewhere where date/time
   values are used, small errors creep in due to assumptions implicit in the
   way TDateTime works. These have been tolerated in the interests of speed.
   Month and year are more variable : the only place where the length of these
   is approximated is for the guestimation of the number of ticks; I will
   be interested to hear if anybody has problems with this assumption }
  cMinute1 = 1/(24 * 60);
  cMinute5 = 5/(24 * 60);
  cMinute30 = 30/(24 * 60);
  cHour = (1/24);
  defMinYScale = 0.0;
  defMinXScale = 0.0;
  MouseToleranceMove = 4; { pixels mouse must move before it counts }
  MouseToleranceHit = 5; { how far from target mouse is allowed to be }
  NO_VALUE = -2.145e45;

  {$IFDEF FMX}
const
  clBlack = TAlphaColors.Black;
  clSilver = TAlphaColors.Silver;
  clRed = TAlphaColors.Red;
  clgreen = TAlphaColors.Green;
  clblue = TAlphaColors.Blue;
  clWhite = TAlphaColors.White;
  clGray = TAlphaColors.Gray;
  clMaroon = TAlphaColors.Maroon;
  clInfoBk = TAlphaColors.Lightyellow;
  mbLeft = TMouseButton.mbLeft;

  psDot = TStrokeDash.Dot;
  psDash = TStrokeDash.Dash;
  PsDashDot = TStrokeDash.DashDot;
  PsDashDotDot = TStrokeDash.DashDotDot;
  psSolid = TStrokeDash.Solid;

  bsClear = TBrushKind.None;
  bsSolid = TBrushKind.Solid;

type
  TCustomPanel = TRectangle;
  TPen = TStrokeBrush;
  TPenStyle = TStrokeDash;
  TBrushStyle = TBrushKind;
  TPenMode = (pmXOr);
  {$ELSE}
Type
  TAlphaColor = TColor;
  {$ENDIF}

type
  TFontAdapted = class (TFont)
  private
    {$IFDEF FMX}
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    property OnChange : TNotifyEvent read GetOnChange write SetOnChange;
    {$ENDIF}
  end;
  TLinePoints = array[0..1] of TPoint;
  pLinePoints = ^TLinePoints;

  TFGraph = class;
  TFGraphSeries = class;

  TFGraphChangeType = (dsUnLoad, dsNewData, dsDataChange, dsZeroChange, dsClearUpdates, dsChangeLook, dsRegressionChange);

  TFGraphComponent = class abstract (TFslObject)
  protected
    FGraph : TFGraph;
    procedure change;
  end;

  TFGraphDataPoint = record
    id : integer;
    x, y : Double;
    x2, y2 : Double; // when the mode = pairs
    error : String;
    xe, ye : Double;

    procedure clear;
  end;

  TDataProcessingRoutine = reference to procedure (pp : TFGraphDataPoint);

  // all these methods are hit constantly and are performance critical
  TFGraphDataProvider = class abstract (TFslObject)
  private
    FSeries : TFGraphSeries;
    procedure iterate(processor : TDataProcessingRoutine);
  protected
    procedure change; virtual;
  public
    function name : String; virtual; abstract;

    function HasDuplicateXValues : boolean; virtual; abstract;

    // count: the total number of points provided
    function count : integer; virtual; abstract;
    // data  count: the number of point that are not errors
    function dataCount : integer; virtual; abstract;

    function getPoint(i : integer) : TFGraphDataPoint; virtual; abstract;  // must be ordered by x
    procedure prepare; virtual; abstract;
    function getMinXValue : Double; virtual; abstract;
    function getMaxXValue : Double; virtual; abstract;
    function getMinYValue : Double; virtual; abstract;
    function getMaxYValue : Double; virtual; abstract;
  end;

  TFGraphLegendStatus = (lsNone, lsNameOnly, lsPoints, lsLines, lsAll);
  TFGraphPointShape = (ps_Square, ps_Circle, ps_Diamond, ps_Cross, ps_Wide, ps_Bar);
  TFGraphBoundType = (bt_None, bt_AsSet, bt_1SD, bt_2SD, bt_3SD, bt_allSD);
  TGraphStyleRegressionType = (rg_None,
                     {1. forms of regression: }
                     rg_Linear, rg_passingBablok, rg_quadratic,
                     {2. Curve Fitting: }
                     rg_Spline, rg_dwls,
                     {3. Other: }
                     rg_RunningAverage, rg_runningAverageProper);

  TFGraphDataStatistics = record
    {admin:}
    current : boolean;
    comment : string;
    error : boolean;
    {basic statistics}
    count : integer;
    mean, median, mode, Total, SD, Skew, Kurtosis, lowquintile, highquintile, lowquartile, highquartile : double;
  end;

  TFGraphAnalysisStatistics = record
    {admin:}
    current:boolean;
    comment:string;
    error:boolean;
    {correlation}
    Rsquared, PearsonR,  {pearson correlation}
    PRMin, PRMax, RSqMin, RSqMax, {conf intervals of r}
    PValue,
    {regressions}
     { y = FRegSlope2 * x^2 + FRegSlope * x + FRegressionIntercept }
     {1. Available in all forms of regression: }
    RegSlope, RegressionIntercept,
     {2. available during quadratic regression: }
    RegSlope2,
     {3. available for Passing/Bablok regression }
    SlopeSD, IntSD: Double;
  end;

  TFGraphRegressionPoint = class (TFslObject)
  private
    x, y : Double;
  end;

  TFGraphSeries = class (TFGraphComponent)
  private
    FData : TFGraphDataProvider;
    FRegenStats : boolean;
    FXStats : TFGraphDataStatistics;
    FYStats : TFGraphDataStatistics;
    FAnalysis : TFGraphAnalysisStatistics;
    FRegressionData : TFslList<TFGraphRegressionPoint>;

    FActive : boolean;
    FYAxis2 : boolean;
    FDrawerrors : boolean;
    FDrawLine : boolean;
    FDrawPoints : boolean;
    FHighlighted : Boolean;
    FZeroOffset : DOuble;
    FAutoZero : Boolean;
    FLineColor : TAlphaColor;
    FLineStyle : TPenStyle;
    FPointColor : TAlphaColor;
    FFillPoints : boolean;
    FLegendStatus : TFGraphLegendStatus;
    FPointSize : Word;
    FPointShape : TFGraphPointShape;
    FBarOffset : Double;
    FBarWidth : Double;
    FBoundsColor : TAlphaColor;
    FBoundsLineStyle : TPenStyle;
    FBoundsType : TFGraphBoundType;
    FUpperBound : Double;
    FLowerBound : Double;
    FErrorslineColor : TAlphaColor;
    FErrorsLineStyle : TPenStyle;
    FRegrLineStyle: TPenStyle;
    FRegressionType: TGraphStyleRegressionType;
    FRegrColor: TAlphaColor;
    FTolerance: Double;
    FRegControl1: LongInt;
    FRegControl2: Double;

    procedure SetActive(v : Boolean);
    procedure SetYAxis2(v : Boolean);
    procedure SetLegendStatus(v : TFGraphLegendStatus);
    procedure SetDrawErrors(v : Boolean);
    procedure setDrawLine(v : Boolean);
    procedure SetDrawPoints(v : Boolean);
    procedure SetHighlighted(v : boolean);
    procedure SetZeroOffset(v: Double);
    procedure SetAutoZero(v: Boolean);
    procedure SetLineColor(v : TAlphaColor);
    procedure SetLinestyle(v : TPenStyle);
    procedure SetPointColor(v : TAlphaColor);
    procedure setFillPoints(v : Boolean);
    procedure SetPointShape(v : TFGraphPointShape);
    procedure setPointSize(v : Word);
    procedure SetBarOffset(v : Double);
    procedure SetBarWidth(v : Double);
    procedure SetBoundsColor(v : TAlphaColor);
    procedure SetBoundsLineStyle(v : TPenstyle);
    procedure setBoundsType(v : TFGraphBoundType);
    Procedure SetUpperBound(v : double);
    procedure SetLowerBound(v : Double);
    procedure setErrorsLinestyle(v : TPenStyle);
    procedure SetErrorsLineColor(v : TAlphaColor);
    procedure SetRegrColor(const v: TAlphaColor);
    procedure SetRegressionType(const v: TGraphStyleRegressionType);
    procedure SetRegrLineStyle(const v: TPenStyle);
    procedure SetRegControl1(v: LongInt);
    procedure setRegcontrol2(v: Double);

    function  GetXStats: TFGraphDataStatistics;
    function  GetYStats: TFGraphDataStatistics;
    function  GetCompStats: TFGraphAnalysisStatistics;

    procedure DoRegression;
    procedure addRegressionValue(x, y : Double);
    procedure populateRegression(m2, m, c: Double);
    procedure checkRegenStats;
    procedure DoLinearRegression;
    procedure DoQuadraticRegression;
    procedure DoPassingBablok;
    procedure DoRunningAverage;
    procedure DoRunningAverageProper;
    procedure DoSplineRegression;
    function  GetDWLS(x, xDiff: Double): Double;
    procedure doDWLS;
    procedure DoCalcStats(points : TList<Double>; var stats: TFGraphDataStatistics);
    procedure CalcStats(var stats: TFGraphDataStatistics; isxaxis:boolean; restrict : boolean; min, max: double);
    procedure correlate;

    procedure RequestPaint(reason : TFGraphChangeType); virtual;
  public
    constructor Create(provider : TFGraphDataProvider);
    destructor Destroy; override;

    function link : TFGraphSeries; overload;

    property Data : TFGraphDataProvider read FData;

    property Active : Boolean read FActive write setActive;
    property YAxis2 : boolean read FYAxis2 write SetYAxis2;

    property XStats : TFGraphDataStatistics read GetXStats;
    property YStats : TFGraphDataStatistics read GetYStats;
    procedure GetRestrictedStats(XMin, XMax: double; var IXStats, IYStats: TFGraphDataStatistics);

    property LegendStatus : TFGraphLegendStatus read FLegendStatus write SetLegendStatus;
    property DrawErrors : Boolean read FDrawErrors write SetDrawErrors;
    property DrawLine : Boolean read FDrawLine write setDrawLine;
    property DrawPoints : Boolean read FDrawPoints write SetDrawPoints;
    property HighLighted : boolean read FHighlighted write SetHighlighted;
    property ZeroOffset : Double read FZeroOffset write setZeroOffset;
    property AutoZero: Boolean read FAutoZero write SetAutoZero;
    property LineColor : TAlphaColor read FLineColor write SetLineColor;
    property LineStyle : TPenStyle read FLineStyle write setLinestyle;
    property PointColor : TAlphaColor read FPointColor write SetPointColor;
    property FillPoints : Boolean read FFillPoints write setFillPoints;
    property PointShape : TFGraphPointShape read FPointShape write SetPointShape;
    property PointSize : Word read FPointSize write setPointsize;
    property BarOffset : Double read FBarOffset write SetBarOffset;
    property BarWidth : Double read FBarWidth write SetBarWidth;
    property BoundsColor : TAlphaColor read FBoundsColor write SetBoundsColor;
    property BoundsLineStyle : TPenStyle read FBoundsLineStyle write SetBoundsLineStyle;
    property BoundsType : TFGraphBoundType read FBoundstype write SetBoundstype;
    property UpperBound : double read FUpperBound write SetUpperBound;
    property LowerBound : double read FLowerBound write SetLowerBound;
    property ErrorsLineColor : TAlphaColor read FErrorsLineColor write SetErrorsLineColor;
    property ErrorsLineStyle : TPenStyle read FErrorsLineStyle write setErrorsLinestyle;

    property RegressionType : TGraphStyleRegressionType read FRegressionType write SetRegressionType;
    property RegressionLineColor : TAlphaColor read FRegrColor write SetRegrColor;
    property RegressionLineStyle : TPenStyle read FRegrLineStyle write SetRegrLineStyle;
    property Tolerance: Double read FTolerance write FTolerance;
    property RegControl1: LongInt read FRegControl1 write SetRegControl1;
    property RegControl2: Double read FRegControl2 write setRegcontrol2;
  end;

// documentation for using and deriving descendents from TFGraphAnnotation
{
  TFGraphAnnotation establishes the adminstration of a list of
  objects that will be drawn to the screen during the painting
  of the graph. descendents of TFGraphAnnotation must defined
  in this unit where they have full access to the internal
  datastructures of the graph and can do whatever they want except
  modify data

  the Annotations will be drawn at one of 4 times :
    dtBefore         immediately before the axes are drawn - after prepaint
    dtBeforeSeries   after the axes and before the series
    dtBeforeLegend   after all series are drawn but before the legend
    dtAfter          after everything but the OnAfterPaint event

  Annotations are added using the AddAnnotation and deleted using the
  deleteAnnotation routines. Annotations are created independently of
  the graph, but are then assigned to a graph. Annotations can only
  be assigned to one graph at a time. When a graph is freed any annotations
  still attached to it are destroyed.  If an annotation is freed while it
  is attached to a graph it will automatically be deleted from that graph

  While an annotation is attached to a graph it can be accessed using
  the annotations[i] property. the value i is the id given in the
  .create(id, caption, time) routine. the id can't be changed while the
  annotation is attached to a graph. Annotation ID's must be unique within
  the graph; and exception will be raised if an attempt is made to add a
  duplicate ID

  see also the addmark routine for further information

  The Z-order is maintained by the ID. lower ID's are drawn first

  A descendent of TFGraphAnnotation need only concern itself with 3 things :
    1. managing the information required for it's own functionality
    2. Painting itself to the screen.
        To achieve this the descendent must override the PaintGraph routine.
        the graph is available through the FGraph variable
    3. Saving itself to a stream and reading itself back again.
        To achieve this the descendent must override 3 methods :
          TypeLabel - returns the string that identifies this type of annotation

        The format established using TxyReader and TxyWriter must be used. When
        the information is written the WriteToStream must use endsection - this
        allows a graph to continue reading the stream when an unknown annotation
        is encountered. ReadFromStream must read up to and including the endsection
        (see WriteAllMarks, readallMarks for examples)

        In addition the name returned in TypeLabel must be coded into the
        CreateSpecifiedAnnotation routine below so that the annotation can
        be read back into the graph

        If there is no information to be stored, the WriteToStream and
        ReadFromStream must still be declared, and just do the endsection thing.

  Note that for backwards compatibility TMark is a special case and doesn't
  go into the Annotations section.

}
  TFGraphDrawTime = (dtBefore, dtBeforeSeries, dtBeforeLegend, dtAfter);

  TFGraphAnnotation = class abstract (TFGraphComponent)
  private
    FCaption : String;
    FVisible : boolean;
    FDrawTime : TFGraphDrawTime;

    procedure SetCaption(v : string);
    procedure SetVisible(v : boolean);
    procedure SetDrawTime(v : TFGraphDrawTime);
  protected
    // the following procedures must be overridden by any descendents
    function  TypeLabel : string; virtual; abstract;
  public
    constructor Create(const caption : string; DrawTime  : TFGraphDrawTime); virtual;

    property Caption : string read FCaption write SetCaption;
    property Visible : boolean read FVisible write SetVisible;
    property DrawTime : TFGraphDrawTime read FDrawTime write SetDrawTime;
  end;

  TFGraphMarkType = (mtXMark,mtYMark,mtPoint,mtLine);
  TFGraphMarkPos = (mpUpLeft, mpDownLeft, mpUpRight, mpDownRight);

  TFGraphMark = class (TFGraphAnnotation)
  private
    FColor : TAlphaColor;
    FX1, FY1, FX2, FY2 : double;
    FMarkType : TFGraphMarkType;
    FMarkPos : TFGraphMarkPos;
    procedure SeTAlphaColor(v : TAlphaColor);
    procedure Setx1(v : double);
    procedure Sety1(v : double);
    procedure Setx2(v : double);
    procedure Sety2(v : double);
    procedure SetMarkType(v : TFGraphMarkType);
    procedure SetMarkPos(v : TFGraphMarkPos);
  protected
    function  TypeLabel : string; override;
  public
    property  Color : TAlphaColor read FColor write SeTAlphaColor;
    property  x1 : double read Fx1 write Setx1;
    property  y1 : double read Fy1 write Sety1;
    property  x2 : double read Fx2 write Setx2;
    property  y2 : double read Fy2 write Sety2;
    property  MarkType : TFGraphMarkType read FMarkType write SetMarkType;
    property  MarkPos : TFGraphMarkPos read FMarkPos write SetMarkPos;
  end;

  TFGraphFunctionEvent = function (sender : TObject; v : Double) : Double of object;

  TFGraphBand = class (TFGraphComponent)
  private
    FOpacity: Double;
    FMax: Double;
    FMin: Double;
    FColor: TAlphaColor;
    FYAxis2: boolean;
    procedure SeTAlphaColor(const Value: TAlphaColor);
    procedure SetMax(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetOpacity(const Value: Double);
    procedure SetYAxis2(const Value: boolean);
  public
    function link : TFGraphBand; overload;

    property color : TAlphaColor read FColor write SeTAlphaColor;
    property YAxis2 : boolean read FYAxis2 write SetYAxis2;
    property Max : Double read FMax write SetMax;
    property Min : Double read FMin write SetMin;
    property opacity : Double read FOpacity write SetOpacity;
  end;

  //  A convenient way to graph a function; automatically fills graph; handles
  //  singularities in the function.
  TFGraphFunction = class (TFGraphComponent)
  private
    FOnGetValue: TFGraphFunctionEvent;
    FColor: TAlphaColor;
    FXMax: Double;
    FBounded: boolean;
    FStyle: TPenStyle;
    FXMin: Double;
    procedure SetBounded(const Value: boolean);
    procedure SeTAlphaColor(const Value: TAlphaColor);
    procedure SetStyle(const Value: TPenStyle);
    procedure SetXMax(const Value: Double);
    procedure SetXMin(const Value: Double);
  public
    property OnGetValue : TFGraphFunctionEvent read FOnGetValue write FOnGetValue;
    property color : TAlphaColor read FColor write SeTAlphaColor;
    property style : TPenStyle read FStyle write SetStyle;
    property bounded : boolean read FBounded write SetBounded;
    property xMin : Double read FXMin write SetXMin;
    property xMax : Double read FXMax write SetXMax;
  end;

  TFGraphAxisOffsetType = (ao_Minimum, ao_Maximum, ao_percentage, ao_absolute);

  TFGraphDateTickType = (dt_minute, dt_5minute, dt_halfhourly, dt_hourly,
         dt_daily, dt_weekly, dt_monthly, dt_2monthly, dt_quarterly,
         dt_annually, dt_decade, dt_century, dt_custom);

  TFGraphLogTickInfo = record
    LogTickArray: array[0..MaxLogTickCount+1] of Double;
    LogTickCount: Word;
    LogTickIndex: Word;
    LogStepping: Boolean; {signals linear steps}
    DrawMinorLabels: Boolean;
  end;

  TFGraphAxis = class (TFGraphComponent)
  private
    FSecondAxis, FScaledOK : boolean;
    FM : Double;
    FOMin, FOMax : Double;
    FMin, FMax, FStep : Double;
    FAutoSizing : Boolean;
    FAutoStepping : Boolean;
    FLogging : Boolean;
    FLogTickInfo : TFGraphLogTickInfo;
    FAxisTitle : String;
    FLabelDec : Integer;
    FMinScale, FOffset : Double;
    FShowAsTime, FShowAxis, FReversed, FGridlines, FAutoLabelDecimals : Boolean;
    FDateFormat : string;
    FDateTickType : TFGraphDateTickType;
    FOffsetType : TFGraphAxisOffsetType;
    procedure SetLabelDec(v : Integer);
    function  GetLogTickCount : Word;
    procedure SetLogTickCount(v : Word);
    procedure SetLogging(v : Boolean);
    procedure SetAutoSizing(v : Boolean);
    procedure SetAutoStepping(v : Boolean);
    procedure SetMax(v : Double);
    procedure SetMin(v : Double);
    procedure SetStep(v : Double);
    procedure SetMinDiff(v : Double);
    procedure SetAxisTitle(v : String);
    procedure setShowAsTime(v : Boolean);
    procedure setDateFormat(v : string);
    procedure setDateTickType(v : TFGraphDateTickType);
    procedure setshowAxis(v : boolean);
    procedure setreversed(v : boolean);
    procedure SetOffsetType(v : TFGraphAxisOffsetType);
    procedure setoffset(v : double);
    procedure SetGridLines(v : Boolean);
  protected
    procedure InitLogTicks;
    procedure AdjustLogTickCount;
    procedure CheckDrawMinorLabels;
    procedure SetMinMax;
    procedure SetDateMinMax;
    procedure SetLogMinMax;
    function  DoResize : Boolean;
    function  GetFirstTick(var logTickInfo : TFGraphLogTickInfo) : Double;
    function  GetFirstDateTick : double;
    function  GetNextTick(tick : Double; var logTickInfo : TFGraphLogTickInfo;
                          var drawThisLabel : Boolean) : Double;
    function  GetNextDateTick(tick : Double) : double;
    procedure AdjustLabelDecs;
    procedure AdjustLogLabelDecs(v : Double);
    procedure CalcMetrics;
    function  LabelString(tick : Double) : String;
  public
    function  CheckScale : Boolean;
  published
    property Title : string read FAxisTitle write SetAxisTitle;
    property LabelDecimals : Integer read FLabelDec write SetLabelDec default 1;
    property AutoLabelDecimals : boolean read FAutoLabelDecimals write FAutoLabelDecimals default true;
    property LogCycleDivisions : Word read GetLogTickCount write SetLogTickCount default 2;
    property LogScale : Boolean read FLogging write SetLogging;
    property Max : Double read FMax write SetMax;
    property Min : Double read FMin write SetMin;
    property StepSize : Double read FStep write SetStep;
    property MinScaleLength : Double read FMinScale write SetMinDiff;
    property ShowAsTime : Boolean read FShowASTime write setShowAsTime default false;
    property DateTimeFormat : string read FDateFormat write SetDateFormat;
    property DateTickType : TFGraphDateTickType read FDateTickType write setDateTickType;
    property ShowAxis : boolean read FShowAxis write SetShowAxis;
    property Reversed : boolean read FReversed write setreversed;
    property OffsetType : TFGraphAxisOffsetType read FOffsettype write SetOffsetType;
    property Offset : double read FOffset write setoffset;
    property Gridlines : boolean read FGridlines write SetGridlines;

    property AutoSizing : Boolean read FAutoSizing write SetAutoSizing;
    property AutoStepping : Boolean read FAutoStepping write SetAutoStepping;
  end;

  TFGraphDimensions = class (TFGraphComponent)
  private
    FLeft, FRight, FTop, FBottom : Word;
    FTMLength : Word;
    FXAxisTitleDistance : Integer;
    FYAxisTitleDistance : Integer;
    FXAxisLabelDistance : Integer;
    FYAxisLabelDistance : Integer;
    FGraphTitleDistance : Integer;
    FScalePct : Word;
    FXOffsetPct, FYOffsetPct : Word;
    procedure SetMargBottom(v : Word);
    procedure SetMargTop(v : Word);
    procedure SetMargLeft(v : Word);
    procedure SetMargRight(v : Word);
    procedure SetTickLength(v : Word);
    procedure SetXAxisTitleDistance(v : Integer);
    procedure SetYAxisTitleDistance(v : Integer);
    procedure SetXAxisLabelDistance(v : Integer);
    procedure SetYAxisLabelDistance(v : Integer);
    procedure SetGraphTitleDistance(v : Integer);
    procedure SetScale(v : Word);
    procedure SetXOffset(v : Word);
    procedure SetYOffset(v : Word);
  published
    property BottomMargin : Word read FBottom write SetMargBottom default 40;
    property LeftMargin : Word read FLeft write SetMargLeft default 40;
    property RightMargin : Word read FRight write SetMargRight default 15;
    property TopMargin : Word read FTop write SetMargTop default 30;
    property TickLength : Word read FTMLength write SetTickLength default 4;
    property XAxisTitleOffset : Integer read FXAxisTitleDistance write SetXAxisTitleDistance default 4;
    property XAxisLabelOffset : Integer read FXAxisLabelDistance write SetXAxisLabelDistance default 2;
    property YAxisTitleOffset : Integer read FYAxisTitleDistance write SetYAxisTitleDistance default 4;
    property YAxisLabelOffset : Integer read FYAxisLabelDistance write SetYAxisLabelDistance default 2;
    property GraphTitleOffset : Integer read FGraphTitleDistance write SetGraphTitleDistance default 7;
   {set print offsets as integer percent (<=100) of page width, height}
    property PrintXOffsetPct : Word read FXOffsetPct write SetXOffset default 5;
    property PrintYOffsetPct : Word read FYOffsetPct write SetYOffset default 20;
   {set print scale as integer percent (<=100) of full page}
    property PrintScalePct : Word read FScalePct write SetScale default 90;
  end;

  TFGraphAppearance = class (TFGraphComponent)
  private
    FTickMarks, FShowMarks, FCrossAtZero,FPrintLineStyle : Boolean;
    FPlotOffGraph, FAllowDuplicates : Boolean;
    FLabelGraph : Boolean;
    FGridColor, FAxesColor, FBkgdColor, FMarginColor, FCrossColor : TAlphaColor;
    FGridStyle : TPenStyle;
    FBkgdWhenPrint : Boolean;
    FErrorCaption : string;
    FGraphTitle : string;
    FMinSteps,FMaxSteps, FCrossLength : Word;
    FTitleFont, FCaptionFont, FLabelFont : TFontAdapted;
    FMinPointClearance : double;
    FCrossWire : Boolean;
    FErrorFont: TFontAdapted;
    procedure SetAxesColor(v : TAlphaColor);
    procedure SetBkgdColor(v : TAlphaColor);
    procedure SetMarginColor(v : TAlphaColor);
    procedure SetErrorCaption(v : string);
    procedure SetGridColor(v : TAlphaColor);
    procedure SetGridStyle(v : TPenStyle);
    procedure SetLabelGraph(v : Boolean);
    procedure SetPlotOffGraph(v : Boolean);
    procedure SetShowMarks(v : Boolean);
    procedure SetTickMarks(v : Boolean);
    procedure SetGraphTitle(v : string);
    procedure SetMinSteps(v : Word);
    procedure SetMaxSteps(v : Word);
    procedure setTitleFont(v : TFontAdapted);
    procedure setLabelFont(v : TFontAdapted);
    procedure setCaptionFont(v : TFontAdapted);
    procedure setErrorFont(const Value: TFontAdapted);
    procedure setCrossAtZero(v : boolean);
    procedure setCrossColor(v : TAlphaColor);
    procedure setCrosslength(v : word);
    procedure SetMinPointClearance(v : double);
    procedure FontChanged(Sender : TObject);
  published
    property AxesColor : TAlphaColor read FAxesColor write SetAxescolor default clBlack;
    property BackgroundColor : TAlphaColor read FBkgdColor write SetBkgdColor;
    property MarginColor : TAlphaColor read FMarginColor write SetMarginColor;
    property PrintBkgndColor : Boolean read FBkgdWhenPrint write FBkgdWhenPrint;
    property ErrorCaption : string read FErrorCaption write SetErrorCaption;
    property GridColor : TAlphaColor read FGridColor write SetGridcolor default clSilver;
    property GridStyle : TPenStyle read FGridStyle write SetGridStyle default psDot;
    property ShowGraphLabels : Boolean read FLabelGraph write SetLabelGraph default True;
    property PlotOffGraph : Boolean read FPlotOffGraph write SetPlotOffGraph default False;
    property ShowMarks : Boolean read FShowMarks write SetShowMarks default false;
    property ShowTicks : Boolean read FTickMarks write SetTickmarks default True;
    property GraphTitle : string read FGraphTitle write SetGraphTitle;
    property MinSteps : Word read FMinSteps write SetMinSteps default 5;
    property MaxSteps : Word read FMaxSteps write SetMaxSteps default 50;
    property TitleFont : TFontAdapted read FTitleFont write setTitleFont;
    property CaptionFont : TFontAdapted read FCaptionFont write setCaptionFont;
    property ErrorFont : TFontAdapted read FErrorFont write setErrorFont;
    property LabelFont : TFontAdapted read FLabelFont write setLabelFont;
    property CrossAtZero : Boolean read FCrossatZero write setCrossAtZero default false;
    property CrossColor : TAlphaColor read FCrossColor write SetCrossColor default clRed;
    property Crosslength : word read FCrosslength write SetCrossLength default 4;
    property PrintLineStyle : boolean read FPrintLineStyle write FPrintLineStyle default true;
    property MinPointClearance : double read FMinPointClearance write setMinPointClearance;
    property CrossWire : Boolean read FCrossWire write FCrossWire default false;
  end;

  TFGraphLegendStyle = (lsAcross, lsDown);

  TFGraphLegend = class (TFGraphComponent)
  private
     Fvisible : Boolean;
     Fcolor : TAlphaColor;
     FborderStyle : TPenStyle;
     FborderColor : TAlphaColor;
     FLayout : TFGraphLegendStyle;
     Ftop, Fleft, Fwidth, Fheight, FSymbolSpace, FXMargin, FYmargin : integer;
     Ffont : TFontAdapted;
     procedure setvisible(v : Boolean);
     procedure seTAlphaColor(v : TAlphaColor);
     procedure setborderStyle(v : TPenStyle);
     procedure setborderColor(v : TAlphaColor);
     procedure setLayout(v : TFGraphLegendStyle);
     procedure settop(v : integer);
     procedure setleft(v : integer);
     procedure setwidth(v : integer);
     procedure setheight(v : integer);
     procedure setfont(v : TFontAdapted);
     procedure setSymbolSpace(v : integer);
     procedure setXMargin(v : integer);
     procedure setYMargin(v : integer);

     procedure fontchanged(sender : TObject);
  published
     property visible : Boolean read FVisible write SetVisible default false;
     property color : TAlphaColor read Fcolor write SeTAlphaColor default clWhite;
     property borderStyle : TPenStyle read FBorderstyle write SetBorderStyle;
     property borderColor : TAlphaColor read FBorderColor write SetBorderColor;
     property Layout : TFGraphLegendStyle read FLayout write SetLayout default lsAcross;
     property top : integer read FTop write settop default 10;
     property left : integer read FLeft write setleft default 10;
     property width : integer read FWidth write setWidth;
     property height : integer read Fheight write setheight default 20;
     property font : TFontAdapted read FFont write setFont;
     property SymbolSpace : integer read FSymbolSpace write setSymbolSpace default 20;
     property XMargin : integer read FXMargin write setXmargin default 10;
     property YMargin : integer read FYMargin write setYMargin default 5;
  end;

  TFGraphSeriesEvent =  procedure (sender:TObject; index:integer) of object;
  TFGraphPaintEvent = procedure (sender:TObject; Canvas:TCanvas) of object;

  TGraphCanvas = class (TFslObject)
  private
    FCanvas : TCanvas;
    {$IFDEF FMX}
    FState : TCanvasSaveState;
    FFonTAlphaColor : TAlphaColor;
    {$ENDIF}
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetPenColor: TAlphaColor;
    procedure SetPenColor(const Value: TAlphaColor);
    function GetPenStyle: TPenStyle;
    procedure SetPenStyle(const Value: TPenStyle);
    function GetPenMode: TPenMode;
    procedure SetPenMode(const Value: TPenMode);
    function GetPenWidth: integer;
    procedure SetPenWidth(const Value: integer);
    function GetBrushStyle: TBrushStyle;
    procedure SetBrushStyle(const Value: TBrushStyle);
    function GetBrushColor: TAlphaColor;
    procedure SetBrushColor(const Value: TAlphaColor);
    function GetFonTAlphaColor: TAlphaColor;
    procedure SetFonTAlphaColor(const Value: TAlphaColor);
  public
    constructor Create(canvas : TCanvas);
    destructor Destroy; override;

    procedure start;
    procedure finish;

    property PenColor : TAlphaColor read GetPenColor write SetPenColor;
    property PenStyle : TPenStyle read GetPenStyle write SetPenStyle;
    property PenMode : TPenMode read GetPenMode write SetPenMode;
    property PenWidth : integer read GetPenWidth write SetPenWidth;
    property BrushStyle : TBrushStyle read GetBrushStyle write SetBrushStyle;
    property BrushColor : TAlphaColor read GetBrushColor write SetBrushColor;
    property Font : TFont read GetFont write SetFont;
    property fonTAlphaColor : TAlphaColor read GetFonTAlphaColor write SetFonTAlphaColor;
    procedure Polyline(const Points: array of TPoint); overload;
    procedure Polyline(points : pLinePoints; count : integer); overload;
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    procedure Polygon(const Points: array of TPoint);
    procedure Line(x1,y1, x2,y2 : integer);
    procedure Rectangle(x1,y1, x2,y2 : integer; opacity : Double = 1.0);
    procedure Clip(x1,y1, x2,y2 : integer);
    procedure UnClip(x1,y1, x2,y2 : integer);
    function TextWidth(text : String) : integer;
    function TextHeight(text : String) : integer;
    procedure TextOut(x,y : integer; text : String); overload;
    procedure TextOut(xOrigin, yOrigin : integer; xDelta, yDelta : integer; text : String; angle : double); overload; // rotation in .. radians...?   rotate around the origin point
    function SelectObject(obj : THandle) : THandle;
  end;

  TFGraph = class (TCustomPanel)
  private
    FCanvas : TGraphCanvas;
    FSeries : TFslList<TFGraphSeries>;
    FAnnotations : TFslList<TFGraphAnnotation>;
    FBands : TFslList<TFGraphBand>;
    FFunctions : TFslList<TFGraphFunction>;

    FDimensions : TFGraphDimensions;
    FAppearance : TFGraphAppearance;
    FXAxis, FYAxis, FYAxis2 : TFGraphAxis;
    FLegend : TFGraphLegend;
    AllScalesOK, FNoGridlines : Boolean;

  {Graph property data : }
    FPlotting, FPainting : Boolean;
    FScale : Double;
    FXOffset, FYOffset, FCurrWidth, FCurrHeight : Word;

   {Mouse : }
    dRect : TRect;
    dx, dy : Integer;
    FDragging, FLegDragging, FHasDragged : Boolean;
    FHoldXAS, FHoldYAS, FHoldY2AS : boolean;
    FHoldXmin, FHoldXmax, FHoldYmin, FHoldYmax, FHoldY2min, FHoldY2max : double;
    cwx,cwy : Integer;
    CVisible : Boolean;

   {Events : }
    FOnRescale, FOnScaleError, FOnWantDesigner : TNotifyEvent;
    FOnPaintStart, FOnPaintEnd : TFGraphPaintEvent;

    FSaveData : boolean;

    // scaling:
    FSM, FSD : Integer;
    FColor: TAlphaColor;

    procedure SetPlotting(v : Boolean);
    procedure SetHasDragged(v : boolean);

    procedure setDefaultPropertyValues;

    procedure PaintSeries(series : TFGraphSeries);
    procedure PaintSeriesLine(series : TFGraphSeries);
    procedure PaintSeriesPoints(series : TFGraphSeries);
    procedure PaintSeriesBounds(series : TFGraphSeries);
    procedure PaintSeriesErrors(series : TFGraphSeries);
    procedure PaintSeriesRegression(series : TFGraphSeries);
    procedure PaintBand(band : TFGraphBand);
    procedure PaintAnnotation(annotation : TFGraphAnnotation);
    procedure PaintXMark(mark : TFGraphMark);
    procedure PaintYMark(mark : TFGraphMark);
    procedure PaintBoxMark(mark : TFGraphMark);
    procedure PaintLineMark(mark : TFGraphMark);
    procedure PaintFunction(f : TFGraphFunction);
  protected
    procedure DoTightFit;
    procedure ClipGraph;
    procedure UnclipGraph;
    procedure CalcMetrics;
    function  fx(v : Double) : Integer;
    function  fy(v : Double; a2 : boolean) : Integer;
    procedure DrawXGridlines;
    procedure DrawYGridlines(CYAxis : TFGraphAxis);
    procedure DrawXTickMarks(base : integer; reverse : boolean);
    procedure DrawYTickMarks(CYAxis : TFGraphAxis; base : integer; reverse : boolean);
    function  DrawXLabels(base : integer; reverse : boolean) : integer;
    function  DrawYLabels(CYAxis : TFGraphAxis; base : integer; reverse : boolean) : integer;
    procedure DrawGraphTitle;
    procedure DrawAnnotations(time : TFGraphDrawTime);
    procedure DrawXAxis;
    procedure DrawYAxis;
    procedure DrawY2Axis;
    procedure DrawAxes;
    procedure ColorBackground;
    procedure PaintCross;
    procedure DrawLegend;
    procedure PaintGraph(PaintAxes : boolean);
    procedure Paint; override;
    function  CheckScalesOK : Boolean;
    procedure DoRescaleEvent;
    function  DataFound(yaxis2 : boolean) : Boolean;
    procedure SeriesChange(Sender: TObject; const Item: TFGraphSeries; Action: TCollectionNotification);
    procedure BandChange(Sender: TObject; const Item: TFGraphBand; Action: TCollectionNotification);
    procedure AnnotationChange(Sender: TObject; const Item: TFGraphAnnotation; Action: TCollectionNotification);
    procedure FunctionsChange(Sender: TObject; const Item: TFGraphFunction; Action: TCollectionNotification);

    {$IFNDEF FMX}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
    function GetYAxis2(which : boolean) : TFGraphAxis;

    {$IFNDEF FMX}
    {Implements dragging and resizing : }
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X,Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X,Y : Integer); override;
    Procedure SetUpDragging(X,Y : Integer);
    {$ENDIF}
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    property Series : TFslList<TFGraphSeries> read FSeries;
    property Annotations : TFslList<TFGraphAnnotation> read FAnnotations;
    property Bands : TFslList<TFGraphBand> read FBands;
    property Functions : TFslList<TFGraphFunction> read FFunctions;

    procedure ClearAll; // clear series, bands, functions, and annotations

   { for mouse events }
    property HasDragged : boolean read FHasDragged write SetHasDragged;
    function  GetMouseX(x : Integer) : Double;
    function  GetMouseY(y : Integer) : Double;
    function  GetMouseY2(y : Integer) : Double;
    function findNearestPoint(x,y : integer; var series : TFGraphSeries; var point : TFGraphDataPoint) : boolean;

    function createMark(x1, y1, x2, y2: Double; c: TAlphaColor; name: string; marktype: TFGraphMarkType; markpos: TFGraphMarkPos; DrawTime: TFGraphDrawTime): TFGraphMark;
    function createSeries(data : TFGraphDataProvider) : TFGraphSeries;
    function CreateFunction(event : TFGraphFunctionEvent) : TFGraphFunction;

    procedure addBand(color :  TAlphaColor; min, max, opacity : Double);
    {$IFNDEF FMX}
    function settingsText(code : boolean) : String;
    {$ENDIF}

    property YAxis[which : boolean] : TFGraphAxis read GetYAxis2;
  published
    property XAxis : TFGraphAxis read FXAxis write FXAxis;
    property YAxis1 : TFGraphAxis read FYAxis write FYAxis;
    property YAxis2 : TFGraphAxis read FYAxis2 write FYAxis2;
    property Dimensions : TFGraphDimensions read FDimensions write FDimensions;
    property Appearance : TFGraphAppearance read FAppearance write FAppearance;
    property Legend : TFGraphLegend read FLegend write Flegend;

    property OnRescale : TNotifyEvent read FOnRescale write FOnRescale;
    property OnScaleError : TNotifyEvent read FOnScaleError write FOnScaleError;
    property OnPaintStart : TFGraphPaintEvent read FOnPaintStart write FOnPaintStart;
    property OnPaintEnd : TFGraphPaintEvent read FOnPaintEnd write FOnPaintEnd;
    property OnWantDesigner : TNotifyEvent read FOnWantDesigner write FOnWantDesigner;

    property Plotting : Boolean read FPlotting write SetPlotting;
    {$IFDEF FMX}
    property Color : TAlphaColor read FColor write FColor;
    {$ELSE}
    property Align;
    property BevelInner default bvNone;
    property BevelOuter default bvNone;
    property BevelWidth;
    property BorderStyle default bsSingle;
    property BorderWidth;
    property Ctl3D;
    property Cursor;
    property ParentCtl3D;
    property ParentFont;
    property PopupMenu;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$ENDIF}
  end;

implementation

procedure swap(var n1,n2 : integer); inline;
var t : integer;
begin
  t := n1;
  n1 := n2;
  n2 := t;
end;

function Pt(AX, AY: Integer): TPoint; inline;
begin
  Result.X := AX;
  Result.Y := AY;
end;

{ TFGraphComponent }

procedure TFGraphComponent.change;
begin
  if FGraph <> nil then
    FGraph.Repaint;
end;


{ TFGraphDataProvider }

procedure TFGraphDataProvider.change;
begin
  FSeries.change;
end;

procedure TFGraphDataProvider.iterate(processor: TDataProcessingRoutine);
var
  i : integer;
begin
  for i := 0 to count - 1 do
    processor(getPoint(i));
end;

{ TFGraphSeries }

constructor TFGraphSeries.Create(provider: TFGraphDataProvider);
begin
  inherited Create;
  FData := provider;
  FData.FSeries := self;
  FRegressionData := TFslList<TFGraphRegressionPoint>.create;
end;

destructor TFGraphSeries.Destroy;
begin
  FRegressionData.Free;
  FData.Free;
  inherited;
end;

procedure TFGraphSeries.RequestPaint(reason: TFGraphChangeType);
begin
  if FGraph = nil then
    exit;

  if (reason <> dsUnLoad) and FGraph.fplotting then
    if (reason = dsNewData) and (not FGraph.FXAxis.FAutoSizing and not FGraph.YAxis[YAxis2].FAutoSizing) then
      FGraph.PaintGraph(false)
    else
      FGraph.Repaint;
end;

procedure TFGraphSeries.SetActive(v: Boolean);
begin
  if v <> FActive then
  begin
    FActive := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetAutoZero(v: Boolean);
begin
  if v <> FAutoZero then
  begin
    FAutoZero := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetLegendStatus(v: TFGraphLegendStatus);
begin
  if v <> FLegendStatus then
  begin
    FLegendStatus := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetYAxis2(v: Boolean);
begin
  if v <> FYAxis2 then
  begin
    FYAxis2 := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetZeroOffset(v: Double);
begin
  if v <> FZeroOffset then
  begin
    FZeroOffset := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetDrawErrors(v : boolean);
begin
  if v <> FDrawerrors then
  begin
    FDrawErrors := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetDrawLine(v : boolean);
begin
  if v <> FDrawline then
  begin
    FDrawline := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetDrawPoints(v : boolean);
begin
  if v <> FDrawpoints then
  begin
    FDrawpoints := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetHighlighted(v : boolean);
begin
  if v <> FHighlighted then
  begin
    FHighlighted := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetLineColor(v : TAlphaColor);
begin
  if v <> FLineColor then
  begin
    FLineColor := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetLinestyle(v : TPenStyle);
begin
  if v <> fLinestyle then
  begin
    fLinestyle := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetPointColor(v : TAlphaColor);
begin
  if v <> FPointColor then
  begin
    FPointColor := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetFillPoints(v : boolean);
begin
  if v <> Ffillpoints then
  begin
    Ffillpoints := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetPointShape(v : TFGraphPointShape);
begin
  if v <> FPointShape then
  begin
    FPointShape := v;
    if (FPointShape <> ps_Wide) then
    begin
      if FPointsize < 2 then
        FPointsize := 2;
      if odd(FPointsize) then
        Inc(FPointsize);
    end;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetPointsize(v : word);
begin
  if v <> fPointsize then
  begin
    fPointsize := v;
    if (FPointShape <> ps_Wide) then
    begin
      if FPointsize < 2 then
        FPointsize := 2;
      if odd(FPointsize) then
        Inc(FPointsize);
    end;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetRegcontrol2(v: Double);
begin
  if v <> FRegControl2 then
  begin
    FRegControl2 := v;
    FRegenStats := true;
    requestpaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetBarOffset(v : Double);
begin
  if v <> FBarOffset then
    begin
    FBarOffset := v;
    RequestPaint(dsChangeLook);
    end;
end;

procedure TFGraphSeries.SetBarWidth(v : Double);
begin
  if v <> FBarWidth then
    begin
    FBarWidth := v;
    RequestPaint(dsChangeLook);
    end;
end;

procedure TFGraphSeries.SetBoundsColor(v : TAlphaColor);
begin
  if v <> FBoundsColor then
  begin
    FBoundsColor := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetBoundsLinestyle(v : TPenStyle);
begin
  if v <> fBoundsLinestyle then
  begin
    fBoundsLinestyle := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetRegControl1(v: LongInt);
begin
  if v <> FRegControl1 then
  begin
    FRegControl1 := v;
    FRegenStats := true;
    requestpaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetRegrColor(const v: TAlphaColor);
begin
  if v <> FRegrColor then
  begin
    FRegrColor := v;
    requestpaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetRegressionType(const v: TGraphStyleRegressionType);
begin
  if v <> FRegressionType then
  begin
    FRegressionType := v;
    FRegenStats := true;
    requestpaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetRegrLineStyle(const v : TPenStyle);
begin
  if v <> FRegrLineStyle then
  begin
    FRegrLineStyle := v;
    requestpaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetUpperBound(v : double);
begin
  if v <> FUpperBound then
  begin
    FUpperBound := v;
    if FBoundsType = bt_AsSet then requestpaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetLowerBound(v : Double);
begin
 if v <> FLowerBound then
   begin
   FLowerBound := v;
   if FBoundsType = bt_AsSet then requestpaint(dsChangeLook);
   end;
end;

procedure TFGraphSeries.SetBoundsType(v : TFGraphBoundType);
begin
  if v <> FBoundsType then
    begin
    FBoundsType := v;
    requestpaint(dsChangeLook);
    end;
end;

procedure TFGraphSeries.SetErrorsLinestyle(v : TPenStyle);
begin
  if v <> FErrorsLinestyle then
  begin
    FErrorsLinestyle := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.SetErrorsLineColor(v : TAlphaColor);
begin
  if v <> FErrorsLineColor then
  begin
    FErrorsLineColor := v;
    RequestPaint(dsChangeLook);
  end;
end;

procedure TFGraphSeries.populateRegression(m2, m, c: Double);
begin
  FRegressionData.clear;
  Data.iterate(procedure (pp : TFGraphDataPoint) begin
    if (pp.error = '') then
      addRegressionValue(pp.x, (pp.x * pp.x * m2) + (pp.x * m) + c);
  end);
end;

{Linear Regression. reference, Devore, JL.: "Probability and Statistics for the engineering and Scientists", Duxbury Press, Belmont, Ca. 1990, pp460 - 461}
procedure TFGraphSeries.DoLinearRegression;
var
  sumX, sumY, sumXY, sumX2: Double;
  pp: TFGraphDataPoint;
  i : Integer;
begin
  if (FData.Datacount < 2) or (FXStats.sd = 0) then
  begin
    FAnalysis.RegSlope2 := 0;
    if FXStats.SD = 0 then
      FAnalysis.RegSlope := 0
    else
      FAnalysis.RegSlope := 1;
    FAnalysis.RegressionIntercept := 0;
    populateRegression(FAnalysis.RegSlope2, FAnalysis.RegSlope, FAnalysis.RegressionIntercept);
  end
  else
  begin
    sumX := 0;
    sumY := 0;
    sumX2 := 0;
    sumXY := 0;
    Data.iterate(procedure (pp : TFGraphDataPoint) begin
      if (pp.error = '') then
      begin
        sumX := sumX + pp.x;
        sumY := sumY + pp.y;
        sumX2 := sumX2 + (pp.x * pp.x);
        sumXY := sumXY + (pp.x * pp.y);
      end;
    end);
    FAnalysis.RegSlope2 := 0;
    FAnalysis.RegSlope := ((Data.Datacount * sumXY) - (sumX * sumY)) / ((Data.DataCount * sumX2) - (sumX * sumX));
    FAnalysis.RegressionIntercept := (sumY / Data.DataCount) - FAnalysis.RegSlope * (sumX / Data.DataCount);
  end;
  populateRegression(FAnalysis.RegSlope2, FAnalysis.RegSlope, FAnalysis.RegressionIntercept);
end;

{
  Passing Bablok Regression: Suited for analytical comparisons - no assumption that x is without error
  reference Bablok W. and Passing, H.: "Application of Statistical Procedures in Analytical instrument testing", J. Auto. Chem. v7, 1985, pp74-79.
}
procedure TFGraphSeries.DoPassingBablok;
var
  i, j: LongInt;
  points : TList<Double>;
//  datat, pp: psPoint;
  p1, p2: TFGraphDataPoint;
  rstats : TFGraphDataStatistics;
begin
  if (FData.DataCount < 2) or (FXStats.sd = 0) then
  begin
    FAnalysis.RegSlope2 := 0;
    if FXStats.SD = 0 then
      FAnalysis.RegSlope := 0
    else
      FAnalysis.RegSlope := 1;
    FAnalysis.RegressionIntercept := 0;
    populateRegression(FAnalysis.RegSlope2, FAnalysis.RegSlope, FAnalysis.RegressionIntercept);
  end
  else
  begin
    points := TList<Double>.create;
    try
      for i := 0 to Data.count - 1 do
      begin
        p1 := Data.getPoint(i);
        if (p1.error = '') then
          for j := i+1 to Data.count - 1 do
          begin
            p2 := Data.getPoint(j);
            if p1.error = '' then
            begin
              if not (abs(p2.x - p1.x) <= FTolerance) then
                {check is really here to eliminate divide by zero errors. so FTolerance
                 is perhaps superfluous. However conceptually points considered equal
                 by FTolerance shouldn't really contribute to the regression}
                points.add((p1.y - p2.y) / (p1.x - p2.x));
            end;
          end;
      end;
      points.Sort;
      DoCalcStats(points, rstats);
      FAnalysis.RegSlope2 := 0;
      FAnalysis.RegSlope := rstats.median;
      FAnalysis.SlopeSD := rstats.SD;

      points.Clear;
      Data.iterate(procedure (pp : TFGraphDataPoint) begin
        if (pp.error = '') then
          points.Add(pp.y - (FAnalysis.RegSlope * pp.x));
      end);
      points.Sort;
      DoCalcStats(points, rstats);
      FAnalysis.RegressionIntercept := rstats.median;
      FAnalysis.IntSD := rstats.SD;  {!! this gives a falsely low sense of the sd on the intercept, as it does not include contributory uncertainty from the slope sd. yet to be fixed }
    finally
      points.Free;
    end;
    populateRegression(FAnalysis.RegSlope2, FAnalysis.RegSlope, FAnalysis.RegressionIntercept);
  end;
end;

{Quadratic Regression. reference, Devore, JL.: "Probability and Statistics for the engineering and Scientists", Duxbury Press, Belmont, Ca. 1990, pp516 - 517}
procedure TFGraphSeries.DoQuadraticRegression;
var
  sumX, sumY, sumXY, sumX2, Sumx2y, Sumx3, Sumx4: Double;
  s1y,s22,s2y,s12,s11,xmean,ymean, x2mean: extended;
  pp: TFGraphDataPoint;
  i: Integer;
begin
  if (FData.dataCount < 3) or (FXStats.SD = 0)  then
  begin
    FAnalysis.RegSlope2 := 0;
    if FXStats.sd = 0 then
      FAnalysis.Regslope := 0
    else
      FAnalysis.RegressionIntercept := 0;
  end
  else
  begin
    sumX := 0;
    sumY := 0;
    sumX2 := 0;
    sumXY := 0;
    sumx2y := 0;
    sumx3 := 0;
    sumx4 := 0;
    Data.iterate(procedure (pp : TFGraphDataPoint) begin
      if (pp.error = '') then
      begin
        sumX := sumX + pp.x;
        sumY := sumY + pp.y;
        sumX2 := sumX2 + (pp.x * pp.x);
        sumXY := sumXY + (pp.x * pp.y);
        sumx2y := sumx2y + (pp.x * pp.x * pp.y);
        sumx3 := sumx3 +  + (pp.x * pp.x * pp.x);
        sumx4 := sumx4 + (pp.x * pp.x * pp.x * pp.x);
      end;
    end);
    xmean := sumx/Data.DataCount;
    ymean := sumy/Data.DataCount;
    x2mean := sumx2/Data.DataCount;
    s1y := sumxy - Data.DataCount * xmean * ymean;
//    s2y := sumx2y - Data.DataCount * xmean * xmean * ymean;
    s2y := sumx2y - Data.DataCount * x2mean * ymean;
    s11 := sumx2 - Data.DataCount * xmean * xmean;
    s12 := sumx3 - Data.DataCount * xmean * x2mean;
    s22 := sumx4 - Data.DataCount * x2mean * x2mean;
    FAnalysis.RegSlope2 := (s2y*s11 - s1y*s12) / (s11*s22 - s12*s12);
    FAnalysis.RegSlope := (s1y*s22 - s2y*s12) / (s11*s22 - s12*s12);
    FAnalysis.RegressionIntercept := (sumY / Data.DataCount) - FAnalysis.RegSlope * (sumX / Data.DataCount) - FAnalysis.RegSlope2 * (sumX2 / Data.DataCount);
  end;
  populateRegression(FAnalysis.RegSlope2, FAnalysis.RegSlope, FAnalysis.RegressionIntercept);
end;

{ Running Average - applications in QA procedures. No reference }
{ this is a cheat procedure that is fast. it actually gives almost the same
  results as the one below for half the regcontrol2 }
procedure TFGraphSeries.DoRunningAverage;
var
  i : integer;
  pp: TFGraphDataPoint;
  avg: Double;
begin
  FRegressionData.Clear;
  if Data.DataCount > 0 then
  begin
    pp := FData.getPoint(0);
    avg := pp.y;
    addRegressionValue(pp.x, pp.y);
    for i := 1 to Data.count - 1 do
    begin
      pp := Data.getPoint(i);
      if (pp.error = '') then

      avg := FRegControl2 * (pp.y - FZeroOffset) + (1 - FRegControl2) * avg;
      addRegressionValue(pp.x, avg);
    end;
  end;
end;

{ Running Average - applications in QA procedures. No reference }
procedure TFGraphSeries.DoRunningAverageProper;
var
  queue : TList<Double>;
  queuelimit : integer;
  queuesum : double;
  pp : TFGraphDataPoint;
  i : integer;

  procedure initqueue(y:double);
  begin
    queue := TList<Double>.create;
    queue.Add(y);
    queuesum := y;
    queuelimit := trunc(1/FRegControl2);
    if queuelimit < 2 then
      raise EFslException.create('Invalid Running Average limit');
  end;

  procedure addqueue(y:double);
  begin
    if queue.Count = queuelimit then
    begin
      queuesum := queuesum - queue[0];
      queue.Delete(0);
    end;
    queue.Add(y);
    queuesum := queuesum + y;
  end;

  function queueaverage:double;
  begin
    result := queuesum / queue.count;
  end;

begin
  FRegressionData.clear;
  if Data.DataCount > 0 then
  begin
    i := -1;
    repeat
      inc(i);
      pp := FData.getPoint(i);
    until pp.error = '';
    initqueue(pp.y - FZeroOffset);
    try
      addRegressionValue(pp.x, queueaverage);
      while i < Data.count - 1 do
      begin
        inc(i);
        pp := FData.getPoint(i);
        if (pp.error = '') then
        begin
          addqueue(pp.y - FZeroOffset);
          addRegressionValue(pp.x, queueaverage);
        end;
      end;
    finally
      queue.Free;
    end;
  end;
end;

{ Distance Weighted Least Squares line fitting. McLain DH.: "Drawing Contours from arbitrary data points." Comp. J., v17, 1973? pp 318 - 324. }

function TFGraphSeries.GetDWLS(x, xDiff: Double): Double;
var
  sumX, sumY, sumXY, sumX2, j, Weight, vx, cx, cy: Double;
  pp: TFGraphDataPoint;
  i : integer;
begin
  { there's a problem with this algorythm - only works for data spread
    over a x range of 1. also suffers severely around x = 0. So the data is
    internally mapped to the range 1 to 2. Not wonderful - but has the advantage
    of standardising the meaning of regcontrol2 across different ranges }
  vx := (x - FData.getMinXValue + xDiff) / xDiff;
  j := 0;
  sumX := 0;
  sumY := 0;
  sumX2 := 0;
  sumXY := 0;
  Data.iterate(procedure (pp : TFGraphDataPoint) begin
    if (pp.error = '') then
    begin
      cx := (pp.x - FData.getMinXValue + xDiff) / xDiff;
      cy := pp.y;
      Weight := (Abs(vx - cx));
      { theory:  for DWLS,  w(d^2) = 1/(d^2 + e)^4. for NExpo, w(d^2) = exp(-a(d^2)) where a is in the order of 1/j^2 where j = avg distance between data points practice: the formula below seems to give the best results in 2d }
      Weight := Exp( Weight) / ( Weight *  Weight * Weight + FRegControl2);
      j := j + Weight;
      sumX := sumX + (Weight * cx);
      sumY := sumY + (Weight * cy);
      sumX2 := sumX2 + (cx * cx);
      sumXY := sumXY + Weight * (cx * cy);
    end;
  end);
  Result := (((j * sumXY) - (sumX * sumY)) / ((j * sumX2) - (sumX * sumX))) * vx
            + (sumY / j) - (((j * sumXY) - (sumX * sumY)) / ((j * sumX2) - (sumX * sumX))) * (sumX / j);
end;

procedure TFGraphSeries.doDWLS;
var
  rp : TFGraphRegressionPoint;
  x, xs, Xmax, xDiff: Double;
begin
  FRegressionData.Clear;
  If FRegControl1 = 0 then
    exit;
  if Data.getMinXValue = Data.getMaxXValue then
    Exit;
  xDiff := (Data.getMaxXValue - Data.getMinXValue);
  xs := xDiff / RegControl1;
  Xmax := Data.getMaxXValue + 0.00001*xs; {OK for < 100,000 points}
  x := Data.getMinXValue;
  while x <= Xmax do
  begin
    rp := TFGraphRegressionPoint.Create;
    FRegressionData.Add(rp);
    rp.x := x;
    rp.y := GetDWLS(x, xDiff);
    x := x + xs;
  end;
end;

{ spline algorithm - adapted from the "Numerical Recipes in Pascal"}
(*
function GetSplineResult(x: Double; var pp, ps: TFGraphDataPoint): Double;
var
  pp1: TFGraphDataPoint;
  h, a, b: Double;
begin
  pp := ps;
  pp1 := nil;
  while (pp.next <> nil) and (pp.x < x) do
  begin
    pp1 := pp;
    pp := pp.next;
  end;
  if pp1 = nil then pp1 := pp;
  h := pp1^.next^.x - pp1^.x;
  a := (pp1^.next^.x - x) / h;
  b := (x - pp1^.x) / h;
  Result := a * pp1^.y + b * pp1^.next^.y +
            ((a * a * a - a) * pp1^.rv + (b * b * b - b) * pp1^.next^.rv) * h * h / 6;
  pp := pp1;
end;
*)

procedure TFGraphSeries.DoSplineRegression;
var
  pp, pp1, pp2: TFGraphDataPoint;
  pn, qn, sig, un, x, xs, Xmin, Xmax: Double;
  u, u1: TFGraphRegressionPoint;
  i : integer;
begin
  FRegressionData.Clear;
//  if (FRegControl1 = 0) or Data.HasDuplicateXValues or (FRegControl2 > 0.99e30) or (FData.count < 2) then
    exit;

(*  for i := 0 to data.count - 2 do
  begin
    pp := Data.getPoint(i);
    u := TFGraphRegressionPoint.Create;
    FRegressionData.Add(u);
    if (i = 0) then
    begin
      FRegressionValues[i] := -0.5;
      pp1 := Data.getPoint(i+1);
      u.y := (3.0 / (pp1.x - pp.x)) * ((pp1.y - pp.y) / (pp1.x - pp.x) - FRegControl2)
    end
    else
    begin
      pp1 := Data.getPoint(i-1);
      pp2 := Data.getPoint(i+11);
      sig := (pp.x - pp1.x) / (pp2.x - pp1.x);
      pn := sig * FRegressionValues[i-1] + 2;
      FRegressionValues[i]:= (sig - 1) / pn;
      u.y := (pp2.y - pp.y) / (pp2.x - pp.x) - (pp.y - pp1.y) / (pp.x - pp1.x);
      u.y := (6 * u.y / (pp2.x - pp1.x) - sig * FRegressionValues[i-1]) / pn;
    end;
  end;
  qn := 0.5;
  un := (3 / (pp.x - pp1.x)) * (FRegControl2 - (pp.y - pp1.y) / (pp.x - pp1.x));
  FRegressionValues[FRegressionValues.count-1] := (un - qn * u.y) / (qn * FRegressionValues[i-1] + 1);

  for i := data.count - 2 downto 0 do
    FRegressionValues[i] := FRegressionValues[i] * FRegressionValues[i+1] + FRegressionData[i].y;

  { build regression }
  Xmin := Data.getMinXValue;
  xs := (Data.getMaxXValue - Xmin) / RegControl1;
  Xmax := Data.getMaxXValue + 0.00001*xs; {OK for < 100,000 points}
  x := Data.getMinXValue;
(*  pp := nil;
  pp2 := nil;
  while x <= Xmax do
  begin
    if FRegrData = nil then
    begin
      New(pp2);
      pp2^.next := nil;
      FRegrData := pp2;
    end
    else
    begin
      New(pp1);
      pp1^.next := nil;
      pp2^.next := pp1;
      pp2 := pp1;
    end;
    inc(FRegCount);
    pp2^.x := x;
    pp2^.rv := GetSplineResult(x, pp, FData);
    x := x + xs;
  end; *)
end;

procedure TFGraphSeries.DoRegression;
begin
  CalcStats(FXStats, true, false, 0, 0);
  CalcStats(FYStats, false, false, 0, 0);

  case FRegressionType of
    rg_None: raise EFslException.Create('internal error: regression');
    rg_Linear: DoLinearRegression;
    rg_passingBablok: DoPassingBablok;
    rg_RunningAverage: DoRunningAverage;
    rg_runningAverageproper: DoRunningAverageProper;
    rg_Spline: DoSplineRegression;
    rg_dwls: doDWLS;
    rg_quadratic: DoQuadraticRegression;
   else raise EFslException.Create('Unknown regression type');
  end;
end;

procedure TFGraphSeries.DoCalcStats(points : TList<Double>; var stats: TFGraphDataStatistics);
var
  mostVal,totalsq,last,v,svar,v1:double;
  mostCount,currCount,c,i1,i2,i3,i4,i5:longint;
  i1m,i2m,i3m,i4m,i5m:boolean;
  i : integer;
begin
  if points.Count < 2 then
  begin
    stats.comment := 'No Statistics are available unless more than one point exists';
    stats.error := true;
    exit;
  end;

  stats.comment := '';
  stats.error := false;
  stats.count := points.Count;
  stats.total := 0;
  for v in points do
    stats.total := stats.total + v;
  stats.mean := stats.total / stats.count;

  i1 := points.count div 5;
  i1m := (points.count mod 5) <> 0;
  i2 := points.count div 4;
  i2m := (points.count mod 4) <> 0;
  i3 := points.count div 2;
  i3m := (points.count mod 2) <> 0;
  i4 := points.count - points.count div 4;
  i4m := (points.count mod 4) <> 0;
  i5 := points.count - points.count div 5;
  i5m := (points.count mod 5) <> 0;

  mostcount := 0;
  mostval := 0;
  totalsq := 0;
  svar := 0;
  stats.skew := 0;
  stats.kurtosis := 0;
  c := 0;
  CurrCount := 0;
  for i := 0 to points.Count - 1 do
  begin
    inc(c);
    v := points[i];
    if c = i1 then if i1m and (i < points.Count - 1) then stats.lowquintile  := (v + points[i+1])/2 else stats.lowquintile := v;
    if c = i5 then if i5m and (i < points.Count - 1) then stats.highquintile := (v + points[i+1])/2 else stats.highquintile := v;
    if c = i2 then if i2m and (i < points.Count - 1) then stats.lowquartile  := (v + points[i+1])/2 else stats.lowquartile := v;
    if c = i4 then if i4m and (i < points.Count - 1) then stats.highquartile := (v + points[i+1])/2 else stats.highquartile := v;
    if c = i3 then if i3m and (i < points.Count - 1) then stats.median       := (v + points[i+1])/2 else stats.median := v;
    totalsq := totalsq + v * v;
    if not (abs(v - last) <= FTolerance) then
    begin
      last := v;
      currCount := 0;
    end
    else
    begin
      inc(CurrCount);
      if CurrCount > MostCount then
      begin
        MostCount := CurrCount;
        MostVal := v;
      end;
    end;
    v := v - stats.mean;
    v1 := v * v;
    svar := svar + v1;
    v1 := v1 * v;
    stats.skew := stats.skew + v1;
    v1 := v1 * v;
    stats.kurtosis := stats.kurtosis + v1;
  end;
  stats.mode := MostVal;
  stats.SD := sqrt((totalsq - (stats.Total*stats.total)/points.count) / (points.count-1));
  if (svar = 0) or (stats.SD = 0) THEN
  begin
    stats.comment := 'No skew/kurtosis are available because there is no deviance in the data';
    stats.skew := 0;
    stats.Kurtosis := 0;
  end
  else
  begin
    stats.skew := stats.skew/(points.count*stats.SD*stats.SD*stats.SD);
    stats.kurtosis := stats.kurtosis/(points.count*sqr(svar))-3.0;
  end;
end;

procedure TFGraphSeries.addRegressionValue(x, y: Double);
var
  p : TFGraphRegressionPoint;
begin
  p := TFGraphRegressionPoint.Create;
  FRegressionData.Add(p);
  p.x := x;
  p.y := y;
end;

procedure TFGraphSeries.CalcStats(var stats: TFGraphDataStatistics; isxaxis:boolean; restrict : boolean; min, max: double);
var
  pp : TFGraphDataPoint;
  list : TList<Double>;
  i : integer;
begin
  Stats.current := true;
  if Data.DataCount = 0 then
  begin
    Stats.Count := 0;
    Stats.Total := 0;
  end
  else
  begin
    list := TList<Double>.create;
    try
      Data.iterate(procedure (pp : TFGraphDataPoint) begin
        if (pp.error = '') and (not restrict or ((pp.x <= max) and (pp.x >= min))) then
          if isxaxis then
            list.Add(pp.x)
          else
            list.Add(pp.y);
      end);
      if not isxaxis then
        list.Sort;
       DoCalcStats(list,Stats);
    finally
      list.Free;
    end;
  end;
end;

procedure TFGraphSeries.GetRestrictedStats(XMin, XMax:double; var IXStats, IYStats : TFGraphDataStatistics);
begin
  CalcStats(IXStats, true, true, XMin, Xmax);
  CalcStats(IYStats, false, true, XMin, Xmax);
end;

procedure TFGraphSeries.checkRegenStats;
begin
  if FRegenStats then
    DoRegression;
  FRegenStats := false;
end;

procedure TFGraphSeries.correlate;
{this code for Pearson Correlation from Numerical Recipes in Pascal}
   function betacf(a,b,x: double): double;
   const
     itmax=100;
     eps=3.0e-7;
   var
     tem,qap,qam,qab,em,d,bz,bpp,bp,bm,az,app,am,aold,ap: double;
     m: integer;
   begin
     am := 1.0;
     bm := 1.0;
     az := 1.0;
     qab := a+b;
     qap := a+1.0;
     qam := a-1.0;
     bz := 1.0-qab*x/qap;
     for m := 1 to itmax do
     begin
       em := m;
       tem := em+em;
       d := em*(b-m)*x/((qam+tem)*(a+tem));
       ap := az+d*am;
       bp := bz+d*bm;
       d := -(a+em)*(qab+em)*x/((a+tem)*(qap+tem));
       app := ap+d*az;
       bpp := bp+d*bz;
       aold := az;
       am := ap/bpp;
       bm := bp/bpp;
       az := app/bpp;
       bz := 1.0;
       if ((abs(az-aold)) < (eps*abs(az))) THEN break;
     end;
     if ((abs(az-aold)) >= (eps*abs(az))) then
       raise EFslException.create('internal data error in correlation');
     result := az;
   end;
   function gammln(xx: double): double;
   const
     stp = 2.50662827465;
     half = 0.5;
     one = 1.0;
     fpf = 5.5;
   var
     x,tmp,ser: double;
     j: integer;
     cof: array [1..6] of double;
   begin
     cof[1] := 76.18009173;
     cof[2] := -86.50532033;
     cof[3] := 24.01409822;
     cof[4] := -1.231739516;
     cof[5] := 0.120858003e-2;
     cof[6] := -0.536382e-5;
     x := xx-one;
     tmp := x+fpf;
     tmp := (x+half)*ln(tmp)-tmp;
     ser := one;
     for j := 1 to 6 do
     begin
       x := x+one;
       ser := ser+cof[j]/x
     end;
     result := tmp+ln(stp*ser)
   end;
   function betai(a,b,x: double): double;
   var bt: double;
   begin
     if ((x < 0.0) or (x > 1.0)) then
       raise EFslException.create('Invalid internal data, Correlation');
     bt := exp(gammln(a+b)-gammln(a)-gammln(b) +a*ln(x)+b*ln(1.0-x));
     if (x < ((a+1.0)/(a+b+2.0))) then
       result := bt*betacf(a,b,x)/a
     else
       result := 1.0-bt*betacf(b,a,1.0-x)/b;
   end;

var
  yt,xt,t,sumyy,sumxy,sumxx,df,z,zl,zu,YAverage,XAverage: double;
  i:longint;
  pp:TFGraphDataPoint;
begin
  FAnalysis.current := true;
  FAnalysis.Error := false;
  if Data.DataCount < 3 then
  begin
    FAnalysis.error := true;
    FAnalysis.Comment := 'No Correlation available unless at least 3 points exist';
    exit;
  end;

  XAverage := 0.0;
  YAverage := 0.0;
  Data.iterate(procedure (pp : TFGraphDataPoint) begin
    if (pp.error = '') then
    begin
      XAverage := XAverage + pp.x;
      YAverage := YAverage + pp.y;
    end;
  end);
  XAverage := XAverage/Data.DataCount;
  YAverage := YAverage/Data.DataCount;
  sumxx := 0.0;
  sumyy := 0.0;
  sumxy := 0.0;
  Data.iterate(procedure (pp : TFGraphDataPoint) begin
    if (pp.error = '') then
    begin
      xt := pp.x-XAverage;
      yt := pp.y-YAverage;
      sumxx := sumxx+sqr(xt);
      sumyy := sumyy+sqr(yt);
      sumxy := sumxy+xt*yt;
    end;
  end);

  with FAnalysis do
    begin
    if (sumxx = 0) or (sumyy = 0) then
      begin
      Comment := 'Invalid Correlation due to lack of variance in a variable';
      exit;
      end;
    PearsonR := sumxy/sqrt(sumxx*sumyy);
    RSquared := PearsonR * PearsonR;
    if Data.DataCount < 10 then
      begin
      PRMin := 0;
      PRMax := 0;
      end
    else
      begin
      z := 0.5 * ln((1 + PearsonR) / (1 - PearsonR));
      zl := z - 1.96 / sqrt(Data.DataCount-3);
      zu := z + 1.96 / sqrt(Data.DataCount-3);
      PRMin := (exp(2 * zl) - 1) / (exp(2 * zl) + 1);
      PRMax := (exp(2 * zu) - 1) / (exp(2 * zu) + 1);
      end;
    RSqMin := PRMin * PRMin;
    RSqMax := PRMax * PRMax;
{    z := 0.5*ln(((1.0+PearsonR)+tiny)/((1.0-PearsonR)+tiny));}
    df := Data.DataCount-2;
    t := PearsonR*sqrt(df/(((1.0-PearsonR)+tiny)*((1.0+PearsonR)+tiny)));
    PValue := betai(0.5*df,0.5,df/(df+sqr(t)))
    end;
end;

function TFGraphSeries.GetYStats;
begin
  checkRegenStats;
  result := FYStats;
end;

function TFGraphSeries.link: TFGraphSeries;
begin
  result := TFGraphSeries(inherited link);
end;

function TFGraphSeries.GetXStats;
begin
  checkRegenStats;
  result := FXStats;
end;

function TFGraphSeries.GetCompStats;
begin
  checkRegenStats;
  result := FAnalysis;
end;

{ TFGraphAnnotation }

constructor TFGraphAnnotation.create(const caption : string; DrawTime : TFGraphDrawTime);
begin
  inherited create;
  FCaption := caption;
  FVisible := true;
  FDrawTime := DrawTime;
end;

procedure TFGraphAnnotation.SetCaption(v : string);
begin
  if v <> FCaption then
  begin
    FCaption := v;
    Change;
  end;
end;

procedure TFGraphAnnotation.SetVisible(v : boolean);
begin
  if v <> FVisible then
  begin
    FVisible := v;
    Change;
  end;
end;

procedure TFGraphAnnotation.SetDrawTime(v : TFGraphDrawTime);
begin
  if v <> FDrawTime then
  begin
    FDrawTime := v;
    Change;
  end;
end;

{ TFGraphMark }

procedure TFGraphMark.SetAlphaColor(v : TAlphaColor);
begin
  if v <> FColor then
  begin
    FColor := v;
    Change;
  end;
end;

procedure TFGraphMark.Setx1(v : double);
begin
  if v <> Fx1 then
  begin
    Fx1 := v;
    Change;
    end;
end;

procedure TFGraphMark.Sety1(v : double);
begin
  if v <> Fy1 then
  begin
    Fy1 := v;
    Change;
  end;
end;

procedure TFGraphMark.Setx2(v : double);
begin
  if v <> Fx2 then
  begin
    Fx2 := v;
    Change;
  end;
end;

procedure TFGraphMark.Sety2(v : double);
begin
  if v <> Fy2 then
  begin
    Fy2 := v;
    Change;
  end;
end;

procedure TFGraphMark.SetMarkType(v : TFGraphMarkType);
begin
  if v <> FMarkType then
  begin
    FMarkType := v;
    Change;
  end;
end;

procedure TFGraphMark.SetMarkPos(v : TFGraphMarkPos);
begin
  if v <> FMarkPos then
  begin
    FMarkPos := v;
    Change;
  end;
end;

function  TFGraphMark.TypeLabel : string;
begin
  result := 'Mark';
end;

{ TFGraphAxis }

procedure TFGraphAxis.InitLogTicks;
begin
  with FLogTickInfo do
  begin
    DrawMinorLabels := true;
    if LogTickCount > MaxLogTickCount then LogTickCount := MaxLogTickCount;
    LogTickArray[0] := 1;
   {CAREFUL! these LogTickCount values must correspond with AllowedLogTickCounts}
    case LogTickCount of
      0 :
         begin
         LogTickCount := 0;
         end;
      1 :
         begin
         LogTickCount := 1;
         LogTickArray[1] := 5;
         end;
      2, 3 :
         begin
         LogTickCount := 2;
         LogTickArray[1] := 2;
         LogTickArray[2] := 5;
         end;
      4, 5, 6 :
         begin
         LogTickCount := 5;
         LogTickArray[1] := 1.5;
         LogTickArray[2] := 2;
         LogTickArray[3] := 3;
         LogTickArray[4] := 5;
         LogTickArray[5] := 7;
         end;
      7, 8 :
         begin
         LogTickCount := 7;
         LogTickArray[1] := 1.5;
         LogTickArray[2] := 2;
         LogTickArray[3] := 3;
         LogTickArray[4] := 4;
         LogTickArray[5] := 5;
         LogTickArray[6] := 6;
         LogTickArray[7] := 8;
         end;
      9 : begin
         LogTickArray[1] := 1.2;
         LogTickArray[2] := 1.5;
         LogTickArray[3] := 2;
         LogTickArray[4] := 2.5;
         LogTickArray[5] := 3;
         LogTickArray[6] := 4;
         LogTickArray[7] := 5;
         LogTickArray[8] := 6;
         LogTickArray[9] := 8;
         end;
    end;
    LogTickArray[LogTickCount + 1] := 10;
    {This is a guard value for the linear searches in GetFirstTick and SetLogMinMax}
  end;
end;

procedure TFGraphAxis.AdjustLogTickCount;
{CAREFUL! these LogTickCount values must correspond with AllowedLogTickCounts}
var
  r : Double;
begin
  r := FGraph.FAppearance.MinSteps/(ln(FMax/FMin)*Loge);
  with FLogTickInfo do
    if r > 10 then
      LogTickCount := 9
    else if r > 8 then
      LogTickCount := 7
    else if r > 6 then
      LogTickCount := 5
    else if r > 3 then
      LogTickCount := 2
    else if r > 2 then
      LogTickCount := 1
    else
      LogTickCount := 0;
  InitLogTicks;
end;

procedure TFGraphAxis.CheckDrawMinorLabels;
begin
  with FLogTickInfo do
  if LogStepping then
    DrawMinorLabels := true
  else
    DrawMinorLabels := ( (ln(FMax/FMin)*Loge)*LogTickCount < 10)
end;

{ step size chosen in a 1,2,5,10 squence depending not only on the
  characteristic, but also the mantissa, of the range}
function GetStep(minSteps : Word; FMax, FMin : double) : Double;
var
  w, t, B : Double;
begin
  w := FMax - FMin;
  if w <= 0 then raise EFslException.Create('GetStep entered with bad range');
  t := ln(w)*Loge;
  if t < 0 then t := t - 1;
  B := exp( trunc(t * 1.001) / Loge );
  if         w/B >= minSteps then Result := B
  else if  2*w/B >= minSteps then Result := B/2
  else if  5*w/B >= minSteps then Result := B/5
  else if 10*w/B >= minSteps then Result := B/10
  else if 20*w/B >= minSteps then Result := B/20
  else if 50*w/B >= minSteps then Result := B/50
  else                            Result := B/100;
  {sufficient for maxSteps <= 125}
end;

function getDateStep(FDateTickType : TFGraphDateTickType; minSteps : Word; FMax, FMin : double) : double;
begin
{ these are approximations - they are not used for actually
  plotting the ticks, but for checking the scale }
  case FDateTickType of
    dt_minute    : result := cminute1;
    dt_5minute   : result := cminute5;
    dt_halfhourly : result := cminute30;
    dt_hourly    : result := chour;
    dt_daily     : result := 1;
    dt_weekly    : result := 7;
    dt_monthly   : result := 30;
    dt_2monthly  : result := 61;
    dt_quarterly : result := 91;
    dt_annually  : result := 365;
    dt_decade    : result := 3654;
    dt_century   : result := 36540;
    dt_custom    : result := getStep(minsteps, fmax, fmin);
  else result := 1;
  end;
end;

procedure TFGraphAxis.SetMinMax;
begin
  if FOMin > FOMax then
    raise EFslException.Create('Impossible : FOMin > FOMax in SetMinMax!');

  if (FOMin = FOMax) then
  begin
    if (FOMin <> 0) then
    begin
      if (FOMin < 0) then
      begin
        FMin := FOMin * 1.2;
        FMax := 0;
      end
      else
      begin
        FMin := 0;
        FMax := FOMax * 1.2;
      end;
      FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin);
      FMax := trunc(FMax / FStep) * FStep;
    end
    else
    begin
      FMin := -1;
      FMax := 1;
      FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin);
    end;
    Exit;
  end
  else
  begin
    FMin := FOMin;
    FMax := FOMax;
  end;
  {assert : FMin<FMax}
  if FAutoStepping or (FStep <= 0) or ( FStep/(FMax-FMin) < 1/FGraph.appearance.FMaxSteps ) then
    FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin);

  FMin := trunc( FMin / FStep) * FStep;
  if (FOMin < 0) then FMin := FMin - FStep;
  if (FMin > FOMin - 0.01*FStep) then FMin := FMin - FStep;

  FMax := trunc(FMax / FStep) * FStep + FStep;
  if (FOMax < 0) then FMax := FMax - FStep;
  if (FMax < FOMax + 0.01*FStep) then FMax := FMax + FStep;

  if (FOMin > 0) and (FOMin /(FMax - FMin) < 0.25) then
    FMin := 0
  else if (FOMax < 0) and (-FOMax / (FMax - FMin) < 0.25) then
    FMax := 0;
if Fmin >= FMax then raise EFslException.Create('SetMinMax failed');
end;


procedure TFGraphAxis.SetLogMinMax;
const
  above = true;
  below = not above;

  function GetLogTick(b : Boolean; v : Double) : Double;
  var
    ix : Word;
  begin
    with FLogTickInfo do
    begin
      if b = above then
      begin
        ix := 1;
        while (ix < LogTickCount+1) and
              (LogTickArray[ix] < v) do Inc(ix);
      end
      else {below}
      begin
        ix := LogTickCount;
        while (ix > 0) and
              (LogTickArray[ix] > v) do Dec(ix);
      end;
      Result := LogTickArray[ix];
    end;
  end;

var
  minChi, maxChi, minB, maxB, t : Double;
  minN, maxN : Integer;

begin {SetLogMinMax}
  if (FOMin = FOMax) then
  begin
    FMin := 0.5*FOMax;
    FMax := FOMax;
  end
  else
  begin
    FMin := FOMin;
    FMax := FOMax;
  end;

  t := ln(FMin) * Loge;
  minN := trunc(t);
  if (t < 0) and (t <> minN) then Dec(minN);
  minChi := exp( (t - minN) / Loge );
  minB := exp(minN/Loge);

  t := ln(FMax) * Loge;
  maxN := trunc(t);
  if ((t < 0) and (t <> maxN)) or (t = 0) then Dec(maxN);
  maxChi := exp( (t - maxN) / Loge );
  maxB := exp(maxN/Loge);

  if FAutoStepping then
    AdjustLogTickCount;

  if (FMax/FMin < 4) then
  begin
    {need to set up for constant ticks across FMin..FMax}
    FLogTickInfo.LogStepping := true;
    SetMinMax;
  end
  else
  begin
    FLogTickInfo.LogStepping := false;
    if (FMax/FMin < 10) then
    begin
      FMin := minB*GetLogTick(below, minChi);
      FMax := maxB*GetLogTick(above, maxChi);
    end
    else  if (FMax/FMin < 30) then
    begin
      FMin := GetLogTick(below, minChi);
      if FMin > 1 then FMin := GetLogTick(below, FMin);
      FMin := minB*FMin;
      FMax := GetLogTick(above, maxChi);
      if FMax < 10 then FMax := GetLogTick(above, FMax);
      FMax := maxB*FMax;
    end
    else
    begin
      FMin := minB;
      FMax := maxB*10;
    end;
  end;
  CheckDrawMinorLabels;
end;

procedure incmonth(var y,m : word; i : word);
{ add i months to date xx/m/y }
begin
 if m + i > 12 then
   begin
   inc(y);
   m := m + i - 12;
   end
 else inc(m,i);
end;

procedure TFGraphAxis.SetDateMinMax;
var nYear, nMonth, nDay, nHour, nMin, nSec, nMSec,
    xYear, xMonth, xDay, xHour, xMin, xSec, xMSec : Word;
begin
  if (FOMax - FOMin <= 0) or (FOMin < 1) then exit;
  DecodeDate(FOMin, nYear, nMonth, nDay);
  DecodeDate(FOMax, xYear, xMonth, xDay);
  DecodeTime(FOMin, nHour, nMin, nSec, nMSec);
  DecodeTime(FOMax, xHour, xMin, xSec, xMSec);
  case FDateTickType of
   {dt_custom : { in this case no action is taken to change the scale of the axes }
      {setMinMax; called elsewhere}
   dt_minute :      {set begin to nearest whole minute before start and set end to}
                   {nearest whole minute after end}
      begin
      FMin := EncodeDate(nYear, nMonth, nDay) + EncodeTime(nHour, nMin, 0, 0);
      If xSec <> 0
       then FMax := EncodeDate(xYear, xMonth, xDay)
                      + EncodeTime(xHour, xMin, 0, 0) + encodetime(0,1,0,0)
       else FMax := EncodeDate(xYear, xMonth, xDay)
                      + EncodeTime(xHour, xMin, 0, 0);
      end;
   dt_5minute : begin
      FMin := EncodeDate(nYear, nMonth, nDay) + EncodeTime(nHour, 5 * (nMin div 5), 0, 0);
      If (xMin mod 5 <> 0) or (xSec <> 0)
        then FMax := EncodeDate(xYear, xMonth, xDay) + EncodeTime(xHour,
                        5 * (xMin div 5), 0, 0) + encodetime(0,5,0,0)
        else FMax := EncodeDate(xYear, xMonth, xDay) + EncodeTime(xHour, xMin, 0, 0);
      end;
   dt_halfhourly : begin
      if nMin >= 30
       then FMin := EncodeDate(nYear, nMonth, nDay) + EncodeTime(nHour, 30, 0, 0)
       else FMin := EncodeDate(nYear, nMonth, nDay) + EncodeTime(nHour, 0, 0, 0);
      If (xMin = 0) and (xSec = 0)
       then FMax := EncodeDate(xYear, xMonth, xDay) + EncodeTime(xHour, 0, 0, 0)
       else if (xMin >= 30)
        then FMax := EncodeDate(xYear, xMonth, xDay) + EncodeTime(xHour, 0, 0, 0)
                      + encodetime(1,0,0,0)
        else FMax := EncodeDate(xYear, xMonth, xDay) + EncodeTime(xHour, 30, 0, 0);
      end;
   dt_hourly : begin
      Fmin := EncodeDate(nYear, nMonth, nDay) + EncodeTime(nHour, 0, 0, 0);
      if (xSec <> 0) or (xMin <> 0)
       then FMax := EncodeDate(xYear, xMonth, xDay) + EncodeTime(xHour, 0, 0, 0)
                  + encodetime(1,0,0,0)
       else FMax := EncodeDate(xYear, xMonth, xDay) + EncodeTime(xHour, 0, 0, 0);
      end;
   dt_daily : begin
      FMin :=  EncodeDate(nYear, nMonth, nDay);
      if (xHour <> 0) or (xMin <>0) or (xSec <> 0)
       then FMax := EncodeDate(xYear, xMonth, xDay) + 1
       else FMax := EncodeDate(xYear, xMonth, xDay);
      end;
   dt_weekly : begin
      FMin := EncodeDate(nYear, nMonth, nDay) - (DayOfWeek(FOMin) - 1);
      FMax := EncodeDate(xYear, xMonth, xDay) + 8 - DayOfWeek(FOMin); {???}
      end;
   dt_monthly : begin
      FMin :=  EncodeDate(nYear, nMonth, 1);
      if (xday > 1) or (xHour <> 0) or (xMin <>0) or (xSec <> 0)
       then incmonth(xYear,xMonth,1); {month is not set time period}
      FMax := EnCodeDate(xYear,xMonth, 1);
      end;
   dt_2monthly : begin
      if nMonth mod 2 <> 1 then dec(nMonth);
      FMin :=  EncodeDate(nYear, nMonth, 1);
      if (xday > 1) or (xHour <> 0) or (xMin <>0) or (xSec <> 0)
       then incmonth(xYear,xMonth,1); {month is not set time period}
      if xMonth mod 2 <> 1 then inc(xMonth);
      FMax := EnCodeDate(xYear,xMonth, 1);
      end;
   dt_quarterly : begin
      while nMonth mod 3 <> 1 do dec(nMonth);
      FMin :=  EncodeDate(nYear, nMonth, 1);
      if (xday > 1) or (xHour <> 0) or (xMin <>0) or (xSec <> 0)
        then incmonth(xYear,xMonth,1); {month is not set time period}
      while xMonth mod 3 <> 1 do incmonth(xYear,xMonth,1);
      FMax := EnCodeDate(xYear,xMonth, 1);
      end;
   dt_annually : begin
      FMin := EncodeDate(nYear, 1, 1);
      if (xMonth > 1) or(xday > 1) or (xHour <> 0) or (xMin <>0) or (xSec <> 0)
       then FMax := EncodeDate(xYear+1,1,1)
                  { year is also not a set time,  but there can't be overflow here }
       else FMax := EnCodeDate(xYear,1,1);
      end;
   dt_decade : begin
      FMin := EncodeDate(10 * (nYear div 10), 1, 1);
      if (xMonth > 1) or(xday > 1) or (xHour <> 0) or (xMin <>0) or (xSec <> 0)
        then inc(xYear);
      FMax := EncodeDate(10 * (xYear div 10),1,1);
      end;
   dt_century : begin
      FMin := EncodeDate(100 * (nYear div 100), 1, 1);
      if (xMonth > 1) or(xday > 1) or (xHour <> 0) or (xMin <>0) or (xSec <> 0)
        then inc(xYear);
      FMax := EncodeDate(100 * (xYear div 100),1,1);
      end;
  end;
end;

function TFGraphAxis.DoResize : Boolean;

  function ArbitraryMinMax : Boolean;
  var hold : Double;
  begin
    Result := true;
    if not FLogging then
      if FShowAsTime then
      begin
      FMax := now;
      Fmin := FMax - 5 * GetDateStep(FDateTickType, FGraph.FAppearance.MinSteps, FMax, FMin);
      end
      else
      begin
        if (FMin = FMax) then
        begin
          if (FMin <> 0) then
            FMin := 0
          else
          begin
            FMin := -1;
            FMax := 1;
          end;
        end
        else if (FMin > FMax) then
        begin
          hold := FMin;
          FMin := FMax;
          FMax := hold;
        end
        else
          Result := false;
        FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin);
    end
    else {logging}
    begin
      Result := false;
      if (FMin > FMax) then
      begin
        hold := FMin;
        FMin := FMax;
        FMax := hold;
        Result := true;
      end;
      if (FMax <= 0) then
      begin
        FMax := 10;
        FMin := 1;
      end
      else if (FMin <= 0) then
        FMin := FMax/10
      else
        Result := false or Result;
    end
  end;

begin {DoResize}
  if FOMax - FOMin < FMinScale then FOMax := FOMin + FMinScale;
  Result := false;
  if not FGraph.DataFound(self.FSecondAxis) then
  begin
    Result := ArbitraryMinMax;
    if FLogging then
      CheckDrawMinorLabels;
    Exit;
  end;

 {data : pick intelligent min/max/tick values : }
  if FAutoSizing then
  begin
    if not FLogging then
      if (FShowAsTime) and (FDateTickType <> dt_custom) then
        SetDateMinMax
      else
      begin
        SetMinMax;
        if FAutoStepping and (FOMin < FOMax) then
        begin
          FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin);
          SetMinMax;
        end;
      end
    else
    begin {logging}
      if (FOMin <= 0) then Exit; {nothing can be done}
      SetLogMinMax;
      if FAutoStepping and (FOMin < FOMax) and (not FLogTickInfo.LogStepping) then
      begin
        AdjustLogTickCount;
        SetLogMinMax;
      end;
    end;
    Result := (FMin <> FOMin) or (FMax <> FOMax);
  end;
end;

function OneSigFigDecs(v : Double) : Integer;
{eg : OneSigFigDecs(0.1)->1; (100)-> -2}
var
  t : Double;
begin
  t := ln(v*1.01)*Loge;
  if t < 0 then
    Result  := -trunc(t) + 1
  else
    Result  := -trunc(t);
end;


procedure  TFGraphAxis.AdjustLabelDecs;
begin
  if Not FAutoLabelDecimals then exit;
  FLabelDec := OneSigFigDecs(FStep*1.01);
  if FLabelDec < 0 then
    FLabelDec := 0;
end;

procedure  TFGraphAxis.AdjustLogLabelDecs(v : Double);
{the >= 5 must correspond with InitLogTicks : where do fraction steps start?}
begin
  FLabelDec := OneSigFigDecs(v*1.01);
  if FLogTickInfo.LogTickCount >= 5 then Inc(FLabelDec);
  if FLabelDec < 0 then
    FLabelDec := 0;
end;

function TFGraphAxis.GetFirstTick(var logTickInfo : TFGraphLogTickInfo) : Double;
var
  t, B : Double;
  j : Word;
begin
  if not FLogging or logTickInfo.LogStepping then
  if FShowAsTime then result := getfirstdatetick else
  begin
    Result := FMin + 0.01*FStep;
    Result := trunc( Result / FStep ) * FStep;
    if (FMin < 0) then Result := Result - FStep;
    if (FMin > Result + 0.01*FStep) then Result := Result + FStep;
    AdjustLabelDecs;
  end
  else {logging}
  begin
    t := ln(FMin) * Loge;
    if t < 0 then t := t - 1;
    B := exp( trunc(1.001 * t) / Loge ); { OK for FMin < 10^1000 }
    if B > FMin then B := B/10;

    {pre-condition : the TFGraphLogTickInfo has been initialized with 1..10 : }
    with logTickInfo do
    begin
      for j := 0 to LogTickCount+1 do
        LogTickArray[j] := B * LogTickArray[j];

      LogTickIndex := 0;
      t := FMin*0.999;
      while logTickInfo.LogTickArray[LogTickIndex] < t do
        Inc(LogTickIndex);
      Result := LogTickArray[LogTickIndex];
      AdjustLogLabelDecs(Result);
    end;
  end;
end;

function TFGraphAxis.GetFirstDateTick;
var Year, Month, Day, Hour, mMin, Sec, MSec : Word;
begin
  DecodeDate(Fmin, Year, Month, Day);
  DecodeTime(FMin, Hour, mMin, Sec, MSec);
  case FDateTickType of
   dt_custom : result := FMin;
   dt_minute : If Sec <> 0 then
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, mMin, 0, 0)
                  + encodetime(0,1,0,0) else
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, mMin, 0, 0);
   dt_5minute : If (mMin mod 5 <> 0) or (Sec <> 0) then
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour,
                        5 * (mMin div 5), 0, 0) + encodetime(0,5,0,0)  else
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, mMin, 0, 0);
   dt_halfhourly : If (mMin = 0) and (Sec = 0) then
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, 0, 0, 0)
                 else if (mMin >= 30) then
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, 0, 0, 0)
               + encodetime(1,0,0,0) else
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, 30, 0, 0);
   dt_hourly : if (Sec <> 0) or (mMin <> 0) then
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, 0, 0, 0)
                  + encodetime(1,0,0,0) else
              result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, 0, 0, 0);
   dt_daily : if (Hour <> 0) or (mMin <>0) or (Sec <> 0) then
              result := EncodeDate(Year, Month, Day) + 1 else
              result := EncodeDate(Year, Month, Day);
   dt_weekly : result := EncodeDate(Year, Month, Day) + 8 - DayOfWeek(FOMin); {???}
   dt_monthly : begin
             if (Day > 1) or (Hour <> 0) or (mMin <>0) or (Sec <> 0) then
                incmonth(Year,Month,1); {month is not set time period}
             result := EnCodeDate(Year,Month, 1);
             end;
   dt_2monthly : begin
               if (Day > 1) or (Hour <> 0) or (mMin <>0) or (Sec <> 0) then
                 incmonth(Year,Month,1); {month is not set time period}
               if Month mod 2 <> 1 then inc(Month);
               result := EnCodeDate(Year,Month, 1);
               end;
   dt_quarterly : begin
               if (Day > 1) or (Hour <> 0) or (mMin <>0) or (Sec <> 0) then
                 incmonth(Year,Month,1); {month is not set time period}
               while Month mod 3 <> 1 do incmonth(Year,Month,1);
               result := EnCodeDate(Year,Month, 1);
               end;
   dt_annually : begin
               if (Month > 1) or(Day > 1) or (Hour <> 0) or
                     (mMin <>0) or (Sec <> 0) then
                result := EncodeDate(Year+1,1,1)
                  { year is also not a set time,  but there can't be overflow here }
               else result := EnCodeDate(Year,1,1);
               end;
   dt_decade : begin
               if (Month > 1) or(Day > 1) or (Hour <> 0) or
                     (mMin <>0) or (Sec <> 0) then  inc(Year);
               result := EncodeDate(10 * (Year div 10),1,1);
               end;
   dt_century : begin
               if (Month > 1) or(Day > 1) or (Hour <> 0) or
                     (mMin <>0) or (Sec <> 0) then  inc(Year);
               result := EncodeDate(100 * (Year div 100),1,1);
               end;
   else result := 1;
  end;
end;

function TFGraphAxis.GetNextDateTick;
var year,month,day : word;
begin
 case FDateTickType of
  dt_minute : result := tick + cminute1;
  dt_5minute : result := tick + cminute5;
  dt_halfhourly : result := tick + cminute30;
  dt_hourly : result := tick + chour;
  dt_daily : result := tick + 1;
  dt_weekly : result := tick + 7;
  dt_monthly : begin
             decodedate(tick,year,month,day);
             incmonth(year,month,1);
             result := encodedate(year, month, 1);
             end;
  dt_2monthly : begin
             decodedate(tick,year,month,day);
             incmonth(year,month,2);
             result := encodedate(year, month, 1);
             end;
  dt_quarterly : begin
             decodedate(tick,year,month,day);
             incmonth(year,month,3);
             result := encodedate(year, month, 1);
             end;
  dt_annually : begin
             decodedate(tick,year,month,day);
             result := encodedate(year+1, 1, 1);
             end;
  dt_decade : begin
             decodedate(tick,year,month,day);
             result := encodedate(year+10, 1, 1);
             end;
  dt_century : begin
             decodedate(tick,year,month,day);
             result := encodedate(year+100, 1, 1);
             end;
  dt_custom : Result := tick + FStep;
  else result := tick+FStep;
 end;
end;

function TFGraphAxis.GetNextTick(tick : Double; var logTickInfo : TFGraphLogTickInfo; var drawThisLabel : Boolean) : Double;
var j : Word;
begin
  if not FLogging or logTickInfo.LogStepping then
    if FShowAsTime then
    begin
      result := getnextdateTick(tick);
      drawthislabel := true;
    end
    else
    begin
      Result := tick + FStep;
      drawThisLabel := true;
    end
  else {logging}
    with logTickInfo do
    begin
      Inc(LogTickIndex);
      if LogTickIndex >= LogTickCount + 1 then
      begin
        for j := 0 to LogTickCount + 1 do
          LogTickArray[j] := 10 * LogTickArray[j];
        LogTickIndex := 0;
      end;
      Result := LogTickArray[LogTickIndex + 1];
      if (LogTickIndex = 0) then
        AdjustLogLabelDecs(Result);
      drawThisLabel := DrawMinorLabels or
                       (LogTickIndex = 0) or
                       (LogTickIndex = LogTickCount+1);
    end;
end;

function TFGraphAxis.CheckScale : Boolean;
begin
  Result := (FMin < FMax) and ((not FLogging) or (FMin > 0)) and
              not (FShowAsTime and (Fmin < 1));
  FScaledOk := result;
end;

procedure TFGraphAxis.CalcMetrics;
begin
  if (FMin>=FMax) then
    raise EFslException.Create(FAxisTitle+'CalcMetrics : XMin>=XMax');
  if self = FGraph.FXAxis then
    if FLogging then
      FM := (FGraph.FCurrWidth - FGraph.FDimensions.FLeft - FGraph.FDimensions.FRight) / (Ln(FMax) - Ln(FMin))
    else
      FM := (FGraph.FCurrWidth - FGraph.FDimensions.FLeft - FGraph.FDimensions.FRight) / (FMax - FMin)
  else if FLogging then
    FM := (FGraph.FCurrHeight - FGraph.FDimensions.FTop - FGraph.FDimensions.FBottom) / (Ln(FMax) - Ln(FMin))
  else
    FM := (FGraph.FCurrHeight - FGraph.FDimensions.FTop - FGraph.FDimensions.FBottom) / (FMax - FMin);
end;

procedure TFGraphAxis.SetAxisTitle(v : string);
begin
  FAxisTitle := v;
  if FGraph.fplotting then FGraph.RePaint;
end;

procedure TFGraphAxis.SetLabelDec(v : Integer);
begin
  FLabelDec := v;
  change;
end;

procedure TFGraphAxis.SetLogTickCount(v : Word);
begin
  FLogTickInfo.LogTickCount := v;
  InitLogTicks;
  change;
end;

procedure TFGraphAxis.Setdateticktype;
begin
  if v <> FDateTickType then
    begin
    FDateTickType := v;
    change;
    end;
end;

function TFGraphAxis.GetLogTickCount : Word;
begin
  Result := FLogTickInfo.LogTickCount;
end;

procedure TFGraphAxis.SetLogging(v : Boolean);
begin
  FLogging := v;
  change;
end;

procedure TFGraphAxis.SetAutoSizing(v : Boolean);
begin
  FAutoSizing := v;
  change;
end;

procedure TFGraphAxis.SetAutoStepping(v : Boolean);
begin
  FAutoStepping := v;
  change;
end;

procedure TFGraphAxis.SetMax(v : Double);
begin
  FAutoSizing := False;
  if FMax <> v then
  begin
    FMax := v;
    if FShowASTime and not FGraph.datafound(self.FSecondAxis) then FOMax := FMax;
    FGraph.DoRescaleEvent;
    change;
  end;
end;

procedure TFGraphAxis.SetMin(v : Double);
begin
  FAutoSizing := False;
  if FMin <> v then
  begin
    FMin := v;
    if FShowASTime and not FGraph.datafound(self.FSecondAxis) then FOMin := FMin;
    FGraph.DoRescaleEvent;
    change;
  end;
end;

procedure TFGraphAxis.SetStep(v : Double);
begin
  FAutoStepping := False;
  if FStep <> v then
  begin
    FStep := v;
    FGraph.DoRescaleEvent;
    change;
  end;
end;

procedure TFGraphAxis.SetMinDiff(v : Double);
var holdmin : Double;
begin
  holdmin := FMinScale;
  FMinScale := v;
  if FGraph.Plotting and ((FMax - FMin < FMinScale) or (FMax - FMin = holdmin))
  then FGraph.rePaint;
end;

procedure TFGraphAxis.SetShowASTime;
begin
  if v <> FShowASTime then
  begin
    FShowAsTime := v;
    if FShowASTime and not FGraph.datafound(self.FSecondAxis) then
      begin
      FOMin := FMin;
      FOMax := FMax;
      end;
    if FGraph.Plotting then FGraph.Repaint;
  end;
end;

procedure TFGraphAxis.SetDateFormat;
begin
  if v <> FDateFormat then
    begin
    FDateFormat := v;
    if FGraph.Plotting then FGraph.RePaint;
    end;
end;

procedure TFGraphAxis.SetShowAxis;
begin
  if v <> FShowAxis then
    begin
    FShowAxis := v;
    if FGraph.Plotting then FGraph.RePaint;
    end;
end;

procedure TFGraphAxis.SetReversed;
begin
  if v <> FReversed then
    begin
    FReversed := v;
    if FGraph.Plotting then FGraph.RePaint;
    end;
end;

procedure TFGraphAxis.SetOffsetType;
begin
  if v <> FOffsetType then
    begin
    FOffsetType := v;
    if FGraph.Plotting then FGraph.RePaint;
    end;
end;

procedure TFGraphAxis.Setoffset;
begin
  if v <> FOffset then
    begin
    FOffset := v;
    if FGraph.Plotting then FGraph.RePaint;
    end;
end;

procedure TFGraphAxis.SetGridlines(v : Boolean);
begin
  FGridlines := v;
  change;
end;

procedure TFGraphDimensions.SetMargBottom(v : Word);
begin
  FBottom := v;
  change;
end;

procedure TFGraphDimensions.SetMargTop(v : Word);
begin
  FTop := v;
  change;
end;

procedure TFGraphDimensions.SetMargLeft(v : Word);
begin
  FLeft := v;
  change;
end;

procedure TFGraphDimensions.SetMargRight(v : Word);
begin
  FRight := v;
  change;
end;

procedure TFGraphDimensions.SetTickLength(v : Word);
begin
  FTMLength := v;
  if v = 0 then FGraph.FAppearance.FTickMarks := False;
  change;
end;

procedure TFGraphDimensions.SetGraphTitleDistance(v : Integer);
begin
  FGraphTitleDistance := v;
  change;
end;

procedure TFGraphDimensions.SetXAxisTitleDistance(v : Integer);
begin
  FXAxisTitleDistance := v;
  change;
end;

procedure TFGraphDimensions.SetYAxisTitleDistance(v : Integer);
begin
  FYAxisTitleDistance := v;
  change;
end;

procedure TFGraphDimensions.SetXAxisLabelDistance(v : Integer);
begin
  FXAxisLabelDistance := v;
  change;
end;

procedure TFGraphDimensions.SetYAxisLabelDistance(v : Integer);
begin
  FYAxisLabelDistance := v;
  change;
end;

procedure TFGraphDimensions.SetScale(v : Word);
begin
  if v < 20 then v := 20 else if v > 100 then v := 100;
  FScalePct := v;
end;

procedure TFGraphDimensions.SetXOffset(v : Word);
begin
  if v > 100 then v := 100;
  FXOffsetPct := v;
end;

procedure TFGraphDimensions.SetYOffset(v : Word);
begin
  if v > 100 then v := 100;
  FYOffsetPct := v;
end;

{ TFGraphAppearance }

procedure TFGraphAppearance.SetPlotOffGraph;
begin
  FPlotOffGraph := v;
  change;
end;

procedure TFGraphAppearance.SetLabelGraph(v : Boolean);
begin
  FLabelGraph := v;
  change;
end;

procedure TFGraphAppearance.SetTickmarks(v : Boolean);
begin
  FTickMarks := v;
  change;
end;

procedure TFGraphAppearance.SetShowMarks;
begin
  if v <> FShowMarks then
  begin
    FShowMarks := v;
    change;
  end;
end;

procedure TFGraphAppearance.SetGridcolor(v : TAlphaColor);
begin
  FGridColor := v;
  change;
end;

procedure TFGraphAppearance.SetAxescolor(v : TAlphaColor);
begin
  FAxesColor := v;
  change;
end;

procedure TFGraphAppearance.SetBkgdcolor(v : TAlphaColor);
begin
  FBkgdColor := v;
  change;
end;

procedure TFGraphAppearance.SetMarginColor(v : TAlphaColor);
begin
  FMarginColor := v;
  change;
end;

procedure TFGraphAppearance.SetGridStyle(v : TPenStyle);
begin
  FGridStyle := v;
  change;
end;

procedure TFGraphAppearance.SetErrorCaption;
begin
  FErrorCaption := v;
  change;
end;

procedure TFGraphAppearance.SetErrorFont(const Value: TFontAdapted);
begin
  FErrorFont.Assign(Value);
  change;
end;

procedure TFGraphAppearance.SetGraphTitle;
begin
  FGraphTitle := v;
  change;
end;

procedure TFGraphAppearance.SetMinSteps;
begin
  if (v > 0) and (v < FMaxSteps/2.5) then
    FMinSteps := v;
  change;
end;

procedure TFGraphAppearance.SetMaxSteps;
begin
  if (v > FMinSteps*2.5) and (v <= 90) then
    FMaxSteps := v;
  change;
end;

procedure TFGraphAppearance.SetTitleFont(v : TFontAdapted);
begin
  FTitleFont.assign(v);
  change;
end;

procedure TFGraphAppearance.SetCaptionFont(v : TFontAdapted);
begin
  FCaptionFont.assign(v);
  change;
end;

procedure TFGraphAppearance.SetLabelFont(v : TFontAdapted);
begin
  FLabelFont.assign(v);
  change;
end;

procedure TFGraphAppearance.SetCrossAtZero;
begin
  if v <> FCrossAtZero then
  begin
    FCrossAtZero := v;
    change;
  end;
end;

procedure TFGraphAppearance.SetCrossColor;
begin
  if v <> FCrossColor then
  begin
    FCrossColor := v;
    change;
  end;
end;

procedure TFGraphAppearance.SetCrossLength;
begin
  if v <> FCrossLength then
  begin
    FCrosslength := v;
    change;
  end;
end;

procedure TFGraphAppearance.SetMinPointClearance;
begin
  if v <> FMinPointClearance then
  begin
    FMinPointClearance := v;
    change;
  end;
end;

procedure TFGraphAppearance.FontChanged(Sender : TObject);
begin
  if FGraph.Fplotting {and (FGraph.FCanvas <> Printer.Canvas)} then
    FGraph.Repaint;
end;

{ TFGraphLegend }

procedure TFGraphLegend.Setvisible(v : Boolean);
begin
  if v <> FVisible then
  begin
    FVisible := v;
    change;
  end;
end;

procedure TFGraphLegend.SetAlphaColor(v : TAlphaColor);
begin
  if v <> FColor then
  begin
    FColor := v;
    change;
  end;
end;

procedure TFGraphLegend.SetborderColor(v: TAlphaColor);
begin
  if v <> FborderColor then
  begin
    FborderColor := v;
    change;
  end;
end;

procedure TFGraphLegend.SetborderStyle(v : TPenStyle);
begin
  if v <> Fborderstyle then
  begin
    FBorderstyle := v;
    change;
  end;
end;

procedure TFGraphLegend.SetLayout(v : TFGraphLegendStyle);
begin
  if v <> FLayout then
  begin
    Flayout := v;
    change;
  end;
end;

procedure TFGraphLegend.Settop(v : integer);
begin
  if v <> Ftop then
  begin
    FTop := v;
    change;
  end;
end;

procedure TFGraphLegend.Setleft(v : integer);
begin
  if v <> Fleft then
  begin
    FLeft := v;
    change;
  end;
end;

procedure TFGraphLegend.Setwidth(v : integer);
begin
  if v <> Fwidth then
  begin
    fwidth := v;
    change;
  end;
end;

procedure TFGraphLegend.Setheight(v : integer);
begin
  if v <> Fheight then
  begin
    Fheight := v;
    change;
  end;
end;

procedure TFGraphLegend.SetSymbolSpace(v : integer);
begin
  if v <> FSymbolSpace then
  begin
    FSymbolSpace := v;
    change;
  end;
end;

procedure TFGraphLegend.SetXMargin(v : integer);
begin
  if v <> FXMargin then
  begin
    FXMargin := v;
    change;
  end;
end;

procedure TFGraphLegend.SetYMargin(v : integer);
begin
  if v <> FYMargin then
  begin
    FYMargin := v;
    change;
  end;
end;

procedure TFGraphLegend.Setfont(v : TFontAdapted);
begin
  FFont.assign(v);    {procedure below hooks FFont.OnChange}
end;

procedure TFGraphLegend.Fontchanged;
begin
 if FGraph.fplotting {and (FGraph.FCanvas <> Printer.Canvas)} then
   FGraph.Repaint;
end;

{ TFGraph }

constructor TFGraph.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FSeries := TFslList<TFGraphSeries>.create;
  FSeries.OnNotify := SeriesChange;
  FAnnotations := TFslList<TFGraphAnnotation>.create;
  FAnnotations.OnNotify := AnnotationChange;
  FFunctions := TFslList<TFGraphFunction>.create;
  FFunctions.OnNotify := FunctionsChange;
  FBands := TFslList<TFGraphBand>.create;
  FBands.OnNotify := BandChange;

  FCanvas := TGraphCanvas.Create(Canvas);
  FScale := 1.0;
  FXOffset := 0;
  FYOffset := 0;
  FDragging := false;
  FLegDragging := false;
  FNoGridlines := false;
  Width := 200;
  Height := 200;
  {$IFDEF FMX}
  Fill.Color := clWhite;
  Stroke.Thickness := 0;
  {$ELSE}
  if (csDesigning in ComponentState) then
    Caption := name
  else
    Caption := '';
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsSingle;
  Font.Color := clBlack;
  Font.Height := - 9;
  Font.Name := 'Arial';
  Font.Style := [];
  Color := clWhite;
  {$ENDIF}

  FXAxis := TFGraphAxis.Create;
  FXAxis.FGraph := self;
  FYAxis := TFGraphAxis.Create;
  FYAxis.FGraph := self;
  FYAxis2 := TFGraphAxis.Create;
  FYAxis2.FGraph := self;
  FDimensions := TFGraphDimensions.Create;
  FDimensions.FGraph := self;
  FAppearance := TFGraphAppearance.Create;
  FAppearance.FGraph := self;
  with FAppearance do
  begin
    FTitleFont := TFontAdapted.Create;
    FLabelFont := TFontAdapted.Create;
    FCaptionFont := TFontAdapted.Create;
    FErrorFont := TFontAdapted.Create;
    if (FCanvas.FCanvas <> nil) then
    begin
      FLabelFont.assign(FCanvas.font);
      FTitleFont.assign(FCanvas.font);
      FCaptionFont.assign(FCanvas.font);
      FErrorFont.assign(FCanvas.font);
    end;
    FTitleFont.OnChange := FontChanged;
    FLabelFont.OnChange := FontChanged;
    FCaptionFont.OnChange := FontChanged;
    FErrorFont.OnChange := FontChanged;
  end;
  FLegend := TFGraphLegend.create;
  with FLegend do
  begin
    FGraph := self;
    Ffont := TFontAdapted.create;
//    if (FCanvas.Font <> nil) then
//      FFont.assign(FCanvas.font);
    Ffont.Onchange := fontchanged;
  end;

  FPlotting := True;
  setDefaultPropertyValues;
  FSaveData := true;
end;

function TFGraph.CreateFunction(event: TFGraphFunctionEvent): TFGraphFunction;
begin
  result := TFGraphFunction.Create;
  try
    result.OnGetValue := event;
    result.style := psDash;
    result.color := clRed;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TFGraph.SetDefaultPropertyValues;
begin
  with FXAxis do
  begin
    FSecondAxis := false;
    FShowAxis := true;
    Freversed := false;
    FMinScale := defMinXScale;
    FMin := 0;
    FMax := 1.0;
    FStep := FMax / 5;
    FLogTickInfo.LogTickCount := 2;
    InitLogTicks;
    FAutoSizing := False;
    FAutoStepping := False;
    FLogging := False;
    FAxisTitle := 'X axis';
    FGridlines := True;
    FLabelDec := 1;
    FAutoLabelDecimals := true;
    FShowASTime := false;
    FDateFormat := 'dd-mmm';
    FOffsetType := ao_Minimum;
    FOffset := 0;
  end;

  with FYAxis do
  begin
    FSecondAxis := false;
    FShowAxis := true;
    FReversed := false;
    FMinScale := defMinYScale;
    FMin := 0;
    FMax := 1.0;
    FStep := FMax / 5;
    FLogTickInfo.LogTickCount := 2;
    InitLogTicks;
    FAutoSizing := False;
    FAutoStepping := False;
    FLogging := False;
    FGridlines := True;
    FAxisTitle := 'Y axis';
    FLabelDec := 1;
    FAutoLabelDecimals := true;
    FShowASTime := false;
    FDateFormat := 'dd-mmm';
    FOffsetType := ao_Minimum;
    FOffset := 0;
  end;

  with FYAxis2 do
  begin
    FSecondAxis := true;
    FShowAxis := false;
    FReversed := true;
    FMinScale := defMinYScale;
    FMin := 0;
    FMax := 1.0;
    FStep := FMax / 5;
    FLogTickInfo.LogTickCount := 2;
    InitLogTicks;
    FAutoSizing := False;
    FAutoStepping := False;
    FLogging := False;
    FGridlines := False;
    FAxisTitle := 'Second Y axis';
    FLabelDec := 1;
    FShowASTime := false;
    FDateFormat := 'dd-mmm';
    FOffsetType := ao_Maximum;
    FOffset := 0;
  end;

  with FDimensions do
  begin
    FBottom := 40;
    FLeft := 40;
    FRight := 15;
    FTop := 30;
    FTMLength := 4;
    FGraphTitleDistance := 7;
    FXAxisTitleDistance := 4;
    FXAxisLabelDistance := 2;
    FYAxisTitleDistance := 4;
    FYAxisLabelDistance := 2;
    FScalePct := 90;
    FXOffsetPct := 5;
    FYOffsetPct := 20;
  end;

  with FAppearance do
  begin
    FAllowDuplicates := False;
    FLabelGraph := True;
    FShowMarks := False;
    FTickMarks := True;
    FPlotOffGraph := False;
    FErrorCaption := 'MAX/MIN VALUES INCONSISTENT';
    FAxesColor := clBlack;
    FGridColor := clSilver;
    FGridStyle := psdot;
    FBkgdColor := color;
    FMarginColor := color;
    FGraphTitle := 'FHIR Graph';
    FMinSteps := 5;
    FMaxSteps := 50;
    FCrossAtZero := true;
    FCrossColor := clred;
    FCrosslength := 4;
    FPrintLineStyle := true;
    FMinPointClearance := 0;
    FErrorFont.Size := 10;
  end;

  with FLegend do
    begin
    Fvisible := false;
    Fcolor := FAppearance.FBkGdcolor;
    FborderStyle := psSolid;
    FborderColor := clGray;
    FLayout := lsAcross;
    Ftop := 10;
    Fleft := 10;
    Fwidth := trunc(self.width - 20);
    Fheight := 20;
    FSymbolSpace := 20;
    FXMargin := 10;
    FYmargin := 5;
    end;
  {dragging : }
  FHasDragged := false;
  drect := rect(0,0,0,0);
end;


destructor TFGraph.Destroy;
begin
  FPlotting := false;
  with FAppearance do
  begin
    FTitleFont.Free;
    FTitleFont := nil;
    FLabelFont.Free;
    FLabelFont := nil;
    FCaptionFont.Free;
    FCaptionFont := nil;
    FErrorFont.Free;
    FErrorFont := nil;
  end;
  with FLegend do
  begin
    FFont.Free;
    FFont := nil
  end;
  FXAxis.Free;
  FYAxis.Free;
  FYAxis2.Free;
  FDimensions.free;
  FAppearance.free;
  FLegend.free;
  FCanvas.Free;
  FSeries.Free;
  FFunctions.Free;
  FAnnotations.Free;
  FBands.Free;
  inherited Destroy;
end;

{general outline of responding to mouse events :
  if it's not the left mouse button, just call the inherited handler

  if the shift button is down, the user wants to change the plot area
  if the ctrl button is down, the user wants to run the designer
  if Draggable is set to true :
     if the mouse is in the top left corner, the user wants to move the graph
     if the mouse is in the bottom right corner, the user wants to resize the graph}

{$IFNDEF FMX}
procedure TFGraph.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
const
  SC_DragMove = $f012;
  SC_SizeLR   = $f008;
begin
  if (Button = mbLeft) then
  begin
    if (ssCtrl in Shift) then
      if assigned(FOnWantDesigner) then
        FOnWantDesigner(self)
      else
    else
    if (ssShift in Shift) then
      setupdragging(x,y);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TFGraph.SetupDragging;
begin
 if (abs(x -(FLegend.Left+3))<3) and (abs(y-(FLegend.top+3))<3) and FLegend.FVisible then
   begin
   FLegDragging := true;
   dx := FLegend.width;
   dy := FLegend.height;
   end
 else
   begin
   fdragging := true;
   dx := x;
   dy := y;
   {$IFNDEF FMX}
   screen.cursor := crCross;
   {$ENDIF}
   if not FHasDragged then
     begin
     FHoldXAS := FXAxis.Autosizing;
     FHoldXmin := FXAxis.Min;
     FHoldXmax := FXAxis.Max;
     FHoldYAS := FYAxis.Autosizing;
     FHoldYmin := FYAxis.Min;
     FHoldYmax := FYAxis.Max;
     FHoldY2AS := FYAxis2.Autosizing;
     FHoldY2min := FYAxis2.Min;
     FHoldY2max := FYAxis2.Max;
     end;
   FHasDragged := true;
   end;
   {$IFNDEF FMX}
   FCanvas.penColor := Self.color;
   FCanvas.penStyle := psDot;
   FCanvas.penMode := pmXOr;
   {$ENDIF}
end;

procedure TFGraph.MouseMove(Shift : TShiftState; X, Y : Integer);
  procedure drawMyRect(r : TRect);
  begin
  FCanvas.Polyline([pt(r.left, r.top),    pt(r.right,r.top),
                    pt(r.right,r.bottom), pt(r.left, r.bottom),
                    pt(r.left, r.top)]);
  end;
begin
 if fLegDragging then
   begin
   DrawMyRect(drect);
   drect.left := x;
   drect.Top := y;
   drect.Right := x+dx;
   drect.Bottom := y+dy;
   DrawMyRect(drect);
   end else
 if fdragging then
 begin
   drawMyRect(drect);
   if x > dx then
   begin
     drect.left := dx;
     drect.right := x;
   end
   else
   begin
     drect.left := x;
     drect.right := dx;
   end;
   if y > dy then
   begin
     drect.top := dy;
     drect.bottom := y;
   end
   else
   begin
     drect.top := y;
     drect.bottom := dy;
   end;
   drawMyRect(drect);
   end
   else if (FAppearance.FCrossWire) then
   begin
     FCanvas.PenColor := clBlack;
     FCanvas.PenMode := pmNot;
     if (CVisible) then begin
       FCanvas.line(FDimensions.FLeft+FXOffset,cwy, -FDimensions.FRight+FCurrWidth+FXOffset,cwy);
       FCanvas.line(cwx,FDimensions.FTop+FYOffset, cwx,-FDimensions.FBottom+FCurrHeight+FYOffset);
       CVisible := false;
     end;
     with FDimensions do
       if (x < FCurrWidth - Fright) and (x > FLeft) and (y < FCurrHeight - FBottom) and (y > FTop) then
       begin
         FCanvas.Line(FLeft+FXOffset,Y, -FRight+FCurrWidth+FXOffset,Y);
         FCanvas.Line(X,FTop+FYOffset, X,-FBottom+FCurrHeight+FYOffset);
         cwx :=X;
         cwy :=Y;
         CVisible :=true;
       end;
     FCanvas.PenMode :=pmCopy;
   end;
   inherited mousemove(shift, x, y);
end;

procedure TFGraph.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  x1,x2,y1,y2,y3,y4 : Double;
begin
  If FLegDragging or FDragging then
  begin
    FCanvas.penColor := clBlack;
    FCanvas.penMode := pmCopy; {//pmXOr;}
    drect := rect(0,0,0,0);
    releaseCapture;
  end;
  if FLegDragging then
    begin
    FLegend.left := x;
    FLegend.top := y;
    FLegDragging := false;
    end;
  if Fdragging then
  begin
    FDragging := False;
    screen.cursor := crDefault;
    if (abs (x - dx) > MouseToleranceMove) and (abs (y - dy) > MouseToleranceMove) then
    begin
      if x > dx then
      begin
        x1 := getmousex(dx);
        x2 := getmousex(x);
      end
      else
      begin
        x1 := getmousex(x);
        x2 := getmousex(dx);
      end;
      if y < dy then
      begin
        y1 := getmousey(dy);
        y2 := getmousey(y);
        y3 := getmousey2(dy);
        y4 := getmousey2(y);
      end
      else
      begin
        y1 := getmousey(y);
        y2 := getmousey(dy);
        y3 := getmousey2(y);
        y4 := getmousey2(dy);
      end;
      plotting := false;
      {restrain zooming to 1000x original : errors start
       to become visible at extremely high magnifications}
      if abs(x1-x2) < (FHoldXMax - FHoldXMin)/1000 then
        if x1 < x2 then
          x2 := x1 + (FHoldXMax - FHoldXMin)/1000
        else
          x1 := x2 + (FHoldXMax - FHoldXMin)/1000;
      Fxaxis.min := x1;
      Fxaxis.max := x2;
      if abs(y1-y2) < (FHoldYMax - FHoldYMin)/1000 then
        if y1 < y2 then
          y2 := y1 + (FHoldYMax - FHoldYMin)/1000
        else
          y1 := y2 + (FHoldYMax - FHoldYMin)/1000;
      Fyaxis.min := y1;
      Fyaxis.max := y2;
      if abs(y3-y4) < (FHoldY2Max - FHoldY2Min)/1000 then
        if y3 < y4 then
          y4 := y3 + (FHoldY2Max - FHoldY2Min)/1000
        else
          y3 := y4 + (FHoldY2Max - FHoldY2Min)/1000;
      Fyaxis2.min := y3;
      FYAxis2.max := y4;
      plotting := true;
    end
    else
    begin
      FHasDragged := false;
      plotting := false;
      FXaxis.autosizing := FHoldXAS;
      if not FHoldXAS then
      begin
        FXAxis.min := FHoldXMin;
        FXAxis.max := FHoldXMax;
      end;
      FYaxis.autosizing := FHoldYAS;
      if not FHoldYAS then
      begin
        FYAxis.min := FHoldYMin;
        FYAxis.max := FHoldYMax;
      end;
      FYaxis2.autosizing := FHoldY2AS;
      if not FHoldY2AS then
      begin
        FYAxis2.min := FHoldY2Min;
        FYAxis2.max := FHoldY2Max;
      end;
      plotting := true;
    end;
  end;
  inherited mouseup(button, shift, x, y);
end;

{$ENDIF}
{ getmousex/y return the value as determined by the scales }
function TFGraph.GetMouseX(x : Integer) : Double;
begin
  with FXAxis do
    if FLogging then
      Result := FMin*Exp((x - FDimensions.FLeft) / FM)
    else
      Result := FMin + ((x - FDimensions.FLeft) / FM);
end;

function TFGraph.GetMouseY(y : Integer) : Double;
begin
  with FYAxis do
    if FLogging then
      Result := FMax*Exp(-(y - FDimensions.FTop) / FM)
    else
      Result := FMax - ((y - FDimensions.FTop) / FM);
end;

function TFGraph.GetMouseY2(y : Integer) : Double;
begin
  with FYAxis2 do
    if FLogging then
      Result := FMax*Exp(-(y - FDimensions.FTop) / FM)
    else
      Result := FMax - ((y - FDimensions.FTop) / FM);
end;


function TFGraph.GetYAxis2(which: boolean): TFGraphAxis;
begin
  if which then
    result := FYAxis2
  else
    result := FYAxis;
end;

{FindNearestPoint - get the handle to the nearest point to the
                    mouse.
  x,y : mouse position.
  series : a series to search, or 0 for all series.
  returns the series in series (hence var pars)
FindNearestPoint finds the point nearest _visually_. (if scales
are logged, it may not be the point closest to the data value
of x,y)

comment : Yes, I know, this is a slow way to do it. You could probably
think of faster ways. But this is enough for me.}

function TFGraph.findNearestPoint(x,y : integer; var series : TFGraphSeries; var point : TFGraphDataPoint) : boolean;
//var
//current,currs : longint;
//    pp,currpp : TFGraphDataPoint;
//    p : TFGraphSeries;
var
  found : boolean;
  dist, min : integer;
  s, sr : TFGraphSeries;
  p : TFGraphDataPoint;
//     function sqr(w : longint) : longint;
//     begin
//       result := w*w;
//     end;
begin
  found := false;
  min := MaxInt;
  sr := nil;
  for s in FSeries do
  begin
    s.Data.iterate(procedure (pp : TFGraphDataPoint) begin
      if (pp.error = '') then
      begin
        dist := sqr(fx(pp.x)-x) + sqr(Fy(pp.y, s.FYAxis2) - y);
        if (dist < min) then
        begin
          sr := s;
          p := pp;
          min := dist;
          found := true;
        end;
      end;
    end);
  end;
  result := found;
  series := sr;
  point := p;
end;

procedure TFGraph.FunctionsChange(Sender: TObject; const Item: TFGraphFunction; Action: TCollectionNotification);
begin
  if Action = cnAdded then
    item.FGraph := self;
  if FPlotting then
    RePaint;
end;

procedure TFGraph.DoRescaleEvent;
begin
  if FPlotting and Assigned(FOnRescale) then
    FOnRescale(self);
end;

procedure TFGraph.addBand(color: TAlphaColor; min, max, opacity: Double);
var
  bnd : TFGraphBand;
begin
  bnd := TFGraphBand.Create;
  try
    bnd.color := color;
    bnd.Min := min;
    bnd.Max := max;
    bnd.opacity := opacity;
    Bands.Add(bnd.link);
  finally
    bnd.Free;
  end;
end;

procedure TFGraph.AnnotationChange(Sender: TObject; const Item: TFGraphAnnotation; Action: TCollectionNotification);
begin
  if Action = cnAdded then
    item.FGraph := self;
  if FPlotting then
    Repaint;
end;

procedure TFGraph.BandChange(Sender: TObject; const Item: TFGraphBand; Action: TCollectionNotification);
begin
  if Action = cnAdded then
    item.FGraph := self;
  if FPlotting then
    Repaint;
end;

procedure TFGraph.CalcMetrics;
begin
  if FXaxis.FScaledOk then FXAxis.CalcMetrics;
  if FYAxis.FScaledOk then FYAxis.CalcMetrics;
  if FYAxis2.FScaledOk then FYAxis2.CalcMetrics;
end;

function TFGraph.fx(v : Double) : Integer;
var w : Double;
begin
{  if not FXAxis.FScaledOk then raise EFslException.create('call to fx when x fails');}
  with FXAxis do
    if FLogging then
      w := (Ln(v) - Ln(FMin)) * FM
    else
      w :=(v - FMin) * FM;
  // fix thanks to Wolfgang Gross <ce4@ix.urz.uni-heidelberg.de>
  if abs(w) > 20000 then
    if w < 0 then
      Result := -20000
    else
      Result := 20000
  else
    Result := Round(w) + FDimensions.FLeft + FXOffset;
{20000  is large compared with device pixel size; this avoids range
 error if some points are far outside the axis range}
end;

function TFGraph.fy(v : Double; a2 : boolean) : Integer;
var w : Double;
begin
  with YAxis[a2] do
    if FLogging then
      w := (Ln(FMax) - Ln(v)) * FM
    else
      w := (FMax - v) * FM;
  if abs(w) > 20000 then
    if w < 0 then
      Result := -20000
    else
      Result := 20000
  else
    Result := Round(w) + FDimensions.FTop + FYOffset;
end;

procedure TFGraph.DoTightFit;
var
  initx, inity, inity2 : Boolean;
  p : TFGraphSeries;
begin
  initx := true;
  inity := true;
  inity2 := true;

  for p in FSeries do
  begin
    if p.Active then
    begin
      if initx then
      begin
        FXAxis.FOMin := p.Data.getMinXValue;
        FXAxis.FOMax := p.Data.getMaxXValue;
        initx := false;
      end
      else
      begin
        if p.Data.getMinXValue < FXAxis.FOMin then
          FXAxis.FOMin := p.Data.getMinXValue;
        if p.Data.getMaxXValue > FXAxis.FOMax then
          FXAxis.FOMax := p.Data.getMaxXValue;
      end;
      if not p.YAxis2 then
        begin
        if inity then
          begin
          FYAxis.FOMin := p.Data.getMinYValue;
          FYAxis.FOMax := p.Data.getMaxYValue;
          inity := false
          end
        else
          begin
          if p.Data.getMinYValue < FYAxis.FOMin then
            FYAxis.FOMin := p.Data.getMinYValue;
          if p.Data.getMaxYValue > FYAxis.FOMax then
            FYAxis.FOMax := p.Data.getMaxYValue;
          end;
        end
      else
        begin
        if inity2 then
          begin
          FYAxis2.FOMin := p.Data.getMinYValue;
          FYAxis2.FOMax := p.Data.getMaxYValue;
          inity2 := false
          end
        else
          begin
          if p.Data.getMinYValue < FYAxis2.FOMin then
            FYAxis2.FOMin := p.Data.getMinYValue;
          if p.Data.getMaxYValue > FYAxis2.FOMax then
            FYAxis2.FOMax := p.Data.getMaxYValue;
          end;
        end
    end;
  end;
end;

(*function TFGraph.DoResize : Boolean;
var b : Boolean;
begin
  b := FYAxis.DoResize;    {force evaluation of both functions - need side effects!}
  b := FYAxis2.DoResize or b;
  Result := FXAxis.DoResize or b;
end;*)

procedure TFGraph.ClipGraph;
begin
  FCanvas.Clip(FDimensions.FLeft + FXOffset,
                            FDimensions.FTop + FYOffset,
                            FCurrWidth - FDimensions.FRight + 1 + FXOffset,
                            FCurrHeight - FDimensions.FBottom + 1 + FYOffset);
end;

procedure TFGraph.UnclipGraph;
begin
  { note for confused readers : the shortcut to unclip that is valid for
       screen metrics is not valid for printers : ) }
   FCanvas.UnClip(FXOffset,   FYOffset,
                FCurrWidth + 1 + FXOffset, FCurrHeight + 1 + FYOffset);
end;

procedure TFGraph.DrawXGridlines;
var
  tick, maxTick : Double;
  tx1, ty1, ty2 : Word;
  b : Boolean;
  tempLogTickInfo : TFGraphLogTickInfo;
begin
  FCanvas.PenColor := FAppearance.FGridColor;
  FCanvas.PenStyle := FAppearance.FGridStyle;
  FCanvas.BrushColor := Color;
  with FXAxis do
    maxTick := FMax + 0.001*(FMax-FMin); { rounding errors might exclude last point }
  with FYAxis do
  begin
    ty1 := fy(FMin, false);
    ty2 := fy(FMax, false);
  end;
  with FXAxis do
  begin
    if FLogging then tempLogTickInfo := FLogTickInfo;
    tick := GetFirstTick(tempLogTickInfo);
   {don't draw the first grid line on top of axis : }
    if tick < FMin + 0.01*FStep then
      tick := GetNextTick(tick, tempLogTickInfo, b);
    while tick < maxTick do
    begin
      tx1 := fx(tick);
      FCanvas.Line(tx1, ty1, tx1, ty2);
      tick := GetNextTick(tick, tempLogTickInfo, b);
    end;
  end;
end;

procedure TFGraph.DrawYGridlines(CYAxis : TFGraphAxis);
var
  tick, maxTick : Double;
  tx1, tx2, ty1 : Word;
  b : Boolean;
  tempLogTickInfo : TFGraphLogTickInfo;
begin
  FCanvas.PenColor := FAppearance.FGridColor;
  FCanvas.PenStyle := FAppearance.FGridStyle;
  with cYAxis do
    maxTick := FMax + 0.001*(FMax-FMin); { rounding errors might exclude last point }
  with FXAxis do
  begin
    tx1 := fx(FMin);
    tx2 := fx(FMax);
  end;
  with cYAxis do
  begin
    if FLogging then tempLogTickInfo := FLogTickInfo;
    tick := GetFirstTick(tempLogTickInfo);
   {don't draw the first grid line on top of axis : }
    if tick < FMin + 0.01*FStep then
      tick := GetNextTick(tick, tempLogTickInfo, b);
    while tick < maxTick do
    begin
      ty1 := fy(tick, CYAxis.FSecondAxis);
      FCanvas.Line(tx1, ty1, tx2, ty1);
      tick := GetNextTick(tick, tempLogTickInfo, b);
    end;
  end;
end;

procedure TFGraph.DrawXTickMarks;
var
  tick, maxTick : Double;
  tx, ty1, ty2 : {Word} longint;
  b : Boolean;
  tempLogTickInfo : TFGraphLogTickInfo;
begin
  FCanvas.PenColor := FAppearance.FAxesColor;
  FCanvas.PenStyle := psSolid;
  with FXAxis do
    maxTick := FMax + 0.001*(FMax-FMin);
  ty1 := base;
  if reverse then
    ty2 := ty1 - FDimensions.FTMLength
  else
    ty2 := ty1 + FDimensions.FTMLength;
  with FXAxis do
  begin
    if FLogging then tempLogTickInfo := FLogTickInfo;
    tick := GetFirstTick(tempLogTickInfo);
    while tick < maxTick do
    begin
      tx := fx(tick);
      FCanvas.Line(tx, ty1, tx, ty2);
      tick := GetNextTick(tick, tempLogTickInfo, b);
    end;
  end;
end;

procedure TFGraph.DrawYTickMarks(CYAxis : TFGraphAxis; base : integer; reverse : boolean);
var
  tick, maxTick : Double;
  tx1, tx2, ty : {Word} longint;
  b : Boolean;
  tempLogTickInfo : TFGraphLogTickInfo;
begin
  FCanvas.PenColor := FAppearance.FAxesColor;
  FCanvas.PenStyle := psSolid;
  with cYAxis do
    maxTick := FMax + 0.001*(FMax-FMin);
  tx1 := base;
  if reverse then
    tx2 := tx1 + FDimensions.FTMLength
  else
    tx2 := tx1 - FDimensions.FTMLength;
  with cYAxis do
  begin
    if FLogging then tempLogTickInfo := FLogTickInfo;
    tick := GetFirstTick(tempLogTickInfo);
    while tick < maxTick do
    begin
      ty := fy(tick, CYAxis.FSecondAxis);
      FCanvas.Line(tx1, ty, tx2, ty);
      tick := GetNextTick(tick, tempLogTickInfo, b);
    end;
  end;
end;

function  TFGraphAxis.LabelString(tick : Double) : string;
begin
  if not FShowAsTime then
    if (abs(tick) < (FMax - FMin)*0.001 {zero}) or
        ((abs(tick) > 0.000999) and (abs(tick) < 9999)) then
      Result := FloatToStrF(tick, ffFixed, 5, FLabelDec)
    else {very small or very large}
      Result := FloatToStrF(tick, ffExponent, 2, 0)
  else
   result := FormatDateTime(FDateFormat,tick);
end;

function TFGraph.DrawXLabels;
var
  tick, maxTick : Double;
  ty : longint;
  lblStr : string;
  drawIt : Boolean;
  tempLogTickInfo : TFGraphLogTickInfo;
  {$IFNDEF FASTDRAW}
  lblStrLastDrawTick : string;
  LastDrawTick  : Double;
  {$ENDIF}
begin
{ X-axis labels }
  FCanvas.Font := FAppearance.FLabelFont;
  FCanvas.Font := FAppearance.FLabelFont; {if FIsMetafiling FCanvas <> FCanvas}
  FCanvas.BrushStyle := bsClear;
  with FXAxis do
    maxTick := FMax + 0.001*(FMax-FMin);   { rounding errors might exclude last point }
  if reverse then
    ty := base - (FDimensions.FTMLength + FDimensions.FXAxisLabelDistance)
  else
    ty := base + FDimensions.FTMLength + FDimensions.FXAxisLabelDistance;
  with FXAxis do
  begin
    if FLogging then tempLogTickInfo := FLogTickInfo;
    tick := GetFirstTick(tempLogTickInfo);
    {$IFNDEF FASTDRAW}
    LastDrawTick  := tick;
    lblStrLastDrawTick := '';
    {$ENDIF}
    drawIt := true;
    while tick < maxTick do
    begin
      lblStr := LabelString(tick);
      {$IFNDEF FASTDRAW}
      if (tick <> LastDrawTick) then
        begin
        lblStrLastDrawTick := LabelString(LastDrawTick);
        if (fx(LastDrawTick) + FCanvas.TextWidth(lblStrLastDrawTick) div 2)
           >= (fx(tick) - FCanvas.TextWidth(lblStr) div 2) then
          DrawIt := False
        else
          LastDrawTick := tick;
        end;
      {$ENDIF}
      if drawIt or (maxTick - tick < FStep) {ie, last one} then
        if reversed then
          FCanvas.TextOut(fx(tick) - FCanvas.TextWidth(lblStr) div 2,
                        ty - FCanvas.TextHeight(lblstr),
                        lblStr)
        else
          FCanvas.TextOut(fx(tick) - FCanvas.TextWidth(lblStr) div 2,
                        ty,
                        lblStr);
      tick := GetNextTick(tick, tempLogTickInfo, drawIt);
    end;
  end;
  result := FCanvas.Textheight(lblstr);
end;

function TFGraph.DrawYLabels;
var
  tick, maxTick : Double;
  tx, ty : Integer;
  lblStr : string;
  drawIt : Boolean;
  tempLogTickInfo : TFGraphLogTickInfo;
begin
{ Y-axis Labels }
  FCanvas.Font := FAppearance.FLabelFont;
  FCanvas.Font := FAppearance.FLabelFont; {if FIsMetafiling FCanvas <> FCanvas}
  with cYAxis do
    maxTick := FMax + 0.001*(FMax-FMin);   { rounding errors might exclude last point }
  if reverse then
    tx := base + (FDimensions.FTMLength + FDimensions.FYAxisLabelDistance)
  else
    tx := base - (FDimensions.FTMLength + FDimensions.FYAxisLabelDistance);
  with cYAxis do
  begin
    if FLogging then tempLogTickInfo := FLogTickInfo;
    tick := GetFirstTick(tempLogTickInfo);
    drawIt := true;
    while tick < maxTick do
    begin
      lblStr := LabelString(tick);
      ty := fy(tick, cYAxis.FSecondAxis);
      if drawIt or (maxTick - tick < FStep) then
        if reverse then
          FCanvas.TextOut(tx,
                        ty - FCanvas.TextHeight(lblStr) div 2,
                        lblStr)
        else
          FCanvas.TextOut(tx - FCanvas.TextWidth(lblStr),
                        ty - FCanvas.TextHeight(lblStr) div 2,
                        lblStr);
      tick := GetNextTick(tick, tempLogTickInfo, drawIt);
    end;
  end;
  result := FCanvas.TextWidth(lblstr);
end;

procedure TFGraph.DrawGraphTitle;
var
  tx : Word;
begin
  FCanvas.Font.Assign(FAppearance.FCaptionFont);

          {if FIsMetafiling FCanvas <> FCanvas}
  with FDimensions, FAppearance do
  begin
    tx := FLeft + (FCurrWidth - FLeft - FRight) div 2 + FXOffset;
    with FCanvas do
    begin
      TextOut(tx - TextWidth(FGraphTitle) div 2,
              FTop - FGraphTitleDistance - TextHeight(FGraphTitle) + FYOffset,
              FGraphTitle);
    end;
  end;
end;

procedure TFGraph.DrawAnnotations;
var
  p : TFGraphAnnotation;
begin
  if not FAppearance.FPlotOffGraph then
    ClipGraph;
  for p in FAnnotations do
  begin
    if p.DrawTime = time then
      PaintAnnotation(p);
  end;
 if not FAppearance.FPlotOffGraph then
   UnclipGraph;
end;


procedure TFGraph.PaintAnnotation(annotation : TFGraphAnnotation);
begin
  if annotation is TFGraphMark then
    case (annotation as TFGraphMark).FMarkType of
      mtXMark : PaintXMark(annotation as TFGraphMark);
      mtYMark : PaintYMark(annotation as TFGraphMark);
      mtPoint : PaintBoxMark(annotation as TFGraphMark);
      mtLine : PaintLineMark(annotation as TFGraphMark);
    end;
end;


procedure TFGraph.PaintXMark(mark : TFGraphMark);
begin
  Fcanvas.pencolor := mark.fcolor;
  FCanvas.Font := Appearance.CaptionFont;
  FCanvas.FonTAlphaColor := mark.FColor;
  FCanvas.TextOut(fx(mark.fx1),fy(FYAxis.FMin, false) - 3, 0, trunc(FCanvas.font.size + 5), mark.FCaption, pi/2);
  FCanvas.Line(fx(mark.fx1), fy(FYAxis.FMin, false), fx(mark.fx1), fy(FYAxis.FMax, false));
end;

procedure TFGraph.PaintYMark(mark : TFGraphMark);
var
  xPos, yPos : integer;
begin
  FCanvas.Font := FAppearance.FCaptionFont;
  FCanvas.fonTAlphaColor := mark.fcolor;
  case mark.FMarkPos of
    mpUpLeft :
      begin
      xpos := FDimensions.FLeft + Round(FScale*10) + FXOffset;
      ypos := fy(mark.fy1, false) - Round(FScale*2) - FCanvas.TextHeight(mark.fcaption);
      end;
   mpUpRight :
      begin
      xpos := FCurrWidth-FXOffSet-FDimensions.FRight- FCanvas.TextWidth(mark.fcaption)-Round(FScale*10);
      ypos := fy(mark.fy1, false) - Round(FScale*2) - FCanvas.TextHeight(mark.fcaption);
      end;
   mpDownLeft :
      begin
      xpos := FDimensions.FLeft + Round(FScale*10) + FXOffset;
      ypos := fy(mark.fy1, false) + Round(FScale*2);
      end;
   mpDownRight :
      begin
      xpos := FCurrWidth-FXOffSet-FDimensions.FRight- FCanvas.TextWidth(mark.fcaption)-Round(FScale*10);
      ypos := fy(mark.fy1, false) + Round(FScale*2);
      end;
   else
      begin
      xpos := 0;
      ypos := 0;
      end
  end;
  FCanvas.TextOut(xpos, ypos, mark.fcaption);
  Fcanvas.pencolor := mark.fcolor;
  FCanvas.Line(fx(FXAxis.Fmin), fy(mark.fy1, false), fx(FXAxis.Fmax), fy(mark.fy1, false));
end;

procedure TFGraph.PaintBand(band: TFGraphBand);
begin
  FCanvas.BrushStyle := bsSolid;
  FCanvas.BrushColor := band.color;
  FCanvas.Rectangle(fx(XAxis.FMin), fy(band.Min, band.YAxis2), fx(XAxis.FMax), fy(band.Max, band.YAxis2), band.opacity);
end;

procedure TFGraph.PaintBoxMark(mark : TFGraphMark);
var
  d : integer;
begin
  d := trunc(FScale*mark.fx2);
  FCanvas.pencolor := mark.color;
  FCanvas.brushstyle := bsClear;
  FCanvas.rectangle(fx(mark.fx1) - d, fy(mark.fy1, false) - d, fx(mark.fx1) + d, fy(mark.fy1, false) + d);
  FCanvas.fonTAlphaColor := mark.color;
  FCanvas.TextOut(fx(mark.fx1) - d, fy(mark.fy1, false) + d, mark.caption);
end;

procedure TFGraph.PaintLineMark(mark : TFGraphMark);
var
  minx,miny,maxy,maxx : integer;
  angle : double;
begin
  FCanvas.pencolor := mark.fcolor;
  minx := fx(mark.fx1);
  miny := fy(mark.fy1, false);
  maxx := fx(mark.fx2);
  maxy := fy(mark.fy2, false);
  if maxx < minx then
    begin
    swap(minx,maxx);
    swap(miny,maxy);
    end;
  if maxx - minx = 0 then
    angle := pi/2
  else
    angle := - arctan((maxy-miny)/(maxx-minx));
  FCanvas.Font := Appearance.CaptionFont;
  FCanvas.FonTAlphaColor := mark.FColor;
  FCanvas.TextOut(minx, miny, 0, FCanvas.TextHeight(mark.FCaption) + 5, mark.FCaption, angle);
  FCanvas.Line(minx, miny, maxx, maxy);
end;


procedure TFGraph.ColorBackground;
begin
  {$IFNDEF FMX}
  FCanvas.brushcolor := FAppearance.FMarginColor;
  FCanvas.brushstyle := bsSolid;
  FCanvas.pencolor := FAppearance.FMarginColor;
  FCanvas.penstyle := psSolid;
  With FDimensions do
    FCanvas.rectangle(0 + FXOffset, 0 + FYOffset, FCurrWidth + FXOffset, FCurrHeight + FYOffSet);
  FCanvas.brushcolor := FAppearance.FBkgdColor;
  FCanvas.brushstyle := bsSolid;
  FCanvas.pencolor := FAppearance.FBkgdColor;
  FCanvas.penstyle := psSolid;
  With FDimensions do
   FCanvas.rectangle(Fleft + FXOffset, Ftop + FYOffSet, FCurrWidth - (FRight) + FXOffset, FCurrHeight - (FBottom) + FYOffSet);
     {same as (fx(FXAxis.FMin), fy(FYAxis.FMax), fx(FXAxis.FMax),
      fy(FYAxis.FMin)) , except that fx/fy may not be available}                     ;
   { FCanvas.brush.color := Color;}
  {$ENDIF}
end;

procedure TFGraph.DrawXAxis;
var
  holdXStep : Double;
  ypoint,tx,ty,ts, oldPenWidth : integer;
  s : string;
begin
 ts := 0;
 with FAppearance, FXAxis do
  begin
  if FYAxis.FScaledOk then
    case FOffsetType of
     ao_Minimum : ypoint := fy(FYAxis.FMin, false);
     ao_Maximum : ypoint := fy(FYAxis.FMax, false);
     ao_percentage : ypoint := fy(FYAxis.FMin +
                (FOffset / 100 * (FYAxis.FMax - FYAxis.FMin)), false);
     ao_absolute : ypoint := fy(FOffset, false);
     else ypoint := fy(FYAxis.FMin, false);
    end
  else
    with FDimensions do ypoint := FCurrHeight - (FBottom) + FYOffSet;
  if FScaledOk then
    begin
    holdXStep := FStep;
    {if the number of steps is ridiculously large, do something reasonable : }
    if FShowAsTime then
      begin
       FStep := GetDatestep(FDateTickType, FGraph.FAppearance.MinSteps, FMax, FMin);
       while ((FMax - FMin) / FStep > FMaxSteps) do
        begin
        inc(FDateTickType); {this can't finish up in an infinite loop
           because the last datetick type will always terminate the loop}
        FStep := GetDatestep(FDateTickType, FGraph.FAppearance.MinSteps, FMax, FMin);
        SetDateMinMax;
        {FGraph.}Calcmetrics;
        end;
      end else
    if not FLogging and ((FStep = 0) or ((FMax - FMin) / FStep > FMaxSteps)) then
      FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin);
    if FLogging then InitLogTicks;
    OldPenWidth := FCanvas.PenWidth;
    if OldPenWidth <> 1 then
      if FAppearance.FPrintLineStyle then
          FCanvas.PenWidth := 1 else
       if OldPenWidth > 3 then FCanvas.PenWidth := oldpenwidth div 2 + 1;
    if not FNoGridlines and FGridlines and FYAxis.FScaledOk then DrawXGridlines;
    if not FShowAxis then
      begin
      FCanvas.PenWidth := OldPenWidth;
      exit;
      end;
    if FTickMarks then DrawXTickMarks(ypoint,Freversed);
    if FLabelGraph then ts := DrawXLabels(ypoint,Freversed);
    FStep := holdXStep;
    FCanvas.PenColor := FAxesColor;
    FCanvas.PenStyle := psSolid;
    FCanvas.Line(fx(FXAxis.FMin), ypoint, fx(FXAxis.FMax), ypoint);
    s := FXAxis.Title;
    end
  else s := ErrorCaption;
  if not FShowAxis then exit;
  FCanvas.Font := FAppearance.FTitleFont;
  FCanvas.Font := FAppearance.FTitleFont; {if FIsMetafiling FCanvas <> FCanvas}
  with FDimensions do
    begin
    tx := FLeft + (FCurrWidth - FLeft - FRight) div 2 + FXOffset;
    if FXAxis.freversed then
      ty := ypoint - (FTMLength + FXAxisTitleDistance)
               - FCanvas.TextHeight(s) - ts
    else
      ty := ypoint + FTMLength + FXAxisTitleDistance +
          ts {FCanvas.TextHeight(FXAxis.Title)};
    end;
  FCanvas.TextOut(tx - FCanvas.TextWidth(FXAxis.Title) div 2,
                  ty, s);
  end;
end;

procedure TFGraph.DrawYAxis;
var
  holdYStep : Double;
  tx,ty,xpoint,ts, oldPenWidth : integer;
  Angle : integer;
  s : string;
begin
  ts := 0;
  with FAppearance, FYAxis do
  begin
    if FXAxis.FScaledOk then
      case FOffsetType of
       ao_Minimum : xpoint := fx(FXAxis.FMin);
       ao_Maximum : xpoint := fx(FXAxis.FMax);
       ao_percentage : xpoint := fx(FXAxis.FMin +
                  (FOffset / 100 * (FXAxis.FMax - FXAxis.FMin)));
       ao_absolute : xpoint := fx(FOffset);
       else xpoint :=  fx(FXAxis.FMin);
      end
    else
      with FDimensions do xpoint := FLeft + FXOffSet;
    if FScaledOk then
      begin
      holdYStep := FStep;
      if FShowAsTime then
        begin
         FStep := GetDatestep(FDateTickType, FGraph.FAppearance.MinSteps, FMax, FMin);
         while ((FMax - FMin) / FStep > FMaxSteps) do
          begin
          inc(FDateTickType);  {this can't finsh up in an infinite loop
             because the last datetick type will always terminate the loop}
          FStep := GetDatestep(FDateTickType, FGraph.FAppearance.MinSteps, FMax, FMin);
          SetDateMinMax;
          {FGraph.}Calcmetrics;
          end;
        end else
      if not FLogging and ((FStep = 0) or ((FMax - FMin) / FStep > FMaxSteps)) then
        FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin);
      if FLogging then FYAxis.InitLogTicks;
      OldPenWidth := FCanvas.PenWidth;
      if OldPenWidth <> 1 then
        if FAppearance.FPrintLineStyle then FCanvas.PenWidth := 1 else
         if OldPenWidth > 3 then FCanvas.PenWidth := oldpenwidth div 2 + 1;
      if not FNoGridlines and FGridlines and FXAxis.FScaledOk then DrawYGridlines(FYAxis);
      if not FShowAxis then
        begin
        FCanvas.PenWidth := OldPenWidth;
        exit;
        end;
      if FTickMarks then DrawYTickMarks(FYAxis, xpoint,Freversed);
      if FLabelGraph then ts := DrawYLabels(FYAxis, xpoint,Freversed);
      FStep := holdYStep;
      FCanvas.PenColor := FAxesColor;
      FCanvas.PenStyle := psSolid;
      FCanvas.Line(xpoint, fy(FYAxis.FMax, false), xpoint, fy(FYAxis.FMin, false));
      s := FYAxis.Title;
      end
    else s := ErrorCaption;
  {Y-axis title}
    if not FShowAxis then exit;
    FCanvas.Font := FAppearance.FTitleFont;
    FCanvas.Font := FAppearance.FTitleFont; {if FIsMetafiling FCanvas <> FCanvas}
    Angle := 90;
    with FDimensions do
    begin
      if Freversed then
        tx := xpoint + (FTMLength + FYAxisTitleDistance)
      else
        tx := xpoint - (FTMLength + FYAxisTitleDistance);
      ty := FTop + (FCurrHeight - FTop - FBottom) div 2 + FYOffset;
    end;
    with FCanvas do
    begin
      if Freversed then
        tx := tx + ts
      else
        tx := tx - ts - TextHeight(s);
      if tx < 0 then tx := 0;
      TextOut(tx, ty + (TextWidth(s) div 2), 0, 0, s, angle/180 * pi);
    end;
  end;
end;

procedure TFGraph.DrawY2Axis;
var
  holdYStep : Double;
  tx,ty,xpoint,ts, OldPenWidth : integer;
  Angle : integer;
  s : string;
begin
 ts := 0;
 with FAppearance, FYAxis2 do
  begin
  if FXAxis.FScaledOk then
    case FOffsetType of
       ao_Minimum : xpoint := fx(FXAxis.FMin);
       ao_Maximum : xpoint := fx(FXAxis.FMax);
       ao_percentage : xpoint := fx(FXAxis.FMin +
                  (FOffset / 100 * (FXAxis.FMax - FXAxis.FMin)));
       ao_absolute : xpoint := fx(FOffset);
     else xpoint := 0;
    end
  else
    with FDimensions do xpoint := FCurrWidth - (FRight) + FXOffSet;
  if FScaledOk then
    begin
    holdYStep := FStep;
    if FShowAsTime then
      begin
       FStep := GetDatestep(FDateTickType, FGraph.FAppearance.MinSteps, FMax, FMin);
       while ((FMax - FMin) / FStep > FMaxSteps) do
        begin
        inc(FDateTickType);  {this can't finish up in an infinite loop
           because the last datetick type will always terminate the loop}
        FStep := GetDatestep(FDateTickType, FGraph.FAppearance.MinSteps, FMax, FMin);
        SetDateMinMax;
        {FGraph.}Calcmetrics;
        end;
      end else
    if not FLogging and ((FStep = 0) or ((FMax - FMin) / FStep > FMaxSteps)) then
      FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin);
    if FLogging then InitLogTicks;
    OldPenWidth := FCanvas.PenWidth;
    if OldPenWidth <> 1 then
      if FAppearance.FPrintLineStyle then FCanvas.PenWidth := 1 else
       if OldPenWidth > 3 then FCanvas.PenWidth := oldpenwidth div 2 + 1;
    if not FNoGridlines and FGridlines and FXAxis.FScaledOk then DrawYGridlines(FYAxis2);
    if not FShowAxis then
      begin
      FCanvas.PenWidth := OldPenWidth;
      exit;
      end;
    if FTickMarks then DrawYTickMarks(FYAxis2, xpoint,Freversed);
    if FLabelGraph then
      ts := DrawYLabels(FYAxis2, xpoint,Freversed);
    FStep := holdYStep;
    FCanvas.PenColor := FAxesColor;
    FCanvas.PenStyle := psSolid;
    FCanvas.Line(xpoint, fy(FYAxis2.FMax, true), xpoint, fy(FYAxis2.FMin, true));
    s := Title;
    end
  else s := ErrorCaption;
{Y2-axis title}
  if not FShowAxis then exit;
  FCanvas.Font := FAppearance.FTitleFont;
  FCanvas.Font := FAppearance.FTitleFont; {if FIsMetafiling FCanvas <> FCanvas}
  Angle := 90;
  with FDimensions do
  begin
    if Freversed then
      tx := xpoint + (FTMLength + FYAxisTitleDistance)
    else
      tx := xpoint - (FTMLength + FYAxisTitleDistance);
    ty := FTop + (FCurrHeight - FTop - FBottom) div 2 + FYOffset;
  end;
  with FCanvas do
  begin
    if Freversed then
      tx := tx + ts
    else
      tx := tx - ts - TextHeight(s);
    if tx < 0 then tx := 0;
    TextOut(tx, ty + (TextWidth(s) div 2), 0, 0, s, angle/180 * pi);
  end;
  end;
end;

procedure TFGraph.DrawAxes;
var
  bnd : TFGraphBand;
begin
  {if (FCanvas <> Printer.Canvas) or FAppearance.FBkgdWhenPrint then}
  ColorBackground;
  for bnd in FBands do
    PaintBand(bnd);
  if FAppearance.FShowMarks then
    DrawAnnotations(dtBefore);
  DrawXAxis;
  DrawYAxis;
  DrawY2Axis;
  if FAppearance.FLabelGraph then
    DrawGraphTitle;
end;

procedure TFGraph.PaintCross;
var
  crslen : word;
begin
  if not FAppearance.FPlotOffGraph and ((FXAxis.Min > 0) or (FXAxis.Max < 0) or (FYAxis.Min > 0) or (FYAxis.Max < 0)) then
    exit;
  crslen := round(Fappearance.FCrosslength*Fscale);
  FCanvas.PenColor := FAppearance.FCrossColor;
  FCanvas.Line(fx(0), fy(0, false) + crslen, fx(0), fy(0, false) - Crslen - 1);
  FCanvas.Line(fx(0) - Crslen, fy(0, false), fx(0) + Crslen + 1, fy(0, false))
end;

procedure TFGraph.PaintFunction(f: TFGraphFunction);
const
  STEP_COUNT = 1000;
var
  min, max, x, y : Double;
  i, c : integer;
  lp: pLinePoints;
begin
  if f.FBounded then
  begin
    min := f.xMin;
    max := f.xMin;
  end
  else
  begin
    min := XAxis.FMin;
    max := XAxis.FMax;
  end;
  FCanvas.PenColor := f.color;
  FCanvas.PenStyle := f.style;
  if not Appearance.PlotOffGraph then
    ClipGraph;
  GetMem(lp, (STEP_COUNT+1) * sizeof(TPoint));
  try
    c := 0;
    for i := 0 to STEP_COUNT do
    begin
      x := min + (max - min) * (i/STEP_COUNT);
      try
        y := f.OnGetValue(self, x);
        lp^[c].x := fx(x);
        lp^[c].y := fy(y, false);
        inc(c);
      except
        // draw what we have
        if (c > 1) then
          FCanvas.Polyline(lp, c);
        c := 0;
      end;
    end;
    if (c > 0) then
      FCanvas.Polyline(lp, c);
  finally
    Freemem(lp);
  end;
  if not Appearance.PlotOffGraph then
    UnclipGraph;
end;

procedure TFGraph.DrawLegend;
var
  EntryCount,i : word;
  p : TFGraphSeries;
  xbase,ybase,xwid,yhei,xleft,ytop,xoffset,yoffset,th,symspace, w, h, t : integer;
  OldPenWidth : word;

  procedure DrawLegendEntry(SeriesName : string; LegendStatus : TFGraphLegendStatus; LineColor, PointColor : TAlphaColor; DrawLine, DrawPoints, FillPoints : boolean;
      Pointshape : TFGraphPointShape; Linestyle : TPenstyle; Psize, x, y : integer);
  begin
    FCanvas.BrushStyle := bsClear;
    FCanvas.Textout(x+SymSpace+2,y,SeriesName);
    y := y+th div 2;
    if Drawline and ((LegendStatus = lsAll) or (legendstatus = lslines)) then
      with FCanvas do
      begin
        PenStyle := Linestyle;
        pencolor := LineColor;
        Line(x,y,x+SymSpace,y);
      end;
    if Drawpoints and ((LegendStatus = lsAll) or (legendstatus = lspoints)) then
      with FCanvas do
      begin
        x := x + SymSpace div 2;
        brushcolor := PointColor;
        if FillPoints then
          brushStyle := bsSolid
        else
          brushstyle := bsClear;
        pencolor := PointColor;
        case Pointshape of
          ps_Square : Polygon([Pt(x - pSize, y - pSize), Pt(x + psize, y - pSize), Pt(x + pSize, y + psize), Pt(x - pSize, y + pSize)]);
          ps_Circle : Ellipse(x - pSize, y - pSize, x + psize, y + psize);
          ps_Diamond : Polygon([Pt(x, y - pSize), Pt(x + psize, y), Pt(x, y + psize), Pt(x - pSize, y)]);
          ps_cross :
            begin
            Line(x - pSize, y, x + psize, y);
            Line(x, y - pSize, x, y + psize);
            end;
        end;
      end;
  end;

begin
  EntryCount := 0;
  for p in FSeries do
  begin
    if p.LegendStatus <> lsNone then
      inc(EntryCount);
  end;
  if EntryCount = 0 then
    exit;

  FCanvas.Font.Assign(FLegend.font);
  FCanvas.BrushColor := FLegend.FColor;
  FCanvas.BrushStyle := bsSolid;
  FCanvas.PenStyle := FLegend.borderStyle;
  FCanvas.pencolor := FLegend.borderColor;
  FCanvas.PenWidth := 1;

  w := FLegend.width;
  h := FLegend.height;
  if FLegend.FLayout = lsAcross then
  begin
    h := max(h, FCanvas.TextHeight('XX')+Legend.FYMargin*2);
    t := 0;
    for p in FSeries do
    begin
      if p.LegendStatus <> lsNone then
        inc(t, Legend.SymbolSpace + 2 + FCanvas.TextWidth(p.Data.name)+6);
    end;
    w := max(w, t+Legend.FXMargin*2);
  end
  else
  begin
    t := 0;
    for p in FSeries do
    begin
      if p.LegendStatus <> lsNone then
      begin
        w := max(w, Legend.SymbolSpace + 2 + FCanvas.TextWidth(p.Data.name)+Legend.FXMargin*2);
        t := t + FCanvas.TextHeight('XX')+5;
      end;
    end;
    h := max(h, t + Legend.FYMargin*2);
  end;


  xleft := round(FLegend.left*Fscale)+FXoffset;
  ytop := round(FLegend.top*FScale) + FYOffset;
  xwid := round(w *FScale);
  yhei := round(h*Fscale);
  FCanvas.Rectangle(xleft,ytop,xleft+xwid,ytop+yhei);
  xbase := round((FLegend.Xmargin+FLegend.left)*Fscale)+FXoffset;
  ybase := round((FLegend.top+FLegend.ymargin)*FScale) +FYOffset;
  symspace := round(FLegend.FSymbolSpace*Fscale);
  th := FCanvas.TextHeight('T');
  if FLegend.FLayout = lsAcross then
  begin
    yoffset := 0;
    xoffset := (xwid - (FLegend.fxmargin*2)) div EntryCount;
  end
  else
  begin
    xoffset := 0;
    yoffset := (yhei - (FLegend.fymargin*2)) div entrycount;
  end;

  OldPenWidth := FCanvas.PenWidth;
  if FAppearance.FPrintLineStyle then
    FCanvas.PenWidth := 1;
  i := 0;
  t := FLegend.left+Legend.XMargin;
  for p in FSeries do
  begin
    if p.LegendStatus <> lsNone then
    begin
      DrawLegendEntry(p.Data.name, p.LegendStatus, p.LineColor, p.PointColor, p.DrawLine, p.DrawPoints, p.fillPoints, p.Pointshape, p.Linestyle, round((p.Pointsize div 2)*FScale), t, ybase + i*yoffset);
      if FLegend.Layout = lsAcross then
        inc(t, Legend.SymbolSpace+2+FCanvas.TextWidth(p.Data.name)+6);
      FCanvas.BrushColor := FLegend.Fcolor;
      inc(i);
    end;
  end;
end;

{$IFNDEF FMX}
procedure TFGraph.ChangeScale(M, D: Integer);
begin
  inherited;
  FSM := M;
  FSD := D;
  RePaint;
end;
{$ENDIF}

function TFGraph.CheckScalesOK : Boolean;
var
  XOK, YOK, Y2OK, xResized, yResized, y2Resized : Boolean;
begin
  xResized := false;
  with FXAxis do
    begin
    if FAutoSizing then
      xResized := DoResize;
    XOK := CheckScale;
    if XOK and FAutoStepping then
      begin
      if not FLogging then
        FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin)
      else
        begin
        AdjustLogTickCount;
        CheckDrawMinorLabels;
        end;
      end;
    end;
  yResized := false;
  with FYAxis do
    begin
    if FAutoSizing then
      yResized := DoResize;
    YOK := CheckScale;
    if YOK and FAutoStepping then
      begin
      if not FLogging then
        FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin)
      else
        begin
        AdjustLogTickCount;
        CheckDrawMinorLabels;
        end;
      end;
    end;
  y2Resized := false;
  with FYAxis2 do
    begin
    if FAutoSizing then
      y2Resized := DoResize;
    Y2OK := CheckScale;
    if Y2OK and FAutoStepping then
      begin
      if not FLogging then
        FStep := GetStep(FGraph.FAppearance.MinSteps, FMax, FMin)
      else
        begin
        AdjustLogTickCount;
        CheckDrawMinorLabels;
        end;
      end;
    end;

  result := XOK and YOK and Y2OK;
  if result and (xResized or yResized or y2resized) then DoRescaleEvent;
end;

{new after version 3.01 : calling Paintgraph with PaintAxes = false suppresses
 the drawing of the axes. This is called when PreventFlicker is false
 and a new point has been added and it doesn't require a resizing.

 Caution! This also means that a number of checking routines are not called}

procedure TFGraph.PaintGraph(PaintAxes : boolean);
var
  p : TFGraphSeries;
  f : TFGraphFunction;
  oldPenWidth : word;
  obc : TAlphaColor;

begin
  FCanvas.start;
  try
    If not (csDesigning in ComponentState) and Assigned(FOnPaintStart) then
      FOnPaintStart(self, FCanvas.FCanvas);
    if PaintAxes then
    begin
      if FPlotting and (DataFound(true) or DataFound(false)) then
        DoTightFit;
      AllScalesOK := CheckScalesOK;
      CalcMetrics;
      DrawAxes;
      if not AllScalesOK then
        exit;
      if Fappearance.FCrossAtZero and not FXaxis.FLogging and not FYAxis.Flogging then
        PaintCross;
      if not FPlotting then
        Exit;
      {Printing : linewidth for series, points thinner than for axes}
      if FCanvas.PenWidth >= 3 then
        FCanvas.PenWidth := (FCanvas.PenWidth + 1) div 2;
      if FAppearance.FShowMarks then
        DrawAnnotations(dtBeforeSeries);
      end;
    if not FPlotting then
      Exit;
    OldPenWidth := FCanvas.PenWidth;
    if FAppearance.FPrintLineStyle then
      FCanvas.PenWidth := 1;
    for f in FFunctions do
      PaintFunction(f);
    for p in FSeries do
      PaintSeries(p);
    for p in FSeries do
      PaintSeries(p);
    FCanvas.PenWidth := OldPenWidth;
    if FAppearance.FShowMarks then
      DrawAnnotations(dtBeforeLegend);
    if PaintAxes then
    begin
      if FLegend.FVisible then
        DrawLegend;
      if FAppearance.FShowMarks then
        DrawAnnotations(dtAfter);
      If not (csDesigning in ComponentState) and Assigned(FOnPaintEnd) then
         FOnPaintEnd(self, FCanvas.FCanvas);
    end;
  finally
    FCanvas.finish;
  end;
end;

procedure TFGraph.PaintSeries(series : TFGraphSeries);
begin
  if not series.FActive or (series.FData = nil) then
    Exit;

  series.Data.Prepare;
  if (FXAxis.LogScale and (series.Data.getMinXValue <= 0)) or
     (YAxis[series.YAxis2].LogScale and (series.Data.getMinYValue <= 0)) then
    Exit;

  if series.FAutoZero then
    series.ZeroOffset := series.Data.getMinXValue;

  if FAppearance.FPlotOffGraph then
    ClipGraph;
  PaintSeriesBounds(series);

  if (series.FRegressionType <> rg_None)
      then PaintSeriesRegression(series);

  if (series.DrawErrors) and (not YAxis[series.YAxis2].FLogging) and (not FXAxis.FLogging) then
    PaintSeriesErrors(series);

  if series.DrawLine then
    PaintSeriesLine(series);
  if not Appearance.PlotOffGraph then
    UnclipGraph; { it'd be nice if we were able to clip the points, but the clipping applies to the centre of the point, not the plotted shape }
  if series.DrawPoints then
    PaintSeriesPoints(series);
end;

procedure TFGraph.PaintSeriesLine(series : TFGraphSeries);
var
  pp : TFGraphDataPoint;
  i, t : integer;
  lp : pLinePoints;
  lpsize : integer;
  total: integer;
begin
  total := series.Data.count;
  t := 0;
  GetMem(lp, series.Data.count * sizeof(TPoint));
  try
    if series.Highlighted then
    begin
      for i := 0 to total - 1 do
      begin
        pp := series.Data.getPoint(i);
        if (pp.error = '') then
        begin
          lp^[t].x := fx(pp.x);
          lp^[t].y := fy(pp.y - series.ZeroOffset, series.FYAxis2)+ 2;
          inc(t);
        end;
      end;
      FCanvas.PenColor := clBlack;
      FCanvas.PenStyle := psDot;
      FCanvas.Polyline(lp, t);
    end;

    t := 0;
    for i := 0 to total - 1 do
    begin
      pp := series.Data.getPoint(i);
      if (pp.error = '') then
      begin
        lp^[t].x := fx(pp.x);
        lp^[t].y := fy(pp.y - series.ZeroOffset, series.FYAxis2);
        inc(t);
      end;
    end;
    FCanvas.PenColor := series.FLineColor;
    FCanvas.PenStyle := series.FLineStyle;
    FCanvas.Polyline(lp, t);
  finally
    Freemem(lp);
  end;
end;

procedure TFGraph.PaintSeriesPoints(series : TFGraphSeries);
var
  s1Size : Word;
  LastX, LastY, MinPointClearance, pencolor, brushcolor : integer;

  procedure PlotSquare(x, y : Integer);
  begin
    if sqrt(sqr(x-LastX)+sqr(y-LastY)) < MinPointClearance then
      exit;
    LastX := x;
    LastY := y;
    if series.FHighlighted then
    begin
      FCanvas.PenColor := clwhite xor color;
      FCanvas.BrushColor := color;
      FCanvas.Polygon([Pt(x-s1Size-2, y-s1Size-2),Pt(x+s1size+2, y-s1Size-2),
                            Pt(x+s1Size+2, y+s1size+2),Pt(x-s1Size-2, y+s1Size+2)]);
      FCanvas.PenColor := pencolor;
      FCanvas.BrushColor := brushcolor;
    end;
    FCanvas.Polygon([Pt(x - s1Size, y - s1Size), Pt(x + s1size, y - s1Size), Pt(x + s1Size, y + s1size), Pt(x - s1Size, y + s1Size)]);
  end;

  procedure PlotCircle(x, y : Integer);
  begin
    if sqrt(sqr(x-LastX)+sqr(y-LastY)) < MinPointClearance then
      exit;
    LastX := x;
    LastY := y;
    if series.FHighlighted then
    begin
      FCanvas.PenColor := clwhite xor color;
      FCanvas.BrushColor := color;
      FCanvas.Ellipse(x - s1Size-2, y - s1Size-2, x + s1size+2, y + s1size+2);
      FCanvas.PenColor := pencolor;
      FCanvas.BrushColor := brushcolor;
    end;
    FCanvas.Ellipse(x - s1Size, y - s1Size, x + s1size, y + s1size);
  end;

  procedure PlotDiamond(x, y : Integer);
  begin
    if sqrt(sqr(x-LastX)+sqr(y-LastY)) < MinPointClearance then
      exit;
    LastX := x;
    LastY := y;
    if series.FHighlighted then
    begin
      FCanvas.PenColor := clwhite xor color;
      FCanvas.BrushColor := color;
      FCanvas.Polygon([Pt(x, y - s1Size-2), Pt(x + s1size+2, y),
                             Pt(x, y + s1size+2), Pt(x - s1Size-2, y)]);
      FCanvas.PenColor := pencolor;
      FCanvas.BrushColor := brushcolor;
    end;
    FCanvas.Polygon([Pt(x, y - s1Size), Pt(x + s1size, y), Pt(x, y + s1size), Pt(x - s1Size, y)]);
  end;

  procedure PlotCross(x, y : Integer);
  begin
    if sqrt(sqr(x-LastX)+sqr(y-LastY)) < MinPointClearance then
      exit;
    LastX := x;
    LastY := y;
    if series.FHighlighted then
    begin
      FCanvas.PenColor := clwhite xor color;
      FCanvas.BrushColor := color;
      FCanvas.Line(x - s1Size+2, y+2, x + s1size+2, y+2);
      FCanvas.Line(x+2, y - s1Size+2, x+2, y + s1size+2);
      FCanvas.PenColor := pencolor;
      FCanvas.BrushColor := brushcolor;
    end;
    FCanvas.Line(x - s1Size, y, x + s1size, y);
    FCanvas.Line(x, y - s1Size, x, y + s1size);
  end;

  procedure PlotBar(x1,x2,y,y2 : integer);
  begin
    if x1-x2 <> 0 then
      FCanvas.Rectangle(x1, y, x2, y2);
  end;

  procedure PlotError(x,y : integer; error : String);
  var
    w, h : integer;
  begin
    FCanvas.PenColor := series.LineColor;
    FCanvas.BrushColor := clInfoBk;
    FCanvas.Font.Assign(Appearance.ErrorFont);
    FCanvas.fonTAlphaColor := clMaroon;
    h := FCanvas.TextHeight(error)+8;
    w := FCanvas.TextWidth(error)+8;
    FCanvas.Rectangle(x-(h div 2), y, x+(h div 2), y-w);
    FCanvas.TextOut(x-6, y-4, 4, 4, error, 90/180 * pi);
  end;

  function getErrorY(pp : TFGraphDataPoint; index : integer) : Double;
  var
    i : integer;
    yp, yn : Double;
  begin
    result := pp.y;
    yp := NO_VALUE;
    yn := NO_VALUE;
    if result = NO_VALUE then
    begin
      i := index;
      repeat
        dec(i);
        if (i >= 0) then
        begin
          pp := series.Data.getPoint(i);
          yp := pp.y;
        end;
      until (i < 0) or (yp <> NO_VALUE);
      i := index;
      repeat
        inc(i);
        if (i < series.Data.count) then
        begin
          pp := series.Data.getPoint(i);
          yn := pp.y;
        end;
      until (i = series.Data.count) or (yn <> NO_VALUE);
      if (yp <> NO_VALUE) and (yn <> NO_VALUE) then
        result := (yp + yn) / 2
      else if (yp <> NO_VALUE) then
        result := yp
      else
        result := yn;
    end;
  end;

var
  pp : TFGraphDataPoint;
  ty, ty2, OldPenWidth : integer;
  tx1,tx2 : double;
  i : integer;
begin
  ty2 := 0;
  OldPenWidth := FCanvas.PenWidth;
  FCanvas.PenColor := series.FPointColor;
  FCanvas.PenStyle := psSolid;
  FCanvas.BrushStyle := bsSolid;
  if series.FFillPoints then
    FCanvas.BrushColor := series.FPointColor
  else
    FCanvas.BrushColor := Color;

  pencolor := FCanvas.PenColor;
  brushcolor := FCanvas.BrushColor;
  if series.FPointShape = ps_Wide then
  begin
    FCanvas.PenWidth := Round(FScale*(series.FPointSize div 2));
    FCanvas.PenColor := series.FPointColor;
  end
  else if series.FPointShape = ps_bar then
  begin
    ty2 := trunc(height - (Fdimensions.FBottom));
    FCanvas.PenColor := series.FLineColor;
  end
  else
    OldPenWidth := 0;

  s1Size := Round(FScale*(series.FPointSize div 2));
  LastX := -99;
  LastY := -99;
  MinPointClearance := trunc(FAppearance.FMinPointClearance*s1size*2);

  for i := 0 to series.Data.count - 1 do
  begin

    pp := series.Data.getPoint(i);
    if (Appearance.PlotOffGraph and (series.FPointShape <> ps_Bar)) or
      ((pp.x >= FXAxis.Min) and (pp.x <= FXAxis.Max) and
      ((pp.error <> '') or ((pp.y - series.ZeroOffset >= YAxis[series.YAxis2].Min) and (pp.y - series.ZeroOffset <= YAxis[series.YAxis2].Max)))) then
    begin
      if pp.error <> '' then
      begin
        plotError(fx(pp.x), fy(getErrorY(pp, i), series.FYAxis2), pp.error);
      end
      else
      case series.FPointShape of  { only test this once }
        ps_Square :  PlotSquare(fx(pp.x), fy(pp.y - series.ZeroOffset, series.FYAxis2));
        ps_Circle :  PlotCircle(fx(pp.x), fy(pp.y - series.ZeroOffset, series.FYAxis2));
        ps_Diamond : PlotDiamond(fx(pp.x), fy(pp.y - series.ZeroOffset, series.FYAxis2));
        ps_Cross :   PlotCross(fx(pp.x), fy(pp.y - series.ZeroOffset, series.FYAxis2));
        ps_Wide :    begin
                     ty := fy(pp.y, series.FYAxis2);
                     if i < series.Data.count -1  then //todo: this behaves badly with errors
                       FCanvas.Line(fx(pp.x), ty, fx(series.data.getPoint(i+1).x), ty)
                     else
                       FCanvas.Line(fx(pp.x), ty, fx(XAxis.Max), ty);
                     end;
        ps_bar :     begin
                     ty := fy(pp.y, series.FYAxis2);
                     tx1 := pp.x + series.FBarOffset;
                     tx2 := pp.x + series.FBarOffset + series.FBarWidth;
                     ty := Min(ty, FDimensions.FTop);
                     tx1 := Min(tx1, FXAxis.min);
                     tx2 := Min(tx2, FXAxis.min);
                     tx1 := Max(tx1, FXAxis.max);
                     tx2 := Max(tx2, FXAxis.max);
                     PlotBar(fx(tx1), fx(tx2), ty, ty2);
                     end;
      end;
    end;
  end;
  FCanvas.BrushColor := Color;
  FCanvas.BrushStyle := bsClear;
  if series.FPointShape = ps_Wide then
    FCanvas.PenWidth := OldPenWidth;
end;

procedure TFGraph.PaintSeriesRegression(series : TFGraphSeries);
var
  i : integer;
  lp: pLinePoints;
begin
  series.checkRegenStats;
  GetMem(lp, series.FRegressionData.count * sizeof(TPoint));
  try
    for i := 0 to series.FRegressionData.Count - 1 do
    begin
      lp^[i].x := fx(series.FRegressionData[i].x);
      lp^[i].y := fy(series.FRegressionData[i].y - series.ZeroOffset, series.FYAxis2);
    end;
    FCanvas.PenColor := series.FRegrColor;
    FCanvas.PenStyle := series.FRegrLineStyle;
    FCanvas.Polyline(lp, series.FRegressionData.Count);
  finally
    Freemem(lp);
  end;
end;

procedure TFGraph.PaintSeriesBounds(series : TFGraphSeries);
  procedure DrawBound(y : double);
  begin
    FCanvas.PenColor := series.FBoundsColor;
    FCanvas.penstyle := series.FBoundsLineStyle;
    FCanvas.Line(fx(series.Data.getMinXValue), fy(y, series.FYAxis2), fx(series.Data.getMaxXValue), fy(y, series.FYAxis2));
  end;
var
  mean, SD : double;
begin
  if series.FBoundsType = bt_None then
    exit;

  if series.FBoundsType = bt_AsSet then
  begin
    DrawBound(series.FUpperBound);
    DrawBound(series.FLowerBound);
  end
  else
  begin
    mean := series.YStats.Mean;
    SD := series.YStats.SD; {fix!}
    case series.FBoundsType of
      bt_1SD  : begin
        drawbound(mean + SD);
        drawbound(mean - SD);
        end;
      bt_2SD  : begin
        drawbound(mean + 2 * SD);
        drawbound(mean - 2 * SD);
        end;
      bt_3SD  : begin
        drawbound(mean + 3 * SD);
        drawbound(mean - 3 * SD);
        end;
      bt_allSD : begin
        drawbound(mean + SD);
        drawbound(mean - SD);
        drawbound(mean + 2 * SD);
        drawbound(mean - 2 * SD);
        drawbound(mean + 3 * SD);
        drawbound(mean - 3 * SD);
        end;
    end;
  end;
end;

procedure TFGraph.PaintSeriesErrors;
var
  pp : TFGraphDataPoint;
  xi, xf, yi, yf : integer;
  i : integer;
begin
  FCanvas.PenColor := series.FErrorsLineColor;
  FCanvas.PenStyle := series.FErrorsLineStyle;
  for i := 0 to series.Data.count - 1 do
  begin
    pp := series.data.getPoint(i);
    if (pp.error = '') then
    begin
      if pp.ye <> 0 then
      begin
        xi := fx(pp.x);
        yi := fy(pp.y - pp.ye - series.ZeroOffset, series.FYAxis2);
        yf := fy(pp.y + pp.ye - series.ZeroOffset, series.FYAxis2);
        FCanvas.Line(xi,yi,xi,yf);
        FCanvas.Line(xi-2, yi, xi+3, yi);
        FCanvas.Line(xi-2, yf, xi+3, yf);
      end;
      if pp.xe <> 0 then
      begin
        xi := fx(pp.x - pp.xe);
        xf := fx(pp.x + pp.xe);
        yi := fy(pp.y - series.ZeroOffset, series.FYAxis2);
        FCanvas.Line(xi,yi,xf,yi);
        FCanvas.Line(xi, yi-2, xi, yi+3);
        FCanvas.Line(xf, yi-2, xf, yi+3);
      end;
    end;
  end;
end;

procedure TFGraph.SeriesChange(Sender: TObject; const Item: TFGraphSeries; Action: TCollectionNotification);
begin
  if Action = cnAdded then
    item.FGraph := self;
  if FPlotting then
    RePaint;
end;

procedure TFGraph.Paint;
begin
  {$IFNDEF FMX}
  if not HandleAllocated then
    exit;
  {$ENDIF}
  inherited Paint;
  if FPainting then
    exit;

  if FCanvas.FCanvas = nil then
    FCanvas.FCanvas := Canvas;

  {stop re-entrancy problems if a window - such as a print dialog
   box - is cleared just before printing, calling a paint and
   screwing the printer metrics up}
  FCurrWidth := trunc(width);
  FCurrHeight := trunc(height);
  PaintGraph(true);
  CVisible := False;
end;

procedure TFGraph.SetPlotting(v : Boolean);
begin
  FPlotting := v;
  RePaint;
  if v then DoRescaleEvent;
end;

{$IFNDEF FMX}
var
  CODES_TAlign : array [TAlign] of String = ('alNone', 'alTop', 'alBottom', 'alLeft', 'alRight', 'alClient', 'alCustom');
  CODES_TBevelCut : array [TBevelCut] of String = ('bvNone', 'bvLowered', 'bvRaised', 'bvSpace');
  CODES_TFormBorderStyle : array [TFormBorderStyle] of String = ('bsNone', 'bsSingle', 'bsSizeable', 'bsDialog', 'bsToolWindow', 'bsSizeToolWin');
  CODES_TPenStyle : array [TPenStyle] of String = ('psSolid', 'psDash', 'psDot', 'psDashDot', 'psDashDotDot', 'psClear', 'psInsideFrame', 'psUserStyle', 'psAlternate');
  CODES_TFontStyle : array [TFontStyle] of String = ('fsBold', 'fsItalic', 'fsUnderline', 'fsStrikeOut');
  CODES_TFGraphLegendStyle : array [TFGraphLegendStyle] of String = ('lsAcross', 'lsDown');
  CODES_TFGraphDateTickType : array [TFGraphDateTickType] of String = ('dt_minute', 'dt_5minute', 'dt_halfhourly', 'dt_hourly', 'dt_daily', 'dt_weekly', 'dt_monthly', 'dt_2monthly', 'dt_quarterly', 'dt_annually', 'dt_decade', 'dt_century', 'dt_custom');
  CODES_TFGraphAxisOffsetType : array [TFGraphAxisOffsetType] of String = ('ao_Minimum', 'ao_Maximum', 'ao_percentage', 'ao_absolute');

function TFGraph.SettingsText(code : boolean): String;
var
  b : TStringBuilder;
  procedure p(name, value : String); overload;
  begin
    b.Append('  '+self.Name+'.'+name+' := '''+value+''';');
    b.Append(#13#10);
  end;
  procedure p(name : String; value : boolean); overload;
  begin
    b.Append('  '+self.Name+'.'+name+' := '+BoolToStr(value, true)+';');
    b.Append(#13#10);
  end;
  procedure p(name : String; value : integer); overload;
  begin
    b.Append('  '+self.Name+'.'+name+' := '+inttostr(value)+';');
    b.Append(#13#10);
  end;
  procedure pc(name : String; value : integer); overload;
  var
    s : String;
  begin
    if (value = clSystemColor) then s := 'clSystemColor'
    else if (value = clScrollBar) then s := 'clScrollBar'
    else if (value = clBackground) then s := 'clBackground'
    else if (value = clActiveCaption) then s := 'clActiveCaption'
    else if (value = clInactiveCaption) then s := 'clInactiveCaption'
    else if (value = clMenu) then s := 'clMenu'
    else if (value = clWindow) then s := 'clWindow'
    else if (value = clWindowFrame) then s := 'clWindowFrame'
    else if (value = clMenuText) then s := 'clMenuText'
    else if (value = clWindowText) then s := 'clWindowText'
    else if (value = clCaptionText) then s := 'clCaptionText'
    else if (value = clActiveBorder) then s := 'clActiveBorder'
    else if (value = clInactiveBorder) then s := 'clInactiveBorder'
    else if (value = clAppWorkSpace) then s := 'clAppWorkSpace'
    else if (value = clHighlight) then s := 'clHighlight'
    else if (value = clHighlightText) then s := 'clHighlightText'
    else if (value = clBtnFace) then s := 'clBtnFace'
    else if (value = clBtnShadow) then s := 'clBtnShadow'
    else if (value = clGrayText) then s := 'clGrayText'
    else if (value = clBtnText) then s := 'clBtnText'
    else if (value = clInactiveCaptionText) then s := 'clInactiveCaptionText'
    else if (value = clBtnHighlight) then s := 'clBtnHighlight'
    else if (value = cl3DDkShadow) then s := 'cl3DDkShadow'
    else if (value = cl3DLight) then s := 'cl3DLight'
    else if (value = clInfoText) then s := 'clInfoText'
    else if (value = clInfoBk) then s := 'clInfoBk'
    else if (value = clHotLight) then s := 'clHotLight'
    else if (value = clGradientActiveCaption) then s := 'clGradientActiveCaption'
    else if (value = clGradientInactiveCaption) then s := 'clGradientInactiveCaption'
    else if (value = clMenuHighlight) then s := 'clMenuHighlight'
    else if (value = clMenuBar) then s := 'clMenuBar'
    else if (value = clBlack) then s := 'clBlack'
    else if (value = clMaroon) then s := 'clMaroon'
    else if (value = clGreen) then s := 'clGreen'
    else if (value = clOlive) then s := 'clOlive'
    else if (value = clNavy) then s := 'clNavy'
    else if (value = clPurple) then s := 'clPurple'
    else if (value = clTeal) then s := 'clTeal'
    else if (value = clGray) then s := 'clGray'
    else if (value = clSilver) then s := 'clSilver'
    else if (value = clRed) then s := 'clRed'
    else if (value = clLime) then s := 'clLime'
    else if (value = clYellow) then s := 'clYellow'
    else if (value = clBlue) then s := 'clBlue'
    else if (value = clFuchsia) then s := 'clFuchsia'
    else if (value = clAqua) then s := 'clAqua'
    else if (value = clLtGray) then s := 'clLtGray'
    else if (value = clDkGray) then s := 'clDkGray'
    else if (value = clWhite) then s := 'clWhite'
    else if (value = clMoneyGreen) then s := 'clMoneyGreen'
    else if (value = clSkyBlue) then s := 'clSkyBlue'
    else if (value = clCream) then s := 'clCream'
    else if (value = clMedGray) then s := 'clMedGray'
    else if (value = clWebSnow) then s := 'clWebSnow'
    else if (value = clWebFloralWhite) then s := 'clWebFloralWhite'
    else if (value = clWebLavenderBlush) then s := 'clWebLavenderBlush'
    else if (value = clWebOldLace) then s := 'clWebOldLace'
    else if (value = clWebIvory) then s := 'clWebIvory'
    else if (value = clWebCornSilk) then s := 'clWebCornSilk'
    else if (value = clWebBeige) then s := 'clWebBeige'
    else if (value = clWebAntiqueWhite) then s := 'clWebAntiqueWhite'
    else if (value = clWebWheat) then s := 'clWebWheat'
    else if (value = clWebAliceBlue) then s := 'clWebAliceBlue'
    else if (value = clWebGhostWhite) then s := 'clWebGhostWhite'
    else if (value = clWebLavender) then s := 'clWebLavender'
    else if (value = clWebSeashell) then s := 'clWebSeashell'
    else if (value = clWebLightYellow) then s := 'clWebLightYellow'
    else if (value = clWebPapayaWhip) then s := 'clWebPapayaWhip'
    else if (value = clWebNavajoWhite) then s := 'clWebNavajoWhite'
    else if (value = clWebMoccasin) then s := 'clWebMoccasin'
    else if (value = clWebBurlywood) then s := 'clWebBurlywood'
    else if (value = clWebAzure) then s := 'clWebAzure'
    else if (value = clWebMintcream) then s := 'clWebMintcream'
    else if (value = clWebHoneydew) then s := 'clWebHoneydew'
    else if (value = clWebLinen) then s := 'clWebLinen'
    else if (value = clWebLemonChiffon) then s := 'clWebLemonChiffon'
    else if (value = clWebBlanchedAlmond) then s := 'clWebBlanchedAlmond'
    else if (value = clWebBisque) then s := 'clWebBisque'
    else if (value = clWebPeachPuff) then s := 'clWebPeachPuff'
    else if (value = clWebTan) then s := 'clWebTan'
    else if (value = clWebYellow) then s := 'clWebYellow'
    else if (value = clWebDarkOrange) then s := 'clWebDarkOrange'
    else if (value = clWebRed) then s := 'clWebRed'
    else if (value = clWebDarkRed) then s := 'clWebDarkRed'
    else if (value = clWebMaroon) then s := 'clWebMaroon'
    else if (value = clWebIndianRed) then s := 'clWebIndianRed'
    else if (value = clWebSalmon) then s := 'clWebSalmon'
    else if (value = clWebCoral) then s := 'clWebCoral'
    else if (value = clWebGold) then s := 'clWebGold'
    else if (value = clWebTomato) then s := 'clWebTomato'
    else if (value = clWebCrimson) then s := 'clWebCrimson'
    else if (value = clWebBrown) then s := 'clWebBrown'
    else if (value = clWebChocolate) then s := 'clWebChocolate'
    else if (value = clWebSandyBrown) then s := 'clWebSandyBrown'
    else if (value = clWebLightSalmon) then s := 'clWebLightSalmon'
    else if (value = clWebLightCoral) then s := 'clWebLightCoral'
    else if (value = clWebOrange) then s := 'clWebOrange'
    else if (value = clWebOrangeRed) then s := 'clWebOrangeRed'
    else if (value = clWebFirebrick) then s := 'clWebFirebrick'
    else if (value = clWebSaddleBrown) then s := 'clWebSaddleBrown'
    else if (value = clWebSienna) then s := 'clWebSienna'
    else if (value = clWebPeru) then s := 'clWebPeru'
    else if (value = clWebDarkSalmon) then s := 'clWebDarkSalmon'
    else if (value = clWebRosyBrown) then s := 'clWebRosyBrown'
    else if (value = clWebPaleGoldenrod) then s := 'clWebPaleGoldenrod'
    else if (value = clWebLightGoldenrodYellow) then s := 'clWebLightGoldenrodYellow'
    else if (value = clWebOlive) then s := 'clWebOlive'
    else if (value = clWebForestGreen) then s := 'clWebForestGreen'
    else if (value = clWebGreenYellow) then s := 'clWebGreenYellow'
    else if (value = clWebChartreuse) then s := 'clWebChartreuse'
    else if (value = clWebLightGreen) then s := 'clWebLightGreen'
    else if (value = clWebAquamarine) then s := 'clWebAquamarine'
    else if (value = clWebSeaGreen) then s := 'clWebSeaGreen'
    else if (value = clWebGoldenRod) then s := 'clWebGoldenRod'
    else if (value = clWebKhaki) then s := 'clWebKhaki'
    else if (value = clWebOliveDrab) then s := 'clWebOliveDrab'
    else if (value = clWebGreen) then s := 'clWebGreen'
    else if (value = clWebYellowGreen) then s := 'clWebYellowGreen'
    else if (value = clWebLawnGreen) then s := 'clWebLawnGreen'
    else if (value = clWebPaleGreen) then s := 'clWebPaleGreen'
    else if (value = clWebMediumAquamarine) then s := 'clWebMediumAquamarine'
    else if (value = clWebMediumSeaGreen) then s := 'clWebMediumSeaGreen'
    else if (value = clWebDarkGoldenRod) then s := 'clWebDarkGoldenRod'
    else if (value = clWebDarkKhaki) then s := 'clWebDarkKhaki'
    else if (value = clWebDarkOliveGreen) then s := 'clWebDarkOliveGreen'
    else if (value = clWebDarkgreen) then s := 'clWebDarkgreen'
    else if (value = clWebLimeGreen) then s := 'clWebLimeGreen'
    else if (value = clWebLime) then s := 'clWebLime'
    else if (value = clWebSpringGreen) then s := 'clWebSpringGreen'
    else if (value = clWebMediumSpringGreen) then s := 'clWebMediumSpringGreen'
    else if (value = clWebDarkSeaGreen) then s := 'clWebDarkSeaGreen'
    else if (value = clWebLightSeaGreen) then s := 'clWebLightSeaGreen'
    else if (value = clWebPaleTurquoise) then s := 'clWebPaleTurquoise'
    else if (value = clWebLightCyan) then s := 'clWebLightCyan'
    else if (value = clWebLightBlue) then s := 'clWebLightBlue'
    else if (value = clWebLightSkyBlue) then s := 'clWebLightSkyBlue'
    else if (value = clWebCornFlowerBlue) then s := 'clWebCornFlowerBlue'
    else if (value = clWebDarkBlue) then s := 'clWebDarkBlue'
    else if (value = clWebIndigo) then s := 'clWebIndigo'
    else if (value = clWebMediumTurquoise) then s := 'clWebMediumTurquoise'
    else if (value = clWebTurquoise) then s := 'clWebTurquoise'
    else if (value = clWebCyan) then s := 'clWebCyan'
    else if (value = clWebAqua) then s := 'clWebAqua'
    else if (value = clWebPowderBlue) then s := 'clWebPowderBlue'
    else if (value = clWebSkyBlue) then s := 'clWebSkyBlue'
    else if (value = clWebRoyalBlue) then s := 'clWebRoyalBlue'
    else if (value = clWebMediumBlue) then s := 'clWebMediumBlue'
    else if (value = clWebMidnightBlue) then s := 'clWebMidnightBlue'
    else if (value = clWebDarkTurquoise) then s := 'clWebDarkTurquoise'
    else if (value = clWebCadetBlue) then s := 'clWebCadetBlue'
    else if (value = clWebDarkCyan) then s := 'clWebDarkCyan'
    else if (value = clWebTeal) then s := 'clWebTeal'
    else if (value = clWebDeepskyBlue) then s := 'clWebDeepskyBlue'
    else if (value = clWebDodgerBlue) then s := 'clWebDodgerBlue'
    else if (value = clWebBlue) then s := 'clWebBlue'
    else if (value = clWebNavy) then s := 'clWebNavy'
    else if (value = clWebDarkViolet) then s := 'clWebDarkViolet'
    else if (value = clWebDarkOrchid) then s := 'clWebDarkOrchid'
    else if (value = clWebMagenta) then s := 'clWebMagenta'
    else if (value = clWebFuchsia) then s := 'clWebFuchsia'
    else if (value = clWebDarkMagenta) then s := 'clWebDarkMagenta'
    else if (value = clWebMediumVioletRed) then s := 'clWebMediumVioletRed'
    else if (value = clWebPaleVioletRed) then s := 'clWebPaleVioletRed'
    else if (value = clWebBlueViolet) then s := 'clWebBlueViolet'
    else if (value = clWebMediumOrchid) then s := 'clWebMediumOrchid'
    else if (value = clWebMediumPurple) then s := 'clWebMediumPurple'
    else if (value = clWebPurple) then s := 'clWebPurple'
    else if (value = clWebDeepPink) then s := 'clWebDeepPink'
    else if (value = clWebLightPink) then s := 'clWebLightPink'
    else if (value = clWebViolet) then s := 'clWebViolet'
    else if (value = clWebOrchid) then s := 'clWebOrchid'
    else if (value = clWebPlum) then s := 'clWebPlum'
    else if (value = clWebThistle) then s := 'clWebThistle'
    else if (value = clWebHotPink) then s := 'clWebHotPink'
    else if (value = clWebPink) then s := 'clWebPink'
    else if (value = clWebLightSteelBlue) then s := 'clWebLightSteelBlue'
    else if (value = clWebMediumSlateBlue) then s := 'clWebMediumSlateBlue'
    else if (value = clWebLightSlateGray) then s := 'clWebLightSlateGray'
    else if (value = clWebWhite) then s := 'clWebWhite'
    else if (value = clWebLightgrey) then s := 'clWebLightgrey'
    else if (value = clWebGray) then s := 'clWebGray'
    else if (value = clWebSteelBlue) then s := 'clWebSteelBlue'
    else if (value = clWebSlateBlue) then s := 'clWebSlateBlue'
    else if (value = clWebSlateGray) then s := 'clWebSlateGray'
    else if (value = clWebWhiteSmoke) then s := 'clWebWhiteSmoke'
    else if (value = clWebSilver) then s := 'clWebSilver'
    else if (value = clWebDimGray) then s := 'clWebDimGray'
    else if (value = clWebMistyRose) then s := 'clWebMistyRose'
    else if (value = clWebDarkSlateBlue) then s := 'clWebDarkSlateBlue'
    else if (value = clWebDarkSlategray) then s := 'clWebDarkSlategray'
    else if (value = clWebGainsboro) then s := 'clWebGainsboro'
    else if (value = clWebDarkGray) then s := 'clWebDarkGray'
    else if (value = clWebBlack) then s := 'clWebBlack'
    else s := inttostr(value);

    b.Append('  '+self.Name+'.'+name+' := '+s+';');
    b.Append(#13#10);
  end;
  procedure p(name : String; value : double); overload;
  begin
    b.Append('  '+self.Name+'.'+name+' := '+FloatToStr(value)+';');
    b.Append(#13#10);
  end;
  procedure p(name : String; value : integer; codes : array of String); overload;
  begin
    b.Append('  '+self.Name+'.'+name+' := '+codes[value]+';');
    b.Append(#13#10);
  end;
  procedure ps(name : String; value : TFontStyles; codes : array of String); overload;
  var
    f : boolean;
    a : TFontStyle;
  begin
    f := false;
    b.Append('  '+self.Name+'.'+name+' := [');
    for a := low(TFontStyle) to high(TFontStyle) do
      if a in value then
      begin
        if f then b.Append(',') else f := true;
        b.Append(codes[ord(a)]);
      end;
    b.Append('];');
    b.Append(#13#10);
  end;
  procedure p(name : String; value : TFontAdapted); overload;
  begin
    pc(name+'.Color', value.Color);
    p(name+'.Height', value.Height);
    p(name+'.Name', value.Name);
    p(name+'.Orientation', value.Orientation);
    p(name+'.Size', value.Size);
    ps(name+'.Style', value.Style, CODES_TFontStyle);
  end;
begin
  b := TStringBuilder.Create;
  try
    p('Align', Ord(Align), CODES_TAlign);
    p('BevelInner', ord(BevelInner), CODES_TBevelCut);
    p('BevelOuter', ord(BevelOuter), CODES_TBevelCut);
    p('BevelWidth', BevelWidth);
    p('BorderStyle', ord(BorderStyle), CODES_TFormBorderStyle);
    p('BorderWidth', BorderWidth);
    p('Ctl3D', Ctl3D);
    p('Cursor', Cursor);
    p('ParentCtl3D', ParentCtl3D);
    p('ParentFont', ParentFont);
    p('Visible', Visible);
    p('Plotting', Plotting);

    p('Dimensions.BottomMargin', Dimensions.BottomMargin);
    p('Dimensions.LeftMargin', Dimensions.LeftMargin);
    p('Dimensions.RightMargin', Dimensions.RightMargin);
    p('Dimensions.TopMargin', Dimensions.TopMargin);
    p('Dimensions.TickLength', Dimensions.TickLength);
    p('Dimensions.XAxisTitleOffset', Dimensions.XAxisTitleOffset);
    p('Dimensions.XAxisLabelOffset', Dimensions.XAxisLabelOffset);
    p('Dimensions.YAxisTitleOffset', Dimensions.YAxisTitleOffset);
    p('Dimensions.YAxisLabelOffset', Dimensions.YAxisLabelOffset);
    p('Dimensions.GraphTitleOffset', Dimensions.GraphTitleOffset);
    p('Dimensions.PrintXOffsetPct', Dimensions.PrintXOffsetPct);
    p('Dimensions.PrintYOffsetPct', Dimensions.PrintYOffsetPct);
    p('Dimensions.PrintScalePct', Dimensions.PrintScalePct);

    pc('Appearance.AxesColor', Appearance.AxesColor);
    pc('Appearance.BackgroundColor', Appearance.BackgroundColor);
    pc('Appearance.MarginColor', Appearance.MarginColor);
    p('Appearance.PrintBkgndColor', Appearance.PrintBkgndColor);
    p('Appearance.ErrorCaption', Appearance.ErrorCaption);
    pc('Appearance.GridColor', Appearance.GridColor);
    p('Appearance.GridStyle', Ord(Appearance.GridStyle), CODES_TPenStyle);
    p('Appearance.ShowGraphLabels', Appearance.ShowGraphLabels);
    p('Appearance.PlotOffGraph', Appearance.PlotOffGraph);
    p('Appearance.ShowMarks', Appearance.ShowMarks);
    p('Appearance.ShowTicks', Appearance.ShowTicks);
    p('Appearance.GraphTitle', Appearance.GraphTitle);
    p('Appearance.MinSteps', Appearance.MinSteps);
    p('Appearance.MaxSteps', Appearance.MaxSteps);
    p('Appearance.TitleFont', Appearance.TitleFont);
    p('Appearance.CaptionFont', Appearance.CaptionFont);
    p('Appearance.LabelFont', Appearance.LabelFont);
    p('Appearance.CrossAtZero', Appearance.CrossAtZero);
    pc('Appearance.CrossColor', Appearance.CrossColor);
    p('Appearance.Crosslength', Appearance.Crosslength);
    p('Appearance.PrintLineStyle', Appearance.PrintLineStyle);
    p('Appearance.MinPointClearance', Appearance.MinPointClearance);
    p('Appearance.CrossWire', Appearance.CrossWire);

    p('Legend.visible', Legend.visible);
    pc('Legend.color', Legend.color);
    p('Legend.borderStyle', ord(Legend.borderStyle), CODES_TPenStyle);
    pc('Legend.borderColor', Legend.borderColor);
    p('Legend.Layout', Ord(Legend.Layout), CODES_TFGraphLegendStyle);
    p('Legend.top', Legend.top);
    p('Legend.left', Legend.left);
    p('Legend.width', Legend.width);
    p('Legend.height', Legend.height);
    p('Legend.font', Legend.font);
    p('Legend.SymbolSpace', Legend.SymbolSpace);
    p('Legend.XMargin', Legend.XMargin);
    p('Legend.YMargin', Legend.YMargin);

    p('XAxis.Title', XAxis.Title);
    p('XAxis.LabelDecimals', XAxis.LabelDecimals);
    p('XAxis.AutoLabelDecimals', XAxis.AutoLabelDecimals);
    p('XAxis.LogCycleDivisions', XAxis.LogCycleDivisions);
    p('XAxis.LogScale', XAxis.LogScale);
    p('XAxis.Max', XAxis.Max);
    p('XAxis.Min', XAxis.Min);
    p('XAxis.StepSize', XAxis.StepSize);
    p('XAxis.MinScaleLength', XAxis.MinScaleLength);
    p('XAxis.ShowAsTime', XAxis.ShowAsTime);
    p('XAxis.DateTimeFormat', XAxis.DateTimeFormat);
    p('XAxis.DateTickType', ord(XAxis.DateTickType), CODES_TFGraphDateTickType);
    p('XAxis.ShowAxis', XAxis.ShowAxis);
    p('XAxis.Reversed', XAxis.Reversed);
    p('XAxis.OffsetType', Ord(XAxis.OffsetType), CODES_TFGraphAxisOffsetType);
    p('XAxis.Offset', XAxis.Offset);
    p('XAxis.Gridlines', XAxis.Gridlines);
    p('XAxis.AutoSizing', XAxis.AutoSizing);
    p('XAxis.AutoStepping', XAxis.AutoStepping);

    p('YAxis1.Title', YAxis1.Title);
    p('YAxis1.LabelDecimals', YAxis1.LabelDecimals);
    p('YAxis1.AutoLabelDecimals', YAxis1.AutoLabelDecimals);
    p('YAxis1.LogCycleDivisions', YAxis1.LogCycleDivisions);
    p('YAxis1.LogScale', YAxis1.LogScale);
    p('YAxis1.Max', YAxis1.Max);
    p('YAxis1.Min', YAxis1.Min);
    p('YAxis1.StepSize', YAxis1.StepSize);
    p('YAxis1.MinScaleLength', YAxis1.MinScaleLength);
    p('YAxis1.ShowAsTime', YAxis1.ShowAsTime);
    p('YAxis1.DateTimeFormat', YAxis1.DateTimeFormat);
    p('YAxis1.DateTickType', ord(YAxis1.DateTickType), CODES_TFGraphDateTickType);
    p('YAxis1.ShowAxis', YAxis1.ShowAxis);
    p('YAxis1.Reversed', YAxis1.Reversed);
    p('YAxis1.OffsetType', Ord(YAxis1.OffsetType), CODES_TFGraphAxisOffsetType);
    p('YAxis1.Offset', YAxis1.Offset);
    p('YAxis1.Gridlines', YAxis1.Gridlines);
    p('YAxis1.AutoSizing', YAxis1.AutoSizing);
    p('YAxis1.AutoStepping', YAxis1.AutoStepping);

    p('YAxis2.Title', YAxis2.Title);
    p('YAxis2.LabelDecimals', YAxis2.LabelDecimals);
    p('YAxis2.AutoLabelDecimals', YAxis2.AutoLabelDecimals);
    p('YAxis2.LogCycleDivisions', YAxis2.LogCycleDivisions);
    p('YAxis2.LogScale', YAxis2.LogScale);
    p('YAxis2.Max', YAxis2.Max);
    p('YAxis2.Min', YAxis2.Min);
    p('YAxis2.StepSize', YAxis2.StepSize);
    p('YAxis2.MinScaleLength', YAxis2.MinScaleLength);
    p('YAxis2.ShowAsTime', YAxis2.ShowAsTime);
    p('YAxis2.DateTimeFormat', YAxis2.DateTimeFormat);
    p('YAxis2.DateTickType', ord(YAxis2.DateTickType), CODES_TFGraphDateTickType);
    p('YAxis2.ShowAxis', YAxis2.ShowAxis);
    p('YAxis2.Reversed', YAxis2.Reversed);
    p('YAxis2.OffsetType', Ord(YAxis2.OffsetType), CODES_TFGraphAxisOffsetType);
    p('YAxis2.Offset', YAxis2.Offset);
    p('YAxis2.Gridlines', YAxis2.Gridlines);
    p('YAxis2.AutoSizing', YAxis2.AutoSizing);
    p('YAxis2.AutoStepping', YAxis2.AutoStepping);

    result := b.ToString;
  finally
    b.Free;
  end;
end;
{$ENDIF}

procedure TFGraph.SetHasDragged(v : boolean);
{ this procedure is primarily present to set FHasdragged to false
  and override any user changes }
begin
 if v <> FHasDragged then
  if v then
  begin
    FHasDragged := true;
    FHoldXAS := FXAxis.Autosizing;
    FHoldXmin := FXAxis.Min;
    FHoldXmax := FXAxis.Max;
    FHoldYAS := FYAxis.Autosizing;
    FHoldYmin := FYAxis.Min;
    FHoldYmax := FYAxis.Max;
    FHoldY2AS := FYAxis2.Autosizing;
    FHoldY2min := FYAxis2.Min;
    FHoldY2max := FYAxis2.Max;
  end
  else
  begin
    FHasDragged := false;
    plotting := false;
    FXaxis.autosizing := FHoldXAS;
    if not FHoldXAS then
    begin
      FXAxis.min := FHoldXMin;
      FXAxis.max := FHoldXMax;
    end;
    FYaxis.autosizing := FHoldYAS;
    if not FHoldYAS then
    begin
      FYAxis.min := FHoldYMin;
      FYAxis.max := FHoldYMax;
    end;
    FYaxis2.autosizing := FHoldY2AS;
    if not FHoldY2AS then
    begin
      FYAxis2.min := FHoldY2Min;
      FYAxis2.max := FHoldY2Max;
    end;
    plotting := true;
  end;
end;

function TFGraph.DataFound(yaxis2 : boolean) : Boolean;
var
  p : TFGraphSeries;
begin
  Result := False;
  for p in series do
  begin
    if (p.FYAxis2 = yaxis2) and (p.Data.count > 0) then
      exit(true);
  end;
end;

procedure TFGraph.ClearAll;
var
  holdPlotting : Boolean;
begin
  holdPlotting := FPlotting;
  FPlotting := false;
  try
    FSeries.Clear;
  finally
    FPlotting := holdPlotting;
  end;
  if FPlotting then
    RePaint;
end;

function TFGraph.createMark(x1,y1, x2,y2 : Double; c : TAlphaColor; name : string; marktype : TFGraphMarkType; markpos : TFGraphMarkPos; DrawTime : TFGraphDrawTime) : TFGraphMark;
begin
  result := TFGraphMark.create(name, drawtime);
  try
    result.Color := c;
    result.x1 := x1;
    result.y1 := y1;
    result.x2 := x2;
    result.y2 := y2;
    result.MarkType := marktype;
    result.MarkPos := markpos;
    result.link;
  finally
    result.free;
  end;
end;

function TFGraph.createSeries(data : TFGraphDataProvider) : TFGraphSeries;
begin
  result := TFGraphSeries.Create(data);
  try
    result.FYAxis2 := false;
    result.FActive := true;
    result.FDrawLine := true;
    result.FDrawPoints := true;
    result.FHighlighted := false;
    case FSeries.Count + 1 of
     1 : result.FPointColor := clgreen;
     2 : result.FPointColor := clblue;
     3 : result.FPointColor := clred;
     4 : result.FPointColor := clGray;
     5 : result.FPointColor := clMaroon;
     else result.FPointColor := clgreen;
    end;
    result.FLineColor := result.FPointColor;
    case FSeries.Count + 1 of
     1 : result.FPointShape := ps_Square;
     2 : result.FPointShape := ps_Circle;
     3 : result.FPointShape := ps_Diamond;
     4 : result.FPointShape := ps_Cross;
     5 : result.FPointShape := ps_Square;
     else result.FPointShape := ps_Circle;
    end;
    result.FFillPoints := (FSeries.Count <= 5);
     case FSeries.Count + 1 of
     1 : result.FLineStyle := psSolid;
     2 : result.FLineStyle := psDash;
     3 : result.FLineStyle := psDot;
     4 : result.FLineStyle := PsDashDot;
     5 : result.FLineStyle := PsDashDotDot;
     else result.FLineStyle := psSolid;
    end;
    result.FPointSize := 4;
    result.FBarWidth := 0;
    result.FBarOffset := 0;
    result.FUpperBound := 0;
    result.FLowerBound := 0;
    result.FBoundsType := bt_none;
    result.FBoundsLineStyle := psDashDot;
    result.FRegrColor := clgray;
    result.FRegrLineStyle := psDot;
    result.FlegendStatus := lsNone;
    result.FRegControl2 := 0.04;
    result.RegControl1 := 100;
    result.link;
  finally
    result.Free;
  end;
end;

{ TFGraphFunction }

procedure TFGraphFunction.SetBounded(const Value: boolean);
begin
  FBounded := Value;
  if assigned(FGraph) then
    FGraph.RePaint;
end;

procedure TFGraphFunction.SetAlphaColor(const Value: TAlphaColor);
begin
  FColor := Value;
  if assigned(FGraph) then
    FGraph.RePaint;
end;

procedure TFGraphFunction.SetStyle(const Value: TPenStyle);
begin
  FStyle := Value;
  if assigned(FGraph) then
    FGraph.RePaint;
end;

procedure TFGraphFunction.SetXMax(const Value: Double);
begin
  FXMax := Value;
  if assigned(FGraph) then
    FGraph.RePaint;
end;

procedure TFGraphFunction.SetXMin(const Value: Double);
begin
  FXMin := Value;
  if assigned(FGraph) then
    FGraph.RePaint;
end;

{ TGraphCanvas }

constructor TGraphCanvas.create(canvas: TCanvas);
begin
  inherited create;
  FCanvas := canvas;
  {$IFDEF FMX}
  FFonTAlphaColor := TAlphaColors.Black;
  {$ENDIF}
end;

destructor TGraphCanvas.Destroy;
begin
  inherited;
end;

procedure TGraphCanvas.clip(x1, y1, x2, y2: integer);
{$IFNDEF FMX}
var
  ClipRgn : HRgn;
{$ENDIF}
begin
{$IFDEF FMX}
//  FState := FCanvas.SaveState;
//  FCanvas.IntersectClipRect(TRectF.Create(x1, y1, x2, y2));
{$ELSE}
  ClipRgn := CreateRectRgn(x1,y1,x2,y2);
  SelectClipRgn(FCanvas.Handle, ClipRgn);
  DeleteObject(ClipRgn);
{$ENDIF}
end;

procedure TGraphCanvas.unClip(x1, y1, x2, y2: integer);
{$IFNDEF FMX}
var
  ClipRgn : HRgn;
{$ENDIF}
begin
{$IFDEF FMX}
//  FCanvas.RestoreState(FState);
{$ELSE}
  ClipRgn := CreateRectRgn(x1,y1,x2,y2);
  SelectClipRgn(FCanvas.Handle, ClipRgn);
  DeleteObject(ClipRgn);
{$ENDIF}
end;

procedure TGraphCanvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  {$IFDEF FMX}
  FCanvas.DrawEllipse(TRectF.Create(x1, y1, x2, y2), 1.0);
  {$ELSE}
  FCanvas.Ellipse(x1, y1, x2, y2);
  {$ENDIF}
end;

procedure TGraphCanvas.finish;
begin
  {$IFDEF FMX}
  FCanvas.EndScene;
  {$ENDIF}
end;

function TGraphCanvas.GetBrushColor: TAlphaColor;
begin
  {$IFDEF FMX}
  result := FCanvas.Fill.Color;
  {$ELSE}
  result := FCanvas.Brush.Color;
  {$ENDIF}
end;

function TGraphCanvas.GetBrushStyle: TBrushStyle;
begin
  {$IFDEF FMX}
  result := FCanvas.Fill.Kind;
  {$ELSE}
  result := FCanvas.Brush.Style;
  {$ENDIF}
end;

function TGraphCanvas.GetFont: TFont;
begin
  result := FCanvas.Font;
end;

function TGraphCanvas.GetFonTAlphaColor: TAlphaColor;
begin
  {$IFDEF FMX}
  result := FFonTAlphaColor;
  {$ELSE}
  result := FCanvas.Font.Color;
  {$ENDIF}
end;

function TGraphCanvas.GetPenColor: TAlphaColor;
begin
  {$IFDEF FMX}
  result := FCanvas.Stroke.Color;
  {$ELSE}
  result := FCanvas.Pen.Color;
  {$ENDIF}
end;

function TGraphCanvas.GetPenMode: TPenMode;
begin
  {$IFDEF FMX}
  result := pmXOr;
  {$ELSE}
  result := FCanvas.Pen.Mode;
  {$ENDIF}
end;

function TGraphCanvas.GetPenStyle: TPenStyle;
begin
  {$IFDEF FMX}
  result := FCanvas.Stroke.Dash;
  {$ELSE}
  result := FCanvas.Pen.Style;
  {$ENDIF}
end;

function TGraphCanvas.GetPenWidth: integer;
begin
  {$IFDEF FMX}
  result := trunc(FCanvas.Stroke.Thickness);
  {$ELSE}
  result := FCanvas.Pen.Width;
  {$ENDIF}
end;

procedure TGraphCanvas.Line(x1, y1, x2, y2: integer);
begin
  {$IFDEF FMX}
  FCanvas.DrawLine(TPointF.Create(x1, y1), TPointF.Create(x2,y2), 1.0);
  {$ELSE}
  FCanvas.MoveTo(x1, y1);
  FCanvas.LineTo(x2, y2);
  {$ENDIF}
end;

procedure TGraphCanvas.Polygon(const Points: array of TPoint);
{$IFDEF FMX}
var
  p : TPolygon;
  i : integer;
{$ENDIF}
begin
  {$IFDEF FMX}
  SetLength(p, length(points));
  for i := 0 to length(Points) - 1 do
    p[i] := TPointF.create(points[i].x, points[i].y);
  FCanvas.DrawPolygon(p, 1.0);
  {$ELSE}
  FCanvas.Polygon(points);
  {$ENDIF}
end;

procedure TGraphCanvas.Polyline(points: pLinePoints; count: integer);
{$IFDEF FMX}
var
  p : TPathData;
  i : integer;
{$ENDIF}
begin
  {$IFDEF FMX}
  p := TPathData.Create;
  try
    p.MoveTo(TPointF.Create(points[0].x, points[0].y));
    for i := 1 to count - 1 do
      p.LineTo(TPointF.create(points[i].x, points[i].y));
    FCanvas.DrawPath(p, 1.0);
  finally
    p.free;
  end;
  {$ELSE}
  WinApi.Windows.Polyline(FCanvas.Handle, points^, count);
  {$ENDIF}
end;


procedure TGraphCanvas.Polyline(const Points: array of TPoint);
{$IFDEF FMX}
var
  p : TPathData;
  i : integer;
{$ENDIF}
begin
  {$IFDEF FMX}
  p := TPathData.Create;
  try
    p.MoveTo(TPointF.Create(points[0].x, points[0].y));
    for i := 1 to length(points) - 1 do
      p.LineTo(TPointF.create(points[i].x, points[i].y));
    FCanvas.DrawPath(p, 1.0);
  finally
    p.free;
  end;
  {$ELSE}
  FCanvas.Polyline(points);
  {$ENDIF}
end;

procedure TGraphCanvas.Rectangle(x1, y1, x2, y2: integer; opacity : double = 1.0);
{$IFNDEF FMX}
Var
  bmp : TBitmap;
  Blend : _BLENDFUNCTION;
{$ENDIF}
begin
  {$IFDEF FMX}
  FCanvas.FillRect(TRectF.Create(x1,y1,x2,y2), 0, 0, AllCorners, opacity);
  FCanvas.DrawRect(TRectF.Create(x1,y1,x2,y2), 0, 0, AllCorners, opacity);
  {$ELSE}
  if opacity <> 1 then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Width := 1;
      bmp.Height := 1;
      bmp.Canvas.Pixels[0,0] := BrushColor;
      Blend.BlendOp := AC_SRC_OVER;
      Blend.BlendFlags := 0;
      Blend.SourceConstantAlpha := trunc(50+255*opacity*100) Div 100;
      Blend.AlphaFormat := 0;
      AlphaBlend(FCanvas.Handle, x1, y2, x2-x1, y1-y2, bmp.Canvas.Handle, 0, 0, 1, 1, Blend);
    finally
      bmp.Free;
     end;
  end
  else
    FCanvas.Rectangle(x1, y1, x2, y2);
 {$ENDIF}
end;

function TGraphCanvas.SelectObject(obj: THandle): THandle;
begin
  {$IFDEF FMX}
  {$ELSE}
  result := WinApi.Windows.SelectObject(FCanvas.Handle, obj);
  {$ENDIF}
end;

procedure TGraphCanvas.SetBrushColor(const Value: TAlphaColor);
begin
  {$IFDEF FMX}
  FCanvas.Fill.Color := Value;
  {$ELSE}
  FCanvas.Brush.Color := Value;
  {$ENDIF}
end;

procedure TGraphCanvas.SetBrushStyle(const Value: TBrushStyle);
begin
  {$IFDEF FMX}
  FCanvas.Fill.Kind := Value;
  {$ELSE}
  FCanvas.Brush.Style := Value;
  {$ENDIF}
end;

procedure TGraphCanvas.SetFont(const Value: TFont);
begin
  {$IFDEF FMX}
  FCanvas.Font.Assign(value);
  {$ELSE}
  FCanvas.Font := value;
  {$ENDIF}
end;

procedure TGraphCanvas.SetFonTAlphaColor(const Value: TAlphaColor);
begin
  {$IFDEF FMX}
  FFonTAlphaColor := Value;
  {$ELSE}
  FCanvas.Font.Color := Value;
  {$ENDIF}
end;

procedure TGraphCanvas.SetPenColor(const Value: TAlphaColor);
begin
  {$IFDEF FMX}
  FCanvas.Stroke.Color := value;
  {$ELSE}
  FCanvas.Pen.Color := Value;
  {$ENDIF}
end;

procedure TGraphCanvas.SetPenMode(const Value: TPenMode);
begin
  {$IFDEF FMX}
  {$ELSE}
  FCanvas.Pen.Mode := Value;
  {$ENDIF}
end;

procedure TGraphCanvas.SetPenStyle(const Value: TPenStyle);
begin
  {$IFDEF FMX}
  FCanvas.Stroke.Dash := value;
  {$ELSE}
  FCanvas.Pen.Style := Value;
  {$ENDIF}
end;

procedure TGraphCanvas.SetPenWidth(const Value: integer);
begin
  {$IFDEF FMX}
  FCanvas.Stroke.Thickness := value;
  {$ELSE}
  FCanvas.Pen.Width := Value;
  {$ENDIF}
end;

procedure TGraphCanvas.start;
begin
  {$IFDEF FMX}
  FCanvas.BeginScene;
  {$ENDIF}
end;

function TGraphCanvas.TextHeight(text: String): integer;
begin
  {$IFDEF FMX}
  result := trunc(FCanvas.TextHeight(text));
  {$ELSE}
  result := FCanvas.TextHeight(text);
  {$ENDIF}
end;

procedure TGraphCanvas.TextOut(xOrigin, yOrigin : integer; xDelta, yDelta : integer; text : String; angle : double);
var
{$IFDEF FMX}
  l_OriginalMatrix: TMatrix;
  l_Matrix: TMatrix;
  l_CenterPoint : TPointF;
{$ELSE}
  LogRec  : TLOGFONT;
  OldFont, NewFont : HFONT;
  xoffst, yoffst : integer;
{$ENDIF}
begin
  {$IFDEF FMX}
  l_OriginalMatrix := FCanvas.Matrix;
  try
    // create a point around which will rotate
    l_CenterPoint := TPointF.Create(xOrigin, yOrigin);
    l_Matrix := l_OriginalMatrix;
    l_Matrix := l_Matrix * TMatrix.CreateTranslation(-l_CenterPoint.X,-l_CenterPoint.Y);
    l_Matrix := l_Matrix * TMatrix.CreateRotation(-angle);
    l_Matrix := l_Matrix * TMatrix.CreateTranslation(l_CenterPoint.X,l_CenterPoint.Y);
    FCanvas.SetMatrix(l_Matrix);
    Textout(xOrigin+yDelta, yOrigin-yDelta, text);
  finally
    FCanvas.SetMatrix(l_OriginalMatrix);
  end;
  {$ELSE}
  { Get the current font information. We only want to modify the angle }
  GetObject(FCanvas.Font.Handle,SizeOf(LogRec),@LogRec);
  LogRec.lfEscapement := trunc(angle * 10 * 180 / pi);
  LogRec.lfOrientation := trunc(angle * 10 * 180 / pi);    {see win32 api?}
  LogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;  {doesn't work under win95??}
  NewFont := CreateFontIndirect(LogRec);
  OldFont := WinApi.Windows.SelectObject(FCanvas.Handle, NewFont);  {Save old font}
  { offsets }
  xoffst := Round(sin(angle) * yDelta);
  yoffst := Round(cos(angle) * yDelta);
  TextOut(xOrigin - xoffst, yOrigin - yoffst, text);
  NewFont := WinApi.Windows.SelectObject(FCanvas.Handle,OldFont); {Restore oldfont}
  DeleteObject(NewFont);
  {$ENDIF}
end;

procedure TGraphCanvas.TextOut(x, y: integer; text: String);
begin
  {$IFDEF FMX}
  FCanvas.Fill.Color := fonTAlphaColor;
  FCanvas.FillText(TRectF.Create(x, y, x+TextWidth(text), y+Textheight(text)), text, false, 1.0, [], TTextAlign.Leading);
  {$ELSE}
  FCanvas.TextOut(x,y, text);
  {$ENDIF}
end;

function TGraphCanvas.TextWidth(text: String): integer;
begin
  {$IFDEF FMX}
  result := trunc(FCanvas.TextWidth(text));
  {$ELSE}
  result := FCanvas.TextWidth(text);
  {$ENDIF}
end;

{$IFDEF FMX}

{ TFontAdapted }

function TFontAdapted.GetOnChange: TNotifyEvent;
begin
  result := OnChanged;
end;

procedure TFontAdapted.SetOnChange(const Value: TNotifyEvent);
begin
  OnChanged := Value;
end;
{$ENDIF}

{ TFGraphDataPoint }

procedure TFGraphDataPoint.clear;
begin
  id := 0;
  x := NO_VALUE;
  y := NO_VALUE;
  x2 := NO_VALUE;
  y2 := NO_VALUE;
  xe := NO_VALUE;
  ye := NO_VALUE;
  error := '';
end;

{ TFGraphBand }

function TFGraphBand.link: TFGraphBand;
begin
   result := TFGraphBand(inherited link);
end;

procedure TFGraphBand.SetAlphaColor(const Value: TAlphaColor);
begin
  FColor := Value;
  change;
end;

procedure TFGraphBand.SetMax(const Value: Double);
begin
  FMax := Value;
  change;
end;

procedure TFGraphBand.SetMin(const Value: Double);
begin
  FMin := Value;
  change;
end;

procedure TFGraphBand.SetOpacity(const Value: Double);
begin
  FOpacity := Value;
  change;
end;

procedure TFGraphBand.SetYAxis2(const Value: boolean);
begin
  FYAxis2 := Value;
  change;
end;

end.



