unit SourceViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FHIRResources, FHIRParser, DifferenceEngine;

type
  TSourceViewerForm = class(TForm)
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    rbXml: TRadioButton;
    rbJson: TRadioButton;
    rbTurtle: TRadioButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    mSource: TMemo;
    StyleBook1: TStyleBook;
    procedure FormShow(Sender: TObject);
    procedure statusControlChange(Sender: TObject);
  private
    FCurrent: TFHIRResource;
    FOriginal: TFHIRResource;
    procedure SetCurrent(const Value: TFHIRResource);
    procedure SetOriginal(const Value: TFHIRResource);

    procedure render;
  public
    destructor Destroy; override;
    property current : TFHIRResource read FCurrent write SetCurrent;
    property original : TFHIRResource read FOriginal write SetOriginal;
  end;

var
  SourceViewerForm: TSourceViewerForm;

implementation

{$R *.fmx}

{ TSourceViewerForm }

destructor TSourceViewerForm.Destroy;
begin
  FCurrent.Free;
  FOriginal.Free;
  inherited;
end;

procedure TSourceViewerForm.FormShow(Sender: TObject);
begin
  render;
end;

procedure TSourceViewerForm.render;
var
  c : TFHIRComposer;
  engine : TDifferenceEngine;
  diff : TFHIRParameters;
  html : string;
begin
  if rbXml.IsChecked then
    c := TFHIRXmlComposer.Create(nil, 'en')
  else if rbJson.IsChecked then
    c := TFHIRJsonComposer.Create(nil, 'en')
  else
    c := TFHIRTurtleComposer.Create(nil, 'en');
  try
    case TabControl1.TabIndex of
      0 : mSource.Lines.Text := c.Compose(FCurrent, true);
      1 : mSource.Lines.Text := c.Compose(FOriginal, true);
      2 :
        begin
        engine := TDifferenceEngine.Create(nil);
        try
          diff := engine.generateDifference(FOriginal, FCurrent, html);
          try
            mSource.Lines.Text := c.Compose(diff, true);
          finally
            diff.Free;
          end;
        finally
          engine.Free;
        end;
        end;
    end;
  finally
    c.free;
  end;
end;

procedure TSourceViewerForm.SetCurrent(const Value: TFHIRResource);
begin
  FCurrent.Free;
  FCurrent := Value;
end;

procedure TSourceViewerForm.SetOriginal(const Value: TFHIRResource);
begin
  FOriginal.Free;
  FOriginal := Value;
end;

procedure TSourceViewerForm.statusControlChange(Sender: TObject);
begin
  render;
end;

end.
