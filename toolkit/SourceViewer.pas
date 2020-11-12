unit SourceViewer;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  fhir_objects, fhir_factory, fhir_common, fsl_http,
  FHIR.Version.Resources, FHIR.Version.Parser, fhir_diff;

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
    FFactory: TFHIRFactory;
    procedure SetCurrent(const Value: TFHIRResource);
    procedure SetOriginal(const Value: TFHIRResource);

    procedure render;
    procedure SetFactory(const Value: TFHIRFactory);
  public
    destructor Destroy; override;
    property factory : TFHIRFactory read FFactory write SetFactory;
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
  FFactory.Free;
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
  diff : TFHIRParametersW;
  html : string;
begin
  if rbXml.IsChecked then
    c := TFHIRXmlComposer.Create(nil, OutputStylePretty, THTTPLanguages.create('en'))
  else if rbJson.IsChecked then
    c := TFHIRJsonComposer.Create(nil, OutputStylePretty, THTTPLanguages.create('en'))
  else
    c := TFHIRTurtleComposer.Create(nil, OutputStylePretty, THTTPLanguages.create('en'));
  try
    case TabControl1.TabIndex of
      0 : mSource.Lines.Text := c.Compose(FCurrent);
      1 : mSource.Lines.Text := c.Compose(FOriginal);
      2 :
        begin
        engine := TDifferenceEngine.Create(nil, FFactory.link);
        try
          diff := engine.generateDifference(FOriginal, FCurrent, html);
          try
            mSource.Lines.Text := c.Compose(diff.Resource);
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

procedure TSourceViewerForm.SetFactory(const Value: TFHIRFactory);
begin
  FFactory.Free;
  FFactory := Value;
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
