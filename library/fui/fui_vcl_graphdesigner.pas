Unit FHIR.Ui.GraphDesigner;

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


Interface

Uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, ClipBrd,
  Spin, Buttons, TabNotBk, Tabs, FHIR.Ui.Graph;

Type
  TGraphDesignerForm = Class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    fd: TFontDialog;
    cd: TColorDialog;
    BitBtn2: TBitBtn;
    pnlAxes: TPanel;
    Bevel10: TBevel;
    Bevel7: TBevel;
    Label34: TLabel;
    Label37: TLabel;
    Label36: TLabel;
    Label38: TLabel;
    Label33: TLabel;
    Label31: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label35: TLabel;
    Label40: TLabel;
    Label42: TLabel;
    Label41: TLabel;
    Label32: TLabel;
    Label30: TLabel;
    Bevel9: TBevel;
    CheckBox10: TCheckBox;
    Edit4: TEdit;
    Edit5: TEdit;
    CheckBox11: TCheckBox;
    Edit6: TEdit;
    CheckBox7: TCheckBox;
    CheckBox9: TCheckBox;
    Edit3: TEdit;
    SpinEdit12: TSpinEdit;
    SpinEdit13: TSpinEdit;
    CheckBox13: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Edit7: TEdit;
    ComboBox2: TComboBox;
    Edit2: TEdit;
    CheckBox8: TCheckBox;
    Label3: TLabel;
    Label24: TLabel;
    Label14: TLabel;
    Label26: TLabel;
    Label12: TLabel;
    Label28: TLabel;
    Label27: TLabel;
    Label1: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Label15: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    Label17: TLabel;
    Label16: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Edit1: TEdit;
    Button3: TButton;
    SpinEdit1: TSpinEdit;
    Button7: TButton;
    SpinEdit10: TSpinEdit;
    SpinEdit11: TSpinEdit;
    Button11: TButton;
    CheckBox3: TCheckBox;
    SpinEdit9: TSpinEdit;
    SpinEdit8: TSpinEdit;
    CheckBox4: TCheckBox;
    SpinEdit2: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Panel2: TPanel;
    Label50: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label51: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Bevel8: TBevel;
    SpinEdit16: TSpinEdit;
    SpinEdit18: TSpinEdit;
    SpinEdit19: TSpinEdit;
    SpinEdit17: TSpinEdit;
    SpinEdit20: TSpinEdit;
    SpinEdit21: TSpinEdit;
    SpinEdit22: TSpinEdit;
    CheckBox16: TCheckBox;
    Button6: TButton;
    Button15: TButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    Panel17: TPanel;
    CheckBox15: TCheckBox;
    Panel20: TPanel;
    Label59: TLabel;
    Label60: TLabel;
    ListBox1: TListBox;
    ComboBox8: TComboBox;
    Bevel6: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Label20: TLabel;
    Label10: TLabel;
    Label4: TLabel;
    Label22: TLabel;
    Label21: TLabel;
    Label23: TLabel;
    Label13: TLabel;
    Label29: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label2: TLabel;
    Label25: TLabel;
    Button9: TButton;
    Button1: TButton;
    Button2: TButton;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    Button10: TButton;
    gridstyle: TComboBox;
    CheckBox12: TCheckBox;
    SpinEdit14: TSpinEdit;
    Button5: TButton;
    Bevel11: TBevel;
    Bevel12: TBevel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel3: TPanel;
    TabSheet3: TTabSheet;
    Panel4: TPanel;
    TabSheet4: TTabSheet;
    Panel5: TPanel;
    Panel6: TPanel;
    ComboBox1: TComboBox;
    Button4: TButton;
    Procedure Edit1Change(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure closebutClick(Sender: TObject);
    Procedure SpinEdit1Change(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure SpinEdit8Change(Sender: TObject);
    Procedure SpinEdit9Change(Sender: TObject);
    Procedure SpinEdit10Change(Sender: TObject);
    Procedure SpinEdit11Change(Sender: TObject);
    Procedure CheckBox3Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox5Click(Sender: TObject);
    Procedure CheckBox6Click(Sender: TObject);
    Procedure SpinEdit5Change(Sender: TObject);
    Procedure SpinEdit6Change(Sender: TObject);
    Procedure SpinEdit7Change(Sender: TObject);
    Procedure Button10Click(Sender: TObject);
    Procedure gridstyleChange(Sender: TObject);
    Procedure SpinEdit2Change(Sender: TObject);
    Procedure SpinEdit3Change(Sender: TObject);
    Procedure SpinEdit4Change(Sender: TObject);
    Procedure CheckBox4Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure CheckBox7Click(Sender: TObject);
    Procedure CheckBox9Click(Sender: TObject);
    Procedure Edit3Change(Sender: TObject);
    Procedure SpinEdit12Change(Sender: TObject);
    Procedure SpinEdit13Change(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure CheckBox8Click(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure CheckBox10Click(Sender: TObject);
    Procedure Edit4Change(Sender: TObject);
    Procedure Edit5Change(Sender: TObject);
    Procedure CheckBox11Click(Sender: TObject);
    Procedure Edit6Change(Sender: TObject);
    Procedure RadioButton1Click(Sender: TObject);
    Procedure ComboBox3Change(Sender: TObject);
    Procedure Edit7Change(Sender: TObject);
    Procedure ComboBox4Change(Sender: TObject);
{    procedure Edit10Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioButton7Click(Sender: TObject);
    procedure ComboBox10Change(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure ComboBox11Change(Sender: TObject);
    procedure SpinEdit15Change(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure Edit11Change(Sender: TObject);
    procedure ComboBox12Change(Sender: TObject);}
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure Button5Click(Sender: TObject);
    Procedure CheckBox12Click(Sender: TObject);
    Procedure SpinEdit14Change(Sender: TObject);
    Procedure CheckBox13Click(Sender: TObject);
    Procedure CheckBox15Click(Sender: TObject);
    Procedure SpinEdit16Change(Sender: TObject);
    Procedure SpinEdit17Change(Sender: TObject);
    Procedure SpinEdit18Change(Sender: TObject);
    Procedure SpinEdit19Change(Sender: TObject);
    Procedure SpinEdit20Change(Sender: TObject);
    Procedure SpinEdit21Change(Sender: TObject);
    Procedure SpinEdit22Change(Sender: TObject);
    Procedure RadioButton4Click(Sender: TObject);
    Procedure CheckBox16Click(Sender: TObject);
    Procedure Button15Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ComboBox8Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  Private
    { Private declarations }
    editing : Boolean;
    currentAxis : TFGraphAxis;
    hideshape : TPanel;
    FGraph : TFGraph;
    Procedure updateaxespage;
    Procedure updatelegendpage;

    Procedure SetGraph(v:TFGraph);
  Public
    { Public declarations }
{    fgraph: txygraph;}
    Property Graph : TFGraph Read FGraph Write SetGraph;
  End;

Var
  GraphDesignerForm: TGraphDesignerForm;

Function designGraph(AOwner:TComponent; Graph : TFGraph) : Boolean;

Implementation

{$R *.DFM}

{----------------------------------------------------------------------------
    #1a. set up
 ----------------------------------------------------------------------------}

Procedure TGraphDesignerForm.SetGraph;
Begin
  FGraph := v;
End;

Procedure TGraphDesignerForm.FormShow(Sender: TObject);
Begin
{  checkbox2.checked := FGraph.Appearance.allowduplicates;
}
  {general page}
  edit1.text := FGraph.appearance.graphtitle;
  spinedit1.value := FGraph.dimensions.graphtitleoffset;
  spinedit8.value := FGraph.dimensions.topmargin;
  spinedit9.value := FGraph.dimensions.bottommargin;
  spinedit10.value := FGraph.dimensions.leftmargin;
  spinedit11.value := FGraph.dimensions.rightmargin;
  spinedit5.value := FGraph.dimensions.Ticklength;
  checkbox3.checked := FGraph.appearance.plotoffgraph;
  Checkbox5.checked := FGraph.appearance.showgraphlabels;
  Checkbox6.checked := FGraph.appearance.showticks;
  SpinEdit6.value := FGraph.appearance.MaxSteps;
  SpinEdit7.value := FGraph.appearance.MinSteps;
  Case FGraph.appearance.gridstyle Of
    psSolid:gridstyle.itemindex := 0;
    psDot:gridstyle.itemindex := 1;
    psDash:gridstyle.itemindex := 2;
    psDashDot:gridstyle.itemindex :=3 ;
    psDashDotDot:gridstyle.itemindex := 4;
  End;
  Spinedit2.value := FGraph.Dimensions.printscalepct;
  spinedit3.value := FGraph.dimensions.PrintXOffsetPct;
  spinedit4.value := FGraph.dimensions.PrintYOffsetPct;
  checkbox4.checked := FGraph.appearance.PrintBkgndcolor;
  checkbox12.checked := FGraph.Appearance.CrossAtZero;
  spinedit14.value := FGraph.Appearance.CrossLength;

{axes page}
  currentaxis := FGraph.XAxis;
 {  1:currentaxis := FGraph.YAxis;}
{  2:currentaxis := FGraph.YAxis_Second;}
  updateaxespage;

  {legend page}
  updatelegendpage;

  editing := True;
End;

Procedure TGraphDesignerForm.updateaxespage;
Var oedit:Boolean;
Begin
  oedit := editing;
  editing := False;
  checkbox7.checked := currentaxis.showaxis;
  checkbox9.checked := currentaxis.gridlines;
  edit3.text := currentaxis.title;
  If currentaxis = FGraph.xaxis Then
    Begin
    Spinedit12.value := FGraph.dimensions.XAxisTitleOffset;
    Spinedit13.value := FGraph.dimensions.XAxisLabelOffset
    End
  Else
    Begin
    Spinedit12.value := FGraph.dimensions.YAxisTitleOffset;
    Spinedit13.value := FGraph.dimensions.YAxisLabelOffset;
    End;
  Case currentaxis.offsettype Of
    ao_minimum: combobox2.itemindex := 0;
    ao_Maximum: combobox2.itemindex := 1;
    ao_percentage: combobox2.itemindex := 2;
    ao_absolute: combobox2.itemindex := 3;
  End;
  If combobox2.itemindex > 1 Then
    edit2.Enabled := True
  Else
    edit2.Enabled := False;
  edit2.text := floattostrF(currentaxis.offset,ffFixed, 2,2);
  checkbox8.checked := currentaxis.Reversed;
  checkbox10.checked := currentaxis.autosizing;
  edit4.Enabled := Not checkbox10.checked;
  edit4.text := floattostrF(currentaxis.min,ffFixed, 2,2);
  edit5.Enabled := Not checkbox10.checked;
  edit5.text := floattostrF(currentaxis.max,ffFixed, 2,2);
  checkbox11.checked := currentaxis.autostepping;
  edit6.Enabled := Not checkbox11.checked;
  edit6.text := floattostrF(currentaxis.stepsize,ffFixed, 2,2);
  If currentaxis.logscale Then
    Begin
    radiobutton2.checked := True;
    RadioButton1Click(radiobutton2);
    End
  Else If currentaxis.showastime Then
    Begin
    radiobutton3.checked := True;
    RadioButton1Click(radiobutton3);
    End
  Else
    Begin
    radiobutton1.checked := True;
    RadioButton1Click(radiobutton1);
    End;
  Case currentaxis.LogCycleDivisions Of
     0:combobox3.itemindex := 0;
     1:combobox3.itemindex := 1;
     2:combobox3.itemindex := 2;
     5:combobox3.itemindex := 3;
     7:combobox3.itemindex := 4;
     9:combobox3.itemindex := 5;
    End;
  edit7.text := currentaxis.DateTimeFormat;
  Case currentaxis.DateTickType Of
    dt_minute:Combobox4.itemindex := 0;
    dt_5minute:Combobox4.itemindex := 1;
    dt_halfhourly:Combobox4.itemindex := 2;
    dt_hourly:Combobox4.itemindex := 3;
    dt_daily:Combobox4.itemindex := 4;
    dt_weekly:Combobox4.itemindex := 5;
    dt_monthly:Combobox4.itemindex := 6;
    dt_2monthly:Combobox4.itemindex := 7;
    dt_quarterly:Combobox4.itemindex := 8;
    dt_annually:Combobox4.itemindex := 9;
    dt_decade:Combobox4.itemindex := 10;
    dt_century:Combobox4.itemindex := 11;
    dt_custom:Combobox4.itemindex := 12;
  End;
  checkbox13.checked :=  currentaxis.AutoLabelDecimals;
  editing := oedit;
End;

Procedure TGraphDesignerForm.closebutClick(Sender: TObject);
Begin
  close;
End;

Procedure TGraphDesignerForm.FormCreate(Sender: TObject);
Begin
 editing := False;
(* {$IFNDEF STATISTICS}
 hideshape := TPanel.create(self);
 hideshape.parent := panel19;
 hideshape.top := 73;
 hideshape.left := 6;
 hideshape.width := panel19.width - (6 + 3);
 hideshape.height := panel19.height - (73 + 3);
 hideshape.bevelOuter := bvnone;
 {$ENDIF}*)
End;

Procedure TGraphDesignerForm.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
hideshape.Free;
End;


{----------------------------------------------------------------------------
    #1a. update graph
 ----------------------------------------------------------------------------}

Procedure TGraphDesignerForm.Edit1Change(Sender: TObject);
Begin
  If editing Then FGraph.appearance.graphtitle := edit1.text;
End;

Procedure TGraphDesignerForm.SpinEdit1Change(Sender: TObject);
Begin
 If editing Then FGraph.dimensions.graphtitleoffset := spinedit1.value;
End;

Procedure TGraphDesignerForm.Button3Click(Sender: TObject);
Begin
  fd.font := FGraph.appearance.captionfont;
  If fd.Execute Then FGraph.appearance.captionFont.Assign(fd.font);
End;

procedure TGraphDesignerForm.Button4Click(Sender: TObject);
begin
  ClipBoard.AsText := FGraph.settingsText(true);
end;

Procedure TGraphDesignerForm.Button7Click(Sender: TObject);
Begin
  cd.color := FGraph.appearance.BackgroundColor;
  If cd.Execute Then
    FGraph.appearance.backgroundcolor := cd.color;
End;

Procedure TGraphDesignerForm.SpinEdit8Change(Sender: TObject);
Begin
 If editing Then FGraph.dimensions.topmargin := spinedit8.value;
End;

Procedure TGraphDesignerForm.SpinEdit9Change(Sender: TObject);
Begin
 If editing Then FGraph.dimensions.bottommargin := spinedit9.value;
End;

Procedure TGraphDesignerForm.SpinEdit10Change(Sender: TObject);
Begin
 If editing Then FGraph.dimensions.leftmargin := spinedit10.value;
End;

Procedure TGraphDesignerForm.SpinEdit11Change(Sender: TObject);
Begin
 If editing Then FGraph.dimensions.rightmargin := spinedit11.value;
End;

Procedure TGraphDesignerForm.CheckBox3Click(Sender: TObject);
Begin
  If editing Then FGraph.appearance.plotoffgraph := checkbox3.checked;
End;

Procedure TGraphDesignerForm.Button11Click(Sender: TObject);
Begin
  cd.color := FGraph.Appearance.Margincolor;
  If cd.Execute Then FGraph.Appearance.Margincolor := cd.color;
End;

Procedure TGraphDesignerForm.Button9Click(Sender: TObject);
Begin
  cd.color := FGraph.appearance.axesColor;
  If cd.Execute Then
    FGraph.appearance.axescolor := cd.color;
End;

Procedure TGraphDesignerForm.Button1Click(Sender: TObject);
Begin
  fd.font := FGraph.appearance.Titlefont;
  If fd.Execute Then FGraph.appearance.Titlefont.assign(fd.font);
End;

Procedure TGraphDesignerForm.Button2Click(Sender: TObject);
Begin
  fd.font := FGraph.appearance.labelfont;
  If fd.Execute Then FGraph.appearance.labelfont.assign(fd.font);
End;

Procedure TGraphDesignerForm.CheckBox5Click(Sender: TObject);
Begin
 If editing Then FGraph.appearance.showgraphlabels := Checkbox5.checked;
End;

Procedure TGraphDesignerForm.CheckBox6Click(Sender: TObject);
Begin
 If editing Then FGraph.appearance.showticks := Checkbox6.checked;
End;

Procedure TGraphDesignerForm.SpinEdit5Change(Sender: TObject);
Begin
  If editing Then FGraph.dimensions.Ticklength := SpinEdit5.value;
End;

Procedure TGraphDesignerForm.SpinEdit6Change(Sender: TObject);
Begin
  If editing Then FGraph.appearance.MaxSteps := SpinEdit6.value;
End;

Procedure TGraphDesignerForm.SpinEdit7Change(Sender: TObject);
Begin
  If editing Then FGraph.appearance.MinSteps := SpinEdit7.value;
End;

Procedure TGraphDesignerForm.Button10Click(Sender: TObject);
Begin
 cd.color := FGraph.appearance.gridcolor;
 If cd.Execute Then FGraph.appearance.gridcolor := cd.color;
End;

Procedure TGraphDesignerForm.gridstyleChange(Sender: TObject);
Begin
  If Not editing Then Exit;
  Case gridstyle.itemindex Of
    0:FGraph.appearance.gridstyle := psSolid;
    1:FGraph.appearance.gridstyle := psDot;
    2:FGraph.appearance.gridstyle := psDash;
    3:FGraph.appearance.gridstyle := psDashDot;
    4:FGraph.appearance.gridstyle := psDashDotDot;
  End;
End;

Procedure TGraphDesignerForm.SpinEdit2Change(Sender: TObject);
Begin
  If editing Then FGraph.Dimensions.printscalepct := Spinedit2.value;
End;

Procedure TGraphDesignerForm.SpinEdit3Change(Sender: TObject);
Begin
  If editing Then FGraph.dimensions.PrintXOffsetPct := spinedit3.value;
End;

Procedure TGraphDesignerForm.SpinEdit4Change(Sender: TObject);
Begin
  If editing Then FGraph.dimensions.PrintYOffsetPct := spinedit4.value;
End;

Procedure TGraphDesignerForm.CheckBox4Click(Sender: TObject);
Begin
  If editing Then FGraph.appearance.PrintBkgndcolor := checkbox4.checked;
End;

Procedure TGraphDesignerForm.CheckBox7Click(Sender: TObject);
Begin
  If editing Then currentaxis.showaxis := checkbox7.checked;
End;

Procedure TGraphDesignerForm.CheckBox9Click(Sender: TObject);
Begin
  If editing Then currentaxis.gridlines := checkbox9.checked;
End;

Procedure TGraphDesignerForm.Edit3Change(Sender: TObject);
Begin
  If editing Then currentaxis.title := edit3.text;
End;

Procedure TGraphDesignerForm.SpinEdit12Change(Sender: TObject);
Begin
  If Not editing Then Exit;
  If currentaxis = FGraph.xaxis Then
    FGraph.dimensions.XAxisTitleOffset := Spinedit12.value
  Else
    FGraph.dimensions.YAxisTitleOffset := Spinedit12.value
End;

Procedure TGraphDesignerForm.SpinEdit13Change(Sender: TObject);
Begin
  If Not editing Then Exit;
  If currentaxis = FGraph.xaxis Then
    FGraph.dimensions.XAxisLabelOffset := Spinedit13.value
  Else
    FGraph.dimensions.YAxisLabelOffset := Spinedit13.value
End;

procedure TGraphDesignerForm.ComboBox1Change(Sender: TObject);
begin
  Case ComboBox1.ItemIndex Of
    0:currentaxis := FGraph.XAxis;
    1:currentaxis := FGraph.YAxis1;
    2:currentaxis := FGraph.YAxis2;
  End;
  updateaxespage;
end;

Procedure TGraphDesignerForm.ComboBox2Change(Sender: TObject);
Begin
  If combobox2.itemindex > 1 Then
    edit2.Enabled := True
  Else
    edit2.Enabled := False;
  If Not editing Then Exit;
  Case combobox2.itemindex Of
    0:currentaxis.offsettype := ao_minimum;
    1:currentaxis.offsettype := ao_Maximum;
    2:currentaxis.offsettype := ao_percentage;
    3:currentaxis.offsettype := ao_absolute;
  End;
End;

Procedure TGraphDesignerForm.CheckBox8Click(Sender: TObject);
Begin
  If editing Then currentaxis.Reversed := checkbox8.checked;
End;

Procedure TGraphDesignerForm.Edit2Change(Sender: TObject);
Begin
  If editing Then currentaxis.offset := strtofloat(edit2.text);
End;

Procedure TGraphDesignerForm.CheckBox10Click(Sender: TObject);
Begin
  edit4.Enabled := Not checkbox10.checked;
  edit5.Enabled := Not checkbox10.checked;
  If editing Then
    Begin
    currentaxis.autosizing := checkbox10.checked;
    edit4.text := floattostrF(currentaxis.min,ffFixed, 2,2);
    edit5.text := floattostrF(currentaxis.max,ffFixed, 2,2);
    If checkbox11.checked Then
       edit6.text := floattostrF(currentaxis.stepsize,ffFixed, 2,2);
    End;
End;

Procedure TGraphDesignerForm.Edit4Change(Sender: TObject);
Begin
  If editing Then
    Begin
    currentaxis.min := strtofloat(edit4.text);
    If checkbox11.checked Then
       edit6.text := floattostrF(currentaxis.stepsize,ffFixed, 2,2);
    End;
End;

Procedure TGraphDesignerForm.Edit5Change(Sender: TObject);
Begin
  If editing Then
    Begin
    currentaxis.max := strtofloat(edit5.text);
    If checkbox11.checked Then
       edit6.text := floattostrF(currentaxis.stepsize,ffFixed, 2,2);
    End;
End;

Procedure TGraphDesignerForm.CheckBox11Click(Sender: TObject);
Begin
  edit6.Enabled := Not checkbox11.checked;
  If editing Then
    Begin
    currentaxis.autostepping := checkbox11.checked;
    edit6.text := floattostrF(currentaxis.stepsize,ffFixed, 2,2);
    End;
End;

Procedure TGraphDesignerForm.Edit6Change(Sender: TObject);
Begin
  If editing Then
    currentaxis.stepsize := strtofloat(edit6.text);
End;

Procedure TGraphDesignerForm.RadioButton1Click(Sender: TObject);
Begin
  Case TComponent(sender).tag Of
    0:Begin
      combobox3.Enabled := False;
      combobox4.Enabled := False;
      edit7.Enabled := False;
      End;
    1:Begin
      combobox3.Enabled := True;
      combobox4.Enabled := False;
      edit7.Enabled := False;
      End;
    2:Begin
      combobox3.Enabled := False;
      combobox4.Enabled := True;
      edit7.Enabled := True;
      End;
  End;
  If Not editing Then Exit;
  Case TComponent(sender).tag Of
    0:Begin
      currentaxis.logscale := False;
      currentaxis.ShowAsTime := False;
      End;
    1:Begin
      currentaxis.ShowAsTime := False;
      currentaxis.logscale := True;
      End;
    2:Begin
      currentaxis.logscale := False;
      currentaxis.ShowAsTime := True;
      End;
  End;
End;

Procedure TGraphDesignerForm.ComboBox3Change(Sender: TObject);
Begin
  If editing Then
    Case combobox3.itemindex Of
     0:currentaxis.LogCycleDivisions := 0;
     1:currentaxis.LogCycleDivisions := 1;
     2:currentaxis.LogCycleDivisions := 2;
     3:currentaxis.LogCycleDivisions := 5;
     4:currentaxis.LogCycleDivisions := 7;
     5:currentaxis.LogCycleDivisions := 9;
    End;
End;

Procedure TGraphDesignerForm.Edit7Change(Sender: TObject);
Begin
  If editing Then currentaxis.DateTimeFormat := edit7.text;
End;

Procedure TGraphDesignerForm.ComboBox4Change(Sender: TObject);
Begin
  If Not editing Then Exit;
  Case Combobox4.itemindex Of
    00:currentaxis.DateTickType := dt_minute;
    01:currentaxis.DateTickType := dt_5minute;
    02:currentaxis.DateTickType := dt_halfhourly;
    03:currentaxis.DateTickType := dt_hourly;
    04:currentaxis.DateTickType := dt_daily;
    05:currentaxis.DateTickType := dt_weekly;
    06:currentaxis.DateTickType := dt_monthly;
    07:currentaxis.DateTickType := dt_2monthly;
    08:currentaxis.DateTickType := dt_quarterly;
    09:currentaxis.DateTickType := dt_annually;
    10:currentaxis.DateTickType := dt_decade;
    11:currentaxis.DateTickType := dt_century;
    12:currentaxis.DateTickType := dt_custom;
  End;
End;

Procedure TGraphDesignerForm.Button5Click(Sender: TObject);
Begin
  cd.color := FGraph.appearance.CrossColor;
  If cd.Execute Then
    FGraph.appearance.Crosscolor := cd.color;
End;

Procedure TGraphDesignerForm.CheckBox12Click(Sender: TObject);
Begin
 If editing Then FGraph.appearance.CrossAtZero := Checkbox12.checked;
End;

Procedure TGraphDesignerForm.SpinEdit14Change(Sender: TObject);
Begin
  If editing Then FGraph.Appearance.Crosslength := SpinEdit14.value;
End;

Procedure TGraphDesignerForm.CheckBox13Click(Sender: TObject);
Begin
  If editing Then currentaxis.AutoLabelDecimals := checkbox11.checked;
End;

Procedure TGraphDesignerForm.updateLegendPage;
Var
  oedit : Boolean;
  p : TFGraphSeries;
Begin
  ListBox1.items.clear;
  oedit := editing;
  editing := False;
  checkbox15.checked := FGraph.legend.Visible;
  spinedit16.value := FGraph.legend.left;
  spinedit17.value := FGraph.legend.top;
  spinedit18.value := FGraph.legend.width;
  spinedit19.value := FGraph.legend.height;
  spinedit20.value := FGraph.legend.xmargin;
  spinedit21.value := FGraph.legend.ymargin;
  spinedit22.value := FGraph.legend.symbolspace;
  Case FGraph.legend.Layout Of
    lsAcross:radiobutton4.checked := True;
    lsDown  :radiobutton5.checked := True;
  End;
  checkbox16.checked := (FGraph.legend.Borderstyle = psSolid);
  combobox8.Enabled := FGraph.Series.Count > 0;
  for p in FGraph.Series do
    listbox1.items.addobject(p.data.Name, p);
  If listbox1.itemindex < 0 Then
    listbox1.itemindex := 0;
  If combobox8.Enabled Then
    Case TFGraphSeries(listbox1.items.objects[listbox1.itemindex]).LegendStatus Of
      lsNone    :combobox8.itemindex := 0;
      lsNameOnly:combobox8.itemindex := 1;
      lsPoints  :combobox8.itemindex := 2;
      lsLines   :combobox8.itemindex := 3;
      lsAll     :combobox8.itemindex := 4;
    End;
  editing := oedit;
End;

Procedure TGraphDesignerForm.CheckBox15Click(Sender: TObject);
Begin
  If editing Then FGraph.legend.Visible := checkbox15.checked;
End;

Procedure TGraphDesignerForm.SpinEdit16Change(Sender: TObject);
Begin
  If editing Then  FGraph.legend.left := spinedit16.value;
End;

Procedure TGraphDesignerForm.SpinEdit17Change(Sender: TObject);
Begin
  If editing Then FGraph.legend.top := spinedit17.value;
End;

Procedure TGraphDesignerForm.SpinEdit18Change(Sender: TObject);
Begin
  If editing Then FGraph.legend.width := spinedit18.value;
End;

Procedure TGraphDesignerForm.SpinEdit19Change(Sender: TObject);
Begin
  If editing Then FGraph.legend.height := spinedit19.value;
End;

Procedure TGraphDesignerForm.SpinEdit20Change(Sender: TObject);
Begin
  If editing Then FGraph.legend.xmargin := spinedit20.value;
End;

Procedure TGraphDesignerForm.SpinEdit21Change(Sender: TObject);
Begin
  If editing Then FGraph.legend.ymargin := spinedit21.value;
End;

Procedure TGraphDesignerForm.SpinEdit22Change(Sender: TObject);
Begin
  If editing Then FGraph.legend.symbolspace := spinedit22.value;
End;

Procedure TGraphDesignerForm.RadioButton4Click(Sender: TObject);
Begin
  If Not editing Then Exit;
  If radiobutton4.checked Then
    FGraph.legend.Layout := lsAcross
  Else
    FGraph.legend.layout := lsDown;
End;

Procedure TGraphDesignerForm.CheckBox16Click(Sender: TObject);
Begin
 If editing Then
   If checkbox16.checked Then
     FGraph.legend.Borderstyle := psSolid
   Else
     FGraph.legend.Borderstyle := psClear;
End;

Procedure TGraphDesignerForm.Button15Click(Sender: TObject);
Begin
  fd.font := FGraph.legend.font;
  If fd.Execute Then FGraph.legend.font.assign(fd.font);
End;

Procedure TGraphDesignerForm.Button6Click(Sender: TObject);
Begin
  cd.color := FGraph.legend.Color;
  If cd.Execute Then
    FGraph.legend.color := cd.color;
End;

Procedure TGraphDesignerForm.ListBox1Click(Sender: TObject);
Begin
  If editing And combobox8.Enabled Then
    Case TFGraphSeries(listbox1.items.objects[listbox1.itemindex]).LegendStatus Of
      lsNone    :combobox8.itemindex := 0;
      lsNameOnly:combobox8.itemindex := 1;
      lsPoints  :combobox8.itemindex := 2;
      lsLines   :combobox8.itemindex := 3;
      lsAll     :combobox8.itemindex := 4;
    End;
End;

Procedure TGraphDesignerForm.ComboBox8Change(Sender: TObject);
Begin
  If Not editing Then
    Exit;
  Case combobox8.itemindex Of
    0 : TFGraphSeries(listbox1.items.objects[listbox1.itemindex]).LegendStatus := lsNone;
    1 : TFGraphSeries(listbox1.items.objects[listbox1.itemindex]).LegendStatus := lsnameonly;
    2 : TFGraphSeries(listbox1.items.objects[listbox1.itemindex]).LegendStatus := lsPoints;
    3 : TFGraphSeries(listbox1.items.objects[listbox1.itemindex]).LegendStatus := lsLines;
    4 : TFGraphSeries(listbox1.items.objects[listbox1.itemindex]).LegendStatus := lsAll;
  End;
End;

Function designGraph(AOwner:TComponent; Graph : TFGraph) : Boolean;
Begin
  Result := True;
  GraphDesignerForm := TGraphDesignerForm.Create(AOwner);
  Try
    GraphDesignerForm.Graph := Graph;
    GraphDesignerForm.showmodal;
  Finally
    GraphDesignerForm.Free;
  End;
End;

End.

