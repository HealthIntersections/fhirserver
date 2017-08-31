unit ServerForm;

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
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ListBox, FMX.Layouts,
  FHIRClient,
  AppEndorserForm;

type
  TServerFrameForm = class (TFrame)
    btnTest: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    pnlSearch: TPanel;
    Splitter1: TSplitter;
    Label2: TLabel;
    cbxSearchType: TComboBox;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Panel2: TPanel;
    ListBox1: TListBox;
    procedure btnTestClick(Sender: TObject);
  private
    FClient: TFHIRClient;
    FTabs : TTabControl;
    procedure SetClient(const Value: TFHIRClient);
    { Private declarations }
  public
    { Public declarations }
    Destructor Destroy; override;
    property Client : TFHIRClient read FClient write SetClient;
    property Tabs : TTabControl read FTabs write FTabs;
  end;

implementation

{$R *.fmx}

{ TServerFrameForm }

procedure TServerFrameForm.btnTestClick(Sender: TObject);
var
  tab : TTabItem;
  appForm : TAppEndorsementForm;
begin
  tab := FTabs.Add(TTabItem);
  FTabs.ActiveTab := tab;
  tab.Text := 'AppEndorser for '+FClient.address;
  appForm := TAppEndorsementForm.create(tab);
  appForm.Parent := tab;
//  appForm.Align := TAlignLayout.Client;
//  appForm.Client := client.link;
end;

destructor TServerFrameForm.Destroy;
begin
  FClient.free;
  inherited;
end;

procedure TServerFrameForm.SetClient(const Value: TFHIRClient);
begin
  FClient.free;
  FClient := Value;
end;

end.
