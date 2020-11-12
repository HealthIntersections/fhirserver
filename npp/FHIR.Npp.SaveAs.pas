unit FHIR.Npp.SaveAs;

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

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FHIR.Npp.Form,
  fsl_base, fsl_utilities,
  fhir_objects, fhir_oauth, fhir_client, fhir_utilities, fhir_parser,
  FHIR.Npp.Settings, FHIR.Npp.Context;

type
  TSaveOnServerDialog = class(TNppForm)
    Panel1: TPanel;
    Label4: TLabel;
    cbxServers: TComboBox;
    btnSerevrs: TButton;
    Panel2: TPanel;
    btnCancel: TButton;
    btnSend: TButton;
    Panel3: TPanel;
    rbPost: TRadioButton;
    rbPut: TRadioButton;
    cbUpdate: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSerevrsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    FContext : TFHIRNppContext;
    FServers : TFslList<TRegisteredFHIRServer>;
    FResource: TFHIRResourceV;
    FSource: TBytes;
    FFormat: TFHIRFormat;
    procedure SetResource(const Value: TFHIRResourceV);
    procedure loadServers;
    procedure SetContext(const Value: TFHIRNppContext);
  public
    property resource : TFHIRResourceV read FResource write SetResource;
    property source : TBytes read FSource write FSource;
    property format : TFHIRFormat read FFormat write FFormat;
    property context : TFHIRNppContext read FContext write SetContext;
  end;

var
  SaveOnServerDialog: TSaveOnServerDialog;

implementation

{$R *.dfm}

uses
  FHIR.Npp.Plugin;

procedure TSaveOnServerDialog.btnSendClick(Sender: TObject);
var
  rs : TRegisteredFHIRServer;
  vf : TFHIRNppVersionFactory;
  conn : TFHIRNppServerConnection;
  id : String;
  rSend, rRecv :  TFHIRResourceV;
  p : TFHIRParser;
  c : TFHIRComposer;
begin
  rs := cbxServers.Items.Objects[cbxServers.ItemIndex] as TRegisteredFHIRServer;
  vf := FContext.Version[rs.version];
  if not FContext.connections.TryGetValue(rs.fhirEndpoint, conn) then
  begin
    conn := TFHIRNppServerConnection.Create;
    try
      if rs.format = ffUnspecified then
        conn.client := vf.Factory.makeClient(vf.Worker.link, rs.fhirEndpoint, fctWinInet, ffJson, 5000)
      else
        conn.client := vf.Factory.makeClient(vf.Worker.link, rs.fhirEndpoint, fctWinInet, rs.format, 5000);
      conn.statement := vf.Factory.wrapCapabilityStatement(conn.client.conformanceV(false));
      FContext.connections.Add(rs.fhirEndpoint, conn.Link);
    finally
      conn.free;
    end;
  end;
  if Resource.fhirObjectVersion = conn.client.version then
    rSend := Resource.link
  else
  begin
    p := vf.makeParser(Format);
    try
      rSend := p.parseResource(source);
    finally
      p.free;
    end;
  end;
  try
    if rbPost.Checked then
    begin
      rSend.id := '';
      rRecv := conn.client.createResourceV(resource, id);
      if rRecv = nil then
        rRecv := rSend.link;
      try
        rRecv.id := id;
        resource := rRecv.link;
        ShowMessage('Posted to '+UrlPath([conn.client.address, rRecv.fhirType, id]));
      finally
        rRecv.Free;
      end;
    end
    else
    begin
      rRecv := conn.client.updateResourceV(rSend);
      if rRecv <> nil then
        resource := rRecv;
      SoundBeepOK;
    end;
  finally
    rSend.free;
  end;
end;

procedure TSaveOnServerDialog.btnSerevrsClick(Sender: TObject);
begin
  FNpp.FuncSettings(true);
  loadServers;
end;

procedure TSaveOnServerDialog.FormCreate(Sender: TObject);
begin
  FServers := TFslList<TRegisteredFHIRServer>.create;
end;

procedure TSaveOnServerDialog.FormDestroy(Sender: TObject);
begin
  FResource.free;
  FServers.Free;
  FContext.Free;
end;

procedure TSaveOnServerDialog.FormShow(Sender: TObject);
begin
  loadServers;
  if resource.Id <> '' then
  begin
    rbPost.Caption := 'Server Assgned ID (ignore existing id "'+resource.id+'")';
    rbPut.Caption := 'Use Existing ID "'+resource.id+'" (server may reject this)'
  end
  else
  begin
    rbPost.Caption := 'Server Assgned ID (ignore existing id "'+resource.id+'")';
    rbPut.Caption := 'Use Existing ID (no id, so not possible)';
    rbPut.enabled := false;
  end;
  cbUpdate.Checked := Settings.updateResourceOnSend;
end;

procedure TSaveOnServerDialog.loadServers;
var
  rs : TRegisteredFHIRServer;
begin
  FServers.Clear;
  cbxServers.Items.Clear;
  Settings.ListServers('', FServers);
  for rs in FServers do
    cbxServers.Items.addObject(rs.name + ': '+rs.fhirEndpoint, rs);
  cbxServers.ItemIndex := 0;
  btnSend.enabled := cbxServers.ItemIndex > -1;
end;

procedure TSaveOnServerDialog.SetContext(const Value: TFHIRNppContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TSaveOnServerDialog.SetResource(const Value: TFHIRResourceV);
begin
  FResource.Free;
  FResource := Value;
end;

end.
