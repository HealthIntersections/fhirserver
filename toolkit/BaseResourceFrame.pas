unit BaseResourceFrame;

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
  FMX.ListBox, FMX.Edit, FMX.TabControl, FMX.TreeView, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Platform,
  fhir_objects, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, FHIR.Version.Organiser,
  BaseServerFrame;

type
  TBaseResourceFrame = class(TBaseServerFrame)
  private
    FFilename : String;
    FResource : TFHIRResource;
    FOriginal : TFHIRResource;
    FFormat: TFHIRFormat;
    FInputIsDirty: boolean;
    FResourceIsDirty: boolean;
    FLoading: boolean;
    procedure SetResource(const Value: TFHIRResource);
  public
    destructor Destroy; override;

    property Filename : String read FFilename write FFilename;
    property Format : TFHIRFormat read FFormat write FFormat;
    property Resource : TFHIRResource read FResource write SetResource;
    function canSave : boolean; override;
    function canSaveAs : boolean; override;
    function save : boolean; override;
    function saveAs(filename : String; format : TFHIRFormat) : boolean; override;
    function isDirty : boolean; override;
    function nameForSaveDialog : String; override;
    function hasResource : boolean; override;
    function currentResource : TFHIRResource; override;
    function originalResource : TFHIRResource; override;

    procedure commit; virtual;
    procedure cancel; virtual;

    procedure organise; override;
    function canOrganise : boolean; override;

    Property ResourceIsDirty : boolean read FResourceIsDirty write FResourceIsDirty;
    Property InputIsDirty : boolean read FInputIsDirty write FInputIsDirty;
    Property Loading : boolean read FLoading write FLoading;
  end;
  TBaseResourceFrameClass = class of TBaseResourceFrame;

implementation

{ TBaseResourceFrame }

procedure TBaseResourceFrame.cancel;
begin
  // nothing
end;

function TBaseResourceFrame.canOrganise: boolean;
var
  org : TFHIRResourceOrganiser;
begin
  org := TFHIRResourceOrganiser.create;
  try
    result := org.canOrganise(Fresource.ResourceType);
  finally
    org.free;
  end;
end;

function TBaseResourceFrame.canSave: boolean;
begin
  result := ((Filename <> '') and (Filename <> '$$')) or (Client <> nil);
end;

function TBaseResourceFrame.canSaveAs: boolean;
begin
  result := true;
end;

procedure TBaseResourceFrame.commit;
begin
  // nothing
end;

function TBaseResourceFrame.currentResource: TFHIRResource;
begin
  result := FResource;
end;

destructor TBaseResourceFrame.Destroy;
begin
  FResource.Free;
  FOriginal.Free;
  inherited;
end;

function TBaseResourceFrame.hasResource: boolean;
begin
  result := true;
end;

function TBaseResourceFrame.isDirty: boolean;
begin
  result := FResourceIsDirty or FInputIsDirty;
end;

function TBaseResourceFrame.nameForSaveDialog: String;
begin
  if Filename <> '' then
    result := filename
  else
    result := 'new '+resource.fhirType;
end;

procedure TBaseResourceFrame.organise;
var
  org : TFHIRResourceOrganiser;
begin
  org := TFHIRResourceOrganiser.create;
  try
    if org.canOrganise(Fresource.ResourceType) then
    begin
      org.organise(FResource);
      reload;
      ResourceIsDirty := true;
    end;
  finally
    org.free;
  end;

end;

function TBaseResourceFrame.originalResource: TFHIRResource;
begin
  result := FOriginal;
end;

function TBaseResourceFrame.save : boolean;
var
  res : TFhirResource;
begin
  if InputIsDirty then
    commit;
  result := FFilename <> '';
  if result then
    if FFilename = '$$' then
    begin
      res := Client.updateResource(FResource);
      try
        FResource.Free;
        FResource := res.Link;
        load;
      finally
        res.free;
      end;
    end
    else
      resourceToFile(resource, FFilename, format, OutputStylePretty);
  ResourceIsDirty := false;
end;

function TBaseResourceFrame.saveAs(filename: String; format: TFHIRFormat): boolean;
var
  tab : TTabItem;
begin
  self.Filename := filename;
  self.Format := Format;
  result := save;
  tab := TTabItem(tagObject);
  tab.Text := ExtractFileName(filename);
  tab.Hint := filename;
end;

procedure TBaseResourceFrame.SetResource(const Value: TFHIRResource);
begin
  FResource.Free;
  FResource := Value;
  FOriginal := FResource.Clone;
end;

end.
