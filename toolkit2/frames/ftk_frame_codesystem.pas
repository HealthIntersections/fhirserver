unit ftk_frame_codesystem;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, EditBtn, Buttons, Types,
  fui_lcl_managers,
  ftk_frame_resource;

type
  TFrame = TResourceDesignerFrame;

  { TCodeSystemFrame }

  TCodeSystemFrame = class (TFrame)
    btnAddIdentifier: TButton;
    btnDeleteIdentifier: TButton;
    btnConcepts: TButton;
    chkExperimental: TCheckBox;
    chkCase: TCheckBox;
    chkGrammar: TCheckBox;
    chkNeedsVersion: TCheckBox;
    cbxStatus: TComboBox;
    cbxJurisdiction: TComboBox;
    cbxHeirarchy: TComboBox;
    cbxContent: TComboBox;
    edtDate: TDateEdit;
    edtURL: TEdit;
    edtName: TEdit;
    edtConceptCount: TEdit;
    edtVersion: TEdit;
    editTitle: TEdit;
    edtPublisher: TEdit;
    edtDescription: TEdit;
    edtPurpose: TEdit;
    edtCopyright: TEdit;
    edtValueSet: TEdit;
    edtSupplements: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lvIdentifiers: TListView;
    PageControl1: TPageControl;
    btnName: TSpeedButton;
    btnPublisher: TSpeedButton;
    btnDescription: TSpeedButton;
    btnPurpose: TSpeedButton;
    btnCopyright: TSpeedButton;
    TabSheet1: TTabSheet;
    procedure GroupBox1Click(Sender: TObject);
    procedure GroupBox3Click(Sender: TObject);
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  private
    FManager : TObjectManager;
  public
    procedure initialize; override;

  end;

implementation

{$R *.lfm}

{ TCodeSystemFrame }

procedure TCodeSystemFrame.TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TCodeSystemFrame.initialize;
begin
  FManager := TObjectManager.Create;
  //FManager.registerControl('url', edtURL);
  //FManager.registerControl('name', edtName, btnName);
  //FManager.registerControl('version', edtVersion);
  //FManager.registerControl('title', editTitle);
  //
  //FManager.registerControl('status', cbxStatus);
  //FManager.registerControl('experimental', chkExperimental);
  //FManager.registerControl('jurisdiction', cbxJurisdiction);
  //FManager.registerControl('date', edtDate);
  //FManager.registerControl('publisher', edtPublisher, btnPublisher);
  //FManager.registerControl('description', edtDescription, btnDescription);
  //FManager.registerControl('purpose', edtPurpose, btnPurpose);
  //FManager.registerControl('copyright', edtCopyright, btnCopyright);
  //
  //FManager.registerControl('valueSet', edtValueSet);
  //FManager.registerControl('count', edtConceptCount);
  //FManager.registerControl('supplements', edtSupplements);
  //FManager.registerControl('heirarchy', cbxHeirarchy);
  //FManager.registerControl('content', cbxContent);
  //
  //FManager.registerControl('caseSensitive', chkCase);
  //FManager.registerControl('grammar', chkGrammar);
  //FManager.registerControl('needsVersion', chkNeedsVersion);
end;

procedure TCodeSystemFrame.GroupBox1Click(Sender: TObject);
begin

end;

procedure TCodeSystemFrame.GroupBox3Click(Sender: TObject);
begin

end;

end.

