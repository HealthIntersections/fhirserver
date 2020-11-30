Unit FHIR.WP.Snomed;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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


{
Snomed Client for HL7Connect Snomed Server.

Needs an HL7Connect, configured with Snomed Content. This
client needs a URL, a username, and a password.

In addition, the client needs an INI file which it uses for local
storage. This will be refactored soon.

Also, there is a NLP provider. This will soon be refactored to be provided
internally as well.
}

Interface

Uses
  Windows, SysUtils, Messages, IniFiles, Classes, ComCtrls, Graphics, Controls, commctrl,
  fsl_base, fsl_threads, fsl_utilities, fsl_collections, fsl_stream,
  fui_vclx_Images, fui_vclx_Forms, fui_vclx_Controls,
  FHIR.WP.Control;

 const
  UM_SNOMED_SEARCH_DONE = WM_USER + 4;


Type
  TSnomedClient = class (TFslObject)
  private
    FIni : TIniFile;
    FLock : TFslLock;
    FSnomedDescriptions : TFslStringMatch;

    FPassword: String;
    FUsername: String;
    FServiceURL: String;

    FSessionId : Integer;
    FSnomedVersion : String;
    FLangSet : String;

    FHandle : TThreadHandle; // Handle to the Windows thread.
    FID : TThreadID;      // Unique ID of the Windows thread.
    FError : String;

    FCustomerHandle : THandle;
    FText : String;
    FCategory : Integer;

    procedure SetPassword(const Value: String);
    procedure SetServiceURL(const Value: String);
    procedure SetUsername(const Value: String);

{

      FSnomedServiceURL: String;
    FSnomedServiceURL: String;
    FSearch : TSnomedSearcher;




      Procedure CheckLogin;
      }

    Procedure Connect;
    Procedure Disconnect;
    Procedure TerminateAllThreads;
    Procedure Execute;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TSnomedClient; Overload;

    Property ServiceURL : String read FServiceURL write SetServiceURL;
    Property Username : String read FUsername write SetUsername;
    Property Password : String read FPassword write SetPassword;
    Property Ini : TIniFile read FIni;
{
    Procedure Clear;     }

  End;

Type
  TSnomedPickerContext = (spcAll, spcProcedure, spcOrganism, spcFinding, spcBodyStructure, spcSpecimen, spcSituation, spcProduct);

Const
  NAMES_PICKER_CONTEXTS : Array [TSnomedPickerContext] Of String = ('All Concepts', 'Procedures', 'Organisms', 'Findings', 'Body Structures', 'Specimens', 'Situations', 'Products');

Type
  TSnomedPicker = Class (TUixForm)
  Private
    FImages : TUixImages;
    FpnlLeft : TUixPanel;
    FpnlInput : TUixPanel;
    FedtInput : TUixEdit;
    FPnlOutput : TUixPanel;
    FtabsCategories : TTabControl;
    // FviewMatches : TUixTreeView;
    FViewMatches : TListView;

    FpnlRight : TUixPanel;
    FpnlTools : TUixToolBar;
    FbtnFilter : TUixToolButton;
    FbtnSelect : TUixToolButton;
    FbtnBackward : TUixToolButton;
    FbtnForward : TUixToolButton;

    FwrdBrowser : TWordProcessor;

    FFirstTerm : String;
    FFirstText : String;
    FFirstSearch : Boolean;
    FTerm: String;
    FText: String;
    FError : String;
    FClient : TSnomedClient;
//    FMatches : TSnomedMatches;
    FWaiting : Boolean;
    FTerms : TStringList;
    FTermIndex : Integer;
    FContext: TSnomedPickerContext;

    Procedure SetText(Const Value: String);
    Procedure SetTerm(Const Value: String);

    Procedure HandleInput(oSender : TObject);
    Procedure SnomedSearchDone(Var Msg: TMessage); Message UM_SNOMED_SEARCH_DONE;
    Procedure SetClient(Const Value: TSnomedClient);
//    Procedure MatchesGetNode(oSender : TObject; oNode : TUixTreeViewNode);
//    Procedure MatchesClick(oSender : TObject; oNode : TUixTreeViewNode);
    Procedure ListViewMatchesClick(oSender : TObject);
    Procedure GetViewTip(Sender: TObject; Item: TListItem; Var InfoTip: String);
    Procedure HotSpot(oSender : TWordProcessor; oInfo : TWPHotspotInformation);
    Procedure ViewTerm(Const sTerm : String);
    Procedure ClickForward(oSender : TObject);
    Procedure ClickBackward(oSender : TObject);
    Procedure ClickHome(oSender : TObject);
    Procedure ClickFilter(oSender : TObject);
//    Procedure ClickLabel(oSender : TObject);
    Procedure SetContext(Const Value: TSnomedPickerContext);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;
  Public
    // Access to the Snomed Infrastructure. Needs an HL7Connect behind it.
    Property Client : TSnomedClient Read FClient Write SetClient;

    // the selected Snomed Term, if one. Optional when opening the
    // dialog. Will always be populated if the dialog.execute returns ok
    Property Term : String Read FTerm Write SetTerm;

    // The text selected associated with the text. This is not usually the
    // display text associated with the term - instead it's some other text
    // in the context that should drive the choice of the term. It's still
    // worth providing this even when a term has already been chosen
    Property Text : String Read FText Write SetText;

    // The context of use - what kind of code is required (i.e. organism)
    // In some contexts (i.e. the word processor) the context
    // note: this will be refactored for proper reference set support in the future
    Property Context : TSnomedPickerContext Read FContext Write SetContext;

  End;



Implementation


{ TSnomedClient }

constructor TSnomedClient.Create;
begin
  inherited;
  FLock := TFslLock.Create;
  FSnomedDescriptions := TFslStringMatch.Create;
  FIni := TIniFile.Create('c:\temp\snomed.ini');
  FServiceURL := 'http://localhost:9556/soap';
  FUsername := 'g';
  FPassword := 'g';
end;

destructor TSnomedClient.Destroy;
begin
  Disconnect;
  FSnomedDescriptions.Free;
  FLock.Free;
  FIni.Free;
  inherited;
end;


procedure TSnomedClient.Connect;
var
  i : Integer;
begin
  if FSessionId = 0 then
  Begin
  End;
end;


procedure TSnomedClient.Disconnect;
begin
  Try
    if FSessionId <> 0 Then
    Begin
      TerminateAllThreads;
      FSessionId := 0;
    End;
  Except
  End;
end;

procedure TSnomedClient.SetPassword(const Value: String);
begin
  FPassword := Value;
  Disconnect;
end;

procedure TSnomedClient.SetServiceURL(const Value: String);
begin
  FServiceURL := Value;
  Disconnect;
end;

procedure TSnomedClient.SetUsername(const Value: String);
begin
  FUsername := Value;
  Disconnect;
end;

procedure TSnomedClient.TerminateAllThreads;
begin

end;

(*

{ TSnomedSearcher }

procedure TSnomedSearcher.Clear;
begin
  FLock.Lock;
  Try
    if FHandle <> 0 Then
      TerminateThread(FHandle, 0);
    FError := '';
  Finally
    FLock.Unlock;
  End;
end;

constructor TSnomedSearcher.Create;
begin
  inherited;
  FLock := TFslLock.Create;
end;

destructor TSnomedSearcher.Destroy;
begin
  Clear;
  FLock.Free;
  inherited;
end;

*)

procedure TSnomedClient.Execute;
var
  sText : String;
  iCat : Integer;
  aHandle : THandle;
begin
  Try
    Repeat
      FLock.Lock;
      Try
        sText := FText;
        FText := '';
        iCat := FCategory;
        FCategory := 0;
        aHandle := FCustomerHandle;
        FCustomerHandle := 0;
      Finally
        FLock.Unlock;
      End;
      if aHandle <> 0 Then
      Begin
        Try
        Except
          on e:exception do
            FError := e.Message;
        End;
        PostMessage(aHandle, UM_SNOMED_SEARCH_DONE, 0, 0);
      End;
    Until sText = '';
  Finally
    CloseHandle(FHandle);
    FHandle := 0;
    FId := 0;
  End;
end;


function TSnomedClient.Link: TSnomedClient;
begin
  result := TSnomedClient(Inherited Link);
end;


{$R resources\FHIR.WP.Snomed.res}

{ TSnomedPicker }

Procedure TSnomedPicker.Initialise;
Var
  oSplitter : TUixSplitter;
Begin
  Inherited;

  HasClientPanel := True;
  HasBottomPanel := True;
  HasOKButton := True;
  HasCancelButton := True;

  FTerms := TStringList.Create;
  FTermIndex := -1;

  FImages := TUixImages.Create(Nil);
  FImages.LoadBitmapFromResource('SnomedUIImages', clFuchsia);
  FImages.Name := 'WPToolbarImageList';

  FFirstSearch := True;
  Caption := 'Snomed Code Chooser';


  FpnlLeft := TUixPanel.Create(ClientPanel);
  FpnlLeft.Parent := pnlClient;
  FpnlLeft.AlignLeft;

  FpnlInput := TUixPanel.Create(FpnlLeft);
  FpnlInput.Parent := FpnlLeft;
  FpnlInput.Height := 24;
  FpnlInput.AlignTop;
  FpnlInput.Caption := '  Filter:';
  FpnlInput.Alignment := taLeftJustify;

  FedtInput := TUixEdit.Create(FpnlInput);
  FedtInput.Parent := FpnlInput;
  FedtInput.Left := 50;
  FedtInput.Top := 1;
  FedtInput.height := 24;
  FedtInput.Width := FpnlInput.width - 52;
  FedtInput.AnchoredRight := True;
  FedtInput.OnChange := HandleInput;

  FPnlOutput := TUixPanel.Create(FpnlLeft);
  FPnlOutput.Parent := FpnlLeft;
  FPnlOutput.AlignClient;

  FtabsCategories := TTabControl.Create(FPnlOutput);
  FtabsCategories.Parent := FPnlOutput;
  FtabsCategories.Align := alTop;
  FtabsCategories.Height := 20;
  FtabsCategories.Tabs.Add('Common');
  FtabsCategories.Tabs.Add('All');
  FtabsCategories.Tabs.Add('Procedure');
  FtabsCategories.Tabs.Add('Organism');
  FtabsCategories.Tabs.Add('Finding');
  FtabsCategories.Tabs.Add('Body Structure');
  FtabsCategories.Tabs.Add('Specimen');
  FtabsCategories.Tabs.Add('Situation');
  FtabsCategories.Tabs.Add('Product');
  FtabsCategories.OnChange := HandleInput;
                              {
  FviewMatches := TUixTreeView.Create(FPnlOutput);
  FviewMatches.Parent := FPnlOutput;
  FviewMatches.AlignClient;
  FviewMatches.Columns.Add('Term');
  FviewMatches.Columns.Add('Description').Width := 400;
  FviewMatches.Columns.Add('Rating').Width := 60;
  FviewMatches.Columns.Add('Fully Specified Name').Width := 400;
  FviewMatches.OnGetNode := MatchesGetNode;
  FviewMatches.OnSingleClick := MatchesClick;
                               }
  FviewMatches := TListView.Create(FPnlOutput);
  FviewMatches.Parent := FPnlOutput;
  FviewMatches.align := alClient;
  FviewMatches.ViewStyle := vsList;
  FviewMatches.MultiSelect := False;
  ListView_SetColumnWidth(FViewmatches.Handle, 0, 350);
  FviewMatches.OnClick := ListViewMatchesClick;
  FviewMatches.Font.Size := 10;
  FviewMatches.OnInfoTip := GetViewTip;
  oSplitter := TUixSplitter.Create(ClientPanel);
  oSplitter.Parent := ClientPanel;
  oSplitter.AlignLeft;
  oSplitter.ShuffleRight;

  FpnlRight := TUixPanel.Create(ClientPanel);
  FpnlRight.Parent := pnlClient;
  FpnlRight.AlignClient;

  FpnlTools := TUixToolBar.Create(FpnlRight);
  FpnlTools.Parent := FpnlRight;
  FpnlTools.Height := 24;
  FpnlTools.AlignTop;
  FpnlTools.Images := FImages;


  FbtnForward := TUixToolButton.Create(FpnlTools);
  FbtnForward.Parent := FpnlTools;
  FbtnForward.ImageIndex := 31;
  FbtnForward.Hint := 'Forwards';
  FbtnForward.OnClick := ClickForward;

  FbtnBackward := TUixToolButton.Create(FpnlTools);
  FbtnBackward.Parent := FpnlTools;
  FbtnBackward.ImageIndex := 1;
  FbtnBackward.Hint := 'Back';
  FbtnBackward.OnClick := ClickBackward;

  FbtnSelect := TUixToolButton.Create(FpnlTools);
  FbtnSelect.Parent := FpnlTools;
  FbtnSelect.ImageIndex := 40;
  FbtnSelect.Hint := 'Back to Start';
  FbtnSelect.OnClick := ClickHome;

  FbtnFilter := TUixToolButton.Create(FpnlTools);
  FbtnFilter.Parent := FpnlTools;
  FbtnFilter.ImageIndex := 64;
  FbtnFilter.Hint := 'Search';
  FbtnFilter.OnClick := ClickFilter;



  FwrdBrowser := TWordProcessor.Create(FpnlRight);
  FwrdBrowser.parent := FpnlRight;
  FwrdBrowser.AlignClient;
  FwrdBrowser.Settings.ModeBrowser;
  FwrdBrowser.Settings.TextWrapWidth := 120;
  FwrdBrowser.OnHotSpot := HotSpot;
End;

Procedure TSnomedPicker.SetText(Const Value: String);
Begin
  If FFirstText = '' Then
    FFirstText := Value;
  FText := Value;
  FedtInput.Text := Value;
End;

Procedure TSnomedPicker.HandleInput(oSender: TObject);
Begin
  FWaiting := True;
  FviewMatches.Items.Clear;
  FviewMatches.Refresh;
//  FClient.Search(Handle, FedtInput.Text, FtabsCategories.TabIndex);
End;


Procedure TSnomedPicker.Finalise;
Begin
  FClient.ini.WriteInteger('picker', 'left-width', FpnlLeft.Width);
  FClient.ini.WriteInteger('picker', 'width', ClientWidth);
  FClient.ini.WriteInteger('picker', 'height', ClientHeight);
  FClient.ini.WriteInteger('picker', 'top', Top);
  FClient.ini.WriteInteger('picker', 'left', left);
  FClient.ini.WriteInteger('picker', 'tab', FtabsCategories.TabIndex);

//  If IsModalOK Then
//    FClient.UseTerm(Term);
//  FClient.Free;
//  FMatches.Free;
  FImages.Free;
  FTerms.Free;

  Inherited;
End;


Procedure TSnomedPicker.SetClient(Const Value: TSnomedClient);
Begin
  FClient.Free;
  FClient := Value;
  ClientWidth := FClient.ini.ReadInteger('picker', 'width', 1000);
  ClientHeight := FClient.ini.ReadInteger('picker', 'height', 400);
  If FClient.Ini.ValueExists('picker', 'top') Then
    Top := FClient.ini.ReadInteger('picker', 'top', 0);
  If FClient.Ini.ValueExists('picker', 'left') Then
    left := FClient.ini.ReadInteger('picker', 'left', 0);
  FpnlLeft.Width := FClient.ini.ReadInteger('picker', 'left-width', 590);
  FtabsCategories.TabIndex := FClient.ini.ReadInteger('picker', 'tab', 0);
End;

Procedure TSnomedPicker.SnomedSearchDone(Var Msg: TMessage);
Var
  i : Integer;
  oItem : TListItem;
Begin
  FWaiting := False;
  Try
//    FMatches.Free;
//    FMatches := FClient.AcquireResults;
  Except
    On e:Exception Do
    Begin
      FError := e.Message;
    End;
  End;
//  If ((FMatches = Nil) Or (FMatches.count = 0)) And (FtabsCategories.TabIndex = 0) And FFirstSearch Then
//  Begin
//    FtabsCategories.TabIndex := 1;
//    HandleInput(Self);
//  End;
//  FFirstSearch := False;
//  If FMatches <> Nil Then
//    For i := 0 To FMatches.Count - 1 Do
//    Begin
//      oItem := FviewMatches.Items.Add;
//      oItem.Caption := FMatches[i].Preferred;
//      oItem.Data := FMatches[i];
//    End;
  FviewMatches.Refresh;
End;

//Procedure TSnomedPicker.MatchesGetNode(oSender: TObject; oNode: TUixTreeViewNode);
//Begin
//  If oNode.Parent = Nil Then
//    If (FMatches = Nil) Or (FMatches.count = 0) Then
//      oNode.Children.Count := 1
//    Else
//      oNode.Children.Count := FMatches.Count
//  Else If (FMatches <> Nil) And (FMatches.count > 0) Then
//  Begin
//    oNode.Data := FMatches[oNode.index];
//    oNode.Captions.Add(FMatches[oNode.index].Term);
//    oNode.CaptionVisibilities.Add(True);
//    oNode.Captions.Add(FMatches[oNode.index].Preferred);
//    oNode.CaptionVisibilities.Add(True);
//    oNode.Captions.Add(inttostr(trunc(FMatches[oNode.index].Rating * 10)));
//    oNode.CaptionVisibilities.Add(True);
//    oNode.Captions.Add(FMatches[oNode.index].FSN);
//    oNode.CaptionVisibilities.Add(True);
//  End
//  Else If FWaiting Then
//  Begin
//    oNode.Captions.Add('');
//    oNode.CaptionVisibilities.Add(True);
//    oNode.Captions.Add('Searching....');
//    oNode.CaptionVisibilities.Add(True);
//  End
//  Else If FError <> '' Then
//  Begin
//    oNode.Captions.Add('');
//    oNode.CaptionVisibilities.Add(True);
//    oNode.Captions.Add('Error: '+FError);
//    oNode.CaptionVisibilities.Add(True);
//  End
//  Else
//  Begin
//    oNode.Captions.Add('');
//    oNode.CaptionVisibilities.Add(True);
//    oNode.Captions.Add('[no matches]');
//    oNode.CaptionVisibilities.Add(True);
//  End;
//End;
//
//Procedure TSnomedPicker.MatchesClick(oSender: TObject; oNode: TUixTreeViewNode);
//Begin
//  If (oNode <> Nil) And (oNode.Data <> Nil) Then
//    SetTerm(TSnomedMatch(oNode.Data).Term);
//End;

Procedure TSnomedPicker.ViewTerm(Const sTerm : String);
Var
  oBuffer : TFslBuffer;
Begin
//  Try
//    oBuffer := FClient.DescribeTerm(sTerm);
//    Try
//      FwrdBrowser.DocumentHandler.LoadNative(oBuffer);
//    Finally
//      oBuffer.Free;
//    End;
//  Except
//    On E:Exception Do
//      FwrdBrowser.DocumentHandler.AsText := 'Error: '+e.Message;
//  End;
End;

Procedure TSnomedPicker.HotSpot(oSender: TWordProcessor; oInfo: TWPHotspotInformation);
Var
  l, r : String;
Begin
  If oInfo.Hotspot.URL <> '' Then
  Begin
    StringSplit(oInfo.Hotspot.URL, 'id=', l, r);
    oInfo.Handled := r <> '';
    If oInfo.Handled Then
      Term := r;
  End;
End;

Procedure TSnomedPicker.SetTerm(Const Value: String);
Begin
  If (value = FTerm) Then
    Exit;

  OKButton.Enabled := False;

  If (value <> '') Then
  Begin
    If FFirstTerm = '' Then
      FFirstTerm := value;

    If (FTermIndex = -1) Then
    Begin
      FTerms.Add(Value);
      FTermIndex := 0;
    End
    Else
    Begin
      While (FTerms.Count > FTermIndex + 1) Do
        FTerms.Delete(FTermIndex+1);
      FTerms.Add(Value);
      FTermIndex := FTerms.Count-1;
    End;

    FTerm := Value;
    If FTerm <> '' Then
      ViewTerm(FTerm);
    OKButton.Enabled := FTerm <> '';
  End;
End;

Procedure TSnomedPicker.ClickBackward(oSender: TObject);
Begin
  If FTermIndex > 0 Then
  Begin
    Dec(FTermIndex);
    ViewTerm(FTerms[FTermIndex]);
    FTerm := FTerms[FTermIndex];
  End
  Else
    SoundBeepExclamation;
End;

Procedure TSnomedPicker.ClickFilter(oSender: TObject);
Begin
  If FwrdBrowser.SelectedText <> '' Then
    FedtInput.Text := FwrdBrowser.SelectedText
  Else If FTerm <> '' Then
    // FedtInput.Text := FClient.GetSnomedDisplayName(FTerm)
  Else
    SoundBeepExclamation;
End;

Procedure TSnomedPicker.ClickForward(oSender: TObject);
Begin
  If FTermIndex < FTerms.Count - 1 Then
  Begin
    Inc(FTermIndex);
    ViewTerm(FTerms[FTermIndex]);
    FTerm := FTerms[FTermIndex];
  End
  Else
    SoundBeepExclamation;
End;

Procedure TSnomedPicker.ClickHome(oSender: TObject);
Begin
  Term := FFirstTerm;
  FedtInput.Text := FFirstText;
End;


Procedure TSnomedPicker.SetContext(Const Value: TSnomedPickerContext);
Begin
  FContext := Value;
  Case FContext Of
    spcProcedure :
      Begin
      FtabsCategories.TabIndex := 2;
      FtabsCategories.Visible := False;
      End;
    spcOrganism :
      Begin
      FtabsCategories.TabIndex := 3;
      FtabsCategories.Visible := False;
      End;
    spcFinding :
      Begin
      FtabsCategories.TabIndex := 4;
      FtabsCategories.Visible := False;
      End;
    spcBodyStructure :
      Begin
      FtabsCategories.TabIndex := 5;
      FtabsCategories.Visible := False;
      End;
    spcSpecimen :
      Begin
      FtabsCategories.TabIndex := 6;
      FtabsCategories.Visible := False;
      End;
    spcSituation :
      Begin
      FtabsCategories.TabIndex := 7;
      FtabsCategories.Visible := False;
      End;
    spcProduct :
      Begin
      FtabsCategories.TabIndex := 8;
      FtabsCategories.Visible := False;
      End;
  End;
  Caption := Caption + ' - ' + NAMES_PICKER_CONTEXTS[FContext];
End;

Procedure TSnomedPicker.ListViewMatchesClick(oSender: TObject);
Begin
//  If (FViewMatches.Selected <> Nil) And (FViewMatches.Selected.Data <> Nil) Then
//    SetTerm(TSnomedMatch(FViewMatches.Selected.Data).Term);

End;

Procedure TSnomedPicker.GetViewTip(Sender: TObject; Item: TListItem;  Var InfoTip: String);
//Var
//  oMatch : TSnomedMatch;
Begin
//  oMatch := TSnomedMatch(Item.Data);
//  InfoTip := oMatch.Term+' ' +oMatch.FSN+' '+IntToStr(Trunc(oMatch.Rating))+'%';

End;

End.
