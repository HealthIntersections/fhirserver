Unit FHIR.WP.Printing;


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

Interface

Uses
  Windows, SysUtils, Vcl.Graphics,
  fsl_threads, fsl_utilities,
  wp_graphics, wp_printing_win,
  FHIR.WP.Engine, wp_working, FHIR.WP.Renderer, wp_types, FHIR.WP.Settings;

Type
  TWPPrintCanvas = Class (TWPCanvas)
    Private
      FCanvas : TFslPrinterCanvas;
      Procedure ApplyFont;
      Function GetCanvas : TFslPrinterCanvas;
      Procedure SetCanvas(Const Value : TFslPrinterCanvas);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(oCanvas : TFslPrinterCanvas); Overload; Virtual;
      destructor Destroy; Override;

      Procedure MoveVertical(iTop, iOffset : Integer); Overload; Override;
      Procedure Clip(Const aRect : TRect); Overload; Override;
      Procedure Clip(oObject : TWPMapObject); Overload; Override;
      Procedure UnClip; Overload; Override;

      Procedure DrawRect(aColour : TColour; iLeft, iTop, iRight, iBottom : Integer); Overload; Override;
      Procedure DrawRect(oBitmap : TFslBitmapGraphic; iLeft, iTop, iRight, iBottom : Integer); Overload; Override;
      Procedure DrawLine(iWidth : Integer; aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iX1, iY1, iX2, iY2 : Integer); Overload; Override;
      Procedure DrawText(aBackground : TColour; iLeft, iTop : Integer; Const sText : String); Overload; Override;
      Procedure DrawSquiggle(aColour : TColour; iX1, iY, iX2 : Integer); Overload; Override;
      Procedure DrawImage(oImage : TFslGraphic; aCopyMode : TCopyMode; iLeft, iTop, iRight, iBottom : Integer); Overload; Override;
      Procedure DrawCurve(aColour, aInnerColour : TColour; oBitmap : TFslBitmapGraphic; iX, iY : Integer; iInnerRadius, iOuterRadius : Integer; iRotation : Integer); Overload; Override;
      Procedure DrawPolyLine(aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iXOffset, iYOffset : Integer; oCoords : TWPCoordinateList; rFactorX, rFactorY : Real; bClosed : Boolean = true); Overload; Override;
      Procedure DrawRoundOutline(aColour : TColour; iLeft, iTop, iRight, iBottom, iRadius : Integer); Overload; Override;

      Function DPIX : Integer; Overload; Override;
      Function DPIY : Integer; Overload; Override;

      Function TextExtent(Const sText : String) : TSize; Overload; Override;
      Function GetTextMetrics : TTextMetric; Overload; Override;
      Function TextHeight(Const sText : String) : Integer; Overload; Override;
      Function TextWidth(Const sText : String) : Integer; Overload; Override;
      Procedure GetTextExtents(Const sText : String; Var aSize : TSize; Const Offsets : PWPIntegers); Overload; Override;

      Property Canvas : TFslPrinterCanvas Read GetCanvas Write SetCanvas;
  End;

  TWPPrintRenderer = Class;

  TWPPrinterCheckContinueEvent = Function (oPrinter : TWPPrintRenderer) : Boolean Of Object;

  TWPPrintRenderer = Class (TWPRenderer)
    Private
      FPrinterCanvas : TFslPrinterCanvas;
      FPages : TWPPages;
      FPageLayoutController : TWPPageLayoutController;
      FOnCheckContinue : TWPPrinterCheckContinueEvent;
      FDescription : String;
      FCapabilities : TFslPrinterCapabilitySet;

      Function GetPageLayoutController : TWPPageLayoutController;
      Procedure SetPageLayoutController(Const Value : TWPPageLayoutController);

      Procedure SetPrinterCanvas(oValue : TFslPrinterCanvas);
      Function GetPages : TWPPages;
      Procedure SetPages(oPages : TWPPages);
      Function GetCanvas : TWPPrintCanvas;
      Procedure SetCanvas(Const Value : TWPPrintCanvas);

      Function NewPage : TWPPage;
      Function PlaceContainer(oPage : TWPPage; oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentContainerPiece; iHeight : Integer = -1) : TWPMapContainer;
      Function PlaceSubContainer(oParent : TWPMapContainer; oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentContainerPiece; iTop : Integer = -1) : TWPMapContainer;
      Function PlaceRow(oContainer : TWPMapContainer; oRow : TWPMapRow) : TWPMapRow;
      Procedure MoveItemToRow(oItem : TWPMapItem; oRow : TWPMapRow);

      Procedure AddRowToContainer(oRow : TWPMapRow; oContainer : TWPMapContainer);
      Procedure BuildRows(oRows : TWPMapRows; oContainer : TWPMapContainer; Var iCursor : Integer);
      Procedure AddParaToPage(oContainer : TWPMapContainer; oPara : TWPWorkingDocumentParaPiece; Var oPage : TWPPage);
      Procedure AddParaToContainer(oParent : TWPMapContainer; oContainer : TWPMapContainer; oPara : TWPWorkingDocumentParaPiece);

      Procedure SimpleAddTableCell(oRowContainer : TWPMapContainer; oCellContainer : TWPMapContainer);
      Procedure SimpleAddTableRow(oTableContainer : TWPMapContainer; oRowContainer : TWPMapContainer);
      Procedure SimpleAddTableToPage(oTable : TWPWorkingDocumentTableStartPiece; Var oPage : TWPPage);
      Procedure AddRowToTable(oPage : TWPPage; oParent : TWPMapContainer; oRow : TWPWorkingDocumentTableRowStartPiece; oContainer : TWPMapContainer);
      Procedure AddTableToPage(oTable : TWPWorkingDocumentTableStartPiece; Var oPage : TWPPage);

      Procedure AddSectionToPage(oContainer : TWPMapContainer; oSection : TWPWorkingDocumentSectionStartPiece; Var oPage : TWPPage);
      Procedure AddBreakToPage(oContainer : TWPMapContainer; oBreak : TWPWorkingDocumentBreakPiece; Var oPage : TWPPage);
      Procedure AddToPage(oContainer : TWPMapContainer; Var oPage : TWPPage);
      Procedure BuildPageMap(oContainers : TWPMapContainers; Var oPage : TWPPage);

      Procedure PrintItem(oItem : TWPMapItem);
      Procedure PrintRow(oRow : TWPMapRow);
      Procedure PrintContainer(oMap : TWPMapContainer);
      Function GetPrinterCanvas: TFslPrinterCanvas;
      Procedure DrawTruncationMark(oMap : TWPMapContainer);

      Function CheckContinue : Boolean;

    Protected
      Property Canvas : TWPPrintCanvas Read GetCanvas Write SetCanvas;
      Procedure DoUpdate; Overload; Override;
      Function Printing : Boolean; Overload; Override;
      Function ApplyOutputColourRules(bIsBackground : Boolean; aColour : TColour) : TColour; Override;

      Procedure RaiseError(Const sMethod, sMessage : String); Overload; Override;
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Prepare;
      Procedure Terminate;

      Procedure Print(oPage : TWPPage);

      Property PrinterCanvas : TFslPrinterCanvas Read GetPrinterCanvas Write SetPrinterCanvas;
      Property Capabilities : TFslPrinterCapabilitySet read FCapabilities write FCapabilities;
      Property Pages : TWPPages Read GetPages Write SetPages;
      Property PageLayoutController : TWPPageLayoutController Read GetPageLayoutController Write SetPageLayoutController;
      Property Description : String Read FDescription Write FDescription;
      Property OnCheckContinue : TWPPrinterCheckContinueEvent Read FOnCheckContinue Write FOnCheckContinue;
  End;


  TWPPaginator = Class (TFslThread)
    Private
      FOperator: TWPOperator;
      FDocument: TWPWorkingDocument;
      FVersionTimestamp : Cardinal;
      FPrinter: TFslPrinter;
      FPageLayoutController: TWPPageLayoutController;
      FStyles : TWPStyles;
      FPageOffsets : TWPPaginations;
      FCompleted : Boolean;

      Procedure SetOperator(Const Value: TWPOperator);
      Procedure SetDocument(Const Value: TWPWorkingDocument);
      Procedure SetStyles(Const Value: TWPStyles);
      Procedure SetPrinter(Const Value: TFslPrinter);
      Procedure SetPageLayoutController(Const Value: TWPPageLayoutController);

      Function CheckRequirements : Boolean;
      Function CheckContinue(oPrinter : TWPPrintRenderer) : Boolean;
      Procedure ListPages(oPrinter : TWPPrintRenderer);

//      Procedure DoTest;
    Protected
      Procedure Execute; Override;
    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Function Link : TWPPaginator; Overload;

      Function Paginations : TWPPaginations;

      Property Operator : TWPOperator Read FOperator Write SetOperator;
      Property Document : TWPWorkingDocument Read FDocument Write SetDocument;
      Property Styles : TWPStyles Read FStyles Write SetStyles;
      Property Printer : TFslPrinter Read FPrinter Write SetPrinter;
      Property PageLayoutController : TWPPageLayoutController Read FPageLayoutController Write SetPageLayoutController;
      Property Completed : Boolean Read FCompleted;
  End;


Implementation

{ TWPPaginator }

Destructor TWPPaginator.Destroy;
Begin
  FOperator.Free;
  FDocument.Free;
  FPrinter.Free;
  FPageLayoutController.Free;
  FStyles.Free;
  Inherited;
End;


Procedure TWPPaginator.Execute;
Var
  oPrinter : TWPPrintRenderer;
  oJob : TFslPrinterPreviewJob;
Begin
  Inherited;

  If CheckRequirements Then
  Begin
    FPrinter.Open;
    If (Not CheckContinue(Nil)) Then
      Exit;
    oJob := FPrinter.ProducePreviewJob;
    Try
      If (Not CheckContinue(Nil)) Then
        Exit;
      oJob.Open;
      If (Not CheckContinue(Nil)) Then
        Exit;
      oJob.Start;
      oPrinter := TWPPrintRenderer.Create;
      Try
        oPrinter.PrinterCanvas := oJob.Canvas.Link;
        oPrinter.Capabilities := FPrinter.CapabilitySet;
        oPrinter.Pages := TWPPages.Create;
        oPrinter.PageLayoutController := FPageLayoutController.Link;
        oPrinter.Document := FDocument.Link;
        oPrinter.Styles := FStyles.Link;
        oPrinter.OnCheckContinue := CheckContinue;
        If CheckContinue(oPrinter) Then
        Begin
          oPrinter.Prepare;
          If CheckContinue(oPrinter) Then
          Begin
            ListPages(oPrinter);
            FCompleted := True;
          End;
        End;
      Finally
        oPrinter.Free;
      End;
    Finally
      oJob.Free;
    End;
  End;
End;

Function TWPPaginator.Link: TWPPaginator;
Begin
  Result := TWPPaginator(Inherited Link);
End;

Procedure TWPPaginator.SetOperator(Const Value: TWPOperator);
Begin
  FOperator.Free;
  FOperator := Value;
  FVersionTimestamp := FOperator.LastAction;
End;

Procedure TWPPaginator.SetDocument(Const Value: TWPWorkingDocument);
Begin
  FDocument.Free;
  FDocument := Value;
End;

Procedure TWPPaginator.SetStyles(Const Value: TWPStyles);
Begin
  FStyles.Free;
  FStyles := Value;
End;

Procedure TWPPaginator.SetPageLayoutController(Const Value: TWPPageLayoutController);
Begin
  FPageLayoutController.Free;
  FPageLayoutController := Value;
End;

Procedure TWPPaginator.SetPrinter(Const Value: TFslPrinter);
Begin
  FPrinter.Free;
  FPrinter := Value;
End;

Function TWPPaginator.CheckContinue(oPrinter: TWPPrintRenderer): Boolean;
Begin
  // strictly this isn't thread safe, but since all we are
  // doing is checking identity, it doesn't matter
  Result := not Terminated And (FOperator.LastAction = FVersionTimestamp);
End;

Function TWPPaginator.CheckRequirements: Boolean;
Begin
  Result := ((Assigned(FPrinter) And Assigned(FPageLayoutController)))
             And Assigned(FOperator) And Assigned(FStyles)
             And Assigned(FDocument);
  If Result Then
    FDocument.RegenerateMetrics(False, False);
End;

Procedure TWPPaginator.ListPages(oPrinter: TWPPrintRenderer);
Var
  iLoop : Integer;
Begin
  SetLength(FPageOffsets, oPrinter.Pages.Count);
  For iLoop := 0 To oPrinter.Pages.Count - 1 Do
    FPageOffsets[iLoop] := oPrinter.Pages[iLoop].GetLastOffset;
  If FPageOffsets[oPrinter.Pages.Count - 1] = -1 Then
    FPageOffsets[oPrinter.Pages.Count - 1] := FDocument.CharCount;
End;

Function TWPPaginator.Paginations: TWPPaginations;
Begin
  Result := FPageOffsets;
End;

{Procedure TWPPaginator.DoTest;
Var
  iLoop : Integer;
Begin
  SetLength(FPageOffsets, FDocument.CharCount Div 100 + 1);
  For iLoop := Low(FPageOffsets) To High(FPageOffsets) - 1 Do
    FPageOffsets[iLoop] := (iLoop + 1) * 100;
  FPageOffsets[High(FPageOffsets)] := FDocument.CharCount;
  FCompleted := True;
End;
}


function TWPPaginator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOperator.sizeInBytes);
  inc(result, FDocument.sizeInBytes);
  inc(result, FPrinter.sizeInBytes);
  inc(result, FPageLayoutController.sizeInBytes);
  inc(result, FStyles.sizeInBytes);
end;

Constructor TWPPrintRenderer.Create;
Begin
  Inherited;

  FPageLayoutController := Nil;
  FPages := Nil;
End;


Destructor TWPPrintRenderer.Destroy;
Begin
  FPageLayoutController.Free;
  FPages.Free;
  FPrinterCanvas.Free;

  Inherited;
End;


Procedure TWPPrintRenderer.RaiseError(Const sMethod, sMessage : String);
Begin
  If FDescription = '' Then
    Inherited RaiseError(sMethod, sMessage)
  Else
    Inherited RaiseError(sMethod, FDescription+' ' +sMessage);
End;


Function TWPPrintRenderer.GetPageLayoutController : TWPPageLayoutController;
Begin
  Assert(Invariants('GetPageLayoutController', FPageLayoutController, TWPPageLayoutController, 'PageLayoutController'));
  Result := FPageLayoutController;
End;


Procedure TWPPrintRenderer.SetPageLayoutController(Const Value : TWPPageLayoutController);
Begin
  FPageLayoutController.Free;
  FPageLayoutController := Value;
End;


Function TWPPrintRenderer.GetCanvas : TWPPrintCanvas;
Begin
  Assert(Invariants('GetCanvas', Inherited Canvas, TWPPrintCanvas, 'Canvas'));
  Result := TWPPrintCanvas(Inherited Canvas);
End;


Procedure TWPPrintRenderer.SetCanvas(Const Value : TWPPrintCanvas);
Begin
  Inherited Canvas := Value;
End;


Procedure TWPPrintRenderer.SetPrinterCanvas(oValue : TFslPrinterCanvas);
Begin
  FPrinterCanvas.Free;
  FPrinterCanvas := oValue;

  Canvas := TWPPrintCanvas.Create(FPrinterCanvas.Link);
  Canvas.Font.Clear;
  Canvas.Font.Name := 'Verdana';
  Canvas.Font.Size := 20;
  Canvas.PointSizeY := Trunc(Canvas.DPIY / SCREEN_DPI);
  Canvas.PointSizeX := Trunc(Canvas.DPIX / SCREEN_DPI);
End;


Function TWPPrintRenderer.GetPages : TWPPages;
Begin
  Assert(Invariants('GetPages', FPages, TWPPages, 'Pages'));
  Result := FPages;
End;


Procedure TWPPrintRenderer.SetPages(oPages : TWPPages);
Begin
  FPages.Free;
  FPages := oPages;
End;


Function TWPPrintRenderer.NewPage : TWPPage;
Begin
  If (FPages.Count > 0) And Not PageLayoutController.IsSpanPolicySpan Then
  Begin
    Result := Nil;
    If PageLayoutController.IsSpanPolicyError Then
      RaiseError('NewPage', 'Content exceeds a single page size : '+Document.AsText)
    Else // If (PageLayoutController.IsSpanPolicyTruncate Then
      FPages[0].Truncated := True;
  End
  Else
  Begin
    Result := TWPPage.Create;
    Try
      Result.Map := TWPMapContainer.Create;
      Result.Map.Left := PageLayoutController.Left(FPages.Count, FPrinterCanvas);
      Result.Map.Width := PageLayoutController.Width(FPrinterCanvas);
      Result.Map.Top := PageLayoutController.Top(FPages.Count, FPrinterCanvas);
      Result.Map.Height := PageLayoutController.Height(FPages.Count, FPrinterCanvas);
      Result.Cursor := Result.Map.Top;

      FPages.Add(Result.Link);
    Finally
      Result.Free;
    End;
  End;
End;


Function TWPPrintRenderer.PlaceContainer(oPage : TWPPage; oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentContainerPiece; iHeight : Integer = -1) : TWPMapContainer;
Begin
  Result := TWPMapContainer.Create;
  Try
    Result.Top := oPage.Cursor;
    Result.Left := oContainer.Left + oPage.Map.Left;
    Result.Width := oContainer.Width;
    Result.PrintTopOffset := Result.Top - oContainer.Top;
    Result.PrintLeftOffset := Result.Left - oContainer.Left;

    If iHeight = -1 Then
      Result.Height := oContainer.Height
    Else
      Result.Height := iHeight;
    Result.MarginLeft := oContainer.MarginLeft;
    Result.MarginBottom := oContainer.MarginBottom;
    Result.MarginRight := oContainer.MarginRight;
    Result.MarginTop := oContainer.MarginTop;
    Result.Background := oContainer.Background;
    Result.Piece := oPiece;
    oPiece.Container := Nil;

    oPage.Map.Children.Add(Result.Link);
    oPage.Cursor := oPage.Cursor + Result.Height;
    Result.Cursor := Result.Top;
  Finally
    Result.Free;
  End;
End;


Function TWPPrintRenderer.PlaceSubContainer(oParent : TWPMapContainer; oContainer : TWPMapContainer; oPiece : TWPWorkingDocumentContainerPiece; iTop : Integer = -1) : TWPMapContainer;
Begin
  Result := TWPMapContainer.Create;
  Try
    If iTop = -1 Then
      Result.Top := oContainer.Top + oParent.PrintTopOffset
    Else
      Result.Top := iTop;
    Result.Left := oContainer.Left + oParent.PrintLeftOffset;
    Result.Width := oContainer.Width;
    Result.PrintTopOffset := Result.Top - oContainer.Top;
    Result.PrintLeftOffset := Result.Left - oContainer.Left;
    Result.Height := oContainer.Height;

    Result.MarginLeft := oContainer.MarginLeft;
    Result.MarginBottom := oContainer.MarginBottom;
    Result.MarginRight := oContainer.MarginRight;
    Result.MarginTop := oContainer.MarginTop;
    Result.Background := oContainer.Background;

    Result.Piece := oPiece;
    oPiece.Container := Nil;

    oParent.Children.Add(Result.Link);
    Result.Cursor := Result.Top;
  Finally
    Result.Free;
  End;
End;


Function TWPPrintRenderer.PlaceRow(oContainer : TWPMapContainer; oRow : TWPMapRow) : TWPMapRow;
Begin
  Result := TWPMapRow.Create;
  Try
    Result.Top := oContainer.Cursor;
    Result.Left := oRow.Left + oContainer.PrintLeftOffset;
    Result.PrintTopOffset := Result.Top - oRow.Top;
    Result.PrintLeftOffset := Result.Left - oRow.Left;
    Result.Width := oRow.Width;
    Result.Height := oRow.Height;
    Result.Baseline := oRow.Baseline;
    Result.Background := oRow.Background;
    Result.Piece := Nil;

    oContainer.Rows.Add(Result.Link);
    oContainer.Cursor := oContainer.Cursor + Result.Height;
  Finally
    Result.Free;
  End;
End;


Procedure TWPPrintRenderer.Prepare;
Var
  oPage : TWPPage;
Begin
  Width := PageLayoutController.Width(FPrinterCanvas);
  NominalPageHeight := PageLayoutController.Height(0, FPrinterCanvas);
  Settings.Background := clWhite;
  Settings.FieldWrappers := wpfpNone;
  Settings.Hotspots := wphmNone;

  Working := True; // will prompt Update

  oPage := NewPage;
  Assert(CheckCondition(Assigned(oPage), 'Prepare', 'Failed to prepare first page'));
  If (CheckContinue) Then
    BuildPageMap(Map.Children, oPage);
End;


Procedure TWPPrintRenderer.DoUpdate;
Begin
  If HasDocument Then
  Begin
    MeasurePieces(True);
    InitialiseMap;
    LayoutDocument(Document, Nil);
  End;
End;


Procedure TWPPrintRenderer.AddBreakToPage(oContainer : TWPMapContainer; oBreak : TWPWorkingDocumentBreakPiece; Var oPage : TWPPage);
Begin
  If oBreak.BreakType = btPageBreak Then
    oPage := NewPage
  Else
  Begin
    If Assigned(oPage) And Not oPage.Fits(oContainer.Height) Then
      oPage := NewPage;
    If Assigned(oPage) Then
      PlaceContainer(oPage, oContainer, oBreak, oContainer.Height);
  End;
End;

Procedure TWPPrintRenderer.AddSectionToPage(oContainer : TWPMapContainer; oSection : TWPWorkingDocumentSectionStartPiece; Var oPage : TWPPage);
Begin
  If Not oPage.Fits(oContainer.MarginTop) Then
    oPage := NewPage;
  If Assigned(oPage) Then
  Begin
    PlaceContainer(oPage, oContainer, oSection, oContainer.MarginTop);

    BuildPageMap(oContainer.Children, oPage);

    If Not oPage.Fits(oContainer.MarginBottom) Then
      oPage := NewPage;
    If Assigned(oPage) Then
      PlaceContainer(oPage, oContainer, oSection, oContainer.MarginBottom).Primary := False;
  End;
End;


Procedure TWPPrintRenderer.MoveItemToRow(oItem : TWPMapItem; oRow : TWPMapRow);
Var
  oNew : TWPMapItem;
Begin
  oNew := TWPMapItem.Create;
  Try
    oNew.Assign(oItem);
    oNew.Left := oItem.Left + oRow.PrintLeftOffset;
    oNew.Top := oItem.Top + oRow.PrintTopOffset;
    oNew.Parent := oRow;
    oRow.Items.Add(oNew.Link);
  Finally
    oNew.Free;
  End;
End;


Procedure TWPPrintRenderer.AddRowToContainer(oRow : TWPMapRow; oContainer : TWPMapContainer);
Var
  iLoop : Integer;
  oNew : TWPMapRow;
Begin
  oNew := PlaceRow(oContainer, oRow);
  For iLoop := 0 To oRow.Items.Count - 1 Do
    MoveItemToRow(oRow.Items[iLoop], oNew);
End;


Procedure TWPPrintRenderer.BuildRows(oRows : TWPMapRows; oContainer : TWPMapContainer; Var iCursor : Integer);
Begin
  While (iCursor < oRows.Count) And oContainer.Fits(oRows[iCursor].Height) Do
    Begin
    AddRowToContainer(oRows[iCursor], oContainer);
    Inc(iCursor);
    End;
End;


Procedure TWPPrintRenderer.AddParaToContainer(oParent : TWPMapContainer; oContainer : TWPMapContainer; oPara : TWPWorkingDocumentParaPiece);
Var
  iCursor : Integer;
Begin
  iCursor := 0;
  If Not oParent.Fits(oContainer.Height) Then
    RaiseError('AddParaToContainer', 'Attempt to add Paragraph to container but it doesn''t fit');
  BuildRows(oContainer.Rows, PlaceSubContainer(oParent, oContainer, oPara), iCursor);
End;


Procedure TWPPrintRenderer.AddParaToPage(oContainer : TWPMapContainer; oPara : TWPWorkingDocumentParaPiece; Var oPage : TWPPage);
Var
  iCursor : Integer;
  oNew : TWPMapContainer;
Begin
  iCursor := 0;
  If Not oPage.Fits(oContainer.Height) Then
  Begin
    While Assigned(oPage) And (iCursor < oContainer.Rows.Count) Do
    Begin
      // if there is space for at least 3 rows, then we will add as many as we can fit.
      If (oContainer.Rows.Count > 3) And oPage.Fits(oContainer.Rows[2].Bottom - oContainer.Top) Then
      Begin
        oNew := PlaceContainer(oPage, oContainer, oPara);
        oNew.Primary := iCursor = 0;
        oNew.Height := oPage.Map.Bottom - oNew.Top;
        BuildRows(oContainer.Rows, oNew, iCursor)
      End
      Else // we are at the top of a page, we will add at least one row, and as many as we can
      Begin
        if not oPage.isEmpty then
          oPage := NewPage;
        If Assigned(oPage) Then
        Begin
          oNew := PlaceContainer(oPage, oContainer, oPara);
          oNew.Primary := iCursor = 0;
          oNew.Height := oPage.Map.Bottom - oNew.Top;
          AddRowToContainer(oContainer.Rows[iCursor], oNew);
          Inc(iCursor);
          BuildRows(oContainer.Rows, oNew, iCursor);
          If iCursor >= oContainer.Rows.Count Then
            oNew.Height := oNew.Cursor - oNew.Top;
          oPage.Cursor := oNew.Bottom;
        End;
      End;
    End;
  End
  Else
    BuildRows(oContainer.Rows, PlaceContainer(oPage, oContainer, oPara), iCursor);
End;

Procedure TWPPrintRenderer.SimpleAddTableCell(oRowContainer : TWPMapContainer; oCellContainer : TWPMapContainer);
Var
  oNew : TWPMapContainer;
  iLoop : Integer;
Begin
  oNew := PlaceSubContainer(oRowContainer, oCellContainer, TWPWorkingDocumentTableCellStartPiece(oCellContainer.Piece));
  For iLoop := 0 To oCellContainer.Children.Count - 1 Do
    AddParaToContainer(oNew, oCellContainer.Children[iLoop], TWPWorkingDocumentParaPiece(oCellContainer.Children[iLoop].Piece));
  ClipContents(oNew);
End;

Procedure TWPPrintRenderer.SimpleAddTableRow(oTableContainer : TWPMapContainer; oRowContainer : TWPMapContainer);
Var
  oNew : TWPMapContainer;
  iLoop : Integer;
Begin
  oNew := PlaceSubContainer(oTableContainer, oRowContainer, TWPWorkingDocumentTableRowStartPiece(oRowContainer.Piece));
  For iLoop := 0 To oRowContainer.Children.Count - 1 Do
    SimpleAddTableCell(oNew, oRowContainer.Children[iLoop]);
End;


Procedure TWPPrintRenderer.SimpleAddTableToPage(oTable : TWPWorkingDocumentTableStartPiece; Var oPage : TWPPage);
Var
  oOld : TWPMapContainer;
  oNew : TWPMapContainer;
  iLoop : Integer;
Begin
  oOld := oTable.Container;
  oNew := PlaceContainer(oPage, oTable.Container, oTable);
  For iLoop := 0 To oOld.Children.Count - 1 Do
    SimpleAddTableRow(oNew, oOld.Children[iLoop]);
End;


Procedure TWPPrintRenderer.AddRowToTable(oPage : TWPPage; oParent : TWPMapContainer; oRow : TWPWorkingDocumentTableRowStartPiece; oContainer : TWPMapContainer);
Var
  oNew : TWPMapContainer;
  iLoop : Integer;
Begin
  oNew := PlaceSubContainer(oParent, oContainer, oRow, oPage.Cursor);
  oPage.Cursor := oPage.Cursor + oNew.Height;
  For iLoop := 0 To oContainer.Children.Count - 1 Do
    SimpleAddTableCell(oNew, oContainer.Children[iLoop]);
End;


Procedure TWPPrintRenderer.AddTableToPage(oTable : TWPWorkingDocumentTableStartPiece; Var oPage : TWPPage);
Var
  bHeader : Boolean;
  iHeaderHeight : Integer;
  iCursor : Integer;
  bFirst : Boolean;
  oOld : TWPMapContainer;
  oHead : TWPMapContainer;
  oNew : TWPMapContainer;
Begin
  If oPage.Fits(oTable.Container.Height) Then
    SimpleAddTableToPage(oTable, oPage)
  Else
  Begin
    oOld := oTable.Container;

    bHeader := (oTable.Rows.Count >= 2) And (oTable.Rows[0].Header); // if there is only one row marked as a header, we don't consider it a header

    If bHeader Then
    Begin
      oHead := oTable.Rows[0].Container;
      iHeaderHeight := oHead.Height;
      iCursor := 1;
    End
    Else
    Begin
      iHeaderHeight := 0;
      iCursor := 0;
      oHead := Nil;
    End;

    If (iCursor < oOld.Children.Count) And Not oPage.Fits(oOld.Children[iCursor].Height + iHeaderHeight) Then
      oPage := NewPage;

    bFirst := True;
    While Assigned(oPage) And (iCursor < oOld.Children.Count) Do
    Begin
      If Not bFirst And (oPage.Cursor <> oPage.Map.Top) Then
        oPage := NewPage;
      If Assigned(oPage) Then
      Begin
        bFirst := False;

        oNew := PlaceContainer(oPage, oOld, oTable, 0);
        oPage.Cursor := oNew.Top;

        If Assigned(oHead) Then
          AddRowToTable(oPage, oNew, oTable.Rows[0], oHead);

        If Not oPage.Fits(oOld.Children[iCursor].Height) Then
          RaiseError('AddTableToPage', StringFormat('Unable to fit row %d on a single page. Reduce the number of columns or the content of the row so that it fits on a single page.', [iCursor]));
        TWPWorkingDocumentTableRowStartPiece(oOld.Children[iCursor].Piece).StateFirst;
        Repeat
          AddRowToTable(oPage, oNew, oTable.Rows[iCursor], oTable.Rows[iCursor].Container);
          Inc(iCursor);
        Until (iCursor = oOld.Children.Count) Or Not oPage.Fits(oOld.Children[iCursor].Height) Or TWPWorkingDocumentTableRowStartPiece(oOld.Children[iCursor].Piece).BreakBefore;
        TWPWorkingDocumentTableRowStartPiece(oOld.Children[iCursor-1].Piece).StateLast;

        oNew.Height := oPage.Cursor - oNew.Top;
      End;
    End;
  End;
End;


Procedure TWPPrintRenderer.AddToPage(oContainer : TWPMapContainer; Var oPage : TWPPage);
Begin
  Case TWPWorkingDocumentPiece(oContainer.Piece).PieceType Of
      ptSectionStart : AddSectionToPage(oContainer, TWPWorkingDocumentSectionStartPiece(oContainer.Piece), oPage);
      ptPara : AddParaToPage(oContainer, TWPWorkingDocumentParaPiece(oContainer.Piece), oPage);
      ptTableStart : AddTableToPage(TWPWorkingDocumentTableStartPiece(oContainer.Piece), oPage);
      ptBreak : AddBreakToPage(oContainer, TWPWorkingDocumentBreakPiece(oContainer.Piece), oPage);
    Else
      // ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak, ptTableStop, ptRowStop, ptCellStop, , ptSectionStop, ptCellStart
      RaiseError('PrintContainer', 'Unexpected Container Type '+NAMES_WPPIECETYPE[TWPWorkingDocumentPiece(oContainer.Piece).PieceType]);
  End;
End;


Procedure TWPPrintRenderer.BuildPageMap(oContainers : TWPMapContainers; Var oPage : TWPPage);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oContainers.Count - 1 Do
    If CheckContinue And Assigned(oPage) Then
      AddToPage(oContainers[iLoop], oPage);
End;


Procedure TWPPrintRenderer.Print(oPage : TWPPage);
Begin
  Canvas.Canvas := PrinterCanvas.Link;
  PrintContainer(oPage.Map);
  If oPage.Truncated Then
    DrawTruncationMark(oPage.Map);
End;


Procedure TWPPrintRenderer.DrawTruncationMark(oMap : TWPMapContainer);
Var
  aSize : TSize;
Begin
  Canvas.Font.Clear;
  Canvas.Font.Name := 'MS Sans Serif';
  Canvas.Font.Colour := clGray;
  Canvas.Font.Size := 8;
  Canvas.Font.Italic := True;

  aSize := Canvas.TextExtent(' (Truncated) ');
  If (aSize.cx < oMap.Width Div 2) And (aSize.cy < oMap.Height Div 2) Then
  Begin
    Canvas.DrawRect(clLtGray, oMap.Right - aSize.cx, oMap.Bottom - aSize.cy, oMap.Right, oMap.Bottom);
    Canvas.DrawText(clTransparent, oMap.Right - aSize.cx, oMap.Bottom - aSize.cy, ' (Truncated) ');
    Canvas.DrawBorder(clGray, oMap.Right - aSize.cx, oMap.Bottom - aSize.cy, oMap.Right, oMap.Bottom, Canvas.PointSizeY, Canvas.PointSizeX, Canvas.PointSizeY, Canvas.PointSizeX);
  End
  Else
  Begin
    Canvas.DrawRect(clLtGray, oMap.Right - Canvas.PointSizeX * 20, oMap.Bottom - Canvas.PointSizeY * 9, oMap.Right, oMap.Bottom);

    Canvas.DrawRect(clGray, oMap.Right - Canvas.PointSizeX * 15, oMap.Bottom - Canvas.PointSizeY * 6, oMap.Right - Canvas.PointSizeX * 13, oMap.Bottom - Canvas.PointSizeY * 4);
    Canvas.DrawRect(clGray, oMap.Right - Canvas.PointSizeX * 11, oMap.Bottom - Canvas.PointSizeY * 6, oMap.Right - Canvas.PointSizeX *  9, oMap.Bottom - Canvas.PointSizeY * 4);
    Canvas.DrawRect(clGray, oMap.Right - Canvas.PointSizeX *  7, oMap.Bottom - Canvas.PointSizeY * 6, oMap.Right - Canvas.PointSizeX *  5, oMap.Bottom - Canvas.PointSizeY * 4);

    Canvas.DrawBorder(clGray, oMap.Right - Canvas.PointSizeX * 20, oMap.Bottom - Canvas.PointSizeY * 9, oMap.Right, oMap.Bottom, Canvas.PointSizeY, Canvas.PointSizeX, Canvas.PointSizeY, Canvas.PointSizeX);
  End;
  Canvas.DrawLine(Canvas.PointSizeY, clGray, apsSolid, oMap.Left, oMap.Bottom, oMap.Right, oMap.Bottom);
End;


Procedure TWPPrintRenderer.PrintContainer(oMap : TWPMapContainer);
Var
  iLoop : Integer;
Begin
  If oMap.HasPiece And Not (oMap.Piece Is TWPWorkingDocument) Then
  Begin
    Case TWPWorkingDocumentPiece(oMap.Piece).PieceType Of
      ptSectionStart : PaintSection(oMap, TWPWorkingDocumentSectionStartPiece(oMap.Piece));
      ptPara : PaintParaContainer(oMap, TWPWorkingDocumentParaPiece(oMap.Piece));
      ptBreak : PaintBreak(oMap, TWPWorkingDocumentBreakPiece(oMap.Piece));
      ptTableStart : PaintTable(oMap, TWPWorkingDocumentTableStartPiece(oMap.Piece));
      ptRowStart: ; // nothing
      ptCellStart : PaintTableCell(oMap, TWPWorkingDocumentTableCellStartPiece(oMap.Piece));
    Else
      // ptText, ptImage, ptFieldStart, ptFieldStop, ptLineBreak, ptTableStop, ptRowStop, ptCellStop, , ptSectionStop
      RaiseError('PrintContainer', 'Unexpected Container Type '+NAMES_WPPIECETYPE[TWPWorkingDocumentPiece(oMap.Piece).PieceType]);
    End;
  End;
  For iLoop := 0 To oMap.Children.Count - 1 Do
    PrintContainer(oMap.Children[iLoop]);
  For iLoop := 0 To oMap.Rows.Count - 1 Do
    PrintRow(oMap.Rows[iLoop]);
End;


Procedure TWPPrintRenderer.PrintItem(oItem : TWPMapItem);
Begin
  Case TWPWorkingDocumentPiece(oItem.Piece).PieceType Of
    ptText : PaintText(oItem, TWPWorkingDocumentTextPiece(oItem.Piece), False);
    ptImage : PaintImage(oItem, TWPWorkingDocumentImagePiece(oItem.Piece));
    ptFieldStart : PaintFieldStart(oItem, TWPWorkingDocumentFieldStartPiece(oItem.Piece));
    ptFieldStop : PaintFieldEnd(oItem, TWPWorkingDocumentFieldStopPiece(oItem.Piece));
    ptLineBreak : PaintLineBreak(oItem, TWPWorkingDocumentLineBreakPiece(oItem.Piece));
    ptPara : PaintParaMap(oItem, TWPWorkingDocumentParaPiece(oItem.Piece));
  Else
    // ptTableStart, ptRowStart, ptCellStart, ptTableStop, ptRowStop, ptCellStop, ptSectionStart, ptSectionStop
      RaiseError('PrintItem', 'Unexpected Item Type '+NAMES_WPPIECETYPE[TWPWorkingDocumentPiece(oItem.Piece).PieceType]);
  End;
End;


Procedure TWPPrintRenderer.PrintRow(oRow : TWPMapRow);
Var
  iLoop : Integer;
Begin
  PaintRowEdge(oRow);
  For iLoop := 0 To oRow.Items.Count - 1 Do
    PrintItem(oRow.Items[iLoop]);
End;


Function TWPPrintRenderer.Printing : Boolean;
Begin
  Result := True;
End;


Function TWPPrintRenderer.GetPrinterCanvas: TFslPrinterCanvas;
Begin
  Result := FPrinterCanvas;
End;


Function TWPPrintRenderer.CheckContinue: Boolean;
Begin
  If Assigned(FOnCheckContinue) Then
    Result := FOnCheckContinue(Self)
  Else
    Result := True;
End;


Function TWPPrintRenderer.ApplyOutputColourRules(bIsBackground : Boolean; aColour : TColour) : TColour;
var
  bColours : Boolean;
  bDoingBackground : Boolean;
Begin
  bColours := (pcColor in FCapabilities);
  bDoingBackground := (Settings.PrintBackgrounds = wpbpAlways) or ((Settings.PrintBackgrounds = wpbpInColour) and bColours);

  if bDoingBackground then
    if bColours Then
      Result := aColour
    Else
      Result := ColourMakeGrey(aColour)
  Else if bIsBackground Then
    Result := clWhite
  Else if aColour = clWhite then
    result := clBlack
  Else if bColours Then
    Result := aColour
  Else
    Result := ColourMakeGrey(aColour)
End;


procedure TWPPrintRenderer.Terminate;
Var
  oPiece : TWPWorkingDocumentPiece;
  iLoop : Integer;
begin
  Working := False;
  For iLoop := 0 to Document.Pieces.Count - 1 do
  Begin
    oPiece := Document.Pieces[iLoop];
    oPiece.ResetRendering;
  End;
end;


function TWPPrintRenderer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPrinterCanvas.sizeInBytes);
  inc(result, FPages.sizeInBytes);
  inc(result, FPageLayoutController.sizeInBytes);
  inc(result, (FDescription.length * sizeof(char)) + 12);
end;

Constructor TWPPrintCanvas.Create(oCanvas : TFslPrinterCanvas);
Begin
  Create;
  Canvas := oCanvas;
End;


Destructor TWPPrintCanvas.Destroy;
Begin
  FCanvas.Free;
  Inherited;
End;


Function TWPPrintCanvas.GetCanvas : TFslPrinterCanvas;
Begin
  Assert(Invariants('GetCanvas', FCanvas, TFslPrinterCanvas, 'Canvas'));
  Result := FCanvas;
End;


Procedure TWPPrintCanvas.SetCanvas(Const Value : TFslPrinterCanvas);
Begin
  FCanvas.Free;
  FCanvas := Value;
End;


Procedure TWPPrintCanvas.DrawRect(aColour : TColour; iLeft, iTop, iRight, iBottom : Integer);
Begin
  FCanvas.Pen.Style := apsNone;
  FCanvas.Brush.Colour := aColour;
  FCanvas.Brush.Style := absSolid;
  FCanvas.BoxPixels(iLeft, iTop, iRight - iLeft, iBottom - iTop);
  FCanvas.Brush.Colour := clWhite;
  FCanvas.Brush.Style := absNull;
End;


Procedure TWPPrintCanvas.DrawRect(oBitmap : TFslBitmapGraphic; iLeft, iTop, iRight, iBottom : Integer);
Begin
  FCanvas.Pen.Width := 0;
  FCanvas.Pen.Style := apsNone;
  FCanvas.Brush.Bitmap := oBitmap.Link;
  FCanvas.BoxPixels(iLeft, iTop, iRight - iLeft+1, iBottom - iTop+1);
  FCanvas.Brush.Colour := clWhite;
  FCanvas.Brush.Style := absNull;
  FCanvas.Brush.Bitmap := Nil;
End;


Procedure TWPPrintCanvas.DrawLine(iWidth : Integer; aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iX1, iY1, iX2, iY2 : Integer);
Begin
  FCanvas.Pen.EndStyle := apesSquare;
  FCanvas.Pen.Colour := aColour;
  FCanvas.Pen.Style := aPenStyle;
  FCanvas.Pen.EndStyle := aEndStyle;
  FCanvas.Pen.Width :=  IntegerMax(iWidth, PointSize Div 2);
  FCanvas.LinePixelsXY(iX1, iY1, iX2, iY2);
End;


Procedure TWPPrintCanvas.DrawText(aBackground : TColour; iLeft, iTop : Integer; Const sText : String);
Begin
  ApplyFont;
  FCanvas.TextOutPixels(aBackground, iLeft, iTop, sText);
End;


Procedure TWPPrintCanvas.DrawSquiggle(aColour : TColour; iX1, iY, iX2 : Integer);
Begin
  // ignore squiggles for printing
End;


Procedure TWPPrintCanvas.DrawCurve(aColour, aInnerColour : TColour; oBitmap : TFslBitmapGraphic; iX, iY : Integer; iInnerRadius, iOuterRadius : Integer; iRotation : Integer);
Var
  aPoints : Array Of TPoint;
  idx1, idx2, idy1, idy2 : Integer;
Begin
  idx1 := 0;
  idx2 := 0;
  idy1 := 0;
  idy2 := 0;
  If iRotation = 270 Then
  Begin
    idx2 := 1;
    idy2 := 1;
  End Else If iRotation = 0 Then
  Begin
    idx2 := 1;
    idy2 := -1;
  End;

  If aInnerColour <> DEF_COLOUR Then
  Begin
    FCanvas.Pen.Width := 0;
    FCanvas.Pen.Style := apsNone;
    FCanvas.Pen.Colour := aInnerColour;
    FCanvas.Brush.Colour := aInnerColour;
    SetLength(aPoints, 6);
    aPoints[0].x  := iX;
    aPoints[0].y  := iY;
    aPoints[1].x  := iX + Trunc(cos(DegreesToRadians( 0 + iRotation)) * iInnerRadius)+idx1;
    aPoints[1].y  := iY - Trunc(sin(DegreesToRadians( 0 + iRotation)) * iInnerRadius)+idy1;
    aPoints[2].x  := iX + Trunc(cos(DegreesToRadians(30 + iRotation)) * iInnerRadius)+idx1;
    aPoints[2].y  := iY - Trunc(sin(DegreesToRadians(30 + iRotation)) * iInnerRadius)+idy1;
    aPoints[3].x  := iX + Trunc(cos(DegreesToRadians(60 + iRotation)) * iInnerRadius)+idx1;
    aPoints[3].y  := iY - Trunc(sin(DegreesToRadians(60 + iRotation)) * iInnerRadius)+idy1;
    aPoints[4].x  := iX + Trunc(cos(DegreesToRadians(90 + iRotation)) * iInnerRadius)+idx1;
    aPoints[4].y  := iY - Trunc(sin(DegreesToRadians(90 + iRotation)) * iInnerRadius)+idy1;
    aPoints[5].x  := iX;
    aPoints[5].y  := iY;
    FCanvas.PolygonOffset(aPoints);
  End;

  FCanvas.Pen.Width := 0;
  FCanvas.Pen.Style := apsNone;
  FCanvas.Pen.Colour := aColour;
  FCanvas.Brush.Colour := aColour;
  FCanvas.Brush.Style := absSolid;
  If oBitmap <> Nil Then
    FCanvas.Brush.Bitmap := oBitmap.Link;

  SetLength(aPoints, 12);
  aPoints[0].x  := iX + Trunc(cos(DegreesToRadians( 0 + iRotation)) * iOuterRadius);
  aPoints[0].y  := iY - Trunc(sin(DegreesToRadians( 0 + iRotation)) * iOuterRadius);
  aPoints[1].x  := iX + Trunc(cos(DegreesToRadians( 0 + iRotation)) * iInnerRadius)+idx1;
  aPoints[1].y  := iY - Trunc(sin(DegreesToRadians( 0 + iRotation)) * iInnerRadius)+idy1;
  aPoints[2].x  := iX + Trunc(cos(DegreesToRadians(30 + iRotation)) * iInnerRadius)+idx1;
  aPoints[2].y  := iY - Trunc(sin(DegreesToRadians(30 + iRotation)) * iInnerRadius)+idy1;
  aPoints[3].x  := iX + Trunc(cos(DegreesToRadians(60 + iRotation)) * iInnerRadius)+idx1;
  aPoints[3].y  := iY - Trunc(sin(DegreesToRadians(60 + iRotation)) * iInnerRadius)+idy1;
  aPoints[4].x  := iX + Trunc(cos(DegreesToRadians(90 + iRotation)) * iInnerRadius)+idx1;
  aPoints[4].y  := iY - Trunc(sin(DegreesToRadians(90 + iRotation)) * iInnerRadius)+idy1;
  aPoints[5].x  := iX + Trunc(cos(DegreesToRadians(90 + iRotation)) * iOuterRadius);
  aPoints[5].y  := iY - Trunc(sin(DegreesToRadians(90 + iRotation)) * iOuterRadius);
  aPoints[6].x  := iX + Trunc(cos(DegreesToRadians(75 + iRotation)) * iOuterRadius)+idx2;
  aPoints[6].y  := iY - Trunc(sin(DegreesToRadians(75 + iRotation)) * iOuterRadius)+idy2;
  aPoints[7].x  := iX + Trunc(cos(DegreesToRadians(60 + iRotation)) * iOuterRadius)+idx2;
  aPoints[7].y  := iY - Trunc(sin(DegreesToRadians(60 + iRotation)) * iOuterRadius)+idy2;
  aPoints[8].x  := iX + Trunc(cos(DegreesToRadians(45 + iRotation)) * iOuterRadius)+idx2;
  aPoints[8].y  := iY - Trunc(sin(DegreesToRadians(45 + iRotation)) * iOuterRadius)+idy2;
  aPoints[9].x  := iX + Trunc(cos(DegreesToRadians(30 + iRotation)) * iOuterRadius)+idx2;
  aPoints[9].y  := iY - Trunc(sin(DegreesToRadians(30 + iRotation)) * iOuterRadius)+idy2;
  aPoints[10].x := iX + Trunc(cos(DegreesToRadians(15 + iRotation)) * iOuterRadius)+idx2;
  aPoints[10].y := iY - Trunc(sin(DegreesToRadians(15 + iRotation)) * iOuterRadius)+idy2;
  aPoints[11].x := iX + Trunc(cos(DegreesToRadians( 0 + iRotation)) * iOuterRadius);
  aPoints[11].y := iY - Trunc(sin(DegreesToRadians( 0 + iRotation)) * iOuterRadius)+idy2;

  FCanvas.PolygonOffset(aPoints);
  FCanvas.Brush.Bitmap := Nil;
End;


Procedure TWPPrintCanvas.DrawPolyLine(aColour : TColour; aPenStyle : TFslPenStyle; aEndStyle : TFslPenEndStyle; iXOffset, iYOffset : Integer; oCoords : TWPCoordinateList; rFactorX, rFactorY : Real; bClosed : Boolean = true);
Var
  aPoints : Array Of TPoint;
  iLoop : Integer;
Begin
  If oCoords.Count > 0 Then
  Begin
    FCanvas.Pen.Width := 1;
    FCanvas.Pen.Colour := aColour;
    FCanvas.Pen.Style := aPenStyle;
    FCanvas.Pen.EndStyle := apesRound;
    FCanvas.Pen.JoinStyle := apjsRound;
    If bClosed Then
    Begin
      SetLength(aPoints, oCoords.Count+1);
      aPoints[oCoords.Count].X := iXOffset + round(oCoords[0].X*rFactorX);
      aPoints[oCoords.Count].Y := iXOffset + round(oCoords[0].Y*rFactorX);
    End
    Else
      SetLength(aPoints, oCoords.Count);
    For iLoop := 0 To oCoords.Count - 1 Do
    Begin
      aPoints[iLoop].X := iXOffset + round(oCoords[iLoop].X*rFactorX);
      aPoints[iLoop].Y := iYOffset + round(oCoords[iLoop].Y*rFactorX);
    End;
    FCanvas.Polyline(aPoints);
  End;
End;


Procedure TWPPrintCanvas.DrawImage(oImage : TFslGraphic; aCopyMode : TCopyMode; iLeft, iTop, iRight, iBottom : Integer);
Begin
  FCanvas.PrintImagePixels(oImage, iLeft, iTop, iRight - iLeft, iBottom - iTop);
End;


Procedure TWPPrintCanvas.ApplyFont;
Begin
  Canvas.Font.Assign(Font);
End;

Function TWPPrintCanvas.TextExtent(Const sText : String) : TSize;
Begin
  ApplyFont;
  Result := FCanvas.TextExtentPixels(sText, 0);
End;


Function TWPPrintCanvas.GetTextMetrics : TTextMetric;
Begin
  ApplyFont;
  Result := FCanvas.GetTextMetrics;
End;


Function TWPPrintCanvas.TextHeight(Const sText : String) : Integer;
Begin
  ApplyFont;
  Result := FCanvas.TextHeightPixels(sText);
End;


Function TWPPrintCanvas.TextWidth(Const sText : String) : Integer;
Begin
  ApplyFont;
  Result := FCanvas.TextWidthPixels(sText);
End;


Procedure TWPPrintCanvas.GetTextExtents(Const sText : String; Var aSize : TSize; Const Offsets : PWPIntegers);
Begin
  ApplyFont;
  FCanvas.GetTextExtents(sText, aSize, Offsets);
End;


Procedure TWPPrintCanvas.Clip(Const aRect : TRect);
Begin
//  FCanvas.Clip(aRect.Left, aRect.Top,  aRect.Right, aRect.Bottom);
End;


Procedure TWPPrintCanvas.Clip(oObject : TWPMapObject);
Begin
//  If oObject.HasClip Then
//    FCanvas.Clip(oObject.ClipLeft, oObject.ClipTop,  oObject.ClipRight, oObject.ClipBottom);
End;


Procedure TWPPrintCanvas.UnClip;
Begin
//  FCanvas.UnClip;
End;


Procedure TWPPrintCanvas.MoveVertical(iTop, iOffset : Integer);
Begin
  RaiseError('MoveVertical', 'Not supported for printing');
End;


Function TWPPrintCanvas.DPIX : Integer;
Begin
  Result := Canvas.VCLCanvas.PixelsPerInchX;
End;


Function TWPPrintCanvas.DPIY : Integer;
Begin
  Result := Canvas.VCLCanvas.PixelsPerInchY;
End;


procedure TWPPrintCanvas.DrawRoundOutline(aColour: TColour; iLeft, iTop, iRight, iBottom, iRadius: Integer);
begin
  FCanvas.Pen.Colour := aColour;
  FCanvas.Pen.Style := apsSolid;
  FCanvas.Brush.Style := absNull;
  FCanvas.BoxRoundedPixels(iLeft, iTop, iRight - iLeft, iBottom - iTop, iRadius);
  FCanvas.Brush.Colour := clWhite;
  FCanvas.Brush.Style := absNull;
end;


function TWPPrintCanvas.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCanvas.sizeInBytes);
end;

End.
