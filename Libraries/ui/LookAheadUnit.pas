unit LookAheadUnit;


{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, ExtCtrls;

type
  TLookAheadEdit = class(StdCtrls.TComboBox)
  private
    FStoredItems: TStringList;
    procedure FilterItems;
    procedure StoredItemsChange(Sender: TObject);
    procedure SetStoredItems(const Value: TStringList);
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StoredItems: TStringList read FStoredItems write SetStoredItems;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mine', [TLookAheadEdit]);
end;

constructor TLookAheadEdit.Create(AOwner: TComponent);
begin
  inherited;
  AutoComplete := False;
  FStoredItems := TStringList.Create;
  FStoredItems.OnChange := StoredItemsChange;
end;

destructor TLookAheadEdit.Destroy;
begin
  FStoredItems.Free;
  inherited;
end;

procedure TLookAheadEdit.CNCommand(var AMessage: TWMCommand);
begin
  inherited;
  if AMessage.NotifyCode = CBN_EDITUPDATE then
    FilterItems;
end;

procedure TLookAheadEdit.FilterItems;
type
  TSelection = record
    StartPos, EndPos: Integer;
  end;
var
  I: Integer;
  Selection: TSelection;
  xText: string;
begin
  // store the current combo edit selection
  SendMessage(Handle, CB_GETEDITSEL, WPARAM(@Selection.StartPos), LPARAM(@Selection.EndPos));

  // begin with the items update
  Items.BeginUpdate;
  try
    if Text <> '' then
    begin
      // clear all items
      Items.Clear;
      for I := 0 to FStoredItems.Count - 1 do
        if ContainsText(FStoredItems[I], Text) then
//        if Pos( lowercase(Text), lowercase(FStoredItems[I]))>0 then begin
          Items.Add(FStoredItems[I]);
    end
    else
    begin
      // else the combo edit is empty
      // so then we'll use all what we have in the FStoredItems
      Items.Assign(FStoredItems)
    end;
  finally
    // finish the items update
    Items.EndUpdate;
  end;

  // restore the last combo edit selection
  xText := Text;
  SendMessage(Handle, CB_SHOWDROPDOWN, Integer(True), 0);
  SendMessage(Handle, WM_SETCURSOR, 0, 0); // work around for cursor display bug
  if (Items<>nil) and (Items.Count>0) then begin
    ItemIndex := 0;
  end else begin
    ItemIndex := -1;
  end;
  Text := xText;
  SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Selection.StartPos, Selection.EndPos));
end;

procedure TLookAheadEdit.StoredItemsChange(Sender: TObject);
begin
//  if Assigned(FStoredItems) then
//    FilterItems;
end;

procedure TLookAheadEdit.SetStoredItems(const Value: TStringList);
begin
  if Assigned(FStoredItems) then
    FStoredItems.Assign(Value)
  else
    FStoredItems := Value;
end;


end.
