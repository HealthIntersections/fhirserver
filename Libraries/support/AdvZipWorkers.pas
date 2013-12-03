Unit AdvZipWorkers;

{! 2 !}

Interface

Uses
  AdvStreams,
  AdvZipParts,
  AdvObjects;

Type
  TAdvZipWorker = Class (TAdvObject)
    Private
      FStream : TAdvStream;
      FParts : TAdvZipPartList;
      Function GetStream : TAdvStream;
      Function GetParts : TAdvZipPartList;
      Procedure SetStream(oValue : TAdvStream);
      Procedure SetParts(oValue : TAdvZipPartList);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function HasStream : Boolean;
      Function HasParts : Boolean;

      Property Stream : TAdvStream Read GetStream Write SetStream;
      Property Parts : TAdvZipPartList Read GetParts Write SetParts;
  End;

Implementation


Constructor TAdvZipWorker.Create;
Begin
  Inherited;
  FParts := TAdvZipPartList.Create;
End;

Destructor TAdvZipWorker.Destroy;
Begin
  FStream.Free;
  FParts.Free;
  Inherited;
End;

Function TAdvZipWorker.GetParts: TAdvZipPartList;
Begin
  Assert(Invariants('GetParts', FParts, TAdvZipPartList, 'Parts'));
  Result := FParts;
End;

Function TAdvZipWorker.GetStream: TAdvStream;
Begin
  Assert(Invariants('GetStream', FStream, TAdvStream, 'Stream'));
  Result := FStream;
End;

Function TAdvZipWorker.HasParts: Boolean;
Begin
  Result := FParts <> Nil;
End;

Function TAdvZipWorker.HasStream: Boolean;
Begin
  Result := FStream <> Nil;
End;

Procedure TAdvZipWorker.SetParts(oValue: TAdvZipPartList);
Begin
  FParts.Free;
  FParts := oValue;
End;

Procedure TAdvZipWorker.SetStream(oValue: TAdvStream);
Begin
  FStream.Free;
  FStream := oValue;
End;

End.

