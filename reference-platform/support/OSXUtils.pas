unit OSXUtils;

interface

{$IFDEF OSX}

function InterlockedDecrement(var i : integer) :integer;
function InterlockedIncrement(var i : integer) :integer;

{$ENDIF}

implementation

{$IFDEF OSX}

function InterlockedDecrement(var i : integer) :integer;
begin
  AtomicDecrement(i);
  result := i;
end;

function InterlockedIncrement(var i : integer) :integer;
begin
  AtomicIncrement(i);
  result := i;
end;

{$ENDIF}


end.
