unit ParserSupport;

interface

Uses
  AdvObjects;

type
  TSourceLocation = record
    line, col : integer;
  end;

  TSourceLocationObject = class (TAdvObject)
  public
    locationStart : TSourceLocation;
    locationEnd : TSourceLocation;
  end;

const
  MAP_ATTR_NAME = 'B88BF977DA9543B8A5915C84A70F03F7';

function minLoc(src1, src2 : TSourceLocation) : TSourceLocation;
function maxLoc(src1, src2 : TSourceLocation) : TSourceLocation;
function nullLoc : TSourceLocation;
function isNullLoc(src : TSourceLocation) : boolean;
function locLessOrEqual(src1, src2 : TSourceLocation) : boolean;
function locGreatorOrEqual(src1, src2 : TSourceLocation) : boolean;

implementation

function minLoc(src1, src2 : TSourceLocation) : TSourceLocation;
begin
  if (src1.line < src2.line) then
    result := src1
  else if (src2.line < src1.line) then
    result := src2
  else if (src1.col < src2.col) then
    result := src1
  else
    result := src2
end;

function maxLoc(src1, src2 : TSourceLocation) : TSourceLocation;
begin
  if (src1.line > src2.line) then
    result := src1
  else if (src2.line > src1.line) then
    result := src2
  else if (src1.col > src2.col) then
    result := src1
  else
    result := src2
end;

function nullLoc : TSourceLocation;
begin
  result.line := -1;
  result.col := -1;
end;


function isNullLoc(src : TSourceLocation) : boolean;
begin
  result := (src.line = -1) and (src.col = -1);
end;

function locLessOrEqual(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.line < src2.line then
    result := true
  else if src1.line > src2.line then
    result := false
  else
    result := src1.col <= src2.col;
end;

function locGreatorOrEqual(src1, src2 : TSourceLocation) : boolean;
begin
  if src1.line > src2.line then
    result := true
  else if src1.line < src2.line then
    result := false
  else
    result := src1.col >= src2.col;
end;


end.
