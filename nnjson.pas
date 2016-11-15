unit nnJson;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils, nnTypes, mlpbase, mlptrain, Ap;

  function Integer1DArray2JSON(const aArray: TInteger1DArray): AnsiString;
  function JSON2Integer1DArray(aJSON: AnsiString): TInteger1DArray;
  function Integer2DArray2JSON(const aArray: TInteger2DArray): AnsiString;
  function JSON2Integer2DArray(aJSON: AnsiString): TInteger2DArray;

  function Real1DArray2JSON(const aArray: TReal1DArray): AnsiString;
  function JSON2Real1DArray(aJSON: AnsiString): TReal1DArray;
  function Real2DArray2JSON(const aArray: TReal2DArray): AnsiString;
  function JSON2Real2DArray(aJSON: AnsiString): TReal2DArray;

  function MLP2JSON(const aMLP: MultiLayerPerceptron): AnsiString;
  function JSON2MLP(aJson: AnsiString): MultiLayerPerceptron;

  function Range2JSON(const aRange: TRealRange): AnsiString;
  function JSON2Range(aJson: AnsiString): TRealRange;
  function RangeList2JSON(const aRangeList: TRealRangeList): AnsiString;
  function JSON2RangeList(aJson: AnsiString): TRealRangeList;

  function MLPReport2JSON(const aMLPReport: MLPReport): AnsiString;
  function JSON2MLPReport(aJson: AnsiString): MLPReport;

  function TMLP2JSON(const aMLP: TMLP): AnsiString;
  function JSON2TMLP(aJson: AnsiString): TMLP;

implementation

function Integer1DArray2JSON(const aArray: TInteger1DArray): AnsiString;
var
  _s: AnsiString;
  i: Integer;
begin
  _s := '[';
  for i := 0 to High(aArray) do
  begin
    if i < High(aArray) then
      _s := _s + IntToStr(aArray[i]) + ','
    else
      _s := _s + IntToStr(aArray[i]) + ']';
  end;
  Integer1DArray2JSON := _s;
end;

function JSON2Integer1DArray(aJSON: AnsiString): TInteger1DArray;
var
  p, l: Integer;
  buff: AnsiString;
begin
  if (aJSON[1] = '[') and (aJSON[High(aJSON)] = ']') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  l := 0;
  while Pos(',', aJSON) > 0 do
  begin
    p := Pos(',', aJSON);
    buff := Copy(aJSON, 1, p - 1);
    Delete(aJSON, 1, p);
    Inc(l);
    SetLength(JSON2Integer1DArray, l);
    JSON2Integer1DArray[l - 1] := StrToInt(buff);
  end;
  Inc(l);
  SetLength(JSON2Integer1DArray, l);
  JSON2Integer1DArray[l - 1] := StrToInt(aJSON);
end;

function Integer2DArray2JSON(const aArray: TInteger2DArray): AnsiString;
var
  _s: AnsiString;
  i: Integer;
begin
  _s := '[';
  for i := 0 to High(aArray) do
  begin
    if i < High(aArray) then
      _s := _s + Integer1DArray2JSON(aArray[i]) + ','
    else
      _s := _s + Integer1DArray2JSON(aArray[i]) + ']';
  end;
  Integer2DArray2JSON := _s;
end;

function JSON2Integer2DArray(aJSON: AnsiString): TInteger2DArray;
var
  p, q, l: Integer;
  buff: AnsiString;
begin
  if (aJSON[1] = '[') and (aJSON[High(aJSON)] = ']') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  l := 0;
  while Pos('[', aJSON) > 0 do
  begin
    p := Pos('[', aJSON);
    q := Pos(']', aJSON);
    buff := Copy(aJSON, p, q - p + 1);
    writeln(buff);
    Delete(aJSON, 1, q + 1);
    Inc(l);
    SetLength(JSON2Integer2DArray, l);
    JSON2Integer2DArray[l - 1] := JSON2Integer1DArray(buff);
  end;
end;

function Real1DArray2JSON(const aArray: TReal1DArray): AnsiString;
var
  _s: AnsiString;
  i: Integer;
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  _s := '[';
  for i := 0 to High(aArray) do
  begin
    if i < High(aArray) then
      _s := _s + FloatToStr(aArray[i]) + ','
    else
      _s := _s + FloatToStr(aArray[i]) + ']';
  end;
  Real1DArray2JSON := _s;
end;

function JSON2Real1DArray(aJSON: AnsiString): TReal1DArray;
var
  p, l: Integer;
  buff: AnsiString;
begin
  if (aJSON[1] = '[') and (aJSON[High(aJSON)] = ']') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  l := 0;
  while Pos(',', aJSON) > 0 do
  begin
    p := Pos(',', aJSON);
    buff := Copy(aJSON, 1, p - 1);
    Delete(aJSON, 1, p);
    Inc(l);
    SetLength(JSON2Real1DArray, l);
    DefaultFormatSettings.DecimalSeparator := '.';
    JSON2Real1DArray[l - 1] := StrToFloat(buff);
  end;
  Inc(l);
  SetLength(JSON2Real1DArray, l);
  JSON2Real1DArray[l - 1] := StrToFloat(aJSON);
end;

function Real2DArray2JSON(const aArray: TReal2DArray): AnsiString;
var
  _s: AnsiString;
  i: Integer;
begin
  _s := '[';
  for i := 0 to High(aArray) do
  begin
    if i < High(aArray) then
      _s := _s + Real1DArray2JSON(aArray[i]) + ','
    else
      _s := _s + Real1DArray2JSON(aArray[i]) + ']';
  end;
  Real2DArray2JSON := _s;
end;

function JSON2Real2DArray(aJSON: AnsiString): TReal2DArray;
var
  p, q, l: Integer;
  buff: AnsiString;
begin
  if (aJSON[1] = '[') and (aJSON[High(aJSON)] = ']') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  l := 0;
  while Pos('[', aJSON) > 0 do
  begin
    p := Pos('[', aJSON);
    q := Pos(']', aJSON);
    buff := Copy(aJSON, p, q - p + 1);
    Delete(aJSON, 1, q + 1);
    Inc(l);
    SetLength(JSON2Real2DArray, l);
    JSON2Real2DArray[l - 1] := JSON2Real1DArray(buff);
  end;
end;

function MLP2JSON(const aMLP: MultiLayerPerceptron): AnsiString;
var
  _s: String;
begin
  _s := '{';
  _s := _s + '"StructInfo":'  + Integer1DArray2JSON(aMLP.StructInfo) + ',';
  _s := _s + '"Weights":'           + Real1DArray2JSON(aMLP.Weights) + ',';
  _s := _s + '"ColumnMeans":'   + Real1DArray2JSON(aMLP.ColumnMeans) + ',';
  _s := _s + '"ColumnSigmas":' + Real1DArray2JSON(aMLP.ColumnSigmas) + ',';
  _s := _s + '"Neurons":'           + Real1DArray2JSON(aMLP.Neurons) + ',';
  _s := _s + '"DFDNET":'             + Real1DArray2JSON(aMLP.DFDNET) + ',';
  _s := _s + '"DError":'             + Real1DArray2JSON(aMLP.DError) + ',';
  _s := _s + '"X":'                       + Real1DArray2JSON(aMLP.X) + ',';
  _s := _s + '"Y":'                       + Real1DArray2JSON(aMLP.Y) + ',';
  _s := _s + '"Chunks":'             + Real2DArray2JSON(aMLP.Chunks) + ',';
  _s := _s + '"NWBuf":'               + Real1DArray2JSON(aMLP.NWBuf) + '}';
  MLP2JSON := _s;
end;

function JSON2MLP(aJson: AnsiString): MultiLayerPerceptron;
const
  nameList: array[0..10] of AnsiString = ('StructInfo','Weights','ColumnMeans',
           'ColumnSigmas','Neurons','DFDNET','DError','X','Y','Chunks','NWBuf');
  StructInfo   = 0;
  Weights      = 1;
  ColumnMeans  = 2;
  ColumnSigmas = 3;
  Neurons      = 4;
  DFDNET       = 5;
  DError       = 6;
  X            = 7;
  Y            = 8;
  Chunks       = 9;
  NWBuf        = 10;
var
  p: Integer;
  name, buff: AnsiString;
begin
  if (aJSON[1] = '{') and (aJSON[High(aJSON)] = '}') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  while Pos(':', aJSON) > 0 do
  begin
    p := Pos(':', aJSON);
    name := Copy(aJSON, 2, p - 3);
    Delete(aJSON, 1, p);
    if Pos(':', aJSON) > 0 then
    begin
      p := Pos('"', aJSON);
      buff := Copy(aJSON, 1, p - 2);
      Delete(aJSON, 1, p - 1);
    end else
    begin
      buff := aJSON;
    end;
    case AnsiIndexStr(name, nameList) of
      StructInfo   : JSON2MLP.StructInfo   := JSON2Integer1DArray(buff);
      Weights      : JSON2MLP.Weights      := JSON2Real1DArray(buff);
      ColumnMeans  : JSON2MLP.ColumnMeans  := JSON2Real1DArray(buff);
      ColumnSigmas : JSON2MLP.ColumnSigmas := JSON2Real1DArray(buff);
      Neurons      : JSON2MLP.Neurons      := JSON2Real1DArray(buff);
      DFDNET       : JSON2MLP.DFDNET       := JSON2Real1DArray(buff);
      DError       : JSON2MLP.DError       := JSON2Real1DArray(buff);
      X            : JSON2MLP.X            := JSON2Real1DArray(buff);
      Y            : JSON2MLP.Y            := JSON2Real1DArray(buff);
      Chunks       : JSON2MLP.Chunks       := JSON2Real2DArray(buff);
      NWBuf        : JSON2MLP.NWBuf        := JSON2Real1DArray(buff);
    end;
  end;
end;

function Range2JSON(const aRange: TRealRange): AnsiString;
var
  _s: AnsiString;
begin
  _s := '{';
  _s := _s + '"min":' + FloatToStr(aRange.min) + ',';
  _s := _s + '"max":' + FloatToStr(aRange.max) + '}';
  Range2JSON := _s;
end;

function JSON2Range(aJson: AnsiString): TRealRange;
var
  p, q: Integer;
  buff: AnsiString;
begin
  if (aJSON[1] = '{') and (aJSON[High(aJSON)] = '}') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  p := Pos(':', aJSON);
  q := Pos(',', aJson);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2Range.min := StrToFloat(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  Delete(aJSON, 1, p);
  JSON2Range.max := StrToFloat(aJson);
end;

function RangeList2JSON(const aRangeList: TRealRangeList): AnsiString;
var
  _s: AnsiString;
  i: Integer;
begin
  _s := '[';
  for i := 0 to High(aRangeList) do
  begin
    if i < High(aRangeList) then
      _s := _s + Range2JSON(aRangeList[i]) + ','
    else
      _s := _s + Range2JSON(aRangeList[i]) + ']';
  end;
  RangeList2JSON := _s;
end;

function JSON2RangeList(aJson: AnsiString): TRealRangeList;
var
  p, l: Integer;
  buff: AnsiString;
begin
  if (aJSON[1] = '[') and (aJSON[High(aJSON)] = ']') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  l := 0;
  SetLength(JSON2RangeList, l);
  while Pos('}', aJSON) > 0 do
  begin
    p := Pos('}', aJSON);
    buff := Copy(aJSON, 1, p);
    Inc(l);
    SetLength(JSON2RangeList, l);
    JSON2RangeList[Pred(l)] := JSON2Range(buff);
    Delete(aJSON, 1, p + 1);
  end;
end;

function MLPReport2JSON(const aMLPReport: MLPReport): AnsiString;
var
  _s: AnsiString;
begin
  _s := '{';
  _s := _s + '"NGrad":' + IntToStr(aMLPReport.NGrad) + ',';
  _s := _s + '"NHess":' + IntToStr(aMLPReport.NHess) + ',';
  _s := _s + '"NCholesky":' + IntToStr(aMLPReport.NCholesky) + '}';
  MLPReport2JSON := _s;
end;

function JSON2MLPReport(aJson: AnsiString): MLPReport;
var
  p, q: Integer;
  buff: AnsiString;
begin
  if (aJSON[1] = '{') and (aJSON[High(aJSON)] = '}') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  p := Pos(':', aJSON);
  q := Pos(',', aJson);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2MLPReport.NGrad := StrToInt(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',', aJson);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2MLPReport.NHess := StrToInt(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  Delete(aJSON, 1, p);
  JSON2MLPReport.NCholesky := StrToInt(aJson);
end;

function TMLP2JSON(const aMLP: TMLP): AnsiString;
var
  _s: AnsiString;
begin
  _s := '{';
  _s := _s + '"id":'                       + IntToStr(aMLP.id) + ',';
  _s := _s + '"MLP":'                     + MLP2JSON(aMLP.MLP) + ',';
  _s := _s + '"trainCount":'       + IntToStr(aMLP.trainCount) + ',';
  //_s := _s + '"Data":'           + Real1DArray2JSON(aMLP.Data) + ',';
  _s := _s + '"innerCount":'       + IntToStr(aMLP.innerCount) + ',';
  _s := _s + '"hideCount":'         + IntToStr(aMLP.hideCount) + ',';
  _s := _s + '"classCount":'       + IntToStr(aMLP.classCount) + ',';
  _s := _s + '"classRange":' + RangeList2JSON(aMLP.rangeList)  + ',';
  // _s := _s + '"R1":'                     + FloatToStr(aMLP.R1) + ',';
  // _s := _s + '"R2":'                     + FloatToStr(aMLP.R2) + ',';
  // _s := _s + '"Rep":'               + MLPReport2JSON(aMLP.Rep) + ',';
  // _s := _s + '"Info":'                   + IntToStr(aMLP.Info) + ',';
  _s := _s + '"trainTime":'       + FloatToStr(aMLP.trainTime) + '}';
  TMLP2JSON := _s;
end;

function JSON2TMLP(aJson: AnsiString): TMLP;
var
  buff: AnsiString;
  p, q: Integer;
begin
  if (aJSON[1] = '{') and (aJSON[High(aJSON)] = '}') then
  begin
    Delete(aJSON, 1, 1);
    Delete(aJSON, High(aJSON), 1);
  end else
  begin
    exit;
  end;
  p := Pos(':', aJSON);
  q := Pos(',', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2TMLP.id := StrToInt(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',"trainCount"', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2TMLP.MLP := JSON2MLP(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2TMLP.trainCount := StrToInt(buff);
  Delete(aJSON, 1, q);
  //p := Pos(':', aJSON);
  //q := Pos(',"innerCount"', aJSON);
  //buff := Copy(aJSON, p + 1, q - p - 1);
  //JSON2TMLP.Data := JSON2Real1DArray(buff);
  //Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2TMLP.innerCount := StrToInt(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2TMLP.hideCount := StrToInt(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2TMLP.classCount := StrToInt(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',"R1"', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  JSON2TMLP.rangeList := JSON2RangeList(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  // JSON2TMLP.R1 := StrToFloat(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  // JSON2TMLP.R2 := StrToFloat(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',"Info"', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  // JSON2TMLP.Rep := JSON2MLPReport(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  q := Pos(',', aJSON);
  buff := Copy(aJSON, p + 1, q - p - 1);
  // JSON2TMLP.Info := StrToInt(buff);
  Delete(aJSON, 1, q);
  p := Pos(':', aJSON);
  Delete(aJSON, 1, p);
  JSON2TMLP.trainTime := StrToFloat(aJSON);
end;

end.

