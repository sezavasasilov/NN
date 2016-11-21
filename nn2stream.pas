unit nn2Stream;

{$mode objfpc}{$H+}

interface

uses
	Classes, nnTypes, mlpbase, Ap;

procedure WriteTMLP(aStream: TStream; const aMLP: TMLP);
procedure ReadTMLP(aStream: TStream; out aMLP: TMLP);
function EoS(Stream: TStream): Boolean;

implementation

function EoS(Stream: TStream): Boolean;
begin
  EoS := (Stream.Position >= Stream.Size);
end;

procedure WriteBufferDyn(aStream: TStream; 
  const aArray: TInteger1DArray); overload;
var
  Len: Longint;
begin
  Len := Length(aArray);
  aStream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    aStream.WriteBuffer(aArray[0], Len * SizeOf(aArray[0]));
end;

procedure ReadBufferDyn(aStream: TStream; 
  out aArray: TInteger1DArray); overload;
var
  Len: Longint;
begin
  aStream.ReadBuffer(Len, SizeOf(Len));
  SetLength(aArray, Len);
  if Len > 0 then
    aStream.ReadBuffer(aArray[0], Len * SizeOf(aArray[0]));
end;

procedure WriteBufferDyn(aStream: TStream; 
  const aArray: TInteger2DArray); overload;
var
  Len: Longint;
  i: LongWord;
begin
  Len := Length(aArray);
  aStream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    for i := 0 to Pred(Len) do 
    begin
      WriteBufferDyn(aStream, aArray[i]);
    end;
end;

procedure ReadBufferDyn(aStream: TStream; 
  out aArray: TInteger2DArray); overload;
var
  Len: Longint;
  i: LongWord;
begin
  aStream.ReadBuffer(Len, SizeOf(Len));
  SetLength(aArray, Len);
  if Len > 0 then
    for i := 0 to Pred(Len) do 
    begin
      ReadBufferDyn(aStream, aArray[i]);
    end;
end;

procedure WriteBufferDyn(aStream: TStream; 
  const aArray: TReal1DArray); overload;
var
  Len: Longint;
begin
  Len := Length(aArray);
  aStream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    aStream.WriteBuffer(aArray[0], Len * SizeOf(aArray[0]));
end;

procedure ReadBufferDyn(aStream: TStream; 
  out aArray: TReal1DArray); overload;
var
  Len: Longint;
begin
  aStream.ReadBuffer(Len, SizeOf(Len));
  SetLength(aArray, Len);
  if Len > 0 then
    aStream.ReadBuffer(aArray[0], Len * SizeOf(aArray[0]));
end;

procedure WriteBufferDyn(aStream: TStream; 
  const aArray: TReal2DArray); overload;
var
  Len: Longint;
  i: LongWord;
begin
  Len := Length(aArray);
  aStream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    for i := 0 to Pred(Len) do 
    begin
      WriteBufferDyn(aStream, aArray[i]);
    end;
end;

procedure ReadBufferDyn(aStream: TStream; 
  out aArray: TReal2DArray); overload;
var
  Len: Longint;
  i: LongWord;
begin
  aStream.ReadBuffer(Len, SizeOf(Len));
  SetLength(aArray, Len);
  if Len > 0 then
    for i := 0 to Pred(Len) do 
    begin
      ReadBufferDyn(aStream, aArray[i]);
    end;
end;

procedure WriteMLP(aStream: TStream; const aMLP: MultiLayerPerceptron);
begin
  WriteBufferDyn(aStream, aMLP.StructInfo);
  WriteBufferDyn(aStream, aMLP.Weights);
  WriteBufferDyn(aStream, aMLP.ColumnMeans);
  WriteBufferDyn(aStream, aMLP.ColumnSigmas);
  WriteBufferDyn(aStream, aMLP.Neurons);
  WriteBufferDyn(aStream, aMLP.DFDNET);
  WriteBufferDyn(aStream, aMLP.DError);
  WriteBufferDyn(aStream, aMLP.X);
  WriteBufferDyn(aStream, aMLP.Y);
  WriteBufferDyn(aStream, aMLP.Chunks);
  WriteBufferDyn(aStream, aMLP.NWBuf);
end;

procedure ReadMLP(aStream: TStream; out aMLP: MultiLayerPerceptron);
begin
  ReadBufferDyn(aStream, aMLP.StructInfo);
  ReadBufferDyn(aStream, aMLP.Weights);
  ReadBufferDyn(aStream, aMLP.ColumnMeans);
  ReadBufferDyn(aStream, aMLP.ColumnSigmas);
  ReadBufferDyn(aStream, aMLP.Neurons);
  ReadBufferDyn(aStream, aMLP.DFDNET);
  ReadBufferDyn(aStream, aMLP.DError);
  ReadBufferDyn(aStream, aMLP.X);
  ReadBufferDyn(aStream, aMLP.Y);
  ReadBufferDyn(aStream, aMLP.Chunks);
  ReadBufferDyn(aStream, aMLP.NWBuf);
end;

procedure WriteRange(aStream: TStream; const aRange: TRealRange); overload;
begin
  aStream.WriteBuffer(aRange.min, SizeOf(aRange.min));
  aStream.WriteBuffer(aRange.max, SizeOf(aRange.max));
end;

procedure ReadRange(aStream: TStream; out aRange: TRealRange); overload;
begin
  aStream.ReadBuffer(aRange.min, SizeOf(aRange.min));
  aStream.ReadBuffer(aRange.max, SizeOf(aRange.max));
end;

procedure WriteRange(aStream: TStream; const aRange: TIntRange); overload;
begin
  aStream.WriteBuffer(aRange.min, SizeOf(aRange.min));
  aStream.WriteBuffer(aRange.max, SizeOf(aRange.max));
end;

procedure ReadRange(aStream: TStream; out aRange: TIntRange); overload;
begin
  aStream.ReadBuffer(aRange.min, SizeOf(aRange.min));
  aStream.ReadBuffer(aRange.max, SizeOf(aRange.max));
end;

procedure WriteRangeList(aStream: TStream; 
  const aRangeList: TRealRangeList); overload;
var
  Len: Longint;
  i: LongWord;
begin
  Len := Length(aRangeList);
  aStream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    for i := 0 to Pred(Len) do 
    begin
      WriteRange(aStream, aRangeList[i]);
    end;
end;

procedure ReadRangeList(aStream: TStream; 
  out aRangeList: TRealRangeList); overload;
var
  Len: Longint;
  i: LongWord;
begin
  aStream.ReadBuffer(Len, SizeOf(Len));
  SetLength(aRangeList, Len);
  if Len > 0 then
    for i := 0 to Pred(Len) do 
    begin
      ReadRange(aStream, aRangeList[i]);
    end;
end;

procedure WriteRangeList(aStream: TStream; 
  const aRangeList: TIntRangeList); overload;
var
  Len: Longint;
  i: LongWord;
begin
  Len := Length(aRangeList);
  aStream.WriteBuffer(Len, SizeOf(Len));
  if Len > 0 then
    for i := 0 to Pred(Len) do 
    begin
      WriteRange(aStream, aRangeList[i]);
    end;
end;

procedure ReadRangeList(aStream: TStream; 
  out aRangeList: TIntRangeList); overload;
var
  Len: Longint;
  i: LongWord;
begin
  aStream.ReadBuffer(Len, SizeOf(Len));
  SetLength(aRangeList, Len);
  if Len > 0 then
    for i := 0 to Pred(Len) do 
    begin
      ReadRange(aStream, aRangeList[i]);
    end;
end;

procedure WriteTMLP(aStream: TStream; const aMLP: TMLP);
begin
	aStream.WriteBuffer(aMLP.id, SizeOf(aMLP.id));
  aStream.WriteBuffer(aMLP.withVolume, SizeOf(aMLP.withVolume));
  WriteMLP(aStream, aMLP.MLP);
  aStream.WriteBuffer(aMLP.trainCount, SizeOf(aMLP.trainCount));
  aStream.WriteBuffer(aMLP.innerCount, SizeOf(aMLP.innerCount));
  aStream.WriteBuffer(aMLP.hideCount, SizeOf(aMLP.hideCount));
  aStream.WriteBuffer(aMLP.classCount, SizeOf(aMLP.classCount));
  WriteRangeList(aStream, aMLP.rangeList);
  aStream.WriteBuffer(aMLP.trainTime, SizeOf(aMLP.trainTime));
end;

procedure ReadTMLP(aStream: TStream; out aMLP: TMLP);
begin
  aStream.ReadBuffer(aMLP.id, SizeOf(aMLP.id));
  aStream.ReadBuffer(aMLP.withVolume, SizeOf(aMLP.withVolume));
  ReadMLP(aStream, aMLP.MLP);
  aStream.ReadBuffer(aMLP.trainCount, SizeOf(aMLP.trainCount));
  aStream.ReadBuffer(aMLP.innerCount, SizeOf(aMLP.innerCount));
  aStream.ReadBuffer(aMLP.hideCount, SizeOf(aMLP.hideCount));
  aStream.ReadBuffer(aMLP.classCount, SizeOf(aMLP.classCount));
  ReadRangeList(aStream, aMLP.rangeList);
  aStream.ReadBuffer(aMLP.trainTime, SizeOf(aMLP.trainTime));
end;

end.
