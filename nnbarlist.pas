unit nnBarList;

{$mode objfpc}{$H+}

interface

uses
	Classes, syncobjs, nnLog, nnTypes, Ap;

type
	PBarList = ^TBarList;

	TBarList = class(TObject)
	private
		fData: TBarArray;
		fPerInc: TReal1DArray; // Изменение цены
		fPerVal: TReal1DArray; // Изменение объема
		fLog: PLog;
		fCS: TCriticalSection;
		function Get(Index: Integer): TBar;
		procedure Put(Index: Integer; aBar: TBar);
		function GetPerInc(Index: Integer): Double;
		procedure AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
	public
		constructor Create;
		destructor Destroy; override;
		procedure SetLogPointer(const aPointer: PLog);
		procedure LoadFromFile(const aFileName: String);
		procedure Add(aBar: TBar);
		function Count: Integer;
		function GetPerIncList: TReal1DArray; overload;
		function GetPerIncList(aStart, aCount: Integer): TReal1DArray; overload;
		function GetPerValList: TReal1DArray; overload;
		function GetPerValList(aStart, aCount: Integer): TReal1DArray; overload;
		function GetTrainingData(const aTrainCount: Word;
			const aClassCount: Byte;
			const aInnerCount: Byte;
			out aRangeList: TRealRangeList): TReal1DArray;
		function GetTrainingDataV(const aTrainCount: Word;
			const aClassCount: Byte;
			const aInnerCount: Byte;
			out aRangeList: TRealRangeList;
			out aVolData: TReal1DArray): TReal1DArray;
		function GetRangeVolData(const aCount: Integer): TReal1DArray;
		property Bar[Index: Integer]: TBar read Get write Put; default;
		property PerInc[Index: Integer]: Double read GetPerInc;
	end;

	function DefRanges(const aValues: TReal1DArray; 
		const aRangeList: TRealRangeList): TReal1DArray;

var
	SizeOfTestSamples: Word;


implementation

uses
	SysUtils, Math;

function DefRange(const aValue: Double; 
	const aRangeList: TRealRangeList): Double;
var
	i: Integer;
begin
	DefRange := 0;
	for i := 0 to High(aRangeList) do 
	begin
		if InRange(aValue, aRangeList[i].min, aRangeList[i].max) then 
		begin
			DefRange := i + 1;
			break;
		end;
	end;
	if (DefRange = 0) then
	begin
		if (aValue < 1) then 
		begin
			DefRange := 1;
		end else
		begin
			DefRange := Length(aRangeList);
		end;
	end;
end;

function DefRanges(const aValues: TReal1DArray; 
	const aRangeList: TRealRangeList): TReal1DArray;
var
	i: Integer;
begin
	SetLength(DefRanges, Length(aValues));
	for i := 0 to High(aValues) do 
	begin
		DefRanges[i] := DefRange(aValues[i], aRangeList);
	end;
end;

function StrToBar(aStr: String): TBar;
var
	date, time, open, hight, low, close, volume, buff: String;
	i, j, k: Byte;
begin
	j := 0;
	for i := 1 to 7 do
	begin
		k := AnsiPos(',', aStr);
		if k = 0 then
		begin
			volume := aStr;
			Break;
		end;
		buff := Copy(aStr, 1, k - 1); 
		Inc(j);
		case i of
			1: date  := buff;
			2: time  := buff;
			3: open  := buff;
			4: hight := buff;
			5: low   := buff;
			6: close := buff;
		end;
		Delete(aStr, 1, k);
	end;

	DefaultFormatSettings.DateSeparator := '.';
	DefaultFormatSettings.TimeSeparator := ':';

	StrToBar.datetime := StrToDateTime(date + ' ' + time);
	StrToBar.open     := StrToCurr(open);
	StrToBar.hight    := StrToCurr(hight);
	StrToBar.low      := StrToCurr(low);
	StrToBar.close    := StrToCurr(close);
	StrToBar.volume   := StrToInt(volume);
end;

procedure qSort(var A: TReal1DArray; min, max: Integer);
var 
	i, j: Integer;
	tmp, supp: Double;
begin
	supp := A[max - ((max - min) div 2)];
	i := min;
	j := max;
	while (i < j) do
	begin
		while (A[i] < supp) do i := i + 1;
		while (A[j] > supp) do j := j - 1;
		if (i <= j) then
		begin
			tmp := A[i];
			A[i] := A[j];
			A[j] := tmp;
			i := i + 1;
			j := j - 1;
		end;
	end;
	if (min < j) then qSort(A, min, j);
	if (i < max) then qSort(A, i, max);
end;

function IndexOfDouble(const aArray: TReal1DArray; 
	const aValue: Double): Integer;
var
	i: Integer;
begin
	for i := 0 to High(aArray) do 
	begin
		if (aArray[i] = aValue) then 
		begin
			indexOfDouble := i;
			exit;
		end;
	end;
	indexOfDouble := -1;
end;

function UniqueValues(const aArray: TReal1DArray): TReal1DArray;
var
	i, l: Integer;
begin
	l := 0;
	SetLength(UniqueValues, l);
	for i := 0 to High(aArray) do 
	begin
		if (indexOfDouble(UniqueValues, aArray[i]) = -1) then
		begin
			Inc(l);
			SetLength(UniqueValues, l);
			UniqueValues[Pred(l)] := aArray[i];
		end;
	end;
	qSort(UniqueValues, 0, High(UniqueValues));
end;

function SumInt(aArray: TInteger1DArray): LongInt;
var
	i: Integer;
begin
	SumInt := 0;
	for i := 0 to High(aArray) do 
	begin
		SumInt := SumInt + aArray[i];
	end;
end;

function ValCount(const aValues, aTarget: TReal1DArray): TInteger1DArray;
var
	i: Integer;
begin
	SetLength(ValCount, Length(aValues));
	for i := 0 to High(aTarget) do 
	begin
		Inc(ValCount[indexOfDouble(aValues, aTarget[i])]);
	end;
end;

function BorderRanges(aValues: TReal1DArray; aCounts: TInteger1DArray;
	aRangeCount: Byte): TRealRangeList;
var
	aRange: TRealRange;
	i: Integer;
	aRangeSum: Integer;
	aBuff: Integer;
	aIdx: Integer;
begin
	SetLength(BorderRanges, 0);
	aRangeSum := Round(SumInt(aCounts) / aRangeCount);
	aBuff := 0;
	for i := 0 to High(aCounts) do 
	begin
		if (aBuff + aCounts[i] >= aRangeSum) then 
		begin
			aRange.min := 0;
			aRange.max := 0;
			if (Length(BorderRanges) < 1) then
			begin
				aRange.min := aValues[0];
			end else
			begin
				aIdx := indexOfDouble(aValues, BorderRanges[High(BorderRanges)].max);
				aRange.min := aValues[aIdx + 1];
			end;
			if (Abs(aBuff - aRangeSum) <= 
					Abs(aBuff + aCounts[i] - aRangeSum)) then 
			begin
				aRange.max := aValues[i - 1];
			end else
			begin
				aRange.max := aValues[i];
			end;
			aBuff := 0;
			SetLength(BorderRanges, Length(BorderRanges) + 1);
			BorderRanges[High(BorderRanges)] := aRange;
		end else
		begin
			aBuff := aBuff + aCounts[i];
		end;
	end;
	aIdx := indexOfDouble(aValues, BorderRanges[High(BorderRanges)].max);
	aRange.min := aValues[aIdx + 1];
	aRange.max := aValues[High(aValues)];
	SetLength(BorderRanges, Length(BorderRanges) + 1);
	BorderRanges[High(BorderRanges)] := aRange;
end;

{ TBarList }

function TBarList.Get(Index: Integer): TBar;
begin
	fCS.Enter;
	if (Index >= 0) and (Index < Count) then
	begin
		Get := fData[Index];
	end;
	fCS.Leave;
end;

procedure TBarList.Put(Index: Integer; aBar: TBar);
begin
	fCS.Enter;
	if (Index >= 0) and (Index < Count) then
	begin
		fData[Index] := aBar;
	end;
	fCS.Leave;
end;

function TBarList.GetPerInc(Index: Integer): Double;
begin
	fCS.Enter;
	if (Index >= 0) and (Index < Count) then
	begin
		GetPerInc := fPerInc[Index];
	end;
	fCS.Leave;
end;

procedure TBarList.AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
begin
	if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg, aMsgType);
end;

constructor TBarList.Create;
begin
	inherited Create;
	fLog := nil;
	fCS := TCriticalSection.Create;
end;

destructor TBarList.Destroy;
begin
	FreeAndNil(fCS);
	inherited Destroy;
end;

procedure TBarList.SetLogPointer(const aPointer: PLog);
begin
	fLog := aPointer;
end;

procedure TBarList.LoadFromFile(const aFileName: String);
var
	F: TextFile;
	Buff: String;
begin
	if not FileExists(aFileName) then
	begin
		AddToLog('Файл ' + aFileName + ' не найден', Error);
		Exit;
	end;

	AssignFile(F, aFileName);
	Reset(F);
	SetLength(fData, 0);

	try
		while not Eof(F) do
		begin
			ReadLn(F, Buff);
			Add(StrToBar(Buff));
		end;
	finally
		CloseFile(F);
	end;

	AddToLog('Обработано ' + IntToStr(Count) +
		' записей из файла ' + aFileName, Info);
end;

procedure TBarList.Add(aBar: TBar);
var
	l: Integer;
begin
	fCS.Enter;
	l := Count;
	Inc(l);
	SetLength(fData, l);
	SetLength(fPerInc, l);
	SetLength(fPerVal, l);
	fData[Pred(l)] := aBar;
	if (l > 1) then
	begin
		fPerInc[Pred(l)] := RoundTo(aBar.close / Bar[l - 2].close, -5);
		fPerVal[Pred(l)] := RoundTo(aBar.volume / Bar[l - 2].volume, -5);
	end else
	begin
		fPerInc[Pred(l)] := RoundTo(aBar.close / aBar.open, -5);
		fPerVal[Pred(l)] := 1;
	end;
	fCS.Leave;
end;

function TBarList.Count: Integer;
begin
	fCS.Enter;
	Count := Length(fData);
	fCS.Leave;
end;

function TBarList.GetPerIncList: TReal1DArray;
begin
	fCS.Enter;
	GetPerIncList := fPerInc;
	fCS.Leave;
end;

function TBarList.GetPerIncList(aStart, aCount: Integer): TReal1DArray;
begin
	fCS.Enter;
	GetPerIncList := Copy(fPerInc, aStart, aCount);
	fCS.Leave;
end;

function TBarList.GetPerValList: TReal1DArray;
begin
	fCS.Enter;
	GetPerValList := fPerVal;
	fCS.Leave;
end;

function TBarList.GetPerValList(aStart, aCount: Integer): TReal1DArray;
begin
	fCS.Enter;
	GetPerValList := Copy(fPerVal, aStart, aCount);
	fCS.Leave;
end;

function TBarList.GetTrainingData(const aTrainCount: Word;
	const aClassCount: Byte; const aInnerCount: Byte; 
	out aRangeList: TRealRangeList): TReal1DArray;
var
	l: Integer;
	aPerInc, aUniqueValues: TReal1DArray;
	aValCount: TInteger1DArray;
begin
	l := aTrainCount + aInnerCount + SizeOfTestSamples;
	fCS.Enter;
	aPerInc := GetPerIncList;
	GetTrainingData := GetPerIncList(Count - l, l);
	fCS.Leave;
	aUniqueValues := UniqueValues(aPerInc);
	aValCount := ValCount(aUniqueValues, aPerInc);
	aRangeList := BorderRanges(aUniqueValues, aValCount, aClassCount);
end;

function TBarList.GetTrainingDataV(const aTrainCount: Word;
	const aClassCount: Byte; const aInnerCount: Byte;
	out aRangeList: TRealRangeList; out aVolData: TReal1DArray): TReal1DArray;
var
	l: Integer;
	aPerInc, aUniqueValues: TReal1DArray;
	aValCount: TInteger1DArray;
begin
	l := aTrainCount + aInnerCount + SizeOfTestSamples;
	fCS.Enter;
	aPerInc := GetPerIncList;
	GetTrainingDataV := GetPerIncList(Count - l, l);
	aVolData := GetRangeVolData(l);
	fCS.Leave;

	aUniqueValues := UniqueValues(aPerInc);
	aValCount := ValCount(aUniqueValues, aPerInc);
	aRangeList := BorderRanges(aUniqueValues, aValCount, aClassCount);
end;

function TBarList.GetRangeVolData(const aCount: Integer): TReal1DArray;
var
	aValDataBuff, aValDataBuffOut, aUniqueValues: TReal1DArray;
	aValCount: TInteger1DArray;
	aValRangeList: TRealRangeList;
begin
	fCS.Enter;
	aValDataBuff := GetPerValList;
	aValDataBuffOut := GetPerValList(Count - aCount, aCount);
	fCS.Leave;
	aUniqueValues := UniqueValues(aValDataBuff);
	aValCount := ValCount(aUniqueValues, aValDataBuff);
	aValRangeList := BorderRanges(aUniqueValues, aValCount, 5);
	GetRangeVolData := DefRanges(aValDataBuffOut, aValRangeList);
end;

initialization

SizeOfTestSamples := 50;

end.
