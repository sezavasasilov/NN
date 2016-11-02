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
		fPerInc: TReal1DArray;
		fLog: PLog;
		fCS: TCriticalSection;
    	function Get(Index: Integer): TBar;
    	procedure Put(Index: Integer; aBar: TBar);
    	function GetPerInc(Index: Integer): Double;
		procedure AddToLog(const aMsg: String);
	public
		constructor Create;
		destructor Destroy; override;
		procedure SetLogPointer(const aPointer: PLog);
		procedure LoadFromFile(const aFileName: String);
		procedure Add(aBar: TBar);
		function Count: Integer;
		function GetPerIncList: TReal1DArray; overload;
		function GetPerIncList(aStart, aCount: Integer): TReal1DArray; overload;
		// function GetTrainingData(const aTrainCount: Word;
		// 	const aClassCount: Byte;
		// 	const aInnerCount: Byte;
		// 	var aClassRange: TRealRangeList): TReal1DArray;
		property Bar[Index: Integer]: TBar read Get write Put; default;
		property PerInc[Index: Integer]: Double read GetPerInc;
	end;

implementation

uses
	SysUtils, Math;

function StrToBar(aStr: String): TBar;
var
	date, time, open, hight, low, close, value, buff: String;
	i, j, k: Byte;
begin
	j := 0;
	for i := 1 to 7 do
	begin
		k := AnsiPos(',', aStr);
		if k = 0 then
		begin
			value := aStr;
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
	StrToBar.value    := StrToInt(value);
end;

function UniqueValues(): Integer;
begin
	
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

procedure TBarList.AddToLog(const aMsg: String);
begin
	if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg);
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
		AddToLog('Файл ' + aFileName + ' не найден');
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
		' записей из файла ' + aFileName);
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
	fData[Pred(l)] := aBar;
	if (l > 1) then
	begin
		fPerInc[Pred(l)] := RoundTo(aBar.close / Bar[l - 2].close, -5);
	end else
	begin
		fPerInc[Pred(l)] := RoundTo(aBar.close / aBar.open, -5);
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

// function TBarList.GetTrainingData(const aTrainCount: Word;
// 	const aClassCount: Byte; const aInnerCount: Byte; 
// 	var aClassRange: TRealRangeList): TReal1DArray;
// var
// 	i, l: Integer;
// 	aPerInc, aUniqueValues: TReal1DArray;
// begin
// 	l := aTrainCount + aInnerCount + 50;
// 	fCS.Enter;
// 	GetTrainingData := GetPerIncList(Count - l, l);
// 	fCS.Leave;

// end;

end.
