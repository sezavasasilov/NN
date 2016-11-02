unit nnBarList;

{$mode objfpc}{$H+}

interface

uses
	Classes, syncobjs, nnLog, nnTypes;

type
	PBarList = ^TBarList;

	TBarList = class(TObject)
	private
		fData: TBarArray;
		fLog: PLog;
		fCS: TCriticalSection;
		procedure AddToLog(const aMsg: String);
	public
		constructor Create;
		destructor Destroy; override;
		procedure SetLogPointer(const aPointer: PLog);
		procedure LoadFromFile(const aFileName: String);
		procedure Add(aBar: TBar);
		function Count: Integer;
		// function GetPerInc: TReal1DArray;
		// function GetBarValue(const Index: Integer; const Column: Byte): Double;
		// function GetBar(const Index: Integer): TBar;
		// procedure GetTrainingData(const aTrainCount: Word; const aClassCount: Byte;
		// const aInnerCount: Byte; var aTrainingData: TReal1DArray;
		// var aClassRange: TRangeList);
		// procedure Add(aBar: TBar);
		// procedure LoadFromFile(_fileName: String);
		// function Count: Integer;
		// property Log: PLog write fLog;
	end;

implementation

uses
	SysUtils;

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

{ TBarList }

procedure TBarList.AddToLog(const aMsg: String);
begin
	if (fLog <> nil) then fLog^.Add(aMsg);
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
  l := Count;
  Inc(l);
  SetLength(fData, l);
  fData[Pred(l)] := aBar;
end;

function TBarList.Count: Integer;
begin
  Count := Length(fData);
end;

end.
