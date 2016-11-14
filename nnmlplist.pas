unit nnMLPList;

{$mode objfpc}{$H+}

interface

uses
	Classes, syncobjs, nnBarList, nnLog, nnTypes, Ap;

type
	PMLPList = ^TMLPList;

	TMLPList = class(TObject)
	private
		fLog: PLog;
    fBarList: PBarList;
		fCS: TCriticalSection;
		fMLPParams: TMLPParams;
		fMLPList: array of TMLP;
		fFirstNonTraingIndex: Integer;
    fSizeOfTestSamples: Word;
		procedure AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
	public
		constructor Create;
		destructor Destroy; override;

		procedure SetLogPointer(const aPointer: PLog);
		procedure SetBarListPointer(const aPointer: PBarList);
		procedure SetTrainCountRange(aMin, aMax, aStep: Integer);
		procedure SetInnerCountRange(aMin, aMax: Integer);
		procedure SetHideCountRange(aMin, aMax: Integer);
		procedure SetClassCountRange(aMin, aMax: Integer);
    procedure TrainingMLPList(const aThreadCount: Word);

		function Count: Integer;
		function SaveMLPListToFileT(const aFileName: String): Integer;
		function OpenMLPListFromFileT(const aFileName: String): Integer;
    function SaveMLPListToFile(const aFileName: String): Integer; virtual; abstract;
    function OpenMLPListFromFile(const aFileName: String): Integer; virtual;  abstract;
		function GenerateTrainingList: Integer;
		
    property SizeOfTestSamples: Word 
      read FSizeOfTestSamples write FSizeOfTestSamples;

		// procedure SetTrainingMLP(const aMLP: TMLP);
		// procedure PrintMLPList;
		// procedure PrintBestMLPList;
		// procedure ValidateMLPList;
		// function GenerateTrainingList: Integer;
		// function GetMLPForTraining(var aMLP: TMLP): Integer;
		// function GetPMLP(Index: Integer): PMLP;
	end;

implementation

uses
	SysUtils, nnTrainingThread, nnJson;

{ TMLPList }

procedure TMLPList.AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
begin
	if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg, aMsgType);
end;

constructor TMLPList.Create;
begin
	inherited Create;
	fLog := nil;
	fCS := TCriticalSection.Create;

	fMLPParams.StepCount           := 0;
	fMLPParams.TrainCountRange.min := 0;
	fMLPParams.InnerCountRange.min := 0;
	fMLPParams.HideCountRange.min  := 0;
	fMLPParams.ClassCountRange.min := 0;
	fMLPParams.TrainCountRange.max := 0;
	fMLPParams.InnerCountRange.max := 0;
	fMLPParams.HideCountRange.max  := 0;
	fMLPParams.ClassCountRange.max := 0;

  fSizeOfTestSamples := 50;
end;

destructor TMLPList.Destroy;
begin
	FreeAndNil(fCS);
	inherited Destroy;
end;

procedure TMLPList.SetLogPointer(const aPointer: PLog);
begin
	fLog := aPointer;
end;

procedure TMLPList.SetBarListPointer(const aPointer: PBarList);
begin
	fBarList := aPointer;
end;

procedure TMLPList.SetTrainCountRange(aMin, aMax, aStep: Integer);
begin
	if aMax < aMin then
	begin
		AddToLog('Максимальный размер обучающей выборки ' 
			+ 'меньше минимального', Error);
		Exit;
	end;
	if Frac((aMax - aMin) / aStep) <> 0 then
	begin
		AddToLog('Разница максимального и минимального размера '
			+ 'обучающей выборки не кратно шагу', Error);
		Exit;
	end;
	fMLPParams.StepCount := aStep;
	fMLPParams.TrainCountRange.min := aMin;
	fMLPParams.TrainCountRange.max := aMax;
end;

procedure TMLPList.SetInnerCountRange(aMin, aMax: Integer);
begin
	if aMax < aMin then
	begin
		AddToLog('Максимальный размера окна меньше минимального', Error);
		Exit;
	end;
	fMLPParams.InnerCountRange.min := aMin;
	fMLPParams.InnerCountRange.max := aMax;
end;

procedure TMLPList.SetHideCountRange(aMin, aMax: Integer);
begin
	if aMax < aMin then
	begin
		AddToLog('Максимальное количество скрытых нейронов '
			+ 'меньше минимального', Error);
		Exit;
	end;
	fMLPParams.HideCountRange.min := aMin;
	fMLPParams.HideCountRange.max := aMax;
end;

procedure TMLPList.SetClassCountRange(aMin, aMax: Integer);
begin
	if aMax < aMin then
	begin
		AddToLog('Максимальное количество классов меньше минимального', Error);
		Exit;
	end;
	fMLPParams.ClassCountRange.min := aMin;
	fMLPParams.ClassCountRange.max := aMax;
end;

procedure TMLPList.TrainingMLPList(const aThreadCount: Word);
var
  TTList: array of TTrainingThread;
  i: Integer;
begin
  if (Count < 1) then 
  begin
    AddToLog('Нет сетей для обучения', Error);
    Exit;
  end;
  SetLength(TTList, aThreadCount);
  for i := 0 to Pred(aThreadCount) do 
  begin
    TTList[i] := TTrainingThread.Create(fLog, fBarList, @Self);
    AddToLog('Поток создан', Normal);
  end;
  for i := 0 to Pred(aThreadCount) do 
  begin
    TTList[i].WaitFor;
    FreeAndNil(TTList[i]);
  end;
  AddToLog('Обучение закончено', Info);
end;

function TMLPList.Count: Integer;
begin
	fCS.Enter;
	Count := Length(fMLPList);
	fCS.Leave;
end;

function TMLPList.SaveMLPListToFileT(const aFileName: String): Integer;
var
   F: TextFile;
   i: Integer;
begin
	AssignFile(F, aFileName);
	Rewrite(F);
	try
		fCS.Enter;
		for i := 0 to Pred(Count) do
		begin
			WriteLn(F, TMLP2JSON(fMLPList[i]));
		end;
	finally
		fCS.Leave;
		CloseFile(F);
	end;
	AddToLog(IntToStr(Count) + ' сетей сохранено в файл ' + aFileName, Info);
	SaveMLPListToFileT := Count;
end;

function TMLPList.OpenMLPListFromFileT(const aFileName: String): Integer;
var
   F: TextFile;
   Buff: AnsiString;
   l: Integer; 
begin
	if not FileExists(aFileName) then
	begin
		AddToLog('Файл ' + aFileName + ' не найден', Error);
		Exit;
	end;
	if Count > 0 then
	begin
		SetLength(fMLPList, 0);
	end;
	AssignFile(F, aFileName);
	Reset(F);
	l := 0;
	try
		fCS.Enter;
		while not Eof(F) do
		begin
			Inc(l);
			SetLength(fMLPList, l);
			Readln(F, Buff);
			fMLPList[Pred(l)] := JSON2TMLP(Buff);
		end;
	finally
		fCS.Leave;
		CloseFile(F);
	end;
	OpenMLPListFromFileT := Count;
	AddToLog(IntToStr(Count) + ' сетей открыто из файла ' + aFileName, Info);
end;

function TMLPList.GenerateTrainingList: Integer;
var
	trainCountTo, i, j, k, l, m: Integer;
begin
	with fMLPParams do
	begin
		if  (StepCount = 0) or
			(TrainCountRange.min = 0) or
			(InnerCountRange.min = 0) or
			(HideCountRange.min  = 0) or
			(ClassCountRange.min = 0) or
			(TrainCountRange.max = 0) or
			(InnerCountRange.max = 0) or
			(HideCountRange.max  = 0) or
			(ClassCountRange.max = 0) then
		begin
			AddToLog('Не заданы параметры сетей', Error);
			Exit;
		end;
		trainCountTo:=(TrainCountRange.max 
			- TrainCountRange.min) div StepCount;
		fCS.Enter;
		if Count > 0 then
		begin
			SetLength(fMLPList, 0);
		end;
		l := 0;
		for i := 0 to trainCountTo do
		begin
			for j := InnerCountRange.min to InnerCountRange.max do
			begin
				for k := ClassCountRange.min to ClassCountRange.max do
				begin
					for m := HideCountRange.min to HideCountRange.max do
					begin
						Inc(l);
						SetLength(fMLPList, l);
						with fMLPList[Pred(l)] do
						begin
							id := l;
							trainCount := TrainCountRange.min 
								+ i * StepCount;
							innerCount := j;
							classCount := k;
							hideCount  := m;
						end;
					end;
				end;
			end;
		end;
		fCS.Leave;
	end;
	GenerateTrainingList := Count;
	fFirstNonTraingIndex := 0;
	AddToLog('Сгенерировано '+IntToStr(Count)+' сетей для обучения', Info);
end;

end.
