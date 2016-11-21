unit nnMLPList;

{$mode objfpc}{$H+}

interface

uses
	Classes, syncobjs, nnBarList, nnLog, nnTypes, nn2Stream, Ap;

type
	PMLPList = ^TMLPList;

	TMLPList = class(TObject)
	private
		fLog: PLog;
		fBarList: PBarList;
		fCS: TCriticalSection;
		fMLPParams: TMLPParams;
		fMLPList: array of TMLP;
		fPMLPList: array of PMLP;
		fFirstNonTraingIndex: Integer;
		fTrainingProcess: Integer;
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
		procedure TrainingMLPList(const aThreadCount: Word;
			 const aVolumes: Boolean = false);
		procedure SetTrainingMLP(const aMLP: TMLP);

		function Count: Integer;
		function SaveMLPListToFileT(const aFileName: String): Integer;
		function OpenMLPListFromFileT(const aFileName: String): Integer;
		function SaveMLPListToFile(const aFileName: String): Integer;
		function OpenMLPListFromFile(const aFileName: String): Integer;
		function GenerateTrainingList: Integer;
		function GetMLPForTraining(var aMLP: TMLP): Integer;
		function BestCount: Integer;
		function SelectBestMLP(const aEffect: Byte; 
			const aPrinting: Boolean = false): Integer;
		
		// procedure PrintMLPList;
		// procedure PrintBestMLPList;
		// procedure ValidateMLPList;
	end;

implementation

uses
	SysUtils, nnTrainingThread, nnJson, mlpbase, Math;

procedure AddUnicInt(var aArray: TInteger1DArray; const aValue: Integer);
var
	i, l: Integer;
begin
	l := Length(aArray);
	if (l = 0) then
	begin
		SetLength(aArray, 1);
		aArray[0] := aValue;
	end else
	begin
		for i := 0 to High(aArray) do
		begin
			if aArray[i] = aValue then
				Exit;
		end;
		SetLength(aArray, l + 1);
		aArray[l] := aValue;
	end;
end;

function GetTestSample(const aData: TReal1DArray; 
	const aRangeList: TRealRangeList; const aInnerCount: Byte;
	out XY: TReal2DArray; out Y: TReal1DArray): Integer;
var
	i, j, l: Integer;
	aRangeData: TReal1DArray;
begin
	SetLength(XY, SizeOfTestSamples);
	SetLength(Y, SizeOfTestSamples);
	aRangeData := DefRanges(aData, aRangeList);
	l := Length(aData);
	for i := 0 to Pred(SizeOfTestSamples) do
	begin
		Y[i] := aRangeData[l - SizeOfTestSamples + i];
		SetLength(XY[i], aInnerCount);
		for j := 0 to Pred(aInnerCount) do
			XY[i, j] := aRangeData[l - SizeOfTestSamples + i - aInnerCount + j];
	end;
	GetTestSample := Length(Y);
end;

function GetTestSampleV(const aData, aVolData: TReal1DArray;
	const aRangeList: TRealRangeList; const aInnerCount: Byte;
	out XY: TReal2DArray; out Y: TReal1DArray): Integer;
var
	i, j, l: Integer;
	aRangeData: TReal1DArray;
begin
	SetLength(XY, SizeOfTestSamples);
	SetLength(Y, SizeOfTestSamples);
	aRangeData := DefRanges(aData, aRangeList);
	l := Length(aData);
	for i := 0 to Pred(SizeOfTestSamples) do
	begin
		Y[i] := aRangeData[l - SizeOfTestSamples + i];
		SetLength(XY[i], aInnerCount * 2);
		for j := 0 to Pred(aInnerCount) do
		begin
			XY[i, j * 2] := aRangeData[l - SizeOfTestSamples + i - aInnerCount + j];
			XY[i, j * 2 + 1] := aVolData[l - SizeOfTestSamples + i - aInnerCount + j];
		end;
	end;
	GetTestSampleV := Length(Y);
end;

function Class2Trend(const aClass: Double; const aClassCount: Byte): Shortint;
var
  Zero: Double;
begin
  Zero := aClassCount / 2;
  if (aClass >= Zero) then 
  begin
    Class2Trend := 1;
  end else
  begin
    Class2Trend := -1;
  end;
end; 

function TestMLP(var aMLP: MultiLayerPerceptron; const aClassCount: Byte; 
	const XY: TReal2DArray; const Y: TReal1DArray): Double;
var
	OutY, Z: TReal1DArray;
	i: Integer;
begin
	SetLength(OutY, 1);
	SetLength(Z, Length(XY));
	for i := 0 to High(XY) do
	begin
		MLPProcess(aMLP, XY[i], OutY);
		if Class2Trend(OutY[0], aClassCount) = Class2Trend(Y[i], aClassCount) then
			Z[i] := 1
		else
			Z[i] := 0;
	end;
	TestMLP := Sum(Z) / Length(Z);
end;

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

procedure TMLPList.TrainingMLPList(const aThreadCount: Word;
	const aVolumes: Boolean = false);
var
	TTList: array of TTrainingThread;
	i, t, b: Integer;
begin
	if (Count < 1) then 
	begin
		AddToLog('Нет сетей для обучения', Error);
		Exit;
	end;
	t := fMLPParams.TrainCountRange.max + fMLPParams.InnerCountRange.max;
	b := fBarList^.Count;
	if t > b then
		AddToLog('Недостаточно данных для обучения', Warning);
	AddToLog('Начало обучения...', Info);
	fTrainingProcess := 0;
	SetLength(TTList, aThreadCount);
	for i := 0 to Pred(aThreadCount) do 
	begin
		TTList[i] := TTrainingThread.Create(fLog, fBarList, @Self, aVolumes);
	end;
	for i := 0 to Pred(aThreadCount) do 
	begin
		TTList[i].WaitFor;
		FreeAndNil(TTList[i]);
	end;
	AddToLog('Обучение закончено', Info);
end;

procedure TMLPList.SetTrainingMLP(const aMLP: TMLP);
var
	i: Integer;
begin
	fCS.Enter;
	i := Pred(aMLP.id);
	if fMLPList[i].id = aMLP.id then
		fMLPList[i] := aMLP
	else begin
		for i := 0 to Pred(Count) do
		begin
			if fMLPList[i].id = aMLP.id then
				fMLPList[i] := aMLP;
		end;
	end;
	Inc(fTrainingProcess);
	AddToLog('    Обучение: ' + IntToStr(Round(fTrainingProcess / Count * 100))
		+ '%', Empty);
	fCS.Leave;
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

function TMLPList.SaveMLPListToFile(const aFileName: String): Integer;
var
	i: Integer;
	F: TFileStream;
begin
	F := TFileStream.Create(aFileName, fmCreate or fmShareExclusive);
	try
		fCS.Enter;
		for i := 0 to Pred(Count) do
		begin
			WriteTMLP(F, fMLPList[i]);
		end;
	finally
		fCS.Leave;
		FreeAndNil(F);
		SaveMLPListToFile := Count;
		AddToLog(IntToStr(Count) + ' сетей сохранено в файл ' + aFileName, Info);
	end;
end;

function TMLPList.OpenMLPListFromFile(const aFileName: String): Integer;
var
	l: Integer;
	F: TFileStream;
	aTrainCount : TInteger1DArray;
	aInnerCount : TInteger1DArray;
	aHideCount  : TInteger1DArray;
	aClassCount : TInteger1DArray;
begin
	F := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
	try
		fCS.Enter;
		l := 0;
		SetLength(fMLPList, l);
		while not EoS(F) do
		begin
			Inc(l);
			SetLength(fMLPList, l);
			ReadTMLP(F, fMLPList[Pred(l)]);
			AddUnicInt(aTrainCount, fMLPList[Pred(l)].trainCount);
			AddUnicInt(aInnerCount, fMLPList[Pred(l)].innerCount);
			AddUnicInt(aHideCount,  fMLPList[Pred(l)].hideCount);
			AddUnicInt(aClassCount, fMLPList[Pred(l)].classCount);
		end;
	finally
		fCS.Leave;
		FreeAndNil(F);
		OpenMLPListFromFile := l;
		AddToLog(IntToStr(l) + ' сетей открыто из файла ' + aFileName, Info);
	end;
	fMLPParams.StepCount := aTrainCount[High(aTrainCount)] 
		- aTrainCount[Pred(High(aTrainCount))];
	fMLPParams.TrainCountRange.min := aTrainCount[Low(aTrainCount)];
	fMLPParams.TrainCountRange.max := aTrainCount[High(aTrainCount)];
	fMLPParams.InnerCountRange.min := aInnerCount[Low(aInnerCount)];
	fMLPParams.InnerCountRange.max := aInnerCount[High(aInnerCount)];
	fMLPParams.HideCountRange.min  := aHideCount[Low(aHideCount)];
	fMLPParams.HideCountRange.max  := aHideCount[High(aHideCount)];
	fMLPParams.ClassCountRange.min := aClassCount[Low(aClassCount)];
	fMLPParams.ClassCountRange.max := aClassCount[High(aClassCount)];
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

function TMLPList.GetMLPForTraining(var aMLP: TMLP): Integer;
begin
	fCS.Enter;
	if (Count > 0) and (fFirstNonTraingIndex < Count) then
	begin
		aMLP := fMLPList[fFirstNonTraingIndex];
		GetMLPForTraining := fFirstNonTraingIndex;
		Inc(fFirstNonTraingIndex);
	end else
	begin
		GetMLPForTraining := -1;
	end;
	fCS.Leave;
end;

function TMLPList.BestCount: Integer;
begin
	fCS.Enter;
	BestCount := Length(fPMLPList);
	fCS.Leave;
end;

function TMLPList.SelectBestMLP(const aEffect: Byte;
	const aPrinting: Boolean = false): Integer;
var
	i, l: Integer;
	aResult: Double;
	aData, aVolData, Y: TReal1DArray;
	XY: TReal2DArray;
	aWithVolume: Boolean;
begin
	fCS.Enter;
	if BestCount > 0 then
		SetLength(fPMLPList, 0);
	l := SizeOfTestSamples + fMLPParams.InnerCountRange.max;
	aData := fBarList^.GetPerIncList(fBarList^.Count - l, l);
	aWithVolume := fMLPList[0].withVolume;
	if aWithVolume then
	begin
		aVolData := fBarList^.GetRangeVolData(l);
	end;
	for i := 0 to Pred(Count) do
	begin
		if aWithVolume then
		begin
			GetTestSampleV(aData, aVolData, fMLPList[i].rangeList, 
				fMLPList[i].innerCount, XY, Y);
		end else
			GetTestSample(aData, fMLPList[i].rangeList, 
				fMLPList[i].innerCount, XY, Y);
		aResult := TestMLP(fMLPList[i].MLP, fMLPList[i].classCount, XY, Y);
		if (aResult >= aEffect / 100) then
		begin
			SetLength(fPMLPList, BestCount + 1);
		end;
	end;
	fCS.Leave;
	SelectBestMLP := BestCount;
	AddToLog('Выбрано ' + IntToStr(SelectBestMLP)
		+ ' лучших сетей (эффективность > ' + IntToStr(aEffect) + '%)', Info);
end;

end.
