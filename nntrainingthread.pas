unit nnTrainingThread;

{$mode objfpc}{$H+}

interface

uses
	SysUtils, Classes, nnLog, nnBarList, nnMLPList, nnTypes, 
	mlpbase, mlptrain, Ap;

type
	TTrainingThread = class(TThread)
	private
		fLog : PLog;
		fBarList : PBarList;
		fMLPList : PMLPList;
		procedure AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
		function GetTrainingSample(var aMLP: TMLP): TReal2DArray;
	protected
		procedure Execute; override;
	public
		constructor Create(aLog: PLog; aBarList: PBarList; aMLPList: PMLPList);
	end;

implementation

function InRange(const aValue, aMin, aMax: Double): Boolean;
begin
	if (aValue >= aMin) and (aValue <= aMax) then
		InRange := true
	else
		InRange := false;
end;

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

{ TTrainingThread }

procedure TTrainingThread.AddToLog(const aMsg: String; 
	aMsgType: TMsgType = Normal);
begin
	if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg, aMsgType);
end;

function TTrainingThread.GetTrainingSample(
	var aMLP: TMLP): TReal2DArray;
var
	Data, TrainData: TReal1DArray;
	i, j: Integer;
begin
	Data := fBarList^.GetTrainingData(aMLP.trainCount, aMLP.classCount,
		aMLP.innerCount, aMLP.rangeList);
	TrainData := DefRanges(Data, aMLP.rangeList);
	SetLength(GetTrainingSample, aMLP.trainCount);
	for i := 0 to Pred(aMLP.trainCount) do
	begin
		SetLength(GetTrainingSample[i], aMLP.innerCount + 1);
		for j := 0 to aMLP.innerCount do
		begin
			GetTrainingSample[i, j] := TrainData[i + j];
		end;
	end;
end;

procedure TTrainingThread.Execute;
var
	MLP: TMLP;
	XY: TReal2DArray;
	Info: Integer;
	Rep: MLPReport;
begin
	while not (fMLPList^.GetMLPForTraining(MLP) < 0) do
	begin
		XY := GetTrainingSample(MLP);
		MLPCreate1(MLP.innerCount, MLP.hideCount, 1, MLP.MLP);
		Info := 0;
		MLPTrainLBFGS(MLP.MLP, XY, MLP.trainCount, 0.001, 2, 0.01, 0, Info, Rep);
		if (Info = 2) then
		begin
			MLP.trainTime := Now;
			fMLPList^.SetTrainingMLP(MLP);
		end else
			AddToLog('Сеть ' + IntToStr(MLP.id) + ' не обучилась!', Warning);
	end;
end;

constructor TTrainingThread.Create(aLog: PLog; 
	aBarList: PBarList; aMLPList: PMLPList);
begin
	inherited Create(true);
	Priority := tpLower;

	fLog := aLog;
	fBarList := aBarList;
	fMLPList := aMLPList;

	Start;
end;

end.
