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
		fVolumes : Boolean;
		procedure AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
		function GetTrainingSample(var aMLP: TMLP): TReal2DArray;
	protected
		procedure Execute; override;
	public
		constructor Create(const aLog: PLog; const aBarList: PBarList; 
			const aMLPList: PMLPList; const aVolumes: Boolean = false);
	end;

implementation

{ TTrainingThread }

procedure TTrainingThread.AddToLog(const aMsg: String; 
	aMsgType: TMsgType = Normal);
begin
	if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg, aMsgType);
end;

function TTrainingThread.GetTrainingSample(
	var aMLP: TMLP): TReal2DArray;
var
	Data, aVolData, TrainData: TReal1DArray;
	i, j: Integer;
begin
	if fVolumes then
	begin
		Data := fBarList^.GetTrainingDataV(aMLP.trainCount, aMLP.classCount,
			aMLP.innerCount, aMLP.rangeList, aVolData);
		TrainData := DefRanges(Data, aMLP.rangeList);
		SetLength(GetTrainingSample, aMLP.trainCount);
		for i := 0 to Pred(aMLP.trainCount) do
		begin
			SetLength(GetTrainingSample[i], aMLP.innerCount * 2 + 1);
			for j := 0 to aMLP.innerCount do
			begin
				GetTrainingSample[i, j * 2] := TrainData[i + j];
				if j < aMLP.innerCount then
				begin
					GetTrainingSample[i, j * 2 + 1] := aVolData[i + j];
				end;
			end;
		end;
	end else
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
		MLP.withVolume := fVolumes;
		XY := GetTrainingSample(MLP);
		if fVolumes then
			MLPCreate1(MLP.innerCount * 2, MLP.hideCount, 1, MLP.MLP)
		else
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

constructor TTrainingThread.Create(const aLog: PLog; const aBarList: PBarList; 
	const aMLPList: PMLPList; const aVolumes: Boolean = false);
begin
	inherited Create(true);
	Priority := tpLower;

	fLog := aLog;
	fBarList := aBarList;
	fMLPList := aMLPList;
	fVolumes := aVolumes;

	Start;
end;

end.
