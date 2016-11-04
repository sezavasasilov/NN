unit nnMLPList;

{$mode objfpc}{$H+}

interface

uses
	Classes, syncobjs, nnBarList, nnLog, nnTypes, Ap;

type
	PMLPList = ^TMLPList;

	TMLPList = class(TObject)
	private
		// fTrainingList: array of TMLP;
		// fCount: Integer;
		// fFirstNonTraingIndex: Integer;
		fLog: PLog;
		fCS: TCriticalSection;
		fMLPParams: TMLPParams;
		procedure AddToLog(const aMsg: String);
	public
		constructor Create;
		destructor Destroy; override;
		procedure SetLogPointer(const aPointer: PLog);
		// property Log: PLog write fLog;
		// property Count: Integer read fCount;
		// procedure SetTrainCountStep(aValue: Integer);
		// procedure SetTrainCountRange(aMin, aMax: Integer);
		// procedure SetInnerCountRange(aMin, aMax: Integer);
		// procedure SetHideCountRange(aMin, aMax: Integer);
		// procedure SetClassCountRange(aMin, aMax: Integer);
		// procedure SetTrainingMLP(const aMLP: TMLP);
		// procedure PrintMLPList;
		// procedure PrintBestMLPList;
		// procedure ValidateMLPList;
		// procedure SaveMLPListToFile(const aFileName: String);
		// function OpenMLPListFromFile(const aFileName: String): Integer;
		// function GenerateTrainingList: Integer;
		// function GetMLPForTraining(var aMLP: TMLP): Integer;
		// function GetPMLP(Index: Integer): PMLP;
	end;

implementation

uses
	SysUtils;

{ TMLPList }

procedure TMLPList.AddToLog(const aMsg: String);
begin
	if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg);
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

end.
