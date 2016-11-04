unit nnTypes;

{$mode objfpc}{$H+}

interface

uses
	mlpbase, Ap;

type
	TBar = record
		datetime : TDateTime;
		open     : Currency;
		hight    : Currency;
		low      : Currency;
		close    : Currency;
		value    : Cardinal;
	end;

	TBarArray = array of TBar;

	TRealRange = record
		min : Double;
		max : Double;
	end;

	TRealRangeList = array of TRealRange;

	TIntRange = record
		min : Integer;
		max : Integer;
	end;

	TIntRangeList = array of TIntRange;

	PMLP = ^TMLP;

	TMLP = record
		id         : Word;
		MLP        : MultiLayerPerceptron;
		trainCount : Word;
		Data       : TReal1DArray;
		innerCount : Byte;
		hideCount  : Byte;
		classCount : Byte;
		rangeList  : TRealRangeList;
		R1         : Double;
		R2         : Double;
		Info       : AlglibInteger;
		trainTime  : TDateTime;
	end;

	TMLPParams = record
		StepCount       : Integer;
		TrainCountRange : TIntRange;
		InnerCountRange : TIntRange;
		HideCountRange  : TIntRange;
		ClassCountRange : TIntRange;
	end;

implementation



end.
