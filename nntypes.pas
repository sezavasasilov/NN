unit nnTypes;

{$mode objfpc}{$H+}

interface

type
	TBar = record
		datetime : TDateTime;
		open     : Currency;
		hight    : Currency;
		low      : Currency;
		close    : Currency;
		value    : Cardinal;
	end;

	TBarListArray = array of TBar;

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

implementation



end.
