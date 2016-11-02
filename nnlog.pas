unit nnLog;

{$mode objfpc}{$H+}

interface

uses
	Classes;

type
	PLog = ^TLog;

	TLog = class(TObject)
	private
		fRecords: array of String;     // Строки
		fDateTime: array of TDateTime; // Время добавления
		fOutputLog: Boolean;           // Выводить на экран
	public
		property OutputLog: Boolean read fOutputLog write fOutputLog;
		procedure PrintLog;
		procedure SaveToFile(const aFileName: String);
		function Add(const aMsg: String): Integer;
		function Count: Integer;
	end;

implementation

uses
	SysUtils;

{TLog}

procedure TLog.PrintLog;
var
	i: Integer;
	msg: String;
begin
	for i := 0 to Pred(Count) do
	begin
		msg := DateTimeToStr(fDateTime[i]) + '   ' + fRecords[i];
		WriteLn(msg);
	end;
end;

procedure TLog.SaveToFile(const aFileName: String);
var
	i: Integer;
	F: TextFile;
	msg: String;
begin
	AssignFile(F, aFileName);
	Rewrite(F);
	try
		for i := 0 to Pred(Count) do
		begin
			msg := FormatDateTime('dd.mm.yyyy hh:nn:ss', fDateTime[i]) 
				+ '   ' + fRecords[i];
			WriteLn(F, msg)
		end;	
	finally
		CloseFile(F);
	end;
end;

function TLog.Add(const aMsg: String): Integer;
var
	c: Integer;
	msg: String;
begin
	c := Count + 1;
	SetLength(fRecords,  c);
	SetLength(fDateTime, c);
	fRecords[c - 1] := aMsg;
	fDateTime[c - 1] := Now;
	if fOutputLog then
	begin
		msg := FormatDateTime('dd.mm.yyyy hh:nn:ss', fDateTime[c - 1]) 
			+ '   ' + aMsg;
		WriteLn(msg);
	end;
end;

function TLog.Count: Integer;
begin
	Count := Length(fRecords);
end;

end.
