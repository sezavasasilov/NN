unit nnLog;

{$mode objfpc}{$H+}

interface

uses
	Classes;

type
	TMsgType = (Normal, Info, Warning, Error, Empty);

	PLog = ^TLog;

	TLog = class(TObject)
	private
		fRecords: array of String;     // Строки
		fDateTime: array of TDateTime; // Время добавления
		fMsgType: array of TMsgType;   // Тип сообщения
		fOutputLog: Boolean;           // Выводить на экран
		fColoredLog: Boolean;          // Выводить цветные сообщения
		fOutDateTime: Boolean;         // Выводить дату и время
		fEmpty: Boolean;                // Изменение текущей строки
	public
		property OutputLog: Boolean read fOutputLog write fOutputLog;
		property ColoredLog: Boolean read fColoredLog write fColoredLog;
		property OutDateTime: Boolean read fOutDateTime write fOutDateTime;
		procedure PrintLog;
		procedure SaveToFile(const aFileName: String);
		procedure Add(const aMsg: String; aMsgType: TMsgType = Normal);
		function Count: Integer;
	end;

implementation

uses
	SysUtils, CRT;

{TLog}

procedure TLog.PrintLog;
var
	i: Integer;
	msg: String;
begin
	for i := 0 to Pred(Count) do
	begin
		if (fOutDateTime) then 
		begin
			WriteLn(FormatDateTime('dd.mm.yyyy hh:nn:ss', fDateTime[i]) + '   ');
		end;
		if fColoredLog then
		begin
			case fMsgType[i] of
				   Info : TextColor(Blue);
				Warning : TextColor(Yellow);
				  Error : TextColor(Red);
			end;
			WriteLn(fRecords[i]);
			TextColor(LightGray);
		end else
			WriteLn(fRecords[i]);
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
			msg := FormatDateTime('dd.mm.yyyy hh:nn:ss', fDateTime[i]);
			case fMsgType[i] of
				   Info : msg := msg + ' [Info]';
				Warning : msg := msg + ' [Warning]';
				  Error : msg := msg + ' [Error]';
			end;
			msg := msg + '   ' + fRecords[i];
			WriteLn(F, msg)
		end;	
	finally
		CloseFile(F);
	end;
end;

procedure TLog.Add(const aMsg: String; aMsgType: TMsgType = Normal);
var
	c: Integer;
	msg: String;
begin
	if not (aMsgType = Empty) then
	begin
		c := Count + 1;
		SetLength(fRecords,  c);
		SetLength(fDateTime, c);
		SetLength(fMsgType,  c);
		fRecords[c - 1] := aMsg;
		fDateTime[c - 1] := Now;
		fMsgType[c - 1] := aMsgType;
	end;
	if fOutputLog then
	begin
		if (fOutDateTime) then 
		begin
			WriteLn(FormatDateTime('dd.mm.yyyy hh:nn:ss', fDateTime[c - 1]) + '   ');
		end;
		if fColoredLog then
		begin
			case aMsgType of
				   Info : TextColor(Blue);
				Warning : TextColor(Yellow);
				  Error : TextColor(Red);
			end;
		end;
		
		if (aMsgType = Empty) then
		begin
			if not fEmpty then
				fEmpty := true;
			GotoXy(0,0);
			Write(aMsg);
		end else
		begin
			if fEmpty then
			begin
				fEmpty := false;
				WriteLn;
			end;
			WriteLn(aMsg);
		end;

		if fColoredLog then
			TextColor(LightGray);
	end;
end;

function TLog.Count: Integer;
begin
	Count := Length(fRecords);
end;

end.
