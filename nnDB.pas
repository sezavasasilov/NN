unit nnDB;

{$mode objfpc}{$H+}

interface

uses
	Classes, IBConnection, nnLog, nnTypes;

type
	PDataBase = ^TDataBase;

	TDataBase = class(TObject)
	private
		fLog: PLog;
		fConnection: TIBConnection;
		procedure AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
	public
		constructor Create;
		destructor Destroy; override;

		procedure SetLogPointer(const aPointer: PLog);
	end;

implementation

uses
	SysUtils;

{ TDataBase }

procedure TDataBase.AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
begin
	if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg, aMsgType);
end;

constructor TDataBase.Create;
begin
	fConnection := TIBConnection.Create(nil);
end;

destructor TDataBase.Destroy;
begin
	FreeAndNil(fConnection);
	inherited Destroy;
end;

procedure TDataBase.SetLogPointer(const aPointer: PLog);
begin
	fLog := aPointer;
end;

end.
