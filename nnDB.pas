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
		procedure BeforeConnect(Sender: TObject);
		procedure AfterConnect(Sender: TObject);
		procedure BeforeDisconnect(Sender: TObject);
		procedure AfterDisconnect(Sender: TObject);
	public
		constructor Create;
		destructor Destroy; override;

		procedure SetLogPointer(const aPointer: PLog);
		procedure SetConnectionParams(const CharSet, DatabaseName, 
			HostName, UserName, Password: String);
		function Connect: Boolean;
	end;

implementation

uses
	SysUtils;

{ TDataBase }

procedure TDataBase.AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
begin
	if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg, aMsgType);
end;

procedure TDataBase.BeforeConnect(Sender: TObject);
begin
	AddToLog('Подключение к ' + fConnection.DatabaseName + '...', Empty);
end;

procedure TDataBase.AfterConnect(Sender: TObject);
begin
	AddToLog('[OK]', Empty);
	AddToLog('Подключение к ' + fConnection.DatabaseName + ' установлено', Info);
end;

procedure TDataBase.BeforeDisconnect(Sender: TObject);
begin
	AddToLog('Отключение от ' + fConnection.DatabaseName + '...', Empty);
end;

procedure TDataBase.AfterDisconnect(Sender: TObject);
begin
	AddToLog('[OK]', Empty);
	AddToLog('Подключение к ' + fConnection.DatabaseName + ' разорвано', Info);
end;

constructor TDataBase.Create;
begin
	fConnection := TIBConnection.Create(nil);
	fConnection.BeforeConnect := @BeforeConnect;
	fConnection.AfterConnect := @AfterConnect;
	fConnection.BeforeDisconnect := @BeforeDisconnect;
	fConnection.AfterDisconnect := @AfterDisconnect;
end;

destructor TDataBase.Destroy;
begin
	if fConnection.Connected then
		fConnection.Close;
	FreeAndNil(fConnection);
	inherited Destroy;
end;

procedure TDataBase.SetLogPointer(const aPointer: PLog);
begin
	fLog := aPointer;
end;

procedure TDataBase.SetConnectionParams(const CharSet, DatabaseName, 
	HostName, UserName, Password: String);
var
	msg: String;
begin
	fConnection.CharSet := CharSet;
	fConnection.DatabaseName := DatabaseName;
	fConnection.HostName := HostName;
	fConnection.UserName := UserName;
	fConnection.Password := Password;
	AddToLog('Установлены парметры подключения к БД:');
	msg := '      CharSet: ' + CharSet + #13#10;
	msg := msg + '      DatabaseName: ' + DatabaseName + #13#10;
	msg := msg + '      HostName: ' + HostName + #13#10;
	msg := msg + '      UserName: ' + UserName + #13#10;
	msg := msg + '      Password: ' + Password + #13#10;
	AddToLog(msg, Empty);
end;

function TDataBase.Connect: Boolean;
begin
	if fConnection.Connected then
	begin
		Connect := true;
		Exit;
	end;

	try
		fConnection.Open;
	except
		on E: {Exception}EIBDatabaseError Do
			// case E.GDSErrorCode of
			// 	335544472 : begin
			// 					AddToLog('Ошибка подключения:', Error); 
			// 					AddToLog('   Не верный логин или пароль', Error);
			// 				end;
			// 	335544721 : begin
			// 					AddToLog('Ошибка подключения:', Error);
			// 					AddToLog('   Не удалось подключиться к ' 
			// 						+ fConnection.HostName, Error);
			// 				end;
			// 	335544344 : begin
			// 					AddToLog('Ошибка подключения:', Error);
			// 					AddToLog('   База ' + fConnection.DatabaseName
			// 						+ ' не найдена', Error);
			// 				end;
			// 	else
			// 		AddToLog(IntToStr(E.GDSErrorCode), Error);
			// end;
				AddToLog(E.Message, Error);
	end;
	
	Connect := fConnection.Connected;
end;

end.
