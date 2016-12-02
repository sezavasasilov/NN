unit nnDB;

{$mode objfpc}{$H+}

interface

uses
	Classes, IBConnection, sqldb, nnLog, nnTypes;

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
		function InitTransaction: TSQLTransaction;
		function InitQuery(const aTransaction: TSQLTransaction): TSQLQuery;
	public
		constructor Create;
		destructor Destroy; override;

		procedure SetLogPointer(const aPointer: PLog);
		procedure SetConnectionParams(const CharSet, DatabaseName, 
			HostName, UserName, Password: String);
		function Connect: Boolean;
		function TablesExist: Boolean;
		function CreateCollection(const aSymbol: String;  const aInterval: TInterval;
			const aLastBar: TDateTime; const aParams: TMLPParams): Integer;
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

function TDataBase.InitTransaction: TSQLTransaction;
begin
	InitTransaction := TSQLTransaction.Create(nil);
	InitTransaction.Database := fConnection;
end;

function TDataBase.InitQuery(const aTransaction: TSQLTransaction): TSQLQuery;
begin
	InitQuery := TSQLQuery.Create(nil);
	InitQuery.Database := fConnection;
	InitQuery.Transaction := aTransaction;
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
		on E: EIBDatabaseError Do
			AddToLog(E.Message, Error);
	end;

	Connect := fConnection.Connected;
end;

function TDataBase.TablesExist: Boolean;
var
	Transaction: TSQLTransaction;
	Query: TSQLQuery;
begin
	if not fConnection.Connected then
	begin
		AddToLog('Подключение к базе данных не установлено', Error);
		Exit;
	end;

	Transaction := InitTransaction;
	Query := InitQuery(Transaction);

	try
		Query.SQL.Add('select RDB$RELATION_NAME from RDB$RELATIONS where (RDB$SYSTEM_FLAG = 0) AND (RDB$RELATION_TYPE = 0) order by RDB$RELATION_NAME');
		Transaction.StartTransaction;
		Query.Open;
		AddToLog(IntToStr(Query.RowsAffected));
		Query.Close;
		Transaction.Commit;
	except
		on E: Exception Do
		begin
			AddToLog(E.Message, Error);
			Transaction.Rollback;
		end;
	end;

	FreeAndNil(Query);
	FreeAndNil(Transaction);
end;

function TDataBase.CreateCollection(const aSymbol: String; const aInterval: TInterval;
	const aLastBar: TDateTime; const aParams: TMLPParams): Integer;
var
	Transaction: TSQLTransaction;
	Query: TSQLQuery;
	Interval: Integer;
begin
	CreateCollection := 0;

	if not fConnection.Connected then
	begin
		AddToLog('Подключение к базе данных не установлено', Error);
		Exit;
	end;

	Transaction := InitTransaction;
	Query := InitQuery(Transaction);

	try
		Transaction.StartTransaction;
		with Query do
		begin
			SQL.Add('EXECUTE PROCEDURE PROC_CREATE_COLLECTION(:symbol, :interval, :last_bar, :step_count, :train_count_min, :train_count_max, :inner_count_min, :inner_count_max, :hide_count_min, :hide_count_max, :class_count_min, :class_count_max)');
			Interval := Ord(aInterval) + 1;

			ParamByName('symbol').AsString := aSymbol;
			ParamByName('interval').AsInteger := Interval;
			ParamByName('last_bar').AsDateTime := aLastBar;
			ParamByName('step_count').AsInteger := aParams.StepCount;
			ParamByName('train_count_min').AsInteger := aParams.TrainCountRange.min;
			ParamByName('train_count_max').AsInteger := aParams.TrainCountRange.max;
			ParamByName('inner_count_min').AsInteger := aParams.InnerCountRange.min;
			ParamByName('inner_count_max').AsInteger := aParams.InnerCountRange.max;
			ParamByName('hide_count_min').AsInteger  := aParams.HideCountRange.min;
			ParamByName('hide_count_max').AsInteger  := aParams.HideCountRange.max;
			ParamByName('class_count_min').AsInteger := aParams.ClassCountRange.min;
			ParamByName('class_count_max').AsInteger := aParams.ClassCountRange.max;
			Open;
			CreateCollection := FieldByName('id').AsInteger;
		end;
		Transaction.Commit;
		AddToLog('В базу данных добавлен набор сетей ' + IntToStr(CreateCollection));
	except
		on E: Exception Do
		begin
			AddToLog(E.Message, Error);
			Transaction.Rollback;
		end;
	end;
end;

end.
