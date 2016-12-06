unit nnDB;

{$mode objfpc}{$H+}

interface

uses
	Classes, IBConnection, sqldb, nnMLPList, nnLog, nnTypes;

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
		function Disconnect: Boolean;
		function SaveMLPList(const aPointer: PMLPList; const aSymbol: String;
			const aInterval: TInterval; const aLastBar: TDateTime; 
			const aParams: TMLPParams): Integer;
		function OpenMLPList(const aPointer: PMLPList;
			const aCollectionID: Integer): Integer;

		// function TablesExist: Boolean;
	end;

implementation

uses
	SysUtils, db, nn2Stream;

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

function TDataBase.Disconnect: Boolean;
begin
	if not fConnection.Connected then
	begin
		Disconnect := true;
		Exit;
	end;

	try
		fConnection.Close;
	except
		on E: EIBDatabaseError Do
			AddToLog(E.Message, Error);
	end;

	Disconnect := not fConnection.Connected;
end;

function TDataBase.SaveMLPList(const aPointer: PMLPList; const aSymbol: String;
	const aInterval: TInterval; const aLastBar: TDateTime; 
	const aParams: TMLPParams): Integer;
var
	Transaction: TSQLTransaction;
	Query: TSQLQuery;
	Interval, aCollectionId, i, j: Integer;
	aStream: TMemoryStream;
begin
	SaveMLPList := -1;

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
			SQL.Clear;
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
			aCollectionId := FieldByName('id').AsInteger;

			for i := 0 to Pred(aPointer^.Count) do
			begin
				SQL.Clear;
				SQL.Add('insert into T_MLP(ID, COLLECTION, WITH_VOLUME, MLP, TRAIN_COUNT, INNER_COUNT, HIDE_COUNT, CLASS_COUNT, TRAIN_TIME) values (:id, :collection, :with_volume, :mlp, :train_count, :inner_count, :hide_count, :class_count, :train_time)');

				ParamByName('id').AsInteger := aPointer^[i].id;
				ParamByName('collection').AsInteger := aCollectionId;
				if aPointer^[i].withVolume then
					ParamByName('with_volume').AsInteger := 1
				else
					ParamByName('with_volume').AsInteger := 0;
				aStream := TMemoryStream.Create;
				WriteTMLP(aStream, aPointer^[i]);
				ParamByName('mlp').LoadFromStream(aStream, ftBlob);
				ParamByName('train_count').AsInteger := aPointer^[i].trainCount;
				ParamByName('inner_count').AsInteger := aPointer^[i].innerCount;
				ParamByName('hide_count').AsInteger := aPointer^[i].hideCount;
				ParamByName('class_count').AsInteger := aPointer^[i].classCount;
				ParamByName('train_time').AsDateTime := aPointer^[i].trainTime;

				ExecSQL;

				for j := 0 to Pred(Length(aPointer^[i].rangeList)) do
				begin
					SQL.Clear;
					SQL.Add('insert into T_RANGE_LIST(T_MLP_ID, T_MLP_COLLECTION, T_ID, T_MIN, T_MAX) values (:mlp_id, :collection, :id, :t_min, :t_max)');
					
					ParamByName('mlp_id').AsInteger := aPointer^[i].id;
					ParamByName('collection').AsInteger := aCollectionId;
					ParamByName('id').AsInteger := j;
					ParamByName('t_min').AsFloat := aPointer^[i].rangeList[j].min;
					ParamByName('t_max').AsFloat := aPointer^[i].rangeList[j].max;

					ExecSQL;
				end;
				FreeAndNil(aStream);
			end;
		end;
		Transaction.Commit;
		AddToLog(IntToStr(aPointer^.Count) + ' сетей сохранено в БД под номером ' 
			+ IntToStr(aCollectionId));
		SaveMLPList := aCollectionId;
	except
		on E: Exception Do
		begin
			AddToLog(E.Message, Error);
			Transaction.Rollback;
		end;
	end;
end;

function TDataBase.OpenMLPList(const aPointer: PMLPList;
	const aCollectionID: Integer): Integer;
var
	Transaction: TSQLTransaction;
	Query: TSQLQuery;
	aStream: TStream;
	aInterval: TInterval;
	aMLP: TMLP;
begin
	OpenMLPList := -1;

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
			SQL.Clear;
			SQL.Add('select * from T_COLLECTIONS where ID = :id');
			ParamByName('id').AsInteger := aCollectionID;
			Open;

			aPointer^.SetSymbol(FieldByName('symbol').AsString);
			case FieldByName('interval').AsInteger of
				 0: aPointer^.SetInterval(M1);
				 1: aPointer^.SetInterval(M5);
				 2: aPointer^.SetInterval(M10);
				 3: aPointer^.SetInterval(M15);
				 4: aPointer^.SetInterval(M30);
				 5: aPointer^.SetInterval(H1);
				 6: aPointer^.SetInterval(H2);
				 7: aPointer^.SetInterval(H4);
				 8: aPointer^.SetInterval(Day);
				 9: aPointer^.SetInterval(Week);
				10: aPointer^.SetInterval(Month);
				11: aPointer^.SetInterval(Quarter);
				12: aPointer^.SetInterval(Year);
			end;
			aPointer^.SetLastBar(FieldByName('last_bar').AsDateTime);
			aPointer^.SetSymbol(FieldByName('symbol').AsString);
			aPointer^.SetTrainCountRange(FieldByName('train_count_min').AsInteger, 
				FieldByName('train_count_max').AsInteger, 
				FieldByName('step_count').AsInteger);
			aPointer^.SetInnerCountRange(FieldByName('inner_count_min').AsInteger, 
				FieldByName('inner_count_max').AsInteger);
			aPointer^.SetHideCountRange(FieldByName('hide_count_min').AsInteger,
				FieldByName('hide_count_max').AsInteger);
			aPointer^.SetClassCountRange(FieldByName('class_count_min').AsInteger, 
				FieldByName('class_count_max').AsInteger);
			Close;

			SQL.Clear;
			SQL.Add('select * from T_MLP where COLLECTION = :collectionId order by ID');
			ParamByName('collectionId').AsInteger := aCollectionID;
			Open;
			aPointer^.Clear;
			while not Eof do
			begin
				aStream := CreateBlobStream(FieldByName('mlp'), bmRead);
				aStream.Seek(0, soFromBeginning);
				ReadTMLP(aStream, aMLP);
				aPointer^.AddMLP(aMLP);
				FreeAndNil(aStream);
				Next;
			end;
		end;
		Transaction.Commit;
		AddToLog(IntToStr(aPointer^.Count) + ' сетей загружено из БД');
		OpenMLPList := aPointer^.Count;
	except
		on E: Exception Do
		begin
			AddToLog(E.Message, Error);
			Transaction.Rollback;
		end;
	end;
end;

// function TDataBase.TablesExist: Boolean;
// var
// 	Transaction: TSQLTransaction;
// 	Query: TSQLQuery;
// begin
// 	if not fConnection.Connected then
// 	begin
// 		AddToLog('Подключение к базе данных не установлено', Error);
// 		Exit;
// 	end;

// 	Transaction := InitTransaction;
// 	Query := InitQuery(Transaction);

// 	try
// 		Query.SQL.Add('select RDB$RELATION_NAME from RDB$RELATIONS where (RDB$SYSTEM_FLAG = 0) AND (RDB$RELATION_TYPE = 0) order by RDB$RELATION_NAME');
// 		Transaction.StartTransaction;
// 		Query.Open;
// 		AddToLog(IntToStr(Query.RowsAffected));
// 		Query.Close;
// 		Transaction.Commit;
// 	except
// 		on E: Exception Do
// 		begin
// 			AddToLog(E.Message, Error);
// 			Transaction.Rollback;
// 		end;
// 	end;

// 	FreeAndNil(Query);
// 	FreeAndNil(Transaction);
// end;

end.
