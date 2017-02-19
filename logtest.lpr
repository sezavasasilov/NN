program logtest;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
	{$IFDEF UNIX}{$IFDEF UseCThreads}
	cthreads,
	{$ENDIF}{$ENDIF}
	SysUtils, Classes, nn2Stream, nnMLPList, nnBarList, nnDB, nnLog, nnTypes, Ap;

var
	Log: TLog;
	BarList: TBarList;
	MLPList: TMLPList;
	DataBase: TDataBase;

begin
	writeln('=========================================================');
	writeln();
	
	Log := TLog.Create;
	Log.OutputLog := true;
	Log.ColoredLog := true;

	BarList := TBarList.Create;
	BarList.SetLogPointer(@Log);
	BarList.LoadFromFile('./SBPR.csv');

	DataBase := TDataBase.Create;
	DataBase.SetLogPointer(@Log);
	DataBase.SetConnectionParams('UTF8', 'nnDB', 'localhost', 'SYSDBA', 'masterkey');
	// DataBase.Connect;
	// DataBase.CreateCollection('SBER', M1, BarList.Bar[0].datetime, MLPList.GetParams);

	MLPList := TMLPList.Create;
	MLPList.SetLogPointer(@Log);
	MLPList.SetBarListPointer(@BarList);
	// MLPList.SetTrainCountRange(200, 500, 50);
	// MLPList.SetInnerCountRange(3, 7);
	// MLPList.SetHideCountRange(18, 19);
	// MLPList.SetClassCountRange(4, 6);

	// MLPList.GenerateTrainingList;
	// MLPList.TrainingMLPList(8, true);
	// MLPList.SelectBestMLP(70, true);
	// MLPList.SaveMLPListToFile('./SBPR5m(test).nnl');
	// MLPList.SaveMLPListToDB(@DataBase);
	MLPList.OpenMLPListFromDB(@DataBase, 52);
	// MLPList.OpenMLPListFromFile('./SBPR5m(test).nnl');
	MLPList.SelectBestMLP(70, true);

	Log.Add('Done.');

	FreeAndNil(MLPList);
	FreeAndNil(BarList);
	FreeAndNil(DataBase);
	FreeAndNil(Log);
end.