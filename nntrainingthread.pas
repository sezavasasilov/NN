unit nnTrainingThread;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, nnLog, nnBarList, nnMLPList;

type
  TTrainingThread = class(TThread)
  private
    fLog : PLog;
    fBarList : PBarList;
    fMLPList : PMLPList;
    procedure AddToLog(const aMsg: String; aMsgType: TMsgType = Normal);
  protected
    procedure Execute; override;
  public
    constructor Create(aLog: PLog; aBarList: PBarList; aMLPList: PMLPList);
  end;

implementation

procedure TTrainingThread.AddToLog(const aMsg: String; 
  aMsgType: TMsgType = Normal);
begin
  if (fLog <> nil) and (fLog^ is TLog) then fLog^.Add(aMsg, aMsgType);
end;

procedure TTrainingThread.Execute;
begin
  // AddToLog('Выполнение потока...');
  Sleep(10);
  writeln('Выполнение потока...');
end;

constructor TTrainingThread.Create(aLog: PLog; 
  aBarList: PBarList; aMLPList: PMLPList);
begin
  inherited Create(true);
  Priority := tpLower;

  fLog := aLog;
  fBarList := aBarList;
  fMLPList := aMLPList;

  Start;
end;

end.
