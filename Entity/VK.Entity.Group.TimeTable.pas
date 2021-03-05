unit VK.Entity.Group.TimeTable;

interface

uses
  System.JSON, VK.Entity.Common;

type
  TVkTimeTableDay = record
  private
    FBreak_close_time: Integer;
    FBreak_open_time: Integer;
    FClose_time: Integer;
    FOpen_time: Integer;
    FUse: Boolean;
    FUseBreakCloseTime: Boolean;
    FUseBreakOpenTime: Boolean;
    FUseCloseTime: Boolean;
    FUseOpenTime: Boolean;
    procedure SetBreakCloseTime(const Value: Integer);
    procedure SetBreakOpenTime(const Value: Integer);
    procedure SetCloseTime(const Value: Integer);
    procedure SetOpenTime(const Value: Integer);
  public
    /// <summary>
    /// Конец перерыва (в минутах)
    /// </summary>
    property BreakCloseTime: Integer read FBreak_close_time write SetBreakCloseTime;
    /// <summary>
    /// Начало перерыва (в минутах)
    /// </summary>
    property BreakOpenTime: Integer read FBreak_open_time write SetBreakOpenTime;
    /// <summary>
    /// Конец рабочего дня (в минутах)
    /// </summary>
    property CloseTime: Integer read FClose_time write SetCloseTime;
    /// <summary>
    /// Начало рабочего дня (в минутах)
    /// </summary>
    property OpenTime: Integer read FOpen_time write SetOpenTime;
    property IsUse: Boolean read FUse;
    procedure Clear;
    function ToJSON: TJSONObject;
  end;

  TVkTimeTable = record
  private
    FFri: TVkTimeTableDay;
    FMon: TVkTimeTableDay;
    FSat: TVkTimeTableDay;
    FThu: TVkTimeTableDay;
    FTue: TVkTimeTableDay;
    FWed: TVkTimeTableDay;
    FSun: TVkTimeTableDay;
  public
    /// <summary>
    /// Понетельник
    /// </summary>
    property Monday: TVkTimeTableDay read FMon write FMon;
    /// <summary>
    /// Вторник
    /// </summary>
    property Tuesday: TVkTimeTableDay read FTue write FTue;
    /// <summary>
    /// Среда
    /// </summary>
    property Wednesday: TVkTimeTableDay read FWed write FWed;
    /// <summary>
    /// Четверг
    /// </summary>
    property Thursday: TVkTimeTableDay read FThu write FThu;
    /// <summary>
    /// Пятница
    /// </summary>
    property Friday: TVkTimeTableDay read FFri write FFri;
    /// <summary>
    /// Суббота
    /// </summary>
    property Saturday: TVkTimeTableDay read FSat write FSat;
    /// <summary>
    /// Воскресенье
    /// </summary>
    property Sunday: TVkTimeTableDay read FSun write FSun;
    function ToJSON: string;
  end;

implementation

{ TVkTimeTable }

function TVkTimeTable.ToJSON: string;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    if FMon.IsUse then
      JSON.AddPair('mon', FMon.ToJSON);
    if FTue.IsUse then
      JSON.AddPair('tue', FTue.ToJSON);
    if FWed.IsUse then
      JSON.AddPair('wed', FWed.ToJSON);
    if FThu.IsUse then
      JSON.AddPair('thu', FThu.ToJSON);
    if FFri.IsUse then
      JSON.AddPair('fri', FFri.ToJSON);
    if FSat.IsUse then
      JSON.AddPair('sat', FSat.ToJSON);
    if FSun.IsUse then
      JSON.AddPair('sun', FSun.ToJSON);
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

{ TVkTimeTableDay }

procedure TVkTimeTableDay.Clear;
begin
  FUse := False;
end;

procedure TVkTimeTableDay.SetBreakCloseTime(const Value: Integer);
begin
  FBreak_close_time := Value;
  FUse := True;
  FUseBreakCloseTime := True;
end;

procedure TVkTimeTableDay.SetBreakOpenTime(const Value: Integer);
begin
  FBreak_open_time := Value;
  FUse := True;
  FUseBreakOpenTime := True;
end;

procedure TVkTimeTableDay.SetCloseTime(const Value: Integer);
begin
  FClose_time := Value;
  FUse := True;
  FUseCloseTime := True;
end;

procedure TVkTimeTableDay.SetOpenTime(const Value: Integer);
begin
  FOpen_time := Value;
  FUse := True;
  FUseOpenTime := True;
end;

function TVkTimeTableDay.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  if FUseBreakCloseTime then
    Result.AddPair('break_close_time', TJSONNumber.Create(FBreak_close_time));
  if FUseBreakOpenTime then
    Result.AddPair('break_open_time', TJSONNumber.Create(FBreak_open_time));
  if FUseCloseTime then
    Result.AddPair('close_time', TJSONNumber.Create(FClose_time));
  if FUseOpenTime then
    Result.AddPair('open_time', TJSONNumber.Create(FOpen_time));
end;

end.

