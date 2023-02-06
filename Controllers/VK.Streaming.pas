unit VK.Streaming;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Streaming;

type
  TVkParamsStreamGetStats = record
    List: TParams;
    /// <summary>
    /// Тип статистики
    /// </summary>
    function &Type(const Value: TVkStreamStatType): TVkParamsStreamGetStats;
    /// <summary>
    /// Интервалы статистики
    /// </summary>
    function Interval(const Value: TVkStreamStatInterval): TVkParamsStreamGetStats;
    /// <summary>
    /// Время начала отсчёта. По умолчанию: EndTime минус сутки
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsStreamGetStats;
    /// <summary>
    /// Время окончания отсчёта. По умолчанию: текущее время
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsStreamGetStats;
  end;

  /// <summary>
  /// Список методов секции Streaming
  /// </summary>
  TStreamingController = class(TVkController)
  public
    /// <summary>
    /// Позволяет получить данные для подключения к Streaming API
    /// </summary>
    function GetServerUrl(var Data: TVkStreamServer): Boolean;
    /// <summary>
    /// Позволяет получить значение порога для Streaming API
    /// </summary>
    function GetSettings(var Data: TVkStreamLimit): Boolean;
    /// <summary>
    /// Позволяет получить статистику для подготовленных и доставленных событий Streaming API.
    /// Руководствуясь статистикой, Вы можете отслеживать перебои в получении данных от ВКонтакте
    /// на стороне Вашего сервера — если значения prepared и received расходятся, значит, что-то пошло не так.
    /// </summary>
    function GetStats(var Items: TVkStreamStats; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет получить статистику для подготовленных и доставленных событий Streaming API.
    /// Руководствуясь статистикой, Вы можете отслеживать перебои в получении данных от ВКонтакте
    /// на стороне Вашего сервера — если значения prepared и received расходятся, значит, что-то пошло не так.
    /// </summary>
    function GetStats(var Items: TVkStreamStats; Params: TVkParamsStreamGetStats): Boolean; overload;
    /// <summary>
    /// Позволяет задать значение порога для Streaming API
    /// </summary>
    function SetSettings(const MonthlyTier: TVkMonthlyTier): Boolean;
    /// <summary>
    /// Позволяет получить основу слова.
    /// <param name="const Text: string">Слово, основу которого мы собираемся получить.</param>
    /// </summary>
    function GetStem(out Stem: string; const Word: string): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TStreamingController }

function TStreamingController.GetServerUrl(var Data: TVkStreamServer): Boolean;
begin
  Result := Handler.Execute('streaming.getServerUrl').GetObject(Data);
end;

function TStreamingController.GetSettings(var Data: TVkStreamLimit): Boolean;
begin
  Result := Handler.Execute('streaming.getSettings').GetObject(Data);
end;

function TStreamingController.GetStats(var Items: TVkStreamStats; Params: TVkParamsStreamGetStats): Boolean;
begin
  Result := GetStats(Items, Params.List);
end;

function TStreamingController.GetStem(out Stem: string; const Word: string): Boolean;
begin
  Result := Handler.Execute('streaming.getStem', ['word', Word]).GetValue('stem', Stem);
end;

function TStreamingController.SetSettings(const MonthlyTier: TVkMonthlyTier): Boolean;
begin
  Result := Handler.Execute('streaming.setSettings', ['monthly_tier', MonthlyTier.ToString]).ResponseIsTrue;
end;

function TStreamingController.GetStats(var Items: TVkStreamStats; Params: TParams): Boolean;
begin
  Result := Handler.Execute('streaming.getStats', Params).GetObjects(Items);
end;

{ TVkParamsStreamGetStats }

function TVkParamsStreamGetStats.EndTime(const Value: TDateTime): TVkParamsStreamGetStats;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsStreamGetStats.Interval(const Value: TVkStreamStatInterval): TVkParamsStreamGetStats;
begin
  List.Add('interval', Value.ToString);
  Result := Self;
end;

function TVkParamsStreamGetStats.StartTime(const Value: TDateTime): TVkParamsStreamGetStats;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

function TVkParamsStreamGetStats.&Type(const Value: TVkStreamStatType): TVkParamsStreamGetStats;
begin
  List.Add('type', Value.ToString);
  Result := Self;
end;

end.

