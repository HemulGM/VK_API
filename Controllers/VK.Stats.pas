unit VK.Stats;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Stats;

type
  TVkParamsStatsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: Cardinal): Integer;
    /// <summary>
    /// Идентификатор приложения
    /// </summary>
    function AppId(const Value: Cardinal): Integer;
    /// <summary>
    /// Начало периода статистики
    /// </summary>
    function TimestampFrom(const Value: TDateTime): Integer;
    /// <summary>
    /// Окончание периода статистики
    /// </summary>
    function TimestampTo(const Value: TDateTime): Integer;
    /// <summary>
    /// Временные интервалы
    /// </summary>
    function Interval(const Value: TVkStatInterval = TVkStatInterval.Day): Integer;
    /// <summary>
    /// Количество интервалов времени
    /// </summary>
    function IntervalsCount(const Value: Integer): Integer;
    /// <summary>
    /// [Нет описания]
    /// </summary>
    function Filters(const Value: TArrayOfString): Integer;
    /// <summary>
    /// Фильтр для получения данных по конкретному блоку статистики сообщества
    /// </summary>
    function StatsGroups(const Value: TVkStatReachFilters): Integer;
    /// <summary>
    /// True — возвращать дополнительно агрегированные данные в результатах
    /// </summary>
    function Extended(const Value: Boolean = True): Integer;
  end;

  /// <summary>
  /// Методы для работы со статистикой.
  /// </summary>
  TStatsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает статистику сообщества или приложения.
    /// Обратите внимание, если статистика скрыта (доступна только для администраторов сообщества/приложения), для получения данных в вызове необходимо передавать access_token с правами доступа stats.
    /// </summary>
    function Get(var Items: TVkStatItems; const Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает статистику сообщества или приложения.
    /// Обратите внимание, если статистика скрыта (доступна только для администраторов сообщества/приложения), для получения данных в вызове необходимо передавать access_token с правами доступа stats.
    /// </summary>
    function Get(var Items: TVkStatItems; const Params: TVkParamsStatsGet): Boolean; overload;
    /// <summary>
    /// Возвращает статистику для записи на стене.
    /// Статистика записей в настоящий момент доступна только для сообществ с количеством участников от 5000 и выше, а также официальных сообществ.
    /// Пользователь, от имени которого вызывается метод, должен быть редактором или администратором сообщества, в котором размещена запись.
    /// </summary>
    function GetPostReach(var Items: TVkStatPostReachItems; const OwnerId: Integer; PostIds: TIdList): Boolean; overload;
    /// <summary>
    /// Добавляет данные о текущем сеансе в статистику посещаемости приложения.
    /// После первого вызова данного метода в разделе «Статистика» настроек Вашего приложения станет доступна вкладка «Посещаемость». В ней будет отображена информация о числе запусков и уникальных посетителей Вашего приложения.
    /// Для получения достоверных данных о посещаемости приложения метод нужно вызывать при каждом запуске.
    /// </summary>
    function TrackVisitor: Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TStatsController }

function TStatsController.Get(var Items: TVkStatItems; const Params: TVkParamsStatsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TStatsController.Get(var Items: TVkStatItems; const Params: TParams): Boolean;
begin
  Result := Handler.Execute('stats.get', Params).GetObjects(Items);
end;

function TStatsController.GetPostReach(var Items: TVkStatPostReachItems; const OwnerId: Integer; PostIds: TIdList): Boolean;
begin
  Result := Handler.Execute('stats.getPostReach', [['owner_id', OwnerId.ToString], ['post_ids', PostIds.ToString]]).GetObjects(Items);
end;

function TStatsController.TrackVisitor: Boolean;
begin
  Result := Handler.Execute('stats.trackVisitor').ResponseIsTrue;
end;

{ TVkParamsStatsGet }

function TVkParamsStatsGet.GroupId(const Value: Cardinal): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsStatsGet.AppId(const Value: Cardinal): Integer;
begin
  Result := List.Add('app_id', Value);
end;

function TVkParamsStatsGet.TimestampFrom(const Value: TDateTime): Integer;
begin
  Result := List.Add('timestamp_from', Value);
end;

function TVkParamsStatsGet.TimestampTo(const Value: TDateTime): Integer;
begin
  Result := List.Add('timestamp_to', Value);
end;

function TVkParamsStatsGet.Interval(const Value: TVkStatInterval): Integer;
begin
  Result := List.Add('interval', Value.ToString);
end;

function TVkParamsStatsGet.IntervalsCount(const Value: Integer): Integer;
begin
  Result := List.Add('intervals_count', Value);
end;

function TVkParamsStatsGet.Filters(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('filters', Value);
end;

function TVkParamsStatsGet.StatsGroups(const Value: TVkStatReachFilters): Integer;
begin
  Result := List.Add('stats_groups', Value.ToString);
end;

function TVkParamsStatsGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

end.

