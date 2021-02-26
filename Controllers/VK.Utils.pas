unit VK.Utils;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Link, VK.Entity.ScreenName;

type
  TVkParamsUtilsGetLinkStats = record
    List: TParams;
    /// <summary>
    /// Сокращенная ссылка (часть URL после "vk.cc/")
    /// </summary>
    function Key(const Value: string): Integer;
    /// <summary>
    /// Строка, по умолчанию vk_cc
    /// </summary>
    function Source(const Value: string): Integer;
    /// <summary>
    /// Ключ доступа к приватной статистике ссылки
    /// </summary>
    function AccessKey(const Value: string): Integer;
    /// <summary>
    /// Единица времени для подсчета статистики
    /// </summary>
    function Interval(const Value: TVkStatInterval = TVkStatInterval.Day): Integer;
    /// <summary>
    /// Длительность периода для получения статистики в выбранных единицах (из параметра Interval)
    /// </summary>
    function IntervalsCount(const Value: Integer): Integer;
    /// <summary>
    /// True — возвращать расширенную статистику (пол/возраст/страна/город), False — возвращать только количество переходов
    /// </summary>
    function Extended(const Value: Boolean): Integer;
  end;

  TUtilsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает информацию о том, является ли внешняя ссылка заблокированной на сайте ВКонтакте
    /// </summary>
    function CheckLink(var Info: TVkLinkStatus; Url: string): Boolean;
    /// <summary>
    /// Удаляет сокращенную ссылку из списка пользователя
    /// </summary>
    function DeleteFromLastShortened(Key: string): Boolean;
    /// <summary>
    /// Получает список сокращенных ссылок для текущего пользователя
    /// </summary>
    function GetLastShortenedLinks(var Items: TVkShortLinks; Offset: Integer = 0; Count: Integer = 10): Boolean;
    /// <summary>
    /// Возвращает статистику переходов по сокращенной ссылке
    /// </summary>
    function GetLinkStats(var Item: TVkLinkStats; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает статистику переходов по сокращенной ссылке
    /// </summary>
    function GetLinkStats(var Item: TVkLinkStats; Params: TVkParamsUtilsGetLinkStats): Boolean; overload;
    /// <summary>
    /// Возвращает текущее время на сервере ВКонтакте
    /// </summary>
    function GetServerTime(var ServerTime: TDateTime): Boolean; overload;
    /// <summary>
    /// Возвращает текущее время на сервере ВКонтакте в unixtime
    /// </summary>
    function GetServerTimeUnix(var ServerTime: Int64): Boolean; overload;
    /// <summary>
    /// Позволяет получить URL, сокращенный с помощью vk.cc
    /// </summary>
    function GetShortLink(var Item: TVkShortLink; const Url: string; &Private: Boolean = False): Boolean; overload;
    /// <summary>
    /// Определяет тип объекта (пользователь, сообщество, приложение) и его идентификатор по короткому имени ScreenName
    /// </summary>
    function ResolveScreenName(var Item: TVkScreenNameType; const ScreenName: string): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TUtilsController }

function TUtilsController.GetServerTime(var ServerTime: TDateTime): Boolean;
var
  ST: Int64;
begin
  Result := GetServerTimeUnix(ST);
  if Result then
    ServerTime := UnixToDateTime(ST, False);
end;

function TUtilsController.CheckLink(var Info: TVkLinkStatus; Url: string): Boolean;
begin
  Result := Handler.Execute('utils.checkLink', ['url', Url]).GetObject<TVkLinkStatus>(Info);
end;

function TUtilsController.DeleteFromLastShortened(Key: string): Boolean;
begin
  Result := Handler.Execute('utils.deleteFromLastShortened', ['key', Key]).ResponseIsTrue;
end;

function TUtilsController.GetLastShortenedLinks(var Items: TVkShortLinks; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('utils.getLastShortenedLinks', [
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject<TVkShortLinks>(Items);
end;

function TUtilsController.GetLinkStats(var Item: TVkLinkStats; Params: TVkParamsUtilsGetLinkStats): Boolean;
begin
  Result := GetLinkStats(Item, Params.List);
end;

function TUtilsController.GetLinkStats(var Item: TVkLinkStats; Params: TParams): Boolean;
begin
  Result := Handler.Execute('utils.getLinkStats', Params).GetObject<TVkLinkStats>(Item);
end;

function TUtilsController.GetServerTimeUnix(var ServerTime: Int64): Boolean;
begin
  Result := Handler.Execute('utils.getServerTime').ResponseAsInt64(ServerTime);
end;

function TUtilsController.GetShortLink(var Item: TVkShortLink; const Url: string; &Private: Boolean): Boolean;
begin
  Result := Handler.Execute('utils.getShortLink', [
    ['url', Url],
    ['private', BoolToString(&Private)]]).
    GetObject<TVkShortLink>(Item);
end;

function TUtilsController.ResolveScreenName(var Item: TVkScreenNameType; const ScreenName: string): Boolean;
begin
  Result := Handler.Execute('utils.resolveScreenName', ['screen_name', ScreenName]).GetObject<TVkScreenNameType>(Item);
end;

{ TVkParamsUtilsGetLinkStats }

function TVkParamsUtilsGetLinkStats.Key(const Value: string): Integer;
begin
  Result := List.Add('key', Value);
end;

function TVkParamsUtilsGetLinkStats.Source(const Value: string): Integer;
begin
  Result := List.Add('source', Value);
end;

function TVkParamsUtilsGetLinkStats.AccessKey(const Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsUtilsGetLinkStats.Interval(const Value: TVkStatInterval): Integer;
begin
  Result := List.Add('interval', Value.ToString);
end;

function TVkParamsUtilsGetLinkStats.IntervalsCount(const Value: Integer): Integer;
begin
  Result := List.Add('intervals_count', Value);
end;

function TVkParamsUtilsGetLinkStats.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

end.

