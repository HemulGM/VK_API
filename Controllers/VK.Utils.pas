unit VK.Utils;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Link, VK.Entity.ScreenName;

type
  TVkParamsUtilsGetLinkStats = record
    List: TParams;
    /// <summary>
    /// ����������� ������ (����� URL ����� "vk.cc/")
    /// </summary>
    function Key(const Value: string): TVkParamsUtilsGetLinkStats;
    /// <summary>
    /// ������, �� ��������� vk_cc
    /// </summary>
    function Source(const Value: string): TVkParamsUtilsGetLinkStats;
    /// <summary>
    /// ���� ������� � ��������� ���������� ������
    /// </summary>
    function AccessKey(const Value: string): TVkParamsUtilsGetLinkStats;
    /// <summary>
    /// ������� ������� ��� �������� ����������
    /// </summary>
    function Interval(const Value: TVkStatInterval = TVkStatInterval.Day): TVkParamsUtilsGetLinkStats;
    /// <summary>
    /// ������������ ������� ��� ��������� ���������� � ��������� �������� (�� ��������� Interval)
    /// </summary>
    function IntervalsCount(const Value: Integer): TVkParamsUtilsGetLinkStats;
    /// <summary>
    /// True � ���������� ����������� ���������� (���/�������/������/�����), False � ���������� ������ ���������� ���������
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsUtilsGetLinkStats;
  end;

  TUtilsController = class(TVkController)
  public
    /// <summary>
    /// ���������� ���������� � ���, �������� �� ������� ������ ��������������� �� ����� ���������
    /// </summary>
    function CheckLink(var Info: TVkLinkStatus; Url: string): Boolean;
    /// <summary>
    /// ������� ����������� ������ �� ������ ������������
    /// </summary>
    function DeleteFromLastShortened(Key: string): Boolean;
    /// <summary>
    /// �������� ������ ����������� ������ ��� �������� ������������
    /// </summary>
    function GetLastShortenedLinks(var Items: TVkShortLinks; Offset: Integer = 0; Count: Integer = 10): Boolean;
    /// <summary>
    /// ���������� ���������� ��������� �� ����������� ������
    /// </summary>
    function GetLinkStats(var Item: TVkLinkStats; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ��������� �� ����������� ������
    /// </summary>
    function GetLinkStats(var Item: TVkLinkStats; Params: TVkParamsUtilsGetLinkStats): Boolean; overload;
    /// <summary>
    /// ���������� ������� ����� �� ������� ���������
    /// </summary>
    function GetServerTime(var ServerTime: TDateTime): Boolean; overload;
    /// <summary>
    /// ���������� ������� ����� �� ������� ��������� � unixtime
    /// </summary>
    function GetServerTimeUnix(var ServerTime: Int64): Boolean; overload;
    /// <summary>
    /// ��������� �������� URL, ����������� � ������� vk.cc
    /// </summary>
    function GetShortLink(var Item: TVkShortLink; const Url: string; &Private: Boolean = False): Boolean; overload;
    /// <summary>
    /// ���������� ��� ������� (������������, ����������, ����������) � ��� ������������� �� ��������� ����� ScreenName
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
  Result := Handler.Execute('utils.resolveScreenName', ['screen_name', ScreenName]).GetObject(Item);
end;

{ TVkParamsUtilsGetLinkStats }

function TVkParamsUtilsGetLinkStats.Key(const Value: string): TVkParamsUtilsGetLinkStats;
begin
  List.Add('key', Value);
  Result := Self;
end;

function TVkParamsUtilsGetLinkStats.Source(const Value: string): TVkParamsUtilsGetLinkStats;
begin
  List.Add('source', Value);
  Result := Self;
end;

function TVkParamsUtilsGetLinkStats.AccessKey(const Value: string): TVkParamsUtilsGetLinkStats;
begin
  List.Add('access_key', Value);
  Result := Self;
end;

function TVkParamsUtilsGetLinkStats.Interval(const Value: TVkStatInterval): TVkParamsUtilsGetLinkStats;
begin
  List.Add('interval', Value.ToString);
  Result := Self;
end;

function TVkParamsUtilsGetLinkStats.IntervalsCount(const Value: Integer): TVkParamsUtilsGetLinkStats;
begin
  List.Add('intervals_count', Value);
  Result := Self;
end;

function TVkParamsUtilsGetLinkStats.Extended(const Value: Boolean): TVkParamsUtilsGetLinkStats;
begin
  List.Add('extended', Value);
  Result := Self;
end;

end.

