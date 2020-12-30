unit VK.Utils;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Link, VK.Entity.ScreenName;

type
  TVkParamsUtilsGetLinkStats = record
    List: TParams;
    function Key(Value: string): Integer;
    function Source(Value: string): Integer;
    function AccessKey(Value: string): Integer;
    function Interval(Value: string): Integer;
    function IntervalsCount(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
  end;

  TUtilsController = class(TVkController)
  public
    /// <summary>
    /// ���������� ���������� � ���, �������� �� ������� ������ ��������������� �� ����� ���������.
    /// </summary>
    function CheckLink(var Info: TVkLinkStatus; Url: string): Boolean;
    /// <summary>
    /// ������� ����������� ������ �� ������ ������������.
    /// </summary>
    function DeleteFromLastShortened(Key: string): Boolean;
    /// <summary>
    /// �������� ������ ����������� ������ ��� �������� ������������.
    /// </summary>
    function GetLastShortenedLinks(var Items: TVkShortLinks; Offset: Integer = 0; Count: Integer = 10): Boolean;
    /// <summary>
    /// ���������� ���������� ��������� �� ����������� ������.
    /// </summary>
    function GetLinkStats(var Item: TVkLinkStats; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ��������� �� ����������� ������.
    /// </summary>
    function GetLinkStats(var Item: TVkLinkStats; Params: TVkParamsUtilsGetLinkStats): Boolean; overload;
    /// <summary>
    /// ���������� ������� ����� �� ������� ��������� � TDateTime.
    /// </summary>
    function GetServerTime(var ServerTime: TDateTime): Boolean; overload;
    /// <summary>
    /// ���������� ������� ����� �� ������� ��������� � unixtime.
    /// </summary>
    function GetServerTime(var ServerTime: Int64): Boolean; overload;
    /// <summary>
    /// ��������� �������� URL, ����������� � ������� vk.cc.
    /// </summary>
    function GetShortLink(var Item: TVkShortLink; const Url: string; &Private: Boolean = False): Boolean; overload;
    /// <summary>
    /// ���������� ��� ������� (������������, ����������, ����������) � ��� ������������� �� ��������� ����� screen_name.
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
  Result := GetServerTime(ST);
  if Result then
    ServerTime := UnixToDateTime(ST, False);
end;

function TUtilsController.CheckLink(var Info: TVkLinkStatus; Url: string): Boolean;
begin
  with Handler.Execute('utils.checkLink', ['url', Url]) do
  begin
    Result := Success;
    try
      Info := TVkLinkStatus.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

function TUtilsController.DeleteFromLastShortened(Key: string): Boolean;
begin
  with Handler.Execute('utils.deleteFromLastShortened', ['key', Key]) do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TUtilsController.GetLastShortenedLinks(var Items: TVkShortLinks; Offset, Count: Integer): Boolean;
begin
  with Handler.Execute('utils.getLastShortenedLinks', [['offset', Offset.ToString], ['count', Count.ToString]]) do
  begin
    Result := Success;
    try
      Items := TVkShortLinks.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

function TUtilsController.GetLinkStats(var Item: TVkLinkStats; Params: TVkParamsUtilsGetLinkStats): Boolean;
begin
  Result := GetLinkStats(Item, Params.List);
end;

function TUtilsController.GetLinkStats(var Item: TVkLinkStats; Params: TParams): Boolean;
begin
  with Handler.Execute('utils.getLinkStats', Params) do
  begin
    Result := Success;
    try
      Item := TVkLinkStats.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

function TUtilsController.GetServerTime(var ServerTime: Int64): Boolean;
begin
  with Handler.Execute('utils.getServerTime') do
  begin
    Result := Success and TryStrToInt64(Response, ServerTime);
  end;
end;

function TUtilsController.GetShortLink(var Item: TVkShortLink; const Url: string; &Private: Boolean): Boolean;
begin
  with Handler.Execute('utils.getShortLink', [['url', Url], ['private', BoolToString(&Private)]]) do
  begin
    Result := Success;
    try
      Item := TVkShortLink.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

function TUtilsController.ResolveScreenName(var Item: TVkScreenNameType; const ScreenName: string): Boolean;
begin
  with Handler.Execute('utils.resolveScreenName', ['screen_name', ScreenName]) do
  begin
    Result := Success;
    try
      Item := TVkScreenNameType.FromJsonString(Response);
    except
      Result := False;
    end;
  end;
end;

{ TVkParamsUtilsGetLinkStats }

function TVkParamsUtilsGetLinkStats.Key(Value: string): Integer;
begin
  Result := List.Add('key', Value);
end;

function TVkParamsUtilsGetLinkStats.Source(Value: string): Integer;
begin
  Result := List.Add('source', Value);
end;

function TVkParamsUtilsGetLinkStats.AccessKey(Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsUtilsGetLinkStats.Interval(Value: string): Integer;
begin
  Result := List.Add('interval', Value);
end;

function TVkParamsUtilsGetLinkStats.IntervalsCount(Value: Integer): Integer;
begin
  Result := List.Add('intervals_count', Value);
end;

function TVkParamsUtilsGetLinkStats.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

end.

