unit VK.Stats;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Stats;

type
  TVkStatInterval = (siDay, siWeek, siMonth, siYear, siAll);

  TVkStatIntervalHelper = record helper for TVkStatInterval
    function ToString: string;
  end;

  TVkParamsStatsGet = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function AppId(Value: Integer): Integer;
    function TimestampFrom(Value: Integer): Integer;
    function TimestampTo(Value: Integer): Integer;
    function Interval(Value: TVkStatInterval): Integer;
    function IntervalsCount(Value: Integer): Integer;
    function Filters(Value: TArrayOfString): Integer;
    /// <summary>
    /// visitors, reach, activity
    /// </summary>
    function StatsGroups(Value: TArrayOfString): Integer;
    function Extended(Value: Boolean): Integer;
  end;

  /// <summary>
  /// ������ ��� ������ �� �����������.
  /// </summary>
  TStatsController = class(TVkController)
  public
    /// <summary>
    /// ���������� ���������� ���������� ��� ����������.
    /// �������� ��������, ���� ���������� ������ (�������� ������ ��� ��������������� ����������/����������), ��� ��������� ������ � ������ ���������� ���������� access_token � ������� ������� stats.
    /// </summary>
    function Get(var Items: TVkStatItems; const Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ���������� ��� ����������.
    /// �������� ��������, ���� ���������� ������ (�������� ������ ��� ��������������� ����������/����������), ��� ��������� ������ � ������ ���������� ���������� access_token � ������� ������� stats.
    /// </summary>
    function Get(var Items: TVkStatItems; const Params: TVkParamsStatsGet): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ��� ������ �� �����.
    /// ���������� ������� � ��������� ������ �������� ������ ��� ��������� � ����������� ���������� �� 5000 � ����, � ����� ����������� ���������.
    /// ������������, �� ����� �������� ���������� �����, ������ ���� ���������� ��� ��������������� ����������, � ������� ��������� ������.
    /// </summary>
    function GetPostReach(var Items: TVkStatPostReachItems; const OwnerId: Integer; PostIds: TIds): Boolean; overload;
    /// <summary>
    /// ��������� ������ � ������� ������ � ���������� ������������ ����������.
    /// ����� ������� ������ ������� ������ � ������� ����������� �������� ������ ���������� ������ �������� ������� ��������������. � ��� ����� ���������� ���������� � ����� �������� � ���������� ����������� ������ ����������.
    /// ��� ��������� ����������� ������ � ������������ ���������� ����� ����� �������� ��� ������ �������.
    /// </summary>
    function TrackVisitor: Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TStatsController }

function TStatsController.Get(var Items: TVkStatItems; const Params: TParams): Boolean;
begin
  with Handler.Execute('stats.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkStatItems.FromJsonString(ResponseAsItems);
      except
        Result := False;
      end;
    end;
  end;
end;

function TStatsController.Get(var Items: TVkStatItems; const Params: TVkParamsStatsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TStatsController.GetPostReach(var Items: TVkStatPostReachItems; const OwnerId: Integer; PostIds: TIds): Boolean;
begin
  with Handler.Execute('stats.getPostReach', [['owner_id', OwnerId.ToString], ['post_ids', PostIds.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkStatPostReachItems.FromJsonString(ResponseAsItems);
      except
        Result := False;
      end;
    end;
  end;
end;

function TStatsController.TrackVisitor: Boolean;
begin
  with Handler.Execute('stats.trackVisitor') do
    Result := Success and ResponseIsTrue;
end;

{ TVkStatIntervalHelper }

function TVkStatIntervalHelper.ToString: string;
begin
  case Self of
    siDay:
      Result := 'day';
    siWeek:
      Result := 'week';
    siMonth:
      Result := 'month';
    siYear:
      Result := 'year';
    siAll:
      Result := 'all';
  end;
end;

{ TVkParamsStatsGet }

function TVkParamsStatsGet.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsStatsGet.AppId(Value: Integer): Integer;
begin
  Result := List.Add('app_id', Value);
end;

function TVkParamsStatsGet.TimestampFrom(Value: Integer): Integer;
begin
  Result := List.Add('timestamp_from', Value);
end;

function TVkParamsStatsGet.TimestampTo(Value: Integer): Integer;
begin
  Result := List.Add('timestamp_to', Value);
end;

function TVkParamsStatsGet.Interval(Value: TVkStatInterval): Integer;
begin
  Result := List.Add('interval', Value.ToString);
end;

function TVkParamsStatsGet.IntervalsCount(Value: Integer): Integer;
begin
  Result := List.Add('intervals_count', Value);
end;

function TVkParamsStatsGet.Filters(Value: TArrayOfString): Integer;
begin
  Result := List.Add('filters', Value);
end;

function TVkParamsStatsGet.StatsGroups(Value: TArrayOfString): Integer;
begin
  Result := List.Add('stats_groups', Value);
end;

function TVkParamsStatsGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

end.

