unit VK.Stats;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Stats;

type
  TVkParamsStatsGet = record
    List: TParams;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function GroupId(const Value: Cardinal): Integer;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function AppId(const Value: Cardinal): Integer;
    /// <summary>
    /// ������ ������� ����������
    /// </summary>
    function TimestampFrom(const Value: TDateTime): Integer;
    /// <summary>
    /// ��������� ������� ����������
    /// </summary>
    function TimestampTo(const Value: TDateTime): Integer;
    /// <summary>
    /// ��������� ���������
    /// </summary>
    function Interval(const Value: TVkStatInterval = TVkStatInterval.Day): Integer;
    /// <summary>
    /// ���������� ���������� �������
    /// </summary>
    function IntervalsCount(const Value: Integer): Integer;
    /// <summary>
    /// [��� ��������]
    /// </summary>
    function Filters(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ������ ��� ��������� ������ �� ����������� ����� ���������� ����������
    /// </summary>
    function StatsGroups(const Value: TVkStatReachFilters): Integer;
    /// <summary>
    /// True � ���������� ������������� �������������� ������ � �����������
    /// </summary>
    function Extended(const Value: Boolean = True): Integer;
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
    function GetPostReach(var Items: TVkStatPostReachItems; const OwnerId: Integer; PostIds: TIdList): Boolean; overload;
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

