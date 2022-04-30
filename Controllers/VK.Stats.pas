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
    function GroupId(const Value: Cardinal): TVkParamsStatsGet;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function AppId(const Value: Cardinal): TVkParamsStatsGet;
    /// <summary>
    /// ������ ������� ����������
    /// </summary>
    function TimestampFrom(const Value: TDateTime): TVkParamsStatsGet;
    /// <summary>
    /// ��������� ������� ����������
    /// </summary>
    function TimestampTo(const Value: TDateTime): TVkParamsStatsGet;
    /// <summary>
    /// ��������� ���������
    /// </summary>
    function Interval(const Value: TVkStatInterval = TVkStatInterval.Day): TVkParamsStatsGet;
    /// <summary>
    /// ���������� ���������� �������
    /// </summary>
    function IntervalsCount(const Value: Integer): TVkParamsStatsGet;
    /// <summary>
    /// [��� ��������]
    /// </summary>
    function Filters(const Value: TArrayOfString): TVkParamsStatsGet;
    /// <summary>
    /// ������ ��� ��������� ������ �� ����������� ����� ���������� ����������
    /// </summary>
    function StatsGroups(const Value: TVkStatReachFilters): TVkParamsStatsGet;
    /// <summary>
    /// True � ���������� ������������� �������������� ������ � �����������
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsStatsGet;
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

function TVkParamsStatsGet.GroupId(const Value: Cardinal): TVkParamsStatsGet;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsStatsGet.AppId(const Value: Cardinal): TVkParamsStatsGet;
begin
  List.Add('app_id', Value);
  Result := Self;
end;

function TVkParamsStatsGet.TimestampFrom(const Value: TDateTime): TVkParamsStatsGet;
begin
  List.Add('timestamp_from', Value);
  Result := Self;
end;

function TVkParamsStatsGet.TimestampTo(const Value: TDateTime): TVkParamsStatsGet;
begin
  List.Add('timestamp_to', Value);
  Result := Self;
end;

function TVkParamsStatsGet.Interval(const Value: TVkStatInterval): TVkParamsStatsGet;
begin
  List.Add('interval', Value.ToString);
  Result := Self;
end;

function TVkParamsStatsGet.IntervalsCount(const Value: Integer): TVkParamsStatsGet;
begin
  List.Add('intervals_count', Value);
  Result := Self;
end;

function TVkParamsStatsGet.Filters(const Value: TArrayOfString): TVkParamsStatsGet;
begin
  List.Add('filters', Value);
  Result := Self;
end;

function TVkParamsStatsGet.StatsGroups(const Value: TVkStatReachFilters): TVkParamsStatsGet;
begin
  List.Add('stats_groups', Value.ToString);
  Result := Self;
end;

function TVkParamsStatsGet.Extended(const Value: Boolean): TVkParamsStatsGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

end.

