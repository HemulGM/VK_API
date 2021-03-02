unit VK.Streaming;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.Streaming;

type
  TVkParamsStreamGetStats = record
    List: TParams;
    /// <summary>
    /// ��� ����������
    /// </summary>
    function &Type(const Value: TVkStreamStatType): TVkParamsStreamGetStats;
    /// <summary>
    /// ��������� ����������
    /// </summary>
    function Interval(const Value: TVkStreamStatInterval): TVkParamsStreamGetStats;
    /// <summary>
    /// ����� ������ �������. �� ���������: EndTime ����� �����
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsStreamGetStats;
    /// <summary>
    /// ����� ��������� �������. �� ���������: ������� �����
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsStreamGetStats;
  end;

  /// <summary>
  /// ������ ������� ������ Streaming
  /// </summary>
  TStreamingController = class(TVkController)
  public
    /// <summary>
    /// ��������� �������� ������ ��� ����������� � Streaming API
    /// </summary>
    function GetServerUrl(var Data: TVkStreamServer): Boolean;
    /// <summary>
    /// ��������� �������� �������� ������ ��� Streaming API
    /// </summary>
    function GetSettings(var Data: TVkStreamLimit): Boolean;
    /// <summary>
    /// ��������� �������� ���������� ��� �������������� � ������������ ������� Streaming API.
    /// �������������� �����������, �� ������ ����������� ������� � ��������� ������ �� ���������
    /// �� ������� ������ ������� � ���� �������� prepared � received ����������, ������, ���-�� ����� �� ���.
    /// </summary>
    function GetStats(var Items: TVkStreamStats; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ���������� ��� �������������� � ������������ ������� Streaming API.
    /// �������������� �����������, �� ������ ����������� ������� � ��������� ������ �� ���������
    /// �� ������� ������ ������� � ���� �������� prepared � received ����������, ������, ���-�� ����� �� ���.
    /// </summary>
    function GetStats(var Items: TVkStreamStats; Params: TVkParamsStreamGetStats): Boolean; overload;
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

