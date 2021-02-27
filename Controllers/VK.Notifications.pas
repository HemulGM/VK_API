unit VK.Notifications;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Notifications;

type
  TVkParamsNotificationsGet = record
    List: TParams;
    /// <summary>
    /// ���������, ����� ������������ ����� ���������� ������� ���������� (������������ �������� 100)
    /// </summary>
    function Count(const Value: Integer = 30): Integer;
    /// <summary>
    /// ��������� ������������� ����������, ����������� ��������� � ���������� ������ (��. �������� ���� NextFrom � ����������)
    /// </summary>
    function StartFrom(const Value: string): Integer;
    /// <summary>
    /// ������������� ����� ������� ���� ����������, ������� ���������� ��������.
    /// ���� �������� �� �����, �� ����� �������� ��� ��������� ���� ����������
    /// </summary>
    function Filters(const Value: TVkNotificationFilter): Integer;
    /// <summary>
    /// �����, ������� � �������� ������� �������� ���������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������, ������� ���� ����� �����
    /// </summary>
    function StartTime(const Value: TDateTime): Integer;
    /// <summary>
    /// �����, �� �������� ������� �������� ���������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������
    /// </summary>
    function EndTime(const Value: TDateTime): Integer;
  end;

  TVkParamsNotificationsSendMessage = record
    List: TParams;
    /// <summary>
    /// ������ ��������������� �������������, ������� ����� ��������� ����������� (�������� 100 ���������������)
    /// </summary>
    function UserIds(const Value: TIdList): Integer;
    /// <summary>
    /// ����� ����������� (������������ ����� 254)
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ���������� ���� (����� URL � ������ �� ���������� ���� https://vk.com/app123456#fragment)
    /// </summary>
    function Fragment(const Value: string): Integer;
    /// <summary>
    /// �� ����������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ���������� (� �������� � API_ID � ID �����������) �������������, ��������������� ��� �������������� ��������� �������� ����������� ���������.
    /// �������� RandomId ������������ ��� �������� ������������ ����������� � ������� ���� ����� ��������
    /// </summary>
    function RandomId(const Value: Integer): Integer;
  end;

  TNotificationsController = class(TVkController)
  public
    /// <summary>
    /// ���������� ������ ���������� �� ������� ������ ������������� �� ������ �������� ������������.
    /// </summary>
    function Get(var Items: TVkNotifications; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ���������� �� ������� ������ ������������� �� ������ �������� ������������.
    /// </summary>
    function Get(var Items: TVkNotifications; Params: TVkParamsNotificationsGet): Boolean; overload;
    /// <summary>
    /// ���������� ������� ��������������� ���������� �� ������� ������ ������������� �� ������ �������� ������������.
    /// </summary>
    function MarkAsViewed(var WasNotifies: Boolean): Boolean;
    /// <summary>
    /// ���������� ����������� ������������ ���������� VK Apps.
    /// </summary>
    function SendMessage(var Status: TVkNotificationMessageStatuses; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ����������� ������������ ���������� VK Apps.
    /// </summary>
    function SendMessage(var Status: TVkNotificationMessageStatuses; Params: TVkParamsNotificationsSendMessage): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TNotificationsController }

function TNotificationsController.Get(var Items: TVkNotifications; Params: TParams): Boolean;
begin
  Result := Handler.Execute('notifications.get', Params).GetObject(Items);
end;

function TNotificationsController.Get(var Items: TVkNotifications; Params: TVkParamsNotificationsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TNotificationsController.MarkAsViewed(var WasNotifies: Boolean): Boolean;
begin
  Result := Handler.Execute('notifications.markAsViewed').ResponseAsBool(WasNotifies);
end;

function TNotificationsController.SendMessage(var Status: TVkNotificationMessageStatuses; Params: TVkParamsNotificationsSendMessage): Boolean;
begin
  Result := SendMessage(Status, Params.List);
end;

function TNotificationsController.SendMessage(var Status: TVkNotificationMessageStatuses; Params: TParams): Boolean;
begin
  Result := Handler.Execute('notifications.sendMessage', Params).GetObjects(Status);
  if Result then
    Status.Count := Length(Status.Items);
end;

{ TVkParamsNotificationsGet }

function TVkParamsNotificationsGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNotificationsGet.EndTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNotificationsGet.Filters(const Value: TVkNotificationFilter): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsNotificationsGet.StartFrom(const Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

function TVkParamsNotificationsGet.StartTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

{ TVkParamsNotificationsSendMessage }

function TVkParamsNotificationsSendMessage.Fragment(const Value: string): Integer;
begin
  Result := List.Add('fragment', Value);
end;

function TVkParamsNotificationsSendMessage.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsNotificationsSendMessage.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsNotificationsSendMessage.RandomId(const Value: Integer): Integer;
begin
  Result := List.Add('random_id', Value);
end;

function TVkParamsNotificationsSendMessage.UserIds(const Value: TIdList): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

end.

