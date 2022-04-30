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
    function Count(const Value: Integer = 30): TVkParamsNotificationsGet;
    /// <summary>
    /// ��������� ������������� ����������, ����������� ��������� � ���������� ������ (��. �������� ���� NextFrom � ����������)
    /// </summary>
    function StartFrom(const Value: string): TVkParamsNotificationsGet;
    /// <summary>
    /// ������������� ����� ������� ���� ����������, ������� ���������� ��������.
    /// ���� �������� �� �����, �� ����� �������� ��� ��������� ���� ����������
    /// </summary>
    function Filters(const Value: TVkNotificationFilter): TVkParamsNotificationsGet;
    /// <summary>
    /// �����, ������� � �������� ������� �������� ���������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������, ������� ���� ����� �����
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsNotificationsGet;
    /// <summary>
    /// �����, �� �������� ������� �������� ���������� ��� �������� ������������.
    /// ���� �������� �� �����, �� �� ��������� ������ �������� �������
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsNotificationsGet;
  end;

  TVkParamsNotificationsSendMessage = record
    List: TParams;
    /// <summary>
    /// ������ ��������������� �������������, ������� ����� ��������� ����������� (�������� 100 ���������������)
    /// </summary>
    function UserIds(const Value: TIdList): TVkParamsNotificationsSendMessage;
    /// <summary>
    /// ����� ����������� (������������ ����� 254)
    /// </summary>
    function Message(const Value: string): TVkParamsNotificationsSendMessage;
    /// <summary>
    /// ���������� ���� (����� URL � ������ �� ���������� ���� https://vk.com/app123456#fragment)
    /// </summary>
    function Fragment(const Value: string): TVkParamsNotificationsSendMessage;
    /// <summary>
    /// �� ����������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsNotificationsSendMessage;
    /// <summary>
    /// ���������� (� �������� � API_ID � ID �����������) �������������, ��������������� ��� �������������� ��������� �������� ����������� ���������.
    /// �������� RandomId ������������ ��� �������� ������������ ����������� � ������� ���� ����� ��������
    /// </summary>
    function RandomId(const Value: Integer): TVkParamsNotificationsSendMessage;
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

function TVkParamsNotificationsGet.Count(const Value: Integer): TVkParamsNotificationsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsNotificationsGet.EndTime(const Value: TDateTime): TVkParamsNotificationsGet;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsNotificationsGet.Filters(const Value: TVkNotificationFilter): TVkParamsNotificationsGet;
begin
  List.Add('filters', Value.ToString);
  Result := Self;
end;

function TVkParamsNotificationsGet.StartFrom(const Value: string): TVkParamsNotificationsGet;
begin
  List.Add('start_from', Value);
  Result := Self;
end;

function TVkParamsNotificationsGet.StartTime(const Value: TDateTime): TVkParamsNotificationsGet;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

{ TVkParamsNotificationsSendMessage }

function TVkParamsNotificationsSendMessage.Fragment(const Value: string): TVkParamsNotificationsSendMessage;
begin
  List.Add('fragment', Value);
  Result := Self;
end;

function TVkParamsNotificationsSendMessage.GroupId(const Value: Integer): TVkParamsNotificationsSendMessage;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsNotificationsSendMessage.Message(const Value: string): TVkParamsNotificationsSendMessage;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsNotificationsSendMessage.RandomId(const Value: Integer): TVkParamsNotificationsSendMessage;
begin
  List.Add('random_id', Value);
  Result := Self;
end;

function TVkParamsNotificationsSendMessage.UserIds(const Value: TIdList): TVkParamsNotificationsSendMessage;
begin
  List.Add('user_ids', Value);
  Result := Self;
end;

end.

