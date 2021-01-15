unit VK.Notifications;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Audio, System.JSON, VK.Entity.Notifications;

type
  /// <summary>
  /// wall � ������ �� ����� ������������;
  /// mentions � ���������� � ������� �� �����, � ������������ ��� � �����������;
  /// comments � ����������� � ������� �� �����, ����������� � ������������;
  /// likes � ������� ���� ���������;
  /// reposts � ������������� � �������� ������������ ������ �� �����, ���������� � �����������;
  /// followers � ����� ����������;
  /// friends � �������� ������ � ������.
  /// </summary>
  TVkNotificationType = (nftWall, nftMentions, nftComments, nftLikes, nftReposts, nftFollowers, nftFriends);

  TVkNotificationTypeHelper = record Helper for TVkNotificationType
    function ToString: string; inline;
  end;

  TVkNotificationFilter = set of TVkNotificationType;

  TVkNotificationFilterHelper = record Helper for TVkNotificationFilter
    function ToString: string; inline;
  end;

  TVkParamsNotificationsGet = record
    List: TParams;
    function Count(Value: Integer): Integer;
    function StartFrom(Value: string): Integer;
    function Filters(Value: TVkNotificationFilter): Integer;
    function StartTime(Value: TDateTime): Integer;
    function EndTime(Value: TDateTime): Integer;
  end;

  TVkParamsNotificationsSendMessage = record
    List: TParams;
    /// <summary>
    /// ������ ��������������� �������������, ������� ����� ��������� ����������� (�������� 100 ���������������).
    /// </summary>
    function UserIds(Value: TIds): Integer;
    /// <summary>
    /// ����� �����������. ������������ ����� 254
    /// </summary>
    function Message(Value: string): Integer;
    /// <summary>
    /// ���������� ���� (����� URL � ������ �� ���������� ���� https://vk.com/app123456#fragment).
    /// </summary>
    function Fragment(Value: string): Integer;
    /// <summary>
    /// �� ����������
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// ���������� (� �������� � API_ID � ID �����������) �������������, ��������������� ��� �������������� ��������� �������� ����������� ���������.
    /// �������� random_id ������������ ��� �������� ������������ ����������� � ������� ���� ����� ��������.
    /// </summary>
    function RandomId(Value: Integer): Integer;
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
  System.DateUtils, VK.API, VK.CommonUtils;

{ TNotificationsController }

function TNotificationsController.Get(var Items: TVkNotifications; Params: TParams): Boolean;
begin
  Result := Handler.Execute('notifications.get', Params).GetObject<TVkNotifications>(Items);
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
  Result := Handler.Execute('notifications.sendMessage', Params).GetObjects<TVkNotificationMessageStatuses>(Status);
  if Result then
    Status.Count := Length(Status.Items);
end;

{ TVkParamsNotificationsGet }

function TVkParamsNotificationsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNotificationsGet.EndTime(Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsNotificationsGet.Filters(Value: TVkNotificationFilter): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsNotificationsGet.StartFrom(Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

function TVkParamsNotificationsGet.StartTime(Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

{ TVkNotificationTypeHelper }

function TVkNotificationTypeHelper.ToString: string;
begin
  case Self of
    nftWall:
      Result := 'wall';
    nftMentions:
      Result := 'mentions';
    nftComments:
      Result := 'comments';
    nftLikes:
      Result := 'likes';
    nftReposts:
      Result := 'reposts';
    nftFollowers:
      Result := 'followers';
    nftFriends:
      Result := 'friends';
  else
    Result := ''
  end;
end;

{ TVkNotificationFilterHelper }

function TVkNotificationFilterHelper.ToString: string;
var
  Item: TVkNotificationType;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkParamsNotificationsSendMessage }

function TVkParamsNotificationsSendMessage.Fragment(Value: string): Integer;
begin
  Result := List.Add('fragment', Value);
end;

function TVkParamsNotificationsSendMessage.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsNotificationsSendMessage.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsNotificationsSendMessage.RandomId(Value: Integer): Integer;
begin
  Result := List.Add('random_id', Value);
end;

function TVkParamsNotificationsSendMessage.UserIds(Value: TIds): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

end.

