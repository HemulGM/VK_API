unit VK.Notifications;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Notifications;

type
  TVkParamsNotificationsGet = record
    List: TParams;
    /// <summary>
    /// Указывает, какое максимальное число оповещений следует возвращать (максимальное значение 100)
    /// </summary>
    function Count(const Value: Integer = 30): TVkParamsNotificationsGet;
    /// <summary>
    /// Строковый идентификатор оповещения, полученного последним в предыдущем вызове (см. описание поля NextFrom в результате)
    /// </summary>
    function StartFrom(const Value: string): TVkParamsNotificationsGet;
    /// <summary>
    /// Перечисленные через запятую типы оповещений, которые необходимо получить.
    /// Если параметр не задан, то будут получены все возможные типы оповещений
    /// </summary>
    function Filters(const Value: TVkNotificationFilter): TVkParamsNotificationsGet;
    /// <summary>
    /// Время, начиная с которого следует получить оповещения для текущего пользователя.
    /// Если параметр не задан, то он считается равным значению времени, которое было сутки назад
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsNotificationsGet;
    /// <summary>
    /// Время, до которого следует получить оповещения для текущего пользователя.
    /// Если параметр не задан, то он считается равным текущему времени
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsNotificationsGet;
  end;

  TVkParamsNotificationsSendMessage = record
    List: TParams;
    /// <summary>
    /// Список идентификаторов пользователей, которым нужно отправить уведомление (максимум 100 идентификаторов)
    /// </summary>
    function UserIds(const Value: TIdList): TVkParamsNotificationsSendMessage;
    /// <summary>
    /// Текст уведомления (максимальная длина 254)
    /// </summary>
    function Message(const Value: string): TVkParamsNotificationsSendMessage;
    /// <summary>
    /// Содержимое хэша (часть URL в ссылке на приложение вида https://vk.com/app123456#fragment)
    /// </summary>
    function Fragment(const Value: string): TVkParamsNotificationsSendMessage;
    /// <summary>
    /// Ид сообщества
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsNotificationsSendMessage;
    /// <summary>
    /// Уникальный (в привязке к API_ID и ID отправителя) идентификатор, предназначенный для предотвращения повторной отправки одинакового сообщения.
    /// Заданный RandomId используется для проверки уникальности уведомления в течение часа после отправки
    /// </summary>
    function RandomId(const Value: Integer): TVkParamsNotificationsSendMessage;
  end;

  TNotificationsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список оповещений об ответах других пользователей на записи текущего пользователя.
    /// </summary>
    function Get(var Items: TVkNotifications; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список оповещений об ответах других пользователей на записи текущего пользователя.
    /// </summary>
    function Get(var Items: TVkNotifications; Params: TVkParamsNotificationsGet): Boolean; overload;
    /// <summary>
    /// Сбрасывает счетчик непросмотренных оповещений об ответах других пользователей на записи текущего пользователя.
    /// </summary>
    function MarkAsViewed(var WasNotifies: Boolean): Boolean;
    /// <summary>
    /// Отправляет уведомление пользователю приложения VK Apps.
    /// </summary>
    function SendMessage(var Status: TVkNotificationMessageStatuses; Params: TParams): Boolean; overload;
    /// <summary>
    /// Отправляет уведомление пользователю приложения VK Apps.
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

