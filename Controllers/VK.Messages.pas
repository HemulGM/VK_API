unit VK.Messages;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, System.Json, VK.Controller, VK.Types,
  VK.Handler, VK.Entity.Keyboard, VK.Entity.Message, VK.Entity.Conversation, VK.Entity.User,
  VK.Entity.Group;

type
  TMessagesController = class;

  TNewMessage = class
  private
    FHandler: TVkHandler;
    FParams: TParams;
    procedure SetParams(const Value: TParams);
  public
    function PeerId(Id: Integer): TNewMessage;
    function UserId(Id: Integer): TNewMessage;
    function ChatId(Id: Integer): TNewMessage;
    function UserIds(Ids: TUserIds): TNewMessage;
    function UserDomian(Domian: string): TNewMessage;
    function Message(Text: string): TNewMessage;
    function Payload(Value: string): TNewMessage;
    function Intent(Value: string): TNewMessage;
    function Keyboard(Value: TVkKeyboardConstructor): TNewMessage;
    function DontParseLinks(Value: Boolean): TNewMessage;
    function DisableMentions(Value: Boolean): TNewMessage;
    function StickerId(Id: Integer): TNewMessage;
    function GroupId(Id: Integer): TNewMessage;
    function ReplyTo(Id: Integer): TNewMessage;
    function ForwardMessages(Ids: TArrayOfInteger): TNewMessage;
    function Attachment(Attachments: TAttachmentArray): TNewMessage;
    function Send: TVkMessageSendResponses;
    constructor Create(Controller: TMessagesController);
    property Handler: TVkHandler read FHandler;
    property Params: TParams read FParams write SetParams;
  end;

  TParamConversation = record
    List: TParams;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
    function MajorSortId(Value: Integer): Integer;
    function Filter(Value: string): Integer;
    function Fields(Value: string): Integer;
    function Extended(Value: Boolean): Integer;
    function StartMessageId(Value: Integer): Integer;
  end;

  TParamGet = record
    List: TParams;
    function MessageIds(Value: TIds): Integer; overload;
    function MessageIds(Value: Integer): Integer; overload;
    function PreviewLength(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(Value: string): Integer;
    function GroupId(Value: Integer): Integer;
  end;

  TParamMessageHistory = record
    List: TParams;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Fields(Value: string): Integer;
    function Extended(Value: Boolean): Integer;
    function GroupId(Value: Integer): Integer;
    function UserId(Value: Integer): Integer;
    function PeerId(Value: Integer): Integer;
    function StartMessageId(Value: Integer): Integer;
    function Rev(Value: Boolean): Integer;
  end;

  TMessagesController = class(TVkController)
  public
    /// <summary>
    /// Отправить сообщение.
    /// Для пользователя: id пользователя.
    /// Для групповой беседы: 2000000000 + id беседы.
    /// Для сообщества: -id сообщества.
    /// </summary>
    /// <param name="PeerId">Ид чата</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function Send(PeerId: Integer; Message: string; Attachments: TAttachmentArray = []): Integer; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    /// <param name="UserId">Ид пользователя</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToUser(UserId: Integer; Message: string; Attachments: TAttachmentArray = []):
      Integer; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    /// <param name="UserDomain">Короткий адрес пользователя (например, illarionov)</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToUser(UserDomain: string; Message: string; Attachments: TAttachmentArray = []):
      Integer; overload;
    /// <summary>
    /// Отправить сообщение в беседу
    /// </summary>
    /// <param name="ChatId">Ид беседы</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToChat(ChatId: Integer; Message: string; Attachments: TAttachmentArray = []):
      Integer; overload;
    /// <summary>
    /// Отправить сообщение нескольким пользователям (Доступно только для ключа доступа сообщества)
    /// </summary>
    /// <param name="UserIds">Ид пользователей</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToUsers(UserIds: TUserIds; Message: string; Attachments: TAttachmentArray = []):
      TVkMessageSendResponses; overload;
    /// <summary>
    /// Универсальный метод отправки сообщений (Fluent Method)
    /// Send.PeerId(123456).ReplyTo(12345)...Message('Текст').Send.Free;
    /// </summary>
    /// <returns>Возвращает конструктор сообщений. Метод Send в этом конструкторе, возвращает результат в виде класса</returns>
    function New: TNewMessage; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    /// <param name="var Messages: TVkMessages">Список сообщений</param>
    /// <param name="Params: TParamGet">Параметры</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetById(var Messages: TVkMessages; Params: TParamGet): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    /// <param name="var Messages: TVkMessages">Список сообщений</param>
    /// <param name="Ids: TIds">Идентификаторы сообщений</param>
    /// <param name="PreviewLength: Integer = 0">Обрезать текст сообщений</param>
    /// <param name="GroupId: Integer = 0">Группа, для которой необходимо получить сообщения</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetById(var Messages: TVkMessages; Ids: TIds; PreviewLength: Integer = 0; GroupId:
      Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает сообщение по идентификатору.
    /// </summary>
    /// <param name="var Message: TVkMessages">Сообщение</param>
    /// <param name="var Profiles: TArray(TVkUser)">Список профилей (Extended)</param>
    /// <param name="var Groups: TArray(TVkGroup)">Список групп (Extended)</param>
    /// <param name="Id: TId">Идентификатор сообщения</param>
    /// <param name="PreviewLength: Integer = 0">Обрезать текст сообщений</param>
    /// <param name="GroupId: Integer = 0">Группа, для которой необходимо получить сообщения</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetById(var Message: TVkMessage; var Profiles: TArray<TVkUser>; var Groups: TArray<
      TVkGroup>; Id: Integer; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает сообщение по идентификатору.
    /// </summary>
    /// <param name="var Message: TVkMessages">Сообщение</param>
    /// <param name="Id: TId">Идентификатор сообщения</param>
    /// <param name="PreviewLength: Integer = 0">Обрезать текст сообщений</param>
    /// <param name="GroupId: Integer = 0">Группа, для которой необходимо получить сообщения</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetById(var Message: TVkMessage; Id: Integer; PreviewLength: Integer = 0; GroupId:
      Integer = 0): Boolean; overload;
    /// <summary>
    /// Добавляет в мультидиалог нового пользователя.
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function AddChatUser(ChatId, UserId: Integer; VisibleMessagesCount: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет сообщения
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function Delete(MessageIds: TIds; GroupID: Integer = 0; DeleteForAll: Boolean = False; Spam:
      Boolean = False): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function Delete(MessageId: Integer; GroupID: Integer = 0; DeleteForAll: Boolean = False; Spam:
      Boolean = False): Boolean; overload;
    /// <summary>
    /// Получает ссылку для приглашения пользователя в беседу.
    /// Только создатель беседы имеет доступ к ссылке на беседу.
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetInviteLink(var Link: string; PeerId: Integer; Reset: Boolean = False; GroupId:
      Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список бесед пользователя.
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetConversations(var Conversations: TVkConversationItems; Params: TParamConversation): Boolean;
    /// <summary>
    /// Возвращает историю сообщений для указанного диалога.
    /// </summary>
    /// <param name="var History: TVkMessageHistory">История</param>
    /// <param name="Params: TParamMessageHistory">Параметры</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetHistory(var History: TVkMessageHistory; Params: TParamMessageHistory): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TMessagesController }

function TMessagesController.Send(PeerId: Integer; Message: string; Attachments: TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add(['peer_id', PeerId.ToString]);
  Params.Add(['message', Message]);
  Params.Add(['random_id', GetRandomId.ToString]);
  if Length(Attachments) <> 0 then
    Params.Add(['attachment', Attachments.ToString]);

  with Handler.Execute('messages.send', Params) do
  begin
    if not Success then
      Exit(-1)
    else
    begin
      Result := Response.ToInteger;
    end;
  end;
end;

function TMessagesController.AddChatUser(ChatId, UserId, VisibleMessagesCount: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add(['chat_id', ChatId.ToString]);
  Params.Add(['user_id', UserId.ToString]);
  Params.Add(['visible_messages_count', VisibleMessagesCount.ToString]);

  with Handler.Execute('messages.addChatUser', Params) do
  begin
    if not Success then
      Exit(False)
    else
    begin
      try
        Result := Response.ToInteger = 1;
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.Delete(MessageIds: TIds; GroupID: Integer; DeleteForAll, Spam: Boolean): Boolean;
var
  Params: TParams;
  RespJSON: TJSONValue;
  i, DelRes: Integer;
begin
  if GroupID < 0 then
    raise TVkException.Create('GroupID должен быть положительным числом');
  Params.Add('message_ids', MessageIds.ToString);
  if DeleteForAll then
    Params.Add('delete_for_all', DeleteForAll);
  if DeleteForAll then
    Params.Add('spam', Spam);
  if GroupID > 0 then
    Params.Add('group_id', GroupID.ToString);
  with Handler.Execute('messages.delete', Params) do
  begin
    if not Success then
      Exit(False)
    else
    begin
      RespJSON := TJSONObject.ParseJSONValue(Response);
      try
        Result := True;
        for i := Low(MessageIds) to High(MessageIds) do
        begin
          if RespJSON.TryGetValue<Integer>(MessageIds[i].ToString, DelRes) then
            Result := DelRes = 1
          else
            Result := False;
          if not Result then
            Break;
        end;
      finally
        RespJSON.Free;
      end;
    end;
  end;
end;

function TMessagesController.Delete(MessageId, GroupID: Integer; DeleteForAll, Spam: Boolean): Boolean;
begin
  Result := Delete([MessageId], GroupID, DeleteForAll, Spam);
end;

function TMessagesController.GetById(var Message: TVkMessage; Id: Integer; PreviewLength, GroupId:
  Integer): Boolean;
var
  Profiles: TArray<TVkUser>;
  Groups: TArray<TVkGroup>;
  LUserItem: TVkUser;
  LGroupItem: TVkGroup;
begin
  Result := GetById(Message, Profiles, Groups, Id, PreviewLength, GroupId);

  for LUserItem in Profiles do
    LUserItem.Free;
  for LGroupItem in Groups do
    LGroupItem.Free;
end;

function TMessagesController.GetById(var Message: TVkMessage; var Profiles: TArray<TVkUser>; var
  Groups: TArray<TVkGroup>; Id: Integer; PreviewLength, GroupId: Integer): Boolean;
var
  Params: TParamGet;
  Items: TVkMessages;
begin
  Params.MessageIds([Id]);
  if PreviewLength > 0 then
    Params.PreviewLength(PreviewLength);
  if GroupId <> 0 then
    Params.GroupId(GroupId);
  Params.Extended(True);
  Result := GetById(Items, Params);
  if Result then
  begin
    if Length(Items.Items) > 0 then
    begin
      Message := Items.Items[0];
      Profiles := Items.Profiles;
      Groups := Items.Groups;
      Items.SaveObjects := True;
      Items.Free;
    end
    else
      Result := False;
  end;
end;

function TMessagesController.GetById(var Messages: TVkMessages; Ids: TIds; PreviewLength, GroupId:
  Integer): Boolean;
var
  Params: TParamGet;
begin
  Params.MessageIds(Ids);
  if PreviewLength > 0 then
    Params.PreviewLength(PreviewLength);
  if GroupId > 0 then
    Params.GroupId(GroupId);
  Result := GetById(Messages, Params);
end;

function TMessagesController.GetById(var Messages: TVkMessages; Params: TParamGet): Boolean;
begin
  with Handler.Execute('messages.getById', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Messages := TVkMessages.FromJsonString(Response);
    end;
  end;
end;

function TMessagesController.GetConversations(var Conversations: TVkConversationItems; Params:
  TParamConversation): Boolean;
begin
  with Handler.Execute('messages.getConversations', Params.List) do
  begin
    if not Success then
      Exit(False)
    else
    begin
      try
        Conversations := TVkConversationItems.FromJsonString(Response);
        Result := True;
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.GetHistory(var History: TVkMessageHistory; Params: TParamMessageHistory):
  Boolean;
begin
  with Handler.Execute('messages.getHistory', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      History := TVkMessageHistory.FromJsonString(Response);
    end;
  end;
end;

function TMessagesController.GetInviteLink(var Link: string; PeerId: Integer; Reset: Boolean;
  GroupId: Integer): Boolean;
var
  Params: TParams;
  RespJSON: TJSONValue;
begin
  Params.Add('peer_id', PeerId);
  if Reset then
    Params.Add('reset', Reset);
  if GroupId > 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('messages.getInviteLink', Params) do
  begin
    Result := Success;
    if Result then
    begin
      RespJSON := TJSONObject.ParseJSONValue(Response);
      try
        Link := RespJSON.GetValue<string>('link', '');
        Result := not Link.IsEmpty;
      finally
        RespJSON.Free;
      end;
    end;
  end;
end;

function TMessagesController.New: TNewMessage;
begin
  Result := TNewMessage.Create(Self);
end;

function TMessagesController.SendToChat(ChatId: Integer; Message: string; Attachments:
  TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add('chat_id', ChatId);
  Params.Add('message', Message);
  Params.Add('random_id', GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Add('attachment', Attachments.ToString);

  with Handler.Execute('messages.send', Params) do
  begin
    if not Success then
      Exit(-1)
    else
    begin
      Result := Response.ToInteger;
    end;
  end;
end;

function TMessagesController.SendToUser(UserDomain, Message: string; Attachments: TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add('domian', UserDomain);
  Params.Add('message', Message);
  Params.Add('random_id', GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Add('attachment', Attachments.ToString);

  with Handler.Execute('messages.send', Params) do
  begin
    if not Success then
      Exit(-1)
    else
    begin
      Result := Response.ToInteger;
    end;
  end;
end;

function TMessagesController.SendToUsers(UserIds: TUserIds; Message: string; Attachments:
  TAttachmentArray): TVkMessageSendResponses;
var
  Params: TParams;
begin
  Params.Add('user_ids', UserIds.ToString);
  Params.Add('message', Message);
  Params.Add('random_id', GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Add('attachment', Attachments.ToString);

  with Handler.Execute('messages.send', Params) do
  begin
    if not Success then
      Exit(nil)
    else
    begin
      Result := TVkMessageSendResponses.FromJsonString(Response);
    end;
  end;
end;

function TMessagesController.SendToUser(UserId: Integer; Message: string; Attachments:
  TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add('user_id', UserId);
  Params.Add('message', Message);
  Params.Add('random_id', GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Add('attachment', Attachments.ToString);

  with Handler.Execute('messages.send', Params) do
  begin
    if not Success then
      Exit(-1)
    else
    begin
      Result := Response.ToInteger;
    end;
  end;
end;

{ TNewMessage }

constructor TNewMessage.Create(Controller: TMessagesController);
begin
  FHandler := Controller.Handler;
end;

function TNewMessage.Send: TVkMessageSendResponses;
var
  Value: Integer;
begin
  FParams.Add('random_id', GetRandomId);
  with Handler.Execute('messages.send', Params) do
  begin
    if not Success then
      Exit(TVkMessageSendResponses.CreateFalse)
    else
    begin
      if TryStrToInt(Response, Value) then
        Result := TVkMessageSendResponses.CreateTrue(Value)
      else
        Result := TVkMessageSendResponses.FromJsonString(Response);
    end;
  end;
  Free;
end;

procedure TNewMessage.SetParams(const Value: TParams);
begin
  FParams := Value;
end;

function TNewMessage.DisableMentions(Value: Boolean): TNewMessage;
begin
  Params.Add('disable_mentions', Value);
  Result := Self;
end;

function TNewMessage.DontParseLinks(Value: Boolean): TNewMessage;
begin
  Params.Add('dont_parse_links', Value);
  Result := Self;
end;

function TNewMessage.StickerId(Id: Integer): TNewMessage;
begin
  Params.Add('sticker_id', Id);
  Result := Self;
end;

function TNewMessage.ForwardMessages(Ids: TArrayOfInteger): TNewMessage;
begin
  Params.Add('forward_messages', Ids.ToString);
  Result := Self;
end;

function TNewMessage.GroupID(Id: Integer): TNewMessage;
begin
  Params.Add('group_id', Id);
  Result := Self;
end;

function TNewMessage.Intent(Value: string): TNewMessage;
begin
  Params.Add('intent', Value);
  Result := Self;
end;

function TNewMessage.Keyboard(Value: TVkKeyboardConstructor): TNewMessage;
begin
  Params.Add('keyboard', Value.ToJsonString);
  Result := Self;
end;

function TNewMessage.Attachment(Attachments: TAttachmentArray): TNewMessage;
begin
  Params.Add('attachment', Attachments.ToString);
  Result := Self;
end;

function TNewMessage.ChatId(Id: Integer): TNewMessage;
begin
  Params.Add('chat_id', Id);
  Result := Self;
end;

function TNewMessage.Message(Text: string): TNewMessage;
begin
  Params.Add('message', Text);
  Result := Self;
end;

function TNewMessage.Payload(Value: string): TNewMessage;
begin
  Params.Add('payload', Value);
  Result := Self;
end;

function TNewMessage.PeerId(Id: Integer): TNewMessage;
begin
  Params.Add('peer_id', Id);
  Result := Self;
end;

function TNewMessage.ReplyTo(Id: Integer): TNewMessage;
begin
  Params.Add('reply_to', Id);
  Result := Self;
end;

function TNewMessage.UserDomian(Domian: string): TNewMessage;
begin
  Params.Add('domian', Domian);
  Result := Self;
end;

function TNewMessage.UserId(Id: Integer): TNewMessage;
begin
  Params.Add('user_id', Id);
  Result := Self;
end;

function TNewMessage.UserIds(Ids: TUserIds): TNewMessage;
begin
  Params.Add('user_ids', Ids.ToString);
  Result := Self;
end;

{ TParamConversation }

function TParamConversation.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TParamConversation.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TParamConversation.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TParamConversation.Filter(Value: string): Integer;
begin
  Result := List.Add('filter', Value);
end;

function TParamConversation.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TParamConversation.MajorSortId(Value: Integer): Integer;
begin
  Result := List.Add('major_sort_id', Value);
end;

function TParamConversation.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TParamConversation.StartMessageId(Value: Integer): Integer;
begin
  Result := List.Add('start_message_id', Value);
end;

{ TParamMessageHistory }

function TParamMessageHistory.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TParamMessageHistory.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TParamMessageHistory.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TParamMessageHistory.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TParamMessageHistory.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TParamMessageHistory.PeerId(Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TParamMessageHistory.Rev(Value: Boolean): Integer;
begin
  Result := List.Add('rev', Value);
end;

function TParamMessageHistory.StartMessageId(Value: Integer): Integer;
begin
  Result := List.Add('start_message_id', Value);
end;

function TParamMessageHistory.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TParamGet }

function TParamGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TParamGet.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TParamGet.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TParamGet.MessageIds(Value: Integer): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TParamGet.MessageIds(Value: TIds): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TParamGet.PreviewLength(Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

end.

