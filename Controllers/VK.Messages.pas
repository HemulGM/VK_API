unit VK.Messages;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, REST.Client, System.Json, VK.Controller, VK.Types,
  VK.Handler, VK.Entity.Keyboard, VK.Entity.Message, VK.Entity.Conversation, VK.Entity.User, VK.Entity.Group;

type
  TMessagesController = class;

  TVkMessageNew = class
  private
    FHandler: TVkHandler;
    FParams: TParams;
    procedure SetParams(const Value: TParams);
  public
    function PeerId(Id: Integer): TVkMessageNew;
    function UserId(Id: Integer): TVkMessageNew;
    function ChatId(Id: Integer): TVkMessageNew;
    function UserIds(Ids: TUserIds): TVkMessageNew;
    function UserDomian(Domian: string): TVkMessageNew;
    function Message(Text: string): TVkMessageNew;
    function Payload(Value: string): TVkMessageNew;
    function Intent(Value: string): TVkMessageNew;
    function Keyboard(Value: TVkKeyboardConstructor): TVkMessageNew;
    function DontParseLinks(Value: Boolean): TVkMessageNew;
    function DisableMentions(Value: Boolean): TVkMessageNew;
    function StickerId(Id: Integer): TVkMessageNew;
    function GroupId(Id: Integer): TVkMessageNew;
    function ReplyTo(Id: Integer): TVkMessageNew;
    function ForwardMessages(Ids: TArrayOfInteger): TVkMessageNew;
    function Attachment(Attachments: TAttachmentArray): TVkMessageNew;
    function Send: TVkMessageSendResponses;
    constructor Create(Controller: TMessagesController);
    property Handler: TVkHandler read FHandler;
    property Params: TParams read FParams write SetParams;
  end;

  TVkParamsConversationsGet = record
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

  TVkParamsMessageDelete = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function MessageIds(Value: TIds): Integer; overload;
    function MessageId(Value: Integer): Integer; overload;
    function Spam(Value: Boolean): Integer;
    function DeleteForAll(Value: Boolean): Integer;
  end;

  TVkParamsMessageGet = record
    List: TParams;
    function MessageIds(Value: TIds): Integer; overload;
    function MessageId(Value: Integer): Integer; overload;
    function PreviewLength(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(Value: string): Integer;
    function GroupId(Value: Integer): Integer;
  end;

  TVkParamsMessageHistory = record
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
    function SendToUser(UserId: Integer; Message: string; Attachments: TAttachmentArray = []): Integer; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    /// <param name="UserDomain">Короткий адрес пользователя (например, illarionov)</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToUser(UserDomain: string; Message: string; Attachments: TAttachmentArray = []): Integer; overload;
    /// <summary>
    /// Отправить сообщение в беседу
    /// </summary>
    /// <param name="ChatId">Ид беседы</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToChat(ChatId: Integer; Message: string; Attachments: TAttachmentArray = []): Integer; overload;
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
    function New: TVkMessageNew; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    /// <param name="var Messages: TVkMessages">Список сообщений</param>
    /// <param name="Params: TParamGet">Параметры</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetById(var Messages: TVkMessages; Params: TVkParamsMessageGet): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    /// <param name="var Messages: TVkMessages">Список сообщений</param>
    /// <param name="Ids: TIds">Идентификаторы сообщений</param>
    /// <param name="PreviewLength: Integer = 0">Обрезать текст сообщений</param>
    /// <param name="GroupId: Integer = 0">Группа, для которой необходимо получить сообщения</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetById(var Messages: TVkMessages; Ids: TIds; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
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
    function GetById(var Message: TVkMessage; var Profiles: TArray<TVkUser>; var Groups: TArray<TVkGroup>; Id: Integer;
      PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает сообщение по идентификатору.
    /// </summary>
    /// <param name="var Message: TVkMessages">Сообщение</param>
    /// <param name="Id: TId">Идентификатор сообщения</param>
    /// <param name="PreviewLength: Integer = 0">Обрезать текст сообщений</param>
    /// <param name="GroupId: Integer = 0">Группа, для которой необходимо получить сообщения</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetById(var Message: TVkMessage; Id: Integer; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Добавляет в мультидиалог нового пользователя.
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function AddChatUser(ChatId, UserId: Integer; VisibleMessagesCount: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет сообщения
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function Delete(var Items: TVkMessageDelete; MessageIds: TIds; GroupID: Integer = 0; DeleteForAll: Boolean = False;
      Spam: Boolean = False): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function Delete(var Items: TVkMessageDelete; MessageId: Integer; GroupID: Integer = 0; DeleteForAll: Boolean = False;
      Spam: Boolean = False): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function Delete(var Items: TVkMessageDelete; Params: TVkParamsMessageDelete): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function Delete(var Items: TVkMessageDelete; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает ссылку для приглашения пользователя в беседу.
    /// Только создатель беседы имеет доступ к ссылке на беседу.
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetInviteLink(var Link: string; PeerId: Integer; Reset: Boolean = False; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список бесед пользователя.
    /// </summary>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetConversations(var Conversations: TVkConversationItems; Params: TVkParamsConversationsGet): Boolean;
    /// <summary>
    /// Возвращает историю сообщений для указанного диалога.
    /// </summary>
    /// <param name="var History: TVkMessageHistory">История</param>
    /// <param name="Params: TParamMessageHistory">Параметры</param>
    /// <returns>Возвращает True, если запрос успешно выполнен</returns>
    function GetHistory(var History: TVkMessageHistory; Params: TVkParamsMessageHistory): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TMessagesController }

function TMessagesController.Send(PeerId: Integer; Message: string; Attachments: TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  Params.Add('message', Message);
  Params.Add('random_id', GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Add('attachment', Attachments);

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

function TMessagesController.Delete(var Items: TVkMessageDelete; MessageIds: TIds; GroupID: Integer; DeleteForAll, Spam:
  Boolean): Boolean;
var
  Params: TVkParamsMessageDelete;
begin
  Params.MessageIds(MessageIds);
  if DeleteForAll then
    Params.DeleteForAll(DeleteForAll);
  if Spam then
    Params.Spam(Spam);
  if GroupID <> 0 then
    Params.GroupId(GroupID);
  Result := Delete(Items, Params.List);
end;

function TMessagesController.Delete(var Items: TVkMessageDelete; MessageId, GroupID: Integer; DeleteForAll, Spam:
  Boolean): Boolean;
begin
  Result := Delete(Items, [MessageId], GroupID, DeleteForAll, Spam);
end;

function TMessagesController.Delete(var Items: TVkMessageDelete; Params: TParams): Boolean;
var
  RespJSON: TJSONValue;
  Ids: TStringList;
  i: Integer;
begin
  with Handler.Execute('messages.delete', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Ids := TStringList.Create;
      Ids.Delimiter := ',';
      Ids.DelimitedText := Params.GetValue('message_ids');
      Items := TVkMessageDelete.Create;
      try
        RespJSON := TJSONObject.ParseJSONValue(Response);
        for i := 0 to Ids.Count - 1 do
        begin
          Items.Items.Add(Ids[i], RespJSON.GetValue<Integer>(Ids[i], 0) = 1);
        end;
        RespJSON.Free;
      except
        Items.Free;
        Result := False;
      end;
      Ids.Free;
    end;
  end;
end;

function TMessagesController.Delete(var Items: TVkMessageDelete; Params: TVkParamsMessageDelete): Boolean;
begin
  Result := Delete(Items, Params.List);
end;

function TMessagesController.GetById(var Message: TVkMessage; Id: Integer; PreviewLength, GroupId: Integer): Boolean;
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

function TMessagesController.GetById(var Message: TVkMessage; var Profiles: TArray<TVkUser>; var Groups: TArray<TVkGroup
  >; Id: Integer; PreviewLength, GroupId: Integer): Boolean;
var
  Params: TVkParamsMessageGet;
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

function TMessagesController.GetById(var Messages: TVkMessages; Ids: TIds; PreviewLength, GroupId: Integer): Boolean;
var
  Params: TVkParamsMessageGet;
begin
  Params.MessageIds(Ids);
  if PreviewLength > 0 then
    Params.PreviewLength(PreviewLength);
  if GroupId > 0 then
    Params.GroupId(GroupId);
  Result := GetById(Messages, Params);
end;

function TMessagesController.GetById(var Messages: TVkMessages; Params: TVkParamsMessageGet): Boolean;
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

function TMessagesController.GetConversations(var Conversations: TVkConversationItems; Params: TVkParamsConversationsGet):
  Boolean;
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

function TMessagesController.GetHistory(var History: TVkMessageHistory; Params: TVkParamsMessageHistory): Boolean;
begin
  with Handler.Execute('messages.getHistory', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        History := TVkMessageHistory.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.GetInviteLink(var Link: string; PeerId: Integer; Reset: Boolean; GroupId: Integer): Boolean;
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

function TMessagesController.New: TVkMessageNew;
begin
  Result := TVkMessageNew.Create(Self);
end;

function TMessagesController.SendToChat(ChatId: Integer; Message: string; Attachments: TAttachmentArray): Integer;
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

function TMessagesController.SendToUsers(UserIds: TUserIds; Message: string; Attachments: TAttachmentArray):
  TVkMessageSendResponses;
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

function TMessagesController.SendToUser(UserId: Integer; Message: string; Attachments: TAttachmentArray): Integer;
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

constructor TVkMessageNew.Create(Controller: TMessagesController);
begin
  FHandler := Controller.Handler;
end;

function TVkMessageNew.Send: TVkMessageSendResponses;
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

procedure TVkMessageNew.SetParams(const Value: TParams);
begin
  FParams := Value;
end;

function TVkMessageNew.DisableMentions(Value: Boolean): TVkMessageNew;
begin
  Params.Add('disable_mentions', Value);
  Result := Self;
end;

function TVkMessageNew.DontParseLinks(Value: Boolean): TVkMessageNew;
begin
  Params.Add('dont_parse_links', Value);
  Result := Self;
end;

function TVkMessageNew.StickerId(Id: Integer): TVkMessageNew;
begin
  Params.Add('sticker_id', Id);
  Result := Self;
end;

function TVkMessageNew.ForwardMessages(Ids: TArrayOfInteger): TVkMessageNew;
begin
  Params.Add('forward_messages', Ids.ToString);
  Result := Self;
end;

function TVkMessageNew.GroupID(Id: Integer): TVkMessageNew;
begin
  Params.Add('group_id', Id);
  Result := Self;
end;

function TVkMessageNew.Intent(Value: string): TVkMessageNew;
begin
  Params.Add('intent', Value);
  Result := Self;
end;

function TVkMessageNew.Keyboard(Value: TVkKeyboardConstructor): TVkMessageNew;
begin
  Params.Add('keyboard', Value.ToJsonString);
  Result := Self;
end;

function TVkMessageNew.Attachment(Attachments: TAttachmentArray): TVkMessageNew;
begin
  Params.Add('attachment', Attachments.ToString);
  Result := Self;
end;

function TVkMessageNew.ChatId(Id: Integer): TVkMessageNew;
begin
  Params.Add('chat_id', Id);
  Result := Self;
end;

function TVkMessageNew.Message(Text: string): TVkMessageNew;
begin
  Params.Add('message', Text);
  Result := Self;
end;

function TVkMessageNew.Payload(Value: string): TVkMessageNew;
begin
  Params.Add('payload', Value);
  Result := Self;
end;

function TVkMessageNew.PeerId(Id: Integer): TVkMessageNew;
begin
  Params.Add('peer_id', Id);
  Result := Self;
end;

function TVkMessageNew.ReplyTo(Id: Integer): TVkMessageNew;
begin
  Params.Add('reply_to', Id);
  Result := Self;
end;

function TVkMessageNew.UserDomian(Domian: string): TVkMessageNew;
begin
  Params.Add('domian', Domian);
  Result := Self;
end;

function TVkMessageNew.UserId(Id: Integer): TVkMessageNew;
begin
  Params.Add('user_id', Id);
  Result := Self;
end;

function TVkMessageNew.UserIds(Ids: TUserIds): TVkMessageNew;
begin
  Params.Add('user_ids', Ids.ToString);
  Result := Self;
end;

{ TVkGetConversationParams }

function TVkParamsConversationsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsConversationsGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsConversationsGet.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsConversationsGet.Filter(Value: string): Integer;
begin
  Result := List.Add('filter', Value);
end;

function TVkParamsConversationsGet.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsConversationsGet.MajorSortId(Value: Integer): Integer;
begin
  Result := List.Add('major_sort_id', Value);
end;

function TVkParamsConversationsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsConversationsGet.StartMessageId(Value: Integer): Integer;
begin
  Result := List.Add('start_message_id', Value);
end;

{ TParamMessageHistory }

function TVkParamsMessageHistory.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMessageHistory.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMessageHistory.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsMessageHistory.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageHistory.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMessageHistory.PeerId(Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageHistory.Rev(Value: Boolean): Integer;
begin
  Result := List.Add('rev', Value);
end;

function TVkParamsMessageHistory.StartMessageId(Value: Integer): Integer;
begin
  Result := List.Add('start_message_id', Value);
end;

function TVkParamsMessageHistory.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TParamGet }

function TVkParamsMessageGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMessageGet.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsMessageGet.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageGet.MessageId(Value: Integer): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageGet.MessageIds(Value: TIds): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageGet.PreviewLength(Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

{ TVkParamsMessageDelete }

function TVkParamsMessageDelete.DeleteForAll(Value: Boolean): Integer;
begin
  Result := List.Add('delete_for_all', Value);
end;

function TVkParamsMessageDelete.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageDelete.MessageId(Value: Integer): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageDelete.MessageIds(Value: TIds): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageDelete.Spam(Value: Boolean): Integer;
begin
  Result := List.Add('spam', Value);
end;

end.

