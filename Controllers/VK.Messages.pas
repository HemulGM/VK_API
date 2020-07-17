unit VK.Messages;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, REST.Client, System.Json, VK.Controller, VK.Types,
  VK.Handler, VK.Entity.Keyboard, VK.Entity.Message, VK.Entity.Conversation, VK.Entity.User, VK.Entity.Group,
  VK.Entity.Message.Chat;

type

  /// <summary>
  /// <b>miPromoNewsletter</b> - интент, который должен сопровождать рекламную рассылку для ботов.
  /// <b>miBotAdInvite</b> - интент, который должен сопровождать сообщения, запрашивающее подтверждение пользователя на отправку этому пользователю рекламы.
  /// <b>miBotAdPromo</b> - интент, который должен сопровождать сообщение содержащее рекламу от бота.
  /// </summary>
  TVkMessageIntent = (miDefault, miPromoNewsletter, miBotAdInvite, miBotAdPromo);

  TVkMessageIntentHelper = record helper for TVkMessageIntent
    function ToString: string; inline;
  end;

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
    /// <summary>
    /// ///  Интент — это метка, которая обозначает приблизительное содержание сообщения от сообщества. Передаётся в необязательном параметре messages.send — Intent.
    /// </summary>
    function Intent(Value: TVkMessageIntent): TVkMessageNew;
    function Keyboard(Value: TVkKeyboardConstructor): TVkMessageNew;
    function DontParseLinks(Value: Boolean): TVkMessageNew;
    function DisableMentions(Value: Boolean): TVkMessageNew;
    function StickerId(Id: Integer): TVkMessageNew;
    function SubscribeId(Value: Integer): TVkMessageNew;
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

  TVkParamsMessageSend = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function ChatId(Value: Integer): Integer;
    function Domain(Value: string): Integer;
    function PeerId(Value: Integer): Integer;
    function RandomId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Lat(Value: Extended): Integer;
    function Long(Value: Extended): Integer;
    function Attachment(Value: TAttachmentArray): Integer; overload;
    function Attachment(Value: string): Integer; overload;
    function ReplyTo(Value: Integer): Integer;
    function ForwardMessages(Value: TIds): Integer;
    function StickerId(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
    function Keyboard(Value: TVkKeyboardConstructor): Integer;
    function Payload(Value: string): Integer;
    function Intent(Value: TVkMessageIntent): Integer;
    function DontParseLinks(Value: Boolean): Integer;
    function DisableMentions(Value: Boolean): Integer;
    function SubscribeId(Value: Integer): Integer;
  end;

  TVkParamsMessageSendIds = record
    List: TParams;
    function Domain(Value: string): Integer;
    function UserIds(Value: TIds): Integer;
    function RandomId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Lat(Value: Extended): Integer;
    function Long(Value: Extended): Integer;
    function Attachment(Value: TAttachmentArray): Integer; overload;
    function Attachment(Value: string): Integer; overload;
    function ReplyTo(Value: Integer): Integer;
    function ForwardMessages(Value: TIds): Integer;
    function StickerId(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
    function Keyboard(Value: TVkKeyboardConstructor): Integer;
    function Payload(Value: string): Integer;
    function Intent(Value: TVkMessageIntent): Integer;
    function DontParseLinks(Value: Boolean): Integer;
    function DisableMentions(Value: Boolean): Integer;
    function SubscribeId(Value: Integer): Integer;
  end;

  TVkParamsMessageDeleteConversation = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function PeerId(Value: Integer): Integer;
    function UserId(Value: Integer): Integer;
  end;

  TVkParamsMessageEdit = record
    List: TParams;
    function MessageId(Value: Integer): Integer;
    function ConversationMessageId(Value: Integer): Integer;
    function PeerId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Lat(Value: Extended): Integer;
    function Long(Value: Extended): Integer;
    function LatLong(Lat, Long: Extended): Integer;
    function Attachment(Value: TAttachmentArray): Integer; overload;
    function Attachment(Value: string): Integer; overload;
    function KeepForwardMessages(Value: Boolean): Integer;
    function KeepSnippets(Value: Boolean): Integer;
    function GroupId(Value: Integer): Integer;
    function DontParseLinks(Value: Boolean): Integer;
    function Template(Value: string): Integer;
    function Keyboard(Value: TVkKeyboardConstructor): Integer;
  end;

  TVkParamsMessageGetByConvMesId = record
    List: TParams;
    function PeerId(Value: Integer): Integer;
    function ConversationMessageIds(Value: TIds): Integer; overload;
    function ConversationMessageIds(Value: Integer): Integer; overload;
    function Extended(Value: Boolean): Integer;
    function Fields(GroupFields: TVkGroupFields = []; UserFields: TVkUserFields = []): Integer;
    function GroupId(Value: Integer): Integer;
  end;

  TVkParamsMessageGetChat = record
    List: TParams;
    function ChatId(Value: Integer): Integer;
    function ChatIds(Value: TIds): Integer;
    function Fields(Value: TVkUserFields = []): Integer;
    function NameCase(Value: TVkNameCase): Integer;
  end;

  TMessagesController = class(TVkController)
  public
    /// <summary>
    /// Отправить сообщение.
    /// Для пользователя: id пользователя.
    /// Для групповой беседы: 2000000000 + id беседы.
    /// Для сообщества: -id сообщества.
    /// </summary>
    function SendToPeer(var Item: Integer; PeerId: Integer; Message: string; Attachments: TAttachmentArray = []):
      Boolean; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    function Send(var Item: Integer; UserId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    function Send(var Item: Integer; Domain: string; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Отправить сообщение в беседу
    /// </summary>
    function SendToChat(var Item: Integer; ChatId: Integer; Message: string; Attachments: TAttachmentArray = []):
      Boolean; overload;
    /// <summary>
    /// Отправить сообщение нескольким пользователям (Доступно только для ключа доступа сообщества)
    /// </summary>
    function Send(var Items: TVkMessageSendResponses; UserIds: TUserIds; Message: string; Attachments: TAttachmentArray
      = []): Boolean; overload;
    function Send(var Items: TVkMessageSendResponses; Params: TVkParamsMessageSendIds): Boolean; overload;
    function Send(var Item: Integer; Params: TVkParamsMessageSend): Boolean; overload;
    /// <summary>
    /// Универсальный метод отправки сообщений (Fluent Method)
    /// New.PeerId(123456).ReplyTo(12345)...Message('Текст').Send.Free;
    /// </summary>
    function New: TVkMessageNew; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkMessages; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkMessages; Params: TVkParamsMessageGet): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkMessages; Ids: TIds; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkMessages; Id: Integer; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Добавляет в мультидиалог нового пользователя.
    /// </summary>
    function AddChatUser(ChatId: Integer; UserId: Integer = -1; VisibleMessagesCount: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет сообщения
    /// </summary>
    function Delete(var Items: TVkMessageDelete; MessageIds: TIds; GroupID: Integer = 0; DeleteForAll: Boolean = False;
      Spam: Boolean = False): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    function Delete(MessageId: Integer; GroupID: Integer = 0; DeleteForAll: Boolean = False; Spam: Boolean = False):
      Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    function Delete(var Items: TVkMessageDelete; Params: TVkParamsMessageDelete): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    function Delete(var Items: TVkMessageDelete; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает ссылку для приглашения пользователя в беседу.
    /// Только создатель беседы имеет доступ к ссылке на беседу.
    /// </summary>
    function GetInviteLink(var Link: string; PeerId: Integer; Reset: Boolean = False; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список бесед пользователя.
    /// </summary>
    function GetConversations(var Conversations: TVkConversationItems; Params: TVkParamsConversationsGet): Boolean;
    /// <summary>
    /// Возвращает историю сообщений для указанного диалога.
    /// </summary>
    function GetHistory(var History: TVkMessageHistory; Params: TVkParamsMessageHistory): Boolean; overload;
    /// <summary>
    /// Позволяет разрешить отправку сообщений от сообщества текущему пользователю.
    /// </summary>
    function AllowMessagesFromGroup(GroupId: Integer; Key: string): Boolean;
    /// <summary>
    /// Создаёт беседу с несколькими участниками.
    /// </summary>
    function CreateChat(var ChatId: Integer; UserIds: TIds; Title: string; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Позволяет удалить фотографию мультидиалога.
    /// </summary>
    function DeleteChatPhoto(var Item: TVkChatInfoMessage; ChatId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет беседу.
    /// </summary>
    function DeleteConversation(var LastDeletedId: Integer; Params: TVkParamsMessageDeleteConversation): Boolean;
    /// <summary>
    /// Позволяет запретить отправку сообщений от сообщества текущему пользователю.
    /// </summary>
    function DenyMessagesFromGroup(GroupId: Integer): Boolean;
    /// <summary>
    /// Редактирует сообщение.
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует сообщение.
    /// </summary>
    function Edit(Params: TVkParamsMessageEdit): Boolean; overload;
    /// <summary>
    /// Изменяет название беседы.
    /// </summary>
    function EditChat(ChatId: Integer; Title: string): Boolean;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам в рамках беседы.
    /// </summary>
    function GetByConversationMessageId(var Items: TVkMessages; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам в рамках беседы.
    /// </summary>
    function GetByConversationMessageId(var Items: TVkMessages; Params: TVkParamsMessageGetByConvMesId): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о беседе.
    /// </summary>
    function GetChat(var Items: TVkChats; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о беседе.
    /// </summary>
    function GetChat(var Items: TVkChats; Params: TVkParamsMessageGetChat): Boolean; overload;
    /// <summary>
    /// Получает данные для превью чата с приглашением по ссылке.
    /// </summary>
    function GetChatPreview(var Item: TVkChatPreview; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает данные для превью чата с приглашением по ссылке.
    /// </summary>
    function GetChatPreview(var Item: TVkChatPreview; PeerId: Integer; Link: string; Fields: TVkUserFields): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TMessagesController }

function TMessagesController.SendToPeer(var Item: Integer; PeerId: Integer; Message: string; Attachments:
  TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.PeerId(PeerId);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Attachment(Attachments);

  Result := Send(Item, Params);
end;

function TMessagesController.AddChatUser(ChatId: Integer; UserId: Integer; VisibleMessagesCount: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('chat_id', ChatId);
  if UserId >= 0 then
    Params.Add('user_id', UserId);
  if VisibleMessagesCount > 0 then
    Params.Add('visible_messages_count', VisibleMessagesCount);

  with Handler.Execute('messages.addChatUser', Params) do
  begin
    Result := Success and (Response = '1');
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

function TMessagesController.Delete(MessageId, GroupID: Integer; DeleteForAll, Spam: Boolean): Boolean;
var
  Items: TVkMessageDelete;
begin
  Result := Delete(Items, [MessageId], GroupID, DeleteForAll, Spam) and Items.Items[MessageId.ToString];
end;

function TMessagesController.AllowMessagesFromGroup(GroupId: Integer; Key: string): Boolean;
begin
  with Handler.Execute('messages.allowMessagesFromGroup', [['group_id', GroupId.ToString], ['key', Key]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TMessagesController.CreateChat(var ChatId: Integer; UserIds: TIds; Title: string; GroupId: Integer): Boolean;
begin
  with Handler.Execute('messages.createChat', [['user_ids', UserIds.ToString], ['title', Title], ['group_id', GroupId.ToString]])
    do
  begin
    Result := Success and TryStrToInt(Response, ChatId);
  end;
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

function TMessagesController.DeleteChatPhoto(var Item: TVkChatInfoMessage; ChatId, GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('chat_id', ChatId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('messages.deleteChatPhoto', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkChatInfoMessage.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.DeleteConversation(var LastDeletedId: Integer; Params: TVkParamsMessageDeleteConversation): Boolean;
var
  RespJSON: TJSONValue;
begin
  with Handler.Execute('messages.deleteConversation', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      RespJSON := TJSONObject.ParseJSONValue(Response);
      try
        LastDeletedId := RespJSON.GetValue<Integer>('last_deleted_id', -1);
        Result := LastDeletedId <> -1;
      finally
        RespJSON.Free;
      end;
    end;
  end;
end;

function TMessagesController.DenyMessagesFromGroup(GroupId: Integer): Boolean;
begin
  with Handler.Execute('messages.denyMessagesFromGroup', ['group_id', GroupId.ToString]) do
    Result := Success and (Response = '1');
end;

function TMessagesController.Edit(Params: TVkParamsMessageEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TMessagesController.EditChat(ChatId: Integer; Title: string): Boolean;
begin
  with Handler.Execute('messages.editChat', [['', ChatId.ToString], ['title', Title]]) do
    Result := Success and (Response = '1');
end;

function TMessagesController.Edit(Params: TParams): Boolean;
begin
  with Handler.Execute('messages.edit', Params) do
    Result := Success and (Response = '1');
end;

function TMessagesController.Delete(var Items: TVkMessageDelete; Params: TVkParamsMessageDelete): Boolean;
begin
  Result := Delete(Items, Params.List);
end;

function TMessagesController.GetByConversationMessageId(var Items: TVkMessages; Params: TParams): Boolean;
begin
  with Handler.Execute('messages.getByConversationMessageId', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkMessages.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.GetByConversationMessageId(var Items: TVkMessages; Params: TVkParamsMessageGetByConvMesId): Boolean;
begin
  Result := GetByConversationMessageId(Items, Params.List);
end;

function TMessagesController.GetById(var Items: TVkMessages; Params: TParams): Boolean;
begin
  with Handler.Execute('messages.getById', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkMessages.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.GetById(var Items: TVkMessages; Ids: TIds; PreviewLength, GroupId: Integer): Boolean;
var
  Params: TVkParamsMessageGet;
begin
  Params.MessageIds(Ids);
  if PreviewLength > 0 then
    Params.PreviewLength(PreviewLength);
  if GroupId > 0 then
    Params.GroupId(GroupId);
  Result := GetById(Items, Params);
end;

function TMessagesController.GetById(var Items: TVkMessages; Id, PreviewLength, GroupId: Integer): Boolean;
begin
  Result := GetById(Items, [Id], PreviewLength, GroupId);
end;

function TMessagesController.GetById(var Items: TVkMessages; Params: TVkParamsMessageGet): Boolean;
begin
  Result := GetById(Items, Params.List);
end;

function TMessagesController.GetChat(var Items: TVkChats; Params: TParams): Boolean;
begin
  if not Params.KeyExists('fields') then
    Params.Add('fields', TVkUserField.ufDomain.ToString);
  with Handler.Execute('messages.getChat', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkChats.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.GetChat(var Items: TVkChats; Params: TVkParamsMessageGetChat): Boolean;
begin
  Result := GetChat(Items, Params.List);
end;

function TMessagesController.GetChatPreview(var Item: TVkChatPreview; PeerId: Integer; Link: string; Fields:
  TVkUserFields): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  Params.Add('link', Link);
  if Fields <> [] then
    Params.Add('fields', Fields.ToString);
  Result := GetChatPreview(Item, Params);
end;

function TMessagesController.GetChatPreview(var Item: TVkChatPreview; Params: TParams): Boolean;
begin
  with Handler.Execute('messages.getChatPreview', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkChatPreview.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.GetConversations(var Conversations: TVkConversationItems; Params: TVkParamsConversationsGet):
  Boolean;
begin
  with Handler.Execute('messages.getConversations', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Conversations := TVkConversationItems.FromJsonString(Response);
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

function TMessagesController.Send(var Item: Integer; Params: TVkParamsMessageSend): Boolean;
begin
  with Handler.Execute('messages.send', Params.List) do
  begin
    Result := Success and TryStrToInt(Response, Item);
  end;
end;

function TMessagesController.Send(var Items: TVkMessageSendResponses; Params: TVkParamsMessageSendIds): Boolean;
begin
  with Handler.Execute('messages.send', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkMessageSendResponses.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.SendToChat(var Item: Integer; ChatId: Integer; Message: string; Attachments:
  TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.ChatId(ChatId);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Attachment(Attachments);

  Result := Send(Item, Params);
end;

function TMessagesController.Send(var Item: Integer; Domain: string; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.Domain(Domain);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Attachment(Attachments);

  Result := Send(Item, Params);
end;

function TMessagesController.Send(var Items: TVkMessageSendResponses; UserIds: TUserIds; Message: string; Attachments:
  TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSendIds;
begin
  Params.UserIds(UserIds);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Attachment(Attachments);

  Result := Send(Items, Params);
end;

function TMessagesController.Send(var Item: Integer; UserId: Integer; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.UserId(UserId);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Attachment(Attachments);

  Result := Send(Item, Params);
end;

{ TVkMessageNew }

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

function TVkMessageNew.SubscribeId(Value: Integer): TVkMessageNew;
begin
  Params.Add('subscribe_id', Value);
  Result := Self;
end;

function TVkMessageNew.ForwardMessages(Ids: TArrayOfInteger): TVkMessageNew;
begin
  Params.Add('forward_messages', Ids);
  Result := Self;
end;

function TVkMessageNew.GroupID(Id: Integer): TVkMessageNew;
begin
  Params.Add('group_id', Id);
  Result := Self;
end;

function TVkMessageNew.Intent(Value: TVkMessageIntent): TVkMessageNew;
begin
  Params.Add('intent', Value.ToString);
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

{ TVkParamsConversationsGet }

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

{ TVkParamsMessageHistory }

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

{ TVkParamsMessageGet }

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

{ TVkMessageIntentHelper }

function TVkMessageIntentHelper.ToString: string;
begin
  case Self of
    miDefault:
      Result := 'default';
    miPromoNewsletter:
      Result := 'promo_newsletter';
    miBotAdInvite:
      Result := 'bot_ad_invite';
    miBotAdPromo:
      Result := 'bot_ad_promo';
  end;
end;

{ TVkParamsMessageSend }

function TVkParamsMessageSend.Attachment(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageSend.Attachment(Value: string): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageSend.ChatId(Value: Integer): Integer;
begin
  Result := List.Add('chat_id', Value);
end;

function TVkParamsMessageSend.DisableMentions(Value: Boolean): Integer;
begin
  Result := List.Add('disable_mentions', Value);
end;

function TVkParamsMessageSend.Domain(Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsMessageSend.DontParseLinks(Value: Boolean): Integer;
begin
  Result := List.Add('dont_parse_links', Value);
end;

function TVkParamsMessageSend.ForwardMessages(Value: TIds): Integer;
begin
  Result := List.Add('forward_messages', Value);
end;

function TVkParamsMessageSend.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageSend.Intent(Value: TVkMessageIntent): Integer;
begin
  Result := List.Add('intent', Value.ToString);
end;

function TVkParamsMessageSend.Keyboard(Value: TVkKeyboardConstructor): Integer;
begin
  Result := List.Add('keyboard', Value.ToJsonString);
end;

function TVkParamsMessageSend.Lat(Value: Extended): Integer;
begin
  Result := List.Add('lat', Value);
end;

function TVkParamsMessageSend.Long(Value: Extended): Integer;
begin
  Result := List.Add('long', Value);
end;

function TVkParamsMessageSend.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMessageSend.Payload(Value: string): Integer;
begin
  Result := List.Add('payload', Value);
end;

function TVkParamsMessageSend.PeerId(Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageSend.RandomId(Value: Integer): Integer;
begin
  Result := List.Add('random_id', Value);
end;

function TVkParamsMessageSend.ReplyTo(Value: Integer): Integer;
begin
  Result := List.Add('reply_to', Value);
end;

function TVkParamsMessageSend.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsMessageSend.SubscribeId(Value: Integer): Integer;
begin
  Result := List.Add('subscribe_id', Value);
end;

function TVkParamsMessageSend.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsMessageSendIds }

function TVkParamsMessageSendIds.Attachment(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageSendIds.Attachment(Value: string): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageSendIds.DisableMentions(Value: Boolean): Integer;
begin
  Result := List.Add('disable_mentions', Value);
end;

function TVkParamsMessageSendIds.Domain(Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsMessageSendIds.DontParseLinks(Value: Boolean): Integer;
begin
  Result := List.Add('dont_parse_links', Value);
end;

function TVkParamsMessageSendIds.ForwardMessages(Value: TIds): Integer;
begin
  Result := List.Add('forward_messages', Value);
end;

function TVkParamsMessageSendIds.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageSendIds.Intent(Value: TVkMessageIntent): Integer;
begin
  Result := List.Add('intent', Value.ToString);
end;

function TVkParamsMessageSendIds.Keyboard(Value: TVkKeyboardConstructor): Integer;
begin
  Result := List.Add('keyboard', Value.ToJsonString);
end;

function TVkParamsMessageSendIds.Lat(Value: Extended): Integer;
begin
  Result := List.Add('lat', Value);
end;

function TVkParamsMessageSendIds.Long(Value: Extended): Integer;
begin
  Result := List.Add('long', Value);
end;

function TVkParamsMessageSendIds.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMessageSendIds.Payload(Value: string): Integer;
begin
  Result := List.Add('payload', Value);
end;

function TVkParamsMessageSendIds.RandomId(Value: Integer): Integer;
begin
  Result := List.Add('random_id', Value);
end;

function TVkParamsMessageSendIds.ReplyTo(Value: Integer): Integer;
begin
  Result := List.Add('reply_to', Value);
end;

function TVkParamsMessageSendIds.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsMessageSendIds.SubscribeId(Value: Integer): Integer;
begin
  Result := List.Add('subscribe_id', Value);
end;

function TVkParamsMessageSendIds.UserIds(Value: TIds): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsMessageDeleteConversation }

function TVkParamsMessageDeleteConversation.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageDeleteConversation.PeerId(Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageDeleteConversation.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsMessageEdit }

function TVkParamsMessageEdit.Attachment(Value: string): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageEdit.Attachment(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageEdit.ConversationMessageId(Value: Integer): Integer;
begin
  Result := List.Add('conversation_message_id', Value);
end;

function TVkParamsMessageEdit.DontParseLinks(Value: Boolean): Integer;
begin
  Result := List.Add('dont_parse_links', Value);
end;

function TVkParamsMessageEdit.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageEdit.KeepForwardMessages(Value: Boolean): Integer;
begin
  Result := List.Add('keep_forward_messages', Value);
end;

function TVkParamsMessageEdit.KeepSnippets(Value: Boolean): Integer;
begin
  Result := List.Add('keep_snippets', Value);
end;

function TVkParamsMessageEdit.Keyboard(Value: TVkKeyboardConstructor): Integer;
begin
  Result := List.Add('keyboard', Value.ToJsonString);
end;

function TVkParamsMessageEdit.Lat(Value: Extended): Integer;
begin
  Result := List.Add('lat', Value);
end;

function TVkParamsMessageEdit.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsMessageEdit.Long(Value: Extended): Integer;
begin
  Result := List.Add('long', Value);
end;

function TVkParamsMessageEdit.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMessageEdit.MessageId(Value: Integer): Integer;
begin
  Result := List.Add('message_id', Value);
end;

function TVkParamsMessageEdit.PeerId(Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageEdit.Template(Value: string): Integer;
begin
  Result := List.Add('template', Value);
end;

{ TVkParamsMessageGetByConvMesId }

function TVkParamsMessageGetByConvMesId.ConversationMessageIds(Value: TIds): Integer;
begin
  Result := List.Add('conversation_message_ids', Value);
end;

function TVkParamsMessageGetByConvMesId.ConversationMessageIds(Value: Integer): Integer;
begin
  Result := List.Add('conversation_message_ids', Value);
end;

function TVkParamsMessageGetByConvMesId.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMessageGetByConvMesId.Fields(GroupFields: TVkGroupFields; UserFields: TVkUserFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsMessageGetByConvMesId.GroupID(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageGetByConvMesId.PeerId(Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

{ TVkParamsMessageGetChat }

function TVkParamsMessageGetChat.ChatId(Value: Integer): Integer;
begin
  Result := List.Add('chat_ids', Value);
end;

function TVkParamsMessageGetChat.ChatIds(Value: TIds): Integer;
begin
  Result := List.Add('chat_ids', Value);
end;

function TVkParamsMessageGetChat.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsMessageGetChat.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

end.

