unit VK.Messages;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, System.Json, VK.Controller, VK.Types,
  VK.Handler, VK.Entity.Keyboard, VK.Entity.Message;

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
    function Attachemt(Attachemts: TAttachmentArray): TNewMessage;
    function Send: TVkMessageSendResponses;
    constructor Create(Controller: TMessagesController);
    property Handler: TVkHandler read FHandler;
    property Params: TParams read FParams write SetParams;
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
    function Send(PeerId: Integer; Message: string; Attachemts: TAttachmentArray = []): Integer; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    /// <param name="UserId">Ид пользователя</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToUser(UserId: Integer; Message: string; Attachemts: TAttachmentArray = []): Integer; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    /// <param name="UserDomain">Короткий адрес пользователя (например, illarionov)</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToUser(UserDomain: string; Message: string; Attachemts: TAttachmentArray = []): Integer; overload;
    /// <summary>
    /// Отправить сообщение в беседу
    /// </summary>
    /// <param name="ChatId">Ид беседы</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToChat(ChatId: Integer; Message: string; Attachemts: TAttachmentArray = []): Integer; overload;
    /// <summary>
    /// Отправить сообщение нескольким пользователям (Доступно только для ключа доступа сообщества)
    /// </summary>
    /// <param name="UserIds">Ид пользователей</param>
    /// <param name="Message">Текст сообщения</param>
    /// <param name="Attachemts">Вложения. Массив идентификторов вида: [type][owner_id]_[media_id]_[access_key, если есть]. Например, video85635407_165186811_69dff3de4372ae9b6e </param>
    function SendToUsers(UserIds: TUserIds; Message: string; Attachemts: TAttachmentArray = []):
      TVkMessageSendResponses; overload;
    /// <summary>
    /// Универсальный метод отправки сообщений (Fluent Method)
    /// Send.PeerId(123456).ReplyTo(12345)...Message('Текст').Send.Free;
    /// </summary>
    /// <returns>Возвращает конструктор сообщений. Метод Send в этом конструкторе, возвращает результат в виде класса</returns>
    function Send: TNewMessage; overload;
    function GetById(Ids: TIds; var Messages: TVkMessages; PreviewLength: Integer = 0; GroupId:
      Integer = 0): Boolean; overload;
    function GetById(Id: Integer; var Message: TVkMessage; PreviewLength: Integer = 0; GroupId:
      Integer = 0): Boolean; overload;
    function AddChatUser(ChatId, UserId: Integer; VisibleMessagesCount: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API, VK.Utils;

{ TMessagesController }

function TMessagesController.Send(PeerId: Integer; Message: string; Attachemts: TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add(['peer_id', PeerId.ToString]);
  Params.Add(['message', Message]);
  Params.Add(['random_id', GetRandomId.ToString]);
  if Length(Attachemts) <> 0 then
    Params.Add(['attachment', Attachemts.ToString]);

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

function TMessagesController.GetById(Id: Integer; var Message: TVkMessage; PreviewLength, GroupId: Integer): Boolean;
var
  Params: TParams;
  Items: TJSONArray;
  RespJSON: TJSONValue;
begin
  Params.Add(['message_ids', Id.ToString]);
  if PreviewLength > 0 then
    Params.Add(['preview_length', PreviewLength.ToString]);
  if GroupId > 0 then
    Params.Add(['group_id', GroupId.ToString]);
  Message := nil;
  with Handler.Execute('messages.getById', Params) do
  begin
    if not Success then
      Exit(False)
    else
    begin
      RespJSON := TJSONObject.ParseJSONValue(Response);
      try
        if RespJSON.TryGetValue<TJSONArray>('items', Items) then
        begin
          Result := True;
          Message := TVkMessage.FromJsonString(Items.Items[0].ToString);
        end
        else
          Result := False;
      finally
        RespJSON.Free;
      end;
    end;
  end;
end;

function TMessagesController.GetById(Ids: TIds; var Messages: TVkMessages; PreviewLength, GroupId: Integer): Boolean;
var
  Params: TParams;
  Items: TJSONArray;
  RespJSON: TJSONValue;
  i: Integer;
begin
  Params.Add(['message_ids', Ids.ToString]);
  if PreviewLength > 0 then
    Params.Add(['preview_length', PreviewLength.ToString]);
  if GroupId > 0 then
    Params.Add(['group_id', GroupId.ToString]);
  with Handler.Execute('messages.getById', Params) do
  begin
    if not Success then
      Exit(False)
    else
    begin
      RespJSON := TJSONObject.ParseJSONValue(Response);
      try
        if RespJSON.TryGetValue<TJSONArray>('items', Items) then
        begin
          Result := True;
          SetLength(Messages, Items.Count);
          for i := 0 to Items.Count - 1 do
            Messages[i] := TVkMessage.FromJsonString(Items.Items[i].ToString);
        end
        else
          Result := False;
      finally
        RespJSON.Free;
      end;
    end;
  end;
end;

function TMessagesController.Send: TNewMessage;
begin
  Result := TNewMessage.Create(Self);
end;

function TMessagesController.SendToChat(ChatId: Integer; Message: string; Attachemts: TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add(['chat_id', ChatId.ToString]);
  Params.Add(['message', Message]);
  Params.Add(['random_id', GetRandomId.ToString]);
  if Length(Attachemts) <> 0 then
    Params.Add(['attachment', Attachemts.ToString]);

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

function TMessagesController.SendToUser(UserDomain, Message: string; Attachemts: TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add(['domian', UserDomain]);
  Params.Add(['message', Message]);
  Params.Add(['random_id', GetRandomId.ToString]);
  if Length(Attachemts) <> 0 then
    Params.Add(['attachment', Attachemts.ToString]);

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

function TMessagesController.SendToUsers(UserIds: TUserIds; Message: string; Attachemts:
  TAttachmentArray): TVkMessageSendResponses;
var
  Params: TParams;
begin
  Params.Add(['user_ids', UserIds.ToString]);
  Params.Add(['message', Message]);
  Params.Add(['random_id', GetRandomId.ToString]);
  if Length(Attachemts) <> 0 then
    Params.Add(['attachment', Attachemts.ToString]);

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

function TMessagesController.SendToUser(UserId: Integer; Message: string; Attachemts: TAttachmentArray): Integer;
var
  Params: TParams;
begin
  Params.Add(['user_id', UserId.ToString]);
  Params.Add(['message', Message]);
  Params.Add(['random_id', GetRandomId.ToString]);
  if Length(Attachemts) <> 0 then
    Params.Add(['attachment', Attachemts.ToString]);

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
  FParams.Add('random_id', GetRandomId.ToString);
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
  Params.Add('disable_mentions', BoolToString(Value));
  Result := Self;
end;

function TNewMessage.DontParseLinks(Value: Boolean): TNewMessage;
begin
  Params.Add('dont_parse_links', BoolToString(Value));
  Result := Self;
end;

function TNewMessage.StickerId(Id: Integer): TNewMessage;
begin
  Params.Add('sticker_id', Id.ToString);
  Result := Self;
end;

function TNewMessage.ForwardMessages(Ids: TArrayOfInteger): TNewMessage;
begin
  Params.Add('forward_messages', Ids.ToString);
  Result := Self;
end;

function TNewMessage.GroupId(Id: Integer): TNewMessage;
begin
  Params.Add('group_id', Id.ToString);
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

function TNewMessage.Attachemt(Attachemts: TAttachmentArray): TNewMessage;
begin
  Params.Add('attachment', Attachemts.ToString);
  Result := Self;
end;

function TNewMessage.ChatId(Id: Integer): TNewMessage;
begin
  Params.Add('chat_id', Id.ToString);
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
  Params.Add('peer_id', Id.ToString);
  Result := Self;
end;

function TNewMessage.ReplyTo(Id: Integer): TNewMessage;
begin
  Params.Add('reply_to', Id.ToString);
  Result := Self;
end;

function TNewMessage.UserDomian(Domian: string): TNewMessage;
begin
  Params.Add('domian', Domian);
  Result := Self;
end;

function TNewMessage.UserId(Id: Integer): TNewMessage;
begin
  Params.Add('user_id', Id.ToString);
  Result := Self;
end;

function TNewMessage.UserIds(Ids: TUserIds): TNewMessage;
begin
  Params.Add('user_ids', Ids.ToString);
  Result := Self;
end;

end.

