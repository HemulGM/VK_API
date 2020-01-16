unit VK.Messages;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, System.Json, VK.Controller, VK.Types,
  VK.Structs, VK.Handler, VK.Entity.Keyboard, VK.Entity.Message;

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
    function Keyboard(Value: TVkKeyboard): TNewMessage;
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
    function Send: TNewMessage; overload;
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
      Result := Value.ToInteger;
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
      Result := Value.ToInteger;
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
      Result := Value.ToInteger;
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
      Result := TVkMessageSendResponses.FromJsonString(Value);
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
      Result := Value.ToInteger;
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
  Response: Integer;
begin
  FParams.Add('random_id', GetRandomId.ToString);
  with Handler.Execute('messages.send', Params) do
  begin
    if not Success then
      Exit(TVkMessageSendResponses.CreateFalse)
    else
    begin
      if TryStrToInt(Value, Response) then
        Result := TVkMessageSendResponses.CreateTrue(Response)
      else
        Result := TVkMessageSendResponses.FromJsonString(Value);
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

function TNewMessage.Keyboard(Value: TVkKeyboard): TNewMessage;
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

