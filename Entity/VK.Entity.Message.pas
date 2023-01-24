unit VK.Entity.Message;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, VK.Entity.Common,
  VK.Entity.Media, VK.Types, VK.Entity.Keyboard, VK.Entity.ClientInfo,
  VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common.List,
  VK.Entity.Common.ExtendedList, VK.Wrap.Interceptors, VK.Entity.Geo;

type
  TVkLastActivity = class(TVkEntity)
  private
    FOnline: Boolean;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FTime: TDateTime;
  public
    property Online: Boolean read FOnline write FOnline;
    property Time: TDateTime read FTime write FTime;
  end;

  TVkMessageForward = record
  private
    FConversation_message_ids: TArray<Integer>;
    FIs_reply: Boolean;
    FMessage_ids: TArray<Integer>;
    FOwner_id: TVkPeerId;
    FPeer_id: TVkPeerId;
  public
    /// <summary>
    /// Идентификатор места, из которого необходимо переслать сообщения
    /// </summary>
    property PeerId: TVkPeerId read FPeer_id write FPeer_id;
    /// <summary>
    /// Владелец сообщений. Стоит передавать, если вы хотите переслать сообщения из сообщества в диалог
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    /// <summary>
    /// Массив ConversationMessageId сообщений, которые необходимо переслать. В массив ConversationMessageIds можно передать сообщения:
    /// - находящиеся в личном диалоге с ботом;
    /// - являющиеся исходящими сообщениями бота;
    /// - написанными после того, как бот вступил в беседу и появился доступ к сообщениям
    /// </summary>
    property ConversationMessageIds: TArray<Integer> read FConversation_message_ids write FConversation_message_ids;
    /// <summary>
    /// Массив id сообщений
    /// </summary>
    property MessageIds: TArray<Integer> read FMessage_ids write FMessage_ids;
    /// <summary>
    /// Ответ на сообщения. Стоит передавать, если вы хотите ответить на сообщения в том чате, в котором находятся сообщения.
    /// При этом в ConversationMessageIds/MessageIds должен находиться только один элемент
    /// </summary>
    property IsReply: Boolean read FIs_reply write FIs_reply;
    function ToJSON: string;
  end;

  TVkMessageSendResponse = class(TVkEntity)
  private
    FMessage_id: Integer;
    FPeer_id: TVkPeerId;
    FError: string;
  public
    /// <summary>
    /// Идентификатор назначения
    /// </summary>
    property PeerId: TVkPeerId read FPeer_id write FPeer_id;
    /// <summary>
    /// Идентификатор сообщения
    /// </summary>
    property MessageId: Integer read FMessage_id write FMessage_id;
    /// <summary>
    /// Сообщение об ошибке, если сообщение не было доставлено получателю
    /// </summary>
    property Error: string read FError write FError;
  end;

  TVkMessageSendResponses = class(TVkEntityList<TVkMessageSendResponse>)
  private
    FSuccess: Boolean;
    FResponse: Integer;
    procedure SetSuccess(const Value: Boolean);
    procedure SetResponse(const Value: Integer);
  public
    property Success: Boolean read FSuccess write SetSuccess;
    property Response: Integer read Fresponse write SetResponse;
    constructor CreateFalse;
    constructor CreateTrue(ARespone: Integer);
    class function FromJsonString(AJsonString: string): TVkMessageSendResponses; static;
  end;

  TVkPayloadButton = class(TVkEntity)
  private
    FButton: string;
  public
    property Button: string read FButton write FButton;
  end;

  TVkMessageAction = class(TVkEntity)
  private
    FText: string;
    [JsonReflectAttribute(ctString, rtString, TMessageActionTypeInterceptor)]
    FType: TVkMessageActionType;
    FEmail: string;
    FMember_id: TVkPeerId;
    FPhoto: TVkChatPhoto;
    FStyle: string;
    FMessage: string;
    FConversation_message_id: Int64;
  public
    /// <summary>
    /// Название беседы (для служебных сообщений с type = chat_create или chat_title_update)
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Тип действия
    /// </summary>
    property &Type: TVkMessageActionType read FType write FType;
    /// <summary>
    /// Идентификатор пользователя (если > 0) или email (если < 0), которого пригласили или исключили
    /// (для служебных сообщений с type = chat_invite_user или chat_kick_user).
    /// Идентификатор пользователя, который закрепил/открепил сообщение для action = chat_pin_message или chat_unpin_message
    /// </summary>
    property MemberId: TVkPeerId read FMember_id write FMember_id;
    /// <summary>
    /// Email, который пригласили или исключили (для служебных сообщений с type = chat_invite_user или chat_kick_user и отрицательным member_id)
    /// </summary>
    property Email: string read FEmail write FEmail;
    /// <summary>
    /// Изображение-обложка чата
    /// </summary>
    property Photo: TVkChatPhoto read FPhoto write FPhoto;
    /// <summary>
    /// Стиль чата
    /// </summary>
    property Style: string read FStyle write FStyle;
    /// <summary>
    /// Частичный текст сообщения (прикрепленного)
    /// </summary>
    property Message: string read FMessage write FMessage;
    /// <summary>
    /// Ид сообщения
    /// </summary>
    property ConversationMessageId: Int64 read FConversation_message_id write FConversation_message_id;
    destructor Destroy; override;
  end;

  TVkMessageContentSource = record
  private
    FConversation_message_id: Integer;
    FType: string;
    FOwner_id: TVkPeerId;
    FPeer_id: TVkPeerId;
    FUrl: string;
  public
    property &Type: string read FType write FType; // 'message', 'url'
    property PeerId: TVkPeerId read FPeer_id write FPeer_id;
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property ConversationMessageId: Integer read FConversation_message_id write FConversation_message_id;
    property Url: string read FUrl write FUrl;
    function ToJSON: string;
  end;

  TVkMessage = class(TVkObject)
  private
    FAttachments: TVkAttachmentArray;
    FConversation_message_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_id: TVkPeerId;
    FFwd_messages: TArray<TVkMessage>;
    FImportant: Boolean;
    FIs_hidden: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FOut: Boolean;
    FPeer_id: TVkPeerId;
    FRandom_id: Integer;
    FText: string;
    FRef: string;
    FRef_source: string;
    FGeo: TVkGeo;
    FPayload: string;
    FPayloadButton: TVkPayloadButton;
    FKeyboard: TVkKeyboard;
    FReply_message: TVkMessage;
    FAction: TVkMessageAction;
    FWas_listened: Boolean;
    FAdmin_author_id: TVkPeerId;
    FIs_cropped: Boolean;
    FMembers_count: Integer;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FUpdate_time: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FPinned_at: TDateTime;
    FMessage_tag: string;
    function GetPayloadButton: TVkPayloadButton;
    function GetIsBotFrom: Boolean;
  public
    /// <summary>
    /// Идентификатор сообщения
    /// </summary>
    property Id;
    /// <summary>
    /// Время отправки
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Идентификатор назначения
    /// </summary>
    property PeerId: TVkPeerId read FPeer_id write FPeer_id;
    /// <summary>
    /// Идентификатор отправителя
    /// </summary>
    property FromId: TVkPeerId read FFrom_id write FFrom_id;
    /// <summary>
    /// Текст сообщения
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Идентификатор, используемый при отправке сообщения. Возвращается только для исходящих сообщений
    /// </summary>
    property RandomId: Integer read FRandom_id write FRandom_id;
    /// <summary>
    /// Произвольный параметр для работы с источниками переходов
    /// </summary>
    property Ref: string read FRef write FRef;
    /// <summary>
    /// Произвольный параметр для работы с источниками переходов
    /// </summary>
    property RefSource: string read FRef_source write FRef_source;
    /// <summary>
    /// Медиавложения сообщения (фотографии, ссылки и т.п.). Описание
    /// массива attachments находится на отдельной странице.
    /// </summary>
    property Attachments: TVkAttachmentArray read FAttachments write FAttachments;
    /// <summary>
    /// true, если сообщение помечено как важное
    /// </summary>
    property Important: Boolean read FImportant write FImportant;
    /// <summary>
    /// Было ли вложенное аудиосообщение уже прослушано вами
    /// </summary>
    property WasListened: Boolean read FWas_listened write FWas_listened;
    /// <summary>
    /// Информация о местоположении
    /// </summary>
    property Geo: TVkGeo read FGeo write FGeo;
    /// <summary>
    /// Сервисное поле для сообщений ботам (полезная нагрузка)
    /// </summary>
    property Payload: string read FPayload write FPayload;
    /// <summary>
    /// Объект клавиатуры для ботов
    /// </summary>
    property Keyboard: TVkKeyboard read FKeyboard write FKeyboard;
    /// <summary>
    /// Массив пересланных сообщений (если есть). Максимальное количество элементов — 100.
    /// Максимальная глубина вложенности для пересланных сообщений — 45, общее максимальное
    /// количество в цепочке с учетом вложенности — 500.
    /// </summary>
    property FwdMessages: TArray<TVkMessage> read FFwd_messages write FFwd_messages;
    /// <summary>
    /// Сообщение, в ответ на которое отправлено текущее
    /// </summary>
    property ReplyMessage: TVkMessage read FReply_message write FReply_message;
    /// <summary>
    /// Информация о сервисном действии с чатом
    /// </summary>
    property Action: TVkMessageAction read FAction write FAction;
    property PayloadButton: TVkPayloadButton read GetPayloadButton;
    /// <summary>
    /// Уникальный автоматически увеличивающийся номер для всех сообщений с этим peer
    /// </summary>
    property ConversationMessageId: Integer read FConversation_message_id write FConversation_message_id;
    property IsHidden: Boolean read FIs_hidden write FIs_hidden;
    /// <summary>
    /// Исходящее сообщение
    /// </summary>
    property &Out: Boolean read FOut write FOut;
    /// <summary>
    /// Только для сообщений сообщества. Содержит идентификатор пользователя (администратора сообщества),
    /// отправившего это сообщение
    /// </summary>
    property AdminAuthorId: TVkPeerId read FAdmin_author_id write FAdmin_author_id;
    /// <summary>
    /// Это сообщение обрезано для бота
    /// </summary>
    property IsCropped: Boolean read FIs_cropped write FIs_cropped;
    /// <summary>
    /// Количество участников
    /// </summary>
    property MembersCount: Integer read FMembers_count write FMembers_count;
    /// <summary>
    /// Дата, когда сообщение было обновлено
    /// </summary>
    property UpdateTime: TDateTime read FUpdate_time write FUpdate_time;
    /// <summary>
    /// Дата, когда сообщение было закреплено
    /// </summary>
    property PinnedAt: TDateTime read FPinned_at write FPinned_at;
    /// <summary>
    /// Строка для сопоставления пользователя Notify и ВКонтакте.
    /// Данные копирует в момент ответа пользователем на сообщение из Notify,
    /// отправленное с параметром session_id. Подробнее про Notify: https://notify.mail.ru/
    /// </summary>
    property MessageTag: string read FMessage_tag write FMessage_tag;
    //
    property IsBotFrom: Boolean read GetIsBotFrom;
    //
    function GetPreviewAttachment(var Url: string; Index: Integer): Boolean;
    destructor Destroy; override;
  end;

  TVkMessages = TVkEntityExtendedList<TVkMessage>;

  TVkLongPollHistory = class(TVkEntity)
  private
    FHistory: TArray<TArray<Integer>>;
    FMessages: TVkMessages;
    FProfiles: TArray<TVkProfile>;
    FNew_pts: Integer;
    FGroups: TArray<TVkGroup>;
  public
    property History: TArray<TArray<Integer>> read FHistory write FHistory;
    property Messages: TVkMessages read FMessages write FMessages;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property NewPts: Integer read FNew_pts write FNew_pts;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils, System.Json, System.DateUtils, VK.CommonUtils;

{TVkMessageAction}

destructor TVkMessageAction.Destroy;
begin
  if Assigned(FPhoto) then
    FPhoto.Free;
  inherited;
end;

{TVkMessage}

destructor TVkMessage.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkMessage>(FFwd_messages);
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  if Assigned(FReply_message) then
    FReply_message.Free;
  if Assigned(FKeyboard) then
    FKeyboard.Free;
  if Assigned(FGeo) then
    FGeo.Free;
  if Assigned(FPayloadButton) then
    FPayloadButton.Free;
  if Assigned(FAction) then
    FAction.Free;
  {$ENDIF}
  inherited;
end;

function TVkMessage.GetPreviewAttachment(var Url: string; Index: Integer): Boolean;
var
  c: Integer;
  Item: TVkAttachment;
begin
  Result := False;
  c := -1;
  for Item in FAttachments do
  begin
    if Item.&Type in [TVkAttachmentType.Photo, TVkAttachmentType.Video] then
    begin
      Inc(c);
      if c = Index then
      begin
        Url := Item.GetPreviewUrl;
        Exit(True);
      end;
    end;
  end;
end;

function TVkMessage.GetIsBotFrom: Boolean;
begin
  Result := FFrom_id < 0;
end;

function TVkMessage.GetPayloadButton: TVkPayloadButton;
begin
  if Assigned(FPayloadButton) then
    Exit(FPayloadButton);
  if Payload.IsEmpty then
    Exit(nil);
  try
    FPayloadButton := TVkPayloadButton.FromJsonString<TVkPayloadButton>(Payload);
    Result := FPayloadButton;
  except
    Exit(nil);
  end;
end;

{ TVkMessageSendResponses }

constructor TVkMessageSendResponses.CreateFalse;
begin
  inherited;
  FSuccess := False;
end;

constructor TVkMessageSendResponses.CreateTrue(ARespone: Integer);
begin
  inherited;
  FSuccess := True;
  Fresponse := ARespone;
end;

class function TVkMessageSendResponses.FromJsonString(AJsonString: string): TVkMessageSendResponses;
begin
  Result := TJson.JsonToObject<TVkMessageSendResponses>(AJsonString);
  Result.FSuccess := True;
  Result.Response := -1;
end;

procedure TVkMessageSendResponses.SetResponse(const Value: Integer);
begin
  FResponse := Value;
end;

procedure TVkMessageSendResponses.SetSuccess(const Value: Boolean);
begin
  FSuccess := Value;
end;

{ TVkLongPollHistory }

destructor TVkLongPollHistory.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  if Assigned(FMessages) then
    FMessages.Free;
  {$ENDIF}
  inherited;
end;

{ TVkMessageForward }

function TVkMessageForward.ToJSON: string;
var
  JSON: TJSONObject;
  Arr: TJSONArray;
  Item: Integer;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('owner_id', TJSONNumber.Create(FOwner_id));
    JSON.AddPair('peer_id', TJSONNumber.Create(FPeer_id));
    Arr := TJSONArray.Create;
    for Item in FConversation_message_ids do
      Arr.Add(Item);
    JSON.AddPair('conversation_message_ids', Arr);
    Arr := TJSONArray.Create;
    for Item in FMessage_ids do
      Arr.Add(Item);
    JSON.AddPair('message_ids', Arr);
    JSON.AddPair('is_reply', TJSONNumber.Create(BoolToString(FIs_reply)));
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

{ TVkMessageContentSource }

function TVkMessageContentSource.ToJSON: string;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    if FType = '' then
      FType := 'message';
    JSON.AddPair('type', FType);
    if FType = 'message' then
    begin
      JSON.AddPair('owner_id', TJSONNumber.Create(FOwner_id));
      JSON.AddPair('peer_id', TJSONNumber.Create(FPeer_id));
      JSON.AddPair('conversation_message_id', TJSONNumber.Create(FConversation_message_id));
    end
    else if FType = 'url' then
    begin
      JSON.AddPair('url', FUrl);
    end;
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

end.

