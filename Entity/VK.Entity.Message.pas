unit VK.Entity.Message;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Common, VK.Entity.Media, VK.Types, VK.Entity.Keyboard,
  VK.Entity.ClientInfo, VK.Entity.Profile, VK.Entity.Group,
  VK.Entity.Common.List, VK.Entity.Common.ExtendedList, VK.Wrap.Interceptors,
  VK.Entity.Geo;

type
  TVkLastActivity = class(TVkEntity)
  private
    FOnline: Boolean;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
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
    FOwner_id: Integer;
    FPeer_id: Integer;
  public
    /// <summary>
    /// Идентификатор места, из которого необходимо переслать сообщения
    /// </summary>
    property PeerId: Integer read FPeer_id write FPeer_id;
    /// <summary>
    /// Владелец сообщений. Стоит передавать, если вы хотите переслать сообщения из сообщества в диалог
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
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
    FPeer_id: Integer;
    FError: string;
  public
    /// <summary>
    /// Идентификатор назначения
    /// </summary>
    property PeerId: Integer read FPeer_id write FPeer_id;
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
    FMember_id: integer;
    FPhoto: TVkChatPhoto;
  public
    /// <summary>
    /// Название беседы (для служебных сообщений с type = chat_create или chat_title_update)
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Тип действия
    /// </summary>
    property&Type: TVkMessageActionType read FType write FType;
    /// <summary>
    /// Идентификатор пользователя (если > 0) или email (если < 0), которого пригласили или исключили
    /// (для служебных сообщений с type = chat_invite_user или chat_kick_user).
    /// Идентификатор пользователя, который закрепил/открепил сообщение для action = chat_pin_message или chat_unpin_message
    /// </summary>
    property MemberId: integer read FMember_id write FMember_id;
    /// <summary>
    /// Email, который пригласили или исключили (для служебных сообщений с type = chat_invite_user или chat_kick_user и отрицательным member_id)
    /// </summary>
    property Email: string read FEmail write FEmail;
    /// <summary>
    /// Изображение-обложка чата
    /// </summary>
    property Photo: TVkChatPhoto read FPhoto write FPhoto;
    destructor Destroy; override;
  end;

  TVkMessageContentSource = record
  private
    FConversation_message_id: Integer;
    FType: string;
    FOwner_id: Integer;
    FPeer_id: Integer;
    FToJSON: string;
    FUrl: string;
  public
    property&Type: string read FType write FType; // 'message', 'url'
    property PeerId: Integer read FPeer_id write FPeer_id;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property ConversationMessageId: Integer read FConversation_message_id write FConversation_message_id;
    property Url: string read FUrl write FUrl;
    function ToJSON: string;
  end;

  TVkMessage = class(TVkObject)
  private
    FAttachments: TArray<TVkAttachment>;
    FConversation_message_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_id: Integer;
    FFwd_messages: TArray<TVkMessage>;
    FImportant: Boolean;
    FIs_hidden: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FOut: Boolean;
    FPeer_id: Integer;
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
    function GetPayloadButton: TVkPayloadButton;
  public
    property Date: TDateTime read FDate write FDate;
    property PeerId: Integer read FPeer_id write FPeer_id;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Text: string read FText write FText;
    property RandomId: Integer read FRandom_id write FRandom_id;
    property Ref: string read FRef write FRef;
    property RefSource: string read FRef_source write FRef_source;
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property Important: Boolean read FImportant write FImportant;
    property WasListened: Boolean read FWas_listened write FWas_listened;
    property Geo: TVkGeo read FGeo write FGeo;
    property Payload: string read FPayload write FPayload;
    property Keyboard: TVkKeyboard read FKeyboard write FKeyboard;
    property FwdMessages: TArray<TVkMessage> read FFwd_messages write FFwd_messages;
    property ReplyMessage: TVkMessage read FReply_message write FReply_message;
    property Action: TVkMessageAction read FAction write FAction;
    property PayloadButton: TVkPayloadButton read GetPayloadButton;
    //
    function GetPreviewAttachment(var Url: string; Index: Integer): Boolean;
    property ConversationMessageId: Integer read FConversation_message_id write FConversation_message_id;
    property IsHidden: Boolean read FIs_hidden write FIs_hidden;
    /// <summary>
    /// Исходящее сообщение
    /// </summary>
    property&Out: Boolean read FOut write FOut;
    constructor Create; override;
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
    constructor Create; override;
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

constructor TVkMessage.Create;
begin
  inherited;
  FAction := TVkMessageAction.Create;
end;

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

function TVkMessage.GetPayloadButton: TVkPayloadButton;
begin
  if Payload.IsEmpty then
    Exit(nil);
  if Assigned(FPayloadButton) then
    Exit(FPayloadButton);
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

constructor TVkLongPollHistory.Create;
begin
  FMessages := TVkMessages.Create;
end;

destructor TVkLongPollHistory.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
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

