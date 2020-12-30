unit VK.Entity.Message;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Media, VK.Types,
  VK.Entity.Keyboard, VK.Entity.ClientInfo, VK.Entity.Profile, VK.Entity.Group;

type
  TVkMessageSendResponse = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageSendResponse;
  end;

  TVkMessageSendResponses = class
  private
    FItems: TArray<TVkMessageSendResponse>;
    FSuccess: Boolean;
    Fresponse: Integer;
    procedure SetSuccess(const Value: Boolean);
    procedure SetResponse(const Value: Integer);
  public
    property Items: TArray<TVkMessageSendResponse> read FItems write FItems;
    property Success: Boolean read FSuccess write SetSuccess;
    property Response: Integer read Fresponse write SetResponse;
    constructor CreateFalse;
    constructor CreateTrue(ARespone: Integer);
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageSendResponses;
  end;

  TVkMessageAction = class
  private
    FText: string;
    FType: string;
    FEmail: string;
    FMember_id: integer;
    FPhoto: TVkChatPhoto;
    function GetType: TVkMessageActionType;
    procedure SetType(const Value: TVkMessageActionType);
  public
    /// <summary>
    /// Название беседы (для служебных сообщений с type = chat_create или chat_title_update)
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Тип действия (строка)
    /// </summary>
    property TypeStr: string read FType write FType;
    /// <summary>
    /// Тип действия
    /// </summary>
    property&Type: TVkMessageActionType read GetType write SetType;
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageAction;
  end;

  TVkMessageDelete = class
  private
    FItems: TDictionary<string, Boolean>;
    procedure SetItems(const Value: TDictionary<string, Boolean>);
  public
    property Items: TDictionary<string, Boolean> read FItems write SetItems;
  end;

  TVkMessage = class(TVkObject)
  private
    FAttachments: TArray<TVkAttachment>;
    FConversation_message_id: Integer;
    FDate: Int64;
    FFrom_id: Integer;
    FFwd_messages: TArray<TVkMessage>;
    FImportant: Boolean;
    FIs_hidden: Boolean;
    FOut: Integer;
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
    function GetOut: Boolean;
    procedure SetOut(const Value: Boolean);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
  public
    property Date: TDateTime read GetDate write SetDate;
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
    property&Out: Boolean read GetOut write SetOut;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessage;
  end;

  TVkMessages = class
  private
    FItems: TArray<TVkMessage>;
    FCount: Integer;
    FSaveObjects: Boolean;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkMessage> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Users: TVkMessages);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessages;
  end;

  TVkLongPollHistory = class
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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLongPollHistory;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, VK.CommonUtils;

{TVkMessageAction}

function TVkMessageAction.GetType: TVkMessageActionType;
begin
  Result := TVkMessageActionType.Create(FType);
end;

procedure TVkMessageAction.SetType(const Value: TVkMessageActionType);
begin
  FType := Value.ToString;
end;

function TVkMessageAction.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

class function TVkMessageAction.FromJsonString(AJsonString: string): TVkMessageAction;
begin
  Result := TJson.JsonToObject<TVkMessageAction>(AJsonString)
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
  if Assigned(FAction) then
    FAction.Free;
  if Assigned(FGeo) then
    FGeo.Free;
  if Assigned(FPayloadButton) then
    FPayloadButton.Free;
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
    if Item.&Type in [atPhoto, atVideo] then
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

function TVkMessage.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(self);
end;

class function TVkMessage.FromJsonString(AJsonString: string): TVkMessage;
begin
  Result := TJson.JsonToObject<TVkMessage>(AJsonString)
end;

function TVkMessage.GetDate: TDateTime;
begin
  Result := UnixToDateTime(FDate, False);
end;

function TVkMessage.GetOut: Boolean;
begin
  Result := FOut = 1;
end;

function TVkMessage.GetPayloadButton: TVkPayloadButton;
begin
  if Payload.IsEmpty then
    Exit(nil);
  if Assigned(FPayloadButton) then
    Exit(FPayloadButton);
  try
    FPayloadButton := TVkPayloadButton.FromJsonString(Payload);
    Result := FPayloadButton;
  except
    Exit(nil);
  end;
end;

procedure TVkMessage.SetDate(const Value: TDateTime);
begin
  FDate := DateTimeToUnix(Value, False);
end;

procedure TVkMessage.SetOut(const Value: Boolean);
begin
  FOut := BoolToInt(Value);
end;

{ TVkMessageSendResponse }

class function TVkMessageSendResponse.FromJsonString(AJsonString: string): TVkMessageSendResponse;
begin
  Result := TJson.JsonToObject<TVkMessageSendResponse>(AJsonString);
end;

function TVkMessageSendResponse.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
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

function TVkMessageSendResponses.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

{TVkMessages}

procedure TVkMessages.Append(Users: TVkMessages);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(Users.Items));
  Move(Users.Items[0], FItems[OldLen], Length(Users.Items) * SizeOf(TVkMessage));
end;

constructor TVkMessages.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkMessages.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if not FSaveObjects then
  begin
    TArrayHelp.FreeArrayOfObject<TVkMessage>(FItems);
    TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
    TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  end;
  {$ENDIF}
  inherited;
end;

function TVkMessages.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

class function TVkMessages.FromJsonString(AJsonString: string): TVkMessages;
begin
  Result := TJson.JsonToObject<TVkMessages>(AJsonString);
end;

procedure TVkMessages.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

{ TVkMessageDelete }

procedure TVkMessageDelete.SetItems(const Value: TDictionary<string, Boolean>);
begin
  FItems := Value;
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

class function TVkLongPollHistory.FromJsonString(AJsonString: string): TVkLongPollHistory;
begin
  Result := TJson.JsonToObject<TVkLongPollHistory>(AJsonString);
end;

function TVkLongPollHistory.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

end.

