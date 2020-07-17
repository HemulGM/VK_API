unit VK.Entity.Message;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Media, VK.Entity.Keyboard, VK.Entity.ClientInfo,
  VK.Entity.User, VK.Entity.Group;

type
  TVkMessageSendResponse = class
  private
    FMessage_id: Integer; // Ч идентификатор сообщени€;
    FPeer_id: Integer;    // Ч идентификатор назначени€;
    FError: string;       // Ч сообщение об ошибке, если сообщение не было доставлено получателю.
  public
    property PeerId: Integer read FPeer_id write FPeer_id;
    property MessageId: Integer read FMessage_id write FMessage_id;
    property Error: string read FError write FError;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageSendResponse;
  end;

  TVkMessageSendResponses = class
  private
    FItems: TArray<TVkMessageSendResponse>;
    Fsuccess: Boolean;
    Fresponse: Integer;
    procedure SetSuccess(const Value: Boolean);
    procedure SetResponse(const Value: Integer);
  public
    property Items: TArray<TVkMessageSendResponse> read FItems write FItems;
    property Success: Boolean read Fsuccess write SetSuccess;
    property Response: Integer read Fresponse write SetResponse;
    constructor CreateFalse;
    constructor CreateTrue(ARespone: Integer);
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageSendResponses;
  end;

  TVkMessageAction = class
  private
    FText: string; // Ч название беседы (дл€ служебных сообщений с type = chat_create или chat_title_update).
    FType: string; // Ч тип действи€. ¬озможные значени€:
                   {chat_photo_update Ч обновлена фотографи€ беседы;
                    chat_photo_remove Ч удалена фотографи€ беседы;
                    chat_create Ч создана беседа;
                    chat_title_update Ч обновлено название беседы;
                    chat_invite_user Ч приглашен пользователь;
                    chat_kick_user Ч исключен пользователь;
                    chat_pin_message Ч закреплено сообщение;
                    chat_unpin_message Ч откреплено сообщение;
                    chat_invite_user_by_link Ч пользователь присоединилс€ к беседе по ссылке.}
    FEmail: string; // Ч email, который пригласили или исключили (дл€ служебных сообщений с type = chat_invite_user или chat_kick_user и отрицательным member_id).
    FMember_id: integer; // Ч идентификатор пользовател€ (если > 0) или email (если < 0), которого пригласили или исключили (дл€ служебных сообщений с type = chat_invite_user или chat_kick_user). »дентификатор пользовател€, который закрепил/открепил сообщение дл€ action = chat_pin_message или chat_unpin_message.
    FPhoto: TVkChatPhoto; // Ч изображение-обложка чата. ќбъект, который содержит пол€:
  public
    property Text: string read FText write FText;
    property&Type: string read FType write FType;
    property MemberId: integer read FMember_id write FMember_id;
    property Email: string read FEmail write FEmail;
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

  TVkMessage = class
  private
    FAttachments: TArray<TVkAttachment>;
    FConversation_message_id: Integer;
    FDate: Int64;
    FFrom_id: Integer;
    FFwd_messages: TArray<TVkMessage>;
    FId: Integer;
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
    function GetPayloadButton: TVkPayloadButton;
    function GetOut: Boolean;
    procedure SetOut(const Value: Boolean);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
  public
    property Id: Integer read FId write FId;
    property Date: TDateTime read GetDate write SetDate;
    property PeerId: Integer read FPeer_id write FPeer_id;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Text: string read FText write FText;
    property RandomId: Integer read FRandom_id write FRandom_id;
    property Ref: string read FRef write FRef;
    property RefSource: string read FRef_source write FRef_source;
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property Important: Boolean read FImportant write FImportant;
    property Geo: TVkGeo read FGeo write FGeo;
    property Payload: string read FPayload write FPayload;
    property Keyboard: TVkKeyboard read FKeyboard write FKeyboard;
    property FwdMessages: TArray<TVkMessage> read FFwd_messages write FFwd_messages;
    property ReplyMessage: TVkMessage read FReply_message write FReply_message;
    property Action: TVkMessageAction read FAction write FAction;
    property PayloadButton: TVkPayloadButton read GetPayloadButton;
    //
    property ConversationMessageId: Integer read FConversation_message_id write FConversation_message_id;
    property IsHidden: Boolean read FIs_hidden write FIs_hidden;
    property&Out: Boolean read GetOut write SetOut;
    function GetPreviewAttachment(var Url: string; Index: Integer): Boolean;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessage;
  end;

  TVkMessages = class
  private
    FItems: TArray<TVkMessage>;
    FCount: Integer;
    FSaveObjects: Boolean;
    FProfiles: TArray<TVkUser>;
    FGroups: TArray<TVkGroup>;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkMessage> read FItems write FItems;
    property Profiles: TArray<TVkUser> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Users: TVkMessages);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessages;
  end;

implementation

uses
  SysUtils, DateUtils, VK.CommonUtils, VK.Types;

{TVkMessageAction}

function TVkMessageAction.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkMessageAction.FromJsonString(AJsonString: string): TVkMessageAction;
begin
  result := TJson.JsonToObject<TVkMessageAction>(AJsonString)
end;

{TVkMessage}

destructor TVkMessage.Destroy;
var
  Lfwd_messagesItem: TVkMessage;
  LattachmentsItem: TVkAttachment;
begin

  for Lfwd_messagesItem in FFwd_messages do
    Lfwd_messagesItem.Free;
  for LattachmentsItem in FAttachments do
    LattachmentsItem.Free;
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
  inherited;
end;

function TVkMessage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkMessage.FromJsonString(AJsonString: string): TVkMessage;
begin
  result := TJson.JsonToObject<TVkMessage>(AJsonString)
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

function TVkMessage.GetPreviewAttachment(var Url: string; Index: Integer): Boolean;
var
  i, c: Integer;
begin
  c := -1;
  Result := False;
  for i := Low(FAttachments) to High(FAttachments) do
    if FAttachments[i].&Type in [atPhoto, atVideo] then
    begin
      Inc(c);
      if c = Index then
      begin
        Url := FAttachments[i].GetPreviewUrl;
        Exit(True);
      end;
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
  result := TJson.JsonToObject<TVkMessageSendResponse>(AJsonString);
end;

function TVkMessageSendResponse.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkMessageSendResponses }

constructor TVkMessageSendResponses.CreateFalse;
begin
  inherited;
  Fsuccess := False;
end;

constructor TVkMessageSendResponses.CreateTrue(ARespone: Integer);
begin
  inherited;
  Fsuccess := True;
  Fresponse := ARespone;
end;

class function TVkMessageSendResponses.FromJsonString(AJsonString: string): TVkMessageSendResponses;
begin
  result := TJson.JsonToObject<TVkMessageSendResponses>(AJsonString);
  result.Fsuccess := True;
  Result.Response := -1;
end;

procedure TVkMessageSendResponses.SetResponse(const Value: Integer);
begin
  Fresponse := Value;
end;

procedure TVkMessageSendResponses.SetSuccess(const Value: Boolean);
begin
  Fsuccess := Value;
end;

function TVkMessageSendResponses.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
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
var
  LItemsItem: TVkMessage;
  LGroupItem: TVkGroup;
  LUserItem: TVkUser;
begin
  if not FSaveObjects then
  begin
    for LItemsItem in FItems do
      LItemsItem.Free;
    for LUserItem in FProfiles do
      LUserItem.Free;
    for LGroupItem in FGroups do
      LGroupItem.Free;
  end;

  inherited;
end;

function TVkMessages.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkMessages.FromJsonString(AJsonString: string): TVkMessages;
begin
  result := TJson.JsonToObject<TVkMessages>(AJsonString);
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

end.

