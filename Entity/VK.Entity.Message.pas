unit VK.Entity.Message;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Media, VK.Entity.Keyboard;

type
  TVkMessageSendResponse = class
  private
    FMessage_id: Integer; // Ч идентификатор сообщени€;
    FPeer_id: Integer;    // Ч идентификатор назначени€;
    FError: string;       // Ч сообщение об ошибке, если сообщение не было доставлено получателю.
  public
    property peer_id: Integer read FPeer_id write FPeer_id;
    property message_id: Integer read FMessage_id write FMessage_id;
    property error: string read FError write FError;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageSendResponse;
  end;

  TVkMessageSendResponses = class
  private
    FItems: TArray<TVkMessageSendResponse>;
    Fsuccess: Boolean;
    Fresponse: Integer;
    procedure Setsuccess(const Value: Boolean);
    procedure Setresponse(const Value: Integer);
  public
    property items: TArray<TVkMessageSendResponse> read FItems write FItems;
    property success: Boolean read Fsuccess write Setsuccess;
    property response: Integer read Fresponse write Setresponse;
    constructor CreateFalse;
    constructor CreateTrue(ARespone: Integer);
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageSendResponses;
  end;

  TVkMessageAction = class
  private
    FText: string; // Ч название беседы (дл€ служебных сообщений с type = chat_create или chat_title_update).
    FType: string; // Ч тип действи€. ¬озможные значени€:
    FEmail: string; // Ч email, который пригласили или исключили (дл€ служебных сообщений с type = chat_invite_user или chat_kick_user и отрицательным member_id).
    FMember_id: integer; // Ч идентификатор пользовател€ (если > 0) или email (если < 0), которого пригласили или исключили (дл€ служебных сообщений с type = chat_invite_user или chat_kick_user). »дентификатор пользовател€, который закрепил/открепил сообщение дл€ action = chat_pin_message или chat_unpin_message.
    FPhoto: TVkChatPhoto; // Ч изображение-обложка чата. ќбъект, который содержит пол€:
  public
    property text: string read FText write FText;
    property&type: string read FType write FType;
    property member_id: integer read FMember_id write FMember_id;
    property email: string read FEmail write FEmail;
    property photo: TVkChatPhoto read FPhoto write FPhoto;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageAction;
  end;

  TVkMessage = class
  private
    FAttachments: TArray<TVkAttachment>;
    FConversation_message_id: Extended;
    FDate: Extended;
    FFrom_id: Extended;
    FFwd_messages: TArray<TVkMessage>;
    FId: Extended;
    FImportant: Boolean;
    FIs_hidden: Boolean;
    FOut: Extended;
    FPeer_id: Extended;
    FRandom_id: Extended;
    FText: string;
    FRef: string;
    FRef_source: string;
    FGeo: TVkGeo;
    FPayload: string;
    FKeyboard: TVkKeyboard;
    FReply_message: TVkMessage;
    FAction: TVkMessageAction;
  public
    property id: Extended read FId write FId;
    property date: Extended read FDate write FDate;
    property peer_id: Extended read FPeer_id write FPeer_id;
    property from_id: Extended read FFrom_id write FFrom_id;
    property text: string read FText write FText;
    property random_id: Extended read FRandom_id write FRandom_id;
    property ref: string read FRef write FRef;
    property ref_source: string read FRef_source write FRef_source;
    property attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property important: Boolean read FImportant write FImportant;
    property geo: TVkGeo read FGeo write FGeo;
    property payload: string read FPayload write FPayload;
    property keyboard: TVkKeyboard read FKeyboard write FKeyboard;
    property fwd_messages: TArray<TVkMessage> read FFwd_messages write FFwd_messages;
    property reply_message: TVkMessage read FReply_message write FReply_message;
    property action: TVkMessageAction read FAction write FAction;
    //
    property conversation_message_id: Extended read FConversation_message_id write FConversation_message_id;
    property is_hidden: Boolean read FIs_hidden write FIs_hidden;
    property&out: Extended read FOut write FOut;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessage;
  end;

  TVkMessages = TArray<TVkMessage>;

implementation

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
  Result.response := -1;
end;

procedure TVkMessageSendResponses.Setresponse(const Value: Integer);
begin
  Fresponse := Value;
end;

procedure TVkMessageSendResponses.Setsuccess(const Value: Boolean);
begin
  Fsuccess := Value;
end;

function TVkMessageSendResponses.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

