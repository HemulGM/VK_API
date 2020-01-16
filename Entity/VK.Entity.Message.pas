unit VK.Entity.Message;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Attachment, VK.Entity.Keyboard;

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

  TVkPlace = class
  private
    FCity: string; // Ч название города;
    FCountry: string; // Ч название страны;
    FTitle: string; // Ч название места (если назначено);
    FId: integer; // Ч идентификатор места (если назначено);
    FLatitude: Extended; // Ч географическа€ широта;
    FLongitude: Extended; // Ч географическа€ долгота;
    FCreated: integer; // Ч дата создани€ (если назначено);
    FIcon: string; // Ч URL изображени€-иконки;
  public
    property city: string read FCity write FCity;
    property country: string read FCountry write FCountry;
    property title: string read FTitle write FTitle;
    property id: integer read FId write FId;
    property latitude: Extended read FLatitude write FLatitude;
    property longitude: Extended read FLongitude write FLongitude;
    property created: integer read FCreated write FCreated;
    property icon: string read FIcon write FIcon;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPlace;
  end;

  TVkCoordinates = class
  private
    FLatitude: Extended;
    FLongitude: Extended;
  public
    property latitude: Extended read FLatitude write FLatitude;
    property longitude: Extended read FLongitude write FLongitude;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCoordinates;
  end;

  TVkGeo = class
  private
    FCoordinates: TVkCoordinates;
    FPlace: TVkPlace;
    FType: string;
  public
    property coordinates: TVkCoordinates read FCoordinates write FCoordinates;
    property place: TVkPlace read FPlace write FPlace;
    property&type: string read FType write FType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGeo;
  end;

  TVkChatPhoto = class
  private
    FPhoto_50: string;
    FPhoto_200: string;
    FPhoto_100: string;
  public
    property photo_50: string read FPhoto_50 write FPhoto_50; // Ч URL изображени€ 50x50px;
    property photo_100: string read FPhoto_100 write FPhoto_100; // Ч URL изображени€ 100x100px;
    property photo_200: string read FPhoto_200 write FPhoto_200; // Ч URL изображени€ 200x200px;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatPhoto;
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
    property action: TVkMessageAction read FAction write FAction;
    property attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property geo: TVkGeo read FGeo write FGeo;
    property conversation_message_id: Extended read FConversation_message_id write FConversation_message_id;
    property date: Extended read FDate write FDate;
    property from_id: Extended read FFrom_id write FFrom_id;
    property fwd_messages: TArray<TVkMessage> read FFwd_messages write FFwd_messages;
    property id: Extended read FId write FId;
    property important: Boolean read FImportant write FImportant;
    property is_hidden: Boolean read FIs_hidden write FIs_hidden;
    property&out: Extended read FOut write FOut;
    property peer_id: Extended read FPeer_id write FPeer_id;
    property random_id: Extended read FRandom_id write FRandom_id;
    property text: string read FText write FText;
    property ref: string read FRef write FRef;
    property ref_source: string read FRef_source write FRef_source;
    property payload: string read FPayload write FPayload;
    property keyboard: TVkKeyboard read FKeyboard write FKeyboard;
    property reply_message: TVkMessage read FReply_message write FReply_message;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessage;
  end;

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

{TVkPlace}

function TVkPlace.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPlace.FromJsonString(AJsonString: string): TVkPlace;
begin
  result := TJson.JsonToObject<TVkPlace>(AJsonString)
end;

{TVkCoordinates}

function TVkCoordinates.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCoordinates.FromJsonString(AJsonString: string): TVkCoordinates;
begin
  result := TJson.JsonToObject<TVkCoordinates>(AJsonString)
end;

{TVkGeo}

constructor TVkGeo.Create;
begin
  inherited;
  FCoordinates := TVkCoordinates.Create();
  FPlace := TVkPlace.Create();
end;

destructor TVkGeo.Destroy;
begin
  FCoordinates.Free;
  FPlace.Free;
  inherited;
end;

function TVkGeo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGeo.FromJsonString(AJsonString: string): TVkGeo;
begin
  result := TJson.JsonToObject<TVkGeo>(AJsonString)
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

{ TVkChatPhoto }

class function TVkChatPhoto.FromJsonString(AJsonString: string): TVkChatPhoto;
begin
  result := TJson.JsonToObject<TVkChatPhoto>(AJsonString)
end;

function TVkChatPhoto.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
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

