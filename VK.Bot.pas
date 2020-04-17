unit VK.Bot;

interface

uses
  System.SysUtils, VK.API, VK.Components, VK.GroupEvents, VK.Entity.Message, VK.Entity.ClientInfo;

type
  TVkBot = class;

  TVkBotCallback = reference to procedure(Bot: TVkBot);

  TVkBotMessage = reference to procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo);

  TVkBotMessageEdit = reference to procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage);

  TVkBotError = reference to procedure(Bot: TVkBot; E: Exception; Code: Integer; Text: string);

  TVkBot = class
    class var
      Instance: TVkBot;
  private
    FVK: TVK;
    FLongPoll: TVkGroupEvents;
    FOnInit: TVkBotCallback;
    FOnDestroy: TVkBotCallback;
    FOnError: TVkBotError;
    function GetGroupId: Integer;
    procedure SetOnInit(const Value: TVkBotCallback);
    procedure SetOnDestroy(const Value: TVkBotCallback);
    procedure FOnVkError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure SetOnError(const Value: TVkBotError);
    procedure SetGroupId(const Value: Integer);
    function GetToken: string;
    procedure SetToken(const Value: string);
  public
    class function GetInstance<T: TVkBot>: T; overload;
    function Run: Boolean; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
    property OnInit: TVkBotCallback read FOnInit write SetOnInit;
    property OnError: TVkBotError read FOnError write SetOnError;
    property OnDestroy: TVkBotCallback read FOnDestroy write SetOnDestroy;
    property VK: TVK read FVK;
    property GroupId: Integer read GetGroupId write SetGroupId;
    property Token: string read GetToken write SetToken;
    property LongPoll: TVkGroupEvents read FLongPoll;
  end;

  TVkBotChat = class(TVkBot)
  private
    FSkipOtherBotMessages: Boolean;
    FOnMessage: TVkBotMessage;
    FOnMessageEdit: TVkBotMessageEdit;
    procedure FOnNewMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; EventId: string);
    procedure FOnEditMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; EventId: string);
    procedure SetOnMessageEdit(const Value: TVkBotMessageEdit);
    procedure SetOnMessage(const Value: TVkBotMessage);
    procedure SetSkipOtherBotMessages(const Value: Boolean);
  public
    class function GetInstance: TVkBotChat; overload;
    constructor Create; override;
    property OnMessage: TVkBotMessage read FOnMessage write SetOnMessage;
    property OnMessageEdit: TVkBotMessageEdit read FOnMessageEdit write SetOnMessageEdit;
    property SkipOtherBotMessages: Boolean read FSkipOtherBotMessages write SetSkipOtherBotMessages;
  end;

implementation

{ TVkBot }

constructor TVkBot.Create;
begin
  FVK := TVK.Create(nil);
  FVK.OnError := FOnVkError;
  FLongPoll := TVkGroupEvents.Create(nil);
  FLongPoll.VK := FVK;
end;

destructor TVkBot.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  FLongPoll.Free;
  FVK.Free;
  inherited;
end;

procedure TVkBot.FOnVkError(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, E, Code, Text);
end;

function TVkBot.GetGroupId: Integer;
begin
  Result := FLongPoll.GroupID;
end;

class function TVkBot.GetInstance<T>: T;
begin
  if not Assigned(Instance) then
    Instance := T.Create;
  Result := T(Instance);
end;

function TVkBot.GetToken: string;
begin
  Result := FVK.Token;
end;

function TVkBot.Run: Boolean;
begin
  if Assigned(FOnInit) then
  try
    FOnInit(Self);
  except
    Exit(False);
  end;
  Result := FVK.Login and FLongPoll.Start;
end;

procedure TVkBot.SetGroupId(const Value: Integer);
begin
  FLongPoll.GroupID := Value;
end;

procedure TVkBot.SetOnDestroy(const Value: TVkBotCallback);
begin
  FOnDestroy := Value;
end;

procedure TVkBot.SetOnError(const Value: TVkBotError);
begin
  FOnError := Value;
end;

procedure TVkBot.SetOnInit(const Value: TVkBotCallback);
begin
  FOnInit := Value;
end;

procedure TVkBot.SetToken(const Value: string);
begin
  FVK.Token := Value;
end;

{ TVkBotChat }

constructor TVkBotChat.Create;
begin
  inherited;
  LongPoll.OnMessageNew := FOnNewMessage;
  LongPoll.OnMessageEdit := FOnEditMessage;
end;

procedure TVkBotChat.FOnEditMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; EventId: string);
begin
  if Assigned(FOnMessage) then
  begin
    if FSkipOtherBotMessages and (Message.FromId < 0) then
      Exit;
    FOnMessageEdit(Self, GroupId, Message);
  end;
end;

procedure TVkBotChat.FOnNewMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo;
  EventId: string);
begin
  if Assigned(FOnMessage) then
  begin
    if FSkipOtherBotMessages and (Message.FromId < 0) then
      Exit;
    FOnMessage(Self, GroupId, Message, ClientInfo);
  end;
end;

class function TVkBotChat.GetInstance: TVkBotChat;
begin
  if not Assigned(Instance) then
    Instance := TVkBotChat.Create;
  Result := TVkBotChat(Instance);
end;

procedure TVkBotChat.SetOnMessage(const Value: TVkBotMessage);
begin
  FOnMessage := Value;
end;

procedure TVkBotChat.SetOnMessageEdit(const Value: TVkBotMessageEdit);
begin
  FOnMessageEdit := Value;
end;

procedure TVkBotChat.SetSkipOtherBotMessages(const Value: Boolean);
begin
  FSkipOtherBotMessages := Value;
end;

end.

