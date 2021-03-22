unit VK.Bot;

interface

uses
  System.SysUtils, VK.API, VK.Components, VK.GroupEvents, VK.Entity.Message,
  VK.Types, VK.Entity.ClientInfo, System.Classes;

type
  TVkBot = class;

  TVkBotCallback = reference to procedure(Bot: TVkBot);

  TVkBotMessage = reference to procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo);

  TVkBotMessageEdit = reference to procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage);

  TVkBotError = reference to procedure(Bot: TVkBot; E: Exception; Code: Integer; Text: string);

  TVkBotJoin = reference to procedure(Bot: TVkBot; GroupId, UserId: Integer; JoinType: TVkGroupJoinType; const EventId: string);

  TVkBot = class
    class var
      Instance: TVkBot;
  private
    FVK: TCustomVK;
    FLongPoll: TVkGroupEvents;
    FOnInit: TVkBotCallback;
    FOnDestroy: TVkBotCallback;
    FOnError: TVkBotError;
    function GetGroupId: Integer;
    function GetToken: string;
    procedure SetOnInit(const Value: TVkBotCallback);
    procedure SetOnDestroy(const Value: TVkBotCallback);
    procedure FOnVkError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure SetOnError(const Value: TVkBotError);
    procedure SetGroupId(const Value: Integer);
    procedure SetToken(const Value: string);
  public
    class function GetInstance<T: TVkBot>: T; overload;
    constructor Create; virtual;
    destructor Destroy; override;
    function Init: Boolean; virtual;
    function Run: Boolean;
    procedure Stop;
    property API: TCustomVK read FVK;
    property GroupId: Integer read GetGroupId write SetGroupId;
    property LongPoll: TVkGroupEvents read FLongPoll;
    property OnDestroy: TVkBotCallback read FOnDestroy write SetOnDestroy;
    property OnError: TVkBotError read FOnError write SetOnError;
    property OnInit: TVkBotCallback read FOnInit write SetOnInit;
    property Token: string read GetToken write SetToken;
  end;

  TVkBotChat = class(TVkBot)
  private
    FSkipOtherBotMessages: Boolean;
    FOnMessage: TVkBotMessage;
    FOnMessageEdit: TVkBotMessageEdit;
    FOnJoin: TVkBotJoin;
    procedure FOnNewMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
    procedure FOnEditMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; const EventId: string);
    procedure FOnGroupJoin(Sender: TObject; GroupId, UserId: Integer; JoinType: TVkGroupJoinType; const EventId: string);
    procedure SetOnMessageEdit(const Value: TVkBotMessageEdit);
    procedure SetOnMessage(const Value: TVkBotMessage);
    procedure SetSkipOtherBotMessages(const Value: Boolean);
    procedure SetOnJoin(const Value: TVkBotJoin);
  public
    class function GetInstance: TVkBotChat; overload;
    constructor Create; override;
    property OnJoin: TVkBotJoin read FOnJoin write SetOnJoin;
    property OnMessage: TVkBotMessage read FOnMessage write SetOnMessage;
    property OnMessageEdit: TVkBotMessageEdit read FOnMessageEdit write SetOnMessageEdit;
    property SkipOtherBotMessages: Boolean read FSkipOtherBotMessages write SetSkipOtherBotMessages;
  end;

implementation

uses
  VK.Bot.Utils, System.Threading, System.StrUtils;

{ TVkBot }

constructor TVkBot.Create;
begin
  FVK := TCustomVK.Create(nil);
  FVK.OnError := FOnVkError;
  FLongPoll := TVkGroupEvents.Create(nil);
  FLongPoll.VK := FVK;
  FLongPoll.LongPollServer.DoSync := False;
end;

destructor TVkBot.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  Stop;
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

function TVkBot.Init: Boolean;
begin
  Result := False;
  Console.AddText('Initializate...');
  if Assigned(FOnInit) then
  try
    FOnInit(Self);
  except
    Exit(False);
  end;
  try
    Result := FVK.Login;
  finally
    if Result then
      Console.AddLine('I''m ready!', GREEN)
    else if API.Token.IsEmpty then
      Console.AddLine('Error! Token Need', RED)
    else
      Console.AddLine('Error!', RED);
  end;
end;

function TVkBot.Run: Boolean;
begin
  Result := FLongPoll.Start;
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

procedure TVkBot.Stop;
begin
  FLongPoll.Stop;
end;

{ TVkBotChat }

constructor TVkBotChat.Create;
begin
  inherited;
  LongPoll.OnMessageNew := FOnNewMessage;
  LongPoll.OnMessageEdit := FOnEditMessage;
  LongPoll.OnGroupJoin := FOnGroupJoin;
end;

procedure TVkBotChat.FOnEditMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; const EventId: string);
begin
  if Assigned(FOnMessage) then
  begin
    if FSkipOtherBotMessages and (Message.FromId < 0) then
      Exit;
    TTask.Run(
      procedure
      begin
        try
          FOnMessageEdit(Self, GroupId, Message);
        finally
          Message.Free;
        end;
      end);
  end;
end;

procedure TVkBotChat.FOnGroupJoin(Sender: TObject; GroupId, UserId: Integer; JoinType: TVkGroupJoinType; const EventId: string);
begin
  if Assigned(FOnJoin) then
  begin
    if FSkipOtherBotMessages and (UserId < 0) then
      Exit;
    TTask.Run(
      procedure
      begin
        FOnJoin(Self, GroupId, UserId, JoinType, EventId);
      end);
  end;
end;

procedure TVkBotChat.FOnNewMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
begin
  if Assigned(FOnMessage) then
  begin
    if FSkipOtherBotMessages and (Message.FromId < 0) then
      Exit;
    TTask.Run(
      procedure
      begin
        try
          FOnMessage(Self, GroupId, Message, ClientInfo);
        finally
          Message.Free;
          ClientInfo.Free;
        end;
      end);
  end;
end;

class function TVkBotChat.GetInstance: TVkBotChat;
begin
  if not Assigned(Instance) then
    Instance := TVkBotChat.Create;
  Result := TVkBotChat(Instance);
end;

procedure TVkBotChat.SetOnJoin(const Value: TVkBotJoin);
begin
  FOnJoin := Value;
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

