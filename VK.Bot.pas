unit VK.Bot;

interface

uses
  System.SysUtils, VK.API, VK.Components, VK.GroupEvents, VK.Entity.Message,
  VK.Types, VK.Entity.ClientInfo, System.Classes, System.Generics.Collections;

type
  TVkBot = class;

  TVkBotCallback = reference to procedure(Bot: TVkBot);

  TVkBotMessage = reference to procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo);

  TVkBotMessageListener = reference to function(Bot: TVkBot; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo): Boolean;

  TVkBotMessageEdit = reference to procedure(Bot: TVkBot; GroupId: Integer; Message: TVkMessage);

  TVkBotError = reference to procedure(Bot: TVkBot; E: Exception; Code: Integer; Text: string);

  TVkBotJoin = reference to procedure(Bot: TVkBot; GroupId, UserId: Integer; JoinType: TVkGroupJoinType; const EventId: string);

  TVkPeerTypes = set of TVkPeerType;

  TMessageListener = record
    PeerTypes: TVkPeerTypes;
    Method: TVkBotMessageListener;
  end;

  TMessageListeners = TList<TMessageListener>;

  TEventListiner = record
    Method: Pointer;
    TypeInfo: Pointer;
    class function Create(Method, TypeInfo: Pointer): TEventListiner; static;
  end;

  TEventListeners = class(TList<TEventListiner>)
    function ForEach<T>(Proc: TFunc<TEventListiner, Boolean>): Boolean;
  end;

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
    FMessageListeners: TMessageListeners;
    FListeners: TEventListeners;
    procedure FOnNewMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
    procedure FOnEditMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; const EventId: string);
    procedure FOnGroupJoin(Sender: TObject; GroupId, UserId: Integer; JoinType: TVkGroupJoinType; const EventId: string);
    procedure SetOnMessageEdit(const Value: TVkBotMessageEdit);
    procedure SetOnMessage(const Value: TVkBotMessage);
    procedure SetSkipOtherBotMessages(const Value: Boolean);
    procedure SetOnJoin(const Value: TVkBotJoin);
    function HandleMessageListener(GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo): Boolean;
  public
    class function GetInstance(const GroupId: Integer; const Token: string): TVkBotChat; overload;
    constructor Create; override;
    destructor Destroy; override;
    function IsAdmin(PeerId: Integer; UserId: Integer): Boolean;
    procedure AddMessageListener(PeerTypes: TVkPeerTypes; Method: TVkBotMessageListener);
    procedure AddListener<T>(Method: T);
    property MessageListeners: TMessageListeners read FMessageListeners;
    property OnJoin: TVkBotJoin read FOnJoin write SetOnJoin;
    property OnMessage: TVkBotMessage read FOnMessage write SetOnMessage;
    property OnMessageEdit: TVkBotMessageEdit read FOnMessageEdit write SetOnMessageEdit;
    property SkipOtherBotMessages: Boolean read FSkipOtherBotMessages write SetSkipOtherBotMessages;
  end;

implementation

uses
  VK.Bot.Utils, System.Threading, System.StrUtils, VK.Entity.Conversation,
  VK.Messages, HGM.ArrayHelper;

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
    FOnError(Self, E, Code, Text)
  else
    Console.AddLine(Text, RED);
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
  Console.AddText('Initializate... ');
  if Assigned(FOnInit) then
    FOnInit(Self);
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

procedure TVkBotChat.AddListener<T>(Method: T);
begin
  FListeners.Add(TEventListiner.Create(@Method, TypeInfo(T)));
  if TypeInfo(T) = TypeInfo(TOnWallPostAction) then
  begin

  end;
  if TypeInfo(T) = TypeInfo(TOnGroupMessageNew) then
  begin
    TOnGroupMessageNew((@Method)^)(nil, 0, nil, nil, '123');
  end;
end;

procedure TVkBotChat.AddMessageListener(PeerTypes: TVkPeerTypes; Method: TVkBotMessageListener);
var
  Item: TMessageListener;
begin
  Item.PeerTypes := PeerTypes;
  Item.Method := Method;
  FMessageListeners.Add(Item);
end;

constructor TVkBotChat.Create;
begin
  inherited;
  FSkipOtherBotMessages := False;
  FMessageListeners := TMessageListeners.Create;
  FListeners := TEventListeners.Create;
  LongPoll.OnMessageNew := FOnNewMessage;
  LongPoll.OnMessageEdit := FOnEditMessage;
  LongPoll.OnGroupJoin := FOnGroupJoin;
end;

destructor TVkBotChat.Destroy;
begin
  FMessageListeners.Free;
  FListeners.Free;
  inherited;
end;

procedure TVkBotChat.FOnEditMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; const EventId: string);
var
  FMessage: TVkMessage;
begin
  if FSkipOtherBotMessages and (Message.FromId < 0) then
    Exit;
  if Assigned(FOnMessage) then
  begin
    FMessage := Message.Clone<TVkMessage>;
    TTask.Run(
      procedure
      begin
        try
          FOnMessageEdit(Self, GroupId, FMessage);
        finally
          FMessage.Free;
        end;
      end);
  end;
end;

procedure TVkBotChat.FOnGroupJoin(Sender: TObject; GroupId, UserId: Integer; JoinType: TVkGroupJoinType; const EventId: string);
begin
  if FSkipOtherBotMessages and (UserId < 0) then
    Exit;
  if Assigned(FOnJoin) then
  begin
    TTask.Run(
      procedure
      begin
        FOnJoin(Self, GroupId, UserId, JoinType, EventId);
      end);
  end;
end;

function TVkBotChat.HandleMessageListener(GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo): Boolean;
var
  Listener: TMessageListener;
begin
  for Listener in FMessageListeners do
    if TVkPeerType.Create(Message.PeerId) in Listener.PeerTypes then
    try
      if Listener.Method(Self, GroupId, Message, ClientInfo) then
        Exit(True);
    except
      on E: Exception do
        Console.AddLine('Error with Listener: "' + E.Message + '"', RED);
    end;
  Result := False;
end;

function TVkBotChat.IsAdmin(PeerId, UserId: Integer): Boolean;
var
  Members: TVkConversationMembers;
  Params: TVkParamsConversationMembersGet;
  UserIsAdmin, Found: Boolean;
begin
  UserIsAdmin := False;
  try
    Params.PeerId(PeerId);
    Params.Extended(False);
    Found := False;
    API.Walk(
      function(Offset: Integer; var Cancel: Boolean): Integer
      begin
        Params.Count(200);
        Params.Offset(Offset);
        Result := 0;
        if API.Messages.GetConversationMembers(Members, Params) then
        try
          Result := Length(Members.Items);
          TArrayHelp.Walk<TVkConversationMember>(Members.Items,
            procedure(const Item: TVkConversationMember; Index: Integer; var Cancel: Boolean)
            begin
              if Item.MemberId = UserId then
              begin
                Found := True;
                Cancel := True;
                UserIsAdmin := Item.IsAdmin;
              end;
            end);
          Cancel := Found;
        finally
          Members.Free;
        end;
      end, 200);
    Result := UserIsAdmin;
  except
    Result := False;
  end;
end;

procedure TVkBotChat.FOnNewMessage(Sender: TObject; GroupId: Integer; Message: TVkMessage; ClientInfo: TVkClientInfo; const EventId: string);
var
  FMessage: TVkMessage;
  FClientInfo: TVkClientInfo;
begin
  if FSkipOtherBotMessages and Message.IsBotFrom then
    Exit;
  if Assigned(FOnMessage) or (FMessageListeners.Count > 0) or (FListeners.Count > 0) then
  begin
    FMessage := Message.Clone<TVkMessage>;
    FClientInfo := ClientInfo.Clone<TVkClientInfo>;
    TTask.Run(
      procedure
      begin
        try
          if FMessageListeners.Count > 0 then
            if HandleMessageListener(GroupId, FMessage, FClientInfo) then
              Exit;
          if FListeners.Count > 0 then
            if FListeners.ForEach<TOnGroupMessageNew>(
              function(Event: TEventListiner): Boolean
              begin
                TOnGroupMessageNew((@Event.Method)^)(Self, GroupId, Message, ClientInfo, EventId);
                Result := False;
              end) then
              Exit;
          if Assigned(FOnMessage) then
            FOnMessage(Self, GroupId, FMessage, FClientInfo);
        finally
          FMessage.Free;
          FClientInfo.Free;
        end;
      end);
  end;
end;

class function TVkBotChat.GetInstance(const GroupId: Integer; const Token: string): TVkBotChat;
begin
  if not Assigned(Instance) then
    Instance := TVkBotChat.Create;
  Result := TVkBotChat(Instance);
  Result.GroupId := GroupId;
  Result.Token := Token;
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

{ TEventListiner }

class function TEventListiner.Create(Method, TypeInfo: Pointer): TEventListiner;
begin
  Result.Method := Method;
  Result.TypeInfo := TypeInfo;
end;

{ TEventListeners }

function TEventListeners.ForEach<T>(Proc: TFunc<TEventListiner, Boolean>): Boolean;
var
  Listener: TEventListiner;
begin
  for Listener in Self do
    if Listener.TypeInfo = TypeInfo(T) then
    try
      if Proc(Listener) then
        Exit(True);
    except
      on E: Exception do
        Console.AddLine('Error with Listener: "' + E.Message + '"', RED);
    end;
  Result := False;
end;

end.

