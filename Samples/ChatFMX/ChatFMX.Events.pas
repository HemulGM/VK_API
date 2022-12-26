unit ChatFMX.Events;

interface

uses
  System.Classes, VK.Types, System.Messaging, VK.UserEvents;

type
  TEventUserStatus = class(TMessage)
  private
    FUserId: TVkPeerId;
    FIsOnline: Boolean;
    FVkPlatform: TVkPlatform;
  public
    property UserId: TVkPeerId read FUserId;
    property IsOnline: Boolean read FIsOnline;
    property VkPlatform: TVkPlatform read FVkPlatform;
    constructor Create(AUserId: TVkPeerId; AIsOnline: Boolean; AVkPlatform: TVkPlatform = TVkPlatform.Unknown); reintroduce;
  end;

  TEventMessage = class(TMessage)
    Data: TMessageData;
    constructor Create(MessageData: TMessageData); reintroduce;
  end;

  TEventNewMessage = class(TEventMessage);

  TEventEditMessage = class(TEventMessage);

  TEventReadMessages = class(TMessage)
    Incoming: Boolean;
    PeerId, LocalId: TVkPeerId;
    constructor Create(AIncoming: Boolean; APeerId, ALocalId: TVkPeerId); reintroduce;
  end;

  Event = class
  private
    class var
      FManager: TMessageManager;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Send(Message: TMessage);
    class function Subscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer; overload;
    class procedure Unsubscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod; Immediate: Boolean = False); overload;
  end;

implementation

{ Event }

class constructor Event.Create;
begin
  FManager := TMessageManager.Create;
end;

class destructor Event.Destroy;
begin
  FManager.Free;
end;

class procedure Event.Send(Message: TMessage);
begin
  TThread.Queue(nil,
    procedure
    begin
      FManager.SendMessage(FManager, Message, True);
    end);
end;

class function Event.Subscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod): Integer;
var
  Res: Integer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Res := FManager.SubscribeToMessage(AMessageClass, AListenerMethod);
    end);
  Result := Res;
end;

class procedure Event.Unsubscribe(const AMessageClass: TClass; const AListenerMethod: TMessageListenerMethod; Immediate: Boolean);
begin
  TThread.Queue(nil,
    procedure
    begin
      FManager.Unsubscribe(AMessageClass, AListenerMethod, Immediate);
    end);
end;

{ TEventUserStatus }

constructor TEventUserStatus.Create(AUserId: TVkPeerId; AIsOnline: Boolean; AVkPlatform: TVkPlatform);
begin
  inherited Create;
  FUserId := AUserId;
  FIsOnline := AIsOnline;
  FVkPlatform := AVkPlatform;
end;

{ TEventMessage }

constructor TEventMessage.Create(MessageData: TMessageData);
begin
  inherited Create;
  Data := MessageData;
end;

{ TEventReadMessages }

constructor TEventReadMessages.Create(AIncoming: Boolean; APeerId, ALocalId: TVkPeerId);
begin
  inherited Create;
  Incoming := AIncoming;
  PeerId := APeerId;
  LocalId := ALocalId;
end;

end.

