unit VK.Entity.PushSettings;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkConversation = class
  private
    FDisabled_until: Extended;
    FPeer_id: Extended;
    FSound: Extended;
  public
    property disabled_until: Extended read FDisabled_until write FDisabled_until;
    property peer_id: Extended read FPeer_id write FPeer_id;
    property sound: Extended read FSound write FSound;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversation;
  end;

  TVkConversations = class
  private
    FCount: Extended;
    FItems: TArray<TVkConversation>;
  public
    property count: Extended read FCount write FCount;
    property items: TArray<TVkConversation> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkConversations;
  end;

  TVkPushSettings = class
  private
    FConversations: TVkConversations;
    FDisabled: Extended;
  public
    property conversations: TVkConversations read FConversations write FConversations;
    property disabled: Extended read FDisabled write FDisabled;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPushSettings;
  end;

implementation

{TVkConversation}

function TVkConversation.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkConversation.FromJsonString(AJsonString: string): TVkConversation;
begin
  result := TJson.JsonToObject<TVkConversation>(AJsonString)
end;

{TVkConversations}

destructor TVkConversations.Destroy;
var
  LitemsItem: TVkConversation;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkConversations.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkConversations.FromJsonString(AJsonString: string): TVkConversations;
begin
  result := TJson.JsonToObject<TVkConversations>(AJsonString)
end;

{TVkPushSettings}

constructor TVkPushSettings.Create;
begin
  inherited;
  FConversations := TVkConversations.Create();
end;

destructor TVkPushSettings.Destroy;
begin
  FConversations.Free;
  inherited;
end;

function TVkPushSettings.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPushSettings.FromJsonString(AJsonString: string): TVkPushSettings;
begin
  result := TJson.JsonToObject<TVkPushSettings>(AJsonString)
end;

end.

