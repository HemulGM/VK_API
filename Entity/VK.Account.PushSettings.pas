unit VK.Account.PushSettings;

interface

uses
  Generics.Collections, Rest.Json;

type
  TItemsClass = class
  private
    FDisabled_until: Extended;
    FPeer_id: Extended;
    FSound: Extended;
  public
    property disabled_until: Extended read FDisabled_until write FDisabled_until;
    property peer_id: Extended read FPeer_id write FPeer_id;
    property sound: Extended read FSound write FSound;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TItemsClass;
  end;

  TConversationsClass = class
  private
    FCount: Extended;
    FItems: TArray<TItemsClass>;
  public
    property count: Extended read FCount write FCount;
    property items: TArray<TItemsClass> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TConversationsClass;
  end;

  TPushSettingsClass = class
  private
    FConversations: TConversationsClass;
    FDisabled: Extended;
  public
    property conversations: TConversationsClass read FConversations write FConversations;
    property disabled: Extended read FDisabled write FDisabled;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TPushSettingsClass;
  end;

implementation

{TItemsClass}

function TItemsClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TItemsClass.FromJsonString(AJsonString: string): TItemsClass;
begin
  result := TJson.JsonToObject<TItemsClass>(AJsonString)
end;

{TConversationsClass}

destructor TConversationsClass.Destroy;
var
  LitemsItem: TItemsClass;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TConversationsClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TConversationsClass.FromJsonString(AJsonString: string): TConversationsClass;
begin
  result := TJson.JsonToObject<TConversationsClass>(AJsonString)
end;

{TPushSettingsClass}

constructor TPushSettingsClass.Create;
begin
  inherited;
  FConversations := TConversationsClass.Create();
end;

destructor TPushSettingsClass.Destroy;
begin
  FConversations.Free;
  inherited;
end;

function TPushSettingsClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TPushSettingsClass.FromJsonString(AJsonString: string): TPushSettingsClass;
begin
  result := TJson.JsonToObject<TPushSettingsClass>(AJsonString)
end;

end.

