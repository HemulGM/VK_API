unit VK.Entity.Keyboard;

interface

uses
  Generics.Collections, Rest.Json, System.Json, VK.Types, VK.Entity.Common, VK.Wrap.Interceptors, REST.JsonReflect;

type
  TVkPayloadButton = class(TVkEntity)
  private
    FButton: string;
  public
    property Button: string read FButton write FButton;
  end;

  TVkKeyboardAction = class
  private
    FApp_id: Integer;
    FHash: string;
    FLabel: string;
    FPayload: string;
    FOwner_id: Integer;
    FType: string;  //text, open_link, location, vkpay, open_app
    FLink: string;
  public
    property AppId: Integer read FApp_id write FApp_id;
    property Hash: string read FHash write FHash;
    property Link: string read FLink write FLink;
    property Payload: string read FPayload write FPayload;
    property&Label: string read FLabel write FLabel;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property&Type: string read FType write FType;
  end;

  TVkKeyboardButton = class(TVkEntity)
  private
    FAction: TVkKeyboardAction;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FColor: TVkKeyboardButtonColor;
  public
    property Action: TVkKeyboardAction read FAction write FAction;
    property Color: TVkKeyboardButtonColor read FColor write FColor;
    constructor Create; override;
    constructor CreateText(const Text, Payload: string);
    constructor CreateOpenLink(const Text, Link, Payload: string);
    constructor CreateLocation(const Payload: string);
    constructor CreateVKApps(const AppId: Integer; const Payload, Text, Hash: string; OwnerId: Integer = 0);
    {$WARNINGS OFF}
    constructor CreateVKPay(const Payload, Hash: string);
    constructor CreateCallback(const Text, Payload: string);
    {$WARNINGS ON}
    destructor Destroy; override;
  end;

  TVkKeyboardButtons = TArray<TVkKeyboardButton>;

  TVkKeyboard = class(TVkEntity)
  private
    FButtons: TArray<TVkKeyboardButtons>;
    FOne_time: Boolean;
    FInline: Boolean;
    FAuthor_id: Integer;
  public
    function AddButtonLine: Integer;
    function AddButtonToLine(const Index: Integer; const Button: TVkKeyboardButton): Integer;
    property Buttons: TArray<TVkKeyboardButtons> read FButtons write FButtons;
    property OneTime: Boolean read FOne_time write FOne_time;
    property&Inline: Boolean read FInline write FInline;
    property AuthorId: Integer read FAuthor_id write FAuthor_id;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkKeyboardAction}

function TVkKeyboard.AddButtonLine: Integer;
begin
  SetLength(FButtons, Length(FButtons) + 1);
  Result := High(FButtons);
end;

function TVkKeyboard.AddButtonToLine(const Index: Integer; const Button: TVkKeyboardButton): Integer;
begin
  SetLength(FButtons[Index], Length(FButtons[Index]) + 1);
  Result := High(FButtons[Index]);
  FButtons[Index][Result] := Button;
end;

destructor TVkKeyboard.Destroy;
begin
  TArrayHelp.FreeArrayOfArrayOfObject<TVkKeyboardButton>(FButtons);
  inherited;
end;

{ TVkKeyboardButton }

constructor TVkKeyboardButton.Create;
begin
  inherited;
  Action := TVkKeyboardAction.Create;
end;

{$WARNINGS OFF}
constructor TVkKeyboardButton.CreateVKPay;
begin
  Create;
  Action.&Type := 'vkpay';
  Action.Payload := Payload;
  Action.Hash := Hash;
end;

constructor TVkKeyboardButton.CreateCallback;
begin
  Create;
  Action.&Type := 'callback';
  Action.&Label := Text;
  Action.Payload := Payload;
end;
{$WARNINGS ON}

constructor TVkKeyboardButton.CreateLocation;
begin
  Create;
  Action.&Type := 'location';
  Action.Payload := Payload;
end;

constructor TVkKeyboardButton.CreateOpenLink;
begin
  Create;
  Action.&Type := 'open_link';
  Action.&Label := Text;
  Action.Payload := Payload;
  Action.Link := Link;
end;

constructor TVkKeyboardButton.CreateText;
begin
  Create;
  Action.&Type := 'text';
  Action.&Label := Text;
  Action.Payload := Payload;
end;

constructor TVkKeyboardButton.CreateVKApps;
begin
  Create;
  Action.&Type := 'open_app';
  Action.&Label := Text;
  Action.Payload := Payload;
  Action.Hash := Hash;
  Action.AppId := AppId;
  Action.OwnerId := OwnerId;
end;

destructor TVkKeyboardButton.Destroy;
begin
  FAction.Free;
  inherited;
end;

end.

