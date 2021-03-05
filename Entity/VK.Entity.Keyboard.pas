unit VK.Entity.Keyboard;

interface

uses
  Generics.Collections, Rest.Json, System.Json, VK.Types, VK.Entity.Common, VK.Wrap.Interceptors, REST.JsonReflect;

type
  TVkKeyboardAction = class
  private
    FApp_id: Integer;
    FHash: string;
    FLabel: string;
    FPayload: string;
    FOwner_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TKeyboardActionTypeInterceptor)]
    FType: TVkKeyboardActionType;
    FLink: string;
  public
    property AppId: Integer read FApp_id write FApp_id;
    property Hash: string read FHash write FHash;
    property Link: string read FLink write FLink;
    property Payload: string read FPayload write FPayload;
    property&Label: string read FLabel write FLabel;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property&Type: TVkKeyboardActionType read FType write FType;
  end;

  TVkKeyboardButton = class(TVkEntity)
  private
    FAction: TVkKeyboardAction;
    [JsonReflectAttribute(ctString, rtString, TKeyboardButtonColorInterceptor)]
    FColor: TVkKeyboardButtonColor;
  public
    property Action: TVkKeyboardAction read FAction write FAction;
    property Color: TVkKeyboardButtonColor read FColor write FColor;
    constructor Create; override;
    class function CreateOpenLink(const Text, Link, Payload: string): TVkKeyboardButton;
    class function CreateLocation(const Payload: string): TVkKeyboardButton;
    class function CreateVKApps(const AppId: Integer; const Payload, Text, Hash: string; OwnerId: Integer = 0): TVkKeyboardButton;
    class function CreateText(const Text, Payload: string): TVkKeyboardButton;
    class function CreateVKPay(const Payload, Hash: string): TVkKeyboardButton;
    class function CreateCallback(const Text, Payload: string): TVkKeyboardButton;
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

class function TVkKeyboardButton.CreateVKPay;
begin
  Result := TVkKeyboardButton.Create;
  Result.Action.&Type := TVkKeyboardActionType.VKPay;
  Result.Action.Payload := Payload;
  Result.Action.Hash := Hash;
end;

class function TVkKeyboardButton.CreateCallback;
begin
  Result := TVkKeyboardButton.Create;
  Result.Action.&Type := TVkKeyboardActionType.Callback;
  Result.Action.&Label := Text;
  Result.Action.Payload := Payload;
end;

class function TVkKeyboardButton.CreateText;
begin
  Result := TVkKeyboardButton.Create;
  Result.Action.&Type := TVkKeyboardActionType.Text;
  Result.Action.&Label := Text;
  Result.Action.Payload := Payload;
end;

class function TVkKeyboardButton.CreateLocation;
begin
  Result := TVkKeyboardButton.Create;
  Result.Action.&Type := TVkKeyboardActionType.Location;
  Result.Action.Payload := Payload;
end;

class function TVkKeyboardButton.CreateOpenLink;
begin
  Result := TVkKeyboardButton.Create;
  Result.Action.&Type := TVkKeyboardActionType.OpenLink;
  Result.Action.&Label := Text;
  Result.Action.Payload := Payload;
  Result.Action.Link := Link;
end;

class function TVkKeyboardButton.CreateVKApps;
begin
  Result := TVkKeyboardButton.Create;
  Result.Action.&Type := TVkKeyboardActionType.OpenApp;
  Result.Action.&Label := Text;
  Result.Action.Payload := Payload;
  Result.Action.Hash := Hash;
  Result.Action.AppId := AppId;
  Result.Action.OwnerId := OwnerId;
end;

destructor TVkKeyboardButton.Destroy;
begin
  FAction.Free;
  inherited;
end;

end.

