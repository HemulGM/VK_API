unit VK.Entity.Keyboard;

interface

uses
  Generics.Collections, Rest.Json, System.Json, VK.Types, VK.Entity.Common,
  VK.Params, VK.Wrap.Interceptors, Rest.JsonReflect;

type
  TVkKeyboardAction = class
  private
    FApp_id: Integer;
    FHash: string;
    FLabel: string;
    FPayload: string;
    FOwner_id: TVkPeerId;
    [JsonReflectAttribute(ctString, rtString, TKeyboardActionTypeInterceptor)]
    FType: TVkKeyboardActionType;
    FLink: string;
  public
    property AppId: Integer read FApp_id write FApp_id;
    property Hash: string read FHash write FHash;
    property Link: string read FLink write FLink;
    property Payload: string read FPayload write FPayload;
    property &Label: string read FLabel write FLabel;
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property &Type: TVkKeyboardActionType read FType write FType;
  end;

  TVkKeyboardButton = class(TVkEntity)
  private
    FAction: TVkKeyboardAction;
    [JsonReflectAttribute(ctString, rtString, TKeyboardButtonColorInterceptor)]
    FColor: TVkKeyboardButtonColor;
  public
    property Action: TVkKeyboardAction read FAction write FAction;
    property Color: TVkKeyboardButtonColor read FColor write FColor;
    destructor Destroy; override;
  end;

  TVkKeyboardButtons = TArray<TVkKeyboardButton>;

  TVkKeyboardActionConstruct = class(TJSONParam)
  public
    procedure AppId(const Value: Integer);
    procedure Hash(const Value: string);
    procedure Link(const Value: string);
    procedure Payload(const Value: string);
    procedure &Label(const Value: string);
    procedure OwnerId(const Value: TVkPeerId);
    procedure &Type(const Value: TVkKeyboardActionType);
  end;

  TVkKeyboardButtonConstruct = class(TJSONParam)
  private
    FAction: TJSONObject;
    function GetAction: TJSONObject;
  public
    class function CreateOpenLink(const Text, Link, Payload: string): TVkKeyboardButtonConstruct;
    class function CreateLocation(const Payload: string): TVkKeyboardButtonConstruct;
    class function CreateVKApps(const AppId: Integer; const Payload, Text, Hash: string; OwnerId: TVkPeerId = 0): TVkKeyboardButtonConstruct;
    class function CreateText(const Text, Payload: string; Color: TVkKeyboardButtonColor): TVkKeyboardButtonConstruct;
    class function CreateVKPay(const Payload, Hash: string): TVkKeyboardButtonConstruct;
    class function CreateCallback(const Text, Payload: string; Color: TVkKeyboardButtonColor): TVkKeyboardButtonConstruct;
    procedure Color(const Value: TVkKeyboardButtonColor);
    property Action: TJSONObject read GetAction;
  end;

  TVkKeyboardButtonsConstruct = TArray<TVkKeyboardButtonConstruct>;

  TVkKeyboardConstruct = class;

  TVkKeyboardButtonLine = record
    Construct: TVkKeyboardConstruct;
    Index: Integer;
    function AddButton(const Button: TVkKeyboardButtonConstruct): Integer;
  end;

  TVkKeyboardConstruct = class(TJSONParam)
  private
    FButtons: TArray<TVkKeyboardButtonsConstruct>;
  public
    procedure OneTime(const Value: Boolean);
    procedure InlineKeys(const Value: Boolean);
    function AddButtonLine: Integer;
    function AddLine: TVkKeyboardButtonLine;
    function AddButtonToLine(const Index: Integer; const Button: TVkKeyboardButtonConstruct): Integer;
    property Buttons: TArray<TVkKeyboardButtonsConstruct> read FButtons write FButtons;
    function ToJsonString(FreeObject: Boolean = False): string; override;
    destructor Destroy; override;
  end;

  TVkKeyboard = class(TVkEntity)
  private
    FButtons: TArray<TVkKeyboardButtons>;
    FOne_time: Boolean;
    FInline: Boolean;
    FAuthor_id: TVkPeerId;
  public
    function AddButtonLine: Integer;
    function AddButtonToLine(const Index: Integer; const Button: TVkKeyboardButton): Integer;
    property Buttons: TArray<TVkKeyboardButtons> read FButtons write FButtons;
    property OneTime: Boolean read FOne_time write FOne_time;
    property &Inline: Boolean read FInline write FInline;
    property AuthorId: TVkPeerId read FAuthor_id write FAuthor_id;
    class function Construct: TVkKeyboardConstruct;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkKeyboard }

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

class function TVkKeyboard.Construct: TVkKeyboardConstruct;
begin
  Result := TVkKeyboardConstruct.Create;
end;

destructor TVkKeyboard.Destroy;
begin
  TArrayHelp.FreeArrayOfArrayOfObject<TVkKeyboardButton>(FButtons);
  inherited;
end;

{ TVkKeyboardButton }

destructor TVkKeyboardButton.Destroy;
begin
  if Assigned(FAction) then
    FAction.Free;
  inherited;
end;

{ TVkKeyboardButtonConstruct }

function TVkKeyboardButtonConstruct.GetAction: TJSONObject;
begin
  if not Assigned(FAction) then
  begin
    FAction := TJSONObject.Create;
    Add('action', FAction);
  end;
  Result := FAction;
end;

procedure TVkKeyboardButtonConstruct.Color(const Value: TVkKeyboardButtonColor);
begin
  Add('color', Value.ToString);
end;

class function TVkKeyboardButtonConstruct.CreateVKPay;
begin
  Result := TVkKeyboardButtonConstruct.Create;
  Result.Action.AddPair('type', TVkKeyboardActionType.VKPay.ToString);
  Result.Action.AddPair('payload', Payload);
  Result.Action.AddPair('hash', Hash);
end;

class function TVkKeyboardButtonConstruct.CreateCallback;
begin
  Result := TVkKeyboardButtonConstruct.Create;
  Result.Action.AddPair('type', TVkKeyboardActionType.Callback.ToString);
  Result.Action.AddPair('label', Text);
  Result.Action.AddPair('payload', Payload);
  Result.Color(Color);
end;

class function TVkKeyboardButtonConstruct.CreateText;
begin
  Result := TVkKeyboardButtonConstruct.Create;
  Result.Action.AddPair('type', TVkKeyboardActionType.Text.ToString);
  Result.Action.AddPair('label', Text);
  Result.Action.AddPair('payload', Payload);
  Result.Color(Color);
end;

class function TVkKeyboardButtonConstruct.CreateLocation;
begin
  Result := TVkKeyboardButtonConstruct.Create;
  Result.Action.AddPair('type', TVkKeyboardActionType.Location.ToString);
  Result.Action.AddPair('payload', Payload);
end;

class function TVkKeyboardButtonConstruct.CreateOpenLink;
begin
  Result := TVkKeyboardButtonConstruct.Create;
  Result.Action.AddPair('type', TVkKeyboardActionType.OpenLink.ToString);
  Result.Action.AddPair('label', Text);
  Result.Action.AddPair('payload', Payload);
  Result.Action.AddPair('link', Link);
end;

class function TVkKeyboardButtonConstruct.CreateVKApps;
begin
  Result := TVkKeyboardButtonConstruct.Create;
  Result.Action.AddPair('type', TVkKeyboardActionType.OpenApp.ToString);
  Result.Action.AddPair('label', Text);
  Result.Action.AddPair('payload', Payload);
  Result.Action.AddPair('hash', Hash);
  Result.Action.AddPair('app_id', TJSONNumber.Create(AppId));
  Result.Action.AddPair('owner_id', TJSONNumber.Create(OwnerId));
end;

{ TVkKeyboardConstruct }

function TVkKeyboardConstruct.ToJsonString(FreeObject: Boolean): string;
var
  Items, Item: TJSONArray;
  Btns: TVkKeyboardButtonsConstruct;
  Btn: TVkKeyboardButtonConstruct;
begin
  if Length(FButtons) > 0 then
  begin
    Items := TJSONArray.Create;
    for Btns in FButtons do
    begin
      Item := TJSONArray.Create;
      for Btn in Btns do
        Item.AddElement(TJSONObject(Btn.Json.Clone));
      Items.AddElement(Item);
    end;
    Add('buttons', Items);
    Result := inherited;
    Delete('buttons');
  end
  else
    Result := inherited;
end;

function TVkKeyboardConstruct.AddButtonLine: Integer;
begin
  SetLength(FButtons, Length(FButtons) + 1);
  Result := High(FButtons);
end;

function TVkKeyboardConstruct.AddLine: TVkKeyboardButtonLine;
begin
  SetLength(FButtons, Length(FButtons) + 1);
  Result.Index := High(FButtons);
  Result.Construct := Self;
end;

function TVkKeyboardConstruct.AddButtonToLine(const Index: Integer; const Button: TVkKeyboardButtonConstruct): Integer;
begin
  SetLength(FButtons[Index], Length(FButtons[Index]) + 1);
  Result := High(FButtons[Index]);
  FButtons[Index][Result] := Button;
end;

destructor TVkKeyboardConstruct.Destroy;
begin
  TArrayHelp.FreeArrayOfArrayOfObject<TVkKeyboardButtonConstruct>(FButtons);
  inherited;
end;

procedure TVkKeyboardConstruct.InlineKeys(const Value: Boolean);
begin
  Add('inline', Value);
end;

procedure TVkKeyboardConstruct.OneTime(const Value: Boolean);
begin
  Add('one_time', Value);
end;

{ TVkKeyboardActionConstruct }

procedure TVkKeyboardActionConstruct.AppId(const Value: Integer);
begin
  Add('app_id', Value);
end;

procedure TVkKeyboardActionConstruct.Hash(const Value: string);
begin
  Add('hash', Value);
end;

procedure TVkKeyboardActionConstruct.&Label(const Value: string);
begin
  Add('label', Value);
end;

procedure TVkKeyboardActionConstruct.Link(const Value: string);
begin
  Add('link', Value);
end;

procedure TVkKeyboardActionConstruct.OwnerId(const Value: TVkPeerId);
begin
  Add('owner_id', Value);
end;

procedure TVkKeyboardActionConstruct.Payload(const Value: string);
begin
  Add('payload', Value);
end;

procedure TVkKeyboardActionConstruct.&Type(const Value: TVkKeyboardActionType);
begin
  Add('type', Value.ToString);
end;

{ TVkKeyboardButtonLine }

function TVkKeyboardButtonLine.AddButton(const Button: TVkKeyboardButtonConstruct): Integer;
begin
  Result := Construct.AddButtonToLine(Index, Button);
end;

end.

