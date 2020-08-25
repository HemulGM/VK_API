unit VK.Entity.Keyboard;

interface

uses
  Generics.Collections, Rest.Json, System.Json;

type
  TVkKeyboardButtonColor = (bcPositive, bcNegative, bcPrimary, bcSecondary);

  TVkKeyboardButtonColorHelper = record helper for TVkKeyboardButtonColor
    function ToString: string; inline;
  end;

  TVkKeyboardConstructor = record
    FList: TArray<TArray<string>>;
    FOneTime: Boolean;
    FInline: Boolean;
    procedure AddButtonText(Group: Integer; Caption, Payload, Color: string); overload;
    procedure AddButtonText(Group: Integer; Caption, Payload: string; Color: TVkKeyboardButtonColor); overload;
    procedure AddButtonOpenLink(Group: Integer; Link, Caption, Payload: string);
    procedure SetOneTime(Value: Boolean);
    procedure SetInline(Value: Boolean);
    function ToJsonString: string;
  end;

  TVkPayloadButton = class
  private
    FButton: string;
  public
    property Button: string read FButton write FButton;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPayloadButton;
  end;

  TVkKeyboardAction = class
  private
    FApp_id: Extended;
    FHash: string;
    FLabel: string;
    FPayload: string;
    FOwner_id: Extended;
    FType: string;  //text, open_link, location, vkpay, open_app
    FLink: string;
  public
    property AppId: Extended read FApp_id write FApp_id;
    property Hash: string read FHash write FHash;
    property Link: string read FLink write FLink;
    property Payload: string read FPayload write FPayload;
    property&Label: string read FLabel write FLabel;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property&Type: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkKeyboardAction;
  end;

  TVkKeyboardButton = class
  private
    FAction: TVkKeyboardAction;
    FColor: string;
  public
    property Action: TVkKeyboardAction read FAction write FAction;
    property Color: string read FColor write FColor;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkKeyboardButton;
  end;

  TVkKeyboardButtons = TArray<TVkKeyboardButton>;

  TVkKeyboard = class
  private
    FButtons: TArray<TVkKeyboardButtons>;
    FOne_time: Boolean;
    FInline: Boolean;
    FAuthor_id: Integer;
  public
    property Buttons: TArray<TVkKeyboardButtons> read FButtons write FButtons;
    property OneTime: Boolean read FOne_time write FOne_time;
    property&Inline: Boolean read FInline write FInline;
    property AuthorId: Integer read FAuthor_id write FAuthor_id;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkKeyboard;
  end;

implementation

{TVkKeyboardAction}

function TVkKeyboardAction.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkKeyboardAction.FromJsonString(AJsonString: string): TVkKeyboardAction;
begin
  result := TJson.JsonToObject<TVkKeyboardAction>(AJsonString)
end;

destructor TVkKeyboard.Destroy;
var
  LbuttonsItem: TVkKeyboardButtons;
  LbuttonItem: TVkKeyboardButton;
begin

  for LbuttonsItem in FButtons do
    for LbuttonItem in LbuttonsItem do
      LbuttonItem.Free;

  inherited;
end;

function TVkKeyboard.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkKeyboard.FromJsonString(AJsonString: string): TVkKeyboard;
begin
  result := TJson.JsonToObject<TVkKeyboard>(AJsonString);
end;

{ TVkKeyboardButton }

constructor TVkKeyboardButton.Create;
begin
  inherited;
  Action := TVkKeyboardAction.Create;
end;

destructor TVkKeyboardButton.Destroy;
begin
  FAction.Free;
  inherited;
end;

class function TVkKeyboardButton.FromJsonString(AJsonString: string): TVkKeyboardButton;
begin
  result := TJson.JsonToObject<TVkKeyboardButton>(AJsonString);
end;

function TVkKeyboardButton.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkKeyboardConstructor }

procedure TVkKeyboardConstructor.AddButtonOpenLink(Group: Integer; Link, Caption, Payload: string);
begin

end;

procedure TVkKeyboardConstructor.AddButtonText(Group: Integer; Caption, Payload, Color: string);
var
  JS, Act: TJSONObject;
begin
  while Length(FList) < Group + 1 do
    SetLength(FList, Group + 1);
  SetLength(FList[Group], Length(FList[Group]) + 1);

  Act := TJSONObject.Create;
  Act.AddPair('label', Caption);
  Act.AddPair('payload', '{"button":"' + Payload + '"}');
  Act.AddPair('type', 'text');

  JS := TJSONObject.Create;
  JS.AddPair('action', Act);
  JS.AddPair('color', Color);
  FList[Group][Length(FList[Group]) - 1] := JS.ToJSON;
  JS.Free;
end;

procedure TVkKeyboardConstructor.AddButtonText(Group: Integer; Caption, Payload: string; Color: TVkKeyboardButtonColor);
begin
  AddButtonText(Group, Caption, Payload, Color.ToString);
end;

procedure TVkKeyboardConstructor.SetInline(Value: Boolean);
begin
  FInline := Value;
end;

procedure TVkKeyboardConstructor.SetOneTime(Value: Boolean);
begin
  FOneTime := Value;
end;

function TVkKeyboardConstructor.ToJsonString: string;
var
  FButtons: string;
  i, j: Integer;
begin
  FButtons := '';
  for i := Low(FList) to High(FList) do
  begin
    if FButtons <> '' then
      FButtons := FButtons + ',';
    FButtons := FButtons + '[';
    for j := Low(FList[i]) to High(FList[i]) do
    begin
      if j <> Low(FList[i]) then
        FButtons := FButtons + ',';
      FButtons := FButtons + FList[i][j];
    end;
    FButtons := FButtons + ']';
  end;
  Result := '{"buttons": [' + FButtons + ']';
  if FOneTime then
    Result := Result + ',"one_time": true'
  else
    Result := Result + ',"one_time": false';
  if FInline then
    Result := Result + ',"inline": true'
  else
    Result := Result + ',"inline": false';
  Result := Result + '}'
end;

{ TVkPayloadButton }

class function TVkPayloadButton.FromJsonString(AJsonString: string): TVkPayloadButton;
begin
  result := TJson.JsonToObject<TVkPayloadButton>(AJsonString);
end;

function TVkPayloadButton.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkKeyboardButtonColorHelper }

function TVkKeyboardButtonColorHelper.ToString: string;
begin
  case Self of
    bcPositive:
      Result := 'positive';
    bcNegative:
      Result := 'negative';
    bcPrimary:
      Result := 'primary';
    bcSecondary:
      Result := 'secondary';
  else
    Result := ''
  end;
end;

end.

