unit VK.Entity.Keyboard;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkKeyboardAction = class
  private
    FApp_id: Extended;
    FHash: string;
    FLabel: string;
    FOwner_id: Extended;
    FType: string;
  public
    property app_id: Extended read FApp_id write FApp_id;
    property hash: string read FHash write FHash;
    property&label: string read FLabel write FLabel;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property&type: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkKeyboardAction;
  end;

  TVkKeyboardButton = class
  private
    FAction: TVkKeyboardAction;
    FColor: string;
  public
    property action: TVkKeyboardAction read FAction write FAction;
    property color: string read FColor write FColor;
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
  public
    property buttons: TArray<TVkKeyboardButtons> read FButtons write FButtons;
    property one_time: Boolean read FOne_time write FOne_time;
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

{TVkKeyboard}

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
  action := TVkKeyboardAction.Create;
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

end.

