unit VK.Entity.Keyboard;

interface

uses
  Generics.Collections, Rest.Json, System.Json, VK.Entity.Common;

type
  TVkKeyboardButtonColor = (bcPositive, bcNegative, bcPrimary, bcSecondary);

  TVkKeyboardButtonColorHelper = record helper for TVkKeyboardButtonColor
    function ToString: string; inline;
  end;

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
    FColor: string;
  public
    property Action: TVkKeyboardAction read FAction write FAction;
    property Color: string read FColor write FColor;
    constructor Create; override;
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

destructor TVkKeyboardButton.Destroy;
begin
  FAction.Free;
  inherited;
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

