unit VK.Entity.MoneyTransfer;

interface

uses
  VK.Entity.Common, VK.Types, REST.JsonReflect, REST.Json.Interceptors;

type
  TMoneyAmount = class
  private
    FAmount: string;
    FCurrency: TVkProductCurrency;
    FText: string;
  public
    property Amount: string read FAmount write FAmount;
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
  end;

  TVkMoneyTransfer = class(TVkEntity)
  private
    FAmount: TMoneyAmount;
    FBy_Phone: Boolean;
    FComment: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_Id: Integer;
    FId: Integer;
    FIs_Anonymous: Boolean;
    FIs_Vkpay: Boolean;
    FStatus: Integer;
    FTo_Id: Integer;
  public
    property Amount: TMoneyAmount read FAmount write FAmount;
    property ByPhone: Boolean read FBy_Phone write FBy_Phone;
    property Comment: string read FComment write FComment;
    property Date: TDateTime read FDate write FDate;
    property FromId: Integer read FFrom_Id write FFrom_Id;
    property Id: Integer read FId write FId;
    property IsAnonymous: Boolean read FIs_Anonymous write FIs_Anonymous;
    property IsVkpay: Boolean read FIs_Vkpay write FIs_Vkpay;
    property Status: Integer read FStatus write FStatus;
    property ToId: Integer read FTo_Id write FTo_Id;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TMoneyAmount }

constructor TMoneyAmount.Create;
begin
  inherited;
  FCurrency := TVkProductCurrency.Create;
end;

destructor TMoneyAmount.Destroy;
begin
  FCurrency.Free;
  inherited;
end;

{ TVkMoneyTransfer }

constructor TVkMoneyTransfer.Create;
begin
  inherited;
  FAmount := TMoneyAmount.Create;
end;

destructor TVkMoneyTransfer.Destroy;
begin
  FAmount.Free;
  inherited;
end;

end.

