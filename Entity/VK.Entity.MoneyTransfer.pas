unit VK.Entity.MoneyTransfer;

interface

uses
  VK.Entity.Common, VK.Types, REST.JsonReflect, REST.Json.Interceptors;

type
  TVkMoneyAmount = class
  private
    FAmount: string;
    FCurrency: TVkProductCurrency;
    FText: string;
  public
    property Amount: string read FAmount write FAmount;
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Text: string read FText write FText;
    destructor Destroy; override;
  end;

  TVkMoneyTransfer = class(TVkEntity)
  private
    FAmount: TVkMoneyAmount;
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
    property Amount: TVkMoneyAmount read FAmount write FAmount;
    property ByPhone: Boolean read FBy_Phone write FBy_Phone;
    property Comment: string read FComment write FComment;
    property Date: TDateTime read FDate write FDate;
    property FromId: Integer read FFrom_Id write FFrom_Id;
    property Id: Integer read FId write FId;
    property IsAnonymous: Boolean read FIs_Anonymous write FIs_Anonymous;
    property IsVkpay: Boolean read FIs_Vkpay write FIs_Vkpay;
    property Status: Integer read FStatus write FStatus;
    property ToId: Integer read FTo_Id write FTo_Id;
    destructor Destroy; override;
  end;

implementation

{ TVkMoneyAmount }

destructor TVkMoneyAmount.Destroy;
begin
  if Assigned(FCurrency) then
    FCurrency.Free;
  inherited;
end;

{ TVkMoneyTransfer }

destructor TVkMoneyTransfer.Destroy;
begin
  if Assigned(FAmount) then
    FAmount.Free;
  inherited;
end;

end.

