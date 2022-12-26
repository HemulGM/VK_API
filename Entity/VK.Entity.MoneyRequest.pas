unit VK.Entity.MoneyRequest;

interface

uses
  VK.Entity.Common, VK.Types, REST.JsonReflect, REST.Json.Interceptors,
  Vk.Wrap.Interceptors;

type
  TVkMoneyRequest = class(TVkEntity)
  private
    FAmount: TVkMoneyAmount;
    FFrom_Id: TVkPeerId;
    FId: Integer;
    FTo_Id: TVkPeerId;
    FProcessed: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FUser_is_owner: Boolean;
    FInit_url: string;
    FTotal_amount: TVkMoneyAmount;
    FTransferred_amount: TVkMoneyAmount;
    FHeld_amount: TVkMoneyAmount;
    FUsers_count: Integer;
    FUser_sent: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_accept_vkpay_only: Boolean;
    FUsers: TArray<TVkPeerId>;
    FReceive_method: string;
  public
    property Amount: TVkMoneyAmount read FAmount write FAmount;
    property FromId: TVkPeerId read FFrom_Id write FFrom_Id;
    property Id: Integer read FId write FId;
    property ToId: TVkPeerId read FTo_Id write FTo_Id;
    property Processed: Integer read FProcessed write FProcessed;
    property UserIsOwner: Boolean read FUser_is_owner write FUser_is_owner;
    property InitUrl: string read FInit_url write FInit_url;
    property TotalAmount: TVkMoneyAmount read FTotal_amount write FTotal_amount;
    property TransferredAmount: TVkMoneyAmount read FTransferred_amount write FTransferred_amount;
    property HeldAmount: TVkMoneyAmount read FHeld_amount write FHeld_amount;
    property UsersCount: Integer read FUsers_count write FUsers_count;
    property UserSent: Integer read FUser_sent write FUser_sent;
    property IsAcceptVkpayOnly: Boolean read FIs_accept_vkpay_only write FIs_accept_vkpay_only;
    property Users: TArray<TVkPeerId> read FUsers write FUsers;
    /// card
    property ReceiveMethod: string read FReceive_method write FReceive_method;
    destructor Destroy; override;
  end;

implementation

{ TVkMoneyRequest }

destructor TVkMoneyRequest.Destroy;
begin
  if Assigned(FAmount) then
    FAmount.Free;
  if Assigned(FTotal_amount) then
    FTotal_amount.Free;
  if Assigned(FTransferred_amount) then
    FTransferred_amount.Free;
  if Assigned(FHeld_amount) then
    FHeld_amount.Free;
  inherited;
end;

end.

