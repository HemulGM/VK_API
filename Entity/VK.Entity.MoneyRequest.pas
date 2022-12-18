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
  public
    property Amount: TVkMoneyAmount read FAmount write FAmount;
    property FromId: TVkPeerId read FFrom_Id write FFrom_Id;
    property Id: Integer read FId write FId;
    property ToId: TVkPeerId read FTo_Id write FTo_Id;
    property Processed: Integer read FProcessed write FProcessed;
    property UserIsOwner: Boolean read FUser_is_owner write FUser_is_owner;
    destructor Destroy; override;
  end;

implementation

{ TVkMoneyRequest }

destructor TVkMoneyRequest.Destroy;
begin
  if Assigned(FAmount) then
    FAmount.Free;
  inherited;
end;

end.

