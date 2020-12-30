unit VK.Orders;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Market;

type
  TVkOrderStateAction = (saCancel, saCharge, saRefund);

  TOrdersController = class(TVkController)
  public
    /// <summary>
    /// Отменяет подписку.
    /// </summary>
    function CancelSubscription(const UserId, SubscriptionId: Integer; PendingCancel: Boolean = False): Boolean;
    /// <summary>
    /// Изменяет состояние заказа.
    /// </summary>
    function OhangeState(var OrderState: string; OrderId: Integer; Action: TVkOrderStateAction; AppOrderId: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TOrdersController }

function TOrdersController.CancelSubscription(const UserId, SubscriptionId: Integer; PendingCancel: Boolean): Boolean;
begin
  with Handler.Execute('orders.cancelSubscription', [['user_id', UserId.ToString], ['subscription_id', SubscriptionId.ToString], ['pending_cancel', BoolToString(PendingCancel)]]) do
    Result := Success and ResponseIsTrue;
end;

function TOrdersController.OhangeState(var OrderState: string; OrderId: Integer; Action: TVkOrderStateAction; AppOrderId: Integer): Boolean;
var
  Params: TParams;
begin
  with Handler.Execute('orders.changeState', Params) do
  begin
    Result := Success;
    if Result then
      OrderState := Response;
  end;
end;

end.

