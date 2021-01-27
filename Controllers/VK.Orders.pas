unit VK.Orders;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Market;

type
  TVkOrderStateAction = (saCancel, saCharge, saRefund);

  TVkOrderStateActionHelper = record helper for TVkOrderStateAction
    function ToString: string; inline;
  end;

  TOrdersController = class(TVkController)
  public
    /// <summary>
    /// Отменяет подписку.
    /// </summary>
    function CancelSubscription(var Status: Boolean; const UserId, SubscriptionId: Integer; PendingCancel: Boolean = False): Boolean;
    /// <summary>
    /// Изменяет состояние заказа.
    /// </summary>
    function ChangeState(var OrderState: string; OrderId: Integer; Action: TVkOrderStateAction; AppOrderId: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TOrdersController }

function TOrdersController.CancelSubscription(var Status: Boolean; const UserId, SubscriptionId: Integer; PendingCancel: Boolean): Boolean;
begin
  Result := Handler.Execute('orders.cancelSubscription', [
    ['user_id', UserId.ToString],
    ['subscription_id', SubscriptionId.ToString],
    ['pending_cancel', BoolToString(PendingCancel)]]).
    ResponseAsBool(Status);
end;

function TOrdersController.ChangeState(var OrderState: string; OrderId: Integer; Action: TVkOrderStateAction; AppOrderId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('order_id', OrderId);
  Params.Add('action', Action.ToString);
  Params.Add('app_order_id', AppOrderId);
  Result := Handler.Execute('orders.changeState', Params).ResponseAsStr(OrderState);
end;

{ TVkOrderStateActionHelper }

function TVkOrderStateActionHelper.ToString: string;
begin
  case Self of
    saCancel:
      Exit('cancel');
    saCharge:
      Exit('charge');
    saRefund:
      Exit('refund');
  end;
end;

end.

