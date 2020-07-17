unit VK.Gifts;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON, VK.Entity.Gift,
  VK.CommonUtils;

type
  TGiftsController = class(TVkController)
  public
    /// <summary>
    /// ���������� ������ ���������� �������� ������������.
    /// </summary>
    function Get(var Items: TVkGiftItems; UserId: Integer; Count: Integer = 10; Offset: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API;

{ TGiftsController }

function TGiftsController.Get(var Items: TVkGiftItems; UserId, Count, Offset: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('user_id', UserId);
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  with Handler.Execute('gifts.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkGiftItems.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

end.

