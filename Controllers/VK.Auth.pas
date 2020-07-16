unit VK.Auth;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types;

type
  TAuthController = class(TVkController)
  public
    /// <summary>
    /// ѕровер€ет правильность введЄнного номера (возможность его использовани€ дл€ регистрации или авторизации).
    /// </summary>
    function CheckPhone(Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean = False): Boolean; overload;
    /// <summary>
    /// ѕровер€ет правильность введЄнного номера (возможность его использовани€ дл€ регистрации или авторизации).
    /// — указанием текущих данных приложени€ ClientId и ClientSecret
    /// </summary>
    function CheckPhone(Phone: string; AuthByPhone: Boolean = False): Boolean; overload;
    /// <summary>
    /// ѕозвол€ет восстановить доступ к аккаунту, использу€ код, полученный через SMS.
    /// </summary>
    function Restore(Phone, LastName: string): TResponse;
  end;

implementation

uses
  VK.API;

{ TAuthController }

function TAuthController.CheckPhone(Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('client_id', ClientId);
  Params.Add('client_secret', ClientSecret);
  Params.Add('auth_by_phone', Ord(AuthByPhone));
  with Handler.Execute('auth.checkPhone', Params) do
    Result := Success and (Response = '1');
end;

function TAuthController.CheckPhone(Phone: string; AuthByPhone: Boolean): Boolean;
var
  ClientId, ClientSecret: string;
begin
  ClientId := TCustomVK(VK).AppID;
  ClientSecret := TCustomVK(VK).AppKey;
  Result := CheckPhone(Phone, ClientId, ClientSecret, AuthByPhone);
end;

function TAuthController.Restore(Phone, LastName: string): TResponse;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('last_name', LastName);
  Result := Handler.Execute('auth.checkPhone', Params);
end;

end.

