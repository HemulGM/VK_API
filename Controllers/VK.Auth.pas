unit VK.Auth;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Structs;

type
  TAuthController = class(TVkController)
  public
    /// <summary>
    /// ѕровер€ет правильность введЄнного номера (возможность его использовани€ дл€ регистрации или авторизации).
    /// </summary>
    /// <param name="Phone">номер телефона регистрируемого пользовател€</param>
    /// <param name="ClientId">идентификатор ¬ашего приложени€</param>
    /// <param name="ClientSecret">секретный ключ приложени€, доступный в разделе редактировани€ приложени€</param>
    /// <param name="AuthByPhone">True Ч проверить правильность номера дл€ авторизации,
    ///                           а не дл€ регистрации нового аккаунта. ѕо умолчанию: False.</param>
    function CheckPhone(Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean = False): Boolean; overload;
    /// <summary>
    /// ѕровер€ет правильность введЄнного номера (возможность его использовани€ дл€ регистрации или авторизации).
    /// — указанием текущих данных приложени€ ClientId и ClientSecret
    /// </summary>
    /// <param name="Phone">номер телефона регистрируемого пользовател€</param>
    /// <param name="AuthByPhone">True Ч проверить правильность номера дл€ авторизации,
    ///                           а не дл€ регистрации нового аккаунта. ѕо умолчанию: False.</param>
    function CheckPhone(Phone: string; AuthByPhone: Boolean = False): Boolean; overload;
    /// <summary>
    /// https://vk.com/dev/auth.restore
    /// </summary>
    function Restore(Phone, LastName: string): TResponse;
  end;

implementation

uses
  VK.API;

{ TAuth }

function TAuthController.CheckPhone(Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean): Boolean;
begin
  with Handler.Execute('auth.checkPhone', [['phone', Phone], ['client_id', ClientId], ['client_secret',
    ClientSecret], ['auth_by_phone', Ord(AuthByPhone).ToString]]) do
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
begin
  Result := Handler.Execute('auth.checkPhone', [['phone', Phone], ['last_name', LastName]]);
end;

end.

