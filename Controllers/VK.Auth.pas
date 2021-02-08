unit VK.Auth;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Auth;

type
  TAuthController = class(TVkController)
  public
    /// <summary>
    /// <b>Данный метод устарел и может быть отключён через некоторое время, пожалуйста, избегайте его использования.</b>
    /// Проверяет правильность введённого номера (возможность его использования для регистрации или авторизации).
    /// </summary>
    function CheckPhone(var Status: Boolean; Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean = False):
      Boolean; overload; deprecated 'Метод отключен на уровне API v5.124+';
    /// <summary>
    /// <b>Данный метод устарел и может быть отключён через некоторое время, пожалуйста, избегайте его использования.</b>
    /// Проверяет правильность введённого номера (возможность его использования для регистрации или авторизации).
    /// С указанием текущих данных приложения ClientId и ClientSecret
    /// </summary>
    function CheckPhone(var Status: Boolean; Phone: string; AuthByPhone: Boolean = False): Boolean; overload; deprecated
      'Метод отключен на уровне API v5.124+';
    /// <summary>
    /// Позволяет восстановить доступ к аккаунту, используя код, полученный через SMS.
    /// Для завершения восстановления доступа необходимо обратиться по адресу:
    /// Список параметров:
    /// grant_type – необходимо передать значение: restore_code;
    /// client_id – Идентификатор приложения;
    /// client_secret – Секретный ключ;
    /// username – Номер телефона по которому был восстановлен пароль;
    /// scope – список прав доступа, разделенных через запятую;
    /// sid – идентификатор сессии, полученный в результате выполнения этого метода;
    /// code – код, полученный через SMS.
    /// В результате авторизации через restore_code OAuth вернет данные аналогичные обычной авторизации, с дополнительным параметром change_password_hash необходимым для метода account.changePassword.
    /// </summary>
    // https://oauth.vk.com/token?grant_type=restore_code&client_id={Идентификатор приложения}&client_secret={Секретный_ключ}&username={Номер телефона}&scope={Список прав доступа}&sid={Параметр, получаемый в данном методе}&code={Код полученный через SMS}
    function Restore(var Status: TVkAuthRestore; const Phone, LastName: string): Boolean;
  end;

implementation

uses
  VK.API;

{ TAuthController }

function TAuthController.CheckPhone(var Status: Boolean; Phone: string; ClientId, ClientSecret: string; AuthByPhone:
  Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('client_id', ClientId);
  Params.Add('client_secret', ClientSecret);
  Params.Add('auth_by_phone', Ord(AuthByPhone));
  Result := Handler.Execute('auth.checkPhone', Params).ResponseAsBool(Status);
end;

function TAuthController.CheckPhone(var Status: Boolean; Phone: string; AuthByPhone: Boolean): Boolean;
var
  ClientId, ClientSecret: string;
begin
  ClientId := TCustomVK(VK).AppID;
  ClientSecret := TCustomVK(VK).AppKey;
  {$WARNINGS OFF}
  Result := CheckPhone(Status, Phone, ClientId, ClientSecret, AuthByPhone);
  {$WARNINGS ON}
end;

function TAuthController.Restore(var Status: TVkAuthRestore; const Phone, LastName: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('last_name', LastName);
  Result := Handler.Execute('auth.checkPhone', Params).GetObject<TVkAuthRestore>(Status);
end;

end.

