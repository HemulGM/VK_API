unit VK.Auth;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Auth;

type
  TVkParamsSignup = record
    List: TParams;
    /// <summary>
    /// Имя пользователя. строка, обязательный параметр
    /// </summary>
    function FirstName(const Value: string): TVkParamsSignup;
    /// <summary>
    /// Фамилия пользователя. строка, обязательный параметр
    /// </summary>
    function LastName(const Value: string): TVkParamsSignup;
    /// <summary>
    /// Идентификатор Вашего приложения. целое число, обязательный параметр
    /// </summary>
    function ClientId(const Value: string): TVkParamsSignup;
    /// <summary>
    /// Секретный ключ приложения, доступный в резделе редактирования приложения. строка, обязательный параметр
    /// </summary>
    function ClientSecret(const Value: string): TVkParamsSignup;
    /// <summary>
    /// Номер телефона регистрируемого пользователя. Номер телефона может быть проверен заранее методом auth.checkPhone. строка, обязательный параметр
    /// </summary>
    function Phone(const Value: string): TVkParamsSignup;
    /// <summary>
    /// Пароль пользователя, который будет использоваться при входе. Не меньше 6 символов. Также пароль может быть указан позже, при вызове метода auth.confirm. строка
    /// </summary>
    function Password(const Value: string): TVkParamsSignup;
    /// <summary>
    /// 1 — тестовый режим, при котором не будет зарегистрирован новый пользователь, но при этом номер не будет проверяться на использованность. 0 — (по умолчанию) рабочий. флаг, может принимать значения 1 или 0
    /// </summary>
    function TestMode(const Value: Boolean): TVkParamsSignup;
    /// <summary>
    /// 1 — в случае, если вместо SMS необходимо позвонить на указанный номер и продиктовать код голосом. 0 — (по умолчанию) необходимо отправить SMS. В случае если СМС не дошло до пользователя – необходимо вызвать метод повторно указав voice=1 и sid, полученный при первом вызове метода. флаг, может принимать значения 1 или 0
    /// </summary>
    function Voice(const Value: Boolean): TVkParamsSignup;
    /// <summary>
    /// Пол пользователя
    /// </summary>
    function Sex(const Value: TVkSex): TVkParamsSignup;
    /// <summary>
    /// Идентификатор сессии, необходимый при повторном вызове метода, в случае если SMS сообщение доставлено не было. При первом вызове этот параметр не передается. строка
    /// </summary>
    function Sid(const Value: string): TVkParamsSignup;
    /// <summary>
    /// Дата рождения
    /// </summary>
    function Birthday(const Value: TDate): TVkParamsSignup;
  end;

  TAuthController = class(TVkController)
  public
    /// <summary>
    /// <b>Данный метод устарел и может быть отключён через некоторое время, пожалуйста, избегайте его использования.</b>
    /// Проверяет правильность введённого номера (возможность его использования для регистрации или авторизации).
    /// </summary>
    function CheckPhone(const Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean = False): Boolean; overload; deprecated 'Метод отключен на уровне API v5.124+';
    /// <summary>
    /// <b>Данный метод устарел и может быть отключён через некоторое время, пожалуйста, избегайте его использования.</b>
    /// Проверяет правильность введённого номера (возможность его использования для регистрации или авторизации).
    /// С указанием текущих данных приложения ClientId и ClientSecret
    /// </summary>
    function CheckPhone(const Phone: string; AuthByPhone: Boolean = False): Boolean; overload; deprecated 'Метод отключен на уровне API v5.124+';
    /// <summary>
    /// Позволяет восстановить доступ к аккаунту, используя код, полученный через SMS.
    /// Для завершения восстановления доступа необходимо обратиться по адресу: https://oauth.vk.com/token
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
    /// <summary>
    /// Регистрирует нового пользователя по номеру телефона
    /// </summary>
    function Signup(var Status: TVkAuthSignup; Params: TParams): Boolean; overload;
    /// <summary>
    /// Регистрирует нового пользователя по номеру телефона
    /// </summary>
    function Signup(var Status: TVkAuthSignup; Params: TVkParamsSignup): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TAuthController }

function TAuthController.CheckPhone(const Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('client_id', ClientId);
  Params.Add('client_secret', ClientSecret);
  Params.Add('auth_by_phone', Ord(AuthByPhone));
  Result := Handler.Execute('auth.checkPhone', Params).ResponseIsTrue;
end;

function TAuthController.CheckPhone(const Phone: string; AuthByPhone: Boolean): Boolean;
var
  ClientId, ClientSecret: string;
begin
  ClientId := TCustomVK(VK).AppID;
  ClientSecret := TCustomVK(VK).AppKey;
  {$WARNINGS OFF}
  Result := CheckPhone(Phone, ClientId, ClientSecret, AuthByPhone);
  {$WARNINGS ON}
end;

function TAuthController.Restore(var Status: TVkAuthRestore; const Phone, LastName: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('last_name', LastName);
  Result := Handler.Execute('auth.restore', Params).GetObject(Status);
end;

function TAuthController.Signup(var Status: TVkAuthSignup; Params: TVkParamsSignup): Boolean;
begin
  Result := Signup(Status, Params.List);
end;

function TAuthController.Signup(var Status: TVkAuthSignup; Params: TParams): Boolean;
begin
  Result := Handler.Execute('auth.signup', Params).GetObject(Status);
end;

{ TVkParamsSignup }

function TVkParamsSignup.Birthday(const Value: TDate): TVkParamsSignup;
begin
  List.Add('birthday', Value, 'DD.MM.YYYY');
  Result := Self;
end;

function TVkParamsSignup.ClientId(const Value: string): TVkParamsSignup;
begin
  List.Add('client_id', Value);
  Result := Self;
end;

function TVkParamsSignup.ClientSecret(const Value: string): TVkParamsSignup;
begin
  List.Add('client_secret', Value);
  Result := Self;
end;

function TVkParamsSignup.FirstName(const Value: string): TVkParamsSignup;
begin
  List.Add('first_name', Value);
  Result := Self;
end;

function TVkParamsSignup.LastName(const Value: string): TVkParamsSignup;
begin
  List.Add('last_name', Value);
  Result := Self;
end;

function TVkParamsSignup.Password(const Value: string): TVkParamsSignup;
begin
  List.Add('password', Value);
  Result := Self;
end;

function TVkParamsSignup.Phone(const Value: string): TVkParamsSignup;
begin
  List.Add('phone', Value);
  Result := Self;
end;

function TVkParamsSignup.Sex(const Value: TVkSex): TVkParamsSignup;
begin
  List.Add('sex', Ord(Value));
  Result := Self;
end;

function TVkParamsSignup.Sid(const Value: string): TVkParamsSignup;
begin
  List.Add('sid', Value);
  Result := Self;
end;

function TVkParamsSignup.TestMode(const Value: Boolean): TVkParamsSignup;
begin
  List.Add('test_mode', Value);
  Result := Self;
end;

function TVkParamsSignup.Voice(const Value: Boolean): TVkParamsSignup;
begin
  List.Add('voice', Value);
  Result := Self;
end;

end.

