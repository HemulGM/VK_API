unit VK.Secure;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Secure;

type
  TSecureController = class(TVkController)
  public
    /// <summary>
    /// Добавляет информацию о достижениях пользователя в приложении.
    /// </summary>
    function AddAppEvent(const UserId: TVkPeerId; ActivityId: TAppActivity; Value: Integer): Boolean;
    /// <summary>
    /// Позволяет проверять валидность пользователя в IFrame, VK Mini Apps и Standalone-приложениях с помощью передаваемого в приложения параметра access_token.
    /// Обратите внимание, что для iFrame-приложений токен становится валидным только после запроса прав у пользователя и установки приложения.
    /// </summary>
    function CheckToken(var Value: TVkSecureCheckToken; const Token, IP: string): Boolean;
    /// <summary>
    /// Возвращает платежный баланс (счет) приложения в сотых долях голоса.
    /// </summary>
    function GetAppBalance(var Value: Integer): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TSecureController }

function TSecureController.AddAppEvent(const UserId: TVkPeerId; ActivityId: TAppActivity; Value: Integer): Boolean;
begin
  with Handler.Execute('secure.addAppEvent', [['user_id', UserId.ToString], ['activity_id', Ord(ActivityId).ToString], ['value', Value.ToString]]) do
    Result := ResponseIsTrue;
end;

function TSecureController.CheckToken(var Value: TVkSecureCheckToken; const Token, IP: string): Boolean;
begin
  Result := Handler.Execute('secure.checkToken', [['token', Token], ['ip', IP]]).GetObject(Value);
end;

function TSecureController.GetAppBalance(var Value: Integer): Boolean;
begin
  { TODO -oМалинин Геннадий -c : Not work 15.01.2021 11:12:57 }
  with Handler.Execute('secure.getAppBalance') do
    Result := Success; // and ResponseIsTrue;
end;

end.

