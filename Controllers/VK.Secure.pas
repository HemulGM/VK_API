unit VK.Secure;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Secure;

type
  TAppActivity = (aaNewLevel = 1, aaNewScore = 2);

  TSecureController = class(TVkController)
  public
    /// <summary>
    /// ƒобавл€ет информацию о достижени€х пользовател€ в приложении.
    /// </summary>
    function AddAppEvent(const UserId: Integer; ActivityId: TAppActivity; Value: Integer): Boolean;
    /// <summary>
    /// ѕозвол€ет провер€ть валидность пользовател€ в IFrame, VK Mini Apps и Standalone-приложени€х с помощью передаваемого в приложени€ параметра access_token.
    /// ќбратите внимание, что дл€ iFrame-приложений токен становитс€ валидным только после запроса прав у пользовател€ и установки приложени€.
    /// </summary>
    function CheckToken(var Value: TVkSecureCheckToken; const Token, IP: string): Boolean;
    /// <summary>
    /// ¬озвращает платежный баланс (счет) приложени€ в сотых дол€х голоса.
    /// </summary>
    function GetAppBalance(var Value: Integer): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TSecureController }

function TSecureController.AddAppEvent(const UserId: Integer; ActivityId: TAppActivity; Value: Integer): Boolean;
begin
  with Handler.Execute('secure.addAppEvent', [['user_id', UserId.ToString], ['activity_id', Ord(ActivityId).ToString], ['value', Value.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TSecureController.CheckToken(var Value: TVkSecureCheckToken; const Token, IP: string): Boolean;
begin
  Result := Handler.Execute('secure.checkToken', [['token', Token], ['ip', IP]]).GetObject<TVkSecureCheckToken>(Value);
end;

function TSecureController.GetAppBalance(var Value: Integer): Boolean;
begin
  { TODO -oћалинин √еннадий -c : Not owrk 15.01.2021 11:12:57 }
  with Handler.Execute('secure.getAppBalance') do
    Result := Success; // and ResponseIsTrue;
end;

end.

