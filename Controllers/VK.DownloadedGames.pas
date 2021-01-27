unit VK.DownloadedGames;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types;

type
  /// <summary>
  /// Список методов секции downloadedGames
  /// </summary>
  TDownloadedGamesController = class(TVkController)
  public
    /// <summary>
    /// Возвращает информацию о том, куплено ли приложение
    /// </summary>
    function GetPaidStatus(var IsPaid: Boolean; UserId: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TDownloadedGamesController }

function TDownloadedGamesController.GetPaidStatus(var IsPaid: Boolean; UserId: Integer): Boolean;
begin
  Result := Handler.Execute('downloadedGames.getPaidStatus', ['user_id', UserId.ToString]).GetValue('is_paid', IsPaid);
end;

end.

