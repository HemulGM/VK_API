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
var
  Params: TParams;
begin
  Params.Add('user_id', UserId);
  with Handler.Execute('downloadedGames.getPaidStatus', Params) do
    Result := Success and GetValue('is_paid', IsPaid);
end;

end.

