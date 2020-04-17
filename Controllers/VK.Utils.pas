unit VK.Utils;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON;

type
  TUtilsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает текущее время на сервере ВКонтакте в unixtime.
    /// </summary>
    /// <param name="ServerTime">Время на сервере</param>
    function GetServerTime(var ServerTime: TDateTime): Boolean; overload;
    function GetServerTime(var ServerTime: Int64): Boolean; overload;
  end;

implementation

uses
  VK.API, System.DateUtils;

{ TUtilsController }

function TUtilsController.GetServerTime(var ServerTime: TDateTime): Boolean;
var
  ST: Int64;
begin
  Result := GetServerTime(ST);
  if Result then
    ServerTime := UnixToDateTime(ST, False);
end;

function TUtilsController.GetServerTime(var ServerTime: Int64): Boolean;
begin
  with Handler.Execute('utils.getServerTime') do
  begin
    Result := Success and TryStrToInt64(Response, ServerTime);
  end;
end;

end.

