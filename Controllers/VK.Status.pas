unit VK.Status;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Status;

type
  TStatusController = class(TVkController)
  public
    /// <summary>
    /// Устанавливает новый статус текущему пользователю или сообществу.
    /// </summary>
    /// <param name="Text">текст нового статуса</param>
    /// <param name="GroupId">идентификатор сообщества, в котором будет установлен статус. По умолчанию статус устанавливается текущему пользователю</param>
    function &Set(Text: string; GroupId: Integer = -1): Boolean; overload;
    /// <summary>
    /// Получает текст статуса пользователя или сообщества.
    /// </summary>
    /// <param name="Status">Возвращается статус (текст и аудио, если есть)</param>
    /// <param name="Id">идентификатор пользователя или сообщества, информацию о статусе которого нужно получить</param>
    /// <param name="IsGroup">если нужно получить статус сообщества</param>
    function Get(var Status: TVkStatus; Id: Integer = -1; IsGroup: Boolean = False): Boolean;
  end;

implementation

uses
  VK.API;

{ TStatusController }

function TStatusController.Get(var Status: TVkStatus; Id: Integer; IsGroup: Boolean): Boolean;
var
  Params: TParams;
begin
  if IsGroup then
    Params.Add('group_id', Id)
  else if Id >= 0 then
    Params.Add('user_id', Id);
  Result := Handler.Execute('status.get', Params).GetObject(Status);
end;

function TStatusController.&Set(Text: string; GroupId: Integer = -1): Boolean;
var
  Params: TParams;
begin
  Params.Add('text', Text);
  if GroupId >= 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('status.set', Params).ResponseIsTrue;
end;

end.

