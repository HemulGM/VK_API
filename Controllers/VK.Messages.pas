unit VK.Messages;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Structs;

type
  TMessagesController = class(TVkController)
  public
    /// <summary>
    ///
    /// </summary>
    /// <param name=""></param>
    function Send(PeerId: Integer; Message: string): Integer; overload;
  end;

implementation

uses
  VK.API, VK.Utils;

{ TMessagesController }

function TMessagesController.Send(PeerId: Integer; Message: string): Integer;
begin
  with Handler.Execute('messages.send', [['peer_id', PeerId.ToString], ['message', Message], ['random_id',
    Random(GetRandomId).ToString]]) do
  begin
    if not Success then
      Exit(-1)
    else
    begin
      Result := Value.ToInteger;
    end;
  end;
end;

end.

