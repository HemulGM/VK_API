unit VK.Status;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Status;

type
  TStatusController = class(TVkController)
  public
    /// <summary>
    /// ������������� ����� ������ �������� ������������ ��� ����������.
    /// </summary>
    /// <param name="Text">����� ������ �������</param>
    /// <param name="GroupId">������������� ����������, � ������� ����� ���������� ������. �� ��������� ������ ��������������� �������� ������������</param>
    function &Set(Text: string; GroupId: Integer = -1): Boolean; overload;
    /// <summary>
    /// �������� ����� ������� ������������ ��� ����������.
    /// </summary>
    /// <param name="Status">������������ ������ (����� � �����, ���� ����)</param>
    /// <param name="Id">������������� ������������ ��� ����������, ���������� � ������� �������� ����� ��������</param>
    /// <param name="IsGroup">���� ����� �������� ������ ����������</param>
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
  with Handler.Execute('status.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Status := TVkStatus.FromJsonString(Response);
    end;
  end;
end;

function TStatusController.&Set(Text: string; GroupId: Integer = -1): Boolean;
var
  Params: TParams;
begin
  Params.Add('text', Text);
  if GroupId >= 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('status.set', Params) do
    Result := Success and (Response = '1');
end;

end.

