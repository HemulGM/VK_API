unit VK.Status;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Status;

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

