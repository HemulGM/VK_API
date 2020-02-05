unit VK.Auth;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types;

type
  TAuthController = class(TVkController)
  public
    /// <summary>
    /// ��������� ������������ ��������� ������ (����������� ��� ������������� ��� ����������� ��� �����������).
    /// </summary>
    /// <param name="Phone">����� �������� ��������������� ������������</param>
    /// <param name="ClientId">������������� ������ ����������</param>
    /// <param name="ClientSecret">��������� ���� ����������, ��������� � ������� �������������� ����������</param>
    /// <param name="AuthByPhone">True � ��������� ������������ ������ ��� �����������,
    ///                           � �� ��� ����������� ������ ��������. �� ���������: False.</param>
    function CheckPhone(Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean = False): Boolean; overload;
    /// <summary>
    /// ��������� ������������ ��������� ������ (����������� ��� ������������� ��� ����������� ��� �����������).
    /// � ��������� ������� ������ ���������� ClientId � ClientSecret
    /// </summary>
    /// <param name="Phone">����� �������� ��������������� ������������</param>
    /// <param name="AuthByPhone">True � ��������� ������������ ������ ��� �����������,
    ///                           � �� ��� ����������� ������ ��������. �� ���������: False.</param>
    function CheckPhone(Phone: string; AuthByPhone: Boolean = False): Boolean; overload;
    /// <summary>
    /// https://vk.com/dev/auth.restore
    /// </summary>
    function Restore(Phone, LastName: string): TResponse;
  end;

implementation

uses
  VK.API;

{ TAuthController }

function TAuthController.CheckPhone(Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('client_id', ClientId);
  Params.Add('client_secret', ClientSecret);
  Params.Add('auth_by_phone', Ord(AuthByPhone));
  with Handler.Execute('auth.checkPhone', Params) do
    Result := Success and (Response = '1');
end;

function TAuthController.CheckPhone(Phone: string; AuthByPhone: Boolean): Boolean;
var
  ClientId, ClientSecret: string;
begin
  ClientId := TCustomVK(VK).AppID;
  ClientSecret := TCustomVK(VK).AppKey;
  Result := CheckPhone(Phone, ClientId, ClientSecret, AuthByPhone);
end;

function TAuthController.Restore(Phone, LastName: string): TResponse;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('last_name', LastName);
  Result := Handler.Execute('auth.checkPhone', Params);
end;

end.

