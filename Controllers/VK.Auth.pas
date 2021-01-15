unit VK.Auth;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Auth;

type
  TAuthController = class(TVkController)
  public
    /// <summary>
    /// ��������� ������������ ��������� ������ (����������� ��� ������������� ��� ����������� ��� �����������).
    /// </summary>
    function CheckPhone(var Status: Boolean; Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean = False): Boolean; overload;
    /// <summary>
    /// ��������� ������������ ��������� ������ (����������� ��� ������������� ��� ����������� ��� �����������).
    /// � ��������� ������� ������ ���������� ClientId � ClientSecret
    /// </summary>
    function CheckPhone(var Status: Boolean; Phone: string; AuthByPhone: Boolean = False): Boolean; overload;
    /// <summary>
    /// ��������� ������������ ������ � ��������, ��������� ���, ���������� ����� SMS.
    /// ��� ���������� �������������� ������� ���������� ���������� �� ������:
    /// ������ ����������:
    /// grant_type � ���������� �������� ��������: restore_code;
    /// client_id � ������������� ����������;
    /// client_secret � ��������� ����;
    /// username � ����� �������� �� �������� ��� ������������ ������;
    /// scope � ������ ���� �������, ����������� ����� �������;
    /// sid � ������������� ������, ���������� � ���������� ���������� ����� ������;
    /// code � ���, ���������� ����� SMS.
    /// � ���������� ����������� ����� restore_code OAuth ������ ������ ����������� ������� �����������, � �������������� ���������� change_password_hash ����������� ��� ������ account.changePassword.
    /// </summary>
    // https://oauth.vk.com/token?grant_type=restore_code&client_id={������������� ����������}&client_secret={���������_����}&username={����� ��������}&scope={������ ���� �������}&sid={��������, ���������� � ������ ������}&code={��� ���������� ����� SMS}
    function Restore(var Status: TVkAuthRestore; const Phone, LastName: string): Boolean;
  end;

implementation

uses
  VK.API;

{ TAuthController }

function TAuthController.CheckPhone(var Status: Boolean; Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('client_id', ClientId);
  Params.Add('client_secret', ClientSecret);
  Params.Add('auth_by_phone', Ord(AuthByPhone));
  Result := Handler.Execute('auth.checkPhone', Params).ResponseAsBool(Status);
end;

function TAuthController.CheckPhone(var Status: Boolean; Phone: string; AuthByPhone: Boolean): Boolean;
var
  ClientId, ClientSecret: string;
begin
  ClientId := TCustomVK(VK).AppID;
  ClientSecret := TCustomVK(VK).AppKey;
  Result := CheckPhone(Status, Phone, ClientId, ClientSecret, AuthByPhone);
end;

function TAuthController.Restore(var Status: TVkAuthRestore; const Phone, LastName: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('last_name', LastName);
  Result := Handler.Execute('auth.checkPhone', Params).GetObject<TVkAuthRestore>(Status);
end;

end.

