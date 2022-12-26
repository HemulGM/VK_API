unit VK.Auth;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Auth;

type
  TVkParamsSignup = record
    List: TParams;
    /// <summary>
    /// ��� ������������. ������, ������������ ��������
    /// </summary>
    function FirstName(const Value: string): TVkParamsSignup;
    /// <summary>
    /// ������� ������������. ������, ������������ ��������
    /// </summary>
    function LastName(const Value: string): TVkParamsSignup;
    /// <summary>
    /// ������������� ������ ����������. ����� �����, ������������ ��������
    /// </summary>
    function ClientId(const Value: string): TVkParamsSignup;
    /// <summary>
    /// ��������� ���� ����������, ��������� � ������� �������������� ����������. ������, ������������ ��������
    /// </summary>
    function ClientSecret(const Value: string): TVkParamsSignup;
    /// <summary>
    /// ����� �������� ��������������� ������������. ����� �������� ����� ���� �������� ������� ������� auth.checkPhone. ������, ������������ ��������
    /// </summary>
    function Phone(const Value: string): TVkParamsSignup;
    /// <summary>
    /// ������ ������������, ������� ����� �������������� ��� �����. �� ������ 6 ��������. ����� ������ ����� ���� ������ �����, ��� ������ ������ auth.confirm. ������
    /// </summary>
    function Password(const Value: string): TVkParamsSignup;
    /// <summary>
    /// 1 � �������� �����, ��� ������� �� ����� ��������������� ����� ������������, �� ��� ���� ����� �� ����� ����������� �� ����������������. 0 � (�� ���������) �������. ����, ����� ��������� �������� 1 ��� 0
    /// </summary>
    function TestMode(const Value: Boolean): TVkParamsSignup;
    /// <summary>
    /// 1 � � ������, ���� ������ SMS ���������� ��������� �� ��������� ����� � ������������ ��� �������. 0 � (�� ���������) ���������� ��������� SMS. � ������ ���� ��� �� ����� �� ������������ � ���������� ������� ����� �������� ������ voice=1 � sid, ���������� ��� ������ ������ ������. ����, ����� ��������� �������� 1 ��� 0
    /// </summary>
    function Voice(const Value: Boolean): TVkParamsSignup;
    /// <summary>
    /// ��� ������������
    /// </summary>
    function Sex(const Value: TVkSex): TVkParamsSignup;
    /// <summary>
    /// ������������� ������, ����������� ��� ��������� ������ ������, � ������ ���� SMS ��������� ���������� �� ����. ��� ������ ������ ���� �������� �� ����������. ������
    /// </summary>
    function Sid(const Value: string): TVkParamsSignup;
    /// <summary>
    /// ���� ��������
    /// </summary>
    function Birthday(const Value: TDate): TVkParamsSignup;
  end;

  TAuthController = class(TVkController)
  public
    /// <summary>
    /// <b>������ ����� ������� � ����� ���� �������� ����� ��������� �����, ����������, ��������� ��� �������������.</b>
    /// ��������� ������������ ��������� ������ (����������� ��� ������������� ��� ����������� ��� �����������).
    /// </summary>
    function CheckPhone(const Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean = False): Boolean; overload; deprecated '����� �������� �� ������ API v5.124+';
    /// <summary>
    /// <b>������ ����� ������� � ����� ���� �������� ����� ��������� �����, ����������, ��������� ��� �������������.</b>
    /// ��������� ������������ ��������� ������ (����������� ��� ������������� ��� ����������� ��� �����������).
    /// � ��������� ������� ������ ���������� ClientId � ClientSecret
    /// </summary>
    function CheckPhone(const Phone: string; AuthByPhone: Boolean = False): Boolean; overload; deprecated '����� �������� �� ������ API v5.124+';
    /// <summary>
    /// ��������� ������������ ������ � ��������, ��������� ���, ���������� ����� SMS.
    /// ��� ���������� �������������� ������� ���������� ���������� �� ������: https://oauth.vk.com/token
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
    /// <summary>
    /// ������������ ������ ������������ �� ������ ��������
    /// </summary>
    function Signup(var Status: TVkAuthSignup; Params: TParams): Boolean; overload;
    /// <summary>
    /// ������������ ������ ������������ �� ������ ��������
    /// </summary>
    function Signup(var Status: TVkAuthSignup; Params: TVkParamsSignup): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TAuthController }

function TAuthController.CheckPhone(const Phone: string; ClientId, ClientSecret: string; AuthByPhone: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('client_id', ClientId);
  Params.Add('client_secret', ClientSecret);
  Params.Add('auth_by_phone', Ord(AuthByPhone));
  Result := Handler.Execute('auth.checkPhone', Params).ResponseIsTrue;
end;

function TAuthController.CheckPhone(const Phone: string; AuthByPhone: Boolean): Boolean;
var
  ClientId, ClientSecret: string;
begin
  ClientId := TCustomVK(VK).AppID;
  ClientSecret := TCustomVK(VK).AppKey;
  {$WARNINGS OFF}
  Result := CheckPhone(Phone, ClientId, ClientSecret, AuthByPhone);
  {$WARNINGS ON}
end;

function TAuthController.Restore(var Status: TVkAuthRestore; const Phone, LastName: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('phone', Phone);
  Params.Add('last_name', LastName);
  Result := Handler.Execute('auth.restore', Params).GetObject(Status);
end;

function TAuthController.Signup(var Status: TVkAuthSignup; Params: TVkParamsSignup): Boolean;
begin
  Result := Signup(Status, Params.List);
end;

function TAuthController.Signup(var Status: TVkAuthSignup; Params: TParams): Boolean;
begin
  Result := Handler.Execute('auth.signup', Params).GetObject(Status);
end;

{ TVkParamsSignup }

function TVkParamsSignup.Birthday(const Value: TDate): TVkParamsSignup;
begin
  List.Add('birthday', Value, 'DD.MM.YYYY');
  Result := Self;
end;

function TVkParamsSignup.ClientId(const Value: string): TVkParamsSignup;
begin
  List.Add('client_id', Value);
  Result := Self;
end;

function TVkParamsSignup.ClientSecret(const Value: string): TVkParamsSignup;
begin
  List.Add('client_secret', Value);
  Result := Self;
end;

function TVkParamsSignup.FirstName(const Value: string): TVkParamsSignup;
begin
  List.Add('first_name', Value);
  Result := Self;
end;

function TVkParamsSignup.LastName(const Value: string): TVkParamsSignup;
begin
  List.Add('last_name', Value);
  Result := Self;
end;

function TVkParamsSignup.Password(const Value: string): TVkParamsSignup;
begin
  List.Add('password', Value);
  Result := Self;
end;

function TVkParamsSignup.Phone(const Value: string): TVkParamsSignup;
begin
  List.Add('phone', Value);
  Result := Self;
end;

function TVkParamsSignup.Sex(const Value: TVkSex): TVkParamsSignup;
begin
  List.Add('sex', Ord(Value));
  Result := Self;
end;

function TVkParamsSignup.Sid(const Value: string): TVkParamsSignup;
begin
  List.Add('sid', Value);
  Result := Self;
end;

function TVkParamsSignup.TestMode(const Value: Boolean): TVkParamsSignup;
begin
  List.Add('test_mode', Value);
  Result := Self;
end;

function TVkParamsSignup.Voice(const Value: Boolean): TVkParamsSignup;
begin
  List.Add('voice', Value);
  Result := Self;
end;

end.

