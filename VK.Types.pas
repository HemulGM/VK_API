unit VK.Types;

interface

uses
  System.Classes, System.Generics.Collections;

type
  TOnLogin = procedure(Sender: TObject) of object;

  TOnConfirm = procedure(Ans: string; var Accept: Boolean) of object;

  TOnVKError = procedure(Code: Integer; Text: string) of object;

  TFields = array of string;

  TParam = array of string;

  TParams = array of TParam;

  TPremission = string;

  TResponse = record
    Success: Boolean;
    Value: string;
    Error: record
      Code: Integer;
      Text: string;
    end;
  end;

  TPermissions = class(TList<TPremission>)
    function ToString: string; override;
    procedure Assign(Source: TStrings);
  end;

  TRegisterDeviceData = record
    token: string;
    device_model: string;
    device_year: Integer;
    device_id: string;
    system_version: string;
    settings: string;
    sandbox: string;
  end;

  TProfileInfoData = record
    first_name: string;
    last_name: string;
    maiden_name: string;
    screen_name: string;
    cancel_request_id: string;
    sex: string;
    relation: string;
    relation_partner_id: string;
    bdate: string;
    bdate_visibility: string;
    home_town: string;
    country_id: string;
    city_id: string;
    status: string;
  end;

function FieldsToString(Fields: TFields): string;

function VKErrorString(ErrorCode: Integer): string;

implementation

function FieldsToString(Fields: TFields): string;
var
  i: Integer;
begin
  for i := Low(Fields) to High(Fields) do
  begin
    if i <> Low(Fields) then
      Result := Result + ',';
    Result := Result + Fields[i];
  end;
end;

function VKErrorString(ErrorCode: Integer): string;
var
  ErrStr: string;
begin
  case ErrorCode of
    1:
      ErrStr := '��������� ����������� ������.����������� ��������� ������ �����.';
    2:
      ErrStr :=
        '���������� ���������.����������� �������� ���������� � �����������https://vk.com/editapp?id={��� API_ID} ��� ������������ �������� ����� (test_mode=1)';
    3:
      ErrStr :=
        '������� ����������� �����.����������, ��������� �� ������� �������� ����������� ������:�https://vk.com/dev/methods.';
    4:
      ErrStr := '�������� �������.';
    5:
      ErrStr := '����������� ������������ �� �������.����������, ��� �� ����������� ������������ �����������.';
    6:
      ErrStr :=
        '������� ����� �������� � �������.�������� ������� �������� ����� �������� ��� ����������� �����execute. ��������� �� ������������ �� ������� ������� ��. �� ��������https://vk.com/dev/api_requests.';
    7:
      ErrStr :=
        '��� ���� ��� ���������� ����� ��������.����������, �������� �� ����������� ���������� �����������. ��� ����� ������� � ������� ������account.getAppPermissions.';
    8:
      ErrStr :=
        '�������� ������.������������������� �������� ������ ������������ ���������� (��� ����� ����� �� �������� � ��������� ������).';
    9:
      ErrStr :=
        '������� ����� ���������� ��������.������ ��������� ����� ���������� ���������. ��� ����� ����������� ������ �� ������ �������������execute����JSONP.';
    10:
      ErrStr := '��������� ���������� ������ �������.����������� ��������� ������ �����.';
    11:
      ErrStr :=
        '� �������� ������ ���������� ������ ���� ��������� ��� ������������ ������ ���� ���������.���������� ���������� � �����������https://vk.com/editapp?id={��� API_ID}';
    14:
      ErrStr := '��������� ���� ���� � �������� (Captcha).';
    15:
      ErrStr :=
        '������ ��������.����������, ��� �� ����������� ������ ��������������, � ������ � �������� ��� �������� ������������ ���� � ������ ������ �����.';
    16:
      ErrStr :=
        '��������� ���������� �������� �� ���������HTTPS, �.�. ������������ ������� ���������, ��������� ������ ����� ���������� ����������.'#13#10 +
        '������ �������� ��������� ����� ������, � Standalone-���������� �� ������ �������������� ��������� ��������� ���� ��������� � ������������ �������account.getInfo.';
    17:
      ErrStr :=
        '��������� ��������� ������������.��������� ������� ������������� � ���������� ������������� ������������ �� ��������� �������� ��� ���������.';
    18:
      ErrStr := '�������� ������� ��� �������������.��������� ������������ ���� ������� ��� �������������';
    20:
      ErrStr :=
        '������ �������� ��������� ��� �� Standalone ����������.����� ������ ��������� �������� �� ��, ��� ���� ���������� ����� ��� Standalone, ���������, ��� ��� ����������� �� �����������redirect_uri=https://oauth.vk.com/blank.html.';
    21:
      ErrStr := '������ �������� ��������� ������ ��� Standalone � Open API ����������.';
    23:
      ErrStr :=
        '����� ��� ��������.���� ���������� ������ �� API, ������� �������� � ��������� ������, ����������� �����:�https://vk.com/dev/methods.';
    24:
      ErrStr := '��������� ������������� �� ������� ������������.';
    27:
      ErrStr := '���� ������� ���������� ��������������.';
    28:
      ErrStr := '���� ������� ���������� ��������������.';
    29:
      ErrStr :=
        '��������� �������������� ����� �� ����� ��������������� �� ������������ �� ���������� ������� ��. �� �������� https://vk.com/dev/data_limits';
    30:
      ErrStr :=
        '������� �������� �������������������, ������������� � �������, ���������� � ������������ ������ �������';
    33:
      ErrStr := 'Not implemented yet';
    100:
      ErrStr :=
        '���� �� ����������� ���������� ��� �� ������� ��� �������.���������� ������ ��������� ���������� � �� ������ �� �������� � ��������� ������.';
    101:
      ErrStr :=
        '�������� API ID ����������.�������� ���������� � ������ ���������������� �� ��������https://vk.com/apps?act=settings�� ������� � ������� ������API_ID�(������������� ����������).';
    113:
      ErrStr :=
        '�������� ������������� ������������.����������, ��� �� ����������� ������ �������������. �������� ID �� ��������� ����� ����� �������utils.resolveScreenName.';
    150:
      ErrStr := '�������� timestamp.��������� ���������� �������� �� ������ �������utils.getServerTime.';
    200:
      ErrStr :=
        '������ � ������� ��������.����������, ��� �� ����������� ������ �������������� (��� �������������owner_id�������������, ��� ��������� � �������������), � ������ � �������������� �������� ��� �������� ������������ ���� � ������ ������ �����.';
    201:
      ErrStr :=
        '������ � ����� ��������.����������, ��� �� ����������� ������ �������������� (��� �������������owner_id�������������, ��� ��������� � �������������), � ������ � �������������� �������� ��� �������� ������������ ���� � ������ ������ �����.';
    203:
      ErrStr :=
        '������ � ������ ��������.����������, ��� ������� ������������ �������� ���������� ��� ������������� ���������� (��� �������� � ������� ����� � ������).';
    300:
      ErrStr :=
        '������ ����������.������ ������������ ������ ����� ������� ������ ������� �� ������� ��� ������������ ������ ������.';
    500:
      ErrStr :=
        '�������� ���������. �� ������ �������� �������� ������� � ���������� ����������.���������� ��������� ����������:�https://vk.com/editapp?id={��� API_ID}&section=payments';
    600:
      ErrStr := '��� ���� �� ���������� ������ �������� � ��������� ���������.';
    603:
      ErrStr := '��������� ������ ��� ������ � ��������� ���������.';
    1260:
      ErrStr := 'Invalid screen name';
    3300:
      ErrStr := 'Recaptcha needed';
    3301:
      ErrStr := 'Phone validation needed';
    3302:
      ErrStr := 'Password validation needed';
    3303:
      ErrStr := 'Otp app validation needed';
    3304:
      ErrStr := 'Email confirmation needed';
    3305:
      ErrStr := 'Assert votes';
  else
    ErrStr := '����������� ������';
  end;

  Result := ErrStr;
end;

{ TPermissions }

procedure TPermissions.Assign(Source: TStrings);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
    Add(Source[i]);
end;

function TPermissions.ToString: string;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Result + Items[i];
    if i < (Count - 1) then
      Result := Result + ',';
  end;
end;

end.

