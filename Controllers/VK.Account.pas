unit VK.Account;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.AccountInfo,
  VK.Entity.ProfileInfo, VK.Entity.ActiveOffers, VK.Entity.Counters, VK.Entity.PushSettings, VK.Entity.Common,
  VK.Entity.AccountInfoRequest, VK.Entity.Account.Banned, VK.CommonUtils;

type
  TVkParamsRegisterDevice = record
    List: TParams;
    function Token(Value: string): Integer;
    function DeviceModel(Value: string): Integer;
    function DeviceYear(Value: Integer): Integer;
    function DeviceId(Value: string): Integer;
    function SystemVersion(Value: string): Integer;
    function Settings(Value: string): Integer;
    function Sandbox(Value: string): Integer;
  end;

  TVkParamsProfileInfo = record
    List: TParams;
    function FirstName(Value: string): Integer;
    function LastName(Value: string): Integer;
    function MaidenName(Value: string): Integer;
    function ScreenName(Value: string): Integer;
    function CancelRequestId(Value: Integer): Integer;
    function Sex(Value: TVkSex): Integer;
    function Relation(Value: TVkRelation): Integer;
    function RelationPartnerId(Value: Integer): Integer;
    function BirthDate(Value: TDateTime): Integer;
    function BirthDateVisibility(Value: TVkBirthDateVisibility): Integer;
    function HomeTown(Value: string): Integer;
    function CountryId(Value: Integer): Integer;
    function CityId(Value: Integer): Integer;
    function Status(Value: string): Integer;
  end;

  TAccountController = class(TVkController)
  public
    /// <summary>
    /// ��������� ������������ ��� ������ � ������ ������.
    /// </summary>
    function Ban(const OwnerID: Integer): Boolean;
    /// <summary>
    /// ��������� ������� ������ ������������ ����� ��������� �������������� ������� � �������� ����� ���, ��������� ����� Auth.Restore.
    /// </summary>
    function ChangePassword(var Token: string; NewPassword: string; RestoreSid, ChangePasswordHash, OldPassword: string):
      Boolean;
    /// <summary>
    /// ���������� ������ �������� ��������� ����������� (�������), �������� ������� ������������ ������ �������� ��������������� ���������� ������� �� ���� ���� ������ ����������.
    /// </summary>
    function GetActiveOffers(var Items: TVkActiveOffers; Count: Integer = 100; Offset: Integer = 0): Boolean;
    /// <summary>
    /// �������� ��������� �������� ������������ � ������ ����������.
    /// </summary>
    function GetAppPermissions(var Mask: Integer; UserId: Integer): Boolean;
    /// <summary>
    /// ���������� ������ �������������, ����������� � ������ ������.
    /// </summary>
    function GetBanned(var Items: TVkBannedList; Count: Integer = 20; Offset: Integer = 0): Boolean;
    /// <summary>
    /// ���������� ��������� �������� ��������� ������������.
    /// </summary>
    function GetCounters(var Counters: TVkCounters; Filter: TVkCounterFilters = []): Boolean;
    /// <summary>
    /// ���������� ���������� � ������� ��������.
    /// </summary>
    function GetInfo(var Info: TVkAccountInfo; Fields: TVkInfoFilters = []): Boolean;
    /// <summary>
    /// ���������� ���������� � ������� �������.
    /// </summary>
    function GetProfileInfo(var ProfileInfo: TVkProfileInfo): Boolean;
    /// <summary>
    /// ��������� �������� ��������� Push-�����������.
    /// </summary>
    function GetPushSettings(var PushSettings: TVkPushSettings; DeviceId: string): Boolean;
    /// <summary>
    /// ����������� ���������� �� ���� iOS, Android, Windows Phone ��� Mac �� ��������� Push-�����������.
    /// </summary>
    function RegisterDevice(const Data: TVkParamsRegisterDevice): Boolean;
    /// <summary>
    /// ����������� ���������� �������� �������.
    /// </summary>
    function SaveProfileInfo(const Data: TVkParamsProfileInfo; var Request: TVkAccountInfoRequest): Boolean;
    /// <summary>
    /// ��������� ������������� ���������� � ������� ��������.
    /// </summary>
    function SetInfo(const Name, Value: string): Boolean;
    /// <summary>
    /// ������������� �������� �������� ���������� (�� 17 ��������), ������� ��������� ������������ � ����� ����.
    /// </summary>
    function SetNameInMenu(const UserId: Integer; Name: string): Boolean;
    /// <summary>
    /// �������� �������� ������������ ��� offline (������ � ������� ����������).
    /// </summary>
    function SetOffline: Boolean;
    /// <summary>
    /// �������� �������� ������������ ��� online �� 5 �����.
    /// </summary>
    function SetOnline(Voip: Boolean = False): Boolean;
    /// <summary>
    /// �������� ��������� Push-�����������.
    /// </summary>
    function SetPushSettings(const DeviceId, Settings, Key, Value: string): Boolean;
    /// <summary>
    /// ��������� push-����������� �� �������� ���������� �������.
    /// </summary>
    function SetSilenceMode(const DeviceId: string; Time: Integer; PeerId: string; Sound: Boolean): Boolean;
    /// <summary>
    /// ������� ������������ ��� ������ �� ������� ������.
    /// </summary>
    function UnBan(const OwnerID: Integer): Boolean;
    /// <summary>
    /// ���������� ���������� �� Push �����������.
    /// </summary>
    function UnRegisterDevice(const DeviceId: string; const Token: string; Sandbox: Boolean): Boolean;
  end;

implementation

uses
  System.Json;

{ TAccountController }

function TAccountController.ChangePassword(var Token: string; NewPassword: string; RestoreSid, ChangePasswordHash,
  OldPassword: string): Boolean;
var
  JsonResp: TJSONValue;
begin
  with Handler.Execute('account.changePassword', [['new_password', NewPassword], ['restore_sid', RestoreSid], ['change_password_hash',
    ChangePasswordHash], ['old_password', OldPassword]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        JsonResp := TJSONObject.ParseJSONValue(JSON);
        try
          Token := JsonResp.GetValue<string>('token', '');
          Result := not Token.IsEmpty;
        finally
          JsonResp.Free;
        end;
      except
        Result := False;
      end;
    end;
  end;
end;

function TAccountController.GetActiveOffers(var Items: TVkActiveOffers; Count: Integer; Offset: Integer): Boolean;
begin
  with Handler.Execute('account.getActiveOffers', [['offset', Offset.ToString], ['count', Count.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkActiveOffers.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAccountController.GetAppPermissions(var Mask: Integer; UserId: Integer): Boolean;
begin
  with Handler.Execute('account.getAppPermissions', ['user_id', UserId.ToString]) do
  begin
    Result := Success and TryStrToInt(Response, Mask);
  end;
end;

function TAccountController.GetBanned(var Items: TVkBannedList; Count, Offset: Integer): Boolean;
begin
  with Handler.Execute('account.getBanned', [['count', Count.ToString], ['offset', Offset.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkBannedList.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAccountController.GetCounters(var Counters: TVkCounters; Filter: TVkCounterFilters): Boolean;
begin
  with Handler.Execute('account.getCounters', ['filter', Filter.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Counters := TVkCounters.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAccountController.GetInfo(var Info: TVkAccountInfo; Fields: TVkInfoFilters = []): Boolean;
begin
  with Handler.Execute('account.getInfo', ['fields', Fields.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkAccountInfo.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAccountController.GetProfileInfo(var ProfileInfo: TVkProfileInfo): Boolean;
begin
  with Handler.Execute('account.getProfileInfo') do
  begin
    Result := Success;
    if Result then
    begin
      try
        ProfileInfo := TVkProfileInfo.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAccountController.GetPushSettings(var PushSettings: TVkPushSettings; DeviceId: string): Boolean;
begin
  with Handler.Execute('account.getPushSettings', ['device_id', DeviceId]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        PushSettings := TVkPushSettings.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAccountController.RegisterDevice(const Data: TVkParamsRegisterDevice): Boolean;
begin
  with Handler.Execute('account.registerDevice', Data.List) do
    Result := Success and (Response = '1');
end;

function TAccountController.SaveProfileInfo(const Data: TVkParamsProfileInfo; var Request: TVkAccountInfoRequest): Boolean;
begin
  with Handler.Execute('account.saveProfileInfo', Data.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Request := TVkAccountInfoRequest.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAccountController.SetInfo(const Name, Value: string): Boolean;
begin
  with Handler.Execute('account.setInfo', [['name', Name], ['value', Value]]) do
    Result := Success and (Response = '1');
end;

function TAccountController.SetNameInMenu(const UserId: Integer; Name: string): Boolean;
begin
  with Handler.Execute('account.setNameInMenu', [['user_id', UserId.ToString], ['name', Name]]) do
    Result := Success and (Response = '1');
end;

function TAccountController.SetOffline: Boolean;
begin
  with Handler.Execute('account.setOffline') do
    Result := Success and (Response = '1');
end;

function TAccountController.SetOnline(Voip: Boolean): Boolean;
begin
  with Handler.Execute('account.setOnline', ['voip', BoolToString(Voip)]) do
    Result := Success and (Response = '1');
end;

function TAccountController.SetPushSettings(const DeviceId, Settings, Key, Value: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('device_id', DeviceId);
  if not Settings.IsEmpty then
    Params.Add('settings', Settings);
  if not Key.IsEmpty then
  begin
    Params.Add(['key', Key]);
    Params.Add(['value', Value]);
  end;
  with Handler.Execute('account.setPushSettings', Params) do
    Result := Success and (Response = '1');
end;

function TAccountController.SetSilenceMode(const DeviceId: string; Time: Integer; PeerId: string; Sound: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('device_id', DeviceId);
  Params.Add('time', Time);
  Params.Add('peer_id', PeerId);
  Params.Add('sound', Sound);
  with Handler.Execute('account.setSilenceMode', Params) do
    Result := Success and (Response = '1');
end;

function TAccountController.Ban(const OwnerID: Integer): Boolean;
begin
  with Handler.Execute('account.ban', ['owner_id', OwnerID.ToString]) do
    Result := Success and (Response = '1');
end;

function TAccountController.UnBan(const OwnerID: Integer): Boolean;
begin
  with Handler.Execute('account.unban', ['owner_id', OwnerID.ToString]) do
    Result := Success and (Response = '1');
end;

function TAccountController.UnRegisterDevice(const DeviceId: string; const Token: string; Sandbox: Boolean): Boolean;
var
  Params: TParams;
begin
  if not DeviceId.IsEmpty then
    Params.Add('device_id', DeviceId);
  if not Token.IsEmpty then
    Params.Add('token', Token);
  Params.Add('sandbox', Sandbox);
  with Handler.Execute('account.unregisterDevice', Params) do
    Result := Success and (Response = '1');
end;

{ TVkRegisterDeviceParams }

function TVkParamsRegisterDevice.DeviceId(Value: string): Integer;
begin
  Result := List.Add('device_id', Value);
end;

function TVkParamsRegisterDevice.DeviceModel(Value: string): Integer;
begin
  Result := List.Add('device_model', Value);
end;

function TVkParamsRegisterDevice.DeviceYear(Value: Integer): Integer;
begin
  Result := List.Add('device_year', Value);
end;

function TVkParamsRegisterDevice.Sandbox(Value: string): Integer;
begin
  Result := List.Add('sandbox', Value);
end;

function TVkParamsRegisterDevice.Settings(Value: string): Integer;
begin
  Result := List.Add('settings', Value);
end;

function TVkParamsRegisterDevice.SystemVersion(Value: string): Integer;
begin
  Result := List.Add('system_version', Value);
end;

function TVkParamsRegisterDevice.Token(Value: string): Integer;
begin
  Result := List.Add('token', Value);
end;

{ TVkProfileInfoParams }

function TVkParamsProfileInfo.BirthDate(Value: TDateTime): Integer;
begin
  Result := List.Add('bdate', FormatDateTime('DD.MM.YYYY', Value));
end;

function TVkParamsProfileInfo.BirthDateVisibility(Value: TVkBirthDateVisibility): Integer;
begin
  Result := List.Add('bdate_visibility', Ord(Value).ToString);
end;

function TVkParamsProfileInfo.CancelRequestId(Value: Integer): Integer;
begin
  Result := List.Add('cancel_request_id', Value);
end;

function TVkParamsProfileInfo.CityId(Value: Integer): Integer;
begin
  Result := List.Add('cancel_request_id', Value);
end;

function TVkParamsProfileInfo.CountryId(Value: Integer): Integer;
begin
  Result := List.Add('country_id', Value);
end;

function TVkParamsProfileInfo.FirstName(Value: string): Integer;
begin
  Result := List.Add('first_name', Value);
end;

function TVkParamsProfileInfo.HomeTown(Value: string): Integer;
begin
  Result := List.Add('home_town', Value);
end;

function TVkParamsProfileInfo.LastName(Value: string): Integer;
begin
  Result := List.Add('last_name', Value);
end;

function TVkParamsProfileInfo.MaidenName(Value: string): Integer;
begin
  Result := List.Add('maiden_name', Value);
end;

function TVkParamsProfileInfo.Relation(Value: TVkRelation): Integer;
begin
  Result := List.Add('relation', Ord(Value).ToString);
end;

function TVkParamsProfileInfo.RelationPartnerId(Value: Integer): Integer;
begin
  Result := List.Add('relation_partner_id', Value);
end;

function TVkParamsProfileInfo.ScreenName(Value: string): Integer;
begin
  Result := List.Add('screen_name', Value);
end;

function TVkParamsProfileInfo.Sex(Value: TVkSex): Integer;
begin
  Result := List.Add('sex', Ord(Value).ToString);
end;

function TVkParamsProfileInfo.Status(Value: string): Integer;
begin
  Result := List.Add('status', Value);
end;

end.

