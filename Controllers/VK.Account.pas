unit VK.Account;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types,
  VK.Entity.AccountInfo, VK.Entity.ProfileInfo, VK.Entity.ActiveOffers, VK.Entity.Counters,
  VK.Entity.PushSettings, VK.Structs;

type
  TAccountController = class(TVkController)
  public
    function GetInfo(var Info: TAccountInfoClass; Fields: TFields = []): Boolean;
    function SetInfo(const Name, Value: string): Boolean;
    function GetProfileInfo(var ProfileInfo: TVkProfileInfo): Boolean;
    function Ban(const OwnerID: Integer): Boolean;
    function UnBan(const OwnerID: Integer): Boolean;
    function ChangePassword(var Response: TResponse; NewPassword: string; RestoreSid,
      ChangePasswordHash, OldPassword: string): Boolean;
    function GetActiveOffers(var Offers: TVkActiveOffers; Offset: Integer; Count: Integer = 100): Boolean;
    function GetAppPermissions(var Mask: Int64; UserId: Integer): Boolean;
    function GetCounters(var Counters: TCountersClass; Filter: string = ''): Boolean;
    function GetPushSettings(var PushSettings: TVkPushSettings; DeviceId: string): Boolean;
    function RegisterDevice(const Data: TRegisterDeviceData): Boolean;
    function SaveProfileInfo(const Data: TProfileInfoData; var Response: TResponse): Boolean;
    function SetNameInMenu(const UserId: Integer; Name: string): Boolean;
    function SetOffline(): Boolean;
    function SetOnline(Voip: Boolean = False): Boolean;
    function SetPushSettings(const DeviceId, Settings, Key, Value: string): Boolean;
    function SetSilenceMode(const DeviceId: string; Time: Integer; PeerId: string; Sound: Boolean): Boolean;
    function UnRegisterDevice(const DeviceId: string; const Token: string; Sandbox: Boolean): Boolean;
  end;

implementation

{ TAccount }

function TAccountController.ChangePassword(var Response: TResponse; NewPassword: string; RestoreSid,
  ChangePasswordHash, OldPassword: string): Boolean;
begin
  Response := Handler.Execute('account.changePassword', [
    ['new_password', NewPassword],
    ['restore_sid', RestoreSid],
    ['change_password_hash', ChangePasswordHash],
    ['old_password', OldPassword]]);
  Result := Response.Success;
end;

function TAccountController.GetActiveOffers(var Offers: TVkActiveOffers; Offset: Integer; Count:
  Integer = 100): Boolean;
begin
  if (Count > 100) or (Count < 0) then
    raise Exception.Create('Count - положительное число, по умолчанию 100, максимальное значение 100');
  if (Offset < 0) then
    raise Exception.Create('Count - положительное число, по умолчанию 0');
  with Handler.Execute('account.getActiveOffers', [['offset', Offset.ToString], ['count', Count.ToString]]) do
  begin
    Result := Success;
    if Result then
      Offers := TVkActiveOffers.FromJsonString(Response);
  end;
end;

function TAccountController.GetAppPermissions(var Mask: Int64; UserId: Integer): Boolean;
begin
  with Handler.Execute('account.getAppPermissions', ['user_id', UserId.ToString]) do
  begin
    Result := Success;
    if Result then
      Mask := StrToIntDef(Response, 0);
  end;
end;

function TAccountController.GetCounters(var Counters: TCountersClass; Filter: string = ''): Boolean;
begin
  if Filter = '' then
    Filter :=
      'friends, messages, photos, videos, notes, gifts, events, groups, notifications, sdk, app_requests, friends_recommendations';
  with Handler.Execute('account.getCounters', ['filter', Filter]) do
  begin
    Result := Success;
    if Result then
      Counters := TCountersClass.FromJsonString(Response);
  end;
end;

function TAccountController.GetInfo(var Info: TAccountInfoClass; Fields: TFields = []): Boolean;
begin
  with Handler.Execute('account.getInfo', ['fields', FieldsToString(Fields)]) do
  begin
    Result := Success;
    if Result then
      Info := TAccountInfoClass.FromJsonString(Response);
  end;
end;

function TAccountController.GetProfileInfo(var ProfileInfo: TVkProfileInfo): Boolean;
begin
  with Handler.Execute('account.getProfileInfo') do
  begin
    Result := Success;
    if Result then
      ProfileInfo := TVkProfileInfo.FromJsonString(Response);
  end;
end;

function TAccountController.GetPushSettings(var PushSettings: TVkPushSettings; DeviceId: string): Boolean;
begin
  with Handler.Execute('account.getPushSettings', ['device_id', DeviceId]) do
  begin
    Result := Success;
    if Result then
      PushSettings := TVkPushSettings.FromJsonString(Response);
  end;
end;

function TAccountController.RegisterDevice(const Data: TRegisterDeviceData): Boolean;
var
  Response: TResponse;
  Params: TParams;
begin
  if Data.Ftoken_need then
    AddParam(Params, ['token', Data.token]);
  if Data.Fdevice_model_need then
    AddParam(Params, ['device_model', Data.device_model]);
  if Data.Fdevice_year_need then
    AddParam(Params, ['device_year', Data.device_year.ToString]);
  if Data.Fdevice_id_need then
    AddParam(Params, ['device_id', Data.device_id]);
  if Data.Fsystem_version_need then
    AddParam(Params, ['system_version', Data.system_version]);
  if Data.Fsettings_need then
    AddParam(Params, ['settings', Data.settings]);
  if Data.Fsandbox_need then
    AddParam(Params, ['sandbox', Data.sandbox]);
  Response := Handler.Execute('account.registerDevice', Params);
  Result := Response.Success and (Response.Response = '1');
end;

function TAccountController.SaveProfileInfo(const Data: TProfileInfoData; var Response: TResponse): Boolean;
var
  Params: TParams;
begin
  if Data.Ffirst_name_need then
    AddParam(Params, ['first_name', Data.first_name]);
  if Data.Flast_name_need then
    AddParam(Params, ['last_name', Data.last_name]);
  if Data.Fmaiden_name_need then
    AddParam(Params, ['maiden_name', Data.maiden_name]);
  if Data.Fscreen_name_need then
    AddParam(Params, ['screen_name', Data.screen_name]);
  if Data.Fcancel_request_id_need then
    AddParam(Params, ['cancel_request_id', Data.cancel_request_id]);
  if Data.Fsex_need then
    AddParam(Params, ['sex', Data.sex]);
  if Data.Frelation_need then
    AddParam(Params, ['relation', Data.relation]);
  if Data.Frelation_partner_id_need then
    AddParam(Params, ['relation_partner_id', Data.relation_partner_id]);
  if Data.Fbdate_need then
    AddParam(Params, ['bdate', Data.bdate]);
  if Data.Fbdate_visibility_need then
    AddParam(Params, ['bdate_visibility', Data.bdate_visibility]);
  if Data.Fhome_town_need then
    AddParam(Params, ['home_town', Data.home_town]);
  if Data.Fcountry_id_need then
    AddParam(Params, ['country_id', Data.country_id]);
  if Data.Fcity_id_need then
    AddParam(Params, ['city_id', Data.city_id]);
  if Data.Fstatus_need then
    AddParam(Params, ['status', Data.status]);
  Response := Handler.Execute('account.saveProfileInfo', Params);
  Result := Response.Success;
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
  with Handler.Execute('account.setOnline', ['voip', Ord(Voip).ToString]) do
    Result := Success and (Response = '1');
end;

function TAccountController.SetPushSettings(const DeviceId, Settings, Key, Value: string): Boolean;
var
  Params: TParams;
begin
  Params.Add(['device_id', DeviceId]);
  if not Settings.IsEmpty then
    Params.Add(['settings', Settings]);
  if not Key.IsEmpty then
  begin
    Params.Add(['key', Key]);
    Params.Add(['value', Value]);
  end;
  with Handler.Execute('account.setPushSettings', Params) do
    Result := Success and (Response = '1');
end;

function TAccountController.SetSilenceMode(const DeviceId: string; Time: Integer; PeerId: string;
  Sound: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add(['device_id', DeviceId]);
  Params.Add(['time', Time.ToString]);
  Params.Add(['peer_id', PeerId]);
  Params.Add(['sound', Ord(Sound).ToString]);
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
    AddParam(Params, ['device_id', DeviceId]);
  if not Token.IsEmpty then
    AddParam(Params, ['token', Token]);
  AddParam(Params, ['sandbox', Ord(Sandbox).ToString]);
  with Handler.Execute('account.unregisterDevice', Params) do
    Result := Success and (Response = '1');
end;

end.

