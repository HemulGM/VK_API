unit VK.Account;

interface

uses
  System.SysUtils, REST.Client, VK.Entity, VK.Types, VK.Account.Info, VK.Account.ProfileInfo,
  VK.Account.ActiveOffers, VK.Account.Counters, VK.Account.PushSettings;

type
  TAccount = class(TVKEntity)
    function GetInfo(var Info: TAccountInfoClass; Fields: TFields = []): Boolean;
    function SetInfo(const Name, Value: string): Boolean;
    function GetProfileInfo(var ProfileInfo: TProfileInfoClass): Boolean;
    function Ban(const OwnerID: Integer): Boolean;
    function UnBan(const OwnerID: Integer): Boolean;
    function ChangePassword(var Response: TResponse; NewPassword: string; RestoreSid,
      ChangePasswordHash, OldPassword: string): Boolean;
    function GetActiveOffers(var Offers: TActiveOffers; Offset: Integer; Count: Integer = 100): Boolean;
    function GetAppPermissions(var Mask: Int64; UserId: Integer): Boolean;
    function GetCounters(var Counters: TCountersClass; Filter: string = ''): Boolean;
    function GetPushSettings(var PushSettings: TPushSettingsClass; DeviceId: string): Boolean;
    function RegisterDevice(const Data: TRegisterDeviceData): Boolean;
    function SaveProfileInfo(const Data: TProfileInfoData; var Response: TResponse): Boolean;
  end;

implementation

{ TAccount }

function TAccount.ChangePassword(var Response: TResponse; NewPassword: string; RestoreSid,
  ChangePasswordHash, OldPassword: string): Boolean;
begin
  Response := Handler.Execute('account.changePassword', [
    ['new_password', NewPassword],
    ['restore_sid', RestoreSid],
    ['change_password_hash', ChangePasswordHash],
    ['old_password', OldPassword]]);
  Result := Response.Success;
end;

function TAccount.GetActiveOffers(var Offers: TActiveOffers; Offset: Integer; Count: Integer = 100): Boolean;
var
  Response: TResponse;
begin
  if (Count > 100) or (Count < 0) then
    raise Exception.Create('Count - положительное число, по умолчанию 100, максимальное значение 100');
  if (Offset < 0) then
    raise Exception.Create('Count - положительное число, по умолчанию 0');
  Response := Handler.Execute('account.getActiveOffers', [['offset', Offset.ToString], ['count', Count.ToString]]);
  Result := Response.Success;
  if Result then
    Offers := TActiveOffers.FromJsonString(Response.Value);
end;

function TAccount.GetAppPermissions(var Mask: Int64; UserId: Integer): Boolean;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.getAppPermissions', ['user_id', UserId.ToString]);
  Result := Response.Success;
  if Result then
    Mask := StrToIntDef(Response.Value, 0);
end;

function TAccount.GetCounters(var Counters: TCountersClass; Filter: string = ''): Boolean;
var
  Response: TResponse;
begin
  if Filter = '' then
    Filter :=
      'friends, messages, photos, videos, notes, gifts, events, groups, notifications, sdk, app_requests, friends_recommendations';
  Response := Handler.Execute('account.getCounters', ['filter', Filter]);
  Result := Response.Success;
  if Result then
    Counters := TCountersClass.FromJsonString(Response.Value);
end;

function TAccount.GetInfo(var Info: TAccountInfoClass; Fields: TFields = []): Boolean;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.getInfo', ['fields', FieldsToString(Fields)]);
  Result := Response.Success;
  if Result then
    Info := TAccountInfoClass.FromJsonString(Response.Value);
end;

function TAccount.GetProfileInfo(var ProfileInfo: TProfileInfoClass): Boolean;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.getProfileInfo');
  Result := Response.Success;
  if Result then
    ProfileInfo := TProfileInfoClass.FromJsonString(Response.Value);
end;

function TAccount.GetPushSettings(var PushSettings: TPushSettingsClass; DeviceId: string): Boolean;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.getPushSettings');
  Result := Response.Success;
  if Result then
    PushSettings := TPushSettingsClass.FromJsonString(Response.Value);
end;

function TAccount.RegisterDevice(const Data: TRegisterDeviceData): Boolean;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.registerDevice', [
    ['token', Data.token],
    ['device_model', Data.device_model],
    ['device_year', Data.device_year.ToString],
    ['device_id', Data.device_id],
    ['system_version', Data.system_version],
    ['settings', Data.settings],
    ['sandbox', Data.sandbox]]);
  Result := Response.Success and (Response.Value = '1');
end;

function TAccount.SaveProfileInfo(const Data: TProfileInfoData; var Response: TResponse): Boolean;
begin
  Response := Handler.Execute('account.saveProfileInfo', [
    ['first_name', Data.first_name],
    ['last_name', Data.last_name],
    ['maiden_name', Data.maiden_name],
    ['screen_name', Data.screen_name],
    ['cancel_request_id', Data.cancel_request_id],
    ['sex', Data.sex],
    ['relation', Data.relation],
    ['relation_partner_id', Data.relation_partner_id],
    ['bdate', Data.bdate],
    ['bdate_visibility', Data.bdate_visibility],
    ['home_town', Data.home_town],
    ['country_id', Data.country_id],
    ['city_id', Data.city_id],
    ['relation', Data.status]]);
  Result := Response.Success;
end;

function TAccount.SetInfo(const Name, Value: string): Boolean;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.setInfo', [['name', Name], ['value', Value]]);
  Result := Response.Success and (Response.Value = '1');
end;

function TAccount.Ban(const OwnerID: Integer): Boolean;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.ban', ['owner_id', OwnerID.ToString]);
  Result := Response.Success and (Response.Value = '1');
end;

function TAccount.UnBan(const OwnerID: Integer): Boolean;
var
  Response: TResponse;
begin
  Response := Handler.Execute('account.unban', ['owner_id', OwnerID.ToString]);
  Result := Response.Success and (Response.Value = '1');
end;

end.

