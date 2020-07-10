unit VK.Account;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.AccountInfo,
  VK.Entity.ProfileInfo, VK.Entity.ActiveOffers, VK.Entity.Counters, VK.Entity.PushSettings, VK.Entity.Common,
  VK.Entity.AccountInfoRequest;

type
  TVkRegisterDeviceParams = record
    List: TParams;
    function Token(Value: string): Integer;
    function DeviceModel(Value: string): Integer;
    function DeviceYear(Value: Integer): Integer;
    function DeviceId(Value: string): Integer;
    function SystemVersion(Value: string): Integer;
    function Settings(Value: string): Integer;
    function Sandbox(Value: string): Integer;
  end;

  TVkProfileInfoParams = record
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
    function GetInfo(var Info: TVkAccountInfo; Fields: TFields = []): Boolean;
    function SetInfo(const Name, Value: string): Boolean;
    function GetProfileInfo(var ProfileInfo: TVkProfileInfo): Boolean;
    function Ban(const OwnerID: Integer): Boolean;
    function UnBan(const OwnerID: Integer): Boolean;
    function ChangePassword(var Response: TResponse; NewPassword: string; RestoreSid, ChangePasswordHash, OldPassword:
      string): Boolean;
    function GetActiveOffers(var Offers: TVkActiveOffers; Offset: Integer; Count: Integer = 100): Boolean;
    function GetAppPermissions(var Mask: Int64; UserId: Integer): Boolean;
    function GetCounters(var Counters: TVkCounters; Filter: string = ''): Boolean;
    function GetPushSettings(var PushSettings: TVkPushSettings; DeviceId: string): Boolean;
    function RegisterDevice(const Data: TVkRegisterDeviceParams): Boolean;
    function SaveProfileInfo(const Data: TVkProfileInfoParams; var Request: TVkAccountInfoRequest): Boolean;
    function SetNameInMenu(const UserId: Integer; Name: string): Boolean;
    function SetOffline(): Boolean;
    function SetOnline(Voip: Boolean = False): Boolean;
    function SetPushSettings(const DeviceId, Settings, Key, Value: string): Boolean;
    function SetSilenceMode(const DeviceId: string; Time: Integer; PeerId: string; Sound: Boolean): Boolean;
    function UnRegisterDevice(const DeviceId: string; const Token: string; Sandbox: Boolean): Boolean;
  end;

implementation

{ TAccountController }

function TAccountController.ChangePassword(var Response: TResponse; NewPassword: string; RestoreSid, ChangePasswordHash,
  OldPassword: string): Boolean;
begin
  Response := Handler.Execute('account.changePassword', [
    ['new_password', NewPassword],
    ['restore_sid', RestoreSid],
    ['change_password_hash', ChangePasswordHash],
    ['old_password', OldPassword]]);
  Result := Response.Success;
end;

function TAccountController.GetActiveOffers(var Offers: TVkActiveOffers; Offset: Integer; Count: Integer = 100): Boolean;
begin
  if (Count > 100) or (Count < 0) then
    raise TVkWrongParamException.Create('Count - положительное число, по умолчанию 100, максимальное значение 100');
  if (Offset < 0) then
    raise TVkWrongParamException.Create('Count - положительное число, по умолчанию 0');
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

function TAccountController.GetCounters(var Counters: TVkCounters; Filter: string = ''): Boolean;
begin
  if Filter = '' then
    Filter :=
      'friends, messages, photos, videos, notes, gifts, events, groups, notifications, sdk, app_requests, friends_recommendations';
  with Handler.Execute('account.getCounters', ['filter', Filter]) do
  begin
    Result := Success;
    if Result then
      Counters := TVkCounters.FromJsonString(Response);
  end;
end;

function TAccountController.GetInfo(var Info: TVkAccountInfo; Fields: TFields = []): Boolean;
begin
  with Handler.Execute('account.getInfo', ['fields', Fields.ToString]) do
  begin
    Result := Success;
    if Result then
      Info := TVkAccountInfo.FromJsonString(Response);
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

function TAccountController.RegisterDevice(const Data: TVkRegisterDeviceParams): Boolean;
begin
  with Handler.Execute('account.registerDevice', Data.List) do
    Result := Success and (Response = '1');
end;

function TAccountController.SaveProfileInfo(const Data: TVkProfileInfoParams; var Request: TVkAccountInfoRequest): Boolean;
begin
  with Handler.Execute('account.saveProfileInfo', Data.List) do
  begin
    Result := Success;
    if Result then
      Request := TVkAccountInfoRequest.FromJsonString(Response);
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

function TAccountController.SetSilenceMode(const DeviceId: string; Time: Integer; PeerId: string; Sound: Boolean): Boolean;
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

{ TVkRegisterDeviceParams }

function TVkRegisterDeviceParams.DeviceId(Value: string): Integer;
begin
  Result := List.Add('device_id', Value);
end;

function TVkRegisterDeviceParams.DeviceModel(Value: string): Integer;
begin
  Result := List.Add('device_model', Value);
end;

function TVkRegisterDeviceParams.DeviceYear(Value: Integer): Integer;
begin
  Result := List.Add('device_year', Value);
end;

function TVkRegisterDeviceParams.Sandbox(Value: string): Integer;
begin
  Result := List.Add('sandbox', Value);
end;

function TVkRegisterDeviceParams.Settings(Value: string): Integer;
begin
  Result := List.Add('settings', Value);
end;

function TVkRegisterDeviceParams.SystemVersion(Value: string): Integer;
begin
  Result := List.Add('system_version', Value);
end;

function TVkRegisterDeviceParams.Token(Value: string): Integer;
begin
  Result := List.Add('token', Value);
end;

{ TVkProfileInfoParams }

function TVkProfileInfoParams.BirthDate(Value: TDateTime): Integer;
begin
  Result := List.Add('bdate', FormatDateTime('DD.MM.YYYY', Value));
end;

function TVkProfileInfoParams.BirthDateVisibility(Value: TVkBirthDateVisibility): Integer;
begin
  Result := List.Add('bdate_visibility', Ord(Value).ToString);
end;

function TVkProfileInfoParams.CancelRequestId(Value: Integer): Integer;
begin
  Result := List.Add('cancel_request_id', Value);
end;

function TVkProfileInfoParams.CityId(Value: Integer): Integer;
begin
  Result := List.Add('cancel_request_id', Value);
end;

function TVkProfileInfoParams.CountryId(Value: Integer): Integer;
begin
  Result := List.Add('country_id', Value);
end;

function TVkProfileInfoParams.FirstName(Value: string): Integer;
begin
  Result := List.Add('first_name', Value);
end;

function TVkProfileInfoParams.HomeTown(Value: string): Integer;
begin
  Result := List.Add('home_town', Value);
end;

function TVkProfileInfoParams.LastName(Value: string): Integer;
begin
  Result := List.Add('last_name', Value);
end;

function TVkProfileInfoParams.MaidenName(Value: string): Integer;
begin
  Result := List.Add('maiden_name', Value);
end;

function TVkProfileInfoParams.Relation(Value: TVkRelation): Integer;
begin
  Result := List.Add('relation', Ord(Value).ToString);
end;

function TVkProfileInfoParams.RelationPartnerId(Value: Integer): Integer;
begin
  Result := List.Add('relation_partner_id', Value);
end;

function TVkProfileInfoParams.ScreenName(Value: string): Integer;
begin
  Result := List.Add('screen_name', Value);
end;

function TVkProfileInfoParams.Sex(Value: TVkSex): Integer;
begin
  Result := List.Add('sex', Ord(Value).ToString);
end;

function TVkProfileInfoParams.Status(Value: string): Integer;
begin
  Result := List.Add('status', Value);
end;

end.

