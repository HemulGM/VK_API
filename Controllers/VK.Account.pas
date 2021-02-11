unit VK.Account;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.AccountInfo, VK.Entity.ProfileInfo, VK.Entity.ActiveOffers,
  VK.Entity.Counters, VK.Entity.PushSettings, VK.Entity.Common,
  VK.Entity.AccountInfoRequest, VK.Entity.Account.Banned, VK.CommonUtils;

type
  TVkParamsRegisterDevice = record
    List: TParams;
    function Token(Value: string): TVkParamsRegisterDevice;
    function DeviceModel(Value: string): TVkParamsRegisterDevice;
    function DeviceYear(Value: Integer): TVkParamsRegisterDevice;
    function DeviceId(Value: string): TVkParamsRegisterDevice;
    function SystemVersion(Value: string): TVkParamsRegisterDevice;
    function Settings(Value: string): TVkParamsRegisterDevice;
    function Sandbox(Value: string): TVkParamsRegisterDevice;
  end;

  TVkParamsProfileInfo = record
    List: TParams;
    function FirstName(Value: string): TVkParamsProfileInfo;
    function LastName(Value: string): TVkParamsProfileInfo;
    function MaidenName(Value: string): TVkParamsProfileInfo;
    function ScreenName(Value: string): TVkParamsProfileInfo;
    function CancelRequestId(Value: Integer): TVkParamsProfileInfo;
    function Sex(Value: TVkSex): TVkParamsProfileInfo;
    function Relation(Value: TVkRelation): TVkParamsProfileInfo;
    function RelationPartnerId(Value: Integer): TVkParamsProfileInfo;
    function BirthDate(Value: TDateTime): TVkParamsProfileInfo;
    function BirthDateVisibility(Value: TVkBirthDateVisibility): TVkParamsProfileInfo;
    function HomeTown(Value: string): TVkParamsProfileInfo;
    function CountryId(Value: Integer): TVkParamsProfileInfo;
    function CityId(Value: Integer): TVkParamsProfileInfo;
    function Status(Value: string): TVkParamsProfileInfo;
  end;

  TAccountController = class(TVkController)
  public
    /// <summary>
    /// Добавляет пользователя или группу в черный список.
    /// </summary>
    function Ban(var Status: Boolean; const OwnerID: Integer): Boolean;
    /// <summary>
    /// Позволяет сменить пароль пользователя после успешного восстановления доступа к аккаунту через СМС, используя метод Auth.Restore.
    /// </summary>
    function ChangePassword(var Token: string; NewPassword: string; RestoreSid, ChangePasswordHash, OldPassword: string): Boolean;
    /// <summary>
    /// Возвращает список активных рекламных предложений (офферов), выполнив которые пользователь сможет получить соответствующее количество голосов на свой счёт внутри приложения.
    /// </summary>
    function GetActiveOffers(var Items: TVkActiveOffers; Count: Integer = 100; Offset: Integer = 0): Boolean;
    /// <summary>
    /// Получает настройки текущего пользователя в данном приложении.
    /// </summary>
    function GetAppPermissions(var Mask: Integer; UserId: Integer): Boolean;
    /// <summary>
    /// Возвращает список пользователей, находящихся в черном списке.
    /// </summary>
    function GetBanned(var Items: TVkBannedList; Count: Integer = 20; Offset: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает ненулевые значения счетчиков пользователя.
    /// </summary>
    function GetCounters(var Counters: TVkCounters; Filter: TVkCounterFilters = []): Boolean;
    /// <summary>
    /// Возвращает информацию о текущем аккаунте.
    /// </summary>
    function GetInfo(var Info: TVkAccountInfo; Fields: TVkInfoFilters = []): Boolean;
    /// <summary>
    /// Возвращает информацию о текущем профиле.
    /// </summary>
    function GetProfileInfo(var ProfileInfo: TVkProfileInfo): Boolean;
    /// <summary>
    /// Позволяет получать настройки Push-уведомлений.
    /// </summary>
    function GetPushSettings(var PushSettings: TVkPushSettings; DeviceId: string): Boolean;
    /// <summary>
    /// Подписывает устройство на базе iOS, Android, Windows Phone или Mac на получение Push-уведомлений.
    /// </summary>
    function RegisterDevice(var Status: Boolean; const Data: TVkParamsRegisterDevice): Boolean;
    /// <summary>
    /// Редактирует информацию текущего профиля.
    /// </summary>
    function SaveProfileInfo(const Data: TVkParamsProfileInfo; var Request: TVkAccountInfoRequest): Boolean;
    /// <summary>
    /// Позволяет редактировать информацию о текущем аккаунте.
    /// </summary>
    function SetInfo(var Status: Boolean; const Name, Value: string): Boolean;
    /// <summary>
    /// Устанавливает короткое название приложения (до 17 символов), которое выводится пользователю в левом меню.
    /// </summary>
    function SetNameInMenu(var Status: Boolean; const UserId: Integer; Name: string): Boolean;
    /// <summary>
    /// Помечает текущего пользователя как offline (только в текущем приложении).
    /// </summary>
    function SetOffline(var Status: Boolean): Boolean;
    /// <summary>
    /// Помечает текущего пользователя как online на 5 минут.
    /// </summary>
    function SetOnline(var Status: Boolean; Voip: Boolean = False): Boolean;
    /// <summary>
    /// Изменяет настройку Push-уведомлений.
    /// </summary>
    function SetPushSettings(var Status: Boolean; const DeviceId, Settings, Key, Value: string): Boolean;
    /// <summary>
    /// Отключает push-уведомления на заданный промежуток времени.
    /// </summary>
    function SetSilenceMode(var Status: Boolean; const DeviceId: string; Time: Integer; PeerId: string; Sound: Boolean): Boolean;
    /// <summary>
    /// Удаляет пользователя или группу из черного списка.
    /// </summary>
    function UnBan(var Status: Boolean; const OwnerID: Integer): Boolean;
    /// <summary>
    /// Отписывает устройство от Push уведомлений.
    /// </summary>
    function UnRegisterDevice(var Status: Boolean; const DeviceId: string; const Token: string; Sandbox: Boolean): Boolean;
  end;

implementation

{ TAccountController }

function TAccountController.ChangePassword(var Token: string; NewPassword: string; RestoreSid, ChangePasswordHash, OldPassword: string): Boolean;
begin
  Result := Handler.Execute('account.changePassword', [
    ['new_password', NewPassword],
    ['restore_sid', RestoreSid],
    ['change_password_hash', ChangePasswordHash],
    ['old_password', OldPassword]]).
    GetValue<string>('token', Token) and (not Token.IsEmpty);
end;

function TAccountController.GetActiveOffers(var Items: TVkActiveOffers; Count: Integer; Offset: Integer): Boolean;
begin
  Result := Handler.Execute('account.getActiveOffers', [
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject<TVkActiveOffers>(Items);
end;

function TAccountController.GetAppPermissions(var Mask: Integer; UserId: Integer): Boolean;
begin
  Result := Handler.Execute('account.getAppPermissions', ['user_id', UserId.ToString]).ResponseAsInt(Mask);
end;

function TAccountController.GetBanned(var Items: TVkBannedList; Count, Offset: Integer): Boolean;
begin
  Result := Handler.Execute('account.getBanned', [
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject<TVkBannedList>(Items);
end;

function TAccountController.GetCounters(var Counters: TVkCounters; Filter: TVkCounterFilters): Boolean;
begin
  Result := Handler.Execute('account.getCounters', ['filter', Filter.ToString]).GetObject<TVkCounters>(Counters);
end;

function TAccountController.GetInfo(var Info: TVkAccountInfo; Fields: TVkInfoFilters = []): Boolean;
begin
  Result := Handler.Execute('account.getInfo', ['fields', Fields.ToString]).GetObject<TVkAccountInfo>(Info);
end;

function TAccountController.GetProfileInfo(var ProfileInfo: TVkProfileInfo): Boolean;
begin
  Result := Handler.Execute('account.getProfileInfo').GetObject<TVkProfileInfo>(ProfileInfo);
end;

function TAccountController.GetPushSettings(var PushSettings: TVkPushSettings; DeviceId: string): Boolean;
begin
  Result := Handler.Execute('account.getPushSettings', ['device_id', DeviceId]).GetObject<TVkPushSettings>(PushSettings);
end;

function TAccountController.RegisterDevice(var Status: Boolean; const Data: TVkParamsRegisterDevice): Boolean;
begin
  Result := Handler.Execute('account.registerDevice', Data.List).ResponseAsBool(Status);
end;

function TAccountController.SaveProfileInfo(const Data: TVkParamsProfileInfo; var Request: TVkAccountInfoRequest): Boolean;
begin
  Result := Handler.Execute('account.saveProfileInfo', Data.List).GetObject<TVkAccountInfoRequest>(Request);
end;

function TAccountController.SetInfo(var Status: Boolean; const Name, Value: string): Boolean;
begin
  Result := Handler.Execute('account.setInfo', [['name', Name], ['value', Value]]).ResponseAsBool(Status);
end;

function TAccountController.SetNameInMenu(var Status: Boolean; const UserId: Integer; Name: string): Boolean;
begin
  Result := Handler.Execute('account.setNameInMenu', [['user_id', UserId.ToString], ['name', Name]]).ResponseAsBool(Status);
end;

function TAccountController.SetOffline(var Status: Boolean): Boolean;
begin
  Result := Handler.Execute('account.setOffline').ResponseAsBool(Status);
end;

function TAccountController.SetOnline(var Status: Boolean; Voip: Boolean): Boolean;
begin
  Result := Handler.Execute('account.setOnline', ['voip', BoolToString(Voip)]).ResponseAsBool(Status);
end;

function TAccountController.SetPushSettings(var Status: Boolean; const DeviceId, Settings, Key, Value: string): Boolean;
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
  Result := Handler.Execute('account.setPushSettings', Params).ResponseAsBool(Status);
end;

function TAccountController.SetSilenceMode(var Status: Boolean; const DeviceId: string; Time: Integer; PeerId: string; Sound: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('device_id', DeviceId);
  Params.Add('time', Time);
  Params.Add('peer_id', PeerId);
  Params.Add('sound', Sound);
  Result := Handler.Execute('account.setSilenceMode', Params).ResponseAsBool(Status);
end;

function TAccountController.Ban(var Status: Boolean; const OwnerID: Integer): Boolean;
begin
  Result := Handler.Execute('account.ban', ['owner_id', OwnerID.ToString]).ResponseAsBool(Status);
end;

function TAccountController.UnBan(var Status: Boolean; const OwnerID: Integer): Boolean;
begin
  Result := Handler.Execute('account.unban', ['owner_id', OwnerID.ToString]).ResponseAsBool(Status);
end;

function TAccountController.UnRegisterDevice(var Status: Boolean; const DeviceId: string; const Token: string; Sandbox: Boolean): Boolean;
var
  Params: TParams;
begin
  if not DeviceId.IsEmpty then
    Params.Add('device_id', DeviceId);
  if not Token.IsEmpty then
    Params.Add('token', Token);
  Params.Add('sandbox', Sandbox);
  Result := Handler.Execute('account.unregisterDevice', Params).ResponseAsBool(Status);
end;

{ TVkRegisterDeviceParams }

function TVkParamsRegisterDevice.DeviceId(Value: string): TVkParamsRegisterDevice;
begin
  Result := Self;
  List.Add('device_id', Value);
end;

function TVkParamsRegisterDevice.DeviceModel(Value: string): TVkParamsRegisterDevice;
begin
  Result := Self;
  List.Add('device_model', Value);
end;

function TVkParamsRegisterDevice.DeviceYear(Value: Integer): TVkParamsRegisterDevice;
begin
  Result := Self;
  List.Add('device_year', Value);
end;

function TVkParamsRegisterDevice.Sandbox(Value: string): TVkParamsRegisterDevice;
begin
  Result := Self;
  List.Add('sandbox', Value);
end;

function TVkParamsRegisterDevice.Settings(Value: string): TVkParamsRegisterDevice;
begin
  Result := Self;
  List.Add('settings', Value);
end;

function TVkParamsRegisterDevice.SystemVersion(Value: string): TVkParamsRegisterDevice;
begin
  Result := Self;
  List.Add('system_version', Value);
end;

function TVkParamsRegisterDevice.Token(Value: string): TVkParamsRegisterDevice;
begin
  Result := Self;
  List.Add('token', Value);
end;

{ TVkProfileInfoParams }

function TVkParamsProfileInfo.BirthDate(Value: TDateTime): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('bdate', FormatDateTime('DD.MM.YYYY', Value));
end;

function TVkParamsProfileInfo.BirthDateVisibility(Value: TVkBirthDateVisibility): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('bdate_visibility', Ord(Value));
end;

function TVkParamsProfileInfo.CancelRequestId(Value: Integer): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('cancel_request_id', Value);
end;

function TVkParamsProfileInfo.CityId(Value: Integer): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('city_id', Value);
end;

function TVkParamsProfileInfo.CountryId(Value: Integer): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('country_id', Value);
end;

function TVkParamsProfileInfo.FirstName(Value: string): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('first_name', Value);
end;

function TVkParamsProfileInfo.HomeTown(Value: string): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('home_town', Value);
end;

function TVkParamsProfileInfo.LastName(Value: string): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('last_name', Value);
end;

function TVkParamsProfileInfo.MaidenName(Value: string): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('maiden_name', Value);
end;

function TVkParamsProfileInfo.Relation(Value: TVkRelation): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('relation', Ord(Value));
end;

function TVkParamsProfileInfo.RelationPartnerId(Value: Integer): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('relation_partner_id', Value);
end;

function TVkParamsProfileInfo.ScreenName(Value: string): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('screen_name', Value);
end;

function TVkParamsProfileInfo.Sex(Value: TVkSex): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('sex', Ord(Value));
end;

function TVkParamsProfileInfo.Status(Value: string): TVkParamsProfileInfo;
begin
  Result := Self;
  List.Add('status', Value);
end;

end.

