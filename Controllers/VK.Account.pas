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
    /// <summary>
    /// Идентификатор устройства, используемый для отправки уведомлений.
    /// (для mpns идентификатор должен представлять из себя URL для отправки уведомлений)
    /// </summary>
    function Token(const Value: string): TVkParamsRegisterDevice;
    /// <summary>
    /// Строковое название модели устройства
    /// </summary>
    function DeviceModel(const Value: string): TVkParamsRegisterDevice;
    /// <summary>
    /// Год устройства
    /// </summary>
    function DeviceYear(const Value: Integer): TVkParamsRegisterDevice;
    /// <summary>
    /// Уникальный идентификатор устройства
    /// </summary>
    function DeviceId(const Value: string): TVkParamsRegisterDevice;
    /// <summary>
    /// Строковая версия операционной системы устройства
    /// </summary>
    function SystemVersion(const Value: string): TVkParamsRegisterDevice;
    /// <summary>
    /// Настройки уведомлений (https://vk.com/dev/push_settings)
    /// </summary>
    function Settings(const Value: string): TVkParamsRegisterDevice;
    /// <summary>
    /// Флаг предназначен для iOS устройств.
    /// True — использовать Sandbox сервер для отправки push-уведомлений, False — не использовать
    /// </summary>
    function Sandbox(const Value: string): TVkParamsRegisterDevice;
  end;

  TVkParamsProfileInfo = record
    List: TParams;
    /// <summary>
    /// Имя пользователя. Обязательно с большой буквы
    /// </summary>
    function FirstName(const Value: string): TVkParamsProfileInfo;
    /// <summary>
    /// Фамилия пользователя. Обязательно с большой буквы
    /// </summary>
    function LastName(const Value: string): TVkParamsProfileInfo;
    /// <summary>
    /// Девичья фамилия пользователя (только для женского пола)
    /// </summary>
    function MaidenName(const Value: string): TVkParamsProfileInfo;
    /// <summary>
    /// Короткое имя страницы
    /// </summary>
    function ScreenName(const Value: string): TVkParamsProfileInfo;
    /// <summary>
    /// Идентификатор заявки на смену имени, которую необходимо отменить.
    /// Если передан этот параметр, все остальные параметры игнорируются
    /// </summary>
    function CancelRequestId(const Value: Integer): TVkParamsProfileInfo;
    /// <summary>
    /// Пол пользователя
    /// </summary>
    function Sex(const Value: TVkSex): TVkParamsProfileInfo;
    /// <summary>
    /// Семейное положение пользователя
    /// </summary>
    function Relation(const Value: TVkRelation): TVkParamsProfileInfo;
    /// <summary>
    /// Идентификатор пользователя, с которым связано семейное положение
    /// </summary>
    function RelationPartnerId(const Value: TVkPeerId): TVkParamsProfileInfo;
    /// <summary>
    /// Дата рождения пользователя
    /// </summary>
    function BirthDate(const Value: TDateTime): TVkParamsProfileInfo;
    /// <summary>
    /// Видимость даты рождения
    /// </summary>
    function BirthDateVisibility(const Value: TVkBirthDateVisibility): TVkParamsProfileInfo;
    /// <summary>
    /// Родной город пользователя
    /// </summary>
    function HomeTown(const Value: string): TVkParamsProfileInfo;
    /// <summary>
    /// Идентификатор страны пользователя
    /// </summary>
    function CountryId(const Value: Integer): TVkParamsProfileInfo;
    /// <summary>
    /// Идентификатор города пользователя
    /// </summary>
    function CityId(const Value: Integer): TVkParamsProfileInfo;
    /// <summary>
    /// Статус пользователя, который также может быть изменен методом status.set
    /// </summary>
    function Status(const Value: string): TVkParamsProfileInfo;
  end;

  TAccountController = class(TVkController)
  public
    /// <summary>
    /// Добавляет пользователя или группу в черный список.
    /// </summary>
    function Ban(var Status: Boolean; const OwnerId: TVkPeerId): Boolean;
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
    function GetAppPermissions(var Mask: Integer; UserId: TVkPeerId): Boolean;
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
    /// При отсутствии поля settings в запросе будут применены текущие настройки.
    /// Для нового идентификатора устройства token будут применены последние настройки для данного DeviceId
    /// </summary>
    function RegisterDevice(const Data: TVkParamsRegisterDevice): Boolean;
    /// <summary>
    /// Редактирует информацию текущего профиля.
    /// </summary>
    function SaveProfileInfo(const Data: TVkParamsProfileInfo; var Request: TVkAccountInfoRequest): Boolean;
    /// <summary>
    /// Позволяет редактировать информацию о текущем аккаунте.
    /// </summary>
    function SetInfo(const Name, Value: string): Boolean;
    /// <summary>
    /// Устанавливает короткое название приложения (до 17 символов), которое выводится пользователю в левом меню.
    /// </summary>
    function SetNameInMenu(const UserId: TVkPeerId; Name: string): Boolean;
    /// <summary>
    /// Помечает текущего пользователя как offline (только в текущем приложении).
    /// </summary>
    function SetOffline: Boolean;
    /// <summary>
    /// Помечает текущего пользователя как online на 5 минут.
    /// </summary>
    function SetOnline(const Voip: Boolean = False): Boolean;
    /// <summary>
    /// Изменяет настройку Push-уведомлений.
    /// </summary>
    function SetPushSettings(const DeviceId, Settings, Key, Value: string): Boolean;
    /// <summary>
    /// Отключает push-уведомления на заданный промежуток времени.
    /// </summary>
    function SetSilenceMode(const DeviceId: string; Time: Integer; PeerId: TVkPeerId; Sound: Boolean): Boolean;
    /// <summary>
    /// Удаляет пользователя или группу из черного списка.
    /// OwnerId - Идентификатор пользователя или группы, которого нужно удалить из черного списка.
    /// </summary>
    function UnBan(const OwnerId: TVkPeerId): Boolean;
    /// <summary>
    /// Отписывает устройство от Push уведомлений.
    /// </summary>
    function UnRegisterDevice(const DeviceId: string; const Token: string; Sandbox: Boolean): Boolean;
  end;

implementation

uses
  System.DateUtils;

{ TAccountController }

function TAccountController.ChangePassword(var Token: string; NewPassword: string; RestoreSid, ChangePasswordHash, OldPassword: string): Boolean;
begin
  Result := Handler.Execute('account.changePassword', [
    ['new_password', NewPassword],
    ['restore_sid', RestoreSid],
    ['change_password_hash', ChangePasswordHash],
    ['old_password', OldPassword]]).
    GetValue('token', Token);
end;

function TAccountController.GetActiveOffers(var Items: TVkActiveOffers; Count: Integer; Offset: Integer): Boolean;
begin
  Result := Handler.Execute('account.getActiveOffers', [
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject(Items);
end;

function TAccountController.GetAppPermissions(var Mask: Integer; UserId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('account.getAppPermissions', ['user_id', UserId.ToString]).ResponseAsInt(Mask);
end;

function TAccountController.GetBanned(var Items: TVkBannedList; Count, Offset: Integer): Boolean;
begin
  Result := Handler.Execute('account.getBanned', [
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject(Items);
end;

function TAccountController.GetCounters(var Counters: TVkCounters; Filter: TVkCounterFilters): Boolean;
begin
  Result := Handler.Execute('account.getCounters', ['filter', Filter.ToString]).GetObject(Counters);
end;

function TAccountController.GetInfo(var Info: TVkAccountInfo; Fields: TVkInfoFilters = []): Boolean;
begin
  Result := Handler.Execute('account.getInfo', ['fields', Fields.ToString]).GetObject(Info);
end;

function TAccountController.GetProfileInfo(var ProfileInfo: TVkProfileInfo): Boolean;
begin
  Result := Handler.Execute('account.getProfileInfo').GetObject(ProfileInfo);
end;

function TAccountController.GetPushSettings(var PushSettings: TVkPushSettings; DeviceId: string): Boolean;
begin
  Result := Handler.Execute('account.getPushSettings', ['device_id', DeviceId]).GetObject(PushSettings);
end;

function TAccountController.RegisterDevice(const Data: TVkParamsRegisterDevice): Boolean;
begin
  Result := Handler.Execute('account.registerDevice', Data.List).ResponseIsTrue;
end;

function TAccountController.SaveProfileInfo(const Data: TVkParamsProfileInfo; var Request: TVkAccountInfoRequest): Boolean;
begin
  Result := Handler.Execute('account.saveProfileInfo', Data.List).GetObject(Request);
end;

function TAccountController.SetInfo(const Name, Value: string): Boolean;
begin
  Result := Handler.Execute('account.setInfo', [['name', Name], ['value', Value]]).ResponseIsTrue;
end;

function TAccountController.SetNameInMenu(const UserId: TVkPeerId; Name: string): Boolean;
begin
  Result := Handler.Execute('account.setNameInMenu', [['user_id', UserId.ToString], ['name', Name]]).ResponseIsTrue;
end;

function TAccountController.SetOffline: Boolean;
begin
  Result := Handler.Execute('account.setOffline').ResponseIsTrue;
end;

function TAccountController.SetOnline(const Voip: Boolean): Boolean;
begin
  Result := Handler.Execute('account.setOnline', ['voip', BoolToString(Voip)]).ResponseIsTrue;
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
  Result := Handler.Execute('account.setPushSettings', Params).ResponseIsTrue;
end;

function TAccountController.SetSilenceMode(const DeviceId: string; Time: Integer; PeerId: TVkPeerId; Sound: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('device_id', DeviceId);
  Params.Add('time', Time);
  Params.Add('peer_id', PeerId);
  Params.Add('sound', Sound);
  Result := Handler.Execute('account.setSilenceMode', Params).ResponseIsTrue;
end;

function TAccountController.Ban(var Status: Boolean; const OwnerId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('account.ban', ['owner_id', OwnerId.ToString]).ResponseAsBool(Status);
end;

function TAccountController.UnBan(const OwnerId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('account.unban', ['owner_id', OwnerId.ToString]).ResponseIsTrue;
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
  Result := Handler.Execute('account.unregisterDevice', Params).ResponseIsTrue;
end;

{ TVkRegisterDeviceParams }

function TVkParamsRegisterDevice.DeviceId(const Value: string): TVkParamsRegisterDevice;
begin
  List.Add('device_id', Value);
  Result := Self;
end;

function TVkParamsRegisterDevice.DeviceModel(const Value: string): TVkParamsRegisterDevice;
begin
  List.Add('device_model', Value);
  Result := Self;
end;

function TVkParamsRegisterDevice.DeviceYear(const Value: Integer): TVkParamsRegisterDevice;
begin
  List.Add('device_year', Value);
  Result := Self;
end;

function TVkParamsRegisterDevice.Sandbox(const Value: string): TVkParamsRegisterDevice;
begin
  List.Add('sandbox', Value);
  Result := Self;
end;

function TVkParamsRegisterDevice.Settings(const Value: string): TVkParamsRegisterDevice;
begin
  List.Add('settings', Value);
  Result := Self;
end;

function TVkParamsRegisterDevice.SystemVersion(const Value: string): TVkParamsRegisterDevice;
begin
  List.Add('system_version', Value);
  Result := Self;
end;

function TVkParamsRegisterDevice.Token(const Value: string): TVkParamsRegisterDevice;
begin
  List.Add('token', Value);
  Result := Self;
end;

{ TVkProfileInfoParams }

function TVkParamsProfileInfo.BirthDate(const Value: TDateTime): TVkParamsProfileInfo;
begin
  List.Add('bdate', Value, 'DD.MM.YYYY');
  Result := Self;
end;

function TVkParamsProfileInfo.BirthDateVisibility(const Value: TVkBirthDateVisibility): TVkParamsProfileInfo;
begin
  List.Add('bdate_visibility', Ord(Value));
  Result := Self;
end;

function TVkParamsProfileInfo.CancelRequestId(const Value: Integer): TVkParamsProfileInfo;
begin
  List.Add('cancel_request_id', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.CityId(const Value: Integer): TVkParamsProfileInfo;
begin
  List.Add('city_id', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.CountryId(const Value: Integer): TVkParamsProfileInfo;
begin
  List.Add('country_id', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.FirstName(const Value: string): TVkParamsProfileInfo;
begin
  List.Add('first_name', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.HomeTown(const Value: string): TVkParamsProfileInfo;
begin
  List.Add('home_town', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.LastName(const Value: string): TVkParamsProfileInfo;
begin
  List.Add('last_name', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.MaidenName(const Value: string): TVkParamsProfileInfo;
begin
  List.Add('maiden_name', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.Relation(const Value: TVkRelation): TVkParamsProfileInfo;
begin
  List.Add('relation', Ord(Value));
  Result := Self;
end;

function TVkParamsProfileInfo.RelationPartnerId(const Value: TVkPeerId): TVkParamsProfileInfo;
begin
  List.Add('relation_partner_id', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.ScreenName(const Value: string): TVkParamsProfileInfo;
begin
  List.Add('screen_name', Value);
  Result := Self;
end;

function TVkParamsProfileInfo.Sex(const Value: TVkSex): TVkParamsProfileInfo;
begin
  List.Add('sex', Ord(Value));
  Result := Self;
end;

function TVkParamsProfileInfo.Status(const Value: string): TVkParamsProfileInfo;
begin
  List.Add('status', Value);
  Result := Self;
end;

end.

