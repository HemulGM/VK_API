unit VK.Apps;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.App;

type
  TVkParamsAppsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор приложения, данные которого необходимо получить.
    /// Если этот параметр и параметр app_ids не указан,
    /// возвращается идентификатор приложения, через которое выдан ключ доступа (access_token)
    /// </summary>
    function AppId(const Value: Integer): TVkParamsAppsGet;
    /// <summary>
    /// Список идентификаторов приложений, данные которых необходимо получить (не более 100)
    /// </summary>
    function AppIds(const Value: TIdList): TVkParamsAppsGet;
    /// <summary>
    /// Платформа, для которой необходимо вернуть данные
    /// </summary>
    function &Platform(const Value: TVkPlatform = TVkPlatform.Web): TVkParamsAppsGet;
    /// <summary>
    /// True — возвращать дополнительные поля. По умолчанию возвращает только основные поля приложений
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsAppsGet;
    /// <summary>
    /// True – возвращать список друзей, установивших это приложение. По умолчанию: False
    /// Параметр учитывается только при передаче AccessToken
    /// </summary>
    function ReturnFriends(const Value: Boolean = False): TVkParamsAppsGet;
    /// <summary>
    /// Список дополнительных полей, которые необходимо вернуть для профилей пользователей и групп
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsAppsGet;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователей
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsAppsGet;
  end;

  /// <summary>
  /// Методы для работы с приложениями.
  /// </summary>
  TAppsController = class(TVkController)
  public
    /// <summary>
    /// Удаляет все уведомления о запросах, отправленных из текущего приложения.
    /// </summary>
    function DeleteAppRequests: Boolean;
    /// <summary>
    /// Возвращает данные о запрошенном приложении.
    /// </summary>
    function Get(var Items: TVkApps; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает данные о запрошенном приложении.
    /// </summary>
    function Get(var Items: TVkApps; Params: TVkParamsAppsGet): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TAppsController }

function TAppsController.DeleteAppRequests: Boolean;
begin
  Result := Handler.Execute('apps.deleteAppRequests').ResponseIsTrue;
end;

function TAppsController.Get(var Items: TVkApps; Params: TParams): Boolean;
begin
  Result := Handler.Execute('apps.get').GetObject(Items);
end;

function TAppsController.Get(var Items: TVkApps; Params: TVkParamsAppsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

{ TVkParamsAppsGet }

function TVkParamsAppsGet.AppId(const Value: Integer): TVkParamsAppsGet;
begin
  List.Add('app_ids', Value);
  Result := Self;
end;

function TVkParamsAppsGet.AppIds(const Value: TIdList): TVkParamsAppsGet;
begin
  List.Add('app_ids', Value);
  Result := Self;
end;

function TVkParamsAppsGet.&Platform(const Value: TVkPlatform): TVkParamsAppsGet;
begin
  List.Add('platform', Value.ToString);
  Result := Self;
end;

function TVkParamsAppsGet.Extended(const Value: Boolean): TVkParamsAppsGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsAppsGet.ReturnFriends(const Value: Boolean): TVkParamsAppsGet;
begin
  List.Add('return_friends', Value);
  Result := Self;
end;

function TVkParamsAppsGet.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): TVkParamsAppsGet;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsAppsGet.NameCase(const Value: TVkNameCase): TVkParamsAppsGet;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

end.

