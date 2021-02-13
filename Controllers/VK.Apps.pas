unit VK.Apps;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.App;

type
  TVkParamsAppsGet = record
    List: TParams;
    function AppId(Value: Integer): TVkParamsAppsGet;
    function AppIds(Value: TIdList): TVkParamsAppsGet;
    function &Platform(Value: TVkPlatform): TVkParamsAppsGet;
    function Extended(Value: Boolean): TVkParamsAppsGet;
    function ReturnFriends(Value: Boolean): TVkParamsAppsGet;
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsAppsGet;
    function NameCase(Value: TVkNameCase): TVkParamsAppsGet;
  end;

  /// <summary>
  /// ћетоды дл€ работы с приложени€ми.
  /// </summary>
  TAppsController = class(TVkController)
  public
    /// <summary>
    /// ”дал€ет все уведомлени€ о запросах, отправленных из текущего приложени€.
    /// </summary>
    function DeleteAppRequests(var Status: Boolean): Boolean;
    /// <summary>
    /// ¬озвращает данные о запрошенном приложении.
    /// </summary>
    function Get(var Items: TVkApps; Params: TParams): Boolean; overload;
    /// <summary>
    /// ¬озвращает данные о запрошенном приложении.
    /// </summary>
    function Get(var Items: TVkApps; Params: TVkParamsAppsGet): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TAppsController }

function TAppsController.DeleteAppRequests(var Status: Boolean): Boolean;
begin
  Result := Handler.Execute('apps.deleteAppRequests').ResponseAsBool(Status);
end;

function TAppsController.Get(var Items: TVkApps; Params: TParams): Boolean;
begin
  Result := Handler.Execute('apps.get').GetObject<TVkApps>(Items);
end;

function TAppsController.Get(var Items: TVkApps; Params: TVkParamsAppsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

{ TVkParamsAppsGet }

function TVkParamsAppsGet.AppId(Value: Integer): TVkParamsAppsGet;
begin
  List.Add('app_ids', Value);
  Result := Self;
end;

function TVkParamsAppsGet.AppIds(Value: TIdList): TVkParamsAppsGet;
begin
  List.Add('app_ids', Value);
  Result := Self;
end;

function TVkParamsAppsGet.&Platform(Value: TVkPlatform): TVkParamsAppsGet;
begin
  List.Add('platform', Value.ToString);
  Result := Self;
end;

function TVkParamsAppsGet.Extended(Value: Boolean): TVkParamsAppsGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsAppsGet.ReturnFriends(Value: Boolean): TVkParamsAppsGet;
begin
  List.Add('return_friends', Value);
  Result := Self;
end;

function TVkParamsAppsGet.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): TVkParamsAppsGet;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsAppsGet.NameCase(Value: TVkNameCase): TVkParamsAppsGet;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

end.

