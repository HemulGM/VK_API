unit VK.Apps;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.App;

type
  TVkParamsAppsGet = record
    List: TParams;
    function AppId(Value: Integer): Integer;
    function AppIds(Value: TIds): Integer;
    /// <summary>
    /// ios Ч iOS;
    /// android Ч Android;
    /// winphone Ч Windows Phone;
    /// web Ч приложени€ на vk.com.
    /// </summary>
    function &Platform(Value: string): Integer;
    function Extended(Value: Boolean): Integer;
    function ReturnFriends(Value: Boolean): Integer;
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
    function NameCase(Value: TVkNameCase): Integer;
  end;

  /// <summary>
  /// ћетоды дл€ работы с приложени€ми.
  /// </summary>
  TAppsController = class(TVkController)
  public
    /// <summary>
    /// ”дал€ет все уведомлени€ о запросах, отправленных из текущего приложени€.
    /// </summary>
    function DeleteAppRequests: Boolean;
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

function TAppsController.DeleteAppRequests: Boolean;
begin
  with Handler.Execute('apps.deleteAppRequests') do
  begin
    Result := Success and ResponseIsTrue;
  end;
end;

function TAppsController.Get(var Items: TVkApps; Params: TParams): Boolean;
begin
  with Handler.Execute('apps.get') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkApps.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAppsController.Get(var Items: TVkApps; Params: TVkParamsAppsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

{ TVkParamsAppsGet }

function TVkParamsAppsGet.AppId(Value: Integer): Integer;
begin
  Result := List.Add('app_ids', Value);
end;

function TVkParamsAppsGet.AppIds(Value: TIds): Integer;
begin
  Result := List.Add('app_ids', Value);
end;

function TVkParamsAppsGet.&Platform(Value: string): Integer;
begin
  Result := List.Add('platform', Value);
end;

function TVkParamsAppsGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsAppsGet.ReturnFriends(Value: Boolean): Integer;
begin
  Result := List.Add('return_friends', Value);
end;

function TVkParamsAppsGet.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsAppsGet.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

end.

