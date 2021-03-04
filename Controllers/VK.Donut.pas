unit VK.Donut;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Profile, VK.Entity.Donut;

type
  TVkParamsDonutGetFriends = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsDonutGetFriends;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkProfileFields = []): TVkParamsDonutGetFriends;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества друзей
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsDonutGetFriends;
    /// <summary>
    /// Количество друзей, информацию о которых необходимо вернуть (максимальное значение 100)
    /// </summary>
    function Count(const Value: Integer = 10): TVkParamsDonutGetFriends;
  end;

  TVkParamsDonutGetSubscriptions = record
    List: TParams;
    /// <summary>
    /// Список дополнительных полей профилей и групп, которые необходимо вернуть
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsDonutGetSubscriptions;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества подписок
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsDonutGetSubscriptions;
    /// <summary>
    /// Количество подписок, информацию о которых необходимо вернуть (максимальное значение 100)
    /// </summary>
    function Count(const Value: Integer = 10): TVkParamsDonutGetSubscriptions;
  end;

  /// <summary>
  /// Методы для работы с донатом.
  /// </summary>
  TDonutController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список донов, которые подписаны на определенные сообщества, из числа друзей пользователя.
    /// </summary>
    function GetFriends(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список донов, которые подписаны на определенные сообщества, из числа друзей пользователя.
    /// </summary>
    function GetFriends(var Items: TVkProfiles; Params: TVkParamsDonutGetFriends): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о подписке VK Donut.
    /// </summary>
    function GetSubscription(var Item: TVkDonutSubscription; OwnerId: Integer): Boolean;
    /// <summary>
    /// Возвращает информацию о подписках пользователя.
    /// </summary>
    function GetSubscriptions(var Items: TVkDonutSubscriptions; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о подписках пользователя.
    /// </summary>
    function GetSubscriptions(var Items: TVkDonutSubscriptions; Params: TVkParamsDonutGetSubscriptions): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о том, подписан ли пользователь на платный контент (является доном).
    /// </summary>
    function IsDon(OwnerId: Integer): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TDonutController }

function TDonutController.GetFriends(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('donut.getFriends').GetObject(Items);
end;

function TDonutController.GetFriends(var Items: TVkProfiles; Params: TVkParamsDonutGetFriends): Boolean;
begin
  Result := GetFriends(Items, Params.List);
end;

function TDonutController.GetSubscription(var Item: TVkDonutSubscription; OwnerId: Integer): Boolean;
begin
  Result := Handler.Execute('donut.getSubscription').GetObject(Item);
end;

function TDonutController.GetSubscriptions(var Items: TVkDonutSubscriptions; Params: TVkParamsDonutGetSubscriptions): Boolean;
begin
  Result := GetSubscriptions(Items, Params.List);
end;

function TDonutController.IsDon(OwnerId: Integer): Boolean;
begin
  Result := Handler.Execute('donut.isDon').ResponseIsTrue;
end;

function TDonutController.GetSubscriptions(var Items: TVkDonutSubscriptions; Params: TParams): Boolean;
begin
  Result := Handler.Execute('donut.getSubscriptions').GetObject(Items);
end;

{ TVkParamsDonutGetFriends }

function TVkParamsDonutGetFriends.Offset(const Value: Integer): TVkParamsDonutGetFriends;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsDonutGetFriends.OwnerId(const Value: Integer): TVkParamsDonutGetFriends;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsDonutGetFriends.Count(const Value: Integer): TVkParamsDonutGetFriends;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsDonutGetFriends.Fields(const Value: TVkProfileFields): TVkParamsDonutGetFriends;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

{ TVkParamsDonutGetSubscriptions }

function TVkParamsDonutGetSubscriptions.Count(const Value: Integer): TVkParamsDonutGetSubscriptions;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsDonutGetSubscriptions.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsDonutGetSubscriptions;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsDonutGetSubscriptions.Offset(const Value: Integer): TVkParamsDonutGetSubscriptions;
begin
  List.Add('offset', Value);
  Result := Self;
end;

end.

