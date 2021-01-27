unit VK.Donut;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.Profile, VK.Entity.Donut;

type
  TVkParamsDonutGetFriends = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Fields(UserFields: TVkProfileFields = []): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkParamsDonutGetSubscriptions = record
    List: TParams;
    function Fields(GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  /// <summary>
  /// ������ ��� ������ � �������.
  /// </summary>
  TDonutController = class(TVkController)
  public
    /// <summary>
    /// ���������� ������ �����, ������� ��������� �� ������������ ����������, �� ����� ������ ������������.
    /// </summary>
    function GetFriends(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ �����, ������� ��������� �� ������������ ����������, �� ����� ������ ������������.
    /// </summary>
    function GetFriends(var Items: TVkProfiles; Params: TVkParamsDonutGetFriends): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � �������� VK Donut.
    /// </summary>
    function GetSubscription(var Item: TVkDonutSubscription; OwnerId: Integer): Boolean;
    /// <summary>
    /// ���������� ���������� � ��������� ������������.
    /// </summary>
    function GetSubscriptions(var Items: TVkDonutSubscriptions; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ��������� ������������.
    /// </summary>
    function GetSubscriptions(var Items: TVkDonutSubscriptions; Params: TVkParamsDonutGetSubscriptions): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ���, �������� �� ������������ �� ������� ������� (�������� �����).
    /// </summary>
    function IsDon(var Value: Boolean; OwnerId: Integer): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TDonutController }

function TDonutController.GetFriends(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('donut.getFriends').GetObject<TVkProfiles>(Items);
end;

function TDonutController.GetFriends(var Items: TVkProfiles; Params: TVkParamsDonutGetFriends): Boolean;
begin
  Result := GetFriends(Items, Params.List);
end;

function TDonutController.GetSubscription(var Item: TVkDonutSubscription; OwnerId: Integer): Boolean;
begin
  Result := Handler.Execute('donut.getSubscription').GetObject<TVkDonutSubscription>(Item);
end;

function TDonutController.GetSubscriptions(var Items: TVkDonutSubscriptions; Params: TVkParamsDonutGetSubscriptions): Boolean;
begin
  Result := GetSubscriptions(Items, Params.List);
end;

function TDonutController.IsDon(var Value: Boolean; OwnerId: Integer): Boolean;
begin
  with Handler.Execute('donut.isDon') do
  begin
    Result := Success;
    if Result then
      Value := ResponseIsTrue;
  end;
end;

function TDonutController.GetSubscriptions(var Items: TVkDonutSubscriptions; Params: TParams): Boolean;
begin
  Result := Handler.Execute('donut.getSubscriptions').GetObject<TVkDonutSubscriptions>(Items);
end;

{ TVkParamsDonutGetFriends }

function TVkParamsDonutGetFriends.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsDonutGetFriends.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsDonutGetFriends.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsDonutGetFriends.Fields(UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', UserFields.ToString);
end;

{ TVkParamsDonutGetSubscriptions }

function TVkParamsDonutGetSubscriptions.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsDonutGetSubscriptions.Fields(GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsDonutGetSubscriptions.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

end.

