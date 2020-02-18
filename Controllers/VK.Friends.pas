unit VK.Friends;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.User;

type
  TVkFriendsSort = (fsNone, fsHints, fsRandom, fsMobile, fsName);

  TVkFriendsSortHelper = record helper for TVkFriendsSort
    function ToConst: string; inline;
  end;
  {hints � ����������� �� ��������, ���������� ����, ��� ������ ����������� � ������� ��� ������ (��� �������� �������� ������ ��� Standalone-���������� � ������ �������, ���������� �� ����� Implicit Flow.).
  random � ���������� ������ � ��������� �������.
  mobile � ���������� ���� ��� ������, � ������� ����������� ��������� ����������.
  name � ����������� �� �����. ������ ��� ���������� �������� ��������, ��� ��� ������ ����� �������� ���� ������ � �� ������ ��������� ���������� count. (�������� ������ ��� ���������� ��������� fields).}

  TVkFriendsGetParams = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function Order(Value: TVkFriendsSort): Integer;
    function ListId(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Fields(Value: string): Integer;
    function NameCase(Value: TVkNameCase): Integer;
    function Ref(Value: string): Integer;
  end;

  TFriendsController = class(TVkController)
  public
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� �����������
    /// ���������� � ������� ������������ (��� ������������� ��������� fields).
    /// </summary>
    function Get(var Users: TVkUsers; UserId: Integer = -1; Fields: string = ''; Order:
      TVkFriendsSort = fsNone): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� �����������
    /// ���������� � ������� ������������ (��� ������������� ��������� fields).
    /// </summary>
    function Get(var Users: TVkUsers; Fields: string; Order: TVkFriendsSort = fsNone): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� �����������
    /// ���������� � ������� ������������ (��� ������������� ��������� fields).
    /// </summary>
    function Get(var Users: TVkUsers; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������ ������������ ��� �����������
    /// ���������� � ������� ������������ (��� ������������� ��������� fields).
    /// </summary>
    function Get(var Users: TVkUsers; Params: TVkFriendsGetParams): Boolean; overload;
  end;

implementation

uses
  VK.API;

{ TFriendsController }

function TFriendsController.Get(var Users: TVkUsers; Fields: string; Order: TVkFriendsSort): Boolean;
begin
  Result := Get(Users, -1, Fields, Order);
end;

function TFriendsController.Get(var Users: TVkUsers; UserId: Integer; Fields: string; Order: TVkFriendsSort): Boolean;
var
  Params: TParams;
begin
  if UserId > 0 then
    Params.Add('user_id', UserId);
  if not Fields.IsEmpty then
    Params.Add('fields', Fields)
  else
    Params.Add('fields', 'domian');
  if Order <> fsNone then
    Params.Add('order', Order.ToConst);
  Result := Get(Users, Params);
end;

function TFriendsController.Get(var Users: TVkUsers; Params: TVkFriendsGetParams): Boolean;
begin
  Result := Get(Users, Params.List);
end;

function TFriendsController.Get(var Users: TVkUsers; Params: TParams): Boolean;
begin
  with Handler.Execute('friends.get', Params) do
  begin
    Result := Success;
    try
      if Result then
        Users := TVkUsers.FromJsonString(Response);
    except
      raise TVkParserException.Create('������ �������������� �������� Json');
    end;
  end;
end;

{ TVkFriendsGetParams }

function TVkFriendsGetParams.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkFriendsGetParams.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkFriendsGetParams.ListId(Value: Integer): Integer;
begin
  Result := List.Add('list_id', Value);
end;

function TVkFriendsGetParams.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToConst);
end;

function TVkFriendsGetParams.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkFriendsGetParams.Order(Value: TVkFriendsSort): Integer;
begin
  Result := List.Add('order', Value.ToConst);
end;

function TVkFriendsGetParams.Ref(Value: string): Integer;
begin
  Result := List.Add('ref', Value);
end;

function TVkFriendsGetParams.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkFriendsSortHelper }

function TVkFriendsSortHelper.ToConst: string;
begin
  case Self of
    fsHints:
      Exit('hints');
    fsRandom:
      Exit('random');
    fsMobile:
      Exit('mobile');
    fsName:
      Exit('name');
  else
    Exit('');
  end;
end;

end.

