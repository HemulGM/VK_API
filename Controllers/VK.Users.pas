unit VK.Users;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Profile, VK.Entity.Subscription;

type
  TVkParamsUsersGet = record
    List: TParams;
    /// <summary>
    /// ������������� ����� ������� �������������� ������������� ��� �� �������� ����� (ScreenName).
    /// �� ��������� � ������������� �������� ������������
    /// </summary>
    function UserIds(const Value: TIdList): Integer;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: string): Integer; overload;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer; overload;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer; overload;
  end;

  TVkParamsUsersGetFollowers = record
    List: TParams;
    /// <summary>
    /// ������������� ������������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// ���������� �����������, ���������� � ������� ����� ��������
    /// </summary>
    function Count(const Value: Integer = 100): Integer; overload;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �����������
    /// </summary>
    function Offset(const Value: Integer): Integer; overload;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: string): Integer; overload;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer; overload;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer; overload;
  end;

  TVkParamsUsersGetSubscriptions = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, �������� �������� ���������� ��������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// True � ���������� ������������ ������, ���������� ������� group � user ������.
    /// False � ���������� ������ ��������������� ����� � ������������� ��������. (�� ���������)
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// �������� ����������� ��� ������� ������������� ������������ ��������.
    /// ���� �������� ������������ ������ ���� ������� Extended = True
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ��������, ������� ���������� �������.
    /// ���� �������� ������������ ������ ���� ������� Extended = True
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// ������ �������������� ����� ��� �������� User � Group, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsUsersSearch = record
    List: TParams;
    /// <summary>
    /// ������ ���������� �������
    /// </summary>
    function Query(const Value: string): Integer;
    /// <summary>
    /// ���������� �����������
    /// </summary>
    function Sort(const Value: TVkSortUser): Integer;
    /// <summary>
    /// �������� ������������ ������� ���������� ������������ ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ������������ �������������
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function City(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function Country(const Value: Integer): Integer;
    /// <summary>
    /// �������� ������ �������
    /// </summary>
    function Hometown(const Value: string): Integer;
    /// <summary>
    /// ������������� ������, � ������� ������������ ��������� ���
    /// </summary>
    function UniversityCountry(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����
    /// </summary>
    function University(const Value: Integer): Integer;
    /// <summary>
    /// ��� ��������� ����
    /// </summary>
    function UniversityYear(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function UniversityFaculty(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function UniversityChair(const Value: Integer): Integer;
    /// <summary>
    /// ���
    /// </summary>
    function Sex(const Value: TVkSex): Integer;
    /// <summary>
    /// �������� ���������
    /// </summary>
    function Status(const Value: TVkRelation): Integer;
    /// <summary>
    /// �������, ��
    /// </summary>
    function AgeFrom(const Value: Integer): Integer;
    /// <summary>
    /// �������, ��
    /// </summary>
    function AgeTo(const Value: Integer): Integer;
    /// <summary>
    /// ���� ��������
    /// </summary>
    function BirthDay(const Value: Integer): Integer;
    /// <summary>
    /// ����� ��������
    /// </summary>
    function BirthMonth(const Value: Integer): Integer;
    /// <summary>
    /// ��� ��������
    /// </summary>
    function BirthYear(const Value: Integer): Integer;
    /// <summary>
    /// ��������� �� ������ �������
    /// </summary>
    function Online(const Value: Boolean): Integer;
    /// <summary>
    /// ��������� �� ������� ����
    /// </summary>
    function HasPhoto(const Value: Boolean): Integer;
    /// <summary>
    /// ������������� ������, � ������� ������������ ��������� �����
    /// </summary>
    function SchoolCountry(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������, � ������� ������������ ��������� �����
    /// </summary>
    function SchoolCity(const Value: Integer): Integer;
    /// <summary>
    /// ����� ������
    /// </summary>
    function SchoolClass(const Value: TIdList): Integer;
    /// <summary>
    /// ������������� �����, ������� ��������� ������������
    /// </summary>
    function School(const Value: Integer): Integer;
    /// <summary>
    /// ��� ��������� �����
    /// </summary>
    function SchoolYear(const Value: Integer): Integer;
    /// <summary>
    /// ����������� �������
    /// </summary>
    function Religion(const Value: string): Integer;
    /// <summary>
    /// �������� ��������, � ������� �������� ������������
    /// </summary>
    function Company(const Value: string): Integer;
    /// <summary>
    /// �������� ���������
    /// </summary>
    function Position(const Value: string): Integer;
    /// <summary>
    /// ������������� ������, ����� ������������� ������� ���������� ��������� �����
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ������� ����� ������� ����� ����������� �����
    /// </summary>
    function FromList(const Value: TVkSearchTargets): Integer;
  end;

  TUsersController = class(TVkController)
  public
    /// <summary>
    /// ���������� ����������� ���������� � �������������.
    /// ���� Counters, Military ����� ���������� ������ � ������, ���� ������� ����� ���� UserId
    /// </summary>
    function Get(var Items: TVkProfiles; UserIds: TIdList; Fields: TVkProfileFields = []; NameCase: TVkNameCase = TVkNameCase.Nom): Boolean; overload;
    /// <summary>
    /// ���������� ����������� ���������� � �������������.
    /// ���� Counters, Military ����� ���������� ������ � ������, ���� ������� ����� ���� UserId
    /// </summary>
    function Get(var User: TVkProfile; UserId: Integer; Fields: TVkProfileFields = []; NameCase: TVkNameCase = TVkNameCase.Nom): Boolean; overload;
    /// <summary>
    /// ���������� ����������� ���������� � �������������.
    /// ���� Counters, Military ����� ���������� ������ � ������, ���� ������� ����� ���� UserId
    /// </summary>
    function Get(var User: TVkProfile; Fields: TVkProfileFields = []; NameCase: TVkNameCase = TVkNameCase.Nom): Boolean; overload;
    /// <summary>
    /// ���������� ����������� ���������� � �������������.
    /// ���� Counters, Military ����� ���������� ������ � ������, ���� ������� ����� ���� UserId
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ����������� ���������� � �������������.
    /// ���� Counters, Military ����� ���������� ������ � ������, ���� ������� ����� ���� UserId
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TVkParamsUsersGet): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� �������������, ������� �������� ������������ ������������.
    /// </summary>
    function GetFollowers(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� �������������, ������� �������� ������������ ������������.
    /// </summary>
    function GetFollowers(var Items: TVkProfiles; Params: TVkParamsUsersGetFollowers): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������������� � ��������� �������, ������� ������ � ������ �������� ������������.
    /// </summary>
    function GetSubscriptions(var Items: TVkSubscriptions; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������������� ������������� � ��������� �������, ������� ������ � ������ �������� ������������.
    /// </summary>
    function GetSubscriptions(var Items: TVkSubscriptions; Params: TVkParamsUsersGetSubscriptions): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� ������������.
    /// </summary>
    function Report(UserId: Integer; Reason: TVkUserReport = TVkUserReport.Spam; Comment: string = ''): Boolean;
    /// <summary>
    /// ���������� ������ ������������� � ������������ � �������� ��������� ������.
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������� � ������������ � �������� ��������� ������.
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TVkParamsUsersSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TUsersController }

function TUsersController.Get(var User: TVkProfile; UserId: Integer; Fields: TVkProfileFields; NameCase: TVkNameCase): Boolean;
var
  Params: TVkParamsUsersGet;
  Users: TVkProfiles;
begin
  if UserId <> 0 then
    Params.UserIds([UserId]);
  if Fields <> [] then
    Params.Fields(Fields);
  Params.NameCase(NameCase);
  with Handler.Execute('users.get', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Users := TVkProfiles.FromJsonString<TVkProfiles>(ResponseAsItems);
        try
          if Length(Users.Items) > 0 then
          begin
            Users.SaveObjects := True;
            User := Users.Items[0];
          end
          else
            Result := False;
        finally
          Users.Free;
        end;
      except
        Result := False;
      end;
    end;
  end;
end;

function TUsersController.Get(var Items: TVkProfiles; UserIds: TIdList; Fields: TVkProfileFields; NameCase: TVkNameCase): Boolean;
var
  Params: TParams;
begin
  if Length(UserIds) > 0 then
    Params.Add('user_ids', UserIds);
  if Fields <> [] then
    Params.Add('fields', Fields.ToString);
  Params.Add('num', NameCase.ToString);
  Result := Get(Items, Params);
end;

function TUsersController.Get(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('users.get', Params).GetObjects<TVkProfiles>(Items);
end;

function TUsersController.Get(var Items: TVkProfiles; Params: TVkParamsUsersGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TUsersController.Get(var User: TVkProfile; Fields: TVkProfileFields; NameCase: TVkNameCase): Boolean;
begin
  Result := Get(User, 0, Fields, NameCase);
end;

function TUsersController.GetFollowers(var Items: TVkProfiles; Params: TVkParamsUsersGetFollowers): Boolean;
begin
  Result := GetFollowers(Items, Params.List);
end;

function TUsersController.GetSubscriptions(var Items: TVkSubscriptions; Params: TVkParamsUsersGetSubscriptions): Boolean;
begin
  Result := GetSubscriptions(Items, Params.List);
end;

function TUsersController.Report(UserId: Integer; Reason: TVkUserReport; Comment: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('user_id', UserId);
  Params.Add('type', Reason.ToString);
  if not Comment.IsEmpty then
    Params.Add('comment', Comment);
  with Handler.Execute('users.report', Params) do
    Result := Success and ResponseIsTrue;
end;

function TUsersController.Search(var Items: TVkProfiles; Params: TVkParamsUsersSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TUsersController.Search(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('users.search', Params).GetObject<TVkProfiles>(Items);
end;

function TUsersController.GetSubscriptions(var Items: TVkSubscriptions; Params: TParams): Boolean;
begin
  Result := Handler.Execute('users.getSubscriptions', Params).GetObject<TVkSubscriptions>(Items);
end;

function TUsersController.GetFollowers(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('users.getFollowers', Params).GetObject<TVkProfiles>(Items);
end;

{ TVkParamsUsersGet }

function TVkParamsUsersGet.Fields(const Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsUsersGet.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsUsersGet.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsUsersGet.UserIds(const Value: TIdList): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsUsersGetFollowers }

function TVkParamsUsersGetFollowers.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsUsersGetFollowers.Fields(const Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsUsersGetFollowers.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsUsersGetFollowers.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsUsersGetFollowers.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsUsersGetFollowers.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsUsersGetSubscriptions }

function TVkParamsUsersGetSubscriptions.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

function TVkParamsUsersGetSubscriptions.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsUsersGetSubscriptions.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsUsersGetSubscriptions.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsUsersGetSubscriptions.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

{ TVkParamsUsersSearch }

function TVkParamsUsersSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsUsersSearch.Sort(const Value: TVkSortUser): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsUsersSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsUsersSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsUsersSearch.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsUsersSearch.City(const Value: Integer): Integer;
begin
  Result := List.Add('city', Value);
end;

function TVkParamsUsersSearch.Country(const Value: Integer): Integer;
begin
  Result := List.Add('country', Value);
end;

function TVkParamsUsersSearch.Hometown(const Value: string): Integer;
begin
  Result := List.Add('hometown', Value);
end;

function TVkParamsUsersSearch.UniversityCountry(const Value: Integer): Integer;
begin
  Result := List.Add('university_country', Value);
end;

function TVkParamsUsersSearch.University(const Value: Integer): Integer;
begin
  Result := List.Add('university', Value);
end;

function TVkParamsUsersSearch.UniversityYear(const Value: Integer): Integer;
begin
  Result := List.Add('university_year', Value);
end;

function TVkParamsUsersSearch.UniversityFaculty(const Value: Integer): Integer;
begin
  Result := List.Add('university_faculty', Value);
end;

function TVkParamsUsersSearch.UniversityChair(const Value: Integer): Integer;
begin
  Result := List.Add('university_chair', Value);
end;

function TVkParamsUsersSearch.Sex(const Value: TVkSex): Integer;
begin
  Result := List.Add('sex', Ord(Value));
end;

function TVkParamsUsersSearch.Status(const Value: TVkRelation): Integer;
begin
  Result := List.Add('status', Ord(Value));
end;

function TVkParamsUsersSearch.AgeFrom(const Value: Integer): Integer;
begin
  Result := List.Add('age_from', Value);
end;

function TVkParamsUsersSearch.AgeTo(const Value: Integer): Integer;
begin
  Result := List.Add('age_to', Value);
end;

function TVkParamsUsersSearch.BirthDay(const Value: Integer): Integer;
begin
  Result := List.Add('birth_day', Value);
end;

function TVkParamsUsersSearch.BirthMonth(const Value: Integer): Integer;
begin
  Result := List.Add('birth_month', Value);
end;

function TVkParamsUsersSearch.BirthYear(const Value: Integer): Integer;
begin
  Result := List.Add('birth_year', Value);
end;

function TVkParamsUsersSearch.Online(const Value: Boolean): Integer;
begin
  Result := List.Add('online', Value);
end;

function TVkParamsUsersSearch.HasPhoto(const Value: Boolean): Integer;
begin
  Result := List.Add('has_photo', Value);
end;

function TVkParamsUsersSearch.SchoolCountry(const Value: Integer): Integer;
begin
  Result := List.Add('school_country', Value);
end;

function TVkParamsUsersSearch.SchoolCity(const Value: Integer): Integer;
begin
  Result := List.Add('school_city', Value);
end;

function TVkParamsUsersSearch.SchoolClass(const Value: TIdList): Integer;
begin
  Result := List.Add('school_class', Value);
end;

function TVkParamsUsersSearch.School(const Value: Integer): Integer;
begin
  Result := List.Add('school', Value);
end;

function TVkParamsUsersSearch.SchoolYear(const Value: Integer): Integer;
begin
  Result := List.Add('school_year', Value);
end;

function TVkParamsUsersSearch.Religion(const Value: string): Integer;
begin
  Result := List.Add('religion', Value);
end;

function TVkParamsUsersSearch.Company(const Value: string): Integer;
begin
  Result := List.Add('company', Value);
end;

function TVkParamsUsersSearch.Position(const Value: string): Integer;
begin
  Result := List.Add('position', Value);
end;

function TVkParamsUsersSearch.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsUsersSearch.FromList(const Value: TVkSearchTargets): Integer;
begin
  Result := List.Add('from_list', Value.ToString);
end;

end.

