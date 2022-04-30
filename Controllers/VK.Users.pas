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
    function UserIds(const Value: TIdList): TVkParamsUsersGet;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: string): TVkParamsUsersGet; overload;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsUsersGet; overload;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsUsersGet; overload;
  end;

  TVkParamsUsersGetFollowers = record
    List: TParams;
    /// <summary>
    /// ������������� ������������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsUsersGetFollowers;
    /// <summary>
    /// ���������� �����������, ���������� � ������� ����� ��������
    /// </summary>
    function Count(const Value: Integer = 100): TVkParamsUsersGetFollowers; overload;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �����������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsUsersGetFollowers; overload;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: string): TVkParamsUsersGetFollowers; overload;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsUsersGetFollowers; overload;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsUsersGetFollowers; overload;
  end;

  TVkParamsUsersGetSubscriptions = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, �������� �������� ���������� ��������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsUsersGetSubscriptions;
    /// <summary>
    /// True � ���������� ������������ ������, ���������� ������� group � user ������.
    /// False � ���������� ������ ��������������� ����� � ������������� ��������. (�� ���������)
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsUsersGetSubscriptions;
    /// <summary>
    /// �������� ����������� ��� ������� ������������� ������������ ��������.
    /// ���� �������� ������������ ������ ���� ������� Extended = True
    /// </summary>
    function Offset(const Value: Integer): TVkParamsUsersGetSubscriptions;
    /// <summary>
    /// ���������� ��������, ������� ���������� �������.
    /// ���� �������� ������������ ������ ���� ������� Extended = True
    /// </summary>
    function Count(const Value: Integer): TVkParamsUsersGetSubscriptions;
    /// <summary>
    /// ������ �������������� ����� ��� �������� User � Group, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsUsersGetSubscriptions;
  end;

  TVkParamsUsersSearch = record
    List: TParams;
    /// <summary>
    /// ������ ���������� �������
    /// </summary>
    function Query(const Value: string): TVkParamsUsersSearch;
    /// <summary>
    /// ���������� �����������
    /// </summary>
    function Sort(const Value: TVkSortUser): TVkParamsUsersSearch;
    /// <summary>
    /// �������� ������������ ������� ���������� ������������ ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ���������� ������������ �������������
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsUsersSearch;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function City(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function Country(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// �������� ������ �������
    /// </summary>
    function Hometown(const Value: string): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� ������, � ������� ������������ ��������� ���
    /// </summary>
    function UniversityCountry(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� ����
    /// </summary>
    function University(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ��� ��������� ����
    /// </summary>
    function UniversityYear(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function UniversityFaculty(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function UniversityChair(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ���
    /// </summary>
    function Sex(const Value: TVkSex): TVkParamsUsersSearch;
    /// <summary>
    /// �������� ���������
    /// </summary>
    function Status(const Value: TVkRelation): TVkParamsUsersSearch;
    /// <summary>
    /// �������, ��
    /// </summary>
    function AgeFrom(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// �������, ��
    /// </summary>
    function AgeTo(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ���� ��������
    /// </summary>
    function BirthDay(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ����� ��������
    /// </summary>
    function BirthMonth(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ��� ��������
    /// </summary>
    function BirthYear(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ��������� �� ������ �������
    /// </summary>
    function Online(const Value: Boolean): TVkParamsUsersSearch;
    /// <summary>
    /// ��������� �� ������� ����
    /// </summary>
    function HasPhoto(const Value: Boolean): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� ������, � ������� ������������ ��������� �����
    /// </summary>
    function SchoolCountry(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� ������, � ������� ������������ ��������� �����
    /// </summary>
    function SchoolCity(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ����� ������
    /// </summary>
    function SchoolClass(const Value: TIdList): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� �����, ������� ��������� ������������
    /// </summary>
    function School(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ��� ��������� �����
    /// </summary>
    function SchoolYear(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ����������� �������
    /// </summary>
    function Religion(const Value: string): TVkParamsUsersSearch;
    /// <summary>
    /// �������� ��������, � ������� �������� ������������
    /// </summary>
    function Company(const Value: string): TVkParamsUsersSearch;
    /// <summary>
    /// �������� ���������
    /// </summary>
    function Position(const Value: string): TVkParamsUsersSearch;
    /// <summary>
    /// ������������� ������, ����� ������������� ������� ���������� ��������� �����
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsUsersSearch;
    /// <summary>
    /// ������� ����� ������� ����� ����������� �����
    /// </summary>
    function FromList(const Value: TVkSearchTargets): TVkParamsUsersSearch;
  end;

  TUsersController = class(TVkController)
  public    /// <summary>
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

function TVkParamsUsersGet.Fields(const Value: string): TVkParamsUsersGet;
begin
  List.Add('fields', Value);
  Result := Self;
end;

function TVkParamsUsersGet.Fields(const Value: TVkProfileFields): TVkParamsUsersGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsUsersGet.NameCase(const Value: TVkNameCase): TVkParamsUsersGet;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsUsersGet.UserIds(const Value: TIdList): TVkParamsUsersGet;
begin
  List.Add('user_ids', Value);
  Result := Self;
end;

{ TVkParamsUsersGetFollowers }

function TVkParamsUsersGetFollowers.Count(const Value: Integer): TVkParamsUsersGetFollowers;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsUsersGetFollowers.Fields(const Value: string): TVkParamsUsersGetFollowers;
begin
  List.Add('fields', Value);
  Result := Self;
end;

function TVkParamsUsersGetFollowers.Fields(const Value: TVkProfileFields): TVkParamsUsersGetFollowers;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsUsersGetFollowers.NameCase(const Value: TVkNameCase): TVkParamsUsersGetFollowers;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsUsersGetFollowers.Offset(const Value: Integer): TVkParamsUsersGetFollowers;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsUsersGetFollowers.UserId(const Value: Integer): TVkParamsUsersGetFollowers;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsUsersGetSubscriptions }

function TVkParamsUsersGetSubscriptions.UserId(const Value: Integer): TVkParamsUsersGetSubscriptions;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

function TVkParamsUsersGetSubscriptions.Extended(const Value: Boolean): TVkParamsUsersGetSubscriptions;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsUsersGetSubscriptions.Offset(const Value: Integer): TVkParamsUsersGetSubscriptions;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsUsersGetSubscriptions.Count(const Value: Integer): TVkParamsUsersGetSubscriptions;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsUsersGetSubscriptions.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): TVkParamsUsersGetSubscriptions;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

{ TVkParamsUsersSearch }

function TVkParamsUsersSearch.Query(const Value: string): TVkParamsUsersSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Sort(const Value: TVkSortUser): TVkParamsUsersSearch;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

function TVkParamsUsersSearch.Offset(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Count(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Fields(const Value: TVkProfileFields): TVkParamsUsersSearch;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsUsersSearch.City(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('city', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Country(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('country', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Hometown(const Value: string): TVkParamsUsersSearch;
begin
  List.Add('hometown', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.UniversityCountry(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('university_country', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.University(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('university', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.UniversityYear(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('university_year', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.UniversityFaculty(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('university_faculty', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.UniversityChair(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('university_chair', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Sex(const Value: TVkSex): TVkParamsUsersSearch;
begin
  List.Add('sex', Ord(Value));
  Result := Self;
end;

function TVkParamsUsersSearch.Status(const Value: TVkRelation): TVkParamsUsersSearch;
begin
  List.Add('status', Ord(Value));
  Result := Self;
end;

function TVkParamsUsersSearch.AgeFrom(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('age_from', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.AgeTo(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('age_to', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.BirthDay(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('birth_day', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.BirthMonth(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('birth_month', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.BirthYear(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('birth_year', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Online(const Value: Boolean): TVkParamsUsersSearch;
begin
  List.Add('online', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.HasPhoto(const Value: Boolean): TVkParamsUsersSearch;
begin
  List.Add('has_photo', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.SchoolCountry(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('school_country', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.SchoolCity(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('school_city', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.SchoolClass(const Value: TIdList): TVkParamsUsersSearch;
begin
  List.Add('school_class', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.School(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('school', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.SchoolYear(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('school_year', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Religion(const Value: string): TVkParamsUsersSearch;
begin
  List.Add('religion', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Company(const Value: string): TVkParamsUsersSearch;
begin
  List.Add('company', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.Position(const Value: string): TVkParamsUsersSearch;
begin
  List.Add('position', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.GroupId(const Value: Integer): TVkParamsUsersSearch;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsUsersSearch.FromList(const Value: TVkSearchTargets): TVkParamsUsersSearch;
begin
  List.Add('from_list', Value.ToString);
  Result := Self;
end;

end.

