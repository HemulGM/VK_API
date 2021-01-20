unit VK.Users;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Profile, VK.Entity.Subscription;

type
  TVkParamsUsersGet = record
    List: TParams;
    function UserIds(Value: TIdList): Integer;
    function Fields(Value: string): Integer; overload;
    function Fields(Value: TVkProfileFields): Integer; overload;
    function NameCase(Value: TVkNameCase): Integer; overload;
  end;

  TVkParamsUsersGetFollowers = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function Fields(Value: string): Integer; overload;
    function Count(Value: Integer): Integer; overload;
    function Offset(Value: Integer): Integer; overload;
    function Fields(Value: TVkProfileFields): Integer; overload;
    function NameCase(Value: TVkNameCase): Integer; overload;
  end;

  TVkParamsUsersGetSubscriptions = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsUsersSearch = record
    List: TParams;
    function Query(Value: string): Integer;
    function Sort(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Fields(Value: TVkProfileFields): Integer;
    function City(Value: Integer): Integer;
    function Country(Value: Integer): Integer;
    function Hometown(Value: string): Integer;
    function UniversityCountry(Value: Integer): Integer;
    function University(Value: Integer): Integer;
    function UniversityYear(Value: Integer): Integer;
    function UniversityFaculty(Value: Integer): Integer;
    function UniversityChair(Value: Integer): Integer;
    function Sex(Value: TVkSexSearch): Integer;
    function Status(Value: Integer): Integer;
    function AgeFrom(Value: Integer): Integer;
    function AgeTo(Value: Integer): Integer;
    function BirthDay(Value: Integer): Integer;
    function BirthMonth(Value: Integer): Integer;
    function BirthYear(Value: Integer): Integer;
    function Online(Value: Boolean): Integer;
    function HasPhoto(Value: Boolean): Integer;
    function SchoolCountry(Value: Integer): Integer;
    function SchoolCity(Value: Integer): Integer;
    function SchoolClass(Value: TIdList): Integer;
    function School(Value: Integer): Integer;
    function SchoolYear(Value: Integer): Integer;
    function Religion(Value: string): Integer;
    function Company(Value: string): Integer;
    function Position(Value: string): Integer;
    function GroupId(Value: Integer): Integer;
    function FromList(Value: TArrayOfString): Integer;
  end;

  TUsersController = class(TVkController)
  public
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Items: TVkProfiles; UserIds: TIdList; Fields: TVkProfileFields = []; NameCase: TVkNameCase = ncNom): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var User: TVkProfile; UserId: Integer; Fields: TVkProfileFields = []; NameCase: TVkNameCase = ncNom): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var User: TVkProfile; Fields: TVkProfileFields = []; NameCase: TVkNameCase = ncNom): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TVkParamsUsersGet): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов пользователей, которые являются подписчиками пользователя.
    /// </summary>
    function GetFollowers(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов пользователей, которые являются подписчиками пользователя.
    /// </summary>
    function GetFollowers(var Items: TVkProfiles; Params: TVkParamsUsersGetFollowers): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов пользователей и публичных страниц, которые входят в список подписок пользователя.
    /// </summary>
    function GetSubscriptions(var Items: TVkSubscriptions; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список идентификаторов пользователей и публичных страниц, которые входят в список подписок пользователя.
    /// </summary>
    function GetSubscriptions(var Items: TVkSubscriptions; Params: TVkParamsUsersGetSubscriptions): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на пользователя.
    /// </summary>
    function Report(UserId: Integer; Reason: TVkUserReport = urSpam; Comment: string = ''): Boolean;
    /// <summary>
    /// Возвращает список пользователей в соответствии с заданным критерием поиска.
    /// </summary>
    function Search(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список пользователей в соответствии с заданным критерием поиска.
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

function TVkParamsUsersGet.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsUsersGet.Fields(Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsUsersGet.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsUsersGet.UserIds(Value: TIdList): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsUsersGetFollowers }

function TVkParamsUsersGetFollowers.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsUsersGetFollowers.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsUsersGetFollowers.Fields(Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsUsersGetFollowers.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsUsersGetFollowers.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsUsersGetFollowers.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsUsersGetSubscriptions }

function TVkParamsUsersGetSubscriptions.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

function TVkParamsUsersGetSubscriptions.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsUsersGetSubscriptions.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsUsersGetSubscriptions.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsUsersGetSubscriptions.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

{ TVkParamsUsersSearch }

function TVkParamsUsersSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsUsersSearch.Sort(Value: Integer): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsUsersSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsUsersSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsUsersSearch.Fields(Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsUsersSearch.City(Value: Integer): Integer;
begin
  Result := List.Add('city', Value);
end;

function TVkParamsUsersSearch.Country(Value: Integer): Integer;
begin
  Result := List.Add('country', Value);
end;

function TVkParamsUsersSearch.Hometown(Value: string): Integer;
begin
  Result := List.Add('hometown', Value);
end;

function TVkParamsUsersSearch.UniversityCountry(Value: Integer): Integer;
begin
  Result := List.Add('university_country', Value);
end;

function TVkParamsUsersSearch.University(Value: Integer): Integer;
begin
  Result := List.Add('university', Value);
end;

function TVkParamsUsersSearch.UniversityYear(Value: Integer): Integer;
begin
  Result := List.Add('university_year', Value);
end;

function TVkParamsUsersSearch.UniversityFaculty(Value: Integer): Integer;
begin
  Result := List.Add('university_faculty', Value);
end;

function TVkParamsUsersSearch.UniversityChair(Value: Integer): Integer;
begin
  Result := List.Add('university_chair', Value);
end;

function TVkParamsUsersSearch.Sex(Value: TVkSexSearch): Integer;
begin
  Result := List.Add('sex', Ord(Value));
end;

function TVkParamsUsersSearch.Status(Value: Integer): Integer;
begin
  Result := List.Add('status', Value);
end;

function TVkParamsUsersSearch.AgeFrom(Value: Integer): Integer;
begin
  Result := List.Add('age_from', Value);
end;

function TVkParamsUsersSearch.AgeTo(Value: Integer): Integer;
begin
  Result := List.Add('age_to', Value);
end;

function TVkParamsUsersSearch.BirthDay(Value: Integer): Integer;
begin
  Result := List.Add('birth_day', Value);
end;

function TVkParamsUsersSearch.BirthMonth(Value: Integer): Integer;
begin
  Result := List.Add('birth_month', Value);
end;

function TVkParamsUsersSearch.BirthYear(Value: Integer): Integer;
begin
  Result := List.Add('birth_year', Value);
end;

function TVkParamsUsersSearch.Online(Value: Boolean): Integer;
begin
  Result := List.Add('online', Value);
end;

function TVkParamsUsersSearch.HasPhoto(Value: Boolean): Integer;
begin
  Result := List.Add('has_photo', Value);
end;

function TVkParamsUsersSearch.SchoolCountry(Value: Integer): Integer;
begin
  Result := List.Add('school_country', Value);
end;

function TVkParamsUsersSearch.SchoolCity(Value: Integer): Integer;
begin
  Result := List.Add('school_city', Value);
end;

function TVkParamsUsersSearch.SchoolClass(Value: TIdList): Integer;
begin
  Result := List.Add('school_class', Value);
end;

function TVkParamsUsersSearch.School(Value: Integer): Integer;
begin
  Result := List.Add('school', Value);
end;

function TVkParamsUsersSearch.SchoolYear(Value: Integer): Integer;
begin
  Result := List.Add('school_year', Value);
end;

function TVkParamsUsersSearch.Religion(Value: string): Integer;
begin
  Result := List.Add('religion', Value);
end;

function TVkParamsUsersSearch.Company(Value: string): Integer;
begin
  Result := List.Add('company', Value);
end;

function TVkParamsUsersSearch.Position(Value: string): Integer;
begin
  Result := List.Add('position', Value);
end;

function TVkParamsUsersSearch.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsUsersSearch.FromList(Value: TArrayOfString): Integer;
begin
  Result := List.Add('from_list', Value);
end;

end.

