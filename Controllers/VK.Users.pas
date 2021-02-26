unit VK.Users;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Profile, VK.Entity.Subscription;

type
  TVkParamsUsersGet = record
    List: TParams;
    /// <summary>
    /// Перечисленные через запятую идентификаторы пользователей или их короткие имена (ScreenName).
    /// По умолчанию — идентификатор текущего пользователя
    /// </summary>
    function UserIds(const Value: TIdList): Integer;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: string): Integer; overload;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer; overload;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer; overload;
  end;

  TVkParamsUsersGetFollowers = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// Количество подписчиков, информацию о которых нужно получить
    /// </summary>
    function Count(const Value: Integer = 100): Integer; overload;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества подписчиков
    /// </summary>
    function Offset(const Value: Integer): Integer; overload;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: string): Integer; overload;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer; overload;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer; overload;
  end;

  TVkParamsUsersGetSubscriptions = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, подписки которого необходимо получить
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// True – возвращает объединенный список, содержащий объекты group и user вместе.
    /// False – возвращает список идентификаторов групп и пользователей отдельно. (по умолчанию)
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// Смещение необходимое для выборки определенного подмножества подписок.
    /// Этот параметр используется только если передан Extended = True
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// Количество подписок, которые необходимо вернуть.
    /// Этот параметр используется только если передан Extended = True
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// Список дополнительных полей для объектов User и Group, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsUsersSearch = record
    List: TParams;
    /// <summary>
    /// Строка поискового запроса
    /// </summary>
    function Query(const Value: string): Integer;
    /// <summary>
    /// Сортировка результатов
    /// </summary>
    function Sort(const Value: TVkSortUser): Integer;
    /// <summary>
    /// Смещение относительно первого найденного пользователя для выборки определенного подмножества
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// Количество возвращаемых пользователей
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer;
    /// <summary>
    /// Идентификатор города
    /// </summary>
    function City(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор страны
    /// </summary>
    function Country(const Value: Integer): Integer;
    /// <summary>
    /// Название города строкой
    /// </summary>
    function Hometown(const Value: string): Integer;
    /// <summary>
    /// Идентификатор страны, в которой пользователи закончили ВУЗ
    /// </summary>
    function UniversityCountry(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор ВУЗа
    /// </summary>
    function University(const Value: Integer): Integer;
    /// <summary>
    /// Год окончания ВУЗа
    /// </summary>
    function UniversityYear(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор факультета
    /// </summary>
    function UniversityFaculty(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор кафедры
    /// </summary>
    function UniversityChair(const Value: Integer): Integer;
    /// <summary>
    /// Пол
    /// </summary>
    function Sex(const Value: TVkSex): Integer;
    /// <summary>
    /// Семейное положение
    /// </summary>
    function Status(const Value: TVkRelation): Integer;
    /// <summary>
    /// Возраст, от
    /// </summary>
    function AgeFrom(const Value: Integer): Integer;
    /// <summary>
    /// Возраст, до
    /// </summary>
    function AgeTo(const Value: Integer): Integer;
    /// <summary>
    /// День рождения
    /// </summary>
    function BirthDay(const Value: Integer): Integer;
    /// <summary>
    /// Месяц рождения
    /// </summary>
    function BirthMonth(const Value: Integer): Integer;
    /// <summary>
    /// Год рождения
    /// </summary>
    function BirthYear(const Value: Integer): Integer;
    /// <summary>
    /// Учитывать ли статус «онлайн»
    /// </summary>
    function Online(const Value: Boolean): Integer;
    /// <summary>
    /// Учитывать ли наличие фото
    /// </summary>
    function HasPhoto(const Value: Boolean): Integer;
    /// <summary>
    /// Идентификатор страны, в которой пользователи закончили школу
    /// </summary>
    function SchoolCountry(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор города, в котором пользователи закончили школу
    /// </summary>
    function SchoolCity(const Value: Integer): Integer;
    /// <summary>
    /// Буква класса
    /// </summary>
    function SchoolClass(const Value: TIdList): Integer;
    /// <summary>
    /// Идентификатор школы, которую закончили пользователи
    /// </summary>
    function School(const Value: Integer): Integer;
    /// <summary>
    /// Год окончания школы
    /// </summary>
    function SchoolYear(const Value: Integer): Integer;
    /// <summary>
    /// Религиозные взгляды
    /// </summary>
    function Religion(const Value: string): Integer;
    /// <summary>
    /// Название компании, в которой работают пользователи
    /// </summary>
    function Company(const Value: string): Integer;
    /// <summary>
    /// Название должности
    /// </summary>
    function Position(const Value: string): Integer;
    /// <summary>
    /// Идентификатор группы, среди пользователей которой необходимо проводить поиск
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// Разделы среди которых нужно осуществить поиск
    /// </summary>
    function FromList(const Value: TVkSearchTargets): Integer;
  end;

  TUsersController = class(TVkController)
  public
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// Поля Counters, Military будут возвращены только в случае, если передан ровно один UserId
    /// </summary>
    function Get(var Items: TVkProfiles; UserIds: TIdList; Fields: TVkProfileFields = []; NameCase: TVkNameCase = TVkNameCase.Nom): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// Поля Counters, Military будут возвращены только в случае, если передан ровно один UserId
    /// </summary>
    function Get(var User: TVkProfile; UserId: Integer; Fields: TVkProfileFields = []; NameCase: TVkNameCase = TVkNameCase.Nom): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// Поля Counters, Military будут возвращены только в случае, если передан ровно один UserId
    /// </summary>
    function Get(var User: TVkProfile; Fields: TVkProfileFields = []; NameCase: TVkNameCase = TVkNameCase.Nom): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// Поля Counters, Military будут возвращены только в случае, если передан ровно один UserId
    /// </summary>
    function Get(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о пользователях.
    /// Поля Counters, Military будут возвращены только в случае, если передан ровно один UserId
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
    function Report(UserId: Integer; Reason: TVkUserReport = TVkUserReport.Spam; Comment: string = ''): Boolean;
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

