unit VK.Database;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.Database.Chairs,
  VK.Entity.Database.Cities, VK.Entity.Database.Countries, VK.Entity.Database.Faculties,
  VK.Entity.Database.MetroStations, VK.Entity.Database.Regions, VK.Entity.Database.Schools,
  VK.Entity.Database.Universities;

type
  TVkParamsGetCities = record
    List: TParams;
    function CountryId(Value: Integer): TVkParamsGetCities;
    function RegionId(Value: Integer): TVkParamsGetCities;
    function Query(Value: string): TVkParamsGetCities;
    function NeedAll(Value: Boolean): TVkParamsGetCities;
    function Offset(Value: Integer): TVkParamsGetCities;
    function Count(Value: Integer): TVkParamsGetCities;
  end;

  TVkParamsGetCountries = record
    List: TParams;
    function NeedAll(Value: Boolean): TVkParamsGetCountries;
    function Code(Value: TArrayOfString): TVkParamsGetCountries;
    function Offset(Value: Integer): TVkParamsGetCountries;
    function Count(Value: Integer): TVkParamsGetCountries;
  end;

  TVkParamsGetUniversities = record
    List: TParams;
    function Query(Value: string): TVkParamsGetUniversities;
    function CountryId(Value: Integer): TVkParamsGetUniversities;
    function CityId(Value: Integer): TVkParamsGetUniversities;
    function Offset(Value: Integer): TVkParamsGetUniversities;
    function Count(Value: Integer): TVkParamsGetUniversities;
  end;

  /// <summary>
  /// Методы этой секции предоставляют доступ к базе данных учебных заведений ВКонтакте. Доступ к данным является бесплатным и не требует авторизации, однако количество запросов с одного IP адреса может быть ограничено, при необходимости делать большое количество запросов рекомендуется выполнять запросы с клиентской стороны, используя JSONP.
  /// </summary>
  TDatabaseController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список кафедр университета по указанному факультету.
    /// </summary>
    function GetChairs(var Items: TVkChairs; const FacultyId: Integer; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список городов.
    /// </summary>
    function GetCities(var Items: TVkCities; const Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список городов.
    /// </summary>
    function GetCities(var Items: TVkCities; const Params: TVkParamsGetCities): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о городах и регионах по их идентификаторам.
    /// </summary>
    function GetCitiesById(var Items: TVkCities; const CityIds: TIdList): Boolean;
    /// <summary>
    /// Возвращает список стран.
    /// </summary>
    function GetCountries(var Items: TVkCountries; const Params: TVkParamsGetCountries): Boolean;
    /// <summary>
    /// Возвращает информацию о странах по их идентификаторам.
    /// </summary>
    function GetCountriesById(var Items: TVkCountries; const CountryIds: TIdList): Boolean;
    /// <summary>
    /// Возвращает список факультетов.
    /// </summary>
    function GetFaculties(var Items: TVkFaculties; const UniversityId: Integer; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список станций метро.
    /// </summary>
    function GetMetroStations(var Items: TVkMetroStations; const CityId: Integer; Extended: Boolean = False; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает информацию об одной или нескольких станциях метро по их идентификаторам.
    /// </summary>
    function GetMetroStationsById(var Items: TVkMetroStations; const StationIds: TIdList): Boolean;
    /// <summary>
    /// Возвращает список регионов.
    /// </summary>
    function GetRegions(var Items: TVkRegions; const CountryId: Integer; Query: string = ''; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список классов, характерных для школ определенной страны.
    /// </summary>
    function GetSchoolClasses(var Items: TVkSchoolClasses; const CountryId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список школ.
    /// </summary>
    function GetSchools(var Items: TVkSchools; const CityId: Integer; Query: string = ''; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список высших учебных заведений.
    /// </summary>
    function GetUniversities(var Items: TVkUniversities; const Params: TVkParamsGetUniversities): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TDatabaseController }

function TDatabaseController.GetChairs(var Items: TVkChairs; const FacultyId: Integer; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getChairs', [
    ['faculty_id', FacultyId.ToString],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject<TVkChairs>(Items);
end;

function TDatabaseController.GetCities(var Items: TVkCities; const Params: TParams): Boolean;
begin
  Result := Handler.Execute('database.getCities', Params).GetObject<TVkCities>(Items);
end;

function TDatabaseController.GetCities(var Items: TVkCities; const Params: TVkParamsGetCities): Boolean;
begin
  Result := GetCities(Items, Params.List);
end;

function TDatabaseController.GetCitiesById(var Items: TVkCities; const CityIds: TIdList): Boolean;
begin
  Result := Handler.Execute('database.getCitiesById', ['city_ids', CityIds.ToString]).GetObject<TVkCities>(Items);
end;

function TDatabaseController.GetCountries(var Items: TVkCountries; const Params: TVkParamsGetCountries): Boolean;
begin
  Result := Handler.Execute('database.getCountries', Params.List).GetObject<TVkCountries>(Items);
end;

function TDatabaseController.GetCountriesById(var Items: TVkCountries; const CountryIds: TIdList): Boolean;
begin
  Result := Handler.Execute('database.getCountriesById', ['country_ids', CountryIds.ToString]).GetObject<TVkCountries>(Items);
end;

function TDatabaseController.GetFaculties(var Items: TVkFaculties; const UniversityId: Integer; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getFaculties', [
    ['university_id', UniversityId.ToString],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject<TVkFaculties>(Items);
end;

function TDatabaseController.GetMetroStations(var Items: TVkMetroStations; const CityId: Integer; Extended: Boolean; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getMetroStations', [
    ['city_id', CityId.ToString],
    ['extended', BoolToString(Extended)],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject<TVkMetroStations>(Items);
end;

function TDatabaseController.GetMetroStationsById(var Items: TVkMetroStations; const StationIds: TIdList): Boolean;
begin
  Result := Handler.Execute('database.getMetroStationsById', ['station_ids', StationIds.ToString]).
    GetObject<TVkMetroStations>(Items);
end;

function TDatabaseController.GetRegions(var Items: TVkRegions; const CountryId: Integer; Query: string; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getRegions', [
    ['country_id', CountryId.ToString],
    ['q', Query],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject<TVkRegions>(Items);
end;

function TDatabaseController.GetSchoolClasses(var Items: TVkSchoolClasses; const CountryId: Integer): Boolean;
begin
  Result := Handler.Execute('database.getSchoolClasses', ['country_id', CountryId.ToString]).GetObjects<TVkSchoolClasses>(Items);
end;

function TDatabaseController.GetSchools(var Items: TVkSchools; const CityId: Integer; Query: string; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getSchools', [
    ['city_id', CityId.ToString],
    ['q', Query],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).GetObject<TVkSchools>(Items);
end;

function TDatabaseController.GetUniversities(var Items: TVkUniversities; const Params: TVkParamsGetUniversities): Boolean;
begin
  Result := Handler.Execute('database.getUniversities', Params.List).GetObject<TVkUniversities>(Items);
end;

{ TVkParamsGetCities }

function TVkParamsGetCities.CountryId(Value: Integer): TVkParamsGetCities;
begin
  List.Add('country_id', Value);
  Result := Self;
end;

function TVkParamsGetCities.RegionId(Value: Integer): TVkParamsGetCities;
begin
  List.Add('region_id', Value);
  Result := Self;
end;

function TVkParamsGetCities.Query(Value: string): TVkParamsGetCities;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsGetCities.NeedAll(Value: Boolean): TVkParamsGetCities;
begin
  List.Add('need_all', Value);
  Result := Self;
end;

function TVkParamsGetCities.Offset(Value: Integer): TVkParamsGetCities;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGetCities.Count(Value: Integer): TVkParamsGetCities;
begin
  List.Add('count', Value);
  Result := Self;
end;

{ TVkParamsGetCountries }

function TVkParamsGetCountries.NeedAll(Value: Boolean): TVkParamsGetCountries;
begin
  List.Add('need_all', Value);
  Result := Self;
end;

function TVkParamsGetCountries.Code(Value: TArrayOfString): TVkParamsGetCountries;
begin
  List.Add('code', Value);
  Result := Self;
end;

function TVkParamsGetCountries.Offset(Value: Integer): TVkParamsGetCountries;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGetCountries.Count(Value: Integer): TVkParamsGetCountries;
begin
  List.Add('count', Value);
  Result := Self;
end;

{ TVkParamsGetUniversities }

function TVkParamsGetUniversities.Query(Value: string): TVkParamsGetUniversities;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsGetUniversities.CountryId(Value: Integer): TVkParamsGetUniversities;
begin
  List.Add('country_id', Value);
  Result := Self;
end;

function TVkParamsGetUniversities.CityId(Value: Integer): TVkParamsGetUniversities;
begin
  List.Add('city_id', Value);
  Result := Self;
end;

function TVkParamsGetUniversities.Offset(Value: Integer): TVkParamsGetUniversities;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGetUniversities.Count(Value: Integer): TVkParamsGetUniversities;
begin
  List.Add('count', Value);
  Result := Self;
end;

end.

