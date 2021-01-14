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
    function CountryId(Value: Integer): Integer;
    function RegionId(Value: Integer): Integer;
    function Query(Value: string): Integer;
    function NeedAll(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkParamsGetCountries = record
    List: TParams;
    function NeedAll(Value: Boolean): Integer;
    function Code(Value: TArrayOfString): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkParamsGetUniversities = record
    List: TParams;
    function Query(Value: string): Integer;
    function CountryId(Value: Integer): Integer;
    function CityId(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
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
    function GetCitiesById(var Items: TVkCities; const CityIds: TIds): Boolean;
    /// <summary>
    /// Возвращает список стран.
    /// </summary>
    function GetCountries(var Items: TVkCountries; const Params: TVkParamsGetCountries): Boolean;
    /// <summary>
    /// Возвращает информацию о странах по их идентификаторам.
    /// </summary>
    function GetCountriesById(var Items: TVkCountries; const CountryIds: TIds): Boolean;
    /// <summary>
    /// Возвращает список факультетов.
    /// </summary>
    function GetFaculties(var Items: TVkFaculties; const UniversityId: Integer; Offset: Integer = 0; Count: Integer =
      100): Boolean;
    /// <summary>
    /// Возвращает список станций метро.
    /// </summary>
    function GetMetroStations(var Items: TVkMetroStations; const CityId: Integer; Extended: Boolean = False; Offset:
      Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает информацию об одной или нескольких станциях метро по их идентификаторам.
    /// </summary>
    function GetMetroStationsById(var Items: TVkMetroStations; const StationIds: TIds): Boolean;
    /// <summary>
    /// Возвращает список регионов.
    /// </summary>
    function GetRegions(var Items: TVkRegions; const CountryId: Integer; Query: string = ''; Offset: Integer = 0; Count:
      Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список классов, характерных для школ определенной страны.
    /// </summary>
    function GetSchoolClasses(var Items: TVkSchoolClasses; const CountryId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список школ.
    /// </summary>
    function GetSchools(var Items: TVkSchools; const CityId: Integer; Query: string = ''; Offset: Integer = 0; Count:
      Integer = 100): Boolean;
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

function TDatabaseController.GetCitiesById(var Items: TVkCities; const CityIds: TIds): Boolean;
begin
  Result := Handler.Execute('database.getCitiesById', ['city_ids', CityIds.ToString]).GetObject<TVkCities>(Items);
end;

function TDatabaseController.GetCountries(var Items: TVkCountries; const Params: TVkParamsGetCountries): Boolean;
begin
  Result := Handler.Execute('database.getCountries', Params.List).GetObject<TVkCountries>(Items);
end;

function TDatabaseController.GetCountriesById(var Items: TVkCountries; const CountryIds: TIds): Boolean;
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

function TDatabaseController.GetMetroStations(var Items: TVkMetroStations; const CityId: Integer; Extended: Boolean;
  Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getMetroStations', [
    ['city_id', CityId.ToString],
    ['extended', BoolToString(Extended)],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject<TVkMetroStations>(Items);
end;

function TDatabaseController.GetMetroStationsById(var Items: TVkMetroStations; const StationIds: TIds): Boolean;
begin
  Result := Handler.Execute('database.getMetroStationsById', ['station_ids', StationIds.ToString]).
    GetObject<TVkMetroStations>(Items);
end;

function TDatabaseController.GetRegions(var Items: TVkRegions; const CountryId: Integer; Query: string; Offset, Count:
  Integer): Boolean;
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
  Result := Handler.Execute('database.getSchoolClasses', ['country_id', CountryId.ToString]).GetObject<TVkSchoolClasses>(Items);
end;

function TDatabaseController.GetSchools(var Items: TVkSchools; const CityId: Integer; Query: string; Offset, Count:
  Integer): Boolean;
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

function TVkParamsGetCities.CountryId(Value: Integer): Integer;
begin
  Result := List.Add('country_id', Value);
end;

function TVkParamsGetCities.RegionId(Value: Integer): Integer;
begin
  Result := List.Add('region_id', Value);
end;

function TVkParamsGetCities.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsGetCities.NeedAll(Value: Boolean): Integer;
begin
  Result := List.Add('need_all', Value);
end;

function TVkParamsGetCities.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGetCities.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

{ TVkParamsGetCountries }

function TVkParamsGetCountries.NeedAll(Value: Boolean): Integer;
begin
  Result := List.Add('need_all', Value);
end;

function TVkParamsGetCountries.Code(Value: TArrayOfString): Integer;
begin
  Result := List.Add('code', Value);
end;

function TVkParamsGetCountries.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGetCountries.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

{ TVkParamsGetUniversities }

function TVkParamsGetUniversities.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsGetUniversities.CountryId(Value: Integer): Integer;
begin
  Result := List.Add('country_id', Value);
end;

function TVkParamsGetUniversities.CityId(Value: Integer): Integer;
begin
  Result := List.Add('city_id', Value);
end;

function TVkParamsGetUniversities.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGetUniversities.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

end.

