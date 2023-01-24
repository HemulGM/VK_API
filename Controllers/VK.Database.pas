﻿unit VK.Database;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Database.Chairs, VK.Entity.Database.Cities,
  VK.Entity.Database.Countries, VK.Entity.Database.Faculties,
  VK.Entity.Database.MetroStations, VK.Entity.Database.Regions,
  VK.Entity.Database.Schools, VK.Entity.Database.Universities;

type
  TVkParamsGetCities = record
    List: TParams;
    /// <summary>
    /// Идентификатор страны, полученный GetCountries
    /// </summary>
    function CountryId(const Value: Integer): TVkParamsGetCities;
    /// <summary>
    /// Идентификатор региона, города которого необходимо получить
    /// </summary>
    function RegionId(const Value: Integer): TVkParamsGetCities;
    /// <summary>
    /// Строка поискового запроса
    /// </summary>
    function Query(const Value: string): TVkParamsGetCities;
    /// <summary>
    /// True – возвращать все города.
    /// False – возвращать только основные города
    /// </summary>
    function NeedAll(const Value: Boolean): TVkParamsGetCities;
    /// <summary>
    /// Отступ, необходимый для получения определенного подмножества городов
    /// </summary>
    function Offset(const Value: Integer): TVkParamsGetCities;
    /// <summary>
    /// Количество городов, которые необходимо вернуть (максимальное значение 1000)
    /// </summary>
    function Count(const Value: Integer = 100): TVkParamsGetCities;
  end;

  TVkParamsGetCountries = record
    List: TParams;
    /// <summary>
    /// True — вернуть список всех стран
    /// </summary>
    function NeedAll(const Value: Boolean): TVkParamsGetCountries;
    /// <summary>
    /// Перечисленные через запятую двухбуквенные коды стран в стандарте ISO 3166-1 alpha-2,
    /// для которых необходимо выдать информацию.
    /// Пример значения code:
    /// RU,UA,BY
    /// </summary>
    function Code(const Value: TArrayOfString): TVkParamsGetCountries;
    /// <summary>
    /// Отступ, необходимый для выбора определенного подмножества стран
    /// </summary>
    function Offset(const Value: Integer): TVkParamsGetCountries;
    /// <summary>
    /// Количество стран, которое необходимо вернуть (максимальное значение 1000)
    /// </summary>
    function Count(const Value: Integer = 100): TVkParamsGetCountries;
  end;

  TVkParamsGetUniversities = record
    List: TParams;
    /// <summary>
    /// Строка поискового запроса
    /// </summary>
    function Query(const Value: string): TVkParamsGetUniversities;
    /// <summary>
    /// Идентификатор страны, учебные заведения которой необходимо вернуть
    /// </summary>
    function CountryId(const Value: Integer): TVkParamsGetUniversities;
    /// <summary>
    /// Идентификатор города, учебные заведения которого необходимо вернуть
    /// </summary>
    function CityId(const Value: Integer): TVkParamsGetUniversities;
    /// <summary>
    /// Отступ, необходимый для получения определенного подмножества учебных заведений
    /// </summary>
    function Offset(const Value: Integer): TVkParamsGetUniversities;
    /// <summary>
    /// Количество учебных заведений, которое необходимо вернуть (максимальное значение 10000)
    /// </summary>
    function Count(const Value: Integer = 100): TVkParamsGetUniversities;
  end;

  /// <summary>
  /// Методы этой секции предоставляют доступ к базе данных учебных заведений ВКонтакте. Доступ к данным является бесплатным и не требует авторизации, однако количество запросов с одного IP адреса может быть ограничено, при необходимости делать большое количество запросов рекомендуется выполнять запросы с клиентской стороны, используя JSONP.
  /// </summary>
  TDatabaseController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список кафедр университета по указанному факультету
    /// </summary>
    function GetChairs(var Items: TVkChairs; const FacultyId: Integer; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список городов
    /// </summary>
    function GetCities(var Items: TVkCities; const Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список городов
    /// </summary>
    function GetCities(var Items: TVkCities; const Params: TVkParamsGetCities): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о городах и регионах по их идентификаторам
    /// </summary>
    function GetCitiesById(var Items: TVkCities; const CityIds: TIdList): Boolean;
    /// <summary>
    /// Возвращает список стран
    /// </summary>
    function GetCountries(var Items: TVkCountries; const Params: TVkParamsGetCountries): Boolean;
    /// <summary>
    /// Возвращает информацию о странах по их идентификаторам
    /// </summary>
    function GetCountriesById(var Items: TVkCountries; const CountryIds: TIdList): Boolean;
    /// <summary>
    /// Возвращает список факультетов
    /// </summary>
    function GetFaculties(var Items: TVkFaculties; const UniversityId: Integer; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список станций метро
    /// </summary>
    function GetMetroStations(var Items: TVkMetroStations; const CityId: Integer; Extended: Boolean = False; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает информацию об одной или нескольких станциях метро по их идентификаторам
    /// </summary>
    function GetMetroStationsById(var Items: TVkMetroStations; const StationIds: TIdList): Boolean;
    /// <summary>
    /// Возвращает список регионов
    /// <code> var Regions: TVkRegions;
    /// if GetRegions(Regions, 0) then
    /// try
    ///   for var Region in Regions do
    ///     //do something
    /// finally
    ///   Regions.Free;
    /// end;
    /// </code>
    /// </summary>
    function GetRegions(var Items: TVkRegions; const CountryId: Integer; Query: string = ''; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список классов, характерных для школ определенной страны
    /// </summary>
    function GetSchoolClasses(var Items: TVkSchoolClasses; const CountryId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список школ
    /// </summary>
    function GetSchools(var Items: TVkSchools; const CityId: Integer; Query: string = ''; Offset: Integer = 0; Count: Integer = 100): Boolean;
    /// <summary>
    /// Возвращает список высших учебных заведений
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
    GetObject(Items);
end;

function TDatabaseController.GetCities(var Items: TVkCities; const Params: TParams): Boolean;
begin
  Result := Handler.Execute('database.getCities', Params).GetObject(Items);
end;

function TDatabaseController.GetCities(var Items: TVkCities; const Params: TVkParamsGetCities): Boolean;
begin
  Result := GetCities(Items, Params.List);
end;

function TDatabaseController.GetCitiesById(var Items: TVkCities; const CityIds: TIdList): Boolean;
begin
  Result := Handler.Execute('database.getCitiesById', ['city_ids', CityIds.ToString]).GetObject(Items);
end;

function TDatabaseController.GetCountries(var Items: TVkCountries; const Params: TVkParamsGetCountries): Boolean;
begin
  Result := Handler.Execute('database.getCountries', Params.List).GetObject(Items);
end;

function TDatabaseController.GetCountriesById(var Items: TVkCountries; const CountryIds: TIdList): Boolean;
begin
  Result := Handler.Execute('database.getCountriesById', ['country_ids', CountryIds.ToString]).GetObject(Items);
end;

function TDatabaseController.GetFaculties(var Items: TVkFaculties; const UniversityId: Integer; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getFaculties', [
    ['university_id', UniversityId.ToString],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject(Items);
end;

function TDatabaseController.GetMetroStations(var Items: TVkMetroStations; const CityId: Integer; Extended: Boolean; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getMetroStations', [
    ['city_id', CityId.ToString],
    ['extended', BoolToString(Extended)],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject(Items);
end;

function TDatabaseController.GetMetroStationsById(var Items: TVkMetroStations; const StationIds: TIdList): Boolean;
begin
  Result := Handler.Execute('database.getMetroStationsById', ['station_ids', StationIds.ToString]).GetObject(Items);
end;

function TDatabaseController.GetRegions(var Items: TVkRegions; const CountryId: Integer; Query: string; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getRegions', [
    ['country_id', CountryId.ToString],
    ['q', Query],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject(Items);
end;

function TDatabaseController.GetSchoolClasses(var Items: TVkSchoolClasses; const CountryId: Integer): Boolean;
begin
  Result := Handler.Execute('database.getSchoolClasses', ['country_id', CountryId.ToString]).GetObjects(Items);
end;

function TDatabaseController.GetSchools(var Items: TVkSchools; const CityId: Integer; Query: string; Offset, Count: Integer): Boolean;
begin
  Result := Handler.Execute('database.getSchools', [
    ['city_id', CityId.ToString],
    ['q', Query],
    ['offset', Offset.ToString],
    ['count', Count.ToString]]).
    GetObject(Items);
end;

function TDatabaseController.GetUniversities(var Items: TVkUniversities; const Params: TVkParamsGetUniversities): Boolean;
begin
  Result := Handler.Execute('database.getUniversities', Params.List).GetObject(Items);
end;

{ TVkParamsGetCities }

function TVkParamsGetCities.CountryId(const Value: Integer): TVkParamsGetCities;
begin
  List.Add('country_id', Value);
  Result := Self;
end;

function TVkParamsGetCities.RegionId(const Value: Integer): TVkParamsGetCities;
begin
  List.Add('region_id', Value);
  Result := Self;
end;

function TVkParamsGetCities.Query(const Value: string): TVkParamsGetCities;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsGetCities.NeedAll(const Value: Boolean): TVkParamsGetCities;
begin
  List.Add('need_all', Value);
  Result := Self;
end;

function TVkParamsGetCities.Offset(const Value: Integer): TVkParamsGetCities;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGetCities.Count(const Value: Integer): TVkParamsGetCities;
begin
  List.Add('count', Value);
  Result := Self;
end;

{ TVkParamsGetCountries }

function TVkParamsGetCountries.NeedAll(const Value: Boolean): TVkParamsGetCountries;
begin
  List.Add('need_all', Value);
  Result := Self;
end;

function TVkParamsGetCountries.Code(const Value: TArrayOfString): TVkParamsGetCountries;
begin
  List.Add('code', Value);
  Result := Self;
end;

function TVkParamsGetCountries.Offset(const Value: Integer): TVkParamsGetCountries;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGetCountries.Count(const Value: Integer): TVkParamsGetCountries;
begin
  List.Add('count', Value);
  Result := Self;
end;

{ TVkParamsGetUniversities }

function TVkParamsGetUniversities.Query(const Value: string): TVkParamsGetUniversities;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsGetUniversities.CountryId(const Value: Integer): TVkParamsGetUniversities;
begin
  List.Add('country_id', Value);
  Result := Self;
end;

function TVkParamsGetUniversities.CityId(const Value: Integer): TVkParamsGetUniversities;
begin
  List.Add('city_id', Value);
  Result := Self;
end;

function TVkParamsGetUniversities.Offset(const Value: Integer): TVkParamsGetUniversities;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGetUniversities.Count(const Value: Integer): TVkParamsGetUniversities;
begin
  List.Add('count', Value);
  Result := Self;
end;

end.

