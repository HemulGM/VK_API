unit VK.Search;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.Search;

type
  TVkParamsSearch = record
    List: TParams;
    function Query(Value: string): Integer;
    function Offset(Value: Integer): Integer;
    function Limit(Value: Integer): Integer;
    function Filters(Value: TVkSearchFilters): Integer;
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
    function SearchGlobal(Value: Boolean): Integer;
  end;

  /// <summary>
  /// Методы для работы с поиском.
  /// </summary>
  TSearchController = class(TVkController)
  public
    /// <summary>
    /// Метод позволяет получить результаты быстрого поиска по произвольной подстроке
    /// </summary>
    function GetHints(var Items: TVkSearchItems; Params: TParams): Boolean; overload;
    /// <summary>
    /// Метод позволяет получить результаты быстрого поиска по произвольной подстроке
    /// </summary>
    function GetHints(var Items: TVkSearchItems; Params: TVkParamsSearch): Boolean; overload;
    /// <summary>
    /// Метод позволяет получить результаты быстрого поиска по произвольной подстроке
    /// </summary>
    function GetHints(var Items: TVkSearchItems; const Query: string): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TSearchController }

function TSearchController.GetHints(var Items: TVkSearchItems; Params: TParams): Boolean;
begin
  Result := Handler.Execute('search.getHints', Params).GetObject<TVkSearchItems>(Items);
end;

function TSearchController.GetHints(var Items: TVkSearchItems; Params: TVkParamsSearch): Boolean;
begin
  Result := GetHints(Items, Params.List);
end;

function TSearchController.GetHints(var Items: TVkSearchItems; const Query: string): Boolean;
var
  Params: TVkParamsSearch;
begin
  Params.Query(Query);
  Result := GetHints(Items, Params.List);
end;

{ TVkParamsSearch }

function TVkParamsSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsSearch.Limit(Value: Integer): Integer;
begin
  Result := List.Add('limit', Value);
end;

function TVkParamsSearch.Filters(Value: TVkSearchFilters): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsSearch.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsSearch.SearchGlobal(Value: Boolean): Integer;
begin
  Result := List.Add('search_global', Value);
end;

end.

