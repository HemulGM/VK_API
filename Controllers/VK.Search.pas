unit VK.Search;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Search;

type
  TVkSearchFilter = (sfFriends, sfIdols, sfPublics, sfGroups, sfEvents, sfCorrespondents, sfMutualFriends);

  TVkSearchFilterHelper = record helper for TVkSearchFilter
    function ToString: string; inline;
  end;

  TVkSearchFilters = set of TVkSearchFilter;

  TVkSearchFiltersHelper = record helper for TVkSearchFilters
    function ToString: string; inline;
  end;

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
  with Handler.Execute('search.getHints', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkSearchItems.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
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

{ TVkSearchFilterHelper }

function TVkSearchFilterHelper.ToString: string;
begin
  Result := '';
  case Self of
    sfFriends:
      Result := 'friends';
    sfIdols:
      Result := 'idols';
    sfPublics:
      Result := 'publics';
    sfGroups:
      Result := 'groups';
    sfEvents:
      Result := 'events';
    sfCorrespondents:
      Result := 'correspondents';
    sfMutualFriends:
      Result := 'mutual_friends';
  end;
end;

{ TVkSearchFiltersHelper }

function TVkSearchFiltersHelper.ToString: string;
var
  Item: TVkSearchFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

end.

