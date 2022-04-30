unit VK.Search;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Search;

type
  TVkParamsSearch = record
    List: TParams;
    /// <summary>
    /// ����� �������, ���������� �������� ����� ��������
    /// </summary>
    function Query(const Value: string): TVkParamsSearch;
    /// <summary>
    /// �������� ��� ������� ������������ ������������ �����������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsSearch;
    /// <summary>
    /// ����������� �� ���������� ������������ �����������
    /// </summary>
    function Limit(const Value: Integer = 9): TVkParamsSearch;
    /// <summary>
    /// ������������� ����� ������� ���� ������, ������� ���������� ������� (�� ��������� ������������ ���)
    /// </summary>
    function Filters(const Value: TVkSearchFilters = []): TVkParamsSearch;
    /// <summary>
    /// �������������� ���� �������� � ��������� ��� ���������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsSearch;
    /// <summary>
    /// True � � ����������� ������ ����������� ���������� ����������� ������ �� ���� ������������� � �������
    /// </summary>
    function SearchGlobal(const Value: Boolean = True): TVkParamsSearch;
  end;

  /// <summary>
  /// ������ ��� ������ � �������.
  /// </summary>
  TSearchController = class(TVkController)
  public
    /// <summary>
    /// ����� ��������� �������� ���������� �������� ������ �� ������������ ���������
    /// </summary>
    function GetHints(var Items: TVkSearchItems; Params: TParams): Boolean; overload;
    /// <summary>
    /// ����� ��������� �������� ���������� �������� ������ �� ������������ ���������
    /// </summary>
    function GetHints(var Items: TVkSearchItems; Params: TVkParamsSearch): Boolean; overload;
    /// <summary>
    /// ����� ��������� �������� ���������� �������� ������ �� ������������ ���������
    /// </summary>
    function GetHints(var Items: TVkSearchItems; const Query: string): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TSearchController }

function TSearchController.GetHints(var Items: TVkSearchItems; Params: TParams): Boolean;
begin
  Result := Handler.Execute('search.getHints', Params).GetObject(Items);
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

function TVkParamsSearch.Query(const Value: string): TVkParamsSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsSearch.Offset(const Value: Integer): TVkParamsSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsSearch.Limit(const Value: Integer): TVkParamsSearch;
begin
  List.Add('limit', Value);
  Result := Self;
end;

function TVkParamsSearch.Filters(const Value: TVkSearchFilters): TVkParamsSearch;
begin
  List.Add('filters', Value.ToString);
  Result := Self;
end;

function TVkParamsSearch.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): TVkParamsSearch;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsSearch.SearchGlobal(const Value: Boolean): TVkParamsSearch;
begin
  List.Add('search_global', Value);
  Result := Self;
end;

end.

