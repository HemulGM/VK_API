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
    function Query(const Value: string): Integer;
    /// <summary>
    /// �������� ��� ������� ������������ ������������ �����������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ����������� �� ���������� ������������ �����������
    /// </summary>
    function Limit(const Value: Integer = 9): Integer;
    /// <summary>
    /// ������������� ����� ������� ���� ������, ������� ���������� ������� (�� ��������� ������������ ���)
    /// </summary>
    function Filters(const Value: TVkSearchFilters = []): Integer;
    /// <summary>
    /// �������������� ���� �������� � ��������� ��� ���������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
    /// <summary>
    /// True � � ����������� ������ ����������� ���������� ����������� ������ �� ���� ������������� � �������
    /// </summary>
    function SearchGlobal(const Value: Boolean = True): Integer;
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

function TVkParamsSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsSearch.Limit(const Value: Integer): Integer;
begin
  Result := List.Add('limit', Value);
end;

function TVkParamsSearch.Filters(const Value: TVkSearchFilters): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsSearch.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsSearch.SearchGlobal(const Value: Boolean): Integer;
begin
  Result := List.Add('search_global', Value);
end;

end.

