unit VK.Pages;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Page;

type
  TVkParamsPagesGet = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ����-��������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����-��������
    /// </summary>
    function PageId(const Value: Integer): Integer;
    /// <summary>
    /// True � ��������� �������� ���������� � ���������� ����-��������
    /// </summary>
    function Global(const Value: Boolean): Integer;
    /// <summary>
    /// True � ���������� wiki �������� �������� �������������� ��� ������������� ������
    /// </summary>
    function SitePreview(const Value: Boolean): Integer;
    /// <summary>
    /// �������� ��������
    /// </summary>
    function Title(const Value: string): Integer;
    /// <summary>
    /// True � ��������� ������� ���������� �������� � ����-�������
    /// </summary>
    function NeedSource(const Value: Boolean): Integer;
    /// <summary>
    /// True � ��������� ������� html-������������� ��������
    /// </summary>
    function NeedHtml(const Value: Boolean): Integer;
  end;

  TVkParamsPagesGetVersion = record
    List: TParams;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function VersionId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������, �������� ����������� ����-��������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������������, ������� ������ ��������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// ����������, ��������� �� � ������ html-������������� ����-��������
    /// </summary>
    function NeedHtml(const Value: Boolean): Integer;
  end;

  TVkParamsPagesSave = record
    List: TParams;
    /// <summary>
    /// ����� ����� �������� � ����-�������
    /// </summary>
    function Text(const Value: string): Integer;
    /// <summary>
    /// ������������� ����-��������. ������ PageId ����� ���� ������� �������� Title
    /// </summary>
    function PageId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������, �������� ����������� ����-��������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������������, ���������� ����-��������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// �������� ����-��������
    /// </summary>
    function Title(const Value: string): Integer;
  end;

  TVkParamsPagesSaveAccess = record
    List: TParams;
    /// <summary>
    /// ������������� ����-��������
    /// </summary>
    function PageId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������, �������� ����������� ����-��������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������������, ���������� ����-��������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// �������� ��������� ������� �� ������
    /// </summary>
    function View(const Value: TVkPageAccess): Integer;
    /// <summary>
    /// �������� ��������� ������� �� ��������������
    /// </summary>
    function Edit(const Value: TVkPageAccess): Integer;
  end;

  /// <summary>
  /// ������ ��� ������ � ����-����������.
  /// </summary>
  TPagesController = class(TVkController)
  public
    /// <summary>
    /// ��������� �������� ��� ��������� ������� �������, ������� ����� ���� ����������� � ������� ���������. ����� ������� ���� ��� ����������� ������������ ������ � ������, ������ � �������� ����� ���������.
    /// ������� �������� � �������� ������� ������������� � ������� ������ � ������� � ��������� �� ������ "������������".
    /// </summary>
    function ClearCache(var Status: Boolean; const Url: string): Boolean;
    /// <summary>
    /// ���������� ���������� � ����-��������.
    /// </summary>
    function Get(var Item: TVkPage; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ����-��������.
    /// </summary>
    function Get(var Item: TVkPage; Params: TVkParamsPagesGet): Boolean; overload;
    /// <summary>
    /// ���������� ������ ���� ������ ������ ����-��������.
    /// </summary>
    function GetHistory(var Items: TVkPageVersions; const PageId: Integer; GroupId: Integer = 0; UserId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ������ ����-������� � ������.
    /// </summary>
    function GetTitles(var Items: TVkPages; const GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ����� ����� �� ������ ������ ��������.
    /// </summary>
    function GetVersion(var Item: TVkPage; Params: TVkParamsPagesGetVersion): Boolean; overload;
    /// <summary>
    /// ���������� html-������������� ����-��������.
    /// </summary>
    function ParseWiki(var Html: string; const Text: string; const GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ��������� ����� ����-��������.
    /// </summary>
    function Save(var Id: Integer; Params: TVkParamsPagesSave): Boolean;
    /// <summary>
    /// ��������� ����� ��������� ������� �� ������ � �������������� ����-��������.
    /// https://vk.com/dev/pages.saveAccess
    /// </summary>
    function SaveAccess(var Id: Integer; Params: TVkParamsPagesSaveAccess): Boolean;
  end;

implementation

uses
  VK.CommonUtils;

{ TPagesController }

function TPagesController.ClearCache(var Status: Boolean; const Url: string): Boolean;
begin
  Result := Handler.Execute('pages.clearCache', ['url', Url]).ResponseAsBool(Status);
end;

function TPagesController.Get(var Item: TVkPage; Params: TParams): Boolean;
begin
  Result := Handler.Execute('pages.get', Params).GetObject(Item);
end;

function TPagesController.Get(var Item: TVkPage; Params: TVkParamsPagesGet): Boolean;
begin
  Result := Get(Item, Params.List);
end;

function TPagesController.GetHistory(var Items: TVkPageVersions; const PageId: Integer; GroupId, UserId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('page_id', PageId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  if UserId <> 0 then
    Params.Add('user_id', UserId);
  Result := Handler.Execute('pages.getHistory', Params).GetObject(Items);
end;

function TPagesController.GetTitles(var Items: TVkPages; const GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('pages.getTitles', Params).GetObjects(Items);
end;

function TPagesController.GetVersion(var Item: TVkPage; Params: TVkParamsPagesGetVersion): Boolean;
begin
  Result := Handler.Execute('pages.getVersion', Params.List).GetObject(Item);
end;

function TPagesController.ParseWiki(var Html: string; const Text: string; const GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Params.Add('text', Text);
  Result := Handler.Execute('pages.parseWiki', Params).ResponseAsStr(Html);
end;

function TPagesController.Save(var Id: Integer; Params: TVkParamsPagesSave): Boolean;
begin
  Result := Handler.Execute('pages.save', Params.List).ResponseAsInt(Id);
end;

function TPagesController.SaveAccess(var Id: Integer; Params: TVkParamsPagesSaveAccess): Boolean;
begin
  Result := Handler.Execute('pages.saveAccess', Params.List).ResponseAsInt(Id);
end;

{ TVkParamsPagesGet }

function TVkParamsPagesGet.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPagesGet.PageId(const Value: Integer): Integer;
begin
  Result := List.Add('page_id', Value);
end;

function TVkParamsPagesGet.Global(const Value: Boolean): Integer;
begin
  Result := List.Add('global', Value);
end;

function TVkParamsPagesGet.SitePreview(const Value: Boolean): Integer;
begin
  Result := List.Add('site_preview', Value);
end;

function TVkParamsPagesGet.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsPagesGet.NeedSource(const Value: Boolean): Integer;
begin
  Result := List.Add('need_source', Value);
end;

function TVkParamsPagesGet.NeedHtml(const Value: Boolean): Integer;
begin
  Result := List.Add('need_html', Value);
end;

{ TVkParamsPagesGetVersion }

function TVkParamsPagesGetVersion.VersionId(const Value: Integer): Integer;
begin
  Result := List.Add('version_id', Value);
end;

function TVkParamsPagesGetVersion.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPagesGetVersion.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

function TVkParamsPagesGetVersion.NeedHtml(const Value: Boolean): Integer;
begin
  Result := List.Add('need_html', Value);
end;

{ TVkParamsPagesSave }

function TVkParamsPagesSave.Text(const Value: string): Integer;
begin
  Result := List.Add('text', Value);
end;

function TVkParamsPagesSave.PageId(const Value: Integer): Integer;
begin
  Result := List.Add('page_id', Value);
end;

function TVkParamsPagesSave.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPagesSave.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

function TVkParamsPagesSave.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

{ TVkParamsPagesSaveAccess }

function TVkParamsPagesSaveAccess.PageId(const Value: Integer): Integer;
begin
  Result := List.Add('page_id', Value);
end;

function TVkParamsPagesSaveAccess.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPagesSaveAccess.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

function TVkParamsPagesSaveAccess.View(const Value: TVkPageAccess): Integer;
begin
  Result := List.Add('view', Ord(Value));
end;

function TVkParamsPagesSaveAccess.Edit(const Value: TVkPageAccess): Integer;
begin
  Result := List.Add('edit', Ord(Value));
end;

end.

