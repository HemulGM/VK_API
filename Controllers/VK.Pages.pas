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
    function OwnerId(const Value: Integer): TVkParamsPagesGet;
    /// <summary>
    /// ������������� ����-��������
    /// </summary>
    function PageId(const Value: Integer): TVkParamsPagesGet;
    /// <summary>
    /// True � ��������� �������� ���������� � ���������� ����-��������
    /// </summary>
    function Global(const Value: Boolean = True): TVkParamsPagesGet;
    /// <summary>
    /// True � ���������� wiki �������� �������� �������������� ��� ������������� ������
    /// </summary>
    function SitePreview(const Value: Boolean = True): TVkParamsPagesGet;
    /// <summary>
    /// �������� ��������
    /// </summary>
    function Title(const Value: string): TVkParamsPagesGet;
    /// <summary>
    /// True � ��������� ������� ���������� �������� � ����-�������
    /// </summary>
    function NeedSource(const Value: Boolean = True): TVkParamsPagesGet;
    /// <summary>
    /// True � ��������� ������� html-������������� ��������
    /// </summary>
    function NeedHtml(const Value: Boolean = True): TVkParamsPagesGet;
  end;

  TVkParamsPagesGetVersion = record
    List: TParams;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function VersionId(const Value: Integer): TVkParamsPagesGetVersion;
    /// <summary>
    /// ������������� ����������, �������� ����������� ����-��������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPagesGetVersion;
    /// <summary>
    /// ������������� ������������, ������� ������ ��������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPagesGetVersion;
    /// <summary>
    /// ����������, ��������� �� � ������ html-������������� ����-��������
    /// </summary>
    function NeedHtml(const Value: Boolean): TVkParamsPagesGetVersion;
  end;

  TVkParamsPagesSave = record
    List: TParams;
    /// <summary>
    /// ����� ����� �������� � ����-�������
    /// </summary>
    function Text(const Value: string): TVkParamsPagesSave;
    /// <summary>
    /// ������������� ����-��������. ������ PageId ����� ���� ������� �������� Title
    /// </summary>
    function PageId(const Value: Integer): TVkParamsPagesSave;
    /// <summary>
    /// ������������� ����������, �������� ����������� ����-��������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPagesSave;
    /// <summary>
    /// ������������� ������������, ���������� ����-��������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPagesSave;
    /// <summary>
    /// �������� ����-��������
    /// </summary>
    function Title(const Value: string): TVkParamsPagesSave;
  end;

  TVkParamsPagesSaveAccess = record
    List: TParams;
    /// <summary>
    /// ������������� ����-��������
    /// </summary>
    function PageId(const Value: Integer): TVkParamsPagesSaveAccess;
    /// <summary>
    /// ������������� ����������, �������� ����������� ����-��������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPagesSaveAccess;
    /// <summary>
    /// ������������� ������������, ���������� ����-��������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPagesSaveAccess;
    /// <summary>
    /// �������� ��������� ������� �� ������
    /// </summary>
    function View(const Value: TVkPageAccess): TVkParamsPagesSaveAccess;
    /// <summary>
    /// �������� ��������� ������� �� ��������������
    /// </summary>
    function Edit(const Value: TVkPageAccess): TVkParamsPagesSaveAccess;
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
  Result := Handler.Execute('pages.parseWiki', Params.Add('text', Text)).ResponseAsStr(Html);
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

function TVkParamsPagesGet.OwnerId(const Value: Integer): TVkParamsPagesGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPagesGet.PageId(const Value: Integer): TVkParamsPagesGet;
begin
  List.Add('page_id', Value);
  Result := Self;
end;

function TVkParamsPagesGet.Global(const Value: Boolean): TVkParamsPagesGet;
begin
  List.Add('global', Value);
  Result := Self;
end;

function TVkParamsPagesGet.SitePreview(const Value: Boolean): TVkParamsPagesGet;
begin
  List.Add('site_preview', Value);
  Result := Self;
end;

function TVkParamsPagesGet.Title(const Value: string): TVkParamsPagesGet;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsPagesGet.NeedSource(const Value: Boolean): TVkParamsPagesGet;
begin
  List.Add('need_source', Value);
  Result := Self;
end;

function TVkParamsPagesGet.NeedHtml(const Value: Boolean): TVkParamsPagesGet;
begin
  List.Add('need_html', Value);
  Result := Self;
end;

{ TVkParamsPagesGetVersion }

function TVkParamsPagesGetVersion.VersionId(const Value: Integer): TVkParamsPagesGetVersion;
begin
  List.Add('version_id', Value);
  Result := Self;
end;

function TVkParamsPagesGetVersion.GroupId(const Value: Integer): TVkParamsPagesGetVersion;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPagesGetVersion.UserId(const Value: Integer): TVkParamsPagesGetVersion;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

function TVkParamsPagesGetVersion.NeedHtml(const Value: Boolean): TVkParamsPagesGetVersion;
begin
  List.Add('need_html', Value);
  Result := Self;
end;

{ TVkParamsPagesSave }

function TVkParamsPagesSave.Text(const Value: string): TVkParamsPagesSave;
begin
  List.Add('text', Value);
  Result := Self;
end;

function TVkParamsPagesSave.PageId(const Value: Integer): TVkParamsPagesSave;
begin
  List.Add('page_id', Value);
  Result := Self;
end;

function TVkParamsPagesSave.GroupId(const Value: Integer): TVkParamsPagesSave;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPagesSave.UserId(const Value: Integer): TVkParamsPagesSave;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

function TVkParamsPagesSave.Title(const Value: string): TVkParamsPagesSave;
begin
  List.Add('title', Value);
  Result := Self;
end;

{ TVkParamsPagesSaveAccess }

function TVkParamsPagesSaveAccess.PageId(const Value: Integer): TVkParamsPagesSaveAccess;
begin
  List.Add('page_id', Value);
  Result := Self;
end;

function TVkParamsPagesSaveAccess.GroupId(const Value: Integer): TVkParamsPagesSaveAccess;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPagesSaveAccess.UserId(const Value: Integer): TVkParamsPagesSaveAccess;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

function TVkParamsPagesSaveAccess.View(const Value: TVkPageAccess): TVkParamsPagesSaveAccess;
begin
  List.Add('view', Ord(Value));
  Result := Self;
end;

function TVkParamsPagesSaveAccess.Edit(const Value: TVkPageAccess): TVkParamsPagesSaveAccess;
begin
  List.Add('edit', Ord(Value));
  Result := Self;
end;

end.

