unit VK.Pages;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Page;

type
  TVkParamsPagesGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца вики-страницы
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPagesGet;
    /// <summary>
    /// Идентификатор вики-страницы
    /// </summary>
    function PageId(const Value: Integer): TVkParamsPagesGet;
    /// <summary>
    /// True — требуется получить информацию о глобальной вики-странице
    /// </summary>
    function Global(const Value: Boolean = True): TVkParamsPagesGet;
    /// <summary>
    /// True — получаемая wiki страница является предпросмотром для прикрепленной ссылки
    /// </summary>
    function SitePreview(const Value: Boolean = True): TVkParamsPagesGet;
    /// <summary>
    /// Название страницы
    /// </summary>
    function Title(const Value: string): TVkParamsPagesGet;
    /// <summary>
    /// True — требуется вернуть содержимое страницы в вики-формате
    /// </summary>
    function NeedSource(const Value: Boolean = True): TVkParamsPagesGet;
    /// <summary>
    /// True — требуется вернуть html-представление страницы
    /// </summary>
    function NeedHtml(const Value: Boolean = True): TVkParamsPagesGet;
  end;

  TVkParamsPagesGetVersion = record
    List: TParams;
    /// <summary>
    /// Идентификатор версии
    /// </summary>
    function VersionId(const Value: Integer): TVkParamsPagesGetVersion;
    /// <summary>
    /// Идентификатор сообщества, которому принадлежит вики-страница
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPagesGetVersion;
    /// <summary>
    /// Идентификатор пользователя, который создал страницу
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPagesGetVersion;
    /// <summary>
    /// Определяет, требуется ли в ответе html-представление вики-страницы
    /// </summary>
    function NeedHtml(const Value: Boolean): TVkParamsPagesGetVersion;
  end;

  TVkParamsPagesSave = record
    List: TParams;
    /// <summary>
    /// Новый текст страницы в вики-формате
    /// </summary>
    function Text(const Value: string): TVkParamsPagesSave;
    /// <summary>
    /// Идентификатор вики-страницы. Вместо PageId может быть передан параметр Title
    /// </summary>
    function PageId(const Value: Integer): TVkParamsPagesSave;
    /// <summary>
    /// Идентификатор сообщества, которому принадлежит вики-страница
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPagesSave;
    /// <summary>
    /// Идентификатор пользователя, создавшего вики-страницу
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPagesSave;
    /// <summary>
    /// Название вики-страницы
    /// </summary>
    function Title(const Value: string): TVkParamsPagesSave;
  end;

  TVkParamsPagesSaveAccess = record
    List: TParams;
    /// <summary>
    /// Идентификатор вики-страницы
    /// </summary>
    function PageId(const Value: Integer): TVkParamsPagesSaveAccess;
    /// <summary>
    /// Идентификатор сообщества, которому принадлежит вики-страница
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPagesSaveAccess;
    /// <summary>
    /// Идентификатор пользователя, создавшего вики-страницу
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPagesSaveAccess;
    /// <summary>
    /// Значение настройки доступа на чтение
    /// </summary>
    function View(const Value: TVkPageAccess): TVkParamsPagesSaveAccess;
    /// <summary>
    /// Значение настройки доступа на редактирование
    /// </summary>
    function Edit(const Value: TVkPageAccess): TVkParamsPagesSaveAccess;
  end;

  /// <summary>
  /// Методы для работы с вики-страницами.
  /// </summary>
  TPagesController = class(TVkController)
  public
    /// <summary>
    /// Позволяет очистить кеш отдельных внешних страниц, которые могут быть прикреплены к записям ВКонтакте. После очистки кеша при последующем прикреплении ссылки к записи, данные о странице будут обновлены.
    /// Внешние страницы – страницы которые прикрепляются к записям вместе с ссылкой и доступные по кнопке "Предпросмотр".
    /// </summary>
    function ClearCache(var Status: Boolean; const Url: string): Boolean;
    /// <summary>
    /// Возвращает информацию о вики-странице.
    /// </summary>
    function Get(var Item: TVkPage; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о вики-странице.
    /// </summary>
    function Get(var Item: TVkPage; Params: TVkParamsPagesGet): Boolean; overload;
    /// <summary>
    /// Возвращает список всех старых версий вики-страницы.
    /// </summary>
    function GetHistory(var Items: TVkPageVersions; const PageId: Integer; GroupId: Integer = 0; UserId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список вики-страниц в группе.
    /// </summary>
    function GetTitles(var Items: TVkPages; const GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает текст одной из старых версий страницы.
    /// </summary>
    function GetVersion(var Item: TVkPage; Params: TVkParamsPagesGetVersion): Boolean; overload;
    /// <summary>
    /// Возвращает html-представление вики-разметки.
    /// </summary>
    function ParseWiki(var Html: string; const Text: string; const GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Сохраняет текст вики-страницы.
    /// </summary>
    function Save(var Id: Integer; Params: TVkParamsPagesSave): Boolean;
    /// <summary>
    /// Сохраняет новые настройки доступа на чтение и редактирование вики-страницы.
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

