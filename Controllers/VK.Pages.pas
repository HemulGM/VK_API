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
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор вики-страницы
    /// </summary>
    function PageId(const Value: Integer): Integer;
    /// <summary>
    /// True — требуется получить информацию о глобальной вики-странице
    /// </summary>
    function Global(const Value: Boolean): Integer;
    /// <summary>
    /// True — получаемая wiki страница является предпросмотром для прикрепленной ссылки
    /// </summary>
    function SitePreview(const Value: Boolean): Integer;
    /// <summary>
    /// Название страницы
    /// </summary>
    function Title(const Value: string): Integer;
    /// <summary>
    /// True — требуется вернуть содержимое страницы в вики-формате
    /// </summary>
    function NeedSource(const Value: Boolean): Integer;
    /// <summary>
    /// True — требуется вернуть html-представление страницы
    /// </summary>
    function NeedHtml(const Value: Boolean): Integer;
  end;

  TVkParamsPagesGetVersion = record
    List: TParams;
    /// <summary>
    /// Идентификатор версии
    /// </summary>
    function VersionId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор сообщества, которому принадлежит вики-страница
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор пользователя, который создал страницу
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// Определяет, требуется ли в ответе html-представление вики-страницы
    /// </summary>
    function NeedHtml(const Value: Boolean): Integer;
  end;

  TVkParamsPagesSave = record
    List: TParams;
    /// <summary>
    /// Новый текст страницы в вики-формате
    /// </summary>
    function Text(const Value: string): Integer;
    /// <summary>
    /// Идентификатор вики-страницы. Вместо PageId может быть передан параметр Title
    /// </summary>
    function PageId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор сообщества, которому принадлежит вики-страница
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор пользователя, создавшего вики-страницу
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// Название вики-страницы
    /// </summary>
    function Title(const Value: string): Integer;
  end;

  TVkParamsPagesSaveAccess = record
    List: TParams;
    /// <summary>
    /// идентификатор вики-страницы
    /// </summary>
    function PageId(const Value: Integer): Integer;
    /// <summary>
    /// идентификатор сообщества, которому принадлежит вики-страница
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// идентификатор пользователя, создавшего вики-страницу
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// значение настройки доступа на чтение
    /// </summary>
    function View(const Value: TVkPageAccess): Integer;
    /// <summary>
    /// значение настройки доступа на редактирование
    /// </summary>
    function Edit(const Value: TVkPageAccess): Integer;
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

