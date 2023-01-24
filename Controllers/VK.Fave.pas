﻿unit VK.Fave;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Audio, System.JSON, VK.Entity.Fave, VK.Entity.Fave.Pages;

type
  TVkParamsFaveGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор метки, закладки отмеченные которой требуется вернуть
    /// </summary>
    function TagId(const Value: Integer): TVkParamsFaveGet;
    /// <summary>
    /// Типы объектов, которые необходимо вернуть
    /// </summary>
    function ItemType(const Value: TVkFaveType): TVkParamsFaveGet;
    /// <summary>
    /// Смещение относительно первого объекта в закладках пользователя для выборки определенного подмножества
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFaveGet;
    /// <summary>
    /// Количество возвращаемых закладок (максимальное значение 100)
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsFaveGet;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsFaveGet;
    /// <summary>
    /// IsFromSnackbar
    /// </summary>
    function IsFromSnackbar(const Value: Boolean): TVkParamsFaveGet;
    /// <summary>
    /// True, если необходимо получить информацию о пользователе
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsFaveGet;
  end;

  TVkParamsFavePageTagsSet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, которому требуется проставить метку в закладках.
    /// Обязательный параметр, если не задан параметр GroupId
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsFavePageTagsSet;
    /// <summary>
    /// Идентификатор сообщества, которому требуется проставить метку в закладках.
    /// Обязательный параметр, если не задан параметр UserId
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsFavePageTagsSet;
    /// <summary>
    /// Перечисленные через запятую идентификаторы тегов, которые требуется присвоить странице
    /// </summary>
    function TagIds(const Value: TArrayOfInteger): TVkParamsFavePageTagsSet;
  end;

  TVkParamsFavePostAdd = record
    List: TParams;
    function Ref(const Value: string): TVkParamsFavePostAdd;
    function TrackCode(const Value: string): TVkParamsFavePostAdd;
    function Source(const Value: string): TVkParamsFavePostAdd;
  end;

  TVkParamsFaveTagsSet = record
    List: TParams;
    /// <summary>
    /// Тип объекта, которому необходимо присвоить метку
    /// </summary>
    function ItemType(const Value: TVkFaveType): TVkParamsFaveTagsSet;
    /// <summary>
    /// Идентификатор владельца объекта, которому требуется присвоить метку
    /// </summary>
    function ItemOwnerId(const Value: TVkPeerId): TVkParamsFaveTagsSet;
    /// <summary>
    /// Идентификатор объекта
    /// </summary>
    function ItemId(const Value: Integer): TVkParamsFaveTagsSet;
    /// <summary>
    /// Идентификатор метки, которую требуется присвоить объекту
    /// </summary>
    function TagIds(const Value: TArrayOfInteger): TVkParamsFaveTagsSet;
    /// <summary>
    /// Идентификатор ссылки, которой требуется присвоить метку
    /// </summary>
    function LinkId(const Value: string): TVkParamsFaveTagsSet;
    /// <summary>
    /// LinkUrl
    /// </summary>
    function LinkUrl(const Value: string): TVkParamsFaveTagsSet;
  end;

  TVkParamsFavePagesGet = record
    List: TParams;
    /// <summary>
    /// Смещение относительно первого объекта в закладках пользователя для выборки определенного подмножества
    /// (максимальное значение 10000)
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFavePagesGet;
    /// <summary>
    /// Количество возвращаемых закладок (максимальное значение 500)
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsFavePagesGet;
    /// <summary>
    /// Типы объектов, которые необходимо вернуть
    /// Если параметр не указан — вернутся объекты пользователей и сообществ, добавленных в закладки, в порядке добавления
    /// </summary>
    function &Type(const Value: TVkFavePageType): TVkParamsFavePagesGet;
    /// <summary>
    /// Список дополнительных полей для объектов user и group, которые необходимо вернуть
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsFavePagesGet;
    /// <summary>
    /// Идентификатор метки, закладки отмеченные которой требуется вернуть
    /// </summary>
    function TagId(const Value: Integer): TVkParamsFavePagesGet;
  end;

  TFaveController = class(TVkController)
  public
    /// <summary>
    /// Возвращает объекты, добавленные в закладки пользователя.
    /// </summary>
    function Get(var Faves: TVkFaves; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает объекты, добавленные в закладки пользователя.
    /// </summary>
    function Get(var Faves: TVkFaves; Params: TVkParamsFaveGet): Boolean; overload;
    /// <summary>
    /// Удаляет ссылку из списка закладок пользователя.
    /// </summary>
    function RemoveLink(LinkId: Integer): Boolean;
    /// <summary>
    /// Удаляет из закладок страницу пользователя.
    /// </summary>
    function RemoveUserPage(UserId: TVkPeerId): Boolean;
    /// <summary>
    /// Удаляет из закладок сообщество.
    /// </summary>
    function RemoveGroupPage(GroupId: TVkPeerId): Boolean;
    /// <summary>
    /// Удаляет из закладок запись на стене пользователя или сообщества.
    /// </summary>
    function RemovePost(OwnerId: TVkPeerId; Id: Integer): Boolean;
    /// <summary>
    /// Удаляет статью из закладок.
    /// </summary>
    function RemoveArticle(OwnerId: TVkPeerId; ArticleId: Integer; Ref: string): Boolean;
    /// <summary>
    /// Удаляет товар из закладок.
    /// </summary>
    function RemoveProduct(OwnerId: TVkPeerId; Id: Integer): Boolean;
    /// <summary>
    /// Удаляет метку закладок.
    /// </summary>
    function RemoveTag(Id: Integer): Boolean;
    /// <summary>
    /// Удаляет видеозапись из списка закладок.
    /// </summary>
    function RemoveVideo(OwnerId: TVkPeerId; Id: Integer): Boolean;
    /// <summary>
    /// Меняет порядок меток закладок в списке.
    /// </summary>
    function ReorderTags(Ids: TArrayOfInteger): Boolean;
    /// <summary>
    /// Устанавливает метку странице пользователя или сообщества.
    /// </summary>
    function SetPageTags(Params: TVkParamsFavePageTagsSet): Boolean;
    /// <summary>
    /// Устанавливает метку странице пользователя.
    /// </summary>
    function SetUserPageTags(UserId: TVkPeerId; Tags: TArrayOfInteger): Boolean;
    /// <summary>
    /// Устанавливает метку странице сообщества.
    /// </summary>
    function SetGroupPageTags(GroupId: TVkPeerId; Tags: TArrayOfInteger): Boolean;
    /// <summary>
    /// Устанавливает метку выбранному объекту в списке закладок.
    /// </summary>
    function SetTags(Params: TVkParamsFaveTagsSet): Boolean;
    /// <summary>
    /// Устанавливает страницу пользователя в топ закладок.
    /// </summary>
    function TrackPageInteractionUser(UserId: TVkPeerId): Boolean;
    /// <summary>
    /// Устанавливает страницу сообщества в топ закладок.
    /// </summary>
    function TrackPageInteractionGroup(GroupId: TVkPeerId): Boolean;
    /// <summary>
    /// Возвращает список меток в закладках.
    /// </summary>
    function GetTags(var Items: TVkFaveTags): Boolean;
    /// <summary>
    /// Возвращает страницы пользователей и сообществ, добавленных в закладки.
    /// </summary>
    function GetPages(var Items: TVkFavePages; Params: TVkParamsFavePagesGet): Boolean;
    /// <summary>
    /// Отмечает закладки как просмотренные.
    /// </summary>
    function MarkSeen: Boolean;
    /// <summary>
    /// Редактирует метку.
    /// </summary>
    function EditTag(Id: Integer; Name: string): Boolean;
    /// <summary>
    /// Добавляет видеозапись в закладки.
    /// </summary>
    function AddVideo(OwnerId: TVkPeerId; Id: Integer; AccessKey: string): Boolean;
    /// <summary>
    /// Создает метку закладок.
    /// </summary>
    function AddTag(Name: string; Position: TVkTagPosition): Boolean;
    /// <summary>
    /// Добавляет товар в закладки.
    /// </summary>
    function AddProduct(OwnerId: TVkPeerId; Id: Integer; AccessKey: string): Boolean;
    /// <summary>
    /// Добавляет товар в закладки.
    /// </summary>
    function AddPost(OwnerId: TVkPeerId; Id: Integer; AccessKey: string; Params: TVkParamsFavePostAdd): Boolean;
    /// <summary>
    /// Добавляет пользователя в закладки.
    /// </summary>
    function AddUserPage(Id: TVkPeerId): Boolean;
    /// <summary>
    /// Добавляет сообщество в закладки.
    /// </summary>
    function AddGroupPage(Id: TVkPeerId): Boolean;
    /// <summary>
    /// Добавляет ссылку в закладки.
    /// </summary>
    function AddLink(Link: string): Boolean;
    /// <summary>
    /// Добавляет статью в закладки.
    /// </summary>
    function AddArticle(Url: string): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TFaveController }

function TFaveController.Get(var Faves: TVkFaves; Params: TParams): Boolean;
begin
  Result := Handler.Execute('fave.get', Params).GetObject(Faves);
end;

function TFaveController.AddLink(Link: string): Boolean;
begin
  Result := Handler.Execute('fave.addLink', ['link', Link]).ResponseIsTrue;
end;

function TFaveController.AddPost(OwnerId: TVkPeerId; Id: Integer; AccessKey: string; Params: TVkParamsFavePostAdd): Boolean;
begin
  Params.List.Add('owner_id', OwnerId);
  Params.List.Add('id', Id);
  Params.List.Add('access_key', AccessKey);
  Result := Handler.Execute('fave.addPost', Params.List).ResponseIsTrue;
end;

function TFaveController.AddProduct(OwnerId: TVkPeerId; Id: Integer; AccessKey: string): Boolean;
begin
  Result := Handler.Execute('fave.addProduct', [
    ['owner_id', OwnerId.ToString],
    ['id', Id.ToString],
    ['access_key', AccessKey]]).
    ResponseIsTrue;
end;

function TFaveController.AddTag(Name: string; Position: TVkTagPosition): Boolean;
begin
  Result := Handler.Execute('fave.addVideo', [['name', Name], ['position', Position.ToString]]).ResponseIsTrue;
end;

function TFaveController.AddArticle(Url: string): Boolean;
begin
  Result := Handler.Execute('fave.addArticle', ['url', Url]).ResponseIsTrue;
end;

function TFaveController.AddGroupPage(Id: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('fave.addPage', ['group_id', Id.ToString]).ResponseIsTrue;
end;

function TFaveController.AddUserPage(Id: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('fave.addPage', ['user_id', Id.ToString]).ResponseIsTrue;
end;

function TFaveController.AddVideo(OwnerId: TVkPeerId; Id: Integer; AccessKey: string): Boolean;
begin
  Result := Handler.Execute('fave.addVideo', [
    ['owner_id', OwnerId.ToString],
    ['id', Id.ToString],
    ['access_key', AccessKey]]).
    ResponseIsTrue;
end;

function TFaveController.EditTag(Id: Integer; Name: string): Boolean;
begin
  Result := Handler.Execute('fave.editTag', [['id', Id.ToString], ['name', Name]]).ResponseIsTrue;
end;

function TFaveController.Get(var Faves: TVkFaves; Params: TVkParamsFaveGet): Boolean;
begin
  Result := Get(Faves, Params.List);
end;

function TFaveController.GetPages(var Items: TVkFavePages; Params: TVkParamsFavePagesGet): Boolean;
begin
  Result := Handler.Execute('fave.getPages', Params.List).GetObject(Items);
end;

function TFaveController.GetTags(var Items: TVkFaveTags): Boolean;
begin
  Result := Handler.Execute('fave.getTags').GetObject(Items);
end;

function TFaveController.MarkSeen: Boolean;
begin
  Result := Handler.Execute('fave.markSeen').ResponseIsTrue;
end;

function TFaveController.RemoveArticle(OwnerId: TVkPeerId; ArticleId: Integer; Ref: string): Boolean;
begin
  Result := Handler.Execute('fave.removeArticle', [
    ['owner_id', OwnerId.ToString],
    ['article_id', ArticleId.ToString],
    ['ref', Ref]]).
    ResponseIsTrue;
end;

function TFaveController.RemoveGroupPage(GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('fave.removePage', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TFaveController.RemoveLink(LinkId: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removeLink', ['link_id', LinkId.ToString]).ResponseIsTrue;
end;

function TFaveController.RemovePost(OwnerId: TVkPeerId; Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removePost', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]).ResponseIsTrue;
end;

function TFaveController.RemoveProduct(OwnerId: TVkPeerId; Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removeProduct', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]).ResponseIsTrue;
end;

function TFaveController.RemoveTag(Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removeTag', ['id', Id.ToString]).ResponseIsTrue;
end;

function TFaveController.RemoveUserPage(UserId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('fave.removePage', ['user_id', UserId.ToString]).ResponseIsTrue;
end;

function TFaveController.RemoveVideo(OwnerId: TVkPeerId; Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removeVideo', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]).ResponseIsTrue;
end;

function TFaveController.ReorderTags(Ids: TArrayOfInteger): Boolean;
begin
  Result := Handler.Execute('fave.reorderTags', ['ids', Ids.ToString]).ResponseIsTrue;
end;

function TFaveController.SetGroupPageTags(GroupId: TVkPeerId; Tags: TArrayOfInteger): Boolean;
begin
  Result := Handler.Execute('fave.setPageTags', [['group_id', GroupId.ToString], ['tag_ids', Tags.ToString]]).ResponseIsTrue;
end;

function TFaveController.SetPageTags(Params: TVkParamsFavePageTagsSet): Boolean;
begin
  Result := Handler.Execute('fave.setPageTags', Params.List).ResponseIsTrue;
end;

function TFaveController.SetTags(Params: TVkParamsFaveTagsSet): Boolean;
begin
  Result := Handler.Execute('fave.setTags', Params.List).ResponseIsTrue;
end;

function TFaveController.SetUserPageTags(UserId: TVkPeerId; Tags: TArrayOfInteger): Boolean;
begin
  Result := Handler.Execute('fave.setPageTags', [['user_id', UserId.ToString], ['tag_ids', Tags.ToString]]).ResponseIsTrue;
end;

function TFaveController.TrackPageInteractionGroup(GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('fave.trackPageInteraction', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TFaveController.TrackPageInteractionUser(UserId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('fave.trackPageInteraction', ['user_id', UserId.ToString]).ResponseIsTrue;
end;

{ TVkGetFaveParams }

function TVkParamsFaveGet.Count(const Value: Integer): TVkParamsFaveGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFaveGet.Extended(const Value: Boolean): TVkParamsFaveGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsFaveGet.Fields(const Value: TVkExtendedFields): TVkParamsFaveGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFaveGet.IsFromSnackbar(const Value: Boolean): TVkParamsFaveGet;
begin
  List.Add('is_from_snackbar', Value);
  Result := Self;
end;

function TVkParamsFaveGet.ItemType(const Value: TVkFaveType): TVkParamsFaveGet;
begin
  List.Add('item_type', Value.ToString);
  Result := Self;
end;

function TVkParamsFaveGet.Offset(const Value: Integer): TVkParamsFaveGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFaveGet.TagId(const Value: Integer): TVkParamsFaveGet;
begin
  List.Add('tag_id', Value);
  Result := Self;
end;

{ TVkParamsFavePageTagsSet }

function TVkParamsFavePageTagsSet.GroupId(const Value: TVkPeerId): TVkParamsFavePageTagsSet;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsFavePageTagsSet.TagIds(const Value: TArrayOfInteger): TVkParamsFavePageTagsSet;
begin
  List.Add('tag_ids', Value);
  Result := Self;
end;

function TVkParamsFavePageTagsSet.UserId(const Value: TVkPeerId): TVkParamsFavePageTagsSet;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsFaveTagsSet }

function TVkParamsFaveTagsSet.ItemId(const Value: Integer): TVkParamsFaveTagsSet;
begin
  List.Add('item_id', Value);
  Result := Self;
end;

function TVkParamsFaveTagsSet.ItemOwnerId(const Value: TVkPeerId): TVkParamsFaveTagsSet;
begin
  List.Add('item_owner_id', Value);
  Result := Self;
end;

function TVkParamsFaveTagsSet.ItemType(const Value: TVkFaveType): TVkParamsFaveTagsSet;
begin
  List.Add('item_type', Value.ToString);
  Result := Self;
end;

function TVkParamsFaveTagsSet.LinkId(const Value: string): TVkParamsFaveTagsSet;
begin
  List.Add('link_id', Value);
  Result := Self;
end;

function TVkParamsFaveTagsSet.LinkUrl(const Value: string): TVkParamsFaveTagsSet;
begin
  List.Add('link_url', Value);
  Result := Self;
end;

function TVkParamsFaveTagsSet.TagIds(const Value: TArrayOfInteger): TVkParamsFaveTagsSet;
begin
  List.Add('tag_ids', Value);
  Result := Self;
end;

{ TVkParamsFavePagesGet }

function TVkParamsFavePagesGet.Count(const Value: Integer): TVkParamsFavePagesGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsFavePagesGet.Fields(Value: TVkExtendedFields): TVkParamsFavePagesGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsFavePagesGet.Offset(const Value: Integer): TVkParamsFavePagesGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsFavePagesGet.TagId(const Value: Integer): TVkParamsFavePagesGet;
begin
  List.Add('tag_id', Value);
  Result := Self;
end;

function TVkParamsFavePagesGet.&Type(const Value: TVkFavePageType): TVkParamsFavePagesGet;
begin
  List.Add('type', Value.ToString);
  Result := Self;
end;

{ TVkParamsFavePostAdd }

function TVkParamsFavePostAdd.Ref(const Value: string): TVkParamsFavePostAdd;
begin
  List.Add('ref', Value);
  Result := Self;
end;

function TVkParamsFavePostAdd.Source(const Value: string): TVkParamsFavePostAdd;
begin
  List.Add('source', Value);
  Result := Self;
end;

function TVkParamsFavePostAdd.TrackCode(const Value: string): TVkParamsFavePostAdd;
begin
  List.Add('track_code', Value);
  Result := Self;
end;

end.

