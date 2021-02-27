unit VK.Fave;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Fave, VK.Entity.Fave.Pages;

type
  TVkParamsFaveGet = record
    List: TParams;
    function TagId(const Value: Integer): Integer;
    function ItemType(const Value: TVkFaveType): Integer;
    function Offset(const Value: Integer): Integer;
    function Count(const Value: Integer): Integer;
    function Fields(const Value: TArrayOfString): Integer;
    function IsFromSnackbar(const Value: Boolean): Integer;
    function Extended(const Value: Boolean): Integer;
  end;

  TVkParamsFavePageTagsSet = record
    List: TParams;
    function UserId(const Value: Integer): Integer;
    function GroupId(const Value: Integer): Integer;
    function TagIds(const Value: TArrayOfInteger): Integer;
  end;

  TVkParamsFavePostAdd = record
    List: TParams;
    function Ref(const Value: string): Integer;
    function TrackCode(const Value: string): Integer;
    function Source(const Value: string): Integer;
  end;

  TVkParamsFaveTagsSet = record
    List: TParams;
    function ItemType(const Value: TVkFaveType): Integer;
    function ItemOwnerId(const Value: Integer): Integer;
    function ItemId(const Value: Integer): Integer;
    function TagIds(const Value: TArrayOfInteger): Integer;
    function LinkId(const Value: string): Integer;
    function LinkUrl(const Value: string): Integer;
  end;

  TVkParamsFavePagesGet = record
    List: TParams;
    function Offset(const Value: Integer): Integer;
    function Count(const Value: Integer): Integer;
    function &Type(const Value: TVkFavePageType): Integer;
    function Fields(GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    function TagId(const Value: Integer): Integer;
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
    function RemoveUserPage(UserId: Integer): Boolean;
    /// <summary>
    /// Удаляет из закладок сообщество.
    /// </summary>
    function RemoveGroupPage(GroupId: Integer): Boolean;
    /// <summary>
    /// Удаляет из закладок запись на стене пользователя или сообщества.
    /// </summary>
    function RemovePost(OwnerId, Id: Integer): Boolean;
    /// <summary>
    /// Удаляет статью из закладок.
    /// </summary>
    function RemoveArticle(OwnerId, ArticleId: Integer; Ref: string): Boolean;
    /// <summary>
    /// Удаляет товар из закладок.
    /// </summary>
    function RemoveProduct(OwnerId, Id: Integer): Boolean;
    /// <summary>
    /// Удаляет метку закладок.
    /// </summary>
    function RemoveTag(Id: Integer): Boolean;
    /// <summary>
    /// Удаляет видеозапись из списка закладок.
    /// </summary>
    function RemoveVideo(OwnerId, Id: Integer): Boolean;
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
    function SetUserPageTags(UserId: Integer; Tags: TArrayOfInteger): Boolean;
    /// <summary>
    /// Устанавливает метку странице сообщества.
    /// </summary>
    function SetGroupPageTags(GroupId: Integer; Tags: TArrayOfInteger): Boolean;
    /// <summary>
    /// Устанавливает метку выбранному объекту в списке закладок.
    /// </summary>
    function SetTags(Params: TVkParamsFaveTagsSet): Boolean;
    /// <summary>
    /// Устанавливает страницу пользователя в топ закладок.
    /// </summary>
    function TrackPageInteractionUser(UserId: Integer): Boolean;
    /// <summary>
    /// Устанавливает страницу сообщества в топ закладок.
    /// </summary>
    function TrackPageInteractionGroup(GroupId: Integer): Boolean;
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
    function AddVideo(OwnerId, Id: Integer; AccessKey: string): Boolean;
    /// <summary>
    /// Создает метку закладок.
    /// </summary>
    function AddTag(Name: string; Position: TVkTagPosition): Boolean;
    /// <summary>
    /// Добавляет товар в закладки.
    /// </summary>
    function AddProduct(OwnerId, Id: Integer; AccessKey: string): Boolean;
    /// <summary>
    /// Добавляет товар в закладки.
    /// </summary>
    function AddPost(OwnerId, Id: Integer; AccessKey: string; Params: TVkParamsFavePostAdd): Boolean;
    /// <summary>
    /// Добавляет пользователя в закладки.
    /// </summary>
    function AddUserPage(Id: Integer): Boolean;
    /// <summary>
    /// Добавляет сообщество в закладки.
    /// </summary>
    function AddGroupPage(Id: Integer): Boolean;
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

function TFaveController.AddPost(OwnerId, Id: Integer; AccessKey: string; Params: TVkParamsFavePostAdd): Boolean;
begin
  Params.List.Add('owner_id', OwnerId);
  Params.List.Add('id', Id);
  Params.List.Add('access_key', AccessKey);
  Result := Handler.Execute('fave.addPost', Params.List).ResponseIsTrue;
end;

function TFaveController.AddProduct(OwnerId, Id: Integer; AccessKey: string): Boolean;
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

function TFaveController.AddGroupPage(Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.addPage', ['group_id', Id.ToString]).ResponseIsTrue;
end;

function TFaveController.AddUserPage(Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.addPage', ['user_id', Id.ToString]).ResponseIsTrue;
end;

function TFaveController.AddVideo(OwnerId, Id: Integer; AccessKey: string): Boolean;
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

function TFaveController.RemoveArticle(OwnerId, ArticleId: Integer; Ref: string): Boolean;
begin
  Result := Handler.Execute('fave.removeArticle', [
    ['owner_id', OwnerId.ToString],
    ['article_id', ArticleId.ToString],
    ['ref', Ref]]).
    ResponseIsTrue;
end;

function TFaveController.RemoveGroupPage(GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removePage', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TFaveController.RemoveLink(LinkId: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removeLink', ['link_id', LinkId.ToString]).ResponseIsTrue;
end;

function TFaveController.RemovePost(OwnerId, Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removePost', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]).ResponseIsTrue;
end;

function TFaveController.RemoveProduct(OwnerId, Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removeProduct', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]).ResponseIsTrue;
end;

function TFaveController.RemoveTag(Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removeTag', ['id', Id.ToString]).ResponseIsTrue;
end;

function TFaveController.RemoveUserPage(UserId: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removePage', ['user_id', UserId.ToString]).ResponseIsTrue;
end;

function TFaveController.RemoveVideo(OwnerId, Id: Integer): Boolean;
begin
  Result := Handler.Execute('fave.removeVideo', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]).ResponseIsTrue;
end;

function TFaveController.ReorderTags(Ids: TArrayOfInteger): Boolean;
begin
  Result := Handler.Execute('fave.reorderTags', ['ids', Ids.ToString]).ResponseIsTrue;
end;

function TFaveController.SetGroupPageTags(GroupId: Integer; Tags: TArrayOfInteger): Boolean;
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

function TFaveController.SetUserPageTags(UserId: Integer; Tags: TArrayOfInteger): Boolean;
begin
  Result := Handler.Execute('fave.setPageTags', [['user_id', UserId.ToString], ['tag_ids', Tags.ToString]]).ResponseIsTrue;
end;

function TFaveController.TrackPageInteractionGroup(GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('fave.trackPageInteraction', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TFaveController.TrackPageInteractionUser(UserId: Integer): Boolean;
begin
  Result := Handler.Execute('fave.trackPageInteraction', ['user_id', UserId.ToString]).ResponseIsTrue;
end;

{ TVkGetFaveParams }

function TVkParamsFaveGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFaveGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsFaveGet.Fields(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFaveGet.IsFromSnackbar(const Value: Boolean): Integer;
begin
  Result := List.Add('is_from_snackbar', Value);
end;

function TVkParamsFaveGet.ItemType(const Value: TVkFaveType): Integer;
begin
  Result := List.Add('item_type', Value.ToString);
end;

function TVkParamsFaveGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFaveGet.TagId(const Value: Integer): Integer;
begin
  Result := List.Add('tag_id', Value);
end;

{ TVkParamsFavePageTagsSet }

function TVkParamsFavePageTagsSet.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsFavePageTagsSet.TagIds(const Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('tag_ids', Value);
end;

function TVkParamsFavePageTagsSet.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsFaveTagsSet }

function TVkParamsFaveTagsSet.ItemId(const Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsFaveTagsSet.ItemOwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('item_owner_id', Value);
end;

function TVkParamsFaveTagsSet.ItemType(const Value: TVkFaveType): Integer;
begin
  Result := List.Add('item_type', Value.ToString);
end;

function TVkParamsFaveTagsSet.LinkId(const Value: string): Integer;
begin
  Result := List.Add('link_id', Value);
end;

function TVkParamsFaveTagsSet.LinkUrl(const Value: string): Integer;
begin
  Result := List.Add('link_url', Value);
end;

function TVkParamsFaveTagsSet.TagIds(const Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('tag_ids', Value);
end;

{ TVkParamsFavePagesGet }

function TVkParamsFavePagesGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFavePagesGet.Fields(GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsFavePagesGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFavePagesGet.TagId(const Value: Integer): Integer;
begin
  Result := List.Add('tag_id', Value);
end;

function TVkParamsFavePagesGet.&Type(const Value: TVkFavePageType): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

{ TVkParamsFavePostAdd }

function TVkParamsFavePostAdd.Ref(const Value: string): Integer;
begin
  Result := List.Add('ref', Value);
end;

function TVkParamsFavePostAdd.Source(const Value: string): Integer;
begin
  Result := List.Add('source', Value);
end;

function TVkParamsFavePostAdd.TrackCode(const Value: string): Integer;
begin
  Result := List.Add('track_code', Value);
end;

end.

