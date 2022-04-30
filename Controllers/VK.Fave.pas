unit VK.Fave;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Audio, System.JSON, VK.Entity.Fave, VK.Entity.Fave.Pages;

type
  TVkParamsFaveGet = record
    List: TParams;
    /// <summary>
    /// ������������� �����, �������� ���������� ������� ��������� �������
    /// </summary>
    function TagId(const Value: Integer): TVkParamsFaveGet;
    /// <summary>
    /// ���� ��������, ������� ���������� �������
    /// </summary>
    function ItemType(const Value: TVkFaveType): TVkParamsFaveGet;
    /// <summary>
    /// �������� ������������ ������� ������� � ��������� ������������ ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFaveGet;
    /// <summary>
    /// ���������� ������������ �������� (������������ �������� 100)
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsFaveGet;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsFaveGet;
    /// <summary>
    /// IsFromSnackbar
    /// </summary>
    function IsFromSnackbar(const Value: Boolean): TVkParamsFaveGet;
    /// <summary>
    /// True, ���� ���������� �������� ���������� � ������������
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsFaveGet;
  end;

  TVkParamsFavePageTagsSet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, �������� ��������� ���������� ����� � ���������.
    /// ������������ ��������, ���� �� ����� �������� GroupId
    /// </summary>
    function UserId(const Value: Integer): TVkParamsFavePageTagsSet;
    /// <summary>
    /// ������������� ����������, �������� ��������� ���������� ����� � ���������.
    /// ������������ ��������, ���� �� ����� �������� UserId
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsFavePageTagsSet;
    /// <summary>
    /// ������������� ����� ������� �������������� �����, ������� ��������� ��������� ��������
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
    /// ��� �������, �������� ���������� ��������� �����
    /// </summary>
    function ItemType(const Value: TVkFaveType): TVkParamsFaveTagsSet;
    /// <summary>
    /// ������������� ��������� �������, �������� ��������� ��������� �����
    /// </summary>
    function ItemOwnerId(const Value: Integer): TVkParamsFaveTagsSet;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function ItemId(const Value: Integer): TVkParamsFaveTagsSet;
    /// <summary>
    /// ������������� �����, ������� ��������� ��������� �������
    /// </summary>
    function TagIds(const Value: TArrayOfInteger): TVkParamsFaveTagsSet;
    /// <summary>
    /// ������������� ������, ������� ��������� ��������� �����
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
    /// �������� ������������ ������� ������� � ��������� ������������ ��� ������� ������������� ������������
    /// (������������ �������� 10000)
    /// </summary>
    function Offset(const Value: Integer): TVkParamsFavePagesGet;
    /// <summary>
    /// ���������� ������������ �������� (������������ �������� 500)
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsFavePagesGet;
    /// <summary>
    /// ���� ��������, ������� ���������� �������
    /// ���� �������� �� ������ � �������� ������� ������������� � ���������, ����������� � ��������, � ������� ����������
    /// </summary>
    function &Type(const Value: TVkFavePageType): TVkParamsFavePagesGet;
    /// <summary>
    /// ������ �������������� ����� ��� �������� user � group, ������� ���������� �������
    /// </summary>
    function Fields(GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsFavePagesGet;
    /// <summary>
    /// ������������� �����, �������� ���������� ������� ��������� �������
    /// </summary>
    function TagId(const Value: Integer): TVkParamsFavePagesGet;
  end;

  TFaveController = class(TVkController)
  public
    /// <summary>
    /// ���������� �������, ����������� � �������� ������������.
    /// </summary>
    function Get(var Faves: TVkFaves; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� �������, ����������� � �������� ������������.
    /// </summary>
    function Get(var Faves: TVkFaves; Params: TVkParamsFaveGet): Boolean; overload;
    /// <summary>
    /// ������� ������ �� ������ �������� ������������.
    /// </summary>
    function RemoveLink(LinkId: Integer): Boolean;
    /// <summary>
    /// ������� �� �������� �������� ������������.
    /// </summary>
    function RemoveUserPage(UserId: Integer): Boolean;
    /// <summary>
    /// ������� �� �������� ����������.
    /// </summary>
    function RemoveGroupPage(GroupId: Integer): Boolean;
    /// <summary>
    /// ������� �� �������� ������ �� ����� ������������ ��� ����������.
    /// </summary>
    function RemovePost(OwnerId, Id: Integer): Boolean;
    /// <summary>
    /// ������� ������ �� ��������.
    /// </summary>
    function RemoveArticle(OwnerId, ArticleId: Integer; Ref: string): Boolean;
    /// <summary>
    /// ������� ����� �� ��������.
    /// </summary>
    function RemoveProduct(OwnerId, Id: Integer): Boolean;
    /// <summary>
    /// ������� ����� ��������.
    /// </summary>
    function RemoveTag(Id: Integer): Boolean;
    /// <summary>
    /// ������� ����������� �� ������ ��������.
    /// </summary>
    function RemoveVideo(OwnerId, Id: Integer): Boolean;
    /// <summary>
    /// ������ ������� ����� �������� � ������.
    /// </summary>
    function ReorderTags(Ids: TArrayOfInteger): Boolean;
    /// <summary>
    /// ������������� ����� �������� ������������ ��� ����������.
    /// </summary>
    function SetPageTags(Params: TVkParamsFavePageTagsSet): Boolean;
    /// <summary>
    /// ������������� ����� �������� ������������.
    /// </summary>
    function SetUserPageTags(UserId: Integer; Tags: TArrayOfInteger): Boolean;
    /// <summary>
    /// ������������� ����� �������� ����������.
    /// </summary>
    function SetGroupPageTags(GroupId: Integer; Tags: TArrayOfInteger): Boolean;
    /// <summary>
    /// ������������� ����� ���������� ������� � ������ ��������.
    /// </summary>
    function SetTags(Params: TVkParamsFaveTagsSet): Boolean;
    /// <summary>
    /// ������������� �������� ������������ � ��� ��������.
    /// </summary>
    function TrackPageInteractionUser(UserId: Integer): Boolean;
    /// <summary>
    /// ������������� �������� ���������� � ��� ��������.
    /// </summary>
    function TrackPageInteractionGroup(GroupId: Integer): Boolean;
    /// <summary>
    /// ���������� ������ ����� � ���������.
    /// </summary>
    function GetTags(var Items: TVkFaveTags): Boolean;
    /// <summary>
    /// ���������� �������� ������������� � ���������, ����������� � ��������.
    /// </summary>
    function GetPages(var Items: TVkFavePages; Params: TVkParamsFavePagesGet): Boolean;
    /// <summary>
    /// �������� �������� ��� �������������.
    /// </summary>
    function MarkSeen: Boolean;
    /// <summary>
    /// ����������� �����.
    /// </summary>
    function EditTag(Id: Integer; Name: string): Boolean;
    /// <summary>
    /// ��������� ����������� � ��������.
    /// </summary>
    function AddVideo(OwnerId, Id: Integer; AccessKey: string): Boolean;
    /// <summary>
    /// ������� ����� ��������.
    /// </summary>
    function AddTag(Name: string; Position: TVkTagPosition): Boolean;
    /// <summary>
    /// ��������� ����� � ��������.
    /// </summary>
    function AddProduct(OwnerId, Id: Integer; AccessKey: string): Boolean;
    /// <summary>
    /// ��������� ����� � ��������.
    /// </summary>
    function AddPost(OwnerId, Id: Integer; AccessKey: string; Params: TVkParamsFavePostAdd): Boolean;
    /// <summary>
    /// ��������� ������������ � ��������.
    /// </summary>
    function AddUserPage(Id: Integer): Boolean;
    /// <summary>
    /// ��������� ���������� � ��������.
    /// </summary>
    function AddGroupPage(Id: Integer): Boolean;
    /// <summary>
    /// ��������� ������ � ��������.
    /// </summary>
    function AddLink(Link: string): Boolean;
    /// <summary>
    /// ��������� ������ � ��������.
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

function TVkParamsFaveGet.Fields(const Value: TVkProfileFields): TVkParamsFaveGet;
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

function TVkParamsFavePageTagsSet.GroupId(const Value: Integer): TVkParamsFavePageTagsSet;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsFavePageTagsSet.TagIds(const Value: TArrayOfInteger): TVkParamsFavePageTagsSet;
begin
  List.Add('tag_ids', Value);
  Result := Self;
end;

function TVkParamsFavePageTagsSet.UserId(const Value: Integer): TVkParamsFavePageTagsSet;
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

function TVkParamsFaveTagsSet.ItemOwnerId(const Value: Integer): TVkParamsFaveTagsSet;
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

function TVkParamsFavePagesGet.Fields(GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsFavePagesGet;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
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

