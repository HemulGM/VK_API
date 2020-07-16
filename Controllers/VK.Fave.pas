unit VK.Fave;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Fave, VK.Entity.Fave.Pages;

type
  TVkTagPosition = (tpFront, tpBack);

  TVkTagPositionHelper = record helper for TVkTagPosition
    function ToString: string; inline;
  end;

  TVkFaveTypeGet = (ftPost, ftVideo, ftProduct, ftArticle, ftLink);

  TVkFaveTypeGetHelper = record helper for TVkFaveTypeGet
    function ToString: string; inline;
  end;

  TVkFavePageType = (ftUsers, ftGroups, ftHints);

  TVkFavePageTypeHelper = record helper for TVkFavePageType
    function ToString: string; inline;
  end;

  TVkParamsFaveGet = record
    List: TParams;
    function TagId(Value: Integer): Integer;
    function ItemType(Value: TVkFaveTypeGet): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Fields(Value: TArrayOfString): Integer;
    function IsFromSnackbar(Value: Boolean): Integer;
    function Extended(Value: Boolean): Integer;
  end;

  TVkParamsFavePageTagsSet = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
    function TagIds(Value: TArrayOfInteger): Integer;
  end;

  TVkParamsFavePostAdd = record
    List: TParams;
    function Ref(Value: string): Integer;
    function TrackCode(Value: string): Integer;
    function Source(Value: string): Integer;
  end;

  TVkParamsFaveTagsSet = record
    List: TParams;
    function ItemType(Value: TVkFaveTypeGet): Integer;
    function ItemOwnerId(Value: Integer): Integer;
    function ItemId(Value: Integer): Integer;
    function TagIds(Value: TArrayOfInteger): Integer;
    function LinkId(Value: string): Integer;
    function LinkUrl(Value: string): Integer;
  end;

  TVkParamsFavePagesGet = record
    List: TParams;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function &Type(Value: TVkFavePageType): Integer;
    function Fields(GroupFields: TVkGroupFields = []; UserFields: TVkUserFields = []): Integer;
    function TagId(Value: Integer): Integer;
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
    /// ������� �� �������� ���������� ��� �������� ������������.
    /// </summary>
    function RemovePage(UserId, GroupId: Integer): Boolean;
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
    /// ������������� �������� ������������ ��� ���������� � ��� ��������.
    /// </summary>
    function TrackPageInteractionUser(UserId: Integer): Boolean;
    /// <summary>
    /// ������������� �������� ������������ ��� ���������� � ��� ��������.
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
  with Handler.Execute('fave.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Faves := TVkFaves.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFaveController.AddLink(Link: string): Boolean;
begin
  with Handler.Execute('fave.addLink', ['link', Link]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.AddPost(OwnerId, Id: Integer; AccessKey: string; Params: TVkParamsFavePostAdd): Boolean;
begin
  Params.List.Add('owner_id', OwnerId);
  Params.List.Add('id', Id);
  Params.List.Add('access_key', AccessKey);
  with Handler.Execute('fave.addPost', Params.List) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.AddProduct(OwnerId, Id: Integer; AccessKey: string): Boolean;
begin
  with Handler.Execute('fave.addProduct', [['owner_id', OwnerId.ToString], ['id', Id.ToString], ['access_key', AccessKey]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.AddTag(Name: string; Position: TVkTagPosition): Boolean;
begin
  with Handler.Execute('fave.addVideo', [['name', Name], ['position', Position.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.AddArticle(Url: string): Boolean;
begin
  with Handler.Execute('fave.addArticle', ['url', Url]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.AddGroupPage(Id: Integer): Boolean;
begin
  with Handler.Execute('fave.addPage', ['group_id', Id.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.AddUserPage(Id: Integer): Boolean;
begin
  with Handler.Execute('fave.addPage', ['user_id', Id.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.AddVideo(OwnerId, Id: Integer; AccessKey: string): Boolean;
begin
  with Handler.Execute('fave.addVideo', [['owner_id', OwnerId.ToString], ['id', Id.ToString], ['access_key', AccessKey]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.EditTag(Id: Integer; Name: string): Boolean;
begin
  with Handler.Execute('fave.editTag', [['id', Id.ToString], ['name', Name]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.Get(var Faves: TVkFaves; Params: TVkParamsFaveGet): Boolean;
begin
  Result := Get(Faves, Params.List);
end;

function TFaveController.GetPages(var Items: TVkFavePages; Params: TVkParamsFavePagesGet): Boolean;
begin
  with Handler.Execute('fave.getPages', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkFavePages.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFaveController.GetTags(var Items: TVkFaveTags): Boolean;
begin
  with Handler.Execute('fave.getTags') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkFaveTags.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TFaveController.MarkSeen: Boolean;
begin
  with Handler.Execute('fave.markSeen') do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.RemoveArticle(OwnerId, ArticleId: Integer; Ref: string): Boolean;
begin
  with Handler.Execute('fave.removeArticle', [['owner_id', OwnerId.ToString], ['article_id', ArticleId.ToString], ['ref',
    Ref]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.RemoveLink(LinkId: Integer): Boolean;
begin
  with Handler.Execute('fave.removeLink', ['link_id', LinkId.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.RemovePage(UserId, GroupId: Integer): Boolean;
begin
  with Handler.Execute('fave.removePage', [['user_id', UserId.ToString], ['group_id', GroupId.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.RemovePost(OwnerId, Id: Integer): Boolean;
begin
  with Handler.Execute('fave.removePost', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.RemoveProduct(OwnerId, Id: Integer): Boolean;
begin
  with Handler.Execute('fave.removeProduct', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.RemoveTag(Id: Integer): Boolean;
begin
  with Handler.Execute('fave.removeTag', ['id', Id.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.RemoveVideo(OwnerId, Id: Integer): Boolean;
begin
  with Handler.Execute('fave.removeVideo', [['owner_id', OwnerId.ToString], ['id', Id.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.ReorderTags(Ids: TArrayOfInteger): Boolean;
begin
  with Handler.Execute('fave.reorderTags', ['ids', Ids.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.SetGroupPageTags(GroupId: Integer; Tags: TArrayOfInteger): Boolean;
begin
  with Handler.Execute('fave.setPageTags', [['group_id', GroupId.ToString], ['tag_ids', Tags.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.SetPageTags(Params: TVkParamsFavePageTagsSet): Boolean;
begin
  with Handler.Execute('fave.setPageTags', Params.List) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.SetTags(Params: TVkParamsFaveTagsSet): Boolean;
begin
  with Handler.Execute('fave.setTags', Params.List) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.SetUserPageTags(UserId: Integer; Tags: TArrayOfInteger): Boolean;
begin
  with Handler.Execute('fave.setPageTags', [['user_id', UserId.ToString], ['tag_ids', Tags.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.TrackPageInteractionGroup(GroupId: Integer): Boolean;
begin
  with Handler.Execute('fave.trackPageInteraction', ['group_id', GroupId.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TFaveController.TrackPageInteractionUser(UserId: Integer): Boolean;
begin
  with Handler.Execute('fave.trackPageInteraction', ['user_id', UserId.ToString]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

{ TVkGetFaveParams }

function TVkParamsFaveGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFaveGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsFaveGet.Fields(Value: TArrayOfString): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsFaveGet.IsFromSnackbar(Value: Boolean): Integer;
begin
  Result := List.Add('is_from_snackbar', Value);
end;

function TVkParamsFaveGet.ItemType(Value: TVkFaveTypeGet): Integer;
begin
  Result := List.Add('item_type', Value.ToString);
end;

function TVkParamsFaveGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFaveGet.TagId(Value: Integer): Integer;
begin
  Result := List.Add('tag_id', Value);
end;

{ TVkFaveTypeGetHelper }

function TVkFaveTypeGetHelper.ToString: string;
begin
  case Self of
    ftPost:
      Result := 'post';
    ftVideo:
      Result := 'video';
    ftProduct:
      Result := 'product';
    ftArticle:
      Result := 'article';
    ftLink:
      Result := 'link';
  end;
end;

{ TVkParamsFavePageTagsSet }

function TVkParamsFavePageTagsSet.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsFavePageTagsSet.TagIds(Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('tag_ids', Value);
end;

function TVkParamsFavePageTagsSet.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsFaveTagsSet }

function TVkParamsFaveTagsSet.ItemId(Value: Integer): Integer;
begin
  Result := List.Add('item_id', Value);
end;

function TVkParamsFaveTagsSet.ItemOwnerId(Value: Integer): Integer;
begin
  Result := List.Add('item_owner_id', Value);
end;

function TVkParamsFaveTagsSet.ItemType(Value: TVkFaveTypeGet): Integer;
begin
  Result := List.Add('item_type', Value.ToString);
end;

function TVkParamsFaveTagsSet.LinkId(Value: string): Integer;
begin
  Result := List.Add('link_id', Value);
end;

function TVkParamsFaveTagsSet.LinkUrl(Value: string): Integer;
begin
  Result := List.Add('link_url', Value);
end;

function TVkParamsFaveTagsSet.TagIds(Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('tag_ids', Value);
end;

{ TVkParamsFavePagesGet }

function TVkParamsFavePagesGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsFavePagesGet.Fields(GroupFields: TVkGroupFields; UserFields: TVkUserFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsFavePagesGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsFavePagesGet.TagId(Value: Integer): Integer;
begin
  Result := List.Add('tag_id', Value);
end;

function TVkParamsFavePagesGet.&Type(Value: TVkFavePageType): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

{ TVkFavePageTypeHelper }

function TVkFavePageTypeHelper.ToString: string;
begin
  case Self of
    ftUsers:
      Result := 'users';
    ftGroups:
      Result := 'groups';
    ftHints:
      Result := 'hints';
  else
    Result := '';
  end;
end;

{ TVkTagPositionHelper }

function TVkTagPositionHelper.ToString: string;
begin
  case Self of
    tpFront:
      Result := 'front';
    tpBack:
      Result := 'back';
  else
    Result := '';
  end;
end;

{ TVkParamsFavePostAdd }

function TVkParamsFavePostAdd.Ref(Value: string): Integer;
begin
  Result := List.Add('ref', Value);
end;

function TVkParamsFavePostAdd.Source(Value: string): Integer;
begin
  Result := List.Add('source', Value);
end;

function TVkParamsFavePostAdd.TrackCode(Value: string): Integer;
begin
  Result := List.Add('track_code', Value);
end;

end.

