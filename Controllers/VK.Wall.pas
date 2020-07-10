unit VK.Wall;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON, VK.Entity.Media,
  VK.Entity.CommentInfo;

type
  TVkParamsWallPost = record
    List: TParams;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
    function OwnerId(Value: Integer): Integer;
    function CloseComments(Value: Boolean): Integer;
    function FriendsOnly(Value: Boolean): Integer;
    function FromGroup(Value: Boolean): Integer;
    function Guid(Value: string): Integer;
    function LatLong(Lat, Long: Extended): Integer;
    function MarkAsAds(Value: Boolean): Integer;
    function MuteNotifications(Value: Boolean): Integer;
    function PlaceId(Value: Integer): Integer;
    function PostId(Value: Integer): Integer;
    function PublishDate(Value: TDateTime): Integer;
    function Services(Value: TArrayOfString): Integer;
    function Signed(Value: Boolean): Integer;
  end;

  TVkParamsWallGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Domain(Value: string): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Filter(Value: TVkPostType): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(Value: string): Integer;
  end;

  TVkCommentCreateParams = record
    List: TParams;
    function PostId(Value: Integer): Integer;
    function OwnerId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function FromGroup(Value: Boolean): Integer;
    function Guid(Value: string): Integer;
    function ReplyToComment(Value: Integer): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
    function StickerID(Value: Integer): Integer;
  end;

  TWallController = class(TVkController)
  public
    /// <summary>
    /// Позволяет создать запись на своей стене
    /// </summary>
    /// <param name="PostId">ИД записи</param>
    /// <param name="Message">Текст поста</param>
    /// <param name="Attachments">Вложения</param>
    function Post(var PostId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на своей стене
    /// </summary>
    function Post(Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на своей стене
    /// </summary>
    function Post(var PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на своей стене
    /// </summary>
    function Post(Message: string; OwnerId: Integer; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на своей стене
    /// </summary>
    function Post(var PostId: Integer; Params: TVkParamsWallPost): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на своей стене
    /// </summary>
    function Post(Params: TVkParamsWallPost): Boolean; overload;
    /// <summary>
    /// Добавляет комментарий к записи на стене.
    /// </summary>
    function CreateComment(var CommentInfo: TVkCommentInfo; Params: TVkCommentCreateParams): Boolean; overload;
    /// <summary>
    /// Добавляет комментарий к записи на стене.
    /// </summary>
    function CreateComment(Params: TVkCommentCreateParams): Boolean; overload;
    /// <summary>
    /// Добавляет комментарий к записи на стене.
    /// </summary>
    function CreateComment(PostId: Integer; Message: string; OwnerId: Integer = 0; Attachments: TAttachmentArray = []):
      Boolean; overload;
    function Get(var Posts: TVkPosts; Params: TVkParamsWallGet): Boolean; overload;
    function Get(var Posts: TVkPosts; OwnerId: Integer; Offset, Count: Integer): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TWallController }

function TWallController.Post(var PostId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean;
var
  Params: TParams;
  JSONItem: TJSONValue;
begin
  if not Attachments.IsEmpty then
    Params.Add('attachments', Attachments.ToString);
  if not Message.IsEmpty then
    Params.Add('message', Message);
  with Handler.Execute('wall.post', Params) do
  begin
    Result := Success;
    if Result then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      try
        PostId := JSONItem.GetValue<integer>('post_id', -1);
      finally
        JSONItem.Free;
      end;
    end;
  end;
end;

function TWallController.Post(Message: string; Attachments: TAttachmentArray): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Message, Attachments);
end;

function TWallController.Post(Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Message, OwnerId, Attachments);
end;

function TWallController.Post(var PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  Params: TParams;
  JSONItem: TJSONValue;
begin
  if not Attachments.IsEmpty then
    Params.Add('attachments', Attachments.ToString);
  if not Message.IsEmpty then
    Params.Add('message', Message);
  Params.Add('owner_id', OwnerId);
  with Handler.Execute('wall.post', Params) do
  begin
    Result := Success;
    if Result then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      try
        PostId := JSONItem.GetValue<integer>('post_id', -1);
      finally
        JSONItem.Free;
      end;
    end;
  end;
end;

function TWallController.CreateComment(var CommentInfo: TVkCommentInfo; Params: TVkCommentCreateParams): Boolean;
begin
  with Handler.Execute('wall.createComment', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        CommentInfo := TVkCommentInfo.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TWallController.CreateComment(Params: TVkCommentCreateParams): Boolean;
var
  CommentInfo: TVkCommentInfo;
begin
  Result := CreateComment(CommentInfo, Params);
  if Result then
    CommentInfo.Free;
end;

function TWallController.CreateComment(PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray):
  Boolean;
var
  CommentInfo: TVkCommentInfo;
  Params: TVkCommentCreateParams;
begin
  Params.PostId(PostId);
  Params.Message(Message);
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  if Length(Attachments) > 0 then
    Params.Attachments(Attachments);
  Result := CreateComment(CommentInfo, Params);
  if Result then
    CommentInfo.Free;
end;

function TWallController.Get(var Posts: TVkPosts; OwnerId, Offset, Count: Integer): Boolean;
var
  Params: TVkParamsWallGet;
begin
  Params.OwnerId(OwnerId);
  Params.Offset(Offset);
  Params.Count(Count);
  Result := Get(Posts, Params);
end;

function TWallController.Get(var Posts: TVkPosts; Params: TVkParamsWallGet): Boolean;
begin
  with Handler.Execute('wall.get', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Posts := TVkPosts.FromJsonString(Response);
    end;
  end;
end;

function TWallController.Post(Params: TVkParamsWallPost): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Params);
end;

function TWallController.Post(var PostId: Integer; Params: TVkParamsWallPost): Boolean;
var
  JSONItem: TJSONValue;
begin
  with Handler.Execute('wall.post', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      try
        PostId := JSONItem.GetValue<integer>('post_id', -1);
      finally
        JSONItem.Free;
      end;
    end;
  end;
end;

{ TVkWallParams }

function TVkParamsWallPost.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToString);
end;

function TVkParamsWallPost.CloseComments(Value: Boolean): Integer;
begin
  Result := List.Add('close_comments', BoolToString(Value));
end;

function TVkParamsWallPost.FriendsOnly(Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', BoolToString(Value));
end;

function TVkParamsWallPost.FromGroup(Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsWallPost.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsWallPost.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat.ToString);
  Result := List.Add('long', Long.ToString);
end;

function TVkParamsWallPost.MarkAsAds(Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', BoolToString(Value));
end;

function TVkParamsWallPost.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallPost.MuteNotifications(Value: Boolean): Integer;
begin
  Result := List.Add('mute_notifications', BoolToString(Value));
end;

function TVkParamsWallPost.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value.ToString);
end;

function TVkParamsWallPost.PlaceId(Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value.ToString);
end;

function TVkParamsWallPost.PostId(Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value.ToString);
end;

function TVkParamsWallPost.PublishDate(Value: TDateTime): Integer;
begin
  Result := List.Add('publish_date', DateTimeToUnix(Value));
end;

function TVkParamsWallPost.Services(Value: TArrayOfString): Integer;
begin
  Result := List.Add('services', Value.ToString);
end;

function TVkParamsWallPost.Signed(Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

{ TVkCommentCreateParams }

function TVkCommentCreateParams.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToString);
end;

function TVkCommentCreateParams.FromGroup(Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkCommentCreateParams.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkCommentCreateParams.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkCommentCreateParams.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value.ToString);
end;

function TVkCommentCreateParams.PostId(Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value.ToString);
end;

function TVkCommentCreateParams.ReplyToComment(Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkCommentCreateParams.StickerID(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value.ToString);
end;

{ TVkWallGetParams }

function TVkParamsWallGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallGet.Domain(Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsWallGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGet.Fields(Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsWallGet.Filter(Value: TVkPostType): Integer;
begin
  Result := List.Add('value', Value.ToConst);
end;

function TVkParamsWallGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

end.

