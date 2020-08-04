unit VK.Wall;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON, VK.Entity.Media,
  VK.Entity.CommentInfo;

type
  TVkParamsWallPost = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function FriendsOnly(Value: Boolean): Integer;
    function FromGroup(Value: Boolean): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer; overload;
    function Attachments(Value: TAttachment): Integer; overload;
    function Services(Value: TArrayOfString): Integer;
    function Signed(Value: Boolean): Integer;
    function PublishDate(Value: TDateTime): Integer;
    function LatLong(Lat, Long: Extended): Integer;
    function PlaceId(Value: Integer): Integer;
    function PostId(Value: Integer): Integer;
    function Guid(Value: string): Integer;
    function MarkAsAds(Value: Boolean): Integer;
    function CloseComments(Value: Boolean): Integer;
    function MuteNotifications(Value: Boolean): Integer;
    function Copyright(Value: string): Integer;
  end;

  TVkParamsWallEdit = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PostId(Value: Integer): Integer;
    function FriendsOnly(Value: Boolean): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer; overload;
    function Attachments(Value: TAttachment): Integer; overload;
    function Services(Value: TArrayOfString): Integer;
    function Signed(Value: Boolean): Integer;
    function PublishDate(Value: TDateTime): Integer;
    function LatLong(Lat, Long: Extended): Integer;
    function PlaceId(Value: Integer): Integer;
    function MarkAsAds(Value: Boolean): Integer;
    function CloseComments(Value: Boolean): Integer;
    function PosterBkgId(Value: Integer): Integer;
    function PosterBkgOwnerId(Value: Integer): Integer;
    function PosterBkgAccessHash(Value: string): Integer;
    function Copyright(Value: string): Integer;
  end;

  TVkParamsWallEditAdsStealth = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PostId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer; overload;
    function Attachments(Value: TAttachment): Integer; overload;
    function Signed(Value: Boolean): Integer;
    function LatLong(Lat, Long: Extended): Integer;
    function PlaceId(Value: Integer): Integer;
    function LinkButton(Value: string): Integer;
    function LinkTitle(Value: string): Integer;
    function LinkImage(Value: string): Integer;
    function LinkVideo(Value: string): Integer;
  end;

  TVkParamsWallEditComment = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function CommentId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer; overload;
    function Attachments(Value: TAttachment): Integer; overload;
  end;

  TVkParamsWallGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Domain(Value: string): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Filter(Value: TVkPostType): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(UserFields: TVkUserFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsWallGetById = record
    List: TParams;
    function Posts(Value: TArrayOfString): Integer;
    function Extended(Value: Boolean): Integer;
    function CopyHistoryDepth(Value: Integer = 2): Integer;
    function Fields(UserFields: TVkUserFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsWallGetComment = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function CommentId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(UserFields: TVkUserFields = []; GroupFields: TVkGroupFields = []): Integer;
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

  TVkParamsWallGetComments = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PostId(Value: Integer): Integer;
    function NeedLikes(Value: Boolean): Integer;
    function StartCommentId(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Sort(Value: TVkSort): Integer;
    function PreviewLength(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(UserFields: TVkUserFields = []; GroupFields: TVkGroupFields = []): Integer;
    function CommentId(Value: Integer): Integer;
    function ThreadItemsCount(Value: Integer): Integer;
  end;

  TVkParamsWallPostAdsStealth = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
    function Signed(Value: Boolean): Integer;
    function LatLong(Lat, Long: Extended): Integer;
    function PlaceId(Value: Integer): Integer;
    function Guid(Value: string): Integer;
    function LinkButton(Value: string): Integer; overload;
    function LinkButton(Value: TVkPostLinkButton): Integer; overload;
    function LinkTitle(Value: string): Integer;
    function LinkImage(Value: string): Integer;
    function LinkVideo(Value: string): Integer;
  end;

  TVkParamsWallRepost = record
    List: TParams;
    function &Object(Value: string): Integer;
    function Message(Value: string): Integer;
    function GroupId(Value: Integer): Integer;
    function MarkAsAds(Value: Boolean): Integer;
    function MuteNotifications(Value: Boolean): Integer;
  end;

  TVkParamsWallSearch = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Domain(Value: string): Integer;
    function Query(Value: string): Integer;
    function OwnersOnly(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(UserFields: TVkUserFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TWallController = class(TVkController)
  public
    /// <summary>
    /// Проверяет ссылку для указания источника.
    /// </summary>
    function СheckCopyrightLink(const Link: string): Boolean;
    /// <summary>
    /// Выключает комментирование записи.
    /// </summary>
    function CloseComments(const OwnerId, PostId: Integer): Boolean;
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
    function CreateComment(const PostId: Integer; const Message: string; OwnerId: Integer = 0; Attachments:
      TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Удаляет запись со стены.
    /// </summary>
    function Delete(const PostId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет комментарий к записи на стене.
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Редактирует запись на стене.
    /// </summary>
    function Edit(var PostId: Integer; Params: TVkParamsWallEdit): Boolean; overload;
    /// <summary>
    /// Позволяет отредактировать скрытую запись.
    /// Создание скрытых записей возможно только в сообществах от имени группы, публичной страницы или мероприятия; пользователь должен обладать правами администратора или редактора.
    /// </summary>
    function EditAdsStealth(Params: TVkParamsWallEditAdsStealth): Boolean; overload;
    /// <summary>
    /// Редактирует комментарий на стене.
    /// </summary>
    function EditComment(Params: TVkParamsWallEditComment): Boolean; overload;
    /// <summary>
    /// Возвращает список записей со стены пользователя или сообщества.
    /// 5000 вызовов в сутки.
    /// </summary>
    function Get(var Items: TVkPosts; Params: TVkParamsWallGet): Boolean; overload;
    /// <summary>
    /// Возвращает список записей со стены пользователя или сообщества.
    /// 5000 вызовов в сутки.
    /// </summary>
    function Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список записей со стен пользователей или сообществ по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkPosts; Params: TVkParamsWallGetById): Boolean; overload;
    /// <summary>
    /// Получает информацию о комментарии на стене.
    /// </summary>
    function GetComment(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает информацию о комментарии на стене.
    /// </summary>
    function GetComment(var Items: TVkComments; Params: TVkParamsWallGetComment): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к записи на стене.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к записи на стене.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsWallGetComments): Boolean; overload;
    /// <summary>
    /// Позволяет получать список репостов заданной записи.
    /// Обратите внимание, получить список репостов можно только для записи, созданной текущим пользователем, или в сообществе, где текущий пользователь является администратором.
    /// </summary>
    function GetReposts(var Items: TVkPosts; PostId: Integer; Offset: Integer = 0; Count: Integer = 0; OwnerId: Integer
      = 0): Boolean; overload;
    /// <summary>
    /// Включает комментирование записи
    /// Работает только с конкретными записями, комментирование которых было выключено с помощью wall.closeComments
    /// </summary>
    function OpenComments(const OwnerId, PostId: Integer): Boolean; overload;
    /// <summary>
    /// Закрепляет запись на стене (запись будет отображаться выше остальных).
    /// </summary>
    function Pin(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(var PostId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(const Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(var PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(const Message: string; OwnerId: Integer; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(var PostId: Integer; Params: TVkParamsWallPost): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(Params: TVkParamsWallPost): Boolean; overload;
    /// <summary>
    /// Позволяет создать скрытую запись, которая не попадает на стену сообщества и в дальнейшем может быть использована для создания рекламного объявления типа "Запись в сообществе".
    /// Создание скрытых записей возможно только в сообществах от имени группы, публичной страницы или мероприятия; пользователь должен обладать правами администратора или редактора. Обратите внимание — в сутки можно создать не более 100 скрытых записей.
    /// </summary>
    function PostAdsStealth(var PostId: Integer; Params: TVkParamsWallPostAdsStealth): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к записи.
    /// </summary>
    function ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason = prSpam): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на запись.
    /// </summary>
    function ReportPost(const OwnerId, PostId: Integer; Reason: TVkMediaReportReason = prSpam): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к записи.
    /// </summary>
    function Repost(var Info: TVkRepostInfo; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к записи.
    /// </summary>
    function Repost(var Info: TVkRepostInfo; Params: TVkParamsWallRepost): Boolean; overload;
    /// <summary>
    /// Восстанавливает удаленную запись на стене пользователя или сообщества.
    /// </summary>
    function Restore(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Восстанавливает удаленный комментарий к записи на стене.
    /// </summary>
    function RestoreComment(const CommentId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Позволяет искать записи на стене в соответствии с заданными критериями.
    /// 1000 вызовов в сутки
    /// </summary>
    function Search(var Items: TVkPosts; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет искать записи на стене в соответствии с заданными критериями.
    /// 1000 вызовов в сутки
    /// </summary>
    function Search(var Items: TVkPosts; Params: TVkParamsWallSearch): Boolean; overload;
    /// <summary>
    /// Отменяет закрепление записи на стене.
    /// </summary>
    function Unpin(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TWallController }

function TWallController.Post(var PostId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean;
var
  Params: TVkParamsWallPost;
begin
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  if not Message.IsEmpty then
    Params.Message(Message);
  Result := Post(PostId, Params);
end;

function TWallController.Post(const Message: string; Attachments: TAttachmentArray): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Message, Attachments);
end;

function TWallController.Post(const Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Message, OwnerId, Attachments);
end;

function TWallController.Post(Params: TVkParamsWallPost): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Params);
end;

function TWallController.PostAdsStealth(var PostId: Integer; Params: TVkParamsWallPostAdsStealth): Boolean;
begin
  with Handler.Execute('wall.postAdsStealth', Params.List) do
    Result := Success and GetValue('post_id', PostId);
end;

function TWallController.ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('owner_id', OwnerId);
  Params.Add('reason', Reason.ToString);
  with Handler.Execute('wall.reportComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.ReportPost(const OwnerId, PostId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  Params.Add('owner_id', OwnerId);
  Params.Add('reason', Reason.ToString);
  with Handler.Execute('wall.reportPost', Params) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.Repost(var Info: TVkRepostInfo; Params: TVkParamsWallRepost): Boolean;
begin
  Result := Repost(Info, Params.List);
end;

function TWallController.Restore(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  Params.Add('owner_id', OwnerId);
  with Handler.Execute('wall.restore', Params) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.RestoreComment(const CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('owner_id', OwnerId);
  with Handler.Execute('wall.restoreComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.Search(var Items: TVkPosts; Params: TVkParamsWallSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TWallController.Unpin(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('wall.unpin', Params) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.Search(var Items: TVkPosts; Params: TParams): Boolean;
begin
  with Handler.Execute('wall.search', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPosts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TWallController.Repost(var Info: TVkRepostInfo; Params: TParams): Boolean;
begin
  with Handler.Execute('wall.repost', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkRepostInfo.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TWallController.Post(var PostId: Integer; Params: TVkParamsWallPost): Boolean;
begin
  with Handler.Execute('wall.post', Params.List) do
    Result := Success and GetValue('post_id', PostId);
end;

function TWallController.Post(var PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsWallPost;
begin
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  if not Message.IsEmpty then
    Params.Message(Message);
  Params.OwnerId(OwnerId);
  Result := Post(PostId, Params);
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

function TWallController.CloseComments(const OwnerId, PostId: Integer): Boolean;
begin
  with Handler.Execute('wall.closeComments', [['owner_id', OwnerId.ToString], ['post_id', PostId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.CreateComment(const PostId: Integer; const Message: string; OwnerId: Integer; Attachments:
  TAttachmentArray): Boolean;
var
  CommentInfo: TVkCommentInfo;
  Params: TVkCommentCreateParams;
begin
  Params.PostId(PostId);
  Params.Message(Message);
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  Result := CreateComment(CommentInfo, Params);
  if Result then
    CommentInfo.Free;
end;

function TWallController.Delete(const PostId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('wall.delete', Params) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.DeleteComment(const CommentId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('wall.deleteComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.Edit(var PostId: Integer; Params: TVkParamsWallEdit): Boolean;
begin
  with Handler.Execute('wall.edit', Params.List) do
    Result := Success and ResponseAsInt(PostId);
end;

function TWallController.EditAdsStealth(Params: TVkParamsWallEditAdsStealth): Boolean;
begin
  with Handler.Execute('wall.editAdsStealth', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.EditComment(Params: TVkParamsWallEditComment): Boolean;
begin
  with Handler.Execute('wall.editComment', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: Integer): Boolean;
var
  Params: TVkParamsWallGet;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Params.Offset(Offset);
  Params.Count(Count);
  Result := Get(Items, Params);
end;

function TWallController.GetById(var Items: TVkPosts; Params: TVkParamsWallGetById): Boolean;
begin
  with Handler.Execute('wall.getById', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPosts.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TWallController.GetComment(var Items: TVkComments; Params: TVkParamsWallGetComment): Boolean;
begin
  Result := GetComment(Items, Params.List);
end;

function TWallController.GetComments(var Items: TVkComments; Params: TVkParamsWallGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TWallController.GetReposts(var Items: TVkPosts; PostId, Offset, Count, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if Offset <> 0 then
    Params.Add('offset', Offset);
  if Count <> 0 then
    Params.Add('count', Count);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('wall.getReposts', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPosts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TWallController.OpenComments(const OwnerId, PostId: Integer): Boolean;
begin
  with Handler.Execute('wall.openComments', [['owner_id', OwnerId.ToString], ['post_id', PostId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  with Handler.Execute('wall.getComments', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkComments.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TWallController.GetComment(var Items: TVkComments; Params: TParams): Boolean;
begin
  with Handler.Execute('wall.getComment', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkComments.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TWallController.Get(var Items: TVkPosts; Params: TVkParamsWallGet): Boolean;
begin
  with Handler.Execute('wall.get', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPosts.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TWallController.Pin(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('wall.pin', Params) do
    Result := Success and ResponseIsTrue;
end;

function TWallController.СheckCopyrightLink(const Link: string): Boolean;
begin
  with Handler.Execute('wall.checkCopyrightLink', ['link', Link]) do
    Result := Success and ResponseIsTrue;
end;

{ TVkWallParams }

function TVkParamsWallPost.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPost.Attachments(Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPost.CloseComments(Value: Boolean): Integer;
begin
  Result := List.Add('close_comments', Value);
end;

function TVkParamsWallPost.Copyright(Value: string): Integer;
begin
  Result := List.Add('copyright', Value);
end;

function TVkParamsWallPost.FriendsOnly(Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
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
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallPost.MarkAsAds(Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallPost.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallPost.MuteNotifications(Value: Boolean): Integer;
begin
  Result := List.Add('mute_notifications', Value);
end;

function TVkParamsWallPost.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallPost.PlaceId(Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallPost.PostId(Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallPost.PublishDate(Value: TDateTime): Integer;
begin
  Result := List.Add('publish_date', Value);
end;

function TVkParamsWallPost.Services(Value: TArrayOfString): Integer;
begin
  Result := List.Add('services', Value);
end;

function TVkParamsWallPost.Signed(Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

{ TVkCommentCreateParams }

function TVkCommentCreateParams.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
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

function TVkParamsWallGet.Fields(UserFields: TVkUserFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGet.Filter(Value: TVkPostType): Integer;
begin
  Result := List.Add('value', Value.ToString);
end;

function TVkParamsWallGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsWallEdit }

function TVkParamsWallEdit.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEdit.PostId(Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallEdit.FriendsOnly(Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkParamsWallEdit.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEdit.Services(Value: TArrayOfString): Integer;
begin
  Result := List.Add('services', Value);
end;

function TVkParamsWallEdit.Signed(Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallEdit.PublishDate(Value: TDateTime): Integer;
begin
  Result := List.Add('publish_date', Value);
end;

function TVkParamsWallEdit.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallEdit.PlaceId(Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallEdit.MarkAsAds(Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallEdit.Attachments(Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEdit.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEdit.CloseComments(Value: Boolean): Integer;
begin
  Result := List.Add('close_comments', Value);
end;

function TVkParamsWallEdit.PosterBkgId(Value: Integer): Integer;
begin
  Result := List.Add('poster_bkg_id', Value);
end;

function TVkParamsWallEdit.PosterBkgOwnerId(Value: Integer): Integer;
begin
  Result := List.Add('poster_bkg_owner_id', Value);
end;

function TVkParamsWallEdit.PosterBkgAccessHash(Value: string): Integer;
begin
  Result := List.Add('poster_bkg_access_hash', Value);
end;

function TVkParamsWallEdit.Copyright(Value: string): Integer;
begin
  Result := List.Add('copyright', Value);
end;

{ TVkParamsWallEditAdsStealth }

function TVkParamsWallEditAdsStealth.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEditAdsStealth.PostId(Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallEditAdsStealth.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEditAdsStealth.Signed(Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallEditAdsStealth.Attachments(Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditAdsStealth.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditAdsStealth.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallEditAdsStealth.PlaceId(Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallEditAdsStealth.LinkButton(Value: string): Integer;
begin
  Result := List.Add('link_button', Value);
end;

function TVkParamsWallEditAdsStealth.LinkTitle(Value: string): Integer;
begin
  Result := List.Add('link_title', Value);
end;

function TVkParamsWallEditAdsStealth.LinkImage(Value: string): Integer;
begin
  Result := List.Add('link_image', Value);
end;

function TVkParamsWallEditAdsStealth.LinkVideo(Value: string): Integer;
begin
  Result := List.Add('link_video', Value);
end;

{ TVkParamsWallEditComment }

function TVkParamsWallEditComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEditComment.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallEditComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEditComment.Attachments(Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

{ TVkParamsWallGetById }

function TVkParamsWallGetById.CopyHistoryDepth(Value: Integer): Integer;
begin
  Result := List.Add('copy_history_depth', Value);
end;

function TVkParamsWallGetById.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetById.Fields(UserFields: TVkUserFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetById.Posts(Value: TArrayOfString): Integer;
begin
  Result := List.Add('posts', Value);
end;

{ TVkParamsWallGetComment }

function TVkParamsWallGetComment.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallGetComment.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetComment.Fields(UserFields: TVkUserFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsWallGetComments }

function TVkParamsWallGetComments.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallGetComments.PostId(Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallGetComments.NeedLikes(Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsWallGetComments.StartCommentId(Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsWallGetComments.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallGetComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallGetComments.Sort(Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsWallGetComments.PreviewLength(Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

function TVkParamsWallGetComments.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetComments.Fields(UserFields: TVkUserFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetComments.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallGetComments.ThreadItemsCount(Value: Integer): Integer;
begin
  Result := List.Add('thread_items_count', Value);
end;

{ TVkParamsWallPostAdsStealth }

function TVkParamsWallPostAdsStealth.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallPostAdsStealth.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallPostAdsStealth.Attachments(Value: TArrayOfString): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPostAdsStealth.Signed(Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallPostAdsStealth.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallPostAdsStealth.PlaceId(Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallPostAdsStealth.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsWallPostAdsStealth.LinkButton(Value: string): Integer;
begin
  Result := List.Add('link_button', Value);
end;

function TVkParamsWallPostAdsStealth.LinkTitle(Value: string): Integer;
begin
  Result := List.Add('link_title', Value);
end;

function TVkParamsWallPostAdsStealth.LinkButton(Value: TVkPostLinkButton): Integer;
begin
  Result := List.Add('link_button', Value.ToString);
end;

function TVkParamsWallPostAdsStealth.LinkImage(Value: string): Integer;
begin
  Result := List.Add('link_image', Value);
end;

function TVkParamsWallPostAdsStealth.LinkVideo(Value: string): Integer;
begin
  Result := List.Add('link_video', Value);
end;

{ TVkParamsWallRepost }

function TVkParamsWallRepost.&Object(Value: string): Integer;
begin
  Result := List.Add('object', Value);
end;

function TVkParamsWallRepost.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallRepost.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsWallRepost.MarkAsAds(Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallRepost.MuteNotifications(Value: Boolean): Integer;
begin
  Result := List.Add('mute_notifications', Value);
end;

{ TVkParamsWallSearch }

function TVkParamsWallSearch.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallSearch.Domain(Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsWallSearch.Query(Value: string): Integer;
begin
  Result := List.Add('query', Value);
end;

function TVkParamsWallSearch.OwnersOnly(Value: Boolean): Integer;
begin
  Result := List.Add('owners_only', Value);
end;

function TVkParamsWallSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallSearch.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallSearch.Fields(UserFields: TVkUserFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

end.

