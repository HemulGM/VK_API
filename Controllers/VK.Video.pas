unit VK.Video;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Video, System.JSON, VK.Entity.Status, VK.Entity.Media,
  VK.Entity.Video.Save;

type
  TVkVideosFilter = (vfMP4, vfYouTube, vfVimeo, vfShort, vfLong);

  TVkVideosFilterHelper = record helper for TVkVideosFilter
    function ToString: string; inline;
  end;

  TVkVideosFilters = set of TVkVideosFilter;

  TVkVideosFiltersHelper = record helper for TVkVideosFilters
    function ToString: string; inline;
    class function All: TVkVideosFilters; static; inline;
  end;

  TVkParamsVideoGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Videos(Value: TArrayOfString): Integer;
  end;

  TVkParamsVideoGetAlbums = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function NeedSystem(Value: Boolean): Integer;
  end;

  TVkParamsVideoAddToAlbum = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function TargetId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function AlbumIds(Value: TIds): Integer;
    function VideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoCreateComment = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function VideoId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
    function FromGroup(Value: Integer): Integer;
    function ReplyToComment(Value: Integer): Integer;
    function StickerId(Value: Integer): Integer;
    function Guid(Value: string): Integer;
  end;

  TVkParamsVideoEdit = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function VideoId(Value: Integer): Integer;
    function Name(Value: string): Integer;
    function Desc(Value: string): Integer;
    function PrivacyView(Value: TArrayOfString): Integer;
    function PrivacyComment(Value: TArrayOfString): Integer;
    function NoComments(Value: Boolean): Integer;
    function &Repeat(Value: Boolean): Integer;
  end;

  TVkParamsVideoEditAlbum = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Title(Value: string): Integer;
    function Privacy(Value: TArrayOfString): Integer;
  end;

  TVkParamsVideoEditComment = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function CommentId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TIds): Integer;
  end;

  TVkParamsVideoGetComments = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function VideoId(Value: Integer): Integer;
    function NeedLikes(Value: Boolean): Integer;
    function StartCommentId(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Sort(Value: TVkSort): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsVideoRemoveFromAlbum = record
    List: TParams;
    function TargetId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function AlbumIds(Value: TIds): Integer;
    function OwnerId(Value: Integer): Integer;
    function VideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoReorderAlbums = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Before(Value: Integer): Integer;
    function After(Value: Integer): Integer;
  end;

  TVkParamsVideoReorderVideos = record
    List: TParams;
    function TargetId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function OwnerId(Value: Integer): Integer;
    function VideoId(Value: Integer): Integer;
    function BeforeOwnerId(Value: Integer): Integer;
    function BeforeVideoId(Value: Integer): Integer;
    function AfterOwnerId(Value: Integer): Integer;
    function AfterVideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoReport = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function VideoId(Value: Integer): Integer;
    function Reason(Value: TVkMediaReportReason): Integer;
    function Comment(Value: string): Integer;
    function SearchQuery(Value: string): Integer;
  end;

  TVkParamsVideoSave = record
    List: TParams;
    function Name(Value: string): Integer;
    function Description(Value: string): Integer;
    function IsPrivate(Value: Boolean): Integer;
    function Wallpost(Value: Boolean): Integer;
    function Link(Value: string): Integer;
    function GroupId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function PrivacyView(Value: TArrayOfString): Integer;
    function PrivacyComment(Value: TArrayOfString): Integer;
    function NoComments(Value: Boolean): Integer;
    function &Repeat(Value: Boolean): Integer;
    function Compression(Value: Boolean): Integer;
  end;

  TVkParamsVideoSearch = record
    List: TParams;
    function Query(Value: string): Integer;
    function Sort(Value: Integer): Integer;
    function Hd(Value: Integer): Integer;
    function Adult(Value: Boolean): Integer;
    function Filters(Value: TVkVideosFilters): Integer;
    function SearchOwn(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Longer(Value: Integer): Integer;
    function Shorter(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
  end;

  TVideoController = class(TVkController)
  public
    /// <summary>
    /// Добавляет видеозапись в список пользователя.
    /// </summary>
    function Add(const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean;
    /// <summary>
    /// Создает пустой альбом видеозаписей.
    /// </summary>
    function AddAlbum(var AlbumId: Integer; Title: string; Privacy: TArrayOfString = []; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Позволяет добавить видеозапись в альбом.
    /// </summary>
    function AddToAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет добавить видеозапись в альбом.
    /// </summary>
    function AddToAlbum(Params: TVkParamsVideoAddToAlbum): Boolean; overload;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TVkParamsVideoCreateComment): Boolean; overload;
    /// <summary>
    /// Удаляет видеозапись со страницы пользователя.
    /// </summary>
    function Delete(const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет альбом видеозаписей.
    /// </summary>
    function DeleteAlbum(const AlbumId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет комментарий к видеозаписи.
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function Edit(Params: TVkParamsVideoEdit): Boolean; overload;
    /// <summary>
    /// Редактирует альбом с видео.
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует альбом с видео.
    /// </summary>
    function EditAlbum(Params: TVkParamsVideoEditAlbum): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к видеозаписи.
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к видеозаписи.
    /// </summary>
    function EditComment(Params: TVkParamsVideoEditComment): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о видеозаписях.
    /// </summary>
    function Get(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о видеозаписях.
    /// </summary>
    function Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео.
    /// </summary>
    function GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео.
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео.
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean; overload;
    /// <summary>
    /// Возвращает список альбомов, в которых находится видеозапись.
    /// </summary>
    function GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к видеозаписи.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к видеозаписи.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsVideoGetComments): Boolean; overload;
    /// <summary>
    /// Позволяет убрать видеозапись из альбома.
    /// </summary>
    function RemoveFromAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет убрать видеозапись из альбома.
    /// </summary>
    function RemoveFromAlbum(Params: TVkParamsVideoRemoveFromAlbum): Boolean; overload;
    /// <summary>
    /// Позволяет изменить порядок альбомов с видео.
    /// </summary>
    function ReorderAlbums(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет изменить порядок альбомов с видео.
    /// </summary>
    function ReorderAlbums(Params: TVkParamsVideoReorderAlbums): Boolean; overload;
    /// <summary>
    /// Позволяет переместить видеозапись в альбоме.
    /// </summary>
    function ReorderVideos(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет переместить видеозапись в альбоме.
    /// </summary>
    function ReorderVideos(Params: TVkParamsVideoReorderVideos): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на видеозапись.
    /// </summary>
    function Report(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на видеозапись.
    /// </summary>
    function Report(Params: TVkParamsVideoReport): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к видеозаписи.
    /// </summary>
    function ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean; overload;
    /// <summary>
    /// Восстанавливает удаленную видеозапись.
    /// </summary>
    function Restore(VideoId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Восстанавливает удаленный комментарий к видеозаписи.
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TVkParamsVideoSave): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean; overload;
    /// <summary>
    /// Возвращает список видеозаписей в соответствии с заданным критерием поиска.
    /// </summary>
    function Search(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список видеозаписей в соответствии с заданным критерием поиска.
    /// </summary>
    function Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TVideoController }

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean;
begin
  with Handler.Execute('video.getAlbums', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkVideoAlbums.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean;
begin
  Result := GetAlbums(Items, Params.List);
end;

function TVideoController.GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Params.Add('extended', True);
  with Handler.Execute('video.getAlbumsByVideo', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkVideoAlbums.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TVideoController.GetComments(var Items: TVkComments; Params: TVkParamsVideoGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TVideoController.RemoveFromAlbum(Params: TVkParamsVideoRemoveFromAlbum): Boolean;
begin
  Result := RemoveFromAlbum(Params.List);
end;

function TVideoController.ReorderAlbums(Params: TVkParamsVideoReorderAlbums): Boolean;
begin
  Result := ReorderAlbums(Params.List);
end;

function TVideoController.ReorderVideos(Params: TVkParamsVideoReorderVideos): Boolean;
begin
  Result := ReorderVideos(Params.List);
end;

function TVideoController.Report(Params: TVkParamsVideoReport): Boolean;
begin
  Result := Report(Params.List);
end;

function TVideoController.ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  Params.Add('reason', Reason.ToConst.ToString);
  with Handler.Execute('video.reportComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.Restore(VideoId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('video.restore', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.RestoreComment(CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('video.restoreComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Params: TVkParamsVideoSave): Boolean;
begin
  Result := Save(VideoSaved, Params.List);
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Params: TParams): Boolean;
begin
  with Handler.Execute('video.save', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        VideoSaved := TVkVideoSaved.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TVideoController.Report(Params: TParams): Boolean;
begin
  with Handler.Execute('video.report', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.ReorderVideos(Params: TParams): Boolean;
begin
  with Handler.Execute('video.reorderVideos', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.ReorderAlbums(Params: TParams): Boolean;
begin
  with Handler.Execute('video.reorderAlbums', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.RemoveFromAlbum(Params: TParams): Boolean;
begin
  with Handler.Execute('video.removeFromAlbum', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  with Handler.Execute('video.getComments', Params) do
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

function TVideoController.GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  Params.Add('owner_id', OwnerId);
  with Handler.Execute('video.getAlbumById', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkVideoAlbum.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TVideoController.Get(var Items: TVkVideos; Params: TParams): Boolean;
begin
  with Handler.Execute('video.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkVideos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TVideoController.Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TVideoController.Add(const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  with Handler.Execute('video.add', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.AddAlbum(var AlbumId: Integer; Title: string; Privacy: TArrayOfString; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('title', Title);
  if Length(Privacy) > 0 then
    Params.Add('privacy', Privacy);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('video.addAlbum', Params) do
    Result := Success and TryStrToInt(Response, AlbumId);
end;

function TVideoController.AddToAlbum(Params: TVkParamsVideoAddToAlbum): Boolean;
begin
  Result := AddToAlbum(Params.List);
end;

function TVideoController.CreateComment(var CommentId: Integer; Params: TVkParamsVideoCreateComment): Boolean;
begin
  Result := CreateComment(CommentId, Params.List);
end;

function TVideoController.Delete(const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  with Handler.Execute('video.delete', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.DeleteAlbum(const AlbumId: Integer; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('video.deleteAlbum', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.DeleteComment(const CommentId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('video.deleteComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.Edit(Params: TVkParamsVideoEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TVideoController.EditAlbum(Params: TVkParamsVideoEditAlbum): Boolean;
begin
  Result := EditAlbum(Params.List);
end;

function TVideoController.EditComment(Params: TVkParamsVideoEditComment): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TVideoController.EditComment(Params: TParams): Boolean;
begin
  with Handler.Execute('video.editComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.EditAlbum(Params: TParams): Boolean;
begin
  with Handler.Execute('video.editAlbum', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.Edit(Params: TParams): Boolean;
begin
  with Handler.Execute('video.edit', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.CreateComment(var CommentId: Integer; Params: TParams): Boolean;
begin
  with Handler.Execute('video.createComment', Params) do
    Result := Success and TryStrToInt(Response, CommentId);
end;

function TVideoController.AddToAlbum(Params: TParams): Boolean;
begin
  with Handler.Execute('video.addToAlbum', Params) do
    Result := Success and ResponseIsTrue;
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean;
var
  Params: TParams;
  SaveResp: string;
begin
  Params.Add('link', Link);
  Result := False;
  with Handler.Execute('video.save', Params) do
  begin
    if Success then
    begin
      try
        VideoSaved := TVkVideoSaved.FromJsonString(Response);
        Result := True;
      except
        Result := False;
      end;
    end;
  end;
  if Result then
  begin
    Result := False;
    if TCustomVK(VK).Upload(VideoSaved.UploadUrl, [''], SaveResp) then
    begin
      Result := not SaveResp.IsEmpty;
    end
    else
      TCustomVK(VK).DoError(Self, TVkException.Create(SaveResp), -1, SaveResp);
  end;
end;

function TVideoController.Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TVideoController.Search(var Items: TVkVideos; Params: TParams): Boolean;
begin
  with Handler.Execute('video.search', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkVideos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkVideosGetParams }

function TVkParamsVideoGet.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoGet.Videos(Value: TArrayOfString): Integer;
begin
  Result := List.Add('videos', Value.ToString);
end;

{ TVkParamsVideoAlbumsGet }

function TVkParamsVideoGetAlbums.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGetAlbums.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGetAlbums.NeedSystem(Value: Boolean): Integer;
begin
  Result := List.Add('need_system', Value);
end;

function TVkParamsVideoGetAlbums.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGetAlbums.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsVideosAddToAlbum }

function TVkParamsVideoAddToAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoAddToAlbum.AlbumIds(Value: TIds): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsVideoAddToAlbum.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoAddToAlbum.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoAddToAlbum.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

{ TVkParamsVideosCreateComment }

function TVkParamsVideoCreateComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoCreateComment.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoCreateComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsVideoCreateComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsVideoCreateComment.FromGroup(Value: Integer): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsVideoCreateComment.ReplyToComment(Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkParamsVideoCreateComment.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsVideoCreateComment.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

{ TVkParamsVideosEdit }

function TVkParamsVideoEdit.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoEdit.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoEdit.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsVideoEdit.Desc(Value: string): Integer;
begin
  Result := List.Add('desc', Value);
end;

function TVkParamsVideoEdit.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsVideoEdit.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsVideoEdit.NoComments(Value: Boolean): Integer;
begin
  Result := List.Add('no_comments', Value);
end;

function TVkParamsVideoEdit.&Repeat(Value: Boolean): Integer;
begin
  Result := List.Add('repeat', Value);
end;

{ TVkParamsVideosEditAlbum }

function TVkParamsVideoEditAlbum.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsVideoEditAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoEditAlbum.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsVideoEditAlbum.Privacy(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy', Value);
end;

{ TVkParamsVideosEditComment }

function TVkParamsVideoEditComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoEditComment.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsVideoEditComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsVideoEditComment.Attachments(Value: TIds): Integer;
begin
  Result := List.Add('attachments', Value);
end;

{ TVkParamsVideoGetComments }

function TVkParamsVideoGetComments.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoGetComments.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoGetComments.NeedLikes(Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsVideoGetComments.StartCommentId(Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsVideoGetComments.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGetComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGetComments.Sort(Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsVideoGetComments.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGetComments.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

{ TVkParamsVideoRemoveFromAlbum }

function TVkParamsVideoRemoveFromAlbum.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.AlbumIds(Value: TIds): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsVideoRemoveFromAlbum.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

{ TVkParamsVideoReorderAlbums }

function TVkParamsVideoReorderAlbums.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReorderAlbums.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoReorderAlbums.Before(Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsVideoReorderAlbums.After(Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

{ TVkParamsVideoReorderVideos }

function TVkParamsVideoReorderVideos.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoReorderVideos.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoReorderVideos.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReorderVideos.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoReorderVideos.BeforeOwnerId(Value: Integer): Integer;
begin
  Result := List.Add('before_owner_id', Value);
end;

function TVkParamsVideoReorderVideos.BeforeVideoId(Value: Integer): Integer;
begin
  Result := List.Add('before_video_id', Value);
end;

function TVkParamsVideoReorderVideos.AfterOwnerId(Value: Integer): Integer;
begin
  Result := List.Add('after_owner_id', Value);
end;

function TVkParamsVideoReorderVideos.AfterVideoId(Value: Integer): Integer;
begin
  Result := List.Add('after_video_id', Value);
end;

{ TVkParamsVideosReport }

function TVkParamsVideoReport.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReport.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoReport.Reason(Value: TVkMediaReportReason): Integer;
begin
  Result := List.Add('reason', Value.ToConst.ToString);
end;

function TVkParamsVideoReport.Comment(Value: string): Integer;
begin
  Result := List.Add('comment', Value);
end;

function TVkParamsVideoReport.SearchQuery(Value: string): Integer;
begin
  Result := List.Add('search_query', Value);
end;

{ TVkParamsVideosSave }

function TVkParamsVideoSave.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsVideoSave.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsVideoSave.IsPrivate(Value: Boolean): Integer;
begin
  Result := List.Add('is_private', Value);
end;

function TVkParamsVideoSave.Wallpost(Value: Boolean): Integer;
begin
  Result := List.Add('wallpost', Value);
end;

function TVkParamsVideoSave.Link(Value: string): Integer;
begin
  Result := List.Add('link', Value);
end;

function TVkParamsVideoSave.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsVideoSave.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoSave.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsVideoSave.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsVideoSave.NoComments(Value: Boolean): Integer;
begin
  Result := List.Add('no_comments', Value);
end;

function TVkParamsVideoSave.&Repeat(Value: Boolean): Integer;
begin
  Result := List.Add('repeat', Value);
end;

function TVkParamsVideoSave.Compression(Value: Boolean): Integer;
begin
  Result := List.Add('compression', Value);
end;

{ TVkVideosFilterHelper }

function TVkVideosFilterHelper.ToString: string;
begin
  case Self of
    vfMP4:
      Result := 'mp4';
    vfYouTube:
      Result := 'youtube';
    vfVimeo:
      Result := 'vimeo';
    vfShort:
      Result := 'short';
    vfLong:
      Result := 'long';
  else
    Result := '';
  end;
end;

{ TVkVideosFiltersHelper }

class function TVkVideosFiltersHelper.All: TVkVideosFilters;
begin
  Result := [vfMP4, vfYouTube, vfVimeo, vfShort, vfLong];
end;

function TVkVideosFiltersHelper.ToString: string;
var
  Item: TVkVideosFilter;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkParamsVideosSearch }

function TVkParamsVideoSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsVideoSearch.Sort(Value: Integer): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsVideoSearch.Hd(Value: Integer): Integer;
begin
  Result := List.Add('hd', Value);
end;

function TVkParamsVideoSearch.Adult(Value: Boolean): Integer;
begin
  Result := List.Add('adult', Value);
end;

function TVkParamsVideoSearch.Filters(Value: TVkVideosFilters): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsVideoSearch.SearchOwn(Value: Boolean): Integer;
begin
  Result := List.Add('search_own', Value);
end;

function TVkParamsVideoSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoSearch.Longer(Value: Integer): Integer;
begin
  Result := List.Add('longer', Value);
end;

function TVkParamsVideoSearch.Shorter(Value: Integer): Integer;
begin
  Result := List.Add('shorter', Value);
end;

function TVkParamsVideoSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoSearch.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

end.

