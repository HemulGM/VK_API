unit VK.Photos;

interface

uses
  System.SysUtils, System.Types, System.Generics.Collections, VK.Controller, VK.Types, VK.Entity.Album, System.JSON,
  REST.Json, VK.Entity.Photo.Upload, VK.Entity.Photo, VK.Entity.Media, VK.Entity.Group, VK.Entity.Common;

type
  TVkParamsPhotosGetAll = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function PhotoSizes(Value: Boolean): Integer;
    function NoServiceAlbums(Value: Boolean): Integer;
    function NeedHidden(Value: Boolean): Integer;
    function SkipHidden(Value: Boolean): Integer;
  end;

  TVkParamsPhotosGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer; overload;
    function AlbumId(Value: TVkPhotoSystemAlbum): Integer; overload;
    function PhotoIds(Value: TArrayOfInteger): Integer;
    function Extended(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function PhotoSizes(Value: Boolean): Integer;
    function FeedType(Value: TVkPhotoFeedType): Integer;
    function Feed(Value: Integer): Integer;
    function Uid(Value: Integer): Integer;
    function Rev(Value: Boolean): Integer;
  end;

  TVkParamsAlbumsGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumIds(Value: TArrayOfInteger): Integer; overload;
    function AlbumIds(Value: Integer): Integer; overload;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function PhotoSizes(Value: Boolean): Integer;
    function NeedSystem(Value: Boolean): Integer;
    function NeedCovers(Value: Boolean): Integer;
  end;

  TVkParamsPhotosCreateAlbum = record
    List: TParams;
    function Title(Value: string): Integer;
    function GroupId(Value: Integer): Integer;
    function Description(Value: string): Integer;
    function PrivacyView(Value: TArrayOfString): Integer;
    function PrivacyComment(Value: TArrayOfString): Integer;
    function UploadByAdminsOnly(Value: Boolean): Integer;
    function CommentsDisabled(Value: Boolean): Integer;
  end;

  TVkParamsPhotosEditAlbum = record
    List: TParams;
    function AlbumId(Value: Integer): Integer;
    function Title(Value: string): Integer;
    function Description(Value: string): Integer;
    function OwnerId(Value: Integer): Integer;
    function PrivacyView(Value: TArrayOfString): Integer;
    function PrivacyComment(Value: TArrayOfString): Integer;
    function UploadByAdminsOnly(Value: Boolean): Integer;
    function CommentsDisabled(Value: Boolean): Integer;
  end;

  TVkParamsPhotosCreateComment = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PhotoId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
    function FromGroup(Value: Boolean): Integer;
    function ReplyToComment(Value: Integer): Integer;
    function StickerId(Value: Integer): Integer;
    function AccessKey(Value: string): Integer;
    function Guid(Value: string): Integer;
  end;

  TVkParamsPhotosEdit = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PhotoId(Value: Integer): Integer;
    function Caption(Value: string): Integer;
    function Latitude(Value: Extended): Integer;
    function Longitude(Value: Extended): Integer;
    function PlaceStr(Value: string): Integer;
    function FoursquareId(Value: string): Integer;
    function DeletePlace(Value: Boolean): Integer;
  end;

  TVkParamsPhotosSave = record
    List: TParams;
    function AlbumId(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
    function Server(Value: string): Integer;
    function PhotosList(Value: TArrayOfString): Integer;
    function Hash(Value: string): Integer;
    function Latitude(Value: Extended): Integer;
    function Longitude(Value: Extended): Integer;
    function Caption(Value: string): Integer;
  end;

  TVkParamsPhotosEditComment = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function CommentId(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
  end;

  TVkParamsPhotosGetAlbumsCount = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
  end;

  TVkParamsPhotosGetAllComments = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function NeedLikes(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
  end;

  TVkParamsPhotosGetComments = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PhotoId(Value: Integer): Integer;
    function NeedLikes(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function StartCommentId(Value: Integer): Integer;
    function Sort(Value: TVkSort): Integer;
    function AccessKey(Value: string): Integer;
    function Extended(Value: Boolean): Integer;
    function Fields(Value: TVkUserFields): Integer;
  end;

  TVkParamsPhotosGetMarketUploadServer = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function MainPhoto(Value: Boolean): Integer;
    function Crop(Value: TPoint): Integer;
    function CropWidth(Value: Integer): Integer;
  end;

  TVkParamsPhotosGetUserPhotos = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Sort(Value: Boolean): Integer;
  end;

  TVkParamsPhotosReorderAlbums = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Before(Value: Integer): Integer;
    function After(Value: Integer): Integer;
  end;

  TVkParamsPhotosReorderPhotos = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PhotoId(Value: Integer): Integer;
    function Before(Value: Integer): Integer;
    function After(Value: Integer): Integer;
  end;

  TVkParamsPhotosSaveMarketPhoto = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function Photo(Value: string): Integer;
    function Server(Value: Integer): Integer;
    function Hash(Value: string): Integer;
    function CropData(Value: string): Integer;
    function CropHash(Value: string): Integer;
  end;

  TVkParamsPhotosSaveWallPhoto = record
    List: TParams;
    function UserId(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
    function Photo(Value: string): Integer;
    function Server(Value: string): Integer;
    function Hash(Value: string): Integer;
    function Latitude(Value: Extended): Integer;
    function Longitude(Value: Extended): Integer;
    function Caption(Value: string): Integer;
  end;

  TVkParamsPhotosSearch = record
    List: TParams;
    function Query(Value: string): Integer;
    function Latitude(Value: Extended): Integer;
    function Longitude(Value: Extended): Integer;
    function StartTime(Value: TDateTime): Integer;
    function EndTime(Value: TDateTime): Integer;
    function Sort(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    /// <summary>
    /// Радиус поиска в метрах. (работает очень приближенно, поэтому реальное расстояние до цели может отличаться от заданного). Может принимать значения: 10, 100, 800, 6000, 50000
    /// </summary>
    function Radius(Value: Integer = 5000): Integer;
  end;

  TPhotosController = class(TVkController)
  public
    /// <summary>
    /// Подтверждает отметку на фотографии.
    /// </summary>
    function СonfirmTag(PhotoId, TagId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Подтверждает отметки.
    /// </summary>
    function ConfirmTags(Tags: TIds): Boolean;
    /// <summary>
    /// Позволяет скопировать фотографию в альбом "Сохраненные фотографии"
    /// </summary>
    function Copy(var Id: Integer; OwnerId, PhotoId: Integer; AccessKey: string = ''): Boolean;
    /// <summary>
    /// Создает пустой альбом для фотографий.
    /// </summary>
    function CreateAlbum(var Item: TVkPhotoAlbum; Params: TParams): Boolean; overload;
    /// <summary>
    /// Создает пустой альбом для фотографий.
    /// </summary>
    function CreateAlbum(var Item: TVkPhotoAlbum; Params: TVkParamsPhotosCreateAlbum): Boolean; overload;
    /// <summary>
    /// Создает новый комментарий к фотографии.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Создает новый комментарий к фотографии.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TVkParamsPhotosCreateComment): Boolean; overload;
    /// <summary>
    /// хз че делает)
    /// https://vk.com/dev/photos.declineTags
    /// </summary>
    function DeclineTags: Boolean;
    /// <summary>
    /// Удаление фотографии на сайте.
    /// </summary>
    function Delete(OwnerId, PhotoId: Integer): Boolean;
    /// <summary>
    /// Удаляет указанный альбом для фотографий у текущего пользователя
    /// </summary>
    function DeleteAlbum(AlbumId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет комментарий к фотографии.
    /// </summary>
    function DeleteComment(OwnerId, CommentId: Integer): Boolean;
    /// <summary>
    /// Редактирует описание или геометку у фотографии.
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует описание или геометку у фотографии.
    /// </summary>
    function Edit(Params: TVkParamsPhotosEdit): Boolean; overload;
    /// <summary>
    /// Редактирует данные альбома для фотографий.
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует данные альбома для фотографий.
    /// </summary>
    function EditAlbum(Params: TVkParamsPhotosEditAlbum): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к фотографии.
    /// Обратите внимание, что редактирование комментария доступно только в течение суток после его создания.
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к фотографии.
    /// Обратите внимание, что редактирование комментария доступно только в течение суток после его создания.
    /// </summary>
    function EditComment(Params: TVkParamsPhotosEditComment): Boolean; overload;
    /// <summary>
    /// Возвращает список фотографий в альбоме.
    /// </summary>
    function Get(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список фотографий в альбоме.
    /// </summary>
    function Get(var Items: TVkPhotos; Params: TVkParamsPhotosGet): Boolean; overload;
    /// <summary>
    /// Возвращает список фотоальбомов пользователя или сообщества.
    /// </summary>
    function GetAlbums(var Items: TVkPhotoAlbums; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список фотоальбомов пользователя или сообщества.
    /// </summary>
    function GetAlbums(var Items: TVkPhotoAlbums; Params: TVkParamsAlbumsGet): Boolean; overload;
    /// <summary>
    /// Возвращает количество доступных альбомов пользователя или сообщества.
    /// </summary>
    function GetAlbumsCount(var Count: Integer; Params: TVkParamsPhotosGetAlbumsCount): Boolean;
    /// <summary>
    /// Возвращает все фотографии пользователя или сообщества в антихронологическом порядке.
    /// </summary>
    function GetAll(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает все фотографии пользователя или сообщества в антихронологическом порядке.
    /// </summary>
    function GetAll(var Items: TVkPhotos; Params: TVkParamsPhotosGetAll): Boolean; overload;
    /// <summary>
    /// Возвращает отсортированный в антихронологическом порядке список всех комментариев к конкретному альбому или ко всем альбомам пользователя.
    /// </summary>
    function GetAllComments(var Items: TVkComments; Params: TVkParamsPhotosGetAllComments): Boolean;
    /// <summary>
    /// Возвращает информацию о фотографиях по их идентификаторам.
    /// <b>Photos</b> - перечисленные через запятую идентификаторы, которые представляют собой идущие через знак подчеркивания id пользователей, разместивших фотографии, и id самих фотографий. Чтобы получить информацию о фотографии в альбоме группы, вместо id пользователя следует указать -id группы. Пример значения photos: 1_263219656,6492_456239863,-1_456239099
    /// Некоторые фотографии, идентификаторы которых могут быть получены через API, закрыты приватностью, и не будут получены. В этом случае следует использовать ключ доступа фотографии (access_key) в её идентификаторе.
    /// Пример значения photos: 1_129207899_220df2876123d3542f, 6492_135055734_e0a9bcc31144f67fbd
    /// Поле access_key будет возвращено вместе с остальными данными фотографии в методах, которые возвращают фотографии, закрытые приватностью но доступные в данном контексте. Например данное поле имеют фотографии, возвращаемые методом newsfeed.get.
    /// </summary>
    function GetById(var Items: TVkPhotos; Photos: TArrayOfString; Extended: Boolean = False; PhotoSizes: Boolean =
      False): Boolean; overload;
    /// <summary>
    /// Позволяет получить адрес для загрузки обложки чата.
    /// </summary>
    function GetChatUploadServer(var UploadUrl: string; ChatId: Integer; Crop: TPoint; CropWidth: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список комментариев к фотографии.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к фотографии.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsPhotosGetComments): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии подборки товаров в сообществе.
    /// Минимальный размер фотографии — 1280x720 пикселей.
    /// </summary>
    function GetMarketAlbumUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии товара.
    /// </summary>
    function GetMarketUploadServer(var UploadUrl: string; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии товара.
    /// </summary>
    function GetMarketUploadServer(var UploadUrl: string; Params: TVkParamsPhotosGetMarketUploadServer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии в личное сообщение.
    /// </summary>
    function GetMessagesUploadServer(var UploadUrl: string; PeerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии в личное сообщение.
    /// </summary>
    function GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список фотографий, на которых есть непросмотренные отметки.
    /// </summary>
    function GetNewTags(var Items: TVkPhotos; Count: Integer = 20; Offset: Integer = 0): Boolean;
    /// <summary>
    /// Получает адрес для загрузки обложки сообщества.
    /// </summary>
    function GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: Integer; CropLeft: TPoint; CropRight: TPoint):
      Boolean; overload;
    /// <summary>
    /// Получает адрес для загрузки обложки сообщества.
    /// </summary>
    function GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки главной фотографии на страницу пользователя или сообщества.
    /// </summary>
    function GetOwnerPhotoUploadServer(var UploadUrl: string; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список отметок на фотографии.
    /// </summary>
    function GetTags(var Items: TVkPhotoTags; PhotoId: Integer; OwnerId: Integer = 0; AccessKey: string = ''): Boolean;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографий.
    /// </summary>
    function GetUploadServer(var UploadData: TVkPhotoGetUploadResponse; AlbumId: Integer = 0; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список фотографий, на которых отмечен пользователь.
    /// </summary>
    function GetUserPhotos(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список фотографий, на которых отмечен пользователь.
    /// </summary>
    function GetUserPhotos(var Items: TVkPhotos; Params: TVkParamsPhotosGetUserPhotos): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии на стену пользователя или сообщества.
    /// </summary>
    function GetWallUploadServer(var UploadData: TVkPhotoGetUploadResponse; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Делает фотографию обложкой альбома.
    /// </summary>
    function MakeCover(PhotoId, AlbumId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Переносит фотографию из одного альбома в другой.
    /// </summary>
    function Move(PhotoId, TargetAlbumId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Добавляет отметку на фотографию.
    /// </summary>
    function PutTag(var TagId: Integer; PhotoId, UserId: Integer; Left, Right: TPoint; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет отметку с фотографии.
    /// </summary>
    function RemoveTag(PhotoId, TagId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Меняет порядок альбома в списке альбомов пользователя.
    /// </summary>
    function ReorderAlbums(Params: TVkParamsPhotosReorderAlbums): Boolean;
    /// <summary>
    /// Меняет порядок фотографии в списке фотографий альбома пользователя.
    /// </summary>
    function ReorderPhotos(Params: TVkParamsPhotosReorderPhotos): Boolean;
    /// <summary>
    /// Позволяет пожаловаться на фотографию.
    /// </summary>
    function Report(OwnerId, PhotoId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к фотографии.
    /// </summary>
    function ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// Восстанавливает удаленную фотографию.
    /// </summary>
    function Restore(PhotoId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Восстанавливает удаленный комментарий к фотографии.
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки.
    /// </summary>
    function Save(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки.
    /// </summary>
    function Save(var Items: TVkPhotos; Params: TVkParamsPhotosSave): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getMarketAlbumUploadServer.
    /// </summary>
    function SaveMarketAlbumPhoto(var Items: TVkPhotos; GroupId: Integer; Photo, Server, Hash: string): Boolean;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getMarketUploadServer.
    /// </summary>
    function SaveMarketPhoto(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getMarketUploadServer.
    /// </summary>
    function SaveMarketPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveMarketPhoto): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографию после успешной загрузки на URI, полученный методом photos.getMessagesUploadServer.
    /// </summary>
    function SaveMessagesPhoto(var Items: TVkPhotos; Data: TVkPhotoUploadResponse): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографию после успешной загрузки на URI, полученный методом photos.getMessagesUploadServer.
    /// </summary>
    function SaveMessagesPhoto(var Items: TVkPhotos; Server: Integer; Photo, Hash: string): Boolean; overload;
    /// <summary>
    /// Сохраняет изображение для обложки сообщества после успешной загрузки.
    /// </summary>
    function SaveOwnerCoverPhoto(var Items: TVkCoverImages; Photo, Hash: string): Boolean;
    /// <summary>
    /// Позволяет сохранить главную фотографию пользователя или сообщества. Адрес для загрузки фотографии Вы можете получить с помощью метода photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(var Info: TVkOwnerPhoto; Server: Integer; Photo, Hash: string): Boolean; overload;
    /// <summary>
    /// Позволяет сохранить главную фотографию пользователя или сообщества. Адрес для загрузки фотографии Вы можете получить с помощью метода photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(var Info: TVkOwnerPhoto; Data: TVkPhotoUploadResponse): Boolean; overload;
    /// <summary>
    /// Позволяет сохранить главную фотографию пользователя или сообщества. Адрес для загрузки фотографии Вы можете получить с помощью метода photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(FileName: string): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getWallUploadServer.
    /// </summary>
    function SaveWallPhoto(var Item: TVkPhoto; Params: TParams): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getWallUploadServer.
    /// </summary>
    function SaveWallPhoto(var Item: TVkPhoto; Params: TVkParamsPhotosSaveWallPhoto): Boolean; overload;
    /// <summary>
    /// Осуществляет поиск изображений по местоположению или описанию.
    /// </summary>
    function Search(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Осуществляет поиск изображений по местоположению или описанию.
    /// </summary>
    function Search(var Items: TVkPhotos; Params: TVkParamsPhotosSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TPhotosController }

function TPhotosController.GetMessagesUploadServer(var UploadUrl: string; PeerId: Integer): Boolean;
var
  Upload: TVkPhotoGetUploadResponse;
begin
  Result := GetMessagesUploadServer(Upload, PeerId);
  if Result then
  begin
    try
      UploadUrl := Upload.UploadUrl;
    finally
      Upload.Free;
    end;
  end;
end;

function TPhotosController.Get(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetAll(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.getAll', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetAlbums(var Items: TVkPhotoAlbums; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.getAlbums', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotoAlbums.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetAlbums(var Items: TVkPhotoAlbums; Params: TVkParamsAlbumsGet): Boolean;
begin
  Result := GetAlbums(Items, Params.List);
end;

function TPhotosController.GetAlbumsCount(var Count: Integer; Params: TVkParamsPhotosGetAlbumsCount): Boolean;
begin
  with Handler.Execute('photos.getAlbumsCount', Params.List) do
    Result := Success and TryStrToInt(Response, Count);
end;

function TPhotosController.ConfirmTags(Tags: TIds): Boolean;
begin
  with Handler.Execute('photos.confirmTag', ['tags', Tags.ToString]) do
    Result := Success and (Response = '1');
end;

function TPhotosController.Copy(var Id: Integer; OwnerId, PhotoId: Integer; AccessKey: string): Boolean;
begin
  with Handler.Execute('photos.confirmTag', [['owner_id', OwnerId.ToString], ['photo_id', PhotoId.ToString], ['access_key',
    AccessKey]]) do
    Result := Success and TryStrToInt(Response, Id);
end;

function TPhotosController.CreateAlbum(var Item: TVkPhotoAlbum; Params: TVkParamsPhotosCreateAlbum): Boolean;
begin
  Result := CreateAlbum(Item, Params.List);
end;

function TPhotosController.CreateComment(var Id: Integer; Params: TVkParamsPhotosCreateComment): Boolean;
begin
  Result := CreateComment(Id, Params.List);
end;

function TPhotosController.DeclineTags: Boolean;
begin
  with Handler.Execute('photos.declineTags') do
    Result := Success and (Response = '1');
end;

function TPhotosController.Delete(OwnerId, PhotoId: Integer): Boolean;
begin
  with Handler.Execute('photos.delete', [['owner_id', OwnerId.ToString], ['photo_id', PhotoId.ToString]]) do
    Result := Success and (Response = '1');
end;

function TPhotosController.DeleteAlbum(AlbumId, GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('photos.deleteAlbum', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.DeleteComment(OwnerId, CommentId: Integer): Boolean;
begin
  with Handler.Execute('photos.deleteComment', [['owner_id', OwnerId.ToString], ['comment_id', CommentId.ToString]]) do
    Result := Success and (Response = '1');
end;

function TPhotosController.Edit(Params: TVkParamsPhotosEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TPhotosController.EditAlbum(Params: TVkParamsPhotosEditAlbum): Boolean;
begin
  Result := EditAlbum(Params.List);
end;

function TPhotosController.EditComment(Params: TVkParamsPhotosEditComment): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TPhotosController.EditComment(Params: TParams): Boolean;
begin
  with Handler.Execute('photos.editComment', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.EditAlbum(Params: TParams): Boolean;
begin
  with Handler.Execute('photos.editAlbum', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.Edit(Params: TParams): Boolean;
begin
  with Handler.Execute('photos.edit', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.CreateComment(var Id: Integer; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.createComment', Params) do
    Result := Success and TryStrToInt(Response, Id);
end;

function TPhotosController.CreateAlbum(var Item: TVkPhotoAlbum; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.createAlbum', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoAlbum.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.Get(var Items: TVkPhotos; Params: TVkParamsPhotosGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TPhotosController.GetAll(var Items: TVkPhotos; Params: TVkParamsPhotosGetAll): Boolean;
begin
  Result := GetAll(Items, Params.List);
end;

function TPhotosController.GetAllComments(var Items: TVkComments; Params: TVkParamsPhotosGetAllComments): Boolean;
begin
  with Handler.Execute('photos.getAllComments', Params.List) do
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

function TPhotosController.GetById(var Items: TVkPhotos; Photos: TArrayOfString; Extended, PhotoSizes: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('photos', Photos);
  if Extended then
    Params.Add('extended', Extended);
  if PhotoSizes then
    Params.Add('photo_sizes', PhotoSizes);
  with Handler.Execute('photos.getById', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetChatUploadServer(var UploadUrl: string; ChatId: Integer; Crop: TPoint; CropWidth: Integer): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Params.Add('chat_id', ChatId);
  if not Crop.IsZero then
  begin
    Params.Add('crop_x', Crop.X);
    Params.Add('crop_y', Crop.Y);
  end;
  if CropWidth <> 0 then
    Params.Add('crop_width', CropWidth);
  with Handler.Execute('photos.getChatUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetComments(var Items: TVkComments; Params: TVkParamsPhotosGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TPhotosController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.getComments', Params) do
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

function TPhotosController.GetMarketAlbumUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
var
  Item: TVkPhotoGetUploadResponse;
begin
  with Handler.Execute('photos.getMarketAlbumUploadServer', ['group_id', GroupId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetMarketUploadServer(var UploadUrl: string; Params: TVkParamsPhotosGetMarketUploadServer): Boolean;
begin
  Result := GetMarketUploadServer(UploadUrl, Params.List);
end;

function TPhotosController.GetMarketUploadServer(var UploadUrl: string; Params: TParams): Boolean;
var
  Item: TVkPhotoGetUploadResponse;
begin
  with Handler.Execute('photos.getMarketUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: Integer): Boolean;
var
  Params: TParams;
begin
  if PeerId <> 0 then
    Params.Add('peer_id', PeerId);
  with Handler.Execute('photos.getMessagesUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Upload := TVkPhotoGetUploadResponse.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetNewTags(var Items: TVkPhotos; Count, Offset: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  with Handler.Execute('photos.getNewTags', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
begin
  Result := GetOwnerCoverPhotoUploadServer(UploadUrl, GroupId, TPoint.Zero, TPoint.Zero);
end;

function TPhotosController.GetOwnerPhotoUploadServer(var UploadUrl: string; OwnerId: Integer): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Params.Add('owner_id', OwnerId);
  with Handler.Execute('photos.getOwnerPhotoUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetTags(var Items: TVkPhotoTags; PhotoId, OwnerId: Integer; AccessKey: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo_id', PhotoId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('access_key', AccessKey);
  with Handler.Execute('photos.getTags', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotoTags.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetUploadServer(var UploadData: TVkPhotoGetUploadResponse; AlbumId, GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  if AlbumId <> 0 then
    Params.Add('album_id', AlbumId);
  with Handler.Execute('photos.getUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        UploadData := TVkPhotoGetUploadResponse.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetUserPhotos(var Items: TVkPhotos; Params: TVkParamsPhotosGetUserPhotos): Boolean;
begin
  Result := GetUserPhotos(Items, Params.List);
end;

function TPhotosController.GetWallUploadServer(var UploadData: TVkPhotoGetUploadResponse; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('photos.getWallUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        UploadData := TVkPhotoGetUploadResponse.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.MakeCover(PhotoId, AlbumId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('album_id', AlbumId);
  with Handler.Execute('photos.makeCover', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.Move(PhotoId, TargetAlbumId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('target_album_id', TargetAlbumId);
  with Handler.Execute('photos.move', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.PutTag(var TagId: Integer; PhotoId, UserId: Integer; Left, Right: TPoint; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('user_id', UserId);
  Params.Add('x', Left.X);
  Params.Add('y', Left.y);
  Params.Add('x2', Right.X);
  Params.Add('y2', Right.y);
  with Handler.Execute('photos.putTag', Params) do
    Result := Success and TryStrToInt(Response, TagId);
end;

function TPhotosController.RemoveTag(PhotoId, TagId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('tag_id', TagId);
  with Handler.Execute('photos.removeTag', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.ReorderAlbums(Params: TVkParamsPhotosReorderAlbums): Boolean;
begin
  with Handler.Execute('photos.reorderAlbums', Params.List) do
    Result := Success and (Response = '1');
end;

function TPhotosController.ReorderPhotos(Params: TVkParamsPhotosReorderPhotos): Boolean;
begin
  with Handler.Execute('photos.reorderPhotos', Params.List) do
    Result := Success and (Response = '1');
end;

function TPhotosController.Report(OwnerId, PhotoId: Integer; Reason: TVkMediaReportReason): Boolean;
begin
  with Handler.Execute('photos.report', [['owner_id', OwnerId.ToString], ['photo_id', PhotoId.ToString], ['reason',
    Reason.ToConst.ToString]]) do
    Result := Success and (Response = '1');
end;

function TPhotosController.ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
begin
  with Handler.Execute('photos.reportComment', [['owner_id', OwnerId.ToString], ['comment_id', CommentId.ToString], ['reason',
    Reason.ToConst.ToString]]) do
    Result := Success and (Response = '1');
end;

function TPhotosController.Restore(PhotoId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  with Handler.Execute('photos.restore', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.RestoreComment(CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  with Handler.Execute('photos.restoreComment', Params) do
    Result := Success and (Response = '1');
end;

function TPhotosController.GetUserPhotos(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.getUserPhotos', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: Integer; CropLeft, CropRight:
  TPoint): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Params.Add('group_id', GroupId);
  Params.Add('crop_x', CropLeft.X);
  Params.Add('crop_y', CropLeft.y);
  Params.Add('crop_x2', CropRight.X);
  Params.Add('crop_y2', CropRight.y);
  with Handler.Execute('photos.getOwnerCoverPhotoUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.Save(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.save', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.Save(var Items: TVkPhotos; Params: TVkParamsPhotosSave): Boolean;
begin
  Result := Save(Items, Params.List);
end;

function TPhotosController.SaveMarketAlbumPhoto(var Items: TVkPhotos; GroupId: Integer; Photo, Server, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  with Handler.Execute('photos.saveMarketAlbumPhoto', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.SaveMarketPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveMarketPhoto): Boolean;
begin
  Result := SaveMarketPhoto(Items, Params.List);
end;

function TPhotosController.SaveMessagesPhoto(var Items: TVkPhotos; Server: Integer; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  with Handler.Execute('photos.saveMessagesPhoto', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.SaveOwnerCoverPhoto(var Items: TVkCoverImages; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('hash', Hash);
  with Handler.Execute('photos.saveOwnerCoverPhoto', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkCoverImages.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.SaveOwnerPhoto(FileName: string): Boolean;
var
  VKAPI: TCustomVK;
var
  Server: string;
  Upload: TVkPhotoUploadResponse;
  Info: TVkOwnerPhoto;
begin
  Result := False;
  VKAPI := TCustomVK(VK);
  if VKAPI.Photos.GetOwnerPhotoUploadServer(Server) then
  begin
    if VKAPI.Uploader.UploadPhotos(Server, FileName, Upload) then
    begin
      if VKAPI.Photos.SaveOwnerPhoto(Info, Upload) then
      begin
        try
          Result := Info.Saved;
        except
          Result := False;
        end;
        Info.Free;
      end;
      Upload.Free;
    end;
  end;
end;

function TPhotosController.SaveOwnerPhoto(var Info: TVkOwnerPhoto; Data: TVkPhotoUploadResponse): Boolean;
begin
  Result := SaveOwnerPhoto(Info, Data.Server, Data.Photo, Data.Hash);
end;

function TPhotosController.SaveOwnerPhoto(var Info: TVkOwnerPhoto; Server: Integer; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  with Handler.Execute('photos.saveOwnerPhoto', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Info := TVkOwnerPhoto.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.SaveWallPhoto(var Item: TVkPhoto; Params: TVkParamsPhotosSaveWallPhoto): Boolean;
begin
  Result := SaveWallPhoto(Item, Params.List);
end;

function TPhotosController.Search(var Items: TVkPhotos; Params: TVkParamsPhotosSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TPhotosController.Search(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.search', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.SaveWallPhoto(var Item: TVkPhoto; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.saveWallPhoto', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhoto.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.SaveMarketPhoto(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.saveMarketPhoto', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPhotos.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.SaveMessagesPhoto(var Items: TVkPhotos; Data: TVkPhotoUploadResponse): Boolean;
begin
  Result := SaveMessagesPhoto(Items, Data.Server, Data.Photo, Data.Hash);
end;

function TPhotosController.СonfirmTag(PhotoId, TagId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('tag_id', TagId);
  with Handler.Execute('photos.confirmTag', Params) do
    Result := Success and (Response = '1');
end;

{ TVkGetAllParams }

function TVkParamsPhotosGetAll.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetAll.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPhotosGetAll.NeedHidden(Value: Boolean): Integer;
begin
  Result := List.Add('need_hidden', Value);
end;

function TVkParamsPhotosGetAll.NoServiceAlbums(Value: Boolean): Integer;
begin
  Result := List.Add('no_service_albums', Value);
end;

function TVkParamsPhotosGetAll.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGetAll.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosGetAll.PhotoSizes(Value: Boolean): Integer;
begin
  Result := List.Add('photo_sizes', Value);
end;

function TVkParamsPhotosGetAll.SkipHidden(Value: Boolean): Integer;
begin
  Result := List.Add('skip_hidden', Value);
end;

{ TVkParamsAlbumsGet }

function TVkParamsAlbumsGet.AlbumIds(Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsAlbumsGet.AlbumIds(Value: Integer): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsAlbumsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsAlbumsGet.NeedCovers(Value: Boolean): Integer;
begin
  Result := List.Add('need_covers', Value);
end;

function TVkParamsAlbumsGet.NeedSystem(Value: Boolean): Integer;
begin
  Result := List.Add('need_system', Value);
end;

function TVkParamsAlbumsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsAlbumsGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsAlbumsGet.PhotoSizes(Value: Boolean): Integer;
begin
  Result := List.Add('photo_sizes', Value);
end;

{ TVkPhotosGetParams }

function TVkParamsPhotosGet.AlbumId(Value: TVkPhotoSystemAlbum): Integer;
begin
  Result := List.Add('album_id', Value.ToString);
end;

function TVkParamsPhotosGet.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPhotosGet.Feed(Value: Integer): Integer;
begin
  Result := List.Add('feed', Value);
end;

function TVkParamsPhotosGet.FeedType(Value: TVkPhotoFeedType): Integer;
begin
  Result := List.Add('feed_type', Value.ToString);
end;

function TVkParamsPhotosGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosGet.PhotoIds(Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('photo_ids', Value);
end;

function TVkParamsPhotosGet.PhotoSizes(Value: Boolean): Integer;
begin
  Result := List.Add('photo_sizes', Value);
end;

function TVkParamsPhotosGet.Rev(Value: Boolean): Integer;
begin
  Result := List.Add('rev', Value);
end;

function TVkParamsPhotosGet.Uid(Value: Integer): Integer;
begin
  Result := List.Add('uid', Value);
end;

{ TVkParamsPhotosCreateAlbum }

function TVkParamsPhotosCreateAlbum.CommentsDisabled(Value: Boolean): Integer;
begin
  Result := List.Add('comments_disabled', Value);
end;

function TVkParamsPhotosCreateAlbum.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsPhotosCreateAlbum.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosCreateAlbum.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsPhotosCreateAlbum.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsPhotosCreateAlbum.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsPhotosCreateAlbum.UploadByAdminsOnly(Value: Boolean): Integer;
begin
  Result := List.Add('upload_by_admins_only', Value);
end;

{ TVkParamsPhotosCreateComment }

function TVkParamsPhotosCreateComment.AccessKey(Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsPhotosCreateComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsPhotosCreateComment.FromGroup(Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsPhotosCreateComment.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsPhotosCreateComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsPhotosCreateComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosCreateComment.PhotoId(Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPhotosCreateComment.ReplyToComment(Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkParamsPhotosCreateComment.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

{ TVkParamsPhotosEdit }

function TVkParamsPhotosEdit.Caption(Value: string): Integer;
begin
  Result := List.Add('caption', Value);
end;

function TVkParamsPhotosEdit.DeletePlace(Value: Boolean): Integer;
begin
  Result := List.Add('delete_place', Value);
end;

function TVkParamsPhotosEdit.FoursquareId(Value: string): Integer;
begin
  Result := List.Add('foursquare_id', Value);
end;

function TVkParamsPhotosEdit.Latitude(Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsPhotosEdit.Longitude(Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsPhotosEdit.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosEdit.PhotoId(Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPhotosEdit.PlaceStr(Value: string): Integer;
begin
  Result := List.Add('place_str', Value);
end;

{ TVkParamsPhotosEditAlbum }

function TVkParamsPhotosEditAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosEditAlbum.CommentsDisabled(Value: Boolean): Integer;
begin
  Result := List.Add('comments_disabled', Value);
end;

function TVkParamsPhotosEditAlbum.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsPhotosEditAlbum.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosEditAlbum.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsPhotosEditAlbum.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsPhotosEditAlbum.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsPhotosEditAlbum.UploadByAdminsOnly(Value: Boolean): Integer;
begin
  Result := List.Add('upload_by_admins_only', Value);
end;

{ TVkParamsPhotosEditComment }

function TVkParamsPhotosEditComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsPhotosEditComment.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsPhotosEditComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsPhotosEditComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsPhotosGetAlbumsCount }

function TVkParamsPhotosGetAlbumsCount.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosGetAlbumsCount.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsPhotosGetAllComments }

function TVkParamsPhotosGetAllComments.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosGetAllComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetAllComments.NeedLikes(Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsPhotosGetAllComments.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGetAllComments.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsPhotosGetComments }

function TVkParamsPhotosGetComments.AccessKey(Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsPhotosGetComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetComments.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPhotosGetComments.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsPhotosGetComments.NeedLikes(Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsPhotosGetComments.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGetComments.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetComments.PhotoId(Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPhotosGetComments.Sort(Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsPhotosGetComments.StartCommentId(Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

{ TVkParamsPhotosGetMarketUploadServer }

function TVkParamsPhotosGetMarketUploadServer.Crop(Value: TPoint): Integer;
begin
  List.Add('crop_x', Value.X);
  Result := List.Add('crop_y', Value.Y);
end;

function TVkParamsPhotosGetMarketUploadServer.CropWidth(Value: Integer): Integer;
begin
  Result := List.Add('crop_width', Value);
end;

function TVkParamsPhotosGetMarketUploadServer.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosGetMarketUploadServer.MainPhoto(Value: Boolean): Integer;
begin
  Result := List.Add('main_photo', Value);
end;

{ TVkParamsPhotosGetUserPhotos }

function TVkParamsPhotosGetUserPhotos.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetUserPhotos.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPhotosGetUserPhotos.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGetUserPhotos.Sort(Value: Boolean): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsPhotosGetUserPhotos.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsPhotosReorderAlbums }

function TVkParamsPhotosReorderAlbums.After(Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

function TVkParamsPhotosReorderAlbums.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosReorderAlbums.Before(Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsPhotosReorderAlbums.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsPhotosReorderPhotos }

function TVkParamsPhotosReorderPhotos.After(Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

function TVkParamsPhotosReorderPhotos.Before(Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsPhotosReorderPhotos.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosReorderPhotos.PhotoId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsPhotosSave }

function TVkParamsPhotosSave.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosSave.Caption(Value: string): Integer;
begin
  Result := List.Add('caption', Value);
end;

function TVkParamsPhotosSave.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosSave.Hash(Value: string): Integer;
begin
  Result := List.Add('hash', Value);
end;

function TVkParamsPhotosSave.Latitude(Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsPhotosSave.Longitude(Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsPhotosSave.PhotosList(Value: TArrayOfString): Integer;
begin
  Result := List.Add('photos_list', Value);
end;

function TVkParamsPhotosSave.Server(Value: string): Integer;
begin
  Result := List.Add('server', Value);
end;

{ TVkParamsPhotosSaveMarketPhoto }

function TVkParamsPhotosSaveMarketPhoto.CropData(Value: string): Integer;
begin
  Result := List.Add('crop_data', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.CropHash(Value: string): Integer;
begin
  Result := List.Add('crop_hash', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.Hash(Value: string): Integer;
begin
  Result := List.Add('hash', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.Photo(Value: string): Integer;
begin
  Result := List.Add('photo', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.Server(Value: Integer): Integer;
begin
  Result := List.Add('server', Value);
end;

{ TVkParamsPhotosSaveWallPhoto }

function TVkParamsPhotosSaveWallPhoto.Caption(Value: string): Integer;
begin
  Result := List.Add('caption', Value);
end;

function TVkParamsPhotosSaveWallPhoto.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Hash(Value: string): Integer;
begin
  Result := List.Add('hash', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Latitude(Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Longitude(Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Photo(Value: string): Integer;
begin
  Result := List.Add('photo', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Server(Value: string): Integer;
begin
  Result := List.Add('server', Value);
end;

function TVkParamsPhotosSaveWallPhoto.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsPhotosSearch }

function TVkParamsPhotosSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosSearch.EndTime(Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsPhotosSearch.Latitude(Value: Extended): Integer;
begin
  Result := List.Add('lat', Value);
end;

function TVkParamsPhotosSearch.Longitude(Value: Extended): Integer;
begin
  Result := List.Add('long', Value);
end;

function TVkParamsPhotosSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offse', Value);
end;

function TVkParamsPhotosSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsPhotosSearch.Radius(Value: Integer): Integer;
begin
  Result := List.Add('radius', Value);
end;

function TVkParamsPhotosSearch.Sort(Value: Boolean): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsPhotosSearch.StartTime(Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

end.

