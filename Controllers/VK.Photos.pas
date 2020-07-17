unit VK.Photos;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Album, System.JSON,
  REST.Json, VK.Entity.Photo.Upload, VK.Entity.Photo;

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

  TVkPhotoFeedType = (ftPhoto, ftPhotoTag);

  TVkPhotoFeedTypeHelper = record helper for TVkPhotoFeedType
    function ToString: string; inline;
  end;

  TVkPhotoSystemAlbum = (saWall, saSaved, saProfile);

  TVkPhotoSystemAlbumHelper = record helper for TVkPhotoSystemAlbum
    function ToString: string; inline;
    function ToVkId: Integer; inline;
  end;

  TVkParamsPhotosGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer; overload;
    function AlbumId(Value: TVkPhotoSystemAlbum): Integer; overload;
    function PhotoIds(Value: TArrayOfInteger): Integer;
    function Extended(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): TVkParamsPhotosGet;
    function PhotoSizes(Value: Boolean): Integer;
    function FeedType(Value: TVkPhotoFeedType): Integer;
    function Feed(Value: Integer): Integer;
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

  TPhotosController = class(TVkController)
  public
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии в личное сообщение
    /// </summary>
    function GetMessagesUploadServer(var UploadUrl: string; PeerId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии в личное сообщение
    /// </summary>
    function GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: Integer): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографию после успешной загрузки на URI, полученный методом Photos.GetMessagesUploadServer.
    /// </summary>
    function SaveMessagesPhoto(PhotoSaveData: TVkPhotoUploadResponse; var Photos: TVkPhotos): Boolean;
    /// <summary>
    /// https://vk.com/dev/photos.get
    /// </summary>
    function Get(var Photos: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// https://vk.com/dev/photos.get
    /// </summary>
    function Get(var Photos: TVkPhotos; Params: TVkParamsPhotosGet): Boolean; overload;
    function GetAlbums(var Items: TVkPhotoAlbums; Params: TParams): Boolean; overload;
    function GetAlbums(var Items: TVkPhotoAlbums; Params: TVkParamsAlbumsGet): Boolean; overload;
    function GetAll(var Photos: TVkPhotos; Params: TParams): Boolean; overload;
    function GetAll(var Photos: TVkPhotos; Params: TVkParamsPhotosGetAll): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TPhotosController }

function TPhotosController.GetMessagesUploadServer(var UploadUrl: string; PeerId: Integer): Boolean;
var
  Upload: TVkPhotoGetUploadResponse;
begin
  Result := GetMessagesUploadServer(Upload, PeerId);
  if Result then
  begin
    UploadUrl := Upload.UploadUrl;
    Upload.Free;
  end;
end;

function TPhotosController.Get(var Photos: TVkPhotos; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Photos := TVkPhotos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetAll(var Photos: TVkPhotos; Params: TParams): Boolean;
begin
  with Handler.Execute('photos.getAll', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Photos := TVkPhotos.FromJsonString(Response);
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

function TPhotosController.Get(var Photos: TVkPhotos; Params: TVkParamsPhotosGet): Boolean;
begin
  Result := Get(Photos, Params.List);
end;

function TPhotosController.GetAll(var Photos: TVkPhotos; Params: TVkParamsPhotosGetAll): Boolean;
begin
  Result := GetAll(Photos, Params.List);
end;

function TPhotosController.GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: Integer): Boolean;
var
  Params: TParams;
begin
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

function TPhotosController.SaveMessagesPhoto(PhotoSaveData: TVkPhotoUploadResponse; var Photos: TVkPhotos): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', PhotoSaveData.Photo);
  Params.Add('server', PhotoSaveData.Server);
  Params.Add('hash', PhotoSaveData.Hash);
  with Handler.Execute('photos.saveMessagesPhoto', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Photos := TVkPhotos.FromJsonString('{"Items":' + Response + '}');
      except
        Result := False;
      end;
    end;
  end;
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
  Result := List.Add('album_ids', Value.ToString);
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

function TVkParamsPhotosGet.Offset(Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('offset', Value);
  Result := Self;
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

{ TVkPhotoSystemAlbumHelper }

function TVkPhotoSystemAlbumHelper.ToString: string;
begin
  case Self of
    saWall:
      Exit('wall');
    saSaved:
      Exit('saved');
    saProfile:
      Exit('profile');
  else
    Result := '';
  end;
end;

function TVkPhotoSystemAlbumHelper.ToVkId: Integer;
begin
  case Self of
    saWall:
      Exit(-7);
    saSaved:
      Exit(-15);
    saProfile:
      Exit(-6);
  else
    Result := 0;
  end;
end;

{ TVkPhotoFeedTypeHelper }

function TVkPhotoFeedTypeHelper.ToString: string;
begin
  case Self of
    ftPhoto:
      Exit('photo');
    ftPhotoTag:
      Exit('photo_tag');
  else
    Result := '';
  end;
end;

end.

