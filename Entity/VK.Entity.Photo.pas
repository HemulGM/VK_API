unit VK.Entity.Photo;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, VK.Entity.Common,
  VK.Entity.Info, VK.Types, VK.Entity.Common.List, VK.Wrap.Interceptors;

type
  TVkOwnerPhoto = class(TVkEntity)
  private
    FPhoto_src: string;
    FPhoto_src_small: string;
    FPhoto_src_big: string;
    FPost_id: Integer;
    FPhoto_hash: string;
    FSaved: Boolean;
  public
    property PhotoHash: string read FPhoto_hash write FPhoto_hash;
    property PhotoSrc: string read FPhoto_src write FPhoto_src;
    property PhotoSrcBig: string read Fphoto_src_big write Fphoto_src_big;
    property PhotoSrcSmall: string read FPhoto_src_small write FPhoto_src_small;
    property Saved: Boolean read FSaved write FSaved;
    property PostId: Integer read FPost_id write FPost_id;
  end;

  TVkPhotoTag = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FPlacer_id: Integer;
    FTagged_name: string;
    FUser_id: TVkPeerId;
    FViewed: Boolean;
    FX: Integer;
    FX2: Integer;
    FY: Integer;
    FY2: Integer;
  public
    property Date: TDateTime read FDate write FDate;
    property PlacerId: Integer read FPlacer_id write FPlacer_id;
    property TaggedName: string read FTagged_name write FTagged_name;
    property UserId: TVkPeerId read FUser_id write FUser_id;
    property Viewed: Boolean read FViewed write FViewed;
    property X: Integer read FX write FX;
    property X2: Integer read FX2 write FX2;
    property Y: Integer read FY write FY;
    property Y2: Integer read FY2 write FY2;
  end;

  TVkPhotoTags = class(TVkEntity)
  private
    FItems: TArray<TVkPhotoTag>;
    FCount: Integer;
  public
    property Items: TArray<TVkPhotoTag> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
  end;

  TVkPhoto = class(TVkObject, IAttachment)
  private
    FAlbum_id: Int64;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_comment: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_repost: Boolean;
    FComments: TVkCommentsInfo;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FLikes: TVkLikesInfo;
    FOwner_id: TVkPeerId;
    FReposts: TVkRepostsInfo;
    FSizes: TVkSizes;
    FTags: TVkCounterEntity;
    FText: string;
    FUser_id: TVkPeerId;
    FWidth: Integer;
    FHeight: Integer;
    FAccess_key: string;
    FPhoto_604: string;
    FPhoto_75: string;
    FPhoto_1280: string;
    FPhoto_807: string;
    FPhoto_2560: string;
    FPhoto_130: string;
    FHas_tags: Boolean;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FTag_created: TDateTime;
    FPlacer_id: Int64;
    FTag_id: Int64;
    FPost_id: Int64;
    FSquare_crop: string;
    FLat: Extended;
    FLong: Extended;
  public
    /// <summary>
    /// Идентификатор фотографии
    /// </summary>
    property Id;
    /// <summary>
    /// Идентификатор альбома, в котором находится фотография
    /// </summary>
    property AlbumId: Int64 read FAlbum_id write FAlbum_id;
    /// <summary>
    /// Идентификатор владельца фотографии
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    /// <summary>
    /// Идентификатор пользователя, загрузившего фото (если фотография размещена в сообществе).
    /// Для фотографий, размещенных от имени сообщества, UserId = 100.
    /// </summary>
    property UserId: TVkPeerId read FUser_id write FUser_id;
    /// <summary>
    /// Текст описания фотографии
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Дата добавления
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Массив с копиями изображения в разных размерах
    /// </summary>
    property Sizes: TVkSizes read FSizes write FSizes;
    /// <summary>
    /// Ширина оригинала фотографии в пикселах
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// Высота оригинала фотографии в пикселах
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    //
    property CanComment: Boolean read FCan_comment write FCan_comment;
    property CanRepost: Boolean read FCan_repost write FCan_repost;
    property Comments: TVkCommentsInfo read FComments write FComments;
    property Lat: Extended read FLat write FLat;
    property Long: Extended read FLong write FLong;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    property Tags: TVkCounterEntity read FTags write FTags;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    property HasTags: Boolean read FHas_tags write FHas_tags;
    // old field api < 5.77
    /// <summary>
    /// URL копии фотографии с максимальным размером 75x75px
    /// </summary>
    property Photo75: string read FPhoto_75 write FPhoto_75;
    /// <summary>
    /// URL копии фотографии с максимальным размером 130x130px
    /// </summary>
    property Photo130: string read FPhoto_130 write FPhoto_130;
    /// <summary>
    /// UURL копии фотографии с максимальным размером 604x604px
    /// </summary>
    property Photo604: string read FPhoto_604 write FPhoto_604;
    /// <summary>
    /// URL копии фотографии с максимальным размером 807x807px
    /// </summary>
    property Photo807: string read FPhoto_807 write FPhoto_807;
    /// <summary>
    /// URL копии фотографии с максимальным размером 1280x1024px
    /// </summary>
    property Photo1280: string read FPhoto_1280 write FPhoto_1280;
    /// <summary>
    /// URL копии фотографии с максимальным размером 2560x2048px
    /// </summary>
    property Photo2560: string read FPhoto_2560 write FPhoto_2560;
    //
    property PlacerId: Int64 read FPlacer_id write FPlacer_id;
    property TagCreated: TDateTime read FTag_created write FTag_created;
    property TagId: Int64 read FTag_id write FTag_id;
    property SquareCrop: string read FSquare_crop write FSquare_crop;
    /// <summary>
    /// Идентификатор записи, в которую была загружена фотография
    /// </summary>
    property PostId: Int64 read FPost_id write FPost_id;
    //
    destructor Destroy; override;
    function ToAttachment: TAttachment;
    function ToStringId: string;
  end;

  TVkCropPhoto = class(TVkEntity)
  private
    FCrop: TVkRect;
    FPhoto: TVkPhoto;
    FRect: TVkRect;
  public
    /// <summary>
    /// Объект photo фотографии пользователя, из которой вырезается главное фото сообщества
    /// </summary>
    property Photo: TVkPhoto read FPhoto write FPhoto;
    /// <summary>
    /// Вырезанная фотография сообщества
    /// </summary>
    property Crop: TVkRect read FCrop write FCrop;
    /// <summary>
    /// Миниатюрная квадратная фотография, вырезанная из фотографии Crop
    /// </summary>
    property Rect: TVkRect read FRect write FRect;
    destructor Destroy; override;
  end;

  TVkPostedPhoto = class(TVkObject)
  private
    FOwner_id: TVkPeerId;
    FPhoto_130: string;
    FPhoto_604: string;
    FAccess_key: string;
  public
    /// <summary>
    /// Идентификатор фотографии
    /// </summary>
    property Id;
    /// <summary>
    /// Идентификатор владельца фотографии
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// URL изображения для предпросмотра
    /// </summary>
    property Photo130: string read FPhoto_130 write FPhoto_130;
    /// <summary>
    /// URL полноразмерного изображения
    /// </summary>
    property Photo604: string read FPhoto_604 write FPhoto_604;
  end;

implementation

uses
  VK.CommonUtils, System.SysUtils;

{TVkPhoto}

destructor TVkPhoto.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
  if Assigned(FLikes) then
    FLikes.Free;
  if Assigned(FReposts) then
    FReposts.Free;
  if Assigned(FComments) then
    FComments.Free;
  if Assigned(FTags) then
    FTags.Free;
  inherited;
end;

function TVkPhoto.ToAttachment: TAttachment;
begin
  Result := TAttachment.Photo(OwnerId, Id, AccessKey);
end;

function TVkPhoto.ToStringId: string;
begin
  Result := Format('%d_%d', [OwnerId, Id]);
  if not AccessKey.IsEmpty then
    Result := Result + '_' + AccessKey;
end;

{ TVkPhotoTags }

destructor TVkPhotoTags.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPhotoTag>(FItems);
  inherited;
end;

{ TVkCropPhoto }

destructor TVkCropPhoto.Destroy;
begin
  if Assigned(FCrop) then
    FCrop.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FRect) then
    FRect.Free;
  inherited;
end;

end.

