unit VK.Entity.Video;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, VK.Entity.Common,
  VK.Entity.Privacy, VK.Entity.Common.List, VK.Entity.Info, VK.Types,
  VK.Wrap.Interceptors;

type
  TVkVideoFiles = class(TVkEntity)
  private
    FExternal: string;
    FMp4_720: string;
    FMp4_360: string;
    FMp4_480: string;
    FMp4_240: string;
    FHls_ondemand: string;
    FDash_ondemand: string;
    FFailover_host: string;
    FDash_uni: string;
    FDash_sep: string;
    FMp4_144: string;
    FMp4_1080: string;
  public
    property &External: string read FExternal write FExternal;
    property MP4_144: string read FMp4_144 write FMp4_144;
    property MP4_240: string read FMp4_240 write FMp4_240;
    property MP4_360: string read FMp4_360 write FMp4_360;
    property MP4_480: string read FMp4_480 write FMp4_480;
    property MP4_720: string read FMp4_720 write FMp4_720;
    property MP4_1080: string read FMp4_1080 write FMp4_1080;
    property HLSOndemand: string read FHls_ondemand write FHls_ondemand;
    property FashOndemand: string read FDash_ondemand write FDash_ondemand;
    property FailoverHost: string read FFailover_host write FFailover_host;
    property DashUni: string read FDash_uni write FDash_uni;
    property DashSep: string read FDash_sep write FDash_sep;
  end;

  TVkVideoAdsParams = class
  private
    FContent_id: string;
    FDuration: Int64;
    FGroupId: TVkPeerId;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_xz_video: Boolean;
    FLang: Integer;
    FPl: Int64;
    FPuid1: Integer;
    FPuid10: Integer;
    FPuid12: Integer;
    FPuid13: Integer;
    FPuid14: Integer;
    FPuid15: Integer;
    FPuid18: Integer;
    FPuid2: Integer;
    FPuid21: Integer;
    FPuid3: Integer;
    FPuid5: Integer;
    FPuid6: Integer;
    FPuid7: Integer;
    FPuid9: Integer;
    FSign: string;
    FVideo_id: string;
    FVk_catid: Int64;
    FVk_id: Int64;
  public
    property ContentId: string read FContent_id write FContent_id;
    property Duration: Int64 read FDuration write FDuration;
    property GroupId: TVkPeerId read FGroupId write FGroupId;
    property IsXzVideo: Boolean read FIs_xz_video write FIs_xz_video;
    property Lang: Integer read FLang write FLang;
    property Pl: Int64 read FPl write FPl;
    property PUID1: Integer read FPuid1 write FPuid1;
    property PUID10: Integer read FPuid10 write FPuid10;
    property PUID12: Integer read FPuid12 write FPuid12;
    property PUID13: Integer read FPuid13 write FPuid13;
    property PUID14: Integer read FPuid14 write FPuid14;
    property PUID15: Integer read FPuid15 write FPuid15;
    property PUID18: Integer read FPuid18 write FPuid18;
    property PUID2: Integer read FPuid2 write FPuid2;
    property PUID21: Integer read FPuid21 write FPuid21;
    property PUID3: Integer read FPuid3 write FPuid3;
    property PUID5: Integer read FPuid5 write FPuid5;
    property PUID6: Integer read FPuid6 write FPuid6;
    property PUID7: Integer read FPuid7 write FPuid7;
    property PUID9: Integer read FPuid9 write FPuid9;
    property Sign: string read FSign write FSign;
    property VideoId: string read FVideo_id write FVideo_id;
    property VkCatId: Int64 read FVk_catid write FVk_catid;
    property VkId: Int64 read FVk_id write FVk_id;
  end;

  TVkVideoAds = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_play: Boolean;
    FMidroll_percents: TArray<Extended>;
    FParams: TVkVideoAdsParams;
    FSections: TArray<string>;
    FSlot_id: Int64;
    FTimeout: Extended;
  public
    property CanPlay: Boolean read FCan_play write FCan_play;
    property MidrollPercents: TArray<Extended> read FMidroll_percents write FMidroll_percents;
    property Params: TVkVideoAdsParams read FParams write FParams;
    /// <summary>
    /// ("preroll", "midroll", "postroll", ...)
    /// </summary>
    property Sections: TArray<string> read FSections write FSections;
    property SlotId: Int64 read FSlot_id write FSlot_id;
    property Timeout: Extended read FTimeout write FTimeout;
    destructor Destroy; override;
  end;

  TVkVideoImage = class(TVkSize)
  private
    FWith_padding: Integer;
  public
    property WithPadding: Integer read FWith_padding write FWith_padding;
  end;

  TVkTimelineThumbs = class(TVkEntity)
  private
    FCount_per_image: Integer;
    FCount_per_row: Integer;
    FCount_total: Integer;
    FFrame_height: Extended;
    FFrame_width: Extended;
    FFrequency: Extended;
    FIs_uv: Boolean;
    FLinks: TArray<string>;
  public
    property CountPerImage: Integer read FCount_per_image write FCount_per_image;
    property CountPerRow: Integer read FCount_per_row write FCount_per_row;
    property CountTotal: Integer read FCount_total write FCount_total;
    property FrameHeight: Extended read FFrame_height write FFrame_height;
    property FrameWidth: Extended read FFrame_width write FFrame_width;
    property Frequency: Extended read FFrequency write FFrequency;
    property IsUV: Boolean read FIs_uv write FIs_uv;
    property Links: TArray<string> read FLinks write FLinks;
  end;

  TVkVideo = class(TVkObject, IAttachment)
  private
    FAccess_key: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FAdded: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_add: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_add_to_faves: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_attach_link: Boolean;
    FCan_comment: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_edit: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_like: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_repost: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_subscribe: Boolean;
    FComments: Integer;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDescription: string;
    FDuration: Int64;
    FFiles: TVkVideoFiles;
    FImage: TArray<TVkVideoImage>;
    FIs_favorite: Boolean;
    FLikes: TVkLikesInfo;
    FLocal_views: Integer;
    FOwner_id: TVkPeerId;
    FPlatform: string;
    FPlayer: string;
    FReposts: TVkRepostsInfo;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TVideoTypeInterceptor)]
    FType: TVkVideoType; //video
    FViews: Integer;
    Ffirst_frame_800: string;
    Fphoto_640: string;
    Ffirst_frame_320: string;
    Ffirst_frame_130: string;
    Fphoto_1280: string;
    Ffirst_frame_640: string;
    Fphoto_800: string;
    Fphoto_320: string;
    Ffirst_frame_1280: string;
    Fphoto_130: string;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FAdding_date: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_private: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FProcessing: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FLive: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FUpcoming: Boolean;
    FFirstFrame: TArray<TVkSize>;
    FWidth: Integer;
    FHeight: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FRepeat: Boolean;
    FUser_id: TVkPeerId;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FConverting: Boolean;
    FIs_subscribed: Boolean;
    FBalance: Integer;
    [JsonReflectAttribute(ctString, rtString, TLiveStatusInterceptor)]
    FLive_status: TVkLiveStatus;
    FSpectators: Integer;
    FIs_author: Boolean;
    FTrack_code: string;
    FTimeline_thumbs: TVkTimelineThumbs;
    FCan_download: integer;
    FOv_id: string;
    FAds: TVkVideoAds;
  public
    /// <summary>
    /// Ключ доступа к объекту
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Добавлено ли видео в альбомы пользователя
    /// </summary>
    property Added: Boolean read FAdded write FAdded;
    /// <summary>
    /// Дата добавления видеозаписи пользователем или группой
    /// </summary>
    property AddingDate: TDateTime read FAdding_date write FAdding_date;
    property Ads: TVkVideoAds read FAds write FAds;
    /// <summary>
    /// Баланс донатов в прямой трансляции
    /// </summary>
    property Balance: Integer read FBalance write FBalance;
    /// <summary>
    /// Может ли пользователь добавить видеозапись к себе
    /// </summary>
    property CanAdd: Boolean read FCan_add write FCan_add;
    /// <summary>
    /// Может ли пользователь добавить видео в избранное
    /// </summary>
    property CanAddToFaves: Boolean read FCan_add_to_faves write FCan_add_to_faves;
    /// <summary>
    /// Может ли пользователь прикрепить кнопку действия к видео
    /// </summary>
    property CanAttachLink: Boolean read FCan_attach_link write FCan_attach_link;
    /// <summary>
    /// Может ли пользователь комментировать видео
    /// </summary>
    property CanComment: Boolean read FCan_comment write FCan_comment;
    /// <summary>
    /// Может ли скачать (не известно) (0, 1, 2, ...)
    /// </summary>
    property CanDownload: integer read FCan_download write FCan_download;
    /// <summary>
    /// Может ли пользователь редактировать видео
    /// </summary>
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    /// <summary>
    /// Может ли пользователь добавить видео в список <<Мне нравится>>
    /// </summary>
    property CanLike: Boolean read FCan_like write FCan_like;
    /// <summary>
    /// Может ли пользователь сделать репост видео
    /// </summary>
    property CanRepost: Boolean read FCan_repost write FCan_repost;
    /// <summary>
    /// Может ли пользователь подписаться на автора видео
    /// </summary>
    property CanSubscribe: Boolean read FCan_subscribe write FCan_subscribe;
    /// <summary>
    /// Количество комментариев к видеозаписи
    /// </summary>
    property Comments: Integer read FComments write FComments;
    /// <summary>
    /// Конвертируется ли видео
    /// </summary>
    property Converting: Boolean read FConverting write FConverting;
    /// <summary>
    /// Дата создания видеозаписи
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Текст описания видеозаписи
    /// </summary>
    property Description: string read FDescription write FDescription;
    /// <summary>
    /// Длительность ролика в секундах
    /// </summary>
    property Duration: Int64 read FDuration write FDuration;
    property Files: TVkVideoFiles read FFiles write FFiles;
    property FirstFrame1280: string read Ffirst_frame_1280 write Ffirst_frame_1280;
    property FirstFrame130: string read Ffirst_frame_130 write Ffirst_frame_130;
    property FirstFrame320: string read Ffirst_frame_320 write Ffirst_frame_320;
    property FirstFrame640: string read Ffirst_frame_640 write Ffirst_frame_640;
    property FirstFrame800: string read Ffirst_frame_800 write Ffirst_frame_800;
    /// <summary>
    /// Изображение первого кадра
    /// </summary>
    property FirstFrame: TArray<TVkSize> read FFirstFrame write FFirstFrame;
    /// <summary>
    /// Высота видео
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    property Id;
    /// <summary>
    /// Изображение обложки
    /// </summary>
    property Image: TArray<TVkVideoImage> read FImage write FImage;
    /// <summary>
    /// True, если юзер - автор объекта
    /// </summary>
    property IsAuthor: Boolean read FIs_author write FIs_author;
    /// <summary>
    /// True, если объект добавлен в закладки у текущего пользователя
    /// </summary>
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    /// <summary>
    /// Поле возвращается, если видеозапись приватная (например, была загружена в личное сообщение), всегда содержит True
    /// </summary>
    property IsPrivate: Boolean read FIs_private write FIs_private;
    /// <summary>
    /// Подписан ли пользователь на автора видео
    /// </summary>
    property IsSubscribed: Boolean read FIs_subscribed write FIs_subscribed;
    /// <summary>
    /// Содержит объект отметки <<Мне нравится>>
    /// </summary>
    property Likes: TVkLikesInfo read FLikes write FLikes;
    /// <summary>
    /// Поле возвращается в том случае, если видеозапись является прямой трансляцией, всегда содержит True. Обратите внимание, в этом случае в поле duration содержится значение False
    /// </summary>
    property Live: Boolean read FLive write FLive;
    /// <summary>
    /// Статус прямой трансляции
    /// </summary>
    property LiveStatus: TVkLiveStatus read FLive_status write FLive_status;
    /// <summary>
    /// Если видео внешнее, количество просмотров в ВК
    /// </summary>
    property LocalViews: Integer read FLocal_views write FLocal_views;
    property OvId: string read FOv_id write FOv_id;
    /// <summary>
    /// Идентификатор владельца видеозаписи
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property Photo130: string read Fphoto_130 write Fphoto_130;
    property Photo320: string read Fphoto_320 write Fphoto_320;
    property Photo640: string read Fphoto_640 write Fphoto_640;
    property Photo800: string read Fphoto_800 write Fphoto_800;
    property Photo1280: string read Fphoto_1280 write Fphoto_1280;
    /// <summary>
    /// URL страницы с плеером, который можно использовать для воспроизведения ролика в браузере. Поддерживается flash и html5, плеер всегда масштабируется по размеру окна
    /// </summary>
    property Player: string read FPlayer write FPlayer;
    /// <summary>
    /// Поле возвращается в том случае, если видеоролик находится в процессе обработки, всегда содержит 1
    /// </summary>
    property Processing: Boolean read FProcessing write FProcessing;
    /// <summary>
    /// Поле возвращается в том случае, если видео зациклено, всегда содержит 1
    /// </summary>
    property &Repeat: Boolean read FRepeat write FRepeat;
    /// <summary>
    /// Содержит объект репоста
    /// </summary>
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    /// <summary>
    /// Количество зрителей прямой трансляции
    /// </summary>
    property Spectators: Integer read FSpectators write FSpectators;
    property TimelineThumbs: TVkTimelineThumbs read FTimeline_thumbs write FTimeline_thumbs;
    /// <summary>
    /// Название видеозаписи
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// TrackCode
    /// </summary>
    property TrackCode: string read FTrack_code write FTrack_code;
    /// <summary>
    /// Поле свидетельствует о том, что трансляция скоро начнётся (для live = 1)
    /// </summary>
    property UpComing: Boolean read FUpcoming write FUpcoming;
    /// <summary>
    /// Идентификатор пользователя, загрузившего видео, если оно было загружено в группу одним из участников
    /// </summary>
    property UserId: TVkPeerId read FUser_id write FUser_id;
    /// <summary>
    /// Количество просмотров видеозаписи
    /// </summary>
    property Views: Integer read FViews write FViews;
    /// <summary>
    /// Ширина видео
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// Название платформы (для видеозаписей, добавленных с внешних сайтов)
    /// </summary>
    property &Platform: string read FPlatform write FPlatform;
    /// <summary>
    /// Тип видеозаписи
    /// </summary>
    property &Type: TVkVideoType read FType write FType;
    ///Методы
    function ToAttachment: TAttachment;
    function ToStringId: string;
    destructor Destroy; override;
  end;

  TVkVideos = TVkEntityList<TVkVideo>;

  TVkVideoAlbum = class(TVkObject)
  private
    FCount: Integer;
    FImage: TArray<TVkVideoImage>;
    FOwner_id: TVkPeerId;
    FPrivacy: TVkPrivacy;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FUpdated_time: TDateTime;
  public
    property Count: Integer read FCount write FCount;
    property Image: TArray<TVkVideoImage> read FImage write FImage;
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property Privacy: TVkPrivacy read FPrivacy write FPrivacy;
    property Title: string read FTitle write FTitle;
    property UpdatedTime: TDateTime read FUpdated_time write FUpdated_time;
    destructor Destroy; override;
  end;

  TVkVideoAlbums = TVkEntityList<TVkVideoAlbum>;

implementation

uses
  VK.CommonUtils, System.SysUtils;

{TVkVideo}

destructor TVkVideo.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoImage>(FImage);
  TArrayHelp.FreeArrayOfObject<TVkSize>(FFirstFrame);
  if Assigned(FFiles) then
    FFiles.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  if Assigned(FReposts) then
    FReposts.Free;
  if Assigned(FAds) then
    FAds.Free;
  if Assigned(FTimeline_thumbs) then
    FTimeline_thumbs.Free;
  inherited;
end;

function TVkVideo.ToAttachment: TAttachment;
begin
  Result := TAttachment.Video(OwnerId, Id, AccessKey);
end;

function TVkVideo.ToStringId: string;
begin
  Result := Format('%d_%d', [OwnerId, Id]);
  if not AccessKey.IsEmpty then
    Result := Result + '_' + AccessKey;
end;

{TVkVideoAlbum}

destructor TVkVideoAlbum.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoImage>(FImage);
  if Assigned(FPrivacy) then
    FPrivacy.Free;
  inherited;
end;

{ TVkVideoAds }

destructor TVkVideoAds.Destroy;
begin
  if Assigned(FParams) then
    FParams.Free;
  inherited;
end;

end.

