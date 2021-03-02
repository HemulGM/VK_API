unit VK.Entity.Video;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Common, VK.Entity.Privacy, VK.Entity.Common.List, VK.Entity.Info,
  VK.Types, VK.Wrap.Interceptors;

type
  TVkVideoFiles = class(TVkEntity)
  private
    FExternal: string;
    FMp4_720: string;
    FMp4_360: string;
    FMp4_480: string;
    FMp4_240: string;
  public
    property&External: string read FExternal write FExternal;
    property Mp4_720: string read FMp4_720 write FMp4_720;
    property Mp4_360: string read FMp4_360 write FMp4_360;
    property Mp4_240: string read FMp4_240 write FMp4_240;
    property Mp4_480: string read FMp4_480 write FMp4_480;
  end;

  TVkVideoImage = class(TVkImage)
  private
    FWith_padding: Integer;
  public
    property WithPadding: Integer read FWith_padding write FWith_padding;
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
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDescription: string;
    FDuration: Integer;
    FFiles: TVkVideoFiles;
    FImage: TArray<TVkVideoImage>;
    FIs_favorite: Boolean;
    FLikes: TVkLikesInfo;
    FLocal_views: Integer;
    FOwner_id: Integer;
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
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FAdding_date: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_private: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FProcessing: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FLive: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FUpcoming: Boolean;
    FFirstFrame: TArray<TVkImage>;
    FWidth: Integer;
    FHeight: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FRepeat: Boolean;
    FUser_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FConverting: Boolean;
    FIs_subscribed: Boolean;
    FBalance: Integer;
    FLive_status: string;
    FSpectators: Integer;
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
    property Duration: Integer read FDuration write FDuration;
    property Files: TVkVideoFiles read FFiles write FFiles;
    property FirstFrame1280: string read Ffirst_frame_1280 write Ffirst_frame_1280;
    property FirstFrame130: string read Ffirst_frame_130 write Ffirst_frame_130;
    property FirstFrame320: string read Ffirst_frame_320 write Ffirst_frame_320;
    property FirstFrame640: string read Ffirst_frame_640 write Ffirst_frame_640;
    property FirstFrame800: string read Ffirst_frame_800 write Ffirst_frame_800;
    /// <summary>
    /// Изображение первого кадра
    /// </summary>
    property FirstFrame: TArray<TVkImage> read FFirstFrame write FFirstFrame;
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
    /// Статус прямой трансляции. Может принимать значения: "waiting", "started", "finished", "failed", "upcoming"
    /// </summary>
    property LiveStatus: string read FLive_status write FLive_status;
    /// <summary>
    /// Если видео внешнее, количество просмотров в ВК
    /// </summary>
    property LocalViews: Integer read FLocal_views write FLocal_views;
    /// <summary>
    /// Идентификатор владельца видеозаписи
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
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
    property&Repeat: Boolean read FRepeat write FRepeat;
    /// <summary>
    /// Содержит объект репоста
    /// </summary>
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    /// <summary>
    /// Количество зрителей прямой трансляции
    /// </summary>
    property Spectators: Integer read FSpectators write FSpectators;
    /// <summary>
    /// Название видеозаписи
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// Поле свидетельствует о том, что трансляция скоро начнётся (для live = 1)
    /// </summary>
    property UpComing: Boolean read FUpcoming write FUpcoming;
    /// <summary>
    /// Идентификатор пользователя, загрузившего видео, если оно было загружено в группу одним из участников
    /// </summary>
    property UserId: Integer read FUser_id write FUser_id;
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
    property&Platform: string read FPlatform write FPlatform;
    /// <summary>
    /// Тип видеозаписи
    /// </summary>
    property&Type: TVkVideoType read FType write FType;
    ///Методы
    function ToAttachment: TAttachment;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkVideos = TVkEntityList<TVkVideo>;

  TVkVideoAlbum = class(TVkObject)
  private
    FCount: Integer;
    FImage: TArray<TVkVideoImage>;
    FOwner_id: Integer;
    FPrivacy: TVkPrivacy;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FUpdated_time: TDateTime;
  public
    property Count: Integer read FCount write FCount;
    property Image: TArray<TVkVideoImage> read FImage write FImage;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Privacy: TVkPrivacy read FPrivacy write FPrivacy;
    property Title: string read FTitle write FTitle;
    property UpdatedTime: TDateTime read FUpdated_time write FUpdated_time;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkVideoAlbums = TVkEntityList<TVkVideoAlbum>;

implementation

uses
  VK.CommonUtils;

{TVkVideo}

constructor TVkVideo.Create;
begin
  inherited;
  FFiles := TVkVideoFiles.Create();
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
end;

destructor TVkVideo.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoImage>(FImage);
  TArrayHelp.FreeArrayOfObject<TVkImage>(FFirstFrame);
  FFiles.Free;
  FLikes.Free;
  FReposts.Free;
  inherited;
end;

function TVkVideo.ToAttachment: TAttachment;
begin
  Result := TAttachment.Video(OwnerId, Id, AccessKey);
end;

{TVkVideoAlbum}

constructor TVkVideoAlbum.Create;
begin
  inherited;
  FPrivacy := TVkPrivacy.Create();
end;

destructor TVkVideoAlbum.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkVideoImage>(FImage);
  FPrivacy.Free;
  inherited;
end;

end.

