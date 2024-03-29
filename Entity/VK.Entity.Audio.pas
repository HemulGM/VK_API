﻿unit VK.Entity.Audio;

interface

uses
  Generics.Collections, REST.JsonReflect, System.SysUtils, Rest.Json,
  System.Json, VK.Entity.Common, VK.Types, VK.Entity.Common.List,
  VK.Wrap.Interceptors, VK.Entity.Group, VK.Entity.Photo;

type
  TVkAudioArtistPhoto = class(TVkEntity)
  private
    FType: string;
    FPhoto: TArray<TVkSize>;
  public
    property &Type: string read FType write FType;
    property Photo: TArray<TVkSize> read FPhoto write FPhoto;
    destructor Destroy; override;
  end;

  TVkAudioArtist = class(TVkEntity)
  private
    FDomain: string;
    FName: string;
    FPhoto: TArray<TVkSize>;
    FId: string;
    FPages: TArray<Int64>;
    FGroups: TArray<TVkGroup>;
    FPhotos: TArray<TVkAudioArtistPhoto>;
    FIs_followed: Boolean;
    FCan_follow: Boolean;
  public
    /// <summary>
    /// Идентификатор артиста/группы
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Домен-ссылка на аккаунт
    /// </summary>
    property Domain: string read FDomain write FDomain;
    /// <summary>
    /// Имя артиста/название группы
    /// </summary>
    property Name: string read FName write FName;
    property Photo: TArray<TVkSize> read FPhoto write FPhoto;
    property Photos: TArray<TVkAudioArtistPhoto> read FPhotos write FPhotos;
    property Pages: TArray<Int64> read FPages write FPages;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property IsFollowed: Boolean read FIs_followed write FIs_followed;
    property CanFollow: Boolean read FCan_follow write FCan_follow;
    destructor Destroy; override;
  end;

  TVkAudioArtists = TVkEntityList<TVkAudioArtist>;



  TVkAlbumThumb = class(TVkEntity)
  private
    FPhoto_34: string;
    FPhoto_1200: string;
    FPhoto_300: string;
    FPhoto_68: string;
    FPhoto_270: string;
    FPhoto_600: string;
    FPhoto_135: string;
    FWidth: Integer;
    FHeight: Integer;
  public
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property Photo34: string read FPhoto_34 write FPhoto_34;
    property Photo68: string read FPhoto_68 write FPhoto_68;
    property Photo135: string read FPhoto_135 write FPhoto_135;
    property Photo270: string read FPhoto_270 write FPhoto_270;
    property Photo300: string read FPhoto_300 write FPhoto_300;
    property Photo600: string read FPhoto_600 write FPhoto_600;
    property Photo1200: string read FPhoto_1200 write FPhoto_1200;

  end;

  TVkAudioAlbum = class(TVkObject)
  private
    FAccess_key: string;
    FOwner_id: TVkPeerId;
    FThumb: TVkAlbumThumb;
    FTitle: string;
  public
    property Id;
    property AccessKey: string read FAccess_key write FAccess_key;
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property Thumb: TVkAlbumThumb read FThumb write FThumb;
    property Title: string read FTitle write FTitle;
    destructor Destroy; override;
  end;

  TVkAudioAds = class
  private
    FAccount_age_type: string;
    FContent_id: string;
    FDuration: string;
    FPuid22: string;
  public
    property AccountAgeType: string read FAccount_age_type write FAccount_age_type;
    property ContentId: string read FContent_id write FContent_id;
    property Duration: string read FDuration write FDuration;
    property PUID22: string read FPuid22 write FPuid22;
  end;

  TVkAudioChartInfo = class
  private
    FPosition: Integer;
  public
    property Position: Integer read FPosition write FPosition;
  end;

  TVkAudio = class(TVkObject, IAttachment)
  private
    FAccess_key: string;
    FAds: TVkAudioAds;
    FAlbum: TVkAudioAlbum;
    FArtist: string;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDuration: Integer;
    [JsonReflectAttribute(ctString, rtString, TAudioGenreInterceptor)]
    FGenre_id: TVkAudioGenre;
    FIs_licensed: Boolean;
    FMain_artists: TArray<TVkAudioArtist>;
    FOwner_id: TVkPeerId;
    FTitle: string;
    FTrack_code: string;
    FUrl: string;
    FLyrics_id: Int64;
    FAlbum_id: Int64;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FNo_search: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_hq: Boolean;
    FContent_restricted: Integer;
    FAudio_chart_info: TVkAudioChartInfo;
    FIs_explicit: Boolean;
    FStories_allowed: Boolean;
    FShort_videos_allowed: Boolean;
    FIs_focus_track: Boolean;
    FStories_cover_allowed: Boolean;
    FFeatured_artists: TArray<TVkAudioArtist>;
    FSubtitle: string;
    [JsonReflectAttribute(ctString, rtString, TAudioGenreInterceptor)]
    FTrack_genre_id: TVkAudioGenre;
  public
    /// <summary>
    /// Идентификатор аудиозаписи
    /// </summary>
    property Id;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Информация о рекламе
    /// </summary>
    property Ads: TVkAudioAds read FAds write FAds;
    /// <summary>
    /// Альбом аудиозаписи
    /// </summary>
    property Album: TVkAudioAlbum read FAlbum write FAlbum;
    /// <summary>
    /// Идентификатор альбома, в котором находится аудиозапись (если присвоен)
    /// </summary>
    property AlbumId: Int64 read FAlbum_id write FAlbum_id;
    /// <summary>
    /// Исполнитель
    /// </summary>
    property Artist: string read FArtist write FArtist;
    /// <summary>
    /// Информация о позиции в Чарте
    /// </summary>
    property AudioChartInfo: TVkAudioChartInfo read FAudio_chart_info write FAudio_chart_info;
    /// <summary>
    /// Доступна ли аудиозапись
    /// </summary>
    property ContentRestricted: Integer read FContent_restricted write FContent_restricted;
    /// <summary>
    /// Дата добавления
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Длительность аудиозаписи в секундах
    /// </summary>
    property Duration: Integer read FDuration write FDuration;
    /// <summary>
    /// Список второстепенных исполнителей
    /// </summary>
    property FeaturedArtists: TArray<TVkAudioArtist> read FFeatured_artists write FFeatured_artists;
    /// <summary>
    /// Идентификатор жанра из списка аудио жанров
    /// </summary>
    property Genre: TVkAudioGenre read FGenre_id write FGenre_id;
    /// <summary>
    /// Идентификатор жанра из списка аудио жанров
    /// </summary>
    property TrackGenreId: TVkAudioGenre read FTrack_genre_id write FTrack_genre_id;
    /// <summary>
    /// Содержит ли трек ненормативную лексику
    /// </summary>
    property IsExplicit: Boolean read FIs_explicit write FIs_explicit;
    /// <summary>
    /// [Не документирован]
    /// </summary>
    property IsFocusTrack: Boolean read FIs_focus_track write FIs_focus_track;
    /// <summary>
    /// True, если аудио в высоком качестве
    /// </summary>
    property IsHQ: Boolean read FIs_hq write FIs_hq;
    /// <summary>
    /// True, если аудиозапись лицензируется
    /// </summary>
    property IsLicensed: Boolean read FIs_licensed write FIs_licensed;
    /// <summary>
    /// Идентификатор текста аудиозаписи (если доступно)
    /// </summary>
    property LyricsId: Int64 read FLyrics_id write FLyrics_id;
    /// <summary>
    /// Список главных исполнителей
    /// </summary>
    property MainArtists: TArray<TVkAudioArtist> read FMain_artists write FMain_artists;
    /// <summary>
    /// True, если включена опция «Не выводить при поиске». Если опция отключена, поле не возвращается
    /// </summary>
    property NoSearch: Boolean read FNo_search write FNo_search;
    /// <summary>
    /// Идентификатор владельца аудиозаписи
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    /// <summary>
    /// Возможно ли использование этого трека в "Клипах"
    /// </summary>
    property ShortVideosAllowed: Boolean read FShort_videos_allowed write FShort_videos_allowed;
    /// <summary>
    /// Возможно ли использование этого трека в "Историях"
    /// </summary>
    property StoriesAllowed: Boolean read FStories_allowed write FStories_allowed;
    /// <summary>
    /// Возможно ли использование обложки этого трека в "Историях"
    /// </summary>
    property StoriesCoverAllowed: Boolean read FStories_cover_allowed write FStories_cover_allowed;
    /// <summary>
    /// Подзаголовок композиции
    /// </summary>
    property Subtitle: string read FSubtitle write FSubtitle;
    /// <summary>
    /// Название композиции
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// [Не документирован]
    /// </summary>
    property TrackCode: string read FTrack_code write FTrack_code;
    /// <summary>
    /// Ссылка на аудиозапись (привязана к ip-адресу клиентского приложения)
    /// </summary>
    property Url: string read FUrl write FUrl;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
    function DurationText(const AFormat: string): string;
  end;

  TVkAudioIndex = record
    OwnerId: TVkPeerId;
    AudioId: Int64;
  end;

  TVkAudioIndexes = TArray<TVkAudioIndex>;

  TVkAudios = class(TVkEntityList<TVkAudio>);

  TVkAudioInfo = class(TVkEntity)
  private
    FAudio_id: Int64;
  public
    property AudioId: Int64 read FAudio_id write FAudio_id;
  end;

  TVkAudioInfoItems = TVkEntityList<TVkAudioInfo>;

implementation

uses
  VK.CommonUtils;

{TVkAudio}

destructor TVkAudio.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAudioArtist>(FMain_artists);
  TArrayHelp.FreeArrayOfObject<TVkAudioArtist>(FFeatured_artists);
  if Assigned(FAudio_chart_info) then
    FAudio_chart_info.Free;
  if Assigned(FAlbum) then
    FAlbum.Free;
  if Assigned(FAds) then
    FAds.Free;
  inherited;
end;

function TVkAudio.DurationText(const AFormat: string): string;
var
  M, S: Integer;
begin
  S := Trunc(Duration);
  M := S div 60;
  S := S mod 60;
  Result := Format(AFormat, [M, S]);
end;

function TVkAudio.ToAttachment: TAttachment;
begin
  Result := TAttachment.Audio(OwnerId, Id, AccessKey);
end;

{TVkAudioAlbum}

destructor TVkAudioAlbum.Destroy;
begin
  if Assigned(FThumb) then
    FThumb.Free;
  inherited;
end;

{ TVkAudioArtist }

destructor TVkAudioArtist.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FPhoto);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkAudioArtistPhoto>(FPhotos);
  inherited;
end;

{ TVkAudioArtistPhoto }

destructor TVkAudioArtistPhoto.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FPhoto);
  inherited;
end;

end.

