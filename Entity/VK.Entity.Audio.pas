unit VK.Entity.Audio;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors,
  System.SysUtils, Rest.Json, System.Json, VK.Entity.Common, VK.Types,
  VK.Entity.Common.List, VK.Wrap.Interceptors, VK.Entity.Group;

type
  TVkAudioArtistPhoto = class(TVkEntity)
  private
    FType: string;
    FPhoto: TArray<TVkImage>;
  public
    property &Type: string read FType write FType;
    property Photo: TArray<TVkImage> read FPhoto write FPhoto;
    destructor Destroy; override;
  end;

  TVkAudioArtist = class(TVkEntity)
  private
    FDomain: string;
    FName: string;
    FPhoto: TArray<TVkImage>;
    FId: string;
    FPages: TArray<Integer>;
    FGroups: TArray<TVkGroup>;
    FPhotos: TArray<TVkAudioArtistPhoto>;
  public
    /// <summary>
    /// ������������� �������/������
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// �����-������ �� �������
    /// </summary>
    property Domain: string read FDomain write FDomain;
    /// <summary>
    /// ��� �������/�������� ������
    /// </summary>
    property Name: string read FName write FName;
    property Photo: TArray<TVkImage> read FPhoto write FPhoto;
    property Photos: TArray<TVkAudioArtistPhoto> read FPhotos write FPhotos;
    property Pages: TArray<Integer> read FPages write FPages;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    destructor Destroy; override;
  end;

  TVkAudioArtists = TVkEntityList<TVkAudioArtist>;

  TVkAudioAlbum = class(TVkObject)
  private
    FAccess_key: string;
    FOwner_id: Integer;
    FThumb: TVkThumb;
    FTitle: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Thumb: TVkThumb read FThumb write FThumb;
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
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDuration: Integer;
    [JsonReflectAttribute(ctString, rtString, TAudioGenreInterceptor)]
    FGenre_id: TVkAudioGenre;
    FIs_licensed: Boolean;
    FMain_artists: TArray<TVkAudioArtist>;
    FOwner_id: Integer;
    FTitle: string;
    FTrack_code: string;
    FUrl: string;
    FLyrics_id: Integer;
    FAlbum_id: Integer;
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
    /// ������������� �����������
    /// </summary>
    property Id;
    /// <summary>
    /// ���� �������
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ���������� � �������
    /// </summary>
    property Ads: TVkAudioAds read FAds write FAds;
    /// <summary>
    /// ������ �����������
    /// </summary>
    property Album: TVkAudioAlbum read FAlbum write FAlbum;
    /// <summary>
    /// ������������� �������, � ������� ��������� ����������� (���� ��������)
    /// </summary>
    property AlbumId: Integer read FAlbum_id write FAlbum_id;
    /// <summary>
    /// �����������
    /// </summary>
    property Artist: string read FArtist write FArtist;
    /// <summary>
    /// ���������� � ������� � �����
    /// </summary>
    property AudioChartInfo: TVkAudioChartInfo read FAudio_chart_info write FAudio_chart_info;
    /// <summary>
    /// �������� �� �����������
    /// </summary>
    property ContentRestricted: Integer read FContent_restricted write FContent_restricted;
    /// <summary>
    /// ���� ����������
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// ������������ ����������� � ��������
    /// </summary>
    property Duration: Integer read FDuration write FDuration;
    /// <summary>
    /// ������ �������������� ������������
    /// </summary>
    property FeaturedArtists: TArray<TVkAudioArtist> read FFeatured_artists write FFeatured_artists;
    /// <summary>
    /// ������������� ����� �� ������ ����� ������
    /// </summary>
    property Genre: TVkAudioGenre read FGenre_id write FGenre_id;
    /// <summary>
    /// ������������� ����� �� ������ ����� ������
    /// </summary>
    property TrackGenreId: TVkAudioGenre read FTrack_genre_id write FTrack_genre_id;
    /// <summary>
    /// �������� �� ���� ������������� �������
    /// </summary>
    property IsExplicit: Boolean read FIs_explicit write FIs_explicit;
    /// <summary>
    /// [�� ��������������]
    /// </summary>
    property IsFocusTrack: Boolean read FIs_focus_track write FIs_focus_track;
    /// <summary>
    /// True, ���� ����� � ������� ��������
    /// </summary>
    property IsHQ: Boolean read FIs_hq write FIs_hq;
    /// <summary>
    /// True, ���� ����������� �������������
    /// </summary>
    property IsLicensed: Boolean read FIs_licensed write FIs_licensed;
    /// <summary>
    /// ������������� ������ ����������� (���� ��������)
    /// </summary>
    property LyricsId: Integer read FLyrics_id write FLyrics_id;
    /// <summary>
    /// ������ ������� ������������
    /// </summary>
    property MainArtists: TArray<TVkAudioArtist> read FMain_artists write FMain_artists;
    /// <summary>
    /// True, ���� �������� ����� ��� �������� ��� ������. ���� ����� ���������, ���� �� ������������
    /// </summary>
    property NoSearch: Boolean read FNo_search write FNo_search;
    /// <summary>
    /// ������������� ��������� �����������
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    /// <summary>
    /// �������� �� ������������� ����� ����� � "������"
    /// </summary>
    property ShortVideosAllowed: Boolean read FShort_videos_allowed write FShort_videos_allowed;
    /// <summary>
    /// �������� �� ������������� ����� ����� � "��������"
    /// </summary>
    property StoriesAllowed: Boolean read FStories_allowed write FStories_allowed;
    /// <summary>
    /// �������� �� ������������� ������� ����� ����� � "��������"
    /// </summary>
    property StoriesCoverAllowed: Boolean read FStories_cover_allowed write FStories_cover_allowed;
    /// <summary>
    /// ������������ ����������
    /// </summary>
    property Subtitle: string read FSubtitle write FSubtitle;
    /// <summary>
    /// �������� ����������
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// [�� ��������������]
    /// </summary>
    property TrackCode: string read FTrack_code write FTrack_code;
    /// <summary>
    /// ������ �� ����������� (��������� � ip-������ ����������� ����������)
    /// </summary>
    property Url: string read FUrl write FUrl;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
    function DurationText(const AFormat: string): string;
  end;

  TVkAudioIndex = record
    OwnerId: Integer;
    AudioId: Integer;
  end;

  TVkAudioIndexes = TArray<TVkAudioIndex>;

  TVkAudios = TVkEntityList<TVkAudio>;

  TVkAudioInfo = class(TVkEntity)
  private
    FAudio_id: Integer;
  public
    property AudioId: Integer read FAudio_id write FAudio_id;
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
  TArrayHelp.FreeArrayOfObject<TVkImage>(FPhoto);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkAudioArtistPhoto>(FPhotos);
  inherited;
end;

{ TVkAudioArtistPhoto }

destructor TVkAudioArtistPhoto.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkImage>(FPhoto);
  inherited;
end;

end.

