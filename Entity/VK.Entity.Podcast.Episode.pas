unit VK.Entity.Podcast.Episode;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Common;

type
  TVkPodcastCover = class
  private
    FSizes: TVkSizes;
  public
    property Sizes: TVkSizes read FSizes write FSizes;
    destructor Destroy; override;
  end;

  TVkPodcastInfo = class(TVkEntity)
  private
    FCover: TVkPodcastCover;
    FDescription: string;
    FIs_favorite: Boolean;
    FPlays: Integer;
    FPosition: Integer;
  public
    property Cover: TVkPodcastCover read FCover write FCover;
    property Description: string read FDescription write FDescription;
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    property Plays: Integer read FPlays write FPlays;
    property Position: Integer read FPosition write FPosition;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkPodcastsEpisode = class(TVkObject)
  private
    FArtist: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FDuration: Integer;
    FIs_explicit: Boolean;
    FIs_focus_track: Boolean;
    FLyrics_id: Integer;
    FNo_search: Boolean;
    FOwner_id: Integer;
    FPodcast_info: TVkPodcastInfo;
    FShort_videos_allowed: Boolean;
    FStories_allowed: Boolean;
    FTitle: string;
    FTrack_code: string;
    FUrl: string;
  public
    property Artist: string read FArtist write FArtist;
    property Date: TDateTime read FDate write FDate;
    property Duration: Integer read FDuration write FDuration;
    property IsExplicit: Boolean read FIs_explicit write FIs_explicit;
    property IsFocusTrack: Boolean read FIs_focus_track write FIs_focus_track;
    property LyricsId: Integer read FLyrics_id write FLyrics_id;
    property NoSearch: Boolean read FNo_search write FNo_search;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PodcastInfo: TVkPodcastInfo read FPodcast_info write FPodcast_info;
    property ShortVideosAllowed: Boolean read FShort_videos_allowed write FShort_videos_allowed;
    property StoriesAllowed: Boolean read FStories_allowed write FStories_allowed;
    property Title: string read FTitle write FTitle;
    property TrackCode: string read FTrack_code write FTrack_code;
    property Url: string read FUrl write FUrl;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkPodcastCover}

destructor TVkPodcastCover.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
  inherited;
end;

{TVkPodcastInfo}

constructor TVkPodcastInfo.Create;
begin
  inherited;
  FCover := TVkPodcastCover.Create();
end;

destructor TVkPodcastInfo.Destroy;
begin
  FCover.Free;
  inherited;
end;

{TVkPodcastsEpisode}

constructor TVkPodcastsEpisode.Create;
begin
  inherited;
  FPodcast_info := TVkPodcastInfo.Create();
end;

destructor TVkPodcastsEpisode.Destroy;
begin
  FPodcast_info.Free;
  inherited;
end;

end.

