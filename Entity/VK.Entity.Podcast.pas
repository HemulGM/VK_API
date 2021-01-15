unit VK.Entity.Podcast;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Podcast.Episode, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common;

type
  TVkPodcast = class
  private
    FOwner_id: Integer;
    FOwner_title: string;
    FUrl: string;
  public
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property OwnerTitle: string read FOwner_title write FOwner_title;
    property Url: string read FUrl write FUrl;
  end;

  TVkPodcasts = class(TVkEntity)
  private
    FItems: TArray<TVkPodcast>;
  public
    property Items: TArray<TVkPodcast> read FItems write FItems;
    destructor Destroy; override;
  end;

  TVkPodcastSearch = class(TVkEntity)
  private
    FPodcasts: TArray<TVkPodcast>;
    FEpisodes: TArray<TVkPodcastsEpisode>;
    FProfiles: TArray<TVkProfile>;
    FGroup: TArray<TVkGroup>;
  public
    property Podcasts: TArray<TVkPodcast> read FPodcasts write FPodcasts;
    property Episodes: TArray<TVkPodcastsEpisode> read FEpisodes write FEpisodes;
    property Group: TArray<TVkGroup> read FGroup write FGroup;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkPodcasts}

destructor TVkPodcasts.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPodcast>(FItems);
  inherited;
end;

{ TVkPodcastSearch }

destructor TVkPodcastSearch.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPodcast>(FPodcasts);
  TArrayHelp.FreeArrayOfObject<TVkPodcastsEpisode>(FEpisodes);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroup);
  inherited;
end;

end.

