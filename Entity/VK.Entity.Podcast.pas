unit VK.Entity.Podcast;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Podcast.Episode, VK.Entity.Profile,
  VK.Entity.Group, VK.Entity.Common, VK.Entity.Common.List, VK.Types;

type
  TVkPodcast = class(TVkEntity)
  private
    FOwner_id: TVkPeerId;
    FOwner_title: string;
    FUrl: string;
    FOwner_name: string;
    FCover: TVkPodcastCover;
    FTitle: string;
    FOwner_url: string;
  public
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property OwnerTitle: string read FOwner_title write FOwner_title;
    property OwnerName: string read FOwner_name write FOwner_name;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;
    property OwnerUrl: string read FOwner_url write FOwner_url;
    property Cover: TVkPodcastCover read FCover write FCover;
  end;

  TVkPodcasts = TVkEntityList<TVkPodcast>;

  TVkPodcastSearch = class(TVkEntity)
  private
    FPodcasts: TArray<TVkPodcast>;
    FEpisodes: TArray<TVkPodcastsEpisode>;
    FProfiles: TArray<TVkProfile>;
    FGroup: TArray<TVkGroup>;
    FResults_total: Integer;
  public
    property Podcasts: TArray<TVkPodcast> read FPodcasts write FPodcasts;
    property Episodes: TArray<TVkPodcastsEpisode> read FEpisodes write FEpisodes;
    property Group: TArray<TVkGroup> read FGroup write FGroup;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property ResultsTotal: Integer read FResults_total write FResults_total;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

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

