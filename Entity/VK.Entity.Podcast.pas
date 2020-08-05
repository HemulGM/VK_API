unit VK.Entity.Podcast;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Podcast.Episode, VK.Entity.Profile, VK.Entity.Group;

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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPodcast;
  end;

  TVkPodcasts = class
  private
    FItems: TArray<TVkPodcast>;
  public
    property Items: TArray<TVkPodcast> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPodcasts;
  end;

  TVkPodcastSearch = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPodcastSearch;
  end;

implementation

{TVkPodcast}

function TVkPodcast.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPodcast.FromJsonString(AJsonString: string): TVkPodcast;
begin
  result := TJson.JsonToObject<TVkPodcast>(AJsonString)
end;

{TVkPodcasts}

destructor TVkPodcasts.Destroy;
var
  LresponseItem: TVkPodcast;
begin

  for LresponseItem in FItems do
    LresponseItem.Free;

  inherited;
end;

function TVkPodcasts.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPodcasts.FromJsonString(AJsonString: string): TVkPodcasts;
begin
  result := TJson.JsonToObject<TVkPodcasts>(AJsonString)
end;

{ TVkPodcastSearch }

destructor TVkPodcastSearch.Destroy;
begin
  for var Item in FPodcasts do
    Item.Free;
  for var Item in FEpisodes do
    Item.Free;
  for var Item in FProfiles do
    Item.Free;
  for var Item in FGroup do
    Item.Free;
  inherited;
end;

class function TVkPodcastSearch.FromJsonString(AJsonString: string): TVkPodcastSearch;
begin
  result := TJson.JsonToObject<TVkPodcastSearch>(AJsonString)
end;

function TVkPodcastSearch.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

