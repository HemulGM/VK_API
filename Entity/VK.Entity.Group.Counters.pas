unit VK.Entity.Group.Counters;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkGroupCounters = class
  private
    FAlbums: Integer;
    FArticles: Integer;
    FAudio_playlists: Integer;
    FAudios: Integer;
    FMarket: Integer;
    FPhotos: Integer;
    FTopics: Integer;
    FVideos: Integer;
    FDocs: Integer;
    FNarratives: Integer;
    FAddresses: Integer;
  public
    property Albums: Integer read FAlbums write FAlbums;
    property Articles: Integer read FArticles write FArticles;
    property AudioPlaylists: Integer read FAudio_playlists write FAudio_playlists;
    property Audios: Integer read FAudios write FAudios;
    property Market: Integer read FMarket write FMarket;
    property Photos: Integer read FPhotos write FPhotos;
    property Topics: Integer read FTopics write FTopics;
    property Videos: Integer read FVideos write FVideos;
    property Docs: Integer read FDocs write FDocs;
    property Narratives: Integer read FNarratives write FNarratives;
    property Addresses: Integer read FAddresses write FAddresses;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupCounters;
  end;

implementation

{TVkGroupCounters}

function TVkGroupCounters.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupCounters.FromJsonString(AJsonString: string): TVkGroupCounters;
begin
  result := TJson.JsonToObject<TVkGroupCounters>(AJsonString)
end;

end.

