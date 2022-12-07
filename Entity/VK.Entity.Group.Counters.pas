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
    FClips: Integer;
    FClips_followers: Integer;
    FClips_views: Integer;
    FClips_likes: Integer;
  public
    property Addresses: Integer read FAddresses write FAddresses;
    property Albums: Integer read FAlbums write FAlbums;
    property Audios: Integer read FAudios write FAudios;
    property AudioPlaylists: Integer read FAudio_playlists write FAudio_playlists;
    property Market: Integer read FMarket write FMarket;
    property Photos: Integer read FPhotos write FPhotos;
    property Topics: Integer read FTopics write FTopics;
    property Videos: Integer read FVideos write FVideos;
    property Articles: Integer read FArticles write FArticles;
    property Narratives: Integer read FNarratives write FNarratives;
    property Docs: Integer read FDocs write FDocs;
    property Clips: Integer read FClips write FClips;
    property ClipsFollowers: Integer read FClips_followers write FClips_followers;
    property ClipsViews: Integer read FClips_views write FClips_views;
    property ClipsLikes: Integer read FClips_likes write FClips_likes;
  end;

implementation

end.

