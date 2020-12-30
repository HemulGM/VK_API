unit VK.Entity.Audio.Catalog;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Audio, VK.Entity.Playlist,
  VK.Entity.Group, VK.Entity.Profile, VK.Entity.Catalog.Section;

type
  TVkAudioCatalogItem = class
  private
    FAudios: TArray<TVkAudio>;
    FCount: Integer;
    FId: string;
    FNext_from: string;
    FSource: string;
    FSubtitle: string;
    FThumbs: TArray<TVkAlbumThumb>;
    FTitle: string;
    FType: string;
    FPlaylists: TArray<TVkAudioPlaylist>;
    FItems: TArray<TVkCatalogLink>;
  public
    property Audios: TArray<TVkAudio> read FAudios write FAudios;
    property Playlists: TArray<TVkAudioPlaylist> read FPlaylists write FPlaylists;
    property Items: TArray<TVkCatalogLink> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property Id: string read FId write FId;
    property NextFrom: string read FNext_from write FNext_from;
    property Source: string read FSource write FSource;
    property Subtitle: string read FSubtitle write FSubtitle;
    property Thumbs: TArray<TVkAlbumThumb> read FThumbs write FThumbs;
    property Title: string read FTitle write FTitle;
    property&Type: string read FType write FType;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioCatalogItem;
  end;

  TVkAudioCatalog = class
  private
    FGroups: TArray<TVkGroup>;
    FItems: TArray<TVkAudioCatalogItem>;
    FProfiles: TArray<TVkProfile>;
  public
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Items: TArray<TVkAudioCatalogItem> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioCatalog;
  end;

implementation

uses
  VK.CommonUtils;

{TVkAudioCatalogItem}

destructor TVkAudioCatalogItem.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAudio>(FAudios);
  TArrayHelp.FreeArrayOfObject<TVkAlbumThumb>(FThumbs);
  TArrayHelp.FreeArrayOfObject<TVkAudioPlaylist>(FPlaylists);
  TArrayHelp.FreeArrayOfObject<TVkCatalogLink>(FItems);
  inherited;
end;

function TVkAudioCatalogItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioCatalogItem.FromJsonString(AJsonString: string): TVkAudioCatalogItem;
begin
  result := TJson.JsonToObject<TVkAudioCatalogItem>(AJsonString)
end;

{TVkAudioCatalog}

destructor TVkAudioCatalog.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkAudioCatalogItem>(FItems);
  inherited;
end;

function TVkAudioCatalog.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioCatalog.FromJsonString(AJsonString: string): TVkAudioCatalog;
begin
  result := TJson.JsonToObject<TVkAudioCatalog>(AJsonString)
end;

end.

