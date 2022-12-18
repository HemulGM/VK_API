unit VK.Entity.Audio.Catalog;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Audio, VK.Entity.Playlist,
  VK.Entity.Common, VK.Entity.Group, VK.Entity.Profile,
  VK.Entity.Catalog.Section, VK.Entity.Common.List,
  VK.Entity.Common.ExtendedList, VK.Entity.Photo;

type
  TVkAudioCatalogItem = class(TVkEntityList<TVkCatalogLink>)
  private
    FAudios: TArray<TVkAudio>;
    FId: string;
    FNext_from: string;
    FSource: string;
    FSubtitle: string;
    FThumbs: TArray<TVkPhoto>;
    FTitle: string;
    FType: string;
    FPlaylists: TArray<TVkAudioPlaylist>;
  public
    property Audios: TArray<TVkAudio> read FAudios write FAudios;
    property Id: string read FId write FId;
    property NextFrom: string read FNext_from write FNext_from;
    property Playlists: TArray<TVkAudioPlaylist> read FPlaylists write FPlaylists;
    property Source: string read FSource write FSource;
    property Subtitle: string read FSubtitle write FSubtitle;
    property Thumbs: TArray<TVkPhoto> read FThumbs write FThumbs;
    property Title: string read FTitle write FTitle;
    property &Type: string read FType write FType;
    destructor Destroy; override;
  end;

  TVkAudioCatalog = TVkEntityExtendedList<TVkAudioCatalogItem>;

implementation

uses
  VK.CommonUtils;

{TVkAudioCatalogItem}

destructor TVkAudioCatalogItem.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAudio>(FAudios);
  TArrayHelp.FreeArrayOfObject<TVkPhoto>(FThumbs);
  TArrayHelp.FreeArrayOfObject<TVkAudioPlaylist>(FPlaylists);
  inherited;
end;

end.

