unit VK.Entity.Catalog.Section;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Audio, VK.Entity.Album, VK.Entity.Group,
  VK.Entity.Playlist, VK.Entity.Catalog;

type
  TVkCatalogThumb = class(TVkThumb)
  private
    FId: string;
  public
    property Id: string read FId write FId;
  end;

  TVkLinkMeta = class
  private
    FContent_type: string;
    FIcon: string;
    FTrack_code: string;
  public
    property ContentType: string read FContent_type write FContent_type;
    property Icon: string read FIcon write FIcon;
    property TrackCode: string read FTrack_code write FTrack_code;
  end;

  TVkCatalogLink = class(TVkEntity)
  private
    FId: string;
    FImage: TArray<TVkImage>;
    FMeta: TVkLinkMeta;
    FSubtitle: string;
    FTitle: string;
    FUrl: string;
  public
    property Id: string read FId write FId;
    property Image: TArray<TVkImage> read FImage write FImage;
    property Meta: TVkLinkMeta read FMeta write FMeta;
    property Subtitle: string read FSubtitle write FSubtitle;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkSectionData = class(TVkEntity)
  private
    FAudios: TArray<TVkAudio>;
    FGroups: TArray<TVkGroup>;
    FLinks: TArray<TVkCatalogLink>;
    FPlaylists: TArray<TVkAudioPlaylist>;
    FSection: TVkCatalogSection;
    FThumbs: TArray<TVkCatalogThumb>;
  public
    property Audios: TArray<TVkAudio> read FAudios write FAudios;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Links: TArray<TVkCatalogLink> read FLinks write FLinks;
    property Playlists: TArray<TVkAudioPlaylist> read FPlaylists write FPlaylists;
    property Section: TVkCatalogSection read FSection write FSection;
    property Thumbs: TArray<TVkCatalogThumb> read FThumbs write FThumbs;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkCatalogLink}

constructor TVkCatalogLink.Create;
begin
  inherited;
  FMeta := TVkLinkMeta.Create();
end;

destructor TVkCatalogLink.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkImage>(FImage);
  FMeta.Free;
  inherited;
end;

{TVkSectionAudio}

constructor TVkSectionData.Create;
begin
  inherited;
  FSection := TVkCatalogSection.Create();
end;

destructor TVkSectionData.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkCatalogLink>(FLinks);
  TArrayHelp.FreeArrayOfObject<TVkAudio>(FAudios);
  TArrayHelp.FreeArrayOfObject<TVkCatalogThumb>(FThumbs);
  TArrayHelp.FreeArrayOfObject<TVkAudioPlaylist>(FPlaylists);
  FSection.Free;
  inherited;
end;

end.

