unit VK.Entity.Catalog.Section;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Audio, VK.Entity.Album, VK.Entity.Group,
  VK.Entity.Playlist, VK.Entity.Catalog;

type
  TVkCatalogThumb = class
  private
    FHeight: Integer;
    FWidth: Integer;
    FId: string;
    FPhoto_135: string;
    FPhoto_270: string;
    FPhoto_300: string;
    FPhoto_34: string;
    FPhoto_600: string;
    FPhoto_68: string;
  public
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property Id: string read FId write FId;
    property Photo135: string read FPhoto_135 write FPhoto_135;
    property Photo270: string read FPhoto_270 write FPhoto_270;
    property Photo300: string read FPhoto_300 write FPhoto_300;
    property Photo34: string read FPhoto_34 write FPhoto_34;
    property Photo600: string read FPhoto_600 write FPhoto_600;
    property Photo68: string read FPhoto_68 write FPhoto_68;
  end;

  TVkLinkMeta = class
  private
    FContent_type: string;
    FIcon: string;
  public
    property ContentType: string read FContent_type write FContent_type;
    property Icon: string read FIcon write FIcon;
  end;

  TVkLinkImage = class
  private
    FHeight: Integer;
    FUrl: string;
    FWidth: Integer;
  public
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property Url: string read FUrl write FUrl;
  end;

  TVkCatalogLink = class(TVkEntity)
  private
    FId: string;
    FImage: TArray<TVkLinkImage>;
    FMeta: TVkLinkMeta;
    FSubtitle: string;
    FTitle: string;
    FUrl: string;
  public
    property Id: string read FId write FId;
    property Image: TArray<TVkLinkImage> read FImage write FImage;
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
  TArrayHelp.FreeArrayOfObject<TVkLinkImage>(FImage);
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

