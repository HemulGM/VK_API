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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogThumb;
  end;

  TVkLinkMeta = class
  private
    FContent_type: string;
    FIcon: string;
  public
    property ContentType: string read FContent_type write FContent_type;
    property Icon: string read FIcon write FIcon;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkMeta;
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkImage;
  end;

  TVkCatalogLink = class
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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogLink;
  end;

  TVkSectionData = class
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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSectionData;
  end;

implementation

uses
  VK.CommonUtils;

{TVkCatalogThumb}

function TVkCatalogThumb.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogThumb.FromJsonString(AJsonString: string): TVkCatalogThumb;
begin
  result := TJson.JsonToObject<TVkCatalogThumb>(AJsonString)
end;

{TVkLinkMeta}

function TVkLinkMeta.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLinkMeta.FromJsonString(AJsonString: string): TVkLinkMeta;
begin
  result := TJson.JsonToObject<TVkLinkMeta>(AJsonString)
end;

{TVkLinkImage}

function TVkLinkImage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLinkImage.FromJsonString(AJsonString: string): TVkLinkImage;
begin
  result := TJson.JsonToObject<TVkLinkImage>(AJsonString)
end;

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

function TVkCatalogLink.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogLink.FromJsonString(AJsonString: string): TVkCatalogLink;
begin
  result := TJson.JsonToObject<TVkCatalogLink>(AJsonString)
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

function TVkSectionData.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSectionData.FromJsonString(AJsonString: string): TVkSectionData;
begin
  result := TJson.JsonToObject<TVkSectionData>(AJsonString)
end;

end.

