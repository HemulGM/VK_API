unit VK.Entity.Catalog;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo, VK.Entity.Common,
  VK.Entity.Group, VK.Entity.Audio, VK.Entity.Playlist, VK.Entity.Profile;

type
  TVkCatalogIcon = class
  private
    FHeight: Integer;
    FUrl: string;
    FWidth: Integer;
  public
    property Height: Integer read FHeight write FHeight;
    property Url: string read FUrl write FUrl;
    property Width: Integer read FWidth write FWidth;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogIcon;
  end;

  TVkCatalogAction = class
  private
    FConsume_reason: string;
    FTarget: string;
    FType: string;
    FUrl: string;
  public
    property ConsumeReason: string read FConsume_reason write FConsume_reason;
    property Target: string read FTarget write FTarget;
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogAction;
  end;

  TVkCatalogButton = class
  private
    FAction: TVkCatalogAction;
    FOwner_id: Integer;
    FTitle: string;
    FBlock_id: string;
    FRef_data_type: string;
    FRef_items_count: Extended;
    FRef_layout_name: string;
  public
    property Action: TVkCatalogAction read FAction write FAction;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Title: string read FTitle write FTitle;
    property block_id: string read FBlock_id write FBlock_id;
    property ref_data_type: string read FRef_data_type write FRef_data_type;
    property ref_items_count: Extended read FRef_items_count write FRef_items_count;
    property ref_layout_name: string read FRef_layout_name write FRef_layout_name;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogButton;
  end;

  TVkCatalogPlaceholder = class
  private
    FButtons: TArray<TVkCatalogButton>;
    FIcons: TArray<TVkCatalogIcon>;
    FId: string;
    FImage_mode: string;
    FText: string;
    FTitle: string;
  public
    property Buttons: TArray<TVkCatalogButton> read FButtons write FButtons;
    property Icons: TArray<TVkCatalogIcon> read FIcons write FIcons;
    property Id: string read FId write FId;
    property ImageMode: string read FImage_mode write FImage_mode;
    property Text: string read FText write FText;
    property Title: string read FTitle write FTitle;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogPlaceholder;
  end;

  TVkCatalogLayout = class
  private
    FName: string;
    FOwner_id: Integer;
    FSubtitle: string;
    FTitle: string;
  public
    property Name: string read FName write FName;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property SubTitle: string read FSubtitle write FSubtitle;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogLayout;
  end;

  TVkCatalogBlock = class
  private
    FData_type: string;
    FId: string;
    FLayout: TVkCatalogLayout;
    FPlaceholder_ids: TArray<string>;
    FButtons: TArray<TVkCatalogButton>;
    FNext_from: string;
    FThumbs_ids: TArray<string>;
    FAudios_ids: TArray<string>;
  public
    property DataType: string read FData_type write FData_type;
    property Id: string read FId write FId;
    property Layout: TVkCatalogLayout read FLayout write FLayout;
    property PlaceholderIds: TArray<string> read FPlaceholder_ids write FPlaceholder_ids;
    property Buttons: TArray<TVkCatalogButton> read FButtons write FButtons;
    property NextFrom: string read FNext_from write FNext_from;
    property ThumbsIds: TArray<string> read FThumbs_ids write FThumbs_ids;
    property AudiosIds: TArray<string> read FAudios_ids write FAudios_ids;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogBlock;
  end;

  TVkCatalogSection = class
  private
    FBlocks: TArray<TVkCatalogBlock>;
    FId: string;
    FListen_events: TArray<string>;
    FNext_from: string;
    FTitle: string;
    FUrl: string;
  public
    property Blocks: TArray<TVkCatalogBlock> read FBlocks write FBlocks;
    property Id: string read FId write FId;
    property ListenEvents: TArray<string> read FListen_events write FListen_events;
    property NextFrom: string read FNext_from write FNext_from;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;
    function FindBlock(DataType, LayoutName: string): Integer;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogSection;
  end;

  TVkCatalogItem = class
  private
    FDefault_section: string;
    FSections: TArray<TVkCatalogSection>;
  public
    property DefaultSection: string read FDefault_section write FDefault_section;
    property Sections: TArray<TVkCatalogSection> read FSections write FSections;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalogItem;
  end;

  TVkCatalog = class
  private
    FAudios: TArray<TVkAudio>;
    FCatalog: TVkCatalogItem;
    FGroups: TArray<TVkGroup>;
    FPlaceholders: TArray<TVkCatalogPlaceholder>;
    FPlaylists: TArray<TVkAudioPlaylist>;
    FProfiles: TArray<TVkProfile>;
  public
    property Audios: TArray<TVkAudio> read FAudios write FAudios;
    property Catalog: TVkCatalogItem read FCatalog write FCatalog;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Placeholders: TArray<TVkCatalogPlaceholder> read FPlaceholders write FPlaceholders;
    property Playlists: TArray<TVkAudioPlaylist> read FPlaylists write FPlaylists;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCatalog;
  end;

implementation

uses
  VK.CommonUtils;

{TVkCatalogButton}

constructor TVkCatalogButton.Create;
begin
  inherited;
  FAction := TVkCatalogAction.Create();
end;

destructor TVkCatalogButton.Destroy;
begin
  FAction.Free;
  inherited;
end;

function TVkCatalogButton.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogButton.FromJsonString(AJsonString: string): TVkCatalogButton;
begin
  result := TJson.JsonToObject<TVkCatalogButton>(AJsonString)
end;

{TVkCatalogIcon}

function TVkCatalogIcon.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogIcon.FromJsonString(AJsonString: string): TVkCatalogIcon;
begin
  result := TJson.JsonToObject<TVkCatalogIcon>(AJsonString)
end;

{TVkCatalogAction}

function TVkCatalogAction.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogAction.FromJsonString(AJsonString: string): TVkCatalogAction;
begin
  result := TJson.JsonToObject<TVkCatalogAction>(AJsonString)
end;

{TVkCatalogPlaceholder}

destructor TVkCatalogPlaceholder.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCatalogButton>(FButtons);
  TArrayHelp.FreeArrayOfObject<TVkCatalogIcon>(FIcons);
  inherited;
end;

function TVkCatalogPlaceholder.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogPlaceholder.FromJsonString(AJsonString: string): TVkCatalogPlaceholder;
begin
  result := TJson.JsonToObject<TVkCatalogPlaceholder>(AJsonString)
end;

{TVkCatalogLayout}

function TVkCatalogLayout.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogLayout.FromJsonString(AJsonString: string): TVkCatalogLayout;
begin
  result := TJson.JsonToObject<TVkCatalogLayout>(AJsonString)
end;

{TVkCatalogBlock}

constructor TVkCatalogBlock.Create;
begin
  inherited;
  FLayout := TVkCatalogLayout.Create();
end;

destructor TVkCatalogBlock.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCatalogButton>(FButtons);
  FLayout.Free;
  inherited;
end;

function TVkCatalogBlock.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogBlock.FromJsonString(AJsonString: string): TVkCatalogBlock;
begin
  result := TJson.JsonToObject<TVkCatalogBlock>(AJsonString)
end;

{TVkCatalogSection}

destructor TVkCatalogSection.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCatalogBlock>(FBlocks);
  inherited;
end;

function TVkCatalogSection.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

function TVkCatalogSection.FindBlock(DataType, LayoutName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(FBlocks) to High(FBlocks) do
    if FBlocks[i].DataType = DataType then
    begin
      if LayoutName <> '' then
        if Assigned(FBlocks[i].Layout) and (FBlocks[i].Layout.Name = LayoutName) then
          Exit(i);
    end;
end;

class function TVkCatalogSection.FromJsonString(AJsonString: string): TVkCatalogSection;
begin
  result := TJson.JsonToObject<TVkCatalogSection>(AJsonString)
end;

{TVkCatalogItem}

destructor TVkCatalogItem.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCatalogSection>(FSections);
  inherited;
end;

function TVkCatalogItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalogItem.FromJsonString(AJsonString: string): TVkCatalogItem;
begin
  result := TJson.JsonToObject<TVkCatalogItem>(AJsonString)
end;

{TVkCatalog}

constructor TVkCatalog.Create;
begin
  inherited;
  FCatalog := TVkCatalogItem.Create();
end;

destructor TVkCatalog.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkAudio>(FAudios);
  TArrayHelp.FreeArrayOfObject<TVkCatalogPlaceholder>(FPlaceholders);
  TArrayHelp.FreeArrayOfObject<TVkAudioPlaylist>(FPlaylists);
  FCatalog.Free;
  inherited;
end;

function TVkCatalog.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCatalog.FromJsonString(AJsonString: string): TVkCatalog;
begin
  result := TJson.JsonToObject<TVkCatalog>(AJsonString)
end;

end.

