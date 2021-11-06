unit VK.Entity.Catalog;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo, VK.Entity.Common,
  VK.Entity.Group, VK.Entity.Audio, VK.Entity.Playlist, VK.Entity.Profile;

type
  TVkCatalogAction = class
  private
    FConsume_reason: string;
    FTarget: string;
    FType: string;
    FUrl: string;
  public
    property ConsumeReason: string read FConsume_reason write FConsume_reason;
    property Target: string read FTarget write FTarget;
    property &Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
  end;

  TVkCatalogButton = class(TVkEntity)
  private
    FAction: TVkCatalogAction;
    FOwner_id: Integer;
    FTitle: string;
    FBlock_id: string;
    FRef_data_type: string;
    FRef_items_count: Integer;
    FRef_layout_name: string;
    FSection_id: string;
  public
    property Action: TVkCatalogAction read FAction write FAction;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Title: string read FTitle write FTitle;
    property BlockId: string read FBlock_id write FBlock_id;
    property SectionId: string read FSection_id write FSection_id;
    property RefDataType: string read FRef_data_type write FRef_data_type;
    property RefItemsCount: Integer read FRef_items_count write FRef_items_count;
    property RefLayoutName: string read FRef_layout_name write FRef_layout_name;
    destructor Destroy; override;
  end;

  TVkCatalogPlaceholder = class
  private
    FButtons: TArray<TVkCatalogButton>;
    FIcons: TArray<TVkImage>;
    FId: string;
    FImage_mode: string;
    FText: string;
    FTitle: string;
  public
    property Buttons: TArray<TVkCatalogButton> read FButtons write FButtons;
    property Icons: TArray<TVkImage> read FIcons write FIcons;
    property Id: string read FId write FId;
    property ImageMode: string read FImage_mode write FImage_mode;
    property Text: string read FText write FText;
    property Title: string read FTitle write FTitle;
    destructor Destroy; override;
  end;

  TVkCatalogLayout = class
  private
    FName: string;
    FOwner_id: Integer;
    FSubtitle: string;
    FTitle: string;
    FIs_editable: Integer;
  public
    property Name: string read FName write FName;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property SubTitle: string read FSubtitle write FSubtitle;
    property Title: string read FTitle write FTitle;
    property IsEditable: Integer read FIs_editable write FIs_editable;
  end;

  TVkCatalogBlock = class(TVkEntity)
  private
    FData_type: string;
    FId: string;
    FLayout: TVkCatalogLayout;
    FPlaceholder_ids: TArray<string>;
    FButtons: TArray<TVkCatalogButton>;
    FNext_from: string;
    FThumbs_ids: TArray<string>;
    FAudios_ids: TArray<string>;
    FLinks_ids: TArray<string>;
    FPlaylists_ids: TArray<string>;
  public
    property DataType: string read FData_type write FData_type;
    property Id: string read FId write FId;
    property Layout: TVkCatalogLayout read FLayout write FLayout;
    property Buttons: TArray<TVkCatalogButton> read FButtons write FButtons;
    property NextFrom: string read FNext_from write FNext_from;
    property PlaceholderIds: TArray<string> read FPlaceholder_ids write FPlaceholder_ids;
    property ThumbsIds: TArray<string> read FThumbs_ids write FThumbs_ids;
    property AudiosIds: TArray<string> read FAudios_ids write FAudios_ids;
    property LinksIds: TArray<string> read FLinks_ids write FLinks_ids;
    property PlaylistsIds: TArray<string> read FPlaylists_ids write FPlaylists_ids;
    destructor Destroy; override;
  end;

  TVkCatalogSection = class
  private
    FBlocks: TArray<TVkCatalogBlock>;
    FId: string;
    FListen_events: TArray<string>;
    FNext_from: string;
    FTitle: string;
    FUrl: string;
    FButtons: TArray<TVkCatalogButton>;
  public
    property Buttons: TArray<TVkCatalogButton> read FButtons write FButtons;
    property Blocks: TArray<TVkCatalogBlock> read FBlocks write FBlocks;
    property Id: string read FId write FId;
    property ListenEvents: TArray<string> read FListen_events write FListen_events;
    property NextFrom: string read FNext_from write FNext_from;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;
    function FindBlock(DataType, LayoutName: string): Integer;
    destructor Destroy; override;
  end;

  TVkCatalogItem = class
  private
    FDefault_section: string;
    FSections: TArray<TVkCatalogSection>;
  public
    property DefaultSection: string read FDefault_section write FDefault_section;
    property Sections: TArray<TVkCatalogSection> read FSections write FSections;
    destructor Destroy; override;
  end;

  TVkCatalog = class(TVkEntity)
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
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkCatalogButton}

destructor TVkCatalogButton.Destroy;
begin
  if Assigned(FAction) then
    FAction.Free;
  inherited;
end;

{TVkCatalogPlaceholder}

destructor TVkCatalogPlaceholder.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCatalogButton>(FButtons);
  TArrayHelp.FreeArrayOfObject<TVkImage>(FIcons);
  inherited;
end;

{TVkCatalogBlock}

destructor TVkCatalogBlock.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCatalogButton>(FButtons);
  if Assigned(FLayout) then
    FLayout.Free;
  inherited;
end;

{TVkCatalogSection}

destructor TVkCatalogSection.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCatalogBlock>(FBlocks);
  TArrayHelp.FreeArrayOfObject<TVkCatalogButton>(FButtons);
  inherited;
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

{TVkCatalogItem}

destructor TVkCatalogItem.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkCatalogSection>(FSections);
  inherited;
end;

{TVkCatalog}

destructor TVkCatalog.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkAudio>(FAudios);
  TArrayHelp.FreeArrayOfObject<TVkCatalogPlaceholder>(FPlaceholders);
  TArrayHelp.FreeArrayOfObject<TVkAudioPlaylist>(FPlaylists);
  if Assigned(FCatalog) then
    FCatalog.Free;
  inherited;
end;

end.

