unit VK.Entity.App;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  VK.Entity.Common, VK.Entity.Common.List, VK.Entity.Profile, VK.Entity.Group,
  VK.Entity.Common.ExtendedList;

type
  TVkAppScreenshot = class(TVkObject)
  private
    FAlbum_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FHas_tags: Boolean;
    FOwner_id: Integer;
    FSizes: TArray<TVkSize>;
    FText: string;
    FUser_id: Integer;
  public
    property AlbumId: Integer read FAlbum_id write FAlbum_id;
    property Date: TDateTime read FDate write FDate;
    property HasTags: Boolean read FHas_tags write FHas_tags;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Sizes: TArray<TVkSize> read FSizes write FSizes;
    property Text: string read FText write FText;
    property UserId: Integer read FUser_id write FUser_id;
    destructor Destroy; override;
  end;

  TVkStore = class(TVkBasicObject)
  public
    /// <summary>
    /// Идентификатор магазина
    /// </summary>
    property Id;
    /// <summary>
    /// Название магазина
    /// </summary>
    property Name;
  end;

  TVkStoreApplication = class(TVkEntity)
  private
    FApp_id: Integer;
    FStore: TVkStore;
  public
    /// <summary>
    /// Идентификатор приложения в магазине
    /// </summary>
    property AppId: Integer read FApp_id write FApp_id;
    /// <summary>
    /// Информация о магазине
    /// </summary>
    property Store: TVkStore read FStore write FStore;
    destructor Destroy; override;
  end;

  TVkApp = class(TVkObject)
  private
    FAuthor_owner_id: Integer;
    FAuthor_url: string;
    FBanner_1120: string;
    FBanner_560: string;
    FGenre: string;
    FGenre_id: Integer;
    FHide_tabbar: Boolean;
    FIcon_139: string;
    FIcon_150: string;
    FIcon_278: string;
    FIcon_75: string;
    FInternational: Boolean;
    FIs_in_catalog: Boolean;
    FIs_installed: Boolean;
    FLeaderboard_type: Integer;
    FMembers_count: Integer;
    FMobile_controls_type: Integer;
    FMobile_view_support_type: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FPublished_date: TDateTime;
    FSection: string;
    FTitle: string;
    FType: string;
    FPush_enabled: Boolean;
    FScreenshots: TArray<TVkAppScreenshot>;
    FScreen_name: string;
    FDescription: string;
    FIcon_16: string;
    FFriends: TArray<Integer>;
    FCatalog_position: Integer;
  public
    property Id;
    property AuthorOwnerId: Integer read FAuthor_owner_id write FAuthor_owner_id;
    property AuthorUrl: string read FAuthor_url write FAuthor_url;
    property Banner1120: string read FBanner_1120 write FBanner_1120;
    property Banner560: string read FBanner_560 write FBanner_560;
    property CatalogPosition: Integer read FCatalog_position write FCatalog_position;
    property Description: string read FDescription write FDescription;
    property Friends: TArray<Integer> read FFriends write FFriends;
    property Genre: string read FGenre write FGenre;
    property GenreId: Integer read FGenre_id write FGenre_id;
    property HideTabbar: Boolean read FHide_tabbar write FHide_tabbar;
    property Icon139: string read FIcon_139 write FIcon_139;
    property Icon150: string read FIcon_150 write FIcon_150;
    property Icon16: string read FIcon_16 write FIcon_16;
    property Icon278: string read FIcon_278 write FIcon_278;
    property Icon75: string read FIcon_75 write FIcon_75;
    property International: Boolean read FInternational write FInternational;
    property IsInCatalog: Boolean read FIs_in_catalog write FIs_in_catalog;
    property IsInstalled: Boolean read FIs_installed write FIs_installed;
    property LeaderboardType: Integer read FLeaderboard_type write FLeaderboard_type;
    property MembersCount: Integer read FMembers_count write FMembers_count;
    property MobileControlsType: Integer read FMobile_controls_type write FMobile_controls_type;
    property MobileViewSupportType: Integer read FMobile_view_support_type write FMobile_view_support_type;
    property PublishedDate: TDateTime read FPublished_date write FPublished_date;
    property PushEnabled: Boolean read FPush_enabled write FPush_enabled;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Screenshots: TArray<TVkAppScreenshot> read FScreenshots write FScreenshots;
    property Section: string read FSection write FSection;
    property Title: string read FTitle write FTitle;
    property&Type: string read FType write FType;
    destructor Destroy; override;
  end;

  TVkApps = TVkEntityExtendedList<TVkApp>;

implementation

uses
  VK.CommonUtils;

{TVkApp}

destructor TVkApp.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAppScreenshot>(FScreenshots);
  inherited;
end;

{ TVkAppScreenshot }

destructor TVkAppScreenshot.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
  inherited;
end;

{ TVkStoreApplication }

destructor TVkStoreApplication.Destroy;
begin
  if Assigned(FStore) then
    FStore.Free;
  inherited;
end;

end.

