unit VK.Entity.GroupSettings;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Group, VK.Entity.Market, VK.Entity.Geo;

type
  TVkGroupSettingStr = class(TVkEntity)
  private
    FNew_value: string;
    FOld_value: string;
  public
    property NewValue: string read FNew_value write FNew_value;
    property OldValue: string read FOld_value write FOld_value;
  end;

  TVkGroupSettingInt = class(TVkEntity)
  private
    FNew_value: Integer;
    FOld_value: Integer;
  public
    property NewValue: Integer read FNew_value write FNew_value;
    property OldValue: Integer read FOld_value write FOld_value;
  end;

  TVkGroupChangeList = class(TVkEntity)
  private
    FCity_id: TVkGroupSettingInt;
    FDescription: TVkGroupSettingStr;
    FAudio: TVkGroupSettingInt;
    FTitle: TVkGroupSettingStr;
    FScreen_name: TVkGroupSettingStr;
    FWebsite: TVkGroupSettingStr;
    FAccess: TVkGroupSettingInt;
    FPublic_category: TVkGroupSettingInt;
    FPublic_subcategory: TVkGroupSettingInt;
    FAge_limits: TVkGroupSettingInt;
    FDocs: TVkGroupSettingInt;
    FPhotos: TVkGroupSettingInt;
    FVideo: TVkGroupSettingInt;
    FMarket: TVkGroupSettingInt;
    FTopics: TVkGroupSettingInt;
    FStatus_default: TVkGroupSettingInt;
  public
    property CityId: TVkGroupSettingInt read FCity_id write FCity_id;
    property Access: TVkGroupSettingInt read FAccess write FAccess;
    property AgeLimits: TVkGroupSettingInt read FAge_limits write FAge_limits;
    property PublicCategory: TVkGroupSettingInt read FPublic_category write FPublic_category;
    property PublicSubcategory: TVkGroupSettingInt read FPublic_subcategory write FPublic_subcategory;
    property Audio: TVkGroupSettingInt read FAudio write FAudio;
    property Docs: TVkGroupSettingInt read FDocs write FDocs;
    property Photos: TVkGroupSettingInt read FPhotos write FPhotos;
    property Video: TVkGroupSettingInt read FVideo write FVideo;
    property Market: TVkGroupSettingInt read FMarket write FMarket;
    property Topics: TVkGroupSettingInt read FTopics write FTopics;
    property StatusDefault: TVkGroupSettingInt read FStatus_default write FStatus_default;
    property Description: TVkGroupSettingStr read FDescription write FDescription;
    property Title: TVkGroupSettingStr read FTitle write FTitle;
    property ScreenName: TVkGroupSettingStr read FScreen_name write FScreen_name;
    property Website: TVkGroupSettingStr read FWebsite write FWebsite;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkGroupSettingsChange = class(TVkEntity)
  private
    FChanges: TVkGroupChangeList;
    FUser_id: Integer;
  public
    property Changes: TVkGroupChangeList read FChanges write FChanges;
    property UserId: Integer read FUser_id write FUser_id;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkGroupMarket = class(TVkEntity)
  private
    FCity_ids: TArray<Integer>;
    FComments_enabled: Integer;
    FContact_id: Integer;
    FCountry_ids: TArray<Integer>;
    FCurrency: TVkProductCurrency;
    FEnabled: Integer;
  public
    property CityIds: TArray<Integer> read FCity_ids write FCity_ids;
    property CommentsEnabled: Integer read FComments_enabled write FComments_enabled;
    property ContactId: Integer read FContact_id write FContact_id;
    property CountryIds: TArray<Integer> read FCountry_ids write FCountry_ids;
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property Enabled: Integer read FEnabled write FEnabled;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkGroupSettings = class(TVkEntity)
  private
    FAccess: Integer;
    FAddress: string;
    FAge_limits: Integer;
    FAudio: Integer;
    FDescription: string;
    FDocs: Integer;
    FMarket: TVkGroupMarket;
    FObscene_filter: Integer;
    FObscene_stopwords: Integer;
    FObscene_words: TArray<string>;
    FPhotos: Integer;
    FPlace: TVkPlace;
    FRss: string;
    FSubject: Integer;
    FSubject_list: TArray<TVkGroupSubject>;
    FTitle: string;
    FTopics: Integer;
    FVideo: Integer;
    FWall: Integer;
    FWebsite: string;
    FWiki: Integer;
  public
    property Access: Integer read FAccess write FAccess;
    property Address: string read FAddress write FAddress;
    property AgeLimits: Integer read FAge_limits write FAge_limits;
    property Audio: Integer read FAudio write FAudio;
    property Description: string read FDescription write FDescription;
    property Docs: Integer read FDocs write FDocs;
    property Market: TVkGroupMarket read FMarket write FMarket;
    property ObsceneFilter: Integer read FObscene_filter write FObscene_filter;
    property ObsceneStopwords: Integer read FObscene_stopwords write FObscene_stopwords;
    property ObsceneWords: TArray<string> read FObscene_words write FObscene_words;
    property Photos: Integer read FPhotos write FPhotos;
    property Place: TVkPlace read FPlace write FPlace;
    property RSS: string read FRss write FRss;
    property Subject: Integer read FSubject write FSubject;
    property SubjectList: TArray<TVkGroupSubject> read FSubject_list write FSubject_list;
    property Title: string read FTitle write FTitle;
    property Topics: Integer read FTopics write FTopics;
    property Video: Integer read FVideo write FVideo;
    property Wall: Integer read FWall write FWall;
    property Website: string read FWebsite write FWebsite;
    property Wiki: Integer read FWiki write FWiki;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkGroupChangeList}

constructor TVkGroupChangeList.Create;
begin
  inherited;
  FCity_id := TVkGroupSettingInt.Create;
  FDescription := TVkGroupSettingStr.Create;
  FAudio := TVkGroupSettingInt.Create;
  FTitle := TVkGroupSettingStr.Create;
  FScreen_name := TVkGroupSettingStr.Create;
  FWebsite := TVkGroupSettingStr.Create;
  FAccess := TVkGroupSettingInt.Create;
  FPublic_category := TVkGroupSettingInt.Create;
  FPublic_subcategory := TVkGroupSettingInt.Create;
  FAge_limits := TVkGroupSettingInt.Create;
  FDocs := TVkGroupSettingInt.Create;
  FPhotos := TVkGroupSettingInt.Create;
  FVideo := TVkGroupSettingInt.Create;
  FMarket := TVkGroupSettingInt.Create;
  FTopics := TVkGroupSettingInt.Create;
  FStatus_default := TVkGroupSettingInt.Create;
end;

destructor TVkGroupChangeList.Destroy;
begin
  FCity_id.Free;
  FDescription.Free;
  FAudio.Free;
  FTitle.Free;
  FScreen_name.Free;
  FWebsite.Free;
  FAccess.Free;
  FPublic_category.Free;
  FPublic_subcategory.Free;
  FAge_limits.Free;
  FDocs.Free;
  FPhotos.Free;
  FVideo.Free;
  FMarket.Free;
  FTopics.Free;
  FStatus_default.Free;
  inherited;
end;

{TVkGroupSettingsChange}

constructor TVkGroupSettingsChange.Create;
begin
  inherited;
  FChanges := TVkGroupChangeList.Create();
end;

destructor TVkGroupSettingsChange.Destroy;
begin
  FChanges.Free;
  inherited;
end;

{ TVkGroupSettings }

constructor TVkGroupSettings.Create;
begin
  FPlace := TVkPlace.Create;
  FMarket := TVkGroupMarket.Create;
end;

destructor TVkGroupSettings.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupSubject>(FSubject_list);
  FPlace.Free;
  FMarket.Free;
  inherited;
end;

{ TVkGroupMarket }

constructor TVkGroupMarket.Create;
begin
  FCurrency := TVkProductCurrency.Create;
end;

destructor TVkGroupMarket.Destroy;
begin
  FCurrency.Free;
  inherited;
end;

end.

