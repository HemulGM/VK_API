unit VK.Entity.GroupSettings;

interface

uses
  Generics.Collections, Rest.Json, REST.Json.Interceptors, VK.Entity.Common,
  VK.Entity.Group, VK.Entity.Market, VK.Entity.Geo, VK.Types, REST.JsonReflect,
  VK.Entity.Group.Categories, VK.Wrap.Interceptors, VK.Entity.Group.Youla;

type
  TVkActionButtonTarget = class
  private
    FIs_Internal: Boolean;
    FUrl: string;
    FGoogle_store_url: string;
    FItunes_url: string;
  public
    property IsInternal: Boolean read FIs_Internal write FIs_Internal;
    property Url: string read FUrl write FUrl;
    property GoogleStoreUrl: string read FGoogle_store_url write FGoogle_store_url;
    property ItunesUrl: string read FItunes_url write FItunes_url;
  end;

  TVkActionButton = class(TVkEntity)
  private
    FAction_Type: string; //open_app
    FIs_Enabled: Boolean;
    FTarget: TVkActionButtonTarget;
    FTitle: string;
  public
    property ActionType: string read FAction_Type write FAction_Type;
    property IsEnabled: Boolean read FIs_Enabled write FIs_Enabled;
    property Target: TVkActionButtonTarget read FTarget write FTarget;
    property Title: string read FTitle write FTitle;
    destructor Destroy; override;
  end;

  TVkGroupSetting<T> = record
  private
    FNew_value: T;
    FOld_value: T;
  public
    property NewValue: T read FNew_value write FNew_value;
    property OldValue: T read FOld_value write FOld_value;
  end;

  TVkGroupSettingStr = TVkGroupSetting<string>;

  TVkGroupSettingInt = TVkGroupSetting<Integer>;

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
  end;

  TVkGroupSettingsChange = class(TVkEntity)
  private
    FChanges: TVkGroupChangeList;
    FUser_id: Integer;
  public
    property Changes: TVkGroupChangeList read FChanges write FChanges;
    property UserId: Integer read FUser_id write FUser_id;
    destructor Destroy; override;
  end;

  TVkGroupMarket = class(TVkEntity)
  private
    FCity_ids: TArray<Integer>;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FComments_enabled: Boolean;
    FContact_id: Integer;
    FCountry_ids: TArray<Integer>;
    FCurrency: TVkProductCurrency;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FEnabled: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_message: Boolean;
    FType: string; //advanced,
  public
    property CityIds: TArray<Integer> read FCity_ids write FCity_ids;
    property CommentsEnabled: Boolean read FComments_enabled write FComments_enabled;
    property ContactId: Integer read FContact_id write FContact_id;
    property CountryIds: TArray<Integer> read FCountry_ids write FCountry_ids;
    property Currency: TVkProductCurrency read FCurrency write FCurrency;
    property CanMessage: Boolean read FCan_message write FCan_message;
    property Enabled: Boolean read FEnabled write FEnabled;
    property &Type: string read FType write FType;
    destructor Destroy; override;
  end;

  TVkLiveCover = class
  private
    FIs_Enabled: Boolean;
    FIs_scalable: Boolean;
    FStory_ids: TArray<string>;
  public
    property StoryIds: TArray<string> read FStory_ids write FStory_ids;
    property IsEnabled: Boolean read FIs_Enabled write FIs_Enabled;
    property IsScalable: Boolean read FIs_scalable write FIs_scalable;
  end;

  TVkGroupSettings = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TGroupAccessInterceptor)]
    FAccess: TVkGroupAccess;
    FAddress: string;
    [JsonReflectAttribute(ctString, rtString, TAgeLimitsInterceptor)]
    FAge_limits: TVkAgeLimits;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FAudio: Boolean;
    FDescription: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FDocs: Boolean;
    FMarket: TVkGroupMarket;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FObscene_filter: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FObscene_stopwords: Boolean;
    FObscene_words: TArray<string>;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FPhotos: Boolean;
    FPlace: TVkPlace;
    FRss: string;
    FSubject: Integer;
    FSubject_list: TArray<TVkGroupSubject>;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FTopics: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FVideo: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FWall: Boolean;
    FWebsite: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FWiki: Boolean;
    FAction_button: TVkActionButton;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FRecognize_photo: Boolean;
    FLive_covers: TVkLiveCover;
    FPublic_category: Integer;
    FPublic_category_list: TArray<TVkGroupCategory>;
    FPublic_subcategory: Integer;
    FMain_section: Integer;
    FSecondary_section: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FEvents: Boolean;
    FPhone: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FMessages: Boolean;
    FCountry_id: Integer;
    FCity_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FArticles: Boolean;
    [JsonReflectAttribute(ctString, rtString, TStringDateTimeInterceptor)]
    FPublic_date: TDateTime;
    FPublic_date_label: string;
    FSuggested_privacy: Integer;
    FYoula: TVkGroupYoula;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FContacts: Boolean;
    FLinks: Boolean;
    FEvent_group_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FStart_date: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FFinish_date: TDateTime;
  public
    property Access: TVkGroupAccess read FAccess write FAccess;
    property ActionButton: TVkActionButton read FAction_button write FAction_button;
    property Address: string read FAddress write FAddress;
    property AgeLimits: TVkAgeLimits read FAge_limits write FAge_limits;
    property Articles: Boolean read FArticles write FArticles;
    property Audio: Boolean read FAudio write FAudio;
    property CityId: Integer read FCity_id write FCity_id;
    property Contacts: Boolean read FContacts write FContacts;
    property CountryId: Integer read FCountry_id write FCountry_id;
    property Description: string read FDescription write FDescription;
    property Docs: Boolean read FDocs write FDocs;
    property Events: Boolean read FEvents write FEvents;
    property EventGroupId: Integer read FEvent_group_id write FEvent_group_id;
    property FinishDate: TDateTime read FFinish_date write FFinish_date;
    property Links: Boolean read FLinks write FLinks;
    property LiveCovers: TVkLiveCover read FLive_covers write FLive_covers;
    property MainSection: Integer read FMain_section write FMain_section;
    property Market: TVkGroupMarket read FMarket write FMarket;
    property Messages: Boolean read FMessages write FMessages;
    property ObsceneFilter: Boolean read FObscene_filter write FObscene_filter;
    property ObsceneStopwords: Boolean read FObscene_stopwords write FObscene_stopwords;
    property ObsceneWords: TArray<string> read FObscene_words write FObscene_words;
    property Phone: string read FPhone write FPhone;
    property Photos: Boolean read FPhotos write FPhotos;
    property Place: TVkPlace read FPlace write FPlace;
    property PublicCategory: Integer read FPublic_category write FPublic_category;
    property PublicCategoryList: TArray<TVkGroupCategory> read FPublic_category_list write FPublic_category_list;
    property PublicDate: TDateTime read FPublic_date write FPublic_date;
    property PublicDateLabel: string read FPublic_date_label write FPublic_date_label;
    property PublicSubcategory: Integer read FPublic_subcategory write FPublic_subcategory;
    property RecognizePhoto: Boolean read FRecognize_photo write FRecognize_photo;
    property RSS: string read FRss write FRss;
    property SecondarySection: Integer read FSecondary_section write FSecondary_section;
    property StartDate: TDateTime read FStart_date write FStart_date;
    property Subject: Integer read FSubject write FSubject;
    property SubjectList: TArray<TVkGroupSubject> read FSubject_list write FSubject_list;
    property SuggestedPrivacy: Integer read FSuggested_privacy write FSuggested_privacy;
    property Title: string read FTitle write FTitle;
    property Topics: Boolean read FTopics write FTopics;
    property Video: Boolean read FVideo write FVideo;
    property Wall: Boolean read FWall write FWall;
    property Website: string read FWebsite write FWebsite;
    property Wiki: Boolean read FWiki write FWiki;
    property Youla: TVkGroupYoula read FYoula write FYoula;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkGroupSettingsChange}

destructor TVkGroupSettingsChange.Destroy;
begin
  if Assigned(FChanges) then
    FChanges.Free;
  inherited;
end;

{ TVkGroupSettings }

destructor TVkGroupSettings.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupSubject>(FSubject_list);
  TArrayHelp.FreeArrayOfObject<TVkGroupCategory>(FPublic_category_list);
  if Assigned(FPlace) then
    FPlace.Free;
  if Assigned(FMarket) then
    FMarket.Free;
  if Assigned(FYoula) then
    FYoula.Free;
  if Assigned(FAction_button) then
    FAction_button.Free;
  if Assigned(FLive_covers) then
    FLive_covers.Free;
  inherited;
end;

{ TVkGroupMarket }

destructor TVkGroupMarket.Destroy;
begin
  if Assigned(FCurrency) then
    FCurrency.Free;
  inherited;
end;

{ TVkActionButton }

destructor TVkActionButton.Destroy;
begin
  if Assigned(FTarget) then
    FTarget.Free;
  inherited;
end;

end.

