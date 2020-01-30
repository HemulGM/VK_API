unit VK.Entity.GroupSettings;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkGroupSettingStr = class
  private
    FNew_value: string;
    FOld_value: string;
  public
    property NewValue: string read FNew_value write FNew_value;
    property OldValue: string read FOld_value write FOld_value;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupSettingStr;
  end;

  TVkGroupSettingInt = class
  private
    FNew_value: Integer;
    FOld_value: Integer;
  public
    property NewValue: Integer read FNew_value write FNew_value;
    property OldValue: Integer read FOld_value write FOld_value;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupSettingInt;
  end;

  TVkGroupChangeList = class
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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupChangeList;
  end;

  TVkGroupSettingsChange = class
  private
    FChanges: TVkGroupChangeList;
    FUser_id: Extended;
  public
    property Changes: TVkGroupChangeList read FChanges write FChanges;
    property UserId: Extended read FUser_id write FUser_id;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupSettingsChange;
  end;

implementation

{TVkGroupSettingStr}

function TVkGroupSettingStr.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupSettingStr.FromJsonString(AJsonString: string): TVkGroupSettingStr;
begin
  result := TJson.JsonToObject<TVkGroupSettingStr>(AJsonString)
end;

{TVkGroupSettingInt}

function TVkGroupSettingInt.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupSettingInt.FromJsonString(AJsonString: string): TVkGroupSettingInt;
begin
  result := TJson.JsonToObject<TVkGroupSettingInt>(AJsonString)
end;

{TVkGroupChangeList}

constructor TVkGroupChangeList.Create;
begin
  inherited;
end;

destructor TVkGroupChangeList.Destroy;
begin
  FCity_id.Free;
  FDescription.Free;
  inherited;
end;

function TVkGroupChangeList.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupChangeList.FromJsonString(AJsonString: string): TVkGroupChangeList;
begin
  result := TJson.JsonToObject<TVkGroupChangeList>(AJsonString)
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

function TVkGroupSettingsChange.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupSettingsChange.FromJsonString(AJsonString: string): TVkGroupSettingsChange;
begin
  result := TJson.JsonToObject<TVkGroupSettingsChange>(AJsonString)
end;

end.

