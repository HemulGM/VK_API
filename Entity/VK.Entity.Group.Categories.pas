unit VK.Entity.Group.Categories;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Group;

type
  TVkGroupSubcategory = class
  private
    FId: Integer;
    FName: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupSubcategory;
  end;

  TVkGroupCategory = class
  private
    FId: Integer;
    FName: string;
    FSubcategories: TArray<TVkGroupSubcategory>;
    FPage_count: Integer;
    FPage_previews: TArray<TVkGroup>;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property PageCount: Integer read FPage_count write FPage_count;
    property PagePreviews: TArray<TVkGroup> read FPage_previews write FPage_previews;
    property Subcategories: TArray<TVkGroupSubcategory> read FSubcategories write FSubcategories;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupCategory;
  end;

  TVkGroupCategories = class
  private
    FCategories: TArray<TVkGroupCategory>;
    FEnabled: Integer;
  public
    property Categories: TArray<TVkGroupCategory> read FCategories write FCategories;
    property Enabled: Integer read FEnabled write FEnabled;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupCategories;
  end;

implementation

{TSubcategoriesClass}

function TVkGroupSubcategory.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupSubcategory.FromJsonString(AJsonString: string): TVkGroupSubcategory;
begin
  result := TJson.JsonToObject<TVkGroupSubcategory>(AJsonString)
end;

{TCategoriesClass}

destructor TVkGroupCategory.Destroy;
var
  LsubcategoriesItem: TVkGroupSubcategory;
  Page_previewsItem: TVkGroup;
begin

  for LsubcategoriesItem in FSubcategories do
    LsubcategoriesItem.Free;

  for Page_previewsItem in FPage_previews do
    Page_previewsItem.Free;

  inherited;
end;

function TVkGroupCategory.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupCategory.FromJsonString(AJsonString: string): TVkGroupCategory;
begin
  result := TJson.JsonToObject<TVkGroupCategory>(AJsonString)
end;

{TRootClass}

destructor TVkGroupCategories.Destroy;
var
  LcategoriesItem: TVkGroupCategory;
begin

  for LcategoriesItem in FCategories do
    LcategoriesItem.Free;

  inherited;
end;

function TVkGroupCategories.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupCategories.FromJsonString(AJsonString: string): TVkGroupCategories;
begin
  result := TJson.JsonToObject<TVkGroupCategories>(AJsonString)
end;

end.

