unit VK.Entity.Group.Categories;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Group, VK.Entity.Common;

type
  TVkGroupSubcategory = class(TVkObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupSubcategory;
  end;

  TVkGroupCategory = class(TVkObject)
  private
    FName: string;
    FSubcategories: TArray<TVkGroupSubcategory>;
    FPage_count: Integer;
    FPage_previews: TArray<TVkGroup>;
  public
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

uses
  VK.CommonUtils;

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
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupSubcategory>(FSubcategories);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FPage_previews);
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
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupCategory>(FCategories);
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

