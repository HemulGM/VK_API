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
  end;

  TVkGroupCategories = class(TVkEntity)
  private
    FCategories: TArray<TVkGroupCategory>;
    FEnabled: Integer;
  public
    property Categories: TArray<TVkGroupCategory> read FCategories write FCategories;
    property Enabled: Integer read FEnabled write FEnabled;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TCategoriesClass}

destructor TVkGroupCategory.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupSubcategory>(FSubcategories);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FPage_previews);
  inherited;
end;

{TRootClass}

destructor TVkGroupCategories.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupCategory>(FCategories);
  inherited;
end;

end.

