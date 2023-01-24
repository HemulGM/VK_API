unit VK.Entity.Group.Categories;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Group, VK.Entity.Common;

type
  TVkGroupSubcategory = class(TVkBasicObject)
  public
    property Id;
    property Name;
  end;

  TVkGroupCategory = class(TVkBasicObject)
  private
    FSubcategories: TArray<TVkGroupSubcategory>;
    FPage_count: Integer;
    FPage_previews: TArray<TVkGroup>;
  public
    property Id;
    property Name;
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

{TVkGroupCategory}

destructor TVkGroupCategory.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupSubcategory>(FSubcategories);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FPage_previews);
  inherited;
end;

{TVkGroupCategories}

destructor TVkGroupCategories.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupCategory>(FCategories);
  inherited;
end;

end.

