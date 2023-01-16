unit VK.Entity.Group.Youla;

interface

uses
  REST.Json, System.Generics.Collections, VK.Entity.Common,
  VK.Entity.Group.Categories, REST.Json.Types;

type
  TVkYoulaGroupSettings = class
  private
    FAddress: string;
    FIs_Active: Boolean;
    FIs_Moderated: Boolean;
    FLat: Extended;
    FLong: Extended;
    FModeration_Status: Integer;
    FRadius: Extended;
    FRadius_Area: string;
    FRadiuses: TArray<Extended>;
    FSelected_Category_Ids: TArray<Integer>;
    FShow_Moderation_Setting: Boolean;
  public
    property Address: string read FAddress write FAddress;
    property IsActive: Boolean read FIs_Active write FIs_Active;
    property IsModerated: Boolean read FIs_Moderated write FIs_Moderated;
    property Lat: Extended read FLat write FLat;
    property Long: Extended read FLong write FLong;
    property ModerationStatus: Integer read FModeration_Status write FModeration_Status;
    property Radius: Extended read FRadius write FRadius;
    property RadiusArea: string read FRadius_Area write FRadius_Area;
    property Radiuses: TArray<Extended> read FRadiuses write FRadiuses;
    property SelectedCategoryIds: TArray<Integer> read FSelected_Category_Ids write FSelected_Category_Ids;
    property ShowModerationSetting: Boolean read FShow_Moderation_Setting write FShow_Moderation_Setting;
  end;

  TVkYoulaSubcategory = class(TVkObject)
  private
    FParent_Id: Integer;
    FTitle: string;
  public
    property Id;
    property ParentId: Integer read FParent_Id write FParent_Id;
    property Title: string read FTitle write FTitle;
  end;

  TVkYoulaSubcategories = class(TVkObject)
  private
    FParent_Id: Integer;
    FSubcategories: TArray<TVkYoulaSubcategory>;
    FTitle: string;
  public
    property Id;
    property ParentId: Integer read FParent_Id write FParent_Id;
    property Subcategories: TArray<TVkYoulaSubcategory> read FSubcategories write FSubcategories;
    property Title: string read FTitle write FTitle;
    destructor Destroy; override;
  end;

  TVkYoulaCategoryTree = class(TVkObject)
  private
    FSubcategories: TArray<TVkYoulaSubcategories>;
    FTitle: string;
  public
    property Id;
    property Subcategories: TArray<TVkYoulaSubcategories> read FSubcategories write FSubcategories;
    property Title: string read FTitle write FTitle;
    destructor Destroy; override;
  end;

  TVkGroupYoula = class
  private
    FCategory_Tree: TVkYoulaCategoryTree;
    FGroup_Settings: TVkYoulaGroupSettings;
  public
    property CategoryTree: TVkYoulaCategoryTree read FCategory_Tree write FCategory_Tree;
    property GroupSettings: TVkYoulaGroupSettings read FGroup_Settings write FGroup_Settings;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkYoulaSubcategories }

destructor TVkYoulaSubcategories.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkYoulaSubcategory>(FSubcategories);
  inherited;
end;

{ TVkCategoryTree }

destructor TVkYoulaCategoryTree.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkYoulaSubcategories>(FSubcategories);
  inherited;
end;

{ TVkGroupYoula }

destructor TVkGroupYoula.Destroy;
begin
  if Assigned(FCategory_Tree) then
    FCategory_Tree.Free;
  if Assigned(FGroup_Settings) then
    FGroup_Settings.Free;
  inherited;
end;

end.

