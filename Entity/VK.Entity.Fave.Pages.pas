unit VK.Entity.Fave.Pages;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Fave, VK.Entity.Common,
  VK.Entity.Common.List;

type
  TVkFavePage = class(TVkEntity)
  private
    FDescription: string;
    FType: string;
    FUpdated_date: Int64;
    FUser: TVkProfile;
    FGroup: TVkGroup;
    FTags: TArray<TVkFaveTag>;
  public
    property Description: string read FDescription write FDescription;
    property&Type: string read FType write FType;
    property UpdatedDate: Int64 read FUpdated_date write FUpdated_date;
    property User: TVkProfile read FUser write FUser;
    property Tags: TArray<TVkFaveTag> read FTags write FTags;
    property Group: TVkGroup read FGroup write FGroup;
    destructor Destroy; override;
  end;

  TVkFavePages = TVkEntityList<TVkFavePage>;

implementation

uses
  VK.CommonUtils;

destructor TVkFavePage.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFaveTag>(FTags);
  if Assigned(FUser) then
    FUser.Free;
  if Assigned(FGroup) then
    FGroup.Free;
  inherited;
end;

end.

