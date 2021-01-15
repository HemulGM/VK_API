unit VK.Entity.Fave.Pages;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Fave, VK.Entity.Common;

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
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkFavePages = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkFavePage>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkFavePage> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkFavePage}

constructor TVkFavePage.Create;
begin
  inherited;
end;

destructor TVkFavePage.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFaveTag>(FTags);
  if Assigned(FUser) then
    FUser.Free;
  if Assigned(FGroup) then
    FGroup.Free;
  inherited;
end;

{TVkFavePages}

destructor TVkFavePages.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFavePage>(FItems);
  inherited;
end;

end.

