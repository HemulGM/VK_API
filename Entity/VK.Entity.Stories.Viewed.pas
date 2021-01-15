unit VK.Entity.Stories.Viewed;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Common;

type
  TVkStoryView = class
  private
    FIs_liked: Boolean;
    FUser: TVkProfile;
    FUser_id: Integer;
  public
    property IsLiked: Boolean read FIs_liked write FIs_liked;
    property User: TVkProfile read FUser write FUser;
    property UserId: Integer read FUser_id write FUser_id;
    destructor Destroy; override;
  end;

  TVkStoryViews = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkStoryView>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkStoryView> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkStoryView}

destructor TVkStoryView.Destroy;
begin
  if Assigned(FUser) then
    FUser.Free;
  inherited;
end;

{TVkStoryViews}

destructor TVkStoryViews.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStoryView>(FItems);
  inherited;
end;

end.

