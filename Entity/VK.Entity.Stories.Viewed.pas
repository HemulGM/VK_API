unit VK.Entity.Stories.Viewed;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Common,
  VK.Entity.Common.List, VK.Types;

type
  TVkStoryView = class(TVkEntity)
  private
    FIs_liked: Boolean;
    FUser: TVkProfile;
    FUser_id: TVkPeerId;
  public
    property IsLiked: Boolean read FIs_liked write FIs_liked;
    property User: TVkProfile read FUser write FUser;
    property UserId: TVkPeerId read FUser_id write FUser_id;
    destructor Destroy; override;
  end;

  TVkStoryViews = TVkEntityList<TVkStoryView>;

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

end.

