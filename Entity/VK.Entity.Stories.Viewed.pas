unit VK.Entity.Stories.Viewed;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile;

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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStoryView;
  end;

  TVkStoryViews = class
  private
    FCount: Integer;
    FItems: TArray<TVkStoryView>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkStoryView> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStoryViews;
  end;

implementation

uses
  VK.CommonUtils;

{TVkStoryView}

constructor TVkStoryView.Create;
begin
  inherited;
end;

destructor TVkStoryView.Destroy;
begin
  if Assigned(FUser) then
    FUser.Free;
  inherited;
end;

function TVkStoryView.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStoryView.FromJsonString(AJsonString: string): TVkStoryView;
begin
  result := TJson.JsonToObject<TVkStoryView>(AJsonString)
end;

{TVkStoryViews}

destructor TVkStoryViews.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStoryView>(FItems);
  inherited;
end;

function TVkStoryViews.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStoryViews.FromJsonString(AJsonString: string): TVkStoryViews;
begin
  result := TJson.JsonToObject<TVkStoryViews>(AJsonString)
end;

end.

