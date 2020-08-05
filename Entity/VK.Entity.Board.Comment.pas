unit VK.Entity.Board.Comment;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Media, VK.Entity.Poll;

type
  TVkBoardComment = class
  private
    FCan_edit: Integer;
    FDate: Int64;
    FFrom_id: Integer;
    FId: Integer;
    FLikes: TVkLikesInfo;
    FText: string;
    FAttachments: TArray<TVkAttachment>;
  public
    property CanEdit: Integer read FCan_edit write FCan_edit;
    property Date: Int64 read FDate write FDate;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Id: Integer read FId write FId;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Text: string read FText write FText;
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkBoardComment;
  end;

  TVkBoardComments = class
  private
    FCount: Integer;
    FItems: TArray<TVkBoardComment>;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
    FPoll: TVkPoll;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkBoardComment> read FItems write FItems;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Poll: TVkPoll read FPoll write FPoll;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkBoardComments;
  end;

implementation

{TVkBoardComment}

constructor TVkBoardComment.Create;
begin
  inherited;
  FLikes := TVkLikesInfo.Create();
end;

destructor TVkBoardComment.Destroy;
var
  LitemsItem: TVkAttachment;
begin

  for LitemsItem in FAttachments do
    LitemsItem.Free;
  FLikes.Free;
  inherited;
end;

function TVkBoardComment.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkBoardComment.FromJsonString(AJsonString: string): TVkBoardComment;
begin
  result := TJson.JsonToObject<TVkBoardComment>(AJsonString)
end;

{TVkBoardComments}

constructor TVkBoardComments.Create;
begin
  inherited;
  FPoll := TVkPoll.Create;
end;

destructor TVkBoardComments.Destroy;
var
  LitemsItem: TVkBoardComment;
  User: TVkProfile;
  Group: TVkGroup;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  for User in FProfiles do
    User.Free;

  for Group in FGroups do
    Group.Free;
  FPoll.Free;
  inherited;
end;

function TVkBoardComments.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkBoardComments.FromJsonString(AJsonString: string): TVkBoardComments;
begin
  result := TJson.JsonToObject<TVkBoardComments>(AJsonString)
end;

end.

