unit VK.Entity.Board.Comment;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Media, VK.Entity.Poll;

type
  TVkBoardComment = class(TVkObject)
  private
    FCan_edit: Integer;
    FDate: Int64;
    FFrom_id: Integer;
    FLikes: TVkLikesInfo;
    FText: string;
    FAttachments: TArray<TVkAttachment>;
  public
    property CanEdit: Integer read FCan_edit write FCan_edit;
    property Date: Int64 read FDate write FDate;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Text: string read FText write FText;
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkBoardComments = class(TVkEntity)
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
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkBoardComment}

constructor TVkBoardComment.Create;
begin
  inherited;
  FLikes := TVkLikesInfo.Create();
end;

destructor TVkBoardComment.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  FLikes.Free;
  inherited;
end;

{TVkBoardComments}

constructor TVkBoardComments.Create;
begin
  inherited;
  FPoll := TVkPoll.Create;
end;

destructor TVkBoardComments.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkBoardComment>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  FPoll.Free;
  inherited;
end;

end.

