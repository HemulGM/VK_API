unit VK.Entity.Post;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Attachment;

type
  TVkPostCopyHistory = class
  private
    FAttachments: TArray<TVkAttachment>;
    FDate: Extended;
    FFrom_id: Extended;
    FId: Extended;
    FOwner_id: Extended;
    FPost_source: TVkPostSource;
    FPost_type: string;
    FText: string;
  public
    property attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property date: Extended read FDate write FDate;
    property from_id: Extended read FFrom_id write FFrom_id;
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property post_source: TVkPostSource read FPost_source write FPost_source;
    property post_type: string read FPost_type write FPost_type;
    property text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPostCopyHistory;
  end;

  TVkPost = class
  private
    FAttachments: TArray<TVkAttachment>;
    FComments: TVkCommentsInfo;
    FCopy_history: TArray<TVkPostCopyHistory>;
    FDate: Extended;
    FFrom_id: Extended;
    FId: Extended;
    FIs_favorite: Boolean;
    FLikes: TVkLikesInfo;
    FMarked_as_ads: Extended;
    FOwner_id: Extended;
    FPost_source: TVkPostSource;
    FPost_type: string;
    FReposts: TVkRepostsInfo;
    FText: string;
  public
    property attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    property comments: TVkCommentsInfo read FComments write FComments;
    property copy_history: TArray<TVkPostCopyHistory> read FCopy_history write FCopy_history;
    property date: Extended read FDate write FDate;
    property from_id: Extended read FFrom_id write FFrom_id;
    property id: Extended read FId write FId;
    property is_favorite: Boolean read FIs_favorite write FIs_favorite;
    property likes: TVkLikesInfo read FLikes write FLikes;
    property marked_as_ads: Extended read FMarked_as_ads write FMarked_as_ads;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property post_source: TVkPostSource read FPost_source write FPost_source;
    property post_type: string read FPost_type write FPost_type;
    property reposts: TVkRepostsInfo read FReposts write FReposts;
    property text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPost;
  end;

implementation

{TCopy_historyClass}

constructor TVkPostCopyHistory.Create;
begin
  inherited;
  FPost_source := TVkPostSource.Create();
end;

destructor TVkPostCopyHistory.Destroy;
var
  LattachmentsItem: TVkAttachment;
begin

  for LattachmentsItem in FAttachments do
    LattachmentsItem.Free;

  FPost_source.Free;
  inherited;
end;

function TVkPostCopyHistory.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPostCopyHistory.FromJsonString(AJsonString: string): TVkPostCopyHistory;
begin
  result := TJson.JsonToObject<TVkPostCopyHistory>(AJsonString)
end;

{TRootClass}

constructor TVkPost.Create;
begin
  inherited;
  FPost_source := TVkPostSource.Create();
  FComments := TVkCommentsInfo.Create();
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
end;

destructor TVkPost.Destroy;
var
  Lcopy_historyItem: TVkPostCopyHistory;
begin

  for Lcopy_historyItem in FCopy_history do
    Lcopy_historyItem.Free;

  FPost_source.Free;
  FComments.Free;
  FLikes.Free;
  FReposts.Free;
  inherited;
end;

function TVkPost.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPost.FromJsonString(AJsonString: string): TVkPost;
begin
  result := TJson.JsonToObject<TVkPost>(AJsonString)
end;

end.

