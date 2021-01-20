unit VK.Entity.Board.Comment;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json, VK.Entity.Common, VK.Entity.Common.List,
  VK.Entity.Profile, VK.Entity.Group, VK.Entity.Media, VK.Entity.Poll, VK.Entity.Common.ExtendedList;

type
  TVkBoardComment = class(TVkObject)
  private
    FCan_edit: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_id: Integer;
    FLikes: TVkLikesInfo;
    FText: string;
    FAttachments: TArray<TVkAttachment>;
  public
    property CanEdit: Integer read FCan_edit write FCan_edit;
    property Date: TDateTime read FDate write FDate;
    property FromId: Integer read FFrom_id write FFrom_id;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Text: string read FText write FText;
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkBoardComments = class(TVkEntityExtendedList<TVkBoardComment>)
  private
    FPoll: TVkPoll;
  public
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
  FPoll.Free;
  inherited;
end;

end.

