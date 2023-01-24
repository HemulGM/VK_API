unit VK.Entity.Board.Comment;

interface

uses
  Generics.Collections, VK.Wrap.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Common, VK.Entity.Common.List, VK.Entity.Profile, VK.Entity.Info,
  VK.Entity.Group, VK.Entity.Media, VK.Entity.Poll,
  VK.Entity.Common.ExtendedList, VK.Types;

type
  TVkBoardComment = class(TVkObject)
  private
    FCan_edit: Integer;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_id: TVkPeerId;
    FLikes: TVkLikesInfo;
    FText: string;
    FAttachments: TVkAttachmentArray;
  public
    property CanEdit: Integer read FCan_edit write FCan_edit;
    property Date: TDateTime read FDate write FDate;
    property FromId: TVkPeerId read FFrom_id write FFrom_id;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    property Text: string read FText write FText;
    property Attachments: TVkAttachmentArray read FAttachments write FAttachments;
    destructor Destroy; override;
  end;

  TVkBoardComments = class(TVkEntityExtendedList<TVkBoardComment>)
  private
    FPoll: TVkPoll;
  public
    property Poll: TVkPoll read FPoll write FPoll;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkBoardComment}

destructor TVkBoardComment.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  if Assigned(FLikes) then
    FLikes.Free;
  inherited;
end;

{TVkBoardComments}

destructor TVkBoardComments.Destroy;
begin
  if Assigned(FPoll) then
    FPoll.Free;
  inherited;
end;

end.

