unit VK.Entity.Comment;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkComment = class
  private
    FDate: Extended;
    FFrom_id: Extended;
    FId: Extended;
    FPost_id: Extended;
    FPost_owner_id: Extended;
    FReply_to_comment: Extended;
    FReply_to_user: Extended;
    FText: string;
  public
    property date: Extended read FDate write FDate;
    property from_id: Extended read FFrom_id write FFrom_id;
    property id: Extended read FId write FId;
    property post_id: Extended read FPost_id write FPost_id;
    property post_owner_id: Extended read FPost_owner_id write FPost_owner_id;
    property reply_to_comment: Extended read FReply_to_comment write FReply_to_comment;
    property reply_to_user: Extended read FReply_to_user write FReply_to_user;
    property text: string read FText write FText;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkComment;
  end;

  TVkCommentDeleted = class
  private
    FDeleter_id: Extended;
    FId: Extended;
    FOwner_id: Extended;
    FPost_id: Extended;
  public
    property deleter_id: Extended read FDeleter_id write FDeleter_id;
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property post_id: Extended read FPost_id write FPost_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCommentDeleted;
  end;

  TOnWallReplyAction = procedure(Sender: TObject; GroupId: Integer; Comment: TVkComment; EventId: string) of object;

  TOnWallReplyDelete = procedure(Sender: TObject; GroupId: Integer; Comment: TVkCommentDeleted;
    EventId: string) of object;

implementation


{TWallComment}

function TVkComment.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkComment.FromJsonString(AJsonString: string): TVkComment;
begin
  result := TJson.JsonToObject<TVkComment>(AJsonString)
end;

{ TWallCommentDeleted }

function TVkCommentDeleted.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCommentDeleted.FromJsonString(AJsonString: string): TVkCommentDeleted;
begin
  result := TJson.JsonToObject<TVkCommentDeleted>(AJsonString)
end;

end.

