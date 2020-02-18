unit VK.Entity.CommentInfo;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkCommentInfo = class
  private
    FComment_id: Extended;
    FParents_stack: TArray<Extended>;
  public
    property CommentId: Extended read FComment_id write FComment_id;
    property ParentsStack: TArray<Extended> read FParents_stack write FParents_stack;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCommentInfo;
  end;

implementation

{TVkCommentInfo}

function TVkCommentInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCommentInfo.FromJsonString(AJsonString: string): TVkCommentInfo;
begin
  result := TJson.JsonToObject<TVkCommentInfo>(AJsonString)
end;

end.

