unit VK.Entity.CommentInfo;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkCommentInfo = class(TVkEntity)
  private
    FComment_id: Extended;
    FParents_stack: TArray<Extended>;
  public
    property CommentId: Extended read FComment_id write FComment_id;
    property ParentsStack: TArray<Extended> read FParents_stack write FParents_stack;
  end;

implementation

end.

