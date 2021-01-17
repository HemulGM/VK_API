unit VK.Entity.CommentInfo;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkCommentInfo = class(TVkEntity)
  private
    FComment_id: Integer;
    FParents_stack: TArray<Integer>;
  public
    property CommentId: Integer read FComment_id write FComment_id;
    property ParentsStack: TArray<Integer> read FParents_stack write FParents_stack;
  end;

implementation

end.

