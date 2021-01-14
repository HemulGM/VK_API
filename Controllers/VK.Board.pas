unit VK.Board;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON, VK.Entity.Media,
  VK.Entity.Board, VK.Entity.Board.Comment;

type
  TVkBoardTopicOrder = (btoDateUpCreate = -2, btoDateUpUpdate = -1, btoDateDownCreate = 2, btoDateDownUpdate = 1);

  TVkBoardTopicOrderHelper = record helper for TVkBoardTopicOrder
    function ToConst: Integer; inline;
  end;

  TVkBoardTopicPreview = (btpOnlyFirst = 1, btpOnlyLast = 2, btpBoth = 3);

  TVkBoardTopicPreviewHelper = record helper for TVkBoardTopicPreview
    function ToConst: Integer; inline;
  end;

  TVkParamsBoardCommentCreate = record
    List: TParams;
    /// <summary>
    /// True � ���� ����� ������� �� ����� ������, False � ���� ����� ������� �� ����� ������������ (�� ���������).
    /// </summary>
    function FromGroup(Value: Boolean): Integer;
    /// <summary>
    /// ������������� �������.
    /// </summary>
    function StickerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    function TopicId(Value: Integer): Integer;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ����������� �����������.
    /// </summary>
    function Guid(Value: string): Integer;
    /// <summary>
    /// ����� �����������. ������������ ��������, ���� �� �������� ��������
    /// </summary>
    function Message(Value: string): Integer;
    /// <summary>
    /// C����� ��������, ����������� � ������
    /// </summary>
    function Attachments(Value: TAttachmentArray): Integer;
  end;

  TVkParamsBoardCommentEdit = record
    List: TParams;
    /// <summary>
    /// True � ���� ����� ������� �� ����� ������, False � ���� ����� ������� �� ����� ������������ (�� ���������).
    /// </summary>
    function FromGroup(Value: Boolean): Integer;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������� � ����������.
    /// </summary>
    function CommentId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    function TopicId(Value: Integer): Integer;
    /// <summary>
    /// ����� ����� ����������� (�������� ������������, ���� �� ����� �������� attachments).
    /// </summary>
    function Message(Value: string): Integer;
    /// <summary>
    /// C����� ��������, ����������� � ������
    /// </summary>
    function Attachments(Value: TAttachmentArray): Integer;
  end;

  TVkParamsBoardGet = record
    List: TParams;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// ������ ��������������� ���, ������� ���������� �������� (�� ����� 100). �� ��������� ������������ ��� ����. ���� ������ ������ ��������, ������������ ��������� order, offset � count (������������ ��� ����������� ���� � ��������� �������).
    /// </summary>
    function TopicIds(Value: TIds): Integer;
    /// <summary>
    /// �������, � ������� ���������� ������� ������ ���.
    /// </summary>
    function Order(Value: TVkBoardTopicOrder): Integer;
    /// <summary>
    /// ���������� ���, ������� ���������� �������� (�� �� ����� 100). �� ��������� � 40.
    /// </summary>
    function Count(Value: Integer = 40): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ���.
    /// </summary>
    function Offset(Value: Integer = 0): Integer;
    /// <summary>
    /// ���� ������� � �������� ����� ��������� True, �� ����� ���������� ���������� � �������������, ���������� ����������� ��� ��� ����������� � ��� ��������� ���������. �� ��������� False.
    /// </summary>
    function Extended(Value: Boolean = False): Integer;
    /// <summary>
    /// ����� ������, ������������, ���������� �� ������� ������ � ����������� � ����� ����� ������ � ��������� ��������� � ���. �������� ������ ������
    /// </summary>
    function Preview(Value: TVkBoardTopicPreview): Integer;
    /// <summary>
    /// ���������� ��������, �� �������� ����� �������� ������ � ��������� ���������. ������� 0, ���� �� �� ������ �������� ���������. (�� ��������� � 90).
    /// </summary>
    function PreviewLength(Value: Integer = 90): Integer;
  end;

  TVkParamsBoardAdd = record
    List: TParams;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// �������� ����������.
    /// </summary>
    function Title(Value: string): Integer;
    /// <summary>
    /// ����� ������� ��������� � ����������.
    /// </summary>
    function Text(Value: string): Integer;
    /// <summary>
    /// True � ���� ����� ������� �� ����� ������, False � ���� ����� ������� �� ����� ������������ (�� ���������).
    /// </summary>
    function FromGroup(Value: Boolean): Integer;
    /// <summary>
    /// C����� ��������, ����������� � ������
    /// </summary>
    function Attachments(Value: TAttachmentArray): Integer;
  end;

  TVkParamsBoardCommentsGet = record
    List: TParams;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������.
    /// </summary>
    function TopicId(Value: Integer): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes. �� ��������� ���� likes �� ������������.
    /// </summary>
    function NeedLikes(Value: Boolean): Integer;
    /// <summary>
    /// ������������� �����������, ������� � �������� ����� ������� ������ (����������� ��. ����).
    /// </summary>
    function StartCommentId(Value: Integer): Integer;
    /// <summary>
    /// ���������� ���, ������� ���������� �������� (�� �� ����� 100). �� ��������� � 20.
    /// </summary>
    function Count(Value: Integer = 20): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ���.
    /// </summary>
    function Offset(Value: Integer = 0): Integer;
    /// <summary>
    /// ���� ������� � �������� ����� ��������� True, �� ����� ���������� ���������� � �������������, ���������� �������� ���������. �� ��������� False.
    /// </summary>
    function Extended(Value: Boolean): Integer;
    /// <summary>
    /// ������� ���������� ������������: asc � ���������������, desc � �������������������
    /// </summary>
    function Sort(Value: Boolean): Integer;
  end;

  TBoardController = class(TVkController)
  public
    /// <summary>
    /// ��������� ����� ����������� � ����������
    /// </summary>
    function CreateComment(var Id: Integer; GroupId, TopicId: Integer; Message: string): Boolean; overload;
    /// <summary>
    /// ��������� ����� ����������� � ����������
    /// </summary>
    function CreateComment(GroupId, TopicId: Integer; Message: string): Boolean; overload;
    /// <summary>
    /// ��������� ����� ����������� � ����������
    /// </summary>
    function CreateComment(var Id: Integer; Params: TVkParamsBoardCommentCreate): Boolean; overload;
    /// <summary>
    /// ��������� ����� ����������� � ����������
    /// </summary>
    function CreateComment(Params: TVkParamsBoardCommentCreate): Boolean; overload;
    /// <summary>
    /// ��������� ����� ����������� � ����������
    /// </summary>
    function CreateComment(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ��������� ���� � ������ ���������� ������.
    /// </summary>
    function EditTopic(GroupId, TopicId: Integer; Title: string): Boolean; overload;
    /// <summary>
    /// ��������� ����� ����������� � ����������
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ����� ����������� � ����������
    /// </summary>
    function EditComment(Params: TVkParamsBoardCommentEdit): Boolean; overload;
    /// <summary>
    /// ������� ����������
    /// </summary>
    function DeleteTopic(GroupId, TopicId: Integer): Boolean;
    /// <summary>
    /// ������� ��������� ���� � ����������� ����������.
    /// </summary>
    function DeleteComment(GroupId, TopicId, CommentId: Integer): Boolean;
    /// <summary>
    /// ���������� ������ ��� � ����������� ��������� ������.
    /// </summary>
    function GetTopics(var Items: TVkBoardTopics; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��� � ����������� ��������� ������.
    /// </summary>
    function GetTopics(var Items: TVkBoardTopics; Params: TVkParamsBoardGet): Boolean; overload;
    /// <summary>
    /// ������� ����� ���� � ������ ���������� ������.
    /// </summary>
    function AddTopic(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// ������� ����� ���� � ������ ���������� ������.
    /// </summary>
    function AddTopic(Params: TVkParamsBoardAdd): Boolean; overload;
    /// <summary>
    /// ������� ����� ���� � ������ ���������� ������.
    /// </summary>
    function AddTopic(var Id: Integer; Params: TVkParamsBoardAdd): Boolean; overload;
    /// <summary>
    /// ��������� ���� � ������ ���������� ������ (� ����� ���� ���������� ��������� ����� ���������).
    /// </summary>
    function CloseTopic(GroupId, TopicId: Integer): Boolean; overload;
    /// <summary>
    /// ��������� ����� �������� ���� (� ��� ������ �������� ��������� ����� ���������).
    /// </summary>
    function OpenTopic(GroupId, TopicId: Integer): Boolean; overload;
    /// <summary>
    /// ��������� ����� �������� ���� (� ��� ������ �������� ��������� ����� ���������).
    /// </summary>
    function RestoreComment(GroupId, TopicId, CommentId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ���� � ������ ���������� ������ (����� ���� ��� ����� ���������� ��������� ���� ���������).
    /// </summary>
    function FixTopic(GroupId, TopicId: Integer): Boolean; overload;
    /// <summary>
    /// �������� ������������ ���� � ������ ���������� ������ (���� ����� ���������� �������� ��������� ����������).
    /// </summary>
    function UnfixTopic(GroupId, TopicId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������� � ��������� ����.
    /// </summary>
    function GetComments(var Items: TVkBoardComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������� � ��������� ����.
    /// </summary>
    function GetComments(var Items: TVkBoardComments; Params: TVkParamsBoardCommentsGet): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TBoardController }

function TBoardController.CreateComment(var Id: Integer; GroupId, TopicId: Integer; Message: string): Boolean;
var
  Params: TVkParamsBoardCommentCreate;
begin
  Params.Message(Message);
  Params.GroupId(GroupId);
  Params.TopicId(TopicId);
  Result := CreateComment(Id, Params.List);
end;

function TBoardController.CreateComment(GroupId, TopicId: Integer; Message: string): Boolean;
var
  Id: Integer;
begin
  Result := CreateComment(Id, GroupId, TopicId, Message);
end;

function TBoardController.CreateComment(var Id: Integer; Params: TVkParamsBoardCommentCreate): Boolean;
begin
  Result := CreateComment(Id, Params.List);
end;

function TBoardController.CreateComment(var Id: Integer; Params: TParams): Boolean;
begin
  Id := -1;
  with Handler.Execute('board.createComment', Params) do
  begin
    Result := Success and TryStrToInt(Response, Id);
  end;
end;

function TBoardController.CreateComment(Params: TVkParamsBoardCommentCreate): Boolean;
var
  Id: Integer;
begin
  Result := CreateComment(Id, Params.List);
end;

function TBoardController.AddTopic(var Id: Integer; Params: TParams): Boolean;
begin
  with Handler.Execute('board.addTopic', Params) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.AddTopic(var Id: Integer; Params: TVkParamsBoardAdd): Boolean;
begin
  Result := AddTopic(Id, Params.List);
end;

function TBoardController.AddTopic(Params: TVkParamsBoardAdd): Boolean;
var
  Id: Integer;
begin
  Result := AddTopic(Id, Params.List);
end;

function TBoardController.CloseTopic(GroupId, TopicId: Integer): Boolean;
begin
  with Handler.Execute('board.closeTopic', [['group_id', GroupId.ToString], ['topic_id', TopicId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.DeleteComment(GroupId, TopicId, CommentId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('topic_id', TopicId);
  Params.Add('comment_id', CommentId);
  with Handler.Execute('board.deleteComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.DeleteTopic(GroupId, TopicId: Integer): Boolean;
begin
  with Handler.Execute('board.deleteTopic', [['group_id', GroupId.ToString], ['topic_id', TopicId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.EditComment(Params: TVkParamsBoardCommentEdit): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TBoardController.EditTopic(GroupId, TopicId: Integer; Title: string): Boolean;
begin
  with Handler.Execute('board.editTopic', [['group_id', GroupId.ToString], ['topic_id', TopicId.ToString], ['title', Title]]) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.FixTopic(GroupId, TopicId: Integer): Boolean;
begin
  with Handler.Execute('board.fixTopic', [['group_id', GroupId.ToString], ['topic_id', TopicId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.UnfixTopic(GroupId, TopicId: Integer): Boolean;
begin
  with Handler.Execute('board.unfixTopic', [['group_id', GroupId.ToString], ['topic_id', TopicId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.EditComment(Params: TParams): Boolean;
begin
  with Handler.Execute('board.editComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.GetComments(var Items: TVkBoardComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('board.getComments', Params).GetObject<TVkBoardComments>(Items);
end;

function TBoardController.GetComments(var Items: TVkBoardComments; Params: TVkParamsBoardCommentsGet): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TBoardController.GetTopics(var Items: TVkBoardTopics; Params: TVkParamsBoardGet): Boolean;
begin
  Result := GetTopics(Items, Params.List);
end;

function TBoardController.OpenTopic(GroupId, TopicId: Integer): Boolean;
begin
  with Handler.Execute('board.openTopic', [['group_id', GroupId.ToString], ['topic_id', TopicId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.RestoreComment(GroupId, TopicId, CommentId: Integer): Boolean;
begin
  with Handler.Execute('board.restoreComment', [['group_id', GroupId.ToString], ['topic_id', TopicId.ToString], ['comment_id',
    CommentId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TBoardController.GetTopics(var Items: TVkBoardTopics; Params: TParams): Boolean;
begin
  Result := Handler.Execute('board.getTopics', Params).GetObject<TVkBoardTopics>(Items);
end;

{ TVkParamsBoardCommentEdit }

function TVkParamsBoardCommentEdit.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToString)
end;

function TVkParamsBoardCommentEdit.FromGroup(Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsBoardCommentEdit.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsBoardCommentEdit.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsBoardCommentEdit.TopicId(Value: Integer): Integer;
begin
  Result := List.Add('topic_id', Value);
end;

function TVkParamsBoardCommentEdit.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

{ TVkParamsBoardCommentCreate }

function TVkParamsBoardCommentCreate.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToString)
end;

function TVkParamsBoardCommentCreate.FromGroup(Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsBoardCommentCreate.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsBoardCommentCreate.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsBoardCommentCreate.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsBoardCommentCreate.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsBoardCommentCreate.TopicId(Value: Integer): Integer;
begin
  Result := List.Add('topic_id', Value);
end;

{ TVkBoardTopicOrderHelper }

function TVkBoardTopicOrderHelper.ToConst: Integer;
begin
  Result := Ord(Self);
end;

{ TVkBoardTopicPreviewHelper }

function TVkBoardTopicPreviewHelper.ToConst: Integer;
begin
  Result := Ord(Self);
end;

{ TVkParamsBoardGet }

function TVkParamsBoardGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsBoardGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsBoardGet.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsBoardGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsBoardGet.Order(Value: TVkBoardTopicOrder): Integer;
begin
  Result := List.Add('order', Value.ToConst);
end;

function TVkParamsBoardGet.Preview(Value: TVkBoardTopicPreview): Integer;
begin
  Result := List.Add('preview', Value.ToConst);
end;

function TVkParamsBoardGet.PreviewLength(Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

function TVkParamsBoardGet.TopicIds(Value: TIds): Integer;
begin
  Result := List.Add('topic_ids', Value);
end;

{ TVkParamsBoardAdd }

function TVkParamsBoardAdd.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsBoardAdd.FromGroup(Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsBoardAdd.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsBoardAdd.Text(Value: string): Integer;
begin
  Result := List.Add('text', Value);
end;

function TVkParamsBoardAdd.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

{ TVkParamsBoardCommentsGet }

function TVkParamsBoardCommentsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsBoardCommentsGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsBoardCommentsGet.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsBoardCommentsGet.NeedLikes(Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsBoardCommentsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsBoardCommentsGet.Sort(Value: Boolean): Integer;
begin
  if Value then
    Result := List.Add('sort', 'asc')
  else
    Result := List.Add('sort', 'desc');
end;

function TVkParamsBoardCommentsGet.StartCommentId(Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsBoardCommentsGet.TopicId(Value: Integer): Integer;
begin
  Result := List.Add('topic_id', Value);
end;

end.

