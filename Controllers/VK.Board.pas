unit VK.Board;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON, VK.Entity.Media,
  VK.Entity.Board;

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
    function FromGroup(Value: Boolean): Integer;
    function StickerId(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
    function TopicId(Value: Integer): Integer;
    function Guid(Value: string): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
  end;

  TVkParamsBoardGet = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function TopicIds(Value: TIds): Integer;
    function Order(Value: TVkBoardTopicOrder): Integer;
    function Count(Value: Integer = 40): Integer;
    function Offset(Value: Integer = 0): Integer;
    function Extended(Value: Boolean = False): Integer;
    function Preview(Value: TVkBoardTopicPreview): Integer;
    function PreviewLength(Value: Integer = 90): Integer;
  end;

  TBoardController = class(TVkController)
  public
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    /// <param name="var Id: Integer">Ид созданного комментария</param>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    /// <param name="Message: string">Комментарий</param>
    /// <param name="Attachments: TAttachmentArray = []">Массив вложений</param>
    function CreateComment(var Id: Integer; GroupId, TopicId: Integer; Message: string; Attachments: TAttachmentArray =
      []): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    /// <param name="Message: string">Комментарий</param>
    /// <param name="Attachments: TAttachmentArray = []">Массив вложений</param>
    function CreateComment(GroupId, TopicId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    /// <param name="var Id: Integer">Ид созданного комментария</param>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    /// <param name="Params: TVkParamsBoardCommentCreate">Параметры</param>
    function CreateComment(var Id: Integer; GroupId, TopicId: Integer; Params: TVkParamsBoardCommentCreate): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    /// <param name="Params: TVkParamsBoardCommentCreate">Параметры</param>
    function CreateComment(GroupId, TopicId: Integer; Params: TVkParamsBoardCommentCreate): Boolean; overload;
    /// <summary>
    /// Удаляет обсуждение
    /// </summary>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    function DeleteTopic(GroupId, TopicId: Integer): Boolean;
    /// <summary>
    /// Удаляет комментарий
    /// </summary>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    /// <param name="CommentId: Integer">Ид комментария</param>
    function DeleteComment(GroupId, TopicId, CommentId: Integer): Boolean;
    //
    function GetTopics(var Items: TVkBoardTopics; Params: TParams): Boolean; overload;
    //
    function GetTopics(var Items: TVkBoardTopics; Params: TVkParamsBoardGet): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TBoardController }

function TBoardController.CreateComment(var Id: Integer; GroupId, TopicId: Integer; Params: TVkParamsBoardCommentCreate): Boolean;
begin
  Id := -1;
  Params.GroupId(Abs(GroupId));
  Params.TopicId(TopicId);
  with Handler.Execute('board.createComment', Params.List) do
  begin
    Result := Success and TryStrToInt(Response, Id);
  end;
end;

function TBoardController.CreateComment(var Id: Integer; GroupId, TopicId: Integer; Message: string; Attachments:
  TAttachmentArray): Boolean;
var
  Params: TVkParamsBoardCommentCreate;
begin
  if not Message.IsEmpty then
    Params.Message(Message);
  if Length(Attachments) <> 0 then
    Params.Attachments(Attachments);

  Result := CreateComment(Id, GroupId, TopicId, Params);
end;

function TBoardController.CreateComment(GroupId, TopicId: Integer; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Id: Integer;
begin
  Result := CreateComment(Id, GroupId, TopicId, Message, Attachments);
end;

function TBoardController.CreateComment(GroupId, TopicId: Integer; Params: TVkParamsBoardCommentCreate): Boolean;
var
  Id: Integer;
begin
  Result := CreateComment(Id, GroupId, TopicId, Params);
end;

function TBoardController.DeleteComment(GroupId, TopicId, CommentId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', Abs(GroupId));
  Params.Add('topic_id', TopicId);
  Params.Add('comment_id', CommentId);
  with Handler.Execute('board.deleteComment', Params) do
    Result := Success and (Response = '1');
end;

function TBoardController.DeleteTopic(GroupId, TopicId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', Abs(GroupId));
  Params.Add('topic_id', TopicId);
  with Handler.Execute('board.deleteTopic', Params) do
    Result := Success and (Response = '1');
end;

function TBoardController.GetTopics(var Items: TVkBoardTopics;
  Params: TVkParamsBoardGet): Boolean;
begin
  Result := GetTopics(Items, Params.List);
end;

function TBoardController.GetTopics(var Items: TVkBoardTopics; Params: TParams): Boolean;
begin
  with Handler.Execute('board.getTopics', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkBoardTopics.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
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
  Result := List.Add('group_id', Abs(Value));
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

end.

