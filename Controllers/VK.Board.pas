unit VK.Board;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types,
  VK.Entity.Audio, System.JSON, VK.Entity.Media;

type
  TVkBoardCommentParams = record
    List: TParams;
    function FromGroup(Value: Boolean): Integer;
    function StickerId(Value: Integer): Integer;
    function GroupId(Value: Integer): Integer;
    function TopicId(Value: Integer): Integer;
    function Guid(Value: string): Integer;
    function Message(Value: string): Integer;
    function Attachments(Value: TAttachmentArray): Integer;
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
    function CreateComment(var Id: Integer; GroupId, TopicId: Integer; Message: string; Attachments:
      TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    /// <param name="Message: string">Комментарий</param>
    /// <param name="Attachments: TAttachmentArray = []">Массив вложений</param>
    function CreateComment(GroupId, TopicId: Integer; Message: string; Attachments: TAttachmentArray
      = []): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    /// <param name="var Id: Integer">Ид созданного комментария</param>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    /// <param name="Params: TVkBoardCommentParams">Параметры</param>
    function CreateComment(var Id: Integer; GroupId, TopicId: Integer; Params: TVkBoardCommentParams):
      Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    /// <param name="GroupId: Integer">Ид группы</param>
    /// <param name="TopicId: Integer">Ид обсуждения</param>
    /// <param name="Params: TVkBoardCommentParams">Параметры</param>
    function CreateComment(GroupId, TopicId: Integer; Params: TVkBoardCommentParams): Boolean; overload;
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
  end;

implementation

uses
  VK.API, VK.Utils;

{ TBoardController }

function TBoardController.CreateComment(var Id: Integer; GroupId, TopicId: Integer; Params:
  TVkBoardCommentParams): Boolean;
begin
  Id := -1;
  Params.GroupId(Abs(GroupId));
  Params.TopicId(TopicId);
  with Handler.Execute('board.createComment', Params.List) do
  begin
    Result := Success and TryStrToInt(Response, Id);
  end;
end;

function TBoardController.CreateComment(var Id: Integer; GroupId, TopicId: Integer; Message: string;
  Attachments: TAttachmentArray): Boolean;
var
  Params: TVkBoardCommentParams;
begin
  if not Message.IsEmpty then
    Params.Message(Message);
  if Length(Attachments) <> 0 then
    Params.Attachments(Attachments);

  Result := CreateComment(Id, GroupId, TopicId, Params);
end;

function TBoardController.CreateComment(GroupId, TopicId: Integer; Message: string; Attachments:
  TAttachmentArray): Boolean;
var
  Id: Integer;
begin
  Result := CreateComment(Id, GroupId, TopicId, Message, Attachments);
end;

function TBoardController.CreateComment(GroupId, TopicId: Integer; Params: TVkBoardCommentParams): Boolean;
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

{ TVkBoardCommentParams }

function TVkBoardCommentParams.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToString)
end;

function TVkBoardCommentParams.FromGroup(Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkBoardCommentParams.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkBoardCommentParams.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkBoardCommentParams.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkBoardCommentParams.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkBoardCommentParams.TopicId(Value: Integer): Integer;
begin
  Result := List.Add('topic_id', Value);
end;

end.

