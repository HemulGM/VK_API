unit VK.Board;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, System.JSON, VK.Entity.Media, VK.Entity.Board,
  VK.Entity.Board.Comment;

type
  TVkParamsBoardCommentCreate = record
    List: TParams;
    /// <summary>
    /// True — тема будет создана от имени группы, False — тема будет создана от имени пользователя (по умолчанию).
    /// </summary>
    function FromGroup(const Value: Boolean): TVkParamsBoardCommentCreate;
    /// <summary>
    /// Идентификатор стикера.
    /// </summary>
    function StickerId(const Value: Integer): TVkParamsBoardCommentCreate;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsBoardCommentCreate;
    /// <summary>
    /// Идентификатор обсуждения.
    /// </summary>
    function TopicId(const Value: Integer): TVkParamsBoardCommentCreate;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинакового комментария.
    /// </summary>
    function Guid(const Value: string): TVkParamsBoardCommentCreate;
    /// <summary>
    /// Текст комментария. Обязательный параметр, если не передано значение
    /// </summary>
    function Message(const Value: string): TVkParamsBoardCommentCreate;
    /// <summary>
    /// Cписок объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsBoardCommentCreate;
  end;

  TVkParamsBoardCommentEdit = record
    List: TParams;
    /// <summary>
    /// True — тема будет создана от имени группы, False — тема будет создана от имени пользователя (по умолчанию).
    /// </summary>
    function FromGroup(const Value: Boolean): TVkParamsBoardCommentEdit;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsBoardCommentEdit;
    /// <summary>
    /// Идентификатор комментария в обсуждении.
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsBoardCommentEdit;
    /// <summary>
    /// Идентификатор обсуждения.
    /// </summary>
    function TopicId(const Value: Integer): TVkParamsBoardCommentEdit;
    /// <summary>
    /// Новый текст комментария (является обязательным, если не задан параметр attachments).
    /// </summary>
    function Message(const Value: string): TVkParamsBoardCommentEdit;
    /// <summary>
    /// Cписок объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsBoardCommentEdit;
  end;

  TVkParamsBoardGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsBoardGet;
    /// <summary>
    /// Список идентификаторов тем, которые необходимо получить (не более 100). По умолчанию возвращаются все темы. Если указан данный параметр, игнорируются параметры order, offset и count (возвращаются все запрошенные темы в указанном порядке).
    /// </summary>
    function TopicIds(const Value: TIdList): TVkParamsBoardGet;
    /// <summary>
    /// Порядок, в котором необходимо вернуть список тем.
    /// </summary>
    function Order(const Value: TVkBoardTopicOrder): TVkParamsBoardGet;
    /// <summary>
    /// Количество тем, которое необходимо получить (но не более 100). По умолчанию — 40.
    /// </summary>
    function Count(const Value: Integer = 40): TVkParamsBoardGet;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества тем.
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsBoardGet;
    /// <summary>
    /// Если указать в качестве этого параметра True, то будет возвращена информация о пользователях, являющихся создателями тем или оставившими в них последнее сообщение. По умолчанию False.
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsBoardGet;
    /// <summary>
    /// Набор флагов, определяющий, необходимо ли вернуть вместе с информацией о темах текст первых и последних сообщений в них. Является суммой флагов
    /// </summary>
    function Preview(const Value: TVkBoardTopicPreview): TVkParamsBoardGet;
    /// <summary>
    /// Количество символов, по которому нужно обрезать первое и последнее сообщение. Укажите 0, если Вы не хотите обрезать сообщение. (по умолчанию — 90).
    /// </summary>
    function PreviewLength(const Value: Integer = 90): TVkParamsBoardGet;
  end;

  TVkParamsBoardAdd = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsBoardAdd;
    /// <summary>
    /// Название обсуждения.
    /// </summary>
    function Title(const Value: string): TVkParamsBoardAdd;
    /// <summary>
    /// Текст первого сообщения в обсуждении.
    /// </summary>
    function Text(const Value: string): TVkParamsBoardAdd;
    /// <summary>
    /// True — тема будет создана от имени группы, False — тема будет создана от имени пользователя (по умолчанию).
    /// </summary>
    function FromGroup(const Value: Boolean): TVkParamsBoardAdd;
    /// <summary>
    /// Cписок объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsBoardAdd;
  end;

  TVkParamsBoardCommentsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsBoardCommentsGet;
    /// <summary>
    /// Идентификатор обсуждения.
    /// </summary>
    function TopicId(const Value: Integer): TVkParamsBoardCommentsGet;
    /// <summary>
    /// True — будет возвращено дополнительное поле likes. По умолчанию поле likes не возвращается.
    /// </summary>
    function NeedLikes(const Value: Boolean): TVkParamsBoardCommentsGet;
    /// <summary>
    /// Идентификатор комментария, начиная с которого нужно вернуть список (подробности см. ниже).
    /// </summary>
    function StartCommentId(const Value: Integer): TVkParamsBoardCommentsGet;
    /// <summary>
    /// Количество тем, которое необходимо получить (но не более 100). По умолчанию — 20.
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsBoardCommentsGet;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества тем.
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsBoardCommentsGet;
    /// <summary>
    /// Если указать в качестве этого параметра True, то будет возвращена информация о пользователях, являющихся авторами сообщений. По умолчанию False.
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsBoardCommentsGet;
    /// <summary>
    /// Порядок сортировки комментариев: asc — хронологический, desc — антихронологический
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsBoardCommentsGet;
  end;

  TBoardController = class(TVkController)
  public
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    function CreateComment(var Id: Integer; GroupId, TopicId: Integer; Message: string): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    function CreateComment(GroupId, TopicId: Integer; Message: string): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    function CreateComment(var Id: Integer; Params: TVkParamsBoardCommentCreate): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    function CreateComment(Params: TVkParamsBoardCommentCreate): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    function CreateComment(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Изменяет заголовок темы в списке обсуждений группы.
    /// </summary>
    function EditTopic(GroupId, TopicId: Integer; Title: string): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий в обсуждении
    /// </summary>
    function EditComment(Params: TVkParamsBoardCommentEdit): Boolean; overload;
    /// <summary>
    /// Удаляет обсуждение
    /// </summary>
    function DeleteTopic(GroupId, TopicId: Integer): Boolean;
    /// <summary>
    /// Удаляет сообщение темы в обсуждениях сообщества.
    /// </summary>
    function DeleteComment(GroupId, TopicId, CommentId: Integer): Boolean;
    /// <summary>
    /// Возвращает список тем в обсуждениях указанной группы.
    /// </summary>
    function GetTopics(var Items: TVkBoardTopics; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список тем в обсуждениях указанной группы.
    /// </summary>
    function GetTopics(var Items: TVkBoardTopics; Params: TVkParamsBoardGet): Boolean; overload;
    /// <summary>
    /// Создает новую тему в списке обсуждений группы.
    /// </summary>
    function AddTopic(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Создает новую тему в списке обсуждений группы.
    /// </summary>
    function AddTopic(Params: TVkParamsBoardAdd): Boolean; overload;
    /// <summary>
    /// Создает новую тему в списке обсуждений группы.
    /// </summary>
    function AddTopic(var Id: Integer; Params: TVkParamsBoardAdd): Boolean; overload;
    /// <summary>
    /// Закрывает тему в списке обсуждений группы (в такой теме невозможно оставлять новые сообщения).
    /// </summary>
    function CloseTopic(GroupId, TopicId: Integer): Boolean; overload;
    /// <summary>
    /// Открывает ранее закрытую тему (в ней станет возможно оставлять новые сообщения).
    /// </summary>
    function OpenTopic(GroupId, TopicId: Integer): Boolean; overload;
    /// <summary>
    /// Открывает ранее закрытую тему (в ней станет возможно оставлять новые сообщения).
    /// </summary>
    function RestoreComment(GroupId, TopicId, CommentId: Integer): Boolean; overload;
    /// <summary>
    /// Закрепляет тему в списке обсуждений группы (такая тема при любой сортировке выводится выше остальных).
    /// </summary>
    function FixTopic(GroupId, TopicId: Integer): Boolean; overload;
    /// <summary>
    /// Отменяет прикрепление темы в списке обсуждений группы (тема будет выводиться согласно выбранной сортировке).
    /// </summary>
    function UnfixTopic(GroupId, TopicId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает список сообщений в указанной теме.
    /// </summary>
    function GetComments(var Items: TVkBoardComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список сообщений в указанной теме.
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
  Result := Handler.Execute('board.createComment', Params).ResponseAsInt(Id);
end;

function TBoardController.CreateComment(Params: TVkParamsBoardCommentCreate): Boolean;
var
  Id: Integer;
begin
  Result := CreateComment(Id, Params.List);
end;

function TBoardController.AddTopic(var Id: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('board.addTopic', Params).ResponseIsTrue;
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
  Result := Handler.Execute('board.closeTopic', [
    ['group_id', GroupId.ToString],
    ['topic_id', TopicId.ToString]]).
    ResponseIsTrue;
end;

function TBoardController.DeleteComment(GroupId, TopicId, CommentId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('topic_id', TopicId);
  Params.Add('comment_id', CommentId);
  Result := Handler.Execute('board.deleteComment', Params).ResponseIsTrue;
end;

function TBoardController.DeleteTopic(GroupId, TopicId: Integer): Boolean;
begin
  Result := Handler.Execute('board.deleteTopic', [
    ['group_id', GroupId.ToString],
    ['topic_id', TopicId.ToString]]).
    ResponseIsTrue;
end;

function TBoardController.EditComment(Params: TVkParamsBoardCommentEdit): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TBoardController.EditTopic(GroupId, TopicId: Integer; Title: string): Boolean;
begin
  Result := Handler.Execute('board.editTopic', [
    ['group_id', GroupId.ToString],
    ['topic_id', TopicId.ToString],
    ['title', Title]]).
    ResponseIsTrue;
end;

function TBoardController.FixTopic(GroupId, TopicId: Integer): Boolean;
begin
  Result := Handler.Execute('board.fixTopic', [
    ['group_id', GroupId.ToString],
    ['topic_id', TopicId.ToString]]).
    ResponseIsTrue;
end;

function TBoardController.UnfixTopic(GroupId, TopicId: Integer): Boolean;
begin
  Result := Handler.Execute('board.unfixTopic', [
    ['group_id', GroupId.ToString],
    ['topic_id', TopicId.ToString]]).
    ResponseIsTrue;
end;

function TBoardController.EditComment(Params: TParams): Boolean;
begin
  Result := Handler.Execute('board.editComment', Params).ResponseIsTrue;
end;

function TBoardController.GetComments(var Items: TVkBoardComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('board.getComments', Params).GetObject(Items);
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
  Result := Handler.Execute('board.openTopic', [
    ['group_id', GroupId.ToString],
    ['topic_id', TopicId.ToString]]).
    ResponseIsTrue;
end;

function TBoardController.RestoreComment(GroupId, TopicId, CommentId: Integer): Boolean;
begin
  Result := Handler.Execute('board.restoreComment', [
    ['group_id', GroupId.ToString],
    ['topic_id', TopicId.ToString],
    ['comment_id', CommentId.ToString]]).
    ResponseIsTrue;
end;

function TBoardController.GetTopics(var Items: TVkBoardTopics; Params: TParams): Boolean;
begin
  Result := Handler.Execute('board.getTopics', Params).GetObject(Items);
end;

{ TVkParamsBoardCommentEdit }

function TVkParamsBoardCommentEdit.Attachments(const Value: TAttachmentArray): TVkParamsBoardCommentEdit;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsBoardCommentEdit.FromGroup(const Value: Boolean): TVkParamsBoardCommentEdit;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkParamsBoardCommentEdit.GroupId(const Value: TVkPeerId): TVkParamsBoardCommentEdit;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsBoardCommentEdit.Message(const Value: string): TVkParamsBoardCommentEdit;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsBoardCommentEdit.TopicId(const Value: Integer): TVkParamsBoardCommentEdit;
begin
  List.Add('topic_id', Value);
  Result := Self;
end;

function TVkParamsBoardCommentEdit.CommentId(const Value: Integer): TVkParamsBoardCommentEdit;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

{ TVkParamsBoardCommentCreate }

function TVkParamsBoardCommentCreate.Attachments(const Value: TAttachmentArray): TVkParamsBoardCommentCreate;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsBoardCommentCreate.FromGroup(const Value: Boolean): TVkParamsBoardCommentCreate;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkParamsBoardCommentCreate.GroupId(const Value: TVkPeerId): TVkParamsBoardCommentCreate;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsBoardCommentCreate.Guid(const Value: string): TVkParamsBoardCommentCreate;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkParamsBoardCommentCreate.Message(const Value: string): TVkParamsBoardCommentCreate;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsBoardCommentCreate.StickerId(const Value: Integer): TVkParamsBoardCommentCreate;
begin
  List.Add('sticker_id', Value);
  Result := Self;
end;

function TVkParamsBoardCommentCreate.TopicId(const Value: Integer): TVkParamsBoardCommentCreate;
begin
  List.Add('topic_id', Value);
  Result := Self;
end;

{ TVkParamsBoardGet }

function TVkParamsBoardGet.Count(const Value: Integer): TVkParamsBoardGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsBoardGet.Extended(const Value: Boolean): TVkParamsBoardGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsBoardGet.GroupId(const Value: TVkPeerId): TVkParamsBoardGet;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsBoardGet.Offset(const Value: Integer): TVkParamsBoardGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsBoardGet.Order(const Value: TVkBoardTopicOrder): TVkParamsBoardGet;
begin
  List.Add('order', Ord(Value));
  Result := Self;
end;

function TVkParamsBoardGet.Preview(const Value: TVkBoardTopicPreview): TVkParamsBoardGet;
begin
  List.Add('preview', Ord(Value));
  Result := Self;
end;

function TVkParamsBoardGet.PreviewLength(const Value: Integer): TVkParamsBoardGet;
begin
  List.Add('preview_length', Value);
  Result := Self;
end;

function TVkParamsBoardGet.TopicIds(const Value: TIdList): TVkParamsBoardGet;
begin
  List.Add('topic_ids', Value);
  Result := Self;
end;

{ TVkParamsBoardAdd }

function TVkParamsBoardAdd.Attachments(const Value: TAttachmentArray): TVkParamsBoardAdd;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsBoardAdd.FromGroup(const Value: Boolean): TVkParamsBoardAdd;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkParamsBoardAdd.GroupId(const Value: TVkPeerId): TVkParamsBoardAdd;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsBoardAdd.Text(const Value: string): TVkParamsBoardAdd;
begin
  List.Add('text', Value);
  Result := Self;
end;

function TVkParamsBoardAdd.Title(const Value: string): TVkParamsBoardAdd;
begin
  List.Add('title', Value);
  Result := Self;
end;

{ TVkParamsBoardCommentsGet }

function TVkParamsBoardCommentsGet.Count(const Value: Integer): TVkParamsBoardCommentsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsBoardCommentsGet.Extended(const Value: Boolean): TVkParamsBoardCommentsGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsBoardCommentsGet.GroupId(const Value: TVkPeerId): TVkParamsBoardCommentsGet;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsBoardCommentsGet.NeedLikes(const Value: Boolean): TVkParamsBoardCommentsGet;
begin
  List.Add('need_likes', Value);
  Result := Self;
end;

function TVkParamsBoardCommentsGet.Offset(const Value: Integer): TVkParamsBoardCommentsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsBoardCommentsGet.Sort(const Value: TVkSort): TVkParamsBoardCommentsGet;
begin
  List.Add('sort', Value.ToString);
  Result := Self;
end;

function TVkParamsBoardCommentsGet.StartCommentId(const Value: Integer): TVkParamsBoardCommentsGet;
begin
  List.Add('start_comment_id', Value);
  Result := Self;
end;

function TVkParamsBoardCommentsGet.TopicId(const Value: Integer): TVkParamsBoardCommentsGet;
begin
  List.Add('topic_id', Value);
  Result := Self;
end;

end.

