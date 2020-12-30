unit VK.Notes;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Audio, System.JSON, VK.Entity.Note;

type
  TVkParamsNotesGet = record
    List: TParams;
    function NoteIds(Value: TIds): Integer;
    function UserId(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer = 20): Integer;
    function Sort(Value: Boolean): Integer;
  end;

  TVkParamsNotesAdd = record
    List: TParams;
    function Title(Value: string): Integer;
    function Text(Value: string): Integer;
    function PrivacyView(Value: TArrayOfString): Integer;
    function PrivacyComment(Value: TArrayOfString): Integer;
  end;

  TVkParamsNotesCreateComment = record
    List: TParams;
    function NoteId(Value: Integer): Integer;
    function OwnerId(Value: Integer): Integer;
    function ReplyTo(Value: Integer): Integer;
    function Message(Value: string): Integer;
    function Guid(Value: string): Integer;
  end;

  TVkParamsNotesGetComments = record
    List: TParams;
    function NoteId(Value: Integer): Integer;
    function OwnerId(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer = 20): Integer;
    function Sort(Value: Boolean): Integer;
  end;

  TNotesController = class(TVkController)
  public
    /// <summary>
    /// Создает новую заметку у текущего пользователя.
    /// </summary>
    function Add(var NoteId: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Создает новую заметку у текущего пользователя.
    /// </summary>
    function Add(var NoteId: Integer; Params: TVkParamsNotesAdd): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий к заметке.
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Добавляет новый комментарий к заметке.
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TVkParamsNotesCreateComment): Boolean; overload;
    /// <summary>
    /// Удаляет заметку текущего пользователя.
    /// </summary>
    function Delete(NoteId: Integer): Boolean;
    /// <summary>
    /// Удаляет комментарий к заметке.
    /// </summary>
    function DeleteComment(CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Редактирует заметку текущего пользователя.
    /// </summary>
    function Edit(NoteId: Integer; Params: TVkParamsNotesAdd): Boolean;
    /// <summary>
    /// Редактирует заметку текущего пользователя.
    /// </summary>
    function EditComment(CommentId: Integer; Message: string; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список заметок, созданных пользователем.
    /// </summary>
    function Get(var Items: TVkNotes; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список заметок, созданных пользователем.
    /// </summary>
    function Get(var Items: TVkNotes; Params: TVkParamsNotesGet): Boolean; overload;
    /// <summary>
    /// Возвращает заметку по её id.
    /// </summary>
    function GetById(var Item: TVkNote; NoteId: Integer; OwnerId: Integer = 0; NeedWiki: Boolean = False): Boolean;
    /// <summary>
    /// Возвращает список комментариев к заметке.
    /// </summary>
    function GetComments(var Items: TVkNoteComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к заметке.
    /// </summary>
    function GetComments(var Items: TVkNoteComments; Params: TVkParamsNotesGetComments): Boolean; overload;
    /// <summary>
    /// Восстанавливает удалённый комментарий.
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TNotesController }

function TNotesController.Get(var Items: TVkNotes; Params: TParams): Boolean;
begin
  with Handler.Execute('notes.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkNotes.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TNotesController.Add(var NoteId: Integer; Params: TParams): Boolean;
begin
  with Handler.Execute('notes.add', Params) do
    Result := Success and TryStrToInt(Response, NoteId);
end;

function TNotesController.Add(var NoteId: Integer; Params: TVkParamsNotesAdd): Boolean;
begin
  Result := Add(NoteId, Params.List);
end;

function TNotesController.CreateComment(var CommentId: Integer; Params: TVkParamsNotesCreateComment): Boolean;
begin
  Result := CreateComment(CommentId, Params.List);
end;

function TNotesController.Delete(NoteId: Integer): Boolean;
begin
  with Handler.Execute('notes.delete', ['note_id', NoteId.ToString]) do
    Result := Success and ResponseIsTrue;
end;

function TNotesController.DeleteComment(CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('notes.deleteComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TNotesController.Edit(NoteId: Integer; Params: TVkParamsNotesAdd): Boolean;
begin
  Params.List.Add('note_id', NoteId);
  with Handler.Execute('notes.edit', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TNotesController.EditComment(CommentId: Integer; Message: string; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('message', Message);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('notes.editComment', Params) do
    Result := Success and TryStrToInt(Response, CommentId);
end;

function TNotesController.CreateComment(var CommentId: Integer; Params: TParams): Boolean;
begin
  with Handler.Execute('notes.createComment', Params) do
    Result := Success and TryStrToInt(Response, CommentId);
end;

function TNotesController.Get(var Items: TVkNotes; Params: TVkParamsNotesGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TNotesController.GetById(var Item: TVkNote; NoteId, OwnerId: Integer; NeedWiki: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('note_id', NoteId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  if NeedWiki then
    Params.Add('need_wiki', NeedWiki);
  with Handler.Execute('notes.getById', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkNote.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TNotesController.GetComments(var Items: TVkNoteComments; Params: TVkParamsNotesGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TNotesController.RestoreComment(CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  with Handler.Execute('notes.restoreComment', Params) do
    Result := Success and ResponseIsTrue;
end;

function TNotesController.GetComments(var Items: TVkNoteComments; Params: TParams): Boolean;
begin
  with Handler.Execute('notes.getComments', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkNoteComments.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkParamsNotesGet }

function TVkParamsNotesGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNotesGet.NoteIds(Value: TIds): Integer;
begin
  Result := List.Add('notes_ids', Value.ToString);
end;

function TVkParamsNotesGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsNotesGet.Sort(Value: Boolean): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsNotesGet.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsNotesAdd }

function TVkParamsNotesAdd.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsNotesAdd.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsNotesAdd.Text(Value: string): Integer;
begin
  Result := List.Add('text', Value);
end;

function TVkParamsNotesAdd.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

{ TVkParamsNotesCreateComment }

function TVkParamsNotesCreateComment.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsNotesCreateComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsNotesCreateComment.NoteId(Value: Integer): Integer;
begin
  Result := List.Add('note_id', Value);
end;

function TVkParamsNotesCreateComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsNotesCreateComment.ReplyTo(Value: Integer): Integer;
begin
  Result := List.Add('reply_to', Value);
end;

{ TVkParamsNotesGetComments }

function TVkParamsNotesGetComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNotesGetComments.NoteId(Value: Integer): Integer;
begin
  Result := List.Add('note_id', Value);
end;

function TVkParamsNotesGetComments.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsNotesGetComments.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsNotesGetComments.Sort(Value: Boolean): Integer;
begin
  Result := List.Add('sort', Value);
end;

end.

