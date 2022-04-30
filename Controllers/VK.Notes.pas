unit VK.Notes;

interface

uses
  System.SysUtils, VK.Controller, VK.Types, VK.Entity.Note;

type
  TVkParamsNotesGet = record
    List: TParams;
    /// <summary>
    /// �������������� �������, ���������� � ������� ���������� ��������
    /// </summary>
    function NoteIds(const Value: TIdList): TVkParamsNotesGet;
    /// <summary>
    /// ������������� ������������, ���������� � �������� �������� ��������� ��������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsNotesGet;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �������
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsNotesGet;
    /// <summary>
    /// ���������� �������, ���������� � ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsNotesGet;
    /// <summary>
    /// ���������� ����������� (�� ���� ��������)
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsNotesGet;
  end;

  TVkParamsNotesAdd = record
    List: TParams;
    /// <summary>
    /// ��������� �������
    /// </summary>
    function Title(const Value: string): TVkParamsNotesAdd;
    /// <summary>
    /// ����� �������
    /// </summary>
    function Text(const Value: string): TVkParamsNotesAdd;
    /// <summary>
    /// ��������� ����������� ��������� �������
    /// </summary>
    function PrivacyView(const Value: TVkPrivacySettings): TVkParamsNotesAdd;
    /// <summary>
    /// ��������� ����������� ��������������� �������
    /// </summary>
    function PrivacyComment(const Value: TVkPrivacySettings): TVkParamsNotesAdd;
  end;

  TVkParamsNotesCreateComment = record
    List: TParams;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function NoteId(const Value: Integer): TVkParamsNotesCreateComment;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsNotesCreateComment;
    /// <summary>
    /// ������������� ������������, ������� �� ����������� �������� �������� ����������� ����������� (�� ���������, ���� ����������� �� �������� �������)
    /// </summary>
    function ReplyTo(const Value: Integer): TVkParamsNotesCreateComment;
    /// <summary>
    /// ����� �����������
    /// </summary>
    function Message(const Value: string): TVkParamsNotesCreateComment;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ����������� �����������
    /// </summary>
    function Guid(const Value: string): TVkParamsNotesCreateComment;
  end;

  TVkParamsNotesGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function NoteId(const Value: Integer): TVkParamsNotesGetComments;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsNotesGetComments;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������������
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsNotesGetComments;
    /// <summary>
    /// ���������� ������������, ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsNotesGetComments;
    /// <summary>
    /// ���������� ����������� (�� ���� ����������)
    /// </summary>
    function Sort(const Value: TVkSort = TVkSort.Asc): TVkParamsNotesGetComments;
  end;

  TNotesController = class(TVkController)
  public
    /// <summary>
    /// ������� ����� ������� � �������� ������������.
    /// </summary>
    function Add(var NoteId: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// ������� ����� ������� � �������� ������������.
    /// </summary>
    function Add(var NoteId: Integer; Params: TVkParamsNotesAdd): Boolean; overload;
    /// <summary>
    /// ��������� ����� ����������� � �������.
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ����� ����������� � �������.
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TVkParamsNotesCreateComment): Boolean; overload;
    /// <summary>
    /// ������� ������� �������� ������������.
    /// </summary>
    function Delete(var Status: Boolean; NoteId: Integer): Boolean;
    /// <summary>
    /// ������� ����������� � �������.
    /// </summary>
    function DeleteComment(var Status: Boolean; CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ����������� ������� �������� ������������.
    /// </summary>
    function Edit(var Status: Boolean; NoteId: Integer; Params: TVkParamsNotesAdd): Boolean;
    /// <summary>
    /// ����������� ������� �������� ������������.
    /// </summary>
    function EditComment(CommentId: Integer; Message: string; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ���������� ������ �������, ��������� �������������.
    /// </summary>
    function Get(var Items: TVkNotes; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ �������, ��������� �������������.
    /// </summary>
    function Get(var Items: TVkNotes; Params: TVkParamsNotesGet): Boolean; overload;
    /// <summary>
    /// ���������� ������� �� � id.
    /// </summary>
    function GetById(var Item: TVkNote; NoteId: Integer; OwnerId: Integer = 0; NeedWiki: Boolean = False): Boolean;
    /// <summary>
    /// ���������� ������ ������������ � �������.
    /// </summary>
    function GetComments(var Items: TVkNoteComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � �������.
    /// </summary>
    function GetComments(var Items: TVkNoteComments; Params: TVkParamsNotesGetComments): Boolean; overload;
    /// <summary>
    /// ��������������� �������� �����������.
    /// </summary>
    function RestoreComment(var Status: Boolean; CommentId: Integer; OwnerId: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TNotesController }

function TNotesController.Get(var Items: TVkNotes; Params: TParams): Boolean;
begin
  Result := Handler.Execute('notes.get', Params).GetObject(Items);
end;

function TNotesController.Add(var NoteId: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('notes.add', Params).ResponseAsInt(NoteId);
end;

function TNotesController.Add(var NoteId: Integer; Params: TVkParamsNotesAdd): Boolean;
begin
  Result := Add(NoteId, Params.List);
end;

function TNotesController.CreateComment(var CommentId: Integer; Params: TVkParamsNotesCreateComment): Boolean;
begin
  Result := CreateComment(CommentId, Params.List);
end;

function TNotesController.Delete(var Status: Boolean; NoteId: Integer): Boolean;
begin
  Result := Handler.Execute('notes.delete', ['note_id', NoteId.ToString]).ResponseAsBool(Status);
end;

function TNotesController.DeleteComment(var Status: Boolean; CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('notes.deleteComment', Params).ResponseAsBool(Status);
end;

function TNotesController.Edit(var Status: Boolean; NoteId: Integer; Params: TVkParamsNotesAdd): Boolean;
begin
  Params.List.Add('note_id', NoteId);
  Result := Handler.Execute('notes.edit', Params.List).ResponseAsBool(Status);
end;

function TNotesController.EditComment(CommentId: Integer; Message: string; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('message', Message);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('notes.editComment', Params).ResponseAsInt(CommentId);
end;

function TNotesController.CreateComment(var CommentId: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('notes.createComment', Params).ResponseAsInt(CommentId);
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
  Result := Handler.Execute('notes.getById', Params).GetObject(Item);
end;

function TNotesController.GetComments(var Items: TVkNoteComments; Params: TVkParamsNotesGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TNotesController.RestoreComment(var Status: Boolean; CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('notes.restoreComment', Params).ResponseAsBool(Status);
end;

function TNotesController.GetComments(var Items: TVkNoteComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('notes.getComments', Params).GetObject(Items);
end;

{ TVkParamsNotesGet }

function TVkParamsNotesGet.Count(const Value: Integer): TVkParamsNotesGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsNotesGet.NoteIds(const Value: TIdList): TVkParamsNotesGet;
begin
  List.Add('notes_ids', Value.ToString);
  Result := Self;
end;

function TVkParamsNotesGet.Offset(const Value: Integer): TVkParamsNotesGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsNotesGet.Sort(const Value: TVkSort): TVkParamsNotesGet;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

function TVkParamsNotesGet.UserId(const Value: Integer): TVkParamsNotesGet;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsNotesAdd }

function TVkParamsNotesAdd.PrivacyComment(const Value: TVkPrivacySettings): TVkParamsNotesAdd;
begin
  List.Add('privacy_comment', Value.ToString);
  Result := Self;
end;

function TVkParamsNotesAdd.PrivacyView(const Value: TVkPrivacySettings): TVkParamsNotesAdd;
begin
  List.Add('privacy_view', Value.ToString);
  Result := Self;
end;

function TVkParamsNotesAdd.Text(const Value: string): TVkParamsNotesAdd;
begin
  List.Add('text', Value);
  Result := Self;
end;

function TVkParamsNotesAdd.Title(const Value: string): TVkParamsNotesAdd;
begin
  List.Add('title', Value);
  Result := Self;
end;

{ TVkParamsNotesCreateComment }

function TVkParamsNotesCreateComment.Guid(const Value: string): TVkParamsNotesCreateComment;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkParamsNotesCreateComment.Message(const Value: string): TVkParamsNotesCreateComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsNotesCreateComment.NoteId(const Value: Integer): TVkParamsNotesCreateComment;
begin
  List.Add('note_id', Value);
  Result := Self;
end;

function TVkParamsNotesCreateComment.OwnerId(const Value: Integer): TVkParamsNotesCreateComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsNotesCreateComment.ReplyTo(const Value: Integer): TVkParamsNotesCreateComment;
begin
  List.Add('reply_to', Value);
  Result := Self;
end;

{ TVkParamsNotesGetComments }

function TVkParamsNotesGetComments.Count(const Value: Integer): TVkParamsNotesGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsNotesGetComments.NoteId(const Value: Integer): TVkParamsNotesGetComments;
begin
  List.Add('note_id', Value);
  Result := Self;
end;

function TVkParamsNotesGetComments.Offset(const Value: Integer): TVkParamsNotesGetComments;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsNotesGetComments.OwnerId(const Value: Integer): TVkParamsNotesGetComments;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsNotesGetComments.Sort(const Value: TVkSort): TVkParamsNotesGetComments;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

end.

