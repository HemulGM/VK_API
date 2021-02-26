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
    function NoteIds(const Value: TIdList): Integer;
    /// <summary>
    /// ������������� ������������, ���������� � �������� �������� ��������� ��������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �������
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// ���������� �������, ���������� � ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// ���������� ����������� (�� ���� ��������)
    /// </summary>
    function Sort(const Value: TVkSort): Integer;
  end;

  TVkParamsNotesAdd = record
    List: TParams;
    /// <summary>
    /// ��������� �������
    /// </summary>
    function Title(const Value: string): Integer;
    /// <summary>
    /// ����� �������
    /// </summary>
    function Text(const Value: string): Integer;
    /// <summary>
    /// ��������� ����������� ��������� �������
    /// </summary>
    function PrivacyView(const Value: TVkPrivacySettings): Integer;
    /// <summary>
    /// ��������� ����������� ��������������� �������
    /// </summary>
    function PrivacyComment(const Value: TVkPrivacySettings): Integer;
  end;

  TVkParamsNotesCreateComment = record
    List: TParams;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function NoteId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������������, ������� �� ����������� �������� �������� ����������� ����������� (�� ���������, ���� ����������� �� �������� �������)
    /// </summary>
    function ReplyTo(const Value: Integer): Integer;
    /// <summary>
    /// ����� �����������
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ����������� �����������
    /// </summary>
    function Guid(const Value: string): Integer;
  end;

  TVkParamsNotesGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function NoteId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������������
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// ���������� ������������, ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// ���������� ����������� (�� ���� ����������)
    /// </summary>
    function Sort(const Value: TVkSort = TVkSort.Asc): Integer;
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

function TVkParamsNotesGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNotesGet.NoteIds(const Value: TIdList): Integer;
begin
  Result := List.Add('notes_ids', Value.ToString);
end;

function TVkParamsNotesGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsNotesGet.Sort(const Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsNotesGet.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsNotesAdd }

function TVkParamsNotesAdd.PrivacyComment(const Value: TVkPrivacySettings): Integer;
begin
  Result := List.Add('privacy_comment', Value.ToString);
end;

function TVkParamsNotesAdd.PrivacyView(const Value: TVkPrivacySettings): Integer;
begin
  Result := List.Add('privacy_view', Value.ToString);
end;

function TVkParamsNotesAdd.Text(const Value: string): Integer;
begin
  Result := List.Add('text', Value);
end;

function TVkParamsNotesAdd.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

{ TVkParamsNotesCreateComment }

function TVkParamsNotesCreateComment.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsNotesCreateComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsNotesCreateComment.NoteId(const Value: Integer): Integer;
begin
  Result := List.Add('note_id', Value);
end;

function TVkParamsNotesCreateComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsNotesCreateComment.ReplyTo(const Value: Integer): Integer;
begin
  Result := List.Add('reply_to', Value);
end;

{ TVkParamsNotesGetComments }

function TVkParamsNotesGetComments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNotesGetComments.NoteId(const Value: Integer): Integer;
begin
  Result := List.Add('note_id', Value);
end;

function TVkParamsNotesGetComments.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsNotesGetComments.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsNotesGetComments.Sort(const Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

end.

