unit VK.Polls;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Poll;

type
  TVkParamsPollsAddVote = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPollsAddVote;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): TVkParamsPollsAddVote;
    /// <summary>
    /// ������ ��������������� ������ (��� ������ � �������������)
    /// </summary>
    function AnswerIds(const Value: TIdList): TVkParamsPollsAddVote;
    /// <summary>
    /// True � ����� ��������� � ����������, False � ����� ���������� � �����
    /// </summary>
    function IsBoard(const Value: Boolean): TVkParamsPollsAddVote;
  end;

  TVkParamsPollsCreate = record
    List: TParams;
    /// <summary>
    /// ����� �������
    /// </summary>
    function Question(const Value: string): TVkParamsPollsCreate;
    /// <summary>
    /// True � ��������� �����, ������ ��������������� ����������;
    /// False � ����� ���������, ������ ��������������� ��������
    /// </summary>
    function IsAnonymous(const Value: Boolean): TVkParamsPollsCreate;
    /// <summary>
    /// True � ��� �������� ������ � �������������
    /// </summary>
    function IsMultiple(const Value: Boolean): TVkParamsPollsCreate;
    /// <summary>
    /// ���� ���������� ������
    /// </summary>
    function EndDate(const Value: TDateTime): TVkParamsPollsCreate;
    /// <summary>
    /// ���� ����� ����� �������� � ������, ���������� �������� ������������� ������������� ������. �� ��������� ������� ������������
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPollsCreate;
    /// <summary>
    /// ������ ��������� �������
    /// ����� ���� �� ����� 1 � �� ����� 10 ��������� ������
    /// </summary>
    function AddAnswers(const Value: TArrayOfString): TVkParamsPollsCreate;
    /// <summary>
    /// ������������� ���������� ��� ������������� � �������� ���� ��������
    /// </summary>
    function PhotoId(const Value: Cardinal): TVkParamsPollsCreate;
    /// <summary>
    /// ������������� ������������ ���� ��� ��������
    /// </summary>
    function BackgroundId(const Value: Integer): TVkParamsPollsCreate; overload;
    /// <summary>
    /// ������������� ������������ ���� ��� ��������
    /// </summary>
    function BackgroundId(const Value: TVkPollBackground): TVkParamsPollsCreate; overload;
    /// <summary>
    /// ��������� ������ �����
    /// </summary>
    function DisableUnvote(const Value: Boolean): TVkParamsPollsCreate;
  end;

  TVkParamsPollsDeleteVote = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPollsDeleteVote;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): TVkParamsPollsDeleteVote;
    /// <summary>
    /// ������������� �������� ������
    /// </summary>
    function AnswerId(const Value: Integer): TVkParamsPollsDeleteVote;
    /// <summary>
    /// True � ����� ��������� � ����������, False � ����� ���������� � �����
    /// </summary>
    function IsBoard(const Value: Boolean): TVkParamsPollsDeleteVote;
  end;

  TVkParamsPollsEdit = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPollsEdit;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): TVkParamsPollsEdit;
    /// <summary>
    /// ����� ����� �������������� ������
    /// </summary>
    function Question(const Value: string): TVkParamsPollsEdit;
    /// <summary>
    /// ������ ��������� �������
    /// ����� ���� �� ����� 1 � �� ����� 10 ��������� ������
    /// </summary>
    function AddAnswers(const Value: TArrayOfString): TVkParamsPollsEdit;
    /// <summary>
    /// ������, ���������� �������� �������, ������� ���������� ���������������;
    /// ���� � ������������� ������, �������� � ����� ����� ������
    /// </summary>
    function EditAnswers(const Value: TVkVoteAnswers): TVkParamsPollsEdit;
    /// <summary>
    /// ������ ��������������� �������, ������� ���������� �������
    /// </summary>
    function DeleteAnswers(const Value: TIdList): TVkParamsPollsEdit;
    /// <summary>
    /// ���� ���������� ������
    /// </summary>
    function EndDate(const Value: TDateTime): TVkParamsPollsEdit;
    /// <summary>
    /// ������������� ���������� ��� ��������
    /// </summary>
    function PhotoId(const Value: Cardinal): TVkParamsPollsEdit;
    /// <summary>
    /// ������������� ������������ ���� ��� ��������
    /// </summary>
    function BackgroundId(const Value: Integer): TVkParamsPollsEdit; overload;
    /// <summary>
    /// ������������� ������������ ���� ��� ��������
    /// </summary>
    function BackgroundId(const Value: TVkPollBackground): TVkParamsPollsEdit; overload;
  end;

  TVkParamsPollsGetById = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPollsGetById;
    /// <summary>
    /// True � ����� ��������� � ����������, False � ����� ���������� � �����
    /// </summary>
    function IsBoard(const Value: Boolean = False): TVkParamsPollsGetById;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): TVkParamsPollsGetById;
    /// <summary>
    /// True � ���������� �������������� ���������� � �������� �������������
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsPollsGetById;
    /// <summary>
    /// ����� ��������������� ��������������� ������, ������� ���������� ������� � ������
    /// </summary>
    function FriendsCount(const Value: Integer = 3): TVkParamsPollsGetById;
    /// <summary>
    /// ������ �������������� ����� ��������
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsPollsGetById;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsPollsGetById;
  end;

  TVkParamsPollsGetVoters = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPollsGetVoters;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): TVkParamsPollsGetVoters;
    /// <summary>
    /// ������ ��������������� ������ (��� ������ � �������������)
    /// </summary>
    function AnswerIds(const Value: TIdList): TVkParamsPollsGetVoters;
    /// <summary>
    /// True � ����� ��������� � ����������, False � ����� ���������� � �����
    /// </summary>
    function IsBoard(const Value: Boolean): TVkParamsPollsGetVoters;
    /// <summary>
    /// ���������, ���������� �� ���������� ������ �������������, ������� �������� �������� �������� ������������. �������� ����� ��������� ��������� ��������:
    /// False � ���������� ���� ������������� � ������� �������� ������� �����������;
    /// True � ���������� ������ ������ �������� ������������ � ������� �������� ������� �����������.
    /// ���� �������� �� ��� �����, �� ���������, ��� �� ����� False
    /// </summary>
    function FriendsOnly(const Value: Boolean = False): TVkParamsPollsGetVoters;
    /// <summary>
    /// �������� ������������ ������ ������, ��� ������� ������������� ������������. ���� �������� �� �����, �� ���������, ��� �� ����� 0
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsPollsGetVoters;
    /// <summary>
    /// ���������� ������������ ��������������� �������������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� 100, ���� �� ����� �������� FriendsOnly, � ��������� ������ 10.
    /// ������������ �������� ��������� 1000, ���� �� ����� �������� FriendsOnly, � ��������� ������ 100
    /// </summary>
    function Count(const Value: Integer): TVkParamsPollsGetVoters;
    /// <summary>
    /// ������������� ����� ������� ���� �����, ����������� ��� ���������
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsPollsGetVoters;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsPollsGetVoters;
  end;

  /// <summary>
  /// ������ ��� ������ � ��������.
  /// </summary>
  TPollsController = class(TVkController)
  public
    /// <summary>
    /// ������ ����� �������� ������������ �� ��������� ������� ������ � ��������� ������.
    /// </summary>
    function AddVote(const Params: TVkParamsPollsAddVote): Boolean;
    /// <summary>
    /// ��������� ��������� ������, ������� ������������ ����� ����������� � ������� �� �������� ������������ ��� ����������.
    /// </summary>
    function &Create(var Item: TVkPoll; const Params: TVkParamsPollsCreate): Boolean;
    /// <summary>
    /// ������� ����� �������� ������������ � ���������� �������� ������ � ��������� ������.
    /// </summary>
    function DeleteVote(const Params: TVkParamsPollsDeleteVote): Boolean;
    /// <summary>
    /// ��������� ������������� ��������� ������.
    /// </summary>
    function Edit(const Params: TVkParamsPollsEdit): Boolean;
    /// <summary>
    /// ���������� �������� �������� ����������� ��� �������.
    /// </summary>
    function GetBackgrounds(var Items: TVkPollBackgrounds): Boolean;
    /// <summary>
    /// ���������� ��������� ���������� �� ������ �� ��� ��������������.
    /// </summary>
    function GetById(var Item: TVkPoll; Params: TVkParamsPollsGetById): Boolean; overload;
    /// <summary>
    /// ���������� ��������� ���������� �� ������ �� ��� ��������������.
    /// </summary>
    function GetById(var Item: TVkPoll; PollId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ������� ���������� � �����.
    /// </summary>
    function GetPhotoUploadServer(var UploadUrl: string; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// �������� ������ ��������������� �������������, ������� ������� ������������ �������� ������ � ������.
    /// </summary>
    function GetVoters(var Items: TVkPollVoters; Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ������ ��������������� �������������, ������� ������� ������������ �������� ������ � ������.
    /// </summary>
    function GetVoters(var Items: TVkPollVoters; Params: TVkParamsPollsGetVoters): Boolean; overload;
    /// <summary>
    /// ��������� ����������, ����������� � �����.
    /// </summary>
    function SavePhoto(var Id: Integer; const Photo, Hash: string): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TPollsController }

function TPollsController.AddVote(const Params: TVkParamsPollsAddVote): Boolean;
begin
  Result := Handler.Execute('polls.addVote', Params.List).ResponseIsTrue;
end;

function TPollsController.&Create(var Item: TVkPoll; const Params: TVkParamsPollsCreate): Boolean;
begin
  Result := Handler.Execute('polls.create', Params.List).GetObject(Item);
end;

function TPollsController.DeleteVote(const Params: TVkParamsPollsDeleteVote): Boolean;
begin
  Result := Handler.Execute('polls.deleteVote', Params.List).ResponseIsTrue;
end;

function TPollsController.Edit(const Params: TVkParamsPollsEdit): Boolean;
begin
  Result := Handler.Execute('polls.edit', Params.List).ResponseIsTrue;
end;

function TPollsController.GetBackgrounds(var Items: TVkPollBackgrounds): Boolean;
begin
  Result := Handler.Execute('polls.getBackgrounds').GetObjects(Items);
end;

function TPollsController.GetById(var Item: TVkPoll; PollId: Integer): Boolean;
var
  Params: TVkParamsPollsGetById;
begin
  Params.PollId(PollId);
  Result := GetById(Item, Params);
end;

function TPollsController.GetPhotoUploadServer(var UploadUrl: string; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('polls.getPhotoUploadServer', Params).GetValue('upload_url', UploadUrl);
end;

function TPollsController.GetVoters(var Items: TVkPollVoters; Params: TVkParamsPollsGetVoters): Boolean;
begin
  Result := GetVoters(Items, Params.List);
end;

function TPollsController.SavePhoto(var Id: Integer; const Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('hash', Hash);
  Result := Handler.Execute('polls.savePhoto', Params).ResponseAsInt(Id);
end;

function TPollsController.GetVoters(var Items: TVkPollVoters; Params: TParams): Boolean;
begin
  if not Params.KeyExists('fields') then
    Params.Add('fields', 'domain');
  Result := Handler.Execute('polls.getVoters', Params).GetObject(Items);
end;

function TPollsController.GetById(var Item: TVkPoll; Params: TVkParamsPollsGetById): Boolean;
begin
  Result := Handler.Execute('polls.getById', Params.List).GetObject(Item);
end;

{ TVkParamsPollsAddVote }

function TVkParamsPollsAddVote.OwnerId(const Value: TVkPeerId): TVkParamsPollsAddVote;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPollsAddVote.PollId(const Value: Integer): TVkParamsPollsAddVote;
begin
  List.Add('poll_id', Value);
  Result := Self;
end;

function TVkParamsPollsAddVote.AnswerIds(const Value: TIdList): TVkParamsPollsAddVote;
begin
  List.Add('answer_ids', Value);
  Result := Self;
end;

function TVkParamsPollsAddVote.IsBoard(const Value: Boolean): TVkParamsPollsAddVote;
begin
  List.Add('is_board', Value);
  Result := Self;
end;

{ TVkParamsPollsCreate }

function TVkParamsPollsCreate.Question(const Value: string): TVkParamsPollsCreate;
begin
  List.Add('question', Value);
  Result := Self;
end;

function TVkParamsPollsCreate.IsAnonymous(const Value: Boolean): TVkParamsPollsCreate;
begin
  List.Add('is_anonymous', Value);
  Result := Self;
end;

function TVkParamsPollsCreate.IsMultiple(const Value: Boolean): TVkParamsPollsCreate;
begin
  List.Add('is_multiple', Value);
  Result := Self;
end;

function TVkParamsPollsCreate.EndDate(const Value: TDateTime): TVkParamsPollsCreate;
begin
  List.Add('end_date', Value);
  Result := Self;
end;

function TVkParamsPollsCreate.OwnerId(const Value: TVkPeerId): TVkParamsPollsCreate;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPollsCreate.AddAnswers(const Value: TArrayOfString): TVkParamsPollsCreate;
begin
  List.Add('add_answers', Value.ToJson);
  Result := Self;
end;

function TVkParamsPollsCreate.PhotoId(const Value: Cardinal): TVkParamsPollsCreate;
begin
  List.Add('photo_id', Value);
  Result := Self;
end;

function TVkParamsPollsCreate.BackgroundId(const Value: Integer): TVkParamsPollsCreate;
begin
  List.Add('background_id', Value);
  Result := Self;
end;

function TVkParamsPollsCreate.BackgroundId(const Value: TVkPollBackground): TVkParamsPollsCreate;
begin
  List.Add('background_id', Value.Id);
  Result := Self;
end;

function TVkParamsPollsCreate.DisableUnvote(const Value: Boolean): TVkParamsPollsCreate;
begin
  List.Add('disable_unvote', Value);
  Result := Self;
end;

{ TVkParamsPollsDeleteVote }

function TVkParamsPollsDeleteVote.AnswerId(const Value: Integer): TVkParamsPollsDeleteVote;
begin
  List.Add('answer_id', Value);
  Result := Self;
end;

function TVkParamsPollsDeleteVote.IsBoard(const Value: Boolean): TVkParamsPollsDeleteVote;
begin
  List.Add('is_board', Value);
  Result := Self;
end;

function TVkParamsPollsDeleteVote.OwnerId(const Value: TVkPeerId): TVkParamsPollsDeleteVote;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPollsDeleteVote.PollId(const Value: Integer): TVkParamsPollsDeleteVote;
begin
  List.Add('poll_id', Value);
  Result := Self;
end;

{ TVkParamsPollsEdit }

function TVkParamsPollsEdit.OwnerId(const Value: TVkPeerId): TVkParamsPollsEdit;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPollsEdit.PollId(const Value: Integer): TVkParamsPollsEdit;
begin
  List.Add('poll_id', Value);
  Result := Self;
end;

function TVkParamsPollsEdit.Question(const Value: string): TVkParamsPollsEdit;
begin
  List.Add('question', Value);
  Result := Self;
end;

function TVkParamsPollsEdit.AddAnswers(const Value: TArrayOfString): TVkParamsPollsEdit;
begin
  List.Add('add_answers', Value.ToJson);
  Result := Self;
end;

function TVkParamsPollsEdit.EditAnswers(const Value: TVkVoteAnswers): TVkParamsPollsEdit;
begin
  List.Add('edit_answers', Value.ToJson);
  Result := Self;
end;

function TVkParamsPollsEdit.BackgroundId(const Value: TVkPollBackground): TVkParamsPollsEdit;
begin
  List.Add('background_id', Value.Id);
  Result := Self;
end;

function TVkParamsPollsEdit.DeleteAnswers(const Value: TIdList): TVkParamsPollsEdit;
begin
  List.Add('delete_answers', Value.ToJson);
  Result := Self;
end;

function TVkParamsPollsEdit.EndDate(const Value: TDateTime): TVkParamsPollsEdit;
begin
  List.Add('end_date', Value);
  Result := Self;
end;

function TVkParamsPollsEdit.PhotoId(const Value: Cardinal): TVkParamsPollsEdit;
begin
  List.Add('photo_id', Value);
  Result := Self;
end;

function TVkParamsPollsEdit.BackgroundId(const Value: Integer): TVkParamsPollsEdit;
begin
  List.Add('background_id', Value);
  Result := Self;
end;

{ TVkParamsPollsGetById }

function TVkParamsPollsGetById.OwnerId(const Value: TVkPeerId): TVkParamsPollsGetById;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPollsGetById.IsBoard(const Value: Boolean): TVkParamsPollsGetById;
begin
  List.Add('is_board', Value);
  Result := Self;
end;

function TVkParamsPollsGetById.PollId(const Value: Integer): TVkParamsPollsGetById;
begin
  List.Add('poll_id', Value);
  Result := Self;
end;

function TVkParamsPollsGetById.Extended(const Value: Boolean): TVkParamsPollsGetById;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPollsGetById.FriendsCount(const Value: Integer): TVkParamsPollsGetById;
begin
  List.Add('friends_count', Value);
  Result := Self;
end;

function TVkParamsPollsGetById.Fields(const Value: TVkExtendedFields): TVkParamsPollsGetById;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsPollsGetById.NameCase(const Value: TVkNameCase): TVkParamsPollsGetById;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

{ TVkParamsPollsGetVoters }

function TVkParamsPollsGetVoters.OwnerId(const Value: TVkPeerId): TVkParamsPollsGetVoters;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPollsGetVoters.PollId(const Value: Integer): TVkParamsPollsGetVoters;
begin
  List.Add('poll_id', Value);
  Result := Self;
end;

function TVkParamsPollsGetVoters.AnswerIds(const Value: TIdList): TVkParamsPollsGetVoters;
begin
  List.Add('answer_ids', Value);
  Result := Self;
end;

function TVkParamsPollsGetVoters.IsBoard(const Value: Boolean): TVkParamsPollsGetVoters;
begin
  List.Add('is_board', Value);
  Result := Self;
end;

function TVkParamsPollsGetVoters.FriendsOnly(const Value: Boolean): TVkParamsPollsGetVoters;
begin
  List.Add('friends_only', Value);
  Result := Self;
end;

function TVkParamsPollsGetVoters.Offset(const Value: Integer): TVkParamsPollsGetVoters;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPollsGetVoters.Count(const Value: Integer): TVkParamsPollsGetVoters;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPollsGetVoters.Fields(const Value: TVkExtendedFields): TVkParamsPollsGetVoters;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsPollsGetVoters.NameCase(const Value: TVkNameCase): TVkParamsPollsGetVoters;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

end.

