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
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// ������ ��������������� ������ (��� ������ � �������������)
    /// </summary>
    function AnswerIds(const Value: TIdList): Integer;
    /// <summary>
    /// True � ����� ��������� � ����������, False � ����� ���������� � �����
    /// </summary>
    function IsBoard(const Value: Boolean): Integer;
  end;

  TVkParamsPollsCreate = record
    List: TParams;
    /// <summary>
    /// ����� �������
    /// </summary>
    function Question(const Value: string): Integer;
    /// <summary>
    /// True � ��������� �����, ������ ��������������� ����������;
    /// False � ����� ���������, ������ ��������������� ��������
    /// </summary>
    function IsAnonymous(const Value: Boolean): Integer;
    /// <summary>
    /// True � ��� �������� ������ � �������������
    /// </summary>
    function IsMultiple(const Value: Boolean): Integer;
    /// <summary>
    /// ���� ���������� ������
    /// </summary>
    function EndDate(const Value: TDateTime): Integer;
    /// <summary>
    /// ���� ����� ����� �������� � ������, ���������� �������� ������������� ������������� ������. �� ��������� ������� ������������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������ ��������� �������
    /// ����� ���� �� ����� 1 � �� ����� 10 ��������� ������
    /// </summary>
    function AddAnswers(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ������������� ���������� ��� ������������� � �������� ���� ��������
    /// </summary>
    function PhotoId(const Value: Cardinal): Integer;
    /// <summary>
    /// ������������� ������������ ���� ��� ��������
    /// </summary>
    function BackgroundId(const Value: Integer): Integer; overload;
    /// <summary>
    /// ������������� ������������ ���� ��� ��������
    /// </summary>
    function BackgroundId(const Value: TVkPollBackground): Integer; overload;
    /// <summary>
    /// ��������� ������ �����
    /// </summary>
    function DisableUnvote(const Value: Boolean): Integer;
  end;

  TVkParamsPollsDeleteVote = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������� ������
    /// </summary>
    function AnswerId(const Value: Integer): Integer;
    /// <summary>
    /// True � ����� ��������� � ����������, False � ����� ���������� � �����
    /// </summary>
    function IsBoard(const Value: Boolean): Integer;
  end;

  TVkParamsPollsEdit = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ����� �������������� ������
    /// </summary>
    function Question(const Value: string): Integer;
    /// <summary>
    /// ������ ��������� �������
    /// ����� ���� �� ����� 1 � �� ����� 10 ��������� ������
    /// </summary>
    function AddAnswers(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ������, ���������� �������� �������, ������� ���������� ���������������;
    /// ���� � ������������� ������, �������� � ����� ����� ������
    /// </summary>
    function EditAnswers(const Value: TVkVoteAnswers): Integer;
    /// <summary>
    /// ������ ��������������� �������, ������� ���������� �������
    /// </summary>
    function DeleteAnswers(const Value: TIdList): Integer;
    /// <summary>
    /// ���� ���������� ������
    /// </summary>
    function EndDate(const Value: TDateTime): Integer;
    /// <summary>
    /// ������������� ���������� ��� ��������
    /// </summary>
    function PhotoId(const Value: Cardinal): Integer;
    /// <summary>
    /// ������������� ������������ ���� ��� ��������
    /// </summary>
    function BackgroundId(const Value: Integer): Integer; overload;
    /// <summary>
    /// ������������� ������������ ���� ��� ��������
    /// </summary>
    function BackgroundId(const Value: TVkPollBackground): Integer; overload;
  end;

  TVkParamsPollsGetById = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// True � ����� ��������� � ����������, False � ����� ���������� � �����
    /// </summary>
    function IsBoard(const Value: Boolean = False): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// True � ���������� �������������� ���������� � �������� �������������
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// ����� ��������������� ��������������� ������, ������� ���������� ������� � ������
    /// </summary>
    function FriendsCount(const Value: Integer = 3): Integer;
    /// <summary>
    /// ������ �������������� ����� ��������
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer;
  end;

  TVkParamsPollsGetVoters = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// ������ ��������������� ������ (��� ������ � �������������)
    /// </summary>
    function AnswerIds(const Value: TIdList): Integer;
    /// <summary>
    /// True � ����� ��������� � ����������, False � ����� ���������� � �����
    /// </summary>
    function IsBoard(const Value: Boolean): Integer;
    /// <summary>
    /// ���������, ���������� �� ���������� ������ �������������, ������� �������� �������� �������� ������������. �������� ����� ��������� ��������� ��������:
    /// False � ���������� ���� ������������� � ������� �������� ������� �����������;
    /// True � ���������� ������ ������ �������� ������������ � ������� �������� ������� �����������.
    /// ���� �������� �� ��� �����, �� ���������, ��� �� ����� False
    /// </summary>
    function FriendsOnly(const Value: Boolean = False): Integer;
    /// <summary>
    /// �������� ������������ ������ ������, ��� ������� ������������� ������������. ���� �������� �� �����, �� ���������, ��� �� ����� 0
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// ���������� ������������ ��������������� �������������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� 100, ���� �� ����� �������� FriendsOnly, � ��������� ������ 10.
    /// ������������ �������� ��������� 1000, ���� �� ����� �������� FriendsOnly, � ��������� ������ 100
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����� ������� ���� �����, ����������� ��� ���������
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer;
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
    function GetPhotoUploadServer(var UploadUrl: string; OwnerId: Integer = 0): Boolean;
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

function TPollsController.GetPhotoUploadServer(var UploadUrl: string; OwnerId: Integer): Boolean;
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

function TVkParamsPollsAddVote.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsAddVote.PollId(const Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

function TVkParamsPollsAddVote.AnswerIds(const Value: TIdList): Integer;
begin
  Result := List.Add('answer_ids', Value);
end;

function TVkParamsPollsAddVote.IsBoard(const Value: Boolean): Integer;
begin
  Result := List.Add('is_board', Value);
end;

{ TVkParamsPollsCreate }

function TVkParamsPollsCreate.Question(const Value: string): Integer;
begin
  Result := List.Add('question', Value);
end;

function TVkParamsPollsCreate.IsAnonymous(const Value: Boolean): Integer;
begin
  Result := List.Add('is_anonymous', Value);
end;

function TVkParamsPollsCreate.IsMultiple(const Value: Boolean): Integer;
begin
  Result := List.Add('is_multiple', Value);
end;

function TVkParamsPollsCreate.EndDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_date', Value);
end;

function TVkParamsPollsCreate.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsCreate.AddAnswers(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('add_answers', Value.ToJson);
end;

function TVkParamsPollsCreate.PhotoId(const Value: Cardinal): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPollsCreate.BackgroundId(const Value: Integer): Integer;
begin
  Result := List.Add('background_id', Value);
end;

function TVkParamsPollsCreate.BackgroundId(const Value: TVkPollBackground): Integer;
begin
  Result := List.Add('background_id', Value.Id);
end;

function TVkParamsPollsCreate.DisableUnvote(const Value: Boolean): Integer;
begin
  Result := List.Add('disable_unvote', Value);
end;

{ TVkParamsPollsDeleteVote }

function TVkParamsPollsDeleteVote.AnswerId(const Value: Integer): Integer;
begin
  Result := List.Add('answer_id', Value);
end;

function TVkParamsPollsDeleteVote.IsBoard(const Value: Boolean): Integer;
begin
  Result := List.Add('is_board', Value);
end;

function TVkParamsPollsDeleteVote.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsDeleteVote.PollId(const Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

{ TVkParamsPollsEdit }

function TVkParamsPollsEdit.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsEdit.PollId(const Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

function TVkParamsPollsEdit.Question(const Value: string): Integer;
begin
  Result := List.Add('question', Value);
end;

function TVkParamsPollsEdit.AddAnswers(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('add_answers', Value.ToJson);
end;

function TVkParamsPollsEdit.EditAnswers(const Value: TVkVoteAnswers): Integer;
begin
  Result := List.Add('edit_answers', Value.ToJson);
end;

function TVkParamsPollsEdit.BackgroundId(const Value: TVkPollBackground): Integer;
begin
  Result := List.Add('background_id', Value.Id);
end;

function TVkParamsPollsEdit.DeleteAnswers(const Value: TIdList): Integer;
begin
  Result := List.Add('delete_answers', Value.ToJson);
end;

function TVkParamsPollsEdit.EndDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_date', Value);
end;

function TVkParamsPollsEdit.PhotoId(const Value: Cardinal): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPollsEdit.BackgroundId(const Value: Integer): Integer;
begin
  Result := List.Add('background_id', Value);
end;

{ TVkParamsPollsGetById }

function TVkParamsPollsGetById.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsGetById.IsBoard(const Value: Boolean): Integer;
begin
  Result := List.Add('is_board', Value);
end;

function TVkParamsPollsGetById.PollId(const Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

function TVkParamsPollsGetById.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPollsGetById.FriendsCount(const Value: Integer): Integer;
begin
  Result := List.Add('friends_count', Value);
end;

function TVkParamsPollsGetById.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsPollsGetById.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

{ TVkParamsPollsGetVoters }

function TVkParamsPollsGetVoters.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsGetVoters.PollId(const Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

function TVkParamsPollsGetVoters.AnswerIds(const Value: TIdList): Integer;
begin
  Result := List.Add('answer_ids', Value);
end;

function TVkParamsPollsGetVoters.IsBoard(const Value: Boolean): Integer;
begin
  Result := List.Add('is_board', Value);
end;

function TVkParamsPollsGetVoters.FriendsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkParamsPollsGetVoters.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPollsGetVoters.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPollsGetVoters.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsPollsGetVoters.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

end.

