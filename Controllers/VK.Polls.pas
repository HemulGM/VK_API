unit VK.Polls;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Poll;

type
  TVkVoteAnswer = record
    Id: Integer;
    Text: string;
    function ToString: string;
    class function Create(Id: Integer; Text: string): TVkVoteAnswer; static;
  end;

  TVkVoteAnswers = TArray<TVkVoteAnswer>;

  TVkVoteAnswersHelper = record helper for TVkVoteAnswers
    function ToJson: string;
    function Add(Value: TVkVoteAnswer): Integer; overload;
    function Add(Id: Integer; Text: string): Integer; overload;
  end;

  TVkParamsPollsAddVote = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PollId(Value: Integer): Integer;
    function AnswerIds(Value: TIds): Integer;
    function IsBoard(Value: Boolean): Integer;
  end;

  TVkParamsPollsCreate = record
    List: TParams;
    function Question(Value: string): Integer;
    function IsAnonymous(Value: Boolean): Integer;
    function IsMultiple(Value: Boolean): Integer;
    function EndDate(Value: TDateTime): Integer;
    function OwnerId(Value: Integer): Integer;
    function AddAnswers(Value: TArrayOfString): Integer;
    function PhotoId(Value: Integer): Integer;
    function BackgroundId(Value: Integer): Integer;
    function DisableUnvote(Value: Boolean): Integer;
  end;

  TVkParamsPollsDeleteVote = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PollId(Value: Integer): Integer;
    function AnswerId(Value: Integer): Integer;
    function IsBoard(Value: Boolean): Integer;
  end;

  TVkParamsPollsEdit = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PollId(Value: Integer): Integer;
    function Question(Value: string): Integer;
    function AddAnswers(Value: TArrayOfString): Integer;
    function EditAnswers(Value: TVkVoteAnswers): Integer;
    function DeleteAnswers(Value: TIds): Integer;
    function EndDate(Value: TDateTime): Integer;
    function PhotoId(Value: Integer): Integer;
    function BackgroundId(Value: Integer): Integer;
  end;

  TVkParamsPollsGetById = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function IsBoard(Value: Boolean): Integer;
    function PollId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function FriendsCount(Value: Integer = 3): Integer;
    function Fields(Value: TVkUserFields): Integer;
    function NameCase(Value: TVkNameCase): Integer;
  end;

  TVkParamsPollsGetVoters = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function PollId(Value: Integer): Integer;
    function AnswerIds(Value: TIds): Integer;
    function IsBoard(Value: Boolean): Integer;
    function FriendsOnly(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Fields(Value: TVkUserFields): Integer;
    function NameCase(Value: TVkNameCase): Integer;
  end;

  /// <summary>
  /// ћетоды дл€ работы с опросами.
  /// </summary>
  TPollsController = class(TVkController)
  public
    /// <summary>
    /// ќтдает голос текущего пользовател€ за выбранный вариант ответа в указанном опросе.
    /// </summary>
    function AddVote(const Params: TVkParamsPollsAddVote): Boolean;
    /// <summary>
    /// ѕозвол€ет создавать опросы, которые впоследствии можно прикрепл€ть к запис€м на странице пользовател€ или сообщества.
    /// </summary>
    function &Create(var Item: TVkPoll; const Params: TVkParamsPollsCreate): Boolean;
    /// <summary>
    /// —нимает голос текущего пользовател€ с выбранного варианта ответа в указанном опросе.
    /// </summary>
    function DeleteVote(const Params: TVkParamsPollsDeleteVote): Boolean;
    /// <summary>
    /// ѕозвол€ет редактировать созданные опросы.
    /// </summary>
    function Edit(const Params: TVkParamsPollsEdit): Boolean;
    /// <summary>
    /// ¬озвращает варианты фонового изображени€ дл€ опросов.
    /// </summary>
    function GetBackgrounds(var Items: TVkPollBackgrounds): Boolean;
    /// <summary>
    /// ¬озвращает детальную информацию об опросе по его идентификатору.
    /// </summary>
    function GetById(var Item: TVkPoll; Params: TVkParamsPollsGetById): Boolean; overload;
    /// <summary>
    /// ¬озвращает детальную информацию об опросе по его идентификатору.
    /// </summary>
    function GetById(var Item: TVkPoll; PollId: Integer): Boolean; overload;
    /// <summary>
    /// ¬озвращает адрес сервера дл€ загрузки фоновой фотографии в опрос.
    /// </summary>
    function GetPhotoUploadServer(var UploadUrl: string; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ѕолучает список идентификаторов пользователей, которые выбрали определенные варианты ответа в опросе.
    /// </summary>
    function GetVoters(var Items: TVkPollVoters; Params: TParams): Boolean; overload;
    /// <summary>
    /// ѕолучает список идентификаторов пользователей, которые выбрали определенные варианты ответа в опросе.
    /// </summary>
    function GetVoters(var Items: TVkPollVoters; Params: TVkParamsPollsGetVoters): Boolean; overload;
    /// <summary>
    /// —охран€ет фотографию, загруженную в опрос.
    /// </summary>
    function SavePhoto(var Id: Integer; const Photo, Hash: string): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TPollsController }

function TPollsController.AddVote(const Params: TVkParamsPollsAddVote): Boolean;
begin
  with Handler.Execute('polls.addVote', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TPollsController.&Create(var Item: TVkPoll; const Params: TVkParamsPollsCreate): Boolean;
begin
  with Handler.Execute('polls.create', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPoll.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPollsController.DeleteVote(const Params: TVkParamsPollsDeleteVote): Boolean;
begin
  with Handler.Execute('polls.deleteVote', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TPollsController.Edit(const Params: TVkParamsPollsEdit): Boolean;
begin
  with Handler.Execute('polls.edit', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TPollsController.GetBackgrounds(var Items: TVkPollBackgrounds): Boolean;
begin
  with Handler.Execute('polls.getBackgrounds') do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPollBackgrounds.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
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
  with Handler.Execute('polls.getPhotoUploadServer', Params) do
    Result := Success and GetValue('upload_url', UploadUrl);
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
  with Handler.Execute('polls.savePhoto', Params) do
    Result := Success and ResponseAsInt(Id);
end;

function TPollsController.GetVoters(var Items: TVkPollVoters; Params: TParams): Boolean;
begin
  if not Params.KeyExists('fields') then
    Params.Add('fields', 'domain');
  with Handler.Execute('polls.getVoters', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPollVoters.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TPollsController.GetById(var Item: TVkPoll; Params: TVkParamsPollsGetById): Boolean;
begin
  with Handler.Execute('polls.getById', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPoll.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkParamsPollsAddVote }

function TVkParamsPollsAddVote.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsAddVote.PollId(Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

function TVkParamsPollsAddVote.AnswerIds(Value: TIds): Integer;
begin
  Result := List.Add('answer_ids', Value);
end;

function TVkParamsPollsAddVote.IsBoard(Value: Boolean): Integer;
begin
  Result := List.Add('is_board', Value);
end;

{ TVkParamsPollsCreate }

function TVkParamsPollsCreate.Question(Value: string): Integer;
begin
  Result := List.Add('question', Value);
end;

function TVkParamsPollsCreate.IsAnonymous(Value: Boolean): Integer;
begin
  Result := List.Add('is_anonymous', Value);
end;

function TVkParamsPollsCreate.IsMultiple(Value: Boolean): Integer;
begin
  Result := List.Add('is_multiple', Value);
end;

function TVkParamsPollsCreate.EndDate(Value: TDateTime): Integer;
begin
  Result := List.Add('end_date', Value);
end;

function TVkParamsPollsCreate.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsCreate.AddAnswers(Value: TArrayOfString): Integer;
begin
  Result := List.Add('add_answers', Value.ToJson);
end;

function TVkParamsPollsCreate.PhotoId(Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPollsCreate.BackgroundId(Value: Integer): Integer;
begin
  Result := List.Add('background_id', Value);
end;

function TVkParamsPollsCreate.DisableUnvote(Value: Boolean): Integer;
begin
  Result := List.Add('disable_unvote', Value);
end;

{ TVkParamsPollsDeleteVote }

function TVkParamsPollsDeleteVote.AnswerId(Value: Integer): Integer;
begin
  Result := List.Add('answer_id', Value);
end;

function TVkParamsPollsDeleteVote.IsBoard(Value: Boolean): Integer;
begin
  Result := List.Add('is_board', Value);
end;

function TVkParamsPollsDeleteVote.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsDeleteVote.PollId(Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

{ TVkParamsPollsEdit }

function TVkParamsPollsEdit.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsEdit.PollId(Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

function TVkParamsPollsEdit.Question(Value: string): Integer;
begin
  Result := List.Add('question', Value);
end;

function TVkParamsPollsEdit.AddAnswers(Value: TArrayOfString): Integer;
begin
  Result := List.Add('add_answers', Value.ToJson);
end;

function TVkParamsPollsEdit.EditAnswers(Value: TVkVoteAnswers): Integer;
begin
  Result := List.Add('edit_answers', Value.ToJson);
end;

function TVkParamsPollsEdit.DeleteAnswers(Value: TIds): Integer;
begin
  Result := List.Add('delete_answers', Value.ToJson);
end;

function TVkParamsPollsEdit.EndDate(Value: TDateTime): Integer;
begin
  Result := List.Add('end_date', Value);
end;

function TVkParamsPollsEdit.PhotoId(Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPollsEdit.BackgroundId(Value: Integer): Integer;
begin
  Result := List.Add('background_id', Value);
end;

{ TVkVoteAnswersHelper }

function TVkVoteAnswersHelper.Add(Value: TVkVoteAnswer): Integer;
begin
  Result := System.Length(Self) + 1;
  SetLength(Self, Result);
  Self[Result - 1] := Value;
end;

function TVkVoteAnswersHelper.Add(Id: Integer; Text: string): Integer;
begin
  Result := Add(TVkVoteAnswer.Create(Id, Text));
end;

function TVkVoteAnswersHelper.ToJson: string;
var
  i: Integer;
begin
  Result := '{';
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + Self[i].ToString;
  end;
  Result := Result + '}';
end;

{ TVkVoteAnswer }

class function TVkVoteAnswer.Create(Id: Integer; Text: string): TVkVoteAnswer;
begin
  Result.Id := Id;
  Result.Text := Text;
end;

function TVkVoteAnswer.ToString: string;
begin
  Result := '"' + Id.ToString + '":"' + Text + '"';
end;

{ TVkParamsPollsGetById }

function TVkParamsPollsGetById.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsGetById.IsBoard(Value: Boolean): Integer;
begin
  Result := List.Add('is_board', Value);
end;

function TVkParamsPollsGetById.PollId(Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

function TVkParamsPollsGetById.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPollsGetById.FriendsCount(Value: Integer): Integer;
begin
  Result := List.Add('friends_count', Value);
end;

function TVkParamsPollsGetById.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsPollsGetById.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

{ TVkParamsPollsGetVoters }

function TVkParamsPollsGetVoters.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPollsGetVoters.PollId(Value: Integer): Integer;
begin
  Result := List.Add('poll_id', Value);
end;

function TVkParamsPollsGetVoters.AnswerIds(Value: TIds): Integer;
begin
  Result := List.Add('answer_ids', Value);
end;

function TVkParamsPollsGetVoters.IsBoard(Value: Boolean): Integer;
begin
  Result := List.Add('is_board', Value);
end;

function TVkParamsPollsGetVoters.FriendsOnly(Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkParamsPollsGetVoters.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPollsGetVoters.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPollsGetVoters.Fields(Value: TVkUserFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsPollsGetVoters.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

end.

