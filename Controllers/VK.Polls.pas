unit VK.Polls;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Poll;

type
  TVkParamsPollsAddVote = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит опрос
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор опроса
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// Список идентификаторов ответа (для опроса с мультивыбором)
    /// </summary>
    function AnswerIds(const Value: TIdList): Integer;
    /// <summary>
    /// True – опрос находится в обсуждении, False – опрос прикреплен к стене
    /// </summary>
    function IsBoard(const Value: Boolean): Integer;
  end;

  TVkParamsPollsCreate = record
    List: TParams;
    /// <summary>
    /// Текст вопроса
    /// </summary>
    function Question(const Value: string): Integer;
    /// <summary>
    /// True – анонимный опрос, список проголосовавших недоступен;
    /// False – опрос публичный, список проголосовавших доступен
    /// </summary>
    function IsAnonymous(const Value: Boolean): Integer;
    /// <summary>
    /// True — для создания опроса с мультивыбором
    /// </summary>
    function IsMultiple(const Value: Boolean): Integer;
    /// <summary>
    /// Дата завершения опроса
    /// </summary>
    function EndDate(const Value: TDateTime): Integer;
    /// <summary>
    /// Если опрос будет добавлен в группу, необходимо передать отрицательный идентификатор группы. По умолчанию текущий пользователь
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Список вариантов ответов
    /// Может быть не менее 1 и не более 10 вариантов ответа
    /// </summary>
    function AddAnswers(const Value: TArrayOfString): Integer;
    /// <summary>
    /// Идентификатор фотографии для использования в качестве фона сниппета
    /// </summary>
    function PhotoId(const Value: Cardinal): Integer;
    /// <summary>
    /// Идентификатор стандартного фона для сниппета
    /// </summary>
    function BackgroundId(const Value: Integer): Integer; overload;
    /// <summary>
    /// Идентификатор стандартного фона для сниппета
    /// </summary>
    function BackgroundId(const Value: TVkPollBackground): Integer; overload;
    /// <summary>
    /// Запретить менять выбор
    /// </summary>
    function DisableUnvote(const Value: Boolean): Integer;
  end;

  TVkParamsPollsDeleteVote = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит опрос
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор опроса
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор варианта ответа
    /// </summary>
    function AnswerId(const Value: Integer): Integer;
    /// <summary>
    /// True – опрос находится в обсуждении, False – опрос прикреплен к стене
    /// </summary>
    function IsBoard(const Value: Boolean): Integer;
  end;

  TVkParamsPollsEdit = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца опроса
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор опроса
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// Новый текст редактируемого опроса
    /// </summary>
    function Question(const Value: string): Integer;
    /// <summary>
    /// Список вариантов ответов
    /// Может быть не менее 1 и не более 10 вариантов ответа
    /// </summary>
    function AddAnswers(const Value: TArrayOfString): Integer;
    /// <summary>
    /// Объект, содержащий варианты ответов, которые необходимо отредактировать;
    /// ключ – идентификатор ответа, значение – новый текст ответа
    /// </summary>
    function EditAnswers(const Value: TVkVoteAnswers): Integer;
    /// <summary>
    /// Список идентификаторов ответов, которые необходимо удалить
    /// </summary>
    function DeleteAnswers(const Value: TIdList): Integer;
    /// <summary>
    /// Дата завершения опроса
    /// </summary>
    function EndDate(const Value: TDateTime): Integer;
    /// <summary>
    /// Идентификатор фотографии для сниппета
    /// </summary>
    function PhotoId(const Value: Cardinal): Integer;
    /// <summary>
    /// Идентификатор стандартного фона для сниппета
    /// </summary>
    function BackgroundId(const Value: Integer): Integer; overload;
    /// <summary>
    /// Идентификатор стандартного фона для сниппета
    /// </summary>
    function BackgroundId(const Value: TVkPollBackground): Integer; overload;
  end;

  TVkParamsPollsGetById = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит опрос
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// True — опрос находится в обсуждении, False — опрос прикреплен к стене
    /// </summary>
    function IsBoard(const Value: Boolean = False): Integer;
    /// <summary>
    /// Идентификатор опроса
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// True — возвращать дополнительную информацию о профилях пользователей
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// Число идентификаторов проголосовавших друзей, которые необходимо вернуть в ответе
    /// </summary>
    function FriendsCount(const Value: Integer = 3): Integer;
    /// <summary>
    /// Список дополнительных полей профилей
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer;
  end;

  TVkParamsPollsGetVoters = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит опрос
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор опроса
    /// </summary>
    function PollId(const Value: Integer): Integer;
    /// <summary>
    /// Список идентификаторов ответа (для опроса с мультивыбором)
    /// </summary>
    function AnswerIds(const Value: TIdList): Integer;
    /// <summary>
    /// True — опрос находится в обсуждении, False — опрос прикреплен к стене
    /// </summary>
    function IsBoard(const Value: Boolean): Integer;
    /// <summary>
    /// Указывает, необходимо ли возвращать только пользователей, которые являются друзьями текущего пользователя. Параметр может принимать следующие значения:
    /// False — возвращать всех пользователей в порядке убывания времени голосования;
    /// True — возвращать только друзей текущего пользователя в порядке убывания времени голосования.
    /// Если параметр не был задан, то считается, что он равен False
    /// </summary>
    function FriendsOnly(const Value: Boolean = False): Integer;
    /// <summary>
    /// Смещение относительно начала списка, для выборки определенного подмножества. Если параметр не задан, то считается, что он равен 0
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// Количество возвращаемых идентификаторов пользователей.
    /// Если параметр не задан, то считается, что он равен 100, если не задан параметр FriendsOnly, в противном случае 10.
    /// Максимальное значение параметра 1000, если не задан параметр FriendsOnly, в противном случае 100
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// Перечисленные через запятую поля анкет, необходимые для получения
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): Integer;
  end;

  /// <summary>
  /// Методы для работы с опросами.
  /// </summary>
  TPollsController = class(TVkController)
  public
    /// <summary>
    /// Отдает голос текущего пользователя за выбранный вариант ответа в указанном опросе.
    /// </summary>
    function AddVote(const Params: TVkParamsPollsAddVote): Boolean;
    /// <summary>
    /// Позволяет создавать опросы, которые впоследствии можно прикреплять к записям на странице пользователя или сообщества.
    /// </summary>
    function &Create(var Item: TVkPoll; const Params: TVkParamsPollsCreate): Boolean;
    /// <summary>
    /// Снимает голос текущего пользователя с выбранного варианта ответа в указанном опросе.
    /// </summary>
    function DeleteVote(const Params: TVkParamsPollsDeleteVote): Boolean;
    /// <summary>
    /// Позволяет редактировать созданные опросы.
    /// </summary>
    function Edit(const Params: TVkParamsPollsEdit): Boolean;
    /// <summary>
    /// Возвращает варианты фонового изображения для опросов.
    /// </summary>
    function GetBackgrounds(var Items: TVkPollBackgrounds): Boolean;
    /// <summary>
    /// Возвращает детальную информацию об опросе по его идентификатору.
    /// </summary>
    function GetById(var Item: TVkPoll; Params: TVkParamsPollsGetById): Boolean; overload;
    /// <summary>
    /// Возвращает детальную информацию об опросе по его идентификатору.
    /// </summary>
    function GetById(var Item: TVkPoll; PollId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фоновой фотографии в опрос.
    /// </summary>
    function GetPhotoUploadServer(var UploadUrl: string; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Получает список идентификаторов пользователей, которые выбрали определенные варианты ответа в опросе.
    /// </summary>
    function GetVoters(var Items: TVkPollVoters; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает список идентификаторов пользователей, которые выбрали определенные варианты ответа в опросе.
    /// </summary>
    function GetVoters(var Items: TVkPollVoters; Params: TVkParamsPollsGetVoters): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографию, загруженную в опрос.
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

