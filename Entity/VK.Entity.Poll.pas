unit VK.Entity.Poll;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Photo, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common,
  VK.Entity.Common.List, VK.Types;

type
  TVkPollFriends = class(TVkObject)
  end;

  TVkPollPoints = class
  private
    FColor: string;
    FPosition: Integer;
  public
    property Color: string read FColor write FColor;
    property Position: Integer read FPosition write FPosition;
  end;

  TVkPollBackground = class(TVkBasicObject)
  private
    FAngle: Integer;
    FColor: string;
    FPoints: TArray<TVkPollPoints>;
    FType: string;
    FWidth: Integer;
    FHeight: Integer;
    FImages: TArray<TVkSize>;
  public
    /// <summary>
    /// Идентификатор фона
    /// </summary>
    property Id;
    property Name;
    /// <summary>
    /// (для type = gradient) угол градиента по оси X
    /// </summary>
    property Angle: Integer read FAngle write FAngle;
    /// <summary>
    /// HEX-код замещающего цвета (без #)
    /// </summary>
    property Color: string read FColor write FColor;
    /// <summary>
    /// (для type = gradient) точки градиента. Массив объектов, каждый из которых содержит поля position (number) — положение точки — и color (string) — HEX-код цвета точки.
    /// </summary>
    property Points: TArray<TVkPollPoints> read FPoints write FPoints;
    /// <summary>
    /// Тип фона. Возможные значения: gradient, tile
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// (для type = tile) ширина плитки паттерна
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// (для type = tile) высота плитки паттерна
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// (для type = tile) изображение плитки паттерна. Массив объектов изображений.
    /// </summary>
    property Images: TArray<TVkSize> read FImages write FImages;
    destructor Destroy; override;
  end;

  TVkPollBackgrounds = TVkEntityList<TVkPollBackground>;

  TVkPollAnswer = class(TVkObject)
  private
    FRate: Integer;
    FText: string;
    FVotes: Integer;
  public
    /// <summary>
    /// Идентификатор ответа
    /// </summary>
    property Id;
    /// <summary>
    /// Рейтинг ответа
    /// </summary>
    property Rate: Integer read FRate write FRate;
    /// <summary>
    /// Текст ответа
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Число проголосовавших за этот ответ
    /// </summary>
    property Votes: Integer read FVotes write FVotes;
  end;

  TVkPoll = class(TVkObject, IAttachment)
  private
    FAnonymous: Boolean;
    FAnswer_ids: TArray<Integer>;
    FAnswers: TArray<TVkPollAnswer>;
    FAuthor_id: Integer;
    FBackground: TVkPollBackground;
    FCan_edit: Boolean;
    FCan_report: Boolean;
    FCan_share: Boolean;
    FCan_vote: Boolean;
    FClosed: Boolean;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FCreated: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FEnd_date: TDateTime;
    FIs_board: Boolean;
    FMultiple: Boolean;
    FOwner_id: Integer;
    FQuestion: string;
    FVotes: Integer;
    FPhoto: TVkPhoto;
    FFriends: TArray<TVkPollFriends>;
    FDisable_unvote: Boolean;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
    FAccess_key: string;
  public
    /// <summary>
    /// Идентификатор опроса для получения информации о нем через метод polls.getById.
    /// </summary>
    property Id;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Идентификатор владельца опроса
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    /// <summary>
    /// Дата создания
    /// </summary>
    property Created: TDateTime read FCreated write FCreated;
    /// <summary>
    /// Текст вопроса
    /// </summary>
    property Question: string read FQuestion write FQuestion;
    /// <summary>
    /// Количество голосов
    /// </summary>
    property Votes: Integer read FVotes write FVotes;
    /// <summary>
    /// Массив объектов, которые содержат информацию о вариантах ответа
    /// </summary>
    property Answers: TArray<TVkPollAnswer> read FAnswers write FAnswers;
    /// <summary>
    /// Является ли опрос анонимным.
    /// </summary>
    property Anonymous: Boolean read FAnonymous write FAnonymous;
    /// <summary>
    /// Допускает ли опрос выбор нескольких вариантов ответа
    /// </summary>
    property Multiple: Boolean read FMultiple write FMultiple;
    /// <summary>
    /// Идентификаторы вариантов ответа, выбранных текущим пользователем.
    /// </summary>
    property AnswerIds: TArray<Integer> read FAnswer_ids write FAnswer_ids;
    /// <summary>
    /// Дата завершения опроса. 0, если опрос бессрочный.
    /// </summary>
    property EndDate: TDateTime read FEnd_date write FEnd_date;
    /// <summary>
    /// Является ли опрос завершенным
    /// </summary>
    property Closed: Boolean read FClosed write FClosed;
    /// <summary>
    /// Прикреплён ли опрос к обсуждению
    /// </summary>
    property IsBoard: Boolean read FIs_board write FIs_board;
    /// <summary>
    /// Можно ли отредактировать опрос
    /// </summary>
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    /// <summary>
    /// Можно ли проголосовать в опросе
    /// </summary>
    property CanVote: Boolean read FCan_vote write FCan_vote;
    /// <summary>
    /// Можно ли пожаловаться на опрос
    /// </summary>
    property CanReport: Boolean read FCan_report write FCan_report;
    /// <summary>
    /// Можно ли поделиться опросом
    /// </summary>
    property CanShare: Boolean read FCan_share write FCan_share;
    /// <summary>
    /// Идентификатор автора опроса
    /// </summary>
    property AuthorId: Integer read FAuthor_id write FAuthor_id;
    /// <summary>
    /// Фотография — фон сниппета опроса
    /// </summary>
    property Photo: TVkPhoto read FPhoto write FPhoto;
    /// <summary>
    /// Фон сниппета опроса
    /// </summary>
    property Background: TVkPollBackground read FBackground write FBackground;
    /// <summary>
    /// Идентификаторы 3 друзей, которые проголосовали в опросе
    /// </summary>
    property Friends: TArray<TVkPollFriends> read FFriends write FFriends;
    property DisableUnvote: Boolean read FDisable_unvote write FDisable_unvote;
    //Extended
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    //
    destructor Destroy; override;
    function ToAttachment: TAttachment;
  end;

  TVkPollVoters = class(TVkEntity)
  private
    FAnswer_id: Integer;
    FUsers: TVkProfiles;
  public
    property AnswerId: Integer read FAnswer_id write FAnswer_id;
    property Users: TVkProfiles read FUsers write FUsers;
  end;

implementation

uses
  VK.CommonUtils;

{TVkPollBackground}

destructor TVkPollBackground.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPollPoints>(FPoints);
  TArrayHelp.FreeArrayOfObject<TVkSize>(FImages);
  inherited;
end;

{TVkPoll}

destructor TVkPoll.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPollAnswer>(FAnswers);
  TArrayHelp.FreeArrayOfObject<TVkPollFriends>(FFriends);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FBackground) then
    FBackground.Free;
  inherited;
end;

function TVkPoll.ToAttachment: TAttachment;
begin
  Result := TAttachment.Poll(OwnerId, Id, AccessKey);
end;

end.

