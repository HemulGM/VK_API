unit VK.Groups;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, REST.Json,
  System.Json, VK.Controller, VK.Types, VK.Entity.Profile, System.Classes,
  VK.Entity.Group, VK.CommonUtils, VK.Entity.Common, VK.Entity.Group.TimeTable,
  VK.Entity.Group.Ban, VK.Entity.Group.CallBackServer,
  VK.Entity.Group.CallbackSettings, VK.Entity.Group.Categories,
  VK.Entity.Longpoll, VK.Entity.Group.LongpollSettings, VK.Entity.GroupSettings,
  VK.Entity.Group.TokenPermissions, VK.Entity.Common.List,
  VK.Entity.Group.Invites;

type
  TVkGroupTagAct = (gtaBind, gtaUnbind);

  TVkGroupTagActHelper = record helper for TVkGroupTagAct
    function ToString: string; inline;
  end;

  TVkNoteText = string;
  /// <summary>
  /// <b>wisNoInformation</b> — нет информации о расписании;
  /// <b>wisTemporarilyClosed</b> — временно закрыто;
  /// <b>wisAlwaysOpened</b> — открыто круглосуточно;
  /// <b>wisForeverClosed</b> — закрыто навсегда;
  /// <b>wisTimetable</b> — открыто в указанные часы работы. Для этого типа расписания необходимо передать параметр <b>Timetable: TVkTimeTable</b>;
  /// </summary>

  TVkWorkInfoStatus = (wisNoInformation, wisTemporarilyClosed, wisAlwaysOpened, wisForeverClosed, wisTimetable);

  TVkWorkInfoStatusHelper = record helper for TVkWorkInfoStatus
    function ToString: string; inline;
  end;

  TVkGroupSearchSort = (gssDefault, gssGrowthRate, gssDailyTraffic, gssNumberOfLikes, gssNumberOfComments, gssNumberOfPosts);

  TVkGroupSearchSortHelper = record helper for TVkGroupSearchSort
    function ToConst: Integer; inline;
  end;

  TVkParamsGroupsGetMembers = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(Value: Integer): Integer; overload;
    function GroupId(Value: string): Integer; overload;
    function Filter(Value: TVkGroupMembersFilter): Integer;
    function Fields(Value: TVkGroupMemberFields): Integer;
    function Count(Value: Integer = 1000): Integer;
    function Offset(Value: Integer = 0): Integer;
    function Sort(Value: TVkSortIdTime): Integer;
  end;

  TVkParamsGroupsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя.
    /// </summary>
    function UserId(Value: Integer): Integer;
    function Filter(Value: TVkGroupFilters): Integer; overload;
    function Fields(Value: TVkGroupFields): Integer; overload;
    function Count(Value: Integer = 1000): Integer;
    function Offset(Value: Integer = 0): Integer;
  end;

  TVkParamsGroupsIsMember = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(Value: Integer): Integer; overload;
    /// <summary>
    /// Короткое имя сообщества.
    /// </summary>
    function GroupId(Value: string): Integer; overload;
    /// <summary>
    /// True — вернуть ответ в расширенной форме. По умолчанию — False.
    /// </summary>
    function Extended(Value: Boolean = False): Integer;
    /// <summary>
    /// Идентификатор пользователя.
    /// </summary>
    function UserId(Value: Integer): Integer;
    /// <summary>
    /// Идентификаторы пользователей, не более 500.
    /// </summary>
    function UserIds(Value: TIdList): Integer;
  end;

  TVkParamsGroupsAddAddress = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    function Title(Value: string): Integer;
    function Address(Value: string): Integer;
    function AdditionalAddress(Value: string): Integer;
    function CountryId(Value: Integer): Integer;
    function CityId(Value: Integer): Integer;
    function MetroId(Value: Integer): Integer;
    function Latitude(Value: Extended): Integer;
    function Longitude(Value: Extended): Integer;
    function Phone(Value: string): Integer;
    function WorkInfoStatus(Value: TVkWorkInfoStatus): Integer;
    function Timetable(Value: TVkTimeTable; FreeObject: Boolean = False): Integer;
    function IsMainAddress(Value: Boolean): Integer;
  end;

  TVkParamsGroupsEditAddress = TVkParamsGroupsAddAddress;

  TVkParamsGroupsBan = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    function OwnerId(Value: Integer): Integer;
    function EndDate(Value: TDateTime): Integer;
    function Reason(Value: TVkUserBlockReason = brOther): Integer;
    function Comment(Value: string): Integer;
    function CommentVisible(Value: Boolean = False): Integer;
  end;

  TVkParamsGroupsCreate = record
    List: TParams;
    /// <summary>
    /// Название сообщества
    /// </summary>
    function Title(Value: string): Integer;
    /// <summary>
    /// Описание сообщества, (не учитывается при Type = gtPublic)
    /// </summary>
    function Description(Value: string): Integer;
    /// <summary>
    /// Тип создаваемого сообщества
    /// </summary>
    function &Type(Value: TVkGroupType): Integer;
    /// <summary>
    /// Категория публичной страницы (только для Type = gtPublic).
    /// </summary>
    function PublicCategory(Value: Integer): Integer;
    /// <summary>
    /// Вид публичной страницы (только при Type = gtPublic)
    /// </summary>
    function Subtype(Value: Integer): Integer;
  end;

  TVkParamsGroupsEdit = record
    const
      GroupSectionOff = 0;
      GroupSectionOpen = 1;
      GroupSectionPrivate = 2;
      GroupSectionClosed = 3;
    type
      TGroupSectionWall = GroupSectionOff..GroupSectionClosed;

      TGroupSectionTopics = GroupSectionOff..GroupSectionPrivate;

      TGroupSectionPhotos = GroupSectionOff..GroupSectionPrivate;

      TGroupSectionVideo = GroupSectionOff..GroupSectionPrivate;

      TGroupSectionAudio = GroupSectionOff..GroupSectionPrivate;

      TGroupSectionDocs = GroupSectionOff..GroupSectionPrivate;

      TGroupSectionWiki = GroupSectionOff..GroupSectionPrivate;
  public
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(Value: Integer): Integer;
    function Title(Value: string): Integer;
    function Description(Value: string): Integer;
    function ScreenName(Value: string): Integer;
    function Access(Value: TVkGroupAccess): Integer;
    function Website(Value: string): Integer;
    /// <summary>
    /// 1 — авто/мото;  2 — активный отдых;  3 — бизнес;  4 — домашние животные;  5 — здоровье;
    /// 6 — знакомство и общение;  7 — игры;  8 — ИТ (компьютеры и софт);  9 — кино;  10 — красота и мода;
    /// 11 — кулинария;  12 — культура и искусство;  13 — литература;  14 — мобильная связь и интернет;
    /// 15 — музыка;  16 — наука и техника;  17 — недвижимость;  18 — новости и СМИ;  19 — безопасность;
    /// 20 — образование;  21 — обустройство и ремонт;  22 — политика;  23 — продукты питания;
    /// 24 — промышленность;  25 — путешествия;  26 — работа;  27 — развлечения;  28 — религия;
    /// 29 — дом и семья;  30 — спорт;  31 — страхование;  32 — телевидение;  33 — товары и услуги;
    /// 34 — увлечения и хобби;  35 — финансы;  36 — фото;  37 — эзотерика;  38 — электроника и бытовая техника;
    /// 39 — эротика;  40 — юмор;  41 — общество, гуманитарные науки;  42 — дизайн и графика.
    /// </summary>
    function Subject(Value: Integer): Integer;
    function Email(Value: string): Integer;
    function Phone(Value: string): Integer;
    function Rss(Value: string): Integer;
    function EventStartDate(Value: TDateTime): Integer;
    function EventFinishDate(Value: TDateTime): Integer;
    function EventGroupId(Value: Integer): Integer;
    function PublicCategory(Value: Integer): Integer;
    function PublicSubcategory(Value: Integer): Integer;
    function PublicDate(Value: TDateTime): Integer;
    function Wall(Value: TGroupSectionWall): Integer;
    function Topics(Value: TGroupSectionTopics): Integer;
    function Photos(Value: TGroupSectionPhotos): Integer;
    function Video(Value: TGroupSectionVideo): Integer;
    function Audio(Value: TGroupSectionAudio): Integer;
    function Links(Value: Boolean): Integer;
    function Events(Value: Boolean): Integer;
    function Places(Value: Boolean): Integer;
    function Contacts(Value: Boolean): Integer;
    function Docs(Value: TGroupSectionDocs): Integer;
    function Wiki(Value: TGroupSectionWiki): Integer;
    function Messages(Value: Boolean): Integer;
    function Articles(Value: Boolean): Integer;
    function Addresses(Value: Boolean): Integer;
    function AgeLimits(Value: TVkAgeLimits): Integer;
    function Market(Value: Boolean): Integer;
    function MarketComments(Value: Boolean): Integer;
    function MarketCountry(Value: TIdList): Integer;
    function MarketCity(Value: TIdList): Integer;
    function MarketCurrency(Value: TVkCurrency): Integer;
    function MarketContact(Value: Integer): Integer;
    function MarketWiki(Value: Integer): Integer;
    function ObsceneFilter(Value: Boolean): Integer;
    function ObsceneStopwords(Value: Boolean): Integer;
    function ObsceneWords(Value: TArrayOfString): Integer;
    function MainSection(Value: Integer): Integer;
    function SecondarySection(Value: Integer): Integer;
    function Country(Value: Integer): Integer;
    function City(Value: Integer): Integer;
  end;

  TVkParamsGroupsEditManager = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function UserId(Value: Integer): Integer;
    function Role(Value: TVkGroupRole): Integer;
    function IsContact(Value: Boolean): Integer;
    function ContactPosition(Value: string): Integer;
    function ContactPhone(Value: string): Integer;
    function ContactEmail(Value: string): Integer;
  end;

  TVkParamsGroupsGetAddresses = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function AddressIds(Value: TIdList): Integer; overload;
    function AddressIds(Value: Integer): Integer; overload;
    function Latitude(Value: Extended): Integer;
    function Longitude(Value: Extended): Integer;
    function Fields(Value: TVkGroupAddressFields): Integer; overload;
    function Count(Value: Integer = 10): Integer;
    function Offset(Value: Integer = 0): Integer;
  end;

  TVkParamsGroupsGetBanned = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function Offset(Value: Integer = 0): Integer;
    function Count(Value: Integer = 20): Integer;
    function Fields(GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer; overload;
    function OwnerId(Value: Integer): Integer;
  end;

  TVkParamsGroupsGetInvitedUsers = record
    List: TParams;
    function GroupId(Value: Integer): Integer;
    function Offset(Value: Integer = 0): Integer;
    function Count(Value: Integer = 20): Integer;
    function Fields(Value: TVkProfileFields = []): Integer;
    function NameCase(Value: TVkNameCase): Integer;
  end;

  TVkParamsGroupsSearch = record
    List: TParams;
    function Query(Value: string): Integer;
    function &Type(Value: TVkGroupType): Integer;
    function CountryId(Value: Integer): Integer;
    function CityId(Value: Integer): Integer;
    function Future(Value: Boolean): Integer;
    function Market(Value: Boolean): Integer;
    function Sort(Value: TVkGroupSearchSort): Integer;
    function Offset(Value: Integer = 0): Integer;
    function Count(Value: Integer = 20): Integer;
  end;

  TVkParamsGroupsSetCallbackSettings = record
    List: TParams;
    function GroupId(Value: Integer): integer;
    function ApiVersion(Value: string): integer;
    function ServerId(Value: Integer): integer;
    function AudioNew(Value: Boolean): integer;
    function BoardPostDelete(Value: Boolean): integer;
    function BoardPostEdit(Value: Boolean): integer;
    function BoardPostNew(Value: Boolean): integer;
    function BoardPostRestore(Value: Boolean): integer;
    function GroupChangePhoto(Value: Boolean): integer;
    function GroupChangeSettings(Value: Boolean): integer;
    function GroupJoin(Value: Boolean): integer;
    function GroupLeave(Value: Boolean): integer;
    function GroupOfficersEdit(Value: Boolean): integer;
    function LeadFormsNew(Value: Boolean): integer;
    function LikeAdd(Value: Boolean): integer;
    function LikeRemove(Value: Boolean): integer;
    function MarketCommentDelete(Value: Boolean): integer;
    function MarketCommentEdit(Value: Boolean): integer;
    function MarketCommentNew(Value: Boolean): integer;
    function MarketCommentRestore(Value: Boolean): integer;
    function MessageAllow(Value: Boolean): integer;
    function MessageDeny(Value: Boolean): integer;
    function MessageEdit(Value: Boolean): integer;
    function MessageNew(Value: Boolean): integer;
    function MessageReply(Value: Boolean): integer;
    function MessageTypingState(Value: Boolean): integer;
    function PhotoCommentDelete(Value: Boolean): integer;
    function PhotoCommentEdit(Value: Boolean): integer;
    function PhotoCommentNew(Value: Boolean): integer;
    function PhotoCommentRestore(Value: Boolean): integer;
    function PhotoNew(Value: Boolean): integer;
    function PollVoteNew(Value: Boolean): integer;
    function UserBlock(Value: Boolean): integer;
    function UserUnblock(Value: Boolean): integer;
    function VideoCommentDelete(Value: Boolean): integer;
    function VideoCommentEdit(Value: Boolean): integer;
    function VideoCommentNew(Value: Boolean): integer;
    function VideoCommentRestore(Value: Boolean): integer;
    function VideoNew(Value: Boolean): integer;
    function WallPostNew(Value: Boolean): integer;
    function WallReplyDelete(Value: Boolean): integer;
    function WallReplyEdit(Value: Boolean): integer;
    function WallReplyNew(Value: Boolean): integer;
    function WallReplyRestore(Value: Boolean): integer;
    function WallRepost(Value: Boolean): integer;
  end;

  TVkParamsGroupsSetLongpollSettings = record
    List: TParams;
    function GroupId(Value: Integer): integer;
    function ApiVersion(Value: string): integer;
    function AudioNew(Value: Boolean): integer;
    function BoardPostDelete(Value: Boolean): integer;
    function BoardPostEdit(Value: Boolean): integer;
    function BoardPostNew(Value: Boolean): integer;
    function BoardPostRestore(Value: Boolean): integer;
    function GroupChangePhoto(Value: Boolean): integer;
    function GroupChangeSettings(Value: Boolean): integer;
    function GroupJoin(Value: Boolean): integer;
    function GroupLeave(Value: Boolean): integer;
    function GroupOfficersEdit(Value: Boolean): integer;
    function LeadFormsNew(Value: Boolean): integer;
    function LikeAdd(Value: Boolean): integer;
    function LikeRemove(Value: Boolean): integer;
    function MarketCommentDelete(Value: Boolean): integer;
    function MarketCommentEdit(Value: Boolean): integer;
    function MarketCommentNew(Value: Boolean): integer;
    function MarketCommentRestore(Value: Boolean): integer;
    function MessageAllow(Value: Boolean): integer;
    function MessageDeny(Value: Boolean): integer;
    function MessageEdit(Value: Boolean): integer;
    function MessageNew(Value: Boolean): integer;
    function MessageReply(Value: Boolean): integer;
    function MessageTypingState(Value: Boolean): integer;
    function PhotoCommentDelete(Value: Boolean): integer;
    function PhotoCommentEdit(Value: Boolean): integer;
    function PhotoCommentNew(Value: Boolean): integer;
    function PhotoCommentRestore(Value: Boolean): integer;
    function PhotoNew(Value: Boolean): integer;
    function PollVoteNew(Value: Boolean): integer;
    function UserBlock(Value: Boolean): integer;
    function UserUnblock(Value: Boolean): integer;
    function VideoCommentDelete(Value: Boolean): integer;
    function VideoCommentEdit(Value: Boolean): integer;
    function VideoCommentNew(Value: Boolean): integer;
    function VideoCommentRestore(Value: Boolean): integer;
    function VideoNew(Value: Boolean): integer;
    function WallPostNew(Value: Boolean): integer;
    function WallReplyDelete(Value: Boolean): integer;
    function WallReplyEdit(Value: Boolean): integer;
    function WallReplyNew(Value: Boolean): integer;
    function WallReplyRestore(Value: Boolean): integer;
    function WallRepost(Value: Boolean): integer;
  end;

  TVkParamsGroupsSetSettings = record
    List: TParams;
    function GroupId(Value: Integer): integer;
    function Messages(Value: Boolean): integer;
    function BotsCapabilities(Value: Boolean): integer;
    function BotsStartButton(Value: Boolean): integer;
    function BotsAddToChat(Value: Boolean): integer;
  end;

  TGroupsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список участников сообщества.
    /// </summary>
    function GetMembers(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список участников сообщества.
    /// </summary>
    function GetMembers(var Items: TVkProfiles; Params: TVkParamsGroupsGetMembers): Boolean; overload;
    /// <summary>
    /// Возвращает список id участников сообщества.
    /// </summary>
    function GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembers): Boolean; overload;
    /// <summary>
    /// Включает статус «онлайн» в сообществе.
    /// </summary>
    function EnableOnline(GroupId: Cardinal): Boolean;
    /// <summary>
    /// Выключает статус «онлайн» в сообществе.
    /// </summary>
    function DisableOnline(GroupId: Cardinal): Boolean;
    /// <summary>
    /// Получает информацию о статусе «онлайн» в сообществе.
    /// </summary>
    function GetOnlineStatus(var Value: TVkGroupStatus; GroupId: Cardinal): Boolean;
    /// <summary>
    /// Возвращает список сообществ указанного пользователя.
    /// </summary>
    function Get(var Items: TVkGroups; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список сообществ указанного пользователя.
    /// </summary>
    function Get(var Items: TVkGroups; Params: TVkParamsGroupsGet): Boolean; overload;
    /// <summary>
    /// Возвращает список id сообществ указанного пользователя.
    /// </summary>
    function GetIds(var Items: TVkIdList; Params: TVkParamsGroupsGet): Boolean;
    /// <summary>
    /// Возвращает информацию о том, является ли пользователь участником сообщества.
    /// </summary>
    function IsMember(var Items: TVkGroupMemberStates; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о том, является ли пользователь участником сообщества.
    /// </summary>
    function IsMember(var Items: TVkGroupMemberStates; Params: TVkParamsGroupsIsMember): Boolean; overload;
    /// <summary>
    ///  Позволяет покинуть сообщество или отклонить приглашение в сообщество.
    /// </summary>
    function Leave(GroupId: integer): Boolean;
    /// <summary>
    ///  Данный метод позволяет вступить в группу, публичную страницу, а также подтвердить участие во встрече.
    ///  NotSure - опциональный параметр, учитываемый, если GroupId принадлежит встрече.
    ///  True — Возможно пойду. False — Точно пойду. По умолчанию False.
    /// </summary>
    function Join(GroupId: integer; NotSure: Boolean = False): Boolean;
    /// <summary>
    ///  Позволяет приглашать друзей в группу.
    /// </summary>
    function Invite(GroupId, UserId: integer): Boolean;
    /// <summary>
    ///  Позволяет исключить пользователя из группы или отклонить заявку на вступление.
    /// </summary>
    function RemoveUser(GroupId, UserId: integer): Boolean;
    /// <summary>
    ///  Позволяет одобрить заявку в группу от пользователя.
    /// </summary>
    function ApproveRequest(GroupId, UserId: integer): Boolean;
    /// <summary>
    ///  Позволяет добавить адрес в сообщество.
    ///  Список адресов может быть получен методом groups.getAddresses.
    /// </summary>
    function AddAddress(var Item: TVkGroupAddress; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Позволяет добавить адрес в сообщество.
    ///  Список адресов может быть получен методом groups.getAddresses.
    /// </summary>
    function AddAddress(var Item: TVkGroupAddress; Params: TVkParamsGroupsAddAddress): Boolean; overload;
    /// <summary>
    ///  Добавляет сервер для Callback API в сообщество.
    /// </summary>
    function AddCallbackServer(var ServerId: Integer; GroupId: integer; Url, Title: string; SecretKey: string): Boolean;
    /// <summary>
    ///  Позволяет добавлять ссылки в сообщество.
    /// </summary>
    function AddLink(var Item: TVkGroupLink; GroupId: integer; Link: string; Text: string = ''): Boolean;
    /// <summary>
    ///  Добавляет пользователя или группу в черный список сообщества.
    /// </summary>
    function Ban(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Добавляет пользователя или группу в черный список сообщества.
    /// </summary>
    function Ban(Params: TVkParamsGroupsBan): Boolean; overload;
    /// <summary>
    ///  Создает новое сообщество.
    /// </summary>
    function Create(var Item: TVkGroup; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Создает новое сообщество.
    /// </summary>
    function Create(var Item: TVkGroup; Params: TVkParamsGroupsCreate): Boolean; overload;
    /// <summary>
    ///  Удаляет адрес сообщества.
    /// </summary>
    function DeleteAddress(GroupId, AddressId: integer): Boolean;
    /// <summary>
    ///  Удаляет сервер для Callback API из сообщества.
    /// </summary>
    function DeleteCallbackServer(GroupId, ServerId: integer): Boolean;
    /// <summary>
    ///  Позволяет удалить ссылки из сообщества.
    /// </summary>
    function DeleteLink(GroupId, LinkId: integer): Boolean;
    /// <summary>
    ///  Редактирует сообщество.
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Редактирует сообщество.
    /// </summary>
    function Edit(Params: TVkParamsGroupsEdit): Boolean; overload;
    /// <summary>
    ///  Позволяет отредактировать адрес в сообществе.
    /// </summary>
    function EditAddress(var Item: TVkGroupAddress; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Позволяет отредактировать адрес в сообществе.
    /// </summary>
    function EditAddress(var Item: TVkGroupAddress; AddressId: Integer; Params: TVkParamsGroupsEditAddress): Boolean; overload;
    /// <summary>
    ///  Редактирует данные сервера для Callback API в сообществе.
    /// </summary>
    function EditCallbackServer(GroupId: Integer; ServerId: integer; Url, Title: string; SecretKey: string): Boolean;
    /// <summary>
    ///  Позволяет редактировать ссылки в сообществе.
    /// </summary>
    function EditLink(GroupId: integer; Link: string; Text: string = ''): Boolean;
    /// <summary>
    ///  Позволяет назначить/разжаловать руководителя в сообществе или изменить уровень его полномочий.
    /// </summary>
    function EditManager(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Позволяет назначить/разжаловать руководителя в сообществе или изменить уровень его полномочий.
    /// </summary>
    function EditManager(Params: TVkParamsGroupsEditManager): Boolean; overload;
    /// <summary>
    ///  Возвращает адрес указанного сообщества.
    /// </summary>
    function GetAddresses(var Item: TVkGroupAddresses; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Возвращает адрес указанного сообщества.
    /// </summary>
    function GetAddresses(var Item: TVkGroupAddresses; Params: TVkParamsGroupsGetAddresses): Boolean; overload;
    /// <summary>
    ///  Возвращает список забаненных пользователей и сообществ в сообществе.
    /// </summary>
    function GetBanned(var Items: TVkGroupBans; Params: TVkParamsGroupsGetBanned): Boolean; overload;
    /// <summary>
    ///  Возвращает информацию о заданном сообществе или о нескольких сообществах.
    /// </summary>
    function GetById(var Items: TVkGroups; GroupIds: TIdList; Fields: TVkGroupFields = []): Boolean; overload;
    /// <summary>
    ///  Возвращает информацию о заданном сообществе или о нескольких сообществах.
    /// </summary>
    function GetById(var Items: TVkGroups; GroupId: Integer; Fields: TVkGroupFields = []): Boolean; overload;
    /// <summary>
    ///  Позволяет получить строку, необходимую для подтверждения адреса сервера в Callback API.
    /// </summary>
    function GetCallbackConfirmationCode(var Code: string; GroupId: Integer): Boolean;
    /// <summary>
    ///  Получает информацию о серверах для Callback API в сообществе.
    /// </summary>
    function GetCallbackServers(var Items: TVkGroupCallbackServers; GroupId: Integer; ServerIds: TIdList = []): Boolean;
    /// <summary>
    ///  Позволяет получить настройки уведомлений Callback API для сообщества.
    /// </summary>
    function GetCallbackSettings(var Items: TVkCallbackSettings; GroupId: Integer; ServerId: Integer): Boolean;
    /// <summary>
    ///  Возвращает список сообществ выбранной категории каталога.
    /// </summary>
    function GetCatalog(var Items: TVkGroups; CategoryId: Integer = 0; SubcategoryId: Integer = 0): Boolean;
    /// <summary>
    ///  Возвращает список категорий для каталога сообществ.
    /// </summary>
    function GetCatalogInfo(var Items: TVkGroupCategories; Subcategories: Boolean = False; Extended: Boolean = False): Boolean;
    /// <summary>
    ///  Возвращает список пользователей, которые были приглашены в группу.
    /// </summary>
    function GetInvitedUsers(var Items: TVkProfiles; Params: TVkParamsGroupsGetInvitedUsers): Boolean;
    /// <summary>
    ///  Данный метод возвращает список приглашений в сообщества и встречи текущего пользователя.
    /// </summary>
    function GetInvites(var Items: TVkInvitesGroups; Extended: Boolean = False; Count: Integer = 20; Offset: Integer = 0): Boolean;
    /// <summary>
    ///  Возвращает данные для подключения к Bots Longpoll API.
    /// </summary>
    function GetLongPollServer(var Item: TVkLongpollData; GroupId: Integer): Boolean;
    /// <summary>
    ///  Получает настройки Bots Longpoll API для сообщества.
    /// </summary>
    function GetLongPollSettings(var Item: TVkLongpollSettings; GroupId: Integer): Boolean;
    /// <summary>
    ///  Возвращает список заявок на вступление в сообщество.
    /// </summary>
    function GetRequests(var Items: TVkProfiles; GroupId: Integer; Fields: TVkProfileFields = [ufDomain]; Count: Integer = 20; Offset: Integer = 0): Boolean; overload;
    /// <summary>
    ///  Возвращает список заявок на вступление в сообщество.
    /// </summary>
    function GetRequestsIds(var Items: TVkIdList; GroupId: Integer; Count: Integer = 20; Offset: Integer = 0): Boolean; overload;
    /// <summary>
    ///  Позволяет получать данные, необходимые для отображения страницы редактирования данных сообщества.
    /// </summary>
    function GetSettings(var Item: TVkGroupSettings; GroupId: Integer): Boolean;
    /// <summary>
    ///  Возвращает список тегов сообщества
    /// </summary>
    function GetTagList(var Items: TVkGroupTags; GroupId: Integer): Boolean;
    /// <summary>
    ///  Возвращает настройки прав для ключа доступа сообщества.
    /// </summary>
    function GetTokenPermissions(var Items: TVkTokenPermissions): Boolean;
    /// <summary>
    ///  Позволяет менять местоположение ссылки в списке.
    /// </summary>
    function ReorderLink(GroupId, LinkId: Integer; After: Integer): Boolean;
    /// <summary>
    ///  Осуществляет поиск сообществ по заданной подстроке.
    /// </summary>
    function Search(var Items: TVkGroups; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Осуществляет поиск сообществ по заданной подстроке.
    /// </summary>
    function Search(var Items: TVkGroups; Params: TVkParamsGroupsSearch): Boolean; overload;
    /// <summary>
    ///  Позволяет задать настройки уведомлений о событиях в Callback API.
    /// </summary>
    function SetCallbackSettings(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Позволяет задать настройки уведомлений о событиях в Callback API.
    /// </summary>
    function SetCallbackSettings(Params: TVkParamsGroupsSetCallbackSettings): Boolean; overload;
    /// <summary>
    ///  Задаёт настройки для Bots Long Poll API в сообществе.
    /// </summary>
    function SetLongPollSettings(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Задаёт настройки для Bots Long Poll API в сообществе.
    /// </summary>
    function SetLongPollSettings(Params: TVkParamsGroupsSetLongpollSettings): Boolean; overload;
    /// <summary>
    ///  Устанавливает настройки сообщества
    /// </summary>
    function SetSettings(Params: TVkParamsGroupsSetSettings): Boolean;
    /// <summary>
    ///  Позволяет создать или отредактировать заметку о пользователе в рамках переписки пользователя с сообществом
    /// </summary>
    function SetUserNote(GroupId, UserId: Integer; Note: TVkNoteText): Boolean;
    /// <summary>
    ///  Позволяет добавить новый тег в сообщество
    /// </summary>
    function TagAdd(GroupId: Integer; TagName: string; TagColor: TVkGroupTagColor): Boolean;
    /// <summary>
    ///  Позволяет "привязывать" и "отвязывать" теги сообщества к беседам.
    /// </summary>
    function TagBind(GroupId: Integer; TagId, UserId: Integer; Act: TVkGroupTagAct): Boolean;
    /// <summary>
    ///  Позволяет удалить тег сообщества
    /// </summary>
    function TagDelete(GroupId, TagId: Integer): Boolean;
    /// <summary>
    ///  Позволяет переименовать существующий тег
    /// </summary>
    function TagUpdate(GroupId, TagId: Integer; TagName: string): Boolean;
    /// <summary>
    ///  Убирает пользователя или группу из черного списка сообщества.
    /// </summary>
    function Unban(GroupId, OwnerId: Integer): Boolean;
  end;

implementation

uses
  System.DateUtils;

{ TGroupsController }

function TGroupsController.AddAddress(var Item: TVkGroupAddress; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.addAddress', Params).GetObject<TVkGroupAddress>(Item);
end;

function TGroupsController.AddAddress(var Item: TVkGroupAddress; Params: TVkParamsGroupsAddAddress): Boolean;
begin
  Result := AddAddress(Item, Params.List);
end;

function TGroupsController.AddCallbackServer(var ServerId: Integer; GroupId: integer; Url, Title, SecretKey: string): Boolean;
begin
  Result := Handler.Execute('groups.addCallbackServer', [
    ['group_id', GroupId.ToString],
    ['url', Url],
    ['title', Title],
    ['secret_key', SecretKey]]).
    GetValue('server_id', ServerId);
end;

function TGroupsController.AddLink(var Item: TVkGroupLink; GroupId: integer; Link, Text: string): Boolean;
begin
  Result := Handler.Execute('groups.addLink', [
    ['GroupId', GroupId.ToString],
    ['link', Link],
    ['text', Text]]).
    GetObject<TVkGroupLink>(Item);
end;

function TGroupsController.ApproveRequest(GroupId, UserId: integer): Boolean;
begin
  with Handler.Execute('groups.approveRequest', [['group_id', GroupId.ToString], ['user_id', UserId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.Ban(Params: TVkParamsGroupsBan): Boolean;
begin
  Result := Ban(Params.List);
end;

function TGroupsController.Create(var Item: TVkGroup; Params: TVkParamsGroupsCreate): Boolean;
begin
  Result := Create(Item, Params.List);
end;

function TGroupsController.Create(var Item: TVkGroup; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.create', Params).GetObject<TVkGroup>(Item);
end;

function TGroupsController.Ban(Params: TParams): Boolean;
begin
  with Handler.Execute('groups.ban', Params) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.DeleteAddress(GroupId, AddressId: integer): Boolean;
begin
  with Handler.Execute('groups.deleteAddress', [['group_id', GroupId.ToString], ['address_id', AddressId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.DeleteCallbackServer(GroupId, ServerId: integer): Boolean;
begin
  with Handler.Execute('groups.deleteCallbackServer', [['group_id', GroupId.ToString], ['server_id', ServerId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.DeleteLink(GroupId, LinkId: integer): Boolean;
begin
  with Handler.Execute('groups.deleteLink', [['group_id', GroupId.ToString], ['link_id', LinkId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.DisableOnline(GroupId: Cardinal): Boolean;
begin
  with Handler.Execute('groups.disableOnline', ['group_id', GroupId.ToString]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.Edit(Params: TParams): Boolean;
begin
  with Handler.Execute('groups.edit', Params) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.Edit(Params: TVkParamsGroupsEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TGroupsController.EditAddress(var Item: TVkGroupAddress; AddressId: Integer; Params: TVkParamsGroupsEditAddress): Boolean;
begin
  Params.List.Add('address_id', AddressId);
  Result := EditAddress(Item, Params.List);
end;

function TGroupsController.EditCallbackServer(GroupId, ServerId: integer; Url, Title, SecretKey: string): Boolean;
begin
  with Handler.Execute('groups.editCallbackServer', [['group_id', GroupId.ToString], ['server_id', ServerId.ToString], ['url', Url], ['title', Title], ['secret_key', SecretKey]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.EditLink(GroupId: integer; Link, Text: string): Boolean;
begin
  with Handler.Execute('groups.editLink', [['GroupId', GroupId.ToString], ['link', Link], ['text', Text]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.EditManager(Params: TVkParamsGroupsEditManager): Boolean;
begin
  Result := EditManager(Params.List);
end;

function TGroupsController.EditManager(Params: TParams): Boolean;
begin
  with Handler.Execute('groups.editManager', Params) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.EditAddress(var Item: TVkGroupAddress; Params: TParams): Boolean;
begin
  with Handler.Execute('groups.editAddress', Params) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.EnableOnline(GroupId: Cardinal): Boolean;
begin
  with Handler.Execute('groups.enableOnline', ['group_id', GroupId.ToString]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.Get(var Items: TVkGroups; Params: TVkParamsGroupsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TGroupsController.GetAddresses(var Item: TVkGroupAddresses; Params: TVkParamsGroupsGetAddresses): Boolean;
begin
  Result := GetAddresses(Item, Params.List);
end;

function TGroupsController.GetBanned(var Items: TVkGroupBans; Params: TVkParamsGroupsGetBanned): Boolean;
begin
  Result := Handler.Execute('groups.getBanned', Params.List).GetObject<TVkGroupBans>(Items);
end;

function TGroupsController.GetById(var Items: TVkGroups; GroupId: Integer; Fields: TVkGroupFields): Boolean;
begin
  Result := GetById(Items, [GroupId], Fields);
end;

function TGroupsController.GetCallbackConfirmationCode(var Code: string; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getCallbackConfirmationCode', ['group_id', GroupId.ToString]).GetValue('code', Code);
end;

function TGroupsController.GetCallbackServers(var Items: TVkGroupCallbackServers; GroupId: Integer; ServerIds: TIdList): Boolean;
begin
  Result := Handler.Execute('groups.getCallbackServers', [
    ['group_id', GroupId.ToString],
    ['server_ids', ServerIds.ToString]]).
    GetObject<TVkGroupCallbackServers>(Items);
end;

function TGroupsController.GetCallbackSettings(var Items: TVkCallbackSettings; GroupId, ServerId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getCallbackSettings', [
    ['group_id', GroupId.ToString],
    ['server_id', ServerId.ToString]]).
    GetObject<TVkCallbackSettings>(Items);
end;

function TGroupsController.GetCatalog(var Items: TVkGroups; CategoryId, SubcategoryId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getById', [
    ['category_id', CategoryId.ToString],
    ['subcategory_id', SubcategoryId.ToString]]).
    GetObject<TVkGroups>(Items);
end;

function TGroupsController.GetCatalogInfo(var Items: TVkGroupCategories; Subcategories, Extended: Boolean): Boolean;
begin
  Result := Handler.Execute('groups.getCatalogInfo', [
    ['subcategories', BoolToString(Subcategories)],
    ['extended', BoolToString(Extended)]]).
    GetObject<TVkGroupCategories>(Items);
end;

function TGroupsController.GetById(var Items: TVkGroups; GroupIds: TIdList; Fields: TVkGroupFields): Boolean;
begin
  Result := Handler.Execute('groups.getById', [
    ['group_ids', GroupIds.ToString],
    ['fields', Fields.ToString]]).
    GetObjects<TVkGroups>(Items);
end;

function TGroupsController.GetAddresses(var Item: TVkGroupAddresses; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.getAddresses', Params).GetObject<TVkGroupAddresses>(Item);
end;

function TGroupsController.Get(var Items: TVkGroups; Params: TParams): Boolean;
begin
  Params.Add('extended', True);
  Result := Handler.Execute('groups.get', Params).GetObject<TVkGroups>(Items);
end;

function TGroupsController.GetIds(var Items: TVkIdList; Params: TVkParamsGroupsGet): Boolean;
begin
  Params.List.Add('extended', False);
  Result := Handler.Execute('groups.get', Params.List).GetObject<TVkIdList>(Items);
end;

function TGroupsController.GetInvitedUsers(var Items: TVkProfiles; Params: TVkParamsGroupsGetInvitedUsers): Boolean;
begin
  Result := Handler.Execute('groups.getInvitedUsers', Params.List).GetObject<TVkProfiles>(Items);
end;

function TGroupsController.GetInvites(var Items: TVkInvitesGroups; Extended: Boolean; Count, Offset: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getInvites', [
    ['extended', BoolToString(Extended)],
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject<TVkInvitesGroups>(Items);
end;

function TGroupsController.GetLongPollServer(var Item: TVkLongpollData; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getLongPollServer', ['group_id', GroupId.ToString]).GetObject<TVkLongpollData>(Item);
end;

function TGroupsController.GetLongPollSettings(var Item: TVkLongpollSettings; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getLongPollSettings', ['group_id', GroupId.ToString]).GetObject<TVkLongpollSettings>(Item);
end;

function TGroupsController.GetMembers(var Items: TVkProfiles; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  if not Params.List.KeyExists('fields') then
    Params.Fields([mfDomain]);
  Result := GetMembers(Items, Params.List);
end;

function TGroupsController.GetMembers(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.getMembers', Params).GetObject<TVkProfiles>(Items);
end;

function TGroupsController.GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  Params.Fields([]);
  Result := Handler.Execute('groups.getMembers', Params.List).GetObject<TVkIdList>(Items);
end;

function TGroupsController.GetOnlineStatus(var Value: TVkGroupStatus; GroupId: Cardinal): Boolean;
begin
  Result := Handler.Execute('groups.getOnlineStatus', ['group_id', GroupId.ToString]).GetObject<TVkGroupStatus>(Value);
end;

function TGroupsController.GetRequests(var Items: TVkProfiles; GroupId: Integer; Fields: TVkProfileFields; Count, Offset: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  if Fields = [] then
    Fields := [ufDomain];
  Params.Add('fields', Fields.ToString);
  Result := Handler.Execute('groups.getRequests', Params).GetObject<TVkProfiles>(Items);
end;

function TGroupsController.GetRequestsIds(var Items: TVkIdList; GroupId, Count, Offset: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  Result := Handler.Execute('groups.getRequests', Params).GetObject<TVkIdList>(Items);
end;

function TGroupsController.GetSettings(var Item: TVkGroupSettings; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getSettings', ['group_id', GroupId.ToString]).GetObject<TVkGroupSettings>(Item);
end;

function TGroupsController.GetTagList(var Items: TVkGroupTags; GroupId: Integer): Boolean;
begin
  with Handler.Execute('groups.getTagList', ['group_id', GroupId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkGroupTags.FromJsonString<TVkGroupTags>(ResponseAsItems);
        Items.Count := Length(Items.Items);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.GetTokenPermissions(var Items: TVkTokenPermissions): Boolean;
begin
  Result := Handler.Execute('groups.getTokenPermissions').GetObject<TVkTokenPermissions>(Items);
end;

function TGroupsController.Invite(GroupId, UserId: integer): Boolean;
begin
  with Handler.Execute('groups.invite', [['group_id', GroupId.ToString], ['user_id', UserId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.IsMember(var Items: TVkGroupMemberStates; Params: TVkParamsGroupsIsMember): Boolean;
begin
  Result := IsMember(Items, Params.List);
end;

function TGroupsController.Join(GroupId: integer; NotSure: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  if NotSure then
    Params.Add('not_sure', NotSure);
  with Handler.Execute('groups.join', Params) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.Leave(GroupId: integer): Boolean;
begin
  with Handler.Execute('groups.leave', ['group_id', GroupId.ToString]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.RemoveUser(GroupId, UserId: integer): Boolean;
begin
  with Handler.Execute('groups.removeUser', [['group_id', GroupId.ToString], ['user_id', UserId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.ReorderLink(GroupId, LinkId, After: Integer): Boolean;
begin
  with Handler.Execute('groups.reorderLink', [['group_id', GroupId.ToString], ['link_id', LinkId.ToString], ['after', After.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.Search(var Items: TVkGroups; Params: TVkParamsGroupsSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TGroupsController.SetCallbackSettings(Params: TVkParamsGroupsSetCallbackSettings): Boolean;
begin
  Result := SetCallbackSettings(Params.List);
end;

function TGroupsController.SetCallbackSettings(Params: TParams): Boolean;
begin
  with Handler.Execute('groups.setCallbackSettings', Params) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.SetLongPollSettings(Params: TVkParamsGroupsSetLongpollSettings): Boolean;
begin
  Result := SetLongPollSettings(Params.List);
end;

function TGroupsController.SetSettings(Params: TVkParamsGroupsSetSettings): Boolean;
begin
  with Handler.Execute('groups.setSettings', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.SetUserNote(GroupId, UserId: Integer; Note: TVkNoteText): Boolean;
begin
  with Handler.Execute('groups.setLongPollSettings', [['group_id', GroupId.ToString], ['user_id', UserId.ToString], ['note', string(Note)]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.TagAdd(GroupId: Integer; TagName: string; TagColor: TVkGroupTagColor): Boolean;
begin
  with Handler.Execute('groups.tagAdd', [['group_id', GroupId.ToString], ['tag_name', TagName], ['tag_color', TagColor]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.TagBind(GroupId, TagId, UserId: Integer; Act: TVkGroupTagAct): Boolean;
begin
  with Handler.Execute('groups.tagBind', [['group_id', GroupId.ToString], ['tag_id', TagId.ToString], ['user_id', UserId.ToString], ['act', Act.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.TagDelete(GroupId, TagId: Integer): Boolean;
begin
  with Handler.Execute('groups.tagDelete', [['group_id', GroupId.ToString], ['tag_id', TagId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.TagUpdate(GroupId, TagId: Integer; TagName: string): Boolean;
begin
  with Handler.Execute('groups.tagUpdate', [['group_id', GroupId.ToString], ['tag_name', TagName], ['tag_id', TagId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.Unban(GroupId, OwnerId: Integer): Boolean;
begin
  with Handler.Execute('groups.unban', [['group_id', GroupId.ToString], ['owner_id', OwnerId.ToString]]) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.SetLongPollSettings(Params: TParams): Boolean;
begin
  with Handler.Execute('groups.setLongPollSettings', Params) do
    Result := Success and ResponseIsTrue;
end;

function TGroupsController.Search(var Items: TVkGroups; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.search', Params).GetObject<TVkGroups>(Items);
end;

function TGroupsController.IsMember(var Items: TVkGroupMemberStates; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.isMember', Params).GetObjects<TVkGroupMemberStates>(Items);
end;

{ TVkGetMembersParams }

function TVkParamsGroupsGetMembers.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetMembers.Fields(Value: TVkGroupMemberFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGetMembers.Filter(Value: TVkGroupMembersFilter): Integer;
begin
  Result := List.Add('filter', Value.ToString);
end;

function TVkParamsGroupsGetMembers.GroupId(Value: string): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetMembers.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetMembers.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGetMembers.Sort(Value: TVkSortIdTime): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

{ TVkGroupsGetParams }

function TVkParamsGroupsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGet.Fields(Value: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGet.Filter(Value: TVkGroupFilters): Integer;
begin
  Result := List.Add('filter', Value.ToString);
end;

function TVkParamsGroupsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGet.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsGroupsIsMember }

function TVkParamsGroupsIsMember.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsGroupsIsMember.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsIsMember.GroupId(Value: string): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsIsMember.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

function TVkParamsGroupsIsMember.UserIds(Value: TIdList): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkWorkInfoStatusHelper }

function TVkWorkInfoStatusHelper.ToString: string;
begin
  case Self of
    wisNoInformation:
      Result := 'no_information';
    wisTemporarilyClosed:
      Result := 'temporarily_closed';
    wisAlwaysOpened:
      Result := 'always_opened';
    wisForeverClosed:
      Result := 'forever_closed';
    wisTimetable:
      Result := 'timetable';
  else
    Result := '';
  end;
end;

{ TVkParamsGroupsAddAddress }

function TVkParamsGroupsAddAddress.AdditionalAddress(Value: string): Integer;
begin
  Result := List.Add('additional_address', Value);
end;

function TVkParamsGroupsAddAddress.Address(Value: string): Integer;
begin
  Result := List.Add('address', Value);
end;

function TVkParamsGroupsAddAddress.CityId(Value: Integer): Integer;
begin
  Result := List.Add('city_id', Value);
end;

function TVkParamsGroupsAddAddress.CountryId(Value: Integer): Integer;
begin
  Result := List.Add('country_id', Value);
end;

function TVkParamsGroupsAddAddress.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsAddAddress.IsMainAddress(Value: Boolean): Integer;
begin
  Result := List.Add('is_main_address', Value);
end;

function TVkParamsGroupsAddAddress.Latitude(Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsGroupsAddAddress.Longitude(Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsGroupsAddAddress.MetroId(Value: Integer): Integer;
begin
  Result := List.Add('metro_id', Value);
end;

function TVkParamsGroupsAddAddress.Phone(Value: string): Integer;
begin
  Result := List.Add('phone', Value);
end;

function TVkParamsGroupsAddAddress.Timetable(Value: TVkTimeTable; FreeObject: Boolean): Integer;
begin
  Result := List.Add('timetable', Value.ToJsonString);
  if FreeObject then
    Value.Free;
end;

function TVkParamsGroupsAddAddress.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsGroupsAddAddress.WorkInfoStatus(Value: TVkWorkInfoStatus): Integer;
begin
  Result := List.Add('work_info_status', Value.ToString);
end;

{ TVkParamsGroupsBan }

function TVkParamsGroupsBan.Comment(Value: string): Integer;
begin
  Result := List.Add('comment', Value);
end;

function TVkParamsGroupsBan.CommentVisible(Value: Boolean): Integer;
begin
  Result := List.Add('comment_visible', Value);
end;

function TVkParamsGroupsBan.EndDate(Value: TDateTime): Integer;
begin
  Result := List.Add('end_date', Value);
end;

function TVkParamsGroupsBan.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsBan.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsGroupsBan.Reason(Value: TVkUserBlockReason): Integer;
begin
  Result := List.Add('reason', Value.ToConst);
end;

{ TVkParamsGroupsCreate }

function TVkParamsGroupsCreate.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsGroupsCreate.PublicCategory(Value: Integer): Integer;
begin
  Result := List.Add('public_category', Value);
end;

function TVkParamsGroupsCreate.Subtype(Value: Integer): Integer;
begin
  Result := List.Add('subtype', Value);
end;

function TVkParamsGroupsCreate.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsGroupsCreate.&Type(Value: TVkGroupType): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

{ TVkParamsGroupsEdit }

function TVkParamsGroupsEdit.Access(Value: TVkGroupAccess): Integer;
begin
  Result := List.Add('access', Value.ToConst);
end;

function TVkParamsGroupsEdit.Addresses(Value: Boolean): Integer;
begin
  Result := List.Add('addresses', Value);
end;

function TVkParamsGroupsEdit.AgeLimits(Value: TVkAgeLimits): Integer;
begin
  Result := List.Add('age_limits', Value.ToConst);
end;

function TVkParamsGroupsEdit.Articles(Value: Boolean): Integer;
begin
  Result := List.Add('articles', Value);
end;

function TVkParamsGroupsEdit.Audio(Value: TGroupSectionAudio): Integer;
begin
  Result := List.Add('audio', Value);
end;

function TVkParamsGroupsEdit.City(Value: Integer): Integer;
begin
  Result := List.Add('city', Value);
end;

function TVkParamsGroupsEdit.Contacts(Value: Boolean): Integer;
begin
  Result := List.Add('contacts', Value);
end;

function TVkParamsGroupsEdit.Country(Value: Integer): Integer;
begin
  Result := List.Add('country', Value);
end;

function TVkParamsGroupsEdit.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsGroupsEdit.Docs(Value: TGroupSectionDocs): Integer;
begin
  Result := List.Add('docs', Value);
end;

function TVkParamsGroupsEdit.Email(Value: string): Integer;
begin
  Result := List.Add('email', Value);
end;

function TVkParamsGroupsEdit.EventFinishDate(Value: TDateTime): Integer;
begin
  Result := List.Add('event_finish_date', Value);
end;

function TVkParamsGroupsEdit.EventGroupId(Value: Integer): Integer;
begin
  Result := List.Add('event_group_id', Value);
end;

function TVkParamsGroupsEdit.Events(Value: Boolean): Integer;
begin
  Result := List.Add('events', Value);
end;

function TVkParamsGroupsEdit.EventStartDate(Value: TDateTime): Integer;
begin
  Result := List.Add('event_start_date', Value);
end;

function TVkParamsGroupsEdit.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsEdit.Links(Value: Boolean): Integer;
begin
  Result := List.Add('links', Value);
end;

function TVkParamsGroupsEdit.MainSection(Value: Integer): Integer;
begin
  Result := List.Add('main_section', Value);
end;

function TVkParamsGroupsEdit.Market(Value: Boolean): Integer;
begin
  Result := List.Add('market', Value);
end;

function TVkParamsGroupsEdit.MarketCity(Value: TIdList): Integer;
begin
  Result := List.Add('market_city', Value);
end;

function TVkParamsGroupsEdit.MarketComments(Value: Boolean): Integer;
begin
  Result := List.Add('market_comments', Value);
end;

function TVkParamsGroupsEdit.MarketContact(Value: Integer): Integer;
begin
  Result := List.Add('market_contact', Value);
end;

function TVkParamsGroupsEdit.MarketCountry(Value: TIdList): Integer;
begin
  Result := List.Add('market_country', Value);
end;

function TVkParamsGroupsEdit.MarketCurrency(Value: TVkCurrency): Integer;
begin
  Result := List.Add('market_currency', Value.ToConst);
end;

function TVkParamsGroupsEdit.MarketWiki(Value: Integer): Integer;
begin
  Result := List.Add('market_wiki', Value);
end;

function TVkParamsGroupsEdit.Messages(Value: Boolean): Integer;
begin
  Result := List.Add('messages', Value);
end;

function TVkParamsGroupsEdit.ObsceneFilter(Value: Boolean): Integer;
begin
  Result := List.Add('obscene_filter', Value);
end;

function TVkParamsGroupsEdit.ObsceneStopwords(Value: Boolean): Integer;
begin
  Result := List.Add('obscene_stopwords', Value);
end;

function TVkParamsGroupsEdit.ObsceneWords(Value: TArrayOfString): Integer;
begin
  Result := List.Add('obscene_words', Value);
end;

function TVkParamsGroupsEdit.Phone(Value: string): Integer;
begin
  Result := List.Add('phone', Value);
end;

function TVkParamsGroupsEdit.Photos(Value: TGroupSectionPhotos): Integer;
begin
  Result := List.Add('photos', Value);
end;

function TVkParamsGroupsEdit.Places(Value: Boolean): Integer;
begin
  Result := List.Add('places', Value);
end;

function TVkParamsGroupsEdit.PublicCategory(Value: Integer): Integer;
begin
  Result := List.Add('public_category', Value);
end;

function TVkParamsGroupsEdit.PublicDate(Value: TDateTime): Integer;
begin
  Result := List.Add('public_date', FormatDateTime('dd.mm.YYYY', Value));
end;

function TVkParamsGroupsEdit.PublicSubcategory(Value: Integer): Integer;
begin
  Result := List.Add('public_subcategory', Value);
end;

function TVkParamsGroupsEdit.Rss(Value: string): Integer;
begin
  Result := List.Add('rss', Value);
end;

function TVkParamsGroupsEdit.ScreenName(Value: string): Integer;
begin
  Result := List.Add('screen_name', Value);
end;

function TVkParamsGroupsEdit.SecondarySection(Value: Integer): Integer;
begin
  Result := List.Add('secondary_section', Value);
end;

function TVkParamsGroupsEdit.Subject(Value: Integer): Integer;
begin
  Result := List.Add('subject', Value);
end;

function TVkParamsGroupsEdit.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsGroupsEdit.Topics(Value: TGroupSectionTopics): Integer;
begin
  Result := List.Add('topics', Value);
end;

function TVkParamsGroupsEdit.Video(Value: TGroupSectionVideo): Integer;
begin
  Result := List.Add('video', Value);
end;

function TVkParamsGroupsEdit.Wall(Value: TGroupSectionWall): Integer;
begin
  Result := List.Add('wall', Value);
end;

function TVkParamsGroupsEdit.Website(Value: string): Integer;
begin
  Result := List.Add('website', Value);
end;

function TVkParamsGroupsEdit.Wiki(Value: TGroupSectionWiki): Integer;
begin
  Result := List.Add('wiki', Value);
end;

{ TVkParamsGroupsEditManager }

function TVkParamsGroupsEditManager.ContactEmail(Value: string): Integer;
begin
  Result := List.Add('contact_email', Value);
end;

function TVkParamsGroupsEditManager.ContactPhone(Value: string): Integer;
begin
  Result := List.Add('contact_phone', Value);
end;

function TVkParamsGroupsEditManager.ContactPosition(Value: string): Integer;
begin
  Result := List.Add('contact_position', Value);
end;

function TVkParamsGroupsEditManager.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsEditManager.IsContact(Value: Boolean): Integer;
begin
  Result := List.Add('is_contact', Value);
end;

function TVkParamsGroupsEditManager.Role(Value: TVkGroupRole): Integer;
begin
  Result := List.Add('role', Value.ToString);
end;

function TVkParamsGroupsEditManager.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsGroupsGetAddresses }

function TVkParamsGroupsGetAddresses.AddressIds(Value: TIdList): Integer;
begin
  Result := List.Add('address_ids', Value);
end;

function TVkParamsGroupsGetAddresses.AddressIds(Value: Integer): Integer;
begin
  Result := List.Add('address_ids', Value);
end;

function TVkParamsGroupsGetAddresses.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetAddresses.Fields(Value: TVkGroupAddressFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGetAddresses.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetAddresses.Latitude(Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsGroupsGetAddresses.Longitude(Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsGroupsGetAddresses.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

{ TVkParamsGroupsGetBanned }

function TVkParamsGroupsGetBanned.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetBanned.Fields(GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsGroupsGetBanned.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetBanned.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGetBanned.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsGroupsGetInvitedUsers }

function TVkParamsGroupsGetInvitedUsers.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetInvitedUsers.Fields(Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGetInvitedUsers.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetInvitedUsers.NameCase(Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsGroupsGetInvitedUsers.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

{ TVkGroupSearchSortHelper }

function TVkGroupSearchSortHelper.ToConst: Integer;
begin
  Result := Ord(Self);
end;

{ TVkParamsGroupsSearch }

function TVkParamsGroupsSearch.CityId(Value: Integer): Integer;
begin
  Result := List.Add('city_id', Value);
end;

function TVkParamsGroupsSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsSearch.CountryId(Value: Integer): Integer;
begin
  Result := List.Add('country_id', Value);
end;

function TVkParamsGroupsSearch.Future(Value: Boolean): Integer;
begin
  Result := List.Add('future', Value);
end;

function TVkParamsGroupsSearch.Market(Value: Boolean): Integer;
begin
  Result := List.Add('matket', Value);
end;

function TVkParamsGroupsSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsGroupsSearch.Sort(Value: TVkGroupSearchSort): Integer;
begin
  Result := List.Add('sort', Value.ToConst);
end;

function TVkParamsGroupsSearch.&Type(Value: TVkGroupType): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

{ TVkParamsGroupsSetCallbackSettings }

function TVkParamsGroupsSetCallbackSettings.ApiVersion(Value: string): integer;
begin
  Result := List.Add('api_version', Value);
end;

function TVkParamsGroupsSetCallbackSettings.AudioNew(Value: Boolean): integer;
begin
  Result := List.Add('audio_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostDelete(Value: Boolean): integer;
begin
  Result := List.Add('board_post_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostEdit(Value: Boolean): integer;
begin
  Result := List.Add('board_post_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostNew(Value: Boolean): integer;
begin
  Result := List.Add('board_post_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostRestore(Value: Boolean): integer;
begin
  Result := List.Add('board_post_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupChangePhoto(Value: Boolean): integer;
begin
  Result := List.Add('group_change_photo', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupChangeSettings(Value: Boolean): integer;
begin
  Result := List.Add('group_change_settings', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupId(Value: Integer): integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupJoin(Value: Boolean): integer;
begin
  Result := List.Add('group_join', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupLeave(Value: Boolean): integer;
begin
  Result := List.Add('group_leave', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupOfficersEdit(Value: Boolean): integer;
begin
  Result := List.Add('group_officers_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.LeadFormsNew(Value: Boolean): integer;
begin
  Result := List.Add('lead_forms_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.LikeAdd(Value: Boolean): integer;
begin
  Result := List.Add('like_add', Value);
end;

function TVkParamsGroupsSetCallbackSettings.LikeRemove(Value: Boolean): integer;
begin
  Result := List.Add('like_remove', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentDelete(Value: Boolean): integer;
begin
  Result := List.Add('market_comment_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentEdit(Value: Boolean): integer;
begin
  Result := List.Add('market_comment_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentNew(Value: Boolean): integer;
begin
  Result := List.Add('market_comment_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentRestore(Value: Boolean): integer;
begin
  Result := List.Add('market_comment_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageAllow(Value: Boolean): integer;
begin
  Result := List.Add('message_allow', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageDeny(Value: Boolean): integer;
begin
  Result := List.Add('message_deny', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageEdit(Value: Boolean): integer;
begin
  Result := List.Add('message_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageNew(Value: Boolean): integer;
begin
  Result := List.Add('message_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageReply(Value: Boolean): integer;
begin
  Result := List.Add('message_reply', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageTypingState(Value: Boolean): integer;
begin
  Result := List.Add('message_typing_state', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentDelete(Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentEdit(Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentNew(Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentRestore(Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoNew(Value: Boolean): integer;
begin
  Result := List.Add('post_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PollVoteNew(Value: Boolean): integer;
begin
  Result := List.Add('post_vote_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.ServerId(Value: Integer): integer;
begin
  Result := List.Add('server_id', Value);
end;

function TVkParamsGroupsSetCallbackSettings.UserBlock(Value: Boolean): integer;
begin
  Result := List.Add('user_block', Value);
end;

function TVkParamsGroupsSetCallbackSettings.UserUnblock(Value: Boolean): integer;
begin
  Result := List.Add('user_unblock', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentDelete(Value: Boolean): integer;
begin
  Result := List.Add('video_comment_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentEdit(Value: Boolean): integer;
begin
  Result := List.Add('video_comment_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentNew(Value: Boolean): integer;
begin
  Result := List.Add('video_comment_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentRestore(Value: Boolean): integer;
begin
  Result := List.Add('video_comment_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoNew(Value: Boolean): integer;
begin
  Result := List.Add('video_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallPostNew(Value: Boolean): integer;
begin
  Result := List.Add('wall_post_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyDelete(Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyEdit(Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyNew(Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyRestore(Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallRepost(Value: Boolean): integer;
begin
  Result := List.Add('wall_repost', Value);
end;


{ TVkParamsGroupsSetLongpollSettings }

function TVkParamsGroupsSetLongpollSettings.ApiVersion(Value: string): integer;
begin
  Result := List.Add('api_version', Value);
end;

function TVkParamsGroupsSetLongpollSettings.AudioNew(Value: Boolean): integer;
begin
  Result := List.Add('audio_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostDelete(Value: Boolean): integer;
begin
  Result := List.Add('board_post_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostEdit(Value: Boolean): integer;
begin
  Result := List.Add('board_post_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostNew(Value: Boolean): integer;
begin
  Result := List.Add('board_post_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostRestore(Value: Boolean): integer;
begin
  Result := List.Add('board_post_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupChangePhoto(Value: Boolean): integer;
begin
  Result := List.Add('group_change_photo', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupChangeSettings(Value: Boolean): integer;
begin
  Result := List.Add('group_change_settings', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupId(Value: Integer): integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupJoin(Value: Boolean): integer;
begin
  Result := List.Add('group_join', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupLeave(Value: Boolean): integer;
begin
  Result := List.Add('group_leave', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupOfficersEdit(Value: Boolean): integer;
begin
  Result := List.Add('group_officers_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.LeadFormsNew(Value: Boolean): integer;
begin
  Result := List.Add('lead_forms_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.LikeAdd(Value: Boolean): integer;
begin
  Result := List.Add('like_add', Value);
end;

function TVkParamsGroupsSetLongpollSettings.LikeRemove(Value: Boolean): integer;
begin
  Result := List.Add('like_remove', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentDelete(Value: Boolean): integer;
begin
  Result := List.Add('market_comment_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentEdit(Value: Boolean): integer;
begin
  Result := List.Add('market_comment_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentNew(Value: Boolean): integer;
begin
  Result := List.Add('market_comment_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentRestore(Value: Boolean): integer;
begin
  Result := List.Add('market_comment_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageAllow(Value: Boolean): integer;
begin
  Result := List.Add('message_allow', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageDeny(Value: Boolean): integer;
begin
  Result := List.Add('message_deny', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageEdit(Value: Boolean): integer;
begin
  Result := List.Add('message_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageNew(Value: Boolean): integer;
begin
  Result := List.Add('message_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageReply(Value: Boolean): integer;
begin
  Result := List.Add('message_reply', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageTypingState(Value: Boolean): integer;
begin
  Result := List.Add('message_typing_state', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentDelete(Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentEdit(Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentNew(Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentRestore(Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoNew(Value: Boolean): integer;
begin
  Result := List.Add('post_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PollVoteNew(Value: Boolean): integer;
begin
  Result := List.Add('post_vote_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.UserBlock(Value: Boolean): integer;
begin
  Result := List.Add('user_block', Value);
end;

function TVkParamsGroupsSetLongpollSettings.UserUnblock(Value: Boolean): integer;
begin
  Result := List.Add('user_unblock', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentDelete(Value: Boolean): integer;
begin
  Result := List.Add('video_comment_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentEdit(Value: Boolean): integer;
begin
  Result := List.Add('video_comment_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentNew(Value: Boolean): integer;
begin
  Result := List.Add('video_comment_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentRestore(Value: Boolean): integer;
begin
  Result := List.Add('video_comment_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoNew(Value: Boolean): integer;
begin
  Result := List.Add('video_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallPostNew(Value: Boolean): integer;
begin
  Result := List.Add('wall_post_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyDelete(Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyEdit(Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyNew(Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyRestore(Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallRepost(Value: Boolean): integer;
begin
  Result := List.Add('wall_repost', Value);
end;

{ TVkParamsGroupsSetSettings }

function TVkParamsGroupsSetSettings.BotsAddToChat(Value: Boolean): integer;
begin
  Result := List.Add('bots_add_to_chat', Value);
end;

function TVkParamsGroupsSetSettings.BotsCapabilities(Value: Boolean): integer;
begin
  Result := List.Add('bots_capabilities', Value);
end;

function TVkParamsGroupsSetSettings.BotsStartButton(Value: Boolean): integer;
begin
  Result := List.Add('bots_start_button', Value);
end;

function TVkParamsGroupsSetSettings.GroupId(Value: Integer): integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsSetSettings.Messages(Value: Boolean): integer;
begin
  Result := List.Add('messages', Value);
end;

{ TVkGroupTagActHelper }

function TVkGroupTagActHelper.ToString: string;
begin
  case Self of
    gtaBind:
      Result := 'bind';
    gtaUnbind:
      Result := 'unbind';
  else
    Result := 'bind';
  end;
end;

end.

