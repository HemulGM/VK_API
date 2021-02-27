unit VK.Groups;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, REST.Json, System.Json, VK.Controller, VK.Types,
  VK.Entity.Profile, System.Classes, VK.Entity.Group, VK.CommonUtils, VK.Entity.Common, VK.Entity.Group.TimeTable,
  VK.Entity.Group.Ban, VK.Entity.Group.CallBackServer, VK.Entity.Group.CallbackSettings, VK.Entity.Group.Categories,
  VK.Entity.Longpoll, VK.Entity.Group.LongpollSettings, VK.Entity.GroupSettings, VK.Entity.Group.TokenPermissions,
  VK.Entity.Common.List, VK.Entity.Group.Invites, VK.Entity.Group.Status;

type
  TVkParamsGroupsGetMembers = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: Integer): Integer; overload;
    function GroupId(const Value: string): Integer; overload;
    function Filter(const Value: TVkGroupMembersFilter): Integer;
    function Fields(const Value: TVkProfileFields): Integer;
    function Count(const Value: Integer = 1000): Integer;
    function Offset(const Value: Integer = 0): Integer;
    function Sort(const Value: TVkSortIdTime): Integer;
  end;

  TVkParamsGroupsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя.
    /// </summary>
    function UserId(const Value: Integer): Integer;
    function Filter(const Value: TVkGroupFilters): Integer; overload;
    function Fields(const Value: TVkGroupFields): Integer; overload;
    function Count(const Value: Integer = 1000): Integer;
    function Offset(const Value: Integer = 0): Integer;
  end;

  TVkParamsGroupsIsMember = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: Integer): Integer; overload;
    /// <summary>
    /// Короткое имя сообщества.
    /// </summary>
    function GroupId(const Value: string): Integer; overload;
    /// <summary>
    /// True — вернуть ответ в расширенной форме. По умолчанию — False.
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// Идентификатор пользователя.
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификаторы пользователей, не более 500.
    /// </summary>
    function UserIds(const Value: TIdList): Integer;
  end;

  TVkParamsGroupsAddAddress = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    function Title(const Value: string): Integer;
    function Address(const Value: string): Integer;
    function AdditionalAddress(const Value: string): Integer;
    function CountryId(const Value: Integer): Integer;
    function CityId(const Value: Integer): Integer;
    function MetroId(const Value: Integer): Integer;
    function Latitude(const Value: Extended): Integer;
    function Longitude(const Value: Extended): Integer;
    function Phone(const Value: string): Integer;
    function WorkInfoStatus(const Value: TVkWorkInfoStatus): Integer;
    function Timetable(const Value: TVkTimeTable; FreeObject: Boolean = True): Integer;
    function IsMainAddress(const Value: Boolean): Integer;
  end;

  TVkParamsGroupsEditAddress = TVkParamsGroupsAddAddress;

  TVkParamsGroupsBan = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    function OwnerId(const Value: Integer): Integer;
    function EndDate(const Value: TDateTime): Integer;
    function Reason(const Value: TVkUserBlockReason = TVkUserBlockReason.Other): Integer;
    function Comment(const Value: string): Integer;
    function CommentVisible(const Value: Boolean = False): Integer;
  end;

  TVkParamsGroupsCreate = record
    List: TParams;
    /// <summary>
    /// Название сообщества
    /// </summary>
    function Title(const Value: string): Integer;
    /// <summary>
    /// Описание сообщества, (не учитывается при Type = gtPublic)
    /// </summary>
    function Description(const Value: string): Integer;
    /// <summary>
    /// Тип создаваемого сообщества
    /// </summary>
    function &Type(const Value: TVkGroupTypeCreate): Integer;
    /// <summary>
    /// Категория публичной страницы (только для Type = gtPublic).
    /// </summary>
    function PublicCategory(const Value: Integer): Integer;
    /// <summary>
    /// Вид публичной страницы (только при Type = gtPublic)
    /// </summary>
    function Subtype(const Value: Integer): Integer;
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
    function GroupId(const Value: Integer): Integer;
    function Title(const Value: string): Integer;
    function Description(const Value: string): Integer;
    function ScreenName(const Value: string): Integer;
    function Access(const Value: TVkGroupAccess): Integer;
    function Website(const Value: string): Integer;
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
    function Subject(const Value: TVkGroupSubjectType): Integer;
    function Email(const Value: string): Integer;
    function Phone(const Value: string): Integer;
    function Rss(const Value: string): Integer;
    function EventStartDate(const Value: TDateTime): Integer;
    function EventFinishDate(const Value: TDateTime): Integer;
    function EventGroupId(const Value: Integer): Integer;
    function PublicCategory(const Value: Integer): Integer;
    function PublicSubcategory(const Value: Integer): Integer;
    function PublicDate(const Value: TDateTime): Integer;
    function Wall(const Value: TGroupSectionWall): Integer;
    function Topics(const Value: TGroupSectionTopics): Integer;
    function Photos(const Value: TGroupSectionPhotos): Integer;
    function Video(const Value: TGroupSectionVideo): Integer;
    function Audio(const Value: TGroupSectionAudio): Integer;
    function Links(const Value: Boolean): Integer;
    function Events(const Value: Boolean): Integer;
    function Places(const Value: Boolean): Integer;
    function Contacts(const Value: Boolean): Integer;
    function Docs(const Value: TGroupSectionDocs): Integer;
    function Wiki(const Value: TGroupSectionWiki): Integer;
    function Messages(const Value: Boolean): Integer;
    function Articles(const Value: Boolean): Integer;
    function Addresses(const Value: Boolean): Integer;
    function AgeLimits(const Value: TVkAgeLimits): Integer;
    function Market(const Value: Boolean): Integer;
    function MarketComments(const Value: Boolean): Integer;
    function MarketCountry(const Value: TIdList): Integer;
    function MarketCity(const Value: TIdList): Integer;
    function MarketCurrency(const Value: TVkCurrency): Integer;
    function MarketContact(const Value: Integer): Integer;
    function MarketWiki(const Value: Integer): Integer;
    function ObsceneFilter(const Value: Boolean): Integer;
    function ObsceneStopwords(const Value: Boolean): Integer;
    function ObsceneWords(const Value: TArrayOfString): Integer;
    function MainSection(const Value: Integer): Integer;
    function SecondarySection(const Value: Integer): Integer;
    function Country(const Value: Integer): Integer;
    function City(const Value: Integer): Integer;
  end;

  TVkParamsGroupsEditManager = record
    List: TParams;
    function GroupId(const Value: Integer): Integer;
    function UserId(const Value: Integer): Integer;
    function Role(const Value: TVkGroupRole): Integer;
    function IsContact(const Value: Boolean): Integer;
    function ContactPosition(const Value: string): Integer;
    function ContactPhone(const Value: string): Integer;
    function ContactEmail(const Value: string): Integer;
  end;

  TVkParamsGroupsGetAddresses = record
    List: TParams;
    function GroupId(const Value: Integer): Integer;
    function AddressIds(const Value: TIdList): Integer; overload;
    function AddressIds(const Value: Integer): Integer; overload;
    function Latitude(const Value: Extended): Integer;
    function Longitude(const Value: Extended): Integer;
    function Fields(const Value: TVkGroupAddressFields): Integer; overload;
    function Count(const Value: Integer = 10): Integer;
    function Offset(const Value: Integer = 0): Integer;
  end;

  TVkParamsGroupsGetBanned = record
    List: TParams;
    function GroupId(const Value: Integer): Integer;
    function Offset(const Value: Integer = 0): Integer;
    function Count(const Value: Integer = 20): Integer;
    function Fields(GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer; overload;
    function OwnerId(const Value: Integer): Integer;
  end;

  TVkParamsGroupsGetInvitedUsers = record
    List: TParams;
    function GroupId(const Value: Integer): Integer;
    function Offset(const Value: Integer = 0): Integer;
    function Count(const Value: Integer = 20): Integer;
    function Fields(const Value: TVkProfileFields = []): Integer;
    function NameCase(const Value: TVkNameCase): Integer;
  end;

  TVkParamsGroupsSearch = record
    List: TParams;
    function Query(const Value: string): Integer;
    function &Type(const Value: TVkGroupTypeCreate): Integer;
    function CountryId(const Value: Integer): Integer;
    function CityId(const Value: Integer): Integer;
    function Future(const Value: Boolean): Integer;
    function Market(const Value: Boolean): Integer;
    function Sort(const Value: TVkGroupSearchSort): Integer;
    function Offset(const Value: Integer = 0): Integer;
    function Count(const Value: Integer = 20): Integer;
  end;

  TVkParamsGroupsSetCallbackSettings = record
    List: TParams;
    function GroupId(const Value: Integer): integer;
    function ApiVersion(const Value: string): integer;
    function ServerId(const Value: Integer): integer;
    function AudioNew(const Value: Boolean): integer;
    function BoardPostDelete(const Value: Boolean): integer;
    function BoardPostEdit(const Value: Boolean): integer;
    function BoardPostNew(const Value: Boolean): integer;
    function BoardPostRestore(const Value: Boolean): integer;
    function GroupChangePhoto(const Value: Boolean): integer;
    function GroupChangeSettings(const Value: Boolean): integer;
    function GroupJoin(const Value: Boolean): integer;
    function GroupLeave(const Value: Boolean): integer;
    function GroupOfficersEdit(const Value: Boolean): integer;
    function LeadFormsNew(const Value: Boolean): integer;
    function LikeAdd(const Value: Boolean): integer;
    function LikeRemove(const Value: Boolean): integer;
    function MarketCommentDelete(const Value: Boolean): integer;
    function MarketCommentEdit(const Value: Boolean): integer;
    function MarketCommentNew(const Value: Boolean): integer;
    function MarketCommentRestore(const Value: Boolean): integer;
    function MessageAllow(const Value: Boolean): integer;
    function MessageDeny(const Value: Boolean): integer;
    function MessageEdit(const Value: Boolean): integer;
    function MessageNew(const Value: Boolean): integer;
    function MessageReply(const Value: Boolean): integer;
    function MessageTypingState(const Value: Boolean): integer;
    function PhotoCommentDelete(const Value: Boolean): integer;
    function PhotoCommentEdit(const Value: Boolean): integer;
    function PhotoCommentNew(const Value: Boolean): integer;
    function PhotoCommentRestore(const Value: Boolean): integer;
    function PhotoNew(const Value: Boolean): integer;
    function PollVoteNew(const Value: Boolean): integer;
    function UserBlock(const Value: Boolean): integer;
    function UserUnblock(const Value: Boolean): integer;
    function VideoCommentDelete(const Value: Boolean): integer;
    function VideoCommentEdit(const Value: Boolean): integer;
    function VideoCommentNew(const Value: Boolean): integer;
    function VideoCommentRestore(const Value: Boolean): integer;
    function VideoNew(const Value: Boolean): integer;
    function WallPostNew(const Value: Boolean): integer;
    function WallReplyDelete(const Value: Boolean): integer;
    function WallReplyEdit(const Value: Boolean): integer;
    function WallReplyNew(const Value: Boolean): integer;
    function WallReplyRestore(const Value: Boolean): integer;
    function WallRepost(const Value: Boolean): integer;
  end;

  TVkParamsGroupsSetLongpollSettings = record
    List: TParams;
    function GroupId(const Value: Integer): integer;
    function ApiVersion(const Value: string): integer;
    function AudioNew(const Value: Boolean): integer;
    function BoardPostDelete(const Value: Boolean): integer;
    function BoardPostEdit(const Value: Boolean): integer;
    function BoardPostNew(const Value: Boolean): integer;
    function BoardPostRestore(const Value: Boolean): integer;
    function GroupChangePhoto(const Value: Boolean): integer;
    function GroupChangeSettings(const Value: Boolean): integer;
    function GroupJoin(const Value: Boolean): integer;
    function GroupLeave(const Value: Boolean): integer;
    function GroupOfficersEdit(const Value: Boolean): integer;
    function LeadFormsNew(const Value: Boolean): integer;
    function LikeAdd(const Value: Boolean): integer;
    function LikeRemove(const Value: Boolean): integer;
    function MarketCommentDelete(const Value: Boolean): integer;
    function MarketCommentEdit(const Value: Boolean): integer;
    function MarketCommentNew(const Value: Boolean): integer;
    function MarketCommentRestore(const Value: Boolean): integer;
    function MessageAllow(const Value: Boolean): integer;
    function MessageDeny(const Value: Boolean): integer;
    function MessageEdit(const Value: Boolean): integer;
    function MessageNew(const Value: Boolean): integer;
    function MessageReply(const Value: Boolean): integer;
    function MessageTypingState(const Value: Boolean): integer;
    function PhotoCommentDelete(const Value: Boolean): integer;
    function PhotoCommentEdit(const Value: Boolean): integer;
    function PhotoCommentNew(const Value: Boolean): integer;
    function PhotoCommentRestore(const Value: Boolean): integer;
    function PhotoNew(const Value: Boolean): integer;
    function PollVoteNew(const Value: Boolean): integer;
    function UserBlock(const Value: Boolean): integer;
    function UserUnblock(const Value: Boolean): integer;
    function VideoCommentDelete(const Value: Boolean): integer;
    function VideoCommentEdit(const Value: Boolean): integer;
    function VideoCommentNew(const Value: Boolean): integer;
    function VideoCommentRestore(const Value: Boolean): integer;
    function VideoNew(const Value: Boolean): integer;
    function WallPostNew(const Value: Boolean): integer;
    function WallReplyDelete(const Value: Boolean): integer;
    function WallReplyEdit(const Value: Boolean): integer;
    function WallReplyNew(const Value: Boolean): integer;
    function WallReplyRestore(const Value: Boolean): integer;
    function WallRepost(const Value: Boolean): integer;
  end;

  TVkParamsGroupsSetSettings = record
    List: TParams;
    function GroupId(const Value: Integer): integer;
    function Messages(const Value: Boolean): integer;
    function BotsCapabilities(const Value: Boolean): integer;
    function BotsStartButton(const Value: Boolean): integer;
    function BotsAddToChat(const Value: Boolean): integer;
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
    function Get(var Items: TVkIdList; Params: TVkParamsGroupsGet): Boolean; overload;
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
    ///  Возвращает информацию о заданном сообществе или о нескольких сообществах.
    /// </summary>
    function GetById(var Items: TVkGroups; GroupId: string; Fields: TVkGroupFields = []): Boolean; overload;
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
    function GetRequests(var Items: TVkProfiles; GroupId: Integer; Fields: TVkProfileFields = [TVkProfileField.Domain]; Count: Integer = 20; Offset: Integer = 0): Boolean; overload;
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
  Result := Handler.Execute('groups.addAddress', Params).GetObject(Item);
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
    GetObject(Item);
end;

function TGroupsController.ApproveRequest(GroupId, UserId: integer): Boolean;
begin
  Result := Handler.Execute('groups.approveRequest', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString]]).
    ResponseIsTrue;
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
  Result := Handler.Execute('groups.create', Params).GetObject(Item);
end;

function TGroupsController.Ban(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.ban', Params).ResponseIsTrue;
end;

function TGroupsController.DeleteAddress(GroupId, AddressId: integer): Boolean;
begin
  Result := Handler.Execute('groups.deleteAddress', [
    ['group_id', GroupId.ToString],
    ['address_id', AddressId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.DeleteCallbackServer(GroupId, ServerId: integer): Boolean;
begin
  Result := Handler.Execute('groups.deleteCallbackServer', [
    ['group_id', GroupId.ToString],
    ['server_id', ServerId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.DeleteLink(GroupId, LinkId: integer): Boolean;
begin
  Result := Handler.Execute('groups.deleteLink', [
    ['group_id', GroupId.ToString],
    ['link_id', LinkId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.DisableOnline(GroupId: Cardinal): Boolean;
begin
  Result := Handler.Execute('groups.disableOnline', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TGroupsController.Edit(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.edit', Params).ResponseIsTrue;
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
  Result := Handler.Execute('groups.editCallbackServer', [
    ['group_id', GroupId.ToString],
    ['server_id', ServerId.ToString],
    ['url', Url],
    ['title', Title],
    ['secret_key', SecretKey]]).
    ResponseIsTrue;
end;

function TGroupsController.EditLink(GroupId: integer; Link, Text: string): Boolean;
begin
  Result := Handler.Execute('groups.editLink', [
    ['GroupId', GroupId.ToString],
    ['link', Link],
    ['text', Text]]).
    ResponseIsTrue;
end;

function TGroupsController.EditManager(Params: TVkParamsGroupsEditManager): Boolean;
begin
  Result := EditManager(Params.List);
end;

function TGroupsController.EditManager(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.editManager', Params).ResponseIsTrue;
end;

function TGroupsController.EditAddress(var Item: TVkGroupAddress; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.editAddress', Params).ResponseIsTrue;
end;

function TGroupsController.EnableOnline(GroupId: Cardinal): Boolean;
begin
  Result := Handler.Execute('groups.enableOnline', ['group_id', GroupId.ToString]).ResponseIsTrue;
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
  Result := Handler.Execute('groups.getBanned', Params.List).GetObject(Items);
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
    GetObject(Items);
end;

function TGroupsController.GetCallbackSettings(var Items: TVkCallbackSettings; GroupId, ServerId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getCallbackSettings', [
    ['group_id', GroupId.ToString],
    ['server_id', ServerId.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetCatalog(var Items: TVkGroups; CategoryId, SubcategoryId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getById', [
    ['category_id', CategoryId.ToString],
    ['subcategory_id', SubcategoryId.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetCatalogInfo(var Items: TVkGroupCategories; Subcategories, Extended: Boolean): Boolean;
begin
  Result := Handler.Execute('groups.getCatalogInfo', [
    ['subcategories', BoolToString(Subcategories)],
    ['extended', BoolToString(Extended)]]).
    GetObject(Items);
end;

function TGroupsController.GetById(var Items: TVkGroups; GroupId: string; Fields: TVkGroupFields): Boolean;
begin
  Result := Handler.Execute('groups.getById', [
    ['group_ids', GroupId],
    ['fields', Fields.ToString]]).
    GetObjects(Items);
end;

function TGroupsController.GetById(var Items: TVkGroups; GroupIds: TIdList; Fields: TVkGroupFields): Boolean;
begin
  Result := Handler.Execute('groups.getById', [
    ['group_ids', GroupIds.ToString],
    ['fields', Fields.ToString]]).
    GetObjects(Items);
end;

function TGroupsController.GetAddresses(var Item: TVkGroupAddresses; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.getAddresses', Params).GetObject(Item);
end;

function TGroupsController.Get(var Items: TVkGroups; Params: TParams): Boolean;
begin
  Params.Add('extended', True);
  Result := Handler.Execute('groups.get', Params).GetObject(Items);
end;

function TGroupsController.Get(var Items: TVkIdList; Params: TVkParamsGroupsGet): Boolean;
begin
  Params.List.Add('extended', False);
  Result := Handler.Execute('groups.get', Params.List).GetObject(Items);
end;

function TGroupsController.GetInvitedUsers(var Items: TVkProfiles; Params: TVkParamsGroupsGetInvitedUsers): Boolean;
begin
  Result := Handler.Execute('groups.getInvitedUsers', Params.List).GetObject(Items);
end;

function TGroupsController.GetInvites(var Items: TVkInvitesGroups; Extended: Boolean; Count, Offset: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getInvites', [
    ['extended', BoolToString(Extended)],
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetLongPollServer(var Item: TVkLongpollData; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getLongPollServer', ['group_id', GroupId.ToString]).GetObject(Item);
end;

function TGroupsController.GetLongPollSettings(var Item: TVkLongpollSettings; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getLongPollSettings', ['group_id', GroupId.ToString]).GetObject(Item);
end;

function TGroupsController.GetMembers(var Items: TVkProfiles; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  if not Params.List.KeyExists('fields') then
    Params.Fields([TVkProfileField.Domain]);
  Result := GetMembers(Items, Params.List);
end;

function TGroupsController.GetMembers(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.getMembers', Params).GetObject(Items);
end;

function TGroupsController.GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  Params.Fields([]);
  Result := Handler.Execute('groups.getMembers', Params.List).GetObject(Items);
end;

function TGroupsController.GetOnlineStatus(var Value: TVkGroupStatus; GroupId: Cardinal): Boolean;
begin
  Result := Handler.Execute('groups.getOnlineStatus', ['group_id', GroupId.ToString]).GetObject(Value);
end;

function TGroupsController.GetRequests(var Items: TVkProfiles; GroupId: Integer; Fields: TVkProfileFields; Count, Offset: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  if Fields = [] then
    Fields := [TVkProfileField.Domain];
  Params.Add('fields', Fields.ToString);
  Result := Handler.Execute('groups.getRequests', Params).GetObject(Items);
end;

function TGroupsController.GetRequestsIds(var Items: TVkIdList; GroupId, Count, Offset: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  Result := Handler.Execute('groups.getRequests', Params).GetObject(Items);
end;

function TGroupsController.GetSettings(var Item: TVkGroupSettings; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.getSettings', ['group_id', GroupId.ToString]).GetObject(Item);
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
  Result := Handler.Execute('groups.getTokenPermissions').GetObject(Items);
end;

function TGroupsController.Invite(GroupId, UserId: integer): Boolean;
begin
  Result := Handler.Execute('groups.invite', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString]]).
    ResponseIsTrue;
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
  Result := Handler.Execute('groups.join', Params).ResponseIsTrue;
end;

function TGroupsController.Leave(GroupId: integer): Boolean;
begin
  Result := Handler.Execute('groups.leave', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TGroupsController.RemoveUser(GroupId, UserId: integer): Boolean;
begin
  Result := Handler.Execute('groups.removeUser', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.ReorderLink(GroupId, LinkId, After: Integer): Boolean;
begin
  Result := Handler.Execute('groups.reorderLink', [
    ['group_id', GroupId.ToString],
    ['link_id', LinkId.ToString],
    ['after', After.ToString]]).
    ResponseIsTrue;
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
  Result := Handler.Execute('groups.setCallbackSettings', Params).ResponseIsTrue;
end;

function TGroupsController.SetLongPollSettings(Params: TVkParamsGroupsSetLongpollSettings): Boolean;
begin
  Result := SetLongPollSettings(Params.List);
end;

function TGroupsController.SetSettings(Params: TVkParamsGroupsSetSettings): Boolean;
begin
  Result := Handler.Execute('groups.setSettings', Params.List).ResponseIsTrue;
end;

function TGroupsController.SetUserNote(GroupId, UserId: Integer; Note: TVkNoteText): Boolean;
begin
  Result := Handler.Execute('groups.setLongPollSettings', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString],
    ['note', string(Note)]]).
    ResponseIsTrue;
end;

function TGroupsController.TagAdd(GroupId: Integer; TagName: string; TagColor: TVkGroupTagColor): Boolean;
begin
  Result := Handler.Execute('groups.tagAdd', [
    ['group_id', GroupId.ToString],
    ['tag_name', TagName],
    ['tag_color', TagColor]]).
    ResponseIsTrue;
end;

function TGroupsController.TagBind(GroupId, TagId, UserId: Integer; Act: TVkGroupTagAct): Boolean;
begin
  Result := Handler.Execute('groups.tagBind', [
    ['group_id', GroupId.ToString],
    ['tag_id', TagId.ToString],
    ['user_id', UserId.ToString],
    ['act', Act.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.TagDelete(GroupId, TagId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.tagDelete', [
    ['group_id', GroupId.ToString],
    ['tag_id', TagId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.TagUpdate(GroupId, TagId: Integer; TagName: string): Boolean;
begin
  Result := Handler.Execute('groups.tagUpdate', [
    ['group_id', GroupId.ToString],
    ['tag_name', TagName],
    ['tag_id', TagId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.Unban(GroupId, OwnerId: Integer): Boolean;
begin
  Result := Handler.Execute('groups.unban', [
    ['group_id', GroupId.ToString],
    ['owner_id', OwnerId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.SetLongPollSettings(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.setLongPollSettings', Params).ResponseIsTrue;
end;

function TGroupsController.Search(var Items: TVkGroups; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.search', Params).GetObject(Items);
end;

function TGroupsController.IsMember(var Items: TVkGroupMemberStates; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.isMember', Params).GetObjects(Items);
end;

{ TVkGetMembersParams }

function TVkParamsGroupsGetMembers.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetMembers.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGetMembers.Filter(const Value: TVkGroupMembersFilter): Integer;
begin
  Result := List.Add('filter', Value.ToString);
end;

function TVkParamsGroupsGetMembers.GroupId(const Value: string): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetMembers.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetMembers.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGetMembers.Sort(const Value: TVkSortIdTime): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

{ TVkGroupsGetParams }

function TVkParamsGroupsGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGet.Fields(const Value: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGet.Filter(const Value: TVkGroupFilters): Integer;
begin
  Result := List.Add('filter', Value.ToString);
end;

function TVkParamsGroupsGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGet.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsGroupsIsMember }

function TVkParamsGroupsIsMember.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsGroupsIsMember.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsIsMember.GroupId(const Value: string): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsIsMember.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

function TVkParamsGroupsIsMember.UserIds(const Value: TIdList): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsGroupsAddAddress }

function TVkParamsGroupsAddAddress.AdditionalAddress(const Value: string): Integer;
begin
  Result := List.Add('additional_address', Value);
end;

function TVkParamsGroupsAddAddress.Address(const Value: string): Integer;
begin
  Result := List.Add('address', Value);
end;

function TVkParamsGroupsAddAddress.CityId(const Value: Integer): Integer;
begin
  Result := List.Add('city_id', Value);
end;

function TVkParamsGroupsAddAddress.CountryId(const Value: Integer): Integer;
begin
  Result := List.Add('country_id', Value);
end;

function TVkParamsGroupsAddAddress.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsAddAddress.IsMainAddress(const Value: Boolean): Integer;
begin
  Result := List.Add('is_main_address', Value);
end;

function TVkParamsGroupsAddAddress.Latitude(const Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsGroupsAddAddress.Longitude(const Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsGroupsAddAddress.MetroId(const Value: Integer): Integer;
begin
  Result := List.Add('metro_id', Value);
end;

function TVkParamsGroupsAddAddress.Phone(const Value: string): Integer;
begin
  Result := List.Add('phone', Value);
end;

function TVkParamsGroupsAddAddress.Timetable(const Value: TVkTimeTable; FreeObject: Boolean): Integer;
begin
  Result := List.Add('timetable', Value);
  if FreeObject then
    Value.Free;
end;

function TVkParamsGroupsAddAddress.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsGroupsAddAddress.WorkInfoStatus(const Value: TVkWorkInfoStatus): Integer;
begin
  Result := List.Add('work_info_status', Value.ToString);
end;

{ TVkParamsGroupsBan }

function TVkParamsGroupsBan.Comment(const Value: string): Integer;
begin
  Result := List.Add('comment', Value);
end;

function TVkParamsGroupsBan.CommentVisible(const Value: Boolean): Integer;
begin
  Result := List.Add('comment_visible', Value);
end;

function TVkParamsGroupsBan.EndDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_date', Value);
end;

function TVkParamsGroupsBan.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsBan.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsGroupsBan.Reason(const Value: TVkUserBlockReason): Integer;
begin
  Result := List.Add('reason', Ord(Value));
end;

{ TVkParamsGroupsCreate }

function TVkParamsGroupsCreate.Description(const Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsGroupsCreate.PublicCategory(const Value: Integer): Integer;
begin
  Result := List.Add('public_category', Value);
end;

function TVkParamsGroupsCreate.Subtype(const Value: Integer): Integer;
begin
  Result := List.Add('subtype', Value);
end;

function TVkParamsGroupsCreate.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsGroupsCreate.&Type(const Value: TVkGroupTypeCreate): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

{ TVkParamsGroupsEdit }

function TVkParamsGroupsEdit.Access(const Value: TVkGroupAccess): Integer;
begin
  Result := List.Add('access', Ord(Value));
end;

function TVkParamsGroupsEdit.Addresses(const Value: Boolean): Integer;
begin
  Result := List.Add('addresses', Value);
end;

function TVkParamsGroupsEdit.AgeLimits(const Value: TVkAgeLimits): Integer;
begin
  Result := List.Add('age_limits', Ord(Value));
end;

function TVkParamsGroupsEdit.Articles(const Value: Boolean): Integer;
begin
  Result := List.Add('articles', Value);
end;

function TVkParamsGroupsEdit.Audio(const Value: TGroupSectionAudio): Integer;
begin
  Result := List.Add('audio', Value);
end;

function TVkParamsGroupsEdit.City(const Value: Integer): Integer;
begin
  Result := List.Add('city', Value);
end;

function TVkParamsGroupsEdit.Contacts(const Value: Boolean): Integer;
begin
  Result := List.Add('contacts', Value);
end;

function TVkParamsGroupsEdit.Country(const Value: Integer): Integer;
begin
  Result := List.Add('country', Value);
end;

function TVkParamsGroupsEdit.Description(const Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsGroupsEdit.Docs(const Value: TGroupSectionDocs): Integer;
begin
  Result := List.Add('docs', Value);
end;

function TVkParamsGroupsEdit.Email(const Value: string): Integer;
begin
  Result := List.Add('email', Value);
end;

function TVkParamsGroupsEdit.EventFinishDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('event_finish_date', Value);
end;

function TVkParamsGroupsEdit.EventGroupId(const Value: Integer): Integer;
begin
  Result := List.Add('event_group_id', Value);
end;

function TVkParamsGroupsEdit.Events(const Value: Boolean): Integer;
begin
  Result := List.Add('events', Value);
end;

function TVkParamsGroupsEdit.EventStartDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('event_start_date', Value);
end;

function TVkParamsGroupsEdit.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsEdit.Links(const Value: Boolean): Integer;
begin
  Result := List.Add('links', Value);
end;

function TVkParamsGroupsEdit.MainSection(const Value: Integer): Integer;
begin
  Result := List.Add('main_section', Value);
end;

function TVkParamsGroupsEdit.Market(const Value: Boolean): Integer;
begin
  Result := List.Add('market', Value);
end;

function TVkParamsGroupsEdit.MarketCity(const Value: TIdList): Integer;
begin
  Result := List.Add('market_city', Value);
end;

function TVkParamsGroupsEdit.MarketComments(const Value: Boolean): Integer;
begin
  Result := List.Add('market_comments', Value);
end;

function TVkParamsGroupsEdit.MarketContact(const Value: Integer): Integer;
begin
  Result := List.Add('market_contact', Value);
end;

function TVkParamsGroupsEdit.MarketCountry(const Value: TIdList): Integer;
begin
  Result := List.Add('market_country', Value);
end;

function TVkParamsGroupsEdit.MarketCurrency(const Value: TVkCurrency): Integer;
begin
  Result := List.Add('market_currency', Value.ToConst);
end;

function TVkParamsGroupsEdit.MarketWiki(const Value: Integer): Integer;
begin
  Result := List.Add('market_wiki', Value);
end;

function TVkParamsGroupsEdit.Messages(const Value: Boolean): Integer;
begin
  Result := List.Add('messages', Value);
end;

function TVkParamsGroupsEdit.ObsceneFilter(const Value: Boolean): Integer;
begin
  Result := List.Add('obscene_filter', Value);
end;

function TVkParamsGroupsEdit.ObsceneStopwords(const Value: Boolean): Integer;
begin
  Result := List.Add('obscene_stopwords', Value);
end;

function TVkParamsGroupsEdit.ObsceneWords(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('obscene_words', Value);
end;

function TVkParamsGroupsEdit.Phone(const Value: string): Integer;
begin
  Result := List.Add('phone', Value);
end;

function TVkParamsGroupsEdit.Photos(const Value: TGroupSectionPhotos): Integer;
begin
  Result := List.Add('photos', Value);
end;

function TVkParamsGroupsEdit.Places(const Value: Boolean): Integer;
begin
  Result := List.Add('places', Value);
end;

function TVkParamsGroupsEdit.PublicCategory(const Value: Integer): Integer;
begin
  Result := List.Add('public_category', Value);
end;

function TVkParamsGroupsEdit.PublicDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('public_date', Value, 'DD.MM.YYYY');
end;

function TVkParamsGroupsEdit.PublicSubcategory(const Value: Integer): Integer;
begin
  Result := List.Add('public_subcategory', Value);
end;

function TVkParamsGroupsEdit.Rss(const Value: string): Integer;
begin
  Result := List.Add('rss', Value);
end;

function TVkParamsGroupsEdit.ScreenName(const Value: string): Integer;
begin
  Result := List.Add('screen_name', Value);
end;

function TVkParamsGroupsEdit.SecondarySection(const Value: Integer): Integer;
begin
  Result := List.Add('secondary_section', Value);
end;

function TVkParamsGroupsEdit.Subject(const Value: TVkGroupSubjectType): Integer;
begin
  Result := List.Add('subject', Ord(Value));
end;

function TVkParamsGroupsEdit.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsGroupsEdit.Topics(const Value: TGroupSectionTopics): Integer;
begin
  Result := List.Add('topics', Value);
end;

function TVkParamsGroupsEdit.Video(const Value: TGroupSectionVideo): Integer;
begin
  Result := List.Add('video', Value);
end;

function TVkParamsGroupsEdit.Wall(const Value: TGroupSectionWall): Integer;
begin
  Result := List.Add('wall', Value);
end;

function TVkParamsGroupsEdit.Website(const Value: string): Integer;
begin
  Result := List.Add('website', Value);
end;

function TVkParamsGroupsEdit.Wiki(const Value: TGroupSectionWiki): Integer;
begin
  Result := List.Add('wiki', Value);
end;

{ TVkParamsGroupsEditManager }

function TVkParamsGroupsEditManager.ContactEmail(const Value: string): Integer;
begin
  Result := List.Add('contact_email', Value);
end;

function TVkParamsGroupsEditManager.ContactPhone(const Value: string): Integer;
begin
  Result := List.Add('contact_phone', Value);
end;

function TVkParamsGroupsEditManager.ContactPosition(const Value: string): Integer;
begin
  Result := List.Add('contact_position', Value);
end;

function TVkParamsGroupsEditManager.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsEditManager.IsContact(const Value: Boolean): Integer;
begin
  Result := List.Add('is_contact', Value);
end;

function TVkParamsGroupsEditManager.Role(const Value: TVkGroupRole): Integer;
begin
  Result := List.Add('role', Value.ToString);
end;

function TVkParamsGroupsEditManager.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsGroupsGetAddresses }

function TVkParamsGroupsGetAddresses.AddressIds(const Value: TIdList): Integer;
begin
  Result := List.Add('address_ids', Value);
end;

function TVkParamsGroupsGetAddresses.AddressIds(const Value: Integer): Integer;
begin
  Result := List.Add('address_ids', Value);
end;

function TVkParamsGroupsGetAddresses.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetAddresses.Fields(const Value: TVkGroupAddressFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGetAddresses.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetAddresses.Latitude(const Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsGroupsGetAddresses.Longitude(const Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsGroupsGetAddresses.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

{ TVkParamsGroupsGetBanned }

function TVkParamsGroupsGetBanned.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetBanned.Fields(GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsGroupsGetBanned.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetBanned.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsGetBanned.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsGroupsGetInvitedUsers }

function TVkParamsGroupsGetInvitedUsers.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsGetInvitedUsers.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsGroupsGetInvitedUsers.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsGetInvitedUsers.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

function TVkParamsGroupsGetInvitedUsers.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

{ TVkParamsGroupsSearch }

function TVkParamsGroupsSearch.CityId(const Value: Integer): Integer;
begin
  Result := List.Add('city_id', Value);
end;

function TVkParamsGroupsSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGroupsSearch.CountryId(const Value: Integer): Integer;
begin
  Result := List.Add('country_id', Value);
end;

function TVkParamsGroupsSearch.Future(const Value: Boolean): Integer;
begin
  Result := List.Add('future', Value);
end;

function TVkParamsGroupsSearch.Market(const Value: Boolean): Integer;
begin
  Result := List.Add('matket', Value);
end;

function TVkParamsGroupsSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGroupsSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsGroupsSearch.Sort(const Value: TVkGroupSearchSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsGroupsSearch.&Type(const Value: TVkGroupTypeCreate): Integer;
begin
  Result := List.Add('type', Value.ToString);
end;

{ TVkParamsGroupsSetCallbackSettings }

function TVkParamsGroupsSetCallbackSettings.ApiVersion(const Value: string): integer;
begin
  Result := List.Add('api_version', Value);
end;

function TVkParamsGroupsSetCallbackSettings.AudioNew(const Value: Boolean): integer;
begin
  Result := List.Add('audio_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostDelete(const Value: Boolean): integer;
begin
  Result := List.Add('board_post_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostEdit(const Value: Boolean): integer;
begin
  Result := List.Add('board_post_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostNew(const Value: Boolean): integer;
begin
  Result := List.Add('board_post_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostRestore(const Value: Boolean): integer;
begin
  Result := List.Add('board_post_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupChangePhoto(const Value: Boolean): integer;
begin
  Result := List.Add('group_change_photo', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupChangeSettings(const Value: Boolean): integer;
begin
  Result := List.Add('group_change_settings', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupId(const Value: Integer): integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupJoin(const Value: Boolean): integer;
begin
  Result := List.Add('group_join', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupLeave(const Value: Boolean): integer;
begin
  Result := List.Add('group_leave', Value);
end;

function TVkParamsGroupsSetCallbackSettings.GroupOfficersEdit(const Value: Boolean): integer;
begin
  Result := List.Add('group_officers_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.LeadFormsNew(const Value: Boolean): integer;
begin
  Result := List.Add('lead_forms_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.LikeAdd(const Value: Boolean): integer;
begin
  Result := List.Add('like_add', Value);
end;

function TVkParamsGroupsSetCallbackSettings.LikeRemove(const Value: Boolean): integer;
begin
  Result := List.Add('like_remove', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentDelete(const Value: Boolean): integer;
begin
  Result := List.Add('market_comment_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentEdit(const Value: Boolean): integer;
begin
  Result := List.Add('market_comment_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentNew(const Value: Boolean): integer;
begin
  Result := List.Add('market_comment_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentRestore(const Value: Boolean): integer;
begin
  Result := List.Add('market_comment_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageAllow(const Value: Boolean): integer;
begin
  Result := List.Add('message_allow', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageDeny(const Value: Boolean): integer;
begin
  Result := List.Add('message_deny', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageEdit(const Value: Boolean): integer;
begin
  Result := List.Add('message_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageNew(const Value: Boolean): integer;
begin
  Result := List.Add('message_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageReply(const Value: Boolean): integer;
begin
  Result := List.Add('message_reply', Value);
end;

function TVkParamsGroupsSetCallbackSettings.MessageTypingState(const Value: Boolean): integer;
begin
  Result := List.Add('message_typing_state', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentDelete(const Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentEdit(const Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentNew(const Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentRestore(const Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PhotoNew(const Value: Boolean): integer;
begin
  Result := List.Add('post_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.PollVoteNew(const Value: Boolean): integer;
begin
  Result := List.Add('post_vote_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.ServerId(const Value: Integer): integer;
begin
  Result := List.Add('server_id', Value);
end;

function TVkParamsGroupsSetCallbackSettings.UserBlock(const Value: Boolean): integer;
begin
  Result := List.Add('user_block', Value);
end;

function TVkParamsGroupsSetCallbackSettings.UserUnblock(const Value: Boolean): integer;
begin
  Result := List.Add('user_unblock', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentDelete(const Value: Boolean): integer;
begin
  Result := List.Add('video_comment_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentEdit(const Value: Boolean): integer;
begin
  Result := List.Add('video_comment_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentNew(const Value: Boolean): integer;
begin
  Result := List.Add('video_comment_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentRestore(const Value: Boolean): integer;
begin
  Result := List.Add('video_comment_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.VideoNew(const Value: Boolean): integer;
begin
  Result := List.Add('video_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallPostNew(const Value: Boolean): integer;
begin
  Result := List.Add('wall_post_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyDelete(const Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_delete', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyEdit(const Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_edit', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyNew(const Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_new', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyRestore(const Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_restore', Value);
end;

function TVkParamsGroupsSetCallbackSettings.WallRepost(const Value: Boolean): integer;
begin
  Result := List.Add('wall_repost', Value);
end;


{ TVkParamsGroupsSetLongpollSettings }

function TVkParamsGroupsSetLongpollSettings.ApiVersion(const Value: string): integer;
begin
  Result := List.Add('api_version', Value);
end;

function TVkParamsGroupsSetLongpollSettings.AudioNew(const Value: Boolean): integer;
begin
  Result := List.Add('audio_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostDelete(const Value: Boolean): integer;
begin
  Result := List.Add('board_post_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostEdit(const Value: Boolean): integer;
begin
  Result := List.Add('board_post_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostNew(const Value: Boolean): integer;
begin
  Result := List.Add('board_post_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostRestore(const Value: Boolean): integer;
begin
  Result := List.Add('board_post_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupChangePhoto(const Value: Boolean): integer;
begin
  Result := List.Add('group_change_photo', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupChangeSettings(const Value: Boolean): integer;
begin
  Result := List.Add('group_change_settings', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupId(const Value: Integer): integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupJoin(const Value: Boolean): integer;
begin
  Result := List.Add('group_join', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupLeave(const Value: Boolean): integer;
begin
  Result := List.Add('group_leave', Value);
end;

function TVkParamsGroupsSetLongpollSettings.GroupOfficersEdit(const Value: Boolean): integer;
begin
  Result := List.Add('group_officers_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.LeadFormsNew(const Value: Boolean): integer;
begin
  Result := List.Add('lead_forms_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.LikeAdd(const Value: Boolean): integer;
begin
  Result := List.Add('like_add', Value);
end;

function TVkParamsGroupsSetLongpollSettings.LikeRemove(const Value: Boolean): integer;
begin
  Result := List.Add('like_remove', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentDelete(const Value: Boolean): integer;
begin
  Result := List.Add('market_comment_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentEdit(const Value: Boolean): integer;
begin
  Result := List.Add('market_comment_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentNew(const Value: Boolean): integer;
begin
  Result := List.Add('market_comment_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentRestore(const Value: Boolean): integer;
begin
  Result := List.Add('market_comment_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageAllow(const Value: Boolean): integer;
begin
  Result := List.Add('message_allow', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageDeny(const Value: Boolean): integer;
begin
  Result := List.Add('message_deny', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageEdit(const Value: Boolean): integer;
begin
  Result := List.Add('message_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageNew(const Value: Boolean): integer;
begin
  Result := List.Add('message_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageReply(const Value: Boolean): integer;
begin
  Result := List.Add('message_reply', Value);
end;

function TVkParamsGroupsSetLongpollSettings.MessageTypingState(const Value: Boolean): integer;
begin
  Result := List.Add('message_typing_state', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentDelete(const Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentEdit(const Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentNew(const Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentRestore(const Value: Boolean): integer;
begin
  Result := List.Add('photo_comment_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PhotoNew(const Value: Boolean): integer;
begin
  Result := List.Add('post_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.PollVoteNew(const Value: Boolean): integer;
begin
  Result := List.Add('post_vote_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.UserBlock(const Value: Boolean): integer;
begin
  Result := List.Add('user_block', Value);
end;

function TVkParamsGroupsSetLongpollSettings.UserUnblock(const Value: Boolean): integer;
begin
  Result := List.Add('user_unblock', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentDelete(const Value: Boolean): integer;
begin
  Result := List.Add('video_comment_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentEdit(const Value: Boolean): integer;
begin
  Result := List.Add('video_comment_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentNew(const Value: Boolean): integer;
begin
  Result := List.Add('video_comment_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentRestore(const Value: Boolean): integer;
begin
  Result := List.Add('video_comment_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.VideoNew(const Value: Boolean): integer;
begin
  Result := List.Add('video_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallPostNew(const Value: Boolean): integer;
begin
  Result := List.Add('wall_post_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyDelete(const Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_delete', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyEdit(const Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_edit', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyNew(const Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_new', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyRestore(const Value: Boolean): integer;
begin
  Result := List.Add('wall_reply_restore', Value);
end;

function TVkParamsGroupsSetLongpollSettings.WallRepost(const Value: Boolean): integer;
begin
  Result := List.Add('wall_repost', Value);
end;

{ TVkParamsGroupsSetSettings }

function TVkParamsGroupsSetSettings.BotsAddToChat(const Value: Boolean): integer;
begin
  Result := List.Add('bots_add_to_chat', Value);
end;

function TVkParamsGroupsSetSettings.BotsCapabilities(const Value: Boolean): integer;
begin
  Result := List.Add('bots_capabilities', Value);
end;

function TVkParamsGroupsSetSettings.BotsStartButton(const Value: Boolean): integer;
begin
  Result := List.Add('bots_start_button', Value);
end;

function TVkParamsGroupsSetSettings.GroupId(const Value: Integer): integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGroupsSetSettings.Messages(const Value: Boolean): integer;
begin
  Result := List.Add('messages', Value);
end;

end.

