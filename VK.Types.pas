unit VK.Types;

interface

{$INCLUDE include.inc}

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.Generics.Collections, VK.Entity.Common;

type
  TVkException = Exception;

  TVkParserException = class(TVkException);

  TVkHandlerException = class(TVkException);

  TVkMethodException = class(TVkException)
  private
    FCode: Integer;
  public
    property Code: Integer read FCode write FCode;
    constructor Create(const Msg: string; Code: Integer);
  end;

  TVkCaptchaException = class(TVkMethodException);

  TVkAuthException = class(TVkMethodException);

  TVkInvalidTokenException = class(TVkMethodException);

  TVkConfirmException = class(TVkMethodException);

  TVkAccessDeniedException = class(TVkMethodException);

  TVkTooManySimilarActionException = class(TVkMethodException);

  TVkUnknownMethodException = class(TVkMethodException);

  TVkExecuteErrorException = class(TVkMethodException);

  TVkLongPollServerException = class(TVkException);

  TVkLongPollServerParseException = class(TVkLongPollServerException);

  TVkLongPollServerHTTPException = class(TVkLongPollServerException);

  TVkGroupEventsException = class(TVkLongPollServerException);

  TVkUserEventsException = class(TVkLongPollServerException);

const
  //Inner VK errors
  ERROR_VK_UNKNOWN = -1;
  ERROR_VK_NOTOKEN = -2;
  ERROR_VK_INTERNAL = -3;
  ERROR_VK_NETWORK = -4;
  ERROR_VK_PARSE = -5;
  ERROR_VK_AUTH = -6;
  ERROR_VK_LONGPOLL = -7;

  //Message Flags
  MF_UNREAD = 1;
  MF_OUTBOX = 2;
  MF_REPLIED = 4;
  MF_IMPORTANT = 8;
  MF_CHAT = 16;
  MF_FRIENDS = 32;
  MF_SPAM = 64;
  MF_DELЕTЕD = 128;
  MF_FIXED = 256;
  MF_MEDIA = 512;
  MF_UNKNOWN_1 = 1024;
  MF_UNKNOWN_2 = 2048;
  MF_UNKNOWN_3 = 4096;
  MF_UNREAD_MULTICHAT = 8192;
  MF_UNKNOWN_4 = 16384;
  MF_UNKNOWN_5 = 32768;
  MF_HIDDEN = 65536;
  MF_DELETE_FOR_ALL = 131072;
  MF_NOT_DELIVERED = 262144;
  MF_UNKNOWN_6 = 524288;
  MF_UNKNOWN_7 = 1048576;
  MF_UNKNOWN_8 = 2097152;
  MF_UNKNOWN_9 = 4194304;

  //Права доступа для токена пользователя
  PERM_NOTIFY = 1;
  PERM_FRIENDS = 2;
  PERM_PHOTOS = 4;
  PERM_AUDIO = 8;
  PERM_VIDEO = 16;
  //PERM_UNKNOWN1 = 32;
  PERM_STORIES = 64;
  PERM_PAGES = 128;
  PERM_LINK = 256;
  //PERM_UNKNOWN2 = 512;
  PERM_STATUS = 1024;
  PERM_NOTES = 2048;
  PERM_MESSAGES = 4096;
  PERM_WALL = 8192;
  //PERM_UNKNOWN3 = 16384;
  PERM_ADS = 32768;
  PERM_OFFLINE = 65536;
  PERM_DOCS = 131072;
  PERM_GROUPS = 262144;
  PERM_NOTIFICATIONS = 524288;
  PERM_STATS = 1048576;
  //PERM_UNKNOWN4 = 2097152;
  PERM_EMAIL = 4194304;
  //PERM_UNKNOWN5 = 8388608;
  //PERM_UNKNOWN6 = 16777216;
  //PERM_UNKNOWN7 = 33554432;
  //PERM_UNKNOWN8 = 67108864;
  PERM_MARKET = 134217728;

  //Права доступа для токена сообщества
  PERM_G_STORIES = 1;
  PERM_G_PHOTOS = 4;
  PERM_G_APP_WIDGET = 64;
  PERM_G_MESSAGES = 4096;
  PERM_G_DOCS = 131072;
  PERM_G_MANAGE = 262144;

  //Audio Genres
  AG_NONE = 0;
  AG_ROCK = 1;
  AG_POP = 2;
  AG_RAPANDHIPHOP = 3;
  AG_EASYLISTENING = 4;
  AG_HOUSEANDDANCE = 5;
  AG_INSTRUMENTAL = 6;
  AG_METAL = 7;
  AG_ALTERNATIVE = 21;
  AG_DUBSTEP = 8;
  AG_JAZZANDBLUES = 1001;
  AG_DRUMANDBASS = 10;
  AG_TRANCE = 11;
  AG_CHANSON = 12;
  AG_ETHNIC = 13;
  AG_ACOUSTICANDVOCAL = 14;
  AG_REGGAE = 15;
  AG_CLASSICAL = 16;
  AG_INDIEPOP = 17;
  AG_SPEECH = 19;
  AG_ELECTROPOPANDDISCO = 22;
  AG_OTHER = 18;

  //Group Dialog Flags
  GR_IMPORTANT = 1;
  GR_UNANSWERED = 2;

  //
  VK_CHAT_ID_START = 2000000000;
  VK_GROUP_ID_START = 1000000000;

type
  {$IFDEF OLD_VERSION}
  TArrayOfString = array of string;

  {$ELSE}

  TArrayOfString = TArray<string>;
  {$ENDIF}

  TArrayOfStringHelper = record helper for TArrayOfString
    function ToString: string; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function ToJson: string; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(const Value: string): Integer; {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure Delete(const Value: string); {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure Assign(Source: TStrings); overload;
    function IsEmpty: Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Length: Integer; {$IFNDEF DEBUG} inline; {$ENDIF}
    function IndexOf(const Value: string): Integer; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Contains(const Value: string): Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
  end;

  {$IFDEF OLD_VERSION}
  TArrayOfInteger = array of Int64;

  {$ELSE}

  TArrayOfInteger = TArray<Int64>;
  {$ENDIF}

  TArrayOfIntegerHelper = record helper for TArrayOfInteger
    function ToString: string; overload;  {$IFNDEF DEBUG} inline; {$ENDIF}
    function ToJson: string; overload;  {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Value: Int64): Integer;  {$IFNDEF DEBUG} inline; {$ENDIF}
    procedure Delete(const Value: Int64);  {$IFNDEF DEBUG} inline; {$ENDIF}
    function IsEmpty: Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Length: Integer; {$IFNDEF DEBUG} inline; {$ENDIF}
    function IndexOf(const Value: Int64): Integer; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Contains(const Value: Int64): Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
  end;

  TFields = TArrayOfString;

  TParam = TArrayOfString;

  TIdList = TArrayOfInteger;

  {$IFDEF OLD_VERSION}
  TParams = array of TParam;
  {$ELSE}

  TParams = TArray<TParam>;
  {$ENDIF}

type
  TWalkMethod = reference to function(Offset: Integer; var Cancel: Boolean): Integer;

  TVkValidationType = (Unknown, SMS, App);

  TVkValidationTypeHelper = record helper for TVkValidationType
    class function FromString(const Value: string): TVkValidationType; static;
  end;

  TOn2FA = reference to function(const ValidationType: TVkValidationType; var Code: string; var Remember: Boolean): Boolean;

  TAttachmentKind = (Media, Link, Tel);

  /// <summary>
  /// Типы вложений
  /// </summary>
  TVkAttachmentType = (Unknown, Photo, Video, Audio, Doc, Link, Market,       //
    MarketAlbum, Wall, WallReply, Sticker, Gift, Call, AudioMessage,          //
    PostedPhoto, Graffiti, Note, App, Poll, Page, Album, PhotosList,          //
    PrettyCards, Event, MoneyTransfer);

  TVkAttachmentTypeHelper = record helper for TVkAttachmentType
    function ToString: string; inline;
    class function Create(Value: string): TVkAttachmentType; static;
  end;

  TAttachment = record
    OwnerId, Id: Int64;
    AccessKey: string;
    &Type: TVkAttachmentType;
    Kind: TAttachmentKind;
    Value: string;
  public
    class function Photo(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Video(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Audio(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Doc(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Link(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Market(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function MarketAlbum(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Wall(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function WallReply(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Sticker(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Poll(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Gift(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Album(OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class function Create(&Type: TVkAttachmentType; OwnerId, Id: Integer; const AccessKey: string = ''): TAttachment; static;
    class operator Implicit(const Value: string): TAttachment;
    class operator Implicit(const Value: TAttachment): string;
    function ToString: string; inline;
    function IsEmpty: Boolean; inline;
  end;

  IAttachment = interface(IInterface)
    function ToAttachment: TAttachment;
  end;

  TAttachmentArray = TArray<TAttachment>;

  TAttachmentArrayHelper = record helper for TAttachmentArray
    function ToStrings: TArray<string>; inline;
    function IsEmpty: Boolean; inline;
    function GetByKind(const Kind: TAttachmentKind; var Attachment: TAttachment): Boolean; {$IFDEF RELEASE} inline; {$ENDIF}
    function GetByType(const&Type: TVkAttachmentType; var Attachment: TAttachment): Boolean; {$IFDEF RELEASE} inline; {$ENDIF}
  end;

  TParamsHelper = record helper for TParams
  private
    function AddParam(Param: TParam): TParams; {$IFNDEF DEBUG} inline; {$ENDIF}
  public
    function Add(Param: TParam): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: string): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: Integer): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: Extended): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: TDateTime; Format: string = ''): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: Boolean): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: TArrayOfString): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: TArrayOfInteger): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: TAttachmentArray): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: TVkEntity): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Add(Key: string; Value: TAttachment): TParams; overload; {$IFNDEF DEBUG} inline; {$ENDIF}
    function KeyExists(Key: string): Boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
    function GetValue(Key: string): string; {$IFNDEF DEBUG} inline; {$ENDIF}
    function Remove(Key: string): string; {$IFNDEF DEBUG} inline; {$ENDIF}
  end;

  TVkPhotoFeedType = (Photo, PhotoTag);

  TVkPhotoFeedTypeHelper = record helper for TVkPhotoFeedType
    function ToString: string; {$IFNDEF DEBUG} inline; {$ENDIF}
  end;

  /// <summary>
  /// Wall — фотографии со стены;
  /// Profile — фотографии профиля;
  /// Saved — сохраненные фотографии
  /// </summary>
  TVkPhotoSystemAlbum = (Wall, Saved, Profile);

  TVkPhotoSystemAlbumHelper = record helper for TVkPhotoSystemAlbum
    function ToString: string; {$IFNDEF DEBUG} inline; {$ENDIF}
    function ToVkId: Integer; {$IFNDEF DEBUG} inline; {$ENDIF}
  end;

  TVkMediaSort = (DateAdd, Duration, Popular);

  TVkAudioPlaylistFilter = (All, Owned, Followed, Albums);

  TVkVideosFilter = (MP4, YouTube, Vimeo, Short, Long);

  TVkVideosFilterHelper = record helper for TVkVideosFilter
    function ToString: string; {$IFNDEF DEBUG} inline; {$ENDIF}
  end;

  TVkVideosFilters = set of TVkVideosFilter;

  TVkVideosFiltersHelper = record helper for TVkVideosFilters
    function ToString: string; inline;
    class function All: TVkVideosFilters; static; inline;
  end;

  TVkVideoType = (Video, MusicVideo, Movie);

  TVkVideoTypeHelper = record helper for TVkVideoType
    function ToString: string; inline;
    class function Create(const Value: string): TVkVideoType; static;
  end;

  TVkLiveStatus = (Waiting, Started, Finished, Failed, Upcoming);

  TVkLiveStatusHelper = record helper for TVkLiveStatus
    function ToString: string; inline;
    class function Create(const Value: string): TVkLiveStatus; static;
  end;

  TVkMonthlyTier = (Tier1, Tier2, Tier3, Tier4, Tier5, Tier6, Unlimited);

  TVkMonthlyTierHelper = record helper for TVkMonthlyTier
    function ToString: string; inline;
    class function Create(const Value: string): TVkMonthlyTier; static;
  end;

  TVkBoardTopicOrder = (DateUpCreate = -2, DateUpUpdate = -1, DateDownCreate = 2, DateDownUpdate = 1);

  TVkBoardTopicPreview = (OnlyFirst = 1, OnlyLast = 2, Both = 3);

  /// <summary>
  /// Флаги сообщений
  /// </summary>
  TVkMessageFlag = (UNKNOWN_9, UNKNOWN_8, UNKNOWN_7, UNKNOWN_6, NotDelivered, //
    DeleteForAll, Hidden, UNKNOWN_5, UNKNOWN_4, UnreadMultichat, UNKNOWN_3,   //
    UNKNOWN_2, UNKNOWN_1, Media, Fixed, Deleted, Spam, Friends, Chat,         //
    Important, Replied, Outbox, Unread);

  TVkMessageFlagHelper = record helper for TVkMessageFlag
    function ToString: string; inline;
  end;

  TVkMessageFlags = set of TVkMessageFlag;

  TMessageFlagsHelper = record helper for TVkMessageFlags
    class function FlagDataToFlag(FlagData: Integer): TVkMessageFlag; static;
    class function Create(Data: Integer): TVkMessageFlags; static;
    function ToString: string;
  end;

  /// <summary>
  /// ChatPhotoUpdate — обновлена фотография беседы;
  /// ChatPhotoRemove — удалена фотография беседы;
  /// ChatCreate — создана беседа;
  /// ChatTitleUpdate — обновлено название беседы;
  /// ChatInviteUser — приглашен пользователь;
  /// ChatKickUser — исключен пользователь;
  /// ChatPinMessage — закреплено сообщение;
  /// ChatUnpinMessage — откреплено сообщение;
  /// ChatInviteUserByLink — пользователь присоединился к беседе по ссылке.
  /// ConversationStyleUpdate —
  /// </summary>
  /// <list type="bullet">
  /// <item>
  /// <description>123123</description>
  /// </item>
  /// <item>
  /// <description>fg464g6</description>
  /// </item>
  /// </list>
  TVkMessageActionType = (Unknown, ChatPhotoUpdate, ChatPhotoRemove,          //
    ChatCreate, ChatTitleUpdate, ChatInviteUser, ChatKickUser,                //
    ChatPinMessage, ChatUnpinMessage, ChatInviteUserByLink,                   //
    ConversationStyleUpdate);

  TVkMessageActionTypeHelper = record helper for TVkMessageActionType
    function ToString: string; inline;
    class function Create(const Value: string): TVkMessageActionType; static;
  end;

  TVkMessageActivity = (Typing, AudioMessage);

  TVkMessageActivityHelper = record helper for TVkMessageActivity
    function ToString: string; inline;
  end;

  /// <summary>
  /// Тип материалов, который необходимо вернуть.
  /// <b>Photo</b> — фотографии;
  /// <b>Video</b> — видеозаписи;
  /// <b>Audio</b> — аудиозаписи;
  /// <b>Doc</b> — документы;
  /// <b>Link</b> — ссылки;
  /// <b>Market</b> — товары;
  /// <b>Wall</b> — записи;
  /// <b>Share</b> — ссылки, товары и записи.
  /// Обратите внимание — существует ограничение по дате отправки вложений.
  /// Так, для получения доступны вложения типов photo, video, audio, doc,
  /// отправленные не ранее 25.03.2013, link — не ранее 20.05.13, market, wall — 01.02.2016.
  /// </summary>
  TVkHistoryAttachment = (Photo, Video, Audio, Doc, Link, Market, Wall, Share);

  TVkHistoryAttachmentHelper = record helper for TVkHistoryAttachment
    function ToString: string; inline;
  end;

  /// <summary>
  /// Интент — это метка, которая обозначает приблизительное содержание сообщения от сообщества.
  /// Передаётся в необязательном параметре messages.send — Intent.
  /// Default
  /// <b>PromoNewsletter</b> - интент, который должен сопровождать рекламную рассылку для ботов.
  /// <b>BotAdInvite</b> - интент, который должен сопровождать сообщения, запрашивающее подтверждение пользователя на отправку этому пользователю рекламы.
  /// <b>BotAdPromo</b> - интент, который должен сопровождать сообщение содержащее рекламу от бота.
  /// </summary>
  TVkMessageIntent = (Default, PromoNewsletter, BotAdInvite, BotAdPromo);

  TVkMessageIntentHelper = record helper for TVkMessageIntent
    function ToString: string; inline;
  end;

  /// <summary>
  /// Фильтр бесед
  /// All — все беседы;
  /// Unread — беседы с непрочитанными сообщениями;
  /// Important — беседы, помеченные как важные (только для сообщений сообществ);
  /// Unanswered — беседы, помеченные как неотвеченные (только для сообщений сообществ).
  /// </summary>
  TVkConversationFilter = (All, Unread, Important, Unanswered);

  TVkConversationFilterHelper = record helper for TVkConversationFilter
    function ToString: string; inline;
  end;

  /// <summary>
  /// In — состоит в чате;
  /// Kicked — исключён из чата;
  /// Left — покинул чат.
  /// </summary>
  TVkChatState = (None, &In, Kicked, Left);

  TVkChatStateHelper = record Helper for TVkChatState
    function ToString: string; inline;
    class function Create(Value: string): TVkChatState; static;
  end;

  /// <summary>
  /// Post — новые записи со стен;
  /// Photo — новые фотографии;
  /// PhotoTag — новые отметки на фотографиях;
  /// WallPhoto — новые фотографии на стенах;
  /// Friend — новые друзья;
  /// Note — новые заметки;
  /// Audio — записи сообществ и друзей, содержащие аудиозаписи, а также новые аудиозаписи, добавленные ими;
  /// Video — новые видеозаписи.
  /// AudioPlaylist = плей-лист
  /// </summary>
  TVkNewsfeedType = (Post, Photo, PhotoTag, WallPhoto, Friend, Note, Audio, Video, AudioPlaylist);

  TVkNewsfeedTypeHelper = record Helper for TVkNewsfeedType
    function ToString: string; inline;
    class function Create(Value: string): TVkNewsfeedType; static;
  end;

  TVkNewsfeedTypes = set of TVkNewsfeedType;

  TVkNewsfeedTypesHelper = record Helper for TVkNewsfeedTypes
    function ToString: string; inline;
  end;

  /// <summary>
  /// Wall — запись на стене;
  /// Photo — фотография;
  /// PhotoTag — отметка на фотографии;
  /// ProfilePhoto — фотография профиля;
  /// Video — видеозапись;
  /// Audio — аудиозапись.
  /// </summary>
  TVkNewsfeedIgnoreType = (Wall, Photo, PhotoTag, ProfilePhoto, Video, Audio);

  TVkNewsfeedIgnoreTypeHelper = record Helper for TVkNewsfeedIgnoreType
    function ToString: string; inline;
  end;

  /// <summary>
  /// Post — новые комментарии к записям со стен;
  /// Photo — новые комментарии к фотографиям;
  /// Video — новые комментарии к видеозаписям;
  /// Topic — новые сообщения в обсуждениях;
  /// Market — новые комментарии к товарам;
  /// Note — новые комментарии к заметкам.
  /// </summary>
  TVkNewsfeedCommentsType = (Post, Photo, Video, Topic, Market, Note);

  TVkNewsfeedCommentsTypeHelper = record Helper for TVkNewsfeedCommentsType
    function ToString: string; inline;
  end;

  TVkNewsfeedCommentsTypes = set of TVkNewsfeedCommentsType;

  TVkNewsfeedCommentsTypesHelper = record Helper for TVkNewsfeedCommentsTypes
    function ToString: string; inline;
  end;

  /// <summary>
  /// Wall — записи на стене пользователя;
  /// Mentions — упоминания в записях на стене, в комментариях или в обсуждениях;
  /// Comments — комментарии к записям на стене, фотографиям и видеозаписям;
  /// Likes — отметки «Мне нравится»;
  /// Reposts — скопированные у текущего пользователя записи на стене, фотографии и видеозаписи;
  /// Followers — новые подписчики;
  /// Friends — принятые заявки в друзья.
  /// </summary>
  TVkNotificationType = (Wall, Mentions, Comments, Likes, Reposts, Followers, Friends);

  TVkNotificationTypeHelper = record Helper for TVkNotificationType
    function ToString: string; inline;
  end;

  TVkNotificationFilter = set of TVkNotificationType;

  TVkNotificationFilterHelper = record Helper for TVkNotificationFilter
    function ToString: string; inline;
  end;

  /// <summary>
  /// Жанры музыки
  /// </summary>
  TVkAudioGenre = (None, Rock, Pop, RapAndHipHop, EasyListening,    //
    HouseAndDance, Instrumental, Metal, Alternative, Dubstep,       //
    JazzAndBlues, DrumAndBass, Trance, Chanson, Ethnic,             //
    AcousticAndVocal, Reggae, Classical, IndiePop, Speech,          //
    ElectropopAndDisco, Other);

  TVkAudioGenreHelper = record helper for TVkAudioGenre
    function ToConst: Integer;
    function ToString: string; inline;
    class function Create(Value: Integer): TVkAudioGenre; static;
  end;

  TVkDocUploadType = (Doc, AudioMessage);

  TVkDocTypeFilter = (All, Text, Archives, GIF, Pictures, Audios, Videos,     //
    Books, Other);

  TVkSortUser = (Popular, DateReg);

  TVkPhotoSort = (DateAdd, Likes);

  TVkSort = (Asc, Desc);

  TVkSortHelper = record helper for TVkSort
    function ToString: string; overload; inline;
  end;

  TVkSortIdTime = (IdAsc, IdDesc, TimeAsc, TimeDesc);

  TVkSortIdTimeHelper = record helper for TVkSortIdTime
    function ToString: string; overload; inline;
  end;

  TVkLang = (Auto = -1, RU = 0, UK = 1, BE = 2, EN = 3, ES = 4,   //
    FI = 5, DE = 6, IT = 7);

  TVkGroupAdminLevel = (None, Moderator, Editor, Administrator);

  TVkGroupStatusType = (None, Online, AnswerMark);

  TVkGroupStatusTypeHelper = record helper for TVkGroupStatusType
    function ToString: string; inline;
    class function Create(const Value: string): TVkGroupStatusType; static;
  end;

  /// <summary>
  /// <b>Friends</b> — будут возвращены только друзья в этом сообществе.
  /// <b>Unsure</b> — будут возвращены пользователи, которые выбрали «Возможно
  /// пойду» (если сообщество относится к мероприятиям).
  /// <b>Managers</b> — будут возвращены только руководители сообщества
  /// (доступно при запросе с передачей access_token от имени администратора сообщества).
  /// <b>donut</b> — будут возвращены только доны (пользователи, у которых есть платная подписка VK Donut
  /// </summary>
  TVkGroupMembersFilter = (Friends, Unsure, Managers, Donut);

  TVkGroupMembersFilterHelper = record helper for TVkGroupMembersFilter
    function ToString: string; inline;
  end;

  /// <summary>
  /// (автовыбор), Запустить, Играть, Перейти, Открыть, Подробнее, Позвонить,
  /// Забронировать, Записаться, Зарегистрироваться, Купить, Купить билет,
  /// Заказать, Создать, Установить, Связаться, Заполнить, Подписаться,
  /// Я пойду, Вступить, Связаться, Написать, Начать, Получить, Смотреть,
  /// Скачать, Участвовать, Играть, Подать заявку, Получить предложение,
  /// Написать, Откликнуться
  /// Подробное описание тут https://vk.com/dev/wall.postAdsStealth
  /// </summary>
  TVkPostLinkButton = (Auto, AppJoin, AppGameJoin, OpenUrl, Open, More, Call, //
    Book, Enroll, Register, Buy, BuyTicket, Order, Create, Install, Contact,  //
    Fill, JoinPublic, JoinEvent, Join, IM, IM2, Start, Get, Watch, Download,  //
    Participate, Play, Apply, GetAnOffer, ToWrite, Reply);

  TVkPostLinkButtonHelper = record helper for TVkPostLinkButton
    function ToString: string; inline;
  end;

  /// <summary>
  /// to_store — «В магазин»;
  /// vote — «Голосовать»;
  /// more — «Ещё»;
  /// book — «Забронировать»;
  /// order — «Заказать»;
  /// enroll — «Записаться»;
  /// fill — «Заполнить»;
  /// signup — «Зарегистрироваться»;
  /// buy — «Купить»;
  /// ticket — «Купить билет»;
  /// write — «Написать»;
  /// open — «Открыть»;
  /// learn_more — «Подробнее» (по умолчанию);
  /// view — «Посмотреть»;
  /// go_to — «Перейти»;
  /// contact — «Связаться»;
  /// watch — «Смотреть»;
  /// play — «Слушать»;
  /// install — «Установить»;
  /// read — «Читать»;
  /// game — «Играть».
  /// </summary>
  TVkLinkText = (ToStore, Vote, More, Book, Order, Enroll, Fill, Signup, Buy, //
    Ticket, Write, Open, LearnMore, View, &GoTo, Contact, Watch, Play,        //
    Install, Read, Game);

  TVkLinkTextHelper = record helper for TVkLinkText
    function ToString: string; inline;
  end;

  TVkProfileField = (PhotoId, Verified, Sex, BirthDate, City, Country,           //
    HomeTown, HasPhoto, Photo50, Photo100, Photo200Orig, Photo200, Photo400Orig, //
    PhotoMax, PhotoMaxOrig, PhotoBig, PhotoMedium, Online, Lists, Domain,        //
    HasMobile, Contacts, Site, Education, Universities, Schools, Status,         //
    LastSeen, FollowersCount, CommonCount, Occupation, Nickname, Relatives,      //
    Relation, Personal, Connections, &Exports, WallComments, Activities,         //
    Interests, Music, Movies, TV, Books, Games, About, Quotes, CanPost,          //
    CanSeeAllPosts, CanSeeAudio, CanWritePrivateMessage, CanSendFriendRequest,   //
    IsFavorite, IsHiddenFromFeed, TimeZone, ScreenName, MaidenName, CropPhoto,   //
    IsFriend, FriendStatus, Career, Military, Blacklisted, BlacklistedByMe,      //
    CanBeInvitedGroup, OnlineMobile, Counters, FirstNameNom, FirstNameGen,       //
    FirstNameDat, FirstNameAcc, FirstNameIns, FirstNameAbl, LastNameNom,         //
    LastNameGen, LastNameDat, LastNameAcc, LastNameIns, LastNameAbl,             //
    WallDefault);                                                                //

  TVkProfileFieldHelper = record helper for TVkProfileField
    function ToString: string; inline;
  end;

  TVkProfileFields = set of TVkProfileField;

  TVkProfileFieldsHelper = record helper for TVkProfileFields
  public
    function ToString: string; inline;
    class function All: TVkProfileFields; static; inline;
    class function AllForGroup: TVkProfileFields; static; inline;
  end;

  /// <summary>
  /// <b>hints</b> — сортировать по рейтингу, аналогично тому, как друзья сортируются в разделе Мои друзья (Это значение доступно только для Standalone-приложений с ключом доступа, полученным по схеме Implicit Flow.).
  /// <b>random</b> — возвращает друзей в случайном порядке.
  /// <b>mobile</b> — возвращает выше тех друзей, у которых установлены мобильные приложения.
  /// <b>name</b> — сортировать по имени. Данный тип сортировки работает медленно, так как сервер будет получать всех друзей а не только указанное количество count. (работает только при переданном параметре fields).
  /// </summary>
  TVkFriendsOrder = (None, Hints, Random, Mobile, Name);

  TVkFriendsOrderHelper = record helper for TVkFriendsOrder
    function ToString: string; inline;
  end;

  TVkFriendAddInfo = (Success = 1, Approved = 2, Resended = 4);

  TVkGroupField = (City, Country, Place, Description, WikiPage, MembersCount, //
    Counters, StartDate, FinishDate, CanPost, CanSeeAllPosts, Activity,       //
    Status, Contacts, Links, FixedPost, Verified, Site, CanCreateTopic,       //
    Photo50, Photo100, Photo200, Cover, Addresses, AgeLimits, BanInfo,        //
    CanMessage, CanUploadDoc, CanUploadVideo, CropPhoto, HasPhoto,            //
    IsFavorite, IsHiddenFromFeed, IsMessagesBlocked);

  TVkGroupFieldHelper = record helper for TVkGroupField
    function ToString: string; inline;
  end;

  TVkGroupFields = set of TVkGroupField;

  TVkGroupFieldsHelper = record helper for TVkGroupFields
    function ToString: string; inline;
    class function All: TVkGroupFields; static; inline;
  end;

  TVkGroupAddressField = (Title, Address, AdditionalAddress, CountryId,       //
    CityId, MetroStationId, Latitude, Longitude, WorkInfoStatus, TimeOffset);

  TVkGroupAddressFieldHelper = record helper for TVkGroupAddressField
    function ToString: string; inline;
  end;

  TVkGroupAddressFields = set of TVkGroupAddressField;

  TVkGroupAddressFieldsHelper = record helper for TVkGroupAddressFields
    function ToString: string; inline;
    class function All: TVkGroupAddressFields; static; inline;
  end;

  TVkGroupAccess = (Open, Close, &Private);

  TVkGroupRole = (Moderator, Editor, Admin, Advertiser);

  TVkGroupRoleHelper = record helper for TVkGroupRole
    function ToString: string; inline;
  end;

  TVkGroupMainSection = (None, Photos, Board, Audios, Videos, Market);

  TVkGroupTagAct = (Bind, Unbind);

  TVkGroupTagActHelper = record helper for TVkGroupTagAct
    function ToString: string; inline;
  end;

  TVkOrderStateAction = (Cancel, Charge, Refund);

  TVkOrderStateActionHelper = record helper for TVkOrderStateAction
    function ToString: string; inline;
  end;

  /// <summary>
  /// <b>wisNoInformation</b> — нет информации о расписании;
  /// <b>wisTemporarilyClosed</b> — временно закрыто;
  /// <b>wisAlwaysOpened</b> — открыто круглосуточно;
  /// <b>wisForeverClosed</b> — закрыто навсегда;
  /// <b>wisTimetable</b> — открыто в указанные часы работы. Для этого типа расписания необходимо передать параметр <b>Timetable: TVkTimeTable</b>;
  /// </summary>
  TVkWorkInfoStatus = (NoInformation, TemporarilyClosed, AlwaysOpened, ForeverClosed, Timetable);

  TVkWorkInfoStatusHelper = record helper for TVkWorkInfoStatus
    function ToString: string; inline;
  end;

  TVkGroupSearchSort = (Default, GrowthRate, DailyTraffic, NumberOfLikes, NumberOfComments, NumberOfPosts);

  /// <summary>
  /// GroupTagColor
  /// Тут список цветов -> VkGroupTagColors
  /// </summary>
  TVkGroupTagColor = string;

  /// <summary>
  /// Friends – друзья пользователя;
  /// Idols – подписки пользователя;
  /// Publics – публичные страницы, на которые подписан пользователь;
  /// Groups – группы пользователя;
  /// Events – встречи пользователя;
  /// Correspondents – люди, с которыми пользователь имеет переписку;
  /// MutualFriends – люди, у которых есть общие друзья с текущим пользователем
  /// (этот фильтр позволяет получить не всех пользователей, имеющих общих друзей)
  /// </summary>
  TVkSearchFilter = (Friends, Idols, Publics, Groups, Events, Correspondents, MutualFriends);

  TVkSearchFilterHelper = record helper for TVkSearchFilter
    function ToString: string; inline;
  end;

  TVkSearchFilters = set of TVkSearchFilter;

  TVkSearchFiltersHelper = record helper for TVkSearchFilters
    function ToString: string; inline;
  end;

  TAppActivity = (NewLevel = 1, NewScore = 2);

  TVkDeactivated = (None, Deleted, Banned);

  TVkDeactivatedHelper = record helper for TVkDeactivated
    function ToString: string; inline;
    class function Create(const Value: string): TVkDeactivated; static;
  end;

  TVkAgeLimits = (Unknown = 0, None = 1, Plus16 = 2, Plus18 = 3);

  TVkCurrency = (RUB, UAH, KZT, EUR, USD);

  TVkMarketCurrencyHelper = record helper for TVkCurrency
    function ToConst: Integer; inline;
  end;

  /// <summary>
  /// Вид сортировки товаров.
  /// User — пользовательская расстановка,
  /// DateAdd — по дате добавления товара,
  /// Cost — по цене,
  /// Popular — по популярности.
  /// </summary>
  TVkMarketSort = (User, DateAdd, Cost, Popular);

  TVkGroupTypeCreate = (Group, Event, &Public);

  TVkGroupTypeCreateHelper = record helper for TVkGroupTypeCreate
    function ToString: string; inline;
    class function Create(const Value: string): TVkGroupTypeCreate; static;
  end;

  TVkGroupSubType = (PlaceOrSmallCompany = 1,                                 //
    CompanyOrOrganizationOrWebsite = 2, FamousPersonOrTeam = 3,               //
    CreationOrProduct = 4);

  TVkGroupType = (Group, Event, Page);

  TVkGroupTypeHelper = record helper for TVkGroupType
    function ToString: string; inline;
    class function Create(const Value: string): TVkGroupType; static;
  end;

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
  TVkGroupSubjectType = (                                                     //
    Unknown = 0, AutoMotorcycle = 1, ActiveRest = 2, Business = 3,            //
    Pets = 4, Health = 5, AcquaintanceAndCommunication = 6, Games = 7,        //
    ITComputersAndSoftware = 8, Cinema = 9, BeautyAndFashion = 10,            //
    Cooking = 11, CultureAndArt = 12, Literature = 13,                        //
    MobileCommunicationsAndInternet = 14, Music = 15,                         //
    ScienceAndTechnology = 16, RealEstate = 17, NewsAndMedia = 18,            //
    Security = 19, Education = 20, ArrangementAndRepair = 21, Politics = 22,  //
    FoodProducts = 23, Industry = 24, Travel = 25, Work = 26,                 //
    Entertainment = 27, Religion = 28, HomeAndFamily = 29, Sports = 30,       //
    Insurance = 31, Television = 32, GoodsAndServices = 33, Hobbies = 34,     //
    Finance = 35, Photo = 36, Esotericism = 37,                               //
    ElectronicsAndHouseholdAppliances = 38, Erotic = 39, Humor = 40,          //
    SocietyHumanities = 41, DesignAndGraphics = 42);

  TVkGroupFilter = (Admin, Editor, Moder, Advertiser, Groups, Publics,        //
    Events, HasAddress);

  TVkGroupFilterHelper = record helper for TVkGroupFilter
    function ToString: string; inline;
  end;

  TVkGroupFilters = set of TVkGroupFilter;

  TVkGroupFiltersHelper = record helper for TVkGroupFilters
    function ToString: string; inline;
    class function All: TVkGroupFilters; static; inline;
  end;

  /// <summary>
  /// Флаги диалогов
  /// </summary>
  TVkDialogFlag = (Important, Unanswered);

  TVkDialogFlags = set of TVkDialogFlag;

  TVkDialogFlagsHelper = record helper for TVkDialogFlags
    function ToString: string; overload; inline;
    class function FlagDataToFlag(FlagData: Integer): TVkDialogFlag; static;
    class function Create(Data: Integer): TVkDialogFlags; static;
  end;

  /// <summary>
  /// Идентификатор типа изменения в чате
  /// </summary>
  TVkChatChangeInfoType = (None, Name, Pic, NewAdmin, FixMessage, Join,       //
    Leave, Kick, Unadmin);

  /// <summary>
  /// Платформы
  /// </summary>
  TVkPlatform = (Unknown, Mobile, IPhone, IPad, Android, WindowsPhone,        //
    Windows, Web);

  TVkPlatformHelper = record helper for TVkPlatform
    function ToString: string; inline;
    class function Create(const Value: string): TVkPlatform; static;
  end;

  /// <summary>
  /// All — имя отправителя и сообщение видно всем;
  /// NameOnly — имя отправителя видно всем, сообщение видно только получателю;
  /// Anonymous — имя отправителя скрыто, сообщение видно только получателю.
  /// </summary>
  TVkGiftPrivacy = (All, NameOnly, Anonymous);

  TVkStatInterval = (Day, Week, Month, Year, All);

  TVkStatIntervalHelper = record helper for TVkStatInterval
    function ToString: string;
  end;

  TVkStatReachFilter = (Visitors, Reach, Activity);

  TVkStatReachFilterHelper = record helper for TVkStatReachFilter
    function ToString: string;
  end;

  TVkStatReachFilters = set of TVkStatReachFilter;

  TVkStatReachFiltersHelper = record helper for TVkStatReachFilters
    function ToString: string;
  end;

  /// <summary>
  /// Cтатус заявки
  /// Processing – заявка рассматривается;
  /// Declined – заявка отклонена;
  /// Response – общий ответ по статусу обработки заявки;
  /// ResponseWithLink – общий ответ по статусу обработки заявки, содержащий ссылку в поле link;
  /// </summary>
  TVkNameRequestStatus = (Processing, Declined, Response, ResponseWithLink);

  TVkNameRequestStatusHelper = record helper for TVkNameRequestStatus
    class function Create(const Value: string): TVkNameRequestStatus; static;
    function ToString: string; inline;
  end;

  /// <summary>
  /// Тип смены флагов
  /// </summary>
  TVkFlagsChangeType = (Replace, &Set, Reset);

  TVkFlagsChangeTypeHelper = record helper for TVkFlagsChangeType
    function ToString: string; overload; inline;
  end;

  /// <summary>
  /// Типы объектов
  /// </summary>
  TVkItemType = (Post, Comment, Photo, Audio, Video, Note, Market,            //
    PhotoComment, VideoComment, TopicComment, MarketComment, Sitepage, Story);

  TVkItemTypeHelper = record helper for TVkItemType
    function ToString: string; inline;
  end;

  /// <summary>
  /// Text — текстовые документы;
  /// Archive — архивы;
  /// GIF — gif;
  /// Picture — изображения;
  /// Audio — аудио;
  /// Video — видео;
  /// Book — электронные книги;
  /// Unknown — неизвестно.
  /// </summary>
  TVkDocumentType = (None, Text, Archive, GIF, Picture, Audio, Video,         //
    Book, Unknown);

  TVkPeerType = (Unknown, User, Chat, Group, Email);

  TVkPeerTypeHelper = record helper for TVkPeerType
    function ToString: string; inline;
    class function Create(Value: string): TVkPeerType; overload; static;
    class function Create(PeerId: Integer): TVkPeerType; overload; static;
  end;

  /// <summary>
  /// UserBannedOrDeleted — пользователь заблокирован или удален;
  /// UserBlacklisted — нельзя отправить сообщение пользователю, который в чёрном списке;
  /// UserDisableGroupsMessages — пользователь запретил сообщения от сообщества;
  /// UserPrivacy — пользователь запретил присылать ему сообщения с помощью настроек приватности;
  /// GroupDisableMessages — в сообществе отключены сообщения;
  /// GroupBannedMessages — в сообществе заблокированы сообщения;
  /// NoAccessChat — нет доступа к чату;
  /// NoAccessEMail — нет доступа к e-mail;
  /// NoAccessGroup — нет доступа к сообществу.
  /// </summary>
  TVkConversationDisableReason = (UserBannedOrDeleted, UserBlacklisted,       //
    UserDisableGroupsMessages, UserPrivacy, GroupDisableMessages,             //
    GroupBannedMessages, NoAccessChat, NoAccessEMail, NoAccessGroup);

  TVkConversationDisableReasonHelper = record helper for TVkConversationDisableReason
    function ToString: string; inline;
    class function Create(Value: string): TVkConversationDisableReason; static;
  end;

  TVkKeyboardActionType = (Text, OpenLink, Location, VKPay, OpenApp, Callback);

  TVkKeyboardActionTypeHelper = record helper for TVkKeyboardActionType
    function ToString: string; inline;
    class function Create(Value: string): TVkKeyboardActionType; static;
  end;

  TVkKeyboardButtonColor = (Default, Positive, Negative, Primary, Secondary);

  TVkKeyboardButtonColorHelper = record helper for TVkKeyboardButtonColor
    function ToString: string; inline;
    function ToColor: Cardinal; inline;
    class function Create(Value: string): TVkKeyboardButtonColor; static;
  end;

  /// <summary>
  /// Типы записей
  /// Suggests — предложенные записи на стене сообщества (доступно только при вызове с передачей access_token);
  /// Postponed — отложенные записи (доступно только при вызове с передачей access_token);
  /// Owner — записи владельца стены;
  /// Others — записи не от владельца стены;
  /// All — все записи на стене (owner + others).
  /// </summary>
  TVkPostTypeFilter = (Suggests, Postponed, Owner, Others, All);

  TVkPostTypeFilterHelper = record helper for TVkPostTypeFilter
    function ToString: string; inline;
  end;

  TVkPostType = (Post, Copy, Reply, Postpone, Suggest);

  TVkPostTypeHelper = record helper for TVkPostType
    function ToString: string; inline;
    class function Create(const Value: string): TVkPostType; static;
  end;

  /// <summary>
  /// VK — запись создана через основной интерфейс сайта (http://vk.com/);
  /// Widget — запись создана через виджет на стороннем сайте;
  /// API — запись создана приложением через API;
  /// RSS — запись создана посредством импорта RSS-ленты со стороннего сайта;
  /// SMS — запись создана посредством отправки SMS-сообщения на специальный номер.
  /// </summary>
  TVkPostSourceType = (VK, Widget, API, RSS, SMS);

  TVkPostSourceTypeHelper = record helper for TVkPostSourceType
    function ToString: string; inline;
    class function Create(const Value: string): TVkPostSourceType; static;
  end;

  TVkDonutPaidDuration = (DonutOnly = -1, Days1 = 86400, Days2 = 172800,      //
    Days3 = 172800, Days4 = 345600, Days5 = 432000, Days6 = 518400,           //
    Days7 = 604800);

  TVkPolitical = (None, Communist, Socialist, Moderate, Liberal,              //
    Conservative, Monarchical, UltraConservative, Indifferent, Libertarian);

  TVkTagPosition = (Front, Back);

  TVkTagPositionHelper = record helper for TVkTagPosition
    function ToString: string; inline;
  end;

  TVkFavePageType = (Users, Groups, Hints);

  TVkFavePageTypeHelper = record helper for TVkFavePageType
    function ToString: string; inline;
  end;

  TVkFaveType = (Post, Video, Product, Article, Link);

  TVkFaveTypeHelper = record helper for TVkFaveType
    function ToString: string; inline;
    class function Create(const Value: string): TVkFaveType; static; inline;
  end;

  /// <summary>
  /// All — просматривать страницу могут все;
  /// Member — только участники сообщества;
  /// Admin — только руководители сообщества
  /// </summary>
  TVkPageAccess = (Admin, Member, All);

  /// <summary>
  /// именительный – nom, родительный – gen, дательный – dat, винительный – acc,
  /// творительный – ins, предложный – abl
  /// </summary>
  TVkNameCase = (Nom, Gen, Dat, Acc, Ins, Abl);

  TVkNameCaseHelper = record helper for TVkNameCase
    function ToString: string; inline;
  end;

  /// <summary>
  /// Пол
  /// </summary>
  TVkSex = (None, Female, Male);

  /// <summary>
  /// Видимость даты рождения
  /// </summary>
  TVkBirthDateVisibility = (Hidden, Visible, DayMonOnly);

  /// <summary>
  /// Отношения.
  /// None — не указано.
  /// NotMarried — не женат/не замужем;
  /// HaveFriend — есть друг/есть подруга;
  /// Affiance — помолвлен/помолвлена;
  /// Married — женат/замужем;
  /// Complicated — всё сложно;
  /// ActivelyLooking — в активном поиске;
  /// InLove — влюблён/влюблена;
  /// CivilMarriage — в гражданском браке;
  /// </summary>
  TVkRelation = (None, NotMarried, HaveFriend, Affiance, Married, Complicated, //
    ActivelyLooking, InLove, CivilMarriage);

  TVkSearchTarget = (Friends, Subscriptions);

  TVkSearchTargetHelper = record helper for TVkSearchTarget
    function ToString: string; overload; inline;
  end;

  TVkSearchTargets = set of TVkSearchTarget;

  TVkSearchTargetsHelper = record helper for TVkSearchTargets
    function ToString: string; overload; inline;
  end;

  TVkLinkStatusType = (NotBanned, Banned, Processing);

  TVkLinkStatusTypeHelper = record helper for TVkLinkStatusType
    function ToString: string; overload; inline;
    class function FromString(Value: string): TVkLinkStatusType; static; inline;
  end;

  TVkUserReport = (Porn, Spam, Insult, Advertisеment);

  TVkUserReportHelper = record helper for TVkUserReport
    function ToString: string; overload; inline;
  end;

  TVkUserBlockReason = (Other, Spam, InsultingParticipants,                   //
    ObsceneExpressions, OffTopic);

  TVkUserBlockReasonHelper = record helper for TVkUserBlockReason
    function ToString: string; overload; inline;
  end;

  TVkMediaReportReason = (Spam, ChildPorn, Extremism, Violence, Drug, Adults, //
    Insult, CallForSuicide);

  TVkMediaReportReasonHelper = record helper for TVkMediaReportReason
    function ToString: string; overload; inline;
  end;

  /// <summary>
  /// Join — пользователь вступил в группу или мероприятие (подписался на публичную страницу).
  /// Unsure — для мероприятий: пользователь выбрал вариант «Возможно, пойду».
  /// Accepted — пользователь принял приглашение в группу или на мероприятие.
  /// Approved — заявка на вступление в группу/мероприятие была одобрена руководителем сообщества.
  /// Request — пользователь подал заявку на вступление в сообщество.
  /// </summary>
  TVkGroupJoinType = (Unknown, Join, Unsure, Accepted, Approved, Request);

  TVkGroupJoinTypeHelper = record helper for TVkGroupJoinType
    function ToString: string; overload; inline;
    class function Create(Value: string): TVkGroupJoinType; static;
  end;

  TVkGroupLevel = (None, Moder, Editor, Admin);

  TVkGroupLevelHelper = record helper for TVkGroupLevel
    function ToString: string; overload; inline;
  end;

  TVkCounterFilter = (Friends, Messages, Photos, Videos, Notes, Gifts,        //
    Events, Groups, Notifications, Sdk, AppRequests, FriendsRecommendations);

  TVkCounterFilterHelper = record helper for TVkCounterFilter
    function ToString: string; overload; inline;
  end;

  TVkCounterFilters = set of TVkCounterFilter;

  TVkCounterFiltersHelper = record helper for TVkCounterFilters
    function ToString: string; overload; inline;
  end;

  /// <summary>
  /// Статус заказа
  /// New - новый;
  /// Approved - согласуется;
  /// Assembled - собирается;
  /// Delivered - доставляется;
  /// Completed - выполнен;
  /// Canceled - отменен;
  /// Returned - возвращен.
  /// </summary>
  TVkOrderStatus = (New, Approved, Assembled, Delivered, Completed, Canceled, //
    Returned);

  TVkPaymentStatus = (NotPaid, Paid, Returned);

  TVkPaymentStatusHelper = record helper for TVkPaymentStatus
    function ToString: string; inline;
    class function Create(const Value: string): TVkPaymentStatus; static;
  end;

  /// <summary>
  /// Статус доступности товара
  /// Available — товар доступен;
  /// Removed — товар удален;
  /// NotAvailable — товар недоступен.
  /// </summary>
  TVkProductAvailability = (Available, Removed, NotAvailable);

  TVkInfoFilter = (Country, HttpsRequired, OwnPostsDefault, NoWallReplies,    //
    Intro, Lang);

  TVkInfoFilterHelper = record helper for TVkInfoFilter
    function ToString: string; overload; inline;
  end;

  TVkInfoFilters = set of TVkInfoFilter;

  TVkInfoFiltersHelper = record helper for TVkInfoFilters
    function ToString: string; overload; inline;
  end;

  TVkPermission = (Notify, Friends, Photos, Audio, Video, Stories, Pages,     //
    Status, Notes, Messages, Wall, Ads, Offline, Docs, Groups, Notifications, //
    Stats, Email, Market, AppWidget, Manage);

  TVkPermissionHelper = record helper for TVkPermission
    function ToString: string; overload; inline;
  end;

  TVkPermissions = set of TVkPermission;

  TVkPermissionsHelper = record helper for TVkPermissions
    function ToString: string; overload; inline;
    procedure Include(Value: TVkPermission); inline;
  end;

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

  /// <summary>
  /// Received — события, полученные приложением;
  /// Prepared — события, сгенерированные со стороны ВКонтакте
  /// </summary>
  TVkStreamStatType = (Received, Prepared);

  TVkStreamStatTypeHelper = record helper for TVkStreamStatType
    function ToString: string; overload; inline;
    class function Create(Value: string): TVkStreamStatType; static;
  end;

  /// <summary>
  /// i5m — пять минут. Максимальный период — 3 дня между StartTime и EndTime;
  /// i1h — один час. Максимальный период — 7 дней между StartTime и EndTime;
  /// i24h — сутки. Максимальный период — 31 день между StartTime и EndTime.
  /// </summary>
  TVkStreamStatInterval = (i5m, i1h, i24h);

  TVkStreamStatIntervalHelper = record helper for TVkStreamStatInterval
    function ToString: string; overload; inline;
    class function Create(Value: string): TVkStreamStatInterval; static;
  end;

  /// <summary>
  ///  general — обычный;
  ///  agency — агентский.
  /// </summary>
  TVkAdsAccountType = (General, Agency);

  TVkAdsAccountTypeHelper = record helper for TVkAdsAccountType
    function ToString: string; overload; inline;
    class function Create(Value: string): TVkAdsAccountType; static;
  end;

  /// <summary>
  /// none — товары отключены;
  /// basic — базовые товары;
  /// advanced — расширенные товары
  /// </summary>
  TVkGroupMarketState = (None, Basic, Advanced);

  TVkGroupMarketStateHelper = record helper for TVkGroupMarketState
    function ToString: string; overload; inline;
    class function Create(Value: string): TVkGroupMarketState; static;
  end;

  /// <summary>
  /// Статус задачи на обработку аудиозаписи
  /// processing — аудиозапись обрабатывается.
  /// finished — обработка аудиозаписи закончена.
  /// internal_error — внутренние ошибки сервиса распознавания речи ВКонтакте.
  /// transcoding_error — ошибка перекодирования аудиозаписи во внутренний формат. Попробуйте загрузить аудиозапись в другом поддерживаемом формате.
  /// recognition_error — ошибка распознавания речи, сложности в распознавании. Попробуйте говорить чётче или снизить фоновые шумы.
  /// </summary>
  TVkAsrState = (Processing, Finished, InternalError, TranscodingError, RecognitionError);

  TVkAsrStateHelper = record helper for TVkAsrState
    function ToString: string; inline;
    class function Create(const Value: string): TVkAsrState; static;
  end;

  /// <summary>
  /// Модель распознавания речи, которую нужно использовать
  /// neutral — распознавание разборчивой речи, как в интервью или телешоу.
  /// spontaneous — распознавание речи со сленгом и ненормативной лексикой.
  /// </summary>
  TVkAsrModel = (Neutral, Spontaneous);

  TVkAsrModelHelper = record helper for TVkAsrModel
    function ToString: string; inline;
    class function Create(const Value: string): TVkAsrModel; static;
  end;

  /// <summary>
  ///  admin — главный администратор;
  ///  manager — администратор;
  ///  reports — наблюдатель.
  /// </summary>
  TVkAdsAccessRole = (Admin, Manager, Reports);

  TVkAdsAccessRoleHelper = record helper for TVkAdsAccessRole
    function ToString: string; overload; inline;
    class function Create(Value: string): TVkAdsAccessRole; static;
  end;

  TVkPrivacySettings = record
  private
    FInt: TArrayOfInteger;
    FStr: TArrayOfString;
  public
    procedure Add(const Value: string); overload;
    procedure Add(const Value: Integer); overload;
    procedure Delete(const Value: string); overload;
    procedure Delete(const Value: Integer); overload;
    procedure Clear;
    function ToString: string;
    function IsEmpty: Boolean;
    class function Empty: TVkPrivacySettings; static;
  end;

  TVkMessageDelete = class
  private
    FItems: TDictionary<string, Boolean>;
  public
    property Items: TDictionary<string, Boolean> read FItems write FItems;
    constructor Create;
    destructor Destroy; override;
  end;

  TVkPeerId = type Int64;

  TVkPeerHelper = record helper for TVkPeerId
    function IsChat: Boolean;
    function IsGroup: Boolean;
    function IsUser: Boolean;
    function PeerType: TVkPeerType;
    function ToString: string;
  end;

  TOnLogin = procedure(Sender: TObject) of object;

  TOnAuth = procedure(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash: string) of object;

  TOnConfirm = procedure(Sender: TObject; Ans: string; var Accept: Boolean) of object;

  TOnCaptcha = procedure(Sender: TObject; const CaptchaURL: string; var Answer: string) of object;

  TOnLog = procedure(Sender: TObject; const Value: string) of object;

  TOnVKError = procedure(Sender: TObject; E: Exception; Code: Integer; Text: string) of object;

const
  VkColorDefault = $00000000;
  VkColorPositive = $FF4BB34B;
  VkColorNegative = $FFE64646;
  VkColorPrimary = $FF5181B8;
  VkColorSecondary = $FFFFFFFF;

const
  //Вспомогательное текстовое представление
  VkUserActive: array[Boolean] of string = ('Бездействие', 'Покинул сайт');
  VkPlatforms: array[TVkPlatform] of string = ('Unknown', 'Mobile', 'iPhone', 'iPad', 'Android',
    'Windows Phone', 'Windows', 'Web');
  VkGroupLevel: array[TVkGroupLevel] of string = ('Участник', 'Модератор', 'Редактор', 'Администратор');
  VkUserBlockReason: array[TVkUserBlockReason] of string = ('Другое', 'Спам', 'Оскорбление участников',
    'Мат', 'Разговоры не по теме');
  VkPhotoReportReason: array[TVkMediaReportReason] of string = ('Спам', 'Детская порнография', 'Экстремизм',
    'Насилие', 'Пропаганда наркотиков', 'Материал для взрослых', 'Оскорбление', 'Призыв к суициду');
  VkMessageFlagTypes: array[TVkMessageFlag] of string = ('Unknown_9', 'Unknown_8', 'Unknown_7', 'Unknown_6',
    'NotDelivered', 'DeleteForAll', 'Hidden', 'Unknown_5', 'Unknown_4', 'UnreadMultichat', 'Unknown_3',
    'Unknown_2', 'Unknown_1', 'Media', 'Fixed', 'Deleted', 'Spam', 'Friends', 'Chat', 'Important',
    'Replied', 'Outbox', 'Unread');
  VkAudioGenresStr: array[TVkAudioGenre] of string = ('', 'Rock', 'Pop', 'RapAndHipHop', 'EasyListening',
    'HouseAndDance', 'Instrumental', 'Metal', 'Alternative', 'Dubstep', 'JazzAndBlues', 'DrumAndBass',
    'Trance', 'Chanson', 'Ethnic', 'AcousticAndVocal', 'Reggae', 'Classical', 'IndiePop', 'Speech',
    'ElectropopAndDisco', 'Other');
  VkChatChangeInfoType: array[TVkChatChangeInfoType] of string = ('', 'Изменилось название беседы',
    'Сменилась обложка беседы', 'Назначен новый администратор', 'Закреплено сообщение',
    'Пользователь присоединился к беседе', 'Пользователь покинул беседу', 'Пользователя исключили из беседы',
    'С пользователя сняты права администратора');

const
  //Аналогии типов
  //Соответствия флагов сообщений и битов
  VkMessageFlags: array[TVkMessageFlag] of Integer = (MF_UNKNOWN_9, MF_UNKNOWN_8,
    MF_UNKNOWN_7, MF_UNKNOWN_6, MF_NOT_DELIVERED, MF_DELETE_FOR_ALL, MF_HIDDEN,
    MF_UNKNOWN_5, MF_UNKNOWN_4, MF_UNREAD_MULTICHAT, MF_UNKNOWN_3, MF_UNKNOWN_2,
    MF_UNKNOWN_1, MF_MEDIA, MF_FIXED, MF_DELЕTЕD, MF_SPAM, MF_FRIENDS, MF_CHAT,
    MF_IMPORTANT, MF_REPLIED, MF_OUTBOX, MF_UNREAD);
  VkAudioGenres: array[TVkAudioGenre] of Integer = (AG_NONE, AG_ROCK, AG_POP,
    AG_RAPANDHIPHOP, AG_EASYLISTENING, AG_HOUSEANDDANCE, AG_INSTRUMENTAL,
    AG_METAL, AG_ALTERNATIVE, AG_DUBSTEP, AG_JAZZANDBLUES, AG_DRUMANDBASS,
    AG_TRANCE, AG_CHANSON, AG_ETHNIC, AG_ACOUSTICANDVOCAL, AG_REGGAE,
    AG_CLASSICAL, AG_INDIEPOP, AG_SPEECH, AG_ELECTROPOPANDDISCO, AG_OTHER);
  VkDialogFlags: array[TVkDialogFlag] of Integer = (GR_UNANSWERED, GR_IMPORTANT);
  //
  VkPlatformsType: array[TVkPlatform] of string = ('', 'mobile', 'ios', 'ios', 'android', 'winphone', 'windows', 'web');
  VkAttachmentType: array[TVkAttachmentType] of string = ('', 'photo', 'video', 'audio', 'doc', 'link', 'market',
    'market_album', 'wall', 'wall_reply', 'sticker', 'gift', 'call', 'audio_message', 'posted_photo', 'graffiti',
    'note', 'app', 'poll', 'page', 'album', 'photos_list', 'pretty_cards', 'event', 'money_transfer');
  VkPeerType: array[TVkPeerType] of string = ('', 'user', 'chat', 'group', 'email');
  VkNameCase: array[TVkNameCase] of string = ('nom', 'gen', 'dat', 'acc', 'ins', 'abl');
  VkItemType: array[TVkItemType] of string = ('post', 'comment', 'photo', 'audio', 'video', 'note', 'market',
    'photo_comment', 'video_comment', 'topic_comment', 'market_comment', 'sitepage', 'story');
  VkGroupJoinType: array[TVkGroupJoinType] of string = ('', 'join', 'unsure', 'accepted', 'approved', 'request');
  VkPostTypeFilter: array[TVkPostTypeFilter] of string = ('suggests', 'postponed', 'owner', 'others', 'all');
  VkPremissionStr: array[TVkPermission] of string = ('notify', 'friends', 'photos', 'audio', 'video', 'stories',
    'pages', 'status', 'notes', 'messages', 'wall', 'ads', 'offline', 'docs', 'groups', 'notifications', 'stats',
    'email', 'market', 'app_widget', 'manage');
  VkProfileField: array[TVkProfileField] of string = (
    'photo_id', 'verified', 'sex', 'bdate', 'city', 'country', 'home_town', 'has_photo', 'photo_50',
    'photo_100', 'photo_200_orig', 'photo_200', 'photo_400_orig', 'photo_max', 'photo_max_orig', 'photo_big',
    'photo_medium', 'online', 'lists', 'domain', 'has_mobile', 'contacts', 'site', 'education', 'universities',
    'schools', 'status', 'last_seen', 'followers_count', 'common_count', 'occupation', 'nickname',
    'relatives', 'relation', 'personal', 'connections', 'exports', 'wall_comments', 'activities', 'interests',
    'music', 'movies', 'tv', 'books', 'games', 'about', 'quotes', 'can_post', 'can_see_all_posts',
    'can_see_audio', 'can_write_private_message', 'can_send_friend_request', 'is_favorite',
    'is_hidden_from_feed', 'timezone', 'screen_name', 'maiden_name', 'crop_photo', 'is_friend',
    'friend_status', 'career', 'military', 'blacklisted', 'blacklisted_by_me', 'can_be_invited_group',
    'online_mobile', 'counters', 'first_name_nom', 'first_name_gen', 'first_name_dat', 'first_name_acc',
    'first_name_ins', 'first_name_abl', 'last_name_nom', 'last_name_gen', 'last_name_dat', 'last_name_acc',
    'last_name_ins', 'last_name_abl', 'wall_default');
  VKPostLinkButton: array[TVkPostLinkButton] of string = (
    'auto', 'app_join', 'app_game_join', 'open_url', 'open', 'more', 'call', 'book', 'enroll', 'register', 'buy',
    'buy_ticket', 'order', 'create', 'install', 'contact', 'fill', 'join_public', 'join_event', 'join', 'im',
    'im2', 'begin', 'get', 'watch', 'download', 'participate', 'play', 'apply', 'get_an_offer', 'to_write', 'reply');
  VkGroupField: array[TVkGroupField] of string = ('city', 'country', 'place', 'description', 'wiki_page', 'members_count',
    'counters', 'start_date', 'finish_date', 'can_post', 'can_see_all_posts', 'activity', 'status',
    'contacts', 'links', 'fixed_post', 'verified', 'site', 'can_create_topic', 'photo_50', 'photo_100', 'photo_200',
    'cover', 'addresses', 'age_limits', 'ban_info', 'can_message', 'can_upload_doc', 'can_upload_video',
    'crop_photo', 'has_photo', 'is_favorite', 'is_hidden_from_feed', 'is_messages_blocked');
  VkGroupFilter: array[TVkGroupFilter] of string = ('admin', 'editor', 'moder', 'advertiser', 'groups', 'publics',
    'events', 'hasAddress');
  VkGroupRole: array[TVkGroupRole] of string = ('moderator', 'editor', 'administrator', 'advertiser');
  VkGroupMembersFilter: array[TVkGroupMembersFilter] of string = ('friends', 'unsure', 'managers', 'donut');
  VkCounterFilter: array[TVkCounterFilter] of string = ('friends', 'messages', 'photos', 'videos', 'notes',
    'gifts', 'events', 'groups', 'notifications', 'sdk', 'app_requests', 'friends_recommendations');
  VkInfoFilter: array[TVkInfoFilter] of string = ('country', 'https_required', 'own_posts_default', 'no_wall_replies',
    'intro', 'lang');
  VkCurrencyId: array[TVkCurrency] of Integer = (643, 980, 398, 978, 840);
  VkGroupAddressField: array[TVkGroupAddressField] of string = ('title', 'address', 'additional_address', 'country_id',
    'city_id', 'metro_station_id', 'latitude', 'longitude', 'work_info_status', 'time_offset');
  VkGroupTagColors: array of TVkGroupTagColor = ['4bb34b', '5c9ce6', 'e64646', '792ec0', '63b9ba', 'ffa000', 'ffc107',
    '76787a', '9e8d6b', '45678f', '539b9c', '454647', '7a6c4f', '6bc76b', '5181b8', 'ff5c5c', 'a162de', '7ececf',
    'aaaeb3', 'bbaa84'];
  VkDeactivated: array[TVkDeactivated] of string = ('', 'deleted', 'banned');
  VkNameRequestStatus: array[TVkNameRequestStatus] of string = ('processing', 'declined', 'response', 'response_with_link');
  VkMessageActionType: array[TVkMessageActionType] of string = ('', 'chat_photo_update', 'chat_photo_remove',
    'chat_create', 'chat_title_update', 'chat_invite_user', 'chat_kick_user', 'chat_pin_message', 'chat_unpin_message',
    'chat_invite_user_by_link', 'conversation_style_update');
  VkKeyboardButtonColor: array[TVkKeyboardButtonColor] of string = ('default', 'positive', 'negative', 'primary',
    'secondary');
  VkKeyboardButtonColorValue: array[TVkKeyboardButtonColor] of Cardinal = (VkColorDefault, VkColorPositive,
    VkColorNegative, VkColorPrimary, VkColorSecondary);
  VkGroupStatusType: array[TVkGroupStatusType] of string = ('none', 'online', 'answer_mark');
  VkGroupTypeCreate: array[TVkGroupTypeCreate] of string = ('group', 'event', 'public');
  VkGroupType: array[TVkGroupType] of string = ('group', 'event', 'page');
  VkFlagsChangeType: array[TVkFlagsChangeType] of string = ('Replace', 'Set', 'Reset');
  VkPhotoSystemAlbum: array[TVkPhotoSystemAlbum] of string = ('wall', 'saved', 'profile');
  VkPhotoSystemAlbumId: array[TVkPhotoSystemAlbum] of Integer = (-7, -15, -6);
  VkPhotoFeedType: array[TVkPhotoFeedType] of string = ('photo', 'photo_tag');
  VkSortIdTime: array[TVkSortIdTime] of string = ('id_asc', 'id_desc', 'time_asc', 'time_desc');
  VkSort: array[TVkSort] of string = ('asc', 'desc');
  VkUserReport: array[TVkUserReport] of string = ('porn', 'spam', 'insult', 'advertisеment');
  VkLinkStatusType: array[TVkLinkStatusType] of string = ('not_banned', 'banned', 'processing');
  VkValidationType: array[TVkValidationType] of string = ('', '2fa_sms', '2fa_app');
  VkFriendsOrder: array[TVkFriendsOrder] of string = ('', 'hints', 'random', 'mobile', 'name');
  VkFavePageType: array[TVkFavePageType] of string = ('users', 'groups', 'hints');
  VkTagPosition: array[TVkTagPosition] of string = ('front', 'back');
  VkWorkInfoStatus: array[TVkWorkInfoStatus] of string = ('no_information', 'temporarily_closed', 'always_opened',
    'forever_closed', 'timetable');
  VkGroupTagAct: array[TVkGroupTagAct] of string = ('bind', 'unbind');
  VkMessageActivity: array[TVkMessageActivity] of string = ('typing', 'audiomessage');
  VkHistoryAttachment: array[TVkHistoryAttachment] of string = ('photo', 'video', 'audio', 'doc', 'link', 'market',
    'wall', 'share');
  VkMessageIntent: array[TVkMessageIntent] of string = ('default', 'promo_newsletter', 'bot_ad_invite', 'bot_ad_promo');
  VkNewsfeedIgnoreType: array[TVkNewsfeedIgnoreType] of string = ('wall', 'photo', 'tag', 'profilephoto', 'video',
    'audio');
  VkNewsfeedCommentsType: array[TVkNewsfeedCommentsType] of string = ('post', 'photo', 'video', 'topic', 'market',
    'note');
  VkNewsfeedType: array[TVkNewsfeedType] of string = ('post', 'photo', 'photo_tag', 'wall_photo', 'friend', 'note',
    'audio', 'video', 'audio_playlist');
  VkChatState: array[TVkChatState] of string = ('', 'in', 'kicked', 'left');
  VkStreamStatType: array[TVkStreamStatType] of string = ('received', 'prepared');
  VkStreamStatInterval: array[TVkStreamStatInterval] of string = ('5m', '1h', '24h');
  VkVideoType: array[TVkVideoType] of string = ('video', 'music_video', 'movie');
  VkNotificationType: array[TVkNotificationType] of string = ('wall', 'mentions', 'comments', 'likes', 'reposts',
    'followers', 'friends');
  VkOrderStateAction: array[TVkOrderStateAction] of string = ('cancel', 'charge', 'refund');
  VkSearchFilter: array[TVkSearchFilter] of string = ('friends', 'idols', 'publics', 'groups', 'events', 'correspondents',
    'mutual_friends');
  VkStatInterval: array[TVkStatInterval] of string = ('day', 'week', 'month', 'year', 'all');
  VkStatReachFilter: array[TVkStatReachFilter] of string = ('visitors', 'reach', 'activity');
  VkVideosFilter: array[TVkVideosFilter] of string = ('mp4', 'youtube', 'vimeo', 'short', 'long');
  VkSearchTarget: array[TVkSearchTarget] of string = ('friends', 'subscriptions');
  VkLinkText: array[TVkLinkText] of string = ('to_store', 'vote', 'more', 'book', 'order', 'enroll', 'fill', 'signup',
    'buy', 'ticket', 'write', 'open', 'learn_more', 'view', 'go_to', 'contact', 'watch', 'play', 'install', 'read', 'game');
  VkConversationFilter: array[TVkConversationFilter] of string = ('all', 'unread', 'important', 'unanswered');
  VkKeyboardActionType: array[TVkKeyboardActionType] of string = ('text', 'open_link', 'location', 'vkpay', 'open_app',
    'callback');
  VkLiveStatus: array[TVkLiveStatus] of string = ('waiting', 'started', 'finished', 'failed', 'upcoming');
  VkPaymentStatus: array[TVkPaymentStatus] of string = ('not_paid', 'paid', 'returned');
  VkFaveType: array[TVkFaveType] of string = ('post', 'video', 'product', 'article', 'link');
  VkMonthlyTier: array[TVkMonthlyTier] of string = ('tier_1', 'tier_2', 'tier_3', 'tier_4', 'tier_5', 'tier_6', 'unlimited');
  VkConversationDisableReason: array[TVkConversationDisableReason] of Integer = (18, 900, 901, 902, 915, 916, 917, 918, 203);
  VkPostType: array[TVkPostType] of string = ('post', 'copy', 'reply', 'postpone', 'suggest');
  VkPostSourceType: array[TVkPostSourceType] of string = ('vk', 'widget', 'api', 'rss', 'sms');
  VkAdsAccountType: array[TVkAdsAccountType] of string = ('general', 'agency');
  VkAdsAccessRole: array[TVkAdsAccessRole] of string = ('admin', 'manager', 'reports');
  VkGroupMarketState: array[TVkGroupMarketState] of string = ('none', 'basic', 'advanced');
  VkAsrState: array[TVkAsrState] of string = ('processing', 'finished', 'internal_error', 'transcoding_error', 'recognition_error');
  VkAsrModel: array[TVkAsrModel] of string = ('neutral', 'spontaneous');

function NormalizePeerId(Value: TVkPeerId): TVkPeerId;

function PeerIdIsChat(Value: TVkPeerId): Boolean;

function PeerIdIsUser(Value: TVkPeerId): Boolean;

function PeerIdIsGroup(Value: TVkPeerId): Boolean;

procedure Synchronize(Proc: TThreadProcedure);

implementation

uses
  VK.CommonUtils, System.DateUtils, System.Character, System.StrUtils;

procedure Synchronize(Proc: TThreadProcedure);
begin
  if (TThread.Current.ThreadID = MainThreadID) or IsConsole then
    Proc
  else
    TThread.Synchronize(nil, Proc);
end;

function PeerIdIsChat(Value: TVkPeerId): Boolean;
begin
  Result := Value > VK_CHAT_ID_START;
end;

function PeerIdIsUser(Value: TVkPeerId): Boolean;
begin
  Result := (Value < VK_GROUP_ID_START) and (Value > 0);
end;

function PeerIdIsGroup(Value: TVkPeerId): Boolean;
begin
  Result := (Value > VK_GROUP_ID_START) and (Value < VK_CHAT_ID_START);
end;

function NormalizePeerId(Value: TVkPeerId): TVkPeerId;
begin
  if Value > VK_CHAT_ID_START then
    Exit(Value - VK_CHAT_ID_START);
  if Value > VK_GROUP_ID_START then
    Exit(-(Value - VK_GROUP_ID_START));
  Result := Value;
end;

{ TVkFlagsChangeTypeHelper }

function TVkFlagsChangeTypeHelper.ToString: string;
begin
  Result := VkFlagsChangeType[Self];
end;

{ TVkPhotoSystemAlbumHelper }

function TVkPhotoSystemAlbumHelper.ToString: string;
begin
  Result := VkPhotoSystemAlbum[Self];
end;

function TVkPhotoSystemAlbumHelper.ToVkId: Integer;
begin
  Result := VkPhotoSystemAlbumId[Self];
end;

{ TVkPhotoFeedTypeHelper }

function TVkPhotoFeedTypeHelper.ToString: string;
begin
  Result := VkPhotoFeedType[Self];
end;

{ TMessageFlagsHelper }

class function TMessageFlagsHelper.Create(Data: Integer): TVkMessageFlags;
var
  i: TVkMessageFlag;
begin
  Result := [];
  for i := Low(VkMessageFlags) to High(VkMessageFlags) do
  begin
    if (Data - VkMessageFlags[i]) >= 0 then
    begin
      Include(Result, FlagDataToFlag(VkMessageFlags[i]));
      Data := Data - VkMessageFlags[i];
    end;
  end;
end;

class function TMessageFlagsHelper.FlagDataToFlag(FlagData: Integer): TVkMessageFlag;
var
  i: TVkMessageFlag;
begin
  Result := TVkMessageFlag.Chat;
  for i := Low(VkMessageFlags) to High(VkMessageFlags) do
    if VkMessageFlags[i] = FlagData then
      Exit(i);
end;

function TMessageFlagsHelper.ToString: string;
var
  Item: TVkMessageFlag;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkDialogFlagsHelper }

class function TVkDialogFlagsHelper.Create(Data: Integer): TVkDialogFlags;
var
  i: Integer;
begin
  Result := [];
  for i := Ord(TVkDialogFlag.Important) to Ord(TVkDialogFlag.Unanswered) do
  begin
    if (Data - VkDialogFlags[TVkDialogFlag(i)]) >= 0 then
    begin
      Include(Result, FlagDataToFlag(VkDialogFlags[TVkDialogFlag(i)]));
      Data := Data - VkDialogFlags[TVkDialogFlag(i)];
    end;
  end;
end;

class function TVkDialogFlagsHelper.FlagDataToFlag(FlagData: Integer): TVkDialogFlag;
begin
  Result := TVkDialogFlag(IndexInt(FlagData, VkDialogFlags));
end;

function TVkDialogFlagsHelper.ToString: string;
var
  Flag: TVkDialogFlag;
begin
  for Flag in Self do
    case Flag of
      TVkDialogFlag.Important:
        Result := Result + 'Important ';
      TVkDialogFlag.Unanswered:
        Result := Result + 'Unanswered ';
    end;
end;

{ TArrayOfIntegerHelper }

function TArrayOfIntegerHelper.Add(Value: Int64): Integer;
begin
  Result := System.Length(Self) + 1;
  SetLength(Self, Result);
  Self[Result - 1] := Value;
end;

function TArrayOfIntegerHelper.Contains(const Value: Int64): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

procedure TArrayOfIntegerHelper.Delete(const Value: Int64);
var
  i: Integer;
begin
  i := IndexOf(Value);
  if i >= 0 then
    System.Delete(Self, i, 1);
end;

function TArrayOfIntegerHelper.IndexOf(const Value: Int64): Integer;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
    if Self[i] = Value then
      Exit(i);
  Result := -1;
end;

function TArrayOfIntegerHelper.IsEmpty: Boolean;
begin
  Result := System.Length(Self) = 0;
end;

function TArrayOfIntegerHelper.Length: Integer;
begin
  Result := System.Length(Self);
end;

function TArrayOfIntegerHelper.ToJson: string;
var
  i: Integer;
begin
  Result := '[';
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + '"' + Self[i].ToString + '"';
  end;
  Result := Result + ']';
end;

function TArrayOfIntegerHelper.ToString: string;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + Self[i].ToString;
  end;
end;

{ TArrayOfStringHelper }

function TArrayOfStringHelper.IndexOf(const Value: string): Integer;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
    if Self[i] = Value then
      Exit(i);
  Result := -1;
end;

function TArrayOfStringHelper.IsEmpty: Boolean;
begin
  Result := System.Length(Self) = 0;
end;

function TArrayOfStringHelper.Length: Integer;
begin
  Result := System.Length(Self);
end;

function TArrayOfStringHelper.ToJson: string;
var
  i: Integer;
begin
  Result := '[';
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + '"' + Self[i] + '"';
  end;
  Result := Result + ']';
end;

function TArrayOfStringHelper.ToString: string;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if (i <> Low(Self)) and (not Self[i].IsEmpty) then
      Result := Result + ',';
    Result := Result + Self[i];
  end;
end;

function TArrayOfStringHelper.Add(const Value: string): Integer;
begin
  Result := System.Length(Self) + 1;
  SetLength(Self, Result);
  Self[Result - 1] := Value;
end;

procedure TArrayOfStringHelper.Assign(Source: TStrings);
var
  i: Integer;
begin
  SetLength(Self, Source.Count);
  for i := 0 to Source.Count - 1 do
    Self[i] := Source[i];
end;

function TArrayOfStringHelper.Contains(const Value: string): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

procedure TArrayOfStringHelper.Delete(const Value: string);
var
  i: Integer;
begin
  i := IndexOf(Value);
  if i >= 0 then
    System.Delete(Self, i, 1);
end;

{ TParamsHelper }

function TParamsHelper.AddParam(Param: TParam): TParams;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Param[0] then
    begin
      Self[i] := Param;
      Exit(Self);
    end;
  SetLength(Self, Length(Self) + 1);
  Self[High(Self)] := Param;
  Result := Self;
end;

function TParamsHelper.Add(Param: TParam): TParams;
begin
  Result := AddParam(Param);
end;

function TParamsHelper.Add(Key, Value: string): TParams;
begin
  Result := AddParam([Key, Value]);
end;

function TParamsHelper.Add(Key: string; Value: Integer): TParams;
begin
  Result := AddParam([Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: Boolean): TParams;
begin
  Result := AddParam([Key, BoolToString(Value)]);
end;

function TParamsHelper.Add(Key: string; Value: TArrayOfInteger): TParams;
begin
  Result := AddParam([Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: TDateTime; Format: string): TParams;
begin
  if Format.IsEmpty then
    Result := AddParam([Key, DateTimeToUnix(Value, False).ToString])
  else
    Result := AddParam([Key, FormatDateTime(Format, Value)]);
end;

function TParamsHelper.Add(Key: string; Value: Extended): TParams;
begin
  Result := AddParam([Key, Value.ToString]);
end;

function TParamsHelper.GetValue(Key: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
      Exit(Self[i][1]);
end;

function TParamsHelper.Add(Key: string; Value: TArrayOfString): TParams;
begin
  Result := AddParam([Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: TAttachmentArray): TParams;
begin
  Result := AddParam([Key, Value.ToStrings.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: TVkEntity): TParams;
begin
  Result := AddParam([Key, Value.ToJsonString]);
end;

function TParamsHelper.Add(Key: string; Value: TAttachment): TParams;
begin
  Result := AddParam([Key, Value.ToString]);
end;

function TParamsHelper.KeyExists(Key: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
      Exit(True);
end;

function TParamsHelper.Remove(Key: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Self) to High(Self) do
    if Self[i][0] = Key then
    begin
      Delete(Self, i, 1);
      Break;
    end;
end;

{ TVkAudioGenreHelper }

class function TVkAudioGenreHelper.Create(Value: Integer): TVkAudioGenre;
begin
  Result := TVkAudioGenre(IndexInt(Value, VkAudioGenres));
end;

function TVkAudioGenreHelper.ToConst: Integer;
begin
  Result := VkAudioGenres[Self];
end;

function TVkAudioGenreHelper.ToString: string;
begin
  Result := VkAudioGenresStr[Self];
end;

{ TVkGroupJoinTypeHelper }

class function TVkGroupJoinTypeHelper.Create(Value: string): TVkGroupJoinType;
begin
  Result := TVkGroupJoinType(IndexStr(Value, VkGroupJoinType));
end;

function TVkGroupJoinTypeHelper.ToString: string;
begin
  Result := VkGroupJoinType[Self];
end;

{ TVkUserBlockReasonHelper }

function TVkUserBlockReasonHelper.ToString: string;
begin
  Result := VkUserBlockReason[Self];
end;

{ TVkGroupLevelHelper }

function TVkGroupLevelHelper.ToString: string;
begin
  Result := VkGroupLevel[Self];
end;

{ TVkItemTypeHelper }

function TVkItemTypeHelper.ToString: string;
begin
  Result := VkItemType[Self];
end;

{ TVkNameCaseHelper }

function TVkNameCaseHelper.ToString: string;
begin
  Result := VkNameCase[Self];
end;

{ TVkAttachmentTypeHelper }

class function TVkAttachmentTypeHelper.Create(Value: string): TVkAttachmentType;
begin
  Result := TVkAttachmentType(IndexStr(Value, VkAttachmentType));
end;

function TVkAttachmentTypeHelper.ToString: string;
begin
  Result := VkAttachmentType[Self];
end;

{ TVkPeerTypeHelper }

class function TVkPeerTypeHelper.Create(Value: string): TVkPeerType;
begin
  Result := TVkPeerType(IndexStr(Value, VkPeerType));
end;

class function TVkPeerTypeHelper.Create(PeerId: Integer): TVkPeerType;
begin
  if PeerIdIsChat(PeerId) then
    Exit(TVkPeerType.Chat);
  if PeerIdIsUser(PeerId) then
    Exit(TVkPeerType.User);
  if PeerIdIsGroup(PeerId) then
    Exit(TVkPeerType.Group);
  Result := TVkPeerType.Unknown;
end;

function TVkPeerTypeHelper.ToString: string;
begin
  Result := VkPeerType[Self];
end;

{ TVkPostTypeFilterHelper }

function TVkPostTypeFilterHelper.ToString: string;
begin
  Result := VkPostTypeFilter[Self];
end;

{ TVkPermissionHelper }

function TVkPermissionHelper.ToString: string;
begin
  Result := VkPremissionStr[Self];
end;

{ TVkPermissionsHelper }

procedure TVkPermissionsHelper.Include(Value: TVkPermission);
begin
  System.Include(Self, Value);
end;

function TVkPermissionsHelper.ToString: string;
var
  Item: TVkPermission;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkGroupFieldHelper }

function TVkGroupFieldHelper.ToString: string;
begin
  Result := VkGroupField[Self];
end;

{ TVkGroupFieldsHelper }

class function TVkGroupFieldsHelper.All: TVkGroupFields;
begin
  Result := [TVkGroupField.City, TVkGroupField.Country, TVkGroupField.Place,
    TVkGroupField.Description, TVkGroupField.WikiPage, TVkGroupField.MembersCount,
    TVkGroupField.Counters, TVkGroupField.StartDate, TVkGroupField.FinishDate,
    TVkGroupField.CanPost, TVkGroupField.CanSeeAllPosts, TVkGroupField.Activity,
    TVkGroupField.Status, TVkGroupField.Contacts, TVkGroupField.Links,
    TVkGroupField.FixedPost, TVkGroupField.Verified, TVkGroupField.Site,
    TVkGroupField.CanCreateTopic, TVkGroupField.Photo50, TVkGroupField.Photo100,
    TVkGroupField.Photo200, TVkGroupField.Cover];
end;

function TVkGroupFieldsHelper.ToString: string;
var
  Item: TVkGroupField;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkProfileFieldsHelper }

class function TVkProfileFieldsHelper.All: TVkProfileFields;
begin
  Result := [TVkProfileField.PhotoId, TVkProfileField.Verified, TVkProfileField.Sex,
    TVkProfileField.BirthDate, TVkProfileField.City, TVkProfileField.Country,
    TVkProfileField.HomeTown, TVkProfileField.HasPhoto, TVkProfileField.Photo50,
    TVkProfileField.Photo100, TVkProfileField.Photo200Orig, TVkProfileField.Photo200,
    TVkProfileField.Photo400Orig, TVkProfileField.PhotoMax, TVkProfileField.PhotoMaxOrig,
    TVkProfileField.Online, TVkProfileField.Domain, TVkProfileField.HasMobile,
    TVkProfileField.Contacts, TVkProfileField.Site, TVkProfileField.Education,
    TVkProfileField.Universities, TVkProfileField.Schools, TVkProfileField.Status,
    TVkProfileField.LastSeen, TVkProfileField.FollowersCount, TVkProfileField.CommonCount,
    TVkProfileField.Occupation, TVkProfileField.Nickname, TVkProfileField.Relatives,
    TVkProfileField.Relation, TVkProfileField.Personal, TVkProfileField.Connections,
    TVkProfileField.&Exports, TVkProfileField.Activities, TVkProfileField.Interests,
    TVkProfileField.Music, TVkProfileField.Movies, TVkProfileField.TV, TVkProfileField.Books,
    TVkProfileField.Games, TVkProfileField.About, TVkProfileField.Quotes,
    TVkProfileField.CanPost, TVkProfileField.CanSeeAllPosts, TVkProfileField.CanSeeAudio,
    TVkProfileField.CanWritePrivateMessage, TVkProfileField.CanSendFriendRequest,
    TVkProfileField.IsFavorite, TVkProfileField.IsHiddenFromFeed, TVkProfileField.TimeZone,
    TVkProfileField.ScreenName, TVkProfileField.MaidenName, TVkProfileField.CropPhoto,
    TVkProfileField.IsFriend, TVkProfileField.FriendStatus, TVkProfileField.Career,
    TVkProfileField.Military, TVkProfileField.Blacklisted, TVkProfileField.BlacklistedByMe,
    TVkProfileField.CanBeInvitedGroup, TVkProfileField.OnlineMobile, TVkProfileField.Counters,
    TVkProfileField.FirstNameNom, TVkProfileField.FirstNameGen, TVkProfileField.FirstNameDat,
    TVkProfileField.FirstNameAcc, TVkProfileField.FirstNameIns, TVkProfileField.FirstNameAbl,
    TVkProfileField.LastNameNom, TVkProfileField.LastNameGen, TVkProfileField.LastNameDat,
    TVkProfileField.LastNameAcc, TVkProfileField.LastNameIns, TVkProfileField.LastNameAbl,
    TVkProfileField.WallDefault];
end;

class function TVkProfileFieldsHelper.AllForGroup: TVkProfileFields;
begin
  Result := [TVkProfileField.PhotoId, TVkProfileField.Verified, TVkProfileField.Sex,
    TVkProfileField.BirthDate, TVkProfileField.City, TVkProfileField.Country,
    TVkProfileField.HomeTown, TVkProfileField.HasPhoto, TVkProfileField.Photo50,
    TVkProfileField.Photo100, TVkProfileField.Photo200Orig, TVkProfileField.Photo200,
    TVkProfileField.Photo400Orig, TVkProfileField.PhotoMax, TVkProfileField.PhotoMaxOrig,
    TVkProfileField.Online, TVkProfileField.Domain, TVkProfileField.HasMobile,
    TVkProfileField.Contacts, TVkProfileField.Site, TVkProfileField.Education,
    TVkProfileField.Universities, TVkProfileField.Schools, TVkProfileField.Status,
    TVkProfileField.LastSeen, TVkProfileField.FollowersCount,
    TVkProfileField.Occupation, TVkProfileField.Nickname, TVkProfileField.Relatives,
    TVkProfileField.Relation, TVkProfileField.Personal, TVkProfileField.Connections,
    TVkProfileField.&Exports, TVkProfileField.Activities, TVkProfileField.Interests,
    TVkProfileField.Music, TVkProfileField.Movies, TVkProfileField.TV, TVkProfileField.Books,
    TVkProfileField.Games, TVkProfileField.About, TVkProfileField.Quotes,
    TVkProfileField.CanPost, TVkProfileField.CanSeeAllPosts, TVkProfileField.CanSeeAudio,
    TVkProfileField.CanWritePrivateMessage, TVkProfileField.CanSendFriendRequest,
    TVkProfileField.IsFavorite, TVkProfileField.IsHiddenFromFeed, TVkProfileField.TimeZone,
    TVkProfileField.ScreenName, TVkProfileField.MaidenName, TVkProfileField.CropPhoto,
    TVkProfileField.IsFriend, TVkProfileField.FriendStatus, TVkProfileField.Career,
    TVkProfileField.Military, TVkProfileField.Blacklisted, TVkProfileField.BlacklistedByMe,
    TVkProfileField.CanBeInvitedGroup, TVkProfileField.OnlineMobile, TVkProfileField.Counters];
end;

function TVkProfileFieldsHelper.ToString: string;
var
  Item: TVkProfileField;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkProfileFieldHelper }

function TVkProfileFieldHelper.ToString: string;
begin
  Result := VkProfileField[Self];
end;

{ TVkGroupFilterHelper }

function TVkGroupFilterHelper.ToString: string;
begin
  Result := VkGroupFilter[Self];
end;

{ TVkGroupFiltersHelper }

class function TVkGroupFiltersHelper.All: TVkGroupFilters;
begin
  Result := [TVkGroupFilter.Admin, TVkGroupFilter.Editor, TVkGroupFilter.Moder,
    TVkGroupFilter.Advertiser, TVkGroupFilter.Groups, TVkGroupFilter.Publics,
    TVkGroupFilter.Events, TVkGroupFilter.HasAddress];
end;

function TVkGroupFiltersHelper.ToString: string;
var
  Item: TVkGroupFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkGroupMembersFilterHelper }

function TVkGroupMembersFilterHelper.ToString: string;
begin
  Result := VkGroupMembersFilter[Self];
end;

{ TVkSortIdTimeHelper }

function TVkSortIdTimeHelper.ToString: string;
begin
  Result := VkSortIdTime[Self];
end;

{ TVkCounterFilterHelper }

function TVkCounterFilterHelper.ToString: string;
begin
  Result := VkCounterFilter[Self];
end;

{ TVkCounterFiltersHelper }

function TVkCounterFiltersHelper.ToString: string;
var
  Item: TVkCounterFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkInfoFilterHelper }

function TVkInfoFilterHelper.ToString: string;
begin
  Result := VkInfoFilter[Self];
end;

{ TVkInfoFiltersHelper }

function TVkInfoFiltersHelper.ToString: string;
var
  Item: TVkInfoFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkGroupTypeCreateHelper }

class function TVkGroupTypeCreateHelper.Create(const Value: string): TVkGroupTypeCreate;
begin
  Result := TVkGroupTypeCreate(IndexStr(Value, VkGroupTypeCreate));
end;

function TVkGroupTypeCreateHelper.ToString: string;
begin
  Result := VkGroupTypeCreate[Self];
end;

{ TVkGroupTypeHelper }

class function TVkGroupTypeHelper.Create(const Value: string): TVkGroupType;
begin
  Result := TVkGroupType(IndexStr(Value, VkGroupType));
end;

function TVkGroupTypeHelper.ToString: string;
begin
  Result := VkGroupType[Self];
end;

{ TVkMarketCurrencyHelper }

function TVkMarketCurrencyHelper.ToConst: Integer;
begin
  Result := VkCurrencyId[Self];
end;

{ TVkGroupRoleHelper }

function TVkGroupRoleHelper.ToString: string;
begin
  Result := VkGroupRole[Self];
end;

{ TVkGroupAddressFieldHelper }

function TVkGroupAddressFieldHelper.ToString: string;
begin
  Result := VkGroupAddressField[Self];
end;

{ TVkGroupAddressFieldsHelper }

class function TVkGroupAddressFieldsHelper.All: TVkGroupAddressFields;
begin
  Result := [TVkGroupAddressField.Title, TVkGroupAddressField.Address,
    TVkGroupAddressField.AdditionalAddress, TVkGroupAddressField.CountryId,
    TVkGroupAddressField.CityId, TVkGroupAddressField.MetroStationId,
    TVkGroupAddressField.Latitude, TVkGroupAddressField.Longitude,
    TVkGroupAddressField.WorkInfoStatus, TVkGroupAddressField.TimeOffset];
end;

function TVkGroupAddressFieldsHelper.ToString: string;
var
  Item: TVkGroupAddressField;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkSortHelper }

function TVkSortHelper.ToString: string;
begin
  Result := VkSort[Self];
end;

{ TVkMediaReportReasonHelper }

function TVkMediaReportReasonHelper.ToString: string;
begin
  Result := VkPhotoReportReason[Self];
end;

{ TVkMethodException }

constructor TVkMethodException.Create(const Msg: string; Code: Integer);
begin
  inherited Create(Msg);
  FCode := Code;
end;

{ TVkUserReportHelper }

function TVkUserReportHelper.ToString: string;
begin
  Result := VkUserReport[Self];
end;

{ TVkPostLinkButtonHelper }

function TVkPostLinkButtonHelper.ToString: string;
begin
  Result := VKPostLinkButton[Self];
end;

{ TVkLinkStatusTypeHelper }

class function TVkLinkStatusTypeHelper.FromString(Value: string): TVkLinkStatusType;
begin
  Result := TVkLinkStatusType(IndexStr(Value, VkLinkStatusType));
end;

function TVkLinkStatusTypeHelper.ToString: string;
begin
  Result := VkLinkStatusType[Self];
end;

{ TVkValidationTypeHelper }

class function TVkValidationTypeHelper.FromString(const Value: string): TVkValidationType;
begin
  Result := TVkValidationType(IndexStr(Value, VkValidationType));
end;

{ TVkMessageFlagHelper }

function TVkMessageFlagHelper.ToString: string;
begin
  Result := VkMessageFlagTypes[Self];
end;

{ TVkMessageActionTypeHelper }

class function TVkMessageActionTypeHelper.Create(const Value: string): TVkMessageActionType;
begin
  Result := TVkMessageActionType(IndexStr(Value, VkMessageActionType));
end;

function TVkMessageActionTypeHelper.ToString: string;
begin
  Result := VkMessageActionType[Self];
end;

{ TVkPlatformHelper }

class function TVkPlatformHelper.Create(const Value: string): TVkPlatform;
begin
  Result := TVkPlatform(IndexStr(Value, VkPlatformsType));
end;

function TVkPlatformHelper.ToString: string;
begin
  Result := VkPlatformsType[Self];
end;

{ TVkNameRequestStatusHelper }

class function TVkNameRequestStatusHelper.Create(const Value: string): TVkNameRequestStatus;
begin
  Result := TVkNameRequestStatus(IndexStr(Value, VkNameRequestStatus));
end;

function TVkNameRequestStatusHelper.ToString: string;
begin
  Result := VkNameRequestStatus[Self];
end;

{ TVkDeactivatedHelper }

class function TVkDeactivatedHelper.Create(const Value: string): TVkDeactivated;
begin
  Result := TVkDeactivated(IndexStr(Value, VkDeactivated));
end;

function TVkDeactivatedHelper.ToString: string;
begin
  Result := VkDeactivated[Self];
end;

{ TVkKeyboardButtonColorHelper }

class function TVkKeyboardButtonColorHelper.Create(Value: string): TVkKeyboardButtonColor;
begin
  Result := TVkKeyboardButtonColor(IndexStr(Value, VkKeyboardButtonColor));
end;

function TVkKeyboardButtonColorHelper.ToColor: Cardinal;
begin
  Result := VkKeyboardButtonColorValue[Self];
end;

function TVkKeyboardButtonColorHelper.ToString: string;
begin
  Result := VkKeyboardButtonColor[Self];
end;

{ TAttachment }

class function TAttachment.Create(&Type: TVkAttachmentType; OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result.&Type := &Type;
  Result.OwnerId := OwnerId;
  Result.Id := Id;
  Result.AccessKey := AccessKey;
end;

class function TAttachment.Audio(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Audio, OwnerId, Id, AccessKey);
end;

class function TAttachment.Doc(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Doc, OwnerId, Id, AccessKey);
end;

class function TAttachment.Gift(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Gift, OwnerId, Id, AccessKey);
end;

class function TAttachment.Link(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Link, OwnerId, Id, AccessKey);
end;

class function TAttachment.Market(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Market, OwnerId, Id, AccessKey);
end;

class function TAttachment.MarketAlbum(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.MarketAlbum, OwnerId, Id, AccessKey);
end;

class function TAttachment.Photo(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Photo, OwnerId, Id, AccessKey);
end;

class function TAttachment.Poll(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Poll, OwnerId, Id, AccessKey);
end;

class function TAttachment.Sticker(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Sticker, OwnerId, Id, AccessKey);
end;

class function TAttachment.Video(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Video, OwnerId, Id, AccessKey);
end;

class function TAttachment.Wall(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Wall, OwnerId, Id, AccessKey);
end;

class function TAttachment.WallReply(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.WallReply, OwnerId, Id, AccessKey);
end;

class function TAttachment.Album(OwnerId, Id: Integer; const AccessKey: string): TAttachment;
begin
  Result := Create(TVkAttachmentType.Album, OwnerId, Id, AccessKey);
end;

function TAttachment.ToString: string;
begin
  case Kind of
    TAttachmentKind.Media:
      begin
        Result := &Type.ToString + OwnerId.ToString + '_' + Id.ToString;
        if not AccessKey.IsEmpty then
          Result := Result + '_' + AccessKey;
      end;
    TAttachmentKind.Link:
      begin
        Result := Value;
      end;
    TAttachmentKind.Tel:
      begin
        Result := Value;
      end;
  end;
end;

class operator TAttachment.Implicit(const Value: TAttachment): string;
begin
  Result := Value.ToString;
end;

function TAttachment.IsEmpty: Boolean;
begin
  Result := Id = 0;
end;

class operator TAttachment.Implicit(const Value: string): TAttachment;
var
  i: Integer;
  tmp, tmpValue: string;
begin
  Result.OwnerId := 0;
  Result.Id := 0;
  Result.AccessKey := '';
  Result.&Type := TVkAttachmentType.Unknown;

  tmp := Value.ToLower;

  //http://habrahabr.ru
  if tmp.StartsWith('http') then
  begin
    Result.Kind := TAttachmentKind.Link;
    Result.Value := Value;
    Exit;
  end;

  //tel:+71234567890
  if tmp.StartsWith('tel:') then
  begin
    Result.Kind := TAttachmentKind.Tel;
    Result.Value := Value;
    Exit;
  end;

  //video58553419_456239240_wefq76wegq7we
  Result.Kind := TAttachmentKind.Media;
  Result.Value := '';
  tmp := '';
  tmpValue := Value + '_';
  for i := 0 to Pred(tmpValue.Length) do
  begin
    if Result.&Type = TVkAttachmentType.Unknown then
    begin
      if tmpValue.Chars[i].IsDigit then
      begin
        Result.&Type := TVkAttachmentType.Create(tmp);
        tmp := '';
      end;
    end
    else if Result.OwnerId = 0 then
    begin
      if tmpValue.Chars[i] = '_' then
      begin
        Result.OwnerId := StrToIntDef(tmp, 0);
        tmp := '';
        Continue;
      end;
    end
    else if Result.Id = 0 then
    begin
      if tmpValue.Chars[i] = '_' then
      begin
        Result.Id := StrToIntDef(tmp, 0);
        tmp := '';
        Continue;
      end;
    end
    else if Result.AccessKey.IsEmpty then
    begin
      if tmpValue.Chars[i] = '_' then
      begin
        Result.AccessKey := tmp;
        tmp := '';
        Continue;
      end;
    end;
    tmp := tmp + tmpValue.Chars[i];
  end;
end;

{ TAttachmentArrayHelper }

function TAttachmentArrayHelper.GetByKind(const Kind: TAttachmentKind; var Attachment: TAttachment): Boolean;
begin
  for var Item in Self do
    if Item.Kind = Kind then
    begin
      Attachment := Item;
      Exit(True);
    end;
  Result := False;
end;

function TAttachmentArrayHelper.GetByType(const&Type: TVkAttachmentType; var Attachment: TAttachment): Boolean;
begin
  for var Item in Self do
    if Item.&Type = &Type then
    begin
      Attachment := Item;
      Exit(True);
    end;
  Result := False;
end;

function TAttachmentArrayHelper.IsEmpty: Boolean;
begin
  Result := Length(Self) = 0;
end;

function TAttachmentArrayHelper.ToStrings: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, Length(Self));
  for i := Low(Self) to High(Self) do
    Result[i] := Self[i];
end;

{ TVkGroupStatusTypeHelper }

class function TVkGroupStatusTypeHelper.Create(const Value: string): TVkGroupStatusType;
begin
  Result := TVkGroupStatusType(IndexStr(Value, VkGroupStatusType));
end;

function TVkGroupStatusTypeHelper.ToString: string;
begin
  Result := VkGroupStatusType[Self];
end;

{ TVkFriendsOrderHelper }

function TVkFriendsOrderHelper.ToString: string;
begin
  Result := VkFriendsOrder[Self];
end;

{ TVkFavePageTypeHelper }

function TVkFavePageTypeHelper.ToString: string;
begin
  Result := VkFavePageType[Self];
end;

{ TVkTagPositionHelper }

function TVkTagPositionHelper.ToString: string;
begin
  Result := VkTagPosition[Self];
end;

{ TVkWorkInfoStatusHelper }

function TVkWorkInfoStatusHelper.ToString: string;
begin
  Result := VkWorkInfoStatus[Self];
end;

{ TVkGroupTagActHelper }

function TVkGroupTagActHelper.ToString: string;
begin
  Result := VkGroupTagAct[Self];
end;

{ TVkMessageActivityHelper }

function TVkMessageActivityHelper.ToString: string;
begin
  Result := VkMessageActivity[Self];
end;

{ TVkHistoryAttachmentHelper }

function TVkHistoryAttachmentHelper.ToString: string;
begin
  Result := VkHistoryAttachment[Self];
end;

{ TVkMessageIntentHelper }

function TVkMessageIntentHelper.ToString: string;
begin
  Result := VkMessageIntent[Self];
end;

{ TVkNewsfeedIgnoreTypeHelper }

function TVkNewsfeedIgnoreTypeHelper.ToString: string;
begin
  Result := VkNewsfeedIgnoreType[Self];
end;

{ TVkNewsfeedCommentsTypeHelper }

function TVkNewsfeedCommentsTypeHelper.ToString: string;
begin
  Result := VkNewsfeedCommentsType[Self];
end;

{ TVkNewsfeedCommentsTypesHelper }

function TVkNewsfeedCommentsTypesHelper.ToString: string;
var
  Item: TVkNewsfeedCommentsType;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkNewsfeedTypeHelper }

class function TVkNewsfeedTypeHelper.Create(Value: string): TVkNewsfeedType;
begin
  Result := TVkNewsfeedType(IndexStr(Value, VkNewsfeedType));
end;

function TVkNewsfeedTypeHelper.ToString: string;
begin
  Result := VkNewsfeedType[Self];
end;

{ TVkNewsfeedTypesHelper }

function TVkNewsfeedTypesHelper.ToString: string;
var
  Item: TVkNewsfeedType;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkNotificationTypeHelper }

function TVkNotificationTypeHelper.ToString: string;
begin
  Result := VkNotificationType[Self];
end;

{ TVkNotificationFilterHelper }

function TVkNotificationFilterHelper.ToString: string;
var
  Item: TVkNotificationType;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkOrderStateActionHelper }

function TVkOrderStateActionHelper.ToString: string;
begin
  Result := VkOrderStateAction[Self];
end;

{ TVkSearchFilterHelper }

function TVkSearchFilterHelper.ToString: string;
begin
  Result := VkSearchFilter[Self];
end;

{ TVkSearchFiltersHelper }

function TVkSearchFiltersHelper.ToString: string;
var
  Item: TVkSearchFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkStatIntervalHelper }

function TVkStatIntervalHelper.ToString: string;
begin
  Result := VkStatInterval[Self];
end;

{ TVkVideosFilterHelper }

function TVkVideosFilterHelper.ToString: string;
begin
  Result := VkVideosFilter[Self];
end;

{ TVkVideosFiltersHelper }

class function TVkVideosFiltersHelper.All: TVkVideosFilters;
begin
  Result := [TVkVideosFilter.MP4, TVkVideosFilter.YouTube,
    TVkVideosFilter.Vimeo, TVkVideosFilter.Short, TVkVideosFilter.Long];
end;

function TVkVideosFiltersHelper.ToString: string;
var
  Item: TVkVideosFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkSearchTargetHelper }

function TVkSearchTargetHelper.ToString: string;
begin
  Result := VkSearchTarget[Self];
end;

{ TVkSearchTargetsHelper }

function TVkSearchTargetsHelper.ToString: string;
var
  Item: TVkSearchTarget;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
end;

{ TVkLinkTextHelper }

function TVkLinkTextHelper.ToString: string;
begin
  Result := VkLinkText[Self];
end;

{ TVkStatReachFilterHelper }

function TVkStatReachFilterHelper.ToString: string;
begin
  Result := VkStatReachFilter[Self];
end;

{ TVkStatReachFiltersHelper }

function TVkStatReachFiltersHelper.ToString: string;
var
  Item: TVkStatReachFilter;
begin
  for Item in Self do
    Result := Result + Item.ToString + ',';
  Result.TrimRight([',']);
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

{ TVkPrivacySettings }

procedure TVkPrivacySettings.Add(const Value: string);
begin
  if FStr.IndexOf(Value) < 0 then
    FStr.Add(Value);
end;

procedure TVkPrivacySettings.Add(const Value: Integer);
begin
  if FInt.IndexOf(Value) < 0 then
    FInt.Add(Value);
end;

procedure TVkPrivacySettings.Clear;
begin
  SetLength(FInt, 0);
  SetLength(FStr, 0);
end;

procedure TVkPrivacySettings.Delete(const Value: string);
begin
  FStr.Delete(Value);
end;

procedure TVkPrivacySettings.Delete(const Value: Integer);
begin
  FInt.Delete(Value);
end;

class function TVkPrivacySettings.Empty: TVkPrivacySettings;
begin
  //empty
end;

function TVkPrivacySettings.IsEmpty: Boolean;
begin
  Result := Length(FInt) + Length(FStr) > 0;
end;

function TVkPrivacySettings.ToString: string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(FInt) to High(FInt) do
    Result := Result + FInt[i].ToString + ',';
  for i := Low(FStr) to High(FStr) do
    Result := Result + FStr[i] + ',';
  Result.TrimRight([',']);
end;

{ TVkConversationFilterHelper }

function TVkConversationFilterHelper.ToString: string;
begin
  Result := VkConversationFilter[Self];
end;

{ TVkKeyboardActionTypeHelper }

class function TVkKeyboardActionTypeHelper.Create(Value: string): TVkKeyboardActionType;
begin
  Result := TVkKeyboardActionType(IndexStr(Value, VkKeyboardActionType));
end;

function TVkKeyboardActionTypeHelper.ToString: string;
begin
  Result := VkKeyboardActionType[Self];
end;

{ TVkMessageDelete }

constructor TVkMessageDelete.Create;
begin
  inherited;
  FItems := TDictionary<string, Boolean>.Create;
end;

destructor TVkMessageDelete.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TVkChatStateHelper }

class function TVkChatStateHelper.Create(Value: string): TVkChatState;
begin
  Result := TVkChatState(IndexStr(Value, VkChatState));
end;

function TVkChatStateHelper.ToString: string;
begin
  Result := VkChatState[Self];
end;

{ TVkStreamStatTypeHelper }

class function TVkStreamStatTypeHelper.Create(Value: string): TVkStreamStatType;
begin
  Result := TVkStreamStatType(IndexStr(Value, VkStreamStatType));
end;

function TVkStreamStatTypeHelper.ToString: string;
begin
  Result := VkStreamStatType[Self];
end;

{ TVkStreamStatIntervalHelper }

class function TVkStreamStatIntervalHelper.Create(Value: string): TVkStreamStatInterval;
begin
  Result := TVkStreamStatInterval(IndexStr(Value, VkStreamStatInterval));
end;

function TVkStreamStatIntervalHelper.ToString: string;
begin
  Result := VkStreamStatInterval[Self];
end;

{ TVkVideoTypeHelper }

class function TVkVideoTypeHelper.Create(const Value: string): TVkVideoType;
begin
  Result := TVkVideoType(IndexStr(Value, VkVideoType));
end;

function TVkVideoTypeHelper.ToString: string;
begin
  Result := VkVideoType[Self];
end;

{ TVkLiveStatusHelper }

class function TVkLiveStatusHelper.Create(const Value: string): TVkLiveStatus;
begin
  Result := TVkLiveStatus(IndexStr(Value, VkLiveStatus));
end;

function TVkLiveStatusHelper.ToString: string;
begin
  Result := VkLiveStatus[Self];
end;

{ TVkPaymentStatusHelper }

class function TVkPaymentStatusHelper.Create(const Value: string): TVkPaymentStatus;
begin
  Result := TVkPaymentStatus(IndexStr(Value, VkPaymentStatus));
end;

function TVkPaymentStatusHelper.ToString: string;
begin
  Result := VkPaymentStatus[Self];
end;

{ TVkFaveTypeHelper }

class function TVkFaveTypeHelper.Create(const Value: string): TVkFaveType;
begin
  Result := TVkFaveType(IndexStr(Value, VkFaveType));
end;

function TVkFaveTypeHelper.ToString: string;
begin
  Result := VkFaveType[Self];
end;

{ TVkMonthlyTierHelper }

class function TVkMonthlyTierHelper.Create(const Value: string): TVkMonthlyTier;
begin
  Result := TVkMonthlyTier(IndexStr(Value, VkMonthlyTier));
end;

function TVkMonthlyTierHelper.ToString: string;
begin
  Result := VkMonthlyTier[Self];
end;

{ TVkConversationDisableReasonHelper }

class function TVkConversationDisableReasonHelper.Create(Value: string): TVkConversationDisableReason;
begin
  Result := TVkConversationDisableReason(IndexInt(StrToIntDef(Value, 18), VkConversationDisableReason));
end;

function TVkConversationDisableReasonHelper.ToString: string;
begin
  Result := VkConversationDisableReason[Self].ToString;
end;

{ TVkPostTypeHelper }

class function TVkPostTypeHelper.Create(const Value: string): TVkPostType;
begin
  Result := TVkPostType(IndexStr(Value, VkPostType));
end;

function TVkPostTypeHelper.ToString: string;
begin
  Result := VkPostType[Self];
end;

{ TVkPostSourceTypeHelper }

class function TVkPostSourceTypeHelper.Create(const Value: string): TVkPostSourceType;
begin
  Result := TVkPostSourceType(IndexStr(Value, VkPostSourceType));
end;

function TVkPostSourceTypeHelper.ToString: string;
begin
  Result := VkPostSourceType[Self];
end;

{ TVkAdsAccountTypeHelper }

class function TVkAdsAccountTypeHelper.Create(Value: string): TVkAdsAccountType;
begin
  Result := TVkAdsAccountType(IndexStr(Value, VkAdsAccountType));
end;

function TVkAdsAccountTypeHelper.ToString: string;
begin
  Result := VkAdsAccountType[Self];
end;

{ TVkGroupMarketStateHelper }

class function TVkGroupMarketStateHelper.Create(Value: string): TVkGroupMarketState;
begin
  Result := TVkGroupMarketState(IndexStr(Value, VkGroupMarketState));
end;

function TVkGroupMarketStateHelper.ToString: string;
begin
  Result := VkGroupMarketState[Self];
end;

{ TVkAdsAccessRoleHelper }

class function TVkAdsAccessRoleHelper.Create(Value: string): TVkAdsAccessRole;
begin
  Result := TVkAdsAccessRole(IndexStr(Value, VkAdsAccessRole));
end;

function TVkAdsAccessRoleHelper.ToString: string;
begin
  Result := VkAdsAccessRole[Self];
end;

{ TVkPeerHelper }

function TVkPeerHelper.IsChat: Boolean;
begin
  Result := PeerIdIsChat(Self);
end;

function TVkPeerHelper.IsGroup: Boolean;
begin
  Result := PeerIdIsGroup(Self);
end;

function TVkPeerHelper.IsUser: Boolean;
begin
  Result := PeerIdIsUser(Self);
end;

function TVkPeerHelper.PeerType: TVkPeerType;
begin
  if IsUser then
    Exit(TVkPeerType.User)
  else if IsChat then
    Exit(TVkPeerType.Chat)
  else if IsGroup then
    Exit(TVkPeerType.Group)
  else
    Exit(TVkPeerType.Unknown);
end;

function TVkPeerHelper.ToString: string;
begin
  Result := Int64(Self).ToString;
end;

{ TVkAsrStateHelper }

class function TVkAsrStateHelper.Create(const Value: string): TVkAsrState;
begin
  Result := TVkAsrState(IndexStr(Value, VkAsrState));
end;

function TVkAsrStateHelper.ToString: string;
begin
  Result := VkAsrState[Self];
end;

{ TVkAsrModelHelper }

class function TVkAsrModelHelper.Create(const Value: string): TVkAsrModel;
begin
  Result := TVkAsrModel(IndexStr(Value, VkAsrModel));
end;

function TVkAsrModelHelper.ToString: string;
begin
  Result := VkAsrModel[Self];
end;

end.

