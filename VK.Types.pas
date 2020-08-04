unit VK.Types;

interface

{$INCLUDE include.inc}

uses
  System.Classes, REST.Json, System.SysUtils, System.Generics.Collections, System.JSON;

type
  TVkException = Exception;

  TVkAuthException = TVkException;

  TVkParserException = TVkException;

  TVkHandlerException = TVkException;

  TVkMethodException = class(TVkException)
  private
    FCode: Integer;
    procedure SetCode(const Value: Integer);
  public
    property Code: Integer read FCode write SetCode;
    constructor Create(const Msg: string; Code: Integer);
  end;

  TVkWrongParamException = TVkException;

  TVkLongPollServerException = TVkException;

  TVkLongPollServerParseException = TVkLongPollServerException;

  TVkLongPollServerHTTPException = TVkLongPollServerException;

  TVkGroupEventsException = TVkLongPollServerException;

  TVkUserEventsException = TVkLongPollServerException;

const
  //Inner VK errors
  ERROR_VK_UNKNOWN = -1;
  ERROR_VK_NOTOKEN = -2;
  ERROR_VK_INTERNAL = -3;

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

  //Error Codes
  VK_ERROR_INVALID_TOKEN = 5;
  VK_ERROR_CAPTCHA = 14;
  VK_ERROR_CONFIRM = 24;
  VK_ERROR_REQUESTLIMIT = 6;
  VK_ERROR_INTERNAL_SERVER = 10;

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
    function ToString: string; overload; inline;
    function ToJson: string; overload; inline;
    procedure Assign(Source: TStrings); overload;
    function IsEmpty: Boolean;
    function Length: Integer;
  end;

  {$IFDEF OLD_VERSION}
  TArrayOfInteger = array of Integer;
  {$ELSE}

  TArrayOfInteger = TArray<Integer>;
  {$ENDIF}

  TArrayOfIntegerHelper = record helper for TArrayOfInteger
    function ToString: string; overload; inline;
    function ToJson: string; overload; inline;
    function Add(Value: Integer): Integer;
    function IsEmpty: Boolean;
    function Length: Integer;
  end;

  TFields = TArrayOfString;

  TParam = TArrayOfString;

  TParamInt = TArrayOfInteger;

  TIds = TArrayOfInteger;

  {$IFDEF OLD_VERSION}
  TParams = array of TParam;

  TParamsInt = array of TParamInt;
  {$ELSE}

  TParams = TArray<TParam>;

  TParamsInt = TArray<TParamInt>;
  {$ENDIF}

  TParamsHelper = record helper for TParams
    function Add(Param: TParam): Integer; overload; inline;
    function Add(Key, Value: string): Integer; overload; inline;
    function Add(Key: string; Value: Integer): Integer; overload; inline;
    function Add(Key: string; Value: Extended): Integer; overload; inline;
    function Add(Key: string; Value: TDateTime): Integer; overload; inline;
    function Add(Key: string; Value: TArrayOfString): Integer; overload; inline;
    function Add(Key: string; Value: Boolean): Integer; overload; inline;
    function Add(Key: string; Value: TArrayOfInteger): Integer; overload; inline;
    function KeyExists(Key: string): Boolean; inline;
    function GetValue(Key: string): string; inline;
    function Remove(Key: string): string; inline;
  end;

  Attachment = class
  public
    class function Album(Id: Integer; OwnerId: Integer = 0; AccessKey: string = ''): string;
    class function Doc(Id: Integer; OwnerId: Integer = 0; AccessKey: string = ''): string;
    class function Video(Id: Integer; OwnerId: Integer = 0; AccessKey: string = ''): string;
    class function Audio(Id: Integer; OwnerId: Integer = 0; AccessKey: string = ''): string;
    class function Create(&Type: string; OwnerId, Id: Integer; AccessKey: string): string; static;
  end;

  TAttachment = string;

  TAttachmentArray = TArray<TAttachment>;

  TVkPhotoFeedType = (ftPhoto, ftPhotoTag);

  TVkPhotoFeedTypeHelper = record helper for TVkPhotoFeedType
    function ToString: string; inline;
  end;

  TVkPhotoSystemAlbum = (saWall, saSaved, saProfile);

  TVkPhotoSystemAlbumHelper = record helper for TVkPhotoSystemAlbum
    function ToString: string; inline;
    function ToVkId: Integer; inline;
  end;

  //Флаги сообщений
  TMessageFlag = (mfUNKNOWN_9, mfUNKNOWN_8, mfUNKNOWN_7, mfUNKNOWN_6, mfNotDelivered, mfDeleteForAll, mfHidden,
    mfUNKNOWN_5, mfUNKNOWN_4, mfUnreadMultichat, mfUNKNOWN_3, mfUNKNOWN_2, mfUNKNOWN_1, mfMedia, mfFixed, mfDeleted,
    mfSpam, mfFriends, mfChat, mfImportant, mfReplied, mfOutbox, mfUnread);

  TMessageFlagHelper = record helper for TMessageFlag
    function ToString: string; inline;
  end;

  TMessageFlags = set of TMessageFlag;

  TMessageFlagsHelper = record helper for TMessageFlags
    function ToString: string; overload; inline;
  end;

  {$WARNINGS OFF}
  MessageFlags = class
  public
    class function FlagDataToFlag(FlagData: Integer): TMessageFlag;
    class function Create(Data: Integer): TMessageFlags;
    class function ToString(Flags: TMessageFlags): string;
  end;
  {$WARNINGS ON}

  //Жанры музыки

  TAudioGenre = (agNone, agRock, agPop, agRapAndHipHop, agEasyListening, agHouseAndDance, agInstrumental, agMetal,
    agAlternative, agDubstep, agJazzAndBlues, agDrumAndBass, agTrance, agChanson, agEthnic, agAcousticAndVocal, agReggae,
    agClassical, agIndiePop, agSpeech, agElectropopAndDisco, agOther);

  TAudioGenreHelper = record helper for TAudioGenre
    function ToConst: Integer;
    function ToString: string; inline;
  end;

  AudioGenre = class
    class function Create(Value: Integer): TAudioGenre;
  end;

  TVkSort = (stAsc, stDesc);

  TVkSortHelper = record helper for TVkSort
    function ToString: string; overload; inline;
  end;

  TVkSortIdTime = (sitIdAsc, sitIdDesc, sitTimeAsc, sitTimeDesc);

  TVkSortIdTimeHelper = record helper for TVkSortIdTime
    function ToString: string; overload; inline;
  end;

  TVkLang = (vlAuto = -1, vlRU = 0, vlUK = 1, vlBE = 2, vlEN = 3, vlES = 4, vlFI = 5, vlDE = 6, vlIT = 7);

  /// <summary>
  ///  <b>friends</b> — будут возвращены только друзья в этом сообществе.
  ///  <b>unsure</b> — будут возвращены пользователи, которые выбрали «Возможно пойду» (если сообщество относится к мероприятиям).
  ///  <b>managers</b> — будут возвращены только руководители сообщества (доступно при запросе с передачей access_token от имени администратора сообщества).
  /// </summary>
  TVkGroupMembersFilter = (gmfFriends, mgfUnsure, gmfManagers);

  TVkGroupMembersFilterHelper = record helper for TVkGroupMembersFilter
    function ToString: string; inline;
  end;

  /// <summary>
  /// (автовыбор), Запустить, Играть, Перейти, Открыть, Подробнее, Позвонить, Забронировать, Записаться,
  /// Зарегистрироваться, Купить, Купить билет, Заказать, Создать, Установить, Связаться, Заполнить,
  /// Подписаться, Я пойду, Вступить, Связаться, Написать, Начать, Получить, Смотреть, Скачать, Участвовать,
  /// Играть, Подать заявку, Получить предложение, Написать, Откликнуться
  /// Подробное описание тут https://vk.com/dev/wall.postAdsStealth
  /// </summary>
  TVkPostLinkButton = (lbAuto, lbAppJoin, lbAppGameJoin, lbOpenUrl, lbOpen, lbMore, lbCall, lbBook, lbEnroll, lbRegister,
    lbBuy, lbBuyTicket, lbOrder, lbCreate, lbInstall, lbContact, lbFill, lbJoinPublic, lbJoinEvent, lbJoin, lbIM, lbIM2,
    lbBegin, lbGet, lbWatch, lbDownload, lbParticipate, lbPlay, lbApply, lbGetAnOffer, lbToWrite, lbReply);

  TVkPostLinkButtonHelper = record helper for TVkPostLinkButton
    function ToString: string; inline;
  end;

  TVkFollowerField = (flPhotoId, flVerified, flSex, flBirthDate, flCity, flCountry, flHomeTown, flHasPhoto, flPhoto50,
    flPhoto100, flPhoto200Orig, flPhoto200, flPhoto400Orig, flPhotoMax, flPhotoMaxOrig, flOnline, flLists, flDomain,
    flHasMobile, flContacts, flSite, flEducation, flUniversities, flSchools, flStatus, flLastSeen, flFollowersCount,
    flCommonCount, flOccupation, flNickName, flRelatives, flRelation, flPersonal, flConnections, flExports,
    flWallComments, flActivities, flInterests, flMusic, flMovies, flTV, flBooks, flGames, flAbout, flQuotes, flCanPost,
    flCanSeeAllPosts, flCanSeeAudio, flCanWritePrivateMessage, flCanSendFriendRequest, flIsFavorite, flIsHiddenFromFeed,
    flTimeZone, flScreenName, flMaidenName, flCropPhoto, flIsFriend, flFriendStatus, flCareer, flMilitary, flBlacklisted,
    flBlacklistedByMe);

  TVkFollowerFieldHelper = record helper for TVkFollowerField
    function ToString: string; inline;
  end;

  TVkFollowerFields = set of TVkFollowerField;

  TVkFollowerFieldsHelper = record helper for TVkFollowerFields
    function ToString: string; inline;
    class function All: TVkFollowerFields; static; inline;
  end;

  TVkUserField = (ufPhotoId, ufVerified, ufSex, ufBirthDate, ufCity, ufCountry, ufHomeTown, ufHasPhoto, ufPhoto50,
    ufPhoto100, ufPhoto200Orig, ufPhoto200, ufPhoto400Orig, ufPhotoMax, ufPhotoMaxOrig, ufOnline, ufDomain, ufHasMobile,
    ufContacts, ufSite, ufEducation, ufUniversities, ufSchools, ufStatus, usLastSeen, ufFollowersCount, ufCommonCount,
    ufOccupation, ufNickname, ufRelatives, ufRelation, ufPersonal, ufConnections, ufExports, ufActivities, ufInterests,
    ufMusic, ufMovies, ufTV, ufBooks, ufGames, ufAbout, ufQuotes, ufCanPost, ufCanSeeAllPosts, ufCanSeeAudio,
    ufCanWritePrivateMessage, ufCanSendFriendRequest, ufIsFavorite, ufIsHiddenFromFeed, ufTimeZone, ufScreenName,
    ufMaidenName, ufCropPhoto, ufIsFriend, ufFriendStatus, ufCareer, ufMilitary, ufBlacklisted, ufBlacklistedByMe,
    ufCanBeInvitedGroup);

  TVkUserFieldHelper = record helper for TVkUserField
    function ToString: string; inline;
  end;

  TVkUserFields = set of TVkUserField;

  TVkUserFieldsHelper = record helper for TVkUserFields
  public
    function ToString: string; inline;
    class function All: TVkUserFields; static; inline;
  end;

  TVkGroupMemberField = (mfSex, mfBdate, mfCity, mfCountry, mfPhoto50, mfPhoto100, mfPhoto200orig, mfPhoto200,
    mfPhoto400orig, mfPhotoMax, mfPhotoMaxOrig, mfOnline, mfOnlineMobile, mfLists, mfDomain, mfHasMobile, mfContacts,
    mfConnections, mfSite, mfEducation, mfUniversities, mfSchools, mfCanPost, mfCanSeeAllPosts, mfCanSeeAudio,
    mfCanWritePrivateMessage, mfStatus, mfLastSeen, mfCommonCount, mfRelation, mfRelatives);

  TVkGroupMemberFieldHelper = record helper for TVkGroupMemberField
    function ToString: string; inline;
  end;

  TVkGroupMemberFields = set of TVkGroupMemberField;

  TVkGroupMemberFieldsHelper = record helper for TVkGroupMemberFields
  public
    function ToString: string; inline;
    class function All: TVkGroupMemberFields; static; inline;
  end;

  TVkFriendField = (ffNickName, ffDomain, ffSex, ffBirthDate, ffCity, ffCountry, ffTimeZone, ffPhoto50, ffPhoto100,
    ffPhoto200, ffHasMobile, ffContacts, ffEducation, ffOnline, ffRelation, ffLastSeen, ffStatus,
    ffCanWritePrivateMessage, ffCanSeeAllPosts, ffCanPost, ffUniversities, ffCanSeeAudio);

  TVkFriendFieldHelper = record helper for TVkFriendField
    function ToString: string; inline;
  end;

  TVkFriendFields = set of TVkFriendField;

  TVkFriendFieldsHelper = record helper for TVkFriendFields
    function ToString: string; inline;
    class function All: TVkFriendFields; static; inline;
  end;

  TVkGroupField = (gfCity, gfCountry, gfPlace, gfDescription, gfWikiPage, gfMembersCount, gfCounters, gfStartDate,
    gfFinishDate, gfCanPost, gfCanSeeAllPosts, gfActivity, gfStatus, gfContacts, gfLinks, gfFixedPost, gfVerified,
    gfSite, gfCanCreateTopic, gfPhoto50);

  TVkGroupFieldHelper = record helper for TVkGroupField
    function ToString: string; inline;
  end;

  TVkGroupFields = set of TVkGroupField;

  TVkGroupFieldsHelper = record helper for TVkGroupFields
    function ToString: string; inline;
    class function All: TVkGroupFields; static; inline;
  end;

  TVkGroupAddressField = (gafTitle, gafAddress, gafAdditionalAddress, gafCountryId, gafCityId, gafMetroStationId,
    gafLatitude, gafLongitude, gafWorkInfoStatus, gafTimeOffset);

  TVkGroupAddressFieldHelper = record helper for TVkGroupAddressField
    function ToString: string; inline;
  end;

  TVkGroupAddressFields = set of TVkGroupAddressField;

  TVkGroupAddressFieldsHelper = record helper for TVkGroupAddressFields
    function ToString: string; inline;
    class function All: TVkGroupAddressFields; static; inline;
  end;

  TVkGroupAccess = (gaOpen, gaClose, gaPrivate);

  TVkGroupAccessHelper = record helper for TVkGroupAccess
    function ToConst: Integer; inline;
  end;

  TVkGroupRole = (grModerator, grEditor, grAdmin);

  TVkGroupRoleHelper = record helper for TVkGroupRole
    function ToString: string; inline;
  end;

  /// <summary>
  /// Тут список цветов -> VkGroupTagColors
  /// </summary>
  TVkGroupTagColor = string;

  TVkAgeLimits = (alNone = 1, al16Plus = 2, al18Plus = 3);

  TVkAgeLimitsHelper = record helper for TVkAgeLimits
    function ToConst: Integer; inline;
  end;

  TVkCurrency = (mcRUB, mcUAH, mcKZT, mcEUR, mcUSD);

  TVkMarketCurrencyHelper = record helper for TVkCurrency
    function ToConst: Integer; inline;
  end;

  TVkGroupType = (gtGroup, gtEvent, gtPublic);

  TVkGroupTypeHelper = record helper for TVkGroupType
    function ToString: string; inline;
  end;

  TVkGroupFilter = (gftAdmin, gftEditor, gftModer, gftAdvertiser, gftGroups, gftPublics, gftEvents, gftHasAddress);

  TVkGroupFilterHelper = record helper for TVkGroupFilter
    function ToString: string; inline;
  end;

  TVkGroupFilters = set of TVkGroupFilter;

  TVkGroupFiltersHelper = record helper for TVkGroupFilters
    function ToString: string; inline;
    class function All: TVkGroupFilters; static; inline;
  end;

  TVkMessageInfo = class
  private
    FTitle: string;
    FFrom: string;
    FMentions: TArray<Extended>;
  public
    property Title: string read FTitle write FTitle;
    property From: string read FFrom write FFrom;
    property Mentions: TArray<Extended> read FMentions write FMentions;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageInfo;
  end;

  TVkMessageAttachmentInfo = class
    type
      TAttachInfoType = record
        Attach: string;
        AttachType: string;
        class function Create(AAttach, AAttachType: string): TAttachInfoType; static;
      end;
  private
    FFwd: string;
    FReply: string;
    FAttach1: string;
    FAttach1_type: string;
    FAttach2: string;
    FAttach2_type: string;
    FAttach3: string;
    FAttach3_type: string;
    FAttach4: string;
    FAttach4_type: string;
    FAttach5: string;
    FAttach5_type: string;
    FAttach6: string;
    FAttach6_type: string;
    FAttach7: string;
    FAttach7_type: string;
    FAttach8: string;
    FAttach8_type: string;
    FAttach9: string;
    FAttach9_type: string;
    FAttach10: string;
    FAttach10_type: string;
    function GetCount: Integer;
    function GetAttachments(Index: Integer): TAttachInfoType;
  public
    property Fwd: string read FFwd write FFwd;
    property Reply: string read FReply write FReply;
    property Attach1: string read FAttach1 write FAttach1;
    property Attach1Type: string read FAttach1_type write FAttach1_type;
    property Attach2: string read FAttach2 write FAttach2;
    property Attach2Type: string read FAttach2_type write FAttach2_type;
    property Attach3: string read FAttach3 write FAttach3;
    property Attach3Type: string read FAttach3_type write FAttach3_type;
    property Attach4: string read FAttach4 write FAttach4;
    property Attach4Type: string read FAttach4_type write FAttach4_type;
    property Attach5: string read FAttach5 write FAttach5;
    property Attach5Type: string read FAttach5_type write FAttach5_type;
    property Attach6: string read FAttach6 write FAttach6;
    property Attach6Type: string read FAttach6_type write FAttach6_type;
    property Attach7: string read FAttach7 write FAttach7;
    property Attach7Type: string read FAttach7_type write FAttach7_type;
    property Attach8: string read FAttach8 write FAttach8;
    property Attach8Type: string read FAttach8_type write FAttach8_type;
    property Attach9: string read FAttach9 write FAttach9;
    property Attach9Type: string read FAttach9_type write FAttach9_type;
    property Attach10: string read FAttach10 write FAttach10;
    property Attach10Type: string read FAttach10_type write FAttach10_type;
    property Count: Integer read GetCount;
    property Attachments[Index: Integer]: TAttachInfoType read GetAttachments;
    function ToArray: TArray<TAttachInfoType>;
    function ToArrayOfString: TArrayOfString;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageAttachmentInfo;
  end;

  //Флаги диалогов
  TDialogFlag = (dfImportant, dfUnanswered);

  TDialogFlags = set of TDialogFlag;

  TDialogFlagsHelper = record helper for TDialogFlags
    function ToString: string; overload; inline;
  end;

  {$WARNINGS OFF}
  DialogFlags = class
    class function FlagDataToFlag(FlagData: Integer): TDialogFlag;
    class function Create(Data: Integer): TDialogFlags;
    class function ToString(Flags: TDialogFlags): string;
  end;
  {$WARNINGS ON}

  //Идентификатор типа изменения в чате

  TChatChangeInfoType = (citNone, citName, citPic, citNewAdmin, citFixMessage, citJoin, citLeave, citKick, citUnadmin);

  TChatChangeInfoTypeHelper = record helper for TChatChangeInfoType
    function ToString: string; overload; inline;
  end;

  //Платформы
  TVkPlatform = (pfUnknown, pfMobile, pfIPhone, pfIPad, pfAndroid, pfWindowsPhone, pfWindows, pfWeb);

  //Тип смены флагов
  TFlagsChangeType = (fcFlagsReplace, fcFlagsSet, fcFlagsReset);

  TMessageChangeTypeHelper = record helper for TFlagsChangeType
    function ToString: string; overload; inline;
  end;

  //Типы объектов
  TVkItemType = (itPost, itComment, itPhoto, itAudio, itVideo, itNote, itMarket, itPhotoComment, itVideoComment,
    itTopicComment, itMarketComment, itSitepage, itStory);

  TVkItemTypeHelper = record helper for TVkItemType
    function ToString: string; inline;
  end;

  //Типы объектов
  TVkAttachmentType = (atUnknown, atPhoto, atVideo, atAudio, atDoc, atLink, atMarket, atMarketAlbum, atWall, atWallReply,
    atSticker, atGift, atCall, atAudioMessage, atPostedPhoto, atGraffiti, atNote, atApp, atPoll, atPage, atAlbum,
    atPhotosList, atPrettyCards, atEvent);

  TVkAttachmentTypeHelper = record helper for TVkAttachmentType
    function ToString: string; inline;
    class function Create(Value: string): TVkAttachmentType; static;
  end;

  TVkPeerType = (ptUnknown, ptUser, ptChat, ptGroup, ptEmail);

  TVkPeerTypeHelper = record helper for TVkPeerType
    function ToString: string; inline;
    class function Create(Value: string): TVkPeerType; static;
  end;

  TVkPostType = (ptSuggests, ptPostponed, ptOwner, ptOthers, ptAll);

  TVkPostTypeHelper = record helper for TVkPostType
    function ToString: string; inline;
  end;

  TVkNameCase = (ncNom, ncGen, ncDat, ncAcc, ncIns, ncAbl);

  TVkNameCaseHelper = record helper for TVkNameCase
    function ToString: string; inline;
  end;
  {
  именительный – nom, родительный – gen, дательный – dat, винительный – acc, творительный – ins, предложный – abl. По умолчанию nom.
  }

  //Пол

  TVkSex = (sxMale, sxFemale);

  TVkSexSearch = (sxsAny, sxsMale, sxsFemale);

  //Видимость даты рождения
  TVkBirthDateVisibility = (dvVisible, dvDayMonOnly, dvHidden);

  //Отношения
  TVkRelation = (rnNone, rnNotMarried, rnHaveFriend, rnAffiance, rnMarried, rnComplicated, rnnActivelyLooking, rnInLove,
    rnCivilMarriage);
   {0 — не указано.
    1 — не женат/не замужем;
    2 — есть друг/есть подруга;
    3 — помолвлен/помолвлена;
    4 — женат/замужем;
    5 — всё сложно;
    6 — в активном поиске;
    7 — влюблён/влюблена;
    8 — в гражданском браке;}


  //Структура события входящего сообщения

  TMessageData = record
    MessageId: Integer;
    Flags: TMessageFlags;
    PeerId: Integer;
    TimeStamp: TDateTime;
    Text: string;
    Info: TVkMessageInfo;
    RandomId: Integer;
    Attachments: TVkMessageAttachmentInfo;
  end;

  TMessageChangeData = record
    MessageId: Integer;
    Flags: TMessageFlags;
    PeerId: Integer;
    ChangeType: TFlagsChangeType;
  end;

  TDialogChangeData = record
    PeerId: Integer;
    Flags: TDialogFlags;
    ChangeType: TFlagsChangeType;
  end;

  TResponseError = record
    Code: Integer;
    Text: string;
  end;

  TResponse = record
    Success: Boolean;
    Response: string;
    JSON: string;
    Error: TResponseError;
    function ResponseIsTrue: Boolean;
    function ResponseIsFalse: Boolean;
    function ResponseAsInt(var Value: Integer): Boolean;
    function ResponseAsStr(var Value: string): Boolean;
    function GetJSONValue: TJSONValue;
    function GetJSONResponse: TJSONValue;
    function GetValue<T>(const Field: string; var Value: T): Boolean;
  end;

  TEventExtraFields = record
    PeerId: integer; // идентификатор назначения. Для пользователя: id пользователя. Для групповой беседы: 2000000000 + id беседы. Для сообщества: -id сообщества либо id сообщества + 1000000000 (для version = 0).
    TimeStamp: integer; // время отправки сообщения в Unixtime;
    Text: string; // текст сообщения;
    Info: TVkMessageInfo;
    Attachments: TVkMessageAttachmentInfo;
    RandomId: Integer;
  end;

  TChatTypingData = record
    UserIds: TIds;
    PeerId, TotalCount: Integer;
    TimeStamp: TDateTime;
  end;

  TChatRecordingData = record
    UserIds: TIds;
    PeerId, TotalCount: Integer;
    TimeStamp: TDateTime;
  end;

  TVkLinkStatusType = (lsNotBanned, lsBanned, lsProcessing);

  TVkLinkStatusTypeHelper = record helper for TVkLinkStatusType
    function ToString: string; overload; inline;
    class function FromString(Value: string): TVkLinkStatusType; static; inline;
  end;

  TVkUserReport = (urPorn, urSpam, urInsult, urAdvertisеment);

  TVkUserReportHelper = record helper for TVkUserReport
    function ToString: string; overload; inline;
  end;

  TVkUserBlockReason = (brOther, brSpam, brInsultingParticipants, brObsceneExpressions, brOffTopic);

  TUserBlockReasonHelper = record helper for TVkUserBlockReason
    function ToString: string; overload; inline;
    function ToConst: Integer; overload; inline;
  end;

  TVkMediaReportReason = (prSpam, prChildPorn, prExtremism, prViolence, prDrug, prAdults, prInsult, prCallForSuicide);

  TVkMediaReportReasonHelper = record helper for TVkMediaReportReason
    function ToString: string; overload; inline;
    function ToConst: Integer; overload; inline;
  end;

  TVkGroupJoinType = (jtUnknown, jtJoin, jtUnsure, jtAccepted, jtApproved, jtRequest);

  TGroupJoinTypeHelper = record helper for TVkGroupJoinType
    function ToString: string; overload; inline;
  end;

  GroupJoinType = class
    class function Create(Value: string): TVkGroupJoinType;
  end;

  TVkGroupLevel = (glNone, glModer, glEditor, glAdmin);

  TVkGroupLevelHelper = record helper for TVkGroupLevel
    function ToString: string; overload; inline;
  end;

  TVkCounterFilter = (cfFriends, cfMessages, cfPhotos, cfVideos, cfNotes, cfGifts, cfEvents, cfGroups, cfNotifications,
    cfSdk, cfAppRequests, cfFriendsRecommendations);

  TVkCounterFilterHelper = record helper for TVkCounterFilter
    function ToString: string; overload; inline;
  end;

  TVkCounterFilters = set of TVkCounterFilter;

  TVkCounterFiltersHelper = record helper for TVkCounterFilters
    function ToString: string; overload; inline;
  end;

  TVkInfoFilter = (ifCountry, ifHttpsRequired, ifOwnPostsDefault, ifNoWallReplies, ifIntro, ifLang);

  TVkInfoFilterHelper = record helper for TVkInfoFilter
    function ToString: string; overload; inline;
  end;

  TVkInfoFilters = set of TVkInfoFilter;

  TVkInfoFiltersHelper = record helper for TVkInfoFilters
    function ToString: string; overload; inline;
  end;

  TVkPermission = (Notify, Friends, Photos, Audio, Video, Stories, Pages, Status, Notes, Messages, Wall, Ads, Offline,
    Docs, Groups, Notifications, Stats, Email, Market, AppWidget, Manage);

  TVkPermissionHelper = record helper for TVkPermission
    function ToString: string; overload; inline;
  end;

  TVkPermissions = set of TVkPermission;

  TVkPermissionsHelper = record helper for TVkPermissions
    function ToString: string; overload; inline;
  end;

  TOnLogin = procedure(Sender: TObject) of object;

  TOnAuth = procedure(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var ChangePasswordHash:
    string) of object;

  TOnConfirm = procedure(Sender: TObject; Ans: string; var Accept: Boolean) of object;

  TOnCaptcha = procedure(Sender: TObject; const CaptchaURL: string; var Answer: string) of object;

  TOnLog = procedure(Sender: TObject; const Value: string) of object;

  TOnVKError = procedure(Sender: TObject; E: Exception; Code: Integer; Text: string) of object;

  TCallMethodCallback = reference to procedure(Respone: TResponse);

  TOnLongPollServerUpdate = procedure(Sender: TObject; GroupID: string; Update: TJSONValue) of object;

  TOnNewMessage = procedure(Sender: TObject; MessageData: TMessageData) of object;

  TOnEditMessage = procedure(Sender: TObject; MessageData: TMessageData) of object;

  TOnChangeMessageFlags = procedure(Sender: TObject; MessageChangeData: TMessageChangeData) of object;

  TOnChangeDialogFlags = procedure(Sender: TObject; DialogChangeData: TDialogChangeData) of object;

  TOnUserOnline = procedure(Sender: TObject; UserId: Integer; VkPlatform: TVkPlatform; TimeStamp: TDateTime) of object;

  TOnUserOffline = procedure(Sender: TObject; UserId: Integer; InactiveUser: Boolean; TimeStamp: TDateTime) of object;

  TOnReadMessages = procedure(Sender: TObject; Incoming: Boolean; PeerId, LocalId: Integer) of object;

  TOnRecoverOrDeleteMessages = procedure(Sender: TObject; PeerId, LocalId: Integer) of object;

  TOnChatChanged = procedure(Sender: TObject; const ChatId: Integer; IsSelf: Boolean) of object;

  TOnChatChangeInfo = procedure(Sender: TObject; const PeerId: Integer; TypeId: TChatChangeInfoType; Info: Integer) of object;

  TOnUserTyping = procedure(Sender: TObject; UserId, ChatId: Integer) of object;

  TOnUserCall = procedure(Sender: TObject; UserId, CallId: Integer) of object;

  TOnCountChange = procedure(Sender: TObject; Count: Integer) of object;

  TOnNotifyChange = procedure(Sender: TObject; PeerId: Integer; Sound: Boolean; DisableUntil: Integer) of object;

  TOnUsersTyping = procedure(Sender: TObject; Data: TChatTypingData) of object;

  TOnUsersRecording = procedure(Sender: TObject; Data: TChatRecordingData) of object;

var
  VkUserActive: array[Boolean] of string = ('Бездействие', 'Покинул сайт');
  VkGroupLevel: array[TVkGroupLevel] of string = ('Участник', 'Модератор',
    'Редактор', 'Администратор');
  VkUserBlockReason: array[TVkUserBlockReason] of string = ('Другое', 'Спам',
    'Оскорбление участников', 'Мат', 'Разговоры не по теме');
  VkPhotoReportReason: array[TVkMediaReportReason] of string = ('Спам', 'Детская порнография', 'Экстремизм', 'Насилие',
    'Пропаганда наркотиков', 'Материал для взрослых', 'Оскорбление', 'Призыв к суициду');
  VkMessageFlagTypes: array[TMessageFlag] of string = ('Unknown_9', 'Unknown_8',
    'Unknown_7', 'Unknown_6', 'NotDelivered', 'DeleteForAll', 'Hidden',
    'Unknown_5', 'Unknown_4', 'UnreadMultichat', 'Unknown_3', 'Unknown_2',
    'Unknown_1', 'Media', 'Fixed', 'Deleted', 'Spam', 'Friends', 'Chat',
    'Important', 'Replied', 'Outbox', 'Unread');

var
  VkMessageFlags: array[TMessageFlag] of Integer = (MF_UNKNOWN_9, MF_UNKNOWN_8,
    MF_UNKNOWN_7, MF_UNKNOWN_6, MF_NOT_DELIVERED, MF_DELETE_FOR_ALL, MF_HIDDEN,
    MF_UNKNOWN_5, MF_UNKNOWN_4, MF_UNREAD_MULTICHAT, MF_UNKNOWN_3, MF_UNKNOWN_2,
    MF_UNKNOWN_1, MF_MEDIA, MF_FIXED, MF_DELЕTЕD, MF_SPAM, MF_FRIENDS, MF_CHAT,
    MF_IMPORTANT, MF_REPLIED, MF_OUTBOX, MF_UNREAD);
  VkAudioGenres: array[TAudioGenre] of Integer = (AG_NONE, AG_ROCK, AG_POP,
    AG_RAPANDHIPHOP, AG_EASYLISTENING, AG_HOUSEANDDANCE, AG_INSTRUMENTAL,
    AG_METAL, AG_ALTERNATIVE, AG_DUBSTEP, AG_JAZZANDBLUES, AG_DRUMANDBASS,
    AG_TRANCE, AG_CHANSON, AG_ETHNIC, AG_ACOUSTICANDVOCAL, AG_REGGAE,
    AG_CLASSICAL, AG_INDIEPOP, AG_SPEECH, AG_ELECTROPOPANDDISCO, AG_OTHER);
  VkAudioGenresStr: array[TAudioGenre] of string = ('', 'Rock', 'Pop',
    'RapAndHipHop', 'EasyListening', 'HouseAndDance', 'Instrumental', 'Metal',
    'Alternative', 'Dubstep', 'JazzAndBlues', 'DrumAndBass', 'Trance', 'Chanson',
    'Ethnic', 'AcousticAndVocal', 'Reggae', 'Classical', 'IndiePop', 'Speech',
    'ElectropopAndDisco', 'Other');
  VkDialogFlags: array[TDialogFlag] of Integer = (GR_UNANSWERED, GR_IMPORTANT);
  VkPlatforms: array[TVkPlatform] of string = ('Unknown', 'Mobile', 'iPhone',
    'iPad', 'Android', 'Windows Phone', 'Windows', 'Web');
  VkAttachmentType: array[TVkAttachmentType] of string = ('', 'photo', 'video',
    'audio', 'doc', 'link', 'market', 'market_album', 'wall', 'wall_reply',
    'sticker', 'gift', 'call', 'audio_message', 'posted_photo', 'graffiti', 'note',
    'app', 'poll', 'page', 'album', 'photos_list', 'pretty_cards', 'event');
  VkPeerType: array[TVkPeerType] of string = ('', 'user', 'chat', 'group', 'email');
  VkNameCase: array[TVkNameCase] of string = ('nom', 'gen', 'dat', 'acc', 'ins', 'abl');
  VkItemType: array[TVkItemType] of string = ('post', 'comment', 'photo',
    'audio', 'video', 'note', 'market', 'photo_comment', 'video_comment',
    'topic_comment', 'market_comment', 'sitepage', 'story');
  VkGroupJoinType: array[TVkGroupJoinType] of string = ('', 'join', 'unsure',
    'accepted', 'approved', 'request');
  VkPostType: array[TVkPostType] of string = ('suggests', 'postponed', 'owner', 'others', 'all');
  VkPremissionStr: array[TVkPermission] of string = ('notify', 'friends', 'photos', 'audio',
    'video', 'stories', 'pages', 'status', 'notes', 'messages', 'wall', 'ads', 'offline',
    'docs', 'groups', 'notifications', 'stats', 'email', 'market', 'app_widget', 'manage');
  VkUserField: array[TVkUserField] of string = (
    'photo_id', 'verified', 'sex', 'bdate', 'city', 'country', 'home_town', 'has_photo', 'photo_50',
    'photo_100', 'photo_200_orig', 'photo_200', 'photo_400_orig', 'photo_max', 'photo_max_orig',
    'online', 'domain', 'has_mobile', 'contacts', 'site', 'education', 'universities', 'schools',
    'status', 'last_seen', 'followers_count', 'common_count', 'occupation', 'nickname',
    'relatives', 'relation', 'personal', 'connections', 'exports', 'activities', 'interests',
    'music', 'movies', 'tv', 'books', 'games', 'about', 'quotes', 'can_post', 'can_see_all_posts',
    'can_see_audio', 'can_write_private_message', 'can_send_friend_request', 'is_favorite',
    'is_hidden_from_feed', 'timezone', 'screen_name', 'maiden_name', 'crop_photo', 'is_friend',
    'friend_status', 'career', 'military', 'blacklisted', 'blacklisted_by_me', 'can_be_invited_group');
  VkFollowerField: array[TVkFollowerField] of string = (
    'photo_id', 'verified', 'sex', 'bdate', 'city', 'country', 'home_town', 'has_photo', 'photo_50',
    'photo_100', 'photo_200_orig', 'photo_200', 'photo_400_orig', 'photo_max', 'photo_max_orig',
    'online', 'lists', 'domain', 'has_mobile', 'contacts', 'site', 'education', 'universities',
    'schools', 'status', 'last_seen', 'followers_count', 'common_count', 'occupation', 'nickname',
    'relatives', 'relation', 'personal', 'connections', 'exports', 'wall_comments', 'activities',
    'interests', 'music', 'movies', 'tv', 'books', 'games', 'about', 'quotes', 'can_post', 'can_see_all_posts',
    'can_see_audio', 'can_write_private_message', 'can_send_friend_request', 'is_favorite',
    'is_hidden_from_feed', 'timezone', 'screen_name', 'maiden_name', 'crop_photo', 'is_friend',
    'friend_status', 'career', 'military', 'blacklisted', 'blacklisted_by_me');
  VKPostLinkButton: array[TVkPostLinkButton] of string = (
    'auto', 'app_join', 'app_game_join', 'open_url', 'open', 'more', 'call', 'book', 'enroll', 'register', 'buy', 'buy_ticket', 'order', 'create', 'install', 'contact', 'fill',
    'join_public', 'join_event', 'join', 'im', 'im2', 'begin', 'get', 'watch', 'download', 'participate', 'play',
    'apply', 'get_an_offer', 'to_write', 'reply');
  VkFriendField: array[TVkFriendField] of string = ('nickname', 'domain', 'sex', 'bdate', 'city', 'country', 'timezone',
    'photo_50', 'photo_100', 'photo_200_orig', 'has_mobile', 'contacts', 'education', 'online', 'relation', 'last_seen', 'status',
    'can_write_private_message', 'can_see_all_posts', 'can_post', 'universities', 'can_see_audio');
  VkGroupField: array[TVkGroupField] of string = ('city', 'country', 'place', 'description', 'wiki_page', 'members_count',
    'counters', 'start_date', 'finish_date', 'can_post', 'can_see_all_posts', 'activity', 'status',
    'contacts', 'links', 'fixed_post', 'verified', 'site', 'can_create_topic', 'photo_50');
  VkGroupFilter: array[TVkGroupFilter] of string = ('admin', 'editor', 'moder', 'advertiser', 'groups', 'publics',
    'events', 'hasAddress');
  VkGroupType: array[TVkGroupType] of string = ('group', 'event', 'public');
  VkGroupRole: array[TVkGroupRole] of string = ('moderator', 'editor', 'administrator');
  VkGroupMemberField: array[TVkGroupMemberField] of string = ('sex', 'bdate', 'city', 'country', 'photo_50', 'photo_100',
    'photo_200_orig', 'photo_200', 'photo_400_orig', 'photo_max',
    'photo_max_orig', 'online', 'online_mobile', 'lists', 'domain', 'has_mobile', 'contacts', 'connections', 'site', 'education',
    'universities', 'schools', 'can_post', 'can_see_all_posts', 'can_see_audio', 'can_write_private_message', 'status',
    'last_seen', 'common_count', 'relation', 'relatives');
  VkCounterFilter: array[TVkCounterFilter] of string = ('friends', 'messages', 'photos', 'videos', 'notes',
    'gifts', 'events', 'groups', 'notifications', 'sdk', 'app_requests', 'friends_recommendations');
  VkInfoFilter: array[TVkInfoFilter] of string = ('country', 'https_required', 'own_posts_default', 'no_wall_replies',
    'intro', 'lang');
  VkCurrencyId: array[TVkCurrency] of Integer = (643, 980, 398, 978, 840);
  VkGroupAddressField: array[TVkGroupAddressField] of string = ('title', 'address', 'additional_address', 'country_id',
    'city_id', 'metro_station_id', 'latitude', 'longitude', 'work_info_status', 'time_offset');
  VkGroupTagColors: array of string = ['4bb34b', '5c9ce6', 'e64646', '792ec0', '63b9ba', 'ffa000', 'ffc107', '76787a',
    '9e8d6b', '45678f', '539b9c', '454647', '7a6c4f', '6bc76b', '5181b8', 'ff5c5c', 'a162de', '7ececf', 'aaaeb3', 'bbaa84'];

function VKErrorString(ErrorCode: Integer): string;

function AddParam(var Dest: TParams; Param: TParam): Integer;

function AppendItemsTag(JSON: string): string;

function NormalizePeerId(Value: Integer): Integer;

function PeerIdIsChat(Value: Integer): Boolean;

function PeerIdIsUser(Value: Integer): Boolean;

function PeerIdIsGroup(Value: Integer): Boolean;

implementation

uses
  VK.CommonUtils, System.DateUtils;

function PeerIdIsChat(Value: Integer): Boolean;
begin
  Result := Value > VK_CHAT_ID_START;
end;

function PeerIdIsUser(Value: Integer): Boolean;
begin
  Result := (Value < VK_GROUP_ID_START) and (Value > 0);
end;

function PeerIdIsGroup(Value: Integer): Boolean;
begin
  Result := (Value > VK_GROUP_ID_START) and (Value < VK_CHAT_ID_START);
end;

function NormalizePeerId(Value: Integer): Integer;
begin
  if Value > VK_CHAT_ID_START then
    Exit(Value - VK_CHAT_ID_START);
  if Value > VK_GROUP_ID_START then
    Exit(-(Value - VK_GROUP_ID_START));
  Result := Value;
end;

function AppendItemsTag(JSON: string): string;
begin
  Result := '{"Items": ' + JSON + '}';
end;

function AddParam(var Dest: TParams; Param: TParam): Integer;
var
  i: Integer;
begin
  for i := Low(Dest) to High(Dest) do
    if Dest[i][0] = Param[0] then
    begin
      Dest[i] := Param;
      Exit(i);
    end;
  Result := Length(Dest) + 1;
  SetLength(Dest, Result);
  Dest[Result - 1] := Param;
end;

function VKErrorString(ErrorCode: Integer): string;
var
  ErrStr: string;
begin
  case ErrorCode of
    1:
      ErrStr :=
        'Произошла неизвестная ошибка. Попробуйте повторить запрос позже.';
    2:
      ErrStr :=
        'Приложение выключено. Необходимо включить приложение в настройках https://vk.com/editapp?id={Ваш API_ID} или использовать тестовый режим (test_mode=1)';
    3:
      ErrStr :=
        'Передан неизвестный метод. Проверьте, правильно ли указано название вызываемого метода: https://vk.com/dev/methods.';
    4:
      ErrStr :=
        'Неверная подпись.';
    VK_ERROR_INVALID_TOKEN:
      ErrStr :=
        'Авторизация пользователя не удалась. Убедитесь, что Вы используете верную схему авторизации.';
    6:
      ErrStr :=
        'Слишком много запросов в секунду. Задайте больший интервал между вызовами или используйте метод execute. Подробнее об ограничениях на частоту вызовов см. на странице https://vk.com/dev/api_requests.';
    7:
      ErrStr :=
        'Нет прав для выполнения этого действия. Проверьте, получены ли нужные права доступа при авторизации. Это можно сделать с помощью метода account.getAppPermissions.';
    8:
      ErrStr :=
        'Неверный запрос. Проверьте синтаксис запроса и список используемых параметров (его можно найти на странице с описанием метода).';
    9:
      ErrStr :=
        'Слишком много однотипных действий. Нужно сократить число однотипных обращений. Для более эффективной работы Вы можете использовать execute или JSONP.';
    10:
      ErrStr :=
        'Произошла внутренняя ошибка сервера. Попробуйте повторить запрос позже.';
    11:
      ErrStr :=
        'В тестовом режиме приложение должно быть выключено или пользователь должен быть залогинен. Выключите приложение в настройках https://vk.com/editapp?id={Ваш API_ID}';
    14:
      ErrStr :=
        'Требуется ввод кода с картинки (Captcha).';
    15:
      ErrStr :=
        'Доступ запрещён. Убедитесь, что Вы используете верные идентификаторы, и доступ к контенту для текущего пользователя есть в полной версии сайта.';
    16:
      ErrStr :=
        'Требуется выполнение запросов по протоколу HTTPS, т.к. пользователь включил настройку, требующую работу через безопасное соединение.'#13#10 +
        ' Чтобы избежать появления такой ошибки, в Standalone-приложении Вы можете предварительно проверять состояние этой настройки у пользователя методом account.getInfo.';
    17:
      ErrStr :=
        'Требуется валидация пользователя. Действие требует подтверждения — необходимо перенаправить пользователя на служебную страницу для валидации.';
    18:
      ErrStr :=
        'Страница удалена или заблокирована. Страница пользователя была удалена или заблокирована';
    19:
      ErrStr :=
        'Контент недоступен.';
    20:
      ErrStr :=
        'Данное действие запрещено для не Standalone приложений. Если ошибка возникает несмотря на то, что Ваше приложение имеет тип Standalone, убедитесь, что при авторизации Вы используете redirect_uri=https://oauth.vk.com/blank.html.';
    21:
      ErrStr :=
        'Данное действие разрешено только для Standalone и Open API приложений.';
    22:
      ErrStr :=
        'Ошибка загрузки.';
    23:
      ErrStr :=
        'Метод был выключен. Все актуальные методы ВК API, которые доступны в настоящий момент, перечислены здесь: https://vk.com/dev/methods.';
    24:
      ErrStr :=
        'Требуется подтверждение со стороны пользователя.';
    27:
      ErrStr :=
        'Ключ доступа сообщества недействителен.';
    28:
      ErrStr :=
        'Ключ доступа приложения недействителен.';
    29:
      ErrStr :=
        'Достигнут количественный лимит на вызов метода Подробнее об ограничениях на количество вызовов см. на странице https://vk.com/dev/data_limits';
    30:
      ErrStr :=
        'Профиль является приватным Информация, запрашиваемая о профиле, недоступна с используемым ключом доступа';
    33:
      ErrStr :=
        'Not implemented yet';
    100:
      ErrStr :=
        'Один из необходимых параметров был не передан или неверен. Проверьте список требуемых параметров и их формат на странице с описанием метода.';
    101:
      ErrStr :=
        'Неверный API ID приложения. Найдите приложение в списке администрируемых на странице https://vk.com/apps?act=settings и укажите в запросе верный API_ID (идентификатор приложения).';
    103:
      ErrStr :=
        'Лимит вступлений исчерпан';
    104:
      ErrStr :=
        'Not found';
    113:
      ErrStr :=
        'Неверный идентификатор пользователя. Убедитесь, что Вы используете верный идентификатор. Получить ID по короткому имени можно методом utils.resolveScreenName.';
    114:
      ErrStr :=
        'Недопустимый идентификатор альбома.';
    118:
      ErrStr :=
        'Недопустимый сервер.';
    119:
      ErrStr :=
        'Недопустимое название.';
    121:
      ErrStr :=
        'Неверный хэш.';
    122:
      ErrStr :=
        'Неверные идентификаторы фотографий.';
    125:
      ErrStr :=
        'Invalid group id';
    129:
      ErrStr :=
        'Недопустимый формат фотографии';
    140:
      ErrStr :=
        'Страница не найдена.';
    141:
      ErrStr :=
        'Нет доступа к странице.';
    148:
      ErrStr :=
        'Пользователь не установил приложение в левое меню';
    150:
      ErrStr :=
        'Неверный timestamp. Получить актуальное значение Вы можете методом utils.getServerTime.';
    180:
      ErrStr :=
        'Заметка не найдена.';
    181:
      ErrStr :=
        'Нет доступа к заметке.';
    182:
      ErrStr :=
        'Вы не можете оставлять комментарии к этой заметке.';
    183:
      ErrStr :=
        'Нет доступа к комментарию.';
    200:
      ErrStr :=
        'Доступ к альбому запрещён. Убедитесь, что Вы используете верные идентификаторы (для пользователей owner_idположительный, для сообществ — отрицательный), и доступ к запрашиваемому контенту для текущего пользователя есть в полной версии сайта.';
    201:
      ErrStr :=
        'Доступ к аудио запрещён. Убедитесь, что Вы используете верные идентификаторы (для пользователей owner_idположительный, для сообществ — отрицательный), и доступ к запрашиваемому контенту для текущего пользователя есть в полной версии сайта.';
    203:
      ErrStr :=
        'Доступ к группе запрещён. Убедитесь, что текущий пользователь является участником или руководителем сообщества (для закрытых и частных групп и встреч).';
    204:
      ErrStr :=
        'Нет доступа.';
    210:
      ErrStr :=
        'Нет доступа к записи.';
    211:
      ErrStr :=
        'Нет доступа к комментариям на этой стене.';
    212:
      ErrStr :=
        'Access to post comments denied.';
    214:
      ErrStr :=
        'Нет прав на добавление поста.';
    219:
      ErrStr :=
        'Рекламный пост уже недавно добавлялся.';
    220:
      ErrStr :=
        'Слишком много получателей.';
    221:
      ErrStr :=
        'Пользователь выключил трансляцию названий аудио в статус';
    222:
      ErrStr :=
        'Запрещено размещать ссылки.';
    224:
      ErrStr :=
        'Too many ads posts';
    225:
      ErrStr :=
        'Donut is disabled';
    250:
      ErrStr :=
        'Нет доступа к опросу.';
    251:
      ErrStr :=
        'Недопустимый идентификатор опроса.';
    252:
      ErrStr :=
        'Недопустимый идентификатор ответа.';
    253:
      ErrStr :=
        'Access denied, please vote first';
    260:
      ErrStr :=
        'Access to the groups list is denied due to the user''s privacy settings';
    300:
      ErrStr :=
        'Альбом переполнен. Перед продолжением работы нужно удалить лишние объекты из альбома или использовать другой альбом.';
    302:
      ErrStr :=
        'Создано максимальное количество альбомов.';
    500:
      ErrStr :=
        'Действие запрещено. Вы должны включить переводы голосов в настройках приложения. Проверьте настройки приложения: https://vk.com/editapp?id={Ваш API_ID}&section=payments';
    504:
      ErrStr :=
        'Not enough money on owner''s balance';
    600:
      ErrStr :=
        'Нет прав на выполнение данных операций с рекламным кабинетом.';
    603:
      ErrStr :=
        'Произошла ошибка при работе с рекламным кабинетом.';
    700:
      ErrStr :=
        'Невозможно изменить полномочия создателя.';
    701:
      ErrStr :=
        'Пользователь должен состоять в сообществе.';
    702:
      ErrStr :=
        'Достигнут лимит на количество руководителей в сообществе.';
    703:
      ErrStr :=
        'You need to enable 2FA for this action';
    704:
      ErrStr :=
        'Вы не можете назначить пользователя руководителем, если у Вас не подключена функция подтверждения входа.';
    706:
      ErrStr :=
        'Too many addresses in club';
    711:
      ErrStr :=
        'Application is not installed in community';
    800:
      ErrStr :=
        'Это видео уже добавлено.';
    801:
      ErrStr :=
        'Comments for this video are closed';
    900:
      ErrStr :=
        'Нельзя отправлять сообщение пользователю из черного списка';
    901:
      ErrStr :=
        'Пользователь не давал разрешения на отправку сообщений';
    902:
      ErrStr :=
        'Нельзя отправлять сообщения этому пользователю в связи с настройками приватности';
    907:
      ErrStr :=
        'Значение ts или pts слишком маленькое, получите новое значение.';
    908:
      ErrStr :=
        'Значение ts или pts слишком большое, получите новое значение.';
    909:
      ErrStr :=
        'Невозможно отредактировать сообщение после 24 часов';
    910:
      ErrStr :=
        'Невозможно отредактировать сообщение, поскольку оно слишком большое';
    911:
      ErrStr :=
        'Keyboard format is invalid';
    912:
      ErrStr :=
        'This is a chat bot feature, change this status in settings';
    913:
      ErrStr :=
        'Слишком много пересланных сообщений';
    914:
      ErrStr :=
        'Сообщение слишком длинное';
    917:
      ErrStr :=
        'У вас нет доступа в эту беседу';
    919:
      ErrStr :=
        'Вам недоступны ссылки для приглашения в этот чат.';
    920:
      ErrStr :=
        'Невозможно отредактировать сообщение такого типа';
    921:
      ErrStr :=
        'Невозможно переслать выбранные сообщения';
    924:
      ErrStr :=
        'Невозможно удалить сообщение для получателей';
    925:
      ErrStr :=
        'You are not admin of this chat';
    931:
      ErrStr :=
        'You can''t change invite link for this chat';
    932:
      ErrStr :=
        'Your community can''t interact with this peer';
    935:
      ErrStr :=
        'Такого пользователя в чате нет.';
    936:
      ErrStr :=
        'Contact not found';
    940:
      ErrStr :=
        'Too many posts in messages';
    942:
      ErrStr :=
        'Cannot pin one-time story';
    943:
      ErrStr :=
        'Cannot use this intent';
    944:
      ErrStr :=
        'Limits overflow for this intent';
    945:
      ErrStr :=
        'Chat was disabled';
    946:
      ErrStr :=
        'Chat not supported';
    949:
      ErrStr :=
        'Can''t edit pinned message yet';
    950:
      ErrStr :=
        'Can''t send message, reply timed out';
    1160:
      ErrStr :=
        'Оригинал фотографии был изменен.';
    1170:
      ErrStr :=
        'Слишком много списков новостей (максимум 10)';
    1260:
      ErrStr :=
        'Invalid screen name';
    1310:
      ErrStr :=
        'Каталог не доступен для пользователя';
    1311:
      ErrStr :=
        'Категории каталога не доступны для пользователя';
    1400:
      ErrStr :=
        'Товар невозможно восстановить, прошло слишком много времени с момента удаления.';
    1401:
      ErrStr :=
        'Comments for this market are closed';
    1402:
      ErrStr :=
        'Подборка с заданным идентификатором не найдена.';
    1403:
      ErrStr :=
        'Товар с заданным идентификатором не найден.';
    1404:
      ErrStr :=
        'Товар уже добавлен в выбранную подборку.';
    1405:
      ErrStr :=
        'Превышен лимит на количество товаров (15000).';
    1406:
      ErrStr :=
        'Превышен лимит на количество товаров в подборке.';
    1407:
      ErrStr :=
        'Превышен лимит на количество подборок.';
    1408:
      ErrStr :=
        'Недопустимые ссылки в описании товара.';
    1409:
      ErrStr :=
        'Shop not enabled';
    1416:
      ErrStr :=
        'Variant not found';
    1417:
      ErrStr :=
        'Property not found';
    1425:
      ErrStr :=
        'Grouping must have two or more items';
    1426:
      ErrStr :=
        'Item must have distinct properties';
    1427:
      ErrStr :=
        'Cart is empty';
    1429:
      ErrStr :=
        'Specify width, length, height and weight all together';
    1430:
      ErrStr :=
        'VK Pay status can not be changed';
    2000:
      ErrStr :=
        'Нельзя добавить больше 10 серверов';
    3102:
      ErrStr :=
        'Specified link is incorrect (can''t find source)';
    3300:
      ErrStr :=
        'Recaptcha needed';
    3301:
      ErrStr :=
        'Phone validation needed';
    3302:
      ErrStr :=
        'Password validation needed';
    3303:
      ErrStr :=
        'Otp app validation needed';
    3304:
      ErrStr :=
        'Email confirmation needed';
    3305:
      ErrStr :=
        'Assert votes';
  else
    ErrStr :=
      'Неизвестная ошибка';
  end;

  Result := ErrStr;
end;

{ MessageFlags }

class function MessageFlags.Create(Data: Integer): TMessageFlags;
var
  i: TMessageFlag;
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

class function MessageFlags.FlagDataToFlag(FlagData: Integer): TMessageFlag;
var
  i: TMessageFlag;
begin
  Result := mfChat;
  for i := Low(VkMessageFlags) to High(VkMessageFlags) do
    if VkMessageFlags[i] = FlagData then
      Exit(i);
end;

class function MessageFlags.ToString(Flags: TMessageFlags): string;
var
  Flag: TMessageFlag;
begin
  for Flag in Flags do
    Result := Result + Flag.ToString + ' ';
end;

{ TMessageChangeTypeHelper }

function TMessageChangeTypeHelper.ToString: string;
begin
  case Self of
    fcFlagsReplace:
      Result := 'Replace';
    fcFlagsSet:
      Result := 'Set';
    fcFlagsReset:
      Result := 'Reset';
  else
    Exit('');
  end;
end;

{ TVkPhotoSystemAlbumHelper }

function TVkPhotoSystemAlbumHelper.ToString: string;
begin
  case Self of
    saWall:
      Exit('wall');
    saSaved:
      Exit('saved');
    saProfile:
      Exit('profile');
  else
    Result := '';
  end;
end;

function TVkPhotoSystemAlbumHelper.ToVkId: Integer;
begin
  case Self of
    saWall:
      Exit(-7);
    saSaved:
      Exit(-15);
    saProfile:
      Exit(-6);
  else
    Result := 0;
  end;
end;

{ TVkPhotoFeedTypeHelper }

function TVkPhotoFeedTypeHelper.ToString: string;
begin
  case Self of
    ftPhoto:
      Exit('photo');
    ftPhotoTag:
      Exit('photo_tag');
  else
    Result := '';
  end;
end;

{ DialogFlags }

class function DialogFlags.Create(Data: Integer): TDialogFlags;
var
  i: Integer;
begin
  Result := [];
  for i := Ord(dfImportant) to Ord(dfUnanswered) do
  begin
    if (Data - VkDialogFlags[TDialogFlag(i)]) >= 0 then
    begin
      Include(Result, FlagDataToFlag(VkDialogFlags[TDialogFlag(i)]));
      Data := Data - VkDialogFlags[TDialogFlag(i)];
    end;
  end;
end;

class function DialogFlags.FlagDataToFlag(FlagData: Integer): TDialogFlag;
begin
  case FlagData of
    GR_IMPORTANT:
      Exit(dfImportant);
    GR_UNANSWERED:
      Exit(dfUnanswered);
  else
    Exit(dfUnanswered);
  end;
end;

class function DialogFlags.ToString(Flags: TDialogFlags): string;
var
  Flag: TDialogFlag;
begin
  for Flag in Flags do
    case Flag of
      dfImportant:
        Result := Result + 'Important ';
      dfUnanswered:
        Result := Result + 'Unanswered ';
    end;
end;

{ TMessageFlagsHelper }

function TMessageFlagsHelper.ToString: string;
begin
  Result := MessageFlags.ToString(Self);
end;

{ TDialogFlagsHelper }

function TDialogFlagsHelper.ToString: string;
begin
  Result := DialogFlags.ToString(Self);
end;

{ TChatChangeInfoTypeHelper }

function TChatChangeInfoTypeHelper.ToString: string;
begin
  case Self of
    citNone:
      Exit('');
    citName:
      Exit('Изменилось название беседы');
    citPic:
      Exit('Сменилась обложка беседы');
    citNewAdmin:
      Exit('Назначен новый администратор');
    citFixMessage:
      Exit('Закреплено сообщение');
    citJoin:
      Exit('Пользователь присоединился к беседе');
    citLeave:
      Exit('Пользователь покинул беседу');
    citKick:
      Exit('Пользователя исключили из беседы');
    citUnadmin:
      Exit('С пользователя сняты права администратора');
  else
    Exit('');
  end;
end;

{ TArrayOfIntegerHelper }

function TArrayOfIntegerHelper.Add(Value: Integer): Integer;
begin
  Result := System.Length(Self) + 1;
  SetLength(Self, Result);
  Self[Result - 1] := Value;
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
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + Self[i];
  end;
end;

procedure TArrayOfStringHelper.Assign(Source: TStrings);
var
  i: Integer;
begin
  SetLength(Self, Source.Count);
  for i := 0 to Source.Count - 1 do
    Self[i] := Source[i];
end;

{ TPermissionsHelper }

{$IFDEF OLD_VERSION}

function TPermissionsHelper.ToString: string;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
  begin
    if i <> Low(Self) then
      Result := Result + ',';
    Result := Result + Self[i];
  end;
end;

procedure TPermissionsHelper.Assign(Source: TStrings);
var
  i: Integer;
begin
  SetLength(Self, Source.Count);
  for i := 0 to Source.Count - 1 do
  begin
    Self[i] := Source[i];
  end;
end;

{$ENDIF}

{ TParamsHelper }

function TParamsHelper.Add(Param: TParam): Integer;
begin
  Result := AddParam(Self, Param);
end;

function TParamsHelper.Add(Key, Value: string): Integer;
begin
  Result := AddParam(Self, [Key, Value]);
end;

function TParamsHelper.Add(Key: string; Value: Integer): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: Boolean): Integer;
begin
  Result := AddParam(Self, [Key, BoolToString(Value)]);
end;

function TParamsHelper.Add(Key: string; Value: TArrayOfInteger): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
end;

function TParamsHelper.Add(Key: string; Value: TDateTime): Integer;
begin
  Result := AddParam(Self, [Key, DateTimeToUnix(Value).ToString]);
end;

function TParamsHelper.Add(Key: string; Value: Extended): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
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

function TParamsHelper.Add(Key: string; Value: TArrayOfString): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
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

{ TMessageFlagHelper }

function TMessageFlagHelper.ToString: string;
begin
  Result := VkMessageFlagTypes[Self];
end;

{ TAudioGenreHelper }

function TAudioGenreHelper.ToConst: Integer;
begin
  Result := VkAudioGenres[Self];
end;

function TAudioGenreHelper.ToString: string;
begin
  Result := VkAudioGenresStr[Self];
end;

{ AudioGenre }

class function AudioGenre.Create(Value: Integer): TAudioGenre;
var
  i: TAudioGenre;
begin
  Result := agOther;
  for i := Low(VkAudioGenres) to High(VkAudioGenres) do
    if VkAudioGenres[i] = Value then
      Exit(i);
end;

{ TGroupJoinTypeHelper }

function TGroupJoinTypeHelper.ToString: string;
begin
 {
  join — пользователь вступил в группу или мероприятие (подписался на публичную страницу).
  unsure — для мероприятий: пользователь выбрал вариант «Возможно, пойду».
  accepted — пользователь принял приглашение в группу или на мероприятие.
  approved — заявка на вступление в группу/мероприятие была одобрена руководителем сообщества.
  request — пользователь подал заявку на вступление в сообщество.
 }
  case Self of
    jtUnknown:
      Exit('');
    jtJoin:
      Exit('join');
    jtUnsure:
      Exit('unsure');
    jtAccepted:
      Exit('accepted');
    jtApproved:
      Exit('approved');
    jtRequest:
      Exit('request');
  else
    Exit('');
  end;
end;

{ GroupJoinType }

class function GroupJoinType.Create(Value: string): TVkGroupJoinType;
begin
  Value := LowerCase(Value);
  if Value = 'join' then
    Exit(jtJoin);
  if Value = 'unsure' then
    Exit(jtUnsure);
  if Value = 'accepted' then
    Exit(jtAccepted);
  if Value = 'approved' then
    Exit(jtApproved);
  if Value = 'request' then
    Exit(jtRequest);
  Result := jtUnknown;
end;

{ TUserBlockReasonHelper }

function TUserBlockReasonHelper.ToConst: Integer;
begin
  Result := Ord(Self);
end;

function TUserBlockReasonHelper.ToString: string;
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

{TVkMessageInfo}

function TVkMessageInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkMessageInfo.FromJsonString(AJsonString: string): TVkMessageInfo;
begin
  result := TJson.JsonToObject<TVkMessageInfo>(AJsonString)
end;

{ TVkMessageAttachmentInfo }

function TVkMessageAttachmentInfo.GetCount: Integer;
begin
  if FAttach1_type.IsEmpty then
    Exit(0);
  if FAttach2_type.IsEmpty then
    Exit(1);
  if FAttach3_type.IsEmpty then
    Exit(2);
  if FAttach4_type.IsEmpty then
    Exit(3);
  if FAttach5_type.IsEmpty then
    Exit(4);
  if FAttach6_type.IsEmpty then
    Exit(5);
  if FAttach7_type.IsEmpty then
    Exit(6);
  if FAttach8_type.IsEmpty then
    Exit(7);
  if FAttach9_type.IsEmpty then
    Exit(8);
  if FAttach10_type.IsEmpty then
    Exit(9);
  Result := 10;
end;

function TVkMessageAttachmentInfo.GetAttachments(Index: Integer): TAttachInfoType;
begin
  case Index of
    1:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach1, FAttach1_type);
    2:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach2, FAttach2_type);
    3:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach3, FAttach3_type);
    4:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach4, FAttach4_type);
    5:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach5, FAttach5_type);
    6:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach6, FAttach6_type);
    7:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach7, FAttach7_type);
    8:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach8, FAttach8_type);
    9:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach9, FAttach9_type);
    10:
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach10,
        FAttach10_type);
  end;
end;

function TVkMessageAttachmentInfo.ToArray: TArray<TAttachInfoType>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Attachments[i];
end;

function TVkMessageAttachmentInfo.ToArrayOfString: TArrayOfString;
var
  i: Integer;
begin
  SetLength(Result, Self.Count);
  for i := 0 to Self.Count - 1 do
    Result[i] := Self.Attachments[i].Attach;
end;

function TVkMessageAttachmentInfo.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(self);
end;

class function TVkMessageAttachmentInfo.FromJsonString(AJsonString: string): TVkMessageAttachmentInfo;
begin
  Result := TJson.JsonToObject<TVkMessageAttachmentInfo>(AJsonString)
end;

{ TVkMessageAttachmentInfo.TAttachInfoType }

class function TVkMessageAttachmentInfo.TAttachInfoType.Create(AAttach, AAttachType: string): TAttachInfoType;
begin
  Result.Attach := AAttach;
  Result.AttachType := AAttachType;
end;

{ TResponse }

{$WARNINGS OFF}
function TResponse.GetJSONValue: TJSONValue;
begin
  if not JSON.IsEmpty then
    Result := TJSONObject.ParseJSONValue(UTF8ToString(JSON))
  else
    Result := nil;
end;

function TResponse.GetValue<T>(const Field: string; var Value: T): Boolean;
var
  JSONItem: TJSONValue;
begin
  try
    JSONItem := TJSONObject.ParseJSONValue(Response);
    try
      Result := JSONItem.TryGetValue<T>(Field, Value);
    finally
      JSONItem.Free;
    end;
  except
    Result := False;
  end;
end;

function TResponse.ResponseIsFalse: Boolean;
begin
  Result := Response = '0';
end;

function TResponse.ResponseAsInt(var Value: Integer): Boolean;
begin
  Result := TryStrToInt(Response, Value);
end;

function TResponse.ResponseAsStr(var Value: string): Boolean;
begin
  Result := True;
  Value := Response;
end;

function TResponse.ResponseIsTrue: Boolean;
begin
  Result := Response = '1';
end;

function TResponse.GetJSONResponse: TJSONValue;
begin
  if not Response.IsEmpty then
    Result := TJSONObject.ParseJSONValue(UTF8ToString(Response))
  else
    Result := nil;
end;
{$WARNINGS ON}

{ TVkAttachmentTypeHelper }

class function TVkAttachmentTypeHelper.Create(Value: string): TVkAttachmentType;
var
  i: TVkAttachmentType;
begin
  Result := atUnknown;
  for i := Low(VkAttachmentType) to High(VkAttachmentType) do
    if VkAttachmentType[i] = Value then
      Exit(i);
end;

function TVkAttachmentTypeHelper.ToString: string;
begin
  Result := VkAttachmentType[Self];
end;

{ TVkPeerTypeHelper }

class function TVkPeerTypeHelper.Create(Value: string): TVkPeerType;
var
  i: TVkPeerType;
begin
  Result := ptUnknown;
  for i := Low(VkPeerType) to High(VkPeerType) do
    if VkPeerType[i] = Value then
      Exit(i);
end;

function TVkPeerTypeHelper.ToString: string;
begin
  Result := VkPeerType[Self];
end;

{ TVkPostTypeHelper }

function TVkPostTypeHelper.ToString: string;
begin
  Result := VKPostType[Self];
end;

{ TVkPermissionHelper }

function TVkPermissionHelper.ToString: string;
begin
  Result := VkPremissionStr[Self];
end;

{ TVkPermissionsHelper }

function TVkPermissionsHelper.ToString: string;
var
  Item: TVkPermission;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkFriendFieldHelper }

function TVkFriendFieldHelper.ToString: string;
begin
  Result := VkFriendField[Self];
end;

{ TVkFriendFieldsHelper }

class function TVkFriendFieldsHelper.All: TVkFriendFields;
begin
  Result := [ffNickName, ffDomain, ffSex, ffBirthDate, ffCity, ffCountry, ffTimeZone, ffPhoto50, ffPhoto100, ffPhoto200,
    ffHasMobile, ffContacts, ffEducation, ffOnline, ffRelation, ffLastSeen, ffStatus, ffCanWritePrivateMessage,
    ffCanSeeAllPosts, ffCanPost, ffUniversities];
end;

function TVkFriendFieldsHelper.ToString: string;
var
  Item: TVkFriendField;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
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
  Result := [gfCity, gfCountry, gfPlace, gfDescription, gfWikiPage, gfMembersCount, gfCounters, gfStartDate,
    gfFinishDate, gfCanPost, gfCanSeeAllPosts, gfActivity, gfStatus, gfContacts, gfLinks, gfFixedPost, gfVerified,
    gfSite, gfCanCreateTopic, gfPhoto50];
end;

function TVkGroupFieldsHelper.ToString: string;
var
  Item: TVkGroupField;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkUserFieldsHelper }

class function TVkUserFieldsHelper.All: TVkUserFields;
begin
  Result := [ufPhotoId, ufVerified, ufSex, ufBirthDate, ufCity, ufCountry, ufHomeTown, ufHasPhoto, ufPhoto50, ufPhoto100,
    ufPhoto200Orig, ufPhoto200, ufPhoto400Orig, ufPhotoMax, ufPhotoMaxOrig, ufOnline, ufDomain, ufHasMobile, ufContacts,
    ufSite, ufEducation, ufUniversities, ufSchools, ufStatus, usLastSeen, ufFollowersCount, ufCommonCount, ufOccupation,
    ufNickname, ufRelatives, ufRelation, ufPersonal, ufConnections, ufExports, ufActivities, ufInterests, ufMusic,
    ufMovies, ufTV, ufBooks, ufGames,
    ufAbout, ufQuotes, ufCanPost, ufCanSeeAllPosts, ufCanSeeAudio, ufCanWritePrivateMessage, ufCanSendFriendRequest, ufIsFavorite, ufIsHiddenFromFeed, ufTimeZone, ufScreenName, ufMaidenName, ufCropPhoto, ufIsFriend, ufFriendStatus, ufCareer, ufMilitary, ufBlacklisted, ufBlacklistedByMe, ufCanBeInvitedGroup];
end;

function TVkUserFieldsHelper.ToString: string;
var
  Item: TVkUserField;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkUserFieldHelper }

function TVkUserFieldHelper.ToString: string;
begin
  Result := VkUserField[Self];
end;

{ TVkFollowerFieldHelper }

function TVkFollowerFieldHelper.ToString: string;
begin
  Result := VkFollowerField[Self];
end;

{ TVkFollowerFieldsHelper }

class function TVkFollowerFieldsHelper.All: TVkFollowerFields;
begin
  Result := [flPhotoId, flVerified, flSex, flBirthDate, flCity, flCountry, flHomeTown, flHasPhoto, flPhoto50, flPhoto100,
    flPhoto200Orig, flPhoto200, flPhoto400Orig, flPhotoMax, flPhotoMaxOrig, flOnline, flLists, flDomain, flHasMobile,
    flContacts, flSite, flEducation, flUniversities, flSchools, flStatus, flLastSeen, flFollowersCount, flCommonCount,
    flOccupation, flNickName, flRelatives, flRelation, flPersonal, flConnections, flExports, flWallComments,
    flActivities, flInterests, flMusic, flMovies,
    flTV, flBooks, flGames, flAbout, flQuotes, flCanPost, flCanSeeAllPosts, flCanSeeAudio, flCanWritePrivateMessage, flCanSendFriendRequest, flIsFavorite, flIsHiddenFromFeed, flTimeZone, flScreenName, flMaidenName, flCropPhoto, flIsFriend, flFriendStatus, flCareer, flMilitary, flBlacklisted, flBlacklistedByMe];
end;

function TVkFollowerFieldsHelper.ToString: string;
var
  Item: TVkFollowerField;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkGroupFilterHelper }

function TVkGroupFilterHelper.ToString: string;
begin
  Result := VkGroupFilter[Self];
end;

{ TVkGroupFiltersHelper }

class function TVkGroupFiltersHelper.All: TVkGroupFilters;
begin
  Result := [gftAdmin, gftEditor, gftModer, gftAdvertiser, gftGroups, gftPublics, gftEvents, gftHasAddress];
end;

function TVkGroupFiltersHelper.ToString: string;
var
  Item: TVkGroupFilter;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkGroupMemberFieldHelper }

function TVkGroupMemberFieldHelper.ToString: string;
begin
  Result := VkGroupMemberField[Self];
end;

{ TVkGroupMemberFieldsHelper }

class function TVkGroupMemberFieldsHelper.All: TVkGroupMemberFields;
begin
  Result := [mfSex, mfBdate, mfCity, mfCountry, mfPhoto50, mfPhoto100, mfPhoto200orig, mfPhoto200,
    mfPhoto400orig, mfPhotoMax, mfPhotoMaxOrig, mfOnline, mfOnlineMobile, mfLists, mfDomain, mfHasMobile, mfContacts,
    mfConnections, mfSite, mfEducation, mfUniversities, mfSchools, mfCanPost, mfCanSeeAllPosts, mfCanSeeAudio,
    mfCanWritePrivateMessage, mfStatus, mfLastSeen, mfCommonCount, mfRelation, mfRelatives];
end;

function TVkGroupMemberFieldsHelper.ToString: string;
var
  Item: TVkGroupMemberField;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkGroupMembersFilterHelper }

function TVkGroupMembersFilterHelper.ToString: string;
begin
  case Self of
    gmfFriends:
      Exit('friends');
    mgfUnsure:
      Exit('unsure');
    gmfManagers:
      Exit('managers');
  else
    Result := '';
  end;
end;

{ TVkSortIdTimeHelper }

function TVkSortIdTimeHelper.ToString: string;
begin
  case Self of
    sitIdAsc:
      Exit('id_asc');
    sitIdDesc:
      Exit('id_desc');
    sitTimeAsc:
      Exit('time_asc');
    sitTimeDesc:
      Exit('time_desc');
  else
    Result := '';
  end;
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
  begin
    Result := Result + Item.ToString + ',';
  end;
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
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkGroupTypeHelper }

function TVkGroupTypeHelper.ToString: string;
begin
  Result := VkGroupType[Self];
end;

{ TVkGroupAccessHelper }

function TVkGroupAccessHelper.ToConst: Integer;
begin
  Result := Ord(Self);
end;

{ TVkAgeLimitsHelper }

function TVkAgeLimitsHelper.ToConst: Integer;
begin
  Result := Ord(Self);
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
  Result := [gafTitle, gafAddress, gafAdditionalAddress, gafCountryId, gafCityId, gafMetroStationId,
    gafLatitude, gafLongitude, gafWorkInfoStatus, gafTimeOffset];
end;

function TVkGroupAddressFieldsHelper.ToString: string;
var
  Item: TVkGroupAddressField;
begin
  for Item in Self do
  begin
    Result := Result + Item.ToString + ',';
  end;
  Result.TrimRight([',']);
end;

{ TVkSortHelper }

function TVkSortHelper.ToString: string;
begin
  case Self of
    stAsc:
      Result := 'asc';
    stDesc:
      Result := 'desc';
  else
    Result := '';
  end;
end;

{ TVkPhotoReportReasonHelper }

function TVkMediaReportReasonHelper.ToConst: Integer;
begin
  Result := Ord(Self);
end;

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

procedure TVkMethodException.SetCode(const Value: Integer);
begin
  FCode := Value;
end;

{ TVkUserReportHelper }

function TVkUserReportHelper.ToString: string;
begin
  case Self of
    urPorn:
      Result := 'porn';
    urSpam:
      Result := 'spam';
    urInsult:
      Result := 'insult';
    urAdvertisеment:
      Result := 'advertisеment';
  else
    Result := ''
  end;
end;

{ Attachment }

class function Attachment.Audio(Id, OwnerId: Integer; AccessKey: string): string;
begin
  Result := Create('audio', Id, OwnerId, AccessKey);
end;

class function Attachment.Create(&Type: string; OwnerId, Id: Integer; AccessKey: string): string;
begin
  Result := &Type + OwnerId.ToString + '_' + Id.ToString;
  if not AccessKey.IsEmpty then
    Result := Result + '_' + AccessKey;
end;

class function Attachment.Doc(Id, OwnerId: Integer; AccessKey: string): string;
begin
  Result := Create('doc', Id, OwnerId, AccessKey);
end;

class function Attachment.Video(Id, OwnerId: Integer; AccessKey: string): string;
begin
  Result := Create('video', Id, OwnerId, AccessKey);
end;

class function Attachment.Album(Id, OwnerId: Integer; AccessKey: string): string;
begin
  Result := Create('album', Id, OwnerId, AccessKey);
end;

{ TVkPostLinkButtonHelper }

function TVkPostLinkButtonHelper.ToString: string;
begin
  Result := VKPostLinkButton[Self];
end;

{ TVkLinkStatusTypeHelper }

class function TVkLinkStatusTypeHelper.FromString(Value: string): TVkLinkStatusType;
begin
  if Value = 'not_banned' then
    Result := lsNotBanned
  else if Value = 'banned' then
    Result := lsBanned
  else if Value = 'processing' then
    Result := lsProcessing
  else
    Result := lsProcessing;
end;

function TVkLinkStatusTypeHelper.ToString: string;
begin
  case Self of
    lsNotBanned:
      Result := 'not_banned';
    lsBanned:
      Result := 'banned';
    lsProcessing:
      Result := 'processing';
  end;
end;

end.

