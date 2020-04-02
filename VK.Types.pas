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
    function ToString: string; overload; inline;
    procedure Assign(Source: TStrings); overload;
    function IsEmpty: Boolean;
  end;

  {$IFDEF OLD_VERSION}
  TArrayOfInteger = array of Integer;
  {$ELSE}

  TArrayOfInteger = TArray<Integer>;
  {$ENDIF}

  TArrayOfIntegerHelper = record helper for TArrayOfInteger
    function ToString: string; overload; inline;
    function Add(Value: Integer): Integer;
  end;

  TFields = TArrayOfString;

  TParam = TArrayOfString;

  TParamInt = TArrayOfInteger;

  TUserIds = TArrayOfInteger;

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
    function Add(Key: string; Value: Boolean): Integer; overload; inline;
    function Add(Key: string; Value: TIds): Integer; overload; inline;
  end;

  TPremission = string;

  {$IFDEF OLD_VERSION}
  TPermissions = array of TPremission;

  TPermissionsHelper = record helper for TPermissions
    function ToString: string; overload; inline;
    procedure Assign(Source: TStrings); overload;
  end;
  {$ELSE}

  TPermissions = TArray<TPremission>;
  {$ENDIF}

  TAttachmentArray = TArrayOfString;

  //Флаги сообщений
  TMessageFlag = (mfUNKNOWN_9, mfUNKNOWN_8, mfUNKNOWN_7, mfUNKNOWN_6, mfNotDelivered, mfDeleteForAll,
    mfHidden, mfUNKNOWN_5, mfUNKNOWN_4, mfUnreadMultichat, mfUNKNOWN_3, mfUNKNOWN_2, mfUNKNOWN_1,
    mfMedia, mfFixed, mfDeleted, mfSpam, mfFriends, mfChat, mfImportant, mfReplied, mfOutbox, mfUnread);

  TMessageFlagHelper = record helper for TMessageFlag
    function ToString: string; inline;
  end;

  TMessageFlags = set of TMessageFlag;

  TMessageFlagsHelper = record helper for TMessageFlags
    function ToString: string; overload; inline;
  end;

  MessageFlags = class
  public
    class function FlagDataToFlag(FlagData: Integer): TMessageFlag;
    class function Create(Data: Integer): TMessageFlags;
    class function ToString(Flags: TMessageFlags): string; overload;
  end;

  //Жанры музыки
  TAudioGenre = (agNone, agRock, agPop, agRapAndHipHop, agEasyListening, agHouseAndDance,
    agInstrumental, agMetal, agAlternative, agDubstep, agJazzAndBlues, agDrumAndBass, agTrance,
    agChanson, agEthnic, agAcousticAndVocal, agReggae, agClassical, agIndiePop, agSpeech,
    agElectropopAndDisco, agOther);

  TAudioGenreHelper = record helper for TAudioGenre
    function ToConst: Integer;
    function ToString: string; inline;
  end;

  AudioGenre = class
    class function Create(Value: Integer): TAudioGenre;
  end;

  TVkUserField = (ufSex, ufBDate, ufCity, ufCountry, ufPhoto50, ufPhoto100, ufPhoto200Orig,
    ufPhoto200, ufPhoto400Orig, ufPhotoMax, ufPhotoMaxOrig, ufOnline, ufOnlineMobile, ufLists,
    ufDomain, ufHasMobile, ufContacts, ufConnections, ufSite, ufEducation, ufUniversities, ufSchools,
    ufCanPost, ufCanSeeAllPosts, ufCanSeeAudio, ufCanWritePrivateMessage, ufStatus, ufLastSeen,
    ufCommonCount, ufRelation, ufRelatives);

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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkMessageAttachmentInfo;
  end;

  //Флаги диалогов
  TDialogFlag = (dfImportant, dfUnanswered);

  TDialogFlags = set of TDialogFlag;

  TDialogFlagsHelper = record helper for TDialogFlags
    function ToString: string; overload; inline;
  end;

  DialogFlags = class
    class function FlagDataToFlag(FlagData: Integer): TDialogFlag;
    class function Create(Data: Integer): TDialogFlags;
    class function ToString(Flags: TDialogFlags): string; overload;
  end;

  //Идентификатор типа изменения в чате
  TChatChangeInfoType = (citNone, citName, citPic, citNewAdmin, citFixMessage, citJoin, citLeave,
    citKick, citUnadmin);

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
  TVkItemType = (itPost, itComment, itPhoto, itAudio, itVideo, itNote, itMarket, itPhotoComment,
    itVideoComment, itTopicComment, itMarketComment, itSitepage);

  TVkItemTypeHelper = record helper for TVkItemType
    function ToConst: string; inline;
  end;

  //Типы объектов
  TVkAttachmentType = (atUnknown, atPhoto, atVideo, atAudio, atDoc, atLink, atMarket, atMarketAlbum,
    atWall, atWalReply, atSticker, atGift, atCall);

  TVkAttachmentTypeHelper = record helper for TVkAttachmentType
    function ToConst: string; inline;
    class function Create(Value: string): TVkAttachmentType; static;
  end;

  TVkPeerType = (ptUnknown, ptUser, ptChat, ptGroup, ptEmail);

  TVkPeerTypeHelper = record helper for TVkPeerType
    function ToConst: string; inline;
    class function Create(Value: string): TVkPeerType; static;
  end;

  TVkNameCase = (ncNom, ncGen, ncDat, ncAcc, ncIns, ncAbl);

  TVkNameCaseHelper = record helper for TVkNameCase
    function ToConst: string; inline;
  end;
  {
  именительный – nom, родительный – gen, дательный – dat, винительный – acc, творительный – ins, предложный – abl. По умолчанию nom.
  }

  //Пол

  TVkSex = (sxMale, sxFemale);

  //Видимость даты рождения
  TVkBirthDateVisibility = (dvVisible, dvDayMonOnly, dvHidden);

  //Отношения
  TVkRelation = (rnNone, rnNotMarried, rnHaveFriend, rnAffiance, rnMarried, rnComplicated,
    rnnActivelyLooking, rnInLove, rnCivilMarriage);
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
    function GetJSONValue: TJSONValue;
    function GetJSONResponse: TJSONValue;
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
    UserIds: TUserIds;
    PeerId, TotalCount: Integer;
    TimeStamp: TDateTime;
  end;

  TChatRecordingData = record
    UserIds: TUserIds;
    PeerId, TotalCount: Integer;
    TimeStamp: TDateTime;
  end;

  TVkUserBlockReason = (brOther, brSpam, btInsultingParticipants, btObsceneExpressions, btOffTopic);

  TUserBlockReasonHelper = record helper for TVkUserBlockReason
    function ToString: string; overload; inline;
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

  TOnLogin = procedure(Sender: TObject) of object;

  TOnAuth = procedure(Sender: TObject; Url: string; var Token: string; var TokenExpiry: Int64; var
    ChangePasswordHash: string) of object;

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

  TOnUserOnline = procedure(Sender: TObject; UserId: Integer; VkPlatform: TVkPlatform; TimeStamp:
    TDateTime) of object;

  TOnUserOffline = procedure(Sender: TObject; UserId: Integer; InactiveUser: Boolean; TimeStamp:
    TDateTime) of object;

  TOnReadMessages = procedure(Sender: TObject; Incoming: Boolean; PeerId, LocalId: Integer) of object;

  TOnRecoverOrDeleteMessages = procedure(Sender: TObject; PeerId, LocalId: Integer) of object;

  TOnChatChanged = procedure(Sender: TObject; const ChatId: Integer; IsSelf: Boolean) of object;

  TOnChatChangeInfo = procedure(Sender: TObject; const PeerId: Integer; TypeId: TChatChangeInfoType;
    Info: Integer) of object;

  TOnUserTyping = procedure(Sender: TObject; UserId, ChatId: Integer) of object;

  TOnUserCall = procedure(Sender: TObject; UserId, CallId: Integer) of object;

  TOnCountChange = procedure(Sender: TObject; Count: Integer) of object;

  TOnNotifyChange = procedure(Sender: TObject; PeerId: Integer; Sound: Boolean; DisableUntil:
    Integer) of object;

  TOnUsersTyping = procedure(Sender: TObject; Data: TChatTypingData) of object;

  TOnUsersRecording = procedure(Sender: TObject; Data: TChatRecordingData) of object;

const
  AllUserFields = 'sex, bdate, city, country, photo_50, photo_100, photo_200_orig, ' +
    'photo_200, photo_400_orig, photo_max, photo_max_orig, online, ' +
    'online_mobile, lists, domain, has_mobile, contacts, connections, ' +
    'site, education, universities, schools, can_post, can_see_all_posts, ' +
    'can_see_audio, can_write_private_message, status, last_seen, ' +
    'common_count, relation, relatives';

var
  VkMessageFlags: array[TMessageFlag] of Integer = (MF_UNKNOWN_9, MF_UNKNOWN_8,
    MF_UNKNOWN_7, MF_UNKNOWN_6, MF_NOT_DELIVERED, MF_DELETE_FOR_ALL, MF_HIDDEN,
    MF_UNKNOWN_5, MF_UNKNOWN_4, MF_UNREAD_MULTICHAT, MF_UNKNOWN_3, MF_UNKNOWN_2,
    MF_UNKNOWN_1, MF_MEDIA, MF_FIXED, MF_DELЕTЕD, MF_SPAM, MF_FRIENDS, MF_CHAT,
    MF_IMPORTANT, MF_REPLIED, MF_OUTBOX, MF_UNREAD);
  VkMessageFlagTypes: array[TMessageFlag] of string = ('Unknown_9', 'Unknown_8',
    'Unknown_7', 'Unknown_6', 'NotDelivered', 'DeleteForAll', 'Hidden',
    'Unknown_5', 'Unknown_4', 'UnreadMultichat', 'Unknown_3', 'Unknown_2',
    'Unknown_1', 'Media', 'Fixed', 'Deleted', 'Spam', 'Friends', 'Chat',
    'Important', 'Replied', 'Outbox', 'Unread');
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
  VkUserActive: array[Boolean] of string = ('Бездействие', 'Покинул сайт');
  VkPlatforms: array[TVkPlatform] of string = ('Unknown', 'Mobile', 'iPhone',
    'iPad', 'Android', 'Windows Phone', 'Windows', 'Web');
  VkAttachmentType: array[TVkAttachmentType] of string = ('', 'photo', 'video',
    'audio', 'doc', 'link', 'market', 'market_album', 'wall', 'wall_reply',
    'sticker', 'gift', 'call');
  VkPeerType: array[TVkPeerType] of string = ('', 'user', 'chat', 'group', 'email');
  VkNameCase: array[TVkNameCase] of string = ('nom', 'gen', 'dat', 'acc', 'ins', 'abl');
  VkItemType: array[TVkItemType] of string = ('post', 'comment', 'photo',
    'audio', 'video', 'note', 'market', 'photo_comment', 'video_comment',
    'topic_comment', 'market_comment', 'sitepage');
  VkGroupLevel: array[TVkGroupLevel] of string = ('Участник', 'Модератор',
    'Редактор', 'Администратор');
  VkUserBlockReason: array[TVkUserBlockReason] of string = ('Другое', 'Спам',
    'Оскорбление участников', 'Мат', 'Разговоры не по теме');
  VKGroupJoinType: array[TVkGroupJoinType] of string = ('', 'join', 'unsure',
    'accepted', 'approved', 'request');
  VkUserField: array[TVkUserField] of string = ('sex', 'bdate', 'city', 'country', 'photo_50',
    'photo_100', 'photo_200_orig',
    'photo_200', 'photo_400_orig', 'photo_max', 'photo_max_orig', 'online',
    'online_mobile', 'lists', 'domain', 'has_mobile', 'contacts', 'connections',
    'site', 'education', 'universities', 'schools', 'can_post', 'can_see_all_posts',
    'can_see_audio', 'can_write_private_message', 'status', 'last_seen',
    'common_count', 'relation', 'relatives');

function FieldsToString(Fields: TFields): string;

function VKErrorString(ErrorCode: Integer): string;

function AddParam(var Dest: TParams; Param: TParam): Integer;

function CreateAttachment(&Type: string; OwnerId, Id: Integer; AccessKey: string = ''): string;

function AppendItemsTag(JSON: string): string;

function NormalizePeerId(Value: Integer): Integer;

function PeerIdIsChat(Value: Integer): Boolean;

function PeerIdIsUser(Value: Integer): Boolean;

function PeerIdIsGroup(Value: Integer): Boolean;

implementation

uses
  VK.CommonUtils;

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

function CreateAttachment(&Type: string; OwnerId, Id: Integer; AccessKey: string): string;
begin
  Result := &Type + OwnerId.ToString + '_' + Id.ToString;
  if not AccessKey.IsEmpty then
    Result := Result + '_' + AccessKey;
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

function FieldsToString(Fields: TFields): string;
var
  i: Integer;
begin
  for i := Low(Fields) to High(Fields) do
  begin
    if i <> Low(Fields) then
      Result := Result + ',';
    Result := Result + Fields[i];
  end;
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
    5:
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
    20:
      ErrStr :=
        'Данное действие запрещено для не Standalone приложений. Если ошибка возникает несмотря на то, что Ваше приложение имеет тип Standalone, убедитесь, что при авторизации Вы используете redirect_uri=https://oauth.vk.com/blank.html.';
    21:
      ErrStr :=
        'Данное действие разрешено только для Standalone и Open API приложений.';
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
    113:
      ErrStr :=
        'Неверный идентификатор пользователя. Убедитесь, что Вы используете верный идентификатор. Получить ID по короткому имени можно методом utils.resolveScreenName.';
    148:
      ErrStr :=
        'Пользователь не установил приложение в левое меню';
    150:
      ErrStr :=
        'Неверный timestamp. Получить актуальное значение Вы можете методом utils.getServerTime.';
    200:
      ErrStr :=
        'Доступ к альбому запрещён. Убедитесь, что Вы используете верные идентификаторы (для пользователей owner_idположительный, для сообществ — отрицательный), и доступ к запрашиваемому контенту для текущего пользователя есть в полной версии сайта.';
    201:
      ErrStr :=
        'Доступ к аудио запрещён. Убедитесь, что Вы используете верные идентификаторы (для пользователей owner_idположительный, для сообществ — отрицательный), и доступ к запрашиваемому контенту для текущего пользователя есть в полной версии сайта.';
    203:
      ErrStr :=
        'Доступ к группе запрещён. Убедитесь, что текущий пользователь является участником или руководителем сообщества (для закрытых и частных групп и встреч).';
    221:
      ErrStr :=
        'Пользователь выключил трансляцию названий аудио в статус';
    300:
      ErrStr :=
        'Альбом переполнен. Перед продолжением работы нужно удалить лишние объекты из альбома или использовать другой альбом.';
    500:
      ErrStr :=
        'Действие запрещено. Вы должны включить переводы голосов в настройках приложения. Проверьте настройки приложения: https://vk.com/editapp?id={Ваш API_ID}&section=payments';
    600:
      ErrStr :=
        'Нет прав на выполнение данных операций с рекламным кабинетом.';
    603:
      ErrStr :=
        'Произошла ошибка при работе с рекламным кабинетом.';
    1260:
      ErrStr :=
        'Invalid screen name';
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
  Result := Length(Self) + 1;
  SetLength(Self, Result);
  Self[Result - 1] := Value;
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
  Result := Length(Self) = 0;
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

function TParamsHelper.Add(Key: string; Value: TIds): Integer;
begin
  Result := AddParam(Self, [Key, Value.ToString]);
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

function TVkItemTypeHelper.ToConst: string;
begin
  Result := VkItemType[Self];
end;

{ TVkNameCaseHelper }

function TVkNameCaseHelper.ToConst: string;
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

function TResponse.GetJSONValue: TJSONValue;
begin
  if not JSON.IsEmpty then
    Result := TJSONObject.ParseJSONValue(UTF8ToString(JSON))
  else
    Result := nil;
end;

function TResponse.GetJSONResponse: TJSONValue;
begin
  if not Response.IsEmpty then
    Result := TJSONObject.ParseJSONValue(UTF8ToString(Response))
  else
    Result := nil;
end;

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

function TVkAttachmentTypeHelper.ToConst: string;
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

function TVkPeerTypeHelper.ToConst: string;
begin
  Result := VkPeerType[Self];
end;

end.

