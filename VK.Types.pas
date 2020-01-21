unit VK.Types;

interface

{$INCLUDE include.inc}

uses
  System.Classes, System.Generics.Collections, System.JSON;

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
  MF_DEL�T�D = 128;
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

  {$IFDEF OLD_VERSION}
  TParams = array of TParam;
  {$ELSE}
  TParams = TArray<TParam>;
  {$ENDIF}


  TParamsHelper = record helper for TParams
    function Add(Param: TParam): Integer; overload; inline;
    function Add(Key, Value: string): Integer; overload; inline;
    function Add(Key: string; Value: Integer): Integer; overload; inline;
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

  TUserIds = TArrayOfInteger;

  TIds = TArrayOfInteger;

  //����� ���������
  TMessageFlag = (mfUnread, mfOutbox, mfReplied, mfImportant, mfChat, mfFriends, mfSpam, mfDeleted,
    mfFixed, mfMedia, mfUNKNOWN_1, mfUNKNOWN_2, mfUNKNOWN_3, mfUnreadMultichat, mfUNKNOWN_4,
    mfUNKNOWN_5, mfHidden, mfDeleteForAll, mfNotDelivered, mfUNKNOWN_6, mfUNKNOWN_7, mfUNKNOWN_8, mfUNKNOWN_9);

  TMessageFlagHelper = record helper for TMessageFlag
    function ToString: string; inline;
  end;

  TMessageFlags = set of TMessageFlag;

  TMessageFlagsHelper = record helper for TMessageFlags
    function ToString: string; overload; inline;
  end;

  MessageFlags = class
    class function FlagDataToFlag(FlagData: Integer): TMessageFlag;
    class function Create(Data: Integer): TMessageFlags;
    class function ToString(Flags: TMessageFlags): string; overload;
  end;

  //����� ������
  TAudioGenre = (agRock, agPop, agRapAndHipHop, agEasyListening, agHouseAndDance, agInstrumental,
    agMetal, agAlternative, agDubstep, agJazzAndBlues, agDrumAndBass, agTrance, agChanson, agEthnic,
    agAcousticAndVocal, agReggae, agClassical, agIndiePop, agSpeech, agElectropopAndDisco, agOther);

  TAudioGenreHelper = record helper for TAudioGenre
    function ToConst: Integer;
    function ToString: string; inline;
  end;

  AudioGenre = class
    class function Create(Data: Integer): TAudioGenre;
  end;

  //����� ��������
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

  //������������� ���� ��������� � ����
  TChatChangeInfoType = (citNone, citName, citPic, citNewAdmin, citFixMessage, citJoin, citLeave, citKick, citUnadmin);

  TChatChangeInfoTypeHelper = record helper for TChatChangeInfoType
    function ToString: string; overload; inline;
  end;

  //���������
  TVkPlatform = (pfUnknown, pfMobile, pfIPhone, pfIPad, pfAndroid, pfWindowsPhone, pfWindows, pfWeb);

  //��� ����� ������
  TFlagsChangeType = (fcFlagsReplace, fcFlagsSet, fcFlagsReset);

  TMessageChangeTypeHelper = record helper for TFlagsChangeType
    function ToString: string; overload; inline;
  end;

  //���� ��������
  TVkItemType = (itPost, itComment, itPhoto, itAudio, itVideo, itNote, itMarket, itPhotoComment,
    itVideoComment, itTopicComment, itMarketComment, itSitepage);

  TVkItemTypeHelper = record helper for TVkItemType
    function ToConst: string; inline;
  end;

  //��������� ������� ��������� ���������
  TMessageData = record
    MessageId: Integer;
    Flags: TMessageFlags;
    PeerId: Integer;
    TimeStamp: TDateTime;
    Text: string;
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
  end;

  TEventExtraFields = record
    peer_id: integer; // ������������� ����������. ��� ������������: id ������������. ��� ��������� ������: 2000000000 + id ������. ��� ����������: -id ���������� ���� id ���������� + 1000000000 (��� version = 0).
    timestamp: integer; // ����� �������� ��������� � Unixtime;
    text: string; // ����� ���������;
   //[$attachments] (array) � �������� (���� mode = 2);
   //[$random_id] (integer) � random_id, ���� �������� ��� ������� � messages.send. ����� ��������� 0, ���� �������� �� ������.
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

  TUserBlockReason = (brOther, brSpam, btInsultingParticipants, btObsceneExpressions, btOffTopic);

  TUserBlockReasonHelper = record helper for TUserBlockReason
    function ToString: string; overload; inline;
  end;

  TGroupJoinType = (jtUnknown, jtJoin, jtUnsure, jtAccepted, jtApproved, jtRequest);

  TGroupJoinTypeHelper = record helper for TGroupJoinType
    function ToString: string; overload; inline;
  end;

  GroupJoinType = class
    class function Create(Value: string): TGroupJoinType;
  end;

  TVkGroupLevel = (glNone, glModer, glEditor, glAdmin);

  TVkGroupLevelHelper = record helper for TVkGroupLevel
    function ToString: string; overload; inline;
  end;

  TOnLogin = procedure(Sender: TObject) of object;

  TOnAuth = procedure(Sender: TObject; var Token: string; var TokenExpiry: Int64; var
    ChangePasswordHash: string) of object;

  TOnConfirm = procedure(Sender: TObject; Ans: string; var Accept: Boolean) of object;

  TOnCaptcha = procedure(Sender: TObject; const CaptchaURL: string; var Answer: string) of object;

  TOnLog = procedure(Sender: TObject; const Value: string) of object;

  TOnVKError = procedure(Sender: TObject; Code: Integer; Text: string) of object;

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

  TOnChatChangeInfo = procedure(Sender: TObject; const PeerId: Integer; TypeId: TChatChangeInfoType;
    Info: Integer) of object;

  TOnUserTyping = procedure(Sender: TObject; UserId, ChatId: Integer) of object;

  TOnUserCall = procedure(Sender: TObject; UserId, CallId: Integer) of object;

  TOnCountChange = procedure(Sender: TObject; Count: Integer) of object;

  TOnNotifyChange = procedure(Sender: TObject; PeerId: Integer; Sound: Boolean; DisableUntil: Integer) of object;

  TOnUsersTyping = procedure(Sender: TObject; Data: TChatTypingData) of object;

  TOnUsersRecording = procedure(Sender: TObject; Data: TChatRecordingData) of object;

var
  VkMessageFlags: array[TMessageFlag] of Integer = (MF_UNKNOWN_9, MF_UNKNOWN_8, MF_UNKNOWN_7,
    MF_UNKNOWN_6, MF_NOT_DELIVERED, MF_DELETE_FOR_ALL, MF_HIDDEN,
    MF_UNKNOWN_5, MF_UNKNOWN_4, MF_UNREAD_MULTICHAT, MF_UNKNOWN_3, MF_UNKNOWN_2, MF_UNKNOWN_1, MF_MEDIA,
    MF_FIXED, MF_DEL�T�D, MF_SPAM, MF_FRIENDS, MF_CHAT, MF_IMPORTANT, MF_REPLIED, MF_OUTBOX, MF_UNREAD);
  VkAudioGenres: array[TAudioGenre] of Integer = (AG_ROCK, AG_POP, AG_RAPANDHIPHOP, AG_EASYLISTENING,
    AG_HOUSEANDDANCE, AG_INSTRUMENTAL, AG_METAL, AG_ALTERNATIVE, AG_DUBSTEP, AG_JAZZANDBLUES, AG_DRUMANDBASS,
    AG_TRANCE, AG_CHANSON, AG_ETHNIC, AG_ACOUSTICANDVOCAL, AG_REGGAE, AG_CLASSICAL, AG_INDIEPOP, AG_SPEECH,
    AG_ELECTROPOPANDDISCO, AG_OTHER);
  VkAudioGenresStr: array[TAudioGenre] of string = ('Rock', 'Pop', 'RapAndHipHop', 'EasyListening', 'HouseAndDance',
    'Instrumental', 'Metal', 'Alternative', 'Dubstep', 'JazzAndBlues', 'DrumAndBass', 'Trance', 'Chanson', 'Ethnic',
    'AcousticAndVocal', 'Reggae', 'Classical', 'IndiePop', 'Speech', 'ElectropopAndDisco', 'Other');
  VkDialogFlags: array[TDialogFlag] of Integer = (GR_UNANSWERED, GR_IMPORTANT);
  VkUserActive: array[Boolean] of string = ('�����������', '������� ����');
  VkPlatforms: array[TVkPlatform] of string = ('Unknown', 'Mobile', 'iPhone', 'iPad', 'Android',
    'Windows Phone', 'Windows', 'Web');

function FieldsToString(Fields: TFields): string;

function VKErrorString(ErrorCode: Integer): string;

function AddParam(var Dest: TParams; Param: TParam): Integer;

function CreateAttachment(&Type: string; OwnerId, Id: Integer; AccessKey: string = ''): string;

implementation

uses
  System.SysUtils;

function CreateAttachment(&Type: string; OwnerId, Id: Integer; AccessKey: string): string;
begin
  Result := &Type + OwnerId.ToString + '_' + Id.ToString;
  if not AccessKey.IsEmpty then
    Result := Result + '_' + AccessKey;
end;

function AddParam(var Dest: TParams; Param: TParam): Integer;
begin
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
      ErrStr := '��������� ����������� ������.����������� ��������� ������ �����.';
    2:
      ErrStr :=
        '���������� ���������.����������� �������� ���������� � �����������https://vk.com/editapp?id={��� API_ID} ��� ������������ �������� ����� (test_mode=1)';
    3:
      ErrStr :=
        '������� ����������� �����.����������, ��������� �� ������� �������� ����������� ������:�https://vk.com/dev/methods.';
    4:
      ErrStr := '�������� �������.';
    5:
      ErrStr := '����������� ������������ �� �������.����������, ��� �� ����������� ������������ �����������.';
    6:
      ErrStr :=
        '������� ����� �������� � �������.�������� ������� �������� ����� �������� ��� ����������� �����execute. ��������� �� ������������ �� ������� ������� ��. �� ��������https://vk.com/dev/api_requests.';
    7:
      ErrStr :=
        '��� ���� ��� ���������� ����� ��������.����������, �������� �� ����������� ���������� �����������. ��� ����� ������� � ������� ������account.getAppPermissions.';
    8:
      ErrStr :=
        '�������� ������.������������������� �������� ������ ������������ ���������� (��� ����� ����� �� �������� � ��������� ������).';
    9:
      ErrStr :=
        '������� ����� ���������� ��������.������ ��������� ����� ���������� ���������. ��� ����� ����������� ������ �� ������ �������������execute����JSONP.';
    10:
      ErrStr := '��������� ���������� ������ �������.����������� ��������� ������ �����.';
    11:
      ErrStr :=
        '� �������� ������ ���������� ������ ���� ��������� ��� ������������ ������ ���� ���������.���������� ���������� � �����������https://vk.com/editapp?id={��� API_ID}';
    14:
      ErrStr := '��������� ���� ���� � �������� (Captcha).';
    15:
      ErrStr :=
        '������ ��������.����������, ��� �� ����������� ������ ��������������, � ������ � �������� ��� �������� ������������ ���� � ������ ������ �����.';
    16:
      ErrStr :=
        '��������� ���������� �������� �� ���������HTTPS, �.�. ������������ ������� ���������, ��������� ������ ����� ���������� ����������.'#13#10 +
        '������ �������� ��������� ����� ������, � Standalone-���������� �� ������ �������������� ��������� ��������� ���� ��������� � ������������ �������account.getInfo.';
    17:
      ErrStr :=
        '��������� ��������� ������������.��������� ������� ������������� � ���������� ������������� ������������ �� ��������� �������� ��� ���������.';
    18:
      ErrStr := '�������� ������� ��� �������������.��������� ������������ ���� ������� ��� �������������';
    20:
      ErrStr :=
        '������ �������� ��������� ��� �� Standalone ����������.����� ������ ��������� �������� �� ��, ��� ���� ���������� ����� ��� Standalone, ���������, ��� ��� ����������� �� �����������redirect_uri=https://oauth.vk.com/blank.html.';
    21:
      ErrStr := '������ �������� ��������� ������ ��� Standalone � Open API ����������.';
    23:
      ErrStr :=
        '����� ��� ��������.���� ���������� ������ �� API, ������� �������� � ��������� ������, ����������� �����:�https://vk.com/dev/methods.';
    24:
      ErrStr := '��������� ������������� �� ������� ������������.';
    27:
      ErrStr := '���� ������� ���������� ��������������.';
    28:
      ErrStr := '���� ������� ���������� ��������������.';
    29:
      ErrStr :=
        '��������� �������������� ����� �� ����� ��������������� �� ������������ �� ���������� ������� ��. �� �������� https://vk.com/dev/data_limits';
    30:
      ErrStr :=
        '������� �������� �������������������, ������������� � �������, ���������� � ������������ ������ �������';
    33:
      ErrStr := 'Not implemented yet';
    100:
      ErrStr :=
        '���� �� ����������� ���������� ��� �� ������� ��� �������.���������� ������ ��������� ���������� � �� ������ �� �������� � ��������� ������.';
    101:
      ErrStr :=
        '�������� API ID ����������.�������� ���������� � ������ ���������������� �� ��������https://vk.com/apps?act=settings�� ������� � ������� ������API_ID�(������������� ����������).';
    113:
      ErrStr :=
        '�������� ������������� ������������.����������, ��� �� ����������� ������ �������������. �������� ID �� ��������� ����� ����� �������utils.resolveScreenName.';
    148:
      ErrStr := '������������ �� ��������� ���������� � ����� ����';
    150:
      ErrStr := '�������� timestamp.��������� ���������� �������� �� ������ �������utils.getServerTime.';
    200:
      ErrStr :=
        '������ � ������� ��������.����������, ��� �� ����������� ������ �������������� (��� �������������owner_id�������������, ��� ��������� � �������������), � ������ � �������������� �������� ��� �������� ������������ ���� � ������ ������ �����.';
    201:
      ErrStr :=
        '������ � ����� ��������.����������, ��� �� ����������� ������ �������������� (��� �������������owner_id�������������, ��� ��������� � �������������), � ������ � �������������� �������� ��� �������� ������������ ���� � ������ ������ �����.';
    203:
      ErrStr :=
        '������ � ������ ��������.����������, ��� ������� ������������ �������� ���������� ��� ������������� ���������� (��� �������� � ������� ����� � ������).';
    221:
      ErrStr := '������������ �������� ���������� �������� ����� � ������';
    300:
      ErrStr :=
        '������ ����������.������ ������������ ������ ����� ������� ������ ������� �� ������� ��� ������������ ������ ������.';
    500:
      ErrStr :=
        '�������� ���������. �� ������ �������� �������� ������� � ���������� ����������.���������� ��������� ����������:�https://vk.com/editapp?id={��� API_ID}&section=payments';
    600:
      ErrStr := '��� ���� �� ���������� ������ �������� � ��������� ���������.';
    603:
      ErrStr := '��������� ������ ��� ������ � ��������� ���������.';
    1260:
      ErrStr := 'Invalid screen name';
    3300:
      ErrStr := 'Recaptcha needed';
    3301:
      ErrStr := 'Phone validation needed';
    3302:
      ErrStr := 'Password validation needed';
    3303:
      ErrStr := 'Otp app validation needed';
    3304:
      ErrStr := 'Email confirmation needed';
    3305:
      ErrStr := 'Assert votes';
  else
    ErrStr := '����������� ������';
  end;

  Result := ErrStr;
end;

{ MessageFlags }

class function MessageFlags.Create(Data: Integer): TMessageFlags;
var
  i: Integer;
begin
  Result := [];
  for i := Ord(mfUnread) to Ord(mfUNKNOWN_6) do
  begin
    if (Data - VkMessageFlags[TMessageFlag(i)]) >= 0 then
    begin
      Include(Result, FlagDataToFlag(VkMessageFlags[TMessageFlag(i)]));
      Data := Data - VkMessageFlags[TMessageFlag(i)];
    end;
  end;
end;

class function MessageFlags.FlagDataToFlag(FlagData: Integer): TMessageFlag;
begin
  case FlagData of
    MF_UNREAD:
      Exit(mfUnread);
    MF_OUTBOX:
      Exit(mfOutbox);
    MF_REPLIED:
      Exit(mfReplied);
    MF_IMPORTANT:
      Exit(mfImportant);
    MF_CHAT:
      Exit(mfChat);
    MF_FRIENDS:
      Exit(mfFriends);
    MF_SPAM:
      Exit(mfSpam);
    MF_DEL�T�D:
      Exit(mfDeleted);
    MF_FIXED:
      Exit(mfFixed);
    MF_MEDIA:
      Exit(mfMedia);
    MF_UNKNOWN_1:
      Exit(mfUNKNOWN_1);
    MF_UNKNOWN_2:
      Exit(mfUNKNOWN_2);
    MF_UNKNOWN_3:
      Exit(mfUNKNOWN_3);
    MF_UNREAD_MULTICHAT:
      Exit(mfUnreadMultichat);
    MF_UNKNOWN_4:
      Exit(mfUNKNOWN_4);
    MF_UNKNOWN_5:
      Exit(mfUNKNOWN_5);
    MF_HIDDEN:
      Exit(mfHidden);
    MF_DELETE_FOR_ALL:
      Exit(mfDeleteForAll);
    MF_NOT_DELIVERED:
      Exit(mfNotDelivered);
    MF_UNKNOWN_6:
      Exit(mfUNKNOWN_6);
    MF_UNKNOWN_7:
      Exit(mfUNKNOWN_7);
    MF_UNKNOWN_8:
      Exit(mfUNKNOWN_8);
    MF_UNKNOWN_9:
      Exit(mfUNKNOWN_9);
  else
    Exit(mfChat);
  end;
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
      Exit('���������� �������� ������');
    citPic:
      Exit('��������� ������� ������');
    citNewAdmin:
      Exit('�������� ����� �������������');
    citFixMessage:
      Exit('���������� ���������');
    citJoin:
      Exit('������������ ������������� � ������');
    citLeave:
      Exit('������������ ������� ������');
    citKick:
      Exit('������������ ��������� �� ������');
    citUnadmin:
      Exit('� ������������ ����� ����� ��������������');
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
    Result := Result + Self.ToString;
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
  begin
    Self[i] := Source[i];
  end;
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

{ TMessageFlagHelper }

function TMessageFlagHelper.ToString: string;
begin
  case Self of
    mfUnread:
      Result := 'Unread';
    mfOutbox:
      Result := 'Outbox';
    mfReplied:
      Result := 'Replied';
    mfImportant:
      Result := 'Important';
    mfChat:
      Result := 'Chat';
    mfFriends:
      Result := 'Friends';
    mfSpam:
      Result := 'Spam';
    mfDeleted:
      Result := 'Deleted';
    mfFixed:
      Result := 'Fixed';
    mfMedia:
      Result := 'Media';
    mfUNKNOWN_1:
      Result := 'Unknown_1';
    mfUNKNOWN_2:
      Result := 'Unknown_2';
    mfUNKNOWN_3:
      Result := 'Unknown_3';
    mfUnreadMultichat:
      Result := 'UnreadMultichat';
    mfUNKNOWN_4:
      Result := 'Unknown_4';
    mfUNKNOWN_5:
      Result := 'Unknown_5';
    mfHidden:
      Result := 'Hidden';
    mfDeleteForAll:
      Result := 'DeleteForAll';
    mfNotDelivered:
      Result := 'NotDelivered';
    mfUNKNOWN_6:
      Result := 'Unknown_6';
    mfUNKNOWN_7:
      Result := 'Unknown_7';
    mfUNKNOWN_8:
      Result := 'Unknown_8';
    mfUNKNOWN_9:
      Result := 'Unknown_9';
  else
    Result := '';
  end;
end;

{ TAudioGenreHelper }

function TAudioGenreHelper.ToConst: Integer;
begin
  try
    Result := VkAudioGenres[Self];
  except
    Result := AG_OTHER;
  end;
end;

function TAudioGenreHelper.ToString: string;
begin
  try
    Result := VkAudioGenresStr[Self];
  except
    Result := 'Other';
  end;
end;

{ AudioGenre }

class function AudioGenre.Create(Data: Integer): TAudioGenre;
var
  i: Integer;
begin
  Result := agOther;
  for i := Ord(agRock) to Ord(agOther) do
    if VkAudioGenres[TAudioGenre(i)] = Data then
      Exit(TAudioGenre(i));
end;

{ TGroupJoinTypeHelper }

function TGroupJoinTypeHelper.ToString: string;
begin
 {
  join � ������������ ������� � ������ ��� ����������� (���������� �� ��������� ��������).
  unsure � ��� �����������: ������������ ������ ������� ���������, �����.
  accepted � ������������ ������ ����������� � ������ ��� �� �����������.
  approved � ������ �� ���������� � ������/����������� ���� �������� ������������� ����������.
  request � ������������ ����� ������ �� ���������� � ����������.
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

class function GroupJoinType.Create(Value: string): TGroupJoinType;
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
  case Self of
    brOther:
      Result := '������';
    brSpam:
      Result := '����';
    btInsultingParticipants:
      Result := '����������� ����������';
    btObsceneExpressions:
      Result := '���';
    btOffTopic:
      Result := '��������� �� �� ����';
  else
    Result := '�� ��������';
  end;
end;

{ TVkGroupLevelHelper }

function TVkGroupLevelHelper.ToString: string;
begin
  case Self of
    glNone:
      Result := '��������';
    glModer:
      Result := '���������';
    glEditor:
      Result := '��������';
    glAdmin:
      Result := '�������������';
  end;
end;

{ TVkItemTypeHelper }

function TVkItemTypeHelper.ToConst: string;
begin
  case Self of
    itPost:
      Result := 'post';
    itComment:
      Result := 'comment';
    itPhoto:
      Result := 'photo';
    itAudio:
      Result := 'audio';
    itVideo:
      Result := 'video';
    itNote:
      Result := 'note';
    itMarket:
      Result := 'market';
    itPhotoComment:
      Result := 'photo_comment';
    itVideoComment:
      Result := 'video_comment';
    itTopicComment:
      Result := 'topic_comment';
    itMarketComment:
      Result := 'market_comment';
    itSitepage:
      Result := 'sitepage';
  end;
end;

end.

