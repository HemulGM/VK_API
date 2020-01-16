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
  UNREAD = 1;
  OUTBOX = 2;
  REPLIED = 4;
  IMPORTANT = 8;
  CHAT = 16;
  FRIENDS = 32;
  SPAM = 64;
  DEL�T�D = 128;
  FIXED = 256;
  MEDIA = 512;
  HIDDEN = 65536;
  DELETE_FOR_ALL = 131072;
  NOT_DELIVERED = 262144;

  //Group Dialog Flags
  GR_IMPORTANT = 1;
  GR_UNANSWERED = 2;

type
  {$IFDEF OLD_ARRAYS}
  TArrayOfString = array of string;
  {$ELSE}
  TArrayOfString = TArray<string>;
  {$ENDIF}


  TArrayOfStringHelper = record helper for TArrayOfString
    function ToString: string; overload; inline;
    function CreateAttachmentsFromString(Attachments: string): TArrayOfString;
    procedure Assign(Source: TStrings); overload;
  end;

  {$IFDEF OLD_ARRAYS}
  TArrayOfInteger = array of Integer;
  {$ELSE}
  TArrayOfInteger = TArray<Integer>;
  {$ENDIF}


  TArrayOfIntegerHelper = record helper for TArrayOfInteger
    function ToString: string; overload; inline;
  end;

  TFields = TArrayOfString;

  TParam = TArrayOfString;

  {$IFDEF OLD_ARRAYS}
  TParams = array of TParam;
  {$ELSE}
  TParams = TArray<TParam>;
  {$ENDIF}


  TParamsHelper = record helper for TParams
    function Add(Param: TParam): Integer; overload; inline;
    function Add(Key, Value: string): Integer; overload; inline;
  end;

  TPremission = string;

  {$IFDEF OLD_ARRAYS}
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

  TMessageFlag = (mfUnread, mfOutbox, mfReplied, mfImportant, mfChat, mfFriends, mfSpam, mfDeleted,
    mfFixed, mfMedia, mfHidden, mfDeleteForAll, mfNotDelivered);

  TMessageFlagHelper = record helper for TMessageFlag
    function ToString: string; inline;
  end;

  TMessageFlags = set of TMessageFlag;

  TDialogFlag = (dfImportant, dfUnanswered);

  TDialogFlags = set of TDialogFlag;

  //������������� ���� ��������� � ����
  TChatChangeInfoType = (citNone, citName, citPic, citNewAdmin, citFixMessage, citJoin, citLeave, citKick, citUnadmin);

  TChatChangeInfoTypeHelper = record helper for TChatChangeInfoType
    function ToString: string; overload; inline;
  end;

  TVkPlatform = (pfUnknown, pfMobile, pfIPhone, pfIPad, pfAndroid, pfWindowsPhone, pfWindows, pfWeb);

  TFlagsChangeType = (fcFlagsReplace, fcFlagsSet, fcFlagsReset);

  TMessageChangeTypeHelper = record helper for TFlagsChangeType
    function ToString: string; overload; inline;
  end;

  TMessageFlagsHelper = record helper for TMessageFlags
    function ToString: string; overload; inline;
  end;

  TDialogFlagsHelper = record helper for TDialogFlags
    function ToString: string; overload; inline;
  end;

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
    Value: string;
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

  MessageFlags = class
    class function FlagDataToFlag(FlagData: Integer): TMessageFlag;
    class function Create(Data: Integer): TMessageFlags;
    class function ToString(Flags: TMessageFlags): string; overload;
  end;

  DialogFlags = class
    class function FlagDataToFlag(FlagData: Integer): TDialogFlag;
    class function Create(Data: Integer): TDialogFlags;
    class function ToString(Flags: TDialogFlags): string; overload;
  end;

var
  VkMessageFlags: array[0..12] of Integer = (NOT_DELIVERED, DELETE_FOR_ALL, HIDDEN, MEDIA,
    FIXED, DEL�T�D, SPAM, FRIENDS, CHAT, IMPORTANT, REPLIED, OUTBOX, UNREAD);
  VkDialogFlags: array[0..1] of Integer = (GR_UNANSWERED, GR_IMPORTANT);
  VkUserActive: array[Boolean] of string = ('�����������', '������� ����');
  VkPlatforms: array[TVkPlatform] of string = ('Unknown', 'Mobile', 'iPhone', 'iPad', 'Android',
    'Windows Phone', 'Windows', 'Web');

function FieldsToString(Fields: TFields): string;

function VKErrorString(ErrorCode: Integer): string;

function AddParam(var Dest: TParams; Param: TParam): Integer;

function CreateAttachment(&Type: string; Id: string; AccessKey: string = ''): string;

implementation

uses
  System.SysUtils;

function CreateAttachment(&Type: string; Id: string; AccessKey: string): string;
begin
  Result := &Type + '_' + Id;
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
begin
  case FlagData of
    UNREAD:
      Exit(mfUnread);
    OUTBOX:
      Exit(mfOutbox);
    REPLIED:
      Exit(mfReplied);
    IMPORTANT:
      Exit(mfImportant);
    CHAT:
      Exit(mfChat);
    FRIENDS:
      Exit(mfFriends);
    SPAM:
      Exit(mfSpam);
    DEL�T�D:
      Exit(mfDeleted);
    FIXED:
      Exit(mfFixed);
    MEDIA:
      Exit(mfMedia);
    HIDDEN:
      Exit(mfHidden);
    DELETE_FOR_ALL:
      Exit(mfDeleteForAll);
    NOT_DELIVERED:
      Exit(mfNotDelivered);
  else
    Exit(mfChat);
  end;
end;

class function MessageFlags.ToString(Flags: TMessageFlags): string;
var
  Flag: TMessageFlag;
begin
  for Flag in Flags do
    Result := Result + Flag.ToString;
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
  for i := Low(VkDialogFlags) to High(VkDialogFlags) do
  begin
    if (Data - VkDialogFlags[i]) >= 0 then
    begin
      Include(Result, FlagDataToFlag(VkDialogFlags[i]));
      Data := Data - VkDialogFlags[i];
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

function TArrayOfStringHelper.CreateAttachmentsFromString(Attachments: string): TArrayOfString;
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.Delimiter := ',';
  List.DelimitedText := Attachments;
  Result.Assign(List);
  List.Free;
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

{$IFDEF OLD_ARRAYS}

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

{ TMessageFlagHelper }

function TMessageFlagHelper.ToString: string;
begin
  case Self of
    mfUnread:
      Result := 'Unread ';
    mfOutbox:
      Result := 'Outbox ';
    mfReplied:
      Result := 'Replied ';
    mfImportant:
      Result := 'Important ';
    mfChat:
      Result := 'Chat ';
    mfFriends:
      Result := 'Friends ';
    mfSpam:
      Result := 'Spam ';
    mfDeleted:
      Result := 'Deleted ';
    mfFixed:
      Result := 'Fixed ';
    mfMedia:
      Result := 'Media ';
    mfHidden:
      Result := 'Hidden ';
    mfDeleteForAll:
      Result := 'DeleteForAll ';
    mfNotDelivered:
      Result := 'NotDelivered ';
  else
    Result := '';
  end;
end;

end.

