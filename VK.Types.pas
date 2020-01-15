unit VK.Types;

interface

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
  DELЕTЕD = 128;
  FIXED = 256;
  MEDIA = 512;
  HIDDEN = 65536;
  DELETE_FOR_ALL = 131072;
  NOT_DELIVERED = 262144;

  //Group Dialog Flags
  GR_IMPORTANT = 1;
  GR_UNANSWERED = 2;

type
  TOnLogin = procedure(Sender: TObject) of object;

  TOnAuth = procedure(Sender: TObject; var Token: string; var TokenExpiry: Int64; var
    ChangePasswordHash: string) of object;

  TOnConfirm = procedure(Sender: TObject; Ans: string; var Accept: Boolean) of object;

  TOnCaptcha = procedure(Sender: TObject; const CaptchaURL: string; var Answer: string) of object;

  TOnLog = procedure(Sender: TObject; const Value: string) of object;

  TOnVKError = procedure(Sender: TObject; Code: Integer; Text: string) of object;

  TFields = array of string;

  TParam = array of string;

  TParams = array of TParam;

  TPremission = string;

  TPermissions = class(TList<TPremission>)
    function ToString: string; override;
    procedure Assign(Source: TStrings);
  end;

  TMessageFlag = (mfUnread, mfOutbox, mfReplied, mfImportant, mfChat, mfFriends, mfSpam, mfDeleted,
    mfFixed, mfMedia, mfHidden, mfDeleteForAll, mfNotDelivered);

  TMessageFlags = set of TMessageFlag;

  TDialogFlag = (dfImportant, dfUnanswered);

  TDialogFlags = set of TDialogFlag;

  //$type_id (integer) — идентификатор типа изменения в чате
  {
  1 — Изменилось название беседы
  2 — Сменилась обложка беседы
  3 — Назначен новый администратор
  4 — Закреплено сообщение
  5 — Пользователь присоединился к беседе
  6 — Пользователь покинул беседу
  7 — Пользователя исключили из беседы
  8 — С пользователя сняты права администратора
  }
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
    peer_id: integer; // идентификатор назначения. Для пользователя: id пользователя. Для групповой беседы: 2000000000 + id беседы. Для сообщества: -id сообщества либо id сообщества + 1000000000 (для version = 0).
    timestamp: integer; // время отправки сообщения в Unixtime;
    text: string; // текст сообщения;
   //[$attachments] (array) — вложения (если mode = 2);
   //[$random_id] (integer) — random_id, если параметр был передан в messages.send. Может содержать 0, если значение не задано.
  end;

  TUserIds = array of Integer;

  TUserIdsHelper = record helper for TUserIds
    function ToString: string; overload; inline;
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
    FIXED, DELЕTЕD, SPAM, FRIENDS, CHAT, IMPORTANT, REPLIED, OUTBOX, UNREAD);
  VkDialogFlags: array[0..1] of Integer = (GR_UNANSWERED, GR_IMPORTANT);
  VkUserActive: array[Boolean] of string = ('Бездействие', 'Покинул сайт');
  VkPlatforms: array[TVkPlatform] of string = ('Unknown', 'Mobile', 'iPhone', 'iPad', 'Android',
    'Windows Phone', 'Windows', 'Web');

function FieldsToString(Fields: TFields): string;

function VKErrorString(ErrorCode: Integer): string;

procedure AddParam(var Dest: TParams; Param: TParam);

implementation

procedure AddParam(var Dest: TParams; Param: TParam);
begin
  SetLength(Dest, Length(Dest) + 1);
  Dest[High(Dest)] := Param;
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
      ErrStr := 'Произошла неизвестная ошибка. Попробуйте повторить запрос позже.';
    2:
      ErrStr :=
        'Приложение выключено. Необходимо включить приложение в настройках https://vk.com/editapp?id={Ваш API_ID} или использовать тестовый режим (test_mode=1)';
    3:
      ErrStr :=
        'Передан неизвестный метод. Проверьте, правильно ли указано название вызываемого метода: https://vk.com/dev/methods.';
    4:
      ErrStr := 'Неверная подпись.';
    5:
      ErrStr := 'Авторизация пользователя не удалась. Убедитесь, что Вы используете верную схему авторизации.';
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
      ErrStr := 'Произошла внутренняя ошибка сервера. Попробуйте повторить запрос позже.';
    11:
      ErrStr :=
        'В тестовом режиме приложение должно быть выключено или пользователь должен быть залогинен. Выключите приложение в настройках https://vk.com/editapp?id={Ваш API_ID}';
    14:
      ErrStr := 'Требуется ввод кода с картинки (Captcha).';
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
      ErrStr := 'Страница удалена или заблокирована. Страница пользователя была удалена или заблокирована';
    20:
      ErrStr :=
        'Данное действие запрещено для не Standalone приложений. Если ошибка возникает несмотря на то, что Ваше приложение имеет тип Standalone, убедитесь, что при авторизации Вы используете redirect_uri=https://oauth.vk.com/blank.html.';
    21:
      ErrStr := 'Данное действие разрешено только для Standalone и Open API приложений.';
    23:
      ErrStr :=
        'Метод был выключен. Все актуальные методы ВК API, которые доступны в настоящий момент, перечислены здесь: https://vk.com/dev/methods.';
    24:
      ErrStr := 'Требуется подтверждение со стороны пользователя.';
    27:
      ErrStr := 'Ключ доступа сообщества недействителен.';
    28:
      ErrStr := 'Ключ доступа приложения недействителен.';
    29:
      ErrStr :=
        'Достигнут количественный лимит на вызов метода Подробнее об ограничениях на количество вызовов см. на странице https://vk.com/dev/data_limits';
    30:
      ErrStr :=
        'Профиль является приватным Информация, запрашиваемая о профиле, недоступна с используемым ключом доступа';
    33:
      ErrStr := 'Not implemented yet';
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
      ErrStr := 'Пользователь не установил приложение в левое меню';
    150:
      ErrStr := 'Неверный timestamp. Получить актуальное значение Вы можете методом utils.getServerTime.';
    200:
      ErrStr :=
        'Доступ к альбому запрещён. Убедитесь, что Вы используете верные идентификаторы (для пользователей owner_idположительный, для сообществ — отрицательный), и доступ к запрашиваемому контенту для текущего пользователя есть в полной версии сайта.';
    201:
      ErrStr :=
        'Доступ к аудио запрещён. Убедитесь, что Вы используете верные идентификаторы (для пользователей owner_idположительный, для сообществ — отрицательный), и доступ к запрашиваемому контенту для текущего пользователя есть в полной версии сайта.';
    203:
      ErrStr :=
        'Доступ к группе запрещён. Убедитесь, что текущий пользователь является участником или руководителем сообщества (для закрытых и частных групп и встреч).';
    300:
      ErrStr :=
        'Альбом переполнен. Перед продолжением работы нужно удалить лишние объекты из альбома или использовать другой альбом.';
    500:
      ErrStr :=
        'Действие запрещено. Вы должны включить переводы голосов в настройках приложения. Проверьте настройки приложения: https://vk.com/editapp?id={Ваш API_ID}&section=payments';
    600:
      ErrStr := 'Нет прав на выполнение данных операций с рекламным кабинетом.';
    603:
      ErrStr := 'Произошла ошибка при работе с рекламным кабинетом.';
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
    ErrStr := 'Неизвестная ошибка';
  end;

  Result := ErrStr;
end;

{ TPermissions }

procedure TPermissions.Assign(Source: TStrings);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
    Add(Source[i]);
end;

function TPermissions.ToString: string;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Result + Items[i];
    if i < (Count - 1) then
      Result := Result + ',';
  end;
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
    DELЕTЕD:
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
    case Flag of
      mfUnread:
        Result := Result + 'Unread ';
      mfOutbox:
        Result := Result + 'Outbox ';
      mfReplied:
        Result := Result + 'Replied ';
      mfImportant:
        Result := Result + 'Important ';
      mfChat:
        Result := Result + 'Chat ';
      mfFriends:
        Result := Result + 'Friends ';
      mfSpam:
        Result := Result + 'Spam ';
      mfDeleted:
        Result := Result + 'Deleted ';
      mfFixed:
        Result := Result + 'Fixed ';
      mfMedia:
        Result := Result + 'Media ';
      mfHidden:
        Result := Result + 'Hidden ';
      mfDeleteForAll:
        Result := Result + 'DeleteForAll ';
      mfNotDelivered:
        Result := Result + 'NotDelivered ';
    end;
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

{ TUserIdsHelper }

function TUserIdsHelper.ToString: string;
var
  i: Integer;
begin
  for i := Low(Self) to High(Self) do
    Result := Result + Self.ToString + ' ';
end;

end.

