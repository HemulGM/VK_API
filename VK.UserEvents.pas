unit VK.UserEvents;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.JSON, VK.Types, System.Generics.Collections, VK.LongPollServer, VK.API;

type
  TVkMessageInfo = class
  private
    FTitle: string;
    FFrom: string;
    FMentions: TArray<TVkPeerId>;
    FSource_act: string;
    FSource_mid: string;
    FSource_chat_local_id: string;
    FSource_message: string;
    FPayload: string;
    FPinned_at: string;
  public
    property Title: string read FTitle write FTitle;
    property From: string read FFrom write FFrom;
    property Mentions: TArray<TVkPeerId> read FMentions write FMentions;
    // chat_pin_message, chat_create, chat_title_update, chat_photo_update, chat_invite_user, chat_kick_user
    property SourceAct: string read FSource_act write FSource_act;
    property SourceMid: string read FSource_mid write FSource_mid;
    property SourceChatLocalId: string read FSource_chat_local_id write FSource_chat_local_id;
    property SourceMessage: string read FSource_message write FSource_message;
    property Payload: string read FPayload write FPayload;
    property PinnedAt: string read FPinned_at write FPinned_at;
    //property MarkedUsers: TArray<integer> read FMarked_users write FMarked_users;
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
    FGeo: string;
    FGeo_provider: string;
    FAttach1_product_id: string;
    FAttach2_product_id: string;
    FAttach3_product_id: string;
    FAttach6_product_id: string;
    FAttach7_product_id: string;
    FAttach4_product_id: string;
    FAttach5_product_id: string;
    FAttach8_product_id: string;
    FAttach9_product_id: string;
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
    property Geo: string read FGeo write FGeo;
    property GeoProvider: string read FGeo_provider write FGeo_provider;
    property Attach1ProductId: string read FAttach1_product_id write FAttach1_product_id;
    property Attach2ProductId: string read FAttach2_product_id write FAttach2_product_id;
    property Attach3ProductId: string read FAttach3_product_id write FAttach3_product_id;
    property Attach4ProductId: string read FAttach4_product_id write FAttach4_product_id;
    property Attach5ProductId: string read FAttach5_product_id write FAttach5_product_id;
    property Attach6ProductId: string read FAttach6_product_id write FAttach6_product_id;
    property Attach7ProductId: string read FAttach7_product_id write FAttach7_product_id;
    property Attach8ProductId: string read FAttach8_product_id write FAttach8_product_id;
    property Attach9ProductId: string read FAttach9_product_id write FAttach9_product_id;
    property Count: Integer read GetCount;
    property Attachments[Index: Integer]: TAttachInfoType read GetAttachments;
    function ToArray: TArray<TAttachInfoType>;
    function ToArrayOfString: TArrayOfString;
    class function FromJsonString(AJsonString: string): TVkMessageAttachmentInfo;
  end;

  /// <summary>
  /// Структура события входящего сообщения
  /// </summary>
  TMessageData = record
    MessageId: Int64;
    Flags: TVkMessageFlags;
    PeerId: TVkPeerId;
    TimeStamp: TDateTime;
    Text: string;
    Info: TVkMessageInfo;
    RandomId: Integer;
    MinorId: Int64;
    Attachments: TVkMessageAttachmentInfo;
  end;

  TMessageChangeData = record
    MessageId: Int64;
    Flags: TVkMessageFlags;
    PeerId: TVkPeerId;
    ChangeType: TVkFlagsChangeType;
  end;

  TDialogChangeData = record
    PeerId: TVkPeerId;
    Flags: TVkDialogFlags;
    ChangeType: TVkFlagsChangeType;
  end;

  TEventExtraFields = record
    PeerId: TVkPeerId; // идентификатор назначения. Для пользователя: id пользователя. Для групповой беседы: 2000000000 + id беседы. Для сообщества: -id сообщества либо id сообщества + 1000000000 (для version = 0).
    TimeStamp: Int64; // время отправки сообщения в Unixtime;
    Text: string; // текст сообщения;
    Info: TVkMessageInfo;
    Attachments: TVkMessageAttachmentInfo;
    RandomId: Integer;
  end;

  TChatTypingData = record
    UserIds: TVkPeerIds;
    PeerId: TVkPeerId;
    TotalCount: Integer;
    TimeStamp: TDateTime;
  end;

  TChatRecordingData = record
    UserIds: TVkPeerIds;
    PeerId: TVkPeerId;
    TotalCount: Integer;
    TimeStamp: TDateTime;
  end;

  TOnNewMessage = procedure(Sender: TObject; MessageData: TMessageData) of object;

  TOnEditMessage = procedure(Sender: TObject; MessageData: TMessageData) of object;

  TOnChangeMessageFlags = procedure(Sender: TObject; MessageChangeData: TMessageChangeData) of object;

  TOnChangeDialogFlags = procedure(Sender: TObject; DialogChangeData: TDialogChangeData) of object;

  TOnUserOnline = procedure(Sender: TObject; UserId: TVkPeerId; VkPlatform: TVkPlatform; TimeStamp: TDateTime) of object;

  TOnUserOffline = procedure(Sender: TObject; UserId: TVkPeerId; InactiveUser: Boolean; TimeStamp: TDateTime) of object;

  TOnReadMessages = procedure(Sender: TObject; Incoming: Boolean; PeerId: TVkPeerId; LocalId: Int64) of object;

  TOnRecoverOrDeleteMessages = procedure(Sender: TObject; PeerId: TVkPeerId; LocalId: Int64) of object;

  TOnChatChanged = procedure(Sender: TObject; const ChatId: Int64; IsSelf: Boolean) of object;

  TOnChatChangeInfo = procedure(Sender: TObject; const PeerId: TVkPeerId; TypeId: TVkChatChangeInfoType; Info: Integer) of object;

  TOnUserTyping = procedure(Sender: TObject; UserId: TVkPeerId; ChatId: Int64) of object;

  TOnUserCall = procedure(Sender: TObject; UserId: TVkPeerId; CallId: Int64) of object;

  TOnCountChange = procedure(Sender: TObject; Count: Integer) of object;

  TOnNotifyChange = procedure(Sender: TObject; PeerId: TVkPeerId; Sound: Boolean; DisableUntil: Int64) of object;

  TOnUsersTyping = procedure(Sender: TObject; Data: TChatTypingData) of object;

  TOnUsersRecording = procedure(Sender: TObject; Data: TChatRecordingData) of object;

  TOnUnhandledEvents = procedure(Sender: TObject; const JSON: TJSONValue) of object;

  TCustomUserEvents = class(TComponent)
  private
    FLongPollServer: TVkLongPollServer;
    FOnUsersRecording: TOnUsersRecording;
    FOnDeleteMessages: TOnRecoverOrDeleteMessages;
    FOnUserOnline: TOnUserOnline;
    FOnReadMessages: TOnReadMessages;
    FOnChangeDialogFlags: TOnChangeDialogFlags;
    FOnEditMessage: TOnEditMessage;
    FOnUsersTyping: TOnUsersTyping;
    FOnChatChanged: TOnChatChanged;
    FOnUserOffline: TOnUserOffline;
    FOnRecoverMessages: TOnRecoverOrDeleteMessages;
    FOnUserTyping: TOnUserTyping;
    FOnChatChangeInfo: TOnChatChangeInfo;
    FOnNewMessage: TOnNewMessage;
    FOnChangeMessageFlags: TOnChangeMessageFlags;
    FVK: TCustomVK;
    FOnUserCall: TOnUserCall;
    FOnCountChange: TOnCountChange;
    FOnNotifyChange: TOnNotifyChange;
    FOnUnhandledEvents: TOnUnhandledEvents;
    FVersion: string;
    FLogging: Boolean;
    function GetIsWork: Boolean;
    procedure DoChangeDialogFlags(const PeerId: TVkPeerId; ChangeType: TVkFlagsChangeType; FlagsMasksData: Integer);
    procedure DoChangeMessageFlags(const MessageId: Integer; ChangeType: TVkFlagsChangeType; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoChatChanged(const ChatId: TVkPeerId; IsSelf: Boolean);
    procedure DoChatChangeInfo(const PeerId: TVkPeerId; TypeId, Info: Integer);
    procedure DoCountChange(const Count: Integer);
    procedure DoDeleteMessages(const PeerId, LocalId: TVkPeerId);
    procedure DoEditMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoEvent(Sender: TObject; Update: TJSONValue);
    procedure DoNewMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoNotifyChange(const PeerId: TVkPeerId; Sound, DisabledUntil: Integer);
    procedure DoReadMessages(const Incoming: Boolean; PeerId, LocalId: TVkPeerId);
    procedure DoRecoverMessages(const PeerId, LocalId: TVkPeerId);
    procedure DoUnhandledEvents(const JSON: TJSONValue);
    procedure DoUserCall(const UserId, CallId: TVkPeerId);
    procedure DoUsersRecording(const UserId: TVkPeerIds; PeerId: TVkPeerId; TotalCount, Ts: Integer);
    procedure DoUserStateChange(IsOnline: Boolean; UserId: TVkPeerId; Extra, TimeStamp: Integer);
    procedure DoUsersTyping(const UserId: TVkPeerIds; PeerId: TVkPeerId; TotalCount, Ts: Integer);
    procedure DoUserTyping(const UserId, ChatId: TVkPeerId);
    procedure FOnError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
    procedure SetLogging(const Value: Boolean);
    procedure SetVersion(const Value: string);
    procedure SetVK(const Value: TCustomVK);
    function GetAsync: Boolean;
    procedure SetAsync(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Start: Boolean;
    procedure Stop;
    property IsWork: Boolean read GetIsWork;
    //
    property Async: Boolean read GetAsync write SetAsync;
    property Logging: Boolean read FLogging write SetLogging;
    property Version: string read FVersion write SetVersion;
    property VK: TCustomVK read FVK write SetVK;
    //
    property OnChangeDialogFlags: TOnChangeDialogFlags read FOnChangeDialogFlags write FOnChangeDialogFlags;
    property OnChangeMessageFlags: TOnChangeMessageFlags read FOnChangeMessageFlags write FOnChangeMessageFlags;
    property OnChatChanged: TOnChatChanged read FOnChatChanged write FOnChatChanged;
    property OnChatChangeInfo: TOnChatChangeInfo read FOnChatChangeInfo write FOnChatChangeInfo;
    property OnCountChange: TOnCountChange read FOnCountChange write FOnCountChange;
    property OnDeleteMessages: TOnRecoverOrDeleteMessages read FOnDeleteMessages write FOnDeleteMessages;
    property OnEditMessage: TOnEditMessage read FOnEditMessage write FOnEditMessage;
    property OnNewMessage: TOnNewMessage read FOnNewMessage write FOnNewMessage;
    property OnNotifyChange: TOnNotifyChange read FOnNotifyChange write FOnNotifyChange;
    property OnReadMessages: TOnReadMessages read FOnReadMessages write FOnReadMessages;
    property OnRecoverMessages: TOnRecoverOrDeleteMessages read FOnRecoverMessages write FOnRecoverMessages;
    property OnUnhandledEvents: TOnUnhandledEvents read FOnUnhandledEvents write FOnUnhandledEvents;
    property OnUserCall: TOnUserCall read FOnUserCall write FOnUserCall;
    property OnUserOffline: TOnUserOffline read FOnUserOffline write FOnUserOffline;
    property OnUserOnline: TOnUserOnline read FOnUserOnline write FOnUserOnline;
    property OnUsersRecording: TOnUsersRecording read FOnUsersRecording write FOnUsersRecording;
    property OnUsersTyping: TOnUsersTyping read FOnUsersTyping write FOnUsersTyping;
    property OnUserTyping: TOnUserTyping read FOnUserTyping write FOnUserTyping;
  end;

implementation

uses
  REST.Json, System.DateUtils;

{ TUserEvents }

constructor TCustomUserEvents.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  if Assigned(AOwner) and (csDesigning in ComponentState) then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
      if AOwner.Components[i] is TCustomVK then
      begin
        FVK := AOwner.Components[i] as TCustomVK;
        Break;
      end;
  end;
  FVersion := VK_LP_VERSION;
  FLongPollServer := TVkLongPollServer.Create;
  FLongPollServer.DoSync := True;
  FLongPollServer.OnUpdate := FOnLongPollUpdate;
  FLongPollServer.OnError := FOnError;
end;

destructor TCustomUserEvents.Destroy;
begin
  FLongPollServer.Free;
  inherited;
end;

procedure TCustomUserEvents.DoEvent(Sender: TObject; Update: TJSONValue);
var
  EventType, A1, A2, A3: Integer;
  i: Integer;
  ExtraFields: TEventExtraFields;
  UserIds: TVkPeerIds;
  Arr: TJSONArray;

  procedure DoRaiseProcessing;
  begin
    raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
  end;

begin
  try
    EventType := TJSONArray(Update).Items[0].GetValue<Integer>;
    A1 := 0;
    A2 := 0;
    A3 := 0;
  except
    Exit;
    //raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
  end;
  ExtraFields.Info := nil;
  ExtraFields.Attachments := nil;
  ExtraFields.PeerId := 0;
  ExtraFields.TimeStamp := 0;
  case EventType of
    1..4: //Изменение флагов сообщений и новое сообщение
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          ExtraFields.PeerId := NormalizePeerId(TJSONArray(Update).Items[3].GetValue<TVkPeerId>);
          if TJSONArray(Update).Count > 4 then
          begin
            ExtraFields.TimeStamp := TJSONArray(Update).Items[4].GetValue<Integer>;
            if TJSONArray(Update).Count > 5 then
            try
              ExtraFields.Text := TJSONArray(Update).Items[5].GetValue<string>;
            except
            end;
            if TJSONArray(Update).Count > 6 then
            try
              ExtraFields.Info := TVkMessageInfo.FromJsonString(TJSONArray(Update).Items[6].GetValue<TJSONValue>.ToJSON);
            except
            end;
            if TJSONArray(Update).Count > 7 then
            try
              ExtraFields.Attachments := TVkMessageAttachmentInfo.FromJsonString(TJSONArray(Update).Items[7].GetValue<TJSONValue>.ToJSON);
            except
            end;
            if TJSONArray(Update).Count > 8 then
            try
              ExtraFields.RandomId := TJSONArray(Update).Items[8].GetValue<Integer>;
            except
            end;
          end;
        except
          DoRaiseProcessing;
        end;
        case EventType of
          1: //1	$message_id (integer) $flags (integer) extra_fields*	Замена флагов сообщения (FLAGS:=$flags).
            DoChangeMessageFlags(A1, TVkFlagsChangeType.Replace, A2, ExtraFields);
          2: //2	$message_id (integer) $mask (integer) extra_fields*	Установка флагов сообщения (FLAGS|=$mask).
            DoChangeMessageFlags(A1, TVkFlagsChangeType.&Set, A2, ExtraFields);
          3: //3	$message_id (integer) $mask (integer) extra_fields*	Сброс флагов сообщения (FLAGS&=~$mask).
            DoChangeMessageFlags(A1, TVkFlagsChangeType.Reset, A2, ExtraFields);
          4: //4	$message_id (integer) $flags (integer) extra_fields*	Добавление нового сообщения.
            DoNewMessage(A1, A2, ExtraFields);
        end;
      end;
    5: //Редактирование сообщения
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          ExtraFields.PeerId := NormalizePeerId(TJSONArray(Update).Items[3].GetValue<TVkPeerId>);

          if TJSONArray(Update).Count > 4 then
          begin
            ExtraFields.TimeStamp := TJSONArray(Update).Items[4].GetValue<Integer>;
            if TJSONArray(Update).Count > 5 then
            try
              ExtraFields.Text := TJSONArray(Update).Items[5].GetValue<string>;
            except
            end;
            if TJSONArray(Update).Count > 6 then
            try
              ExtraFields.Info := TVkMessageInfo.FromJsonString(TJSONArray(Update).Items[6].GetValue<TJSONValue>.ToJSON);
            except
            end;
          end;
        except
          DoRaiseProcessing;
        end;
        DoEditMessage(A1, A2, ExtraFields);
      end;
    6, 7: //Прочтение сообщений
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<TVkPeerId>);
          A2 := NormalizePeerId(TJSONArray(Update).Items[2].GetValue<TVkPeerId>);
        except
          DoRaiseProcessing;
        end;
        DoReadMessages(EventType = 6, A1, A2);
      end;
    8, 9: //Online/Offline пользователя
      begin
        try
          A1 := -TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<TVkPeerId>;
          A3 := TJSONArray(Update).Items[3].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        DoUserStateChange(EventType = 8, A1, A2, A3);
      end;
    10, 11, 12: //Изменение флагов диалога
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<TVkPeerId>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        case EventType of
          10: //$peer_id (integer) $mask (integer)	Сброс флагов диалога $peer_id. Соответствует операции (PEER_FLAGS &= ~$flags). Только для диалогов сообществ.
            DoChangeDialogFlags(A1, TVkFlagsChangeType.Reset, A2);
          11: //$peer_id (integer) $flags (integer)	Замена флагов диалога $peer_id. Соответствует операции (PEER_FLAGS:= $flags). Только для диалогов сообществ.
            DoChangeDialogFlags(A1, TVkFlagsChangeType.Replace, A2);
          12: //$peer_id (integer) $mask (integer)	Установка флагов диалога $peer_id. Соответствует операции (PEER_FLAGS|= $flags). Только для диалогов сообществ.
            DoChangeDialogFlags(A1, TVkFlagsChangeType.&Set, A2);
        end;
      end;
    13, 14: //Удаление/восставноление сообщений
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<TVkPeerId>);
          A2 := NormalizePeerId(TJSONArray(Update).Items[2].GetValue<TVkPeerId>);
        except
          DoRaiseProcessing;
        end;
        case EventType of
          13: //Удаление всех сообщений в диалоге $peer_id с идентификаторами вплоть до $local_id.
            DoDeleteMessages(A1, A2);
          14: //Восстановление недавно удаленных сообщений в диалоге $peer_id с идентификаторами вплоть до $local_id.
            DoRecoverMessages(A1, A2);
        end;
      end;
    51: //Один из параметров (состав, тема) беседы $chat_id были изменены. $self — 1 или 0 (вызваны ли изменения самим пользователем).
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<TVkPeerId>;
          if TJSONArray(Update).Count > 2 then
            A2 := TJSONArray(Update).Items[2].GetValue<Integer>
          else
            A2 := 0;
        except
          DoRaiseProcessing;
        end;
        DoChatChanged(A1, A2 = 1);
      end;
    52: //Изменение информации чата $peer_id с типом $type_id, $info — дополнительная информация об изменениях, зависит от типа события
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := NormalizePeerId(TJSONArray(Update).Items[2].GetValue<TVkPeerId>);
          A3 := TJSONArray(Update).Items[3].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        DoChatChangeInfo(A2, A1, A3);
      end;
    61, 62: //Пользователь набирает текст в диалоге/чате
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<TVkPeerId>;
          if EventType = 62 then
            A2 := TJSONArray(Update).Items[2].GetValue<TVkPeerId>
          else
            A2 := A1;
        except
          DoRaiseProcessing;
        end;
        DoUserTyping(A1, A2);
      end;
    63, 64: //Пользователи в беседе набирают текст или записывают аудио
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<TVkPeerId>);
          Arr := TJSONArray(TJSONArray(Update).Items[2]);
          SetLength(UserIds, Arr.Count);
          for i := 0 to Pred(Arr.Count) do
          begin
            UserIds[i] := Arr.Items[i].GetValue<TVkPeerId>;
          end;
          A2 := TJSONArray(Update).Items[3].GetValue<Integer>;
          A3 := TJSONArray(Update).Items[4].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        case EventType of
          63:
            DoUsersTyping(UserIds, A1, A2, A3);
          64:
            DoUsersRecording(UserIds, A1, A2, A3);
        end;
      end;
    70: //Пользователь $user_id совершил звонок с идентификатором $call_id.
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<TVkPeerId>;
          A2 := TJSONArray(Update).Items[2].GetValue<TVkPeerId>;
        except
          DoRaiseProcessing;
        end;
        DoUserCall(A1, A2);
      end;
    80: //Счетчик в левом меню стал равен $count.
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        DoCountChange(A1);
      end;
    114: //Изменились настройки оповещений. $peer_id — идентификатор чата/собеседника,
         //'$sound — 1/0, включены/выключены звуковые оповещения,
         //$disabled_until — выключение оповещений на необходимый срок (-1: навсегда, ''0
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<TVkPeerId>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          if TJSONArray(Update).Count > 3 then
            A3 := TJSONArray(Update).Items[3].GetValue<Integer>
          else
            A3 := 0;
        except
          DoRaiseProcessing;
        end;
        DoNotifyChange(A1, A2, A3);
      end;
  else
    DoUnhandledEvents(Update);
  end;
end;

procedure TCustomUserEvents.FOnError(Sender: TObject; E: Exception; Code: Integer; Text: string);
begin
  FVK.DoError(Sender, E, Code, Text);
end;

procedure TCustomUserEvents.FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
begin
  DoEvent(Sender, Update);
end;

function TCustomUserEvents.GetAsync: Boolean;
begin
  Result := not FLongPollServer.DoSync;
end;

function TCustomUserEvents.GetIsWork: Boolean;
begin
  Result := FLongPollServer.IsWork;
end;

procedure TCustomUserEvents.SetAsync(const Value: Boolean);
begin
  FLongPollServer.DoSync := not Value;
end;

procedure TCustomUserEvents.SetLogging(const Value: Boolean);
begin
  FLogging := Value;
  FLongPollServer.Logging := Value;
end;

procedure TCustomUserEvents.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

procedure TCustomUserEvents.SetVK(const Value: TCustomVK);
begin
  FVK := Value;
end;

function TCustomUserEvents.Start: Boolean;
begin
  if not Assigned(FVK) then
    raise Exception.Create('Для работы необходим VK контроллер (Свойство VK)');
  FLongPollServer.Handler := FVK.Handler;
  FLongPollServer.Method := 'messages.getLongPollServer';
  FLongPollServer.Params.Add(VK_LP_FIELD_VERSION, FVersion);
  FLongPollServer.OnError := FOnError;
  Result := FLongPollServer.Start;
  if Result then
    FVK.DoLog(Self, 'User LongPoll server started')
  else
    FVK.DoLog(Self, 'User LongPoll server not start');
end;

procedure TCustomUserEvents.Stop;
begin
  FLongPollServer.Stop;
end;

procedure TCustomUserEvents.DoChangeMessageFlags(const MessageId: Integer; ChangeType: TVkFlagsChangeType; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
var
  MessageChangeData: TMessageChangeData;
begin
  if Assigned(FOnChangeMessageFlags) then
  begin
    MessageChangeData.MessageId := MessageId;
    MessageChangeData.ChangeType := ChangeType;
    MessageChangeData.Flags := TVkMessageFlags.Create(FlagsMasksData);
    MessageChangeData.PeerId := ExtraFields.PeerId;
    FOnChangeMessageFlags(Self, MessageChangeData);
  end;
  if Assigned(ExtraFields.Attachments) then
    ExtraFields.Attachments.Free;
  if Assigned(ExtraFields.Info) then
    ExtraFields.Info.Free;
end;

procedure TCustomUserEvents.DoChatChanged(const ChatId: TVkPeerId; IsSelf: Boolean);
begin
  if Assigned(FOnChatChanged) then
    FOnChatChanged(Self, ChatId, IsSelf);
end;

procedure TCustomUserEvents.DoChatChangeInfo(const PeerId: TVkPeerId; TypeId, Info: Integer);
begin
  if Assigned(FOnChatChangeInfo) then
    FOnChatChangeInfo(Self, PeerId, TVkChatChangeInfoType(TypeId), Info);
end;

procedure TCustomUserEvents.DoNewMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
var
  MessageData: TMessageData;
begin
  if Assigned(FOnNewMessage) then
  begin
    MessageData.MessageId := MessageId;
    MessageData.Flags := TVkMessageFlags.Create(FlagsMasksData);
    MessageData.PeerId := ExtraFields.PeerId;
    MessageData.TimeStamp := UnixToDateTime(ExtraFields.TimeStamp, False);
    MessageData.RandomId := ExtraFields.RandomId;
    MessageData.Text := ExtraFields.Text;
    if Assigned(ExtraFields.Attachments) then
      MessageData.Attachments := ExtraFields.Attachments;
    if Assigned(ExtraFields.Info) then
      MessageData.Info := ExtraFields.Info;
    FOnNewMessage(Self, MessageData);
  end;
  if Assigned(ExtraFields.Attachments) then
    ExtraFields.Attachments.Free;
  if Assigned(ExtraFields.Info) then
    ExtraFields.Info.Free;
end;

procedure TCustomUserEvents.DoReadMessages(const Incoming: Boolean; PeerId, LocalId: TVkPeerId);
begin
  if Assigned(FOnReadMessages) then
    FOnReadMessages(Self, Incoming, PeerId, LocalId);
end;

procedure TCustomUserEvents.DoRecoverMessages(const PeerId, LocalId: TVkPeerId);
begin
  if Assigned(FOnRecoverMessages) then
    FOnRecoverMessages(Self, PeerId, LocalId);
end;

procedure TCustomUserEvents.DoDeleteMessages(const PeerId, LocalId: TVkPeerId);
begin
  if Assigned(FOnDeleteMessages) then
    FOnDeleteMessages(Self, PeerId, LocalId);
end;

procedure TCustomUserEvents.DoEditMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
var
  MessageData: TMessageData;
begin
  if Assigned(FOnEditMessage) then
  begin
    MessageData.MessageId := MessageId;
    MessageData.Flags := TVkMessageFlags.Create(FlagsMasksData);
    MessageData.PeerId := ExtraFields.PeerId;
    MessageData.TimeStamp := UnixToDateTime(ExtraFields.TimeStamp, False);
    MessageData.Text := ExtraFields.Text;
    FOnEditMessage(Self, MessageData);
  end;
  if Assigned(ExtraFields.Attachments) then
    ExtraFields.Attachments.Free;
  if Assigned(ExtraFields.Info) then
    ExtraFields.Info.Free;
end;

procedure TCustomUserEvents.DoUnhandledEvents(const JSON: TJSONValue);
begin
  try
    if Assigned(FOnUnhandledEvents) then
      FOnUnhandledEvents(Self, JSON);
  finally
    JSON.Free;
  end;
end;

procedure TCustomUserEvents.DoUserCall(const UserId, CallId: TVkPeerId);
begin
  if Assigned(FOnUserCall) then
    FOnUserCall(Self, UserId, CallId);
end;

procedure TCustomUserEvents.DoCountChange(const Count: Integer);
begin
  if Assigned(FOnCountChange) then
    FOnCountChange(Self, Count);
end;

procedure TCustomUserEvents.DoNotifyChange(const PeerId: TVkPeerId; Sound, DisabledUntil: Integer);
begin
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self, PeerId, Sound = 1, DisabledUntil);
end;

procedure TCustomUserEvents.DoUsersRecording(const UserId: TVkPeerIds; PeerId: TVkPeerId; TotalCount, Ts: Integer);
var
  Data: TChatRecordingData;
begin
  if Assigned(FOnUsersRecording) then
  begin
    Data.UserIds := UserId;
    Data.PeerId := PeerId;
    Data.TotalCount := TotalCount;
    Data.TimeStamp := UnixToDateTime(Ts, False);
    FOnUsersRecording(Self, Data);
  end;
end;

procedure TCustomUserEvents.DoUserStateChange(IsOnline: Boolean; UserId: TVkPeerId; Extra, TimeStamp: Integer);
var
  VkPlatform: TVkPlatform;
  InactiveUser: Boolean;
  Dt: TDateTime;
begin
  Dt := UnixToDateTime(TimeStamp, False);
  if IsOnline then
  begin
    if Assigned(FOnUserOnline) then
    begin
      if Extra <> 0 then
        VkPlatform := TVkPlatform(Extra and 255)
      else
        VkPlatform := TVkPlatform.Unknown;
      FOnUserOnline(Self, UserId, VkPlatform, Dt);
    end;
  end
  else
  begin
    if Assigned(FOnUserOffline) then
    begin
      InactiveUser := Extra = 0;
      FOnUserOffline(Self, UserId, InactiveUser, Dt);
    end;
  end;
end;

procedure TCustomUserEvents.DoUsersTyping(const UserId: TVkPeerIds; PeerId: TVkPeerId; TotalCount, Ts: Integer);
var
  Data: TChatTypingData;
begin
  if Assigned(FOnUsersTyping) then
  begin
    Data.UserIds := UserId;
    Data.PeerId := PeerId;
    Data.TotalCount := TotalCount;
    Data.TimeStamp := UnixToDateTime(Ts, False);
    FOnUsersTyping(Self, Data);
  end;
end;

procedure TCustomUserEvents.DoUserTyping(const UserId, ChatId: TVkPeerId);
begin
  if Assigned(FOnUserTyping) then
  begin
    FOnUserTyping(Self, UserId, ChatId);
  end;
end;

procedure TCustomUserEvents.DoChangeDialogFlags(const PeerId: TVkPeerId; ChangeType: TVkFlagsChangeType; FlagsMasksData: Integer);
var
  DialogChangeData: TDialogChangeData;
begin
  if Assigned(FOnChangeDialogFlags) then
  begin
    DialogChangeData.PeerId := PeerId;
    DialogChangeData.ChangeType := ChangeType;
    DialogChangeData.Flags := TVkDialogFlags.Create(FlagsMasksData);
    FOnChangeDialogFlags(Self, DialogChangeData);
  end;
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
      Result := TVkMessageAttachmentInfo.TAttachInfoType.Create(FAttach10, FAttach10_type);
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

end.

