unit VK.UserEvents;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.JSON, VK.Types, System.Generics.Collections, VK.LongPollServer, VK.API;

type
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
    procedure DoChangeDialogFlags(const PeerId: Integer; ChangeType: TFlagsChangeType; FlagsMasksData: Integer);
    procedure DoChangeMessageFlags(const MessageId: Integer; ChangeType: TFlagsChangeType; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoChatChanged(const ChatId: Integer; IsSelf: Boolean);
    procedure DoChatChangeInfo(const PeerId, TypeId, Info: Integer);
    procedure DoCountChange(const Count: Integer);
    procedure DoDeleteMessages(const PeerId, LocalId: Integer);
    procedure DoEditMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoEvent(Sender: TObject; Update: TJSONValue);
    procedure DoNewMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoNotifyChange(const PeerId, Sound, DisabledUntil: Integer);
    procedure DoReadMessages(const Incoming: Boolean; PeerId, LocalId: Integer);
    procedure DoRecoverMessages(const PeerId, LocalId: Integer);
    procedure DoUnhandledEvents(const JSON: TJSONValue);
    procedure DoUserCall(const UserId, CallId: Integer);
    procedure DoUsersRecording(const UserId: TIds; PeerId, TotalCount, Ts: Integer);
    procedure DoUserStateChange(IsOnline: Boolean; UserId, Extra, TimeStamp: Integer);
    procedure DoUsersTyping(const UserId: TIds; PeerId, TotalCount, Ts: Integer);
    procedure DoUserTyping(const UserId, ChatId: Integer);
    procedure FOnError(Sender: TObject; E: Exception; Code: Integer; Text: string);
    procedure FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
    procedure SetLogging(const Value: Boolean);
    procedure SetVersion(const Value: string);
    procedure SetVK(const Value: TCustomVK);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Start: Boolean;
    procedure Stop;
    property IsWork: Boolean read GetIsWork;
    property Logging: Boolean read FLogging write SetLogging;
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
    property Version: string read FVersion write SetVersion;
    property VK: TCustomVK read FVK write SetVK;
  end;

implementation

uses
  System.DateUtils;

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
  FVersion := '3';
  FLongPollServer := TVkLongPollServer.Create;
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
  UserIds: TIdList;
  Arr: TJSONArray;
begin
  try
    EventType := TJSONArray(Update).Items[0].GetValue<Integer>;
  except
    Exit;
    //raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
  end;
  case EventType of
    1..4: //Изменение флагов сообщений и новое сообщение
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          ExtraFields.PeerId := NormalizePeerId(TJSONArray(Update).Items[3].GetValue<Integer>);
          if TJSONArray(Update).Count > 4 then
          begin
            ExtraFields.TimeStamp := TJSONArray(Update).Items[4].GetValue<Integer>;
            if TJSONArray(Update).Count > 5 then
              ExtraFields.Text := TJSONArray(Update).Items[5].GetValue<string>;
            if TJSONArray(Update).Count > 6 then
              ExtraFields.Info := TVkMessageInfo.FromJsonString(TJSONValue(TJSONArray(Update).Items[6].GetValue<
                TJSONValue>).ToJSON);
            if TJSONArray(Update).Count > 7 then
              ExtraFields.Attachments := TVkMessageAttachmentInfo.FromJsonString(TJSONValue(TJSONArray(Update).Items[7].GetValue
                <TJSONValue>).ToJSON);
            if TJSONArray(Update).Count > 8 then
              ExtraFields.RandomId := TJSONArray(Update).Items[8].GetValue<Integer>;
          end;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        case EventType of
          1: //1	$message_id (integer) $flags (integer) extra_fields*	Замена флагов сообщения (FLAGS:=$flags).
            DoChangeMessageFlags(A1, fcFlagsReplace, A2, ExtraFields);
          2: //2	$message_id (integer) $mask (integer) extra_fields*	Установка флагов сообщения (FLAGS|=$mask).
            DoChangeMessageFlags(A1, fcFlagsSet, A2, ExtraFields);
          3: //3	$message_id (integer) $mask (integer) extra_fields*	Сброс флагов сообщения (FLAGS&=~$mask).
            DoChangeMessageFlags(A1, fcFlagsReset, A2, ExtraFields);
          4: //4	$message_id (integer) $flags (integer) extra_fields*	Добавление нового сообщения.
            DoNewMessage(A1, A2, ExtraFields);
        end;
      end;
    5: //Редактирование сообщения
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          ExtraFields.PeerId := NormalizePeerId(TJSONArray(Update).Items[3].GetValue<Integer>);
          ExtraFields.TimeStamp := TJSONArray(Update).Items[4].GetValue<Integer>;
          ExtraFields.Text := TJSONArray(Update).Items[5].GetValue<string>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        DoEditMessage(A1, A2, ExtraFields);
      end;
    6, 7: //Прочтение сообщений
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        DoReadMessages(EventType = 6, A1, A2);
      end;
    8, 9: //Online/Offline пользователя
      begin
        try
          A1 := -TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          A3 := TJSONArray(Update).Items[3].GetValue<Integer>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        DoUserStateChange(EventType = 8, A1, A2, A3);
      end;
    10, 11, 12: //Изменение флагов диалога
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        case EventType of
          10: //$peer_id (integer) $mask (integer)	Сброс флагов диалога $peer_id. Соответствует операции (PEER_FLAGS &= ~$flags). Только для диалогов сообществ.
            DoChangeDialogFlags(A1, fcFlagsReset, A2);
          11: //$peer_id (integer) $flags (integer)	Замена флагов диалога $peer_id. Соответствует операции (PEER_FLAGS:= $flags). Только для диалогов сообществ.
            DoChangeDialogFlags(A1, fcFlagsReplace, A2);
          12: //$peer_id (integer) $mask (integer)	Установка флагов диалога $peer_id. Соответствует операции (PEER_FLAGS|= $flags). Только для диалогов сообществ.
            DoChangeDialogFlags(A1, fcFlagsSet, A2);
        end;
      end;
    13, 14: //Удаление/восставноление сообщений
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
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
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          if TJSONArray(Update).Count > 2 then
            A2 := TJSONArray(Update).Items[2].GetValue<Integer>
          else
            A2 := 0;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        DoChatChanged(A1, A2 = 1);
      end;
    52: //Изменение информации чата $peer_id с типом $type_id, $info — дополнительная информация об изменениях, зависит от типа события
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := NormalizePeerId(TJSONArray(Update).Items[2].GetValue<Integer>);
          A3 := TJSONArray(Update).Items[3].GetValue<Integer>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        DoChatChangeInfo(A2, A1, A3);
      end;
    61, 62: //Пользователь набирает текст в диалоге/чате
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          if EventType = 62 then
            A2 := TJSONArray(Update).Items[2].GetValue<Integer>
          else
            A2 := A1;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        DoUserTyping(A1, A2);
      end;
    63, 64: //Пользователи в беседе набирают текст или записывают аудио
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
          Arr := TJSONArray(TJSONArray(Update).Items[2]);
          SetLength(UserIds, Arr.Count);
          for i := 0 to Pred(Arr.Count) do
          begin
            UserIds[i] := Arr.Items[i].GetValue<Integer>;
          end;
          A2 := TJSONArray(Update).Items[3].GetValue<Integer>;
          A3 := TJSONArray(Update).Items[4].GetValue<Integer>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
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
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        DoUserCall(A1, A2);
      end;
    80: //Счетчик в левом меню стал равен $count.
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
        end;
        DoCountChange(A1);
      end;
    114: //Изменились настройки оповещений. $peer_id — идентификатор чата/собеседника,
         //'$sound — 1/0, включены/выключены звуковые оповещения,
         //$disabled_until — выключение оповещений на необходимый срок (-1: навсегда, ''0
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          if TJSONArray(Update).Count > 3 then
            A3 := TJSONArray(Update).Items[3].GetValue<Integer>
          else
            A3 := 0;
        except
          raise TVkUserEventsException.Create('Ошибка при извлечении данных события пользователя');
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

function TCustomUserEvents.GetIsWork: Boolean;
begin
  Result := FLongPollServer.IsWork;
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
  FLongPollServer.Params.Add('lp_version', FVersion);
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

procedure TCustomUserEvents.DoChangeMessageFlags(const MessageId: Integer; ChangeType: TFlagsChangeType; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
var
  MessageChangeData: TMessageChangeData;
begin
  if Assigned(FOnChangeMessageFlags) then
  begin
    MessageChangeData.MessageId := MessageId;
    MessageChangeData.ChangeType := ChangeType;
    MessageChangeData.Flags := TMessageFlags.Create(FlagsMasksData);
    MessageChangeData.PeerId := ExtraFields.PeerId;
    FOnChangeMessageFlags(Self, MessageChangeData);
  end;
end;

procedure TCustomUserEvents.DoChatChanged(const ChatId: Integer; IsSelf: Boolean);
begin
  if Assigned(FOnChatChanged) then
  begin
    FOnChatChanged(Self, ChatId, IsSelf);
  end;
end;

procedure TCustomUserEvents.DoChatChangeInfo(const PeerId, TypeId, Info: Integer);
begin
  if Assigned(FOnChatChangeInfo) then
  begin
    FOnChatChangeInfo(Self, PeerId, TChatChangeInfoType(TypeId), Info);
  end;
end;

procedure TCustomUserEvents.DoNewMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
var
  MessageData: TMessageData;
begin
  if Assigned(FOnNewMessage) then
  begin
    MessageData.MessageId := MessageId;
    MessageData.Flags := TMessageFlags.Create(FlagsMasksData);
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

procedure TCustomUserEvents.DoReadMessages(const Incoming: Boolean; PeerId, LocalId: Integer);
begin
  if Assigned(FOnReadMessages) then
  begin
    FOnReadMessages(Self, Incoming, PeerId, LocalId);
  end;
end;

procedure TCustomUserEvents.DoRecoverMessages(const PeerId, LocalId: Integer);
begin
  if Assigned(FOnRecoverMessages) then
  begin
    FOnRecoverMessages(Self, PeerId, LocalId);
  end;
end;

procedure TCustomUserEvents.DoDeleteMessages(const PeerId, LocalId: Integer);
begin
  if Assigned(FOnDeleteMessages) then
  begin
    FOnDeleteMessages(Self, PeerId, LocalId);
  end;
end;

procedure TCustomUserEvents.DoEditMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
var
  MessageData: TMessageData;
begin
  if Assigned(FOnEditMessage) then
  begin
    MessageData.MessageId := MessageId;
    MessageData.Flags := TMessageFlags.Create(FlagsMasksData);
    MessageData.PeerId := ExtraFields.PeerId;
    MessageData.TimeStamp := UnixToDateTime(ExtraFields.TimeStamp, False);
    MessageData.Text := ExtraFields.Text;
    FOnEditMessage(Self, MessageData);
  end;
end;

procedure TCustomUserEvents.DoUnhandledEvents(const JSON: TJSONValue);
begin
  if Assigned(FOnUnhandledEvents) then
  begin
    FOnUnhandledEvents(Self, JSON);
  end;
end;

procedure TCustomUserEvents.DoUserCall(const UserId, CallId: Integer);
begin
  if Assigned(FOnUserCall) then
  begin
    FOnUserCall(Self, UserId, CallId);
  end;
end;

procedure TCustomUserEvents.DoCountChange(const Count: Integer);
begin
  if Assigned(FOnCountChange) then
  begin
    FOnCountChange(Self, Count);
  end;
end;

procedure TCustomUserEvents.DoNotifyChange(const PeerId, Sound, DisabledUntil: Integer);
begin
  if Assigned(FOnNotifyChange) then
  begin
    FOnNotifyChange(Self, PeerId, Sound = 1, DisabledUntil);
  end;
end;

procedure TCustomUserEvents.DoUsersRecording(const UserId: TIdList; PeerId, TotalCount, Ts: Integer);
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

procedure TCustomUserEvents.DoUserStateChange(IsOnline: Boolean; UserId, Extra, TimeStamp: Integer);
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
        VkPlatform := pfUnknown;
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

procedure TCustomUserEvents.DoUsersTyping(const UserId: TIdList; PeerId, TotalCount, Ts: Integer);
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

procedure TCustomUserEvents.DoUserTyping(const UserId, ChatId: Integer);
begin
  if Assigned(FOnUserTyping) then
  begin
    FOnUserTyping(Self, UserId, ChatId);
  end;
end;

procedure TCustomUserEvents.DoChangeDialogFlags(const PeerId: Integer; ChangeType: TFlagsChangeType; FlagsMasksData: Integer);
var
  DialogChangeData: TDialogChangeData;
begin
  if Assigned(FOnChangeDialogFlags) then
  begin
    DialogChangeData.PeerId := PeerId;
    DialogChangeData.ChangeType := ChangeType;
    DialogChangeData.Flags := TDialogFlags.Create(FlagsMasksData);
    FOnChangeDialogFlags(Self, DialogChangeData);
  end;
end;

end.

