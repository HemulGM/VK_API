unit VK.UserEvents;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, REST.Client,
  System.JSON, VK.Types, System.Generics.Collections, VK.LongPollServer, VK.API;

type
  TCustomUserEvents = class(TComponent)
  private
    FLongPollServer: TLongPollServer;
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
    procedure FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
    procedure DoEvent(Sender: TObject; Update: TJSONValue);
    procedure DoChangeMessageFlags(const MessageId: Integer; ChangeType: TFlagsChangeType;
      FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoUserTyping(const UserId, ChatId: Integer);
    procedure DoUserCall(const UserId, CallId: Integer);
    procedure DoUsersTyping(const UserId: TUserIds; PeerId, TotalCount, Ts: Integer);
    procedure DoUsersRecording(const UserId: TUserIds; PeerId, TotalCount, Ts: Integer);
    procedure DoChangeDialogFlags(const PeerId: Integer; ChangeType: TFlagsChangeType; FlagsMasksData: Integer);
    procedure DoNewMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoUserStateChange(IsOnline: Boolean; UserId, Extra, TimeStamp: Integer);
    procedure DoEditMessage(const MessageId: Integer; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
    procedure DoReadMessages(const Incoming: Boolean; PeerId, LocalId: Integer);
    procedure DoRecoverMessages(const PeerId, LocalId: Integer);
    procedure DoDeleteMessages(const PeerId, LocalId: Integer);
    procedure DoChatChanged(const ChatId: Integer; IsSelf: Boolean);
    procedure DoChatChangeInfo(const PeerId, TypeId, Info: Integer);
    procedure SetOnChangeDialogFlags(const Value: TOnChangeDialogFlags);
    procedure SetOnChangeMessageFlags(const Value: TOnChangeMessageFlags);
    procedure SetOnChatChanged(const Value: TOnChatChanged);
    procedure SetOnChatChangeInfo(const Value: TOnChatChangeInfo);
    procedure SetOnDeleteMessages(const Value: TOnRecoverOrDeleteMessages);
    procedure SetOnEditMessage(const Value: TOnEditMessage);
    procedure SetOnNewMessage(const Value: TOnNewMessage);
    procedure SetOnReadMessages(const Value: TOnReadMessages);
    procedure SetOnRecoverMessages(const Value: TOnRecoverOrDeleteMessages);
    procedure SetOnUserOffline(const Value: TOnUserOffline);
    procedure SetOnUserOnline(const Value: TOnUserOnline);
    procedure SetOnUsersRecording(const Value: TOnUsersRecording);
    procedure SetOnUsersTyping(const Value: TOnUsersTyping);
    procedure SetOnUserTyping(const Value: TOnUserTyping);
    procedure SetVK(const Value: TCustomVK);
    procedure SetOnUserCall(const Value: TOnUserCall);
    procedure DoCountChange(const Count: Integer);
    procedure SetOnCountChange(const Value: TOnCountChange);
    procedure DoNotifyChange(const PeerId, Sound, DisabledUntil: Integer);
    procedure SetOnNotifyChange(const Value: TOnNotifyChange);
    procedure FOnError(Sender: TObject; Code: Integer; Text: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Stop;
    function Start: Boolean;
    property VK: TCustomVK read FVK write SetVK;
    property OnNewMessage: TOnNewMessage read FOnNewMessage write SetOnNewMessage;
    property OnEditMessage: TOnEditMessage read FOnEditMessage write SetOnEditMessage;
    property OnUserOnline: TOnUserOnline read FOnUserOnline write SetOnUserOnline;
    property OnUserOffline: TOnUserOffline read FOnUserOffline write SetOnUserOffline;
    property OnChangeMessageFlags: TOnChangeMessageFlags read FOnChangeMessageFlags write SetOnChangeMessageFlags;
    property OnChangeDialogFlags: TOnChangeDialogFlags read FOnChangeDialogFlags write SetOnChangeDialogFlags;
    property OnReadMessages: TOnReadMessages read FOnReadMessages write SetOnReadMessages;
    property OnRecoverMessages: TOnRecoverOrDeleteMessages read FOnRecoverMessages write SetOnRecoverMessages;
    property OnDeleteMessages: TOnRecoverOrDeleteMessages read FOnDeleteMessages write SetOnDeleteMessages;
    property OnChatChanged: TOnChatChanged read FOnChatChanged write SetOnChatChanged;
    property OnChatChangeInfo: TOnChatChangeInfo read FOnChatChangeInfo write SetOnChatChangeInfo;
    property OnUserTyping: TOnUserTyping read FOnUserTyping write SetOnUserTyping;
    property OnUsersTyping: TOnUsersTyping read FOnUsersTyping write SetOnUsersTyping;
    property OnUsersRecording: TOnUsersRecording read FOnUsersRecording write SetOnUsersRecording;
    property OnUserCall: TOnUserCall read FOnUserCall write SetOnUserCall;
    property OnCountChange: TOnCountChange read FOnCountChange write SetOnCountChange;
    property OnNotifyChange: TOnNotifyChange read FOnNotifyChange write SetOnNotifyChange;
  end;

implementation

uses
  System.DateUtils;

{ TUserEvents }

constructor TCustomUserEvents.Create(AOwner: TComponent);
begin
  inherited;
  FLongPollServer := TLongPollServer.Create;
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
  UserIds: TUserIds;
  Arr: TJSONArray;

  function NormalizePeerId(Value: Integer): Integer;
  begin
    if Value > 2000000000 then
      Exit(Value - 2000000000);
    if Value > 1000000000 then
      Exit(-(Value - 1000000000));
    Result := Value;
  end;

begin
  EventType := TJSONArray(Update).Items[0].AsType<Integer>;
  case EventType of
    1..4: //Изменение флагов сообщений и новое сообщение
      begin
        A1 := TJSONArray(Update).Items[1].AsType<Integer>;
        A2 := TJSONArray(Update).Items[2].AsType<Integer>;
        ExtraFields.peer_id := NormalizePeerId(TJSONArray(Update).Items[3].AsType<Integer>);
        if TJSONArray(Update).Count > 4 then
        begin
          ExtraFields.timestamp := TJSONArray(Update).Items[4].AsType<Integer>;
          if TJSONArray(Update).Count > 5 then
            ExtraFields.text := TJSONArray(Update).Items[5].AsType<string>;
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
        A1 := TJSONArray(Update).Items[1].AsType<Integer>;
        A2 := TJSONArray(Update).Items[2].AsType<Integer>;
        ExtraFields.peer_id := NormalizePeerId(TJSONArray(Update).Items[3].AsType<Integer>);
        ExtraFields.timestamp := TJSONArray(Update).Items[4].AsType<Integer>;
        ExtraFields.text := TJSONArray(Update).Items[5].AsType<string>;
        DoEditMessage(A1, A2, ExtraFields);
      end;
    6, 7: //Прочтение сообщений
      begin
        A1 := NormalizePeerId(TJSONArray(Update).Items[1].AsType<Integer>);
        A2 := TJSONArray(Update).Items[2].AsType<Integer>;
        DoReadMessages(EventType = 6, A1, A2);
      end;
    8, 9: //Online/Offline пользователя
      begin
        A1 := TJSONArray(Update).Items[1].AsType<Integer> *  - 1;
        A2 := TJSONArray(Update).Items[2].AsType<Integer>;
        A3 := TJSONArray(Update).Items[3].AsType<Integer>;
        DoUserStateChange(EventType = 8, A1, A2, A3);
      end;
    10, 11, 12: //Изменение флагов диалога
      begin
        A1 := NormalizePeerId(TJSONArray(Update).Items[1].AsType<Integer>);
        A2 := TJSONArray(Update).Items[2].AsType<Integer>;
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
        A1 := NormalizePeerId(TJSONArray(Update).Items[1].AsType<Integer>);
        A2 := TJSONArray(Update).Items[2].AsType<Integer>;
        case EventType of
          13: //Удаление всех сообщений в диалоге $peer_id с идентификаторами вплоть до $local_id.
            DoDeleteMessages(A1, A2);
          14: //Восстановление недавно удаленных сообщений в диалоге $peer_id с идентификаторами вплоть до $local_id.
            DoRecoverMessages(A1, A2);
        end;
      end;
    51: //Один из параметров (состав, тема) беседы $chat_id были изменены. $self — 1 или 0 (вызваны ли изменения самим пользователем).
      begin
        A1 := TJSONArray(Update).Items[1].AsType<Integer>;
        if TJSONArray(Update).Count > 2 then
          A2 := TJSONArray(Update).Items[2].AsType<Integer>
        else
          A2 := 0;
        DoChatChanged(A1, A2 = 1);
      end;
    52: //Изменение информации чата $peer_id с типом $type_id, $info — дополнительная информация об изменениях, зависит от типа события
      begin
        A1 := TJSONArray(Update).Items[1].AsType<Integer>;
        A2 := NormalizePeerId(TJSONArray(Update).Items[2].AsType<Integer>);
        A3 := TJSONArray(Update).Items[3].AsType<Integer>;
        DoChatChangeInfo(A2, A1, A3);
      end;
    61, 62: //Пользователь набирает текст в диалоге/чате
      begin
        A1 := TJSONArray(Update).Items[1].AsType<Integer>;
        if EventType = 62 then
          A2 := TJSONArray(Update).Items[2].AsType<Integer>
        else
          A2 := A1;
        DoUserTyping(A1, A2);
      end;
    63, 64: //Пользователи в беседе набирают текст или записывают аудио
      begin
        Arr := TJSONArray(TJSONArray(Update).Items[1]);
        SetLength(UserIds, 0);
        for i := 0 to Arr.Count do
        begin
          SetLength(UserIds, Length(UserIds) + 1);
          UserIds[Length(UserIds) - 1] := Arr.Items[i].AsType<Integer>;
        end;

        A1 := NormalizePeerId(TJSONArray(Update).Items[2].AsType<Integer>);
        A2 := TJSONArray(Update).Items[3].AsType<Integer>;
        A3 := TJSONArray(Update).Items[4].AsType<Integer>;
        case EventType of
          63:
            DoUsersTyping(UserIds, A1, A2, A3);
          64:
            DoUsersRecording(UserIds, A1, A2, A3);
        end;
      end;
    70: //Пользователь $user_id совершил звонок с идентификатором $call_id.
      begin
        A1 := TJSONArray(Update).Items[1].AsType<Integer>;
        A2 := TJSONArray(Update).Items[2].AsType<Integer>;
        DoUserCall(A1, A2);
      end;
    80: //Счетчик в левом меню стал равен $count.
      begin
        A1 := TJSONArray(Update).Items[1].AsType<Integer>;
        DoCountChange(A1);
      end;
    114: //Изменились настройки оповещений. $peer_id — идентификатор чата/собеседника,
         //'$sound — 1/0, включены/выключены звуковые оповещения,
         //$disabled_until — выключение оповещений на необходимый срок (-1: навсегда, ''0
      begin
        A1 := NormalizePeerId(TJSONArray(Update).Items[1].AsType<Integer>);
        A2 := TJSONArray(Update).Items[2].AsType<Integer>;
        if TJSONArray(Update).Count > 3 then
          A3 := TJSONArray(Update).Items[3].AsType<Integer>
        else
          A3 := 0;
        DoNotifyChange(A1, A2, A3);
      end;
  end;
end;

procedure TCustomUserEvents.FOnError(Sender: TObject; Code: Integer; Text: string);
begin
  //
end;

procedure TCustomUserEvents.FOnLongPollUpdate(Sender: TObject; GroupID: string; Update: TJSONValue);
begin
  FVK.DoLog(Self, Update.ToString);
  DoEvent(Sender, Update);
end;

procedure TCustomUserEvents.SetOnChangeDialogFlags(const Value: TOnChangeDialogFlags);
begin
  FOnChangeDialogFlags := Value;
end;

procedure TCustomUserEvents.SetOnChangeMessageFlags(const Value: TOnChangeMessageFlags);
begin
  FOnChangeMessageFlags := Value;
end;

procedure TCustomUserEvents.SetOnChatChanged(const Value: TOnChatChanged);
begin
  FOnChatChanged := Value;
end;

procedure TCustomUserEvents.SetOnChatChangeInfo(const Value: TOnChatChangeInfo);
begin
  FOnChatChangeInfo := Value;
end;

procedure TCustomUserEvents.SetOnCountChange(const Value: TOnCountChange);
begin
  FOnCountChange := Value;
end;

procedure TCustomUserEvents.SetOnDeleteMessages(const Value: TOnRecoverOrDeleteMessages);
begin
  FOnDeleteMessages := Value;
end;

procedure TCustomUserEvents.SetOnEditMessage(const Value: TOnEditMessage);
begin
  FOnEditMessage := Value;
end;

procedure TCustomUserEvents.SetOnNewMessage(const Value: TOnNewMessage);
begin
  FOnNewMessage := Value;
end;

procedure TCustomUserEvents.SetOnNotifyChange(const Value: TOnNotifyChange);
begin
  FOnNotifyChange := Value;
end;

procedure TCustomUserEvents.SetOnReadMessages(const Value: TOnReadMessages);
begin
  FOnReadMessages := Value;
end;

procedure TCustomUserEvents.SetOnRecoverMessages(const Value: TOnRecoverOrDeleteMessages);
begin
  FOnRecoverMessages := Value;
end;

procedure TCustomUserEvents.SetOnUserCall(const Value: TOnUserCall);
begin
  FOnUserCall := Value;
end;

procedure TCustomUserEvents.SetOnUserOffline(const Value: TOnUserOffline);
begin
  FOnUserOffline := Value;
end;

procedure TCustomUserEvents.SetOnUserOnline(const Value: TOnUserOnline);
begin
  FOnUserOnline := Value;
end;

procedure TCustomUserEvents.SetOnUsersRecording(const Value: TOnUsersRecording);
begin
  FOnUsersRecording := Value;
end;

procedure TCustomUserEvents.SetOnUsersTyping(const Value: TOnUsersTyping);
begin
  FOnUsersTyping := Value;
end;

procedure TCustomUserEvents.SetOnUserTyping(const Value: TOnUserTyping);
begin
  FOnUserTyping := Value;
end;

procedure TCustomUserEvents.SetVK(const Value: TCustomVK);
begin
  FVK := Value;
end;

function TCustomUserEvents.Start: Boolean;
begin
  if not Assigned(FVK) then
    raise Exception.Create('Для работы необходим VK контроллер (Свойство VK)');
  FLongPollServer.Client := FVK.Handler.RESTClient;
  FLongPollServer.Method := 'messages.getLongPollServer';
  FLongPollServer.Params := [['lp_version', '3']];
  Result := FLongPollServer.Start;
end;

procedure TCustomUserEvents.Stop;
begin
  FLongPollServer.Stop;
end;

procedure TCustomUserEvents.DoChangeMessageFlags(const MessageId: Integer; ChangeType:
  TFlagsChangeType; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
var
  MessageChangeData: TMessageChangeData;
begin
  if Assigned(FOnChangeMessageFlags) then
  begin
    MessageChangeData.MessageId := MessageId;
    MessageChangeData.ChangeType := ChangeType;
    MessageChangeData.Flags := MessageFlags.Create(FlagsMasksData);
    MessageChangeData.PeerId := ExtraFields.peer_id;
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

procedure TCustomUserEvents.DoNewMessage(const MessageId: Integer; FlagsMasksData: Integer;
  ExtraFields: TEventExtraFields);
var
  MessageData: TMessageData;
begin
  if Assigned(FOnNewMessage) then
  begin
    MessageData.MessageId := MessageId;
    MessageData.Flags := MessageFlags.Create(FlagsMasksData);
    MessageData.PeerId := ExtraFields.peer_id;
    MessageData.TimeStamp := UnixToDateTime(ExtraFields.timestamp, False);
    MessageData.Text := ExtraFields.text;
    FOnNewMessage(Self, MessageData);
  end;
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

procedure TCustomUserEvents.DoEditMessage(const MessageId: Integer; FlagsMasksData: Integer;
  ExtraFields: TEventExtraFields);
var
  MessageData: TMessageData;
begin
  if Assigned(FOnEditMessage) then
  begin
    MessageData.MessageId := MessageId;
    MessageData.Flags := MessageFlags.Create(FlagsMasksData);
    MessageData.PeerId := ExtraFields.peer_id;
    MessageData.TimeStamp := UnixToDateTime(ExtraFields.timestamp, False);
    MessageData.Text := ExtraFields.text;
    FOnEditMessage(Self, MessageData);
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

procedure TCustomUserEvents.DoUsersRecording(const UserId: TUserIds; PeerId, TotalCount, Ts: Integer);
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

procedure TCustomUserEvents.DoUsersTyping(const UserId: TUserIds; PeerId, TotalCount, Ts: Integer);
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

procedure TCustomUserEvents.DoChangeDialogFlags(const PeerId: Integer; ChangeType: TFlagsChangeType;
  FlagsMasksData: Integer);
var
  DialogChangeData: TDialogChangeData;
begin
  if Assigned(FOnChangeDialogFlags) then
  begin
    DialogChangeData.PeerId := PeerId;
    DialogChangeData.ChangeType := ChangeType;
    DialogChangeData.Flags := DialogFlags.Create(FlagsMasksData);
    FOnChangeDialogFlags(Self, DialogChangeData);
  end;
end;

end.

