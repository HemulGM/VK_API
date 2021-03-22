unit VK.UserEvents;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON, VK.Types,
  System.Generics.Collections, VK.LongPollServer, VK.API;

type
  TVkMessageInfo = class
  private
    FTitle: string;
    FFrom: string;
    FMentions: TArray<integer>;
  public
    property Title: string read FTitle write FTitle;
    property From: string read FFrom write FFrom;
    property Mentions: TArray<integer> read FMentions write FMentions;

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
    class function FromJsonString(AJsonString: string): TVkMessageAttachmentInfo;
  end;

  /// <summary>
  /// ��������� ������� ��������� ���������
  /// </summary>
  TMessageData = record
    MessageId: Integer;
    Flags: TVkMessageFlags;
    PeerId: Integer;
    TimeStamp: TDateTime;
    Text: string;
    Info: TVkMessageInfo;
    RandomId: Integer;
    MinorId: Integer;
    Attachments: TVkMessageAttachmentInfo;
  end;

  TMessageChangeData = record
    MessageId: Integer;
    Flags: TVkMessageFlags;
    PeerId: Integer;
    ChangeType: TVkFlagsChangeType;
  end;

  TDialogChangeData = record
    PeerId: Integer;
    Flags: TVkDialogFlags;
    ChangeType: TVkFlagsChangeType;
  end;

  TEventExtraFields = record
    PeerId: integer; // ������������� ����������. ��� ������������: id ������������. ��� ��������� ������: 2000000000 + id ������. ��� ����������: -id ���������� ���� id ���������� + 1000000000 (��� version = 0).
    TimeStamp: integer; // ����� �������� ��������� � Unixtime;
    Text: string; // ����� ���������;
    Info: TVkMessageInfo;
    Attachments: TVkMessageAttachmentInfo;
    RandomId: Integer;
  end;

  TChatTypingData = record
    UserIds: TIdList;
    PeerId, TotalCount: Integer;
    TimeStamp: TDateTime;
  end;

  TChatRecordingData = record
    UserIds: TIdList;
    PeerId, TotalCount: Integer;
    TimeStamp: TDateTime;
  end;

  TOnNewMessage = procedure(Sender: TObject; MessageData: TMessageData) of object;

  TOnEditMessage = procedure(Sender: TObject; MessageData: TMessageData) of object;

  TOnChangeMessageFlags = procedure(Sender: TObject; MessageChangeData: TMessageChangeData) of object;

  TOnChangeDialogFlags = procedure(Sender: TObject; DialogChangeData: TDialogChangeData) of object;

  TOnUserOnline = procedure(Sender: TObject; UserId: Integer; VkPlatform: TVkPlatform; TimeStamp: TDateTime) of object;

  TOnUserOffline = procedure(Sender: TObject; UserId: Integer; InactiveUser: Boolean; TimeStamp: TDateTime) of object;

  TOnReadMessages = procedure(Sender: TObject; Incoming: Boolean; PeerId, LocalId: Integer) of object;

  TOnRecoverOrDeleteMessages = procedure(Sender: TObject; PeerId, LocalId: Integer) of object;

  TOnChatChanged = procedure(Sender: TObject; const ChatId: Integer; IsSelf: Boolean) of object;

  TOnChatChangeInfo = procedure(Sender: TObject; const PeerId: Integer; TypeId: TVkChatChangeInfoType; Info: Integer) of object;

  TOnUserTyping = procedure(Sender: TObject; UserId, ChatId: Integer) of object;

  TOnUserCall = procedure(Sender: TObject; UserId, CallId: Integer) of object;

  TOnCountChange = procedure(Sender: TObject; Count: Integer) of object;

  TOnNotifyChange = procedure(Sender: TObject; PeerId: Integer; Sound: Boolean; DisableUntil: Integer) of object;

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
    procedure DoChangeDialogFlags(const PeerId: Integer; ChangeType: TVkFlagsChangeType; FlagsMasksData: Integer);
    procedure DoChangeMessageFlags(const MessageId: Integer; ChangeType: TVkFlagsChangeType; FlagsMasksData: Integer; ExtraFields: TEventExtraFields);
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
    procedure DoUsersRecording(const UserId: TIdList; PeerId, TotalCount, Ts: Integer);
    procedure DoUserStateChange(IsOnline: Boolean; UserId, Extra, TimeStamp: Integer);
    procedure DoUsersTyping(const UserId: TIdList; PeerId, TotalCount, Ts: Integer);
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

  procedure DoRaiseProcessing;
  begin
    raise TVkUserEventsException.Create('������ ��� ���������� ������ ������� ������������');
  end;

begin
  try
    EventType := TJSONArray(Update).Items[0].GetValue<Integer>;
    A1 := 0;
    A2 := 0;
    A3 := 0;
  except
    Exit;
    //raise TVkUserEventsException.Create('������ ��� ���������� ������ ������� ������������');
  end;
  case EventType of
    1..4: //��������� ������ ��������� � ����� ���������
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
              ExtraFields.Info := TVkMessageInfo.FromJsonString(TJSONArray(Update).Items[6].GetValue<TJSONValue>.ToJSON);
            if TJSONArray(Update).Count > 7 then
              ExtraFields.Attachments := TVkMessageAttachmentInfo.FromJsonString(TJSONArray(Update).Items[7].GetValue<TJSONValue>.ToJSON);
            if TJSONArray(Update).Count > 8 then
              ExtraFields.RandomId := TJSONArray(Update).Items[8].GetValue<Integer>;
          end;
        except
          DoRaiseProcessing;
        end;
        case EventType of
          1: //1	$message_id (integer) $flags (integer) extra_fields*	������ ������ ��������� (FLAGS:=$flags).
            DoChangeMessageFlags(A1, TVkFlagsChangeType.Replace, A2, ExtraFields);
          2: //2	$message_id (integer) $mask (integer) extra_fields*	��������� ������ ��������� (FLAGS|=$mask).
            DoChangeMessageFlags(A1, TVkFlagsChangeType.&Set, A2, ExtraFields);
          3: //3	$message_id (integer) $mask (integer) extra_fields*	����� ������ ��������� (FLAGS&=~$mask).
            DoChangeMessageFlags(A1, TVkFlagsChangeType.Reset, A2, ExtraFields);
          4: //4	$message_id (integer) $flags (integer) extra_fields*	���������� ������ ���������.
            DoNewMessage(A1, A2, ExtraFields);
        end;
      end;
    5: //�������������� ���������
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          ExtraFields.PeerId := NormalizePeerId(TJSONArray(Update).Items[3].GetValue<Integer>);
          ExtraFields.TimeStamp := TJSONArray(Update).Items[4].GetValue<Integer>;
          ExtraFields.Text := TJSONArray(Update).Items[5].GetValue<string>;
        except
          DoRaiseProcessing;
        end;
        DoEditMessage(A1, A2, ExtraFields);
      end;
    6, 7: //��������� ���������
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        DoReadMessages(EventType = 6, A1, A2);
      end;
    8, 9: //Online/Offline ������������
      begin
        try
          A1 := -TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
          A3 := TJSONArray(Update).Items[3].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        DoUserStateChange(EventType = 8, A1, A2, A3);
      end;
    10, 11, 12: //��������� ������ �������
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        case EventType of
          10: //$peer_id (integer) $mask (integer)	����� ������ ������� $peer_id. ������������� �������� (PEER_FLAGS &= ~$flags). ������ ��� �������� ���������.
            DoChangeDialogFlags(A1, TVkFlagsChangeType.Reset, A2);
          11: //$peer_id (integer) $flags (integer)	������ ������ ������� $peer_id. ������������� �������� (PEER_FLAGS:= $flags). ������ ��� �������� ���������.
            DoChangeDialogFlags(A1, TVkFlagsChangeType.Replace, A2);
          12: //$peer_id (integer) $mask (integer)	��������� ������ ������� $peer_id. ������������� �������� (PEER_FLAGS|= $flags). ������ ��� �������� ���������.
            DoChangeDialogFlags(A1, TVkFlagsChangeType.&Set, A2);
        end;
      end;
    13, 14: //��������/�������������� ���������
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        case EventType of
          13: //�������� ���� ��������� � ������� $peer_id � ���������������� ������ �� $local_id.
            DoDeleteMessages(A1, A2);
          14: //�������������� ������� ��������� ��������� � ������� $peer_id � ���������������� ������ �� $local_id.
            DoRecoverMessages(A1, A2);
        end;
      end;
    51: //���� �� ���������� (������, ����) ������ $chat_id ���� ��������. $self � 1 ��� 0 (������� �� ��������� ����� �������������).
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          if TJSONArray(Update).Count > 2 then
            A2 := TJSONArray(Update).Items[2].GetValue<Integer>
          else
            A2 := 0;
        except
          DoRaiseProcessing;
        end;
        DoChatChanged(A1, A2 = 1);
      end;
    52: //��������� ���������� ���� $peer_id � ����� $type_id, $info � �������������� ���������� �� ����������, ������� �� ���� �������
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := NormalizePeerId(TJSONArray(Update).Items[2].GetValue<Integer>);
          A3 := TJSONArray(Update).Items[3].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        DoChatChangeInfo(A2, A1, A3);
      end;
    61, 62: //������������ �������� ����� � �������/����
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          if EventType = 62 then
            A2 := TJSONArray(Update).Items[2].GetValue<Integer>
          else
            A2 := A1;
        except
          DoRaiseProcessing;
        end;
        DoUserTyping(A1, A2);
      end;
    63, 64: //������������ � ������ �������� ����� ��� ���������� �����
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
          DoRaiseProcessing;
        end;
        case EventType of
          63:
            DoUsersTyping(UserIds, A1, A2, A3);
          64:
            DoUsersRecording(UserIds, A1, A2, A3);
        end;
      end;
    70: //������������ $user_id �������� ������ � ��������������� $call_id.
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
          A2 := TJSONArray(Update).Items[2].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        DoUserCall(A1, A2);
      end;
    80: //������� � ����� ���� ���� ����� $count.
      begin
        try
          A1 := TJSONArray(Update).Items[1].GetValue<Integer>;
        except
          DoRaiseProcessing;
        end;
        DoCountChange(A1);
      end;
    114: //���������� ��������� ����������. $peer_id � ������������� ����/�����������,
         //'$sound � 1/0, ��������/��������� �������� ����������,
         //$disabled_until � ���������� ���������� �� ����������� ���� (-1: ��������, ''0
      begin
        try
          A1 := NormalizePeerId(TJSONArray(Update).Items[1].GetValue<Integer>);
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
    raise Exception.Create('��� ������ ��������� VK ���������� (�������� VK)');
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
end;

procedure TCustomUserEvents.DoChatChanged(const ChatId: Integer; IsSelf: Boolean);
begin
  if Assigned(FOnChatChanged) then
    FOnChatChanged(Self, ChatId, IsSelf);
end;

procedure TCustomUserEvents.DoChatChangeInfo(const PeerId, TypeId, Info: Integer);
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

procedure TCustomUserEvents.DoReadMessages(const Incoming: Boolean; PeerId, LocalId: Integer);
begin
  if Assigned(FOnReadMessages) then
    FOnReadMessages(Self, Incoming, PeerId, LocalId);
end;

procedure TCustomUserEvents.DoRecoverMessages(const PeerId, LocalId: Integer);
begin
  if Assigned(FOnRecoverMessages) then
    FOnRecoverMessages(Self, PeerId, LocalId);
end;

procedure TCustomUserEvents.DoDeleteMessages(const PeerId, LocalId: Integer);
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
end;

procedure TCustomUserEvents.DoUnhandledEvents(const JSON: TJSONValue);
begin
  if Assigned(FOnUnhandledEvents) then
    FOnUnhandledEvents(Self, JSON);
end;

procedure TCustomUserEvents.DoUserCall(const UserId, CallId: Integer);
begin
  if Assigned(FOnUserCall) then
    FOnUserCall(Self, UserId, CallId);
end;

procedure TCustomUserEvents.DoCountChange(const Count: Integer);
begin
  if Assigned(FOnCountChange) then
    FOnCountChange(Self, Count);
end;

procedure TCustomUserEvents.DoNotifyChange(const PeerId, Sound, DisabledUntil: Integer);
begin
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self, PeerId, Sound = 1, DisabledUntil);
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

procedure TCustomUserEvents.DoChangeDialogFlags(const PeerId: Integer; ChangeType: TVkFlagsChangeType; FlagsMasksData: Integer);
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

