unit VK.Messages;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, REST.Client,
  System.Json, VK.Controller, VK.Types, VK.Handler, VK.Entity.Keyboard,
  VK.Entity.Message, VK.Entity.Conversation, VK.Entity.Profile, VK.Entity.Group,
  VK.Entity.Message.Chat, VK.Entity.Media, VK.Entity.Common, VK.Entity.LongPoll,
  VK.Entity.Common.List, VK.Entity.Message.Templates;

type
  TMessagesController = class;

  TVkMessageNew = class
  private
    FHandler: TVkHandler;
    FParams: TParams;
    procedure SetParams(const Value: TParams);
  public    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkMessageNew;
    /// <summary>
    /// ������������� ������������, �������� ������������ ���������.
    /// </summary>
    function UserId(const Value: Integer): TVkMessageNew;
    /// <summary>
    /// ������������� ������, � ������� ����� ���������� ���������
    /// </summary>
    function ChatId(const Value: Integer): TVkMessageNew;
    /// <summary>
    /// �������������� ����������� ��������� (��� ������������� ��������� ��������� ����� ���������� �������������).
    /// �������� ������ ��� ����� ������� ����������. ������������ ���������� ���������������: 100
    /// </summary>
    function PeerIds(const Value: TIdList): TVkMessageNew;
    /// <summary>
    /// ����� ������� ���������. ������������ ��������, ���� �� ����� �������� attachment
    /// </summary>
    function Message(const Value: string): TVkMessageNew;
    /// <summary>
    /// �������� ��������
    /// </summary>
    function Payload(const Value: string): TVkMessageNew;
    /// <summary>
    /// ������ � ��� �����, ������� ���������� ��������������� ���������� ��������� �� ����������.
    /// ��������� � �������������� ��������� messages.send � Intent.
    /// </summary>
    function Intent(const Value: TVkMessageIntent): TVkMessageNew;
    /// <summary>
    /// ������, ����������� ���������� ����
    /// </summary>
    function Keyboard(const Value: TVkKeyboard): TVkMessageNew; overload;
    /// <summary>
    /// ������, ����������� ���������� ����
    /// </summary>
    function Keyboard(const Value: string): TVkMessageNew; overload;
    /// <summary>
    /// �������������� ����������
    /// </summary>
    function LatLong(const Lat, Long: Extended): TVkMessageNew;
    /// <summary>
    /// �������� ����� ������������ (��������, illarionov)
    /// </summary>
    function Domain(const Value: string): TVkMessageNew;
    /// <summary>
    /// �� ��������� ������� ������ �� ���������
    /// </summary>
    function DontParseLinks(const Value: Boolean = False): TVkMessageNew;
    /// <summary>
    /// ������, ����������� �������� ����������������� �������� ��� ���-�����
    /// </summary>
    function ContentSource(const Value: TVkMessageContentSource): TVkMessageNew;
    /// <summary>
    /// ��������� ����������� �� ���������� � ���������
    /// </summary>
    function DisableMentions(const Value: Boolean): TVkMessageNew;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StickerId(const Value: Integer): TVkMessageNew;
    /// <summary>
    /// �����, ������� � ������� ����� ������������� ��� ������ � ���������
    /// </summary>
    function SubscribeId(const Value: Integer): TVkMessageNew;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkMessageNew;
    /// <summary>
    /// ������������� ���������, �� ������� ��������� ��������
    /// </summary>
    function ReplyTo(const Value: Integer): TVkMessageNew;
    /// <summary>
    /// [�������� ������ ��������� ��� ���������]
    /// </summary>
    function &Forward(const Value: TVkMessageForward): TVkMessageNew;
    /// <summary>
    /// �������������� ������������ ���������, ������������� ����� �������.
    /// ������������� ��������� ����������� ����� ������������ � ���� ������ � ����������.
    /// �� ����� 100 �������� �� ������� ������, ������������ ������� �����������: 45,
    /// ������������ ���������� ������������ ��������� 500
    /// </summary>
    function ForwardMessages(const Value: TIdList): TVkMessageNew;
    /// <summary>
    /// ������������� � ������� ���������
    /// </summary>
    function Attachment(const Value: TAttachmentArray): TVkMessageNew; overload;
    /// <summary>
    /// ������������� � ������� ���������
    /// </summary>
    function Attachment(const Value: TAttachment): TVkMessageNew; overload;
    /// <summary>
    /// ���� ����� ���������� ����������� ���������, ��������� �������.
    /// ����� ��������� ���������� �� ������� ��� �� �������� ����, ��� � �� ����������������.
    /// �� ������ ������ �������������� ���� ������ � ��������.
    /// �������� ��������, ��� � ����� ��������� ����� �������� ���� Template, ���� Keyboard. ���� ����� �������� ��������, ��� ����� �������� ���������� � ��������� ��� ���� ��������� � ���������� keyboard
    /// </summary>
    function Template(const Value: TVkMessageTemplate): TVkMessageNew;
    function Send: TVkMessageSendResponses;
    constructor Create(Controller: TMessagesController);
    property Handler: TVkHandler read FHandler;
    property Params: TParams read FParams write SetParams;
  end;

  TVkParamsMessageSend = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, �������� ������������ ���������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsMessageSend;
    /// <summary>
    /// ������������� ������, � ������� ����� ���������� ���������
    /// </summary>
    function ChatId(const Value: Integer): TVkParamsMessageSend;
    /// <summary>
    /// �������� ����� ������������ (��������, illarionov)
    /// </summary>
    function Domain(const Value: string): TVkParamsMessageSend;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsMessageSend;
    /// <summary>
    /// ����� � �������� int32 - ���������� (� �������� � API_ID � ID �����������) �������������, ��������������� ��� �������������� ��������� �������� ����������� ���������. ����������� ������ � ���������� � �������� � ������� ���������.
    /// ���������� � ������� random_id ������������ ��� �������� ������������, �������� � �������� ������� ��������� �� ��������� ��� (�� �� ����� 100 ��������� ���������).
    /// ���� �� �������� ��������, ����� ������������ ����� GetRandomId
    /// </summary>
    function RandomId(const Value: Integer = -1): TVkParamsMessageSend;
    /// <summary>
    /// ����� ������� ���������. ������������ ��������, ���� �� ����� �������� Attachment
    /// </summary>
    function Message(const Value: string): TVkParamsMessageSend;
    /// <summary>
    /// �������������� ����������
    /// </summary>
    function LatLong(const Lat, Long: Extended): TVkParamsMessageSend;
    /// <summary>
    /// ������������� � ������� ���������
    /// </summary>
    function Attachment(const Value: TAttachmentArray): TVkParamsMessageSend; overload;
    /// <summary>
    /// ������������� � ������� ���������
    /// </summary>
    function Attachment(const Value: TAttachment): TVkParamsMessageSend; overload;
    /// <summary>
    /// ������������� ���������, �� ������� ��������� ��������
    /// </summary>
    function ReplyTo(const Value: Integer): TVkParamsMessageSend;
    /// <summary>
    /// [�������� ������ ��������� ��� ���������]
    /// </summary>
    function &Forward(const Value: TVkMessageForward): TVkParamsMessageSend;
    /// <summary>
    /// �������������� ������������ ���������, ������������� ����� �������.
    /// ������������� ��������� ����������� ����� ������������ � ���� ������ � ����������.
    /// �� ����� 100 �������� �� ������� ������, ������������ ������� �����������: 45,
    /// ������������ ���������� ������������ ��������� 500
    /// </summary>
    function ForwardMessages(const Value: TIdList): TVkParamsMessageSend;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StickerId(const Value: Integer): TVkParamsMessageSend;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsMessageSend;
    /// <summary>
    /// ������, ����������� ���������� ����
    /// </summary>
    function Keyboard(const Value: TVkKeyboard): TVkParamsMessageSend;
    /// <summary>
    /// �������� ��������
    /// </summary>
    function Payload(const Value: string): TVkParamsMessageSend;
    /// <summary>
    /// ������ � ��� �����, ������� ���������� ��������������� ���������� ��������� �� ����������.
    /// ��������� � �������������� ��������� messages.send � Intent.
    /// </summary>
    function Intent(const Value: TVkMessageIntent): TVkParamsMessageSend;
    /// <summary>
    /// �� ��������� ������� ������ �� ���������
    /// </summary>
    function DontParseLinks(const Value: Boolean): TVkParamsMessageSend;
    /// <summary>
    /// ��������� ����������� �� ���������� � ���������
    /// </summary>
    function DisableMentions(const Value: Boolean): TVkParamsMessageSend;
    /// <summary>
    /// ������, ����������� �������� ����������������� �������� ��� ���-�����
    /// </summary>
    function ContentSource(const Value: TVkMessageContentSource): TVkParamsMessageSend;
    /// <summary>
    /// �����, ������� � ������� ����� ������������� ��� ������ � ���������
    /// </summary>
    function SubscribeId(const Value: Integer): TVkParamsMessageSend;
    /// <summary>
    /// ���� ����� ���������� ����������� ���������, ��������� �������.
    /// ����� ��������� ���������� �� ������� ��� �� �������� ����, ��� � �� ����������������.
    /// �� ������ ������ �������������� ���� ������ � ��������
    /// �������� ��������, ��� � ����� ��������� ����� �������� ���� Template, ���� Keyboard. ���� ����� �������� ��������, ��� ����� �������� ���������� � ��������� ��� ���� ��������� � ���������� keyboard
    /// </summary>
    function Template(const Value: TVkMessageTemplate): TVkParamsMessageSend;
  end;

  TVkParamsConversationsGet = record
    List: TParams;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �����������
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsConversationsGet;
    /// <summary>
    /// ������������ ����� �����������, ������� ����� ��������
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsConversationsGet;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsConversationsGet;
    /// <summary>
    /// ������
    /// </summary>
    function Filter(const Value: TVkConversationFilter = TVkConversationFilter.All): TVkParamsConversationsGet;
    /// <summary>
    /// ������ �������������� ����� ��� ������������� � ���������
    /// </summary>
    function Fields(const UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsConversationsGet;
    /// <summary>
    /// ���������� �������������� ���� ��� ������������� � ���������
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsConversationsGet;
    /// <summary>
    /// ������������� ���������, ������� � �������� ����� ���������� ������
    /// </summary>
    function StartMessageId(const Value: Integer): TVkParamsConversationsGet;
  end;

  TVkParamsConversationMembersGet = record
    List: TParams;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsConversationMembersGet;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �����������
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsConversationMembersGet;
    /// <summary>
    /// ������������ ����� �����������, ������� ����� ��������
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsConversationMembersGet;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsConversationMembersGet;
    /// <summary>
    /// ������ �������������� ����� ��� ������������� � ���������
    /// </summary>
    function Fields(const UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsConversationMembersGet;
    /// <summary>
    /// ���������� �������������� ���� ��� ������������� � ���������
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsConversationMembersGet;
  end;

  TVkParamsMessageDelete = record
    List: TParams;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsMessageDelete;
    /// <summary>
    /// ������������� ������, �� �������� ���������� ������� ��������� �� conversation_message_ids.
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsMessageDelete;
    /// <summary>
    /// ������ ��������������� ���������, ���������� ����� �������
    /// </summary>
    function MessageIds(const Value: TIdList): TVkParamsMessageDelete; overload;
    /// <summary>
    /// ������������� ���������
    /// </summary>
    function MessageId(const Value: Integer): TVkParamsMessageDelete; overload;
    /// <summary>
    /// ������ ��������������� ��������� ������, ���������� ����� �������
    /// </summary>
    function ConversationMessageIds(const Value: TIdList): TVkParamsMessageDelete; overload;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    function ConversationMessageId(const Value: Integer): TVkParamsMessageDelete; overload;
    /// <summary>
    /// �������� ��������� ��� ����
    /// </summary>
    function Spam(const Value: Boolean): TVkParamsMessageDelete;
    /// <summary>
    /// True � ���� ��������� ����� ������� ��� ����������� (���� � ������� �������� ��������� ������ �� ����� 24 �����)
    /// </summary>
    function DeleteForAll(const Value: Boolean): TVkParamsMessageDelete;
  end;

  TVkParamsMessageGet = record
    List: TParams;
    /// <summary>
    /// �������������� ���������. �������� 100 ���������������
    /// </summary>
    function MessageIds(const Value: TIdList): TVkParamsMessageGet;
    /// <summary>
    /// ������������� ���������
    /// </summary>
    function MessageId(const Value: Integer): TVkParamsMessageGet;
    /// <summary>
    /// ���������� ��������, �� �������� ����� �������� ���������.
    /// ������� 0, ���� �� �� ������ �������� ���������. (�� ��������� ��������� �� ����������)
    /// </summary>
    function PreviewLength(const Value: Integer = 0): TVkParamsMessageGet;
    /// <summary>
    /// True � ���������� �������������� ����
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsMessageGet;
    /// <summary>
    /// ������ �������������� ����� ��� ������������� � ���������
    /// </summary>
    function Fields(const Value: string): TVkParamsMessageGet;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Cardinal): TVkParamsMessageGet;
  end;

  TVkParamsMessageHistory = record
    List: TParams;
    /// <summary>
    /// ���������� ���������, ������� ���������� �������� (�� �� ����� 200)
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsMessageHistory;
    /// <summary>
    /// ���� ������� � �������� ����� ��������� True, �� ����� ����������
    /// ���������� � �������������, ���������� �������� ���������
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsMessageHistory;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: string): TVkParamsMessageHistory;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsMessageHistory;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ���������,
    /// ������ ���� >= 0, ���� �� ������� �������� start_message_id, � ������ ���� <= 0, ���� �������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsMessageHistory;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsMessageHistory;
    /// <summary>
    /// True � ���������� ��������� � ��������������� �������.
    /// False � ���������� ��������� � �������� ��������������� ������� (�� ���������)
    /// </summary>
    function Rev(const Value: Boolean = False): TVkParamsMessageHistory;
    /// <summary>
    /// E��� �������� > 0, �� ��� ������������� ���������, ������� � ��������
    /// ����� ������� ������� ���������, ���� �������� �������� 0 �� ��������
    /// ��������� � ������ ������ ���������, ���� �� �������� �������� -1, ��
    /// � �������� ��������� offset ������������ ���������� ��������
    /// ������������� ��������� � ����� ������� (����������� ��. ����)
    /// </summary>
    function StartMessageId(const Value: Integer): TVkParamsMessageHistory;
    /// <summary>
    /// ������������� ������������, ������� ��������� � ������� ���������� �������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsMessageHistory;
  end;

  TVkParamsMessageSendIds = record
    List: TParams;
    /// <summary>
    /// �������������� ����������� ��������� (��� ������������� ��������� ��������� ����� ���������� �������������).
    /// �������� ������ ��� ����� ������� ����������. ������������ ���������� ���������������: 100
    /// </summary>
    function PeerIds(const Value: TIdList): TVkParamsMessageSendIds;
    /// <summary>
    /// �������������� ����������� ��������� (��� ������������� ��������� ��������� ����� ���������� �������������).
    /// �������� ������ ��� ����� ������� ����������. ������������ ���������� ���������������: 100
    /// </summary>
    function UserIds(const Value: TIdList): TVkParamsMessageSendIds;
    /// <summary>
    /// ����� � �������� int32 - ���������� (� �������� � API_ID � ID �����������) �������������, ��������������� ��� �������������� ��������� �������� ����������� ���������. ����������� ������ � ���������� � �������� � ������� ���������.
    /// ���������� � ������� random_id ������������ ��� �������� ������������, �������� � �������� ������� ��������� �� ��������� ��� (�� �� ����� 100 ��������� ���������).
    /// ���� �� �������� ��������, ����� ������������ ����� GetRandomId
    /// </summary>
    function RandomId(const Value: Integer = -1): TVkParamsMessageSendIds;
    /// <summary>
    /// ����� ������� ���������. ������������ ��������, ���� �� ����� �������� Attachment
    /// </summary>
    function Message(const Value: string): TVkParamsMessageSendIds;
    /// <summary>
    /// �������������� ����������
    /// </summary>
    function LatLong(const Lat, Long: Extended): TVkParamsMessageSendIds;
    /// <summary>
    /// ������������� � ������� ���������
    /// </summary>
    function Attachment(const Value: TAttachmentArray): TVkParamsMessageSendIds; overload;
    /// <summary>
    /// ������������� � ������� ���������
    /// </summary>
    function Attachment(const Value: TAttachment): TVkParamsMessageSendIds; overload;
    /// <summary>
    /// ������������� ���������, �� ������� ��������� ��������
    /// </summary>
    function ReplyTo(const Value: Integer): TVkParamsMessageSendIds;
    /// <summary>
    /// [�������� ������ ��������� ��� ���������]
    /// </summary>
    function &Forward(const Value: TVkMessageForward): TVkParamsMessageSendIds;
    /// <summary>
    /// �������������� ������������ ���������, ������������� ����� �������.
    /// ������������� ��������� ����������� ����� ������������ � ���� ������ � ����������.
    /// �� ����� 100 �������� �� ������� ������, ������������ ������� �����������: 45,
    /// ������������ ���������� ������������ ��������� 500
    /// </summary>
    function ForwardMessages(const Value: TIdList): TVkParamsMessageSendIds;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StickerId(const Value: Integer): TVkParamsMessageSendIds;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsMessageSendIds;
    /// <summary>
    /// ������, ����������� ���������� ����
    /// </summary>
    function Keyboard(const Value: TVkKeyboard): TVkParamsMessageSendIds;
    /// <summary>
    /// �������� ��������
    /// </summary>
    function Payload(const Value: string): TVkParamsMessageSendIds;
    /// <summary>
    /// ������ � ��� �����, ������� ���������� ��������������� ���������� ��������� �� ����������.
    /// ��������� � �������������� ��������� messages.send � Intent.
    /// </summary>
    function Intent(const Value: TVkMessageIntent): TVkParamsMessageSendIds;
    /// <summary>
    /// �� ��������� ������� ������ �� ���������
    /// </summary>
    function DontParseLinks(const Value: Boolean): TVkParamsMessageSendIds;
    /// <summary>
    /// ��������� ����������� �� ���������� � ���������
    /// </summary>
    function DisableMentions(const Value: Boolean): TVkParamsMessageSendIds;
    /// <summary>
    /// ������, ����������� �������� ����������������� �������� ��� ���-�����
    /// </summary>
    function ContentSource(const Value: TVkMessageContentSource): TVkParamsMessageSendIds;
    /// <summary>
    /// �����, ������� � ������� ����� ������������� ��� ������ � ���������
    /// </summary>
    function SubscribeId(const Value: Integer): TVkParamsMessageSendIds;
    /// <summary>
    /// ���� ����� ���������� ����������� ���������, ��������� �������.
    /// ����� ��������� ���������� �� ������� ��� �� �������� ����, ��� � �� ����������������.
    /// �� ������ ������ �������������� ���� ������ � ��������
    /// �������� ��������, ��� � ����� ��������� ����� �������� ���� Template, ���� Keyboard. ���� ����� �������� ��������, ��� ����� �������� ���������� � ��������� ��� ���� ��������� � ���������� keyboard
    /// </summary>
    function Template(const Value: TVkMessageTemplate): TVkParamsMessageSendIds;
  end;

  TVkParamsMessageDeleteConversation = record
    List: TParams;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Cardinal): TVkParamsMessageDeleteConversation;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsMessageDeleteConversation;
    /// <summary>
    /// ������������� ������������. ���� ��������� �������� ������� ������, ����������� PeerId
    /// </summary>
    function UserId(const Value: Integer): TVkParamsMessageDeleteConversation;
  end;

  TVkParamsMessageEdit = record
    List: TParams;
    /// <summary>
    /// ������������� ���������
    /// </summary>
    function MessageId(const Value: Integer): TVkParamsMessageEdit;
    /// <summary>
    /// ������������� ��������� � ������
    /// </summary>
    function ConversationMessageId(const Value: Integer): TVkParamsMessageEdit;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsMessageEdit;
    /// <summary>
    /// ����� ���������. ������������ ��������, ���� �� ����� �������� Attachment
    /// </summary>
    function Message(const Value: string): TVkParamsMessageEdit;
    /// <summary>
    /// �������������� ����������
    /// </summary>
    function Lat(const Value: Extended): TVkParamsMessageEdit;
    /// <summary>
    /// �������������� ����������
    /// </summary>
    function Long(const Value: Extended): TVkParamsMessageEdit;
    /// <summary>
    /// �������������� ����������
    /// </summary>
    function LatLong(const Lat, Long: Extended): TVkParamsMessageEdit;
    /// <summary>
    /// �������������
    /// </summary>
    function Attachment(const Value: TAttachmentArray): TVkParamsMessageEdit; overload;
    /// <summary>
    /// �������������
    /// </summary>
    function Attachment(const Value: TAttachment): TVkParamsMessageEdit; overload;
    /// <summary>
    /// True, ����� ��������� ������������ ����������� ���������
    /// </summary>
    function KeepForwardMessages(const Value: Boolean): TVkParamsMessageEdit;
    /// <summary>
    /// True, ����� ��������� ������������ ������� ������ (��������)
    /// </summary>
    function KeepSnippets(const Value: Boolean): TVkParamsMessageEdit;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsMessageEdit;
    /// <summary>
    /// �� ��������� ������� ������ �� ���������
    /// </summary>
    function DontParseLinks(const Value: Boolean): TVkParamsMessageEdit;
    /// <summary>
    /// ���� ����� ���������� ����������� ���������, ��������� �������.
    /// ����� ��������� ���������� �� ������� ��� �� �������� ����, ��� � �� ����������������.
    /// �� ������ ������ �������������� ���� ������ � ��������.
    /// �������� ��������, ��� � ����� ��������� ����� �������� ���� Template, ���� Keyboard. ���� ����� �������� ��������, ��� ����� �������� ���������� � ��������� ��� ���� ��������� � ���������� keyboard
    /// </summary>
    function Template(const Value: TVkMessageTemplate): TVkParamsMessageEdit;
    /// <summary>
    /// ������, ����������� ���������� ����
    /// </summary>
    function Keyboard(const Value: TVkKeyboard): TVkParamsMessageEdit;
  end;

  TVkParamsMessageGetByConvMesId = record
    List: TParams;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsMessageGetByConvMesId;
    /// <summary>
    /// �������������� ���������. �������� 100 ���������������
    /// </summary>
    function ConversationMessageIds(const Value: TIdList): TVkParamsMessageGetByConvMesId; overload;
    /// <summary>
    /// �������������� ���������. �������� 100 ���������������
    /// </summary>
    function ConversationMessageIds(const Value: Integer): TVkParamsMessageGetByConvMesId; overload;
    /// <summary>
    /// True � ���������� �������������� ����
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsMessageGetByConvMesId;
    /// <summary>
    /// �������������� ���� ������������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsMessageGetByConvMesId;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Cardinal): TVkParamsMessageGetByConvMesId;
  end;

  TVkParamsMessageGetChat = record
    List: TParams;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function ChatId(const Value: Integer): TVkParamsMessageGetChat;
    /// <summary>
    /// ������ ��������������� �����
    /// </summary>
    function ChatIds(const Value: TIdList): TVkParamsMessageGetChat;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields = []): TVkParamsMessageGetChat;
    /// <summary>
    /// ����� ��� ��������� ����� � ������� ������������
    /// </summary>
    function NameCase(const Value: TVkNameCase = TVkNameCase.Nom): TVkParamsMessageGetChat;
  end;

  TVkParamsConversationsGetById = record
    List: TParams;
    /// <summary>
    /// �������������� ����������� ��������� (��� ������������� ��������� ��������� ����� ���������� �������������).
    /// �������� ������ ��� ����� ������� ����������. ������������ ���������� ���������������: 100
    /// </summary>
    function PeerIds(const Value: TIdList): TVkParamsConversationsGetById;
    /// <summary>
    /// True � ���������� �������������� ����
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsConversationsGetById;
    /// <summary>
    /// �������������� ���� ������������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsConversationsGetById;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Cardinal): TVkParamsConversationsGetById;
  end;

  TVkParamsGetHistoryAttachments = record
    List: TParams;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsGetHistoryAttachments;
    /// <summary>
    /// ��� ����������, ������� ���������� �������
    /// </summary>
    function MediaType(const Value: TVkHistoryAttachment = TVkHistoryAttachment.Photo): TVkParamsGetHistoryAttachments;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ��������
    /// </summary>
    function StartFrom(const Value: string): TVkParamsGetHistoryAttachments;
    /// <summary>
    /// ���������� ��������, ������� ���������� �������� (�� �� ����� 200)
    /// </summary>
    function Count(const Value: Integer = 30): TVkParamsGetHistoryAttachments;
    /// <summary>
    /// ��������, ����������� ����� �� ���������� �� ��������� ������� ���������� � ����������� �������
    /// </summary>
    function PhotoSizes(const Value: Boolean): TVkParamsGetHistoryAttachments;
    /// <summary>
    /// �������������� ���� �������� ������������� � ���������, ������� ���������� ������� � ������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsGetHistoryAttachments;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsGetHistoryAttachments;
    /// <summary>
    /// ��������, ����������� ����� �� ���������� �������� � ������������ �������
    /// </summary>
    function PreserveOrder(const Value: Boolean): TVkParamsGetHistoryAttachments;
    /// <summary>
    /// ������������ ������� ����������� ����������� ��������� (���� 45)
    /// </summary>
    function MaxForwardsLevel(const Value: Integer = 45): TVkParamsGetHistoryAttachments;
  end;

  TVkParamsGetImportantMessages = record
    List: TParams;
    /// <summary>
    /// ������������ ����� �����������, ������� ����� �������� (������������ �������� 200)
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsGetImportantMessages;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �����������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsGetImportantMessages;
    /// <summary>
    /// ������������� ���������, ������� � �������� ����� ���������� ������
    /// </summary>
    function StartMessageId(const Value: Integer): TVkParamsGetImportantMessages;
    /// <summary>
    /// ���������� ��������, �� �������� ����� �������� ���������.
    /// ������� 0, ���� �� �� ������ �������� ���������. (�� ��������� ��������� �� ����������)
    /// </summary>
    function PreviewLength(const Value: Integer): TVkParamsGetImportantMessages;
    /// <summary>
    /// ������ �������������� ����� ��� ������������� � ���������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsGetImportantMessages;
    /// <summary>
    /// True � ���������� �������������� ���� ��� ������������� � ���������
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsGetImportantMessages;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsGetImportantMessages;
  end;

  TVkParamsLongPollHistory = record
    List: TParams;
    /// <summary>
    /// ��������� �������� ��������� ts, ���������� �� Long Poll ������� ��� � ������� ������ messages.getLongPollServer
    /// </summary>
    function Ts(const Value: Integer): TVkParamsLongPollHistory;
    /// <summary>
    /// ��������� �������� ��������� new_pts, ���������� �� Long Poll �������, ������������ ��� ��������� ��������, ������� �������� ������
    /// </summary>
    function Pts(const Value: Integer): TVkParamsLongPollHistory;
    /// <summary>
    /// ���������� ��������, �� �������� ����� �������� ���������.
    /// ������� 0, ���� �� �� ������ �������� ���������. (�� ��������� ��������� �� ����������)
    /// </summary>
    function PreviewLength(const Value: Integer): TVkParamsLongPollHistory;
    /// <summary>
    /// True � ���������� � ����� ������ ������� 8 � 9 (������������ ���� ������/�������). ����������� ������ ��� ������������� ts
    /// </summary>
    function Onlines(const Value: Boolean): TVkParamsLongPollHistory;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields = [TVkProfileField.PhotoId, TVkProfileField.PhotoMedium, TVkProfileField.Sex, TVkProfileField.Online, TVkProfileField.ScreenName]): TVkParamsLongPollHistory;
    /// <summary>
    /// ����� �� ���������� ���� ������� � �������. �������� ��������, ��������� EventsLimit � MsgsLimit ����������� ���������.
    /// ����� ����������� � ������ �������������� ������ ����������� �������
    /// </summary>
    function EventsLimit(const Value: Integer = 1000): TVkParamsLongPollHistory;
    /// <summary>
    /// ����� �� ���������� ������� � ����������� � �������. �������� ��������, ��������� EventsLimit � MsgsLimit ����������� ���������.
    /// ����� ����������� � ������ �������������� ������ ����������� �������
    /// </summary>
    function MsgsLimit(const Value: Integer = 200): TVkParamsLongPollHistory;
    /// <summary>
    /// ������������ ������������� ��������� ����� ��� ��������� � ��������� �����.
    /// ���������� ��������� ��� ���������, ���������� ����� ������ API (�������� messages.getDialogs, messages.getHistory),
    /// ��� � ������, ���������� �� Long Poll ������� (������� � ����� 4)
    /// </summary>
    function MaxMsgId(const Value: Integer): TVkParamsLongPollHistory;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsLongPollHistory;
    /// <summary>
    /// ������ Long Poll
    /// </summary>
    function LpVersion(const Value: Integer): TVkParamsLongPollHistory;
    /// <summary>
    /// ������������� �����, �� ��������� 0, ������������ �������� 2000
    /// </summary>
    function LastN(const Value: Integer = 0): TVkParamsLongPollHistory;
    /// <summary>
    /// Credentials
    /// </summary>
    function Credentials(const Value: Boolean): TVkParamsLongPollHistory;
  end;

  TVkParamsGetLongPollServer = record
    List: TParams;
    /// <summary>
    /// True � ���������� ���� pts, ����������� ��� ������ ������ messages.getLongPollHistory
    /// </summary>
    function NeedPts(const Value: Boolean): TVkParamsGetLongPollServer;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsGetLongPollServer;
    /// <summary>
    /// ������ ��� ����������� � Long Poll. ���������� ������: 3 (04.03.2021)
    /// </summary>
    function LpVersion(const Value: Integer = 0): TVkParamsGetLongPollServer;
  end;

  TVkParamsMessageMarkAsRead = record
    List: TParams;
    /// <summary>
    /// �������������� ���������
    /// </summary>
    function MessageIds(const Value: TIdList): TVkParamsMessageMarkAsRead;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsMessageMarkAsRead;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsMessageMarkAsRead;
    /// <summary>
    /// ��� �������� ����� ��������� ����� �������� ��� ����������� ��� ���������, ������� � �������
    /// </summary>
    function StartMessageId(const Value: Integer): TVkParamsMessageMarkAsRead;
    /// <summary>
    /// �������� ��� ������ ��� �����������
    /// </summary>
    function MarkConversationAsRead(const Value: Boolean): TVkParamsMessageMarkAsRead;
  end;

  TVkParamsMessageRemoveChatUser = record
    List: TParams;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function ChatId(const Value: Integer): TVkParamsMessageRemoveChatUser;
    /// <summary>
    /// ������������� ������������, �������� ���������� ��������� �� ������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsMessageRemoveChatUser;
    /// <summary>
    /// ������������� ���������, �������� ���������� ��������� �� ������.
    /// ��� ��������� � ������������� ���������� �� ������ ������
    /// </summary>
    function MemberId(const Value: Integer): TVkParamsMessageRemoveChatUser;
  end;

  TVkParamsMessageSearch = record
    List: TParams;
    /// <summary>
    /// ���������, �� ������� ����� ������������� �����
    /// </summary>
    function Query(const Value: string): TVkParamsMessageSearch;
    /// <summary>
    /// ������ �� �������������� ���������� ��� ������ �� ���������� �������
    /// </summary>
    function PeerId(const Value: Integer): TVkParamsMessageSearch;
    /// <summary>
    /// ���� �������� �����, � ������ ����� ������ ���������, ������������ �� ��������� ����.
    /// </summary>
    function Date(const Value: TDateTime): TVkParamsMessageSearch;
    /// <summary>
    /// ���������� ��������, �� �������� ����� �������� ���������.
    /// ������� 0, ���� �� �� ������ �������� ���������. (�� ��������� ��������� �� ����������)
    /// </summary>
    function PreviewLength(const Value: Integer): TVkParamsMessageSearch;
    /// <summary>
    /// ���������� ���������, ������� ���������� ��������.
    /// �� ��������� 20, ������������ �������� 100
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsMessageSearch;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ��������� �� ������ ���������
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsMessageSearch;
    /// <summary>
    /// ������ �������������� ����� ��� ������������� � ���������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsMessageSearch;
    /// <summary>
    /// True � ���������� �������������� ���� ��� ������������� � ���������. � ������ ����� ����������� ������ �������� �����
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsMessageSearch;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsMessageSearch;
  end;

  TVkParamsMessageSearchConversations = record
    List: TParams;
    /// <summary>
    /// ��������� ������
    /// </summary>
    function Query(const Value: string): TVkParamsMessageSearchConversations;
    /// <summary>
    /// ������������ ����� ����������� ��� ��������� (���� 255)
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsMessageSearchConversations;
    /// <summary>
    /// �������������� ���� ������������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): TVkParamsMessageSearchConversations;
    /// <summary>
    /// True � ���������� �������������� ����
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsMessageSearchConversations;
    /// <summary>
    /// ������������� ���������� (��� ��������� ���������� � ������ ������� ������������)
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsMessageSearchConversations;
  end;

  TMessagesController = class(TVkController)
  public    /// <summary>
    /// ��������� ���������.
    /// </summary>
    function SendToPeer(var Item: Integer; PeerId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ���������.
    /// </summary>
    function SendToPeer(const PeerId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ��������� ������������
    /// </summary>
    function Send(var Item: Integer; UserId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ��������� ������������
    /// </summary>
    function Send(var Item: Integer; Domain: string; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ��������� � ������
    /// </summary>
    function SendToChat(var Item: Integer; ChatId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ��������� ���������� ������������� (�������� ������ ��� ����� ������� ����������)
    /// </summary>
    function Send(var Items: TVkMessageSendResponses; UserIds: TIdList; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ��������� ���������� ������������� (�������� ������ ��� ����� ������� ����������)
    /// </summary>
    function Send(var Items: TVkMessageSendResponses; Params: TVkParamsMessageSendIds): Boolean; overload;
    /// <summary>
    /// ��������� ��������� ���������� ������������� (�������� ������ ��� ����� ������� ����������)
    /// </summary>
    function Send(var Item: Integer; Params: TVkParamsMessageSend): Boolean; overload;
    /// <summary>
    /// ������������� ����� �������� ��������� (Fluent Method)
    /// New.PeerId(123456).ReplyTo(12345)...Message('�����').Send.Free;
    /// </summary>
    function New: TVkMessageNew; overload;
    /// <summary>
    /// ���������� ��������� �� �� ���������������.
    /// </summary>
    function GetById(var Items: TVkMessages; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ��������� �� �� ���������������.
    /// </summary>
    function GetById(var Items: TVkMessages; Params: TVkParamsMessageGet): Boolean; overload;
    /// <summary>
    /// ���������� ��������� �� �� ���������������.
    /// </summary>
    function GetById(var Items: TVkMessages; Ids: TIdList; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ��������� �� �� ���������������.
    /// </summary>
    function GetById(var Items: TVkMessages; Id: Integer; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ��������� � ������������ ������ ������������.
    /// </summary>
    function AddChatUser(const ChatId: Integer; UserId: Integer = -1; VisibleMessagesCount: Integer = 0): Boolean;
    /// <summary>
    /// ������� ���������
    /// </summary>
    function Delete(var Items: TVkMessageDelete; MessageIds: TIdList; GroupID: Integer = 0; DeleteForAll: Boolean = False; Spam: Boolean = False): Boolean; overload;
    /// <summary>
    /// ������� ���������
    /// </summary>
    function Delete(const MessageId: Integer; GroupID: Integer = 0; DeleteForAll: Boolean = False; Spam: Boolean = False): Boolean; overload;
    /// <summary>
    /// ������� ���������
    /// </summary>
    function Delete(var Items: TVkMessageDelete; Params: TVkParamsMessageDelete): Boolean; overload;
    /// <summary>
    /// ������� ���������
    /// </summary>
    function Delete(Params: TVkParamsMessageDelete): Boolean; overload;
    /// <summary>
    /// ������� ���������
    /// </summary>
    function DeleteInChat(const PeerId, MessageId: Integer; DeleteForAll: Boolean = False; Spam: Boolean = False): Boolean; overload;
    /// <summary>
    /// ������� ���������
    /// </summary>
    function Delete(var Items: TVkMessageDelete; Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ������ ��� ����������� ������������ � ������.
    /// ������ ��������� ������ ����� ������ � ������ �� ������.
    /// </summary>
    function GetInviteLink(var Link: string; PeerId: Integer; Reset: Boolean = False; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ���������� ������ ����� ������������.
    /// </summary>
    function GetConversations(var Items: TVkConversationItems): Boolean; overload;
    /// <summary>
    /// ���������� ������ ����� ������������.
    /// </summary>
    function GetConversations(var Items: TVkConversationItems; Params: TVkParamsConversationsGet): Boolean; overload;
    /// <summary>
    /// ���������� ������� ��������� ��� ���������� �������.
    /// </summary>
    function GetHistory(var Items: TVkMessageHistory; Params: TVkParamsMessageHistory): Boolean;
    /// <summary>
    /// ��������� ��������� �������� ��������� �� ���������� �������� ������������.
    /// </summary>
    function AllowMessagesFromGroup(const GroupId: Integer; Key: string): Boolean;
    /// <summary>
    /// ������ ������ � ����������� �����������.
    /// </summary>
    function CreateChat(var ChatId: Integer; UserIds: TIdList; Title: string; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ��������� ������� ���������� �������������.
    /// </summary>
    function DeleteChatPhoto(var Item: TVkChatInfoMessage; ChatId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ������.
    /// </summary>
    function DeleteConversation(var LastDeletedId: Integer; Params: TVkParamsMessageDeleteConversation): Boolean;
    /// <summary>
    /// ��������� ��������� �������� ��������� �� ���������� �������� ������������.
    /// </summary>
    function DenyMessagesFromGroup(const GroupId: Integer): Boolean;
    /// <summary>
    /// ����������� ���������.
    /// </summary>
    function Edit(const Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� ���������.
    /// </summary>
    function Edit(const Params: TVkParamsMessageEdit): Boolean; overload;
    /// <summary>
    /// �������� �������� ������.
    /// </summary>
    function EditChat(const ChatId: Integer; Title: string): Boolean;
    /// <summary>
    /// ���������� ��������� �� �� ��������������� � ������ ������.
    /// </summary>
    function GetByConversationMessageId(var Items: TVkMessages; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ��������� �� �� ��������������� � ������ ������.
    /// </summary>
    function GetByConversationMessageId(var Items: TVkMessages; Params: TVkParamsMessageGetByConvMesId): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ������.
    /// </summary>
    function GetChat(var Items: TVkChats; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ������.
    /// </summary>
    function GetChat(var Items: TVkChats; Params: TVkParamsMessageGetChat): Boolean; overload;
    /// <summary>
    /// �������� ������ ��� ������ ���� � ������������ �� ������.
    /// </summary>
    function GetChatPreview(var Item: TVkChatPreview; Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ������ ��� ������ ���� � ������������ �� ������.
    /// </summary>
    function GetChatPreview(var Item: TVkChatPreview; PeerId: Integer; Link: string; Fields: TVkProfileFields): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������ �� � ��������������.
    /// </summary>
    function GetConversationsById(var Items: TVkConversations; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������ �� � ��������������.
    /// </summary>
    function GetConversationMembers(var Items: TVkConversationMembers; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������ �� � ��������������.
    /// </summary>
    function GetConversationMembers(var Items: TVkConversationMembers; Params: TVkParamsConversationMembersGet): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������ ���������� ������.
    /// </summary>
    function GetConversationsById(var Items: TVkConversations; Params: TVkParamsConversationsGetById): Boolean; overload;
    /// <summary>
    /// ���������� ��������� ������� ��� ������.
    /// </summary>
    function GetHistoryAttachments(var Items: TVkAttachmentHistory; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ��������� ������� ��� ������.
    /// </summary>
    function GetHistoryAttachments(var Items: TVkAttachmentHistory; Params: TVkParamsGetHistoryAttachments): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������ ��������� ������������.
    /// </summary>
    function GetImportantMessages(var Items: TVkImportantMessages; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������ ��������� ������������.
    /// </summary>
    function GetImportantMessages(var Items: TVkImportantMessages; Params: TVkParamsGetImportantMessages): Boolean; overload;
    /// <summary>
    /// ���������� ������� ������ � ���� ��������� ���������� ���������� ������������.
    /// </summary>
    function GetLastActivity(var Item: TVkLastActivity; UserId: Integer): Boolean;
    /// <summary>
    /// ���������� ���������� � ������ ���������� ������������.
    /// </summary>
    function GetLongPollHistory(var Item: TVkLongPollHistory; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ������ ���������� ������������.
    /// </summary>
    function GetLongPollHistory(var Item: TVkLongPollHistory; Params: TVkParamsLongPollHistory): Boolean; overload;
    /// <summary>
    /// ���������� ������, ����������� ��� ����������� � Long Poll �������.
    /// <seealso>https://vk.com/dev/using_longpoll</seealso>
    /// </summary>
    function GetLongPollServer(var Item: TVkLongpollData; Params: TVkParamsGetLongPollServer): Boolean;
    /// <summary>
    /// ���������� ���������� � ���, ��������� �� �������� ��������� �� ���������� ������������.
    /// </summary>
    function IsMessagesFromGroupAllowed(var IsAllowed: Boolean; GroupId, UserId: Integer): Boolean;
    /// <summary>
    /// ��������� �������������� � ���� �� ������-�����������.
    /// </summary>
    function JoinChatByInviteLink(var ChatId: Integer; const Link: string): Boolean;
    /// <summary>
    /// �������� ������ ��� ���������� ���� ������� �������.
    /// </summary>
    function MarkAsAnsweredConversation(const PeerId: Integer; Answered: Boolean; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// �������� ��������� ��� ������ ���� ������� �������.
    /// </summary>
    function MarkAsImportant(var Items: TIdList; MessageIds: TIdList; Important: Boolean): Boolean; overload;
    /// <summary>
    /// �������� ��������� ��� ������ ���� ������� �������.
    /// </summary>
    function MarkAsImportant(var Item: Integer; MessageId: Integer; Important: Boolean): Boolean; overload;
    /// <summary>
    /// �������� ������ ��� ������ ���� ������� �������.
    /// </summary>
    function MarkAsImportantConversation(const PeerId: Integer; Important: Boolean; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// �������� ��������� ��� �����������.
    /// </summary>
    function MarkAsRead(const Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ��������� ��� �����������.
    /// </summary>
    function MarkAsRead(const Params: TVkParamsMessageMarkAsRead): Boolean; overload;
    /// <summary>
    /// �������� ��������� ��� �������������.
    /// </summary>
    function MarkAsUnreadConversation(const PeerId: Integer; MessageIds: TIdList = []): Boolean;
    /// <summary>
    /// ���������� ���������.
    /// </summary>
    function Pin(var Message: TVkMessage; PeerId, MessageId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ���������.
    /// </summary>
    function Pin(const PeerId, MessageId: Integer): Boolean; overload;
    /// <summary>
    /// ��������� �� ������������� ������������, ���� ������� ������������ ��� ���������� �������� ��������������� ������ ���� ������� ������������ ��������� ������������ ������������.
    /// </summary>
    function RemoveChatUser(const Params: TVkParamsMessageRemoveChatUser): Boolean;
    /// <summary>
    /// ��������������� ��������� ���������.
    /// </summary>
    function Restore(const MessageId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ���������� ������ ��������� ������ ��������� �������� ������������ �� ��������� ������ ������.
    /// </summary>
    function Search(var Items: TVkMessageHistory; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������� ������ ��������� �������� ������������ �� ��������� ������ ������.
    /// </summary>
    function Search(var Items: TVkMessageHistory; Params: TVkParamsMessageSearch): Boolean; overload;
    /// <summary>
    /// ��������� ������ �������.
    /// </summary>
    function SearchConversations(var Items: TVkConversations; Params: TVkParamsMessageSearchConversations): Boolean;
    /// <summary>
    /// ���������� ������� � ���������, ������� ���������� ��� ������� �� callback-������.
    /// </summary>
    function SendMessageEventAnswer(const EventId: string; UserId, PeerId: Integer; EventData: string): Boolean;
    /// <summary>
    /// �������� ������ ������ ������ ������������� � �������.
    /// ����� �N �������� ���������...� ������������ � ������� 10 ������ ����� ������ ������, ���� �� ������� �������� ���������.
    /// </summary>
    function SetActivity(ActivityType: TVkMessageActivity; PeerId: Integer = 0; const UserId: string = ''; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ��������� ���������� ���������� �������������, ����������� � ������� ������ Photos.GetChatUploadServer.
    /// <b>UploadFile</b> - ���������� ���� Response �� ������ ������������ upload �������, ����������� � ���������� �������� ����������� �� �����, ���������� ������� Photos.GetChatUploadServer.
    /// </summary>
    function SetChatPhoto(var Info: TVkChatInfoMessage; UploadFile: string): Boolean;
    /// <summary>
    /// ���������� ���������.
    /// </summary>
    function Unpin(const PeerId: Integer; GroupId: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TMessagesController }

function TMessagesController.SendToPeer(var Item: Integer; PeerId: Integer; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.PeerId(PeerId);
  Params.Message(Message);
  Params.RandomId(GetRandomId);

  if not Attachments.IsEmpty then
    Params.Attachment(Attachments);

  Result := Send(Item, Params);
end;

function TMessagesController.AddChatUser(const ChatId: Integer; UserId: Integer; VisibleMessagesCount: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('chat_id', ChatId);
  if UserId >= 0 then
    Params.Add('user_id', UserId);
  if VisibleMessagesCount > 0 then
    Params.Add('visible_messages_count', VisibleMessagesCount);
  Result := Handler.Execute('messages.addChatUser', Params).ResponseIsTrue;
end;

function TMessagesController.Delete(var Items: TVkMessageDelete; MessageIds: TIdList; GroupID: Integer; DeleteForAll, Spam: Boolean): Boolean;
var
  Params: TVkParamsMessageDelete;
begin
  Params.MessageIds(MessageIds);
  if DeleteForAll then
    Params.DeleteForAll(DeleteForAll);
  if Spam then
    Params.Spam(Spam);
  if GroupID <> 0 then
    Params.GroupId(GroupID);
  Result := Delete(Items, Params.List);
end;

function TMessagesController.Delete(const MessageId: Integer; GroupID: Integer; DeleteForAll, Spam: Boolean): Boolean;
var
  Items: TVkMessageDelete;
begin
  Result := Delete(Items, [MessageId], GroupID, DeleteForAll, Spam) and Items.Items[MessageId.ToString];
  if Result then
    Items.Free;
end;

function TMessagesController.DeleteInChat(const PeerId, MessageId: Integer; DeleteForAll, Spam: Boolean): Boolean;
var
  Params: TVkParamsMessageDelete;
  Items: TVkMessageDelete;
begin
  Params.ConversationMessageId(MessageId);
  Params.PeerId(PeerId);
  if DeleteForAll then
    Params.DeleteForAll(DeleteForAll);
  if Spam then
    Params.Spam(Spam);
  Result := Delete(Items, Params.List);
  if Result then
    Items.Free;
end;

function TMessagesController.AllowMessagesFromGroup(const GroupId: Integer; Key: string): Boolean;
begin
  Result := Handler.Execute('messages.allowMessagesFromGroup', [
    ['group_id', GroupId.ToString],
    ['key', Key]]).
    ResponseIsTrue;
end;

function TMessagesController.CreateChat(var ChatId: Integer; UserIds: TIdList; Title: string; GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('messages.createChat', [
    ['user_ids', UserIds.ToString],
    ['title', Title],
    ['group_id', GroupId.ToString]]).
    ResponseAsInt(ChatId);
end;

function TMessagesController.Delete(var Items: TVkMessageDelete; Params: TParams): Boolean;
var
  RespJSON: TJSONValue;
  Id: string;
begin
  Result := False;
  with Handler.Execute('messages.delete', Params) do
  begin
    if GetValue(RespJSON) then
    begin
      try
        Items := TVkMessageDelete.Create;
        try
          for Id in Params.GetValue('message_ids').Split([',']) do
            Items.Items.Add(Id, RespJSON.GetValue(Id, 0) = 1);
          Result := True;
        except
          Items.Free;
          Result := False;
        end;
      finally
        RespJSON.Free;
      end;
    end;
  end;
end;

function TMessagesController.DeleteChatPhoto(var Item: TVkChatInfoMessage; ChatId, GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('chat_id', ChatId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('messages.deleteChatPhoto', Params).GetObject(Item);
end;

function TMessagesController.DeleteConversation(var LastDeletedId: Integer; Params: TVkParamsMessageDeleteConversation): Boolean;
begin
  Result := Handler.Execute('messages.deleteConversation', Params.List).GetValue('last_deleted_id', LastDeletedId);
end;

function TMessagesController.DenyMessagesFromGroup(const GroupId: Integer): Boolean;
begin
  Result := Handler.Execute('messages.denyMessagesFromGroup', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TMessagesController.EditChat(const ChatId: Integer; Title: string): Boolean;
begin
  Result := Handler.Execute('messages.editChat', [['chat_id', ChatId.ToString], ['title', Title]]).ResponseIsTrue;
end;

function TMessagesController.Edit(const Params: TVkParamsMessageEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TMessagesController.Edit(const Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.edit', Params).ResponseIsTrue;
end;

function TMessagesController.Delete(var Items: TVkMessageDelete; Params: TVkParamsMessageDelete): Boolean;
begin
  Result := Delete(Items, Params.List);
end;

function TMessagesController.Delete(Params: TVkParamsMessageDelete): Boolean;
var
  Items: TVkMessageDelete;
begin
  Result := Delete(Items, Params.List);
  if Result then
    Items.Free;
end;

function TMessagesController.GetByConversationMessageId(var Items: TVkMessages; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getByConversationMessageId', Params).GetObject(Items);
end;

function TMessagesController.GetByConversationMessageId(var Items: TVkMessages; Params: TVkParamsMessageGetByConvMesId): Boolean;
begin
  Result := GetByConversationMessageId(Items, Params.List);
end;

function TMessagesController.GetById(var Items: TVkMessages; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getById', Params).GetObject(Items);
end;

function TMessagesController.GetById(var Items: TVkMessages; Ids: TIdList; PreviewLength, GroupId: Integer): Boolean;
var
  Params: TVkParamsMessageGet;
begin
  Params.MessageIds(Ids);
  if PreviewLength > 0 then
    Params.PreviewLength(PreviewLength);
  if GroupId > 0 then
    Params.GroupId(GroupId);
  Result := GetById(Items, Params);
end;

function TMessagesController.GetById(var Items: TVkMessages; Id, PreviewLength, GroupId: Integer): Boolean;
begin
  Result := GetById(Items, [Id], PreviewLength, GroupId);
end;

function TMessagesController.GetById(var Items: TVkMessages; Params: TVkParamsMessageGet): Boolean;
begin
  Result := GetById(Items, Params.List);
end;

function TMessagesController.GetChat(var Items: TVkChats; Params: TParams): Boolean;
begin
  if not Params.KeyExists('fields') then
    Params.Add('fields', TVkProfileField.Domain.ToString);
  Result := Handler.Execute('messages.getChat', Params).GetObjects(Items);
end;

function TMessagesController.GetChat(var Items: TVkChats; Params: TVkParamsMessageGetChat): Boolean;
begin
  Result := GetChat(Items, Params.List);
end;

function TMessagesController.GetChatPreview(var Item: TVkChatPreview; PeerId: Integer; Link: string; Fields: TVkProfileFields): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  Params.Add('link', Link);
  if Fields <> [] then
    Params.Add('fields', Fields.ToString);
  Result := GetChatPreview(Item, Params);
end;

function TMessagesController.GetChatPreview(var Item: TVkChatPreview; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getChatPreview', Params).GetObject(Item);
end;

function TMessagesController.GetConversationMembers(var Items: TVkConversationMembers; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getConversationMembers', Params).GetObject(Items);
end;

function TMessagesController.GetConversationMembers(var Items: TVkConversationMembers; Params: TVkParamsConversationMembersGet): Boolean;
begin
  Result := GetConversationMembers(Items, Params.List);
end;

function TMessagesController.GetConversations(var Items: TVkConversationItems; Params: TVkParamsConversationsGet): Boolean;
begin
  Result := Handler.Execute('messages.getConversations', Params.List).GetObject(Items);
end;

function TMessagesController.GetConversations(var Items: TVkConversationItems): Boolean;
begin
  Result := Handler.Execute('messages.getConversations').GetObject(Items);
end;

function TMessagesController.GetConversationsById(var Items: TVkConversations; Params: TVkParamsConversationsGetById): Boolean;
begin
  Result := GetConversationsById(Items, Params.List);
end;

function TMessagesController.GetConversationsById(var Items: TVkConversations; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getConversationsById', Params).GetObject(Items);
end;

function TMessagesController.GetHistory(var Items: TVkMessageHistory; Params: TVkParamsMessageHistory): Boolean;
begin
  Result := Handler.Execute('messages.getHistory', Params.List).GetObject(Items);
end;

function TMessagesController.GetHistoryAttachments(var Items: TVkAttachmentHistory; Params: TVkParamsGetHistoryAttachments): Boolean;
begin
  Result := GetHistoryAttachments(Items, Params.List);
end;

function TMessagesController.GetHistoryAttachments(var Items: TVkAttachmentHistory; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getHistoryAttachments', Params).GetObject(Items);
end;

function TMessagesController.GetImportantMessages(var Items: TVkImportantMessages; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getImportantMessages', Params).GetObject(Items);
end;

function TMessagesController.GetImportantMessages(var Items: TVkImportantMessages; Params: TVkParamsGetImportantMessages): Boolean;
begin
  Result := GetImportantMessages(Items, Params.List);
end;

function TMessagesController.GetInviteLink(var Link: string; PeerId: Integer; Reset: Boolean; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  if Reset then
    Params.Add('reset', Reset);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('messages.getInviteLink', Params).GetValue('link', Link);
end;

function TMessagesController.GetLastActivity(var Item: TVkLastActivity; UserId: Integer): Boolean;
begin
  Result := Handler.Execute('messages.getLastActivity', ['user_id', UserId.ToString]).GetObject(Item);
end;

function TMessagesController.GetLongPollHistory(var Item: TVkLongPollHistory; Params: TVkParamsLongPollHistory): Boolean;
begin
  Result := GetLongPollHistory(Item, Params.List);
end;

function TMessagesController.GetLongPollServer(var Item: TVkLongpollData; Params: TVkParamsGetLongPollServer): Boolean;
begin
  Result := Handler.Execute('messages.getLongPollServer', Params.List).GetObject(Item);
end;

function TMessagesController.IsMessagesFromGroupAllowed(var IsAllowed: Boolean; GroupId, UserId: Integer): Boolean;
begin
  Result := Handler.Execute('messages.isMessagesFromGroupAllowed', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString]]).
    GetValue('is_allowed', IsAllowed);
end;

function TMessagesController.JoinChatByInviteLink(var ChatId: Integer; const Link: string): Boolean;
begin
  Result := Handler.Execute('messages.joinChatByInviteLink', ['link', Link]).GetValue('chat_id', ChatId);
end;

function TMessagesController.MarkAsAnsweredConversation(const PeerId: Integer; Answered: Boolean; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  Params.Add('answered', Answered);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('messages.markAsAnsweredConversation', Params).ResponseIsTrue;
end;

function TMessagesController.MarkAsImportant(var Items: TIdList; MessageIds: TIdList; Important: Boolean): Boolean;
var
  Resp: TVkIdList;
begin
  with Handler.Execute('messages.markAsImportant', [['message_ids', MessageIds.ToString], ['important', BoolToString(Important)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Resp := TVkIdList.FromJsonString<TVkIdList>(ResponseAsItems);
        try
          Items := Resp.Items;
        finally
          Resp.Free;
        end;
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.MarkAsImportant(var Item: Integer; MessageId: Integer; Important: Boolean): Boolean;
var
  Resp: TVkIdList;
begin
  with Handler.Execute('messages.markAsImportant', [['message_ids', MessageId.ToString], ['important', BoolToString(Important)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Resp := TVkIdList.FromJsonString<TVkIdList>(ResponseAsItems);
        try
          if Length(Resp.Items) > 0 then
            Item := Resp.Items[0]
          else
            Item := -1;
        finally
          Resp.Free;
        end;
      except
        Result := False;
      end;
    end;
  end;
end;

function TMessagesController.MarkAsImportantConversation(const PeerId: Integer; Important: Boolean; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  Params.Add('important', Important);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('messages.markAsImportantConversation', Params).ResponseIsTrue;
end;

function TMessagesController.MarkAsRead(const Params: TVkParamsMessageMarkAsRead): Boolean;
begin
  Result := MarkAsRead(Params.List);
end;

function TMessagesController.MarkAsUnreadConversation(const PeerId: Integer; MessageIds: TIdList): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  if not MessageIds.IsEmpty then
    Params.Add('message_ids', MessageIds);
  Result := Handler.Execute('messages.markAsUnreadConversation', Params).ResponseIsTrue;
end;

function TMessagesController.MarkAsRead(const Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.markAsRead', Params).ResponseIsTrue;
end;

function TMessagesController.GetLongPollHistory(var Item: TVkLongPollHistory; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getLongPollHistory', Params).GetObject(Item);
end;

function TMessagesController.New: TVkMessageNew;
begin
  Result := TVkMessageNew.Create(Self);
end;

function TMessagesController.Pin(const PeerId, MessageId: Integer): Boolean;
begin
  Result := Handler.Execute('messages.pin', [['peer_id', PeerId.ToString], ['message_id', MessageId.ToString]]).Success;
end;

function TMessagesController.RemoveChatUser(const Params: TVkParamsMessageRemoveChatUser): Boolean;
begin
  Result := Handler.Execute('messages.removeChatUser', Params.List).ResponseIsTrue;
end;

function TMessagesController.Restore(const MessageId: Integer; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('message_id', MessageId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('messages.restore', Params).ResponseIsTrue;
end;

function TMessagesController.Pin(var Message: TVkMessage; PeerId, MessageId: Integer): Boolean;
begin
  Result := Handler.Execute('messages.pin', [
    ['peer_id', PeerId.ToString],
    ['message_id', MessageId.ToString]]).
    GetObject(Message);
end;

function TMessagesController.Search(var Items: TVkMessageHistory; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.search', Params).GetObject(Items);
end;

function TMessagesController.Search(var Items: TVkMessageHistory; Params: TVkParamsMessageSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TMessagesController.SearchConversations(var Items: TVkConversations; Params: TVkParamsMessageSearchConversations): Boolean;
begin
  Result := Handler.Execute('messages.searchConversations', Params.List).GetObject(Items);
end;

function TMessagesController.Send(var Item: Integer; Params: TVkParamsMessageSend): Boolean;
begin
  Result := Handler.Execute('messages.send', Params.List).ResponseAsInt(Item);
end;

function TMessagesController.SendMessageEventAnswer(const EventId: string; UserId, PeerId: Integer; EventData: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('event_id', EventId);
  Params.Add('user_id', UserId);
  Params.Add('peer_id', PeerId);
  if not EventData.IsEmpty then
    Params.Add('event_data', EventData);
  Result := Handler.Execute('messages.sendMessageEventAnswer', Params).ResponseIsTrue;
end;

function TMessagesController.Send(var Items: TVkMessageSendResponses; Params: TVkParamsMessageSendIds): Boolean;
begin
  Result := Handler.Execute('messages.send', Params.List).GetObject(Items);
end;

function TMessagesController.SendToChat(var Item: Integer; ChatId: Integer; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.ChatId(ChatId);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if not Attachments.IsEmpty then
    Params.Attachment(Attachments);

  Result := Send(Item, Params);
end;

function TMessagesController.SendToPeer(const PeerId: Integer; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Id: Integer;
begin
  Result := SendToPeer(Id, PeerId, Message, Attachments);
end;

function TMessagesController.SetActivity(ActivityType: TVkMessageActivity; PeerId: Integer; const UserId: string; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  if not UserId.IsEmpty then
    Params.Add('user_id', UserId);
  if PeerId <> 0 then
    Params.Add('peer_id', PeerId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Params.Add('type', ActivityType.ToString);
  Result := Handler.Execute('messages.setActivity', Params).ResponseIsTrue;
end;

function TMessagesController.SetChatPhoto(var Info: TVkChatInfoMessage; UploadFile: string): Boolean;
begin
  Result := Handler.Execute('messages.setChatPhoto', ['file', UploadFile]).GetObject(Info);
end;

function TMessagesController.Unpin(const PeerId: Integer; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  Params.Add('group_id', GroupId);
  Result := Handler.Execute('messages.unpin', Params).ResponseIsTrue;
end;

function TMessagesController.Send(var Item: Integer; Domain: string; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.Domain(Domain);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if not Attachments.IsEmpty then
    Params.Attachment(Attachments);

  Result := Send(Item, Params);
end;

function TMessagesController.Send(var Items: TVkMessageSendResponses; UserIds: TIdList; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSendIds;
begin
  Params.UserIds(UserIds);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Attachment(Attachments);

  Result := Send(Items, Params);
end;

function TMessagesController.Send(var Item: Integer; UserId: Integer; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.UserId(UserId);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if Length(Attachments) <> 0 then
    Params.Attachment(Attachments);

  Result := Send(Item, Params);
end;

{ TVkMessageNew }

constructor TVkMessageNew.Create(Controller: TMessagesController);
begin
  FHandler := Controller.Handler;
end;

function TVkMessageNew.Send: TVkMessageSendResponses;
var
  Value: Integer;
begin
  FParams.Add('random_id', GetRandomId);
  with Handler.Execute('messages.send', Params) do
  begin
    if not Success then
      Result := TVkMessageSendResponses.CreateFalse
    else
    begin
      if ResponseAsInt(Value) then
        Result := TVkMessageSendResponses.CreateTrue(Value)
      else
        GetObject(Result);
    end;
  end;
  {$IFNDEF AUTOREFCOUNT}
  Free;
  {$ENDIF}
end;

procedure TVkMessageNew.SetParams(const Value: TParams);
begin
  FParams := Value;
end;

function TVkMessageNew.DisableMentions(const Value: Boolean): TVkMessageNew;
begin
  Params.Add('disable_mentions', Value);
  Result := Self;
end;

function TVkMessageNew.DontParseLinks(const Value: Boolean): TVkMessageNew;
begin
  Params.Add('dont_parse_links', Value);
  Result := Self;
end;

function TVkMessageNew.StickerId(const Value: Integer): TVkMessageNew;
begin
  Params.Add('sticker_id', Value);
  Result := Self;
end;

function TVkMessageNew.SubscribeId(const Value: Integer): TVkMessageNew;
begin
  Params.Add('subscribe_id', Value);
  Result := Self;
end;

function TVkMessageNew.Template(const Value: TVkMessageTemplate): TVkMessageNew;
begin
  Params.Add('template', Value);
  Result := Self;
end;

function TVkMessageNew.&Forward(const Value: TVkMessageForward): TVkMessageNew;
begin
  Params.Add('forward', Value.ToJSON);
  Result := Self;
end;

function TVkMessageNew.ForwardMessages(const Value: TIdList): TVkMessageNew;
begin
  Params.Add('forward_messages', Value);
  Result := Self;
end;

function TVkMessageNew.GroupID(const Value: Integer): TVkMessageNew;
begin
  Params.Add('group_id', Value);
  Result := Self;
end;

function TVkMessageNew.Intent(const Value: TVkMessageIntent): TVkMessageNew;
begin
  Params.Add('intent', Value.ToString);
  Result := Self;
end;

function TVkMessageNew.Keyboard(const Value: string): TVkMessageNew;
begin
  Params.Add('keyboard', Value);
  Result := Self;
end;

function TVkMessageNew.Keyboard(const Value: TVkKeyboard): TVkMessageNew;
begin
  Params.Add('keyboard', Value.ToJsonString);
  Result := Self;
end;

function TVkMessageNew.LatLong(const Lat, Long: Extended): TVkMessageNew;
begin
  Params.Add('lat', Lat);
  Params.Add('long', Long);
  Result := Self;
end;

function TVkMessageNew.Attachment(const Value: TAttachmentArray): TVkMessageNew;
begin
  Params.Add('attachment', Value);
  Result := Self;
end;

function TVkMessageNew.Attachment(const Value: TAttachment): TVkMessageNew;
begin
  Params.Add('attachment', Value);
  Result := Self;
end;

function TVkMessageNew.ChatId(const Value: Integer): TVkMessageNew;
begin
  Params.Add('chat_id', Value);
  Result := Self;
end;

function TVkMessageNew.ContentSource(const Value: TVkMessageContentSource): TVkMessageNew;
begin
  Params.Add('content_source', Value.ToJSON);
  Result := Self;
end;

function TVkMessageNew.Message(const Value: string): TVkMessageNew;
begin
  Params.Add('message', Value);
  Result := Self;
end;

function TVkMessageNew.Payload(const Value: string): TVkMessageNew;
begin
  Params.Add('payload', Value);
  Result := Self;
end;

function TVkMessageNew.PeerId(const Value: Integer): TVkMessageNew;
begin
  Params.Add('peer_id', Value);
  Result := Self;
end;

function TVkMessageNew.ReplyTo(const Value: Integer): TVkMessageNew;
begin
  Params.Add('reply_to', Value);
  Result := Self;
end;

function TVkMessageNew.Domain(const Value: string): TVkMessageNew;
begin
  Params.Add('domain', Value);
  Result := Self;
end;

function TVkMessageNew.UserId(const Value: Integer): TVkMessageNew;
begin
  Params.Add('user_id', Value);
  Result := Self;
end;

function TVkMessageNew.PeerIds(const Value: TIdList): TVkMessageNew;
begin
  Params.Add('peer_ids', Value);
  Result := Self;
end;

{ TVkParamsConversationsGet }

function TVkParamsConversationsGet.Count(const Value: Integer): TVkParamsConversationsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsConversationsGet.Extended(const Value: Boolean): TVkParamsConversationsGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsConversationsGet.Fields(const UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): TVkParamsConversationsGet;
begin
  List.Add('fields', [UserFields.ToString, GroupFields.ToString]);
  Result := Self;
end;

function TVkParamsConversationsGet.Filter(const Value: TVkConversationFilter): TVkParamsConversationsGet;
begin
  List.Add('filter', Value.ToString);
  Result := Self;
end;

function TVkParamsConversationsGet.GroupID(const Value: Integer): TVkParamsConversationsGet;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsConversationsGet.Offset(const Value: Integer): TVkParamsConversationsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsConversationsGet.StartMessageId(const Value: Integer): TVkParamsConversationsGet;
begin
  List.Add('start_message_id', Value);
  Result := Self;
end;

{ TVkParamsMessageHistory }

function TVkParamsMessageHistory.Count(const Value: Integer): TVkParamsMessageHistory;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsMessageHistory.Extended(const Value: Boolean): TVkParamsMessageHistory;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsMessageHistory.Fields(const Value: string): TVkParamsMessageHistory;
begin
  List.Add('fields', Value);
  Result := Self;
end;

function TVkParamsMessageHistory.GroupID(const Value: Integer): TVkParamsMessageHistory;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageHistory.Offset(const Value: Integer): TVkParamsMessageHistory;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsMessageHistory.PeerId(const Value: Integer): TVkParamsMessageHistory;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

function TVkParamsMessageHistory.Rev(const Value: Boolean): TVkParamsMessageHistory;
begin
  List.Add('rev', Value);
  Result := Self;
end;

function TVkParamsMessageHistory.StartMessageId(const Value: Integer): TVkParamsMessageHistory;
begin
  List.Add('start_message_id', Value);
  Result := Self;
end;

function TVkParamsMessageHistory.UserId(const Value: Integer): TVkParamsMessageHistory;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsMessageGet }

function TVkParamsMessageGet.Extended(const Value: Boolean): TVkParamsMessageGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsMessageGet.Fields(const Value: string): TVkParamsMessageGet;
begin
  List.Add('fields', Value);
  Result := Self;
end;

function TVkParamsMessageGet.GroupID(const Value: Cardinal): TVkParamsMessageGet;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageGet.MessageId(const Value: Integer): TVkParamsMessageGet;
begin
  List.Add('message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageGet.MessageIds(const Value: TIdList): TVkParamsMessageGet;
begin
  List.Add('message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageGet.PreviewLength(const Value: Integer): TVkParamsMessageGet;
begin
  List.Add('preview_length', Value);
  Result := Self;
end;

{ TVkParamsMessageDelete }

function TVkParamsMessageDelete.ConversationMessageId(const Value: Integer): TVkParamsMessageDelete;
begin
  List.Add('conversation_message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageDelete.ConversationMessageIds(const Value: TIdList): TVkParamsMessageDelete;
begin
  List.Add('conversation_message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageDelete.DeleteForAll(const Value: Boolean): TVkParamsMessageDelete;
begin
  List.Add('delete_for_all', Value);
  Result := Self;
end;

function TVkParamsMessageDelete.GroupID(const Value: Integer): TVkParamsMessageDelete;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageDelete.MessageId(const Value: Integer): TVkParamsMessageDelete;
begin
  List.Add('message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageDelete.MessageIds(const Value: TIdList): TVkParamsMessageDelete;
begin
  List.Add('message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageDelete.PeerId(const Value: Integer): TVkParamsMessageDelete;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

function TVkParamsMessageDelete.Spam(const Value: Boolean): TVkParamsMessageDelete;
begin
  List.Add('spam', Value);
  Result := Self;
end;

{ TVkParamsMessageSend }

function TVkParamsMessageSend.Attachment(const Value: TAttachmentArray): TVkParamsMessageSend;
begin
  List.Add('attachment', Value);
  Result := Self;
end;

function TVkParamsMessageSend.Attachment(const Value: TAttachment): TVkParamsMessageSend;
begin
  List.Add('attachment', Value);
  Result := Self;
end;

function TVkParamsMessageSend.ChatId(const Value: Integer): TVkParamsMessageSend;
begin
  List.Add('chat_id', Value);
  Result := Self;
end;

function TVkParamsMessageSend.ContentSource(const Value: TVkMessageContentSource): TVkParamsMessageSend;
begin
  List.Add('content_source', Value.ToJSON);
  Result := Self;
end;

function TVkParamsMessageSend.DisableMentions(const Value: Boolean): TVkParamsMessageSend;
begin
  List.Add('disable_mentions', Value);
  Result := Self;
end;

function TVkParamsMessageSend.Domain(const Value: string): TVkParamsMessageSend;
begin
  List.Add('domain', Value);
  Result := Self;
end;

function TVkParamsMessageSend.DontParseLinks(const Value: Boolean): TVkParamsMessageSend;
begin
  List.Add('dont_parse_links', Value);
  Result := Self;
end;

function TVkParamsMessageSend.&Forward(const Value: TVkMessageForward): TVkParamsMessageSend;
begin
  List.Add('forward', Value.ToJSON);
  Result := Self;
end;

function TVkParamsMessageSend.ForwardMessages(const Value: TIdList): TVkParamsMessageSend;
begin
  List.Add('forward_messages', Value);
  Result := Self;
end;

function TVkParamsMessageSend.GroupID(const Value: Integer): TVkParamsMessageSend;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageSend.Intent(const Value: TVkMessageIntent): TVkParamsMessageSend;
begin
  List.Add('intent', Value.ToString);
  Result := Self;
end;

function TVkParamsMessageSend.Keyboard(const Value: TVkKeyboard): TVkParamsMessageSend;
begin
  List.Add('keyboard', Value);
  Result := Self;
end;

function TVkParamsMessageSend.LatLong(const Lat, Long: Extended): TVkParamsMessageSend;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsMessageSend.Message(const Value: string): TVkParamsMessageSend;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsMessageSend.Payload(const Value: string): TVkParamsMessageSend;
begin
  List.Add('payload', Value);
  Result := Self;
end;

function TVkParamsMessageSend.PeerId(const Value: Integer): TVkParamsMessageSend;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

function TVkParamsMessageSend.RandomId(const Value: Integer): TVkParamsMessageSend;
begin
  if Value = -1 then
    List.Add('random_id', GetRandomId)
  else
    List.Add('random_id', Value);
  Result := Self;
end;

function TVkParamsMessageSend.ReplyTo(const Value: Integer): TVkParamsMessageSend;
begin
  List.Add('reply_to', Value);
  Result := Self;
end;

function TVkParamsMessageSend.StickerId(const Value: Integer): TVkParamsMessageSend;
begin
  List.Add('sticker_id', Value);
  Result := Self;
end;

function TVkParamsMessageSend.SubscribeId(const Value: Integer): TVkParamsMessageSend;
begin
  List.Add('subscribe_id', Value);
  Result := Self;
end;

function TVkParamsMessageSend.Template(const Value: TVkMessageTemplate): TVkParamsMessageSend;
begin
  List.Add('template', Value);
  Result := Self;
end;

function TVkParamsMessageSend.UserId(const Value: Integer): TVkParamsMessageSend;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsMessageSendIds }

function TVkParamsMessageSendIds.Attachment(const Value: TAttachmentArray): TVkParamsMessageSendIds;
begin
  List.Add('attachment', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.Attachment(const Value: TAttachment): TVkParamsMessageSendIds;
begin
  List.Add('attachment', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.ContentSource(const Value: TVkMessageContentSource): TVkParamsMessageSendIds;
begin
  List.Add('content_source', Value.ToJSON);
  Result := Self;
end;

function TVkParamsMessageSendIds.DisableMentions(const Value: Boolean): TVkParamsMessageSendIds;
begin
  List.Add('disable_mentions', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.DontParseLinks(const Value: Boolean): TVkParamsMessageSendIds;
begin
  List.Add('dont_parse_links', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.&Forward(const Value: TVkMessageForward): TVkParamsMessageSendIds;
begin
  List.Add('forward', Value.ToJSON);
  Result := Self;
end;

function TVkParamsMessageSendIds.ForwardMessages(const Value: TIdList): TVkParamsMessageSendIds;
begin
  List.Add('forward_messages', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.GroupID(const Value: Integer): TVkParamsMessageSendIds;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.Intent(const Value: TVkMessageIntent): TVkParamsMessageSendIds;
begin
  List.Add('intent', Value.ToString);
  Result := Self;
end;

function TVkParamsMessageSendIds.Keyboard(const Value: TVkKeyboard): TVkParamsMessageSendIds;
begin
  List.Add('keyboard', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.LatLong(const Lat, Long: Extended): TVkParamsMessageSendIds;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsMessageSendIds.Message(const Value: string): TVkParamsMessageSendIds;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.Payload(const Value: string): TVkParamsMessageSendIds;
begin
  List.Add('payload', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.PeerIds(const Value: TIdList): TVkParamsMessageSendIds;
begin
  List.Add('peer_ids', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.RandomId(const Value: Integer): TVkParamsMessageSendIds;
begin
  List.Add('random_id', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.ReplyTo(const Value: Integer): TVkParamsMessageSendIds;
begin
  List.Add('reply_to', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.StickerId(const Value: Integer): TVkParamsMessageSendIds;
begin
  List.Add('sticker_id', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.SubscribeId(const Value: Integer): TVkParamsMessageSendIds;
begin
  List.Add('subscribe_id', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.Template(const Value: TVkMessageTemplate): TVkParamsMessageSendIds;
begin
  List.Add('template', Value);
  Result := Self;
end;

function TVkParamsMessageSendIds.UserIds(const Value: TIdList): TVkParamsMessageSendIds;
begin
  List.Add('user_ids', Value);
  Result := Self;
end;

{ TVkParamsMessageDeleteConversation }

function TVkParamsMessageDeleteConversation.GroupID(const Value: Cardinal): TVkParamsMessageDeleteConversation;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageDeleteConversation.PeerId(const Value: Integer): TVkParamsMessageDeleteConversation;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

function TVkParamsMessageDeleteConversation.UserId(const Value: Integer): TVkParamsMessageDeleteConversation;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsMessageEdit }

function TVkParamsMessageEdit.Attachment(const Value: TAttachment): TVkParamsMessageEdit;
begin
  List.Add('attachment', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.Attachment(const Value: TAttachmentArray): TVkParamsMessageEdit;
begin
  List.Add('attachment', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.ConversationMessageId(const Value: Integer): TVkParamsMessageEdit;
begin
  List.Add('conversation_message_id', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.DontParseLinks(const Value: Boolean): TVkParamsMessageEdit;
begin
  List.Add('dont_parse_links', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.GroupID(const Value: Integer): TVkParamsMessageEdit;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.KeepForwardMessages(const Value: Boolean): TVkParamsMessageEdit;
begin
  List.Add('keep_forward_messages', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.KeepSnippets(const Value: Boolean): TVkParamsMessageEdit;
begin
  List.Add('keep_snippets', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.Keyboard(const Value: TVkKeyboard): TVkParamsMessageEdit;
begin
  List.Add('keyboard', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.Lat(const Value: Extended): TVkParamsMessageEdit;
begin
  List.Add('lat', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.LatLong(const Lat, Long: Extended): TVkParamsMessageEdit;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsMessageEdit.Long(const Value: Extended): TVkParamsMessageEdit;
begin
  List.Add('long', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.Message(const Value: string): TVkParamsMessageEdit;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.MessageId(const Value: Integer): TVkParamsMessageEdit;
begin
  List.Add('message_id', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.PeerId(const Value: Integer): TVkParamsMessageEdit;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

function TVkParamsMessageEdit.Template(const Value: TVkMessageTemplate): TVkParamsMessageEdit;
begin
  List.Add('template', Value);
  Result := Self;
end;

{ TVkParamsMessageGetByConvMesId }

function TVkParamsMessageGetByConvMesId.ConversationMessageIds(const Value: TIdList): TVkParamsMessageGetByConvMesId;
begin
  List.Add('conversation_message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageGetByConvMesId.ConversationMessageIds(const Value: Integer): TVkParamsMessageGetByConvMesId;
begin
  List.Add('conversation_message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageGetByConvMesId.Extended(const Value: Boolean): TVkParamsMessageGetByConvMesId;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsMessageGetByConvMesId.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsMessageGetByConvMesId;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsMessageGetByConvMesId.GroupID(const Value: Cardinal): TVkParamsMessageGetByConvMesId;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageGetByConvMesId.PeerId(const Value: Integer): TVkParamsMessageGetByConvMesId;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

{ TVkParamsMessageGetChat }

function TVkParamsMessageGetChat.ChatId(const Value: Integer): TVkParamsMessageGetChat;
begin
  List.Add('chat_ids', Value);
  Result := Self;
end;

function TVkParamsMessageGetChat.ChatIds(const Value: TIdList): TVkParamsMessageGetChat;
begin
  List.Add('chat_ids', Value);
  Result := Self;
end;

function TVkParamsMessageGetChat.Fields(const Value: TVkProfileFields): TVkParamsMessageGetChat;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsMessageGetChat.NameCase(const Value: TVkNameCase): TVkParamsMessageGetChat;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

{ TVkParamsConversationsGetById }

function TVkParamsConversationsGetById.Extended(const Value: Boolean): TVkParamsConversationsGetById;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsConversationsGetById.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsConversationsGetById;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsConversationsGetById.GroupID(const Value: Cardinal): TVkParamsConversationsGetById;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsConversationsGetById.PeerIds(const Value: TIdList): TVkParamsConversationsGetById;
begin
  List.Add('peer_ids', Value);
  Result := Self;
end;

{ TVkParamsGetHistoryAttachments }

function TVkParamsGetHistoryAttachments.Count(const Value: Integer): TVkParamsGetHistoryAttachments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsGetHistoryAttachments.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsGetHistoryAttachments;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsGetHistoryAttachments.GroupID(const Value: Integer): TVkParamsGetHistoryAttachments;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGetHistoryAttachments.MaxForwardsLevel(const Value: Integer): TVkParamsGetHistoryAttachments;
begin
  List.Add('max_forwards_level', Value);
  Result := Self;
end;

function TVkParamsGetHistoryAttachments.MediaType(const Value: TVkHistoryAttachment): TVkParamsGetHistoryAttachments;
begin
  List.Add('media_type', Value.ToString);
  Result := Self;
end;

function TVkParamsGetHistoryAttachments.PeerId(const Value: Integer): TVkParamsGetHistoryAttachments;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

function TVkParamsGetHistoryAttachments.PhotoSizes(const Value: Boolean): TVkParamsGetHistoryAttachments;
begin
  List.Add('photo_sizes', Value);
  Result := Self;
end;

function TVkParamsGetHistoryAttachments.PreserveOrder(const Value: Boolean): TVkParamsGetHistoryAttachments;
begin
  List.Add('preserve_order', Value);
  Result := Self;
end;

function TVkParamsGetHistoryAttachments.StartFrom(const Value: string): TVkParamsGetHistoryAttachments;
begin
  List.Add('start_from', Value);
  Result := Self;
end;

{ TVkParamsGetImportantMessages }

function TVkParamsGetImportantMessages.Count(const Value: Integer): TVkParamsGetImportantMessages;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsGetImportantMessages.Extended(const Value: Boolean): TVkParamsGetImportantMessages;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsGetImportantMessages.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsGetImportantMessages;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsGetImportantMessages.GroupID(const Value: Integer): TVkParamsGetImportantMessages;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGetImportantMessages.Offset(const Value: Integer): TVkParamsGetImportantMessages;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGetImportantMessages.PreviewLength(const Value: Integer): TVkParamsGetImportantMessages;
begin
  List.Add('preview_length', Value);
  Result := Self;
end;

function TVkParamsGetImportantMessages.StartMessageId(const Value: Integer): TVkParamsGetImportantMessages;
begin
  List.Add('start_message_id', Value);
  Result := Self;
end;

{ TVkParamsLongPollHistory }

function TVkParamsLongPollHistory.Credentials(const Value: Boolean): TVkParamsLongPollHistory;
begin
  List.Add('credentials', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.EventsLimit(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('events_limit', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.Fields(const Value: TVkProfileFields): TVkParamsLongPollHistory;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsLongPollHistory.GroupID(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.LastN(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('last_n', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.LpVersion(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('lp_version', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.MaxMsgId(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('max_msg_id', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.MsgsLimit(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('msgs_limit', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.Onlines(const Value: Boolean): TVkParamsLongPollHistory;
begin
  List.Add('onlines', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.PreviewLength(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('preview_length', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.Pts(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('pts', Value);
  Result := Self;
end;

function TVkParamsLongPollHistory.Ts(const Value: Integer): TVkParamsLongPollHistory;
begin
  List.Add('ts', Value);
  Result := Self;
end;

{ TVkParamsGetLongPollServer }

function TVkParamsGetLongPollServer.GroupID(const Value: Integer): TVkParamsGetLongPollServer;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGetLongPollServer.LpVersion(const Value: Integer): TVkParamsGetLongPollServer;
begin
  List.Add('lp_version', Value);
  Result := Self;
end;

function TVkParamsGetLongPollServer.NeedPts(const Value: Boolean): TVkParamsGetLongPollServer;
begin
  List.Add('need_pts', Value);
  Result := Self;
end;

{ TVkParamsMessageMark }

function TVkParamsMessageMarkAsRead.GroupID(const Value: Integer): TVkParamsMessageMarkAsRead;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageMarkAsRead.MarkConversationAsRead(const Value: Boolean): TVkParamsMessageMarkAsRead;
begin
  List.Add('mark_conversation_as_read', Value);
  Result := Self;
end;

function TVkParamsMessageMarkAsRead.MessageIds(const Value: TIdList): TVkParamsMessageMarkAsRead;
begin
  List.Add('message_ids', Value);
  Result := Self;
end;

function TVkParamsMessageMarkAsRead.PeerId(const Value: Integer): TVkParamsMessageMarkAsRead;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

function TVkParamsMessageMarkAsRead.StartMessageId(const Value: Integer): TVkParamsMessageMarkAsRead;
begin
  List.Add('start_message_id', Value);
  Result := Self;
end;

{ TVkParamsMessageRemoveChatUser }

function TVkParamsMessageRemoveChatUser.ChatId(const Value: Integer): TVkParamsMessageRemoveChatUser;
begin
  List.Add('chat_id', Value);
  Result := Self;
end;

function TVkParamsMessageRemoveChatUser.MemberId(const Value: Integer): TVkParamsMessageRemoveChatUser;
begin
  List.Add('member_id', Value);
  Result := Self;
end;

function TVkParamsMessageRemoveChatUser.UserId(const Value: Integer): TVkParamsMessageRemoveChatUser;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsMessageSearch }

function TVkParamsMessageSearch.Count(const Value: Integer): TVkParamsMessageSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsMessageSearch.Date(const Value: TDateTime): TVkParamsMessageSearch;
begin
  List.Add('date', Value, 'DDMMYYYY');
  Result := Self;
end;

function TVkParamsMessageSearch.Extended(const Value: Boolean): TVkParamsMessageSearch;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsMessageSearch.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsMessageSearch;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsMessageSearch.GroupID(const Value: Integer): TVkParamsMessageSearch;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageSearch.Offset(const Value: Integer): TVkParamsMessageSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsMessageSearch.PeerId(const Value: Integer): TVkParamsMessageSearch;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

function TVkParamsMessageSearch.PreviewLength(const Value: Integer): TVkParamsMessageSearch;
begin
  List.Add('preview_length', Value);
  Result := Self;
end;

function TVkParamsMessageSearch.Query(const Value: string): TVkParamsMessageSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

{ TVkParamsMessageSearchConversations }

function TVkParamsMessageSearchConversations.Count(const Value: Integer): TVkParamsMessageSearchConversations;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsMessageSearchConversations.Extended(const Value: Boolean): TVkParamsMessageSearchConversations;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsMessageSearchConversations.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): TVkParamsMessageSearchConversations;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsMessageSearchConversations.GroupID(const Value: Integer): TVkParamsMessageSearchConversations;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsMessageSearchConversations.Query(const Value: string): TVkParamsMessageSearchConversations;
begin
  List.Add('q', Value);
  Result := Self;
end;

{ TVkParamsConversationMembersGet }

function TVkParamsConversationMembersGet.Count(const Value: Integer): TVkParamsConversationMembersGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsConversationMembersGet.Extended(const Value: Boolean): TVkParamsConversationMembersGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsConversationMembersGet.Fields(const UserFields: TVkProfileFields; GroupFields: TVkGroupFields): TVkParamsConversationMembersGet;
begin
  List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
  Result := Self;
end;

function TVkParamsConversationMembersGet.GroupID(const Value: Integer): TVkParamsConversationMembersGet;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsConversationMembersGet.Offset(const Value: Integer): TVkParamsConversationMembersGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsConversationMembersGet.PeerId(const Value: Integer): TVkParamsConversationMembersGet;
begin
  List.Add('peer_id', Value);
  Result := Self;
end;

end.

