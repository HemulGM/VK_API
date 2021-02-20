unit VK.Messages;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, REST.Client,
  System.Json, VK.Controller, VK.Types, VK.Handler, VK.Entity.Keyboard,
  VK.Entity.Message, VK.Entity.Conversation, VK.Entity.Profile, VK.Entity.Group,
  VK.Entity.Message.Chat, VK.Entity.Media, VK.Entity.Common, VK.Entity.LongPoll,
  VK.Entity.Common.List;

type
  TVkMessageActivity = (maTyping, maAudioMessage);

  TVkMessageActivityHelper = record helper for TVkMessageActivity
    function ToString: string; inline;
  end;

  /// <summary>
  /// Тип материалов, который необходимо вернуть.
  /// <b>haPhoto</b> — фотографии;
  /// <b>haVideo</b> — видеозаписи;
  /// <b>haAudio</b> — аудиозаписи;
  /// <b>haDoc</b> — документы;
  /// <b>haLink</b> — ссылки;
  /// <b>haMarket</b> — товары;
  /// <b>haWall</b> — записи;
  /// <b>haShare</b> — ссылки, товары и записи.
  /// Обратите внимание — существует ограничение по дате отправки вложений. Так, для получения доступны вложения типов photo, video, audio, doc, отправленные не ранее 25.03.2013, link — не ранее 20.05.13, market, wall — 01.02.2016.
  /// </summary>
  TVkHistoryAttachment = (haPhoto, haVideo, haAudio, haDoc, haLink, haMarket, haWall, haShare);

  TVkHistoryAttachmentHelper = record helper for TVkHistoryAttachment
    function ToString: string; inline;
  end;

  /// <summary>
  ///  Интент — это метка, которая обозначает приблизительное содержание сообщения от сообщества. Передаётся в необязательном параметре messages.send — Intent.
  /// <b>miPromoNewsletter</b> - интент, который должен сопровождать рекламную рассылку для ботов.
  /// <b>miBotAdInvite</b> - интент, который должен сопровождать сообщения, запрашивающее подтверждение пользователя на отправку этому пользователю рекламы.
  /// <b>miBotAdPromo</b> - интент, который должен сопровождать сообщение содержащее рекламу от бота.
  /// </summary>
  TVkMessageIntent = (miDefault, miPromoNewsletter, miBotAdInvite, miBotAdPromo);

  TVkMessageIntentHelper = record helper for TVkMessageIntent
    function ToString: string; inline;
  end;

  TMessagesController = class;

  TVkMessageNew = class
  private
    FHandler: TVkHandler;
    FParams: TParams;
    procedure SetParams(const Value: TParams);
  public
    function PeerId(const Value: Integer): TVkMessageNew;
    function UserId(const Value: Integer): TVkMessageNew;
    function ChatId(const Value: Integer): TVkMessageNew;
    function UserIds(const Value: TIdList): TVkMessageNew;
    function UserDomian(const Value: string): TVkMessageNew;
    function Message(const Value: string): TVkMessageNew;
    function Payload(const Value: string): TVkMessageNew;
    /// <summary>
    /// Интент — это метка, которая обозначает приблизительное содержание сообщения от сообщества. Передаётся в необязательном параметре messages.send — Intent.
    /// </summary>
    function Intent(const Value: TVkMessageIntent): TVkMessageNew;
    function Keyboard(const Value: TVkKeyboard): TVkMessageNew;
    function DontParseLinks(const Value: Boolean): TVkMessageNew;
    function DisableMentions(const Value: Boolean): TVkMessageNew;
    function StickerId(const Value: Integer): TVkMessageNew;
    function SubscribeId(const Value: Integer): TVkMessageNew;
    function GroupId(const Value: Integer): TVkMessageNew;
    function ReplyTo(const Value: Integer): TVkMessageNew;
    function ForwardMessages(const Value: TIdList): TVkMessageNew;
    function Attachment(const Value: TAttachmentArray): TVkMessageNew; overload;
    function Attachment(const Value: TAttachment): TVkMessageNew; overload;
    function Send: TVkMessageSendResponses;
    constructor Create(Controller: TMessagesController);
    property Handler: TVkHandler read FHandler;
    property Params: TParams read FParams write SetParams;
  end;

  TVkParamsConversationsGet = record
    List: TParams;
    function Offset(const Value: Integer): Integer;
    function Count(const Value: Integer): Integer;
    function GroupId(const Value: Integer): Integer;
    function MajorSortId(const Value: Integer): Integer;
    function Filter(const Value: string): Integer;
    function Fields(const Value: string): Integer;
    function Extended(const Value: Boolean): Integer;
    function StartMessageId(const Value: Integer): Integer;
  end;

  TVkParamsMessageDelete = record
    List: TParams;
    function GroupId(const Value: Integer): Integer;
    function MessageIds(const Value: TIdList): Integer; overload;
    function MessageId(const Value: Integer): Integer; overload;
    function Spam(const Value: Boolean): Integer;
    function DeleteForAll(const Value: Boolean): Integer;
  end;

  TVkParamsMessageGet = record
    List: TParams;
    function MessageIds(const Value: TIdList): Integer; overload;
    function MessageId(const Value: Integer): Integer; overload;
    function PreviewLength(const Value: Integer): Integer;
    function Extended(const Value: Boolean): Integer;
    function Fields(const Value: string): Integer;
    function GroupId(const Value: Integer): Integer;
  end;

  TVkParamsMessageHistory = record
    List: TParams;
    function Count(const Value: Integer): Integer;
    function Extended(const Value: Boolean): Integer;
    function Fields(const Value: string): Integer;
    function GroupId(const Value: Integer): Integer;
    function Offset(const Value: Integer): Integer;
    function PeerId(const Value: Integer): Integer;
    function Rev(const Value: Boolean): Integer;
    function StartMessageId(const Value: Integer): Integer;
    function UserId(const Value: Integer): Integer;
  end;

  TVkParamsMessageSend = record
    List: TParams;
    function UserId(const Value: Integer): Integer;
    function ChatId(const Value: Integer): Integer;
    function Domain(const Value: string): Integer;
    function PeerId(const Value: Integer): Integer;
    function RandomId(const Value: Integer): Integer;
    function Message(const Value: string): Integer;
    function Lat(const Value: Extended): Integer;
    function Long(const Value: Extended): Integer;
    function Attachment(const Value: TAttachmentArray): Integer; overload;
    function Attachment(const Value: string): Integer; overload;
    function ReplyTo(const Value: Integer): Integer;
    function ForwardMessages(const Value: TIdList): Integer;
    function StickerId(const Value: Integer): Integer;
    function GroupId(const Value: Integer): Integer;
    function Keyboard(const Value: TVkKeyboard): Integer;
    function Payload(const Value: string): Integer;
    function Intent(const Value: TVkMessageIntent): Integer;
    function DontParseLinks(const Value: Boolean): Integer;
    function DisableMentions(const Value: Boolean): Integer;
    function SubscribeId(const Value: Integer): Integer;
  end;

  TVkParamsMessageSendIds = record
    List: TParams;
    function Domain(const Value: string): Integer;
    function UserIds(const Value: TIdList): Integer;
    function RandomId(const Value: Integer): Integer;
    function Message(const Value: string): Integer;
    function Lat(const Value: Extended): Integer;
    function Long(const Value: Extended): Integer;
    function Attachment(const Value: TAttachmentArray): Integer; overload;
    function Attachment(const Value: string): Integer; overload;
    function ReplyTo(const Value: Integer): Integer;
    function ForwardMessages(const Value: TIdList): Integer;
    function StickerId(const Value: Integer): Integer;
    function GroupId(const Value: Integer): Integer;
    function Keyboard(const Value: TVkKeyboard): Integer;
    function Payload(const Value: string): Integer;
    function Intent(const Value: TVkMessageIntent): Integer;
    function DontParseLinks(const Value: Boolean): Integer;
    function DisableMentions(const Value: Boolean): Integer;
    function SubscribeId(const Value: Integer): Integer;
  end;

  TVkParamsMessageDeleteConversation = record
    List: TParams;
    function GroupId(const Value: Integer): Integer;
    function PeerId(const Value: Integer): Integer;
    function UserId(const Value: Integer): Integer;
  end;

  TVkParamsMessageEdit = record
    List: TParams;
    function MessageId(const Value: Integer): Integer;
    function ConversationMessageId(const Value: Integer): Integer;
    function PeerId(const Value: Integer): Integer;
    function Message(const Value: string): Integer;
    function Lat(const Value: Extended): Integer;
    function Long(const Value: Extended): Integer;
    function LatLong(const Lat, Long: Extended): Integer;
    function Attachment(const Value: TAttachmentArray): Integer; overload;
    function Attachment(const Value: string): Integer; overload;
    function KeepForwardMessages(const Value: Boolean): Integer;
    function KeepSnippets(const Value: Boolean): Integer;
    function GroupId(const Value: Integer): Integer;
    function DontParseLinks(const Value: Boolean): Integer;
    function Template(const Value: string): Integer;
    function Keyboard(const Value: TVkKeyboard): Integer;
  end;

  TVkParamsMessageGetByConvMesId = record
    List: TParams;
    function PeerId(const Value: Integer): Integer;
    function ConversationMessageIds(const Value: TIdList): Integer; overload;
    function ConversationMessageIds(const Value: Integer): Integer; overload;
    function Extended(const Value: Boolean): Integer;
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    function GroupId(const Value: Integer): Integer;
  end;

  TVkParamsMessageGetChat = record
    List: TParams;
    function ChatId(const Value: Integer): Integer;
    function ChatIds(const Value: TIdList): Integer;
    function Fields(const Value: TVkProfileFields = []): Integer;
    function NameCase(const Value: TVkNameCase): Integer;
  end;

  TVkParamsConversationsGetById = record
    List: TParams;
    function PeerIds(const Value: TIdList): Integer;
    function Extended(const Value: Boolean): Integer;
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    function GroupId(const Value: Integer): Integer;
  end;

  TVkParamsGetHistoryAttachments = record
    List: TParams;
    function PeerId(const Value: Integer): Integer;
    function MediaType(const Value: TVkHistoryAttachment): Integer;
    function StartFrom(const Value: string): Integer;
    function Count(const Value: Integer = 30): Integer; //200
    function PhotoSizes(const Value: Integer): Integer;
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    function GroupId(const Value: Integer): Integer;
    function PreserveOrder(const Value: Boolean): Integer;
    function MaxForwardsLevel(const Value: Integer = 45): Integer;
  end;

  TVkParamsGetImportantMessages = record
    List: TParams;
    function Count(const Value: Integer = 20): Integer; //200
    function Offset(const Value: Integer): Integer;
    function StartMessageId(const Value: Integer): Integer;
    function PreviewLength(const Value: Integer): Integer;
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    function Extended(const Value: Boolean): Integer;
    function GroupId(const Value: Integer): Integer;
  end;

  TVkParamsLongPollHistory = record
    List: TParams;
    function Ts(const Value: Integer): Integer;
    function Pts(const Value: Integer): Integer;
    function PreviewLength(const Value: Integer): Integer;
    function Onlines(const Value: Boolean): Integer;
    function Fields(const Value: TVkProfileFields = []): Integer;
    function EventsLimit(const Value: Integer = 1000): Integer;
    function MsgsLimit(const Value: Integer = 200): Integer;
    function MaxMsgId(const Value: Integer): Integer;
    function GroupId(const Value: Integer): Integer;
    function LpVersion(const Value: Integer): Integer;
    function LastN(const Value: Integer = 0): Integer;
    function Credentials(const Value: Boolean): Integer;
  end;

  TVkParamsGetLongPollServer = record
    List: TParams;
    function NeedPts(const Value: Boolean): Integer;
    function GroupId(const Value: Integer): Integer;
    function LpVersion(const Value: Integer = 0): Integer;
  end;

  TVkParamsMessageMark = record
    List: TParams;
    function MessageIds(const Value: TIdList): Integer;
    function PeerId(const Value: Integer): Integer;
    function GroupId(const Value: Integer): Integer;
    function StartMessageId(const Value: Integer): Integer;
    function MarkConversationAsRead(const Value: Boolean): Integer;
  end;

  TVkParamsMessageRemoveChatUser = record
    List: TParams;
    function ChatId(const Value: Integer): Integer;
    function UserId(const Value: Integer): Integer;
    function MemberId(const Value: Integer): Integer;
  end;

  TVkParamsMessageSearch = record
    List: TParams;
    function Query(const Value: string): Integer;
    function PeerId(const Value: Integer): Integer;
    /// <summary>
    /// Если параметр задан, в ответе будут только сообщения, отправленные до указанной даты.
    /// </summary>
    function Date(const Value: TDateTime): Integer;
    function PreviewLength(const Value: Integer): Integer;
    /// <summary>
    /// Количество сообщений, которое необходимо получить.
    /// По умолчанию 20, максимальное значение 100
    /// </summary>
    function Count(const Value: Integer = 20): Integer; //100
    function Offset(const Value: Integer): Integer;
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    function Extended(const Value: Boolean): Integer;
    function GroupId(const Value: Integer): Integer;
  end;

  TVkParamsMessageSearchConversations = record
    List: TParams;
    function Query(const Value: string): Integer;
    function Count(const Value: Integer = 20): Integer;
    function Fields(const GroupFields: TVkGroupFields = []; UserFields: TVkProfileFields = []): Integer;
    function Extended(const Value: Boolean): Integer;
    function GroupId(const Value: Integer): Integer;
  end;

  TMessagesController = class(TVkController)
  public
    /// <summary>
    /// Отправить сообщение.
    /// </summary>
    function SendToPeer(var Item: Integer; PeerId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Отправить сообщение.
    /// </summary>
    function SendToPeer(const PeerId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    function Send(var Item: Integer; UserId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Отправить сообщение пользователю
    /// </summary>
    function Send(var Item: Integer; Domain: string; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Отправить сообщение в беседу
    /// </summary>
    function SendToChat(var Item: Integer; ChatId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Отправить сообщение нескольким пользователям (Доступно только для ключа доступа сообщества)
    /// </summary>
    function Send(var Items: TVkMessageSendResponses; UserIds: TIdList; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Отправить сообщение нескольким пользователям (Доступно только для ключа доступа сообщества)
    /// </summary>
    function Send(var Items: TVkMessageSendResponses; Params: TVkParamsMessageSendIds): Boolean; overload;
    /// <summary>
    /// Отправить сообщение нескольким пользователям (Доступно только для ключа доступа сообщества)
    /// </summary>
    function Send(var Item: Integer; Params: TVkParamsMessageSend): Boolean; overload;
    /// <summary>
    /// Универсальный метод отправки сообщений (Fluent Method)
    /// New.PeerId(123456).ReplyTo(12345)...Message('Текст').Send.Free;
    /// </summary>
    function New: TVkMessageNew; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkMessages; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkMessages; Params: TVkParamsMessageGet): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkMessages; Ids: TIdList; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkMessages; Id: Integer; PreviewLength: Integer = 0; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Добавляет в мультидиалог нового пользователя.
    /// </summary>
    function AddChatUser(const ChatId: Integer; UserId: Integer = -1; VisibleMessagesCount: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет сообщения
    /// </summary>
    function Delete(var Items: TVkMessageDelete; MessageIds: TIdList; GroupID: Integer = 0; DeleteForAll: Boolean = False; Spam: Boolean = False): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    function Delete(const MessageId: Integer; GroupID: Integer = 0; DeleteForAll: Boolean = False; Spam: Boolean = False): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    function Delete(var Items: TVkMessageDelete; Params: TVkParamsMessageDelete): Boolean; overload;
    /// <summary>
    /// Удаляет сообщение
    /// </summary>
    function Delete(var Items: TVkMessageDelete; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает ссылку для приглашения пользователя в беседу.
    /// Только создатель беседы имеет доступ к ссылке на беседу.
    /// </summary>
    function GetInviteLink(var Link: string; PeerId: Integer; Reset: Boolean = False; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список бесед пользователя.
    /// </summary>
    function GetConversations(var Items: TVkConversationItems; Params: TVkParamsConversationsGet): Boolean;
    /// <summary>
    /// Возвращает историю сообщений для указанного диалога.
    /// </summary>
    function GetHistory(var Items: TVkMessageHistory; Params: TVkParamsMessageHistory): Boolean;
    /// <summary>
    /// Позволяет разрешить отправку сообщений от сообщества текущему пользователю.
    /// </summary>
    function AllowMessagesFromGroup(const GroupId: Integer; Key: string): Boolean;
    /// <summary>
    /// Создаёт беседу с несколькими участниками.
    /// </summary>
    function CreateChat(var ChatId: Integer; UserIds: TIdList; Title: string; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Позволяет удалить фотографию мультидиалога.
    /// </summary>
    function DeleteChatPhoto(var Item: TVkChatInfoMessage; ChatId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет беседу.
    /// </summary>
    function DeleteConversation(var LastDeletedId: Integer; Params: TVkParamsMessageDeleteConversation): Boolean;
    /// <summary>
    /// Позволяет запретить отправку сообщений от сообщества текущему пользователю.
    /// </summary>
    function DenyMessagesFromGroup(const GroupId: Integer): Boolean;
    /// <summary>
    /// Редактирует сообщение.
    /// </summary>
    function Edit(const Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует сообщение.
    /// </summary>
    function Edit(const Params: TVkParamsMessageEdit): Boolean; overload;
    /// <summary>
    /// Изменяет название беседы.
    /// </summary>
    function EditChat(const ChatId: Integer; Title: string): Boolean;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам в рамках беседы.
    /// </summary>
    function GetByConversationMessageId(var Items: TVkMessages; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает сообщения по их идентификаторам в рамках беседы.
    /// </summary>
    function GetByConversationMessageId(var Items: TVkMessages; Params: TVkParamsMessageGetByConvMesId): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о беседе.
    /// </summary>
    function GetChat(var Items: TVkChats; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о беседе.
    /// </summary>
    function GetChat(var Items: TVkChats; Params: TVkParamsMessageGetChat): Boolean; overload;
    /// <summary>
    /// Получает данные для превью чата с приглашением по ссылке.
    /// </summary>
    function GetChatPreview(var Item: TVkChatPreview; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает данные для превью чата с приглашением по ссылке.
    /// </summary>
    function GetChatPreview(var Item: TVkChatPreview; PeerId: Integer; Link: string; Fields: TVkProfileFields): Boolean; overload;
    /// <summary>
    /// Позволяет получить беседу по её идентификатору.
    /// </summary>
    function GetConversationsById(var Items: TVkConversations; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет получить беседу по её идентификатору.
    /// </summary>
    function GetConversationsById(var Items: TVkConversations; Params: TVkParamsConversationsGetById): Boolean; overload;
    /// <summary>
    /// Возвращает материалы диалога или беседы.
    /// </summary>
    function GetHistoryAttachments(var Items: TVkAttachmentHistory; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает материалы диалога или беседы.
    /// </summary>
    function GetHistoryAttachments(var Items: TVkAttachmentHistory; Params: TVkParamsGetHistoryAttachments): Boolean; overload;
    /// <summary>
    /// Возвращает список важных сообщений пользователя.
    /// </summary>
    function GetImportantMessages(var Items: TVkImportantMessages; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список важных сообщений пользователя.
    /// </summary>
    function GetImportantMessages(var Items: TVkImportantMessages; Params: TVkParamsGetImportantMessages): Boolean; overload;
    /// <summary>
    /// Возвращает текущий статус и дату последней активности указанного пользователя.
    /// </summary>
    function GetLastActivity(var Item: TVkLastActivity; UserId: Integer): Boolean;
    /// <summary>
    /// Возвращает обновления в личных сообщениях пользователя.
    /// </summary>
    function GetLongPollHistory(var Item: TVkLongPollHistory; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает обновления в личных сообщениях пользователя.
    /// </summary>
    function GetLongPollHistory(var Item: TVkLongPollHistory; Params: TVkParamsLongPollHistory): Boolean; overload;
    /// <summary>
    /// Возвращает данные, необходимые для подключения к Long Poll серверу.
    /// </summary>
    function GetLongPollServer(var Item: TVkLongpollData; Params: TVkParamsGetLongPollServer): Boolean;
    /// <summary>
    /// Возвращает информацию о том, разрешена ли отправка сообщений от сообщества пользователю.
    /// </summary>
    function IsMessagesFromGroupAllowed(var IsAllowed: Boolean; GroupId, UserId: Integer): Boolean;
    /// <summary>
    /// Позволяет присоединиться к чату по ссылке-приглашению.
    /// </summary>
    function JoinChatByInviteLink(var ChatId: Integer; const Link: string): Boolean;
    /// <summary>
    /// Помечает беседу как отвеченную либо снимает отметку.
    /// </summary>
    function MarkAsAnsweredConversation(const PeerId: Integer; Answered: Boolean; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Помечает сообщения как важные либо снимает отметку.
    /// </summary>
    function MarkAsImportant(var Items: TIdList; MessageIds: TIdList; Important: Boolean): Boolean;
    /// <summary>
    /// Помечает беседу как важную либо снимает отметку.
    /// </summary>
    function MarkAsImportantConversation(const PeerId: Integer; Important: Boolean; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Помечает сообщения как прочитанные.
    /// </summary>
    function MarkAsRead(const Params: TParams): Boolean; overload;
    /// <summary>
    /// Помечает сообщения как прочитанные.
    /// </summary>
    function MarkAsRead(const Params: TVkParamsMessageMark): Boolean; overload;
    /// <summary>
    /// Помечает сообщения как непрочитанные.
    /// </summary>
    function MarkAsUnreadConversation(const PeerId: Integer; MessageIds: TIdList = []): Boolean;
    /// <summary>
    /// Закрепляет сообщение.
    /// </summary>
    function Pin(var Message: TVkMessage; PeerId, MessageId: Integer): Boolean; overload;
    /// <summary>
    /// Закрепляет сообщение.
    /// </summary>
    function Pin(const PeerId, MessageId: Integer): Boolean; overload;
    /// <summary>
    /// Исключает из мультидиалога пользователя, если текущий пользователь или сообщество является администратором беседы либо текущий пользователь пригласил исключаемого пользователя.
    /// </summary>
    function RemoveChatUser(const Params: TVkParamsMessageRemoveChatUser): Boolean;
    /// <summary>
    /// Восстанавливает удаленное сообщение.
    /// </summary>
    function Restore(const MessageId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список найденных личных сообщений текущего пользователя по введенной строке поиска.
    /// </summary>
    function Search(var Items: TVkMessageHistory; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список найденных личных сообщений текущего пользователя по введенной строке поиска.
    /// </summary>
    function Search(var Items: TVkMessageHistory; Params: TVkParamsMessageSearch): Boolean; overload;
    /// <summary>
    /// Позволяет искать диалоги.
    /// </summary>
    function SearchConversations(var Items: TVkConversations; Params: TVkParamsMessageSearchConversations): Boolean;
    /// <summary>
    /// Отправляет событие с действием, которое произойдет при нажатии на callback-кнопку.
    /// </summary>
    function SendMessageEventAnswer(const EventId: string; UserId, PeerId: Integer; EventData: string): Boolean;
    /// <summary>
    /// Изменяет статус набора текста пользователем в диалоге.
    /// Текст «N набирает сообщение...» отображается в течение 10 секунд после вызова метода, либо до момента отправки сообщения.
    /// </summary>
    function SetActivity(const UserId: string; ActivityType: TVkMessageActivity; PeerId, GroupId: Integer): Boolean;
    /// <summary>
    /// Позволяет установить фотографию мультидиалога, загруженную с помощью метода Photos.GetChatUploadServer.
    /// <b>UploadFile</b> - Содержимое поля Response из ответа специального upload сервера, полученного в результате загрузки изображения на адрес, полученный методом Photos.GetChatUploadServer.
    /// </summary>
    function SetChatPhoto(var Info: TVkChatInfoMessage; UploadFile: string): Boolean;
    /// <summary>
    /// Открепляет сообщение.
    /// </summary>
    function Unpin(const PeerId: Integer; GroupId: Integer = 0): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TMessagesController }

function TMessagesController.SendToPeer(var Item: Integer; PeerId: Integer; Message: string; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsMessageSend;
begin
  Params.PeerId(PeerId);
  Params.Message(Message);
  Params.RandomId(GetRandomId);
  if Length(Attachments) <> 0 then
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

  with Handler.Execute('messages.addChatUser', Params) do
  begin
    Result := Success and ResponseIsTrue;
  end;
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

function TMessagesController.AllowMessagesFromGroup(const GroupId: Integer; Key: string): Boolean;
begin
  with Handler.Execute('messages.allowMessagesFromGroup', [['group_id', GroupId.ToString], ['key', Key]]) do
  begin
    Result := Success and ResponseIsTrue;
  end;
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
  Ids: TStringList;
  i: Integer;
begin
  with Handler.Execute('messages.delete', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Ids := TStringList.Create;
      Ids.Delimiter := ',';
      Ids.DelimitedText := Params.GetValue('message_ids');
      Items := TVkMessageDelete.Create;
      try
        RespJSON := TJSONObject.ParseJSONValue(Response);
        for i := 0 to Ids.Count - 1 do
        begin
          Items.Items.Add(Ids[i], RespJSON.GetValue<Integer>(Ids[i], 0) = 1);
        end;
        RespJSON.Free;
      except
        Items.Free;
        Result := False;
      end;
      Ids.Free;
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
  Result := Handler.Execute('messages.deleteChatPhoto', Params).GetObject<TVkChatInfoMessage>(Item);
end;

function TMessagesController.DeleteConversation(var LastDeletedId: Integer; Params: TVkParamsMessageDeleteConversation): Boolean;
begin
  Result := Handler.Execute('messages.deleteConversation', Params.List).GetValue('last_deleted_id', LastDeletedId);
end;

function TMessagesController.DenyMessagesFromGroup(const GroupId: Integer): Boolean;
begin
  with Handler.Execute('messages.denyMessagesFromGroup', ['group_id', GroupId.ToString]) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.Edit(const Params: TVkParamsMessageEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TMessagesController.EditChat(const ChatId: Integer; Title: string): Boolean;
begin
  with Handler.Execute('messages.editChat', [['', ChatId.ToString], ['title', Title]]) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.Edit(const Params: TParams): Boolean;
begin
  with Handler.Execute('messages.edit', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.Delete(var Items: TVkMessageDelete; Params: TVkParamsMessageDelete): Boolean;
begin
  Result := Delete(Items, Params.List);
end;

function TMessagesController.GetByConversationMessageId(var Items: TVkMessages; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getByConversationMessageId', Params).GetObject<TVkMessages>(Items);
end;

function TMessagesController.GetByConversationMessageId(var Items: TVkMessages; Params: TVkParamsMessageGetByConvMesId): Boolean;
begin
  Result := GetByConversationMessageId(Items, Params.List);
end;

function TMessagesController.GetById(var Items: TVkMessages; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getById', Params).GetObject<TVkMessages>(Items);
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
  Result := Handler.Execute('messages.getChat', Params).GetObjects<TVkChats>(Items);
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
  Result := Handler.Execute('messages.getChatPreview', Params).GetObject<TVkChatPreview>(Item);
end;

function TMessagesController.GetConversations(var Items: TVkConversationItems; Params: TVkParamsConversationsGet): Boolean;
begin
  Result := Handler.Execute('messages.getConversations', Params.List).GetObject<TVkConversationItems>(Items);
end;

function TMessagesController.GetConversationsById(var Items: TVkConversations; Params: TVkParamsConversationsGetById): Boolean;
begin
  Result := GetConversationsById(Items, Params.List);
end;

function TMessagesController.GetConversationsById(var Items: TVkConversations; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getConversationsById', Params).GetObject<TVkConversations>(Items);
end;

function TMessagesController.GetHistory(var Items: TVkMessageHistory; Params: TVkParamsMessageHistory): Boolean;
begin
  Result := Handler.Execute('messages.getHistory', Params.List).GetObject<TVkMessageHistory>(Items);
end;

function TMessagesController.GetHistoryAttachments(var Items: TVkAttachmentHistory; Params: TVkParamsGetHistoryAttachments): Boolean;
begin
  Result := GetHistoryAttachments(Items, Params.List);
end;

function TMessagesController.GetHistoryAttachments(var Items: TVkAttachmentHistory; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getHistoryAttachments', Params).GetObject<TVkAttachmentHistory>(Items);
end;

function TMessagesController.GetImportantMessages(var Items: TVkImportantMessages; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getImportantMessages', Params).GetObject<TVkImportantMessages>(Items);
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
  Result := Handler.Execute('messages.getLastActivity', ['user_id', UserId.ToString]).GetObject<TVkLastActivity>(Item);
end;

function TMessagesController.GetLongPollHistory(var Item: TVkLongPollHistory; Params: TVkParamsLongPollHistory): Boolean;
begin
  Result := GetLongPollHistory(Item, Params.List);
end;

function TMessagesController.GetLongPollServer(var Item: TVkLongpollData; Params: TVkParamsGetLongPollServer): Boolean;
begin
  Result := Handler.Execute('messages.getLongPollServer', Params.List).GetObject<TVkLongpollData>(Item);
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
  with Handler.Execute('messages.markAsAnsweredConversation', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.MarkAsImportant(var Items: TIdList; MessageIds: TIdList; Important: Boolean): Boolean;
var
  Resp: TVkBasicIndexItems;
begin
  with Handler.Execute('messages.markAsImportant', [['message_ids', MessageIds.ToString], ['important', BoolToString(Important)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Resp := TVkBasicIndexItems.FromJsonString<TVkBasicIndexItems>(ResponseAsItems);
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

function TMessagesController.MarkAsImportantConversation(const PeerId: Integer; Important: Boolean; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  Params.Add('important', Important);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('messages.markAsImportantConversation', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.MarkAsRead(const Params: TVkParamsMessageMark): Boolean;
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
  with Handler.Execute('messages.markAsUnreadConversation', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.MarkAsRead(const Params: TParams): Boolean;
begin
  with Handler.Execute('messages.markAsRead', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.GetLongPollHistory(var Item: TVkLongPollHistory; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.getLongPollHistory', Params).GetObject<TVkLongPollHistory>(Item);
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
  with Handler.Execute('messages.removeChatUser', Params.List) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.Restore(const MessageId: Integer; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('message_id', MessageId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  with Handler.Execute('messages.restore', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.Pin(var Message: TVkMessage; PeerId, MessageId: Integer): Boolean;
begin
  Result := Handler.Execute('messages.pin', [
    ['peer_id', PeerId.ToString],
    ['message_id', MessageId.ToString]]).
    GetObject<TVkMessage>(Message);
end;

function TMessagesController.Search(var Items: TVkMessageHistory; Params: TParams): Boolean;
begin
  Result := Handler.Execute('messages.search', Params).GetObject<TVkMessageHistory>(Items);
end;

function TMessagesController.Search(var Items: TVkMessageHistory; Params: TVkParamsMessageSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TMessagesController.SearchConversations(var Items: TVkConversations; Params: TVkParamsMessageSearchConversations): Boolean;
begin
  Result := Handler.Execute('messages.searchConversations', Params.List).GetObject<TVkConversations>(Items);
end;

function TMessagesController.Send(var Item: Integer; Params: TVkParamsMessageSend): Boolean;
begin
  with Handler.Execute('messages.send', Params.List) do
    Result := Success and ResponseAsInt(Item);
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
  with Handler.Execute('messages.sendMessageEventAnswer', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.Send(var Items: TVkMessageSendResponses; Params: TVkParamsMessageSendIds): Boolean;
begin
  Result := Handler.Execute('messages.send', Params.List).GetObject<TVkMessageSendResponses>(Items);
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

function TMessagesController.SetActivity(const UserId: string; ActivityType: TVkMessageActivity; PeerId, GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('user_id', UserId);
  Params.Add('peer_id', PeerId);
  Params.Add('type', ActivityType.ToString);
  Params.Add('group_id', GroupId);
  with Handler.Execute('messages.setActivity', Params) do
    Result := Success and ResponseIsTrue;
end;

function TMessagesController.SetChatPhoto(var Info: TVkChatInfoMessage; UploadFile: string): Boolean;
begin
  Result := Handler.Execute('messages.setChatPhoto', ['file', UploadFile]).GetObject<TVkChatInfoMessage>(Info);
end;

function TMessagesController.Unpin(const PeerId: Integer; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  Params.Add('group_id', GroupId);
  with Handler.Execute('messages.unpin', Params) do
    Result := Success and ResponseIsTrue;
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
      if TryStrToInt(Response, Value) then
        Result := TVkMessageSendResponses.CreateTrue(Value)
      else
        Result := TVkMessageSendResponses.FromJsonString(Response);
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

function TVkMessageNew.Keyboard(const Value: TVkKeyboard): TVkMessageNew;
begin
  Params.Add('keyboard', Value.ToJsonString);
  Result := Self;
end;

function TVkMessageNew.Attachment(const Value: TAttachmentArray): TVkMessageNew;
begin
  Params.Add('attachment', Value.ToStrings);
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

function TVkMessageNew.UserDomian(const Value: string): TVkMessageNew;
begin
  Params.Add('domian', Value);
  Result := Self;
end;

function TVkMessageNew.UserId(const Value: Integer): TVkMessageNew;
begin
  Params.Add('user_id', Value);
  Result := Self;
end;

function TVkMessageNew.UserIds(const Value: TIdList): TVkMessageNew;
begin
  Params.Add('user_ids', Value);
  Result := Self;
end;

{ TVkParamsConversationsGet }

function TVkParamsConversationsGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsConversationsGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsConversationsGet.Fields(const Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsConversationsGet.Filter(const Value: string): Integer;
begin
  Result := List.Add('filter', Value);
end;

function TVkParamsConversationsGet.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsConversationsGet.MajorSortId(const Value: Integer): Integer;
begin
  Result := List.Add('major_sort_id', Value);
end;

function TVkParamsConversationsGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsConversationsGet.StartMessageId(const Value: Integer): Integer;
begin
  Result := List.Add('start_message_id', Value);
end;

{ TVkParamsMessageHistory }

function TVkParamsMessageHistory.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMessageHistory.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMessageHistory.Fields(const Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsMessageHistory.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageHistory.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMessageHistory.PeerId(const Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageHistory.Rev(const Value: Boolean): Integer;
begin
  Result := List.Add('rev', Value);
end;

function TVkParamsMessageHistory.StartMessageId(const Value: Integer): Integer;
begin
  Result := List.Add('start_message_id', Value);
end;

function TVkParamsMessageHistory.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsMessageGet }

function TVkParamsMessageGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMessageGet.Fields(const Value: string): Integer;
begin
  Result := List.Add('fields', Value);
end;

function TVkParamsMessageGet.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageGet.MessageId(const Value: Integer): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageGet.MessageIds(const Value: TIdList): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageGet.PreviewLength(const Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

{ TVkParamsMessageDelete }

function TVkParamsMessageDelete.DeleteForAll(const Value: Boolean): Integer;
begin
  Result := List.Add('delete_for_all', Value);
end;

function TVkParamsMessageDelete.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageDelete.MessageId(const Value: Integer): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageDelete.MessageIds(const Value: TIdList): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageDelete.Spam(const Value: Boolean): Integer;
begin
  Result := List.Add('spam', Value);
end;

{ TVkMessageIntentHelper }

function TVkMessageIntentHelper.ToString: string;
begin
  case Self of
    miDefault:
      Result := 'default';
    miPromoNewsletter:
      Result := 'promo_newsletter';
    miBotAdInvite:
      Result := 'bot_ad_invite';
    miBotAdPromo:
      Result := 'bot_ad_promo';
  end;
end;

{ TVkParamsMessageSend }

function TVkParamsMessageSend.Attachment(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachment', Value.ToStrings);
end;

function TVkParamsMessageSend.Attachment(const Value: string): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageSend.ChatId(const Value: Integer): Integer;
begin
  Result := List.Add('chat_id', Value);
end;

function TVkParamsMessageSend.DisableMentions(const Value: Boolean): Integer;
begin
  Result := List.Add('disable_mentions', Value);
end;

function TVkParamsMessageSend.Domain(const Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsMessageSend.DontParseLinks(const Value: Boolean): Integer;
begin
  Result := List.Add('dont_parse_links', Value);
end;

function TVkParamsMessageSend.ForwardMessages(const Value: TIdList): Integer;
begin
  Result := List.Add('forward_messages', Value);
end;

function TVkParamsMessageSend.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageSend.Intent(const Value: TVkMessageIntent): Integer;
begin
  Result := List.Add('intent', Value.ToString);
end;

function TVkParamsMessageSend.Keyboard(const Value: TVkKeyboard): Integer;
begin
  Result := List.Add('keyboard', Value.ToJsonString);
end;

function TVkParamsMessageSend.Lat(const Value: Extended): Integer;
begin
  Result := List.Add('lat', Value);
end;

function TVkParamsMessageSend.Long(const Value: Extended): Integer;
begin
  Result := List.Add('long', Value);
end;

function TVkParamsMessageSend.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMessageSend.Payload(const Value: string): Integer;
begin
  Result := List.Add('payload', Value);
end;

function TVkParamsMessageSend.PeerId(const Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageSend.RandomId(const Value: Integer): Integer;
begin
  Result := List.Add('random_id', Value);
end;

function TVkParamsMessageSend.ReplyTo(const Value: Integer): Integer;
begin
  Result := List.Add('reply_to', Value);
end;

function TVkParamsMessageSend.StickerId(const Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsMessageSend.SubscribeId(const Value: Integer): Integer;
begin
  Result := List.Add('subscribe_id', Value);
end;

function TVkParamsMessageSend.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsMessageSendIds }

function TVkParamsMessageSendIds.Attachment(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachment', Value.ToStrings);
end;

function TVkParamsMessageSendIds.Attachment(const Value: string): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageSendIds.DisableMentions(const Value: Boolean): Integer;
begin
  Result := List.Add('disable_mentions', Value);
end;

function TVkParamsMessageSendIds.Domain(const Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsMessageSendIds.DontParseLinks(const Value: Boolean): Integer;
begin
  Result := List.Add('dont_parse_links', Value);
end;

function TVkParamsMessageSendIds.ForwardMessages(const Value: TIdList): Integer;
begin
  Result := List.Add('forward_messages', Value);
end;

function TVkParamsMessageSendIds.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageSendIds.Intent(const Value: TVkMessageIntent): Integer;
begin
  Result := List.Add('intent', Value.ToString);
end;

function TVkParamsMessageSendIds.Keyboard(const Value: TVkKeyboard): Integer;
begin
  Result := List.Add('keyboard', Value.ToJsonString);
end;

function TVkParamsMessageSendIds.Lat(const Value: Extended): Integer;
begin
  Result := List.Add('lat', Value);
end;

function TVkParamsMessageSendIds.Long(const Value: Extended): Integer;
begin
  Result := List.Add('long', Value);
end;

function TVkParamsMessageSendIds.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMessageSendIds.Payload(const Value: string): Integer;
begin
  Result := List.Add('payload', Value);
end;

function TVkParamsMessageSendIds.RandomId(const Value: Integer): Integer;
begin
  Result := List.Add('random_id', Value);
end;

function TVkParamsMessageSendIds.ReplyTo(const Value: Integer): Integer;
begin
  Result := List.Add('reply_to', Value);
end;

function TVkParamsMessageSendIds.StickerId(const Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsMessageSendIds.SubscribeId(const Value: Integer): Integer;
begin
  Result := List.Add('subscribe_id', Value);
end;

function TVkParamsMessageSendIds.UserIds(const Value: TIdList): Integer;
begin
  Result := List.Add('user_ids', Value);
end;

{ TVkParamsMessageDeleteConversation }

function TVkParamsMessageDeleteConversation.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageDeleteConversation.PeerId(const Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageDeleteConversation.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsMessageEdit }

function TVkParamsMessageEdit.Attachment(const Value: string): Integer;
begin
  Result := List.Add('attachment', Value);
end;

function TVkParamsMessageEdit.Attachment(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachment', Value.ToStrings);
end;

function TVkParamsMessageEdit.ConversationMessageId(const Value: Integer): Integer;
begin
  Result := List.Add('conversation_message_id', Value);
end;

function TVkParamsMessageEdit.DontParseLinks(const Value: Boolean): Integer;
begin
  Result := List.Add('dont_parse_links', Value);
end;

function TVkParamsMessageEdit.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageEdit.KeepForwardMessages(const Value: Boolean): Integer;
begin
  Result := List.Add('keep_forward_messages', Value);
end;

function TVkParamsMessageEdit.KeepSnippets(const Value: Boolean): Integer;
begin
  Result := List.Add('keep_snippets', Value);
end;

function TVkParamsMessageEdit.Keyboard(const Value: TVkKeyboard): Integer;
begin
  Result := List.Add('keyboard', Value.ToJsonString);
end;

function TVkParamsMessageEdit.Lat(const Value: Extended): Integer;
begin
  Result := List.Add('lat', Value);
end;

function TVkParamsMessageEdit.LatLong(const Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsMessageEdit.Long(const Value: Extended): Integer;
begin
  Result := List.Add('long', Value);
end;

function TVkParamsMessageEdit.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsMessageEdit.MessageId(const Value: Integer): Integer;
begin
  Result := List.Add('message_id', Value);
end;

function TVkParamsMessageEdit.PeerId(const Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageEdit.Template(const Value: string): Integer;
begin
  Result := List.Add('template', Value);
end;

{ TVkParamsMessageGetByConvMesId }

function TVkParamsMessageGetByConvMesId.ConversationMessageIds(const Value: TIdList): Integer;
begin
  Result := List.Add('conversation_message_ids', Value);
end;

function TVkParamsMessageGetByConvMesId.ConversationMessageIds(const Value: Integer): Integer;
begin
  Result := List.Add('conversation_message_ids', Value);
end;

function TVkParamsMessageGetByConvMesId.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMessageGetByConvMesId.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsMessageGetByConvMesId.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageGetByConvMesId.PeerId(const Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

{ TVkParamsMessageGetChat }

function TVkParamsMessageGetChat.ChatId(const Value: Integer): Integer;
begin
  Result := List.Add('chat_ids', Value);
end;

function TVkParamsMessageGetChat.ChatIds(const Value: TIdList): Integer;
begin
  Result := List.Add('chat_ids', Value);
end;

function TVkParamsMessageGetChat.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsMessageGetChat.NameCase(const Value: TVkNameCase): Integer;
begin
  Result := List.Add('name_case', Value.ToString);
end;

{ TVkParamsConversationsGetById }

function TVkParamsConversationsGetById.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsConversationsGetById.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsConversationsGetById.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsConversationsGetById.PeerIds(const Value: TIdList): Integer;
begin
  Result := List.Add('peer_ids', Value);
end;

{ TVkHistoryAttachmentHelper }

function TVkHistoryAttachmentHelper.ToString: string;
begin
  case Self of
    haPhoto:
      Result := 'photo';
    haVideo:
      Result := 'video';
    haAudio:
      Result := 'audio';
    haDoc:
      Result := 'doc';
    haLink:
      Result := 'link';
    haMarket:
      Result := 'market';
    haWall:
      Result := 'wall';
    haShare:
      Result := 'share';
  else
    Result := 'photo';
  end;
end;

{ TVkParamsGetHistoryAttachments }

function TVkParamsGetHistoryAttachments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGetHistoryAttachments.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsGetHistoryAttachments.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGetHistoryAttachments.MaxForwardsLevel(const Value: Integer): Integer;
begin
  Result := List.Add('max_forwards_level', Value);
end;

function TVkParamsGetHistoryAttachments.MediaType(const Value: TVkHistoryAttachment): Integer;
begin
  Result := List.Add('media_type', Value.ToString);
end;

function TVkParamsGetHistoryAttachments.PeerId(const Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsGetHistoryAttachments.PhotoSizes(const Value: Integer): Integer;
begin
  Result := List.Add('photo_sizes', Value);
end;

function TVkParamsGetHistoryAttachments.PreserveOrder(const Value: Boolean): Integer;
begin
  Result := List.Add('preserve_order', Value);
end;

function TVkParamsGetHistoryAttachments.StartFrom(const Value: string): Integer;
begin
  Result := List.Add('start_from', Value);
end;

{ TVkParamsGetImportantMessages }

function TVkParamsGetImportantMessages.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsGetImportantMessages.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsGetImportantMessages.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsGetImportantMessages.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGetImportantMessages.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsGetImportantMessages.PreviewLength(const Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

function TVkParamsGetImportantMessages.StartMessageId(const Value: Integer): Integer;
begin
  Result := List.Add('start_message_id', Value);
end;

{ TVkParamsLongPollHistory }

function TVkParamsLongPollHistory.Credentials(const Value: Boolean): Integer;
begin
  Result := List.Add('credentials', Value);
end;

function TVkParamsLongPollHistory.EventsLimit(const Value: Integer): Integer;
begin
  Result := List.Add('events_limit', Value);
end;

function TVkParamsLongPollHistory.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsLongPollHistory.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsLongPollHistory.LastN(const Value: Integer): Integer;
begin
  Result := List.Add('last_n', Value);
end;

function TVkParamsLongPollHistory.LpVersion(const Value: Integer): Integer;
begin
  Result := List.Add('lp_version', Value);
end;

function TVkParamsLongPollHistory.MaxMsgId(const Value: Integer): Integer;
begin
  Result := List.Add('max_msg_id', Value);
end;

function TVkParamsLongPollHistory.MsgsLimit(const Value: Integer): Integer;
begin
  Result := List.Add('msgs_limit', Value);
end;

function TVkParamsLongPollHistory.Onlines(const Value: Boolean): Integer;
begin
  Result := List.Add('onlines', Value);
end;

function TVkParamsLongPollHistory.PreviewLength(const Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

function TVkParamsLongPollHistory.Pts(const Value: Integer): Integer;
begin
  Result := List.Add('pts', Value);
end;

function TVkParamsLongPollHistory.Ts(const Value: Integer): Integer;
begin
  Result := List.Add('ts', Value);
end;

{ TVkParamsGetLongPollServer }

function TVkParamsGetLongPollServer.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsGetLongPollServer.LpVersion(const Value: Integer): Integer;
begin
  Result := List.Add('lp_version', Value);
end;

function TVkParamsGetLongPollServer.NeedPts(const Value: Boolean): Integer;
begin
  Result := List.Add('need_pts', Value);
end;

{ TVkParamsMessageMark }

function TVkParamsMessageMark.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageMark.MarkConversationAsRead(const Value: Boolean): Integer;
begin
  Result := List.Add('mark_conversation_as_read', Value);
end;

function TVkParamsMessageMark.MessageIds(const Value: TIdList): Integer;
begin
  Result := List.Add('message_ids', Value);
end;

function TVkParamsMessageMark.PeerId(const Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageMark.StartMessageId(const Value: Integer): Integer;
begin
  Result := List.Add('start_message_id', Value);
end;

{ TVkParamsMessageRemoveChatUser }

function TVkParamsMessageRemoveChatUser.ChatId(const Value: Integer): Integer;
begin
  Result := List.Add('chat_id', Value);
end;

function TVkParamsMessageRemoveChatUser.MemberId(const Value: Integer): Integer;
begin
  Result := List.Add('member_id', Value);
end;

function TVkParamsMessageRemoveChatUser.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsMessageSearch }

function TVkParamsMessageSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMessageSearch.Date(const Value: TDateTime): Integer;
begin
  Result := List.Add('date', FormatDateTime('DDMMYYYY', Value));
end;

function TVkParamsMessageSearch.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMessageSearch.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsMessageSearch.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsMessageSearch.PeerId(const Value: Integer): Integer;
begin
  Result := List.Add('peer_id', Value);
end;

function TVkParamsMessageSearch.PreviewLength(const Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

function TVkParamsMessageSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

{ TVkParamsMessageSearchConversations }

function TVkParamsMessageSearchConversations.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsMessageSearchConversations.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsMessageSearchConversations.Fields(const GroupFields: TVkGroupFields; UserFields: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsMessageSearchConversations.GroupID(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsMessageSearchConversations.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

{ TVkMessageActivityHelper }

function TVkMessageActivityHelper.ToString: string;
begin
  case Self of
    maTyping:
      Result := 'typing';
    maAudioMessage:
      Result := 'audiomessage';
  else
    Result := 'typing';
  end;
end;

end.

