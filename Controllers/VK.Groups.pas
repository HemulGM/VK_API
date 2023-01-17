unit VK.Groups;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, REST.Json,
  System.Json, VK.Controller, VK.Types, VK.Entity.Profile, System.Classes,
  VK.Entity.Group, VK.CommonUtils, VK.Entity.Common, VK.Entity.Group.TimeTable,
  VK.Entity.Group.Ban, VK.Entity.Group.CallBackServer,
  VK.Entity.Group.CallbackSettings, VK.Entity.Group.Categories,
  VK.Entity.Longpoll, VK.Entity.Group.LongpollSettings, VK.Entity.GroupSettings,
  VK.Entity.Group.TokenPermissions, VK.Entity.Common.List,
  VK.Entity.Group.Invites, VK.Entity.Group.Status;

type
  TVkParamsGroupsGetMembers = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsGetMembers; overload;
    /// <summary>
    /// Короткое имя сообщества
    /// </summary>
    function GroupId(const Value: string): TVkParamsGroupsGetMembers; overload;
    /// <summary>
    /// Фильтр
    /// </summary>
    function Filter(const Value: TVkGroupMembersFilter): TVkParamsGroupsGetMembers;
    /// <summary>
    /// Список дополнительных полей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsGroupsGetMembers;
    /// <summary>
    /// Количество участников сообщества, информацию о которых необходимо получить (максимальное значение 1000)
    /// </summary>
    function Count(const Value: Int64 = 1000): TVkParamsGroupsGetMembers;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества участников
    /// </summary>
    function Offset(const Value: Int64 = 0): TVkParamsGroupsGetMembers;
    /// <summary>
    /// Сортировка, с которой необходимо вернуть список участников
    /// </summary>
    function Sort(const Value: TVkSortIdTime = TVkSortIdTime.IdAsc): TVkParamsGroupsGetMembers;
  end;

  TVkParamsGroupsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsGroupsGet;
    /// <summary>
    /// Список фильтров сообществ, которые необходимо вернуть
    /// </summary>
    function Filter(const Value: TVkGroupFilters): TVkParamsGroupsGet; overload;
    /// <summary>
    /// Список дополнительных полей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsGroupsGet; overload;
    /// <summary>
    /// Количество сообществ, информацию о которых нужно вернуть (максимальное значение 1000)
    /// </summary>
    function Count(const Value: Int64): TVkParamsGroupsGet;
    /// <summary>
    /// Смещение, необходимое для выборки определённого подмножества сообществ
    /// </summary>
    function Offset(const Value: Int64): TVkParamsGroupsGet;
  end;

  TVkParamsGroupsIsMember = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества.
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsIsMember; overload;
    /// <summary>
    /// Короткое имя сообщества.
    /// </summary>
    function GroupId(const Value: string): TVkParamsGroupsIsMember; overload;
    /// <summary>
    /// True — вернуть ответ в расширенной форме. По умолчанию — False.
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsGroupsIsMember;
    /// <summary>
    /// Идентификатор пользователя.
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsGroupsIsMember;
    /// <summary>
    /// Идентификаторы пользователей, не более 500.
    /// </summary>
    function UserIds(const Value: TVkPeerIds): TVkParamsGroupsIsMember;
  end;

  TVkParamsGroupsAddAddress = record
    List: TParams;
    /// <summary>
    /// идентификатор сообщества, в которое добавляется адрес
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsAddAddress;
    /// <summary>
    /// заголовок адреса (максимальная длина 255)
    /// </summary>
    function Title(const Value: string): TVkParamsGroupsAddAddress;
    /// <summary>
    /// строка адреса (максимальная длина 255)
    /// </summary>
    function Address(const Value: string): TVkParamsGroupsAddAddress;
    /// <summary>
    /// дополнительное описание адреса (максимальная длина 400)
    /// </summary>
    function AdditionalAddress(const Value: string): TVkParamsGroupsAddAddress;
    /// <summary>
    /// идентификатор страны. Для получения можно использовать database.getCountries
    /// </summary>
    function CountryId(const Value: Int64): TVkParamsGroupsAddAddress;
    /// <summary>
    /// идентификатор города. Для получения можно использовать database.getCities
    /// </summary>
    function CityId(const Value: Int64): TVkParamsGroupsAddAddress;
    /// <summary>
    /// идентификатор станции метро. Для получения можно использовать database.getMetroStations
    /// </summary>
    function MetroId(const Value: Int64): TVkParamsGroupsAddAddress;
    /// <summary>
    /// географическая широта отметки, заданная в градусах (от -90 до 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsGroupsAddAddress;
    /// <summary>
    /// географическая долгота отметки, заданная в градусах (от -180 до 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsGroupsAddAddress;
    /// <summary>
    /// номер телефона
    /// </summary>
    function Phone(const Value: string): TVkParamsGroupsAddAddress;
    /// <summary>
    /// тип расписания
    /// </summary>
    function WorkInfoStatus(const Value: TVkWorkInfoStatus = TVkWorkInfoStatus.NoInformation): TVkParamsGroupsAddAddress;
    /// <summary>
    /// Время передается в минутах от 0 часов. Ключ по дню означает, что день рабочий.
    /// OpenTime, CloseTime - начало и конец рабочего дня.
    /// BreakOpenTime, BreakCloseTime — время перерыва
    /// </summary>
    function Timetable(const Value: TVkTimeTable): TVkParamsGroupsAddAddress;
    /// <summary>
    /// Установить адрес основным. Информация об основном адресе сразу показывается в сообществе.
    /// Для получения информации об остальных адресах нужно перейти к списку адресов
    /// </summary>
    function IsMainAddress(const Value: Boolean): TVkParamsGroupsAddAddress;
  end;

  TVkParamsGroupsEditAddress = TVkParamsGroupsAddAddress;

  TVkParamsGroupsBan = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsBan;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которое будет добавлено в черный список
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsGroupsBan;
    /// <summary>
    /// Дата завершения срока действия бана.
    /// Максимальный возможный срок окончания бана, который можно указать, — один год с его начала.
    /// Если параметр не указан, пользователь будет заблокирован навсегда
    /// </summary>
    function EndDate(const Value: TDateTime): TVkParamsGroupsBan;
    /// <summary>
    /// Причина бана
    /// </summary>
    function Reason(const Value: TVkUserBlockReason = TVkUserBlockReason.Other): TVkParamsGroupsBan;
    /// <summary>
    /// Текст комментария к бану
    /// </summary>
    function Comment(const Value: string): TVkParamsGroupsBan;
    /// <summary>
    /// True – текст комментария будет отображаться пользователю.
    /// False – текст комментария не доступен пользователю
    /// </summary>
    function CommentVisible(const Value: Boolean = False): TVkParamsGroupsBan;
  end;

  TVkParamsGroupsCreate = record
    List: TParams;
    /// <summary>
    /// Название сообщества
    /// </summary>
    function Title(const Value: string): TVkParamsGroupsCreate;
    /// <summary>
    /// Описание сообщества, (не учитывается при Type = TVkGroupTypeCreate.Public)
    /// </summary>
    function Description(const Value: string): TVkParamsGroupsCreate;
    /// <summary>
    /// Тип создаваемого сообщества
    /// </summary>
    function &Type(const Value: TVkGroupTypeCreate): TVkParamsGroupsCreate;
    /// <summary>
    /// Категория публичной страницы (только для Type = TVkGroupTypeCreate.Public)
    /// </summary>
    function PublicCategory(const Value: Int64): TVkParamsGroupsCreate;
    /// <summary>
    /// Вид публичной страницы (только при Type = TVkGroupTypeCreate.Public)
    /// </summary>
    function Subtype(const Value: TVkGroupSubType): TVkParamsGroupsCreate;
  end;

  TVkParamsGroupsEdit = record
    const
      /// <summary>
      /// Выключено
      /// </summary>
      GroupSectionOff = 0;
      /// <summary>
      /// Открыто
      /// </summary>
      GroupSectionOpen = 1;
      /// <summary>
      /// Ограниченно (доступно только для групп и событий)
      /// </summary>
      GroupSectionPrivate = 2;
      /// <summary>
      /// Закрыто (доступно только для групп и событий)
      /// </summary>
      GroupSectionClosed = 3;
    type
      TGroupSectionWall = GroupSectionOff..GroupSectionClosed;


      TGroupSectionTopics = GroupSectionOff..GroupSectionPrivate;


      TGroupSectionPhotos = GroupSectionOff..GroupSectionPrivate;


      TGroupSectionVideo = GroupSectionOff..GroupSectionPrivate;


      TGroupSectionAudio = GroupSectionOff..GroupSectionPrivate;


      TGroupSectionDocs = GroupSectionOff..GroupSectionPrivate;


      TGroupSectionWiki = GroupSectionOff..GroupSectionPrivate;
  public
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsEdit;
    /// <summary>
    /// Название сообщества
    /// </summary>
    function Title(const Value: string): TVkParamsGroupsEdit;
    /// <summary>
    /// Описание сообщества
    /// </summary>
    function Description(const Value: string): TVkParamsGroupsEdit;
    /// <summary>
    /// Короткое имя сообщества
    /// </summary>
    function ScreenName(const Value: string): TVkParamsGroupsEdit;
    /// <summary>
    /// Тип группы (доступ)
    /// </summary>
    function Access(const Value: TVkGroupAccess): TVkParamsGroupsEdit;
    /// <summary>
    /// Адрес сайта, который будет указан в информации о группе
    /// </summary>
    function Website(const Value: string): TVkParamsGroupsEdit;
    /// <summary>
    /// Тематика сообщества
    /// </summary>
    function Subject(const Value: TVkGroupSubjectType): TVkParamsGroupsEdit;
    /// <summary>
    /// Электронный адрес организатора (для мероприятий)
    /// </summary>
    function Email(const Value: string): TVkParamsGroupsEdit;
    /// <summary>
    /// Номер телефона организатора (для мероприятий)
    /// </summary>
    function Phone(const Value: string): TVkParamsGroupsEdit;
    /// <summary>
    /// Адрес rss для импорта новостей (доступен только группам,
    /// получившим соответствующее разрешение, обратитесь в http://vk.com/support для получения разрешения)
    /// </summary>
    function Rss(const Value: string): TVkParamsGroupsEdit;
    /// <summary>
    /// Дата начала события
    /// </summary>
    function EventStartDate(const Value: TDateTime): TVkParamsGroupsEdit;
    /// <summary>
    /// Дата окончания события
    /// </summary>
    function EventFinishDate(const Value: TDateTime): TVkParamsGroupsEdit;
    /// <summary>
    /// Идентификатор группы, которая является организатором события (только для событий)
    /// </summary>
    function EventGroupId(const Value: Int64): TVkParamsGroupsEdit;
    /// <summary>
    /// Категория публичной страницы
    /// </summary>
    function PublicCategory(const Value: Int64): TVkParamsGroupsEdit;
    /// <summary>
    /// Подкатегория публичной станицы. Список подкатегорий можно получить методом groups.getSettings
    /// </summary>
    function PublicSubcategory(const Value: Int64): TVkParamsGroupsEdit;
    /// <summary>
    /// Дата основания компании, организации, которой посвящена публичная страница
    /// </summary>
    function PublicDate(const Value: TDateTime): TVkParamsGroupsEdit;
    /// <summary>
    /// Стена
    /// </summary>
    function Wall(const Value: TGroupSectionWall): TVkParamsGroupsEdit;
    /// <summary>
    /// Обсуждения
    /// </summary>
    function Topics(const Value: TGroupSectionTopics): TVkParamsGroupsEdit;
    /// <summary>
    /// Фотографии
    /// </summary>
    function Photos(const Value: TGroupSectionPhotos): TVkParamsGroupsEdit;
    /// <summary>
    /// Видеозаписи
    /// </summary>
    function Video(const Value: TGroupSectionVideo): TVkParamsGroupsEdit;
    /// <summary>
    /// Аудиозаписи
    /// </summary>
    function Audio(const Value: TGroupSectionAudio): TVkParamsGroupsEdit;
    /// <summary>
    /// Ссылки (доступно только для публичных страниц)
    /// </summary>
    function Links(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// События (доступно только для публичных страниц)
    /// </summary>
    function Events(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Места (доступно только для публичных страниц)
    /// </summary>
    function Places(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Контакты (доступно только для публичных страниц)
    /// </summary>
    function Contacts(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Документы сообщества
    /// </summary>
    function Docs(const Value: TGroupSectionDocs): TVkParamsGroupsEdit;
    /// <summary>
    /// Wiki-материалы сообщества
    /// </summary>
    function Wiki(const Value: TGroupSectionWiki): TVkParamsGroupsEdit;
    /// <summary>
    /// Сообщения сообщества
    /// </summary>
    function Messages(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Статьи
    /// </summary>
    function Articles(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Адреса
    /// </summary>
    function Addresses(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Возрастное ограничение для сообщества
    /// </summary>
    function AgeLimits(const Value: TVkAgeLimits): TVkParamsGroupsEdit;
    /// <summary>
    /// Товары
    /// </summary>
    function Market(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Комментарии к товарам
    /// </summary>
    function MarketComments(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Регионы доставки товаров
    /// </summary>
    function MarketCountry(const Value: TIdList): TVkParamsGroupsEdit;
    /// <summary>
    /// Города доставки товаров (в случае если указана одна страна)
    /// </summary>
    function MarketCity(const Value: TIdList): TVkParamsGroupsEdit;
    /// <summary>
    /// Идентификатор валюты магазина
    /// </summary>
    function MarketCurrency(const Value: TVkCurrency): TVkParamsGroupsEdit;
    /// <summary>
    /// Контакт для связи для продавцом.
    /// Для использования сообщений сообщества следует включить их и передать значение 0
    /// </summary>
    function MarketContact(const Value: Int64): TVkParamsGroupsEdit;
    /// <summary>
    /// Идентификатор wiki-страницы с описанием магазина
    /// </summary>
    function MarketWiki(const Value: Int64): TVkParamsGroupsEdit;
    /// <summary>
    /// Фильтр нецензурных выражений в комментариях
    /// </summary>
    function ObsceneFilter(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Фильтр по ключевым словам в комментариях
    /// </summary>
    function ObsceneStopwords(const Value: Boolean): TVkParamsGroupsEdit;
    /// <summary>
    /// Ключевые слова для фильтра комментариев
    /// </summary>
    function ObsceneWords(const Value: TArrayOfString): TVkParamsGroupsEdit;
    /// <summary>
    /// MainSection
    /// </summary>
    function MainSection(const Value: Int64): TVkParamsGroupsEdit;
    /// <summary>
    /// SecondarySection
    /// </summary>
    function SecondarySection(const Value: Int64): TVkParamsGroupsEdit;
    /// <summary>
    /// Страна
    /// </summary>
    function Country(const Value: Int64): TVkParamsGroupsEdit;
    /// <summary>
    /// Город
    /// </summary>
    function City(const Value: Int64): TVkParamsGroupsEdit;
  end;

  TVkParamsGroupsEditManager = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества (указывается без знака «минус»).
    /// </summary>
    function GroupId(const Value: UInt64): TVkParamsGroupsEditManager;
    /// <summary>
    /// Идентификатор пользователя, чьи полномочия в сообществе нужно изменить
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsGroupsEditManager;
    /// <summary>
    /// Уровень полномочий
    /// Если параметр не задан, с пользователя user_id снимаются полномочия руководителя
    /// </summary>
    function Role(const Value: TVkGroupRole): TVkParamsGroupsEditManager;
    /// <summary>
    /// Отображать ли пользователя в блоке контактов сообщества
    /// </summary>
    function IsContact(const Value: Boolean): TVkParamsGroupsEditManager;
    /// <summary>
    /// Должность пользователя, отображаемая в блоке контактов
    /// </summary>
    function ContactPosition(const Value: string): TVkParamsGroupsEditManager;
    /// <summary>
    /// Телефон пользователя, отображаемый в блоке контактов
    /// </summary>
    function ContactPhone(const Value: string): TVkParamsGroupsEditManager;
    /// <summary>
    /// Email пользователя, отображаемый в блоке контактов
    /// </summary>
    function ContactEmail(const Value: string): TVkParamsGroupsEditManager;
  end;

  TVkParamsGroupsGetAddresses = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: UInt64): TVkParamsGroupsGetAddresses;
    /// <summary>
    /// Перечисленные через запятую идентификаторы адресов, информацию о которых необходимо вернуть
    /// </summary>
    function AddressIds(const Value: TIdList): TVkParamsGroupsGetAddresses; overload;
    /// <summary>
    /// Перечисленные через запятую идентификаторы адресов, информацию о которых необходимо вернуть
    /// </summary>
    function AddressIds(const Value: Int64): TVkParamsGroupsGetAddresses; overload;
    /// <summary>
    /// Географическая широта отметки, заданная в градусах (от -90 до 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsGroupsGetAddresses;
    /// <summary>
    /// Географическая долгота отметки, заданная в градусах (от -180 до 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsGroupsGetAddresses;
    /// <summary>
    /// Список дополнительных полей адресов, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkGroupAddressFields): TVkParamsGroupsGetAddresses; overload;
    /// <summary>
    /// Количество адресов, которое необходимо вернуть
    /// </summary>
    function Count(const Value: Int64 = 10): TVkParamsGroupsGetAddresses;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества черного списка
    /// </summary>
    function Offset(const Value: Int64 = 0): TVkParamsGroupsGetAddresses;
  end;

  TVkParamsGroupsGetBanned = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsGetBanned;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества черного списка
    /// </summary>
    function Offset(const Value: Int64 = 0): TVkParamsGroupsGetBanned;
    /// <summary>
    /// Количество пользователей, которое необходимо вернуть (максимальное значение 200)
    /// </summary>
    function Count(const Value: Int64 = 20): TVkParamsGroupsGetBanned;
    /// <summary>
    /// Список дополнительных полей профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields = []): TVkParamsGroupsGetBanned; overload;
    /// <summary>
    /// Идентификатор пользователя или сообщества из чёрного списка, информацию о котором нужно получить
    /// </summary>
    function OwnerId(const Value: Int64): TVkParamsGroupsGetBanned;
  end;

  TVkParamsGroupsGetInvitedUsers = record
    List: TParams;
    /// <summary>
    /// Идентификатор группы, список приглашенных в которую пользователей нужно вернуть
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsGetInvitedUsers;
    /// <summary>
    /// Смещение, необходимое для выборки определённого подмножества пользователей
    /// </summary>
    function Offset(const Value: Int64 = 0): TVkParamsGroupsGetInvitedUsers;
    /// <summary>
    /// Количество пользователей, информацию о которых нужно вернуть
    /// </summary>
    function Count(const Value: Int64 = 20): TVkParamsGroupsGetInvitedUsers;
    /// <summary>
    /// Список дополнительных полей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields = []): TVkParamsGroupsGetInvitedUsers;
    /// <summary>
    /// Падеж для склонения имени и фамилии пользователя
    /// </summary>
    function NameCase(const Value: TVkNameCase): TVkParamsGroupsGetInvitedUsers;
  end;

  TVkParamsGroupsSearch = record
    List: TParams;
    /// <summary>
    /// Текст поискового запроса
    /// </summary>
    function Query(const Value: string): TVkParamsGroupsSearch;
    /// <summary>
    /// Тип сообщества
    /// </summary>
    function &Type(const Value: TVkGroupType): TVkParamsGroupsSearch;
    /// <summary>
    /// Идентификатор страны
    /// </summary>
    function CountryId(const Value: Int64): TVkParamsGroupsSearch;
    /// <summary>
    /// Идентификатор города. При передаче этого параметра поле CountryId игнорируется
    /// </summary>
    function CityId(const Value: Int64): TVkParamsGroupsSearch;
    /// <summary>
    /// При передаче значения True будут выведены предстоящие события.
    /// Учитывается только при передаче в качестве Type значения Event
    /// </summary>
    function Future(const Value: Boolean): TVkParamsGroupsSearch;
    /// <summary>
    /// При передаче значения 1 будут выведены сообщества с включенными товарами
    /// </summary>
    function Market(const Value: Boolean): TVkParamsGroupsSearch;
    /// <summary>
    /// Сортировка
    /// </summary>
    function Sort(const Value: TVkGroupSearchSort): TVkParamsGroupsSearch;
    /// <summary>
    /// Смещение, необходимое для выборки определённого подмножества результатов поиска
    /// </summary>
    function Offset(const Value: Int64 = 0): TVkParamsGroupsSearch;
    /// <summary>
    /// Количество результатов поиска, которое необходимо вернуть (1000)
    /// Обратите внимание — даже при использовании параметра offset для получения информации доступны только первые 1000 результатов
    /// </summary>
    function Count(const Value: Int64 = 20): TVkParamsGroupsSearch;
  end;

  TVkParamsGroupsSetCallbackSettings = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Версия Callback API
    /// </summary>
    function ApiVersion(const Value: string): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Идентификатор сервера
    /// </summary>
    function ServerId(const Value: Int64): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о добавлении новой аудиозаписи
    /// </summary>
    function AudioNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об удалении комментария в обсуждении
    /// </summary>
    function BoardPostDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о редактировании комментария в обсуждении
    /// </summary>
    function BoardPostEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о создании комментария в обсуждении
    /// </summary>
    function BoardPostNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление о восстановлении комментария в обсуждении
    /// </summary>
    function BoardPostRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление о создании подписки
    /// </summary>
    function DonutSubscriptionCreate(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление о продлении подписки
    /// </summary>
    function DonutSubscriptionProlonged(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление об отмене подписки
    /// </summary>
    function DonutSubscriptionCancelled(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление об изменении стоимости подписки
    /// </summary>
    function DonutSubscriptionPriceChanged(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление о том, что подписка истекла
    /// </summary>
    function DonutSubscriptionExpired(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление о выводе денег
    /// </summary>
    function DonutMoneyWithdraw(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление об ошибке при выводе денег
    /// </summary>
    function DonutMoneyWithdrawError(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об изменении главной фотографии
    /// </summary>
    function GroupChangePhoto(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об изменении настроек
    /// </summary>
    function GroupChangeSettings(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о вступлении в сообщество
    /// </summary>
    function GroupJoin(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о выходе из сообщества
    /// </summary>
    function GroupLeave(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об изменении руководства
    /// </summary>
    function GroupOfficersEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о заполнении формы
    /// </summary>
    function LeadFormsNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о новой отметке "Мне нравится"
    /// </summary>
    function LikeAdd(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о снятии отметки "Мне нравится"
    /// </summary>
    function LikeRemove(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об удалении комментария к товару
    /// </summary>
    function MarketCommentDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о редактировании комментария к товару
    /// </summary>
    function MarketCommentEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о добавлении нового комментария к товару
    /// </summary>
    function MarketCommentNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о восстановлении комментария к товару
    /// </summary>
    function MarketCommentRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление о новом заказе
    /// </summary>
    function MarketOrderNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомление о редактировании заказа
    /// </summary>
    function MarketOrderEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о подписке на сообщения
    /// </summary>
    function MessageAllow(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о запрете на сообщения
    /// </summary>
    function MessageDeny(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о редактировании сообщения
    /// </summary>
    function MessageEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// message_event
    /// </summary>
    function MessageEvent(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о новых сообщениях
    /// </summary>
    function MessageNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об исходящем сообщении
    /// </summary>
    function MessageReply(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о наборе текста сообщения
    /// </summary>
    function MessageTypingState(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об удалении комментария к фото
    /// </summary>
    function PhotoCommentDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о редактировании комментария к фото
    /// </summary>
    function PhotoCommentEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о добавлении нового комментария к фото
    /// </summary>
    function PhotoCommentNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о восстановлении комментария к фото
    /// </summary>
    function PhotoCommentRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о добавлении новой фотографии
    /// </summary>
    function PhotoNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о новом голосе в публичных опросах
    /// </summary>
    function PollVoteNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об внесении пользователя в чёрный список
    /// </summary>
    function UserBlock(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об исключении пользователя из чёрного списка
    /// </summary>
    function UserUnblock(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об удалении комментария к видео
    /// </summary>
    function VideoCommentDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о редактировании комментария к видео
    /// </summary>
    function VideoCommentEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о добавлении нового комментария к видео
    /// </summary>
    function VideoCommentNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о восстановлении комментария к видео
    /// </summary>
    function VideoCommentRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о добавлении новой видеозаписи
    /// </summary>
    function VideoNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о новой записи на стене
    /// </summary>
    function WallPostNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления об удалении комментария на стене
    /// </summary>
    function WallReplyDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о редактировании комментария на стене
    /// </summary>
    function WallReplyEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о добавлении нового комментария на стене
    /// </summary>
    function WallReplyNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о восстановлении комментария на стене
    /// </summary>
    function WallReplyRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
    /// <summary>
    /// Уведомления о репосте записи
    /// </summary>
    function WallRepost(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
  end;

  TVkParamsGroupsSetLongpollSettings = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// True — включить Bots Long Poll, False — отключить
    /// </summary>
    function Enabled(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Версия API
    /// </summary>
    function ApiVersion(const Value: string): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о добавлении новой аудиозаписи
    /// </summary>
    function AudioNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об удалении комментария в обсуждении
    /// </summary>
    function BoardPostDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о редактировании комментария в обсуждении
    /// </summary>
    function BoardPostEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о создании комментария в обсуждении
    /// </summary>
    function BoardPostNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление о восстановлении комментария в обсуждении
    /// </summary>
    function BoardPostRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление о создании подписки
    /// </summary>
    function DonutSubscriptionCreate(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление о продлении подписки
    /// </summary>
    function DonutSubscriptionProlonged(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление об отмене подписки
    /// </summary>
    function DonutSubscriptionCancelled(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление об изменении стоимости подписки
    /// </summary>
    function DonutSubscriptionPriceChanged(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление о том, что подписка истекла
    /// </summary>
    function DonutSubscriptionExpired(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление о выводе денег
    /// </summary>
    function DonutMoneyWithdraw(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление об ошибке при выводе денег
    /// </summary>
    function DonutMoneyWithdrawError(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об изменении главной фотографии
    /// </summary>
    function GroupChangePhoto(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об изменении настроек
    /// </summary>
    function GroupChangeSettings(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о вступлении в сообщество
    /// </summary>
    function GroupJoin(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о выходе из сообщества
    /// </summary>
    function GroupLeave(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об изменении руководства
    /// </summary>
    function GroupOfficersEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о заполнении формы
    /// </summary>
    function LeadFormsNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о новой отметке "Мне нравится"
    /// </summary>
    function LikeAdd(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о снятии отметки "Мне нравится"
    /// </summary>
    function LikeRemove(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об удалении комментария к товару
    /// </summary>
    function MarketCommentDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о редактировании комментария к товару
    /// </summary>
    function MarketCommentEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о добавлении нового комментария к товару
    /// </summary>
    function MarketCommentNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о восстановлении комментария к товару
    /// </summary>
    function MarketCommentRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление о новом заказе
    /// </summary>
    function MarketOrderNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомление о редактировании заказа
    /// </summary>
    function MarketOrderEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о подписке на сообщения
    /// </summary>
    function MessageAllow(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о запрете на сообщения
    /// </summary>
    function MessageDeny(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о редактировании сообщения
    /// </summary>
    function MessageEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// message_event
    /// </summary>
    function MessageEvent(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о новых сообщениях
    /// </summary>
    function MessageNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об исходящем сообщении
    /// </summary>
    function MessageReply(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о наборе текста сообщения
    /// </summary>
    function MessageTypingState(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об удалении комментария к фото
    /// </summary>
    function PhotoCommentDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о редактировании комментария к фото
    /// </summary>
    function PhotoCommentEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о добавлении нового комментария к фото
    /// </summary>
    function PhotoCommentNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о восстановлении комментария к фото
    /// </summary>
    function PhotoCommentRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о добавлении новой фотографии
    /// </summary>
    function PhotoNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о новом голосе в публичных опросах
    /// </summary>
    function PollVoteNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об внесении пользователя в чёрный список
    /// </summary>
    function UserBlock(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об исключении пользователя из чёрного списка
    /// </summary>
    function UserUnblock(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об удалении комментария к видео
    /// </summary>
    function VideoCommentDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о редактировании комментария к видео
    /// </summary>
    function VideoCommentEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о добавлении нового комментария к видео
    /// </summary>
    function VideoCommentNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о восстановлении комментария к видео
    /// </summary>
    function VideoCommentRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о добавлении новой видеозаписи
    /// </summary>
    function VideoNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о новой записи на стене
    /// </summary>
    function WallPostNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления об удалении комментария на стене
    /// </summary>
    function WallReplyDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о редактировании комментария на стене
    /// </summary>
    function WallReplyEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о добавлении нового комментария на стене
    /// </summary>
    function WallReplyNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о восстановлении комментария на стене
    /// </summary>
    function WallReplyRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
    /// <summary>
    /// Уведомления о репосте записи
    /// </summary>
    function WallRepost(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
  end;

  TVkParamsToggleMarket = record
    List: TParams;
    function Ref(const Value: string): TVkParamsToggleMarket;
    function UtmSource(const Value: string): TVkParamsToggleMarket;
    function UtmMedium(const Value: string): TVkParamsToggleMarket;
    function UtmCampaign(const Value: string): TVkParamsToggleMarket;
    function UtmContent(const Value: string): TVkParamsToggleMarket;
    function UtmTerm(const Value: string): TVkParamsToggleMarket;
    function PromoCode(const Value: string): TVkParamsToggleMarket;
  end;

  TVkParamsGroupsSetSettings = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsGroupsSetSettings;
    /// <summary>
    /// Сообщения сообщества
    /// </summary>
    function Messages(const Value: Boolean): TVkParamsGroupsSetSettings;
    /// <summary>
    /// Возможности ботов (использование клавиатуры, добавление в беседу)
    /// </summary>
    function BotsCapabilities(const Value: Boolean): TVkParamsGroupsSetSettings;
    /// <summary>
    /// Кнопка «Начать» в диалоге с сообществом.
    /// Работает, в случае если BotsCapabilities = True.
    /// Если эта настройка включена, то при заходе в беседу с Вашим сообществом
    /// в первый раз пользователь увидит кнопку с командой «Начать»,
    /// которая отправляет команду start. Payload этого сообщения будет выглядеть так:
    /// { "command": "start" }
    /// </summary>
    function BotsStartButton(const Value: Boolean): TVkParamsGroupsSetSettings;
    /// <summary>
    /// Добавление бота в беседы.
    /// Работает, в случае если bots_capabilities=1
    /// </summary>
    function BotsAddToChat(const Value: Boolean): TVkParamsGroupsSetSettings;
  end;

  TGroupsController = class(TVkController)
  public
    /// <summary>
    /// Возвращает список участников сообщества
    /// </summary>
    function GetMembers(var Items: TVkProfiles; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список участников сообщества
    /// </summary>
    function GetMembers(var Items: TVkProfiles; Params: TVkParamsGroupsGetMembers): Boolean; overload;
    /// <summary>
    /// Возвращает список id участников сообщества
    /// </summary>
    function GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembers): Boolean; overload;
    /// <summary>
    /// Включает статус «онлайн» в сообществе
    /// </summary>
    function EnableOnline(GroupId: TVkPeerId): Boolean;
    /// <summary>
    /// Выключает статус «онлайн» в сообществе
    /// </summary>
    function DisableOnline(GroupId: TVkPeerId): Boolean;
    /// <summary>
    /// Получает информацию о статусе «онлайн» в сообществе
    /// </summary>
    function GetOnlineStatus(var Value: TVkGroupStatus; GroupId: TVkPeerId): Boolean;
    /// <summary>
    /// Возвращает список сообществ указанного пользователя
    /// </summary>
    function Get(var Items: TVkGroups; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список сообществ указанного пользователя
    /// </summary>
    function Get(var Items: TVkGroups; Params: TVkParamsGroupsGet): Boolean; overload;
    /// <summary>
    /// Возвращает список id сообществ указанного пользователя
    /// </summary>
    function Get(var Items: TVkIdList; Params: TVkParamsGroupsGet): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о том, является ли пользователь участником сообщества
    /// </summary>
    function IsMember(var Items: TVkGroupMemberStates; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о том, является ли пользователь участником сообщества
    /// </summary>
    function IsMember(var Items: TVkGroupMemberStates; Params: TVkParamsGroupsIsMember): Boolean; overload;
    /// <summary>
    ///  Позволяет покинуть сообщество или отклонить приглашение в сообщество
    /// </summary>
    function Leave(GroupId: TVkPeerId): Boolean;
    /// <summary>
    ///  Данный метод позволяет вступить в группу, публичную страницу, а также подтвердить участие во встрече.
    ///  NotSure - опциональный параметр, учитываемый, если GroupId принадлежит встрече.
    ///  True — Возможно пойду. False — Точно пойду
    /// </summary>
    function Join(GroupId: TVkPeerId; NotSure: Boolean = False): Boolean;
    /// <summary>
    ///  Позволяет приглашать друзей в группу
    /// </summary>
    function Invite(GroupId, UserId: TVkPeerId): Boolean;
    /// <summary>
    ///  Позволяет исключить пользователя из группы или отклонить заявку на вступление
    /// </summary>
    function RemoveUser(GroupId, UserId: TVkPeerId): Boolean;
    /// <summary>
    ///  Позволяет одобрить заявку в группу от пользователя
    /// </summary>
    function ApproveRequest(GroupId, UserId: TVkPeerId): Boolean;
    /// <summary>
    ///  Позволяет добавить адрес в сообщество.
    ///  Список адресов может быть получен методом groups.getAddresses
    /// </summary>
    function AddAddress(var Item: TVkGroupAddress; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Позволяет добавить адрес в сообщество.
    ///  Список адресов может быть получен методом groups.getAddresses
    /// </summary>
    function AddAddress(var Item: TVkGroupAddress; Params: TVkParamsGroupsAddAddress): Boolean; overload;
    /// <summary>
    ///  Добавляет сервер для Callback API в сообщество
    /// </summary>
    function AddCallbackServer(var ServerId: Int64; GroupId: TVkPeerId; Url, Title: string; SecretKey: string): Boolean;
    /// <summary>
    ///  Позволяет добавлять ссылки в сообщество
    /// </summary>
    function AddLink(var Item: TVkGroupLink; GroupId: TVkPeerId; Link: string; Text: string = ''): Boolean;
    /// <summary>
    ///  Добавляет пользователя или группу в черный список сообщества
    /// </summary>
    function Ban(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Добавляет пользователя или группу в черный список сообщества
    /// </summary>
    function Ban(Params: TVkParamsGroupsBan): Boolean; overload;
    /// <summary>
    ///  Создает новое сообщество
    /// </summary>
    function Create(var Item: TVkGroup; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Создает новое сообщество
    /// </summary>
    function Create(var Item: TVkGroup; Params: TVkParamsGroupsCreate): Boolean; overload;
    /// <summary>
    ///  Удаляет адрес сообщества
    /// </summary>
    function DeleteAddress(GroupId: TVkPeerId; AddressId: Int64): Boolean;
    /// <summary>
    ///  Удаляет сервер для Callback API из сообщества
    /// </summary>
    function DeleteCallbackServer(GroupId: TVkPeerId; ServerId: Int64): Boolean;
    /// <summary>
    ///  Позволяет удалить ссылки из сообщества
    /// </summary>
    function DeleteLink(GroupId: TVkPeerId; LinkId: Int64): Boolean;
    /// <summary>
    ///  Редактирует сообщество
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Редактирует сообщество
    /// </summary>
    function Edit(Params: TVkParamsGroupsEdit): Boolean; overload;
    /// <summary>
    ///  Позволяет отредактировать адрес в сообществе
    /// </summary>
    function EditAddress(var Item: TVkGroupAddress; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Позволяет отредактировать адрес в сообществе
    /// </summary>
    function EditAddress(var Item: TVkGroupAddress; AddressId: Int64; Params: TVkParamsGroupsEditAddress): Boolean; overload;
    /// <summary>
    ///  Редактирует данные сервера для Callback API в сообществе
    /// </summary>
    function EditCallbackServer(GroupId: TVkPeerId; ServerId: Int64; Url, Title: string; SecretKey: string): Boolean;
    /// <summary>
    ///  Позволяет редактировать ссылки в сообществе
    /// </summary>
    function EditLink(GroupId: TVkPeerId; Link: string; Text: string = ''): Boolean;
    /// <summary>
    ///  Позволяет назначить/разжаловать руководителя в сообществе или изменить уровень его полномочий
    /// </summary>
    function EditManager(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Позволяет назначить/разжаловать руководителя в сообществе или изменить уровень его полномочий
    /// </summary>
    function EditManager(Params: TVkParamsGroupsEditManager): Boolean; overload;
    /// <summary>
    ///  Возвращает адрес указанного сообщества
    /// </summary>
    function GetAddresses(var Item: TVkGroupAddresses; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Возвращает адрес указанного сообщества
    /// </summary>
    function GetAddresses(var Item: TVkGroupAddresses; Params: TVkParamsGroupsGetAddresses): Boolean; overload;
    /// <summary>
    ///  Возвращает список забаненных пользователей и сообществ в сообществе
    /// </summary>
    function GetBanned(var Items: TVkGroupBans; Params: TVkParamsGroupsGetBanned): Boolean; overload;
    /// <summary>
    ///  Возвращает информацию о заданном сообществе или о нескольких сообществах
    /// </summary>
    function GetById(var Items: TVkGroups; GroupIds: TVkPeerIds; Fields: TVkExtendedFields = []): Boolean; overload;
    /// <summary>
    ///  Возвращает информацию о заданном сообществе или о нескольких сообществах
    /// </summary>
    function GetById(var Item: TVkGroup; GroupId: TVkPeerId; Fields: TVkExtendedFields = []): Boolean; overload;
    /// <summary>
    ///  Возвращает информацию о заданном сообществе или о нескольких сообществах
    /// </summary>
    function GetById(var Items: TVkGroups; GroupId: string; Fields: TVkExtendedFields = []): Boolean; overload;
    /// <summary>
    ///  Позволяет получить строку, необходимую для подтверждения адреса сервера в Callback API
    /// </summary>
    function GetCallbackConfirmationCode(var Code: string; GroupId: TVkPeerId): Boolean;
    /// <summary>
    ///  Получает информацию о серверах для Callback API в сообществе
    /// </summary>
    function GetCallbackServers(var Items: TVkGroupCallbackServers; GroupId: TVkPeerId; ServerIds: TIdList = []): Boolean;
    /// <summary>
    ///  Позволяет получить настройки уведомлений Callback API для сообщества
    /// </summary>
    function GetCallbackSettings(var Items: TVkCallbackSettings; GroupId: TVkPeerId; ServerId: Int64): Boolean;
    /// <summary>
    ///  Возвращает список сообществ выбранной категории каталога
    /// </summary>
    function GetCatalog(var Items: TVkGroups; CategoryId: Int64 = 0; SubcategoryId: Int64 = 0): Boolean;
    /// <summary>
    ///  Возвращает список категорий для каталога сообществ
    /// </summary>
    function GetCatalogInfo(var Items: TVkGroupCategories; Subcategories: Boolean = False; Extended: Boolean = False): Boolean;
    /// <summary>
    ///  Возвращает список пользователей, которые были приглашены в группу
    /// </summary>
    function GetInvitedUsers(var Items: TVkProfiles; Params: TVkParamsGroupsGetInvitedUsers): Boolean;
    /// <summary>
    ///  Данный метод возвращает список приглашений в сообщества и встречи текущего пользователя
    /// </summary>
    function GetInvites(var Items: TVkInvitesGroups; Extended: Boolean = False; Count: Int64 = 20; Offset: Int64 = 0): Boolean;
    /// <summary>
    ///  Возвращает данные для подключения к Bots Longpoll API
    /// </summary>
    function GetLongPollServer(var Item: TVkLongpollData; GroupId: TVkPeerId): Boolean;
    /// <summary>
    ///  Получает настройки Bots Longpoll API для сообщества
    /// </summary>
    function GetLongPollSettings(var Item: TVkLongpollSettings; GroupId: TVkPeerId): Boolean;
    /// <summary>
    ///  Возвращает список заявок на вступление в сообщество
    /// </summary>
    function GetRequests(var Items: TVkProfiles; GroupId: TVkPeerId; Fields: TVkExtendedFields = [TVkExtendedField.Domain]; Count: Int64 = 20; Offset: Int64 = 0): Boolean; overload;
    /// <summary>
    ///  Возвращает список заявок на вступление в сообщество
    /// </summary>
    function GetRequestsIds(var Items: TVkIdList; GroupId: TVkPeerId; Count: Int64 = 20; Offset: Int64 = 0): Boolean; overload;
    /// <summary>
    ///  Позволяет получать данные, необходимые для отображения страницы редактирования данных сообщества
    /// </summary>
    function GetSettings(var Item: TVkGroupSettings; GroupId: TVkPeerId): Boolean;
    /// <summary>
    ///  Возвращает список тегов сообщества
    /// </summary>
    function GetTagList(var Items: TVkGroupTags; GroupId: TVkPeerId): Boolean;
    /// <summary>
    ///  Возвращает настройки прав для ключа доступа сообщества
    /// </summary>
    function GetTokenPermissions(var Items: TVkTokenPermissions): Boolean;
    /// <summary>
    ///  Позволяет менять местоположение ссылки в списке
    /// </summary>
    function ReorderLink(GroupId: TVkPeerId; LinkId: Int64; After: Int64): Boolean;
    /// <summary>
    ///  Осуществляет поиск сообществ по заданной подстроке
    /// </summary>
    function Search(var Items: TVkGroups; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Осуществляет поиск сообществ по заданной подстроке
    /// </summary>
    function Search(var Items: TVkGroups; Params: TVkParamsGroupsSearch): Boolean; overload;
    /// <summary>
    ///  Позволяет задать настройки уведомлений о событиях в Callback API
    /// </summary>
    function SetCallbackSettings(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Позволяет задать настройки уведомлений о событиях в Callback API
    /// </summary>
    function SetCallbackSettings(Params: TVkParamsGroupsSetCallbackSettings): Boolean; overload;
    /// <summary>
    ///  Задаёт настройки для Bots Long Poll API в сообществе
    /// </summary>
    function SetLongPollSettings(Params: TParams): Boolean; overload;
    /// <summary>
    ///  Задаёт настройки для Bots Long Poll API в сообществе
    /// </summary>
    function SetLongPollSettings(Params: TVkParamsGroupsSetLongpollSettings): Boolean; overload;
    /// <summary>
    ///  Устанавливает настройки сообщества
    /// </summary>
    function SetSettings(Params: TVkParamsGroupsSetSettings): Boolean;
    /// <summary>
    ///  Позволяет создать или отредактировать заметку о пользователе в рамках переписки пользователя с сообществом
    /// </summary>
    function SetUserNote(GroupId, UserId: TVkPeerId; Note: string): Boolean;
    /// <summary>
    ///  Позволяет добавить новый тег в сообщество
    /// </summary>
    function TagAdd(GroupId: TVkPeerId; TagName: string; TagColor: TVkGroupTagColor): Boolean;
    /// <summary>
    ///  Позволяет "привязывать" и "отвязывать" теги сообщества к беседам
    /// </summary>
    function TagBind(GroupId: TVkPeerId; TagId: Int64; UserId: TVkPeerId; Act: TVkGroupTagAct): Boolean;
    /// <summary>
    ///  Позволяет удалить тег сообщества
    /// </summary>
    function TagDelete(GroupId: TVkPeerId; TagId: Int64): Boolean;
    /// <summary>
    ///  Позволяет переименовать существующий тег
    /// </summary>
    function TagUpdate(GroupId: TVkPeerId; TagId: Int64; const TagName: string): Boolean;
    /// <summary>
    ///  Переключает функционал раздела «Товаров» в выбранной группе.
    /// </summary>
    function ToggleMarket(GroupId: TVkPeerId; State: TVkGroupMarketState; Params: TParams): Boolean; overload;
    /// <summary>
    ///  Переключает функционал раздела «Товаров» в выбранной группе.
    /// </summary>
    function ToggleMarket(GroupId: TVkPeerId; State: TVkGroupMarketState; Params: TVkParamsToggleMarket): Boolean; overload;
    /// <summary>
    ///  Убирает пользователя или группу из черного списка сообщества
    /// </summary>
    function Unban(GroupId, OwnerId: TVkPeerId): Boolean;
  end;

implementation

uses
  System.DateUtils;

{ TGroupsController }

function TGroupsController.AddAddress(var Item: TVkGroupAddress; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.addAddress', Params).GetObject(Item);
end;

function TGroupsController.AddAddress(var Item: TVkGroupAddress; Params: TVkParamsGroupsAddAddress): Boolean;
begin
  Result := AddAddress(Item, Params.List);
end;

function TGroupsController.AddCallbackServer(var ServerId: Int64; GroupId: TVkPeerId; Url, Title, SecretKey: string): Boolean;
begin
  Result := Handler.Execute('groups.addCallbackServer', [
    ['group_id', GroupId.ToString],
    ['url', Url],
    ['title', Title],
    ['secret_key', SecretKey]]).
    GetValue('server_id', ServerId);
end;

function TGroupsController.AddLink(var Item: TVkGroupLink; GroupId: TVkPeerId; Link, Text: string): Boolean;
begin
  Result := Handler.Execute('groups.addLink', [
    ['GroupId', GroupId.ToString],
    ['link', Link],
    ['text', Text]]).
    GetObject(Item);
end;

function TGroupsController.ApproveRequest(GroupId, UserId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.approveRequest', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.Ban(Params: TVkParamsGroupsBan): Boolean;
begin
  Result := Ban(Params.List);
end;

function TGroupsController.Create(var Item: TVkGroup; Params: TVkParamsGroupsCreate): Boolean;
begin
  Result := Create(Item, Params.List);
end;

function TGroupsController.Create(var Item: TVkGroup; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.create', Params).GetObject(Item);
end;

function TGroupsController.Ban(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.ban', Params).ResponseIsTrue;
end;

function TGroupsController.DeleteAddress(GroupId: TVkPeerId; AddressId: Int64): Boolean;
begin
  Result := Handler.Execute('groups.deleteAddress', [
    ['group_id', GroupId.ToString],
    ['address_id', AddressId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.DeleteCallbackServer(GroupId: TVkPeerId; ServerId: Int64): Boolean;
begin
  Result := Handler.Execute('groups.deleteCallbackServer', [
    ['group_id', GroupId.ToString],
    ['server_id', ServerId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.DeleteLink(GroupId: TVkPeerId; LinkId: Int64): Boolean;
begin
  Result := Handler.Execute('groups.deleteLink', [
    ['group_id', GroupId.ToString],
    ['link_id', LinkId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.DisableOnline(GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.disableOnline', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TGroupsController.Edit(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.edit', Params).ResponseIsTrue;
end;

function TGroupsController.Edit(Params: TVkParamsGroupsEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TGroupsController.EditAddress(var Item: TVkGroupAddress; AddressId: Int64; Params: TVkParamsGroupsEditAddress): Boolean;
begin
  Params.List.Add('address_id', AddressId);
  Result := EditAddress(Item, Params.List);
end;

function TGroupsController.EditCallbackServer(GroupId: TVkPeerId; ServerId: Int64; Url, Title, SecretKey: string): Boolean;
begin
  Result := Handler.Execute('groups.editCallbackServer', [
    ['group_id', GroupId.ToString],
    ['server_id', ServerId.ToString],
    ['url', Url],
    ['title', Title],
    ['secret_key', SecretKey]]).
    ResponseIsTrue;
end;

function TGroupsController.EditLink(GroupId: TVkPeerId; Link, Text: string): Boolean;
begin
  Result := Handler.Execute('groups.editLink', [
    ['GroupId', GroupId.ToString],
    ['link', Link],
    ['text', Text]]).
    ResponseIsTrue;
end;

function TGroupsController.EditManager(Params: TVkParamsGroupsEditManager): Boolean;
begin
  Result := EditManager(Params.List);
end;

function TGroupsController.EditManager(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.editManager', Params).ResponseIsTrue;
end;

function TGroupsController.EditAddress(var Item: TVkGroupAddress; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.editAddress', Params).ResponseIsTrue;
end;

function TGroupsController.EnableOnline(GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.enableOnline', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TGroupsController.Get(var Items: TVkGroups; Params: TVkParamsGroupsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TGroupsController.GetAddresses(var Item: TVkGroupAddresses; Params: TVkParamsGroupsGetAddresses): Boolean;
begin
  Result := GetAddresses(Item, Params.List);
end;

function TGroupsController.GetBanned(var Items: TVkGroupBans; Params: TVkParamsGroupsGetBanned): Boolean;
begin
  Result := Handler.Execute('groups.getBanned', Params.List).GetObject(Items);
end;

function TGroupsController.GetById(var Item: TVkGroup; GroupId: TVkPeerId; Fields: TVkExtendedFields): Boolean;
var
  Items: TVkGroups;
begin
  Result := GetById(Items, [GroupId], Fields);
  if Result then
  try
    if Length(Items.Groups) > 0 then
    begin
      Item := Items.Groups[0];
      Items.Groups[0] := nil;
    end
    else
      Result := False;
  finally
    Items.Free;
  end;
end;

function TGroupsController.GetCallbackConfirmationCode(var Code: string; GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.getCallbackConfirmationCode', ['group_id', GroupId.ToString]).GetValue('code', Code);
end;

function TGroupsController.GetCallbackServers(var Items: TVkGroupCallbackServers; GroupId: TVkPeerId; ServerIds: TIdList): Boolean;
begin
  Result := Handler.Execute('groups.getCallbackServers', [
    ['group_id', GroupId.ToString],
    ['server_ids', ServerIds.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetCallbackSettings(var Items: TVkCallbackSettings; GroupId: TVkPeerId; ServerId: Int64): Boolean;
begin
  Result := Handler.Execute('groups.getCallbackSettings', [
    ['group_id', GroupId.ToString],
    ['server_id', ServerId.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetCatalog(var Items: TVkGroups; CategoryId, SubcategoryId: Int64): Boolean;
begin
  Result := Handler.Execute('groups.getById', [
    ['category_id', CategoryId.ToString],
    ['subcategory_id', SubcategoryId.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetCatalogInfo(var Items: TVkGroupCategories; Subcategories, Extended: Boolean): Boolean;
begin
  Result := Handler.Execute('groups.getCatalogInfo', [
    ['subcategories', BoolToString(Subcategories)],
    ['extended', BoolToString(Extended)]]).
    GetObject(Items);
end;

function TGroupsController.GetById(var Items: TVkGroups; GroupId: string; Fields: TVkExtendedFields): Boolean;
begin
  Result := Handler.Execute('groups.getById', [
    ['group_ids', GroupId],
    ['fields', Fields.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetById(var Items: TVkGroups; GroupIds: TVkPeerIds; Fields: TVkExtendedFields): Boolean;
begin
  Result := Handler.Execute('groups.getById', [
    ['group_ids', GroupIds.ToString],
    ['fields', Fields.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetAddresses(var Item: TVkGroupAddresses; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.getAddresses', Params).GetObject(Item);
end;

function TGroupsController.Get(var Items: TVkGroups; Params: TParams): Boolean;
begin
  Params.Add('extended', True);
  Result := Handler.Execute('groups.get', Params).GetObject(Items);
end;

function TGroupsController.Get(var Items: TVkIdList; Params: TVkParamsGroupsGet): Boolean;
begin
  Params.List.Add('extended', False);
  Result := Handler.Execute('groups.get', Params.List).GetObject(Items);
end;

function TGroupsController.GetInvitedUsers(var Items: TVkProfiles; Params: TVkParamsGroupsGetInvitedUsers): Boolean;
begin
  Result := Handler.Execute('groups.getInvitedUsers', Params.List).GetObject(Items);
end;

function TGroupsController.GetInvites(var Items: TVkInvitesGroups; Extended: Boolean; Count, Offset: Int64): Boolean;
begin
  Result := Handler.Execute('groups.getInvites', [
    ['extended', BoolToString(Extended)],
    ['count', Count.ToString],
    ['offset', Offset.ToString]]).
    GetObject(Items);
end;

function TGroupsController.GetLongPollServer(var Item: TVkLongpollData; GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.getLongPollServer', ['group_id', GroupId.ToString]).GetObject(Item);
end;

function TGroupsController.GetLongPollSettings(var Item: TVkLongpollSettings; GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.getLongPollSettings', ['group_id', GroupId.ToString]).GetObject(Item);
end;

function TGroupsController.GetMembers(var Items: TVkProfiles; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  if not Params.List.KeyExists('fields') then
    Params.Fields([TVkExtendedField.Domain]);
  Result := GetMembers(Items, Params.List);
end;

function TGroupsController.GetMembers(var Items: TVkProfiles; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.getMembers', Params).GetObject(Items);
end;

function TGroupsController.GetMembersIds(var Items: TVkIdList; Params: TVkParamsGroupsGetMembers): Boolean;
begin
  Params.Fields([]);
  Result := Handler.Execute('groups.getMembers', Params.List).GetObject(Items);
end;

function TGroupsController.GetOnlineStatus(var Value: TVkGroupStatus; GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.getOnlineStatus', ['group_id', GroupId.ToString]).GetObject(Value);
end;

function TGroupsController.GetRequests(var Items: TVkProfiles; GroupId: TVkPeerId; Fields: TVkExtendedFields; Count, Offset: Int64): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  if Fields = [] then
    Fields := [TVkExtendedField.Domain];
  Params.Add('fields', Fields.ToString);
  Result := Handler.Execute('groups.getRequests', Params).GetObject(Items);
end;

function TGroupsController.GetRequestsIds(var Items: TVkIdList; GroupId: TVkPeerId; Count, Offset: Int64): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  Result := Handler.Execute('groups.getRequests', Params).GetObject(Items);
end;

function TGroupsController.GetSettings(var Item: TVkGroupSettings; GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.getSettings', ['group_id', GroupId.ToString]).GetObject(Item);
end;

function TGroupsController.GetTagList(var Items: TVkGroupTags; GroupId: TVkPeerId): Boolean;
begin
  with Handler.Execute('groups.getTagList', ['group_id', GroupId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkGroupTags.FromJsonString<TVkGroupTags>(ResponseAsItems);
        Items.Count := Length(Items.Items);
      except
        Result := False;
      end;
    end;
  end;
end;

function TGroupsController.GetTokenPermissions(var Items: TVkTokenPermissions): Boolean;
begin
  Result := Handler.Execute('groups.getTokenPermissions').GetObject(Items);
end;

function TGroupsController.Invite(GroupId, UserId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.invite', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.IsMember(var Items: TVkGroupMemberStates; Params: TVkParamsGroupsIsMember): Boolean;
begin
  Result := IsMember(Items, Params.List);
end;

function TGroupsController.Join(GroupId: TVkPeerId; NotSure: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  if NotSure then
    Params.Add('not_sure', NotSure);
  Result := Handler.Execute('groups.join', Params).ResponseIsTrue;
end;

function TGroupsController.Leave(GroupId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.leave', ['group_id', GroupId.ToString]).ResponseIsTrue;
end;

function TGroupsController.RemoveUser(GroupId, UserId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.removeUser', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.ReorderLink(GroupId: TVkPeerId; LinkId, After: Int64): Boolean;
begin
  Result := Handler.Execute('groups.reorderLink', [
    ['group_id', GroupId.ToString],
    ['link_id', LinkId.ToString],
    ['after', After.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.Search(var Items: TVkGroups; Params: TVkParamsGroupsSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TGroupsController.SetCallbackSettings(Params: TVkParamsGroupsSetCallbackSettings): Boolean;
begin
  Result := SetCallbackSettings(Params.List);
end;

function TGroupsController.SetCallbackSettings(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.setCallbackSettings', Params).ResponseIsTrue;
end;

function TGroupsController.SetLongPollSettings(Params: TVkParamsGroupsSetLongpollSettings): Boolean;
begin
  Result := SetLongPollSettings(Params.List);
end;

function TGroupsController.SetSettings(Params: TVkParamsGroupsSetSettings): Boolean;
begin
  Result := Handler.Execute('groups.setSettings', Params.List).ResponseIsTrue;
end;

function TGroupsController.SetUserNote(GroupId, UserId: TVkPeerId; Note: string): Boolean;
begin
  Result := Handler.Execute('groups.setLongPollSettings', [
    ['group_id', GroupId.ToString],
    ['user_id', UserId.ToString],
    ['note', string(Note)]]).
    ResponseIsTrue;
end;

function TGroupsController.TagAdd(GroupId: TVkPeerId; TagName: string; TagColor: TVkGroupTagColor): Boolean;
begin
  Result := Handler.Execute('groups.tagAdd', [
    ['group_id', GroupId.ToString],
    ['tag_name', TagName],
    ['tag_color', TagColor]]).
    ResponseIsTrue;
end;

function TGroupsController.TagBind(GroupId: TVkPeerId; TagId: Int64; UserId: TVkPeerId; Act: TVkGroupTagAct): Boolean;
begin
  Result := Handler.Execute('groups.tagBind', [
    ['group_id', GroupId.ToString],
    ['tag_id', TagId.ToString],
    ['user_id', UserId.ToString],
    ['act', Act.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.TagDelete(GroupId: TVkPeerId; TagId: Int64): Boolean;
begin
  Result := Handler.Execute('groups.tagDelete', [
    ['group_id', GroupId.ToString],
    ['tag_id', TagId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.TagUpdate(GroupId: TVkPeerId; TagId: Int64; const TagName: string): Boolean;
begin
  Result := Handler.Execute('groups.tagUpdate', [
    ['group_id', GroupId.ToString],
    ['tag_name', TagName],
    ['tag_id', TagId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.ToggleMarket(GroupId: TVkPeerId; State: TVkGroupMarketState; Params: TVkParamsToggleMarket): Boolean;
begin
  Result := Handler.Execute('groups.toggleMarket', Params.List).ResponseIsTrue;
end;

function TGroupsController.ToggleMarket(GroupId: TVkPeerId; State: TVkGroupMarketState; Params: TParams): Boolean;
begin
  Params.Add('group_id', GroupId);
  Params.Add('state', State.ToString);
  Result := Handler.Execute('groups.toggleMarket', Params).ResponseIsTrue;
end;

function TGroupsController.Unban(GroupId, OwnerId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('groups.unban', [
    ['group_id', GroupId.ToString],
    ['owner_id', OwnerId.ToString]]).
    ResponseIsTrue;
end;

function TGroupsController.SetLongPollSettings(Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.setLongPollSettings', Params).ResponseIsTrue;
end;

function TGroupsController.Search(var Items: TVkGroups; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.search', Params).GetObject(Items);
end;

function TGroupsController.IsMember(var Items: TVkGroupMemberStates; Params: TParams): Boolean;
begin
  Result := Handler.Execute('groups.isMember', Params).GetObjects(Items);
end;

{ TVkGetMembersParams }

function TVkParamsGroupsGetMembers.Count(const Value: Int64): TVkParamsGroupsGetMembers;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsGroupsGetMembers.Fields(const Value: TVkExtendedFields): TVkParamsGroupsGetMembers;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsGetMembers.Filter(const Value: TVkGroupMembersFilter): TVkParamsGroupsGetMembers;
begin
  List.Add('filter', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsGetMembers.GroupId(const Value: string): TVkParamsGroupsGetMembers;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsGetMembers.GroupId(const Value: TVkPeerId): TVkParamsGroupsGetMembers;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsGetMembers.Offset(const Value: Int64): TVkParamsGroupsGetMembers;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGroupsGetMembers.Sort(const Value: TVkSortIdTime): TVkParamsGroupsGetMembers;
begin
  List.Add('sort', Value.ToString);
  Result := Self;
end;

{ TVkGroupsGetParams }

function TVkParamsGroupsGet.Count(const Value: Int64): TVkParamsGroupsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsGroupsGet.Fields(const Value: TVkExtendedFields): TVkParamsGroupsGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsGet.Filter(const Value: TVkGroupFilters): TVkParamsGroupsGet;
begin
  List.Add('filter', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsGet.Offset(const Value: Int64): TVkParamsGroupsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGroupsGet.UserId(const Value: TVkPeerId): TVkParamsGroupsGet;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsGroupsIsMember }

function TVkParamsGroupsIsMember.Extended(const Value: Boolean): TVkParamsGroupsIsMember;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsGroupsIsMember.GroupId(const Value: TVkPeerId): TVkParamsGroupsIsMember;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsIsMember.GroupId(const Value: string): TVkParamsGroupsIsMember;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsIsMember.UserId(const Value: TVkPeerId): TVkParamsGroupsIsMember;
begin
  List.Add('user_ids', Value);
  Result := Self;
end;

function TVkParamsGroupsIsMember.UserIds(const Value: TVkPeerIds): TVkParamsGroupsIsMember;
begin
  List.Add('user_ids', Value);
  Result := Self;
end;

{ TVkParamsGroupsAddAddress }

function TVkParamsGroupsAddAddress.AdditionalAddress(const Value: string): TVkParamsGroupsAddAddress;
begin
  List.Add('additional_address', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.Address(const Value: string): TVkParamsGroupsAddAddress;
begin
  List.Add('address', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.CityId(const Value: Int64): TVkParamsGroupsAddAddress;
begin
  List.Add('city_id', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.CountryId(const Value: Int64): TVkParamsGroupsAddAddress;
begin
  List.Add('country_id', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.GroupId(const Value: TVkPeerId): TVkParamsGroupsAddAddress;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.IsMainAddress(const Value: Boolean): TVkParamsGroupsAddAddress;
begin
  List.Add('is_main_address', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.Latitude(const Value: Extended): TVkParamsGroupsAddAddress;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.Longitude(const Value: Extended): TVkParamsGroupsAddAddress;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.MetroId(const Value: Int64): TVkParamsGroupsAddAddress;
begin
  List.Add('metro_id', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.Phone(const Value: string): TVkParamsGroupsAddAddress;
begin
  List.Add('phone', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.Timetable(const Value: TVkTimeTable): TVkParamsGroupsAddAddress;
begin
  List.Add('timetable', Value.ToJSON);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.Title(const Value: string): TVkParamsGroupsAddAddress;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsGroupsAddAddress.WorkInfoStatus(const Value: TVkWorkInfoStatus): TVkParamsGroupsAddAddress;
begin
  List.Add('work_info_status', Value.ToString);
  Result := Self;
end;

{ TVkParamsGroupsBan }

function TVkParamsGroupsBan.Comment(const Value: string): TVkParamsGroupsBan;
begin
  List.Add('comment', Value);
  Result := Self;
end;

function TVkParamsGroupsBan.CommentVisible(const Value: Boolean): TVkParamsGroupsBan;
begin
  List.Add('comment_visible', Value);
  Result := Self;
end;

function TVkParamsGroupsBan.EndDate(const Value: TDateTime): TVkParamsGroupsBan;
begin
  List.Add('end_date', Value);
  Result := Self;
end;

function TVkParamsGroupsBan.GroupId(const Value: TVkPeerId): TVkParamsGroupsBan;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsBan.OwnerId(const Value: TVkPeerId): TVkParamsGroupsBan;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsGroupsBan.Reason(const Value: TVkUserBlockReason): TVkParamsGroupsBan;
begin
  List.Add('reason', Ord(Value));
  Result := Self;
end;

{ TVkParamsGroupsCreate }

function TVkParamsGroupsCreate.Description(const Value: string): TVkParamsGroupsCreate;
begin
  List.Add('description', Value);
  Result := Self;
end;

function TVkParamsGroupsCreate.PublicCategory(const Value: Int64): TVkParamsGroupsCreate;
begin
  List.Add('public_category', Value);
  Result := Self;
end;

function TVkParamsGroupsCreate.Subtype(const Value: TVkGroupSubType): TVkParamsGroupsCreate;
begin
  List.Add('subtype', Ord(Value));
  Result := Self;
end;

function TVkParamsGroupsCreate.Title(const Value: string): TVkParamsGroupsCreate;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsGroupsCreate.&Type(const Value: TVkGroupTypeCreate): TVkParamsGroupsCreate;
begin
  List.Add('type', Value.ToString);
  Result := Self;
end;

{ TVkParamsGroupsEdit }

function TVkParamsGroupsEdit.Access(const Value: TVkGroupAccess): TVkParamsGroupsEdit;
begin
  List.Add('access', Ord(Value));
  Result := Self;
end;

function TVkParamsGroupsEdit.Addresses(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('addresses', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.AgeLimits(const Value: TVkAgeLimits): TVkParamsGroupsEdit;
begin
  List.Add('age_limits', Ord(Value));
  Result := Self;
end;

function TVkParamsGroupsEdit.Articles(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('articles', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Audio(const Value: TGroupSectionAudio): TVkParamsGroupsEdit;
begin
  List.Add('audio', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.City(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('city', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Contacts(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('contacts', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Country(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('country', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Description(const Value: string): TVkParamsGroupsEdit;
begin
  List.Add('description', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Docs(const Value: TGroupSectionDocs): TVkParamsGroupsEdit;
begin
  List.Add('docs', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Email(const Value: string): TVkParamsGroupsEdit;
begin
  List.Add('email', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.EventFinishDate(const Value: TDateTime): TVkParamsGroupsEdit;
begin
  List.Add('event_finish_date', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.EventGroupId(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('event_group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Events(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('events', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.EventStartDate(const Value: TDateTime): TVkParamsGroupsEdit;
begin
  List.Add('event_start_date', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.GroupId(const Value: TVkPeerId): TVkParamsGroupsEdit;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Links(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('links', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.MainSection(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('main_section', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Market(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('market', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.MarketCity(const Value: TIdList): TVkParamsGroupsEdit;
begin
  List.Add('market_city', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.MarketComments(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('market_comments', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.MarketContact(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('market_contact', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.MarketCountry(const Value: TIdList): TVkParamsGroupsEdit;
begin
  List.Add('market_country', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.MarketCurrency(const Value: TVkCurrency): TVkParamsGroupsEdit;
begin
  List.Add('market_currency', Value.ToConst);
  Result := Self;
end;

function TVkParamsGroupsEdit.MarketWiki(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('market_wiki', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Messages(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('messages', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.ObsceneFilter(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('obscene_filter', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.ObsceneStopwords(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('obscene_stopwords', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.ObsceneWords(const Value: TArrayOfString): TVkParamsGroupsEdit;
begin
  List.Add('obscene_words', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Phone(const Value: string): TVkParamsGroupsEdit;
begin
  List.Add('phone', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Photos(const Value: TGroupSectionPhotos): TVkParamsGroupsEdit;
begin
  List.Add('photos', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Places(const Value: Boolean): TVkParamsGroupsEdit;
begin
  List.Add('places', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.PublicCategory(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('public_category', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.PublicDate(const Value: TDateTime): TVkParamsGroupsEdit;
begin
  List.Add('public_date', Value, 'DD.MM.YYYY');
  Result := Self;
end;

function TVkParamsGroupsEdit.PublicSubcategory(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('public_subcategory', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Rss(const Value: string): TVkParamsGroupsEdit;
begin
  List.Add('rss', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.ScreenName(const Value: string): TVkParamsGroupsEdit;
begin
  List.Add('screen_name', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.SecondarySection(const Value: Int64): TVkParamsGroupsEdit;
begin
  List.Add('secondary_section', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Subject(const Value: TVkGroupSubjectType): TVkParamsGroupsEdit;
begin
  List.Add('subject', Ord(Value));
  Result := Self;
end;

function TVkParamsGroupsEdit.Title(const Value: string): TVkParamsGroupsEdit;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Topics(const Value: TGroupSectionTopics): TVkParamsGroupsEdit;
begin
  List.Add('topics', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Video(const Value: TGroupSectionVideo): TVkParamsGroupsEdit;
begin
  List.Add('video', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Wall(const Value: TGroupSectionWall): TVkParamsGroupsEdit;
begin
  List.Add('wall', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Website(const Value: string): TVkParamsGroupsEdit;
begin
  List.Add('website', Value);
  Result := Self;
end;

function TVkParamsGroupsEdit.Wiki(const Value: TGroupSectionWiki): TVkParamsGroupsEdit;
begin
  List.Add('wiki', Value);
  Result := Self;
end;

{ TVkParamsGroupsEditManager }

function TVkParamsGroupsEditManager.ContactEmail(const Value: string): TVkParamsGroupsEditManager;
begin
  List.Add('contact_email', Value);
  Result := Self;
end;

function TVkParamsGroupsEditManager.ContactPhone(const Value: string): TVkParamsGroupsEditManager;
begin
  List.Add('contact_phone', Value);
  Result := Self;
end;

function TVkParamsGroupsEditManager.ContactPosition(const Value: string): TVkParamsGroupsEditManager;
begin
  List.Add('contact_position', Value);
  Result := Self;
end;

function TVkParamsGroupsEditManager.GroupId(const Value: UInt64): TVkParamsGroupsEditManager;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsEditManager.IsContact(const Value: Boolean): TVkParamsGroupsEditManager;
begin
  List.Add('is_contact', Value);
  Result := Self;
end;

function TVkParamsGroupsEditManager.Role(const Value: TVkGroupRole): TVkParamsGroupsEditManager;
begin
  List.Add('role', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsEditManager.UserId(const Value: TVkPeerId): TVkParamsGroupsEditManager;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsGroupsGetAddresses }

function TVkParamsGroupsGetAddresses.AddressIds(const Value: TIdList): TVkParamsGroupsGetAddresses;
begin
  List.Add('address_ids', Value);
  Result := Self;
end;

function TVkParamsGroupsGetAddresses.AddressIds(const Value: Int64): TVkParamsGroupsGetAddresses;
begin
  List.Add('address_ids', Value);
  Result := Self;
end;

function TVkParamsGroupsGetAddresses.Count(const Value: Int64): TVkParamsGroupsGetAddresses;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsGroupsGetAddresses.Fields(const Value: TVkGroupAddressFields): TVkParamsGroupsGetAddresses;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsGetAddresses.GroupId(const Value: UInt64): TVkParamsGroupsGetAddresses;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsGetAddresses.Latitude(const Value: Extended): TVkParamsGroupsGetAddresses;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsGroupsGetAddresses.Longitude(const Value: Extended): TVkParamsGroupsGetAddresses;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsGroupsGetAddresses.Offset(const Value: Int64): TVkParamsGroupsGetAddresses;
begin
  List.Add('offset', Value);
  Result := Self;
end;

{ TVkParamsGroupsGetBanned }

function TVkParamsGroupsGetBanned.Count(const Value: Int64): TVkParamsGroupsGetBanned;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsGroupsGetBanned.Fields(const Value: TVkExtendedFields): TVkParamsGroupsGetBanned;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsGetBanned.GroupId(const Value: TVkPeerId): TVkParamsGroupsGetBanned;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsGetBanned.Offset(const Value: Int64): TVkParamsGroupsGetBanned;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGroupsGetBanned.OwnerId(const Value: Int64): TVkParamsGroupsGetBanned;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsGroupsGetInvitedUsers }

function TVkParamsGroupsGetInvitedUsers.Count(const Value: Int64): TVkParamsGroupsGetInvitedUsers;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsGroupsGetInvitedUsers.Fields(const Value: TVkExtendedFields): TVkParamsGroupsGetInvitedUsers;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsGetInvitedUsers.GroupId(const Value: TVkPeerId): TVkParamsGroupsGetInvitedUsers;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsGetInvitedUsers.NameCase(const Value: TVkNameCase): TVkParamsGroupsGetInvitedUsers;
begin
  List.Add('name_case', Value.ToString);
  Result := Self;
end;

function TVkParamsGroupsGetInvitedUsers.Offset(const Value: Int64): TVkParamsGroupsGetInvitedUsers;
begin
  List.Add('offset', Value);
  Result := Self;
end;

{ TVkParamsGroupsSearch }

function TVkParamsGroupsSearch.CityId(const Value: Int64): TVkParamsGroupsSearch;
begin
  List.Add('city_id', Value);
  Result := Self;
end;

function TVkParamsGroupsSearch.Count(const Value: Int64): TVkParamsGroupsSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsGroupsSearch.CountryId(const Value: Int64): TVkParamsGroupsSearch;
begin
  List.Add('country_id', Value);
  Result := Self;
end;

function TVkParamsGroupsSearch.Future(const Value: Boolean): TVkParamsGroupsSearch;
begin
  List.Add('future', Value);
  Result := Self;
end;

function TVkParamsGroupsSearch.Market(const Value: Boolean): TVkParamsGroupsSearch;
begin
  List.Add('matket', Value);
  Result := Self;
end;

function TVkParamsGroupsSearch.Offset(const Value: Int64): TVkParamsGroupsSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsGroupsSearch.Query(const Value: string): TVkParamsGroupsSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsGroupsSearch.Sort(const Value: TVkGroupSearchSort): TVkParamsGroupsSearch;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

function TVkParamsGroupsSearch.&Type(const Value: TVkGroupType): TVkParamsGroupsSearch;
begin
  List.Add('type', Value.ToString);
  Result := Self;
end;

{ TVkParamsGroupsSetCallbackSettings }

function TVkParamsGroupsSetCallbackSettings.ApiVersion(const Value: string): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('api_version', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.AudioNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('audio_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('board_post_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('board_post_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('board_post_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.BoardPostRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('board_post_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.DonutMoneyWithdraw(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('donut_money_withdraw', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.DonutMoneyWithdrawError(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('donut_money_withdraw_error', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.DonutSubscriptionCancelled(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('donut_subscription_cancelled', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.DonutSubscriptionCreate(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('donut_subscription_create', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.DonutSubscriptionExpired(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('donut_subscription_expired', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.DonutSubscriptionPriceChanged(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('donut_subscription_price_changed', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.DonutSubscriptionProlonged(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('donut_subscription_prolonged', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.GroupChangePhoto(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('group_change_photo', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.GroupChangeSettings(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('group_change_settings', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.GroupId(const Value: TVkPeerId): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.GroupJoin(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('group_join', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.GroupLeave(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('group_leave', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.GroupOfficersEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('group_officers_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.LeadFormsNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('lead_forms_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.LikeAdd(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('like_add', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.LikeRemove(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('like_remove', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('market_comment_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('market_comment_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('market_comment_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MarketCommentRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('market_comment_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MarketOrderEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('market_order_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MarketOrderNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('market_order_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MessageAllow(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('message_allow', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MessageDeny(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('message_deny', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MessageEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('message_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MessageEvent(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('message_event', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MessageNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('message_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MessageReply(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('message_reply', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.MessageTypingState(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('message_typing_state', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('photo_comment_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('photo_comment_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('photo_comment_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.PhotoCommentRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('photo_comment_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.PhotoNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('post_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.PollVoteNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('post_vote_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.ServerId(const Value: Int64): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('server_id', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.UserBlock(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('user_block', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.UserUnblock(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('user_unblock', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('video_comment_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('video_comment_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('video_comment_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.VideoCommentRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('video_comment_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.VideoNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('video_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.WallPostNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('wall_post_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyDelete(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('wall_reply_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyEdit(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('wall_reply_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyNew(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('wall_reply_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.WallReplyRestore(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('wall_reply_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetCallbackSettings.WallRepost(const Value: Boolean): TVkParamsGroupsSetCallbackSettings;
begin
  List.Add('wall_repost', Value);
  Result := Self;
end;


{ TVkParamsGroupsSetLongpollSettings }

function TVkParamsGroupsSetLongpollSettings.ApiVersion(const Value: string): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('api_version', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.AudioNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('audio_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('board_post_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('board_post_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('board_post_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.BoardPostRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('board_post_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.DonutMoneyWithdraw(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('donut_money_withdraw', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.DonutMoneyWithdrawError(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('donut_money_withdraw_error', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.DonutSubscriptionCancelled(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('donut_subscription_cancelled', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.DonutSubscriptionCreate(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('donut_subscription_create', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.DonutSubscriptionExpired(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('donut_subscription_expired', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.DonutSubscriptionPriceChanged(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('donut_subscription_price_changed', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.DonutSubscriptionProlonged(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('donut_subscription_prolonged', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.Enabled(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('enabled', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.GroupChangePhoto(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('group_change_photo', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.GroupChangeSettings(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('group_change_settings', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.GroupId(const Value: TVkPeerId): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.GroupJoin(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('group_join', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.GroupLeave(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('group_leave', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.GroupOfficersEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('group_officers_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.LeadFormsNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('lead_forms_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.LikeAdd(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('like_add', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.LikeRemove(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('like_remove', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('market_comment_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('market_comment_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('market_comment_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MarketCommentRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('market_comment_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MarketOrderEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('market_order_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MarketOrderNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('market_order_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MessageAllow(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('message_allow', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MessageDeny(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('message_deny', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MessageEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('message_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MessageEvent(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('message_event', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MessageNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('message_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MessageReply(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('message_reply', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.MessageTypingState(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('message_typing_state', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('photo_comment_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('photo_comment_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('photo_comment_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.PhotoCommentRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('photo_comment_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.PhotoNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('post_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.PollVoteNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('post_vote_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.UserBlock(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('user_block', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.UserUnblock(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('user_unblock', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('video_comment_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('video_comment_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('video_comment_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.VideoCommentRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('video_comment_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.VideoNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('video_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.WallPostNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('wall_post_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyDelete(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('wall_reply_delete', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyEdit(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('wall_reply_edit', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyNew(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('wall_reply_new', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.WallReplyRestore(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('wall_reply_restore', Value);
  Result := Self;
end;

function TVkParamsGroupsSetLongpollSettings.WallRepost(const Value: Boolean): TVkParamsGroupsSetLongpollSettings;
begin
  List.Add('wall_repost', Value);
  Result := Self;
end;

{ TVkParamsGroupsSetSettings }

function TVkParamsGroupsSetSettings.BotsAddToChat(const Value: Boolean): TVkParamsGroupsSetSettings;
begin
  List.Add('bots_add_to_chat', Value);
  Result := Self;
end;

function TVkParamsGroupsSetSettings.BotsCapabilities(const Value: Boolean): TVkParamsGroupsSetSettings;
begin
  List.Add('bots_capabilities', Value);
  Result := Self;
end;

function TVkParamsGroupsSetSettings.BotsStartButton(const Value: Boolean): TVkParamsGroupsSetSettings;
begin
  List.Add('bots_start_button', Value);
  Result := Self;
end;

function TVkParamsGroupsSetSettings.GroupId(const Value: TVkPeerId): TVkParamsGroupsSetSettings;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsGroupsSetSettings.Messages(const Value: Boolean): TVkParamsGroupsSetSettings;
begin
  List.Add('messages', Value);
  Result := Self;
end;

{ TVkParamsToggleMarket }

function TVkParamsToggleMarket.PromoCode(const Value: string): TVkParamsToggleMarket;
begin
  List.Add('promocode', Value);
  Result := Self;
end;

function TVkParamsToggleMarket.Ref(const Value: string): TVkParamsToggleMarket;
begin
  List.Add('ref', Value);
  Result := Self;
end;

function TVkParamsToggleMarket.UtmCampaign(const Value: string): TVkParamsToggleMarket;
begin
  List.Add('utm_campaign', Value);
  Result := Self;
end;

function TVkParamsToggleMarket.UtmContent(const Value: string): TVkParamsToggleMarket;
begin
  List.Add('utm_content', Value);
  Result := Self;
end;

function TVkParamsToggleMarket.UtmMedium(const Value: string): TVkParamsToggleMarket;
begin
  List.Add('utm_medium', Value);
  Result := Self;
end;

function TVkParamsToggleMarket.UtmSource(const Value: string): TVkParamsToggleMarket;
begin
  List.Add('utm_source', Value);
  Result := Self;
end;

function TVkParamsToggleMarket.UtmTerm(const Value: string): TVkParamsToggleMarket;
begin
  List.Add('utm_term', Value);
  Result := Self;
end;

end.

