unit VK.Wall;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON, VK.Entity.Media,
  VK.Entity.Info;

type
  TVkParamsWallPost = record
    List: TParams;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// True — комментарии к записи отключены.
    /// False — комментарии к записи включены.
    /// </summary>
    function CloseComments(const Value: Boolean = False): Integer;
    /// <summary>
    /// Источник материала. Поддерживаются внешние и внутренние ссылки
    /// </summary>
    function Copyright(const Value: string): Integer;
    /// <summary>
    /// Период времени в течение которого запись будет доступна для донов — платных подписчиков VK Donut
    /// </summary>
    function DonutPaidDuration(const Value: TVkDonutPaidDuration): Integer;
    /// <summary>
    /// True — запись будет доступна только друзьям, False — всем пользователям.
    /// По умолчанию публикуемые записи доступны всем пользователям
    /// </summary>
    function FriendsOnly(const Value: Boolean = False): Integer;
    /// <summary>
    /// Данный параметр учитывается, если OwnerId меньше 0 (запись публикуется на стене группы).
    /// True — запись будет опубликована от имени группы,
    /// False — запись будет опубликована от имени пользователя (по умолчанию)
    /// </summary>
    function FromGroup(const Value: Boolean = False): Integer;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинаковой записи.
    /// Действует в течение одного часа
    /// </summary>
    function Guid(const Value: string): Integer;
    /// <summary>
    /// Географическая отметка
    /// Lat - широта, заданная в градусах (от -90 до 90).
    /// Long - долгота, заданная в градусах (от -180 до 180).
    /// </summary>
    function LatLong(Lat, Long: Extended): Integer;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама",
    /// False — метки добавлено не будет.
    /// В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function MarkAsAds(const Value: Boolean): Integer;
    /// <summary>
    /// Текст сообщения (является обязательным, если не задан параметр Attachments)
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// True — уведомления к записи отключены.
    /// False — уведомления к записи включены.
    /// </summary>
    function MuteNotifications(const Value: Boolean): Integer;
    /// <summary>
    /// Идентификатор пользователя или сообщества, на стене которого должна быть опубликована запись
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор места, в котором отмечен пользователь
    /// </summary>
    function PlaceId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор записи, которую необходимо опубликовать.
    /// Данный параметр используется для публикации отложенных записей и предложенных новостей
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// Дата публикации записи. Если параметр указан, публикация записи будет отложена до указанного времени
    /// </summary>
    function PublishDate(const Value: TDateTime): Integer;
    /// <summary>
    /// Список сервисов или сайтов, на которые необходимо экспортировать запись,
    /// в случае если пользователь настроил соответствующую опцию. Например, twitter, facebook
    /// </summary>
    function Services(const Value: TArrayOfString): Integer;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена подпись
    /// (имя пользователя, разместившего запись), False — подписи добавлено не будет.
    /// Параметр учитывается только при публикации на стене сообщества и указании параметра FromGroup.
    /// По умолчанию подпись не добавляется
    /// </summary>
    function Signed(const Value: Boolean = False): Integer;
  end;

  TVkParamsWallEdit = record
    List: TParams;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// True — комментарии к записи отключены.
    /// False — комментарии к записи включены.
    /// </summary>
    function CloseComments(const Value: Boolean): Integer;
    /// <summary>
    /// Источник материала. Поддерживаются внешние и внутренние ссылки
    /// </summary>
    function Copyright(const Value: string): Integer;
    /// <summary>
    /// True — запись будет доступна только друзьям, False — всем пользователям.
    /// По умолчанию публикуемые записи доступны всем пользователям
    /// </summary>
    function FriendsOnly(const Value: Boolean): Integer;
    /// <summary>
    /// Географическая отметка
    /// Lat - широта, заданная в градусах (от -90 до 90).
    /// Long - долгота, заданная в градусах (от -180 до 180).
    /// </summary>
    function LatLong(Lat, Long: Extended): Integer;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама",
    /// False — метки добавлено не будет. В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function MarkAsAds(const Value: Boolean): Integer;
    /// <summary>
    /// Текст сообщения (является обязательным, если не задан параметр attachments)
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// Идентификатор пользователя или сообщества, на стене которого должна быть опубликована запись
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор места, в котором отмечен пользователь
    /// </summary>
    function PlaceId(const Value: Integer): Integer;
    function PosterBkgAccessHash(const Value: string): Integer;
    function PosterBkgId(const Value: Integer): Integer;
    function PosterBkgOwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор записи, которую необходимо опубликовать.
    /// Данный параметр используется для публикации отложенных записей и предложенных новостей
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// Дата публикации записи. Если параметр указан, публикация записи будет отложена до указанного времени
    /// </summary>
    function PublishDate(const Value: TDateTime): Integer;
    /// <summary>
    /// Список сервисов или сайтов, на которые необходимо экспортировать запись,
    /// в случае если пользователь настроил соответствующую опцию. Например, twitter, facebook
    /// </summary>
    function Services(const Value: TArrayOfString): Integer;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена подпись
    /// (имя пользователя, разместившего запись), False — подписи добавлено не будет.
    /// Параметр учитывается только при публикации на стене сообщества и указании параметра FromGroup.
    /// По умолчанию подпись не добавляется
    /// </summary>
    function Signed(const Value: Boolean): Integer;
  end;

  TVkParamsWallEditAdsStealth = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца стены (идентификатор сообщества нужно указывать со знаком «минус»)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор записи
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// Текст записи
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена подпись
    /// (имя пользователя, разместившего запись), False — подписи добавлено не будет.
    /// Параметр учитывается только при публикации на стене сообщества и указании параметра FromGroup.
    /// По умолчанию подпись не добавляется
    /// </summary>
    function Signed(const Value: Boolean): Integer;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама",
    /// False — метки добавлено не будет.
    /// В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function LatLong(Lat, Long: Extended): Integer;
    /// <summary>
    /// Идентификатор места, в котором отмечен пользователь
    /// </summary>
    function PlaceId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор кнопки, которую необходимо добавить к сниппету для ссылки
    /// </summary>
    function LinkButton(const Value: string): Integer;
    /// <summary>
    /// Заголовок, который должен быть использован для сниппета.
    /// Если не указан, будет автоматически получен с целевой ссылки.
    /// Обязательно указывать в случае, если ссылка является номером телефона
    /// </summary>
    function LinkTitle(const Value: string): Integer;
    /// <summary>
    /// Ссылка на изображение, которое должно быть использовано для сниппета.
    /// Минимальное разрешение: 537x240. Если не указана, будет автоматически загружена с целевой ссылки.
    /// Обязательно указывать в случае, если ссылка является номером телефона.
    /// Одновременно может быть указан либо параметр LinkImage, либо параметр LinkVideo
    /// </summary>
    function LinkImage(const Value: string): Integer;
    /// <summary>
    /// Идентификатор видео в формате "OwnerId_MediaId".
    /// Одновременно может быть указан либо параметр LinkImage, либо параметр LinkVideo.
    /// Кроме того, параметр LinkVideo может быть указан только вместе с параметрами LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(const Value: string): Integer; overload;
    /// <summary>
    /// Идентификатор видео. Одновременно может быть указан либо параметр LinkImage, либо параметр .
    /// Кроме того, параметр LinkVideo может быть указан только вместе с параметрами LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(OwnerId, MediaId: Integer): Integer; overload;
  end;

  TVkParamsWallEditComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца стены
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор комментария, который необходимо отредактировать
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// Новый текст комментария. Обязательный параметр, если не передан параметр Attachments
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
  end;

  TVkParamsWallGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, со стены которого
    /// необходимо получить записи (по умолчанию — текущий пользователь)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Короткий адрес пользователя или сообщества
    /// </summary>
    function Domain(const Value: string): Integer;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества записей
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// Количество записей, которое необходимо получить. Максимальное значение: 100
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// Определяет, какие типы записей на стене необходимо получить
    /// </summary>
    function Filter(const Value: TVkPostTypeFilter = TVkPostTypeFilter.All): Integer;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть.
    /// Обратите внимание, этот параметр учитывается только при Extended = True
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsWallGetById = record
    List: TParams;
    /// <summary>
    /// Перечисленные через запятую идентификаторы, которые представляют
    /// собой идущие через знак подчеркивания id владельцев стен и id самих
    /// записей на стене. Максимум 100 идентификаторов.
    /// Пример значения posts:
    /// 93388_21539,93388_20904,-1_340364
    /// </summary>
    function Posts(const Value: TArrayOfString): Integer;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// Определяет размер массива copy_history, возвращаемого в ответе, если запись является репостом записи с другой стены
    /// Например, copy_history_depth=1 — copy_history будет содержать один элемент с информацией о записи, прямым репостом которой является текущая.
    /// copy_history_depth=2 — copy_history будет содержать два элемента, добавляется информация о записи, репостом которой является первый элемент, и так далее (при условии, что иерархия репостов требуемой глубины для текущей записи существует).
    /// </summary>
    function CopyHistoryDepth(const Value: Integer = 2): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и групп, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsWallGetComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца стены (для сообществ — со знаком «минус»)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор комментария
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkCommentCreateParams = record
    List: TParams;
    /// <summary>
    /// Идентификатор записи
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор пользователя или сообщества, на чьей стене находится запись, к которой необходимо добавить комментарий
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Текст комментария. Обязательный параметр, если не передан параметр attachments
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// Данный параметр учитывается, если OwnerId меньше 0 (запись публикуется на стене группы).
    /// True — запись будет опубликована от имени группы,
    /// False — запись будет опубликована от имени пользователя (по умолчанию)
    /// </summary>
    function FromGroup(const Value: Boolean = False): Integer;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинаковой записи.
    ///  Действует в течение одного часа
    /// </summary>
    function Guid(const Value: string): Integer;
    /// <summary>
    /// Идентификатор комментария, в ответ на который должен быть добавлен новый комментарий
    /// </summary>
    function ReplyToComment(const Value: Integer): Integer;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// Идентификатор стикера
    /// </summary>
    function StickerId(const Value: Integer): Integer;
  end;

  TVkParamsWallGetComments = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца страницы (пользователь или сообщество)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Идентификатор записи
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// True — возвращать информацию о лайках
    /// </summary>
    function NeedLikes(const Value: Boolean): Integer;
    /// <summary>
    /// Идентификатор комментария, начиная с которого нужно вернуть список
    /// </summary>
    function StartCommentId(const Value: Integer): Integer;
    /// <summary>
    /// Сдвиг, необходимый для получения конкретной выборки результатов
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// Число комментариев, которые необходимо получить (максимальное значение: 100)
    /// </summary>
    function Count(const Value: Integer = 10): Integer;
    /// <summary>
    /// Порядок сортировки комментариев
    /// </summary>
    function Sort(const Value: TVkSort): Integer;
    /// <summary>
    /// Количество символов, по которому нужно обрезать текст комментария. Укажите 0, если Вы не хотите обрезать текст
    /// </summary>
    function PreviewLength(const Value: Integer): Integer;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
    /// <summary>
    /// Идентификатор комментария, ветку которого нужно получить
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// Максимальное число элементов в поле thread
    /// </summary>
    function ThreadItemsCount(const Value: Integer = 0): Integer;
  end;

  TVkParamsWallPostAdsStealth = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца стены (идентификатор сообщества нужно указывать со знаком «минус»)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Текст записи
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена подпись
    /// (имя пользователя, разместившего запись), False — подписи добавлено не будет.
    /// Параметр учитывается только при публикации на стене сообщества и указании параметра FromGroup.
    /// По умолчанию подпись не добавляется
    /// </summary>
    function Signed(const Value: Boolean): Integer;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама",
    /// False — метки добавлено не будет.
    /// В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function LatLong(Lat, Long: Extended): Integer;
    /// <summary>
    /// Идентификатор места, в котором отмечен пользователь
    /// </summary>
    function PlaceId(const Value: Integer): Integer;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинаковой записи.
    /// Действует в течение одного часа
    /// </summary>
    function Guid(const Value: string): Integer;
    /// <summary>
    /// Идентификатор кнопки, которую необходимо добавить к сниппету для ссылки
    /// </summary>
    function LinkButton(const Value: string): Integer; overload;
    /// <summary>
    /// Идентификатор кнопки, которую необходимо добавить к сниппету для ссылки
    /// </summary>
    function LinkButton(const Value: TVkPostLinkButton): Integer; overload;
    /// <summary>
    /// Заголовок, который должен быть использован для сниппета.
    /// Если не указан, будет автоматически получен с целевой ссылки.
    /// Обязательно указывать в случае, если ссылка является номером телефона
    /// </summary>
    function LinkTitle(const Value: string): Integer;
    /// <summary>
    /// Ссылка на изображение, которое должно быть использовано для сниппета.
    /// Минимальное разрешение: 537x240. Если не указана, будет автоматически загружена с целевой ссылки.
    /// Обязательно указывать в случае, если ссылка является номером телефона.
    /// Одновременно может быть указан либо параметр LinkImage, либо параметр LinkVideo
    /// </summary>
    function LinkImage(const Value: string): Integer;
    /// <summary>
    /// Идентификатор видео в формате "[owner_id]_[media_id]".
    /// Одновременно может быть указан либо параметр LinkImage, либо параметр LinkVideo.
    /// Кроме того, параметр LinkVideo может быть указан только вместе с параметрами LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(const Value: string): Integer;
  end;

  TVkParamsWallRepost = record
    List: TParams;
    /// <summary>
    /// Строковый идентификатор объекта, который необходимо разместить на стене, например, wall66748_3675 или wall-1_340364.
    /// Формируется из типа объекта (wall, photo, video и т.п.), идентификатора владельца объекта и идентификатора самого объекта
    /// </summary>
    function &Object(const Value: string): Integer;
    /// <summary>
    /// Сопроводительный текст, который будет добавлен к записи с объектом
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// Идентификатор сообщества, на стене которого будет размещена запись с объектом.
    /// Если не указан, запись будет размещена на стене текущего пользователя
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама", False — метки добавлено не будет. В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function MarkAsAds(const Value: Boolean): Integer;
    /// <summary>
    /// True — уведомления к записи отключены.
    /// False — уведомления к записи включены.
    /// </summary>
    function MuteNotifications(const Value: Boolean): Integer;
  end;

  TVkParamsWallSearch = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// Короткий адрес пользователя или сообщества
    /// </summary>
    function Domain(const Value: string): Integer;
    /// <summary>
    /// Поисковой запрос. Для точного результата запрос необходимо передавать в двойных кавычках
    /// </summary>
    function Query(const Value: string): Integer;
    /// <summary>
    /// True — возвращать только записи от имени владельца стены
    /// </summary>
    function OwnersOnly(const Value: Boolean): Integer;
    /// <summary>
    /// Количество записей, которые необходимо вернуть
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// Смещение, необходимо для получения определенного подмножества результатов
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TWallController = class(TVkController)
  public
    /// <summary>
    /// Проверяет ссылку для указания источника.
    /// </summary>
    function СheckCopyrightLink(const Link: string): Boolean;
    /// <summary>
    /// Выключает комментирование записи.
    /// </summary>
    function CloseComments(const OwnerId, PostId: Integer): Boolean;
    /// <summary>
    /// Добавляет комментарий к записи на стене.
    /// </summary>
    function CreateComment(var CommentInfo: TVkCommentInfo; Params: TVkCommentCreateParams): Boolean; overload;
    /// <summary>
    /// Добавляет комментарий к записи на стене.
    /// </summary>
    function CreateComment(Params: TVkCommentCreateParams): Boolean; overload;
    /// <summary>
    /// Добавляет комментарий к записи на стене.
    /// </summary>
    function CreateComment(const PostId: Integer; const Message: string; OwnerId: Integer = 0; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Удаляет запись со стены.
    /// </summary>
    function Delete(const PostId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет комментарий к записи на стене.
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Редактирует запись на стене.
    /// </summary>
    function Edit(var PostId: Integer; Params: TVkParamsWallEdit): Boolean; overload;
    /// <summary>
    /// Позволяет отредактировать скрытую запись.
    /// Создание скрытых записей возможно только в сообществах от имени группы, публичной страницы или мероприятия; пользователь должен обладать правами администратора или редактора.
    /// </summary>
    function EditAdsStealth(Params: TVkParamsWallEditAdsStealth): Boolean; overload;
    /// <summary>
    /// Редактирует комментарий на стене.
    /// </summary>
    function EditComment(Params: TVkParamsWallEditComment): Boolean; overload;
    /// <summary>
    /// Возвращает список записей со стены пользователя или сообщества.
    /// 5000 вызовов в сутки.
    /// </summary>
    function Get(var Items: TVkPosts; Params: TVkParamsWallGet): Boolean; overload;
    /// <summary>
    /// Возвращает список записей со стены пользователя или сообщества.
    /// 5000 вызовов в сутки.
    /// </summary>
    function Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список записей со стен пользователей или сообществ по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkPosts; Params: TVkParamsWallGetById): Boolean; overload;
    /// <summary>
    /// Получает информацию о комментарии на стене.
    /// </summary>
    function GetComment(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Получает информацию о комментарии на стене.
    /// </summary>
    function GetComment(var Items: TVkComments; Params: TVkParamsWallGetComment): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к записи на стене.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к записи на стене.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsWallGetComments): Boolean; overload;
    /// <summary>
    /// Позволяет получать список репостов заданной записи.
    /// Обратите внимание, получить список репостов можно только для записи, созданной текущим пользователем, или в сообществе, где текущий пользователь является администратором.
    /// </summary>
    function GetReposts(var Items: TVkPosts; PostId: Integer; Offset: Integer = 0; Count: Integer = 0; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Включает комментирование записи
    /// Работает только с конкретными записями, комментирование которых было выключено с помощью wall.closeComments
    /// </summary>
    function OpenComments(const OwnerId, PostId: Integer): Boolean; overload;
    /// <summary>
    /// Закрепляет запись на стене (запись будет отображаться выше остальных).
    /// </summary>
    function Pin(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(var PostId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(const Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(var PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(const Message: string; OwnerId: Integer; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(var PostId: Integer; Params: TVkParamsWallPost): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(Params: TVkParamsWallPost): Boolean; overload;
    /// <summary>
    /// Позволяет создать скрытую запись, которая не попадает на стену сообщества и в дальнейшем может быть использована для создания рекламного объявления типа "Запись в сообществе".
    /// Создание скрытых записей возможно только в сообществах от имени группы, публичной страницы или мероприятия; пользователь должен обладать правами администратора или редактора. Обратите внимание — в сутки можно создать не более 100 скрытых записей.
    /// </summary>
    function PostAdsStealth(var PostId: Integer; Params: TVkParamsWallPostAdsStealth): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к записи.
    /// </summary>
    function ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason = TVkMediaReportReason.Spam): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на запись.
    /// </summary>
    function ReportPost(const OwnerId, PostId: Integer; Reason: TVkMediaReportReason = TVkMediaReportReason.Spam): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к записи.
    /// </summary>
    function Repost(var Info: TVkRepostInfo; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к записи.
    /// </summary>
    function Repost(var Info: TVkRepostInfo; Params: TVkParamsWallRepost): Boolean; overload;
    /// <summary>
    /// Восстанавливает удаленную запись на стене пользователя или сообщества.
    /// </summary>
    function Restore(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Восстанавливает удаленный комментарий к записи на стене.
    /// </summary>
    function RestoreComment(const CommentId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Позволяет искать записи на стене в соответствии с заданными критериями.
    /// 1000 вызовов в сутки
    /// </summary>
    function Search(var Items: TVkPosts; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет искать записи на стене в соответствии с заданными критериями.
    /// 1000 вызовов в сутки
    /// </summary>
    function Search(var Items: TVkPosts; Params: TVkParamsWallSearch): Boolean; overload;
    /// <summary>
    /// Отменяет закрепление записи на стене.
    /// </summary>
    function Unpin(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TWallController }

function TWallController.Post(var PostId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean;
var
  Params: TVkParamsWallPost;
begin
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  if not Message.IsEmpty then
    Params.Message(Message);
  Result := Post(PostId, Params);
end;

function TWallController.Post(const Message: string; Attachments: TAttachmentArray): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Message, Attachments);
end;

function TWallController.Post(const Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Message, OwnerId, Attachments);
end;

function TWallController.Post(Params: TVkParamsWallPost): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Params);
end;

function TWallController.PostAdsStealth(var PostId: Integer; Params: TVkParamsWallPostAdsStealth): Boolean;
begin
  Result := Handler.Execute('wall.postAdsStealth', Params.List).GetValue('post_id', PostId);
end;

function TWallController.ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('owner_id', OwnerId);
  Params.Add('reason', Reason.ToString);
  Result := Handler.Execute('wall.reportComment', Params).ResponseIsTrue;
end;

function TWallController.ReportPost(const OwnerId, PostId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  Params.Add('owner_id', OwnerId);
  Params.Add('reason', Reason.ToString);
  Result := Handler.Execute('wall.reportPost', Params).ResponseIsTrue;
end;

function TWallController.Repost(var Info: TVkRepostInfo; Params: TVkParamsWallRepost): Boolean;
begin
  Result := Repost(Info, Params.List);
end;

function TWallController.Restore(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.restore', Params).ResponseIsTrue;
end;

function TWallController.RestoreComment(const CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.restoreComment', Params).ResponseIsTrue;
end;

function TWallController.Search(var Items: TVkPosts; Params: TVkParamsWallSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TWallController.Unpin(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.unpin', Params).ResponseIsTrue;
end;

function TWallController.Search(var Items: TVkPosts; Params: TParams): Boolean;
begin
  Result := Handler.Execute('wall.search', Params).GetObject(Items);
end;

function TWallController.Repost(var Info: TVkRepostInfo; Params: TParams): Boolean;
begin
  Result := Handler.Execute('wall.repost', Params).GetObject(Info);
end;

function TWallController.Post(var PostId: Integer; Params: TVkParamsWallPost): Boolean;
begin
  Result := Handler.Execute('wall.post', Params.List).GetValue('post_id', PostId);
end;

function TWallController.Post(var PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsWallPost;
begin
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  if not Message.IsEmpty then
    Params.Message(Message);
  Params.OwnerId(OwnerId);
  Result := Post(PostId, Params);
end;

function TWallController.CreateComment(var CommentInfo: TVkCommentInfo; Params: TVkCommentCreateParams): Boolean;
begin
  Result := Handler.Execute('wall.createComment', Params.List).GetObject(CommentInfo);
end;

function TWallController.CreateComment(Params: TVkCommentCreateParams): Boolean;
var
  CommentInfo: TVkCommentInfo;
begin
  Result := CreateComment(CommentInfo, Params);
  if Result then
    CommentInfo.Free;
end;

function TWallController.CloseComments(const OwnerId, PostId: Integer): Boolean;
begin
  Result := Handler.Execute('wall.closeComments', [
    ['owner_id', OwnerId.ToString],
    ['post_id', PostId.ToString]]).
    ResponseIsTrue;
end;

function TWallController.CreateComment(const PostId: Integer; const Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  CommentInfo: TVkCommentInfo;
  Params: TVkCommentCreateParams;
begin
  Params.PostId(PostId);
  Params.Message(Message);
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  Result := CreateComment(CommentInfo, Params);
  if Result then
    CommentInfo.Free;
end;

function TWallController.Delete(const PostId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.delete', Params).ResponseIsTrue;
end;

function TWallController.DeleteComment(const CommentId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.deleteComment', Params).ResponseIsTrue;
end;

function TWallController.Edit(var PostId: Integer; Params: TVkParamsWallEdit): Boolean;
begin
  Result := Handler.Execute('wall.edit', Params.List).ResponseAsInt(PostId);
end;

function TWallController.EditAdsStealth(Params: TVkParamsWallEditAdsStealth): Boolean;
begin
  Result := Handler.Execute('wall.editAdsStealth', Params.List).ResponseIsTrue;
end;

function TWallController.EditComment(Params: TVkParamsWallEditComment): Boolean;
begin
  Result := Handler.Execute('wall.editComment', Params.List).ResponseIsTrue;
end;

function TWallController.Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: Integer): Boolean;
var
  Params: TVkParamsWallGet;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Params.Offset(Offset);
  Params.Count(Count);
  Result := Get(Items, Params);
end;

function TWallController.GetById(var Items: TVkPosts; Params: TVkParamsWallGetById): Boolean;
begin
  Result := Handler.Execute('wall.getById', Params.List).GetObjects(Items);
end;

function TWallController.GetComment(var Items: TVkComments; Params: TVkParamsWallGetComment): Boolean;
begin
  Result := GetComment(Items, Params.List);
end;

function TWallController.GetComments(var Items: TVkComments; Params: TVkParamsWallGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TWallController.GetReposts(var Items: TVkPosts; PostId, Offset, Count, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if Offset <> 0 then
    Params.Add('offset', Offset);
  if Count <> 0 then
    Params.Add('count', Count);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.getReposts', Params).GetObject(Items);
end;

function TWallController.OpenComments(const OwnerId, PostId: Integer): Boolean;
begin
  Result := Handler.Execute('wall.openComments', [
    ['owner_id', OwnerId.ToString],
    ['post_id', PostId.ToString]]).
    ResponseIsTrue;
end;

function TWallController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('wall.getComments', Params).GetObject(Items);
end;

function TWallController.GetComment(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('wall.getComment', Params).GetObject(Items);
end;

function TWallController.Get(var Items: TVkPosts; Params: TVkParamsWallGet): Boolean;
begin
  Result := Handler.Execute('wall.get', Params.List).GetObject(Items);
end;

function TWallController.Pin(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.pin', Params).ResponseIsTrue;
end;

function TWallController.СheckCopyrightLink(const Link: string): Boolean;
begin
  Result := Handler.Execute('wall.checkCopyrightLink', ['link', Link]).ResponseIsTrue;
end;

{ TVkWallParams }

function TVkParamsWallPost.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPost.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPost.CloseComments(const Value: Boolean): Integer;
begin
  Result := List.Add('close_comments', Value);
end;

function TVkParamsWallPost.Copyright(const Value: string): Integer;
begin
  Result := List.Add('copyright', Value);
end;

function TVkParamsWallPost.DonutPaidDuration(const Value: TVkDonutPaidDuration): Integer;
begin
  Result := List.Add('donut_paid_duration', Ord(Value));
end;

function TVkParamsWallPost.FriendsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkParamsWallPost.FromGroup(const Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsWallPost.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsWallPost.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallPost.MarkAsAds(const Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallPost.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallPost.MuteNotifications(const Value: Boolean): Integer;
begin
  Result := List.Add('mute_notifications', Value);
end;

function TVkParamsWallPost.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallPost.PlaceId(const Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallPost.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallPost.PublishDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('publish_date', Value);
end;

function TVkParamsWallPost.Services(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('services', Value);
end;

function TVkParamsWallPost.Signed(const Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

{ TVkCommentCreateParams }

function TVkCommentCreateParams.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkCommentCreateParams.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkCommentCreateParams.FromGroup(const Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkCommentCreateParams.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkCommentCreateParams.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkCommentCreateParams.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkCommentCreateParams.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkCommentCreateParams.ReplyToComment(const Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkCommentCreateParams.StickerId(const Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

{ TVkWallGetParams }

function TVkParamsWallGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallGet.Domain(const Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsWallGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGet.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGet.Filter(const Value: TVkPostTypeFilter): Integer;
begin
  Result := List.Add('value', Value.ToString);
end;

function TVkParamsWallGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallGet.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsWallEdit }

function TVkParamsWallEdit.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEdit.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallEdit.FriendsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkParamsWallEdit.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEdit.Services(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('services', Value);
end;

function TVkParamsWallEdit.Signed(const Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallEdit.PublishDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('publish_date', Value);
end;

function TVkParamsWallEdit.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallEdit.PlaceId(const Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallEdit.MarkAsAds(const Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallEdit.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEdit.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEdit.CloseComments(const Value: Boolean): Integer;
begin
  Result := List.Add('close_comments', Value);
end;

function TVkParamsWallEdit.PosterBkgId(const Value: Integer): Integer;
begin
  Result := List.Add('poster_bkg_id', Value);
end;

function TVkParamsWallEdit.PosterBkgOwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('poster_bkg_owner_id', Value);
end;

function TVkParamsWallEdit.PosterBkgAccessHash(const Value: string): Integer;
begin
  Result := List.Add('poster_bkg_access_hash', Value);
end;

function TVkParamsWallEdit.Copyright(const Value: string): Integer;
begin
  Result := List.Add('copyright', Value);
end;

{ TVkParamsWallEditAdsStealth }

function TVkParamsWallEditAdsStealth.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEditAdsStealth.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallEditAdsStealth.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEditAdsStealth.Signed(const Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallEditAdsStealth.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditAdsStealth.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditAdsStealth.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallEditAdsStealth.PlaceId(const Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallEditAdsStealth.LinkButton(const Value: string): Integer;
begin
  Result := List.Add('link_button', Value);
end;

function TVkParamsWallEditAdsStealth.LinkTitle(const Value: string): Integer;
begin
  Result := List.Add('link_title', Value);
end;

function TVkParamsWallEditAdsStealth.LinkVideo(OwnerId, MediaId: Integer): Integer;
begin
  Result := List.Add('link_video', OwnerId.ToString + '_' + MediaId.ToString);
end;

function TVkParamsWallEditAdsStealth.LinkImage(const Value: string): Integer;
begin
  Result := List.Add('link_image', Value);
end;

function TVkParamsWallEditAdsStealth.LinkVideo(const Value: string): Integer;
begin
  Result := List.Add('link_video', Value);
end;

{ TVkParamsWallEditComment }

function TVkParamsWallEditComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEditComment.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallEditComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEditComment.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditComment.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

{ TVkParamsWallGetById }

function TVkParamsWallGetById.CopyHistoryDepth(const Value: Integer): Integer;
begin
  Result := List.Add('copy_history_depth', Value);
end;

function TVkParamsWallGetById.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetById.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetById.Posts(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('posts', Value);
end;

{ TVkParamsWallGetComment }

function TVkParamsWallGetComment.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallGetComment.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetComment.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsWallGetComments }

function TVkParamsWallGetComments.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallGetComments.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallGetComments.NeedLikes(const Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsWallGetComments.StartCommentId(const Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsWallGetComments.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallGetComments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallGetComments.Sort(const Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsWallGetComments.PreviewLength(const Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

function TVkParamsWallGetComments.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetComments.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetComments.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallGetComments.ThreadItemsCount(const Value: Integer): Integer;
begin
  Result := List.Add('thread_items_count', Value);
end;

{ TVkParamsWallPostAdsStealth }

function TVkParamsWallPostAdsStealth.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallPostAdsStealth.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallPostAdsStealth.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPostAdsStealth.Signed(const Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallPostAdsStealth.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallPostAdsStealth.PlaceId(const Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallPostAdsStealth.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPostAdsStealth.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsWallPostAdsStealth.LinkButton(const Value: string): Integer;
begin
  Result := List.Add('link_button', Value);
end;

function TVkParamsWallPostAdsStealth.LinkTitle(const Value: string): Integer;
begin
  Result := List.Add('link_title', Value);
end;

function TVkParamsWallPostAdsStealth.LinkButton(const Value: TVkPostLinkButton): Integer;
begin
  Result := List.Add('link_button', Value.ToString);
end;

function TVkParamsWallPostAdsStealth.LinkImage(const Value: string): Integer;
begin
  Result := List.Add('link_image', Value);
end;

function TVkParamsWallPostAdsStealth.LinkVideo(const Value: string): Integer;
begin
  Result := List.Add('link_video', Value);
end;

{ TVkParamsWallRepost }

function TVkParamsWallRepost.&Object(const Value: string): Integer;
begin
  Result := List.Add('object', Value);
end;

function TVkParamsWallRepost.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallRepost.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsWallRepost.MarkAsAds(const Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallRepost.MuteNotifications(const Value: Boolean): Integer;
begin
  Result := List.Add('mute_notifications', Value);
end;

{ TVkParamsWallSearch }

function TVkParamsWallSearch.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallSearch.Domain(const Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsWallSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('query', Value);
end;

function TVkParamsWallSearch.OwnersOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('owners_only', Value);
end;

function TVkParamsWallSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallSearch.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallSearch.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

end.

