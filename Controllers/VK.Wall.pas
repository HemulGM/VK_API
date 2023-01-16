unit VK.Wall;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, System.JSON, VK.Entity.Media, VK.Entity.Info;

type
  TVkParamsWallPost = record
    List: TParams;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallPost; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallPost; overload;
    /// <summary>
    /// True — комментарии к записи отключены.
    /// False — комментарии к записи включены.
    /// </summary>
    function CloseComments(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// Источник материала. Поддерживаются внешние и внутренние ссылки
    /// </summary>
    function Copyright(const Value: string): TVkParamsWallPost;
    /// <summary>
    /// Период времени в течение которого запись будет доступна для донов — платных подписчиков VK Donut
    /// </summary>
    function DonutPaidDuration(const Value: TVkDonutPaidDuration): TVkParamsWallPost;
    /// <summary>
    /// True — запись будет доступна только друзьям, False — всем пользователям.
    /// По умолчанию публикуемые записи доступны всем пользователям
    /// </summary>
    function FriendsOnly(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// Данный параметр учитывается, если OwnerId меньше 0 (запись публикуется на стене группы).
    /// True — запись будет опубликована от имени группы,
    /// False — запись будет опубликована от имени пользователя (по умолчанию)
    /// </summary>
    function FromGroup(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинаковой записи.
    /// Действует в течение одного часа
    /// </summary>
    function Guid(const Value: string): TVkParamsWallPost;
    /// <summary>
    /// Географическая отметка
    /// Lat - широта, заданная в градусах (от -90 до 90).
    /// Long - долгота, заданная в градусах (от -180 до 180).
    /// </summary>
    function LatLong(Lat, Long: Extended): TVkParamsWallPost;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама",
    /// False — метки добавлено не будет.
    /// В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function MarkAsAds(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// Текст сообщения (является обязательным, если не задан параметр Attachments)
    /// </summary>
    function Message(const Value: string): TVkParamsWallPost;
    /// <summary>
    /// True — уведомления к записи отключены.
    /// False — уведомления к записи включены.
    /// </summary>
    function MuteNotifications(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// Идентификатор пользователя или сообщества, на стене которого должна быть опубликована запись
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallPost;
    /// <summary>
    /// Идентификатор места, в котором отмечен пользователь
    /// </summary>
    function PlaceId(const Value: Integer): TVkParamsWallPost;
    /// <summary>
    /// Идентификатор записи, которую необходимо опубликовать.
    /// Данный параметр используется для публикации отложенных записей и предложенных новостей
    /// </summary>
    function PostId(const Value: Integer): TVkParamsWallPost;
    /// <summary>
    /// Дата публикации записи. Если параметр указан, публикация записи будет отложена до указанного времени
    /// </summary>
    function PublishDate(const Value: TDateTime): TVkParamsWallPost;
    /// <summary>
    /// Список сервисов или сайтов, на которые необходимо экспортировать запись,
    /// в случае если пользователь настроил соответствующую опцию. Например, twitter, facebook
    /// </summary>
    function Services(const Value: TArrayOfString): TVkParamsWallPost;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена подпись
    /// (имя пользователя, разместившего запись), False — подписи добавлено не будет.
    /// Параметр учитывается только при публикации на стене сообщества и указании параметра FromGroup.
    /// По умолчанию подпись не добавляется
    /// </summary>
    function Signed(const Value: Boolean = True): TVkParamsWallPost;
  end;

  TVkParamsWallEdit = record
    List: TParams;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallEdit; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallEdit; overload;
    /// <summary>
    /// True — комментарии к записи отключены.
    /// False — комментарии к записи включены.
    /// </summary>
    function CloseComments(const Value: Boolean = True): TVkParamsWallEdit;
    /// <summary>
    /// Источник материала. Поддерживаются внешние и внутренние ссылки
    /// </summary>
    function Copyright(const Value: string): TVkParamsWallEdit;
    /// <summary>
    /// True — запись будет доступна только друзьям, False — всем пользователям.
    /// По умолчанию публикуемые записи доступны всем пользователям
    /// </summary>
    function FriendsOnly(const Value: Boolean = True): TVkParamsWallEdit;
    /// <summary>
    /// Географическая отметка
    /// Lat - широта, заданная в градусах (от -90 до 90).
    /// Long - долгота, заданная в градусах (от -180 до 180).
    /// </summary>
    function LatLong(Lat, Long: Extended): TVkParamsWallEdit;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама",
    /// False — метки добавлено не будет. В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function MarkAsAds(const Value: Boolean = True): TVkParamsWallEdit;
    /// <summary>
    /// Текст сообщения (является обязательным, если не задан параметр attachments)
    /// </summary>
    function Message(const Value: string): TVkParamsWallEdit;
    /// <summary>
    /// Идентификатор пользователя или сообщества, на стене которого должна быть опубликована запись
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallEdit;
    /// <summary>
    /// Идентификатор места, в котором отмечен пользователь
    /// </summary>
    function PlaceId(const Value: Integer): TVkParamsWallEdit;
    function PosterBkgAccessHash(const Value: string): TVkParamsWallEdit;
    function PosterBkgId(const Value: Integer): TVkParamsWallEdit;
    function PosterBkgOwnerId(const Value: Integer): TVkParamsWallEdit;
    /// <summary>
    /// Идентификатор записи, которую необходимо опубликовать.
    /// Данный параметр используется для публикации отложенных записей и предложенных новостей
    /// </summary>
    function PostId(const Value: Integer): TVkParamsWallEdit;
    /// <summary>
    /// Дата публикации записи. Если параметр указан, публикация записи будет отложена до указанного времени
    /// </summary>
    function PublishDate(const Value: TDateTime): TVkParamsWallEdit;
    /// <summary>
    /// Список сервисов или сайтов, на которые необходимо экспортировать запись,
    /// в случае если пользователь настроил соответствующую опцию. Например, twitter, facebook
    /// </summary>
    function Services(const Value: TArrayOfString): TVkParamsWallEdit;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена подпись
    /// (имя пользователя, разместившего запись), False — подписи добавлено не будет.
    /// Параметр учитывается только при публикации на стене сообщества и указании параметра FromGroup.
    /// По умолчанию подпись не добавляется
    /// </summary>
    function Signed(const Value: Boolean = True): TVkParamsWallEdit;
  end;

  TVkParamsWallEditAdsStealth = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца стены (идентификатор сообщества нужно указывать со знаком «минус»)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// Идентификатор записи
    /// </summary>
    function PostId(const Value: Integer): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// Текст записи
    /// </summary>
    function Message(const Value: string): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallEditAdsStealth; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallEditAdsStealth; overload;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена подпись
    /// (имя пользователя, разместившего запись), False — подписи добавлено не будет.
    /// Параметр учитывается только при публикации на стене сообщества и указании параметра FromGroup.
    /// По умолчанию подпись не добавляется
    /// </summary>
    function Signed(const Value: Boolean = True): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама",
    /// False — метки добавлено не будет.
    /// В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function LatLong(Lat, Long: Extended): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// Идентификатор места, в котором отмечен пользователь
    /// </summary>
    function PlaceId(const Value: Integer): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// Идентификатор кнопки, которую необходимо добавить к сниппету для ссылки
    /// </summary>
    function LinkButton(const Value: string): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// Заголовок, который должен быть использован для сниппета.
    /// Если не указан, будет автоматически получен с целевой ссылки.
    /// Обязательно указывать в случае, если ссылка является номером телефона
    /// </summary>
    function LinkTitle(const Value: string): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// Ссылка на изображение, которое должно быть использовано для сниппета.
    /// Минимальное разрешение: 537x240. Если не указана, будет автоматически загружена с целевой ссылки.
    /// Обязательно указывать в случае, если ссылка является номером телефона.
    /// Одновременно может быть указан либо параметр LinkImage, либо параметр LinkVideo
    /// </summary>
    function LinkImage(const Value: string): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// Идентификатор видео в формате "OwnerId_MediaId".
    /// Одновременно может быть указан либо параметр LinkImage, либо параметр LinkVideo.
    /// Кроме того, параметр LinkVideo может быть указан только вместе с параметрами LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(const Value: string): TVkParamsWallEditAdsStealth; overload;
    /// <summary>
    /// Идентификатор видео. Одновременно может быть указан либо параметр LinkImage, либо параметр .
    /// Кроме того, параметр LinkVideo может быть указан только вместе с параметрами LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(OwnerId: TVkPeerId; MediaId: Integer): TVkParamsWallEditAdsStealth; overload;
  end;

  TVkParamsWallEditComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца стены
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallEditComment;
    /// <summary>
    /// Идентификатор комментария, который необходимо отредактировать
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsWallEditComment;
    /// <summary>
    /// Новый текст комментария. Обязательный параметр, если не передан параметр Attachments
    /// </summary>
    function Message(const Value: string): TVkParamsWallEditComment;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallEditComment; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallEditComment; overload;
  end;

  TVkParamsWallGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, со стены которого
    /// необходимо получить записи (по умолчанию — текущий пользователь)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallGet;
    /// <summary>
    /// Короткий адрес пользователя или сообщества
    /// </summary>
    function Domain(const Value: string): TVkParamsWallGet;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества записей
    /// </summary>
    function Offset(const Value: Integer): TVkParamsWallGet;
    /// <summary>
    /// Количество записей, которое необходимо получить. Максимальное значение: 100
    /// </summary>
    function Count(const Value: Integer): TVkParamsWallGet;
    /// <summary>
    /// Определяет, какие типы записей на стене необходимо получить
    /// </summary>
    function Filter(const Value: TVkPostTypeFilter = TVkPostTypeFilter.All): TVkParamsWallGet;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallGet;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть.
    /// Обратите внимание, этот параметр учитывается только при Extended = True
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsWallGet;
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
    function Posts(const Value: TArrayOfString): TVkParamsWallGetById;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallGetById;
    /// <summary>
    /// Определяет размер массива copy_history, возвращаемого в ответе, если запись является репостом записи с другой стены
    /// Например, copy_history_depth=1 — copy_history будет содержать один элемент с информацией о записи, прямым репостом которой является текущая.
    /// copy_history_depth=2 — copy_history будет содержать два элемента, добавляется информация о записи, репостом которой является первый элемент, и так далее (при условии, что иерархия репостов требуемой глубины для текущей записи существует).
    /// </summary>
    function CopyHistoryDepth(const Value: Integer = 2): TVkParamsWallGetById;
    /// <summary>
    /// Список дополнительных полей для профилей и групп, которые необходимо вернуть
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsWallGetById;
  end;

  TVkParamsWallGetComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца стены (для сообществ — со знаком «минус»)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallGetComment;
    /// <summary>
    /// Идентификатор комментария
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsWallGetComment;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallGetComment;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsWallGetComment;
  end;

  TVkCommentCreateParams = record
    List: TParams;
    /// <summary>
    /// Идентификатор записи
    /// </summary>
    function PostId(const Value: Integer): TVkCommentCreateParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, на чьей стене находится запись, к которой необходимо добавить комментарий
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkCommentCreateParams;
    /// <summary>
    /// Текст комментария. Обязательный параметр, если не передан параметр attachments
    /// </summary>
    function Message(const Value: string): TVkCommentCreateParams;
    /// <summary>
    /// Данный параметр учитывается, если OwnerId меньше 0 (запись публикуется на стене группы).
    /// True — запись будет опубликована от имени группы,
    /// False — запись будет опубликована от имени пользователя (по умолчанию)
    /// </summary>
    function FromGroup(const Value: Boolean = True): TVkCommentCreateParams;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинаковой записи.
    ///  Действует в течение одного часа
    /// </summary>
    function Guid(const Value: string): TVkCommentCreateParams;
    /// <summary>
    /// Идентификатор комментария, в ответ на который должен быть добавлен новый комментарий
    /// </summary>
    function ReplyToComment(const Value: Integer): TVkCommentCreateParams;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkCommentCreateParams; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): TVkCommentCreateParams; overload;
    /// <summary>
    /// Идентификатор стикера
    /// </summary>
    function StickerId(const Value: Integer): TVkCommentCreateParams;
  end;

  TVkParamsWallGetComments = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца страницы (пользователь или сообщество)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallGetComments;
    /// <summary>
    /// Идентификатор записи
    /// </summary>
    function PostId(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// True — возвращать информацию о лайках
    /// </summary>
    function NeedLikes(const Value: Boolean = True): TVkParamsWallGetComments;
    /// <summary>
    /// Идентификатор комментария, начиная с которого нужно вернуть список
    /// </summary>
    function StartCommentId(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// Сдвиг, необходимый для получения конкретной выборки результатов
    /// </summary>
    function Offset(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// Число комментариев, которые необходимо получить (максимальное значение: 100)
    /// </summary>
    function Count(const Value: Integer = 10): TVkParamsWallGetComments;
    /// <summary>
    /// Порядок сортировки комментариев
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsWallGetComments;
    /// <summary>
    /// Количество символов, по которому нужно обрезать текст комментария. Укажите 0, если Вы не хотите обрезать текст
    /// </summary>
    function PreviewLength(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallGetComments;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields = []): TVkParamsWallGetComments;
    /// <summary>
    /// Идентификатор комментария, ветку которого нужно получить
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// Максимальное число элементов в поле thread
    /// </summary>
    function ThreadItemsCount(const Value: Integer = 0): TVkParamsWallGetComments;
  end;

  TVkParamsWallPostAdsStealth = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца стены (идентификатор сообщества нужно указывать со знаком «минус»)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// Текст записи
    /// </summary>
    function Message(const Value: string): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallPostAdsStealth; overload;
    /// <summary>
    /// Объект или несколько объектов, приложенных к записи
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallPostAdsStealth; overload;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена подпись
    /// (имя пользователя, разместившего запись), False — подписи добавлено не будет.
    /// Параметр учитывается только при публикации на стене сообщества и указании параметра FromGroup.
    /// По умолчанию подпись не добавляется
    /// </summary>
    function Signed(const Value: Boolean = True): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама",
    /// False — метки добавлено не будет.
    /// В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function LatLong(Lat, Long: Extended): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// Идентификатор места, в котором отмечен пользователь
    /// </summary>
    function PlaceId(const Value: Integer): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинаковой записи.
    /// Действует в течение одного часа
    /// </summary>
    function Guid(const Value: string): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// Идентификатор кнопки, которую необходимо добавить к сниппету для ссылки
    /// </summary>
    function LinkButton(const Value: string): TVkParamsWallPostAdsStealth; overload;
    /// <summary>
    /// Идентификатор кнопки, которую необходимо добавить к сниппету для ссылки
    /// </summary>
    function LinkButton(const Value: TVkPostLinkButton): TVkParamsWallPostAdsStealth; overload;
    /// <summary>
    /// Заголовок, который должен быть использован для сниппета.
    /// Если не указан, будет автоматически получен с целевой ссылки.
    /// Обязательно указывать в случае, если ссылка является номером телефона
    /// </summary>
    function LinkTitle(const Value: string): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// Ссылка на изображение, которое должно быть использовано для сниппета.
    /// Минимальное разрешение: 537x240. Если не указана, будет автоматически загружена с целевой ссылки.
    /// Обязательно указывать в случае, если ссылка является номером телефона.
    /// Одновременно может быть указан либо параметр LinkImage, либо параметр LinkVideo
    /// </summary>
    function LinkImage(const Value: string): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// Идентификатор видео в формате "[owner_id]_[media_id]".
    /// Одновременно может быть указан либо параметр LinkImage, либо параметр LinkVideo.
    /// Кроме того, параметр LinkVideo может быть указан только вместе с параметрами LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(const Value: string): TVkParamsWallPostAdsStealth;
  end;

  TVkParamsWallRepost = record
    List: TParams;
    /// <summary>
    /// Строковый идентификатор объекта, который необходимо разместить на стене, например, wall66748_3675 или wall-1_340364.
    /// Формируется из типа объекта (wall, photo, video и т.п.), идентификатора владельца объекта и идентификатора самого объекта
    /// </summary>
    function &Object(const Value: string): TVkParamsWallRepost;
    /// <summary>
    /// Сопроводительный текст, который будет добавлен к записи с объектом
    /// </summary>
    function Message(const Value: string): TVkParamsWallRepost;
    /// <summary>
    /// Идентификатор сообщества, на стене которого будет размещена запись с объектом.
    /// Если не указан, запись будет размещена на стене текущего пользователя
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsWallRepost;
    /// <summary>
    /// True — у записи, размещенной от имени сообщества, будет добавлена метка "это реклама", False — метки добавлено не будет. В сутки может быть опубликовано не более пяти рекламных записей, из которых не более трёх — вне Биржи ВКонтакте
    /// </summary>
    function MarkAsAds(const Value: Boolean = True): TVkParamsWallRepost;
    /// <summary>
    /// True — уведомления к записи отключены.
    /// False — уведомления к записи включены.
    /// </summary>
    function MuteNotifications(const Value: Boolean): TVkParamsWallRepost;
  end;

  TVkParamsWallSearch = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallSearch;
    /// <summary>
    /// Короткий адрес пользователя или сообщества
    /// </summary>
    function Domain(const Value: string): TVkParamsWallSearch;
    /// <summary>
    /// Поисковой запрос. Для точного результата запрос необходимо передавать в двойных кавычках
    /// </summary>
    function Query(const Value: string): TVkParamsWallSearch;
    /// <summary>
    /// True — возвращать только записи от имени владельца стены
    /// </summary>
    function OwnersOnly(const Value: Boolean): TVkParamsWallSearch;
    /// <summary>
    /// Количество записей, которые необходимо вернуть
    /// </summary>
    function Count(const Value: Integer): TVkParamsWallSearch;
    /// <summary>
    /// Смещение, необходимо для получения определенного подмножества результатов
    /// </summary>
    function Offset(const Value: Integer): TVkParamsWallSearch;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallSearch;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields = []): TVkParamsWallSearch;
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
    function CloseComments(const OwnerId: TVkPeerId; PostId: Integer): Boolean;
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
    function CreateComment(const PostId: Integer; const Message: string; OwnerId: TVkPeerId = 0; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Удаляет запись со стены.
    /// </summary>
    function Delete(const PostId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Удаляет комментарий к записи на стене.
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: TVkPeerId = 0): Boolean;
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
    function Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: TVkPeerId = 0): Boolean; overload;
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
    function GetReposts(var Items: TVkPosts; PostId: Integer; Offset: Integer = 0; Count: Integer = 0; OwnerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Включает комментирование записи
    /// Работает только с конкретными записями, комментирование которых было выключено с помощью wall.closeComments
    /// </summary>
    function OpenComments(const OwnerId: TVkPeerId; PostId: Integer): Boolean; overload;
    /// <summary>
    /// Закрепляет запись на стене (запись будет отображаться выше остальных).
    /// </summary>
    function Pin(const PostId: Integer; const OwnerId: TVkPeerId = 0): Boolean; overload;
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
    function Post(var PostId: Integer; Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// Позволяет создать запись на стене, предложить запись на стене публичной страницы, опубликовать существующую отложенную запись.
    /// Чтобы создать предложенную запись, необходимо передать в owner_id идентификатор публичной страницы, в которой текущий пользователь не является руководителем.
    /// Для публикации предложенных и отложенных записей используйте параметр post_id, значение для которого можно получить методом wall.get с filter=suggests и postponed соответственно.
    /// </summary>
    function Post(const Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray = []): Boolean; overload;
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
    function ReportComment(const OwnerId: TVkPeerId; CommentId: Integer; Reason: TVkMediaReportReason = TVkMediaReportReason.Spam): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на запись.
    /// </summary>
    function ReportPost(const OwnerId: TVkPeerId; PostId: Integer; Reason: TVkMediaReportReason = TVkMediaReportReason.Spam): Boolean; overload;
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
    function Restore(const PostId: Integer; const OwnerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Восстанавливает удаленный комментарий к записи на стене.
    /// </summary>
    function RestoreComment(const CommentId: Integer; const OwnerId: TVkPeerId = 0): Boolean; overload;
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
    function Unpin(const PostId: Integer; const OwnerId: TVkPeerId = 0): Boolean; overload;
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

function TWallController.Post(const Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray): Boolean;
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

function TWallController.ReportComment(const OwnerId: TVkPeerId; CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('owner_id', OwnerId);
  Params.Add('reason', Reason.ToString);
  Result := Handler.Execute('wall.reportComment', Params).ResponseIsTrue;
end;

function TWallController.ReportPost(const OwnerId: TVkPeerId; PostId: Integer; Reason: TVkMediaReportReason): Boolean;
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

function TWallController.Restore(const PostId: Integer; const OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.restore', Params).ResponseIsTrue;
end;

function TWallController.RestoreComment(const CommentId: Integer; const OwnerId: TVkPeerId): Boolean;
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

function TWallController.Unpin(const PostId: Integer; const OwnerId: TVkPeerId): Boolean;
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

function TWallController.Post(var PostId: Integer; Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray): Boolean;
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

function TWallController.CloseComments(const OwnerId: TVkPeerId; PostId: Integer): Boolean;
begin
  Result := Handler.Execute('wall.closeComments', [
    ['owner_id', OwnerId.ToString],
    ['post_id', PostId.ToString]]).
    ResponseIsTrue;
end;

function TWallController.CreateComment(const PostId: Integer; const Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray): Boolean;
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

function TWallController.Delete(const PostId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.delete', Params).ResponseIsTrue;
end;

function TWallController.DeleteComment(const CommentId: Integer; OwnerId: TVkPeerId): Boolean;
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

function TWallController.Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: TVkPeerId): Boolean;
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

function TWallController.GetReposts(var Items: TVkPosts; PostId, Offset, Count: Integer; OwnerId: TVkPeerId): Boolean;
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

function TWallController.OpenComments(const OwnerId: TVkPeerId; PostId: Integer): Boolean;
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

function TWallController.Pin(const PostId: Integer; const OwnerId: TVkPeerId): Boolean;
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

function TVkParamsWallPost.Attachments(const Value: TAttachmentArray): TVkParamsWallPost;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallPost.Attachments(const Value: TAttachment): TVkParamsWallPost;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallPost.CloseComments(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('close_comments', Value);
  Result := Self;
end;

function TVkParamsWallPost.Copyright(const Value: string): TVkParamsWallPost;
begin
  List.Add('copyright', Value);
  Result := Self;
end;

function TVkParamsWallPost.DonutPaidDuration(const Value: TVkDonutPaidDuration): TVkParamsWallPost;
begin
  List.Add('donut_paid_duration', Ord(Value));
  Result := Self;
end;

function TVkParamsWallPost.FriendsOnly(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('friends_only', Value);
  Result := Self;
end;

function TVkParamsWallPost.FromGroup(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkParamsWallPost.Guid(const Value: string): TVkParamsWallPost;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkParamsWallPost.LatLong(Lat, Long: Extended): TVkParamsWallPost;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsWallPost.MarkAsAds(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('mark_as_ads', Value);
  Result := Self;
end;

function TVkParamsWallPost.Message(const Value: string): TVkParamsWallPost;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallPost.MuteNotifications(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('mute_notifications', Value);
  Result := Self;
end;

function TVkParamsWallPost.OwnerId(const Value: TVkPeerId): TVkParamsWallPost;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallPost.PlaceId(const Value: Integer): TVkParamsWallPost;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsWallPost.PostId(const Value: Integer): TVkParamsWallPost;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkParamsWallPost.PublishDate(const Value: TDateTime): TVkParamsWallPost;
begin
  List.Add('publish_date', Value);
  Result := Self;
end;

function TVkParamsWallPost.Services(const Value: TArrayOfString): TVkParamsWallPost;
begin
  List.Add('services', Value);
  Result := Self;
end;

function TVkParamsWallPost.Signed(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('signed', Value);
  Result := Self;
end;

{ TVkCommentCreateParams }

function TVkCommentCreateParams.Attachments(const Value: TAttachmentArray): TVkCommentCreateParams;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkCommentCreateParams.Attachments(const Value: TAttachment): TVkCommentCreateParams;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkCommentCreateParams.FromGroup(const Value: Boolean): TVkCommentCreateParams;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkCommentCreateParams.Guid(const Value: string): TVkCommentCreateParams;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkCommentCreateParams.Message(const Value: string): TVkCommentCreateParams;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkCommentCreateParams.OwnerId(const Value: TVkPeerId): TVkCommentCreateParams;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkCommentCreateParams.PostId(const Value: Integer): TVkCommentCreateParams;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkCommentCreateParams.ReplyToComment(const Value: Integer): TVkCommentCreateParams;
begin
  List.Add('reply_to_comment', Value);
  Result := Self;
end;

function TVkCommentCreateParams.StickerId(const Value: Integer): TVkCommentCreateParams;
begin
  List.Add('sticker_id', Value);
  Result := Self;
end;

{ TVkWallGetParams }

function TVkParamsWallGet.Count(const Value: Integer): TVkParamsWallGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsWallGet.Domain(const Value: string): TVkParamsWallGet;
begin
  List.Add('domain', Value);
  Result := Self;
end;

function TVkParamsWallGet.Extended(const Value: Boolean): TVkParamsWallGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallGet.Fields(Value: TVkExtendedFields): TVkParamsWallGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGet.Filter(const Value: TVkPostTypeFilter): TVkParamsWallGet;
begin
  List.Add('value', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGet.Offset(const Value: Integer): TVkParamsWallGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsWallGet.OwnerId(const Value: TVkPeerId): TVkParamsWallGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsWallEdit }

function TVkParamsWallEdit.OwnerId(const Value: TVkPeerId): TVkParamsWallEdit;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PostId(const Value: Integer): TVkParamsWallEdit;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.FriendsOnly(const Value: Boolean): TVkParamsWallEdit;
begin
  List.Add('friends_only', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Message(const Value: string): TVkParamsWallEdit;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Services(const Value: TArrayOfString): TVkParamsWallEdit;
begin
  List.Add('services', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Signed(const Value: Boolean): TVkParamsWallEdit;
begin
  List.Add('signed', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PublishDate(const Value: TDateTime): TVkParamsWallEdit;
begin
  List.Add('publish_date', Value);
  Result := Self;
end;

function TVkParamsWallEdit.LatLong(Lat, Long: Extended): TVkParamsWallEdit;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsWallEdit.PlaceId(const Value: Integer): TVkParamsWallEdit;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.MarkAsAds(const Value: Boolean): TVkParamsWallEdit;
begin
  List.Add('mark_as_ads', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Attachments(const Value: TAttachment): TVkParamsWallEdit;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Attachments(const Value: TAttachmentArray): TVkParamsWallEdit;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEdit.CloseComments(const Value: Boolean): TVkParamsWallEdit;
begin
  List.Add('close_comments', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PosterBkgId(const Value: Integer): TVkParamsWallEdit;
begin
  List.Add('poster_bkg_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PosterBkgOwnerId(const Value: Integer): TVkParamsWallEdit;
begin
  List.Add('poster_bkg_owner_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PosterBkgAccessHash(const Value: string): TVkParamsWallEdit;
begin
  List.Add('poster_bkg_access_hash', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Copyright(const Value: string): TVkParamsWallEdit;
begin
  List.Add('copyright', Value);
  Result := Self;
end;

{ TVkParamsWallEditAdsStealth }

function TVkParamsWallEditAdsStealth.OwnerId(const Value: TVkPeerId): TVkParamsWallEditAdsStealth;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.PostId(const Value: Integer): TVkParamsWallEditAdsStealth;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.Message(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.Signed(const Value: Boolean): TVkParamsWallEditAdsStealth;
begin
  List.Add('signed', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.Attachments(const Value: TAttachment): TVkParamsWallEditAdsStealth;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.Attachments(const Value: TAttachmentArray): TVkParamsWallEditAdsStealth;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LatLong(Lat, Long: Extended): TVkParamsWallEditAdsStealth;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.PlaceId(const Value: Integer): TVkParamsWallEditAdsStealth;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkButton(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_button', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkTitle(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_title', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkVideo(OwnerId: TVkPeerId; MediaId: Integer): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_video', OwnerId.ToString + '_' + MediaId.ToString);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkImage(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_image', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkVideo(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_video', Value);
  Result := Self;
end;

{ TVkParamsWallEditComment }

function TVkParamsWallEditComment.OwnerId(const Value: TVkPeerId): TVkParamsWallEditComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallEditComment.CommentId(const Value: Integer): TVkParamsWallEditComment;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsWallEditComment.Message(const Value: string): TVkParamsWallEditComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallEditComment.Attachments(const Value: TAttachment): TVkParamsWallEditComment;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEditComment.Attachments(const Value: TAttachmentArray): TVkParamsWallEditComment;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

{ TVkParamsWallGetById }

function TVkParamsWallGetById.CopyHistoryDepth(const Value: Integer): TVkParamsWallGetById;
begin
  List.Add('copy_history_depth', Value);
  Result := Self;
end;

function TVkParamsWallGetById.Extended(const Value: Boolean): TVkParamsWallGetById;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallGetById.Fields(Value: TVkExtendedFields): TVkParamsWallGetById;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGetById.Posts(const Value: TArrayOfString): TVkParamsWallGetById;
begin
  List.Add('posts', Value);
  Result := Self;
end;

{ TVkParamsWallGetComment }

function TVkParamsWallGetComment.CommentId(const Value: Integer): TVkParamsWallGetComment;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComment.Extended(const Value: Boolean): TVkParamsWallGetComment;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallGetComment.Fields(Value: TVkExtendedFields): TVkParamsWallGetComment;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGetComment.OwnerId(const Value: TVkPeerId): TVkParamsWallGetComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsWallGetComments }

function TVkParamsWallGetComments.OwnerId(const Value: TVkPeerId): TVkParamsWallGetComments;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.PostId(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.NeedLikes(const Value: Boolean): TVkParamsWallGetComments;
begin
  List.Add('need_likes', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.StartCommentId(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('start_comment_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Offset(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Count(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Sort(const Value: TVkSort): TVkParamsWallGetComments;
begin
  List.Add('sort', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGetComments.PreviewLength(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('preview_length', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Extended(const Value: Boolean): TVkParamsWallGetComments;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Fields(const Value: TVkExtendedFields): TVkParamsWallGetComments;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGetComments.CommentId(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.ThreadItemsCount(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('thread_items_count', Value);
  Result := Self;
end;

{ TVkParamsWallPostAdsStealth }

function TVkParamsWallPostAdsStealth.OwnerId(const Value: TVkPeerId): TVkParamsWallPostAdsStealth;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Message(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Attachments(const Value: TAttachmentArray): TVkParamsWallPostAdsStealth;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Signed(const Value: Boolean): TVkParamsWallPostAdsStealth;
begin
  List.Add('signed', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LatLong(Lat, Long: Extended): TVkParamsWallPostAdsStealth;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.PlaceId(const Value: Integer): TVkParamsWallPostAdsStealth;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Attachments(const Value: TAttachment): TVkParamsWallPostAdsStealth;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Guid(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkButton(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_button', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkTitle(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_title', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkButton(const Value: TVkPostLinkButton): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_button', Value.ToString);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkImage(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_image', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkVideo(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_video', Value);
  Result := Self;
end;

{ TVkParamsWallRepost }

function TVkParamsWallRepost.&Object(const Value: string): TVkParamsWallRepost;
begin
  List.Add('object', Value);
  Result := Self;
end;

function TVkParamsWallRepost.Message(const Value: string): TVkParamsWallRepost;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallRepost.GroupId(const Value: TVkPeerId): TVkParamsWallRepost;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsWallRepost.MarkAsAds(const Value: Boolean): TVkParamsWallRepost;
begin
  List.Add('mark_as_ads', Value);
  Result := Self;
end;

function TVkParamsWallRepost.MuteNotifications(const Value: Boolean): TVkParamsWallRepost;
begin
  List.Add('mute_notifications', Value);
  Result := Self;
end;

{ TVkParamsWallSearch }

function TVkParamsWallSearch.OwnerId(const Value: TVkPeerId): TVkParamsWallSearch;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Domain(const Value: string): TVkParamsWallSearch;
begin
  List.Add('domain', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Query(const Value: string): TVkParamsWallSearch;
begin
  List.Add('query', Value);
  Result := Self;
end;

function TVkParamsWallSearch.OwnersOnly(const Value: Boolean): TVkParamsWallSearch;
begin
  List.Add('owners_only', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Count(const Value: Integer): TVkParamsWallSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Offset(const Value: Integer): TVkParamsWallSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Extended(const Value: Boolean): TVkParamsWallSearch;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Fields(const Value: TVkExtendedFields): TVkParamsWallSearch;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

end.

