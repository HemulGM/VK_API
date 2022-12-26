unit VK.Video;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Video, System.JSON, VK.Entity.Status, VK.Entity.Media,
  VK.Entity.Video.Save, System.Net.HttpClient;

type
  TVkParamsVideoGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежат видеозаписи
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoGet;
    /// <summary>
    /// Идентификатор альбома, видеозаписи из которого нужно вернуть
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsVideoGet;
    /// <summary>
    /// Определяет, возвращать ли информацию о настройках приватности видео для текущего пользователя
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsVideoGet;
    /// <summary>
    /// Количество возвращаемых видеозаписей
    /// </summary>
    function Count(const Value: Integer): TVkParamsVideoGet;
    /// <summary>
    /// Смещение относительно первой найденной видеозаписи для выборки определенного подмножества
    /// </summary>
    function Offset(const Value: Integer): TVkParamsVideoGet;
    /// <summary>
    /// Перечисленные через запятую идентификаторы — идущие через знак подчеркивания
    /// id пользователей, которым принадлежат видеозаписи, и id самих видеозаписей.
    /// Если видеозапись принадлежит сообществу, то в качестве первого параметра используется -id сообщества
    /// Примеры: 4363_136089719, 13245770_137352259, 1_129207899_220df2876123d3542f, 6492_135055734_e0a9bcc31144f67fbd
    /// </summary>
    function Videos(const Value: TArrayOfString): TVkParamsVideoGet;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsVideoGet;
  end;

  TVkParamsVideoGetAlbums = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца альбомов (пользователь или сообщество).
    /// По умолчанию — идентификатор текущего пользователя
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoGetAlbums;
    /// <summary>
    /// Количество альбомов, информацию о которых нужно вернуть. По умолчанию: 50, максимальное значение: 100
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsVideoGetAlbums;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества альбомов. По умолчанию: 0
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsVideoGetAlbums;
    /// <summary>
    /// True — возвращать дополнительные поля Count, UpdatedTime и массив объектов Image для каждого альбома.
    /// Если альбом пустой, то массив объектов Image для него возвращен не будет. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsVideoGetAlbums;
    /// <summary>
    /// True — возвращать системные альбомы
    /// </summary>
    function NeedSystem(const Value: Boolean): TVkParamsVideoGetAlbums;
  end;

  TVkParamsVideoAddToAlbum = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца видеозаписи
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoAddToAlbum;
    /// <summary>
    /// Идентификатор владельца альбома, в который нужно добавить видео
    /// </summary>
    function TargetId(const Value: TVkPeerId): TVkParamsVideoAddToAlbum;
    /// <summary>
    /// Идентификатор альбома, в который нужно добавить видео.
    /// Для добавления видео в общий альбом «Добавленные» передавайте -2
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsVideoAddToAlbum;
    /// <summary>
    /// Идентификаторы альбомов, в которые нужно добавить видео
    /// </summary>
    function AlbumIds(const Value: TIdList): TVkParamsVideoAddToAlbum;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(const Value: Integer): TVkParamsVideoAddToAlbum;
  end;

  TVkParamsVideoCreateComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoCreateComment;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(const Value: Integer): TVkParamsVideoCreateComment;
    /// <summary>
    /// Текст комментария. Обязательный параметр, если не задан параметр Attachments
    /// </summary>
    function Message(const Value: string): TVkParamsVideoCreateComment;
    /// <summary>
    /// Список объектов, приложенных к комментарию
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsVideoCreateComment;
    /// <summary>
    /// Этот параметр учитывается, если owner_id меньше 0 (комментарий к видеозаписи группы).
    /// True — комментарий будет опубликован от имени группы,
    /// False — комментарий будет опубликован от имени пользователя. по умолчанию: False
    /// </summary>
    function FromGroup(const Value: Boolean = True): TVkParamsVideoCreateComment;
    /// <summary>
    /// Идентификатор комментария, в ответ на который должен быть добавлен новый комментарий
    /// </summary>
    function ReplyToComment(const Value: Integer): TVkParamsVideoCreateComment;
    /// <summary>
    /// Идентификатор стикера
    /// </summary>
    function StickerId(const Value: Integer): TVkParamsVideoCreateComment;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинакового комментария
    /// </summary>
    function Guid(const Value: string): TVkParamsVideoCreateComment;
  end;

  TVkParamsVideoEdit = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoEdit;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(const Value: Integer): TVkParamsVideoEdit;
    /// <summary>
    /// Новое название для видеозаписи
    /// </summary>
    function Name(const Value: string): TVkParamsVideoEdit;
    /// <summary>
    /// Новое описание для видеозаписи
    /// </summary>
    function Desc(const Value: string): TVkParamsVideoEdit;
    /// <summary>
    /// Настройки приватности просмотра видеозаписи в специальном формате.
    /// Приватность доступна для видеозаписей, которые пользователь загрузил в профиль
    /// </summary>
    function PrivacyView(const Value: TArrayOfString): TVkParamsVideoEdit;
    /// <summary>
    /// настройки приватности комментирования видеозаписи в специальном формате.
    /// Приватность доступна для видеозаписей, которые пользователь загрузил в профиль
    /// </summary>
    function PrivacyComment(const Value: TArrayOfString): TVkParamsVideoEdit;
    /// <summary>
    /// Закрыть комментарии (для видео из сообществ)
    /// </summary>
    function NoComments(const Value: Boolean): TVkParamsVideoEdit;
    /// <summary>
    /// Зацикливание воспроизведения видеозаписи
    /// </summary>
    function &Repeat(const Value: Boolean): TVkParamsVideoEdit;
  end;

  TVkParamsVideoEditAlbum = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества (если нужно отредактировать альбом, принадлежащий сообществу)
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsVideoEditAlbum;
    /// <summary>
    /// Идентификатор альбома
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsVideoEditAlbum;
    /// <summary>
    /// Новое название для альбома
    /// </summary>
    function Title(const Value: string): TVkParamsVideoEditAlbum;
    /// <summary>
    /// Уровень доступа к альбому в специальном формате.
    /// Приватность доступна для альбомов с видео в профиле пользователя
    /// </summary>
    function Privacy(const Value: TArrayOfString): TVkParamsVideoEditAlbum;
  end;

  TVkParamsVideoEditComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoEditComment;
    /// <summary>
    /// Идентификатор комментария
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsVideoEditComment;
    /// <summary>
    /// Новый текст комментария. Обязательный параметр, если не задан параметр Attachments
    /// </summary>
    function Message(const Value: string): TVkParamsVideoEditComment;
    /// <summary>
    /// Новый список объектов, приложенных к комментарию
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsVideoEditComment;
  end;

  TVkParamsVideoGetComments = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoGetComments;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(const Value: Integer): TVkParamsVideoGetComments;
    /// <summary>
    /// True — будет возвращено дополнительное поле Likes. По умолчанию поле Likes не возвращается
    /// </summary>
    function NeedLikes(const Value: Boolean): TVkParamsVideoGetComments;
    /// <summary>
    /// Идентификатор комментария, начиная с которого нужно вернуть список
    /// </summary>
    function StartCommentId(const Value: Integer): TVkParamsVideoGetComments;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества комментариев. По умолчанию: 0
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsVideoGetComments;
    /// <summary>
    /// Количество комментариев, информацию о которых необходимо вернуть
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsVideoGetComments;
    /// <summary>
    /// Порядок сортировки комментариев
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsVideoGetComments;
    /// <summary>
    /// True — возвращать дополнительные поля Count, UpdatedTime и массив объектов Image для каждого альбома.
    /// Если альбом пустой, то массив объектов image для него возвращен не будет. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsVideoGetComments;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsVideoGetComments;
  end;

  TVkParamsVideoRemoveFromAlbum = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца альбома
    /// </summary>
    function TargetId(const Value: TVkPeerId): TVkParamsVideoRemoveFromAlbum;
    /// <summary>
    /// Идентификатор альбома, из которого нужно убрать видео
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsVideoRemoveFromAlbum;
    /// <summary>
    /// Идентификаторы альбомов, из которых нужно убрать видео
    /// </summary>
    function AlbumIds(const Value: TIdList): TVkParamsVideoRemoveFromAlbum;
    /// <summary>
    /// Идентификатор владельца видеозаписи
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoRemoveFromAlbum;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(const Value: Integer): TVkParamsVideoRemoveFromAlbum;
  end;

  TVkParamsVideoReorderAlbums = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит альбом
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoReorderAlbums;
    /// <summary>
    /// Идентификатор альбома, который нужно переместить
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsVideoReorderAlbums;
    /// <summary>
    /// Идентификатор альбома, перед которым нужно поместить текущий
    /// </summary>
    function Before(const Value: Integer): TVkParamsVideoReorderAlbums;
    /// <summary>
    /// Идентификатор альбома, после которого нужно поместить текущий
    /// </summary>
    function After(const Value: Integer): TVkParamsVideoReorderAlbums;
  end;

  TVkParamsVideoReorderVideos = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, в чьем альбоме нужно переместить видео
    /// </summary>
    function TargetId(const Value: TVkPeerId): TVkParamsVideoReorderVideos;
    /// <summary>
    /// Идентификатор альбома с видеозаписью, которую нужно переместить
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsVideoReorderVideos;
    /// <summary>
    /// Идентификатор владельца видеозаписи, которую нужно переместить (пользователь или сообщество)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoReorderVideos;
    /// <summary>
    /// Идентификатор видеозаписи, которую нужно переместить
    /// </summary>
    function VideoId(const Value: Integer): TVkParamsVideoReorderVideos;
    /// <summary>
    /// Идентификатор владельца видеозаписи, перед которой следует поместить текущую (пользователь или сообщество)
    /// </summary>
    function BeforeOwnerId(const Value: TVkPeerId): TVkParamsVideoReorderVideos;
    /// <summary>
    /// Идентификатор видеозаписи, перед которой следует поместить текущую
    /// </summary>
    function BeforeVideoId(const Value: Integer): TVkParamsVideoReorderVideos;
    /// <summary>
    /// Идентификатор владельца видеозаписи, после которой следует поместить текущую (пользователь или сообщество)
    /// </summary>
    function AfterOwnerId(const Value: TVkPeerId): TVkParamsVideoReorderVideos;
    /// <summary>
    /// Идентификатор видеозаписи, после которой следует поместить текущую
    /// </summary>
    function AfterVideoId(const Value: Integer): TVkParamsVideoReorderVideos;
  end;

  TVkParamsVideoReport = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsVideoReport;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(const Value: Integer): TVkParamsVideoReport;
    /// <summary>
    /// Тип жалобы
    /// </summary>
    function Reason(const Value: TVkMediaReportReason): TVkParamsVideoReport;
    /// <summary>
    /// Комментарий для жалобы
    /// </summary>
    function Comment(const Value: string): TVkParamsVideoReport;
    /// <summary>
    /// Поисковой запрос, если видеозапись была найдена через поиск
    /// </summary>
    function SearchQuery(const Value: string): TVkParamsVideoReport;
  end;

  TVkParamsVideoSave = record
    List: TParams;
    /// <summary>
    /// Название видеофайла
    /// </summary>
    function Name(const Value: string): TVkParamsVideoSave;
    /// <summary>
    /// Описание видеофайла
    /// </summary>
    function Description(const Value: string): TVkParamsVideoSave;
    /// <summary>
    /// Указывается True, если видео загружается для отправки личным сообщением.
    /// После загрузки с этим параметром видеозапись не будет отображаться
    /// в списке видеозаписей пользователя и не будет доступна другим
    /// пользователям по ее идентификатору. По умолчанию: False
    /// </summary>
    function IsPrivate(const Value: Boolean = True): TVkParamsVideoSave;
    /// <summary>
    /// Требуется ли после сохранения опубликовать запись с видео на стене (True — требуется, False — не требуется).
    /// Обратите внимание, для публикации записи на стене приложение должно иметь права wall
    /// </summary>
    function Wallpost(const Value: Boolean): TVkParamsVideoSave;
    /// <summary>
    /// Url для встраивания видео с внешнего сайта, например, с Youtube.
    /// В этом случае нужно вызвать полученный UploadUrl, не прикрепляя файл,
    /// достаточно просто обратиться по этому адресу
    /// </summary>
    function Link(const Value: string): TVkParamsVideoSave;
    /// <summary>
    /// Идентификатор сообщества, в которое будет сохранен видеофайл.
    /// По умолчанию файл сохраняется на страницу текущего пользователя
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsVideoSave;
    /// <summary>
    /// Идентификатор альбома, в который будет загружен видео файл
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsVideoSave;
    /// <summary>
    /// Настройки приватности просмотра видеозаписи в специальном формате. Приватность доступна для видеозаписей, которые пользователь загрузил в профиль
    /// </summary>
    function PrivacyView(const Value: TArrayOfString): TVkParamsVideoSave;
    /// <summary>
    /// Настройки приватности комментирования видеозаписи в специальном формате.
    /// Приватность доступна для видеозаписей, которые пользователь загрузил в профиль
    /// </summary>
    function PrivacyComment(const Value: TArrayOfString): TVkParamsVideoSave;
    /// <summary>
    /// True — закрыть комментарии (для видео из сообществ). По умолчанию: False
    /// </summary>
    function NoComments(const Value: Boolean = False): TVkParamsVideoSave;
    /// <summary>
    /// Зацикливание воспроизведения видеозаписи
    /// </summary>
    function &Repeat(const Value: Boolean): TVkParamsVideoSave;
    /// <summary>
    /// Сжимать видео
    /// </summary>
    function Compression(const Value: Boolean): TVkParamsVideoSave;
  end;

  TVkParamsVideoSearch = record
    List: TParams;
    /// <summary>
    /// Строка поискового запроса
    /// </summary>
    function Query(const Value: string): TVkParamsVideoSearch;
    /// <summary>
    /// Сортировка результатов
    /// </summary>
    function Sort(const Value: TVkMediaSort): TVkParamsVideoSearch;
    /// <summary>
    /// Поиск производится только по видеозаписям высокого качества
    /// </summary>
    function HD(const Value: Boolean): TVkParamsVideoSearch;
    /// <summary>
    /// Отключить фильтр «Безопасный поиск»
    /// </summary>
    function Adult(const Value: Boolean = True): TVkParamsVideoSearch;
    /// <summary>
    /// Список критериев, по которым требуется отфильтровать видео
    /// </summary>
    function Filters(const Value: TVkVideosFilters): TVkParamsVideoSearch;
    /// <summary>
    /// True — искать по видеозаписям пользователя, False — не искать по видеозаписям пользователя
    /// </summary>
    function SearchOwn(const Value: Boolean = False): TVkParamsVideoSearch;
    /// <summary>
    /// Смещение относительно первой найденной видеозаписи для выборки определенного подмножества
    /// </summary>
    function Offset(const Value: Integer): TVkParamsVideoSearch;
    /// <summary>
    /// Количество секунд, видеозаписи длиннее которого необходимо вернуть
    /// </summary>
    function Longer(const Value: Integer): TVkParamsVideoSearch;
    /// <summary>
    /// Количество секунд, видеозаписи короче которого необходимо вернуть
    /// </summary>
    function Shorter(const Value: Integer): TVkParamsVideoSearch;
    /// <summary>
    /// Количество возвращаемых видеозаписей
    /// Обратите внимание — даже при использовании параметра Offset для получения информации доступны только первые 1000 результатов
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsVideoSearch;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups, содержащие информацию о пользователях и сообществах. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsVideoSearch;
  end;

  TVideoController = class(TVkController)
  public
    /// <summary>
    /// Добавляет видеозапись в список пользователя
    /// </summary>
    function Add(const VideoId: Integer; OwnerId: TVkPeerId; TargetId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Создает пустой альбом видеозаписей
    /// </summary>
    function AddAlbum(var AlbumId: Integer; Title: string; Privacy: TVkPrivacySettings; GroupId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Создает пустой альбом видеозаписей
    /// </summary>
    function AddAlbum(var AlbumId: Integer; Title: string; GroupId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Позволяет добавить видеозапись в альбом
    /// </summary>
    function AddToAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет добавить видеозапись в альбом
    /// </summary>
    function AddToAlbum(Params: TVkParamsVideoAddToAlbum): Boolean; overload;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TVkParamsVideoCreateComment): Boolean; overload;
    /// <summary>
    /// Удаляет видеозапись со страницы пользователя
    /// </summary>
    function Delete(const VideoId: Integer; OwnerId: TVkPeerId; TargetId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Удаляет альбом видеозаписей
    /// </summary>
    function DeleteAlbum(const AlbumId: Integer; GroupId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Удаляет комментарий к видеозаписи
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function Edit(Params: TVkParamsVideoEdit): Boolean; overload;
    /// <summary>
    /// Редактирует альбом с видео
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует альбом с видео
    /// </summary>
    function EditAlbum(Params: TVkParamsVideoEditAlbum): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к видеозаписи
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к видеозаписи
    /// </summary>
    function EditComment(Params: TVkParamsVideoEditComment): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о видеозаписях
    /// </summary>
    function Get(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о видеозаписях
    /// </summary>
    function Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео
    /// </summary>
    function GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean; overload;
    /// <summary>
    /// Возвращает список альбомов, в которых находится видеозапись
    /// </summary>
    function GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId: Integer; OwnerId: TVkPeerId; TargetId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к видеозаписи
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к видеозаписи
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsVideoGetComments): Boolean; overload;
    /// <summary>
    /// Позволяет убрать видеозапись из альбома
    /// </summary>
    function RemoveFromAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет убрать видеозапись из альбома
    /// </summary>
    function RemoveFromAlbum(Params: TVkParamsVideoRemoveFromAlbum): Boolean; overload;
    /// <summary>
    /// Позволяет изменить порядок альбомов с видео
    /// </summary>
    function ReorderAlbums(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет изменить порядок альбомов с видео
    /// </summary>
    function ReorderAlbums(Params: TVkParamsVideoReorderAlbums): Boolean; overload;
    /// <summary>
    /// Позволяет переместить видеозапись в альбоме
    /// </summary>
    function ReorderVideos(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет переместить видеозапись в альбоме
    /// </summary>
    function ReorderVideos(Params: TVkParamsVideoReorderVideos): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на видеозапись
    /// </summary>
    function Report(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на видеозапись
    /// </summary>
    function Report(Params: TVkParamsVideoReport): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к видеозаписи
    /// </summary>
    function ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean; overload;
    /// <summary>
    /// Восстанавливает удаленную видеозапись
    /// </summary>
    function Restore(VideoId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Восстанавливает удаленный комментарий к видеозаписи
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TVkParamsVideoSave): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean; overload;
    /// <summary>
    /// Возвращает список видеозаписей в соответствии с заданным критерием поиска
    /// </summary>
    function Search(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список видеозаписей в соответствии с заданным критерием поиска
    /// </summary>
    function Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean; overload;
    /// <summary>
    /// Загрузка видео (вручную)
    /// </summary>
    function Upload(var Response: TVkVideoUploadResponse; const UploadUrl: string; const FileName: string; Callback: TReceiveDataEvent = nil): Boolean; overload;
    /// <summary>
    /// Загрузка видео
    /// </summary>
    function Upload(var Response: TVkVideoUploadResponse; Params: TVkParamsVideoSave; const FileName: string; Callback: TReceiveDataEvent = nil): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.NetConsts, System.Net.URLClient,
  System.Net.Mime, System.Classes, IdMultipartFormData, System.Types;

{ TVideoController }

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.getAlbums', Params).GetObject(Items);
end;

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean;
begin
  Result := GetAlbums(Items, Params.List);
end;

function TVideoController.GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId: Integer; OwnerId: TVkPeerId; TargetId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Params.Add('extended', True);
  Result := Handler.Execute('video.getAlbumsByVideo', Params).GetObject(Items);
end;

function TVideoController.GetComments(var Items: TVkComments; Params: TVkParamsVideoGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TVideoController.RemoveFromAlbum(Params: TVkParamsVideoRemoveFromAlbum): Boolean;
begin
  Result := RemoveFromAlbum(Params.List);
end;

function TVideoController.ReorderAlbums(Params: TVkParamsVideoReorderAlbums): Boolean;
begin
  Result := ReorderAlbums(Params.List);
end;

function TVideoController.ReorderVideos(Params: TVkParamsVideoReorderVideos): Boolean;
begin
  Result := ReorderVideos(Params.List);
end;

function TVideoController.Report(Params: TVkParamsVideoReport): Boolean;
begin
  Result := Report(Params.List);
end;

function TVideoController.ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  Params.Add('reason', Ord(Reason).ToString);
  Result := Handler.Execute('video.reportComment', Params).ResponseIsTrue;
end;

function TVideoController.Restore(VideoId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.restore', Params).ResponseIsTrue;
end;

function TVideoController.RestoreComment(CommentId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.restoreComment', Params).ResponseIsTrue;
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Params: TVkParamsVideoSave): Boolean;
begin
  Result := Save(VideoSaved, Params.List);
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.save', Params).GetObject(VideoSaved);
end;

function TVideoController.Report(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.report', Params).ResponseIsTrue;
end;

function TVideoController.ReorderVideos(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.reorderVideos', Params).ResponseIsTrue;
end;

function TVideoController.ReorderAlbums(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.reorderAlbums', Params).ResponseIsTrue;
end;

function TVideoController.RemoveFromAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.removeFromAlbum', Params).ResponseIsTrue;
end;

function TVideoController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.getComments', Params).GetObject(Items);
end;

function TVideoController.GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.getAlbumById', Params).GetObject(Item);
end;

function TVideoController.Get(var Items: TVkVideos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.get', Params).GetObject(Items);
end;

function TVideoController.Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TVideoController.Add(const VideoId: Integer; OwnerId: TVkPeerId; TargetId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Result := Handler.Execute('video.add', Params).ResponseIsTrue;
end;

function TVideoController.AddAlbum(var AlbumId: Integer; Title: string; Privacy: TVkPrivacySettings; GroupId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('title', Title);
  if not Privacy.IsEmpty then
    Params.Add('privacy', Privacy.ToString);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('video.addAlbum', Params).ResponseAsInt(AlbumId);
end;

function TVideoController.AddAlbum(var AlbumId: Integer; Title: string; GroupId: TVkPeerId): Boolean;
var
  Privacy: TVkPrivacySettings;
begin
  Result := AddAlbum(AlbumId, Title, Privacy, GroupId);
end;

function TVideoController.AddToAlbum(Params: TVkParamsVideoAddToAlbum): Boolean;
begin
  Result := AddToAlbum(Params.List);
end;

function TVideoController.CreateComment(var CommentId: Integer; Params: TVkParamsVideoCreateComment): Boolean;
begin
  Result := CreateComment(CommentId, Params.List);
end;

function TVideoController.Delete(const VideoId: Integer; OwnerId: TVkPeerId; TargetId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Result := Handler.Execute('video.delete', Params).ResponseIsTrue;
end;

function TVideoController.DeleteAlbum(const AlbumId: Integer; GroupId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('video.deleteAlbum', Params).ResponseIsTrue;
end;

function TVideoController.DeleteComment(const CommentId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.deleteComment', Params).ResponseIsTrue;
end;

function TVideoController.Edit(Params: TVkParamsVideoEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TVideoController.EditAlbum(Params: TVkParamsVideoEditAlbum): Boolean;
begin
  Result := EditAlbum(Params.List);
end;

function TVideoController.EditComment(Params: TVkParamsVideoEditComment): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TVideoController.EditComment(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.editComment', Params).ResponseIsTrue;
end;

function TVideoController.EditAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.editAlbum', Params).ResponseIsTrue;
end;

function TVideoController.Edit(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.edit', Params).ResponseIsTrue;
end;

function TVideoController.CreateComment(var CommentId: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.createComment', Params).ResponseAsInt(CommentId);
end;

function TVideoController.AddToAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.addToAlbum', Params).ResponseIsTrue;
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean;
begin
  Result := Handler.Execute('video.save', ['link', Link]).GetObject(VideoSaved);
end;

function TVideoController.Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TVideoController.Search(var Items: TVkVideos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.search', Params).GetObject(Items);
end;

function TVideoController.Upload(var Response: TVkVideoUploadResponse; Params: TVkParamsVideoSave; const FileName: string; Callback: TReceiveDataEvent): Boolean;
var
  VideoSaved: TVkVideoSaved;
begin
  Result := False;
  if Save(VideoSaved, Params) then
  try
    Result := Upload(Response, VideoSaved.UploadUrl, FileName, Callback);
    if Result then
      Response.AccessKey := VideoSaved.AccessKey;
  finally
    VideoSaved.Free;
  end;
end;

function TVideoController.Upload(var Response: TVkVideoUploadResponse; const UploadUrl: string; const FileName: string; Callback: TReceiveDataEvent): Boolean;
var
  HTTP: THTTPClient;
  Data: TIdMultiPartFormDataStream;
  ResStream: TStringStream;
begin
  Result := False;
  Data := TIdMultiPartFormDataStream.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    {$IF CompilerVersion >= 34.0}
    HTTP.OnSendData := Callback;
    {$ENDIF}
    Data.AddFile('video_file', FileName);
    HTTP.ContentType := Data.RequestContentType;
    HTTP.CustomHeaders[sContentLength] := Data.Size.ToString;
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      Response := TVkVideoUploadResponse.FromJsonString<TVkVideoUploadResponse>(ResStream.DataString);
      Result := True;
    end;
  finally
    ResStream.Free;
    Data.Free;
    HTTP.Free;
  end;
end;

{ TVkVideosGetParams }

function TVkParamsVideoGet.AlbumId(const Value: Integer): TVkParamsVideoGet;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsVideoGet.Count(const Value: Integer): TVkParamsVideoGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsVideoGet.Extended(const Value: Boolean): TVkParamsVideoGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsVideoGet.Fields(Value: TVkExtendedFields = []): TVkParamsVideoGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsVideoGet.Offset(const Value: Integer): TVkParamsVideoGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsVideoGet.OwnerId(const Value: TVkPeerId): TVkParamsVideoGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoGet.Videos(const Value: TArrayOfString): TVkParamsVideoGet;
begin
  List.Add('videos', Value);
  Result := Self;
end;

{ TVkParamsVideoAlbumsGet }

function TVkParamsVideoGetAlbums.Count(const Value: Integer): TVkParamsVideoGetAlbums;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsVideoGetAlbums.Extended(const Value: Boolean): TVkParamsVideoGetAlbums;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsVideoGetAlbums.NeedSystem(const Value: Boolean): TVkParamsVideoGetAlbums;
begin
  List.Add('need_system', Value);
  Result := Self;
end;

function TVkParamsVideoGetAlbums.Offset(const Value: Integer): TVkParamsVideoGetAlbums;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsVideoGetAlbums.OwnerId(const Value: TVkPeerId): TVkParamsVideoGetAlbums;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsVideosAddToAlbum }

function TVkParamsVideoAddToAlbum.AlbumId(const Value: Integer): TVkParamsVideoAddToAlbum;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsVideoAddToAlbum.AlbumIds(const Value: TIdList): TVkParamsVideoAddToAlbum;
begin
  List.Add('album_ids', Value);
  Result := Self;
end;

function TVkParamsVideoAddToAlbum.OwnerId(const Value: TVkPeerId): TVkParamsVideoAddToAlbum;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoAddToAlbum.TargetId(const Value: TVkPeerId): TVkParamsVideoAddToAlbum;
begin
  List.Add('target_id', Value);
  Result := Self;
end;

function TVkParamsVideoAddToAlbum.VideoId(const Value: Integer): TVkParamsVideoAddToAlbum;
begin
  List.Add('video_id', Value);
  Result := Self;
end;

{ TVkParamsVideosCreateComment }

function TVkParamsVideoCreateComment.OwnerId(const Value: TVkPeerId): TVkParamsVideoCreateComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoCreateComment.VideoId(const Value: Integer): TVkParamsVideoCreateComment;
begin
  List.Add('video_id', Value);
  Result := Self;
end;

function TVkParamsVideoCreateComment.Message(const Value: string): TVkParamsVideoCreateComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsVideoCreateComment.Attachments(const Value: TAttachmentArray): TVkParamsVideoCreateComment;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsVideoCreateComment.FromGroup(const Value: Boolean): TVkParamsVideoCreateComment;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkParamsVideoCreateComment.ReplyToComment(const Value: Integer): TVkParamsVideoCreateComment;
begin
  List.Add('reply_to_comment', Value);
  Result := Self;
end;

function TVkParamsVideoCreateComment.StickerId(const Value: Integer): TVkParamsVideoCreateComment;
begin
  List.Add('sticker_id', Value);
  Result := Self;
end;

function TVkParamsVideoCreateComment.Guid(const Value: string): TVkParamsVideoCreateComment;
begin
  List.Add('guid', Value);
  Result := Self;
end;

{ TVkParamsVideosEdit }

function TVkParamsVideoEdit.OwnerId(const Value: TVkPeerId): TVkParamsVideoEdit;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoEdit.VideoId(const Value: Integer): TVkParamsVideoEdit;
begin
  List.Add('video_id', Value);
  Result := Self;
end;

function TVkParamsVideoEdit.Name(const Value: string): TVkParamsVideoEdit;
begin
  List.Add('name', Value);
  Result := Self;
end;

function TVkParamsVideoEdit.Desc(const Value: string): TVkParamsVideoEdit;
begin
  List.Add('desc', Value);
  Result := Self;
end;

function TVkParamsVideoEdit.PrivacyView(const Value: TArrayOfString): TVkParamsVideoEdit;
begin
  List.Add('privacy_view', Value);
  Result := Self;
end;

function TVkParamsVideoEdit.PrivacyComment(const Value: TArrayOfString): TVkParamsVideoEdit;
begin
  List.Add('privacy_comment', Value);
  Result := Self;
end;

function TVkParamsVideoEdit.NoComments(const Value: Boolean): TVkParamsVideoEdit;
begin
  List.Add('no_comments', Value);
  Result := Self;
end;

function TVkParamsVideoEdit.&Repeat(const Value: Boolean): TVkParamsVideoEdit;
begin
  List.Add('repeat', Value);
  Result := Self;
end;

{ TVkParamsVideosEditAlbum }

function TVkParamsVideoEditAlbum.GroupId(const Value: TVkPeerId): TVkParamsVideoEditAlbum;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsVideoEditAlbum.AlbumId(const Value: Integer): TVkParamsVideoEditAlbum;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsVideoEditAlbum.Title(const Value: string): TVkParamsVideoEditAlbum;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsVideoEditAlbum.Privacy(const Value: TArrayOfString): TVkParamsVideoEditAlbum;
begin
  List.Add('privacy', Value);
  Result := Self;
end;

{ TVkParamsVideosEditComment }

function TVkParamsVideoEditComment.OwnerId(const Value: TVkPeerId): TVkParamsVideoEditComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoEditComment.CommentId(const Value: Integer): TVkParamsVideoEditComment;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsVideoEditComment.Message(const Value: string): TVkParamsVideoEditComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsVideoEditComment.Attachments(const Value: TAttachmentArray): TVkParamsVideoEditComment;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

{ TVkParamsVideoGetComments }

function TVkParamsVideoGetComments.OwnerId(const Value: TVkPeerId): TVkParamsVideoGetComments;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoGetComments.VideoId(const Value: Integer): TVkParamsVideoGetComments;
begin
  List.Add('video_id', Value);
  Result := Self;
end;

function TVkParamsVideoGetComments.NeedLikes(const Value: Boolean): TVkParamsVideoGetComments;
begin
  List.Add('need_likes', Value);
  Result := Self;
end;

function TVkParamsVideoGetComments.StartCommentId(const Value: Integer): TVkParamsVideoGetComments;
begin
  List.Add('start_comment_id', Value);
  Result := Self;
end;

function TVkParamsVideoGetComments.Offset(const Value: Integer): TVkParamsVideoGetComments;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsVideoGetComments.Count(const Value: Integer): TVkParamsVideoGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsVideoGetComments.Sort(const Value: TVkSort): TVkParamsVideoGetComments;
begin
  List.Add('sort', Value.ToString);
  Result := Self;
end;

function TVkParamsVideoGetComments.Extended(const Value: Boolean): TVkParamsVideoGetComments;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsVideoGetComments.Fields(Value: TVkExtendedFields): TVkParamsVideoGetComments;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

{ TVkParamsVideoRemoveFromAlbum }

function TVkParamsVideoRemoveFromAlbum.TargetId(const Value: TVkPeerId): TVkParamsVideoRemoveFromAlbum;
begin
  List.Add('target_id', Value);
  Result := Self;
end;

function TVkParamsVideoRemoveFromAlbum.AlbumId(const Value: Integer): TVkParamsVideoRemoveFromAlbum;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsVideoRemoveFromAlbum.AlbumIds(const Value: TIdList): TVkParamsVideoRemoveFromAlbum;
begin
  List.Add('album_ids', Value);
  Result := Self;
end;

function TVkParamsVideoRemoveFromAlbum.OwnerId(const Value: TVkPeerId): TVkParamsVideoRemoveFromAlbum;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoRemoveFromAlbum.VideoId(const Value: Integer): TVkParamsVideoRemoveFromAlbum;
begin
  List.Add('video_id', Value);
  Result := Self;
end;

{ TVkParamsVideoReorderAlbums }

function TVkParamsVideoReorderAlbums.OwnerId(const Value: TVkPeerId): TVkParamsVideoReorderAlbums;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderAlbums.AlbumId(const Value: Integer): TVkParamsVideoReorderAlbums;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderAlbums.Before(const Value: Integer): TVkParamsVideoReorderAlbums;
begin
  List.Add('before', Value);
  Result := Self;
end;

function TVkParamsVideoReorderAlbums.After(const Value: Integer): TVkParamsVideoReorderAlbums;
begin
  List.Add('after', Value);
  Result := Self;
end;

{ TVkParamsVideoReorderVideos }

function TVkParamsVideoReorderVideos.TargetId(const Value: TVkPeerId): TVkParamsVideoReorderVideos;
begin
  List.Add('target_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderVideos.AlbumId(const Value: Integer): TVkParamsVideoReorderVideos;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderVideos.OwnerId(const Value: TVkPeerId): TVkParamsVideoReorderVideos;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderVideos.VideoId(const Value: Integer): TVkParamsVideoReorderVideos;
begin
  List.Add('video_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderVideos.BeforeOwnerId(const Value: TVkPeerId): TVkParamsVideoReorderVideos;
begin
  List.Add('before_owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderVideos.BeforeVideoId(const Value: Integer): TVkParamsVideoReorderVideos;
begin
  List.Add('before_video_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderVideos.AfterOwnerId(const Value: TVkPeerId): TVkParamsVideoReorderVideos;
begin
  List.Add('after_owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoReorderVideos.AfterVideoId(const Value: Integer): TVkParamsVideoReorderVideos;
begin
  List.Add('after_video_id', Value);
  Result := Self;
end;

{ TVkParamsVideoReport }

function TVkParamsVideoReport.OwnerId(const Value: TVkPeerId): TVkParamsVideoReport;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsVideoReport.VideoId(const Value: Integer): TVkParamsVideoReport;
begin
  List.Add('video_id', Value);
  Result := Self;
end;

function TVkParamsVideoReport.Reason(const Value: TVkMediaReportReason): TVkParamsVideoReport;
begin
  List.Add('reason', Ord(Value).ToString);
  Result := Self;
end;

function TVkParamsVideoReport.Comment(const Value: string): TVkParamsVideoReport;
begin
  List.Add('comment', Value);
  Result := Self;
end;

function TVkParamsVideoReport.SearchQuery(const Value: string): TVkParamsVideoReport;
begin
  List.Add('search_query', Value);
  Result := Self;
end;

{ TVkParamsVideosSave }

function TVkParamsVideoSave.Name(const Value: string): TVkParamsVideoSave;
begin
  List.Add('name', Value);
  Result := Self;
end;

function TVkParamsVideoSave.Description(const Value: string): TVkParamsVideoSave;
begin
  List.Add('description', Value);
  Result := Self;
end;

function TVkParamsVideoSave.IsPrivate(const Value: Boolean): TVkParamsVideoSave;
begin
  List.Add('is_private', Value);
  Result := Self;
end;

function TVkParamsVideoSave.Wallpost(const Value: Boolean): TVkParamsVideoSave;
begin
  List.Add('wallpost', Value);
  Result := Self;
end;

function TVkParamsVideoSave.Link(const Value: string): TVkParamsVideoSave;
begin
  List.Add('link', Value);
  Result := Self;
end;

function TVkParamsVideoSave.GroupId(const Value: TVkPeerId): TVkParamsVideoSave;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsVideoSave.AlbumId(const Value: Integer): TVkParamsVideoSave;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsVideoSave.PrivacyView(const Value: TArrayOfString): TVkParamsVideoSave;
begin
  List.Add('privacy_view', Value);
  Result := Self;
end;

function TVkParamsVideoSave.PrivacyComment(const Value: TArrayOfString): TVkParamsVideoSave;
begin
  List.Add('privacy_comment', Value);
  Result := Self;
end;

function TVkParamsVideoSave.NoComments(const Value: Boolean): TVkParamsVideoSave;
begin
  List.Add('no_comments', Value);
  Result := Self;
end;

function TVkParamsVideoSave.&Repeat(const Value: Boolean): TVkParamsVideoSave;
begin
  List.Add('repeat', Value);
  Result := Self;
end;

function TVkParamsVideoSave.Compression(const Value: Boolean): TVkParamsVideoSave;
begin
  List.Add('compression', Value);
  Result := Self;
end;

{ TVkParamsVideosSearch }

function TVkParamsVideoSearch.Query(const Value: string): TVkParamsVideoSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsVideoSearch.Sort(const Value: TVkMediaSort): TVkParamsVideoSearch;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

function TVkParamsVideoSearch.HD(const Value: Boolean): TVkParamsVideoSearch;
begin
  List.Add('hd', Value);
  Result := Self;
end;

function TVkParamsVideoSearch.Adult(const Value: Boolean): TVkParamsVideoSearch;
begin
  List.Add('adult', Value);
  Result := Self;
end;

function TVkParamsVideoSearch.Filters(const Value: TVkVideosFilters): TVkParamsVideoSearch;
begin
  List.Add('filters', Value.ToString);
  Result := Self;
end;

function TVkParamsVideoSearch.SearchOwn(const Value: Boolean): TVkParamsVideoSearch;
begin
  List.Add('search_own', Value);
  Result := Self;
end;

function TVkParamsVideoSearch.Offset(const Value: Integer): TVkParamsVideoSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsVideoSearch.Longer(const Value: Integer): TVkParamsVideoSearch;
begin
  List.Add('longer', Value);
  Result := Self;
end;

function TVkParamsVideoSearch.Shorter(const Value: Integer): TVkParamsVideoSearch;
begin
  List.Add('shorter', Value);
  Result := Self;
end;

function TVkParamsVideoSearch.Count(const Value: Integer): TVkParamsVideoSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsVideoSearch.Extended(const Value: Boolean): TVkParamsVideoSearch;
begin
  List.Add('extended', Value);
  Result := Self;
end;

end.

