unit VK.Video;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Video, System.JSON, VK.Entity.Status, VK.Entity.Media,
  VK.Entity.Video.Save;

type
  TVkParamsVideoGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежат видеозаписи
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома, видеозаписи из которого нужно вернуть
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// Определяет, возвращать ли информацию о настройках приватности видео для текущего пользователя
    /// </summary>
    function Extended(Value: Boolean): Integer;
    /// <summary>
    /// Количество возвращаемых видеозаписей
    /// </summary>
    function Count(Value: Integer): Integer;
    /// <summary>
    /// Смещение относительно первой найденной видеозаписи для выборки определенного подмножества
    /// </summary>
    function Offset(Value: Integer): Integer;
    /// <summary>
    /// Перечисленные через запятую идентификаторы — идущие через знак подчеркивания
    /// id пользователей, которым принадлежат видеозаписи, и id самих видеозаписей.
    /// Если видеозапись принадлежит сообществу, то в качестве первого параметра используется -id сообщества
    /// Примеры: 4363_136089719, 13245770_137352259, 1_129207899_220df2876123d3542f, 6492_135055734_e0a9bcc31144f67fbd
    /// </summary>
    function Videos(Value: TArrayOfString): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsVideoGetAlbums = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца альбомов (пользователь или сообщество).
    /// По умолчанию — идентификатор текущего пользователя
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Количество альбомов, информацию о которых нужно вернуть. По умолчанию: 50, максимальное значение: 100
    /// </summary>
    function Count(Value: Integer = 50): Integer;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества альбомов. По умолчанию: 0
    /// </summary>
    function Offset(Value: Integer = 0): Integer;
    /// <summary>
    /// True — возвращать дополнительные поля Count, UpdatedTime и массив объектов Image для каждого альбома.
    /// Если альбом пустой, то массив объектов Image для него возвращен не будет. По умолчанию: False
    /// </summary>
    function Extended(Value: Boolean): Integer;
    /// <summary>
    /// True — возвращать системные альбомы
    /// </summary>
    function NeedSystem(Value: Boolean): Integer;
  end;

  TVkParamsVideoAddToAlbum = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца видеозаписи
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор владельца альбома, в который нужно добавить видео
    /// </summary>
    function TargetId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома, в который нужно добавить видео.
    /// Для добавления видео в общий альбом «Добавленные» передавайте -2
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// Идентификаторы альбомов, в которые нужно добавить видео
    /// </summary>
    function AlbumIds(Value: TIdList): Integer;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoCreateComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// Текст комментария. Обязательный параметр, если не задан параметр Attachments
    /// </summary>
    function Message(Value: string): Integer;
    /// <summary>
    /// Список объектов, приложенных к комментарию
    /// </summary>
    function Attachments(Value: TAttachmentArray): Integer;
    /// <summary>
    /// Этот параметр учитывается, если owner_id меньше 0 (комментарий к видеозаписи группы).
    /// True — комментарий будет опубликован от имени группы,
    /// False — комментарий будет опубликован от имени пользователя. по умолчанию: False
    /// </summary>
    function FromGroup(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор комментария, в ответ на который должен быть добавлен новый комментарий
    /// </summary>
    function ReplyToComment(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор стикера
    /// </summary>
    function StickerId(Value: Integer): Integer;
    /// <summary>
    /// Уникальный идентификатор, предназначенный для предотвращения повторной отправки одинакового комментария
    /// </summary>
    function Guid(Value: string): Integer;
  end;

  TVkParamsVideoEdit = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// Новое название для видеозаписи
    /// </summary>
    function Name(Value: string): Integer;
    /// <summary>
    /// Новое описание для видеозаписи
    /// </summary>
    function Desc(Value: string): Integer;
    /// <summary>
    /// Настройки приватности просмотра видеозаписи в специальном формате.
    /// Приватность доступна для видеозаписей, которые пользователь загрузил в профиль
    /// </summary>
    function PrivacyView(Value: TArrayOfString): Integer;
    /// <summary>
    /// настройки приватности комментирования видеозаписи в специальном формате.
    /// Приватность доступна для видеозаписей, которые пользователь загрузил в профиль
    /// </summary>
    function PrivacyComment(Value: TArrayOfString): Integer;
    /// <summary>
    /// Закрыть комментарии (для видео из сообществ)
    /// </summary>
    function NoComments(Value: Boolean): Integer;
    /// <summary>
    /// Зацикливание воспроизведения видеозаписи
    /// </summary>
    function &Repeat(Value: Boolean): Integer;
  end;

  TVkParamsVideoEditAlbum = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества (если нужно отредактировать альбом, принадлежащий сообществу)
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// Новое название для альбома
    /// </summary>
    function Title(Value: string): Integer;
    /// <summary>
    /// Уровень доступа к альбому в специальном формате.
    /// Приватность доступна для альбомов с видео в профиле пользователя
    /// </summary>
    function Privacy(Value: TArrayOfString): Integer;
  end;

  TVkParamsVideoEditComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор комментария
    /// </summary>
    function CommentId(Value: Integer): Integer;
    /// <summary>
    /// Новый текст комментария. Обязательный параметр, если не задан параметр Attachments
    /// </summary>
    function Message(Value: string): Integer;
    /// <summary>
    /// Новый список объектов, приложенных к комментарию
    /// </summary>
    function Attachments(Value: TAttachmentArray): Integer;
  end;

  TVkParamsVideoGetComments = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// True — будет возвращено дополнительное поле Likes. По умолчанию поле Likes не возвращается
    /// </summary>
    function NeedLikes(Value: Boolean): Integer;
    /// <summary>
    /// Идентификатор комментария, начиная с которого нужно вернуть список
    /// </summary>
    function StartCommentId(Value: Integer): Integer;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества комментариев. По умолчанию: 0
    /// </summary>
    function Offset(Value: Integer = 0): Integer;
    /// <summary>
    /// Количество комментариев, информацию о которых необходимо вернуть
    /// </summary>
    function Count(Value: Integer = 20): Integer;
    /// <summary>
    /// Порядок сортировки комментариев
    /// </summary>
    function Sort(Value: TVkSort): Integer;
    /// <summary>
    /// True — возвращать дополнительные поля Count, UpdatedTime и массив объектов Image для каждого альбома.
    /// Если альбом пустой, то массив объектов image для него возвращен не будет. По умолчанию: False
    /// </summary>
    function Extended(Value: Boolean): Integer;
    /// <summary>
    /// Список дополнительных полей для профилей и сообществ, которые необходимо вернуть
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsVideoRemoveFromAlbum = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца альбома
    /// </summary>
    function TargetId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома, из которого нужно убрать видео
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// Идентификаторы альбомов, из которых нужно убрать видео
    /// </summary>
    function AlbumIds(Value: TIdList): Integer;
    /// <summary>
    /// Идентификатор владельца видеозаписи
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoReorderAlbums = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит альбом
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома, который нужно переместить
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома, перед которым нужно поместить текущий
    /// </summary>
    function Before(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома, после которого нужно поместить текущий
    /// </summary>
    function After(Value: Integer): Integer;
  end;

  TVkParamsVideoReorderVideos = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, в чьем альбоме нужно переместить видео
    /// </summary>
    function TargetId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома с видеозаписью, которую нужно переместить
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор владельца видеозаписи, которую нужно переместить (пользователь или сообщество)
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор видеозаписи, которую нужно переместить
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор владельца видеозаписи, перед которой следует поместить текущую (пользователь или сообщество)
    /// </summary>
    function BeforeOwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор видеозаписи, перед которой следует поместить текущую
    /// </summary>
    function BeforeVideoId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор владельца видеозаписи, после которой следует поместить текущую (пользователь или сообщество)
    /// </summary>
    function AfterOwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор видеозаписи, после которой следует поместить текущую
    /// </summary>
    function AfterVideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoReport = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит видеозапись
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор видеозаписи
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// Тип жалобы
    /// </summary>
    function Reason(Value: TVkMediaReportReason): Integer;
    /// <summary>
    /// Комментарий для жалобы
    /// </summary>
    function Comment(Value: string): Integer;
    /// <summary>
    /// Поисковой запрос, если видеозапись была найдена через поиск
    /// </summary>
    function SearchQuery(Value: string): Integer;
  end;

  TVkParamsVideoSave = record
    List: TParams;
    /// <summary>
    /// Название видеофайла
    /// </summary>
    function Name(Value: string): Integer;
    /// <summary>
    /// Описание видеофайла
    /// </summary>
    function Description(Value: string): Integer;
    /// <summary>
    /// Указывается True, если видео загружается для отправки личным сообщением.
    /// После загрузки с этим параметром видеозапись не будет отображаться
    /// в списке видеозаписей пользователя и не будет доступна другим
    /// пользователям по ее идентификатору. По умолчанию: False
    /// </summary>
    function IsPrivate(Value: Boolean = False): Integer;
    /// <summary>
    /// Требуется ли после сохранения опубликовать запись с видео на стене (True — требуется, False — не требуется).
    /// Обратите внимание, для публикации записи на стене приложение должно иметь права wall
    /// </summary>
    function Wallpost(Value: Boolean): Integer;
    /// <summary>
    /// Url для встраивания видео с внешнего сайта, например, с Youtube.
    /// В этом случае нужно вызвать полученный UploadUrl, не прикрепляя файл,
    /// достаточно просто обратиться по этому адресу
    /// </summary>
    function Link(Value: string): Integer;
    /// <summary>
    /// Идентификатор сообщества, в которое будет сохранен видеофайл.
    /// По умолчанию файл сохраняется на страницу текущего пользователя
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// Идентификатор альбома, в который будет загружен видео файл
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// Настройки приватности просмотра видеозаписи в специальном формате. Приватность доступна для видеозаписей, которые пользователь загрузил в профиль
    /// </summary>
    function PrivacyView(Value: TArrayOfString): Integer;
    /// <summary>
    /// Настройки приватности комментирования видеозаписи в специальном формате.
    /// Приватность доступна для видеозаписей, которые пользователь загрузил в профиль
    /// </summary>
    function PrivacyComment(Value: TArrayOfString): Integer;
    /// <summary>
    /// True — закрыть комментарии (для видео из сообществ). По умолчанию: False
    /// </summary>
    function NoComments(Value: Boolean = False): Integer;
    /// <summary>
    /// Зацикливание воспроизведения видеозаписи
    /// </summary>
    function &Repeat(Value: Boolean): Integer;
    /// <summary>
    /// Сжимать видео
    /// </summary>
    function Compression(Value: Boolean): Integer;
  end;

  TVkParamsVideoSearch = record
    List: TParams;
    /// <summary>
    /// Строка поискового запроса
    /// </summary>
    function Query(Value: string): Integer;
    /// <summary>
    /// Сортировка результатов
    /// </summary>
    function Sort(Value: TVkMediaSort): Integer;
    /// <summary>
    /// Поиск производится только по видеозаписям высокого качества
    /// </summary>
    function HD(Value: Boolean): Integer;
    /// <summary>
    /// Отключить фильтр «Безопасный поиск»
    /// </summary>
    function Adult(Value: Boolean): Integer;
    /// <summary>
    /// Список критериев, по которым требуется отфильтровать видео
    /// </summary>
    function Filters(Value: TVkVideosFilters): Integer;
    /// <summary>
    /// True — искать по видеозаписям пользователя, False — не искать по видеозаписям пользователя. По умолчанию: False
    /// </summary>
    function SearchOwn(Value: Boolean = False): Integer;
    /// <summary>
    /// Смещение относительно первой найденной видеозаписи для выборки определенного подмножества
    /// </summary>
    function Offset(Value: Integer): Integer;
    /// <summary>
    /// Количество секунд, видеозаписи длиннее которого необходимо вернуть
    /// </summary>
    function Longer(Value: Integer): Integer;
    /// <summary>
    /// Количество секунд, видеозаписи короче которого необходимо вернуть
    /// </summary>
    function Shorter(Value: Integer): Integer;
    /// <summary>
    /// Количество возвращаемых видеозаписей
    /// Обратите внимание — даже при использовании параметра Offset для получения информации доступны только первые 1000 результатов
    /// </summary>
    function Count(Value: Integer = 20): Integer;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups, содержащие информацию о пользователях и сообществах. По умолчанию: False
    /// </summary>
    function Extended(Value: Boolean): Integer;
  end;

  TVideoController = class(TVkController)
  public
    /// <summary>
    /// Добавляет видеозапись в список пользователя.
    /// </summary>
    function Add(const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean;
    /// <summary>
    /// Создает пустой альбом видеозаписей.
    /// </summary>
    function AddAlbum(var AlbumId: Integer; Title: string; Privacy: TArrayOfString = []; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Позволяет добавить видеозапись в альбом.
    /// </summary>
    function AddToAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет добавить видеозапись в альбом.
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
    /// Удаляет видеозапись со страницы пользователя.
    /// </summary>
    function Delete(const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет альбом видеозаписей.
    /// </summary>
    function DeleteAlbum(const AlbumId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// Удаляет комментарий к видеозаписи.
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// Cоздает новый комментарий к видеозаписи
    /// </summary>
    function Edit(Params: TVkParamsVideoEdit): Boolean; overload;
    /// <summary>
    /// Редактирует альбом с видео.
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует альбом с видео.
    /// </summary>
    function EditAlbum(Params: TVkParamsVideoEditAlbum): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к видеозаписи.
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к видеозаписи.
    /// </summary>
    function EditComment(Params: TVkParamsVideoEditComment): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о видеозаписях.
    /// </summary>
    function Get(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о видеозаписях.
    /// </summary>
    function Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео.
    /// </summary>
    function GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео.
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет получить информацию об альбоме с видео.
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean; overload;
    /// <summary>
    /// Возвращает список альбомов, в которых находится видеозапись.
    /// </summary>
    function GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к видеозаписи.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к видеозаписи.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsVideoGetComments): Boolean; overload;
    /// <summary>
    /// Позволяет убрать видеозапись из альбома.
    /// </summary>
    function RemoveFromAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет убрать видеозапись из альбома.
    /// </summary>
    function RemoveFromAlbum(Params: TVkParamsVideoRemoveFromAlbum): Boolean; overload;
    /// <summary>
    /// Позволяет изменить порядок альбомов с видео.
    /// </summary>
    function ReorderAlbums(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет изменить порядок альбомов с видео.
    /// </summary>
    function ReorderAlbums(Params: TVkParamsVideoReorderAlbums): Boolean; overload;
    /// <summary>
    /// Позволяет переместить видеозапись в альбоме.
    /// </summary>
    function ReorderVideos(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет переместить видеозапись в альбоме.
    /// </summary>
    function ReorderVideos(Params: TVkParamsVideoReorderVideos): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на видеозапись.
    /// </summary>
    function Report(Params: TParams): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на видеозапись.
    /// </summary>
    function Report(Params: TVkParamsVideoReport): Boolean; overload;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к видеозаписи.
    /// </summary>
    function ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean; overload;
    /// <summary>
    /// Восстанавливает удаленную видеозапись.
    /// </summary>
    function Restore(VideoId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Восстанавливает удаленный комментарий к видеозаписи.
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TVkParamsVideoSave): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера, необходимый для загрузки, и данные видеозаписи.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean; overload;
    /// <summary>
    /// Возвращает список видеозаписей в соответствии с заданным критерием поиска.
    /// </summary>
    function Search(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список видеозаписей в соответствии с заданным критерием поиска.
    /// </summary>
    function Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TVideoController }

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.getAlbums', Params).GetObject<TVkVideoAlbums>(Items);
end;

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean;
begin
  Result := GetAlbums(Items, Params.List);
end;

function TVideoController.GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Params.Add('extended', True);
  Result := Handler.Execute('video.getAlbumsByVideo', Params).GetObject<TVkVideoAlbums>(Items);
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
  Params.Add('reason', Reason.ToConst.ToString);
  Result := Handler.Execute('video.reportComment', Params).ResponseIsTrue;
end;

function TVideoController.Restore(VideoId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.restore', Params).ResponseIsTrue;
end;

function TVideoController.RestoreComment(CommentId, OwnerId: Integer): Boolean;
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
  Result := Handler.Execute('video.save', Params).GetObject<TVkVideoSaved>(VideoSaved);
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
  Result := Handler.Execute('video.getComments', Params).GetObject<TVkComments>(Items);
end;

function TVideoController.GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.getAlbumById', Params).GetObject<TVkVideoAlbum>(Item);
end;

function TVideoController.Get(var Items: TVkVideos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.get', Params).GetObject<TVkVideos>(Items);
end;

function TVideoController.Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TVideoController.Add(const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Result := Handler.Execute('video.add', Params).ResponseIsTrue;
end;

function TVideoController.AddAlbum(var AlbumId: Integer; Title: string; Privacy: TArrayOfString; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('title', Title);
  if Length(Privacy) > 0 then
    Params.Add('privacy', Privacy);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('video.addAlbum', Params).ResponseAsInt(AlbumId);
end;

function TVideoController.AddToAlbum(Params: TVkParamsVideoAddToAlbum): Boolean;
begin
  Result := AddToAlbum(Params.List);
end;

function TVideoController.CreateComment(var CommentId: Integer; Params: TVkParamsVideoCreateComment): Boolean;
begin
  Result := CreateComment(CommentId, Params.List);
end;

function TVideoController.Delete(const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Result := Handler.Execute('video.delete', Params).ResponseIsTrue;
end;

function TVideoController.DeleteAlbum(const AlbumId: Integer; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('video.deleteAlbum', Params).ResponseIsTrue;
end;

function TVideoController.DeleteComment(const CommentId: Integer; OwnerId: Integer): Boolean;
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
var
  Params: TParams;
  SaveResp: string;
begin
  Params.Add('link', Link);
  Result := Handler.Execute('video.save', Params).GetObject<TVkVideoSaved>(VideoSaved);
  if Result then
  begin
    Result := False;
    if TCustomVK(VK).Upload(VideoSaved.UploadUrl, [''], SaveResp) then
      Result := not SaveResp.IsEmpty
    else
      TCustomVK(VK).DoError(Self, TVkException.Create(SaveResp), -1, SaveResp);
  end;
end;

function TVideoController.Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TVideoController.Search(var Items: TVkVideos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.search', Params).GetObject<TVkVideos>(Items);
end;

{ TVkVideosGetParams }

function TVkParamsVideoGet.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGet.Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsVideoGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoGet.Videos(Value: TArrayOfString): Integer;
begin
  Result := List.Add('videos', Value.ToString);
end;

{ TVkParamsVideoAlbumsGet }

function TVkParamsVideoGetAlbums.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGetAlbums.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGetAlbums.NeedSystem(Value: Boolean): Integer;
begin
  Result := List.Add('need_system', Value);
end;

function TVkParamsVideoGetAlbums.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGetAlbums.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsVideosAddToAlbum }

function TVkParamsVideoAddToAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoAddToAlbum.AlbumIds(Value: TIdList): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsVideoAddToAlbum.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoAddToAlbum.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoAddToAlbum.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

{ TVkParamsVideosCreateComment }

function TVkParamsVideoCreateComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoCreateComment.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoCreateComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsVideoCreateComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToStrings);
end;

function TVkParamsVideoCreateComment.FromGroup(Value: Integer): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsVideoCreateComment.ReplyToComment(Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkParamsVideoCreateComment.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsVideoCreateComment.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

{ TVkParamsVideosEdit }

function TVkParamsVideoEdit.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoEdit.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoEdit.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsVideoEdit.Desc(Value: string): Integer;
begin
  Result := List.Add('desc', Value);
end;

function TVkParamsVideoEdit.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsVideoEdit.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsVideoEdit.NoComments(Value: Boolean): Integer;
begin
  Result := List.Add('no_comments', Value);
end;

function TVkParamsVideoEdit.&Repeat(Value: Boolean): Integer;
begin
  Result := List.Add('repeat', Value);
end;

{ TVkParamsVideosEditAlbum }

function TVkParamsVideoEditAlbum.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsVideoEditAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoEditAlbum.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsVideoEditAlbum.Privacy(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy', Value);
end;

{ TVkParamsVideosEditComment }

function TVkParamsVideoEditComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoEditComment.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsVideoEditComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsVideoEditComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToStrings);
end;

{ TVkParamsVideoGetComments }

function TVkParamsVideoGetComments.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoGetComments.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoGetComments.NeedLikes(Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsVideoGetComments.StartCommentId(Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsVideoGetComments.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGetComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGetComments.Sort(Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsVideoGetComments.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGetComments.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

{ TVkParamsVideoRemoveFromAlbum }

function TVkParamsVideoRemoveFromAlbum.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.AlbumIds(Value: TIdList): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsVideoRemoveFromAlbum.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

{ TVkParamsVideoReorderAlbums }

function TVkParamsVideoReorderAlbums.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReorderAlbums.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoReorderAlbums.Before(Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsVideoReorderAlbums.After(Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

{ TVkParamsVideoReorderVideos }

function TVkParamsVideoReorderVideos.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoReorderVideos.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoReorderVideos.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReorderVideos.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoReorderVideos.BeforeOwnerId(Value: Integer): Integer;
begin
  Result := List.Add('before_owner_id', Value);
end;

function TVkParamsVideoReorderVideos.BeforeVideoId(Value: Integer): Integer;
begin
  Result := List.Add('before_video_id', Value);
end;

function TVkParamsVideoReorderVideos.AfterOwnerId(Value: Integer): Integer;
begin
  Result := List.Add('after_owner_id', Value);
end;

function TVkParamsVideoReorderVideos.AfterVideoId(Value: Integer): Integer;
begin
  Result := List.Add('after_video_id', Value);
end;

{ TVkParamsVideosReport }

function TVkParamsVideoReport.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReport.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoReport.Reason(Value: TVkMediaReportReason): Integer;
begin
  Result := List.Add('reason', Value.ToConst.ToString);
end;

function TVkParamsVideoReport.Comment(Value: string): Integer;
begin
  Result := List.Add('comment', Value);
end;

function TVkParamsVideoReport.SearchQuery(Value: string): Integer;
begin
  Result := List.Add('search_query', Value);
end;

{ TVkParamsVideosSave }

function TVkParamsVideoSave.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsVideoSave.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsVideoSave.IsPrivate(Value: Boolean): Integer;
begin
  Result := List.Add('is_private', Value);
end;

function TVkParamsVideoSave.Wallpost(Value: Boolean): Integer;
begin
  Result := List.Add('wallpost', Value);
end;

function TVkParamsVideoSave.Link(Value: string): Integer;
begin
  Result := List.Add('link', Value);
end;

function TVkParamsVideoSave.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsVideoSave.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoSave.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsVideoSave.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsVideoSave.NoComments(Value: Boolean): Integer;
begin
  Result := List.Add('no_comments', Value);
end;

function TVkParamsVideoSave.&Repeat(Value: Boolean): Integer;
begin
  Result := List.Add('repeat', Value);
end;

function TVkParamsVideoSave.Compression(Value: Boolean): Integer;
begin
  Result := List.Add('compression', Value);
end;

{ TVkParamsVideosSearch }

function TVkParamsVideoSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsVideoSearch.Sort(Value: TVkMediaSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsVideoSearch.HD(Value: Boolean): Integer;
begin
  Result := List.Add('hd', Value);
end;

function TVkParamsVideoSearch.Adult(Value: Boolean): Integer;
begin
  Result := List.Add('adult', Value);
end;

function TVkParamsVideoSearch.Filters(Value: TVkVideosFilters): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsVideoSearch.SearchOwn(Value: Boolean): Integer;
begin
  Result := List.Add('search_own', Value);
end;

function TVkParamsVideoSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoSearch.Longer(Value: Integer): Integer;
begin
  Result := List.Add('longer', Value);
end;

function TVkParamsVideoSearch.Shorter(Value: Integer): Integer;
begin
  Result := List.Add('shorter', Value);
end;

function TVkParamsVideoSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoSearch.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

end.

