unit VK.Photos;

interface

uses
  System.SysUtils, System.Types, System.Generics.Collections, System.Classes,
  VK.Controller, VK.Types, VK.Entity.Album, REST.Json, VK.Entity.Photo.Upload,
  VK.Entity.Photo, VK.Entity.Media, VK.Entity.Group, VK.Entity.Common,
  VK.Entity.Common.ExtendedList;

type
  TVkParamsPhotosGetAll = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, фотографии которого нужно получить
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosGetAll;
    /// <summary>
    /// True — возвращать расширенную информацию о фотографиях
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsPhotosGetAll;
    /// <summary>
    /// Число фотографий, информацию о которых необходимо получить
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsPhotosGetAll;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества фотографий. По умолчанию — 0
    /// </summary>
    function Offset(const Value: Integer): TVkParamsPhotosGetAll;
    /// <summary>
    /// True— будут возвращены размеры фотографий в специальном формате
    /// </summary>
    function PhotoSizes(const Value: Boolean): TVkParamsPhotosGetAll;
    /// <summary>
    /// False — вернуть все фотографии, включая находящиеся в сервисных альбомах, таких как "Фотографии на моей стене" (по умолчанию);
    /// True — вернуть фотографии только из стандартных альбомов пользователя или сообщества
    /// </summary>
    function NoServiceAlbums(const Value: Boolean): TVkParamsPhotosGetAll;
    /// <summary>
    /// True — возвращает информацию от том, скрыта ли фотография из блока над стеной пользователя
    /// </summary>
    function NeedHidden(const Value: Boolean): TVkParamsPhotosGetAll;
    /// <summary>
    /// True — не возвращать фотографии, скрытые из блока над стеной пользователя
    /// (параметр учитывается только при OwnerId больше 0, параметр NoServiceAlbums игнорируется)
    /// </summary>
    function SkipHidden(const Value: Boolean): TVkParamsPhotosGetAll;
  end;

  TVkParamsPhotosGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор владельца альбома
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosGet;
    /// <summary>
    /// Идентификатор альбома
    /// </summary>
    function AlbumId(const Value: Int64): TVkParamsPhotosGet; overload;
    /// <summary>
    /// Идентификатор альбома
    /// </summary>
    function AlbumId(const Value: TVkPhotoSystemAlbum): TVkParamsPhotosGet; overload;
    /// <summary>
    /// Идентификаторы фотографий, информацию о которых необходимо вернуть
    /// </summary>
    function PhotoIds(const Value: TArrayOfInteger): TVkParamsPhotosGet;
    /// <summary>
    /// Идентификатор фотографии, информацию о которой необходимо вернуть
    /// </summary>
    function PhotoId(const Value: Int64): TVkParamsPhotosGet;
    /// <summary>
    /// True — будут возвращены дополнительные поля likes, comments, tags, can_comment, reposts. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsPhotosGet;
    /// <summary>
    /// Количество записей, которое будет получено
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsPhotosGet;
    /// <summary>
    /// Отступ, необходимый для получения определенного подмножества записей
    /// </summary>
    function Offset(const Value: Integer): TVkParamsPhotosGet;
    /// <summary>
    /// True — возвращать доступные размеры фотографии в специальном формате. По умолчанию: False
    /// </summary>
    function PhotoSizes(const Value: Boolean): TVkParamsPhotosGet;
    /// <summary>
    /// Тип новости, получаемый в поле type метода newsfeed.get, для получения
    /// только загруженных пользователем фотографий, либо только фотографий, на
    /// которых он был отмечен. Может принимать значения photo, photo_tag
    /// </summary>
    function FeedType(const Value: TVkPhotoFeedType): TVkParamsPhotosGet;
    /// <summary>
    /// Время в формате, который может быть получен методом newsfeed.get в поле date,
    /// для получения всех фотографий загруженных пользователем в определённый день
    /// либо на которых пользователь был отмечен. Также нужно указать параметр uid пользователя, с которым произошло событие.
    /// Значение должно отличаться от текущего времени не более, чем на месяц.
    /// </summary>
    function Feed(const Value: Integer): TVkParamsPhotosGet;
    /// <summary>
    /// Порядок сортировки фотографий
    ///  True — антихронологический;
    ///  False — хронологический.
    /// </summary>
    function Rev(const Value: Boolean): TVkParamsPhotosGet;
    /// <summary>
    /// [Нет описания]
    /// </summary>
    function Uid(const Value: Integer): TVkParamsPhotosGet;
  end;

  TVkParamsAlbumsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежат альбомы
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsAlbumsGet;
    /// <summary>
    /// Перечисленные через запятую идентификаторы альбомов (не более 1000)
    /// </summary>
    function AlbumIds(const Value: TArrayOfInteger): TVkParamsAlbumsGet; overload;
    /// <summary>
    /// Перечисленные через запятую идентификаторы альбомов
    /// </summary>
    function AlbumIds(const Value: Integer): TVkParamsAlbumsGet; overload;
    /// <summary>
    /// Количество альбомов, которое нужно вернуть. (по умолчанию возвращаются все альбомы)
    /// </summary>
    function Count(const Value: Integer): TVkParamsAlbumsGet;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества альбомов
    /// </summary>
    function Offset(const Value: Integer): TVkParamsAlbumsGet;
    /// <summary>
    /// True — размеры фотографий будут возвращены в специальном формате
    /// </summary>
    function PhotoSizes(const Value: Boolean): TVkParamsAlbumsGet;
    /// <summary>
    /// True — будут возвращены системные альбомы, имеющие отрицательные идентификаторы.
    /// Обратите внимание, что информация о системных альбомах возвращается даже в том случае, если они не содержат фотографий
    /// </summary>
    function NeedSystem(const Value: Boolean): TVkParamsAlbumsGet;
    /// <summary>
    /// True — будет возвращено дополнительное поле thumb_src с адресом изображения-обложки. По умолчанию поле thumb_src не возвращается
    /// </summary>
    function NeedCovers(const Value: Boolean): TVkParamsAlbumsGet;
  end;

  TVkParamsPhotosCreateAlbum = record
    List: TParams;
    /// <summary>
    /// Название альбома
    /// </summary>
    function Title(const Value: string): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// Идентификатор сообщества, в котором создаётся альбом
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// Текст описания альбома
    /// </summary>
    function Description(const Value: string): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// Настройки приватности просмотра альбома в специальном формате
    /// </summary>
    function PrivacyView(const Value: TVkPrivacySettings): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// Настройки приватности комментирования альбома в специальном формате
    /// </summary>
    function PrivacyComment(const Value: TVkPrivacySettings): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// Кто может загружать фотографии в альбом (только для альбома сообщества)
    /// False — фотографии могут добавлять все пользователи;
    /// True — фотографии могут добавлять только редакторы и администраторы
    /// </summary>
    function UploadByAdminsOnly(const Value: Boolean): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// Отключено ли комментирование альбома (только для альбома сообщества)
    /// False — комментирование включено;
    /// True — комментирование отключено
    /// </summary>
    function CommentsDisabled(const Value: Boolean): TVkParamsPhotosCreateAlbum;
  end;

  TVkParamsPhotosEditAlbum = record
    List: TParams;
    /// <summary>
    /// Идентификатор альбома
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// Название альбома
    /// </summary>
    function Title(const Value: string): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// Текст описания альбома
    /// </summary>
    function Description(const Value: string): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежат альбомы
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// Настройки приватности просмотра альбома в специальном формате
    /// </summary>
    function PrivacyView(const Value: TArrayOfString): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// Настройки приватности комментирования альбома в специальном формате
    /// </summary>
    function PrivacyComment(const Value: TArrayOfString): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// Кто может загружать фотографии в альбом (только для альбома сообщества)
    /// False — фотографии могут добавлять все пользователи;
    /// True — фотографии могут добавлять только редакторы и администраторы
    /// </summary>
    function UploadByAdminsOnly(const Value: Boolean): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// Отключено ли комментирование альбома (только для альбома сообщества)
    /// False — комментирование включено;
    /// True — комментирование отключено
    /// </summary>
    function CommentsDisabled(const Value: Boolean): TVkParamsPhotosEditAlbum;
  end;

  TVkParamsPhotosCreateComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит фотография
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosCreateComment;
    /// <summary>
    /// Идентификатор фотографии
    /// </summary>
    function PhotoId(const Value: Integer): TVkParamsPhotosCreateComment;
    /// <summary>
    /// Текст комментария (является обязательным, если не задан параметр Attachments).
    /// Максимальное количество символов: 2048
    /// </summary>
    function Message(const Value: string): TVkParamsPhotosCreateComment;
    /// <summary>
    /// Список объектов, приложенных к комментарию
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsPhotosCreateComment;
    /// <summary>
    /// Данный параметр учитывается, если OwnerId меньше 0 (комментарий к фотографии группы)
    ///  True — комментарий будет опубликован от имени группы;
    ///  False — комментарий будет опубликован от имени пользователя
    /// </summary>
    function FromGroup(const Value: Boolean = False): TVkParamsPhotosCreateComment;
    /// <summary>
    /// Идентификатор комментария, в ответ на который нужно оставить текущий
    /// </summary>
    function ReplyToComment(const Value: Integer): TVkParamsPhotosCreateComment;
    /// <summary>
    /// Идентификатор стикера, который нужно прикрепить к комментарию
    /// </summary>
    function StickerId(const Value: Cardinal): TVkParamsPhotosCreateComment;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    function AccessKey(const Value: string): TVkParamsPhotosCreateComment;
    /// <summary>
    /// Уникальное значение для предотвращения повторной отправки одного и того же комментария
    /// </summary>
    function Guid(const Value: string): TVkParamsPhotosCreateComment;
  end;

  TVkParamsPhotosEdit = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит фотография
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosEdit;
    /// <summary>
    /// Идентификатор фотографии
    /// </summary>
    function PhotoId(const Value: Integer): TVkParamsPhotosEdit;
    /// <summary>
    /// Новый текст описания к фотографии. Если параметр не задан, то считается, что он равен пустой строке
    /// </summary>
    function Caption(const Value: string): TVkParamsPhotosEdit;
    /// <summary>
    /// Географическая широта
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsPhotosEdit;
    /// <summary>
    /// Географическая долгота
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsPhotosEdit;
    /// <summary>
    /// Название места
    /// </summary>
    function PlaceStr(const Value: string): TVkParamsPhotosEdit;
    /// <summary>
    /// Id в Foursquare
    /// </summary>
    function FoursquareId(const Value: string): TVkParamsPhotosEdit;
    /// <summary>
    /// Удалить место (False — не удалять, True — удалить)
    /// </summary>
    function DeletePlace(const Value: Boolean = False): TVkParamsPhotosEdit;
  end;

  TVkParamsPhotosSave = record
    List: TParams;
    /// <summary>
    /// Идентификатор альбома, в который необходимо сохранить фотографии
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsPhotosSave;
    /// <summary>
    /// Идентификатор сообщества, в которое необходимо сохранить фотографии
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsPhotosSave;
    /// <summary>
    /// Параметр, возвращаемый в результате загрузки фотографий на сервер
    /// </summary>
    function Server(const Value: string): TVkParamsPhotosSave;
    /// <summary>
    /// Параметр, возвращаемый в результате загрузки фотографий на сервер
    /// </summary>
    function PhotosList(const Value: TArrayOfString): TVkParamsPhotosSave;
    /// <summary>
    /// Параметр, возвращаемый в результате загрузки фотографий на сервер
    /// </summary>
    function Hash(const Value: string): TVkParamsPhotosSave;
    /// <summary>
    /// Географическая широта, заданная в градусах (от -90 до 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsPhotosSave;
    /// <summary>
    /// Географическая долгота, заданная в градусах (от -180 до 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsPhotosSave;
    /// <summary>
    /// Текст описания фотографии (максимум 2048 символов)
    /// </summary>
    function Caption(const Value: string): TVkParamsPhotosSave;
  end;

  TVkParamsPhotosEditComment = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит фотография
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosEditComment;
    /// <summary>
    /// Идентификатор комментария
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsPhotosEditComment;
    /// <summary>
    /// Новый текст комментария. Обязательный параметр, если не задан параметр attachments.
    /// Максимальное количество символов: 2048
    /// </summary>
    function Message(const Value: string): TVkParamsPhotosEditComment;
    /// <summary>
    /// Новый список объектов, приложенных к комментарию
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsPhotosEditComment;
  end;

  TVkParamsPhotosGetAlbumsCount = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, количество альбомов которого необходимо получить
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsPhotosGetAlbumsCount;
    /// <summary>
    /// Идентификатор сообщества, количество альбомов которого необходимо получить
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsPhotosGetAlbumsCount;
  end;

  TVkParamsPhotosGetAllComments = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежат фотографии
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosGetAllComments;
    /// <summary>
    /// Идентификатор альбома. Если параметр не задан, то считается, что необходимо получить комментарии ко всем альбомам пользователя или сообщества
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsPhotosGetAllComments;
    /// <summary>
    /// True — будет возвращено дополнительное поле likes. По умолчанию поле likes не возвращается
    /// </summary>
    function NeedLikes(const Value: Boolean): TVkParamsPhotosGetAllComments;
    /// <summary>
    /// Количество комментариев, которое необходимо получить. Если параметр не задан, то считается что он равен 20. Максимальное значение параметра 100.
    /// Обратите внимание, даже при использовании параметра offset для получения доступны только первые 10000 комментариев.
    /// </summary>
    function Count(const Value: Integer): TVkParamsPhotosGetAllComments;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества комментариев.
    /// Если параметр не задан, то считается, что он равен 0
    /// </summary>
    function Offset(const Value: Integer): TVkParamsPhotosGetAllComments;
  end;

  TVkParamsPhotosGetComments = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит фотография
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosGetComments;
    /// <summary>
    /// Идентификатор фотографии
    /// </summary>
    function PhotoId(const Value: Integer): TVkParamsPhotosGetComments;
    /// <summary>
    /// True — будет возвращено дополнительное поле likes. По умолчанию: False
    /// </summary>
    function NeedLikes(const Value: Boolean = False): TVkParamsPhotosGetComments;
    /// <summary>
    /// Количество комментариев, которое необходимо получить
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsPhotosGetComments;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества комментариев. По умолчанию: 0
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsPhotosGetComments;
    /// <summary>
    /// Идентификатор комментария, начиная с которого нужно вернуть список
    /// </summary>
    function StartCommentId(const Value: Integer): TVkParamsPhotosGetComments;
    /// <summary>
    /// Порядок сортировки комментариев
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsPhotosGetComments;
    /// <summary>
    /// Ключ доступа к фотографии
    /// </summary>
    function AccessKey(const Value: string): TVkParamsPhotosGetComments;
    /// <summary>
    /// True — в ответе будут возвращены дополнительные поля profiles и groups,
    /// содержащие информацию о пользователях и сообществах. По умолчанию: False
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsPhotosGetComments;
    /// <summary>
    /// Список дополнительных полей профилей, которые необходимо вернуть
    /// </summary>
    function Fields(const Value: TVkExtendedFields): TVkParamsPhotosGetComments;
  end;

  TVkParamsPhotosGetMarketUploadServer = record
    List: TParams;
    /// <summary>
    /// Идентификатор сообщества, для которого необходимо загрузить фотографию товара
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsPhotosGetMarketUploadServer;
    /// <summary>
    /// Является ли фотография обложкой товара (True — фотография для обложки, False — дополнительная фотография)
    /// </summary>
    function MainPhoto(const Value: Boolean): TVkParamsPhotosGetMarketUploadServer;
    /// <summary>
    /// Координаты для обрезки фотографии (верхний правый угол)
    /// </summary>
    function Crop(const Value: TPoint): TVkParamsPhotosGetMarketUploadServer;
    /// <summary>
    /// Ширина фотографии после обрезки в px (минимальное значение 400)
    /// </summary>
    function CropWidth(const Value: Integer): TVkParamsPhotosGetMarketUploadServer;
  end;

  TVkParamsPhotosGetUserPhotos = record
    List: TParams;
    /// <summary>
    /// идентификатор пользователя, список фотографий для которого нужно получить
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsPhotosGetUserPhotos;
    /// <summary>
    /// True — будут возвращены дополнительные поля likes, comments, tags, can_comment.
    /// Поля comments и tags содержат только количество объектов. По умолчанию данные поля не возвращается
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsPhotosGetUserPhotos;
    /// <summary>
    /// Количество фотографий, которое необходимо получить
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsPhotosGetUserPhotos;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества фотографий
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsPhotosGetUserPhotos;
    /// <summary>
    /// Сортировка результатов (по дате добавления отметки)
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsPhotosGetUserPhotos;
  end;

  TVkParamsPhotosReorderAlbums = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит альбом
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosReorderAlbums;
    /// <summary>
    /// Идентификатор альбома
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsPhotosReorderAlbums;
    /// <summary>
    /// Идентификатор альбома, перед которым следует поместить альбом
    /// </summary>
    function Before(const Value: Integer): TVkParamsPhotosReorderAlbums;
    /// <summary>
    /// Идентификатор альбома, после которого следует поместить альбом
    /// </summary>
    function After(const Value: Integer): TVkParamsPhotosReorderAlbums;
  end;

  TVkParamsPhotosReorderPhotos = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежит фотография
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosReorderPhotos;
    /// <summary>
    /// Идентификатор фотографии
    /// </summary>
    function PhotoId(const Value: Integer): TVkParamsPhotosReorderPhotos;
    /// <summary>
    /// Идентификатор фотографии, перед которой следует поместить фотографию. Если параметр не указан, фотография будет помещена последней
    /// </summary>
    function Before(const Value: Integer): TVkParamsPhotosReorderPhotos;
    /// <summary>
    /// Идентификатор фотографии, после которой следует поместить фотографию. Если параметр не указан, фотография будет помещена первой
    /// </summary>
    function After(const Value: Integer): TVkParamsPhotosReorderPhotos;
  end;

  TVkParamsPhotosSaveMarketPhoto = record
    List: TParams;
    /// <summary>
    /// Иентификатор группы, для которой нужно загрузить фотографию
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// Праметр, возвращаемый в результате загрузки фотографии на сервер
    /// </summary>
    function Photo(const Value: string): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// Праметр, возвращаемый в результате загрузки фотографии на сервер
    /// </summary>
    function Server(const Value: Integer): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// Праметр, возвращаемый в результате загрузки фотографии на сервер
    /// </summary>
    function Hash(const Value: string): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// Параметр, возвращаемый в результате загрузки фотографии на сервер.
    /// Обязательный параметр, если на этапе загрузки фото был передан MainPhoto = True
    /// </summary>
    function CropData(const Value: string): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// Параметр, возвращаемый в результате загрузки фотографии на сервер.
    /// Обязательный параметр, если на этапе загрузки фото был передан MainPhoto = True
    /// </summary>
    function CropHash(const Value: string): TVkParamsPhotosSaveMarketPhoto;
  end;

  TVkParamsPhotosSaveWallPhoto = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя, на стену которого нужно сохранить фотографию
    /// </summary>
    function UserId(const Value: TVkPeerId): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// Идентификатор сообщества, на стену которого нужно сохранить фотографию
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// Параметр, возвращаемый в результате загрузки фотографии на сервер
    /// </summary>
    function Photo(const Value: string): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// Параметр, возвращаемый в результате загрузки фотографии на сервер
    /// </summary>
    function Server(const Value: Integer): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// Параметр, возвращаемый в результате загрузки фотографии на сервер
    /// </summary>
    function Hash(const Value: string): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// Географическая широта, заданная в градусах (от -90 до 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// Географическая долгота, заданная в градусах (от -180 до 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// Текст описания фотографии (максимум 2048 символов)
    /// </summary>
    function Caption(const Value: string): TVkParamsPhotosSaveWallPhoto;
  end;

  TVkParamsPhotosSearch = record
    List: TParams;
    /// <summary>
    /// Строка поискового запроса, например
    /// </summary>
    function Query(const Value: string): TVkParamsPhotosSearch;
    /// <summary>
    /// Географическая широта отметки, заданная в градусах (от -90 до 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsPhotosSearch;
    /// <summary>
    /// Географическая долгота отметки, заданная в градусах (от -180 до 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsPhotosSearch;
    /// <summary>
    /// Время в формате unixtime, не раньше которого должны были быть загружены найденные фотографии
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsPhotosSearch;
    /// <summary>
    /// Время в формате unixtime, не позже которого должны были быть загружены найденные фотографии
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsPhotosSearch;
    /// <summary>
    /// Сортировка результатов
    /// </summary>
    function Sort(const Value: TVkPhotoSort): TVkParamsPhotosSearch;
    /// <summary>
    /// Смещение относительно первой найденной фотографии для выборки определенного подмножества
    /// </summary>
    function Offset(const Value: Integer): TVkParamsPhotosSearch;
    /// <summary>
    /// Количество возвращаемых фотографий (максимальное значение 1000)
    /// </summary>
    function Count(const Value: Integer = 100): TVkParamsPhotosSearch;
    /// <summary>
    /// Радиус поиска в метрах. (работает очень приближенно, поэтому реальное расстояние до цели может отличаться от заданного). Может принимать значения: 10, 100, 800, 6000, 50000
    /// </summary>
    function Radius(const Value: Integer = 5000): TVkParamsPhotosSearch;
  end;

  TPhotosController = class(TVkController)
  public
    /// <summary>
    /// Подтверждает отметку на фотографии.
    /// </summary>
    function СonfirmTag(PhotoId, TagId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Подтверждает отметки.
    /// </summary>
    function ConfirmTags(Tags: TIdList): Boolean;
    /// <summary>
    /// Позволяет скопировать фотографию в альбом "Сохраненные фотографии"
    /// </summary>
    function Copy(var Id: Integer; OwnerId: TVkPeerId; PhotoId: Integer; AccessKey: string = ''): Boolean;
    /// <summary>
    /// Создает пустой альбом для фотографий.
    /// </summary>
    function CreateAlbum(var Item: TVkPhotoAlbum; Params: TParams): Boolean; overload;
    /// <summary>
    /// Создает пустой альбом для фотографий.
    /// </summary>
    function CreateAlbum(var Item: TVkPhotoAlbum; Params: TVkParamsPhotosCreateAlbum): Boolean; overload;
    /// <summary>
    /// Создает новый комментарий к фотографии.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// Создает новый комментарий к фотографии.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TVkParamsPhotosCreateComment): Boolean; overload;
    /// <summary>
    /// хз че делает)
    /// https://vk.com/dev/photos.declineTags
    /// </summary>
    function DeclineTags: Boolean;
    /// <summary>
    /// Удаление фотографии на сайте.
    /// </summary>
    function Delete(OwnerId: TVkPeerId; PhotoId: Integer): Boolean;
    /// <summary>
    /// Удаляет указанный альбом для фотографий у текущего пользователя
    /// </summary>
    function DeleteAlbum(AlbumId: Integer; GroupId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Удаляет комментарий к фотографии.
    /// </summary>
    function DeleteComment(OwnerId: TVkPeerId; CommentId: Integer): Boolean;
    /// <summary>
    /// Редактирует описание или геометку у фотографии.
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует описание или геометку у фотографии.
    /// </summary>
    function Edit(Params: TVkParamsPhotosEdit): Boolean; overload;
    /// <summary>
    /// Редактирует данные альбома для фотографий.
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// Редактирует данные альбома для фотографий.
    /// </summary>
    function EditAlbum(Params: TVkParamsPhotosEditAlbum): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к фотографии.
    /// Обратите внимание, что редактирование комментария доступно только в течение суток после его создания.
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// Изменяет текст комментария к фотографии.
    /// Обратите внимание, что редактирование комментария доступно только в течение суток после его создания.
    /// </summary>
    function EditComment(Params: TVkParamsPhotosEditComment): Boolean; overload;
    /// <summary>
    /// Возвращает список фотографий в альбоме.
    /// </summary>
    function Get(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список фотографий в альбоме.
    /// </summary>
    function Get(var Items: TVkPhotos; Params: TVkParamsPhotosGet): Boolean; overload;
    /// <summary>
    /// Возвращает список фотоальбомов пользователя или сообщества.
    /// </summary>
    function GetAlbums(var Items: TVkPhotoAlbums; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список фотоальбомов пользователя или сообщества.
    /// </summary>
    function GetAlbums(var Items: TVkPhotoAlbums; Params: TVkParamsAlbumsGet): Boolean; overload;
    /// <summary>
    /// Возвращает количество доступных альбомов пользователя или сообщества.
    /// </summary>
    function GetAlbumsCount(var Count: Integer; Params: TVkParamsPhotosGetAlbumsCount): Boolean;
    /// <summary>
    /// Возвращает все фотографии пользователя или сообщества в антихронологическом порядке.
    /// </summary>
    function GetAll(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает все фотографии пользователя или сообщества в антихронологическом порядке.
    /// </summary>
    function GetAll(var Items: TVkPhotos; Params: TVkParamsPhotosGetAll): Boolean; overload;
    /// <summary>
    /// Возвращает отсортированный в антихронологическом порядке список всех комментариев к конкретному альбому или ко всем альбомам пользователя.
    /// </summary>
    function GetAllComments(var Items: TVkComments; Params: TVkParamsPhotosGetAllComments): Boolean;
    /// <summary>
    /// Возвращает информацию о фотографиях по их идентификаторам.
    /// <b>Photos</b> - перечисленные через запятую идентификаторы, которые представляют собой идущие через знак подчеркивания id пользователей, разместивших фотографии, и id самих фотографий. Чтобы получить информацию о фотографии в альбоме группы, вместо id пользователя следует указать -id группы. Пример значения photos: 1_263219656,6492_456239863,-1_456239099
    /// Некоторые фотографии, идентификаторы которых могут быть получены через API, закрыты приватностью, и не будут получены. В этом случае следует использовать ключ доступа фотографии (access_key) в её идентификаторе.
    /// Пример значения photos: 1_129207899_220df2876123d3542f, 6492_135055734_e0a9bcc31144f67fbd
    /// Поле access_key будет возвращено вместе с остальными данными фотографии в методах, которые возвращают фотографии, закрытые приватностью но доступные в данном контексте. Например данное поле имеют фотографии, возвращаемые методом newsfeed.get.
    /// </summary>
    function GetById(var Items: TVkPhotos; Photos: TArrayOfString; Extended: Boolean = False; PhotoSizes: Boolean = False): Boolean; overload;
    /// <summary>
    /// Позволяет получить адрес для загрузки обложки чата.
    /// </summary>
    function GetChatUploadServer(var UploadUrl: string; ChatId: Integer; Crop: TPoint; CropWidth: Integer = 0): Boolean;
    /// <summary>
    /// Возвращает список комментариев к фотографии.
    /// Если был передан параметр StartCommentId, будет найдена позиция комментария в списке (или ближайший к нему более ранний). Начиная с этой позиции будет возвращено Count комментариев. Смещение Offset в этом случае будет отсчитываться от этой позиции (оно может быть отрицательным)
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список комментариев к фотографии.
    /// Если был передан параметр StartCommentId, будет найдена позиция комментария в списке (или ближайший к нему более ранний). Начиная с этой позиции будет возвращено Count комментариев. Смещение Offset в этом случае будет отсчитываться от этой позиции (оно может быть отрицательным)
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsPhotosGetComments): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии подборки товаров в сообществе.
    /// Минимальный размер фотографии — 1280x720 пикселей.
    /// </summary>
    function GetMarketAlbumUploadServer(var UploadUrl: string; GroupId: TVkPeerId): Boolean;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии товара.
    /// </summary>
    function GetMarketUploadServer(var UploadUrl: string; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии товара.
    /// </summary>
    function GetMarketUploadServer(var UploadUrl: string; Params: TVkParamsPhotosGetMarketUploadServer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии в личное сообщение.
    /// </summary>
    function GetMessagesUploadServer(var UploadUrl: string; PeerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии в личное сообщение.
    /// </summary>
    function GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список фотографий, на которых есть непросмотренные отметки.
    /// </summary>
    function GetNewTags(var Items: TVkPhotos; Count: Integer = 20; Offset: Integer = 0): Boolean;
    /// <summary>
    /// Получает адрес для загрузки обложки сообщества.
    /// </summary>
    function GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: TVkPeerId; CropLeft: TPoint; CropRight: TPoint): Boolean; overload;
    /// <summary>
    /// Получает адрес для загрузки обложки сообщества.
    /// </summary>
    function GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: TVkPeerId): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки главной фотографии на страницу пользователя или сообщества.
    /// </summary>
    function GetOwnerPhotoUploadServer(var UploadUrl: string; OwnerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Возвращает список отметок на фотографии.
    /// </summary>
    function GetTags(var Items: TVkPhotoTags; PhotoId: Integer; OwnerId: TVkPeerId = 0; AccessKey: string = ''): Boolean;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографий.
    /// </summary>
    function GetUploadServer(var UploadData: TVkPhotoGetUploadResponse; AlbumId: Integer = 0; GroupId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Возвращает список фотографий, на которых отмечен пользователь.
    /// </summary>
    function GetUserPhotos(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список фотографий, на которых отмечен пользователь.
    /// </summary>
    function GetUserPhotos(var Items: TVkPhotos; Params: TVkParamsPhotosGetUserPhotos): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии на стену пользователя или сообщества.
    /// </summary>
    function GetWallUploadServer(var UploadData: TVkPhotoGetUploadResponse; GroupId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Делает фотографию обложкой альбома.
    /// </summary>
    function MakeCover(PhotoId, AlbumId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Переносит фотографию из одного альбома в другой.
    /// </summary>
    function Move(PhotoId, TargetAlbumId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Добавляет отметку на фотографию.
    /// </summary>
    function PutTag(var TagId: Integer; PhotoId: Integer; UserId: TVkPeerId; Left, Right: TPoint; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Удаляет отметку с фотографии.
    /// </summary>
    function RemoveTag(PhotoId, TagId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Меняет порядок альбома в списке альбомов пользователя.
    /// </summary>
    function ReorderAlbums(Params: TVkParamsPhotosReorderAlbums): Boolean;
    /// <summary>
    /// Меняет порядок фотографии в списке фотографий альбома пользователя.
    /// </summary>
    function ReorderPhotos(Params: TVkParamsPhotosReorderPhotos): Boolean;
    /// <summary>
    /// Позволяет пожаловаться на фотографию.
    /// </summary>
    function Report(OwnerId: TVkPeerId; PhotoId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// Позволяет пожаловаться на комментарий к фотографии.
    /// </summary>
    function ReportComment(OwnerId: TVkPeerId; CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// Восстанавливает удаленную фотографию.
    /// </summary>
    function Restore(PhotoId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Восстанавливает удаленный комментарий к фотографии.
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки.
    /// </summary>
    function Save(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки.
    /// </summary>
    function Save(var Items: TVkPhotos; Params: TVkParamsPhotosSave): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getMarketAlbumUploadServer.
    /// </summary>
    function SaveMarketAlbumPhoto(var Items: TVkPhotos; GroupId: TVkPeerId; Photo, Server, Hash: string): Boolean;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getMarketUploadServer.
    /// </summary>
    function SaveMarketPhoto(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getMarketUploadServer.
    /// </summary>
    function SaveMarketPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveMarketPhoto): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографию после успешной загрузки на URI, полученный методом photos.getMessagesUploadServer.
    /// </summary>
    function SaveMessagesPhoto(var Items: TVkPhotos; Data: TVkPhotoUploadResponse): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографию после успешной загрузки на URI, полученный методом photos.getMessagesUploadServer.
    /// </summary>
    function SaveMessagesPhoto(var Items: TVkPhotos; Server: Integer; Photo, Hash: string): Boolean; overload;
    /// <summary>
    /// Сохраняет изображение для обложки сообщества после успешной загрузки.
    /// </summary>
    function SaveOwnerCoverPhoto(var Items: TVkCoverImages; Photo, Hash: string): Boolean;
    /// <summary>
    /// Позволяет сохранить главную фотографию пользователя или сообщества. Адрес для загрузки фотографии Вы можете получить с помощью метода photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(var Info: TVkOwnerPhoto; Server: Integer; Photo, Hash: string): Boolean; overload;
    /// <summary>
    /// Позволяет сохранить главную фотографию пользователя или сообщества. Адрес для загрузки фотографии Вы можете получить с помощью метода photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(var Info: TVkOwnerPhoto; Data: TVkPhotoUploadResponse): Boolean; overload;
    /// <summary>
    /// Позволяет сохранить главную фотографию пользователя или сообщества. Адрес для загрузки фотографии Вы можете получить с помощью метода photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(FileName: string): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getWallUploadServer.
    /// </summary>
    function SaveWallPhoto(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографии после успешной загрузки на URI, полученный методом photos.getWallUploadServer.
    /// </summary>
    function SaveWallPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveWallPhoto): Boolean; overload;
    /// <summary>
    /// Осуществляет поиск изображений по местоположению или описанию.
    /// </summary>
    function Search(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// Осуществляет поиск изображений по местоположению или описанию.
    /// </summary>
    function Search(var Items: TVkPhotos; Params: TVkParamsPhotosSearch): Boolean; overload;
    /// <summary>
    /// Загрузки фотографии
    /// </summary>
    function Upload(var Response: TVkPhotoUploadResponse; const UploadUrl: string; const FileNames: array of string): Boolean; overload;
    /// <summary>
    /// Загрузки фотографии
    /// </summary>
    function Upload(var Response: TVkPhotoUploadResponse; const UploadUrl: string; Stream: TStream; const FileName: string): Boolean; overload;
    /// <summary>
    /// Загрузки фотографии для отправки в сообщении
    /// </summary>
    function UploadForMessage(var Photos: TVkPhotos; const PeerId: TVkPeerId; const FileNames: array of string): Boolean; overload;
    /// <summary>
    /// Загрузки фотографии для отправки в сообщении
    /// </summary>
    function UploadForMessage(var Photos: TAttachmentArray; const PeerId: TVkPeerId; const FileNames: array of string): Boolean; overload;
    /// <summary>
    /// Загрузки фотографии для отправки в сообщении
    /// </summary>
    function UploadForMessage(var Photos: TAttachmentArray; const PeerId: TVkPeerId; const FileName: string; Stream: TStream): Boolean; overload;
    /// <summary>
    /// Загрузки фотографии для публикации на стену пользователя или сообщества
    /// </summary>
    function UploadForWall(var Photos: TVkPhotos; const FileNames: array of string; Params: TVkParamsPhotosSaveWallPhoto; const GroupId: Cardinal = 0): Boolean;
    /// <summary>
    /// Загрузки фотографии для публикации на стену сообщества
    /// </summary>
    function UploadForGroupWall(var Photos: TVkPhotos; const GroupId: Cardinal; const FileNames: array of string): Boolean;
    /// <summary>
    /// Загрузки фотографии для публикации на стену пользователя
    /// </summary>
    function UploadForUserWall(var Photos: TVkPhotos; const UserId: TVkPeerId; const FileNames: array of string): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils, System.Net.HttpClient,
  System.Net.Mime;

{ TPhotosController }

function TPhotosController.Upload(var Response: TVkPhotoUploadResponse; const UploadUrl: string; Stream: TStream; const FileName: string): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
begin
  Result := False;
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    Data.AddStream('file', Stream, ExtractFileName(FileName));
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      Response := TVkPhotoUploadResponse.FromJsonString<TVkPhotoUploadResponse>(ResStream.DataString);
      Result := True;
    end;
  finally
    ResStream.Free;
    Data.Free;
    HTTP.Free;
  end;
end;

function TPhotosController.Upload(var Response: TVkPhotoUploadResponse; const UploadUrl: string; const FileNames: array of string): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
  FileName: string;
  i: Integer;
begin
  Result := False;
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    i := 1;
    for FileName in FileNames do
    begin
      Data.AddFile('file' + i.ToString, FileName);
      Inc(i);
    end;
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      Response := TVkPhotoUploadResponse.FromJsonString<TVkPhotoUploadResponse>(ResStream.DataString);
      Result := True;
    end;
  finally
    ResStream.Free;
    Data.Free;
    HTTP.Free;
  end;
end;

function TPhotosController.GetMessagesUploadServer(var UploadUrl: string; PeerId: TVkPeerId): Boolean;
var
  Upload: TVkPhotoGetUploadResponse;
begin
  Result := GetMessagesUploadServer(Upload, PeerId);
  if Result then
  begin
    try
      UploadUrl := Upload.UploadUrl;
    finally
      Upload.Free;
    end;
  end;
end;

function TPhotosController.Get(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.get', Params).GetObject(Items);
end;

function TPhotosController.GetAll(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.getAll', Params).GetObject(Items);
end;

function TPhotosController.GetAlbums(var Items: TVkPhotoAlbums; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.getAlbums', Params).GetObject(Items);
end;

function TPhotosController.GetAlbums(var Items: TVkPhotoAlbums; Params: TVkParamsAlbumsGet): Boolean;
begin
  Result := GetAlbums(Items, Params.List);
end;

function TPhotosController.GetAlbumsCount(var Count: Integer; Params: TVkParamsPhotosGetAlbumsCount): Boolean;
begin
  Result := Handler.Execute('photos.getAlbumsCount', Params.List).ResponseAsInt(Count);
end;

function TPhotosController.ConfirmTags(Tags: TIdList): Boolean;
begin
  Result := Handler.Execute('photos.confirmTag', ['tags', Tags.ToString]).ResponseIsTrue;
end;

function TPhotosController.Copy(var Id: Integer; OwnerId: TVkPeerId; PhotoId: Integer; AccessKey: string): Boolean;
begin
  Result := Handler.Execute('photos.confirmTag', [
    ['owner_id', OwnerId.ToString],
    ['photo_id', PhotoId.ToString],
    ['access_key', AccessKey]]).
    ResponseAsInt(Id);
end;

function TPhotosController.CreateAlbum(var Item: TVkPhotoAlbum; Params: TVkParamsPhotosCreateAlbum): Boolean;
begin
  Result := CreateAlbum(Item, Params.List);
end;

function TPhotosController.CreateComment(var Id: Integer; Params: TVkParamsPhotosCreateComment): Boolean;
begin
  Result := CreateComment(Id, Params.List);
end;

function TPhotosController.DeclineTags: Boolean;
begin
  Result := Handler.Execute('photos.declineTags').ResponseIsTrue;
end;

function TPhotosController.Delete(OwnerId: TVkPeerId; PhotoId: Integer): Boolean;
begin
  Result := Handler.Execute('photos.delete', [['owner_id', OwnerId.ToString], ['photo_id', PhotoId.ToString]]).ResponseIsTrue;
end;

function TPhotosController.DeleteAlbum(AlbumId: Integer; GroupId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('photos.deleteAlbum', Params).ResponseIsTrue;
end;

function TPhotosController.DeleteComment(OwnerId: TVkPeerId; CommentId: Integer): Boolean;
begin
  Result := Handler.Execute('photos.deleteComment', [
    ['owner_id', OwnerId.ToString],
    ['comment_id', CommentId.ToString]]).
    ResponseIsTrue;
end;

function TPhotosController.Edit(Params: TVkParamsPhotosEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TPhotosController.EditAlbum(Params: TVkParamsPhotosEditAlbum): Boolean;
begin
  Result := EditAlbum(Params.List);
end;

function TPhotosController.EditComment(Params: TVkParamsPhotosEditComment): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TPhotosController.EditComment(Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.editComment', Params).ResponseIsTrue;
end;

function TPhotosController.EditAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.editAlbum', Params).ResponseIsTrue;
end;

function TPhotosController.Edit(Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.edit', Params).ResponseIsTrue;
end;

function TPhotosController.CreateComment(var Id: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.createComment', Params).ResponseAsInt(Id);
end;

function TPhotosController.CreateAlbum(var Item: TVkPhotoAlbum; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.createAlbum', Params).GetObject(Item);
end;

function TPhotosController.Get(var Items: TVkPhotos; Params: TVkParamsPhotosGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TPhotosController.GetAll(var Items: TVkPhotos; Params: TVkParamsPhotosGetAll): Boolean;
begin
  Result := GetAll(Items, Params.List);
end;

function TPhotosController.GetAllComments(var Items: TVkComments; Params: TVkParamsPhotosGetAllComments): Boolean;
begin
  Result := Handler.Execute('photos.getAllComments', Params.List).GetObject(Items);
end;

function TPhotosController.GetById(var Items: TVkPhotos; Photos: TArrayOfString; Extended, PhotoSizes: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('photos', Photos);
  if Extended then
    Params.Add('extended', Extended);
  if PhotoSizes then
    Params.Add('photo_sizes', PhotoSizes);
  Result := Handler.Execute('photos.getById', Params).GetObjects(Items);
end;

function TPhotosController.GetChatUploadServer(var UploadUrl: string; ChatId: Integer; Crop: TPoint; CropWidth: Integer): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Result := False;
  Params.Add('chat_id', ChatId);
  if not Crop.IsZero then
  begin
    Params.Add('crop_x', Crop.X);
    Params.Add('crop_y', Crop.Y);
  end;
  if CropWidth <> 0 then
    Params.Add('crop_width', CropWidth);
  with Handler.Execute('photos.getChatUploadServer', Params) do
  begin
    if GetObject(Item) then
    begin
      try
        try
          UploadUrl := Item.UploadUrl;
          Result := not UploadUrl.IsEmpty;
        except
          Result := False;
        end;
      finally
        Item.Free;
      end;
    end;
  end;
end;

function TPhotosController.GetComments(var Items: TVkComments; Params: TVkParamsPhotosGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TPhotosController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.getComments', Params).GetObject(Items);
end;

function TPhotosController.GetMarketAlbumUploadServer(var UploadUrl: string; GroupId: TVkPeerId): Boolean;
var
  Item: TVkPhotoGetUploadResponse;
begin
  Result := False;
  with Handler.Execute('photos.getMarketAlbumUploadServer', ['group_id', GroupId.ToString]) do
  begin
    if GetObject(Item) then
    begin
      try
        try
          UploadUrl := Item.UploadUrl;
          Result := not UploadUrl.IsEmpty;
        except
          Result := False;
        end;
      finally
        Item.Free;
      end;
    end;
  end;
end;

function TPhotosController.GetMarketUploadServer(var UploadUrl: string; Params: TVkParamsPhotosGetMarketUploadServer): Boolean;
begin
  Result := GetMarketUploadServer(UploadUrl, Params.List);
end;

function TPhotosController.GetMarketUploadServer(var UploadUrl: string; Params: TParams): Boolean;
var
  Item: TVkPhotoGetUploadResponse;
begin
  Result := False;
  with Handler.Execute('photos.getMarketUploadServer', Params) do
  begin
    if GetObject(Item) then
    begin
      try
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if PeerId <> 0 then
    Params.Add('peer_id', PeerId);
  Result := Handler.Execute('photos.getMessagesUploadServer', Params).GetObject(Upload);
end;

function TPhotosController.GetNewTags(var Items: TVkPhotos; Count, Offset: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  Result := Handler.Execute('photos.getNewTags', Params).GetObject(Items);
end;

function TPhotosController.GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: TVkPeerId): Boolean;
begin
  Result := GetOwnerCoverPhotoUploadServer(UploadUrl, GroupId, TPoint.Zero, TPoint.Zero);
end;

function TPhotosController.GetOwnerPhotoUploadServer(var UploadUrl: string; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Result := False;
  Params.Add('owner_id', OwnerId);
  with Handler.Execute('photos.getOwnerPhotoUploadServer', Params) do
  begin
    if GetObject(Item) then
    begin
      try
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetTags(var Items: TVkPhotoTags; PhotoId: Integer; OwnerId: TVkPeerId; AccessKey: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo_id', PhotoId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('access_key', AccessKey);
  Result := Handler.Execute('photos.getTags', Params).GetObject(Items);
end;

function TPhotosController.GetUploadServer(var UploadData: TVkPhotoGetUploadResponse; AlbumId: Integer; GroupId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  if AlbumId <> 0 then
    Params.Add('album_id', AlbumId);
  Result := Handler.Execute('photos.getUploadServer', Params).GetObject(UploadData);
end;

function TPhotosController.GetUserPhotos(var Items: TVkPhotos; Params: TVkParamsPhotosGetUserPhotos): Boolean;
begin
  Result := GetUserPhotos(Items, Params.List);
end;

function TPhotosController.GetWallUploadServer(var UploadData: TVkPhotoGetUploadResponse; GroupId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('photos.getWallUploadServer', Params).GetObject(UploadData);
end;

function TPhotosController.MakeCover(PhotoId, AlbumId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('album_id', AlbumId);
  Result := Handler.Execute('photos.makeCover', Params).ResponseIsTrue;
end;

function TPhotosController.Move(PhotoId, TargetAlbumId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('target_album_id', TargetAlbumId);
  Result := Handler.Execute('photos.move', Params).ResponseIsTrue;
end;

function TPhotosController.PutTag(var TagId: Integer; PhotoId: Integer; UserId: TVkPeerId; Left, Right: TPoint; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('user_id', UserId);
  Params.Add('x', Left.X);
  Params.Add('y', Left.y);
  Params.Add('x2', Right.X);
  Params.Add('y2', Right.y);
  Result := Handler.Execute('photos.putTag', Params).ResponseAsInt(TagId);
end;

function TPhotosController.RemoveTag(PhotoId, TagId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('tag_id', TagId);
  Result := Handler.Execute('photos.removeTag', Params).ResponseIsTrue;
end;

function TPhotosController.ReorderAlbums(Params: TVkParamsPhotosReorderAlbums): Boolean;
begin
  Result := Handler.Execute('photos.reorderAlbums', Params.List).ResponseIsTrue;
end;

function TPhotosController.ReorderPhotos(Params: TVkParamsPhotosReorderPhotos): Boolean;
begin
  Result := Handler.Execute('photos.reorderPhotos', Params.List).ResponseIsTrue;
end;

function TPhotosController.Report(OwnerId: TVkPeerId; PhotoId: Integer; Reason: TVkMediaReportReason): Boolean;
begin
  Result := Handler.Execute('photos.report', [
    ['owner_id', OwnerId.ToString],
    ['photo_id', PhotoId.ToString],
    ['reason', Ord(Reason).ToString]]).
    ResponseIsTrue;
end;

function TPhotosController.ReportComment(OwnerId: TVkPeerId; CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
begin
  Result := Handler.Execute('photos.reportComment', [
    ['owner_id', OwnerId.ToString],
    ['comment_id', CommentId.ToString],
    ['reason', Ord(Reason).ToString]]).
    ResponseIsTrue;
end;

function TPhotosController.Restore(PhotoId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Result := Handler.Execute('photos.restore', Params).ResponseIsTrue;
end;

function TPhotosController.RestoreComment(CommentId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  Result := Handler.Execute('photos.restoreComment', Params).ResponseIsTrue;
end;

function TPhotosController.GetUserPhotos(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.getUserPhotos', Params).GetObject(Items);
end;

function TPhotosController.GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: TVkPeerId; CropLeft, CropRight: TPoint): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Result := False;
  Params.Add('group_id', GroupId);
  Params.Add('crop_x', CropLeft.X);
  Params.Add('crop_y', CropLeft.y);
  Params.Add('crop_x2', CropRight.X);
  Params.Add('crop_y2', CropRight.y);
  with Handler.Execute('photos.getOwnerCoverPhotoUploadServer', Params) do
  begin
    if GetObject(Item) then
    begin
      try
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.Save(var Items: TVkPhotos; Params: TVkParamsPhotosSave): Boolean;
begin
  Result := Save(Items, Params.List);
end;

function TPhotosController.Save(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.ExecutePost('photos.save', Params).GetObjects(Items);
end;

function TPhotosController.SaveMarketAlbumPhoto(var Items: TVkPhotos; GroupId: TVkPeerId; Photo, Server, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  Result := Handler.ExecutePost('photos.saveMarketAlbumPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveMarketPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveMarketPhoto): Boolean;
begin
  Result := SaveMarketPhoto(Items, Params.List);
end;

function TPhotosController.SaveMessagesPhoto(var Items: TVkPhotos; Server: Integer; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  Result := Handler.ExecutePost('photos.saveMessagesPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveOwnerCoverPhoto(var Items: TVkCoverImages; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('hash', Hash);
  Result := Handler.ExecutePost('photos.saveOwnerCoverPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveOwnerPhoto(FileName: string): Boolean;
var
  Server: string;
  Response: TVkPhotoUploadResponse;
  Info: TVkOwnerPhoto;
begin
  Result := False;
  if GetOwnerPhotoUploadServer(Server) then
  begin
    if Upload(Response, Server, FileName) then
    begin
      try
        if SaveOwnerPhoto(Info, Response) then
        begin
          try
            Result := Info.Saved;
          finally
            Info.Free;
          end;
        end;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TPhotosController.SaveOwnerPhoto(var Info: TVkOwnerPhoto; Data: TVkPhotoUploadResponse): Boolean;
begin
  Result := SaveOwnerPhoto(Info, Data.Server, Data.Photo, Data.Hash);
end;

function TPhotosController.SaveOwnerPhoto(var Info: TVkOwnerPhoto; Server: Integer; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  Result := Handler.Execute('photos.saveOwnerPhoto', Params).GetObject(Info);
end;

function TPhotosController.SaveWallPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveWallPhoto): Boolean;
begin
  Result := SaveWallPhoto(Items, Params.List);
end;

function TPhotosController.Search(var Items: TVkPhotos; Params: TVkParamsPhotosSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TPhotosController.UploadForGroupWall(var Photos: TVkPhotos; const GroupId: Cardinal; const FileNames: array of string): Boolean;
var
  SaveParams: TVkParamsPhotosSaveWallPhoto;
begin
  SaveParams.GroupId(GroupId);
  Result := UploadForWall(Photos, FileNames, SaveParams, GroupId);
end;

function TPhotosController.UploadForMessage(var Photos: TAttachmentArray; const PeerId: TVkPeerId; const FileName: string; Stream: TStream): Boolean;
var
  Items: TVkPhotos;
var
  Url: string;
  Response: TVkPhotoUploadResponse;
begin
  Result := False;
  if GetMessagesUploadServer(Url, PeerId) then
  begin
    if Upload(Response, Url, Stream, FileName) then
    begin
      try
        Result := SaveMessagesPhoto(Items, Response);
      finally
        Response.Free;
      end;
    end;
  end;
  if Result then
  begin
    Photos := Items.ToAttachments;
    Items.Free;
  end;
end;

function TPhotosController.UploadForMessage(var Photos: TAttachmentArray; const PeerId: TVkPeerId; const FileNames: array of string): Boolean;
var
  Items: TVkPhotos;
begin
  Result := UploadForMessage(Items, PeerId, FileNames);
  if Result then
  begin
    Photos := Items.ToAttachments;
    Items.Free;
  end;
end;

function TPhotosController.UploadForMessage(var Photos: TVkPhotos; const PeerId: TVkPeerId; const FileNames: array of string): Boolean;
var
  Url: string;
  Response: TVkPhotoUploadResponse;
begin
  Result := False;
  if GetMessagesUploadServer(Url, PeerId) then
  begin
    if Upload(Response, Url, FileNames) then
    begin
      try
        Result := SaveMessagesPhoto(Photos, Response);
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TPhotosController.UploadForUserWall(var Photos: TVkPhotos; const UserId: TVkPeerId; const FileNames: array of string): Boolean;
var
  SaveParams: TVkParamsPhotosSaveWallPhoto;
begin
  SaveParams.UserId(UserId);
  Result := UploadForWall(Photos, FileNames, SaveParams);
end;

function TPhotosController.UploadForWall(var Photos: TVkPhotos; const FileNames: array of string; Params: TVkParamsPhotosSaveWallPhoto; const GroupId: Cardinal): Boolean;
var
  Response: TVkPhotoUploadResponse;
  PhotoUpload: TVkPhotoGetUploadResponse;
begin
  Result := False;
  if GetWallUploadServer(PhotoUpload, GroupId) then
  begin
    try
      if Upload(Response, PhotoUpload.UploadUrl, FileNames) then
      begin
        try
          Params.Photo(Response.Photo);
          Params.Hash(Response.Hash);
          Params.Server(Response.Server);
          Result := SaveWallPhoto(Photos, Params);
        finally
          Response.Free;
        end;
      end;
    finally
      PhotoUpload.Free;
    end;
  end;
end;

function TPhotosController.Search(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.search', Params).GetObject(Items);
end;

function TPhotosController.SaveWallPhoto(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.ExecutePost('photos.saveWallPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveMarketPhoto(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.saveMarketPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveMessagesPhoto(var Items: TVkPhotos; Data: TVkPhotoUploadResponse): Boolean;
begin
  Result := SaveMessagesPhoto(Items, Data.Server, Data.Photo, Data.Hash);
end;

function TPhotosController.СonfirmTag(PhotoId, TagId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('tag_id', TagId);
  Result := Handler.Execute('photos.confirmTag', Params).ResponseIsTrue;
end;

{ TVkGetAllParams }

function TVkParamsPhotosGetAll.Count(const Value: Integer): TVkParamsPhotosGetAll;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.Extended(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.NeedHidden(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('need_hidden', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.NoServiceAlbums(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('no_service_albums', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.Offset(const Value: Integer): TVkParamsPhotosGetAll;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.OwnerId(const Value: TVkPeerId): TVkParamsPhotosGetAll;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.PhotoSizes(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('photo_sizes', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.SkipHidden(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('skip_hidden', Value);
  Result := Self;
end;

{ TVkParamsAlbumsGet }

function TVkParamsAlbumsGet.AlbumIds(const Value: TArrayOfInteger): TVkParamsAlbumsGet;
begin
  List.Add('album_ids', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.AlbumIds(const Value: Integer): TVkParamsAlbumsGet;
begin
  List.Add('album_ids', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.Count(const Value: Integer): TVkParamsAlbumsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.NeedCovers(const Value: Boolean): TVkParamsAlbumsGet;
begin
  List.Add('need_covers', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.NeedSystem(const Value: Boolean): TVkParamsAlbumsGet;
begin
  List.Add('need_system', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.Offset(const Value: Integer): TVkParamsAlbumsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.OwnerId(const Value: TVkPeerId): TVkParamsAlbumsGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.PhotoSizes(const Value: Boolean): TVkParamsAlbumsGet;
begin
  List.Add('photo_sizes', Value);
  Result := Self;
end;

{ TVkPhotosGetParams }

function TVkParamsPhotosGet.AlbumId(const Value: TVkPhotoSystemAlbum): TVkParamsPhotosGet;
begin
  List.Add('album_id', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosGet.AlbumId(const Value: Int64): TVkParamsPhotosGet;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Count(const Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Extended(const Value: Boolean): TVkParamsPhotosGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Feed(const Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('feed', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.FeedType(const Value: TVkPhotoFeedType): TVkParamsPhotosGet;
begin
  List.Add('feed_type', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosGet.Offset(const Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.OwnerId(const Value: TVkPeerId): TVkParamsPhotosGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.PhotoId(const Value: Int64): TVkParamsPhotosGet;
begin
  List.Add('photo_ids', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.PhotoIds(const Value: TArrayOfInteger): TVkParamsPhotosGet;
begin
  List.Add('photo_ids', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.PhotoSizes(const Value: Boolean): TVkParamsPhotosGet;
begin
  List.Add('photo_sizes', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Rev(const Value: Boolean): TVkParamsPhotosGet;
begin
  List.Add('rev', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Uid(const Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('uid', Value);
  Result := Self;
end;

{ TVkParamsPhotosCreateAlbum }

function TVkParamsPhotosCreateAlbum.CommentsDisabled(const Value: Boolean): TVkParamsPhotosCreateAlbum;
begin
  List.Add('comments_disabled', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.Description(const Value: string): TVkParamsPhotosCreateAlbum;
begin
  List.Add('description', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.GroupId(const Value: TVkPeerId): TVkParamsPhotosCreateAlbum;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.PrivacyComment(const Value: TVkPrivacySettings): TVkParamsPhotosCreateAlbum;
begin
  List.Add('privacy_comment', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.PrivacyView(const Value: TVkPrivacySettings): TVkParamsPhotosCreateAlbum;
begin
  List.Add('privacy_view', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.Title(const Value: string): TVkParamsPhotosCreateAlbum;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.UploadByAdminsOnly(const Value: Boolean): TVkParamsPhotosCreateAlbum;
begin
  List.Add('upload_by_admins_only', Value);
  Result := Self;
end;

{ TVkParamsPhotosCreateComment }

function TVkParamsPhotosCreateComment.AccessKey(const Value: string): TVkParamsPhotosCreateComment;
begin
  List.Add('access_key', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.Attachments(const Value: TAttachmentArray): TVkParamsPhotosCreateComment;
begin
  List.Add('attachments', Value.ToStrings);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.FromGroup(const Value: Boolean): TVkParamsPhotosCreateComment;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.Guid(const Value: string): TVkParamsPhotosCreateComment;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.Message(const Value: string): TVkParamsPhotosCreateComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.OwnerId(const Value: TVkPeerId): TVkParamsPhotosCreateComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.PhotoId(const Value: Integer): TVkParamsPhotosCreateComment;
begin
  List.Add('photo_id', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.ReplyToComment(const Value: Integer): TVkParamsPhotosCreateComment;
begin
  List.Add('reply_to_comment', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.StickerId(const Value: Cardinal): TVkParamsPhotosCreateComment;
begin
  List.Add('sticker_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosEdit }

function TVkParamsPhotosEdit.Caption(const Value: string): TVkParamsPhotosEdit;
begin
  List.Add('caption', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.DeletePlace(const Value: Boolean): TVkParamsPhotosEdit;
begin
  List.Add('delete_place', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.FoursquareId(const Value: string): TVkParamsPhotosEdit;
begin
  List.Add('foursquare_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.Latitude(const Value: Extended): TVkParamsPhotosEdit;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.Longitude(const Value: Extended): TVkParamsPhotosEdit;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.OwnerId(const Value: TVkPeerId): TVkParamsPhotosEdit;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.PhotoId(const Value: Integer): TVkParamsPhotosEdit;
begin
  List.Add('photo_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.PlaceStr(const Value: string): TVkParamsPhotosEdit;
begin
  List.Add('place_str', Value);
  Result := Self;
end;

{ TVkParamsPhotosEditAlbum }

function TVkParamsPhotosEditAlbum.AlbumId(const Value: Integer): TVkParamsPhotosEditAlbum;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.CommentsDisabled(const Value: Boolean): TVkParamsPhotosEditAlbum;
begin
  List.Add('comments_disabled', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.Description(const Value: string): TVkParamsPhotosEditAlbum;
begin
  List.Add('description', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.OwnerId(const Value: TVkPeerId): TVkParamsPhotosEditAlbum;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.PrivacyComment(const Value: TArrayOfString): TVkParamsPhotosEditAlbum;
begin
  List.Add('privacy_comment', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.PrivacyView(const Value: TArrayOfString): TVkParamsPhotosEditAlbum;
begin
  List.Add('privacy_view', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.Title(const Value: string): TVkParamsPhotosEditAlbum;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.UploadByAdminsOnly(const Value: Boolean): TVkParamsPhotosEditAlbum;
begin
  List.Add('upload_by_admins_only', Value);
  Result := Self;
end;

{ TVkParamsPhotosEditComment }

function TVkParamsPhotosEditComment.Attachments(const Value: TAttachmentArray): TVkParamsPhotosEditComment;
begin
  List.Add('attachments', Value.ToStrings);
  Result := Self;
end;

function TVkParamsPhotosEditComment.CommentId(const Value: Integer): TVkParamsPhotosEditComment;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEditComment.Message(const Value: string): TVkParamsPhotosEditComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsPhotosEditComment.OwnerId(const Value: TVkPeerId): TVkParamsPhotosEditComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetAlbumsCount }

function TVkParamsPhotosGetAlbumsCount.GroupId(const Value: TVkPeerId): TVkParamsPhotosGetAlbumsCount;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAlbumsCount.UserId(const Value: TVkPeerId): TVkParamsPhotosGetAlbumsCount;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetAllComments }

function TVkParamsPhotosGetAllComments.AlbumId(const Value: Integer): TVkParamsPhotosGetAllComments;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAllComments.Count(const Value: Integer): TVkParamsPhotosGetAllComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAllComments.NeedLikes(const Value: Boolean): TVkParamsPhotosGetAllComments;
begin
  List.Add('need_likes', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAllComments.Offset(const Value: Integer): TVkParamsPhotosGetAllComments;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAllComments.OwnerId(const Value: TVkPeerId): TVkParamsPhotosGetAllComments;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetComments }

function TVkParamsPhotosGetComments.AccessKey(const Value: string): TVkParamsPhotosGetComments;
begin
  List.Add('access_key', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Count(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Extended(const Value: Boolean): TVkParamsPhotosGetComments;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Fields(const Value: TVkExtendedFields): TVkParamsPhotosGetComments;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosGetComments.NeedLikes(const Value: Boolean): TVkParamsPhotosGetComments;
begin
  List.Add('need_likes', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Offset(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.OwnerId(const Value: TVkPeerId): TVkParamsPhotosGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.PhotoId(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('photo_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Sort(const Value: TVkSort): TVkParamsPhotosGetComments;
begin
  List.Add('sort', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosGetComments.StartCommentId(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('start_comment_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetMarketUploadServer }

function TVkParamsPhotosGetMarketUploadServer.Crop(const Value: TPoint): TVkParamsPhotosGetMarketUploadServer;
begin
  List.Add('crop_x', Value.X).Add('crop_y', Value.Y);
  Result := Self;
end;

function TVkParamsPhotosGetMarketUploadServer.CropWidth(const Value: Integer): TVkParamsPhotosGetMarketUploadServer;
begin
  List.Add('crop_width', Value);
  Result := Self;
end;

function TVkParamsPhotosGetMarketUploadServer.GroupId(const Value: TVkPeerId): TVkParamsPhotosGetMarketUploadServer;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetMarketUploadServer.MainPhoto(const Value: Boolean): TVkParamsPhotosGetMarketUploadServer;
begin
  List.Add('main_photo', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetUserPhotos }

function TVkParamsPhotosGetUserPhotos.Count(const Value: Integer): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetUserPhotos.Extended(const Value: Boolean): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPhotosGetUserPhotos.Offset(const Value: Integer): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGetUserPhotos.Sort(const Value: TVkSort): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

function TVkParamsPhotosGetUserPhotos.UserId(const Value: TVkPeerId): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosReorderAlbums }

function TVkParamsPhotosReorderAlbums.After(const Value: Integer): TVkParamsPhotosReorderAlbums;
begin
  List.Add('after', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderAlbums.AlbumId(const Value: Integer): TVkParamsPhotosReorderAlbums;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderAlbums.Before(const Value: Integer): TVkParamsPhotosReorderAlbums;
begin
  List.Add('before', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderAlbums.OwnerId(const Value: TVkPeerId): TVkParamsPhotosReorderAlbums;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosReorderPhotos }

function TVkParamsPhotosReorderPhotos.After(const Value: Integer): TVkParamsPhotosReorderPhotos;
begin
  List.Add('after', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderPhotos.Before(const Value: Integer): TVkParamsPhotosReorderPhotos;
begin
  List.Add('before', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderPhotos.OwnerId(const Value: TVkPeerId): TVkParamsPhotosReorderPhotos;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderPhotos.PhotoId(const Value: Integer): TVkParamsPhotosReorderPhotos;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosSave }

function TVkParamsPhotosSave.AlbumId(const Value: Integer): TVkParamsPhotosSave;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Caption(const Value: string): TVkParamsPhotosSave;
begin
  List.Add('caption', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.GroupId(const Value: TVkPeerId): TVkParamsPhotosSave;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Hash(const Value: string): TVkParamsPhotosSave;
begin
  List.Add('hash', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Latitude(const Value: Extended): TVkParamsPhotosSave;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Longitude(const Value: Extended): TVkParamsPhotosSave;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.PhotosList(const Value: TArrayOfString): TVkParamsPhotosSave;
begin
  List.Add('photos_list', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Server(const Value: string): TVkParamsPhotosSave;
begin
  List.Add('server', Value);
  Result := Self;
end;

{ TVkParamsPhotosSaveMarketPhoto }

function TVkParamsPhotosSaveMarketPhoto.CropData(const Value: string): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('crop_data', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.CropHash(const Value: string): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('crop_hash', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.GroupId(const Value: TVkPeerId): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.Hash(const Value: string): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('hash', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.Photo(const Value: string): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('photo', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.Server(const Value: Integer): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('server', Value);
  Result := Self;
end;

{ TVkParamsPhotosSaveWallPhoto }

function TVkParamsPhotosSaveWallPhoto.Caption(const Value: string): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('caption', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.GroupId(const Value: TVkPeerId): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Hash(const Value: string): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('hash', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Latitude(const Value: Extended): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Longitude(const Value: Extended): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Photo(const Value: string): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('photo', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Server(const Value: Integer): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('server', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.UserId(const Value: TVkPeerId): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosSearch }

function TVkParamsPhotosSearch.Count(const Value: Integer): TVkParamsPhotosSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.EndTime(const Value: TDateTime): TVkParamsPhotosSearch;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Latitude(const Value: Extended): TVkParamsPhotosSearch;
begin
  List.Add('lat', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Longitude(const Value: Extended): TVkParamsPhotosSearch;
begin
  List.Add('long', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Offset(const Value: Integer): TVkParamsPhotosSearch;
begin
  List.Add('offse', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Query(const Value: string): TVkParamsPhotosSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Radius(const Value: Integer): TVkParamsPhotosSearch;
begin
  List.Add('radius', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Sort(const Value: TVkPhotoSort): TVkParamsPhotosSearch;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

function TVkParamsPhotosSearch.StartTime(const Value: TDateTime): TVkParamsPhotosSearch;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

end.

