unit VK.Entity.Media;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Common, VK.Entity.Photo, VK.Entity.Link, VK.Entity.AudioMessage,
  VK.Entity.Sticker, VK.Entity.Gift, VK.Entity.Market, VK.Entity.Doc,
  VK.Entity.Audio, VK.Entity.Video, VK.Entity.Graffiti, VK.Entity.Note,
  VK.Entity.OldApp, VK.Entity.Poll, VK.Entity.Page, VK.Entity.Album,
  VK.Entity.PrettyCard, VK.Types, VK.Entity.Event, VK.Entity.Profile,
  VK.Entity.Group, VK.Entity.Call, VK.Entity.Market.Album, VK.Entity.Info,
  VK.Entity.Common.List, VK.Entity.Common.ExtendedList, VK.Entity.Donut,
  VK.Wrap.Interceptors, VK.Entity.MoneyTransfer, VK.Entity.MoneyRequest,
  VK.Entity.Geo;

type
  TVkAttachment = class;

  TVkComment = class;

  TVkPost = class;

  TVkCommentThread = class;

  /// <summary>
  /// Объект PostSource, описывающий способ размещения записи на стене
  /// </summary>
  TVkPostSource = class(TVkEntity)
  private
    FData: string;
    FPlatform: string;
    [JsonReflectAttribute(ctString, rtString, TPostSourceTypeInterceptor)]
    FType: TVkPostSourceType;
    FUrl: string;
  public
    /// <summary>
    /// Тип действия (только для Type = VK или Widget)
    /// Возможные значения:
    /// profile_activity — изменение статуса под именем пользователя (для Type = VK);
    /// profile_photo — изменение профильной фотографии пользователя (для Type = VK);
    /// comments — виджет комментариев (для Type = Widget);
    /// like — виджет «Мне нравится» (для Type = Widget);
    /// poll — виджет опросов (для Type = Widget);
    /// </summary>
    property Data: string read FData write FData;
    /// <summary>
    /// Название платформы, если оно доступно (android; iphone; wphone)
    /// </summary>
    property &Platform: string read FPlatform write FPlatform;
    /// <summary>
    /// Тип источника
    /// </summary>
    property &Type: TVkPostSourceType read FType write FType;
    /// <summary>
    /// URL ресурса, с которого была опубликована запись
    /// </summary>
    property Url: string read FUrl write FUrl;
  end;

  TVkAttachment = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TAttachmentTypeInterceptor)]
    FType: TVkAttachmentType;
    FLink: TVkLink;
    FPosted_photo: TVkPostedPhoto;
    FAudio_message: TVkAudioMessage;
    FWall_reply: TVkComment;
    FWall: TVkPost;
    FSticker: TVkSticker;
    FGift: TVkGift;
    FMarket_album: TVkMarketAlbum;
    FMarket: TVkProduct;
    FDoc: TVkDocument;
    FAudio: TVkAudio;
    FVideo: TVkVideo;
    FPhoto: TVkPhoto;
    FGraffiti: TVkGraffiti;
    FNote: TVkNote;
    FApp: TVkOldApp;
    FPoll: TVkPoll;
    FPage: TVkPage;
    FAlbum: TVkPhotoAlbum;
    FPretty_cards: TVkPrettyCards;
    FEvent: TVkEvent;
    FCall: TVkCall;
    FMoney_transfer: TVkMoneyTransfer;
    FMoney_request: TVkMoneyRequest;
  public
    property &Type: TVkAttachmentType read FType write FType;
    /// <summary>
    /// Ссылка
    /// </summary>
    property Link: TVkLink read FLink write FLink;
    /// <summary>
    /// Фотография, загруженная напрямую (Это устаревший тип вложения. Он может быть возвращен лишь для записей, созданных раньше 2013 года)
    /// </summary>
    property PostedPhoto: TVkPostedPhoto read FPosted_photo write FPosted_photo;
    /// <summary>
    /// Аудиосообщение
    /// </summary>
    property AudioMessage: TVkAudioMessage read FAudio_message write FAudio_message;
    /// <summary>
    /// Комментарий на стене
    /// </summary>
    property WallReply: TVkComment read FWall_reply write FWall_reply;
    /// <summary>
    /// Запись на стене
    /// </summary>
    property Wall: TVkPost read FWall write FWall;
    /// <summary>
    /// Звонок
    /// </summary>
    property Call: TVkCall read FCall write FCall;
    /// <summary>
    /// Стикер
    /// </summary>
    property Sticker: TVkSticker read FSticker write FSticker;
    /// <summary>
    /// Подарок
    /// </summary>
    property Gift: TVkGift read FGift write FGift;
    /// <summary>
    /// Подборка товаров
    /// </summary>
    property MarketAlbum: TVkMarketAlbum read FMarket_album write FMarket_album;
    /// <summary>
    /// Товар
    /// </summary>
    property Market: TVkProduct read FMarket write FMarket;
    /// <summary>
    /// Документ
    /// </summary>
    property Doc: TVkDocument read FDoc write FDoc;
    /// <summary>
    /// Аудиозапись
    /// </summary>
    property Audio: TVkAudio read FAudio write FAudio;
    /// <summary>
    /// Видеозапись
    /// </summary>
    property Video: TVkVideo read FVideo write FVideo;
    /// <summary>
    /// Фотография
    /// </summary>
    property Photo: TVkPhoto read FPhoto write FPhoto;
    /// <summary>
    /// Граффити (Это устаревший тип вложения. Он может быть возвращен лишь для записей, созданных раньше 2013 года. Для более новых записей граффити возвращается в виде вложения с типом photo.)
    /// </summary>
    property Graffiti: TVkGraffiti read FGraffiti write FGraffiti;
    /// <summary>
    /// Заметка
    /// </summary>
    property Note: TVkNote read FNote write FNote;
    /// <summary>
    /// Контент приложения (Это устаревший тип вложений. Он может быть возвращен лишь для записей, созданных раньше 2013 года.)
    /// </summary>
    property App: TVkOldApp read FApp write FApp;
    /// <summary>
    /// Опрос
    /// </summary>
    property Poll: TVkPoll read FPoll write FPoll;
    /// <summary>
    /// Вики-страница
    /// </summary>
    property Page: TVkPage read FPage write FPage;
    /// <summary>
    /// Денежный перевод
    /// </summary>
    property MoneyTransfer: TVkMoneyTransfer read FMoney_transfer write FMoney_transfer;
    /// <summary>
    /// Запрос на денежный перевод
    /// </summary>
    property MoneyRequest: TVkMoneyRequest read FMoney_request write FMoney_request;
    /// <summary>
    /// Альбом с фотографиями
    /// </summary>
    property Album: TVkPhotoAlbum read FAlbum write FAlbum;
    // /// <summary>
    // /// Список фотографий
    // /// </summary>
    //property PhotosList: TVkPhotosList read FPhotosList write FPhotosList; -- я хз че это и где найти структуру)
    /// <summary>
    /// Карточки
    /// </summary>
    property PrettyCards: TVkPrettyCards read FPretty_cards write FPretty_cards;
    /// <summary>
    /// Встреча
    /// </summary>
    property Event: TVkEvent read FEvent write FEvent;
    destructor Destroy; override;
    function GetPreviewUrl(const Size: Integer = 50): string;
  end;

  TVkAttachmentHistoryItem = class(TVkEntity)
  private
    FAttachment: TVkAttachment;
    FMessage_id: Integer;
    FFrom_id: Integer;
  public
    property Attachment: TVkAttachment read FAttachment write FAttachment;
    property MessageId: Integer read FMessage_id write FMessage_id;
    property FromId: Integer read FFrom_id write FFrom_id;
    destructor Destroy; override;
  end;

  TVkAttachmentHistory = class(TVkEntityExtendedList<TVkAttachmentHistoryItem>)
  private
    FNext_from: string;
  public
    property NextFrom: string read FNext_from write FNext_from;
  end;

  TVkAttachments = TVkEntityList<TVkAttachment>;

  TVkAttachmentArray = TArray<TVkAttachment>;

  TVkAttachmentArrayHelper = record helper for TVkAttachmentArray
    function GetByType<T: class>(const&Type: TVkAttachmentType; var Attachment: T): Boolean; {$IFDEF RELEASE} inline; {$ENDIF}
  end;

  /// <summary>
  /// Объект, описывающий комментарий к записи
  /// </summary>
  TVkComment = class(TVkObject, IAttachment)
  private
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFrom_id: Integer;
    FPost_id: Integer;
    FPost_owner_id: Integer;
    FReply_to_comment: Integer;
    FReply_to_user: Integer;
    FText: string;
    FOwner_id: Integer;
    FAttachments: TVkAttachmentArray;
    FDeleted: Boolean;
    FParents_stack: TArray<Integer>;
    FLikes: TVkLikesInfo;
    FThread: TVkCommentThread;
    FPid: Integer;
    FAccess_key: string;
    FDonut: TVkDonutInfo;
  public
    /// <summary>
    /// Идентификатор комментария
    /// </summary>
    property Id;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Дата создания комментария
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Информация о VK Donut
    /// </summary>
    property Donut: TVkDonutInfo read FDonut write FDonut;
    /// <summary>
    /// Идентификатор автора комментария
    /// </summary>
    property FromId: Integer read FFrom_id write FFrom_id;
    /// <summary>
    ///  Идентификатор фотографии, к которой был оставлен комментарий
    /// </summary>
    property PhotoId: Integer read FPid write FPid;
    /// <summary>
    /// Идентификатор записи, к которой оставлен комментари
    /// </summary>
    property PostId: Integer read FPost_id write FPost_id;
    /// <summary>
    /// Идентификатор владельца стены, на которой оставлен комментарий
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PostOwnerId: Integer read FPost_owner_id write FPost_owner_id;
    /// <summary>
    /// Идентификатор комментария, в ответ на который оставлен текущий (если применимо)
    /// </summary>
    property ReplyToComment: Integer read FReply_to_comment write FReply_to_comment;
    /// <summary>
    /// Идентификатор пользователя или сообщества, в ответ которому оставлен текущий комментарий (если применимо)
    /// </summary>
    property ReplyToUser: Integer read FReply_to_user write FReply_to_user;
    /// <summary>
    /// Текст комментария
    /// </summary>
    property Text: string read FText write FText;
    property Likes: TVkLikesInfo read FLikes write FLikes;
    /// <summary>
    /// Массив идентификаторов родительских комментариев
    /// </summary>
    property ParentsStack: TArray<Integer> read FParents_stack write FParents_stack;
    property Deleted: Boolean read FDeleted write FDeleted;
    /// <summary>
    /// Медиавложения комментария (фотографии, ссылки и т.п.)
    /// </summary>
    property Attachments: TVkAttachmentArray read FAttachments write FAttachments;
    /// <summary>
    /// Информация о вложенной ветке комментариев
    /// </summary>
    property Thread: TVkCommentThread read FThread write FThread;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
  end;

  TVkCommentThread = class(TVkEntityList<TVkComment>)
  private
    FCan_post: Boolean;
    FShow_reply_button: Boolean;
    FGroups_can_post: Boolean;
  public
    /// <summary>
    /// Массив объектов комментариев к записи (только для метода wall.getComments)
    /// </summary>
    property Items;
    /// <summary>
    /// Количество комментариев в ветке
    /// </summary>
    property Count;
    /// <summary>
    /// Может ли текущий пользователь оставлять комментарии в этой ветке
    /// </summary>
    property CanPost: Boolean read FCan_post write FCan_post;
    /// <summary>
    /// Нужно ли отображать кнопку «ответить» в ветке
    /// </summary>
    property ShowReplyButton: Boolean read FShow_reply_button write FShow_reply_button;
    /// <summary>
    /// Могут ли сообщества оставлять комментарии в ветке
    /// </summary>
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
  end;

  TVkComments = class(TVkEntityExtendedList<TVkComment>)
  private
    FCurrent_level_count: Integer;
    FCan_post: Boolean;
    FShow_reply_button: Boolean;
    FGroups_can_post: Boolean;
  public
    property CurrentLevelCount: Integer read FCurrent_level_count write FCurrent_level_count;
    property CanPost: Boolean read FCan_post write FCan_post;
    property ShowReplyButton: Boolean read FShow_reply_button write FShow_reply_button;
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
  end;

  TVkPostFrom = class(TVkObject)
  private
    FName: string;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FScreen_name: string;
    FType: string;
    FFirst_name: string;
    FLast_name: string;
    FCan_access_closed: Boolean;
  public
    //page
    property Name: string read FName write FName;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property ScreenName: string read FScreen_name write FScreen_name;
    // user
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property FirstName: string read FFirst_name write FFirst_name;
    property LastName: string read FLast_name write FLast_name;
    property &Type: string read FType write FType;
  end;

  /// <summary>
  /// Объект, описывающий запись на стене пользователя или сообщества
  /// </summary>
  TVkPost = class(TVkObject, IAttachment)
  private
    FOwner_id: TVkPeerId;
    FFrom_id: TVkPeerId;
    FCreated_by: TVkPeerId;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FText: string;
    FReply_owner_id: TVkPeerId;
    FReply_post_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FFriends_only: Boolean;
    FComments: TVkCommentsInfo;
    FLikes: TVkLikesInfo;
    FReposts: TVkRepostsInfo;
    FViews: TVkViewsInfo;
    FPost_type: string;
    FPost_source: TVkPostSource;
    FAttachments: TVkAttachmentArray;
    FGeo: TVkGeoWall;
    FSigner_id: TVkPeerId;
    FCopy_history: TArray<TVkPost>;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_pin: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_delete: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_edit: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_pinned: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FMarked_as_ads: Boolean;
    FIs_favorite: Boolean;
    FPostponed_id: Integer;
    FTo_id: TVkPeerId;
    FAccess_key: string;
    FCopyright: TVkCopyright;
    FDonut: TVkDonut;
    [JsonReflectAttribute(ctString, rtString, TPostTypeInterceptor)]
    FType: TVkPostType;
    FCan_archive: Boolean;
    FIs_archived: Boolean;
    FShort_text_rate: Extended;
    FHash: string;
    FFrom: TVkPostFrom;
    FCarousel_offset: Integer;
    FIs_deleted: Boolean;
    FDeleted_reason: string;
    FDeleted_details: string;
  public
    /// <summary>
    /// Идентификатор записи
    /// </summary>
    property Id;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Медиавложения записи (фотографии, ссылки и т.п.)
    /// </summary>
    property Attachments: TVkAttachmentArray read FAttachments write FAttachments;
    /// <summary>
    /// Информация о том, может ли запись помещена в арихв
    /// </summary>
    property CanArchive: Boolean read FCan_archive write FCan_archive;
    /// <summary>
    /// Информация о том, может ли текущий пользователь удалить запись
    /// </summary>
    property CanDelete: Boolean read FCan_delete write FCan_delete;
    /// <summary>
    /// Информация о том, может ли текущий пользователь редактировать запись
    /// </summary>
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    /// <summary>
    /// Информация о том, может ли текущий пользователь закрепить запись
    /// </summary>
    property CanPin: Boolean read FCan_pin write FCan_pin;
    property CarouselOffset: Integer read FCarousel_offset write FCarousel_offset;
    /// <summary>
    /// Информация о комментариях к записи
    /// </summary>
    property Comments: TVkCommentsInfo read FComments write FComments;
    /// <summary>
    /// Массив, содержащий историю репостов для записи. Возвращается только в том случае, если запись является репостом. Каждый из объектов массива, в свою очередь, является объектом-записью стандартного формата.
    /// </summary>
    property CopyHistory: TArray<TVkPost> read FCopy_history write FCopy_history;
    /// <summary>
    /// Источник материала
    /// </summary>
    property Copyright: TVkCopyright read FCopyright write FCopyright;
    /// <summary>
    /// Идентификатор администратора, который опубликовал запись (возвращается только для сообществ при запросе с ключом доступа администратора). Возвращается в записях, опубликованных менее 24 часов назад.
    /// </summary>
    property CreatedBy: TVkPeerId read FCreated_by write FCreated_by;
    /// <summary>
    /// Время публикации записи
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Причина удаления записи (IsDeleted)
    /// </summary>
    property DeletedReason: string read FDeleted_reason write FDeleted_reason;
    /// <summary>
    /// Информация об удалении записи (IsDeleted)
    /// </summary>
    property DeletedDetails: string read FDeleted_details write FDeleted_details;
    /// <summary>
    /// Информация о записи VK Donut
    /// </summary>
    property Donut: TVkDonut read FDonut write FDonut;
    /// <summary>
    /// True, если запись была создана с опцией «Только для друзей».
    /// </summary>
    property FriendsOnly: Boolean read FFriends_only write FFriends_only;
    /// <summary>
    /// Откуда запись
    /// </summary>
    property From: TVkPostFrom read FFrom write FFrom;
    /// <summary>
    /// Идентификатор автора записи (от чьего имени опубликована запись)
    /// </summary>
    property FromId: TVkPeerId read FFrom_id write FFrom_id;
    /// <summary>
    /// Информация о местоположении
    /// </summary>
    property Geo: TVkGeoWall read FGeo write FGeo;
    /// <summary>
    /// Hash
    /// </summary>
    property Hash: string read FHash write FHash;
    /// <summary>
    /// Архивная запись
    /// </summary>
    property IsArchived: Boolean read FIs_archived write FIs_archived;
    /// <summary>
    /// Запись удалена
    /// </summary>
    property IsDeleted: Boolean read FIs_deleted write FIs_deleted;
    /// <summary>
    /// True, если объект добавлен в закладки у текущего пользователя.
    /// </summary>
    property IsFavorite: Boolean read FIs_favorite write FIs_favorite;
    /// <summary>
    /// Информация о том, что запись закреплена
    /// </summary>
    property IsPinned: Boolean read FIs_pinned write FIs_pinned;
    /// <summary>
    /// Информация о лайках к записи
    /// </summary>
    property Likes: TVkLikesInfo read FLikes write FLikes;
    /// <summary>
    /// Информация о том, содержит ли запись отметку "реклама"
    /// </summary>
    property MarkedAsAds: Boolean read FMarked_as_ads write FMarked_as_ads;
    /// <summary>
    /// Идентификатор владельца стены, на которой размещена запись. В версиях API ниже 5.7 это поле называется ToId
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    /// <summary>
    /// Идентификатор отложенной записи. Это поле возвращается тогда, когда запись стояла на таймере
    /// </summary>
    property PostponedId: Integer read FPostponed_id write FPostponed_id;
    /// <summary>
    /// Информация о способе размещения записи (Поле возвращается только для Standalone-приложений с ключом доступа, полученным в Implicit Flow.)
    /// </summary>
    property PostSource: TVkPostSource read FPost_source write FPost_source;
    /// <summary>
    /// Тип записи, может принимать следующие значения: post, copy, reply, postpone, suggest.
    /// </summary>
    property PostType: string read FPost_type write FPost_type;
    /// <summary>
    /// Идентификатор владельца записи, в ответ на которую была оставлена текущая
    /// </summary>
    property ReplyOwnerId: TVkPeerId read FReply_owner_id write FReply_owner_id;
    /// <summary>
    /// Идентификатор записи, в ответ на которую была оставлена текущая
    /// </summary>
    property ReplyPostId: Integer read FReply_post_id write FReply_post_id;
    /// <summary>
    /// Информация о репостах записи («Рассказать друзьям»)
    /// </summary>
    property Reposts: TVkRepostsInfo read FReposts write FReposts;
    /// <summary>
    /// Идентификатор автора, если запись была опубликована от имени сообщества и подписана пользователем
    /// </summary>
    property SignerId: TVkPeerId read FSigner_id write FSigner_id;
    /// <summary>
    /// [Получено экспериментальным путём]
    /// </summary>
    property ShortTextRate: Extended read FShort_text_rate write FShort_text_rate;
    /// <summary>
    /// Текст записи
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Идентификатор владельца стены, на которой размещена запись (API ниже 5.7)
    /// </summary>
    property ToId: TVkPeerId read FTo_id write FTo_id;
    /// <summary>
    /// Тип записи, может принимать следующие значения: post, copy, reply, postpone, suggest.
    /// </summary>
    property &Type: TVkPostType read FType write FType;
    /// <summary>
    /// Информация о просмотрах записи
    /// </summary>
    property Views: TVkViewsInfo read FViews write FViews;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
  end;

  TVkPosts = TVkEntityExtendedList<TVkPost>;

  TVkRepostInfo = class(TVkEntity)
  private
    FLikes_count: Integer;
    FPost_id: Integer;
    FReposts_count: Integer;
    FSuccess: Boolean;
  public
    property LikesCount: Integer read FLikes_count write FLikes_count;
    property PostId: Integer read FPost_id write FPost_id;
    property RepostsCount: Integer read FReposts_count write FReposts_count;
    property Success: Boolean read FSuccess write FSuccess;
  end;

implementation

uses
  System.DateUtils, VK.CommonUtils;

{TVkAttachment}

destructor TVkAttachment.Destroy;
begin
  if Assigned(FLink) then
    FLink.Free;
  if Assigned(FPosted_photo) then
    FPosted_photo.Free;
  if Assigned(FAudio_message) then
    FAudio_message.Free;
  if Assigned(FAudio) then
    FAudio.Free;
  if Assigned(FWall_reply) then
    FWall_reply.Free;
  if Assigned(FWall) then
    FWall.Free;
  if Assigned(FCall) then
    FCall.Free;
  if Assigned(FSticker) then
    FSticker.Free;
  if Assigned(FGift) then
    FGift.Free;
  if Assigned(FMarket_album) then
    FMarket_album.Free;
  if Assigned(FMarket) then
    FMarket.Free;
  if Assigned(FDoc) then
    FDoc.Free;
  if Assigned(FVideo) then
    FVideo.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FGraffiti) then
    FGraffiti.Free;
  if Assigned(FNote) then
    FNote.Free;
  if Assigned(FApp) then
    FApp.Free;
  if Assigned(FPoll) then
    FPoll.Free;
  if Assigned(FPage) then
    FPage.Free;
  if Assigned(FMoney_transfer) then
    FMoney_transfer.Free;
  if Assigned(FMoney_request) then
    FMoney_request.Free;
  if Assigned(FAlbum) then
    FAlbum.Free;
  if Assigned(FPretty_cards) then
    FPretty_cards.Free;
  if Assigned(FEvent) then
    FEvent.Free;
  inherited;
end;

function TVkAttachment.GetPreviewUrl(const Size: Integer): string;
begin
  case&Type of
    TVkAttachmentType.Photo:
      Exit(Self.FPhoto.Sizes.GetSizeUrlOrEmpty(Size));
    TVkAttachmentType.Video:
      begin
        for var Image in Self.FVideo.Image do
          if Image.Height >= Size then
            Exit(Image.Url);
        if Length(Self.FVideo.Image) > 0 then
          Exit(Self.FVideo.Image[High(Self.FVideo.Image)].Url)
        else
          Exit('');
      end;
    TVkAttachmentType.Audio:
      Exit('');
    TVkAttachmentType.Doc:
      Exit('');
    TVkAttachmentType.Link:
      Exit(Self.FLink.PreviewUrl);
    TVkAttachmentType.Market:
      Exit('');
    TVkAttachmentType.MarketAlbum:
      Exit('');
    TVkAttachmentType.Wall:
      Exit('');
    TVkAttachmentType.WallReply:
      Exit('');
    TVkAttachmentType.Sticker:
      Exit(Self.FSticker.Images.GetSizeUrlOrEmpty(Size));
    TVkAttachmentType.Gift:
      Exit(Self.FGift.Thumb96);
  else
    Result := '';
  end;
end;

{ TVkComment }

destructor TVkComment.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  if Assigned(FThread) then
    FThread.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  inherited;
end;

function TVkComment.ToAttachment: TAttachment;
begin
  Result := TAttachment.WallReply(OwnerId, Id, AccessKey);
end;

{TVkPost}

destructor TVkPost.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPost>(FCopy_history);
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  if Assigned(FComments) then
    FComments.Free;
  if Assigned(FLikes) then
    FLikes.Free;
  if Assigned(FReposts) then
    FReposts.Free;
  if Assigned(FViews) then
    FViews.Free;
  if Assigned(FGeo) then
    FGeo.Free;
  if Assigned(FPost_source) then
    FPost_source.Free;
  if Assigned(FCopyright) then
    FCopyright.Free;
  if Assigned(FDonut) then
    FDonut.Free;
  if Assigned(FFrom) then
    FFrom.Free;
  inherited;
end;

function TVkPost.ToAttachment: TAttachment;
begin
  Result := TAttachment.Wall(OwnerId, Id, AccessKey);
end;

{ TVkAttachmentHistoryItem }

destructor TVkAttachmentHistoryItem.Destroy;
begin
  if Assigned(FAttachment) then
    FAttachment.Free;
  inherited;
end;

{ TVkAttachmentArrayHelper }

function TVkAttachmentArrayHelper.GetByType<T>(const&Type: TVkAttachmentType; var Attachment: T): Boolean;
begin
  for var Item in Self do
    if Item.&Type = &Type then
    begin
      case&Type of
        TVkAttachmentType.Photo:
          Attachment := T(Item.FPhoto);
        TVkAttachmentType.Video:
          Attachment := T(Item.FVideo);
        TVkAttachmentType.Audio:
          Attachment := T(Item.FAudio);
        TVkAttachmentType.Doc:
          Attachment := T(Item.FDoc);
        TVkAttachmentType.Link:
          Attachment := T(Item.FLink);
        TVkAttachmentType.Market:
          Attachment := T(Item.FMarket);
        TVkAttachmentType.MarketAlbum:
          Attachment := T(Item.FMarket_album);
        TVkAttachmentType.Wall:
          Attachment := T(Item.FWall);
        TVkAttachmentType.WallReply:
          Attachment := T(Item.FWall_reply);
        TVkAttachmentType.Sticker:
          Attachment := T(Item.FSticker);
        TVkAttachmentType.Gift:
          Attachment := T(Item.FGift);
        TVkAttachmentType.Call:
          Attachment := T(Item.FCall);
        TVkAttachmentType.AudioMessage:
          Attachment := T(Item.FAudio_message);
        TVkAttachmentType.PostedPhoto:
          Attachment := T(Item.FPosted_photo);
        TVkAttachmentType.Graffiti:
          Attachment := T(Item.FGraffiti);
        TVkAttachmentType.Note:
          Attachment := T(Item.FNote);
        TVkAttachmentType.App:
          Attachment := T(Item.FApp);
        TVkAttachmentType.Poll:
          Attachment := T(Item.FPoll);
        TVkAttachmentType.Page:
          Attachment := T(Item.FPage);
        TVkAttachmentType.Album:
          Attachment := T(Item.FAlbum);
        //TVkAttachmentType.PhotosList:
        //  Attachment := T(Item.Ph);
          TVkAttachmentType.PrettyCards:
          Attachment := T(Item.FPretty_cards);
        TVkAttachmentType.Event:
          Attachment := T(Item.FEvent);
        TVkAttachmentType.MoneyTransfer:
          Attachment := T(Item.FMoney_transfer);
      end;
      Exit(True);
    end;
  Result := False;
end;

end.

