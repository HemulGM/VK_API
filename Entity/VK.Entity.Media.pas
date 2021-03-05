unit VK.Entity.Media;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json, VK.Entity.Common, VK.Entity.Photo,
  VK.Entity.Link, VK.Entity.AudioMessage, VK.Entity.Sticker, VK.Entity.Gift, VK.Entity.Market, VK.Entity.Doc,
  VK.Entity.Audio, VK.Entity.Video, VK.Entity.Graffiti, VK.Entity.Note, VK.Entity.OldApp, VK.Entity.Poll, VK.Entity.Page,
  VK.Entity.Album, VK.Entity.PrettyCard, VK.Types, VK.Entity.Event, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Call,
  VK.Entity.Market.Album, VK.Entity.Info, VK.Entity.Common.List, VK.Entity.Common.ExtendedList, VK.Entity.Donut,
  VK.Wrap.Interceptors, VK.Entity.MoneyTransfer, VK.Entity.Geo;

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
    property&Platform: string read FPlatform write FPlatform;
    /// <summary>
    /// Тип источника
    /// </summary>
    property&Type: TVkPostSourceType read FType write FType;
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
  public
    property&Type: TVkAttachmentType read FType write FType;
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
    function GetPreviewUrl: string;
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
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkAttachmentHistory = class(TVkEntityExtendedList<TVkAttachmentHistoryItem>)
  private
    FNext_from: string;
  public
    property NextFrom: string read FNext_from write FNext_from;
  end;

  TVkAttachments = TVkEntityList<TVkAttachment>;

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
    FAttachments: TVkAttachments;
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
    property Attachments: TVkAttachments read FAttachments write FAttachments;
    /// <summary>
    /// Информация о вложенной ветке комментариев
    /// </summary>
    property Thread: TVkCommentThread read FThread write FThread;
    constructor Create; override;
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

  /// <summary>
  /// Объект, описывающий запись на стене пользователя или сообщества
  /// </summary>
  TVkPost = class(TVkObject, IAttachment)
  private
    FOwner_id: Integer;
    FFrom_id: Integer;
    FCreated_by: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FText: string;
    FReply_owner_id: Integer;
    FReply_post_id: Integer;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FFriends_only: Boolean;
    FComments: TVkCommentsInfo;
    FLikes: TVkLikesInfo;
    FReposts: TVkRepostsInfo;
    FViews: TVkViewsInfo;
    FPost_type: string;
    FPost_source: TVkPostSource;
    FAttachments: TArray<TVkAttachment>;
    FGeo: TVkGeo;
    FSigner_id: Integer;
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
    FTo_id: Integer;
    FAccess_key: string;
    FCopyright: TVkCopyright;
    FDonut: TVkDonut;
    [JsonReflectAttribute(ctString, rtString, TPostTypeInterceptor)]
    FType: TVkPostType;
    FCan_archive: Boolean;
    FIs_archived: Boolean;
    FShort_text_rate: Extended;
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
    property Attachments: TArray<TVkAttachment> read FAttachments write FAttachments;
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
    property CreatedBy: Integer read FCreated_by write FCreated_by;
    /// <summary>
    /// Время публикации записи
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Информация о записи VK Donut
    /// </summary>
    property Donut: TVkDonut read FDonut write FDonut;
    /// <summary>
    /// True, если запись была создана с опцией «Только для друзей».
    /// </summary>
    property FriendsOnly: Boolean read FFriends_only write FFriends_only;
    /// <summary>
    /// Идентификатор автора записи (от чьего имени опубликована запись)
    /// </summary>
    property FromId: Integer read FFrom_id write FFrom_id;
    /// <summary>
    /// Информация о местоположении
    /// </summary>
    property Geo: TVkGeo read FGeo write FGeo;
    /// <summary>
    /// Архивная запись
    /// </summary>
    property IsArchived: Boolean read FIs_archived write FIs_archived;
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
    property OwnerId: Integer read FOwner_id write FOwner_id;
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
    property ReplyOwnerId: Integer read FReply_owner_id write FReply_owner_id;
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
    property SignerId: Integer read FSigner_id write FSigner_id;
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
    property ToId: Integer read FTo_id write FTo_id;
    /// <summary>
    /// Тип записи, может принимать следующие значения: post, copy, reply, postpone, suggest.
    /// </summary>
    property&Type: TVkPostType read FType write FType;
    /// <summary>
    /// Информация о просмотрах записи
    /// </summary>
    property Views: TVkViewsInfo read FViews write FViews;
    constructor Create; override;
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
  if Assigned(FAlbum) then
    FAlbum.Free;
  if Assigned(FPretty_cards) then
    FPretty_cards.Free;
  if Assigned(FEvent) then
    FEvent.Free;

  inherited;
end;

function TVkAttachment.GetPreviewUrl: string;
begin
  case&Type of
    TVkAttachmentType.Photo:
      Exit(Self.FPhoto.Sizes[4].Url);
    TVkAttachmentType.Video:
      Exit(Self.FVideo.Image[4].Url);
    TVkAttachmentType.Audio:
      Exit('');
    TVkAttachmentType.Doc:
      Exit('');
    TVkAttachmentType.Link:
      Exit('');
    TVkAttachmentType.Market:
      Exit('');
    TVkAttachmentType.MarketAlbum:
      Exit('');
    TVkAttachmentType.Wall:
      Exit('');
    TVkAttachmentType.WallReply:
      Exit('');
    TVkAttachmentType.Sticker:
      Exit(Self.FSticker.Images[1].Url);
    TVkAttachmentType.Gift:
      Exit('');
  else
    Result := '';
  end;
end;

{ TVkComment }

constructor TVkComment.Create;
begin
  FAttachments := TVkAttachments.Create;
  FLikes := TVkLikesInfo.Create;
  FThread := TVkCommentThread.Create;
end;

destructor TVkComment.Destroy;
begin
  FThread.Free;
  FAttachments.Free;
  FLikes.Free;
  inherited;
end;

function TVkComment.ToAttachment: TAttachment;
begin
  Result := TAttachment.WallReply(OwnerId, Id, AccessKey);
end;

{TVkPost}

constructor TVkPost.Create;
begin
  inherited;
  FComments := TVkCommentsInfo.Create();
  FLikes := TVkLikesInfo.Create();
  FReposts := TVkRepostsInfo.Create();
  FViews := TVkViewsInfo.Create;
end;

destructor TVkPost.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPost>(FCopy_history);
  TArrayHelp.FreeArrayOfObject<TVkAttachment>(FAttachments);
  FComments.Free;
  FLikes.Free;
  FReposts.Free;
  FViews.Free;
  if Assigned(FGeo) then
    FGeo.Free;
  if Assigned(FPost_source) then
    FPost_source.Free;
  if Assigned(FCopyright) then
    FCopyright.Free;
  if Assigned(FDonut) then
    FDonut.Free;
  inherited;
end;

function TVkPost.ToAttachment: TAttachment;
begin
  Result := TAttachment.Wall(OwnerId, Id, AccessKey);
end;

{ TVkAttachmentHistoryItem }

constructor TVkAttachmentHistoryItem.Create;
begin
  FAttachment := TVkAttachment.Create;
end;

destructor TVkAttachmentHistoryItem.Destroy;
begin
  FAttachment.Free;
  inherited;
end;

end.

