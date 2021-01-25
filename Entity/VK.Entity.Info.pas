unit VK.Entity.Info;

interface

uses
  Generics.Collections, Rest.Json, REST.JsonReflect, VK.Entity.Common,
  VK.Wrap.Interceptors;

type
  TVkCommentInfo = class(TVkEntity)
  private
    FComment_id: Integer;
    FParents_stack: TArray<Integer>;
  public
    property CommentId: Integer read FComment_id write FComment_id;
    property ParentsStack: TArray<Integer> read FParents_stack write FParents_stack;
  end;

  TVkViewsInfo = class(TVkCounterEntity)
  public
    /// <summary>
    /// Число просмотров записи
    /// </summary>
    property Count;
  end;

  TVkLikesInfo = class(TVkCounterEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_like: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_publish: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FUser_likes: Boolean;
  public
    /// <summary>
    /// Число отметок «Мне нравится»
    /// </summary>
    property Count;
    /// <summary>
    /// Информация о том, может ли текущий пользователь поставить отметку «Мне нравится»
    /// </summary>
    property CanLike: Boolean read FCan_like write FCan_like;
    /// <summary>
    /// Информация о том, может ли текущий пользователь сделать репост записи
    /// </summary>
    property CanPublish: Boolean read FCan_publish write FCan_publish;
    /// <summary>
    /// Есть ли отметка «Мне нравится» от текущего пользователя
    /// </summary>
    property UserLikes: Boolean read FUser_likes write FUser_likes;
  end;

  TVkRepostsInfo = class(TVkCounterEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FUser_reposted: Boolean;
    FMail_count: Integer;
    FWall_count: Integer;
  public
    /// <summary>
    /// Число пользователей, скопировавших запись
    /// </summary>
    property Count;
    /// <summary>
    /// Наличие репоста от текущего пользователя
    /// </summary>
    property UserReposted: Boolean read FUser_reposted write FUser_reposted;
    property WallCount: Integer read FWall_count write FWall_count;
    property MailCount: Integer read FMail_count write FMail_count;
  end;

  TVkCommentsInfo = class(TVkCounterEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCan_post: Boolean;
    FGroups_can_post: Boolean;
    FCanOpen: Boolean;
    FCan_close: Boolean;
    FCan_open: Boolean;
  public
    /// <summary>
    /// Количество комментариев
    /// </summary>
    property Count;
    /// <summary>
    /// Информация о том, может ли текущий пользователь комментировать запись (1 — может, 0 — не может)
    /// </summary>
    property CanPost: Boolean read FCan_post write FCan_post;
    /// <summary>
    /// Информация о том, могут ли сообщества комментировать запись
    /// </summary>
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
    /// <summary>
    /// Может ли текущий пользователь закрыть комментарии к записи
    /// </summary>
    property CanClose: Boolean read FCan_close write FCan_close;
    /// <summary>
    /// может ли текущий пользователь открыть комментарии к записи
    /// </summary>
    property CanOpen: Boolean read FCan_open write FCan_open;
  end;

implementation

end.

