unit VK.Entity.Page;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, VK.Entity.Common,
  VK.Entity.Common.List, VK.Wrap.Interceptors, VK.Types;

type
  TVkPage = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FCreated: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FEdited: TDateTime;
    FGroup_id: TVkPeerId;
    FParent2: string;
    FTitle: string;
    FView_url: string;
    FViews: integer;
    FWho_can_edit: Integer;
    FWho_can_view: Integer;
    FCreator_id: TVkPeerId;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCurrent_user_can_edit: Boolean;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FCurrent_user_can_edit_access: Boolean;
    FEditor_id: TVkPeerId;
    FParent: string;
    FSource: string;
    FHtml: string;
    FAccess_key: string;
  public
    /// <summary>
    /// »дентификатор вики-страницы
    /// </summary>
    property Id;
    /// <summary>
    ///  люч доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// »дентификатор группы, которой принадлежит вики-страница.
    /// </summary>
    property GroupId: TVkPeerId read FGroup_id write FGroup_id;
    /// <summary>
    /// »дентификатор создател€ вики-страницы.
    /// </summary>
    property CreatorId: TVkPeerId read FCreator_id write FCreator_id;
    /// <summary>
    /// Ќазвание вики-страницы.
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// True, если текущий пользователь может редактировать текст вики-страницы, иначе Ч False.
    /// </summary>
    property CurrentUserCanEdit: Boolean read FCurrent_user_can_edit write FCurrent_user_can_edit;
    /// <summary>
    /// True, если текущий пользователь может измен€ть права доступа на вики-страницу, иначе Ч False.
    /// </summary>
    property CurrentUserCanEditAccess: Boolean read FCurrent_user_can_edit_access write FCurrent_user_can_edit_access;
    /// <summary>
    /// »нформаци€ о том, кто может просматривать вики-страницу:
    /// 2 Ч просматривать страницу могут все;
    /// 1 Ч только участники сообщества;
    /// 0 Ч только руководители сообщества.
    /// </summary>
    property WhoCanView: Integer read FWho_can_view write FWho_can_view;
    /// <summary>
    /// указывает, кто может редактировать вики-страницу:
    /// 2 Ч редактировать страницу могут все;
    /// 1 Ч только участники сообщества;
    /// 0 Ч только руководители сообщества.
    /// </summary>
    property WhoCanEdit: Integer read FWho_can_edit write FWho_can_edit;
    /// <summary>
    /// ƒата последнего изменени€ вики-страницы
    /// </summary>
    property Edited: TDateTime read FEdited write FEdited;
    /// <summary>
    /// ƒата создани€ вики-страницы
    /// </summary>
    property Created: TDateTime read FCreated write FCreated;
    /// <summary>
    /// »дентификатор пользовател€, который редактировал вики-страницу последним
    /// </summary>
    property EditorId: TVkPeerId read FEditor_id write FEditor_id;
    /// <summary>
    ///  оличество просмотров вики-страницы
    /// </summary>
    property Views: Integer read FViews write FViews;
    /// <summary>
    /// «аголовок родительской страницы дл€ навигации, если есть
    /// </summary>
    property Parent: string read FParent write FParent;
    /// <summary>
    /// «аголовок второй родительской страницы дл€ навигации, если есть
    /// </summary>
    property Parent2: string read FParent2 write FParent2;
    /// <summary>
    /// “екст страницы в вики-формате, если был запрошен
    /// </summary>
    property Source: string read FSource write FSource;
    /// <summary>
    /// “екст страницы в html-формате, если был запрошен
    /// </summary>
    property Html: string read FHtml write FHtml;
    /// <summary>
    /// јдрес страницы дл€ отображени€ вики-страницы
    /// </summary>
    property ViewUrl: string read FView_url write FView_url;
  end;

  TVkPages = TVkEntityList<TVkPage>;

  TVkPageVersion = class(TVkObject)
  private
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FEditor_id: Integer;
    FEditor_name: string;
    FLength: Integer;
  public
    property Date: TDateTime read FDate write FDate;
    property EditorId: Integer read FEditor_id write FEditor_id;
    property EditorName: string read FEditor_name write FEditor_name;
    property Length: Integer read FLength write FLength;
  end;

  TVkPageVersions = TVkEntityList<TVkPageVersion>;

implementation

end.

