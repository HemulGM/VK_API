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
    /// Идентификатор вики-страницы
    /// </summary>
    property Id;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Идентификатор группы, которой принадлежит вики-страница.
    /// </summary>
    property GroupId: TVkPeerId read FGroup_id write FGroup_id;
    /// <summary>
    /// Идентификатор создателя вики-страницы.
    /// </summary>
    property CreatorId: TVkPeerId read FCreator_id write FCreator_id;
    /// <summary>
    /// Название вики-страницы.
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// True, если текущий пользователь может редактировать текст вики-страницы, иначе — False.
    /// </summary>
    property CurrentUserCanEdit: Boolean read FCurrent_user_can_edit write FCurrent_user_can_edit;
    /// <summary>
    /// True, если текущий пользователь может изменять права доступа на вики-страницу, иначе — False.
    /// </summary>
    property CurrentUserCanEditAccess: Boolean read FCurrent_user_can_edit_access write FCurrent_user_can_edit_access;
    /// <summary>
    /// Информация о том, кто может просматривать вики-страницу:
    /// 2 — просматривать страницу могут все;
    /// 1 — только участники сообщества;
    /// 0 — только руководители сообщества.
    /// </summary>
    property WhoCanView: Integer read FWho_can_view write FWho_can_view;
    /// <summary>
    /// указывает, кто может редактировать вики-страницу:
    /// 2 — редактировать страницу могут все;
    /// 1 — только участники сообщества;
    /// 0 — только руководители сообщества.
    /// </summary>
    property WhoCanEdit: Integer read FWho_can_edit write FWho_can_edit;
    /// <summary>
    /// Дата последнего изменения вики-страницы
    /// </summary>
    property Edited: TDateTime read FEdited write FEdited;
    /// <summary>
    /// Дата создания вики-страницы
    /// </summary>
    property Created: TDateTime read FCreated write FCreated;
    /// <summary>
    /// Идентификатор пользователя, который редактировал вики-страницу последним
    /// </summary>
    property EditorId: TVkPeerId read FEditor_id write FEditor_id;
    /// <summary>
    /// Количество просмотров вики-страницы
    /// </summary>
    property Views: Integer read FViews write FViews;
    /// <summary>
    /// Заголовок родительской страницы для навигации, если есть
    /// </summary>
    property Parent: string read FParent write FParent;
    /// <summary>
    /// Заголовок второй родительской страницы для навигации, если есть
    /// </summary>
    property Parent2: string read FParent2 write FParent2;
    /// <summary>
    /// Текст страницы в вики-формате, если был запрошен
    /// </summary>
    property Source: string read FSource write FSource;
    /// <summary>
    /// Текст страницы в html-формате, если был запрошен
    /// </summary>
    property Html: string read FHtml write FHtml;
    /// <summary>
    /// Адрес страницы для отображения вики-страницы
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

