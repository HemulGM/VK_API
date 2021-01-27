unit VK.Entity.Notifications;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Profile, VK.Entity.Group,
  VK.Entity.Photo, VK.Entity.Common, VK.Entity.Common.List, VK.Entity.Common.ExtendedList;

type
  TVkNotificationAction = class
  private
    FType: string;
    FUrl: string;
  public
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
  end;

  TVkNotificationButtonActionContext = class
  private
    FQuery: string;
    FSnackbar_text: string;
  public
    property Query: string read FQuery write FQuery;
    property SnackbarText: string read FSnackbar_text write FSnackbar_text;
  end;

  TVkNotificationButtonAction = class(TVkEntity)
  private
    FContext: TVkNotificationButtonActionContext;
    FType: string;
  public
    property Context: TVkNotificationButtonActionContext read FContext write FContext;
    property&Type: string read FType write FType;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkNotificationHideButton = class(TVkEntity)
  private
    FAction: TVkNotificationButtonAction;
    FLabel: string;
    FShort_label: string;
    FStyle: string;
  public
    property Action: TVkNotificationButtonAction read FAction write FAction;
    property&Label: string read FLabel write FLabel;
    property ShortLabel: string read FShort_label write FShort_label;
    property Style: string read FStyle write FStyle;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVKNotificationMainItem = class
  private
    FObject_id: string;
    FType: string;
  public
    property ObjectId: string read FObject_id write FObject_id;
    property&Type: string read FType write FType;
  end;

  TVkNotification = class(TVkEntity)
  private
    FAction: TVkNotificationAction;
    FButton_hide: Boolean;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FFooter: string;
    FHeader: string;
    FHide_buttons: TArray<TVkNotificationHideButton>;
    FIcon_type: string;
    FIcon_url: string;
    FId: string;
    FMain_item: TVKNotificationMainItem;
  public
    property Action: TVkNotificationAction read FAction write FAction;
    property ButtonHide: Boolean read FButton_hide write FButton_hide;
    property Date: TDateTime read FDate write FDate;
    property Footer: string read FFooter write FFooter;
    property Header: string read FHeader write FHeader;
    property HideButtons: TArray<TVkNotificationHideButton> read FHide_buttons write FHide_buttons;
    property IconType: string read FIcon_type write FIcon_type;
    property IconUrl: string read FIcon_url write FIcon_url;
    property Id: string read FId write FId;
    property MainItem: TVKNotificationMainItem read FMain_item write FMain_item;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkNotifications = class(TVkEntityExtendedList<TVkNotification>)
  private
    FLast_viewed: Integer;
    FNext_from: string;
    FPhotos: TArray<TVkPhoto>;
    FTtl: Integer;
  public
    property LastViewed: Integer read FLast_viewed write FLast_viewed;
    property NextFrom: string read FNext_from write FNext_from;
    property Photos: TArray<TVkPhoto> read FPhotos write FPhotos;
    property Ttl: Integer read FTtl write FTtl;
  end;

  TVkNotificationMessageError = class
  private
    FCode: Integer;
    FDescription: string;
  public
    /// <summary>
    /// 1 Ч уведомлени€ приложени€ отключены;
    /// 2 Ч отправлено слишком много уведомлений за последний час;
    /// 3 Ч отправлено слишком много уведомлений за последние сутки;
    /// 4 Ч приложение не установлено.
    /// </summary>
    property Code: Integer read FCode write FCode;
    property Description: string read FDescription write FDescription;
  end;

  TVkNotificationMessageStatus = class(TVkEntity)
  private
    FUser_id: Integer;
    FStatus: Boolean;
    FError: TVkNotificationMessageError;
  public
    property UserId: Integer read FUser_id write FUser_id;
    property Status: Boolean read FStatus write FStatus;
    property Error: TVkNotificationMessageError read FError write FError;
    destructor Destroy; override;
  end;

  TVkNotificationMessageStatuses = TVkEntityList<TVkNotificationMessageStatus>;

implementation

uses
  VK.CommonUtils;

{TVkNotificationButtonAction}

constructor TVkNotificationButtonAction.Create;
begin
  inherited;
  FContext := TVkNotificationButtonActionContext.Create();
end;

destructor TVkNotificationButtonAction.Destroy;
begin
  FContext.Free;
  inherited;
end;

{TVkNotificationHideButton}

constructor TVkNotificationHideButton.Create;
begin
  inherited;
  FAction := TVkNotificationButtonAction.Create();
end;

destructor TVkNotificationHideButton.Destroy;
begin
  FAction.Free;
  inherited;
end;
{TVkNotification}

constructor TVkNotification.Create;
begin
  inherited;
  FMain_item := TVKNotificationMainItem.Create();
  FAction := TVkNotificationAction.Create();
end;

destructor TVkNotification.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkNotificationHideButton>(FHide_buttons);
  FMain_item.Free;
  FAction.Free;
  inherited;
end;

{ TVkNotificationMessageStatus }

destructor TVkNotificationMessageStatus.Destroy;
begin
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

end.

