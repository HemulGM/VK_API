unit VK.Entity.Notifications;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Photo;

type
  TVkNotificationAction = class
  private
    FType: string;
    FUrl: string;
  public
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotificationAction;
  end;

  TVkNotificationButtonActionContext = class
  private
    FQuery: string;
    FSnackbar_text: string;
  public
    property Query: string read FQuery write FQuery;
    property SnackbarText: string read FSnackbar_text write FSnackbar_text;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotificationButtonActionContext;
  end;

  TVkNotificationButtonAction = class
  private
    FContext: TVkNotificationButtonActionContext;
    FType: string;
  public
    property Context: TVkNotificationButtonActionContext read FContext write FContext;
    property&Type: string read FType write FType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotificationButtonAction;
  end;

  TVkNotificationHideButton = class
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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotificationHideButton;
  end;

  TVKNotificationMainItem = class
  private
    FObject_id: string;
    FType: string;
  public
    property ObjectId: string read FObject_id write FObject_id;
    property&Type: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVKNotificationMainItem;
  end;

  TVkNotification = class
  private
    FAction: TVkNotificationAction;
    FButton_hide: Boolean;
    FDate: Int64;
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
    property Date: Int64 read FDate write FDate;
    property Footer: string read FFooter write FFooter;
    property Header: string read FHeader write FHeader;
    property HideButtons: TArray<TVkNotificationHideButton> read FHide_buttons write FHide_buttons;
    property IconType: string read FIcon_type write FIcon_type;
    property IconUrl: string read FIcon_url write FIcon_url;
    property Id: string read FId write FId;
    property MainItem: TVKNotificationMainItem read FMain_item write FMain_item;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotification;
  end;

  TVkNotifications = class
  private
    FCount: Integer;
    FGroups: TArray<TVkGroup>;
    FItems: TArray<TVkNotification>;
    FLast_viewed: Integer;
    FNext_from: string;
    FPhotos: TArray<TVkPhoto>;
    FProfiles: TArray<TVkProfile>;
    FTtl: Integer;
  public
    property Count: Integer read FCount write FCount;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Items: TArray<TVkNotification> read FItems write FItems;
    property LastViewed: Integer read FLast_viewed write FLast_viewed;
    property NextFrom: string read FNext_from write FNext_from;
    property Photos: TArray<TVkPhoto> read FPhotos write FPhotos;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Ttl: Integer read FTtl write FTtl;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotifications;
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotificationMessageError;
  end;

  TVkNotificationMessageStatus = class
  private
    FUser_id: Integer;
    FStatus: Boolean;
    FError: TVkNotificationMessageError;
  public
    property UserId: Integer read FUser_id write FUser_id;
    property Status: Boolean read FStatus write FStatus;
    property Error: TVkNotificationMessageError read FError write FError;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotificationMessageStatus;
  end;

  TVkNotificationMessageStatuses = class
  private
    FCount: Integer;
    FItems: TArray<TVkNotificationMessageStatus>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkNotificationMessageStatus> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotificationMessageStatuses;
  end;

implementation

uses
  VK.CommonUtils;

{TVkNotificationAction}

function TVkNotificationAction.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNotificationAction.FromJsonString(AJsonString: string): TVkNotificationAction;
begin
  result := TJson.JsonToObject<TVkNotificationAction>(AJsonString)
end;

{TVkNotificationButtonActionContext}

function TVkNotificationButtonActionContext.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNotificationButtonActionContext.FromJsonString(AJsonString: string): TVkNotificationButtonActionContext;
begin
  result := TJson.JsonToObject<TVkNotificationButtonActionContext>(AJsonString)
end;

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

function TVkNotificationButtonAction.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNotificationButtonAction.FromJsonString(AJsonString: string): TVkNotificationButtonAction;
begin
  result := TJson.JsonToObject<TVkNotificationButtonAction>(AJsonString)
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

function TVkNotificationHideButton.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNotificationHideButton.FromJsonString(AJsonString: string): TVkNotificationHideButton;
begin
  result := TJson.JsonToObject<TVkNotificationHideButton>(AJsonString)
end;

{TVKNotificationMainItem}

function TVKNotificationMainItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVKNotificationMainItem.FromJsonString(AJsonString: string): TVKNotificationMainItem;
begin
  result := TJson.JsonToObject<TVKNotificationMainItem>(AJsonString)
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

function TVkNotification.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNotification.FromJsonString(AJsonString: string): TVkNotification;
begin
  result := TJson.JsonToObject<TVkNotification>(AJsonString)
end;

{TVkNotifications}

destructor TVkNotifications.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkNotification>(FItems);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkPhoto>(FPhotos);
  inherited;
end;

function TVkNotifications.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNotifications.FromJsonString(AJsonString: string): TVkNotifications;
begin
  result := TJson.JsonToObject<TVkNotifications>(AJsonString)
end;

{ TVkNotificationMessageStatuses }

destructor TVkNotificationMessageStatuses.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkNotificationMessageStatus>(FItems);
  inherited;
end;

class function TVkNotificationMessageStatuses.FromJsonString(AJsonString: string): TVkNotificationMessageStatuses;
begin
  result := TJson.JsonToObject<TVkNotificationMessageStatuses>(AJsonString)
end;

function TVkNotificationMessageStatuses.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkNotificationMessageError }

class function TVkNotificationMessageError.FromJsonString(AJsonString: string): TVkNotificationMessageError;
begin
  result := TJson.JsonToObject<TVkNotificationMessageError>(AJsonString)
end;

function TVkNotificationMessageError.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkNotificationMessageStatus }

destructor TVkNotificationMessageStatus.Destroy;
begin
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

class function TVkNotificationMessageStatus.FromJsonString(AJsonString: string): TVkNotificationMessageStatus;
begin
  result := TJson.JsonToObject<TVkNotificationMessageStatus>(AJsonString)
end;

function TVkNotificationMessageStatus.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

