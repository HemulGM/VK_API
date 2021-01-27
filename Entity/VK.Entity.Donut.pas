unit VK.Entity.Donut;

interface

uses
  REST.Json, REST.JsonReflect, REST.Json.Interceptors, VK.Entity.Profile,
  VK.Entity.Group, VK.Entity.Common;

type
  TVkDonut = class(TVkEntity)
  private
    FIs_donut: Boolean;
    FPaid_duration: Integer;
    FPlaceholder: Integer;
    FCan_publish_free_copy: Boolean;
    FEdit_mode: string;
  public
    /// <summary>
    /// Запись доступна только платным подписчикам VK Donut
    /// </summary>
    property IsDonut: Boolean read FIs_donut write FIs_donut;
    /// <summary>
    /// Можно ли открыть запись для всех пользователей, а не только подписчиков VK Donut
    /// </summary>
    property CanPublishFreeCopy: Boolean read FCan_publish_free_copy write FCan_publish_free_copy;
    /// <summary>
    /// Время, в течение которого запись будет доступна только платным подписчикам VK Donut
    /// </summary>
    property PaidDuration: Integer read FPaid_duration write FPaid_duration;
    /// <summary>
    /// Заглушка для пользователей, которые не оформили подписку VK Donut. Отображается вместо содержимого записи
    /// </summary>
    property Placeholder: Integer read FPlaceholder write FPlaceholder;
    /// <summary>
    /// Информация о том, какие значения VK Donut можно изменить в записи. Возможные значения:
    /// all — всю информацию о VK Donut.
    /// duration — время, в течение которого запись будет доступна только платным подписчикам VK Donut.
    /// </summary>
    property EditMode: string read FEdit_mode write FEdit_mode;
  end;

  TVkDonutInfo = class
  private
    FIs_don: Boolean;
    FPlaceholder: string;
  public
    /// <summary>
    /// Является ли комментатор подписчиком VK Donut
    /// </summary>
    property IsDon: Boolean read FIs_don write FIs_don;
    /// <summary>
    /// Заглушка для пользователей, которые не оформили подписку VK Donut
    /// </summary>
    property Placeholder: string read FPlaceholder write FPlaceholder;
  end;

  TVkDonutSubscription = class(TVkEntity)
  private
    FAmount: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FNext_Payment_Date: TDateTime;
    FOwner_Id: Integer;
    FStatus: string;
  public
    /// <summary>
    /// Стоимость подписки (в рублях)
    /// </summary>
    property Amount: Integer read FAmount write FAmount;
    /// <summary>
    /// Дата следующего платежа
    /// </summary>
    property NextPaymentDate: TDateTime read FNext_Payment_Date write FNext_Payment_Date;
    /// <summary>
    /// Идентификатор сообщества, доном которого является пользователь
    /// </summary>
    property OwnerId: Integer read FOwner_Id write FOwner_Id;
    /// <summary>
    /// Статус подписки
    /// </summary>
    property Status: string read FStatus write FStatus;
  end;

  TVkDonutSubscriptions = class(TVkEntity)
  private
    FCount: Integer;
    FGroups: TArray<TVkGroup>;
    FProfiles: TArray<TVkProfile>;
    FSubscriptions: TArray<TVkDonutSubscription>;
  public
    property Count: Integer read FCount write FCount;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Subscriptions: TArray<TVkDonutSubscription> read FSubscriptions write FSubscriptions;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkDonutSubscriptions }

destructor TVkDonutSubscriptions.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkDonutSubscription>(FSubscriptions);
  inherited;
end;

end.

