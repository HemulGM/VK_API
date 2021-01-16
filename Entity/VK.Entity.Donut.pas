unit VK.Entity.Donut;

interface

uses
  REST.Json, REST.JsonReflect, REST.Json.Interceptors, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common;

type
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

