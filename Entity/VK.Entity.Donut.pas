unit VK.Entity.Donut;

interface

uses
  REST.Json, VK.Entity.Profile, VK.Entity.Group;

type
  TVkDonutSubscription = class
  private
    FAmount: Integer;
    FNext_Payment_Date: Int64;
    FOwner_Id: Integer;
    FStatus: string;
  public
    /// <summary>
    /// Стоимость подписки (в рублях)
    /// </summary>
    property Amount: Integer read FAmount write FAmount;
    /// <summary>
    /// Дата следующего платежа в формате 'unixtime'
    /// </summary>
    property NextPaymentDate: Int64 read FNext_Payment_Date write FNext_Payment_Date;
    /// <summary>
    /// Идентификатор сообщества, доном которого является пользователь
    /// </summary>
    property OwnerId: Integer read FOwner_Id write FOwner_Id;
    /// <summary>
    /// Статус подписки
    /// </summary>
    property Status: string read FStatus write FStatus;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkDonutSubscription;
  end;

  TVkDonutSubscriptions = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkDonutSubscriptions;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkDonutSubscription }

class function TVkDonutSubscription.FromJsonString(AJsonString: string): TVkDonutSubscription;
begin
  result := TJson.JsonToObject<TVkDonutSubscription>(AJsonString);
end;

function TVkDonutSubscription.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(Self);
end;

{ TVkDonutSubscriptions }

destructor TVkDonutSubscriptions.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkDonutSubscription>(FSubscriptions);
  inherited;
end;

class function TVkDonutSubscriptions.FromJsonString(AJsonString: string): TVkDonutSubscriptions;
begin
  result := TJson.JsonToObject<TVkDonutSubscriptions>(AJsonString);
end;

function TVkDonutSubscriptions.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(Self);
end;

end.

