unit VK.Entity.Stats;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkStatBasic = class
  private
    FCount: Integer;
    FValue: string;
  public
    property Count: Integer read FCount write FCount;
    property Value: string read FValue write FValue;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatBasic;
  end;

  TVkStatCountries = class
  private
    FCode: string;
    FCount: Integer;
    FName: string;
    FValue: Integer;
  public
    property Code: string read FCode write FCode;
    property Count: Integer read FCount write FCount;
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatCountries;
  end;

  TVkStatCities = class
  private
    FCount: Integer;
    FName: string;
    FValue: Integer;
  public
    property Count: Integer read FCount write FCount;
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatCities;
  end;

  TVkStatVisitors = class
  private
    FAge: TArray<TVkStatBasic>;
    FCities: TArray<TVkStatCities>;
    FCountries: TArray<TVkStatCountries>;
    FMobile_views: Integer;
    FSex: TArray<TVkStatBasic>;
    FSex_age: TArray<TVkStatBasic>;
    FViews: Integer;
    FVisitors: Integer;
  public
    property Age: TArray<TVkStatBasic> read FAge write FAge;
    property Cities: TArray<TVkStatCities> read FCities write FCities;
    property Countries: TArray<TVkStatCountries> read FCountries write FCountries;
    property MobileViews: Integer read FMobile_views write FMobile_views;
    property Sex: TArray<TVkStatBasic> read FSex write FSex;
    property SexAge: TArray<TVkStatBasic> read FSex_age write FSex_age;
    property Views: Integer read FViews write FViews;
    property Visitors: Integer read FVisitors write FVisitors;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatVisitors;
  end;

  TVkStatReach = class
  private
    FAge: TArray<TVkStatBasic>;
    FCities: TArray<TVkStatCities>;
    FCountries: TArray<TVkStatCountries>;
    FMobile_reach: Integer;
    FReach: Integer;
    FReach_subscribers: Integer;
    FSex: TArray<TVkStatBasic>;
    FSex_age: TArray<TVkStatBasic>;
  public
    property Age: TArray<TVkStatBasic> read FAge write FAge;
    property Cities: TArray<TVkStatCities> read FCities write FCities;
    property Countries: TArray<TVkStatCountries> read FCountries write FCountries;
    property MobileReach: Integer read FMobile_reach write FMobile_reach;
    property Reach: Integer read FReach write FReach;
    property ReachSubscribers: Integer read FReach_subscribers write FReach_subscribers;
    property Sex: TArray<TVkStatBasic> read FSex write FSex;
    property SexAge: TArray<TVkStatBasic> read FSex_age write FSex_age;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatReach;
  end;

  TVkStatItem = class
  private
    FPeriod_from: Integer;
    FPeriod_to: Integer;
    FReach: TVkStatReach;
    FVisitors: TVkStatVisitors;
  public
    property PeriodFrom: Integer read FPeriod_from write FPeriod_from;
    property PeriodTo: Integer read FPeriod_to write FPeriod_to;
    property Reach: TVkStatReach read FReach write FReach;
    property Visitors: TVkStatVisitors read FVisitors write FVisitors;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatItem;
  end;

  TVkStatItems = class
  private
    FItems: TArray<TVkStatItem>;
  public
    property Items: TArray<TVkStatItem> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatItems;
  end;

  TVkStatPostReachItem = class
  private
    FHide: Integer;
    FJoin_group: Integer;
    FLinks: Integer;
    FReach_subscribers: Integer;
    FReach_total: Integer;
    FReport: Integer;
    FTo_group: Integer;
    FUnsubscribe: Integer;
    FReach_ads: Integer;
    FReach_viral: Integer;
  public
    property Hide: Integer read FHide write FHide;
    property JoinGroup: Integer read FJoin_group write FJoin_group;
    property Links: Integer read FLinks write FLinks;
    property ReachSubscribers: Integer read FReach_subscribers write FReach_subscribers;
    property ReachTotal: Integer read FReach_total write FReach_total;
    property ReachAds: Integer read FReach_ads write FReach_ads;
    property ReachViral: Integer read FReach_viral write FReach_viral;
    property Report: Integer read FReport write FReport;
    property ToGroup: Integer read FTo_group write FTo_group;
    property Unsubscribe: Integer read FUnsubscribe write FUnsubscribe;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatPostReachItem;
  end;

  TVkStatPostReachItems = class
  private
    FItems: TArray<TVkStatPostReachItem>;
  public
    property Items: TArray<TVkStatPostReachItem> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatPostReachItems;
  end;

implementation

uses
  VK.CommonUtils;

{TVkStatBasic}

function TVkStatBasic.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatBasic.FromJsonString(AJsonString: string): TVkStatBasic;
begin
  result := TJson.JsonToObject<TVkStatBasic>(AJsonString)
end;

{TVkStatCountries}

function TVkStatCountries.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatCountries.FromJsonString(AJsonString: string): TVkStatCountries;
begin
  result := TJson.JsonToObject<TVkStatCountries>(AJsonString)
end;

{TVkStatCities}

function TVkStatCities.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatCities.FromJsonString(AJsonString: string): TVkStatCities;
begin
  result := TJson.JsonToObject<TVkStatCities>(AJsonString)
end;

{TVkStatVisitors}

destructor TVkStatVisitors.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStatBasic>(FAge);
  TArrayHelp.FreeArrayOfObject<TVkStatCities>(FCities);
  TArrayHelp.FreeArrayOfObject<TVkStatCountries>(FCountries);
  TArrayHelp.FreeArrayOfObject<TVkStatBasic>(FSex);
  TArrayHelp.FreeArrayOfObject<TVkStatBasic>(FSex_age);
  inherited;
end;

function TVkStatVisitors.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatVisitors.FromJsonString(AJsonString: string): TVkStatVisitors;
begin
  result := TJson.JsonToObject<TVkStatVisitors>(AJsonString)
end;

{TVkStatReach}

destructor TVkStatReach.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStatBasic>(FAge);
  TArrayHelp.FreeArrayOfObject<TVkStatCities>(FCities);
  TArrayHelp.FreeArrayOfObject<TVkStatCountries>(FCountries);
  TArrayHelp.FreeArrayOfObject<TVkStatBasic>(FSex);
  TArrayHelp.FreeArrayOfObject<TVkStatBasic>(FSex_age);
  inherited;
end;

function TVkStatReach.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatReach.FromJsonString(AJsonString: string): TVkStatReach;
begin
  result := TJson.JsonToObject<TVkStatReach>(AJsonString)
end;

{TVkStatItem}

constructor TVkStatItem.Create;
begin
  inherited;
  FReach := TVkStatReach.Create();
  FVisitors := TVkStatVisitors.Create();
end;

destructor TVkStatItem.Destroy;
begin
  FReach.Free;
  FVisitors.Free;
  inherited;
end;

function TVkStatItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatItem.FromJsonString(AJsonString: string): TVkStatItem;
begin
  result := TJson.JsonToObject<TVkStatItem>(AJsonString)
end;

{TVkStatItems}

destructor TVkStatItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStatItem>(FItems);
  inherited;
end;

function TVkStatItems.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatItems.FromJsonString(AJsonString: string): TVkStatItems;
begin
  result := TJson.JsonToObject<TVkStatItems>(AJsonString)
end;

{TVkStatPostReachItem}

function TVkStatPostReachItem.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatPostReachItem.FromJsonString(AJsonString: string): TVkStatPostReachItem;
begin
  result := TJson.JsonToObject<TVkStatPostReachItem>(AJsonString)
end;

{TVkStatPostReachItems}

destructor TVkStatPostReachItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStatPostReachItem>(FItems);
  inherited;
end;

function TVkStatPostReachItems.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatPostReachItems.FromJsonString(AJsonString: string): TVkStatPostReachItems;
begin
  result := TJson.JsonToObject<TVkStatPostReachItems>(AJsonString)
end;

end.

