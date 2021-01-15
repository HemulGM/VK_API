unit VK.Entity.Stats;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkStatBasic = class
  private
    FCount: Integer;
    FValue: string;
  public
    property Count: Integer read FCount write FCount;
    property Value: string read FValue write FValue;
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
  end;

  TVkStatItem = class(TVkEntity)
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
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkStatItems = class(TVkEntity)
  private
    FItems: TArray<TVkStatItem>;
  public
    property Items: TArray<TVkStatItem> read FItems write FItems;
    destructor Destroy; override;
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
  end;

  TVkStatPostReachItems = class(TVkEntity)
  private
    FItems: TArray<TVkStatPostReachItem>;
  public
    property Items: TArray<TVkStatPostReachItem> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

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

{TVkStatItems}

destructor TVkStatItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStatItem>(FItems);
  inherited;
end;

{TVkStatPostReachItems}

destructor TVkStatPostReachItems.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStatPostReachItem>(FItems);
  inherited;
end;

end.

