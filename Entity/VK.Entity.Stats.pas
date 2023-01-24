unit VK.Entity.Stats;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkStatBasic = class(TVkCounterEntity)
  private
    FValue: string;
  public
    property Value: string read FValue write FValue;
  end;

  TVkStatCountries = class(TVkCounterEntity)
  private
    FCode: string;
    FName: string;
    FValue: Integer;
  public
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
  end;

  TVkStatCities = class(TVkCounterEntity)
  private
    FName: string;
    FValue: Integer;
  public
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
    destructor Destroy; override;
  end;

  TVkStatItems = TVkEntityList<TVkStatItem>;

  TVkStatPostReachItem = class(TVkEntity)
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
    /// <summary>
    /// Охват подписчиков
    /// </summary>
    property ReachSubscribers: Integer read FReach_subscribers write FReach_subscribers;
    /// <summary>
    /// Количество скрывших запись
    /// </summary>
    property Hide: Integer read FHide write FHide;
    /// <summary>
    /// Вступления в сообщество
    /// </summary>
    property JoinGroup: Integer read FJoin_group write FJoin_group;
    /// <summary>
    /// Переходы по ссылке
    /// </summary>
    property Links: Integer read FLinks write FLinks;
    /// <summary>
    /// Суммарный охват
    /// </summary>
    property ReachTotal: Integer read FReach_total write FReach_total;
    /// <summary>
    /// Рекламный охват (если запись продвигалась с помощью таргетированной рекламы)
    /// </summary>
    property ReachAds: Integer read FReach_ads write FReach_ads;
    /// <summary>
    /// Виральный охват (если запись продвигалась с помощью таргетированной рекламы)
    /// </summary>
    property ReachViral: Integer read FReach_viral write FReach_viral;
    /// <summary>
    /// Количество жалоб на запись
    /// </summary>
    property Report: Integer read FReport write FReport;
    /// <summary>
    /// Переходы в сообщество
    /// </summary>
    property ToGroup: Integer read FTo_group write FTo_group;
    /// <summary>
    /// Количество отписавшихся участников
    /// </summary>
    property Unsubscribe: Integer read FUnsubscribe write FUnsubscribe;
  end;

  TVkStatPostReachItems = TVkEntityList<TVkStatPostReachItem>;

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

destructor TVkStatItem.Destroy;
begin
  if Assigned(FReach) then
    FReach.Free;
  if Assigned(FVisitors) then
    FVisitors.Free;
  inherited;
end;

end.

