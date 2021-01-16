unit VK.Entity.Link;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List, VK.Entity.Photo, VK.Types;

type
  TVkLinkStatus = class(TVkEntity)
  private
    FLink: string;
    FStatus: string;
    function GetStatus: TVkLinkStatusType;
    procedure SetStatus(const Value: TVkLinkStatusType);
  public
    property Link: string read FLink write FLink;
    property Status: TVkLinkStatusType read GetStatus write SetStatus;
  end;

  TVkLinkAction = class
  private
    FType: string;
    FUrl: string;
  public
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
  end;

  TVkLinkButton = class(TVkEntity)
  private
    FAction: TVkLinkAction;
    FTitle: string;
  public
    property Action: TVkLinkAction read FAction write FAction;
    property Title: string read FTitle write FTitle;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkLink = class(TVkEntity)
  private
    FButton: TVkLinkButton;
    FCaption: string;
    FDescription: string;
    FPhoto: TVkPhoto;
    FTitle: string;
    FUrl: string;
    FText: string;
  public
    property Button: TVkLinkButton read FButton write FButton;
    property Caption: string read FCaption write FCaption;
    property Description: string read FDescription write FDescription;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Title: string read FTitle write FTitle;
    property Text: string read FText write FText;
    property Url: string read FUrl write FUrl;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkShortLink = class(TVkEntity)
  private
    FKey: string;
    FShort_url: string;
    FTimestamp: Int64;
    FUrl: string;
    FViews: Integer;
    FAccess_key: string;
  public
    property Key: string read FKey write FKey;
    property AccessKey: string read FAccess_key write FAccess_key;
    property ShortUrl: string read FShort_url write FShort_url;
    property Timestamp: Int64 read FTimestamp write FTimestamp;
    property Url: string read FUrl write FUrl;
    property Views: Integer read FViews write FViews;
  end;

  TVkShortLinks = TVkEntityList<TVkShortLink>;

  TVkLinkViewsCity = class
  private
    FCity_id: Integer;
    FViews: Integer;
  public
    property CityId: Integer read FCity_id write FCity_id;
    property Views: Integer read FViews write FViews;
  end;

  TVkLinkViewsCountries = class
  private
    FCountry_id: Integer;
    FViews: Integer;
  public
    property CountryId: Integer read FCountry_id write FCountry_id;
    property Views: Integer read FViews write FViews;
  end;

  TVkLinkSexAge = class
  private
    FAge_range: string;
    FFemale: Integer;
    FMale: Integer;
  public
    property AgeRange: string read FAge_range write FAge_range;
    property Female: Integer read FFemale write FFemale;
    property Male: Integer read FMale write FMale;
  end;

  TVkLinkStats = class(TVkEntity)
  private
    FCities: TArray<TVkLinkViewsCity>;
    FCountries: TArray<TVkLinkViewsCountries>;
    FSex_age: TArray<TVkLinkSexAge>;
    FTimestamp: Int64;
    FViews: Integer;
  public
    property Cities: TArray<TVkLinkViewsCity> read FCities write FCities;
    property Countries: TArray<TVkLinkViewsCountries> read FCountries write FCountries;
    property SexAge: TArray<TVkLinkSexAge> read FSex_age write FSex_age;
    property Timestamp: Int64 read FTimestamp write FTimestamp;
    property Views: Integer read FViews write FViews;
    destructor Destroy; override;
  end;

  TVkLinkStates = class
  private
    FKey: string;
    FStats: TArray<TVkLinkStats>;
  public
    property Key: string read FKey write FKey;
    property Stats: TArray<TVkLinkStats> read FStats write FStats;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils, System.StrUtils;

{TButtonClass}

constructor TVkLinkButton.Create;
begin
  inherited;
  FAction := TVkLinkAction.Create();
end;

destructor TVkLinkButton.Destroy;
begin
  FAction.Free;
  inherited;
end;

{TLinkClass}

constructor TVkLink.Create;
begin
  inherited;
  FPhoto := TVkPhoto.Create();
  FButton := TVkLinkButton.Create();
end;

destructor TVkLink.Destroy;
begin
  FPhoto.Free;
  FButton.Free;
  inherited;
end;

{ TVkLinkStatus }

function TVkLinkStatus.GetStatus: TVkLinkStatusType;
begin
  Result := TVkLinkStatusType.FromString(FStatus);
end;

procedure TVkLinkStatus.SetStatus(const Value: TVkLinkStatusType);
begin
  FStatus := Value.ToString;
end;

{ TVkLinkStats }

destructor TVkLinkStats.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkLinkViewsCity>(FCities);
  TArrayHelp.FreeArrayOfObject<TVkLinkViewsCountries>(FCountries);
  TArrayHelp.FreeArrayOfObject<TVkLinkSexAge>(FSex_age);
  inherited;
end;

{ TVkLinkStates }

destructor TVkLinkStates.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkLinkStats>(FStats);
  inherited;
end;

end.

