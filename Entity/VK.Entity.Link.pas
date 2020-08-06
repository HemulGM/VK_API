unit VK.Entity.Link;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Photo, VK.Types;

type
  TVkLinkStatus = class
  private
    FLink: string;
    FStatus: string;
    function GetStatus: TVkLinkStatusType;
    procedure SetStatus(const Value: TVkLinkStatusType);
  public
    property Link: string read FLink write FLink;
    property Status: TVkLinkStatusType read GetStatus write SetStatus;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkStatus;
  end;

  TVkLinkAction = class
  private
    FType: string;
    FUrl: string;
  public
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkAction;
  end;

  TVkLinkButton = class
  private
    FAction: TVkLinkAction;
    FTitle: string;
  public
    property Action: TVkLinkAction read FAction write FAction;
    property Title: string read FTitle write FTitle;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkButton;
  end;

  TVkLink = class
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
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLink;
  end;

  TVkShortLink = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkShortLink;
  end;

  TVkShortLinks = class
  private
    FCount: Integer;
    FItems: TArray<TVkShortLink>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkShortLink> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkShortLinks;
  end;

  TVkLinkViewsCity = class
  private
    FCity_id: Integer;
    FViews: Integer;
  public
    property CityId: Integer read FCity_id write FCity_id;
    property Views: Integer read FViews write FViews;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkViewsCity;
  end;

  TVkLinkViewsCountries = class
  private
    FCountry_id: Integer;
    FViews: Integer;
  public
    property CountryId: Integer read FCountry_id write FCountry_id;
    property Views: Integer read FViews write FViews;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkViewsCountries;
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkSexAge;
  end;

  TVkLinkStats = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkStats;
  end;

  TVkLinkStates = class
  private
    FKey: string;
    FStats: TArray<TVkLinkStats>;
  public
    property Key: string read FKey write FKey;
    property Stats: TArray<TVkLinkStats> read FStats write FStats;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLinkStates;
  end;

implementation

{TVkLinkAction}

function TVkLinkAction.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLinkAction.FromJsonString(AJsonString: string): TVkLinkAction;
begin
  result := TJson.JsonToObject<TVkLinkAction>(AJsonString)
end;

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

function TVkLinkButton.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLinkButton.FromJsonString(AJsonString: string): TVkLinkButton;
begin
  result := TJson.JsonToObject<TVkLinkButton>(AJsonString)
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

function TVkLink.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLink.FromJsonString(AJsonString: string): TVkLink;
begin
  result := TJson.JsonToObject<TVkLink>(AJsonString)
end;

{ TVkLinkStatus }

class function TVkLinkStatus.FromJsonString(AJsonString: string): TVkLinkStatus;
begin
  result := TJson.JsonToObject<TVkLinkStatus>(AJsonString)
end;

function TVkLinkStatus.GetStatus: TVkLinkStatusType;
begin
  Result := TVkLinkStatusType.FromString(FStatus);
end;

procedure TVkLinkStatus.SetStatus(const Value: TVkLinkStatusType);
begin
  FStatus := Value.ToString;
end;

function TVkLinkStatus.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkShortLink }

class function TVkShortLink.FromJsonString(AJsonString: string): TVkShortLink;
begin
  result := TJson.JsonToObject<TVkShortLink>(AJsonString)
end;

function TVkShortLink.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkShortLinks }

destructor TVkShortLinks.Destroy;
var
  i: Integer;
begin
  for i := Low(FItems) to High(FItems) do
    FItems[i].Free;
  inherited;
end;

class function TVkShortLinks.FromJsonString(AJsonString: string): TVkShortLinks;
begin
  result := TJson.JsonToObject<TVkShortLinks>(AJsonString)
end;

function TVkShortLinks.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkLinkViewsCity }

class function TVkLinkViewsCity.FromJsonString(AJsonString: string): TVkLinkViewsCity;
begin
  result := TJson.JsonToObject<TVkLinkViewsCity>(AJsonString)
end;

function TVkLinkViewsCity.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkLinkViewsCountries }

class function TVkLinkViewsCountries.FromJsonString(AJsonString: string): TVkLinkViewsCountries;
begin
  result := TJson.JsonToObject<TVkLinkViewsCountries>(AJsonString)
end;

function TVkLinkViewsCountries.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkLinkSexAge }

class function TVkLinkSexAge.FromJsonString(AJsonString: string): TVkLinkSexAge;
begin
  result := TJson.JsonToObject<TVkLinkSexAge>(AJsonString)
end;

function TVkLinkSexAge.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkLinkStats }

destructor TVkLinkStats.Destroy;
var
  i: Integer;
begin
  for i := Low(FCities) to High(FCities) do
    FCities[i].Free;
  for i := Low(FCountries) to High(FCountries) do
    FCountries[i].Free;
  for i := Low(FSex_age) to High(FSex_age) do
    FSex_age[i].Free;
  inherited;
end;

class function TVkLinkStats.FromJsonString(AJsonString: string): TVkLinkStats;
begin
  result := TJson.JsonToObject<TVkLinkStats>(AJsonString)
end;

function TVkLinkStats.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkLinkStates }

destructor TVkLinkStates.Destroy;
var
  i: Integer;
begin
  for i := Low(FStats) to High(FStats) do
    FStats[i].Free;
  inherited;
end;

class function TVkLinkStates.FromJsonString(AJsonString: string): TVkLinkStates;
begin
  result := TJson.JsonToObject<TVkLinkStates>(AJsonString)
end;

function TVkLinkStates.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

