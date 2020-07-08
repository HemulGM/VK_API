unit VK.Entity.Group;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkGroupStatusType = (gsNone, gsOnline, gsAnswerMark);

  TVkGroupStatus = class
  private
    FMinutes: Integer;
    FStatus: string;
    function GetStatus: TVkGroupStatusType;
  public
    /// <summary>
    /// ќценка времени ответа в минутах (дл€ status = answer_mark)
    /// </summary>
    property Minutes: Integer read FMinutes write FMinutes;
    /// <summary>
    /// Cтатус сообщества
    /// </summary>
    /// <returns>none Ч сообщество не онлайн; online Ч сообщество онлайн (отвечает мгновенно); answer_mark Ч сообщество отвечает быстро.</returns>
    property StatusStr: string read FStatus write FStatus;
    /// <summary>
    /// Cтатус сообщества
    /// </summary>
    property Status: TVkGroupStatusType read GetStatus;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupStatus;
  end;

  TVkGroup = class
  private
    FAdmin_level: Extended;
    FId: Int64;
    FIs_admin: Integer;
    FIs_advertiser: Integer;
    FIs_closed: Integer;
    FIs_member: Integer;
    FName: string;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FScreen_name: string;
    FType: string;
    FTrending: Extended;
    FMembers_count: Extended;
    FVerified: Integer;
    FCountry: TVkCountry;
    FMember_status: Extended;
    FCity: TVkCity;
    FActivity: string;
    function GetIs_admin: Boolean;
    procedure SetIs_admin(const Value: Boolean);
    function GetIs_advertiser: Boolean;
    function GetIs_closed: Boolean;
    function GetIs_member: Boolean;
    procedure SetIs_advertiser(const Value: Boolean);
    procedure SetIs_closed(const Value: Boolean);
    procedure SetIs_member(const Value: Boolean);
    function GetVerified: Boolean;
    procedure SetVerified(const Value: Boolean);
  public
    property AdminLevel: Extended read FAdmin_level write FAdmin_level;
    property Id: Int64 read FId write FId;
    property IsAdmin: Boolean read GetIs_admin write SetIs_admin;
    property IsAdvertiser: Boolean read GetIs_advertiser write SetIs_advertiser;
    property IsClosed: Boolean read GetIs_closed write SetIs_closed;
    property IsMember: Boolean read GetIs_member write SetIs_member;
    property Name: string read FName write FName;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property ScreenName: string read FScreen_name write FScreen_name;
    property&Type: string read FType write FType;
    property Activity: string read FActivity write FActivity;
    property City: TVkCity read FCity write FCity;
    property Country: TVkCountry read FCountry write FCountry;
    property MemberStatus: Extended read FMember_status write FMember_status;
    property MembersCount: Extended read FMembers_count write FMembers_count;
    property Trending: Extended read FTrending write FTrending;
    property Verified: Boolean read GetVerified write SetVerified;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroup;
  end;

  TVkGroups = class
  private
    FItems: TArray<TVkGroup>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkGroup> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Users: TVkGroups);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroups;
  end;

function FindGroup(Id: Integer; List: TArray<TVkGroup>): Integer;

implementation

function FindGroup(Id: Integer; List: TArray<TVkGroup>): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(List) to High(List) do
    if List[i].Id = Abs(Id) then
      Exit(i);
end;

{TVkGroup}

function TVkGroup.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

constructor TVkGroup.Create;
begin
  inherited;
end;

destructor TVkGroup.Destroy;
begin
  if Assigned(FCity) then
    FCity.Free;
  if Assigned(FCountry) then
    FCountry.Free;
  inherited;
end;

class function TVkGroup.FromJsonString(AJsonString: string): TVkGroup;
begin
  result := TJson.JsonToObject<TVkGroup>(AJsonString)
end;

function TVkGroup.GetIs_admin: Boolean;
begin
  Result := FIs_admin = 1;
end;

function TVkGroup.GetIs_advertiser: Boolean;
begin
  Result := FIs_advertiser = 1;
end;

function TVkGroup.GetIs_closed: Boolean;
begin
  Result := FIs_closed = 1;
end;

function TVkGroup.GetIs_member: Boolean;
begin
  Result := FIs_member = 1;
end;

function TVkGroup.GetVerified: Boolean;
begin
  Result := FVerified = 1;
end;

procedure TVkGroup.SetIs_admin(const Value: Boolean);
begin
  if Value then
    FIs_admin := 1
  else
    FIs_admin := 0;
end;

procedure TVkGroup.SetIs_advertiser(const Value: Boolean);
begin
  if Value then
    FIs_advertiser := 1
  else
    FIs_advertiser := 0;
end;

procedure TVkGroup.SetIs_closed(const Value: Boolean);
begin
  if Value then
    FIs_closed := 1
  else
    FIs_closed := 0;
end;

procedure TVkGroup.SetIs_member(const Value: Boolean);
begin
  if Value then
    FIs_member := 1
  else
    FIs_member := 0;
end;

procedure TVkGroup.SetVerified(const Value: Boolean);
begin
  if Value then
    FVerified := 1
  else
    FVerified := 0;
end;

{ TVkGroupStatus }

function TVkGroupStatus.GetStatus: TVkGroupStatusType;
begin
  if FStatus = 'none' then
    Exit(gsNone);
  if FStatus = 'online' then
    Exit(gsOnline);
  if FStatus = 'answer_mark' then
    Exit(gsAnswermark);
  Result := gsNone;
end;

function TVkGroupStatus.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupStatus.FromJsonString(AJsonString: string): TVkGroupStatus;
begin
  result := TJson.JsonToObject<TVkGroupStatus>(AJsonString)
end;

{ TVkGroups }

procedure TVkGroups.Append(Users: TVkGroups);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(Users.Items));
  Move(Users.Items[0], FItems[OldLen], Length(Users.Items) * SizeOf(TVkGroup));
end;

constructor TVkGroups.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkGroups.Destroy;
var
  LItemsItem: TVkGroup;
begin
  if not FSaveObjects then
  begin
    for LItemsItem in FItems do
      LItemsItem.Free;
  end;

  inherited;
end;

class function TVkGroups.FromJsonString(AJsonString: string): TVkGroups;
begin
  result := TJson.JsonToObject<TVkGroups>(AJsonString);
end;

procedure TVkGroups.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

function TVkGroups.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

