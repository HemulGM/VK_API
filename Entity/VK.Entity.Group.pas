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
    FId: Extended;
    FIs_admin: Extended;
    FIs_advertiser: Extended;
    FIs_closed: Extended;
    FIs_member: Extended;
    FName: string;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FScreen_name: string;
    FType: string;
    FTrending: Extended;
    FMembers_count: Extended;
    FVerified: Extended;
    FCountry: TVkCountry;
    FMember_status: Extended;
    FCity: TVkCity;
    FActivity: string;
  public
    property AdminLevel: Extended read FAdmin_level write FAdmin_level;
    property Id: Extended read FId write FId;
    property IsAdmin: Extended read FIs_admin write FIs_admin;
    property IsAdvertiser: Extended read FIs_advertiser write FIs_advertiser;
    property IsClosed: Extended read FIs_closed write FIs_closed;
    property IsMember: Extended read FIs_member write FIs_member;
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
    property membersCount: Extended read FMembers_count write FMembers_count;
    property Trending: Extended read FTrending write FTrending;
    property Verified: Extended read FVerified write FVerified;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroup;
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

end.

