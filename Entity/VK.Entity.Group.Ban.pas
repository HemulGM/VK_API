unit VK.Entity.Group.Ban;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Group, VK.Types;

type
  TVkGroupBanInfo = class
  private
    FAdmin_id: Integer;
    FComment: string;
    FComment_visible: Boolean;
    FDate: Int64;
    FEnd_date: Int64;
    FReason: Integer;
    function GetReason: TVkUserBlockReason;
    procedure SetReason(const Value: TVkUserBlockReason);
  public
    property AdminId: Integer read FAdmin_id write FAdmin_id;
    property Comment: string read FComment write FComment;
    property CommentVisible: Boolean read FComment_visible write FComment_visible;
    property Date: Int64 read FDate write FDate;
    property EndDate: Int64 read FEnd_date write FEnd_date;
    property Reason: TVkUserBlockReason read GetReason write SetReason;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupBanInfo;
  end;

  TVkGroupBan = class
  private
    FBan_info: TVkGroupBanInfo;
    FGroup: TVkGroup;
    FProfile: TVkProfile;
    FType: string;
  public
    property BanInfo: TVkGroupBanInfo read FBan_info write FBan_info;
    property Group: TVkGroup read FGroup write FGroup;
    property Profile: TVkProfile read FProfile write FProfile;
    property&Type: string read FType write FType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupBan;
  end;

  TVkGroupBans = class
  private
    FCount: Integer;
    FItems: TArray<TVkGroupBan>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkGroupBan> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGroupBans;
  end;

implementation

uses
  VK.CommonUtils;

{TVkGroupBanInfo}

function TVkGroupBanInfo.GetReason: TVkUserBlockReason;
begin
  try
    Result := TVkUserBlockReason(FReason);
  except
    Result := TVkUserBlockReason.brOther;
  end;
end;

procedure TVkGroupBanInfo.SetReason(const Value: TVkUserBlockReason);
begin
  FReason := Ord(Value);
end;

function TVkGroupBanInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupBanInfo.FromJsonString(AJsonString: string): TVkGroupBanInfo;
begin
  result := TJson.JsonToObject<TVkGroupBanInfo>(AJsonString)
end;

{TVkGroupBan}

constructor TVkGroupBan.Create;
begin
  inherited;
  FProfile := TVkProfile.Create();
  FGroup := TVkGroup.Create();
  FBan_info := TVkGroupBanInfo.Create();
end;

destructor TVkGroupBan.Destroy;
begin
  FProfile.Free;
  FGroup.Free;
  FBan_info.Free;
  inherited;
end;

function TVkGroupBan.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupBan.FromJsonString(AJsonString: string): TVkGroupBan;
begin
  result := TJson.JsonToObject<TVkGroupBan>(AJsonString)
end;

{TVkGroupBans}

destructor TVkGroupBans.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroupBan>(FItems);
  inherited;
end;

function TVkGroupBans.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGroupBans.FromJsonString(AJsonString: string): TVkGroupBans;
begin
  result := TJson.JsonToObject<TVkGroupBans>(AJsonString)
end;

end.

