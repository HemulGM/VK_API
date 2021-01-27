unit VK.Entity.Group.Ban;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json, VK.Entity.Profile, VK.Entity.Group,
  VK.Types, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkGroupBanInfo = class
  private
    FAdmin_id: Integer;
    FComment: string;
    FComment_visible: Boolean;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FEnd_date: TDateTime;
    FReason: Integer;
    function GetReason: TVkUserBlockReason;
    procedure SetReason(const Value: TVkUserBlockReason);
  public
    property AdminId: Integer read FAdmin_id write FAdmin_id;
    property Comment: string read FComment write FComment;
    property CommentVisible: Boolean read FComment_visible write FComment_visible;
    property Date: TDateTime read FDate write FDate;
    property EndDate: TDateTime read FEnd_date write FEnd_date;
    property Reason: TVkUserBlockReason read GetReason write SetReason;
  end;

  TVkGroupBan = class(TVkEntity)
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
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkGroupBans = TVkEntityList<TVkGroupBan>;

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

end.

