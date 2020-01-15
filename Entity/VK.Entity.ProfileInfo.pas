unit VK.Entity.ProfileInfo;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkProfileInfo = class
  private
    FBdate: string;
    FBdate_visibility: Extended;
    FCountry: TVkCountry;
    FFirst_name: string;
    FHome_town: string;
    FLast_name: string;
    FPhone: string;
    FRelation: Extended;
    FRelation_partner: TVkRelationPartner;
    FRelation_requests: TArray<TVkRelationRequests>;
    FScreen_name: string;
    FSex: Extended;
    FStatus: string;
  public
    property bdate: string read FBdate write FBdate;
    property bdate_visibility: Extended read FBdate_visibility write FBdate_visibility;
    property country: TVkCountry read FCountry write FCountry;
    property first_name: string read FFirst_name write FFirst_name;
    property home_town: string read FHome_town write FHome_town;
    property last_name: string read FLast_name write FLast_name;
    property phone: string read FPhone write FPhone;
    property relation: Extended read FRelation write FRelation;
    property relation_partner: TVkRelationPartner read FRelation_partner write FRelation_partner;
    property relation_requests: TArray<TVkRelationRequests> read FRelation_requests write FRelation_requests;
    property screen_name: string read FScreen_name write FScreen_name;
    property sex: Extended read FSex write FSex;
    property status: string read FStatus write FStatus;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProfileInfo;
  end;

implementation

{TProfileInfoClass}

constructor TVkProfileInfo.Create;
begin
  inherited;
  FCountry := TVkCountry.Create();
  FRelation_partner := TVkRelationPartner.Create();
end;

destructor TVkProfileInfo.Destroy;
var
  Lrelation_requestsItem: TVkRelationRequests;
begin

  for Lrelation_requestsItem in FRelation_requests do
    Lrelation_requestsItem.Free;

  FCountry.Free;
  FRelation_partner.Free;
  inherited;
end;

function TVkProfileInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkProfileInfo.FromJsonString(AJsonString: string): TVkProfileInfo;
begin
  result := TJson.JsonToObject<TVkProfileInfo>(AJsonString)
end;

end.

