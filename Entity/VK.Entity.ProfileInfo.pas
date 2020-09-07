unit VK.Entity.ProfileInfo;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Database.Countries;

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
    property BirthDate: string read FBdate write FBdate;
    property BirthDateVisibility: Extended read FBdate_visibility write FBdate_visibility;
    property Country: TVkCountry read FCountry write FCountry;
    property FirstName: string read FFirst_name write FFirst_name;
    property HomeTown: string read FHome_town write FHome_town;
    property LastName: string read FLast_name write FLast_name;
    property Phone: string read FPhone write FPhone;
    property Relation: Extended read FRelation write FRelation;
    property RelationPartner: TVkRelationPartner read FRelation_partner write FRelation_partner;
    property RelationRequests: TArray<TVkRelationRequests> read FRelation_requests write FRelation_requests;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Sex: Extended read FSex write FSex;
    property Status: string read FStatus write FStatus;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkProfileInfo;
  end;

implementation

uses
  VK.CommonUtils;

{TVkProfileInfo}

constructor TVkProfileInfo.Create;
begin
  inherited;
  FCountry := TVkCountry.Create();
  FRelation_partner := TVkRelationPartner.Create();
end;

destructor TVkProfileInfo.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkRelationRequests>(FRelation_requests);
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

