unit VK.Entity.ProfileInfo;

interface

uses
  Generics.Collections, Rest.Json, REST.JsonReflect, VK.Wrap.Interceptors, VK.Entity.Common,
  VK.Entity.Database.Countries, VK.Entity.Database.Cities, VK.Types, VK.Entity.Profile;

type
  TVkNameRequest = class(TVkObject)
  private
    FLast_name: string;
    FFirst_name: string;
    FStatus: TVkNameRequestStatus;
  public
    /// <summary>
    /// Cтатус заявки
    /// </summary>
    [JsonReflectAttribute(ctString, rtString, TNameRequestStatusInterceptor)]
    property Status: TVkNameRequestStatus read FStatus write FStatus;
    /// <summary>
    /// имя пользователя, указанное в заявке;
    /// </summary>
    property FirstName: string read FFirst_name write FFirst_name;
    /// <summary>
    ///  фамилия пользователя, указанная в заявке.
    /// </summary>
    property LastName: string read FLast_name write FLast_name;
  end;

  TVkProfileInfo = class(TVkEntity)
  private
    [JsonReflectAttribute(ctString, rtString, TStringDateTimeInterceptor)]
    FBdate: TDate;
    [JsonReflectAttribute(ctString, rtString, TBirthDateVisibilityInterceptor)]
    FBdate_visibility: TVkBirthDateVisibility;
    FCountry: TVkCountry;
    FFirst_name: string;
    FHome_town: string;
    FLast_name: string;
    FPhone: string;
    [JsonReflectAttribute(ctString, rtString, TRelationInterceptor)]
    FRelation: TVkRelation;
    FRelation_partner: TVkProfile;
    FRelation_requests: TArray<TVkProfile>;
    FScreen_name: string;
    [JsonReflectAttribute(ctString, rtString, TSexInterceptor)]
    FSex: TVkSex;
    FStatus: string;
    FCity: TVkCity;
    FMaiden_name: string;
    FRelation_pending: Boolean;
    FName_request: TVkNameRequest;
  public
    property BirthDate: TDate read FBdate write FBdate;
    property BirthDateVisibility: TVkBirthDateVisibility read FBdate_visibility write FBdate_visibility;
    property Country: TVkCountry read FCountry write FCountry;
    property City: TVkCity read FCity write FCity;
    property FirstName: string read FFirst_name write FFirst_name;
    property LastName: string read FLast_name write FLast_name;
    property HomeTown: string read FHome_town write FHome_town;
    property MaidenName: string read FMaiden_name write FMaiden_name;
    property Phone: string read FPhone write FPhone;
    property Relation: TVkRelation read FRelation write FRelation;
    property RelationPartner: TVkProfile read FRelation_partner write FRelation_partner;
    property RelationRequests: TArray<TVkProfile> read FRelation_requests write FRelation_requests;
    property ScreenName: string read FScreen_name write FScreen_name;
    property Sex: TVkSex read FSex write FSex;
    property Status: string read FStatus write FStatus;
    property RelationPending: Boolean read FRelation_pending write FRelation_pending;
    property NameRequest: TVkNameRequest read FName_request write FName_request;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils, System.StrUtils;

{TVkProfileInfo}

constructor TVkProfileInfo.Create;
begin
  inherited;
  FCountry := TVkCountry.Create();
  FCity := TVkCity.Create();
end;

destructor TVkProfileInfo.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FRelation_requests);
  FCountry.Free;
  FCity.Free;
  if Assigned(FRelation_partner) then
    FRelation_partner.Free;
  if Assigned(FName_request) then
    FName_request.Free;
  inherited;
end;

end.

