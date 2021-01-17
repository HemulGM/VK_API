unit VK.Entity.ProfileInfo;

interface

uses
  Generics.Collections, Rest.Json, REST.JsonReflect, VK.Wrap.Interceptors, VK.Entity.Common,
  VK.Entity.Database.Countries, VK.Entity.Database.Cities, VK.Types, VK.Entity.Profile;

type
  /// <summary>
  /// rsProcessing – заявка рассматривается;
  /// rsDeclined – заявка отклонена;
  /// rsResponse – общий ответ по статусу обработки заявки;
  /// rsResponseWithLink – общий ответ по статусу обработки заявки, содержащий ссылку в поле link;
  /// </summary>
  TVkNameRequestStatus = (rsProcessing, rsDeclined, rsResponse, rsResponseWithLink);

  TVkNameRequestStatusHelper = record helper for TVkNameRequestStatus
    class function Create(const Value: string): TVkNameRequestStatus; static;
    function ToString: string; inline;
  end;

  TVkNameRequest = class(TVkObject)
  private
    FLast_name: string;
    FFirst_name: string;
    FStatus: TVkNameRequestStatus;
  public
    /// <summary>
    /// статус заявки. Возможные значения:
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

{ TVkNameRequestStatusHelper }

class function TVkNameRequestStatusHelper.Create(const Value: string): TVkNameRequestStatus;
begin
  case IndexStr(Value, ['processing', 'declined', 'response', 'response_with_link']) of
    0:
      Exit(rsProcessing);
    1:
      Exit(rsDeclined);
    2:
      Exit(rsResponse);
    3:
      Exit(rsResponseWithLink);
  else
    Result := rsProcessing;
  end;
end;

function TVkNameRequestStatusHelper.ToString: string;
begin
  case Self of
    rsProcessing:
      Exit('processing');
    rsDeclined:
      Exit('declined');
    rsResponse:
      Exit('response');
    rsResponseWithLink:
      Exit('response_with_link');
  else
    Result := '';
  end;
end;

end.

