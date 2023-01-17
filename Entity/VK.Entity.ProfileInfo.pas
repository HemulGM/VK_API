unit VK.Entity.ProfileInfo;

interface

uses
  Generics.Collections, Rest.Json, REST.JsonReflect, VK.Wrap.Interceptors,
  VK.Entity.Common, VK.Entity.Database.Countries, VK.Entity.Database.Cities,
  VK.Types, VK.Entity.Profile;

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
    /// фамилия пользователя, указанная в заявке.
    /// </summary>
    property LastName: string read FLast_name write FLast_name;
  end;

  TVkProfileInfo = class(TVkProfile)
  private
    FRelation_requests: TArray<TVkProfile>;
    FRelation_pending: Boolean;
    FName_request: TVkNameRequest;
  public
    property RelationRequests: TArray<TVkProfile> read FRelation_requests write FRelation_requests;
    property RelationPending: Boolean read FRelation_pending write FRelation_pending;
    property NameRequest: TVkNameRequest read FName_request write FName_request;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkProfileInfo}

destructor TVkProfileInfo.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FRelation_requests);
  if Assigned(FName_request) then
    FName_request.Free;
  inherited;
end;

end.

