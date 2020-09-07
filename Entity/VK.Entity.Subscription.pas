unit VK.Entity.Subscription;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkSubscription = class
  private
    FId: Integer;
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
    FFirst_name: string;
    FLast_name: string;
    FCan_access_closed: Boolean;
  public
    //common
    property Id: Integer read FId write FId;
    //page
    property IsAdmin: Integer read FIs_admin write FIs_admin;
    property IsAdvertiser: Integer read FIs_advertiser write FIs_advertiser;
    property IsClosed: Integer read FIs_closed write FIs_closed;
    property IsMember: Integer read FIs_member write FIs_member;
    property Name: string read FName write FName;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property ScreenName: string read FScreen_name write FScreen_name;
    // user
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property FirstName: string read FFirst_name write FFirst_name;
    property LastName: string read FLast_name write FLast_name;
    property&Type: string read FType write FType;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSubscription;
  end;

  TVkSubscriptions = class
  private
    FItems: TArray<TVkSubscription>;
    FCount: Integer;
  public
    property Items: TArray<TVkSubscription> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSubscriptions;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkSubscription }

function TVkSubscription.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSubscription.FromJsonString(AJsonString: string): TVkSubscription;
begin
  result := TJson.JsonToObject<TVkSubscription>(AJsonString)
end;

{ TVkSubscriptions }

destructor TVkSubscriptions.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSubscription>(FItems);
  inherited;
end;

class function TVkSubscriptions.FromJsonString(AJsonString: string): TVkSubscriptions;
begin
  result := TJson.JsonToObject<TVkSubscriptions>(AJsonString)
end;

function TVkSubscriptions.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

