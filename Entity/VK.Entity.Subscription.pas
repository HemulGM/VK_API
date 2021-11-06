unit VK.Entity.Subscription;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkSubscription = class(TVkObject)
  private
    FIs_admin: Boolean;
    FIs_advertiser: Boolean;
    FIs_closed: Boolean;
    FIs_member: Boolean;
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
    //page
    property IsAdmin: Boolean read FIs_admin write FIs_admin;
    property IsAdvertiser: Boolean read FIs_advertiser write FIs_advertiser;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property IsMember: Boolean read FIs_member write FIs_member;
    property Name: string read FName write FName;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property ScreenName: string read FScreen_name write FScreen_name;
    // user
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property FirstName: string read FFirst_name write FFirst_name;
    property LastName: string read FLast_name write FLast_name;
    property &Type: string read FType write FType;
  end;

  TVkSubscriptions = TVkEntityList<TVkSubscription>;

implementation

end.

