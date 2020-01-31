unit VK.Entity.Counters;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkCounters = class
  private
    FApp_requests: Extended;
    FEvents: Extended;
    FFriends: Extended;
    FFriends_recommendations: Extended;
    FGifts: Extended;
    FGroups: Extended;
    FMenu_discover_badge: Extended;
    FMessages: Extended;
    FNotes: Extended;
    FNotifications: Extended;
    FPhotos: Extended;
    FSdk: Extended;
    FVideos: Extended;
  public
    property AppRequests: Extended read FApp_requests write FApp_requests;
    property Events: Extended read FEvents write FEvents;
    property Friends: Extended read FFriends write FFriends;
    property FriendsRecommendations: Extended read FFriends_recommendations write FFriends_recommendations;
    property Gifts: Extended read FGifts write FGifts;
    property Groups: Extended read FGroups write FGroups;
    property MenuDiscoverBadge: Extended read FMenu_discover_badge write FMenu_discover_badge;
    property Messages: Extended read FMessages write FMessages;
    property Notes: Extended read FNotes write FNotes;
    property Notifications: Extended read FNotifications write FNotifications;
    property Photos: Extended read FPhotos write FPhotos;
    property SDK: Extended read FSdk write FSdk;
    property Videos: Extended read FVideos write FVideos;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCounters;
  end;

implementation

{TVkCounters}

function TVkCounters.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCounters.FromJsonString(AJsonString: string): TVkCounters;
begin
  result := TJson.JsonToObject<TVkCounters>(AJsonString)
end;

end.

