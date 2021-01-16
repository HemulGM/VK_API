unit VK.Entity.Counters;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkCounters = class(TVkEntity)
  private
    FApp_requests: Integer;
    FEvents: Integer;
    FFriends: Integer;
    FFriends_recommendations: Integer;
    FGifts: Integer;
    FGroups: Integer;
    FMenu_discover_badge: Integer;
    FMessages: Integer;
    FNotes: Integer;
    FNotifications: Integer;
    FPhotos: Integer;
    FSdk: Integer;
    FVideos: Integer;
  public
    property AppRequests: Integer read FApp_requests write FApp_requests;
    property Events: Integer read FEvents write FEvents;
    property Friends: Integer read FFriends write FFriends;
    property FriendsRecommendations: Integer read FFriends_recommendations write FFriends_recommendations;
    property Gifts: Integer read FGifts write FGifts;
    property Groups: Integer read FGroups write FGroups;
    property MenuDiscoverBadge: Integer read FMenu_discover_badge write FMenu_discover_badge;
    property Messages: Integer read FMessages write FMessages;
    property Notes: Integer read FNotes write FNotes;
    property Notifications: Integer read FNotifications write FNotifications;
    property Photos: Integer read FPhotos write FPhotos;
    property SDK: Integer read FSdk write FSdk;
    property Videos: Integer read FVideos write FVideos;
  end;

implementation

end.

