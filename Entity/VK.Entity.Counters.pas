unit VK.Entity.Counters;

interface

uses
  Generics.Collections, Rest.Json;

type
  TCountersClass = class
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
    property app_requests: Extended read FApp_requests write FApp_requests;
    property events: Extended read FEvents write FEvents;
    property friends: Extended read FFriends write FFriends;
    property friends_recommendations: Extended read FFriends_recommendations write FFriends_recommendations;
    property gifts: Extended read FGifts write FGifts;
    property groups: Extended read FGroups write FGroups;
    property menu_discover_badge: Extended read FMenu_discover_badge write FMenu_discover_badge;
    property messages: Extended read FMessages write FMessages;
    property notes: Extended read FNotes write FNotes;
    property notifications: Extended read FNotifications write FNotifications;
    property photos: Extended read FPhotos write FPhotos;
    property sdk: Extended read FSdk write FSdk;
    property videos: Extended read FVideos write FVideos;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TCountersClass;
  end;

implementation

{TCountersClass}

function TCountersClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TCountersClass.FromJsonString(AJsonString: string): TCountersClass;
begin
  result := TJson.JsonToObject<TCountersClass>(AJsonString)
end;

end.

