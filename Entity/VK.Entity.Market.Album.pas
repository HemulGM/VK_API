unit VK.Entity.Market.Album;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Photo, VK.Entity.Common,
  VK.Entity.Common.List;

type
  TVkMarketAlbum = class(TVkObject)
  private
    FCount: Integer;
    FOwner_id: Integer;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FUpdated_time: TDateTime;
    FPhoto: TVkPhoto;
  public
    property Count: Integer read FCount write FCount;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Title: string read FTitle write FTitle;
    property UpdatedTime: TDateTime read FUpdated_time write FUpdated_time;
  end;

  TVkMarketAlbums = TVkEntityList<TVkMarketAlbum>;

implementation

end.

