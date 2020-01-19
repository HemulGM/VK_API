unit VK.Entity.Common;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkRect = class
  private
    FX: Extended;
    FX2: Extended;
    FY: Extended;
    FY2: Extended;
  public
    property x: Extended read FX write FX;
    property x2: Extended read FX2 write FX2;
    property y: Extended read FY write FY;
    property y2: Extended read FY2 write FY2;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRect;
  end;

  TVkCrop = class(TVkRect);

  TVkTags = class
  private
    FCount: Extended;
  public
    property count: Extended read FCount write FCount;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkTags;
  end;

  TVkPostSource = class
  private
    FData: string;
    FPlatform: string;
    FType: string;
    FUrl: string;
  public
    property data: string read FData write FData;
    property platform: string read FPlatform write FPlatform;
    property&type: string read FType write FType;
    property url: string read FUrl write FUrl;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPostSource;
  end;

  TVkCommentsInfo = class
  private
    FCan_post: Extended;
    FCount: Extended;
    FGroups_can_post: Boolean;
  public
    property can_post: Extended read FCan_post write FCan_post;
    property count: Extended read FCount write FCount;
    property groups_can_post: Boolean read FGroups_can_post write FGroups_can_post;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCommentsInfo;
  end;

  TVkRepostsInfo = class
  private
    FCount: Extended;
    FUser_reposted: Extended;
  public
    property count: Extended read FCount write FCount;
    property user_reposted: Extended read FUser_reposted write FUser_reposted;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRepostsInfo;
  end;

  TVkViewsInfo = class
  private
    FCount: Extended;
  public
    property count: Extended read FCount write FCount;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkViewsInfo;
  end;

  TVkLikesInfo = class
  private
    FCan_like: Extended;
    FCan_publish: Extended;
    FCount: Extended;
    FUser_likes: Extended;
  public
    property can_like: Extended read FCan_like write FCan_like;
    property can_publish: Extended read FCan_publish write FCan_publish;
    property count: Extended read FCount write FCount;
    property user_likes: Extended read FUser_likes write FUser_likes;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLikesInfo;
  end;

  TVkSizes = class
  private
    FHeight: Extended;
    FType: string;
    FUrl: string;
    FWidth: Extended;
  public
    property height: Extended read FHeight write FHeight;
    property&type: string read FType write FType;
    property url: string read FUrl write FUrl;
    property width: Extended read FWidth write FWidth;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSizes;
  end;

  TVkRelationPartner = class
  private
    FCan_access_closed: Boolean;
    FFirst_name: string;
    FId: Extended;
    FIs_closed: Boolean;
    FLast_name: string;
  public
    property can_access_closed: Boolean read FCan_access_closed write FCan_access_closed;
    property first_name: string read FFirst_name write FFirst_name;
    property id: Extended read FId write FId;
    property is_closed: Boolean read FIs_closed write FIs_closed;
    property last_name: string read FLast_name write FLast_name;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRelationPartner;
  end;

  TVkRelationRequests = class
  private
    FCan_access_closed: Boolean;
    FFirst_name: string;
    FId: Extended;
    FIs_closed: Boolean;
    FLast_name: string;
  public
    property can_access_closed: Boolean read FCan_access_closed write FCan_access_closed;
    property first_name: string read FFirst_name write FFirst_name;
    property id: Extended read FId write FId;
    property is_closed: Boolean read FIs_closed write FIs_closed;
    property last_name: string read FLast_name write FLast_name;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRelationRequests;
  end;

  TVkCountry = class
  private
    FId: Extended;
    FTitle: string;
  public
    property id: Extended read FId write FId;
    property title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCountry;
  end;

  TVkPlace = class
  private
    FCity: string; // Ч название города;
    FCountry: string; // Ч название страны;
    FTitle: string; // Ч название места (если назначено);
    FId: integer; // Ч идентификатор места (если назначено);
    FLatitude: Extended; // Ч географическа€ широта;
    FLongitude: Extended; // Ч географическа€ долгота;
    FCreated: integer; // Ч дата создани€ (если назначено);
    FIcon: string; // Ч URL изображени€-иконки;
  public
    property city: string read FCity write FCity;
    property country: string read FCountry write FCountry;
    property title: string read FTitle write FTitle;
    property id: integer read FId write FId;
    property latitude: Extended read FLatitude write FLatitude;
    property longitude: Extended read FLongitude write FLongitude;
    property created: integer read FCreated write FCreated;
    property icon: string read FIcon write FIcon;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPlace;
  end;

  TVkCoordinates = class
  private
    FLatitude: Extended;
    FLongitude: Extended;
  public
    property latitude: Extended read FLatitude write FLatitude;
    property longitude: Extended read FLongitude write FLongitude;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCoordinates;
  end;

  TVkGeo = class
  private
    FCoordinates: TVkCoordinates;
    FPlace: TVkPlace;
    FType: string;
  public
    property coordinates: TVkCoordinates read FCoordinates write FCoordinates;
    property place: TVkPlace read FPlace write FPlace;
    property&type: string read FType write FType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkGeo;
  end;

  TVkChatPhoto = class
  private
    FPhoto_50: string;
    FPhoto_200: string;
    FPhoto_100: string;
  public
    property photo_50: string read FPhoto_50 write FPhoto_50; // Ч URL изображени€ 50x50px;
    property photo_100: string read FPhoto_100 write FPhoto_100; // Ч URL изображени€ 100x100px;
    property photo_200: string read FPhoto_200 write FPhoto_200; // Ч URL изображени€ 200x200px;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatPhoto;
  end;

implementation

{TCountryClass}

function TVkCountry.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCountry.FromJsonString(AJsonString: string): TVkCountry;
begin
  result := TJson.JsonToObject<TVkCountry>(AJsonString)
end;

{TRelationRequests}

function TVkRelationRequests.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRelationRequests.FromJsonString(AJsonString: string): TVkRelationRequests;
begin
  result := TJson.JsonToObject<TVkRelationRequests>(AJsonString)
end;

{TVkRelationPartner}

function TVkRelationPartner.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRelationPartner.FromJsonString(AJsonString: string): TVkRelationPartner;
begin
  result := TJson.JsonToObject<TVkRelationPartner>(AJsonString)
end;

{TRectClass}

function TVkRect.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRect.FromJsonString(AJsonString: string): TVkRect;
begin
  result := TJson.JsonToObject<TVkRect>(AJsonString)
end;

{TTagsClass}

function TVkTags.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkTags.FromJsonString(AJsonString: string): TVkTags;
begin
  result := TJson.JsonToObject<TVkTags>(AJsonString)
end;

{TCommentsClass}

function TVkCommentsInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCommentsInfo.FromJsonString(AJsonString: string): TVkCommentsInfo;
begin
  result := TJson.JsonToObject<TVkCommentsInfo>(AJsonString)
end;

{TPost_sourceClass}

function TVkPostSource.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPostSource.FromJsonString(AJsonString: string): TVkPostSource;
begin
  result := TJson.JsonToObject<TVkPostSource>(AJsonString)
end;

{TRepostsClass}

function TVkRepostsInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRepostsInfo.FromJsonString(AJsonString: string): TVkRepostsInfo;
begin
  result := TJson.JsonToObject<TVkRepostsInfo>(AJsonString)
end;

{TLikesClass}

function TVkLikesInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLikesInfo.FromJsonString(AJsonString: string): TVkLikesInfo;
begin
  result := TJson.JsonToObject<TVkLikesInfo>(AJsonString)
end;

{TSizesClass}

function TVkSizes.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSizes.FromJsonString(AJsonString: string): TVkSizes;
begin
  result := TJson.JsonToObject<TVkSizes>(AJsonString);
end;

{ TVkViewsInfo }

class function TVkViewsInfo.FromJsonString(AJsonString: string): TVkViewsInfo;
begin
  result := TJson.JsonToObject<TVkViewsInfo>(AJsonString);
end;

function TVkViewsInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkChatPhoto }

class function TVkChatPhoto.FromJsonString(AJsonString: string): TVkChatPhoto;
begin
  result := TJson.JsonToObject<TVkChatPhoto>(AJsonString)
end;

function TVkChatPhoto.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{TVkPlace}

function TVkPlace.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPlace.FromJsonString(AJsonString: string): TVkPlace;
begin
  result := TJson.JsonToObject<TVkPlace>(AJsonString)
end;

{TVkCoordinates}

function TVkCoordinates.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCoordinates.FromJsonString(AJsonString: string): TVkCoordinates;
begin
  result := TJson.JsonToObject<TVkCoordinates>(AJsonString)
end;

{TVkGeo}

constructor TVkGeo.Create;
begin
  inherited;
  FCoordinates := TVkCoordinates.Create();
  FPlace := TVkPlace.Create();
end;

destructor TVkGeo.Destroy;
begin
  FCoordinates.Free;
  FPlace.Free;
  inherited;
end;

function TVkGeo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkGeo.FromJsonString(AJsonString: string): TVkGeo;
begin
  result := TJson.JsonToObject<TVkGeo>(AJsonString)
end;

end.

