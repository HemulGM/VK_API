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
    property X: Extended read FX write FX;
    property X2: Extended read FX2 write FX2;
    property Y: Extended read FY write FY;
    property Y2: Extended read FY2 write FY2;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRect;
  end;

  TVkAddresses = class
  private
    FIs_enabled: Integer;
    function GetIs_enabled: Boolean;
    procedure SetIs_enabled(const Value: Boolean);
  public
    property IsEnabled: Boolean read GetIs_enabled write SetIs_enabled;
  end;

  TVkContact = class
  private
    FEmail: string;
    FPhone: string;
    FDesc: string;
    FUser_id: Integer;
  public
    property UserId: Integer read FUser_id write FUser_id;
    property Desc: string read FDesc write FDesc;
    property Phone: string read FPhone write FPhone;
    property Email: string read FEmail write FEmail;
  end;

  TVkBanInfo = class
  private
    FEnd_date: Integer;
    FComment: string;
  public
    property EndDate: Integer read FEnd_date write FEnd_date;
    property Comment: string read FComment write FComment;
  end;

  TVkCrop = class(TVkRect);

  TVkTags = class
  private
    FCount: Extended;
  public
    property Count: Extended read FCount write FCount;
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
    property Data: string read FData write FData;
    property platform: string read FPlatform write FPlatform;
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPostSource;
  end;

  TVkCommentsInfo = class
  private
    FCan_post: Extended;
    FCount: Extended;
    FGroups_can_post: Boolean;
  public
    property CanPost: Extended read FCan_post write FCan_post;
    property Count: Extended read FCount write FCount;
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCommentsInfo;
  end;

  TVkRepostsInfo = class
  private
    FCount: Extended;
    FUser_reposted: Extended;
  public
    property Count: Extended read FCount write FCount;
    property UserReposted: Extended read FUser_reposted write FUser_reposted;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRepostsInfo;
  end;

  TVkViewsInfo = class
  private
    FCount: Extended;
  public
    property Count: Extended read FCount write FCount;
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
    property CanLike: Extended read FCan_like write FCan_like;
    property CanPublish: Extended read FCan_publish write FCan_publish;
    property Count: Extended read FCount write FCount;
    property UserLikes: Extended read FUser_likes write FUser_likes;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkLikesInfo;
  end;

  {$REGION 'Fields Desc'}
  {
    Возможные значения поля type
    s — пропорциональная копия изображения с максимальной стороной 75px;
    m — пропорциональная копия изображения с максимальной стороной 130px;
    x — пропорциональная копия изображения с максимальной стороной 604px;
    o — если соотношение "ширина/высота" исходного изображения меньше или равно 3:2, то пропорциональная копия с максимальной шириной 130px. Если соотношение "ширина/высота" больше 3:2, то копия обрезанного слева изображения с максимальной шириной 130px и соотношением сторон 3:2.
    p — если соотношение "ширина/высота" исходного изображения меньше или равно 3:2, то пропорциональная копия с максимальной шириной 200px. Если соотношение "ширина/высота" больше 3:2, то копия обрезанного слева и справа изображения с максимальной шириной 200px и соотношением сторон 3:2.
    q — если соотношение "ширина/высота" исходного изображения меньше или равно 3:2, то пропорциональная копия с максимальной шириной 320px. Если соотношение "ширина/высота" больше 3:2, то копия обрезанного слева и справа изображения с максимальной шириной 320px и соотношением сторон 3:2.
    r — если соотношение "ширина/высота" исходного изображения меньше или равно 3:2, то пропорциональная копия с максимальной шириной 510px. Если соотношение "ширина/высота" больше 3:2, то копия обрезанного слева и справа изображения с максимальной шириной 510px и соотношением сторон 3:2
    y — пропорциональная копия изображения с максимальной стороной 807px;
    z — пропорциональная копия изображения с максимальным размером 1080x1024;
    w — пропорциональная копия изображения с максимальным размером 2560x2048px.
  }
  {$ENDREGION}
  /// <summary>
  /// Формат описания размеров фотографии
  /// https://vk.com/dev/photo_sizes
  /// </summary>
  TVkSize = class
  private
    FHeight: Extended;
    FType: string;
    FUrl: string;
    FWidth: Extended;
    FSrc: string;
  public
    property Height: Extended read FHeight write FHeight;
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
    property Src: string read FSrc write FSrc;
    property Width: Extended read FWidth write FWidth;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkSize;
  end;

  TVkSizes = TArray<TVkSize>;

  TVkSizesHelper = record helper for TVkSizes
  private
    function GetPrevSize(Value: string; Circular: Boolean): string;
    function GetNextSize(Value: string; Circular: Boolean): string;
  public
    function Get(Value: string): TVkSize;
    function GetSize(Value: string; Circular: Boolean = True): TVkSize;
    function GetSizeMin(Value: string = 's'; Circular: Boolean = False): TVkSize;
    function GetSizeMax(Value: string = 'w'; Circular: Boolean = False): TVkSize;
  end;

  TVkRelationPartner = class
  private
    FCan_access_closed: Boolean;
    FFirst_name: string;
    FId: Extended;
    FIs_closed: Boolean;
    FLast_name: string;
  public
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property FirstName: string read FFirst_name write FFirst_name;
    property Id: Extended read FId write FId;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property LastName: string read FLast_name write FLast_name;
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
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property FirstName: string read FFirst_name write FFirst_name;
    property Id: Extended read FId write FId;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property LastName: string read FLast_name write FLast_name;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkRelationRequests;
  end;

  TVkCountry = class
  private
    FId: Extended;
    FTitle: string;
  public
    property Id: Extended read FId write FId;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCountry;
  end;

  TVkPlace = class
  private
    FCity: string; // — название города;
    FCountry: string; // — название страны;
    FTitle: string; // — название места (если назначено);
    FId: integer; // — идентификатор места (если назначено);
    FLatitude: Extended; // — географическая широта;
    FLongitude: Extended; // — географическая долгота;
    FCreated: integer; // — дата создания (если назначено);
    FIcon: string;
    FType: string;
    FAddress: string; // — URL изображения-иконки;
  public
    property Id: integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property&Type: string read FType write FType;
    property Country: string read FCountry write FCountry;
    property City: string read FCity write FCity;
    property Created: integer read FCreated write FCreated;
    property Icon: string read FIcon write FIcon;
    property Address: string read FAddress write FAddress;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPlace;
  end;

  TVkCoordinates = class
  private
    FLatitude: Extended;
    FLongitude: Extended;
  public
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCoordinates;
  end;

  TVkCity = class
  private
    FId: Integer;
    FTitle: string;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkCity;
  end;

  TVkGeo = class
  private
    FCoordinates: string;
    FPlace: TVkPlace;
    FType: string;
  public
    property Coordinates: string read FCoordinates write FCoordinates;
    property Place: TVkPlace read FPlace write FPlace;
    property&Type: string read FType write FType;
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
    property Photo50: string read FPhoto_50 write FPhoto_50; // — URL изображения 50x50px;
    property Photo100: string read FPhoto_100 write FPhoto_100; // — URL изображения 100x100px;
    property Photo200: string read FPhoto_200 write FPhoto_200; // — URL изображения 200x200px;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatPhoto;
  end;

  TVkIdList = class
  private
    FCount: Integer;
    FItems: TArray<Integer>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<Integer> read FItems write FItems;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkIdList;
  end;

var
  VkSizes: array[0..5] of Char = ('s', 'm', 'x', 'y', 'z', 'w');

implementation

uses
  System.SysUtils, VK.CommonUtils;

{TVkCountry}

function TVkCountry.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCountry.FromJsonString(AJsonString: string): TVkCountry;
begin
  result := TJson.JsonToObject<TVkCountry>(AJsonString)
end;

{TVkRelationRequests}

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

{TVkRect}

function TVkRect.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRect.FromJsonString(AJsonString: string): TVkRect;
begin
  result := TJson.JsonToObject<TVkRect>(AJsonString)
end;

{TVkTags}

function TVkTags.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkTags.FromJsonString(AJsonString: string): TVkTags;
begin
  result := TJson.JsonToObject<TVkTags>(AJsonString)
end;

{TVkCommentsInfo}

function TVkCommentsInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCommentsInfo.FromJsonString(AJsonString: string): TVkCommentsInfo;
begin
  result := TJson.JsonToObject<TVkCommentsInfo>(AJsonString)
end;

{TVkPostSource}

function TVkPostSource.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPostSource.FromJsonString(AJsonString: string): TVkPostSource;
begin
  result := TJson.JsonToObject<TVkPostSource>(AJsonString)
end;

{TVkRepostsInfo}

function TVkRepostsInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkRepostsInfo.FromJsonString(AJsonString: string): TVkRepostsInfo;
begin
  result := TJson.JsonToObject<TVkRepostsInfo>(AJsonString)
end;

{TVkLikesInfo}

function TVkLikesInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkLikesInfo.FromJsonString(AJsonString: string): TVkLikesInfo;
begin
  result := TJson.JsonToObject<TVkLikesInfo>(AJsonString)
end;

{TVkCity}

function TVkCity.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkCity.FromJsonString(AJsonString: string): TVkCity;
begin
  result := TJson.JsonToObject<TVkCity>(AJsonString)
end;

{TVkSizes}

function TVkSize.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkSize.FromJsonString(AJsonString: string): TVkSize;
begin
  result := TJson.JsonToObject<TVkSize>(AJsonString);
end;

{TVkViewsInfo}

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
  FPlace := TVkPlace.Create();
end;

destructor TVkGeo.Destroy;
begin
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

{ TVkSizesHelper }

function TVkSizesHelper.Get(Value: string): TVkSize;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(Self) to High(Self) do
    if Self[i].FType = Value then
      Exit(Self[i]);
end;

function TVkSizesHelper.GetNextSize(Value: string; Circular: Boolean): string;
var
  i: Integer;
begin
  Result := VkSizes[0];
  for i := 0 to 5 do
    if VkSizes[i] = Value then
    begin
      if i < 5 then
        Exit(VkSizes[i + 1])
      else if Circular then
        Result := VkSizes[0];
    end;
end;

function TVkSizesHelper.GetPrevSize(Value: string; Circular: Boolean): string;
var
  i: Integer;
begin
  Result := VkSizes[0];
  for i := 5 downto 0 do
    if VkSizes[i] = Value then
    begin
      if i > 0 then
        Exit(VkSizes[i - 1])
      else if Circular then
        Result := VkSizes[5];
    end;
end;

function TVkSizesHelper.GetSize(Value: string; Circular: Boolean): TVkSize;
begin
  Result := Get(Value);
  if Assigned(Result) then
    Exit;
  repeat
    Value := GetNextSize(Value, Circular);
    if not Value.IsEmpty then
      Result := Get(Value);
  until Assigned(Result);
end;

function TVkSizesHelper.GetSizeMax(Value: string; Circular: Boolean): TVkSize;
begin
  Result := GetSize(Value, Circular);
end;

function TVkSizesHelper.GetSizeMin(Value: string; Circular: Boolean): TVkSize;
begin
  Result := Get(Value);
  if Assigned(Result) then
    Exit;

  repeat
    Value := GetPrevSize(Value, Circular);
    if not Value.IsEmpty then
      Result := Get(Value);
  until Assigned(Result);
end;


{ TVkAddresses }

function TVkAddresses.GetIs_enabled: Boolean;
begin
  Result := FIs_enabled = 1;
end;

procedure TVkAddresses.SetIs_enabled(const Value: Boolean);
begin
  FIs_enabled := BoolToInt(Value);
end;

{ TVkIdList }

class function TVkIdList.FromJsonString(AJsonString: string): TVkIdList;
begin
  result := TJson.JsonToObject<TVkIdList>(AJsonString)
end;

function TVkIdList.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

