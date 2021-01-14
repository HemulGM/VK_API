unit VK.Entity.Common;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkEntity = class(TInterfacedObject)
    function ToJsonString: string;
    class function FromJsonString<T: class, constructor>(AJsonString: string): T;
    constructor Create; virtual;
  end;

  TVkObject = class(TVkEntity)
  protected
    FId: Integer;
  public
    property Id: Integer read FId write FId;
  end;

  TVkDimensions = class(TVkEntity)
  private
    FWidth: Integer;
    FHeight: Integer;
    FLength: Integer;
  public
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Length: Integer read FLength write FLength;
  end;

  TVkLastActivity = class(TVkEntity)
  private
    FOnline: Integer;
    FTime: Int64;
  public
    property Online: Integer read FOnline write FOnline;
    property Time: Int64 read FTime write FTime;
  end;

  TVkBasicIndexItems = class(TVkEntity)
  private
    FItems: TArray<Integer>;
  public
    property Items: TArray<Integer> read FItems write FItems;
  end;

  TVkBasicObject = class(TVkObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  TVkProductCurrency = TVkBasicObject;

  TVkRect = class(TVkEntity)
  private
    FX: Integer;
    FX2: Integer;
    FY: Integer;
    FY2: Integer;
  public
    property X: Integer read FX write FX;
    property X2: Integer read FX2 write FX2;
    property Y: Integer read FY write FY;
    property Y2: Integer read FY2 write FY2;
  end;

  TVkAddresses = class
  private
    FIs_enabled: Integer;
    FMain_address_id: Integer;
    function GetIs_enabled: Boolean;
    procedure SetIs_enabled(const Value: Boolean);
  public
    property IsEnabled: Boolean read GetIs_enabled write SetIs_enabled;
    property MainAddressId: Integer read FMain_address_id write FMain_address_id;
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

  TVkCrop = TVkRect;

  TVkTags = class(TVkEntity)
  private
    FCount: Integer;
  public
    property Count: Integer read FCount write FCount;
  end;

  TVkPostSource = class(TVkEntity)
  private
    FData: string;
    FPlatform: string;
    FType: string;
    FUrl: string;
  public
    property Data: string read FData write FData;
    property&Platform: string read FPlatform write FPlatform;
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
  end;

  TVkCommentsInfo = class(TVkEntity)
  private
    FCan_post: Integer;
    FCount: Integer;
    FGroups_can_post: Boolean;
  public
    property CanPost: Integer read FCan_post write FCan_post;
    property Count: Integer read FCount write FCount;
    property GroupsCanPost: Boolean read FGroups_can_post write FGroups_can_post;
  end;

  TVkRepostsInfo = class(TVkEntity)
  private
    FCount: Integer;
    FUser_reposted: Integer;
  public
    property Count: Integer read FCount write FCount;
    property UserReposted: Integer read FUser_reposted write FUser_reposted;
  end;

  TVkViewsInfo = class(TVkEntity)
  private
    FCount: Integer;
  public
    property Count: Integer read FCount write FCount;
  end;

  TVkLiked = class(TVkEntity)
  private
    FCopied: Integer;
    FLiked: Integer;
  public
    property Copied: Integer read FCopied write FCopied;
    property Liked: Integer read FLiked write FLiked;
  end;

  TVkLikesInfo = class(TVkEntity)
  private
    FCan_like: Integer;
    FCan_publish: Integer;
    FCount: Integer;
    FUser_likes: Integer;
  public
    property CanLike: Integer read FCan_like write FCan_like;
    property CanPublish: Integer read FCan_publish write FCan_publish;
    property Count: Integer read FCount write FCount;
    property UserLikes: Integer read FUser_likes write FUser_likes;
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
  TVkSize = class(TVkEntity)
  private
    FHeight: Integer;
    FType: string;
    FUrl: string;
    FWidth: Integer;
    FSrc: string;
  public
    property Height: Integer read FHeight write FHeight;
    property&Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
    property Src: string read FSrc write FSrc;
    property Width: Integer read FWidth write FWidth;
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

  TVkRelationPartner = class(TVkObject)
  private
    FCan_access_closed: Boolean;
    FFirst_name: string;
    FIs_closed: Boolean;
    FLast_name: string;
  public
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property FirstName: string read FFirst_name write FFirst_name;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property LastName: string read FLast_name write FLast_name;
  end;

  TVkRelationRequests = class(TVkObject)
  private
    FCan_access_closed: Boolean;
    FFirst_name: string;
    FIs_closed: Boolean;
    FLast_name: string;
  public
    property CanAccessClosed: Boolean read FCan_access_closed write FCan_access_closed;
    property FirstName: string read FFirst_name write FFirst_name;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property LastName: string read FLast_name write FLast_name;
  end;

  TVkEmail = class(TVkObject)
  private
    FAddress: string;
  public
    property Address: string read FAddress write FAddress;
  end;

  TVkPlace = class(TVkObject)
  private
    FCity: string; // — название города;
    FCountry: string; // — название страны;
    FTitle: string; // — название места (если назначено);
    FLatitude: Extended; // — географическая широта;
    FLongitude: Extended; // — географическая долгота;
    FCreated: Int64; // — дата создания (если назначено);
    FIcon: string;
    FType: string;
    FAddress: string;
    FCheckins: Integer; // — URL изображения-иконки;
  public
    property Checkins: Integer read FCheckins write FCheckins;
    property Title: string read FTitle write FTitle;
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
    property&Type: string read FType write FType;
    property Country: string read FCountry write FCountry;
    property City: string read FCity write FCity;
    property Created: Int64 read FCreated write FCreated;
    property Icon: string read FIcon write FIcon;
    property Address: string read FAddress write FAddress;
  end;

  TVkCoordinates = class(TVkEntity)
  private
    FLatitude: Extended;
    FLongitude: Extended;
  public
    property Latitude: Extended read FLatitude write FLatitude;
    property Longitude: Extended read FLongitude write FLongitude;
  end;

  TVkGeo = class(TVkEntity)
  private
    FCoordinates: string;
    FPlace: TVkPlace;
    FType: string;
  public
    property Coordinates: string read FCoordinates write FCoordinates;
    property Place: TVkPlace read FPlace write FPlace;
    property&Type: string read FType write FType;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkChatPhoto = class(TVkEntity)
  private
    FPhoto_50: string;
    FPhoto_200: string;
    FPhoto_100: string;
  public
    property Photo50: string read FPhoto_50 write FPhoto_50; // — URL изображения 50x50px;
    property Photo100: string read FPhoto_100 write FPhoto_100; // — URL изображения 100x100px;
    property Photo200: string read FPhoto_200 write FPhoto_200; // — URL изображения 200x200px;
  end;

  TVkIdList = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<Integer>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<Integer> read FItems write FItems;
  end;

  TVkOwnerPhoto = class(TVkEntity)
  private
    Fphoto_src: string;
    Fphoto_src_small: string;
    Fphoto_src_big: string;
    Fpost_id: Integer;
    Fphoto_hash: string;
    Fsaved: Boolean;
  public
    property PhotoHash: string read Fphoto_hash write Fphoto_hash;
    property PhotoSrc: string read Fphoto_src write Fphoto_src;
    property PhotoSrcBig: string read Fphoto_src_big write Fphoto_src_big;
    property PhotoSrcSmall: string read Fphoto_src_small write Fphoto_src_small;
    property Saved: Boolean read Fsaved write Fsaved;
    property PostId: Integer read Fpost_id write Fpost_id;
  end;

var
  VkSizes: array[0..5] of Char = ('s', 'm', 'x', 'y', 'z', 'w');

implementation

uses
  System.SysUtils, VK.CommonUtils;

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

{ TVkEntity }

constructor TVkEntity.Create;
begin
  //
end;

class function TVkEntity.FromJsonString<T>(AJsonString: string): T;
begin

  Result := TJson.JsonToObject<T>(AJsonString);
end;

function TVkEntity.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

end.

