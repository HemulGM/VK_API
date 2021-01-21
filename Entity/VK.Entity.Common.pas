unit VK.Entity.Common;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  REST.Json.Types;

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
    FOnline: Boolean;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FTime: TDateTime;
  public
    property Online: Boolean read FOnline write FOnline;
    property Time: TDateTime read FTime write FTime;
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
    FIs_enabled: Boolean;
    FMain_address_id: Integer;
  public
    property IsEnabled: Boolean read FIs_enabled write FIs_enabled;
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
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FEnd_date: TDateTime;
    FComment: string;
  public
    property EndDate: TDateTime read FEnd_date write FEnd_date;
    property Comment: string read FComment write FComment;
  end;

  TVkCrop = TVkRect;

  TVkCounterEntity = class(TVkEntity)
  private
    FCount: Integer;
  public
    property Count: Integer read FCount write FCount;
  end;

  TVkTags = TVkCounterEntity;

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

  TVkLiked = class(TVkEntity)
  private
    FCopied: Integer;
    FLiked: Integer;
  public
    property Copied: Integer read FCopied write FCopied;
    property Liked: Integer read FLiked write FLiked;
  end;

  TVkImage = class(TVkEntity)
  private
    FHeight: Integer;
    FUrl: string;
    FWidth: Integer;
  public
    property Height: Integer read FHeight write FHeight;
    property Url: string read FUrl write FUrl;
    property Width: Integer read FWidth write FWidth;
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

  TVkEmail = class(TVkObject)
  private
    FAddress: string;
  public
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

  TVkPlace = class(TVkObject)
  private
    FCity: string; // — название города;
    FCountry: string; // — название страны;
    FTitle: string; // — название места (если назначено);
    FLatitude: Extended; // — географическая широта;
    FLongitude: Extended; // — географическая долгота;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FCreated: TDateTime; // — дата создания (если назначено);
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
    property Created: TDateTime read FCreated write FCreated;
    property Icon: string read FIcon write FIcon;
    property Address: string read FAddress write FAddress;
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
    FIs_default_photo: Boolean;
  public
    /// <summary>
    /// URL изображения 50x50px;
    /// </summary>
    property Photo50: string read FPhoto_50 write FPhoto_50;
    /// <summary>
    /// URL изображения 100x100px;
    /// </summary>
    property Photo100: string read FPhoto_100 write FPhoto_100;
    /// <summary>
    /// URL изображения 200x200px;
    /// </summary>
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property IsDefaultPhoto: Boolean read FIs_default_photo write FIs_default_photo;
  end;

  TVkOwnerPhoto = class(TVkEntity)
  private
    FPhoto_src: string;
    FPhoto_src_small: string;
    FPhoto_src_big: string;
    FPost_id: Integer;
    FPhoto_hash: string;
    FSaved: Boolean;
  public
    property PhotoHash: string read FPhoto_hash write FPhoto_hash;
    property PhotoSrc: string read FPhoto_src write FPhoto_src;
    property PhotoSrcBig: string read Fphoto_src_big write Fphoto_src_big;
    property PhotoSrcSmall: string read FPhoto_src_small write FPhoto_src_small;
    property Saved: Boolean read FSaved write FSaved;
    property PostId: Integer read FPost_id write FPost_id;
  end;

  TVkThumb = class
  private
    FHeight: Integer;
    FPhoto_135: string;
    FPhoto_270: string;
    FPhoto_300: string;
    FPhoto_34: string;
    FPhoto_600: string;
    FPhoto_68: string;
    FWidth: Integer;
    FPhoto_1200: string;
    FId: string;
  public
    property Height: Integer read FHeight write FHeight;
    property Id: string read FId write FId;
    property Photo1200: string read FPhoto_1200 write FPhoto_1200;
    property Photo135: string read FPhoto_135 write FPhoto_135;
    property Photo270: string read FPhoto_270 write FPhoto_270;
    property Photo300: string read FPhoto_300 write FPhoto_300;
    property Photo34: string read FPhoto_34 write FPhoto_34;
    property Photo600: string read FPhoto_600 write FPhoto_600;
    property Photo68: string read FPhoto_68 write FPhoto_68;
    property Width: Integer read FWidth write FWidth;
  end;

var
  VkSizes: array[0..5] of Char = ('s', 'm', 'x', 'y', 'z', 'w');

implementation

uses
  System.SysUtils, System.Json, VK.CommonUtils;

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

