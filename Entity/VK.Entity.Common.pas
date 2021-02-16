unit VK.Entity.Common;

interface

uses
  Generics.Collections, System.Json, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, REST.Json.Types;

type
  TVkEntity = class(TInterfacedObject)
  public
    function ToJsonString: string;
    class function FromJsonString<T: class, constructor>(AJsonString: string): T;
    procedure FromJson(AJson: TJSONObject);
    constructor Create; virtual;
  end;

  TVkCounterEntity = class(TVkEntity)
  private
    FCount: Integer;
  public
    property Count: Integer read FCount write FCount;
  end;

  TVkObject = class(TVkEntity)
  protected
    FId: Integer;
  public
    property Id: Integer read FId write FId;
  end;

  TVkBasicObject = class(TVkObject)
  private
    FName: string;
  public
    property Id;
    property Name: string read FName write FName;
  end;

  TVkCopyright = class(TVkEntity)
  private
    FLink: string;
    FName: string;
    FType: string;
  public
    property Link: string read FLink write FLink;
    property Name: string read FName write FName;
    property&Type: string read FType write FType;
  end;

  TVkProductCurrency = class(TVkBasicObject)
  public
    /// <summary>
    /// Идентификатор валюты
    /// </summary>
    property Id;
    /// <summary>
    /// Обозначение валюты
    /// </summary>
    property Name;
  end;

  TVkRect = class(TVkEntity)
  private
    FX: Integer;
    FX2: Integer;
    FY: Integer;
    FY2: Integer;
  public
    /// <summary>
    /// Координата X левого верхнего угла в процентах
    /// </summary>
    property X: Integer read FX write FX;
    /// <summary>
    /// Координата Y левого верхнего угла в процентах
    /// </summary>
    property X2: Integer read FX2 write FX2;
    /// <summary>
    /// Координата X правого нижнего угла в процентах
    /// </summary>
    property Y: Integer read FY write FY;
    /// <summary>
    /// Координата Y правого нижнего угла в процентах
    /// </summary>
    property Y2: Integer read FY2 write FY2;
  end;

  /// <summary>
  /// Объект post_source, описывающий способ размещения записи на стене
  /// </summary>
  TVkPostSource = class(TVkEntity)
  private
    FData: string;
    FPlatform: string;
    FType: string;
    FUrl: string;
  public
    /// <summary>
    /// Тип действия (только для type = vk или widget)
    /// Возможные значения:
    /// profile_activity — изменение статуса под именем пользователя (для type = vk);
    /// profile_photo — изменение профильной фотографии пользователя (для type = vk);
    /// comments — виджет комментариев (для type = widget);
    /// like — виджет «Мне нравится» (для type = widget);
    /// poll — виджет опросов (для type = widget);
    /// </summary>
    property Data: string read FData write FData;
    /// <summary>
    /// Название платформы, если оно доступно (android; iphone; wphone)
    /// </summary>
    property&Platform: string read FPlatform write FPlatform;
    /// <summary>
    /// Тип источника
    ///  vk — запись создана через основной интерфейс сайта (http://vk.com/);
    ///  widget — запись создана через виджет на стороннем сайте;
    ///  api — запись создана приложением через API;
    ///  rss— запись создана посредством импорта RSS-ленты со стороннего сайта;
    ///  sms — запись создана посредством отправки SMS-сообщения на специальный номер.
    /// </summary>
    property&Type: string read FType write FType;
    /// <summary>
    /// URL ресурса, с которого была опубликована запись
    /// </summary>
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
    /// <summary>
    /// URL копии
    /// </summary>
    property Url: string read FUrl write FUrl;
    /// <summary>
    /// Высота
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// Ширина
    /// </summary>
    property Width: Integer read FWidth write FWidth;
  end;

  {$REGION 'Возможные значения поля Type'}
  {
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
  /// Для фотографий, загруженных на сайт до 2012 года, значения width и height могут быть недоступны, в этом случае соответствующие поля содержат 0
  /// </summary>
  TVkSize = class(TVkEntity)
  private
    FHeight: Integer;
    FType: string;
    FUrl: string;
    FWidth: Integer;
    FSrc: string;
  public
    /// <summary>
    /// Высота копии в пикселах
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// Обозначение размера и пропорций копии
    /// </summary>
    property&Type: string read FType write FType;
    /// <summary>
    /// URL копии
    /// </summary>
    property Url: string read FUrl write FUrl;
    /// <summary>
    /// Ширина копии в пикселах
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    property Src: string read FSrc write FSrc;
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

  /// <summary>
  /// Объект, описывающий место
  /// </summary>
  TVkPlace = class(TVkObject)
  private
    FCity: string;
    FCountry: string;
    FTitle: string;
    FLatitude: Extended;
    FLongitude: Extended;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FCreated: TDateTime;
    FIcon: string;
    FType: Integer;
    FAddress: string;
    FCheckins: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FUpdated: TDateTime;
  public
    /// <summary>
    /// Идентификатор места
    /// </summary>
    property Id;
    /// <summary>
    /// Число отметок в этом месте
    /// </summary>
    property Checkins: Integer read FCheckins write FCheckins;
    /// <summary>
    /// Название места
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// Географическая широта, заданная в градусах (от -90 до 90)
    /// </summary>
    property Latitude: Extended read FLatitude write FLatitude;
    /// <summary>
    /// Географическая широта, заданная в градусах (от -90 до 90)
    /// </summary>
    property Longitude: Extended read FLongitude write FLongitude;
    /// <summary>
    /// Тип места
    /// </summary>
    property&Type: Integer read FType write FType;
    /// <summary>
    /// Идентификатор страны
    /// </summary>
    property Country: string read FCountry write FCountry;
    /// <summary>
    /// Идентификатор города
    /// </summary>
    property City: string read FCity write FCity;
    /// <summary>
    /// Дата создания места
    /// </summary>
    property Created: TDateTime read FCreated write FCreated;
    /// <summary>
    /// дата обновления места в Unixtime.
    /// </summary>
    property Updated: TDateTime read FUpdated write FUpdated;
    /// <summary>
    /// Иконка места, URL изображения
    /// </summary>
    property Icon: string read FIcon write FIcon;
    /// <summary>
    /// Адрес места
    /// </summary>
    property Address: string read FAddress write FAddress;
  end;

  TVkGeo = class(TVkEntity)
  private
    FCoordinates: string;
    FPlace: TVkPlace;
    FType: string;
  public
    /// <summary>
    /// Координаты места
    /// </summary>
    property Coordinates: string read FCoordinates write FCoordinates;
    /// <summary>
    /// Описание места (если оно добавлено)
    /// </summary>
    property Place: TVkPlace read FPlace write FPlace;
    /// <summary>
    /// Тип места
    /// </summary>
    property&Type: string read FType write FType;
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
    /// <summary>
    /// Фото по умолчанию
    /// </summary>
    property IsDefaultPhoto: Boolean read FIs_default_photo write FIs_default_photo;
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
    property Id: string read FId write FId;
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property Photo34: string read FPhoto_34 write FPhoto_34;
    property Photo68: string read FPhoto_68 write FPhoto_68;
    property Photo135: string read FPhoto_135 write FPhoto_135;
    property Photo270: string read FPhoto_270 write FPhoto_270;
    property Photo300: string read FPhoto_300 write FPhoto_300;
    property Photo600: string read FPhoto_600 write FPhoto_600;
    property Photo1200: string read FPhoto_1200 write FPhoto_1200;
  end;

var
  VkSizes: array[0..5] of Char = ('s', 'm', 'x', 'y', 'z', 'w');

implementation

uses
  System.SysUtils, VK.CommonUtils;

{TVkGeo}

destructor TVkGeo.Destroy;
begin
  if Assigned(FPlace) then
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

procedure TVkEntity.FromJson(AJson: TJSONObject);
begin
  TJson.JsonToObject(Self, AJson);
end;

class function TVkEntity.FromJsonString<T>(AJsonString: string): T;
begin
  Result := TJson.JsonToObject<T>(AJsonString, [joIgnoreEmptyArrays]);
end;

function TVkEntity.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

end.

