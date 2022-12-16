unit VK.Entity.Common;

interface

uses
  System.SysUtils, System.Json, Rest.Json, System.Types;

type
  TVkEntity = class(TInterfacedObject)
  public
    function ToJsonString: string;
    function ToJsonObject: TJSONObject;
    class function FromJsonString<T: class, constructor>(AJsonString: string): T; overload;
    class function FromJsonObject<T: class, constructor>(AJson: TJSONObject): T; overload;
    procedure FromJson(AJson: TJSONObject);
    constructor Create; virtual;
    function Clone<T: class, constructor>: T;
  end;

  TVkCounterEntity = class(TVkEntity)
  private
    FCount: Integer;
  public
    property Count: Integer read FCount write FCount;
  end;

  TVkObject = class(TVkEntity)
  protected
    FId: Int64;
  public
    property Id: Int64 read FId write FId;
  end;

  TVkBasicObject = class(TVkObject)
  private
    FName: string;
  public
    property Id;
    property Name: string read FName write FName;
  end;

  //////////////////////////////////////////////////////////////////////////////

  TVkCopyright = class(TVkEntity)
  private
    FLink: string;
    FName: string;
    FType: string;
  public
    property Link: string read FLink write FLink;
    property Name: string read FName write FName;
    property &Type: string read FType write FType;
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

  TVkLiked = class(TVkEntity)
  private
    FCopied: Integer;
    FLiked: Integer;
  public
    property Copied: Integer read FCopied write FCopied;
    property Liked: Integer read FLiked write FLiked;
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
    property &Type: string read FType write FType;
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
  public
    /// <summary>
    /// Получить элемент с указанным размером (например, s, m, w, z, ...)
    /// </summary>
    function Get(Value: Char): TVkSize;
    /// <summary>
    /// Получить гарантировано элемент с указанным или ближайшим размером
    /// </summary>
    function GetSize(Value: Char; Higher: Boolean = True): TVkSize;
    /// <summary>
    /// Получить элемент с минимальным размером
    /// </summary>
    function GetSizeMin(const From: Char = 'a'): TVkSize;
    /// <summary>
    /// Получить элемент с максимальным размером
    /// </summary>
    function GetSizeMax(const From: Char = 'z'): TVkSize;
    /// <summary>
    /// Получить элемент с максимальным размером (макс. по сумме размерностей W + H)
    /// </summary>
    function GetSizeMaxSum: TVkSize;
    function GetSizeFromHeight(const Height: Integer): TVkSize;
    function GetSizeFromWidth(const Width: Integer): TVkSize;
    function GetMaxSizeOrZero: TSize;
    function GetSizeUrlOrEmpty(const Size: Integer = 50): string;
  end;

  TVkChatPhoto = class(TVkEntity)
  private
    FPhoto_50: string;
    FPhoto_200: string;
    FPhoto_100: string;
    FIs_default_photo: Boolean;
    FIs_default_call_photo: Boolean;
    FPhoto_34: string;
    FPhoto_1200: string;
    FPhoto_300: string;
    FPhoto_68: string;
    FPhoto_270: string;
    FPhoto_600: string;
    FPhoto_135: string;
  public
    property Photo34: string read FPhoto_34 write FPhoto_34;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property Photo68: string read FPhoto_68 write FPhoto_68;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo135: string read FPhoto_135 write FPhoto_135;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo270: string read FPhoto_270 write FPhoto_270;
    property Photo300: string read FPhoto_300 write FPhoto_300;
    property Photo600: string read FPhoto_600 write FPhoto_600;
    property Photo1200: string read FPhoto_1200 write FPhoto_1200;
    /// <summary>
    /// Фото по умолчанию
    /// </summary>
    property IsDefaultPhoto: Boolean read FIs_default_photo write FIs_default_photo;
    /// <summary>
    /// Фото по умолчанию (звонки)
    /// </summary>
    property IsDefaultCallPhoto: Boolean read FIs_default_call_photo write FIs_default_call_photo;
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

implementation

uses
  VK.CommonUtils;

{ TVkSizesHelper }

function TVkSizesHelper.Get(Value: Char): TVkSize;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(Self) to High(Self) do
    if Self[i].FType = Value then
      Exit(Self[i]);
end;

function TVkSizesHelper.GetMaxSizeOrZero: TSize;
begin
  if Length(Self) > 0 then
  begin
    var Item := Self[High(Self)];
    Result := TSize.Create(Item.Width, Item.Height);
  end
  else
    Result := TSize.Create(0, 0);
end;

function TVkSizesHelper.GetSize(Value: Char; Higher: Boolean): TVkSize;
begin
  Result := Get(Value);
  if Assigned(Result) then
    Exit;
  if Higher then
    Result := GetSizeMin(Value)
  else
    Result := GetSizeMax(Value);
end;

function TVkSizesHelper.GetSizeFromHeight(const Height: Integer): TVkSize;
var
  D: Integer;
begin
  Result := nil;
  D := -1;
  for var Item in Self do
    if Item.Height >= Height then
    begin
      if (D = -1) or (Item.Height - Height < D) then
      begin
        D := Item.Height - Height;
        Result := Item;
      end;
    end;
  if (not Assigned(Result)) and (Length(Self) > 0) then
    Result := Self[High(Self)];
end;

function TVkSizesHelper.GetSizeFromWidth(const Width: Integer): TVkSize;
var
  D: Integer;
begin
  Result := nil;
  D := -1;
  for var Item in Self do
    if Item.Width >= Width then
    begin
      if (D = -1) or (Item.Width - Width < D) then
      begin
        D := Item.Width - Width;
        Result := Item;
      end;
    end;
  if (not Assigned(Result)) and (Length(Self) > 0) then
    Result := Self[High(Self)];
end;

function TVkSizesHelper.GetSizeMax(const From: Char): TVkSize;
begin
  Result := nil;
  for var C := From downto 'a' do
  begin
    Result := Get(c);
    if Assigned(Result) then
      Exit;
  end;
end;

function TVkSizesHelper.GetSizeMaxSum: TVkSize;
var
  MaxSzSum: Integer;
begin
  MaxSzSum := 0;
  Result := nil;
  for var Item in Self do
    if Item.Height + Item.Width > MaxSzSum then
    begin
      Result := Item;
      MaxSzSum := Item.Height + Item.Width;
    end;
end;

function TVkSizesHelper.GetSizeMin(const From: Char): TVkSize;
begin
  Result := nil;
  for var C := From to 'z' do
  begin
    Result := Get(c);
    if Assigned(Result) then
      Exit;
  end;
end;

function TVkSizesHelper.GetSizeUrlOrEmpty(const Size: Integer): string;
begin
  var Item := GetSizeFromHeight(Size);
  if Assigned(Item) then
  begin
    if not Item.Url.IsEmpty then
      Result := Item.Url
    else
      Result := Item.Src;
  end
  else
    Result := '';
end;

{ TVkEntity }

function TVkEntity.Clone<T>: T;
var
  JObject: TJSONObject;
begin
  JObject := ToJsonObject;
  try
    Result := FromJsonObject<T>(JObject);
  finally
    JObject.Free;
  end;
end;

constructor TVkEntity.Create;
begin
  //dummy
end;

procedure TVkEntity.FromJson(AJson: TJSONObject);
begin
  TJson.JsonToObject(Self, AJson, [joIgnoreEmptyArrays, joIgnoreEmptyStrings]);
end;

class function TVkEntity.FromJsonObject<T>(AJson: TJSONObject): T;
begin
  Result := TJson.JsonToObject<T>(AJson, [joIgnoreEmptyArrays, joIgnoreEmptyStrings]);
end;

class function TVkEntity.FromJsonString<T>(AJsonString: string): T;
begin
  Result := TJson.JsonToObject<T>(AJsonString, [joIgnoreEmptyArrays, joIgnoreEmptyStrings]);
end;

function TVkEntity.ToJsonObject: TJSONObject;
begin
  Result := TJson.ObjectToJsonObject(Self);
end;

function TVkEntity.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

end.

