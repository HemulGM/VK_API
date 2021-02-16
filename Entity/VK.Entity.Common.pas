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
    /// ������������� ������
    /// </summary>
    property Id;
    /// <summary>
    /// ����������� ������
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
    /// ���������� X ������ �������� ���� � ���������
    /// </summary>
    property X: Integer read FX write FX;
    /// <summary>
    /// ���������� Y ������ �������� ���� � ���������
    /// </summary>
    property X2: Integer read FX2 write FX2;
    /// <summary>
    /// ���������� X ������� ������� ���� � ���������
    /// </summary>
    property Y: Integer read FY write FY;
    /// <summary>
    /// ���������� Y ������� ������� ���� � ���������
    /// </summary>
    property Y2: Integer read FY2 write FY2;
  end;

  /// <summary>
  /// ������ post_source, ����������� ������ ���������� ������ �� �����
  /// </summary>
  TVkPostSource = class(TVkEntity)
  private
    FData: string;
    FPlatform: string;
    FType: string;
    FUrl: string;
  public
    /// <summary>
    /// ��� �������� (������ ��� type = vk ��� widget)
    /// ��������� ��������:
    /// profile_activity � ��������� ������� ��� ������ ������������ (��� type = vk);
    /// profile_photo � ��������� ���������� ���������� ������������ (��� type = vk);
    /// comments � ������ ������������ (��� type = widget);
    /// like � ������ ���� ��������� (��� type = widget);
    /// poll � ������ ������� (��� type = widget);
    /// </summary>
    property Data: string read FData write FData;
    /// <summary>
    /// �������� ���������, ���� ��� �������� (android; iphone; wphone)
    /// </summary>
    property&Platform: string read FPlatform write FPlatform;
    /// <summary>
    /// ��� ���������
    ///  vk � ������ ������� ����� �������� ��������� ����� (http://vk.com/);
    ///  widget � ������ ������� ����� ������ �� ��������� �����;
    ///  api � ������ ������� ����������� ����� API;
    ///  rss� ������ ������� ����������� ������� RSS-����� �� ���������� �����;
    ///  sms � ������ ������� ����������� �������� SMS-��������� �� ����������� �����.
    /// </summary>
    property&Type: string read FType write FType;
    /// <summary>
    /// URL �������, � �������� ���� ������������ ������
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
    /// URL �����
    /// </summary>
    property Url: string read FUrl write FUrl;
    /// <summary>
    /// ������
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// ������
    /// </summary>
    property Width: Integer read FWidth write FWidth;
  end;

  {$REGION '��������� �������� ���� Type'}
  {
    s � ���������������� ����� ����������� � ������������ �������� 75px;
    m � ���������������� ����� ����������� � ������������ �������� 130px;
    x � ���������������� ����� ����������� � ������������ �������� 604px;
    o � ���� ����������� "������/������" ��������� ����������� ������ ��� ����� 3:2, �� ���������������� ����� � ������������ ������� 130px. ���� ����������� "������/������" ������ 3:2, �� ����� ����������� ����� ����������� � ������������ ������� 130px � ������������ ������ 3:2.
    p � ���� ����������� "������/������" ��������� ����������� ������ ��� ����� 3:2, �� ���������������� ����� � ������������ ������� 200px. ���� ����������� "������/������" ������ 3:2, �� ����� ����������� ����� � ������ ����������� � ������������ ������� 200px � ������������ ������ 3:2.
    q � ���� ����������� "������/������" ��������� ����������� ������ ��� ����� 3:2, �� ���������������� ����� � ������������ ������� 320px. ���� ����������� "������/������" ������ 3:2, �� ����� ����������� ����� � ������ ����������� � ������������ ������� 320px � ������������ ������ 3:2.
    r � ���� ����������� "������/������" ��������� ����������� ������ ��� ����� 3:2, �� ���������������� ����� � ������������ ������� 510px. ���� ����������� "������/������" ������ 3:2, �� ����� ����������� ����� � ������ ����������� � ������������ ������� 510px � ������������ ������ 3:2
    y � ���������������� ����� ����������� � ������������ �������� 807px;
    z � ���������������� ����� ����������� � ������������ �������� 1080x1024;
    w � ���������������� ����� ����������� � ������������ �������� 2560x2048px.
  }
  {$ENDREGION}
  /// <summary>
  /// ������ �������� �������� ����������
  /// https://vk.com/dev/photo_sizes
  /// ��� ����������, ����������� �� ���� �� 2012 ����, �������� width � height ����� ���� ����������, � ���� ������ ��������������� ���� �������� 0
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
    /// ������ ����� � ��������
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// ����������� ������� � ��������� �����
    /// </summary>
    property&Type: string read FType write FType;
    /// <summary>
    /// URL �����
    /// </summary>
    property Url: string read FUrl write FUrl;
    /// <summary>
    /// ������ ����� � ��������
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
  /// ������, ����������� �����
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
    /// ������������� �����
    /// </summary>
    property Id;
    /// <summary>
    /// ����� ������� � ���� �����
    /// </summary>
    property Checkins: Integer read FCheckins write FCheckins;
    /// <summary>
    /// �������� �����
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// �������������� ������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    property Latitude: Extended read FLatitude write FLatitude;
    /// <summary>
    /// �������������� ������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    property Longitude: Extended read FLongitude write FLongitude;
    /// <summary>
    /// ��� �����
    /// </summary>
    property&Type: Integer read FType write FType;
    /// <summary>
    /// ������������� ������
    /// </summary>
    property Country: string read FCountry write FCountry;
    /// <summary>
    /// ������������� ������
    /// </summary>
    property City: string read FCity write FCity;
    /// <summary>
    /// ���� �������� �����
    /// </summary>
    property Created: TDateTime read FCreated write FCreated;
    /// <summary>
    /// ���� ���������� ����� � Unixtime.
    /// </summary>
    property Updated: TDateTime read FUpdated write FUpdated;
    /// <summary>
    /// ������ �����, URL �����������
    /// </summary>
    property Icon: string read FIcon write FIcon;
    /// <summary>
    /// ����� �����
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
    /// ���������� �����
    /// </summary>
    property Coordinates: string read FCoordinates write FCoordinates;
    /// <summary>
    /// �������� ����� (���� ��� ���������)
    /// </summary>
    property Place: TVkPlace read FPlace write FPlace;
    /// <summary>
    /// ��� �����
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
    /// URL ����������� 50x50px;
    /// </summary>
    property Photo50: string read FPhoto_50 write FPhoto_50;
    /// <summary>
    /// URL ����������� 100x100px;
    /// </summary>
    property Photo100: string read FPhoto_100 write FPhoto_100;
    /// <summary>
    /// URL ����������� 200x200px;
    /// </summary>
    property Photo200: string read FPhoto_200 write FPhoto_200;
    /// <summary>
    /// ���� �� ���������
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

