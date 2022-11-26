unit VK.Entity.Common;

interface

uses
  System.SysUtils, System.Json, Rest.Json;

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
    property &Type: string read FType write FType;
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
  public
    /// <summary>
    /// �������� ������� � ��������� �������� (��������, s, m, w, z, ...)
    /// </summary>
    function Get(Value: Char): TVkSize;
    /// <summary>
    /// �������� ������������� ������� � ��������� ��� ��������� ��������
    /// </summary>
    function GetSize(Value: Char; Higher: Boolean = True): TVkSize;
    /// <summary>
    /// �������� ������� � ����������� ��������
    /// </summary>
    function GetSizeMin(const From: Char = 'a'): TVkSize;
    /// <summary>
    /// �������� ������� � ������������ ��������
    /// </summary>
    function GetSizeMax(const From: Char = 'z'): TVkSize;
    /// <summary>
    /// �������� ������� � ������������ �������� (����. �� ����� ������������ W + H)
    /// </summary>
    function GetSizeMaxSum: TVkSize;
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
  TJson.JsonToObject(Self, AJson);
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

