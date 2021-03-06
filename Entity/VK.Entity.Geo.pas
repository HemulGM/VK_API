unit VK.Entity.Geo;

interface

uses
  Generics.Collections, System.Json, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, REST.Json.Types,
  VK.Entity.Common, VK.Wrap.Interceptors;

type
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

  TVkCoordinates = class
  private
    FLatitude: Extended;
    FLongitude: Extended;
  public
    /// <summary>
    /// �������������� ������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    property Latitude: Extended read FLatitude write FLatitude;
    /// <summary>
    /// �������������� ������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    property Longitude: Extended read FLongitude write FLongitude;
  end;

  TVkGeo = class(TVkEntity)
  private
    FCoordinates: TVkCoordinates;
    FPlace: TVkPlace;
    FType: string; //point,
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FShowmap: Boolean;
  public
    /// <summary>
    /// ���������� �����
    /// </summary>
    property Coordinates: TVkCoordinates read FCoordinates write FCoordinates;
    /// <summary>
    /// �������� ����� (���� ��� ���������)
    /// </summary>
    property Place: TVkPlace read FPlace write FPlace;
    /// <summary>
    /// ��� �����
    /// </summary>
    property&Type: string read FType write FType;
    /// <summary>
    /// ���������� � ���, ������������ �� �����
    /// </summary>
    property Showmap: Boolean read FShowmap write FShowmap;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{TVkGeo}

constructor TVkGeo.Create;
begin
  inherited;
  FCoordinates := TVkCoordinates.Create;
end;

destructor TVkGeo.Destroy;
begin
  FCoordinates.Free;
  if Assigned(FPlace) then
    FPlace.Free;
  inherited;
end;

end.

