unit VK.Entity.Geo;

interface

uses
  Generics.Collections, System.Json, REST.JsonReflect, Rest.Json,
  REST.Json.Types, VK.Entity.Common, VK.Wrap.Interceptors;

type
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
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FCreated: TDateTime;
    FIcon: string;
    FType: Integer;
    FAddress: string;
    FCheckins: Integer;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FUpdated: TDateTime;
    Ftotal_checkins: Integer;
    FIs_deleted: Boolean;
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
    /// Число отметок в этом месте
    /// </summary>
    property TotalCheckins: Integer read Ftotal_checkins write Ftotal_checkins;
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
    /// Тип места (point, ...)
    /// </summary>
    property &Type: Integer read FType write FType;
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
    property IsDeleted: Boolean read FIs_deleted write FIs_deleted;
    /// <summary>
    /// Адрес места
    /// </summary>
    property Address: string read FAddress write FAddress;
  end;

  TVkCoordinates = class
  private
    FLatitude: Extended;
    FLongitude: Extended;
  public
    /// <summary>
    /// Географическая широта, заданная в градусах (от -90 до 90)
    /// </summary>
    property Latitude: Extended read FLatitude write FLatitude;
    /// <summary>
    /// Географическая широта, заданная в градусах (от -90 до 90)
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
    /// Координаты места
    /// </summary>
    property Coordinates: TVkCoordinates read FCoordinates write FCoordinates;
    /// <summary>
    /// Описание места (если оно добавлено)
    /// </summary>
    property Place: TVkPlace read FPlace write FPlace;
    /// <summary>
    /// Тип места
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// Информация о том, отображается ли карта
    /// </summary>
    property Showmap: Boolean read FShowmap write FShowmap;
    destructor Destroy; override;
  end;

  TVkGeoWall = class(TVkEntity)
  private
    FCoordinates: string;
    FPlace: TVkPlace;
    FType: string; //point,
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FShowmap: Boolean;
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
    property &Type: string read FType write FType;
    /// <summary>
    /// Информация о том, отображается ли карта
    /// </summary>
    property Showmap: Boolean read FShowmap write FShowmap;
    destructor Destroy; override;
  end;

implementation

{TVkGeo}

destructor TVkGeo.Destroy;
begin
  if Assigned(FCoordinates) then
    FCoordinates.Free;
  if Assigned(FPlace) then
    FPlace.Free;
  inherited;
end;

{ TVkGeoWall }

destructor TVkGeoWall.Destroy;
begin
  if Assigned(FPlace) then
    FPlace.Free;
  inherited;
end;

end.

