unit VK.Entity.Market.Album;

interface

uses
  Generics.Collections, REST.JsonReflect, VK.Wrap.Interceptors, Rest.Json,
  VK.Entity.Photo, VK.Entity.Common, VK.Entity.Common.List, VK.Types;

type
  TVkMarketAlbum = class(TVkObject)
  private
    FCount: Integer;
    FOwner_id: TVkPeerId;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FUpdated_time: TDateTime;
    FPhoto: TVkPhoto;
    FAccess_key: string;
  public
    /// <summary>
    /// ������������� ��������.
    /// </summary>
    property Id;
    /// <summary>
    /// ������������� ��������� ��������.
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    /// <summary>
    /// ���� �������
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ����� ������� � ��������.
    /// </summary>
    property Count: Integer read FCount write FCount;
    /// <summary>
    /// ������� ��������, ������, ����������� ����������.
    /// </summary>
    property Photo: TVkPhoto read FPhoto write FPhoto;
    /// <summary>
    /// �������� ��������.
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// ���� ���������� ��������
    /// </summary>
    property UpdatedTime: TDateTime read FUpdated_time write FUpdated_time;
    destructor Destroy; override;
  end;

  TVkMarketAlbums = TVkEntityList<TVkMarketAlbum>;

implementation

{ TVkMarketAlbum }

destructor TVkMarketAlbum.Destroy;
begin
  if Assigned(FPhoto) then
    FPhoto.Free;
  inherited;
end;

end.

