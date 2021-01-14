unit VK.Entity.Market.Album;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo, VK.Entity.Common;

type
  TVkMarketAlbum = class(TVkObject)
  private
    FCount: Integer;
    FOwner_id: Integer;
    FTitle: string;
    FUpdated_time: int64;
    FPhoto: TVkPhoto;
  public
    property Count: Integer read FCount write FCount;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Title: string read FTitle write FTitle;
    property UpdatedTime: int64 read FUpdated_time write FUpdated_time;
  end;

  TVkMarketAlbums = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkMarketAlbum>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkMarketAlbum> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkMarketAlbums}

destructor TVkMarketAlbums.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkMarketAlbum>(FItems);
  inherited;
end;

end.

