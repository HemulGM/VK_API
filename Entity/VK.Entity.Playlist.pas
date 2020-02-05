unit VK.Entity.Playlist;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Audio;

type
  TVkAudioOriginal = class
  private
    FAccess_key: string;
    FOwner_id: Extended;
    FPlaylist_id: Extended;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property PlaylistId: Extended read FPlaylist_id write FPlaylist_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioOriginal;
  end;

  TVkAudioGenres = class
  private
    FId: Extended;
    FName: string;
  public
    property Id: Extended read FId write FId;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioGenres;
  end;

  TVkAudioPlaylist = class
  private
    FAccess_key: string;
    FAlbum_type: string;
    FCount: Integer;
    FCreate_time: Extended;
    FDescription: string;
    FFollowers: Extended;
    FGenres: TArray<TVkAudioGenres>;
    FId: Integer;
    FMain_artists: TArray<TVkAudioArtist>;
    FOriginal: TVkAudioOriginal;
    FOwner_id: Integer;
    FPhoto: TVkAlbumThumb;
    FPlays: Extended;
    FTitle: string;
    FType: Extended;
    FUpdate_time: Extended;
    FYear: Extended;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property AlbumType: string read FAlbum_type write FAlbum_type;
    property Count: Integer read FCount write FCount;
    property CreateTime: Extended read FCreate_time write FCreate_time;
    property Description: string read FDescription write FDescription;
    property Followers: Extended read FFollowers write FFollowers;
    property Genres: TArray<TVkAudioGenres> read FGenres write FGenres;
    property Id: Integer read FId write FId;
    property MainArtists: TArray<TVkAudioArtist> read FMain_artists write FMain_artists;
    property Original: TVkAudioOriginal read FOriginal write FOriginal;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Photo: TVkAlbumThumb read FPhoto write FPhoto;
    property Plays: Extended read FPlays write FPlays;
    property Title: string read FTitle write FTitle;
    property&Type: Extended read FType write FType;
    property UpdateTime: Extended read FUpdate_time write FUpdate_time;
    property Year: Extended read FYear write FYear;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioPlaylist;
  end;

  TVkPlaylists = class
  private
    FItems: TArray<TVkAudioPlaylist>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkAudioPlaylist> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(AItems: TVkPlaylists);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPlaylists;
  end;

implementation

{TVkAudioOriginal}

function TVkAudioOriginal.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioOriginal.FromJsonString(AJsonString: string): TVkAudioOriginal;
begin
  result := TJson.JsonToObject<TVkAudioOriginal>(AJsonString)
end;

{TVkAudioGenres}

function TVkAudioGenres.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioGenres.FromJsonString(AJsonString: string): TVkAudioGenres;
begin
  result := TJson.JsonToObject<TVkAudioGenres>(AJsonString)
end;

{TVkAudioPlaylist}

constructor TVkAudioPlaylist.Create;
begin
  inherited;
  FOriginal := TVkAudioOriginal.Create();
  FPhoto := TVkAlbumThumb.Create();
end;

destructor TVkAudioPlaylist.Destroy;
var
  LgenresItem: TVkAudioGenres;
  Lmain_artistsItem: TVkAudioArtist;
begin

  for LgenresItem in FGenres do
    LgenresItem.Free;
  for Lmain_artistsItem in FMain_artists do
    Lmain_artistsItem.Free;

  FOriginal.Free;
  FPhoto.Free;
  inherited;
end;

function TVkAudioPlaylist.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioPlaylist.FromJsonString(AJsonString: string): TVkAudioPlaylist;
begin
  result := TJson.JsonToObject<TVkAudioPlaylist>(AJsonString)
end;

{ TVkPlaylists }

procedure TVkPlaylists.Append(AItems: TVkPlaylists);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(AItems.Items));
  Move(AItems.Items[0], FItems[OldLen], Length(AItems.Items) * SizeOf(TVkAudio));
end;

constructor TVkPlaylists.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkPlaylists.Destroy;
var
  LItemsItem: TVkAudioPlaylist;
begin
  if not FSaveObjects then
  begin
    for LItemsItem in FItems do
      LItemsItem.Free;
  end;

  inherited;
end;

class function TVkPlaylists.FromJsonString(AJsonString: string): TVkPlaylists;
begin
  result := TJson.JsonToObject<TVkPlaylists>(AJsonString);
end;

procedure TVkPlaylists.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

function TVkPlaylists.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

