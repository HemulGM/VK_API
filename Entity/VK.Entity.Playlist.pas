unit VK.Entity.Playlist;

interface

uses
  Generics.Collections, VK.Entity.Audio, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkAudioOriginal = class
  private
    FAccess_key: string;
    FOwner_id: Integer;
    FPlaylist_id: Integer;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property PlaylistId: Integer read FPlaylist_id write FPlaylist_id;
  end;

  TVkAudioGenres = class(TVkObject)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  TVkAudioPlaylist = class(TVkObject)
  private
    FAccess_key: string;
    FAlbum_type: string;
    FCount: Integer;
    FCreate_time: Int64;
    FDescription: string;
    FFollowers: Integer;
    FGenres: TArray<TVkAudioGenres>;
    FMain_artists: TArray<TVkAudioArtist>;
    FOriginal: TVkAudioOriginal;
    FOwner_id: Integer;
    FPhoto: TVkAlbumThumb;
    FPlays: Integer;
    FTitle: string;
    FType: Integer;
    FUpdate_time: Int64;
    FYear: Integer;
    FIs_following: Boolean;
    FIs_explicit: Boolean;
    function GetUpdateTime: TDateTime;
    procedure SetUpdateTime(const Value: TDateTime);
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property AlbumType: string read FAlbum_type write FAlbum_type;
    property Count: Integer read FCount write FCount;
    property IsFollowing: Boolean read FIs_following write FIs_following;
    property IsExplicit: Boolean read FIs_explicit write FIs_explicit;
    property CreateTime: Int64 read FCreate_time write FCreate_time;
    property Description: string read FDescription write FDescription;
    property Followers: Integer read FFollowers write FFollowers;
    property Genres: TArray<TVkAudioGenres> read FGenres write FGenres;
    property MainArtists: TArray<TVkAudioArtist> read FMain_artists write FMain_artists;
    property Original: TVkAudioOriginal read FOriginal write FOriginal;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Photo: TVkAlbumThumb read FPhoto write FPhoto;
    property Plays: Integer read FPlays write FPlays;
    property Title: string read FTitle write FTitle;
    property&Type: Integer read FType write FType;
    property UpdateTime: TDateTime read GetUpdateTime write SetUpdateTime;
    property Year: Integer read FYear write FYear;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkPlaylists = TVkEntityList<TVkAudioPlaylist>;

implementation

uses
  VK.CommonUtils, System.DateUtils;

{TVkAudioPlaylist}

constructor TVkAudioPlaylist.Create;
begin
  inherited;
  FOriginal := TVkAudioOriginal.Create();
  FPhoto := TVkAlbumThumb.Create();
end;

destructor TVkAudioPlaylist.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkAudioGenres>(FGenres);
  TArrayHelp.FreeArrayOfObject<TVkAudioArtist>(FMain_artists);
  FOriginal.Free;
  FPhoto.Free;
  {$ENDIF}
  inherited;
end;

function TVkAudioPlaylist.GetUpdateTime: TDateTime;
begin
  Result := UnixToDateTime(FUpdate_time, False);
end;

procedure TVkAudioPlaylist.SetUpdateTime(const Value: TDateTime);
begin
  FUpdate_time := DateTimeToUnix(Value, False);
end;

end.

