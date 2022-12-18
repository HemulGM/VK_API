unit VK.Entity.Playlist;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect,
  VK.Entity.Audio, VK.Entity.Common, VK.Entity.Common.List, VK.Entity.Photo;

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

  TVkAudioGenres = TVkBasicObject;

  TVkAudioPlaylist = class(TVkObject)
  private
    FAccess_key: string;
    FAlbum_type: string;
    FCount: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FCreate_time: TDateTime;
    FDescription: string;
    FFollowers: Integer;
    FGenres: TArray<TVkAudioGenres>;
    FMain_artists: TArray<TVkAudioArtist>;
    FOriginal: TVkAudioOriginal;
    FOwner_id: Integer;
    FPhoto: TVkPhoto;
    FPlays: Integer;
    FTitle: string;
    FType: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FUpdate_time: TDateTime;
    FYear: Integer;
    FIs_following: Boolean;
    FIs_explicit: Boolean;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property AlbumType: string read FAlbum_type write FAlbum_type;
    property Count: Integer read FCount write FCount;
    property CreateTime: TDateTime read FCreate_time write FCreate_time;
    property Description: string read FDescription write FDescription;
    property Followers: Integer read FFollowers write FFollowers;
    property Genres: TArray<TVkAudioGenres> read FGenres write FGenres;
    property IsExplicit: Boolean read FIs_explicit write FIs_explicit;
    property IsFollowing: Boolean read FIs_following write FIs_following;
    property MainArtists: TArray<TVkAudioArtist> read FMain_artists write FMain_artists;
    property Original: TVkAudioOriginal read FOriginal write FOriginal;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Plays: Integer read FPlays write FPlays;
    property Title: string read FTitle write FTitle;
    property UpdateTime: TDateTime read FUpdate_time write FUpdate_time;
    property Year: Integer read FYear write FYear;
    property &Type: Integer read FType write FType;
    destructor Destroy; override;
  end;

  TVkPlaylists = TVkEntityList<TVkAudioPlaylist>;

implementation

uses
  VK.CommonUtils;

{TVkAudioPlaylist}

destructor TVkAudioPlaylist.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkAudioGenres>(FGenres);
  TArrayHelp.FreeArrayOfObject<TVkAudioArtist>(FMain_artists);
  if Assigned(FOriginal) then
    FOriginal.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  {$ENDIF}
  inherited;
end;

end.

