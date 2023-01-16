unit VK.Entity.Playlist;

interface

uses
  Generics.Collections, VK.Wrap.Interceptors, REST.JsonReflect, VK.Entity.Audio,
  VK.Entity.Common, VK.Entity.Common.List, VK.Entity.Photo, VK.Types;

type
  TVkAudioOriginal = class
  private
    FAccess_key: string;
    FOwner_id: TVkPeerId;
    FPlaylist_id: Integer;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property PlaylistId: Integer read FPlaylist_id write FPlaylist_id;
  end;

  TVkAudioGenreObject = TVkBasicObject;

  TVkAudioGenreObjects = TArray<TVkAudioGenreObject>;

  TVkAudioPlaylistPermissions = class
  private
    FBoom_download: Boolean;
    FDelete: Boolean;
    FEdit: Boolean;
    FFollow: Boolean;
    FPlay: Boolean;
    FSave_as_copy: Boolean;
    FShare: Boolean;
  public
    property BoomDownload: Boolean read FBoom_download write FBoom_download;
    property Delete: Boolean read FDelete write FDelete;
    property Edit: Boolean read FEdit write FEdit;
    property Follow: Boolean read FFollow write FFollow;
    property Play: Boolean read FPlay write FPlay;
    property SaveAsCopy: Boolean read FSave_as_copy write FSave_as_copy;
    property Share: Boolean read FShare write FShare;
  end;

  TVkAudioPlaylistMeta = class
  private
    FView: string;
  public
    /// <summary>
    /// compact, ...
    /// </summary>
    property View: string read FView write FView;
  end;

  TVkAudioPlaylistPhoto = class(TVkEntity)
  private
    FHeight: Integer;
    FPhoto_1200: string;
    FPhoto_135: string;
    FPhoto_270: string;
    FPhoto_300: string;
    FPhoto_34: string;
    FPhoto_600: string;
    FPhoto_68: string;
    FWidth: Integer;
  public
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property Photo1200: string read FPhoto_1200 write FPhoto_1200;
    property Photo135: string read FPhoto_135 write FPhoto_135;
    property Photo270: string read FPhoto_270 write FPhoto_270;
    property Photo300: string read FPhoto_300 write FPhoto_300;
    property Photo34: string read FPhoto_34 write FPhoto_34;
    property Photo600: string read FPhoto_600 write FPhoto_600;
    property Photo68: string read FPhoto_68 write FPhoto_68;
  end;

  TVkAudioPlaylist = class(TVkObject)
  private
    FAccess_key: string;
    FAlbum_type: string;
    FCount: Integer;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FCreate_time: TDateTime;
    FDescription: string;
    FFollowers: Integer;
    FGenres: TArray<TVkAudioGenreObject>;
    FMain_artists: TArray<TVkAudioArtist>;
    FOriginal: TVkAudioOriginal;
    FOwner_id: TVkPeerId;
    FPhoto: TVkAudioPlaylistPhoto;
    FPlays: Integer;
    FTitle: string;
    FType: Integer;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FUpdate_time: TDateTime;
    FYear: Integer;
    FIs_following: Boolean;
    FIs_explicit: Boolean;
    FPermissions: TVkAudioPlaylistPermissions;
    FSubtitle_badge: Boolean;
    FPlay_button: Boolean;
    FSubtitle: string;
    FMeta: TVkAudioPlaylistMeta;
  public
    property Id;
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// playlist, main_only..
    /// </summary>
    property AlbumType: string read FAlbum_type write FAlbum_type;
    property Count: Integer read FCount write FCount;
    property CreateTime: TDateTime read FCreate_time write FCreate_time;
    property Description: string read FDescription write FDescription;
    property Followers: Integer read FFollowers write FFollowers;
    property Genres: TArray<TVkAudioGenreObject> read FGenres write FGenres;
    property IsExplicit: Boolean read FIs_explicit write FIs_explicit;
    property IsFollowing: Boolean read FIs_following write FIs_following;
    property MainArtists: TArray<TVkAudioArtist> read FMain_artists write FMain_artists;
    property Original: TVkAudioOriginal read FOriginal write FOriginal;
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property Photo: TVkAudioPlaylistPhoto read FPhoto write FPhoto;
    property Plays: Integer read FPlays write FPlays;
    property Title: string read FTitle write FTitle;
    property UpdateTime: TDateTime read FUpdate_time write FUpdate_time;
    property Year: Integer read FYear write FYear;
    property &Type: Integer read FType write FType;
    property Permissions: TVkAudioPlaylistPermissions read FPermissions write FPermissions;
    property SubtitleBadge: Boolean read FSubtitle_badge write FSubtitle_badge;
    property PlayButton: Boolean read FPlay_button write FPlay_button;
    property Subtitle: string read FSubtitle write FSubtitle;
    property Meta: TVkAudioPlaylistMeta read FMeta write FMeta;
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
  TArrayHelp.FreeArrayOfObject<TVkAudioGenreObject>(FGenres);
  TArrayHelp.FreeArrayOfObject<TVkAudioArtist>(FMain_artists);
  if Assigned(FOriginal) then
    FOriginal.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FPermissions) then
    FPermissions.Free;
  if Assigned(FMeta) then
    FMeta.Free;
  {$ENDIF}
  inherited;
end;

end.

