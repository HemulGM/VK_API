unit VK.Entity.Audio;

interface

uses
  Generics.Collections, Rest.Json, VK.Types;

type
  TVkAudioArtist = class
  private
    FDomain: string;
    FId: string;
    FName: string;
  public
    property domain: string read FDomain write FDomain;
    property id: string read FId write FId;
    property name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioArtist;
  end;

  TVkAlbumThumb = class
  private
    FHeight: Extended;
    FPhoto_135: string;
    FPhoto_270: string;
    FPhoto_300: string;
    FPhoto_34: string;
    FPhoto_600: string;
    FPhoto_68: string;
    FWidth: Extended;
  public
    property height: Extended read FHeight write FHeight;
    property photo_135: string read FPhoto_135 write FPhoto_135;
    property photo_270: string read FPhoto_270 write FPhoto_270;
    property photo_300: string read FPhoto_300 write FPhoto_300;
    property photo_34: string read FPhoto_34 write FPhoto_34;
    property photo_600: string read FPhoto_600 write FPhoto_600;
    property photo_68: string read FPhoto_68 write FPhoto_68;
    property width: Extended read FWidth write FWidth;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAlbumThumb;
  end;

  TVkAudioAlbum = class
  private
    FAccess_key: string;
    FId: Extended;
    FOwner_id: Extended;
    FThumb: TVkAlbumThumb;
    FTitle: string;
  public
    property access_key: string read FAccess_key write FAccess_key;
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property thumb: TVkAlbumThumb read FThumb write FThumb;
    property title: string read FTitle write FTitle;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioAlbum;
  end;

  TVkAudioAds = class
  private
    FAccount_age_type: string;
    FContent_id: string;
    FDuration: string;
    FPuid22: string;
  public
    property account_age_type: string read FAccount_age_type write FAccount_age_type;
    property content_id: string read FContent_id write FContent_id;
    property duration: string read FDuration write FDuration;
    property puid22: string read FPuid22 write FPuid22;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioAds;
  end;

  TVkAudio = class
  private
    FAccess_key: string;
    FAds: TVkAudioAds;
    FAlbum: TVkAudioAlbum;
    FArtist: string;
    FDate: Extended;
    FDuration: Extended;
    FGenre_id: Integer;
    FId: Extended;
    FIs_licensed: Boolean;
    FMain_artists: TArray<TVkAudioArtist>;
    FOwner_id: Extended;
    FTitle: string;
    FTrack_code: string;
    FUrl: string;
    FLyrics_id: Integer;
    FAlbum_id: Integer;
    FNo_search: Integer;
    FIs_hq: Integer;
    function GetAudioGenre: TAudioGenre;
    procedure SetAudioGenre(const Value: TAudioGenre);
  public
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property artist: string read FArtist write FArtist;
    property title: string read FTitle write FTitle;
    property duration: Extended read FDuration write FDuration;
    property url: string read FUrl write FUrl;
    property lyrics_id: Integer read FLyrics_id write FLyrics_id;
    property album_id: Integer read FAlbum_id write FAlbum_id;
    property genre_id: Integer read FGenre_id write FGenre_id;
    property date: Extended read FDate write FDate;
    property no_search: Integer read FNo_search write FNo_search;
    property is_hq: Integer read FIs_hq write FIs_hq;
    //
    property access_key: string read FAccess_key write FAccess_key;
    property ads: TVkAudioAds read FAds write FAds;
    property is_licensed: Boolean read FIs_licensed write FIs_licensed;
    property track_code: string read FTrack_code write FTrack_code;
    property album: TVkAudioAlbum read FAlbum write FAlbum;
    property main_artists: TArray<TVkAudioArtist> read FMain_artists write FMain_artists;
    //
    property Genre: TAudioGenre read GetAudioGenre write SetAudioGenre;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudio;
  end;

implementation

{TVkAudioAds}

function TVkAudioAds.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioAds.FromJsonString(AJsonString: string): TVkAudioAds;
begin
  result := TJson.JsonToObject<TVkAudioAds>(AJsonString)
end;

{TVkAudio}

constructor TVkAudio.Create;
begin
  inherited;
  FAds := TVkAudioAds.Create();
  FAlbum := TVkAudioAlbum.Create;
end;

destructor TVkAudio.Destroy;
var
  Artist: TVkAudioArtist;
begin
  for Artist in FMain_artists do
    Artist.Free;
  FAlbum.Free;
  FAds.Free;
  inherited;
end;

function TVkAudio.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudio.FromJsonString(AJsonString: string): TVkAudio;
begin
  result := TJson.JsonToObject<TVkAudio>(AJsonString)
end;

function TVkAudio.GetAudioGenre: TAudioGenre;
begin
  Result := AudioGenre.Create(FGenre_Id);
end;

procedure TVkAudio.SetAudioGenre(const Value: TAudioGenre);
begin
  FGenre_id := VkAudioGenres[Value];
end;

{TMain_artistsClass}

function TVkAudioArtist.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioArtist.FromJsonString(AJsonString: string): TVkAudioArtist;
begin
  result := TJson.JsonToObject<TVkAudioArtist>(AJsonString)
end;

{TThumbClass}

function TVkAlbumThumb.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAlbumThumb.FromJsonString(AJsonString: string): TVkAlbumThumb;
begin
  result := TJson.JsonToObject<TVkAlbumThumb>(AJsonString)
end;

{TAlbumClass}

constructor TVkAudioAlbum.Create;
begin
  inherited;
  FThumb := TVkAlbumThumb.Create();
end;

destructor TVkAudioAlbum.Destroy;
begin
  FThumb.Free;
  inherited;
end;

function TVkAudioAlbum.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioAlbum.FromJsonString(AJsonString: string): TVkAudioAlbum;
begin
  result := TJson.JsonToObject<TVkAudioAlbum>(AJsonString)
end;

end.

