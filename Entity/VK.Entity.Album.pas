unit VK.Entity.Album;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo, VK.Entity.Common, VK.Entity.Privacy;

type
  TVkAlbumThumb = class
  private
    FHeight: Integer;
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
    property Photo135: string read FPhoto_135 write FPhoto_135;
    property Photo270: string read FPhoto_270 write FPhoto_270;
    property Photo300: string read FPhoto_300 write FPhoto_300;
    property Photo34: string read FPhoto_34 write FPhoto_34;
    property Photo600: string read FPhoto_600 write FPhoto_600;
    property Photo68: string read FPhoto_68 write FPhoto_68;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAlbumThumb;
  end;

  TVkPhotoAlbum = class
  private
    FCreated: Extended;
    FDescription: string;
    FId: integer;
    FOwner_id: Integer;
    FSize: Integer;
    FThumb: TVkAlbumThumb;
    FTitle: string;
    FUpdated: Extended;
    FThumb_id: integer;
    FThumb_is_last: Integer;
    FPrivacy_view: TVkPrivacy;
    FPrivacy_comment: TVkPrivacy;
    FSizes: TVkSizes;
    FThumb_src: string;
  public
    property Created: Extended read FCreated write FCreated;
    property Description: string read FDescription write FDescription;
    property Id: integer read FId write FId;
    property ThumbId: integer read FThumb_id write FThumb_id;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Size: Integer read FSize write FSize;
    property Thumb: TVkAlbumThumb read FThumb write FThumb;
    property Sizes: TVkSizes read FSizes write FSizes;
    property Title: string read FTitle write FTitle;
    property ThumbSrc: string read FThumb_src write FThumb_src;
    property Updated: Extended read FUpdated write FUpdated;
    property ThumbIsLast: Integer read FThumb_is_last write FThumb_is_last;
    property PrivacyView: TVkPrivacy read FPrivacy_view write FPrivacy_view;
    property PrivacyComment: TVkPrivacy read FPrivacy_comment write FPrivacy_comment;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhotoAlbum;
  end;

  TVkPhotoAlbums = class
  private
    FCount: Integer;
    FItems: TArray<TVkPhotoAlbum>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkPhotoAlbum> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPhotoAlbums;
  end;

implementation

{TVkPhotoAlbum}

constructor TVkPhotoAlbum.Create;
begin
  inherited;
  FThumb := TVkAlbumThumb.Create();
  FPrivacy_view := TVkPrivacy.Create();
  FPrivacy_comment := TVkPrivacy.Create();
end;

destructor TVkPhotoAlbum.Destroy;
var
  LsizesItem: TVkSize;
begin

  for LsizesItem in FSizes do
    LsizesItem.Free;
  FThumb.Free;
  FPrivacy_view.Free;
  FPrivacy_comment.Free;
  inherited;
end;

function TVkPhotoAlbum.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPhotoAlbum.FromJsonString(AJsonString: string): TVkPhotoAlbum;
begin
  result := TJson.JsonToObject<TVkPhotoAlbum>(AJsonString)
end;

{TVkAlbumThumb}

function TVkAlbumThumb.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAlbumThumb.FromJsonString(AJsonString: string): TVkAlbumThumb;
begin
  result := TJson.JsonToObject<TVkAlbumThumb>(AJsonString)
end;

{TVkPhotoAlbums}

destructor TVkPhotoAlbums.Destroy;
var
  LitemsItem: TVkPhotoAlbum;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkPhotoAlbums.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPhotoAlbums.FromJsonString(AJsonString: string): TVkPhotoAlbums;
begin
  result := TJson.JsonToObject<TVkPhotoAlbums>(AJsonString)
end;

end.

