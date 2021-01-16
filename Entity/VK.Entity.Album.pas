unit VK.Entity.Album;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Photo, VK.Entity.Common,
  VK.Entity.Attachment, VK.Entity.Privacy, VK.Entity.Common.List;

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
  end;

  TVkPhotoAlbum = class(TVkObject, IAttachment)
  private
    FCreated: Int64;
    FDescription: string;
    FOwner_id: Integer;
    FSize: Integer;
    FThumb: TVkAlbumThumb;
    FTitle: string;
    FUpdated: Int64;
    FThumb_id: integer;
    FThumb_is_last: Integer;
    FPrivacy_view: TVkPrivacy;
    FPrivacy_comment: TVkPrivacy;
    FSizes: TVkSizes;
    FThumb_src: string;
    FUpload_by_admins_only: Boolean;
    FComments_disabled: Boolean;
    FCan_upload: Boolean;
    FAccess_key: string;
    function GetCreated: TDateTime;
    function GetUpdated: TDateTime;
    procedure SetCreated(const Value: TDateTime);
    procedure SetUpdated(const Value: TDateTime);
  public
    property Created: TDateTime read GetCreated write SetCreated;
    property Description: string read FDescription write FDescription;
    property ThumbId: integer read FThumb_id write FThumb_id;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Size: Integer read FSize write FSize;
    property Thumb: TVkAlbumThumb read FThumb write FThumb;
    property Sizes: TVkSizes read FSizes write FSizes;
    property Title: string read FTitle write FTitle;
    property ThumbSrc: string read FThumb_src write FThumb_src;
    property Updated: TDateTime read GetUpdated write SetUpdated;
    property ThumbIsLast: Integer read FThumb_is_last write FThumb_is_last;
    property PrivacyView: TVkPrivacy read FPrivacy_view write FPrivacy_view;
    property PrivacyComment: TVkPrivacy read FPrivacy_comment write FPrivacy_comment;
    property UploadByAdminsOnly: Boolean read FUpload_by_admins_only write FUpload_by_admins_only;
    property CommentsDisabled: Boolean read FComments_disabled write FComments_disabled;
    property CanUpload: Boolean read FCan_upload write FCan_upload;
    property AccessKey: string read FAccess_key write FAccess_key;
    constructor Create; override;
    destructor Destroy; override;
    function ToAttachment: string;
  end;

  TVkPhotoAlbums = TVkEntityList<TVkPhotoAlbum>;

implementation

uses
  System.DateUtils, VK.Types, VK.CommonUtils;

{TVkPhotoAlbum}

constructor TVkPhotoAlbum.Create;
begin
  inherited;
  FThumb := TVkAlbumThumb.Create();
  FPrivacy_view := TVkPrivacy.Create();
  FPrivacy_comment := TVkPrivacy.Create();
end;

destructor TVkPhotoAlbum.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
  FThumb.Free;
  FPrivacy_view.Free;
  FPrivacy_comment.Free;
  inherited;
end;

function TVkPhotoAlbum.ToAttachment: string;
begin
  Result := Attachment.Album(Id, OwnerId, AccessKey);
end;

function TVkPhotoAlbum.GetCreated: TDateTime;
begin
  Result := UnixToDateTime(FCreated, False);
end;

function TVkPhotoAlbum.GetUpdated: TDateTime;
begin
  Result := UnixToDateTime(FUpdated, False);
end;

procedure TVkPhotoAlbum.SetCreated(const Value: TDateTime);
begin
  FCreated := DateTimeToUnix(Value, False);
end;

procedure TVkPhotoAlbum.SetUpdated(const Value: TDateTime);
begin
  FUpdated := DateTimeToUnix(Value, False);
end;

end.

