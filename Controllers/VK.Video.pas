unit VK.Video;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Video, System.JSON, VK.Entity.Status, VK.Entity.Media,
  VK.Entity.Video.Save;

type
  TVkParamsVideoGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ����������� �� �������� ����� �������
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// ����������, ���������� �� ���������� � ���������� ����������� ����� ��� �������� ������������
    /// </summary>
    function Extended(Value: Boolean): Integer;
    /// <summary>
    /// ���������� ������������ ������������
    /// </summary>
    function Count(Value: Integer): Integer;
    /// <summary>
    /// �������� ������������ ������ ��������� ����������� ��� ������� ������������� ������������
    /// </summary>
    function Offset(Value: Integer): Integer;
    /// <summary>
    /// ������������� ����� ������� �������������� � ������ ����� ���� �������������
    /// id �������������, ������� ����������� �����������, � id ����� ������������.
    /// ���� ����������� ����������� ����������, �� � �������� ������� ��������� ������������ -id ����������
    /// �������: 4363_136089719, 13245770_137352259, 1_129207899_220df2876123d3542f, 6492_135055734_e0a9bcc31144f67fbd
    /// </summary>
    function Videos(Value: TArrayOfString): Integer;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsVideoGetAlbums = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������� (������������ ��� ����������).
    /// �� ��������� � ������������� �������� ������������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ���������� ��������, ���������� � ������� ����� �������. �� ���������: 50, ������������ ��������: 100
    /// </summary>
    function Count(Value: Integer = 50): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ��������. �� ���������: 0
    /// </summary>
    function Offset(Value: Integer = 0): Integer;
    /// <summary>
    /// True � ���������� �������������� ���� Count, UpdatedTime � ������ �������� Image ��� ������� �������.
    /// ���� ������ ������, �� ������ �������� Image ��� ���� ��������� �� �����. �� ���������: False
    /// </summary>
    function Extended(Value: Boolean): Integer;
    /// <summary>
    /// True � ���������� ��������� �������
    /// </summary>
    function NeedSystem(Value: Boolean): Integer;
  end;

  TVkParamsVideoAddToAlbum = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �����������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �������, � ������� ����� �������� �����
    /// </summary>
    function TargetId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, � ������� ����� �������� �����.
    /// ��� ���������� ����� � ����� ������ ������������ ����������� -2
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// �������������� ��������, � ������� ����� �������� �����
    /// </summary>
    function AlbumIds(Value: TIdList): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoCreateComment = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// ����� �����������. ������������ ��������, ���� �� ����� �������� Attachments
    /// </summary>
    function Message(Value: string): Integer;
    /// <summary>
    /// ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(Value: TAttachmentArray): Integer;
    /// <summary>
    /// ���� �������� �����������, ���� owner_id ������ 0 (����������� � ����������� ������).
    /// True � ����������� ����� ����������� �� ����� ������,
    /// False � ����������� ����� ����������� �� ����� ������������. �� ���������: False
    /// </summary>
    function FromGroup(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, � ����� �� ������� ������ ���� �������� ����� �����������
    /// </summary>
    function ReplyToComment(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StickerId(Value: Integer): Integer;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ����������� �����������
    /// </summary>
    function Guid(Value: string): Integer;
  end;

  TVkParamsVideoEdit = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// ����� �������� ��� �����������
    /// </summary>
    function Name(Value: string): Integer;
    /// <summary>
    /// ����� �������� ��� �����������
    /// </summary>
    function Desc(Value: string): Integer;
    /// <summary>
    /// ��������� ����������� ��������� ����������� � ����������� �������.
    /// ����������� �������� ��� ������������, ������� ������������ �������� � �������
    /// </summary>
    function PrivacyView(Value: TArrayOfString): Integer;
    /// <summary>
    /// ��������� ����������� ��������������� ����������� � ����������� �������.
    /// ����������� �������� ��� ������������, ������� ������������ �������� � �������
    /// </summary>
    function PrivacyComment(Value: TArrayOfString): Integer;
    /// <summary>
    /// ������� ����������� (��� ����� �� ���������)
    /// </summary>
    function NoComments(Value: Boolean): Integer;
    /// <summary>
    /// ������������ ��������������� �����������
    /// </summary>
    function &Repeat(Value: Boolean): Integer;
  end;

  TVkParamsVideoEditAlbum = record
    List: TParams;
    /// <summary>
    /// ������������� ���������� (���� ����� ��������������� ������, ������������� ����������)
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// ����� �������� ��� �������
    /// </summary>
    function Title(Value: string): Integer;
    /// <summary>
    /// ������� ������� � ������� � ����������� �������.
    /// ����������� �������� ��� �������� � ����� � ������� ������������
    /// </summary>
    function Privacy(Value: TArrayOfString): Integer;
  end;

  TVkParamsVideoEditComment = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function CommentId(Value: Integer): Integer;
    /// <summary>
    /// ����� ����� �����������. ������������ ��������, ���� �� ����� �������� Attachments
    /// </summary>
    function Message(Value: string): Integer;
    /// <summary>
    /// ����� ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(Value: TAttachmentArray): Integer;
  end;

  TVkParamsVideoGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� Likes. �� ��������� ���� Likes �� ������������
    /// </summary>
    function NeedLikes(Value: Boolean): Integer;
    /// <summary>
    /// ������������� �����������, ������� � �������� ����� ������� ������
    /// </summary>
    function StartCommentId(Value: Integer): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������������. �� ���������: 0
    /// </summary>
    function Offset(Value: Integer = 0): Integer;
    /// <summary>
    /// ���������� ������������, ���������� � ������� ���������� �������
    /// </summary>
    function Count(Value: Integer = 20): Integer;
    /// <summary>
    /// ������� ���������� ������������
    /// </summary>
    function Sort(Value: TVkSort): Integer;
    /// <summary>
    /// True � ���������� �������������� ���� Count, UpdatedTime � ������ �������� Image ��� ������� �������.
    /// ���� ������ ������, �� ������ �������� image ��� ���� ��������� �� �����. �� ���������: False
    /// </summary>
    function Extended(Value: Boolean): Integer;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsVideoRemoveFromAlbum = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function TargetId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, �� �������� ����� ������ �����
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// �������������� ��������, �� ������� ����� ������ �����
    /// </summary>
    function AlbumIds(Value: TIdList): Integer;
    /// <summary>
    /// ������������� ��������� �����������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoReorderAlbums = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ������� ����� �����������
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ����� ������� ����� ��������� �������
    /// </summary>
    function Before(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ����� �������� ����� ��������� �������
    /// </summary>
    function After(Value: Integer): Integer;
  end;

  TVkParamsVideoReorderVideos = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, � ���� ������� ����� ����������� �����
    /// </summary>
    function TargetId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ������� � ������������, ������� ����� �����������
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �����������, ������� ����� ����������� (������������ ��� ����������)
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, ������� ����� �����������
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �����������, ����� ������� ������� ��������� ������� (������������ ��� ����������)
    /// </summary>
    function BeforeOwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, ����� ������� ������� ��������� �������
    /// </summary>
    function BeforeVideoId(Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �����������, ����� ������� ������� ��������� ������� (������������ ��� ����������)
    /// </summary>
    function AfterOwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, ����� ������� ������� ��������� �������
    /// </summary>
    function AfterVideoId(Value: Integer): Integer;
  end;

  TVkParamsVideoReport = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(Value: Integer): Integer;
    /// <summary>
    /// ��� ������
    /// </summary>
    function Reason(Value: TVkMediaReportReason): Integer;
    /// <summary>
    /// ����������� ��� ������
    /// </summary>
    function Comment(Value: string): Integer;
    /// <summary>
    /// ��������� ������, ���� ����������� ���� ������� ����� �����
    /// </summary>
    function SearchQuery(Value: string): Integer;
  end;

  TVkParamsVideoSave = record
    List: TParams;
    /// <summary>
    /// �������� ����������
    /// </summary>
    function Name(Value: string): Integer;
    /// <summary>
    /// �������� ����������
    /// </summary>
    function Description(Value: string): Integer;
    /// <summary>
    /// ����������� True, ���� ����� ����������� ��� �������� ������ ����������.
    /// ����� �������� � ���� ���������� ����������� �� ����� ������������
    /// � ������ ������������ ������������ � �� ����� �������� ������
    /// ������������� �� �� ��������������. �� ���������: False
    /// </summary>
    function IsPrivate(Value: Boolean = False): Integer;
    /// <summary>
    /// ��������� �� ����� ���������� ������������ ������ � ����� �� ����� (True � ���������, False � �� ���������).
    /// �������� ��������, ��� ���������� ������ �� ����� ���������� ������ ����� ����� wall
    /// </summary>
    function Wallpost(Value: Boolean): Integer;
    /// <summary>
    /// Url ��� ����������� ����� � �������� �����, ��������, � Youtube.
    /// � ���� ������ ����� ������� ���������� UploadUrl, �� ���������� ����,
    /// ���������� ������ ���������� �� ����� ������
    /// </summary>
    function Link(Value: string): Integer;
    /// <summary>
    /// ������������� ����������, � ������� ����� �������� ���������.
    /// �� ��������� ���� ����������� �� �������� �������� ������������
    /// </summary>
    function GroupId(Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, � ������� ����� �������� ����� ����
    /// </summary>
    function AlbumId(Value: Integer): Integer;
    /// <summary>
    /// ��������� ����������� ��������� ����������� � ����������� �������. ����������� �������� ��� ������������, ������� ������������ �������� � �������
    /// </summary>
    function PrivacyView(Value: TArrayOfString): Integer;
    /// <summary>
    /// ��������� ����������� ��������������� ����������� � ����������� �������.
    /// ����������� �������� ��� ������������, ������� ������������ �������� � �������
    /// </summary>
    function PrivacyComment(Value: TArrayOfString): Integer;
    /// <summary>
    /// True � ������� ����������� (��� ����� �� ���������). �� ���������: False
    /// </summary>
    function NoComments(Value: Boolean = False): Integer;
    /// <summary>
    /// ������������ ��������������� �����������
    /// </summary>
    function &Repeat(Value: Boolean): Integer;
    /// <summary>
    /// ������� �����
    /// </summary>
    function Compression(Value: Boolean): Integer;
  end;

  TVkParamsVideoSearch = record
    List: TParams;
    /// <summary>
    /// ������ ���������� �������
    /// </summary>
    function Query(Value: string): Integer;
    /// <summary>
    /// ���������� �����������
    /// </summary>
    function Sort(Value: TVkMediaSort): Integer;
    /// <summary>
    /// ����� ������������ ������ �� ������������ �������� ��������
    /// </summary>
    function HD(Value: Boolean): Integer;
    /// <summary>
    /// ��������� ������ ����������� �����
    /// </summary>
    function Adult(Value: Boolean): Integer;
    /// <summary>
    /// ������ ���������, �� ������� ��������� ������������� �����
    /// </summary>
    function Filters(Value: TVkVideosFilters): Integer;
    /// <summary>
    /// True � ������ �� ������������ ������������, False � �� ������ �� ������������ ������������. �� ���������: False
    /// </summary>
    function SearchOwn(Value: Boolean = False): Integer;
    /// <summary>
    /// �������� ������������ ������ ��������� ����������� ��� ������� ������������� ������������
    /// </summary>
    function Offset(Value: Integer): Integer;
    /// <summary>
    /// ���������� ������, ����������� ������� �������� ���������� �������
    /// </summary>
    function Longer(Value: Integer): Integer;
    /// <summary>
    /// ���������� ������, ����������� ������ �������� ���������� �������
    /// </summary>
    function Shorter(Value: Integer): Integer;
    /// <summary>
    /// ���������� ������������ ������������
    /// �������� �������� � ���� ��� ������������� ��������� Offset ��� ��������� ���������� �������� ������ ������ 1000 �����������
    /// </summary>
    function Count(Value: Integer = 20): Integer;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups, ���������� ���������� � ������������� � �����������. �� ���������: False
    /// </summary>
    function Extended(Value: Boolean): Integer;
  end;

  TVideoController = class(TVkController)
  public
    /// <summary>
    /// ��������� ����������� � ������ ������������.
    /// </summary>
    function Add(const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ������ ������ ������������.
    /// </summary>
    function AddAlbum(var AlbumId: Integer; Title: string; Privacy: TArrayOfString = []; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ��������� �������� ����������� � ������.
    /// </summary>
    function AddToAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ����������� � ������.
    /// </summary>
    function AddToAlbum(Params: TVkParamsVideoAddToAlbum): Boolean; overload;
    /// <summary>
    /// C������ ����� ����������� � �����������
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// C������ ����� ����������� � �����������
    /// </summary>
    function CreateComment(var CommentId: Integer; Params: TVkParamsVideoCreateComment): Boolean; overload;
    /// <summary>
    /// ������� ����������� �� �������� ������������.
    /// </summary>
    function Delete(const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ������ ������������.
    /// </summary>
    function DeleteAlbum(const AlbumId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ����������� � �����������.
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// C������ ����� ����������� � �����������
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// C������ ����� ����������� � �����������
    /// </summary>
    function Edit(Params: TVkParamsVideoEdit): Boolean; overload;
    /// <summary>
    /// ����������� ������ � �����.
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� ������ � �����.
    /// </summary>
    function EditAlbum(Params: TVkParamsVideoEditAlbum): Boolean; overload;
    /// <summary>
    /// �������� ����� ����������� � �����������.
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ����� ����������� � �����������.
    /// </summary>
    function EditComment(Params: TVkParamsVideoEditComment): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ������������.
    /// </summary>
    function Get(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ������������.
    /// </summary>
    function Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean; overload;
    /// <summary>
    /// ��������� �������� ���������� �� ������� � �����.
    /// </summary>
    function GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ��������� �������� ���������� �� ������� � �����.
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ���������� �� ������� � �����.
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������, � ������� ��������� �����������.
    /// </summary>
    function GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � �����������.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � �����������.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsVideoGetComments): Boolean; overload;
    /// <summary>
    /// ��������� ������ ����������� �� �������.
    /// </summary>
    function RemoveFromAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ������ ����������� �� �������.
    /// </summary>
    function RemoveFromAlbum(Params: TVkParamsVideoRemoveFromAlbum): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������� �������� � �����.
    /// </summary>
    function ReorderAlbums(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������� �������� � �����.
    /// </summary>
    function ReorderAlbums(Params: TVkParamsVideoReorderAlbums): Boolean; overload;
    /// <summary>
    /// ��������� ����������� ����������� � �������.
    /// </summary>
    function ReorderVideos(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ����������� ����������� � �������.
    /// </summary>
    function ReorderVideos(Params: TVkParamsVideoReorderVideos): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� �����������.
    /// </summary>
    function Report(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� �����������.
    /// </summary>
    function Report(Params: TVkParamsVideoReport): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� ����������� � �����������.
    /// </summary>
    function ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean; overload;
    /// <summary>
    /// ��������������� ��������� �����������.
    /// </summary>
    function Restore(VideoId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ��������������� ��������� ����������� � �����������.
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ���������� ����� �������, ����������� ��� ��������, � ������ �����������.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ����� �������, ����������� ��� ��������, � ������ �����������.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TVkParamsVideoSave): Boolean; overload;
    /// <summary>
    /// ���������� ����� �������, ����������� ��� ��������, � ������ �����������.
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ������������ � �������� ��������� ������.
    /// </summary>
    function Search(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ������������ � �������� ��������� ������.
    /// </summary>
    function Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TVideoController }

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.getAlbums', Params).GetObject<TVkVideoAlbums>(Items);
end;

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean;
begin
  Result := GetAlbums(Items, Params.List);
end;

function TVideoController.GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Params.Add('extended', True);
  Result := Handler.Execute('video.getAlbumsByVideo', Params).GetObject<TVkVideoAlbums>(Items);
end;

function TVideoController.GetComments(var Items: TVkComments; Params: TVkParamsVideoGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TVideoController.RemoveFromAlbum(Params: TVkParamsVideoRemoveFromAlbum): Boolean;
begin
  Result := RemoveFromAlbum(Params.List);
end;

function TVideoController.ReorderAlbums(Params: TVkParamsVideoReorderAlbums): Boolean;
begin
  Result := ReorderAlbums(Params.List);
end;

function TVideoController.ReorderVideos(Params: TVkParamsVideoReorderVideos): Boolean;
begin
  Result := ReorderVideos(Params.List);
end;

function TVideoController.Report(Params: TVkParamsVideoReport): Boolean;
begin
  Result := Report(Params.List);
end;

function TVideoController.ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  Params.Add('reason', Reason.ToConst.ToString);
  Result := Handler.Execute('video.reportComment', Params).ResponseIsTrue;
end;

function TVideoController.Restore(VideoId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.restore', Params).ResponseIsTrue;
end;

function TVideoController.RestoreComment(CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.restoreComment', Params).ResponseIsTrue;
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Params: TVkParamsVideoSave): Boolean;
begin
  Result := Save(VideoSaved, Params.List);
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.save', Params).GetObject<TVkVideoSaved>(VideoSaved);
end;

function TVideoController.Report(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.report', Params).ResponseIsTrue;
end;

function TVideoController.ReorderVideos(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.reorderVideos', Params).ResponseIsTrue;
end;

function TVideoController.ReorderAlbums(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.reorderAlbums', Params).ResponseIsTrue;
end;

function TVideoController.RemoveFromAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.removeFromAlbum', Params).ResponseIsTrue;
end;

function TVideoController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.getComments', Params).GetObject<TVkComments>(Items);
end;

function TVideoController.GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.getAlbumById', Params).GetObject<TVkVideoAlbum>(Item);
end;

function TVideoController.Get(var Items: TVkVideos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.get', Params).GetObject<TVkVideos>(Items);
end;

function TVideoController.Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TVideoController.Add(const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Result := Handler.Execute('video.add', Params).ResponseIsTrue;
end;

function TVideoController.AddAlbum(var AlbumId: Integer; Title: string; Privacy: TArrayOfString; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('title', Title);
  if Length(Privacy) > 0 then
    Params.Add('privacy', Privacy);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('video.addAlbum', Params).ResponseAsInt(AlbumId);
end;

function TVideoController.AddToAlbum(Params: TVkParamsVideoAddToAlbum): Boolean;
begin
  Result := AddToAlbum(Params.List);
end;

function TVideoController.CreateComment(var CommentId: Integer; Params: TVkParamsVideoCreateComment): Boolean;
begin
  Result := CreateComment(CommentId, Params.List);
end;

function TVideoController.Delete(const VideoId, OwnerId: Integer; TargetId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('video_id', VideoId);
  Params.Add('owner_id', OwnerId);
  if TargetId <> 0 then
    Params.Add('target_id', TargetId);
  Result := Handler.Execute('video.delete', Params).ResponseIsTrue;
end;

function TVideoController.DeleteAlbum(const AlbumId: Integer; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('video.deleteAlbum', Params).ResponseIsTrue;
end;

function TVideoController.DeleteComment(const CommentId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.deleteComment', Params).ResponseIsTrue;
end;

function TVideoController.Edit(Params: TVkParamsVideoEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TVideoController.EditAlbum(Params: TVkParamsVideoEditAlbum): Boolean;
begin
  Result := EditAlbum(Params.List);
end;

function TVideoController.EditComment(Params: TVkParamsVideoEditComment): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TVideoController.EditComment(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.editComment', Params).ResponseIsTrue;
end;

function TVideoController.EditAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.editAlbum', Params).ResponseIsTrue;
end;

function TVideoController.Edit(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.edit', Params).ResponseIsTrue;
end;

function TVideoController.CreateComment(var CommentId: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.createComment', Params).ResponseAsInt(CommentId);
end;

function TVideoController.AddToAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.addToAlbum', Params).ResponseIsTrue;
end;

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean;
var
  Params: TParams;
  SaveResp: string;
begin
  Params.Add('link', Link);
  Result := Handler.Execute('video.save', Params).GetObject<TVkVideoSaved>(VideoSaved);
  if Result then
  begin
    Result := False;
    if TCustomVK(VK).Upload(VideoSaved.UploadUrl, [''], SaveResp) then
      Result := not SaveResp.IsEmpty
    else
      TCustomVK(VK).DoError(Self, TVkException.Create(SaveResp), -1, SaveResp);
  end;
end;

function TVideoController.Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TVideoController.Search(var Items: TVkVideos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.search', Params).GetObject<TVkVideos>(Items);
end;

{ TVkVideosGetParams }

function TVkParamsVideoGet.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGet.Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsVideoGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoGet.Videos(Value: TArrayOfString): Integer;
begin
  Result := List.Add('videos', Value.ToString);
end;

{ TVkParamsVideoAlbumsGet }

function TVkParamsVideoGetAlbums.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGetAlbums.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGetAlbums.NeedSystem(Value: Boolean): Integer;
begin
  Result := List.Add('need_system', Value);
end;

function TVkParamsVideoGetAlbums.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGetAlbums.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsVideosAddToAlbum }

function TVkParamsVideoAddToAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoAddToAlbum.AlbumIds(Value: TIdList): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsVideoAddToAlbum.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoAddToAlbum.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoAddToAlbum.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

{ TVkParamsVideosCreateComment }

function TVkParamsVideoCreateComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoCreateComment.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoCreateComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsVideoCreateComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToStrings);
end;

function TVkParamsVideoCreateComment.FromGroup(Value: Integer): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsVideoCreateComment.ReplyToComment(Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkParamsVideoCreateComment.StickerId(Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsVideoCreateComment.Guid(Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

{ TVkParamsVideosEdit }

function TVkParamsVideoEdit.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoEdit.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoEdit.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsVideoEdit.Desc(Value: string): Integer;
begin
  Result := List.Add('desc', Value);
end;

function TVkParamsVideoEdit.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsVideoEdit.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsVideoEdit.NoComments(Value: Boolean): Integer;
begin
  Result := List.Add('no_comments', Value);
end;

function TVkParamsVideoEdit.&Repeat(Value: Boolean): Integer;
begin
  Result := List.Add('repeat', Value);
end;

{ TVkParamsVideosEditAlbum }

function TVkParamsVideoEditAlbum.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsVideoEditAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoEditAlbum.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsVideoEditAlbum.Privacy(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy', Value);
end;

{ TVkParamsVideosEditComment }

function TVkParamsVideoEditComment.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoEditComment.CommentId(Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsVideoEditComment.Message(Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsVideoEditComment.Attachments(Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToStrings);
end;

{ TVkParamsVideoGetComments }

function TVkParamsVideoGetComments.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoGetComments.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoGetComments.NeedLikes(Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsVideoGetComments.StartCommentId(Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsVideoGetComments.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGetComments.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGetComments.Sort(Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsVideoGetComments.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGetComments.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

{ TVkParamsVideoRemoveFromAlbum }

function TVkParamsVideoRemoveFromAlbum.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.AlbumIds(Value: TIdList): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsVideoRemoveFromAlbum.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

{ TVkParamsVideoReorderAlbums }

function TVkParamsVideoReorderAlbums.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReorderAlbums.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoReorderAlbums.Before(Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsVideoReorderAlbums.After(Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

{ TVkParamsVideoReorderVideos }

function TVkParamsVideoReorderVideos.TargetId(Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoReorderVideos.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoReorderVideos.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReorderVideos.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoReorderVideos.BeforeOwnerId(Value: Integer): Integer;
begin
  Result := List.Add('before_owner_id', Value);
end;

function TVkParamsVideoReorderVideos.BeforeVideoId(Value: Integer): Integer;
begin
  Result := List.Add('before_video_id', Value);
end;

function TVkParamsVideoReorderVideos.AfterOwnerId(Value: Integer): Integer;
begin
  Result := List.Add('after_owner_id', Value);
end;

function TVkParamsVideoReorderVideos.AfterVideoId(Value: Integer): Integer;
begin
  Result := List.Add('after_video_id', Value);
end;

{ TVkParamsVideosReport }

function TVkParamsVideoReport.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReport.VideoId(Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoReport.Reason(Value: TVkMediaReportReason): Integer;
begin
  Result := List.Add('reason', Value.ToConst.ToString);
end;

function TVkParamsVideoReport.Comment(Value: string): Integer;
begin
  Result := List.Add('comment', Value);
end;

function TVkParamsVideoReport.SearchQuery(Value: string): Integer;
begin
  Result := List.Add('search_query', Value);
end;

{ TVkParamsVideosSave }

function TVkParamsVideoSave.Name(Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsVideoSave.Description(Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsVideoSave.IsPrivate(Value: Boolean): Integer;
begin
  Result := List.Add('is_private', Value);
end;

function TVkParamsVideoSave.Wallpost(Value: Boolean): Integer;
begin
  Result := List.Add('wallpost', Value);
end;

function TVkParamsVideoSave.Link(Value: string): Integer;
begin
  Result := List.Add('link', Value);
end;

function TVkParamsVideoSave.GroupId(Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsVideoSave.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoSave.PrivacyView(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsVideoSave.PrivacyComment(Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsVideoSave.NoComments(Value: Boolean): Integer;
begin
  Result := List.Add('no_comments', Value);
end;

function TVkParamsVideoSave.&Repeat(Value: Boolean): Integer;
begin
  Result := List.Add('repeat', Value);
end;

function TVkParamsVideoSave.Compression(Value: Boolean): Integer;
begin
  Result := List.Add('compression', Value);
end;

{ TVkParamsVideosSearch }

function TVkParamsVideoSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsVideoSearch.Sort(Value: TVkMediaSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsVideoSearch.HD(Value: Boolean): Integer;
begin
  Result := List.Add('hd', Value);
end;

function TVkParamsVideoSearch.Adult(Value: Boolean): Integer;
begin
  Result := List.Add('adult', Value);
end;

function TVkParamsVideoSearch.Filters(Value: TVkVideosFilters): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsVideoSearch.SearchOwn(Value: Boolean): Integer;
begin
  Result := List.Add('search_own', Value);
end;

function TVkParamsVideoSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoSearch.Longer(Value: Integer): Integer;
begin
  Result := List.Add('longer', Value);
end;

function TVkParamsVideoSearch.Shorter(Value: Integer): Integer;
begin
  Result := List.Add('shorter', Value);
end;

function TVkParamsVideoSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoSearch.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

end.

