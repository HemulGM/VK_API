unit VK.Video;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Video, System.JSON,
  VK.Entity.Status, VK.Entity.Media, VK.Entity.Video.Save;

type
  TVkParamsVideoGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ����������� �� �������� ����� �������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ����������, ���������� �� ���������� � ���������� ����������� ����� ��� �������� ������������
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// ���������� ������������ ������������
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// �������� ������������ ������ ��������� ����������� ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����� ������� �������������� � ������ ����� ���� �������������
    /// id �������������, ������� ����������� �����������, � id ����� ������������.
    /// ���� ����������� ����������� ����������, �� � �������� ������� ��������� ������������ -id ����������
    /// �������: 4363_136089719, 13245770_137352259, 1_129207899_220df2876123d3542f, 6492_135055734_e0a9bcc31144f67fbd
    /// </summary>
    function Videos(const Value: TArrayOfString): Integer;
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
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ��������, ���������� � ������� ����� �������. �� ���������: 50, ������������ ��������: 100
    /// </summary>
    function Count(const Value: Integer = 50): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ��������. �� ���������: 0
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// True � ���������� �������������� ���� Count, UpdatedTime � ������ �������� Image ��� ������� �������.
    /// ���� ������ ������, �� ������ �������� Image ��� ���� ��������� �� �����. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// True � ���������� ��������� �������
    /// </summary>
    function NeedSystem(const Value: Boolean): Integer;
  end;

  TVkParamsVideoAddToAlbum = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �������, � ������� ����� �������� �����
    /// </summary>
    function TargetId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, � ������� ����� �������� �����.
    /// ��� ���������� ����� � ����� ������ ������������ ����������� -2
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// �������������� ��������, � ������� ����� �������� �����
    /// </summary>
    function AlbumIds(const Value: TIdList): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(const Value: Integer): Integer;
  end;

  TVkParamsVideoCreateComment = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(const Value: Integer): Integer;
    /// <summary>
    /// ����� �����������. ������������ ��������, ���� �� ����� �������� Attachments
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer;
    /// <summary>
    /// ���� �������� �����������, ���� owner_id ������ 0 (����������� � ����������� ������).
    /// True � ����������� ����� ����������� �� ����� ������,
    /// False � ����������� ����� ����������� �� ����� ������������. �� ���������: False
    /// </summary>
    function FromGroup(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, � ����� �� ������� ������ ���� �������� ����� �����������
    /// </summary>
    function ReplyToComment(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StickerId(const Value: Integer): Integer;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ����������� �����������
    /// </summary>
    function Guid(const Value: string): Integer;
  end;

  TVkParamsVideoEdit = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(const Value: Integer): Integer;
    /// <summary>
    /// ����� �������� ��� �����������
    /// </summary>
    function Name(const Value: string): Integer;
    /// <summary>
    /// ����� �������� ��� �����������
    /// </summary>
    function Desc(const Value: string): Integer;
    /// <summary>
    /// ��������� ����������� ��������� ����������� � ����������� �������.
    /// ����������� �������� ��� ������������, ������� ������������ �������� � �������
    /// </summary>
    function PrivacyView(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ��������� ����������� ��������������� ����������� � ����������� �������.
    /// ����������� �������� ��� ������������, ������� ������������ �������� � �������
    /// </summary>
    function PrivacyComment(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ������� ����������� (��� ����� �� ���������)
    /// </summary>
    function NoComments(const Value: Boolean): Integer;
    /// <summary>
    /// ������������ ��������������� �����������
    /// </summary>
    function &Repeat(const Value: Boolean): Integer;
  end;

  TVkParamsVideoEditAlbum = record
    List: TParams;
    /// <summary>
    /// ������������� ���������� (���� ����� ��������������� ������, ������������� ����������)
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ����� �������� ��� �������
    /// </summary>
    function Title(const Value: string): Integer;
    /// <summary>
    /// ������� ������� � ������� � ����������� �������.
    /// ����������� �������� ��� �������� � ����� � ������� ������������
    /// </summary>
    function Privacy(const Value: TArrayOfString): Integer;
  end;

  TVkParamsVideoEditComment = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ����� �����������. ������������ ��������, ���� �� ����� �������� Attachments
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ����� ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer;
  end;

  TVkParamsVideoGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(const Value: Integer): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� Likes. �� ��������� ���� Likes �� ������������
    /// </summary>
    function NeedLikes(const Value: Boolean): Integer;
    /// <summary>
    /// ������������� �����������, ������� � �������� ����� ������� ������
    /// </summary>
    function StartCommentId(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������������. �� ���������: 0
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// ���������� ������������, ���������� � ������� ���������� �������
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// ������� ���������� ������������
    /// </summary>
    function Sort(const Value: TVkSort): Integer;
    /// <summary>
    /// True � ���������� �������������� ���� Count, UpdatedTime � ������ �������� Image ��� ������� �������.
    /// ���� ������ ������, �� ������ �������� image ��� ���� ��������� �� �����. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean): Integer;
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
    function TargetId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, �� �������� ����� ������ �����
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// �������������� ��������, �� ������� ����� ������ �����
    /// </summary>
    function AlbumIds(const Value: TIdList): Integer;
    /// <summary>
    /// ������������� ��������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(const Value: Integer): Integer;
  end;

  TVkParamsVideoReorderAlbums = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ������� ����� �����������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ����� ������� ����� ��������� �������
    /// </summary>
    function Before(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ����� �������� ����� ��������� �������
    /// </summary>
    function After(const Value: Integer): Integer;
  end;

  TVkParamsVideoReorderVideos = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, � ���� ������� ����� ����������� �����
    /// </summary>
    function TargetId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������� � ������������, ������� ����� �����������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �����������, ������� ����� ����������� (������������ ��� ����������)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, ������� ����� �����������
    /// </summary>
    function VideoId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �����������, ����� ������� ������� ��������� ������� (������������ ��� ����������)
    /// </summary>
    function BeforeOwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, ����� ������� ������� ��������� �������
    /// </summary>
    function BeforeVideoId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ��������� �����������, ����� ������� ������� ��������� ������� (������������ ��� ����������)
    /// </summary>
    function AfterOwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, ����� ������� ������� ��������� �������
    /// </summary>
    function AfterVideoId(const Value: Integer): Integer;
  end;

  TVkParamsVideoReport = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function VideoId(const Value: Integer): Integer;
    /// <summary>
    /// ��� ������
    /// </summary>
    function Reason(const Value: TVkMediaReportReason): Integer;
    /// <summary>
    /// ����������� ��� ������
    /// </summary>
    function Comment(const Value: string): Integer;
    /// <summary>
    /// ��������� ������, ���� ����������� ���� ������� ����� �����
    /// </summary>
    function SearchQuery(const Value: string): Integer;
  end;

  TVkParamsVideoSave = record
    List: TParams;
    /// <summary>
    /// �������� ����������
    /// </summary>
    function Name(const Value: string): Integer;
    /// <summary>
    /// �������� ����������
    /// </summary>
    function Description(const Value: string): Integer;
    /// <summary>
    /// ����������� True, ���� ����� ����������� ��� �������� ������ ����������.
    /// ����� �������� � ���� ���������� ����������� �� ����� ������������
    /// � ������ ������������ ������������ � �� ����� �������� ������
    /// ������������� �� �� ��������������. �� ���������: False
    /// </summary>
    function IsPrivate(const Value: Boolean = False): Integer;
    /// <summary>
    /// ��������� �� ����� ���������� ������������ ������ � ����� �� ����� (True � ���������, False � �� ���������).
    /// �������� ��������, ��� ���������� ������ �� ����� ���������� ������ ����� ����� wall
    /// </summary>
    function Wallpost(const Value: Boolean): Integer;
    /// <summary>
    /// Url ��� ����������� ����� � �������� �����, ��������, � Youtube.
    /// � ���� ������ ����� ������� ���������� UploadUrl, �� ���������� ����,
    /// ���������� ������ ���������� �� ����� ������
    /// </summary>
    function Link(const Value: string): Integer;
    /// <summary>
    /// ������������� ����������, � ������� ����� �������� ���������.
    /// �� ��������� ���� ����������� �� �������� �������� ������������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, � ������� ����� �������� ����� ����
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ��������� ����������� ��������� ����������� � ����������� �������. ����������� �������� ��� ������������, ������� ������������ �������� � �������
    /// </summary>
    function PrivacyView(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ��������� ����������� ��������������� ����������� � ����������� �������.
    /// ����������� �������� ��� ������������, ������� ������������ �������� � �������
    /// </summary>
    function PrivacyComment(const Value: TArrayOfString): Integer;
    /// <summary>
    /// True � ������� ����������� (��� ����� �� ���������). �� ���������: False
    /// </summary>
    function NoComments(const Value: Boolean = False): Integer;
    /// <summary>
    /// ������������ ��������������� �����������
    /// </summary>
    function &Repeat(const Value: Boolean): Integer;
    /// <summary>
    /// ������� �����
    /// </summary>
    function Compression(const Value: Boolean): Integer;
  end;

  TVkParamsVideoSearch = record
    List: TParams;
    /// <summary>
    /// ������ ���������� �������
    /// </summary>
    function Query(const Value: string): Integer;
    /// <summary>
    /// ���������� �����������
    /// </summary>
    function Sort(const Value: TVkMediaSort): Integer;
    /// <summary>
    /// ����� ������������ ������ �� ������������ �������� ��������
    /// </summary>
    function HD(const Value: Boolean): Integer;
    /// <summary>
    /// ��������� ������ ����������� �����
    /// </summary>
    function Adult(const Value: Boolean): Integer;
    /// <summary>
    /// ������ ���������, �� ������� ��������� ������������� �����
    /// </summary>
    function Filters(const Value: TVkVideosFilters): Integer;
    /// <summary>
    /// True � ������ �� ������������ ������������, False � �� ������ �� ������������ ������������
    /// </summary>
    function SearchOwn(const Value: Boolean = False): Integer;
    /// <summary>
    /// �������� ������������ ������ ��������� ����������� ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ������, ����������� ������� �������� ���������� �������
    /// </summary>
    function Longer(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ������, ����������� ������ �������� ���������� �������
    /// </summary>
    function Shorter(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ������������ ������������
    /// �������� �������� � ���� ��� ������������� ��������� Offset ��� ��������� ���������� �������� ������ ������ 1000 �����������
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups, ���������� ���������� � ������������� � �����������. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean): Integer;
  end;

  TVideoController = class(TVkController)
  public
    /// <summary>
    /// ��������� ����������� � ������ ������������
    /// </summary>
    function Add(const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ������ ������ ������������
    /// </summary>
    function AddAlbum(var AlbumId: Integer; Title: string; Privacy: TVkPrivacySettings; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ������� ������ ������ ������������
    /// </summary>
    function AddAlbum(var AlbumId: Integer; Title: string; GroupId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ��������� �������� ����������� � ������
    /// </summary>
    function AddToAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ����������� � ������
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
    /// ������� ����������� �� �������� ������������
    /// </summary>
    function Delete(const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ������ ������������
    /// </summary>
    function DeleteAlbum(const AlbumId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ����������� � �����������
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
    /// ����������� ������ � �����
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� ������ � �����
    /// </summary>
    function EditAlbum(Params: TVkParamsVideoEditAlbum): Boolean; overload;
    /// <summary>
    /// �������� ����� ����������� � �����������
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ����� ����������� � �����������
    /// </summary>
    function EditComment(Params: TVkParamsVideoEditComment): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ������������
    /// </summary>
    function Get(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� � ������������
    /// </summary>
    function Get(var Items: TVkVideos; Params: TVkParamsVideoGet): Boolean; overload;
    /// <summary>
    /// ��������� �������� ���������� �� ������� � �����
    /// </summary>
    function GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ��������� �������� ���������� �� ������� � �����
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ���������� �� ������� � �����
    /// </summary>
    function GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoGetAlbums): Boolean; overload;
    /// <summary>
    /// ���������� ������ ��������, � ������� ��������� �����������
    /// </summary>
    function GetAlbumsByVideo(var Items: TVkVideoAlbums; const VideoId, OwnerId: Integer; TargetId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � �����������
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � �����������
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsVideoGetComments): Boolean; overload;
    /// <summary>
    /// ��������� ������ ����������� �� �������
    /// </summary>
    function RemoveFromAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ������ ����������� �� �������
    /// </summary>
    function RemoveFromAlbum(Params: TVkParamsVideoRemoveFromAlbum): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������� �������� � �����
    /// </summary>
    function ReorderAlbums(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������� �������� � �����
    /// </summary>
    function ReorderAlbums(Params: TVkParamsVideoReorderAlbums): Boolean; overload;
    /// <summary>
    /// ��������� ����������� ����������� � �������
    /// </summary>
    function ReorderVideos(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ����������� ����������� � �������
    /// </summary>
    function ReorderVideos(Params: TVkParamsVideoReorderVideos): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� �����������
    /// </summary>
    function Report(Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� �����������
    /// </summary>
    function Report(Params: TVkParamsVideoReport): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� ����������� � �����������
    /// </summary>
    function ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean; overload;
    /// <summary>
    /// ��������������� ��������� �����������
    /// </summary>
    function Restore(VideoId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ��������������� ��������� ����������� � �����������
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ���������� ����� �������, ����������� ��� ��������, � ������ �����������
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ����� �������, ����������� ��� ��������, � ������ �����������
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Params: TVkParamsVideoSave): Boolean; overload;
    /// <summary>
    /// ���������� ����� �������, ����������� ��� ��������, � ������ �����������
    /// </summary>
    function Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ������������ � �������� ��������� ������
    /// </summary>
    function Search(var Items: TVkVideos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ������������ � �������� ��������� ������
    /// </summary>
    function Search(var Items: TVkVideos; Params: TVkParamsVideoSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TVideoController }

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.getAlbums', Params).GetObject(Items);
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
  Result := Handler.Execute('video.getAlbumsByVideo', Params).GetObject(Items);
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
  Params.Add('reason', Ord(Reason).ToString);
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
  Result := Handler.Execute('video.save', Params).GetObject(VideoSaved);
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
  Result := Handler.Execute('video.getComments', Params).GetObject(Items);
end;

function TVideoController.GetAlbumById(var Item: TVkVideoAlbum; AlbumId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('video.getAlbumById', Params).GetObject(Item);
end;

function TVideoController.Get(var Items: TVkVideos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('video.get', Params).GetObject(Items);
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

function TVideoController.AddAlbum(var AlbumId: Integer; Title: string; Privacy: TVkPrivacySettings; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('title', Title);
  if not Privacy.IsEmpty then
    Params.Add('privacy', Privacy.ToString);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('video.addAlbum', Params).ResponseAsInt(AlbumId);
end;

function TVideoController.AddAlbum(var AlbumId: Integer; Title: string; GroupId: Integer): Boolean;
var
  Privacy: TVkPrivacySettings;
begin
  Result := AddAlbum(AlbumId, Title, Privacy, GroupId);
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
  Result := Handler.Execute('video.search', Params).GetObject(Items);
end;

{ TVkVideosGetParams }

function TVkParamsVideoGet.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGet.Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsVideoGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGet.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoGet.Videos(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('videos', Value);
end;

{ TVkParamsVideoAlbumsGet }

function TVkParamsVideoGetAlbums.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGetAlbums.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGetAlbums.NeedSystem(const Value: Boolean): Integer;
begin
  Result := List.Add('need_system', Value);
end;

function TVkParamsVideoGetAlbums.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGetAlbums.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsVideosAddToAlbum }

function TVkParamsVideoAddToAlbum.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoAddToAlbum.AlbumIds(const Value: TIdList): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsVideoAddToAlbum.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoAddToAlbum.TargetId(const Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoAddToAlbum.VideoId(const Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

{ TVkParamsVideosCreateComment }

function TVkParamsVideoCreateComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoCreateComment.VideoId(const Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoCreateComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsVideoCreateComment.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsVideoCreateComment.FromGroup(const Value: Integer): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsVideoCreateComment.ReplyToComment(const Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkParamsVideoCreateComment.StickerId(const Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

function TVkParamsVideoCreateComment.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

{ TVkParamsVideosEdit }

function TVkParamsVideoEdit.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoEdit.VideoId(const Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoEdit.Name(const Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsVideoEdit.Desc(const Value: string): Integer;
begin
  Result := List.Add('desc', Value);
end;

function TVkParamsVideoEdit.PrivacyView(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsVideoEdit.PrivacyComment(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsVideoEdit.NoComments(const Value: Boolean): Integer;
begin
  Result := List.Add('no_comments', Value);
end;

function TVkParamsVideoEdit.&Repeat(const Value: Boolean): Integer;
begin
  Result := List.Add('repeat', Value);
end;

{ TVkParamsVideosEditAlbum }

function TVkParamsVideoEditAlbum.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsVideoEditAlbum.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoEditAlbum.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsVideoEditAlbum.Privacy(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy', Value);
end;

{ TVkParamsVideosEditComment }

function TVkParamsVideoEditComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoEditComment.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsVideoEditComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsVideoEditComment.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

{ TVkParamsVideoGetComments }

function TVkParamsVideoGetComments.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoGetComments.VideoId(const Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoGetComments.NeedLikes(const Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsVideoGetComments.StartCommentId(const Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsVideoGetComments.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoGetComments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoGetComments.Sort(const Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsVideoGetComments.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoGetComments.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

{ TVkParamsVideoRemoveFromAlbum }

function TVkParamsVideoRemoveFromAlbum.TargetId(const Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.AlbumIds(const Value: TIdList): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsVideoRemoveFromAlbum.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoRemoveFromAlbum.VideoId(const Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

{ TVkParamsVideoReorderAlbums }

function TVkParamsVideoReorderAlbums.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReorderAlbums.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoReorderAlbums.Before(const Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsVideoReorderAlbums.After(const Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

{ TVkParamsVideoReorderVideos }

function TVkParamsVideoReorderVideos.TargetId(const Value: Integer): Integer;
begin
  Result := List.Add('target_id', Value);
end;

function TVkParamsVideoReorderVideos.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoReorderVideos.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReorderVideos.VideoId(const Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoReorderVideos.BeforeOwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('before_owner_id', Value);
end;

function TVkParamsVideoReorderVideos.BeforeVideoId(const Value: Integer): Integer;
begin
  Result := List.Add('before_video_id', Value);
end;

function TVkParamsVideoReorderVideos.AfterOwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('after_owner_id', Value);
end;

function TVkParamsVideoReorderVideos.AfterVideoId(const Value: Integer): Integer;
begin
  Result := List.Add('after_video_id', Value);
end;

{ TVkParamsVideosReport }

function TVkParamsVideoReport.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideoReport.VideoId(const Value: Integer): Integer;
begin
  Result := List.Add('video_id', Value);
end;

function TVkParamsVideoReport.Reason(const Value: TVkMediaReportReason): Integer;
begin
  Result := List.Add('reason', Ord(Value).ToString);
end;

function TVkParamsVideoReport.Comment(const Value: string): Integer;
begin
  Result := List.Add('comment', Value);
end;

function TVkParamsVideoReport.SearchQuery(const Value: string): Integer;
begin
  Result := List.Add('search_query', Value);
end;

{ TVkParamsVideosSave }

function TVkParamsVideoSave.Name(const Value: string): Integer;
begin
  Result := List.Add('name', Value);
end;

function TVkParamsVideoSave.Description(const Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsVideoSave.IsPrivate(const Value: Boolean): Integer;
begin
  Result := List.Add('is_private', Value);
end;

function TVkParamsVideoSave.Wallpost(const Value: Boolean): Integer;
begin
  Result := List.Add('wallpost', Value);
end;

function TVkParamsVideoSave.Link(const Value: string): Integer;
begin
  Result := List.Add('link', Value);
end;

function TVkParamsVideoSave.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsVideoSave.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideoSave.PrivacyView(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsVideoSave.PrivacyComment(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsVideoSave.NoComments(const Value: Boolean): Integer;
begin
  Result := List.Add('no_comments', Value);
end;

function TVkParamsVideoSave.&Repeat(const Value: Boolean): Integer;
begin
  Result := List.Add('repeat', Value);
end;

function TVkParamsVideoSave.Compression(const Value: Boolean): Integer;
begin
  Result := List.Add('compression', Value);
end;

{ TVkParamsVideosSearch }

function TVkParamsVideoSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsVideoSearch.Sort(const Value: TVkMediaSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsVideoSearch.HD(const Value: Boolean): Integer;
begin
  Result := List.Add('hd', Value);
end;

function TVkParamsVideoSearch.Adult(const Value: Boolean): Integer;
begin
  Result := List.Add('adult', Value);
end;

function TVkParamsVideoSearch.Filters(const Value: TVkVideosFilters): Integer;
begin
  Result := List.Add('filters', Value.ToString);
end;

function TVkParamsVideoSearch.SearchOwn(const Value: Boolean): Integer;
begin
  Result := List.Add('search_own', Value);
end;

function TVkParamsVideoSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoSearch.Longer(const Value: Integer): Integer;
begin
  Result := List.Add('longer', Value);
end;

function TVkParamsVideoSearch.Shorter(const Value: Integer): Integer;
begin
  Result := List.Add('shorter', Value);
end;

function TVkParamsVideoSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoSearch.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

end.

