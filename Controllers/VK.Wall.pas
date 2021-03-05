unit VK.Wall;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, System.JSON, VK.Entity.Media,
  VK.Entity.Info;

type
  TVkParamsWallPost = record
    List: TParams;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// True � ����������� � ������ ���������.
    /// False � ����������� � ������ ��������.
    /// </summary>
    function CloseComments(const Value: Boolean = False): Integer;
    /// <summary>
    /// �������� ���������. �������������� ������� � ���������� ������
    /// </summary>
    function Copyright(const Value: string): Integer;
    /// <summary>
    /// ������ ������� � ������� �������� ������ ����� �������� ��� ����� � ������� ����������� VK Donut
    /// </summary>
    function DonutPaidDuration(const Value: TVkDonutPaidDuration): Integer;
    /// <summary>
    /// True � ������ ����� �������� ������ �������, False � ���� �������������.
    /// �� ��������� ����������� ������ �������� ���� �������������
    /// </summary>
    function FriendsOnly(const Value: Boolean = False): Integer;
    /// <summary>
    /// ������ �������� �����������, ���� OwnerId ������ 0 (������ ����������� �� ����� ������).
    /// True � ������ ����� ������������ �� ����� ������,
    /// False � ������ ����� ������������ �� ����� ������������ (�� ���������)
    /// </summary>
    function FromGroup(const Value: Boolean = False): Integer;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ���������� ������.
    /// ��������� � ������� ������ ����
    /// </summary>
    function Guid(const Value: string): Integer;
    /// <summary>
    /// �������������� �������
    /// Lat - ������, �������� � �������� (�� -90 �� 90).
    /// Long - �������, �������� � �������� (�� -180 �� 180).
    /// </summary>
    function LatLong(Lat, Long: Extended): Integer;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������",
    /// False � ����� ��������� �� �����.
    /// � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function MarkAsAds(const Value: Boolean): Integer;
    /// <summary>
    /// ����� ��������� (�������� ������������, ���� �� ����� �������� Attachments)
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// True � ����������� � ������ ���������.
    /// False � ����������� � ������ ��������.
    /// </summary>
    function MuteNotifications(const Value: Boolean): Integer;
    /// <summary>
    /// ������������� ������������ ��� ����������, �� ����� �������� ������ ���� ������������ ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����, � ������� ������� ������������
    /// </summary>
    function PlaceId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������, ������� ���������� ������������.
    /// ������ �������� ������������ ��� ���������� ���������� ������� � ������������ ��������
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// ���� ���������� ������. ���� �������� ������, ���������� ������ ����� �������� �� ���������� �������
    /// </summary>
    function PublishDate(const Value: TDateTime): Integer;
    /// <summary>
    /// ������ �������� ��� ������, �� ������� ���������� �������������� ������,
    /// � ������ ���� ������������ �������� ��������������� �����. ��������, twitter, facebook
    /// </summary>
    function Services(const Value: TArrayOfString): Integer;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� �������
    /// (��� ������������, ������������� ������), False � ������� ��������� �� �����.
    /// �������� ����������� ������ ��� ���������� �� ����� ���������� � �������� ��������� FromGroup.
    /// �� ��������� ������� �� �����������
    /// </summary>
    function Signed(const Value: Boolean = False): Integer;
  end;

  TVkParamsWallEdit = record
    List: TParams;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// True � ����������� � ������ ���������.
    /// False � ����������� � ������ ��������.
    /// </summary>
    function CloseComments(const Value: Boolean): Integer;
    /// <summary>
    /// �������� ���������. �������������� ������� � ���������� ������
    /// </summary>
    function Copyright(const Value: string): Integer;
    /// <summary>
    /// True � ������ ����� �������� ������ �������, False � ���� �������������.
    /// �� ��������� ����������� ������ �������� ���� �������������
    /// </summary>
    function FriendsOnly(const Value: Boolean): Integer;
    /// <summary>
    /// �������������� �������
    /// Lat - ������, �������� � �������� (�� -90 �� 90).
    /// Long - �������, �������� � �������� (�� -180 �� 180).
    /// </summary>
    function LatLong(Lat, Long: Extended): Integer;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������",
    /// False � ����� ��������� �� �����. � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function MarkAsAds(const Value: Boolean): Integer;
    /// <summary>
    /// ����� ��������� (�������� ������������, ���� �� ����� �������� attachments)
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������������� ������������ ��� ����������, �� ����� �������� ������ ���� ������������ ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����, � ������� ������� ������������
    /// </summary>
    function PlaceId(const Value: Integer): Integer;
    function PosterBkgAccessHash(const Value: string): Integer;
    function PosterBkgId(const Value: Integer): Integer;
    function PosterBkgOwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������, ������� ���������� ������������.
    /// ������ �������� ������������ ��� ���������� ���������� ������� � ������������ ��������
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// ���� ���������� ������. ���� �������� ������, ���������� ������ ����� �������� �� ���������� �������
    /// </summary>
    function PublishDate(const Value: TDateTime): Integer;
    /// <summary>
    /// ������ �������� ��� ������, �� ������� ���������� �������������� ������,
    /// � ������ ���� ������������ �������� ��������������� �����. ��������, twitter, facebook
    /// </summary>
    function Services(const Value: TArrayOfString): Integer;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� �������
    /// (��� ������������, ������������� ������), False � ������� ��������� �� �����.
    /// �������� ����������� ������ ��� ���������� �� ����� ���������� � �������� ��������� FromGroup.
    /// �� ��������� ������� �� �����������
    /// </summary>
    function Signed(const Value: Boolean): Integer;
  end;

  TVkParamsWallEditAdsStealth = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ����� (������������� ���������� ����� ��������� �� ������ ������)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ������
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� �������
    /// (��� ������������, ������������� ������), False � ������� ��������� �� �����.
    /// �������� ����������� ������ ��� ���������� �� ����� ���������� � �������� ��������� FromGroup.
    /// �� ��������� ������� �� �����������
    /// </summary>
    function Signed(const Value: Boolean): Integer;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������",
    /// False � ����� ��������� �� �����.
    /// � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function LatLong(Lat, Long: Extended): Integer;
    /// <summary>
    /// ������������� �����, � ������� ������� ������������
    /// </summary>
    function PlaceId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������, ������� ���������� �������� � �������� ��� ������
    /// </summary>
    function LinkButton(const Value: string): Integer;
    /// <summary>
    /// ���������, ������� ������ ���� ����������� ��� ��������.
    /// ���� �� ������, ����� ������������� ������� � ������� ������.
    /// ����������� ��������� � ������, ���� ������ �������� ������� ��������
    /// </summary>
    function LinkTitle(const Value: string): Integer;
    /// <summary>
    /// ������ �� �����������, ������� ������ ���� ������������ ��� ��������.
    /// ����������� ����������: 537x240. ���� �� �������, ����� ������������� ��������� � ������� ������.
    /// ����������� ��������� � ������, ���� ������ �������� ������� ��������.
    /// ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� LinkVideo
    /// </summary>
    function LinkImage(const Value: string): Integer;
    /// <summary>
    /// ������������� ����� � ������� "OwnerId_MediaId".
    /// ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� LinkVideo.
    /// ����� ����, �������� LinkVideo ����� ���� ������ ������ ������ � ����������� LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(const Value: string): Integer; overload;
    /// <summary>
    /// ������������� �����. ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� .
    /// ����� ����, �������� LinkVideo ����� ���� ������ ������ ������ � ����������� LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(OwnerId, MediaId: Integer): Integer; overload;
  end;

  TVkParamsWallEditComment = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �����
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������, ������� ���������� ���������������
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ����� �����������. ������������ ��������, ���� �� ������� �������� Attachments
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
  end;

  TVkParamsWallGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �� ����� ��������
    /// ���������� �������� ������ (�� ��������� � ������� ������������)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// �������� ����� ������������ ��� ����������
    /// </summary>
    function Domain(const Value: string): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ���������� �������, ������� ���������� ��������. ������������ ��������: 100
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// ����������, ����� ���� ������� �� ����� ���������� ��������
    /// </summary>
    function Filter(const Value: TVkPostTypeFilter = TVkPostTypeFilter.All): Integer;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������.
    /// �������� ��������, ���� �������� ����������� ������ ��� Extended = True
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsWallGetById = record
    List: TParams;
    /// <summary>
    /// ������������� ����� ������� ��������������, ������� ������������
    /// ����� ������ ����� ���� ������������� id ���������� ���� � id �����
    /// ������� �� �����. �������� 100 ���������������.
    /// ������ �������� posts:
    /// 93388_21539,93388_20904,-1_340364
    /// </summary>
    function Posts(const Value: TArrayOfString): Integer;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// ���������� ������ ������� copy_history, ������������� � ������, ���� ������ �������� �������� ������ � ������ �����
    /// ��������, copy_history_depth=1 � copy_history ����� ��������� ���� ������� � ����������� � ������, ������ �������� ������� �������� �������.
    /// copy_history_depth=2 � copy_history ����� ��������� ��� ��������, ����������� ���������� � ������, �������� ������� �������� ������ �������, � ��� ����� (��� �������, ��� �������� �������� ��������� ������� ��� ������� ������ ����������).
    /// </summary>
    function CopyHistoryDepth(const Value: Integer = 2): Integer;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � �����, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkParamsWallGetComment = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ����� (��� ��������� � �� ������ ������)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TVkCommentCreateParams = record
    List: TParams;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������������ ��� ����������, �� ���� ����� ��������� ������, � ������� ���������� �������� �����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ����� �����������. ������������ ��������, ���� �� ������� �������� attachments
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������ �������� �����������, ���� OwnerId ������ 0 (������ ����������� �� ����� ������).
    /// True � ������ ����� ������������ �� ����� ������,
    /// False � ������ ����� ������������ �� ����� ������������ (�� ���������)
    /// </summary>
    function FromGroup(const Value: Boolean = False): Integer;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ���������� ������.
    ///  ��������� � ������� ������ ����
    /// </summary>
    function Guid(const Value: string): Integer;
    /// <summary>
    /// ������������� �����������, � ����� �� ������� ������ ���� �������� ����� �����������
    /// </summary>
    function ReplyToComment(const Value: Integer): Integer;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StickerId(const Value: Integer): Integer;
  end;

  TVkParamsWallGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������� (������������ ��� ����������)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PostId(const Value: Integer): Integer;
    /// <summary>
    /// True � ���������� ���������� � ������
    /// </summary>
    function NeedLikes(const Value: Boolean): Integer;
    /// <summary>
    /// ������������� �����������, ������� � �������� ����� ������� ������
    /// </summary>
    function StartCommentId(const Value: Integer): Integer;
    /// <summary>
    /// �����, ����������� ��� ��������� ���������� ������� �����������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ����� ������������, ������� ���������� �������� (������������ ��������: 100)
    /// </summary>
    function Count(const Value: Integer = 10): Integer;
    /// <summary>
    /// ������� ���������� ������������
    /// </summary>
    function Sort(const Value: TVkSort): Integer;
    /// <summary>
    /// ���������� ��������, �� �������� ����� �������� ����� �����������. ������� 0, ���� �� �� ������ �������� �����
    /// </summary>
    function PreviewLength(const Value: Integer): Integer;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
    /// <summary>
    /// ������������� �����������, ����� �������� ����� ��������
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// ������������ ����� ��������� � ���� thread
    /// </summary>
    function ThreadItemsCount(const Value: Integer = 0): Integer;
  end;

  TVkParamsWallPostAdsStealth = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ����� (������������� ���������� ����� ��������� �� ������ ������)
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ������
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): Integer; overload;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� �������
    /// (��� ������������, ������������� ������), False � ������� ��������� �� �����.
    /// �������� ����������� ������ ��� ���������� �� ����� ���������� � �������� ��������� FromGroup.
    /// �� ��������� ������� �� �����������
    /// </summary>
    function Signed(const Value: Boolean): Integer;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������",
    /// False � ����� ��������� �� �����.
    /// � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function LatLong(Lat, Long: Extended): Integer;
    /// <summary>
    /// ������������� �����, � ������� ������� ������������
    /// </summary>
    function PlaceId(const Value: Integer): Integer;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ���������� ������.
    /// ��������� � ������� ������ ����
    /// </summary>
    function Guid(const Value: string): Integer;
    /// <summary>
    /// ������������� ������, ������� ���������� �������� � �������� ��� ������
    /// </summary>
    function LinkButton(const Value: string): Integer; overload;
    /// <summary>
    /// ������������� ������, ������� ���������� �������� � �������� ��� ������
    /// </summary>
    function LinkButton(const Value: TVkPostLinkButton): Integer; overload;
    /// <summary>
    /// ���������, ������� ������ ���� ����������� ��� ��������.
    /// ���� �� ������, ����� ������������� ������� � ������� ������.
    /// ����������� ��������� � ������, ���� ������ �������� ������� ��������
    /// </summary>
    function LinkTitle(const Value: string): Integer;
    /// <summary>
    /// ������ �� �����������, ������� ������ ���� ������������ ��� ��������.
    /// ����������� ����������: 537x240. ���� �� �������, ����� ������������� ��������� � ������� ������.
    /// ����������� ��������� � ������, ���� ������ �������� ������� ��������.
    /// ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� LinkVideo
    /// </summary>
    function LinkImage(const Value: string): Integer;
    /// <summary>
    /// ������������� ����� � ������� "[owner_id]_[media_id]".
    /// ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� LinkVideo.
    /// ����� ����, �������� LinkVideo ����� ���� ������ ������ ������ � ����������� LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(const Value: string): Integer;
  end;

  TVkParamsWallRepost = record
    List: TParams;
    /// <summary>
    /// ��������� ������������� �������, ������� ���������� ���������� �� �����, ��������, wall66748_3675 ��� wall-1_340364.
    /// ����������� �� ���� ������� (wall, photo, video � �.�.), �������������� ��������� ������� � �������������� ������ �������
    /// </summary>
    function &Object(const Value: string): Integer;
    /// <summary>
    /// ���������������� �����, ������� ����� �������� � ������ � ��������
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������������� ����������, �� ����� �������� ����� ��������� ������ � ��������.
    /// ���� �� ������, ������ ����� ��������� �� ����� �������� ������������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������", False � ����� ��������� �� �����. � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function MarkAsAds(const Value: Boolean): Integer;
    /// <summary>
    /// True � ����������� � ������ ���������.
    /// False � ����������� � ������ ��������.
    /// </summary>
    function MuteNotifications(const Value: Boolean): Integer;
  end;

  TVkParamsWallSearch = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// �������� ����� ������������ ��� ����������
    /// </summary>
    function Domain(const Value: string): Integer;
    /// <summary>
    /// ��������� ������. ��� ������� ���������� ������ ���������� ���������� � ������� ��������
    /// </summary>
    function Query(const Value: string): Integer;
    /// <summary>
    /// True � ���������� ������ ������ �� ����� ��������� �����
    /// </summary>
    function OwnersOnly(const Value: Boolean): Integer;
    /// <summary>
    /// ���������� �������, ������� ���������� �������
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ���������� ��� ��������� ������������� ������������ �����������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(UserFields: TVkProfileFields = []; GroupFields: TVkGroupFields = []): Integer;
  end;

  TWallController = class(TVkController)
  public
    /// <summary>
    /// ��������� ������ ��� �������� ���������.
    /// </summary>
    function �heckCopyrightLink(const Link: string): Boolean;
    /// <summary>
    /// ��������� ��������������� ������.
    /// </summary>
    function CloseComments(const OwnerId, PostId: Integer): Boolean;
    /// <summary>
    /// ��������� ����������� � ������ �� �����.
    /// </summary>
    function CreateComment(var CommentInfo: TVkCommentInfo; Params: TVkCommentCreateParams): Boolean; overload;
    /// <summary>
    /// ��������� ����������� � ������ �� �����.
    /// </summary>
    function CreateComment(Params: TVkCommentCreateParams): Boolean; overload;
    /// <summary>
    /// ��������� ����������� � ������ �� �����.
    /// </summary>
    function CreateComment(const PostId: Integer; const Message: string; OwnerId: Integer = 0; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ������� ������ �� �����.
    /// </summary>
    function Delete(const PostId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ����������� � ������ �� �����.
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ����������� ������ �� �����.
    /// </summary>
    function Edit(var PostId: Integer; Params: TVkParamsWallEdit): Boolean; overload;
    /// <summary>
    /// ��������� ��������������� ������� ������.
    /// �������� ������� ������� �������� ������ � ����������� �� ����� ������, ��������� �������� ��� �����������; ������������ ������ �������� ������� �������������� ��� ���������.
    /// </summary>
    function EditAdsStealth(Params: TVkParamsWallEditAdsStealth): Boolean; overload;
    /// <summary>
    /// ����������� ����������� �� �����.
    /// </summary>
    function EditComment(Params: TVkParamsWallEditComment): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������� �� ����� ������������ ��� ����������.
    /// 5000 ������� � �����.
    /// </summary>
    function Get(var Items: TVkPosts; Params: TVkParamsWallGet): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������� �� ����� ������������ ��� ����������.
    /// 5000 ������� � �����.
    /// </summary>
    function Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������� �� ���� ������������� ��� ��������� �� �� ���������������.
    /// </summary>
    function GetById(var Items: TVkPosts; Params: TVkParamsWallGetById): Boolean; overload;
    /// <summary>
    /// �������� ���������� � ����������� �� �����.
    /// </summary>
    function GetComment(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ���������� � ����������� �� �����.
    /// </summary>
    function GetComment(var Items: TVkComments; Params: TVkParamsWallGetComment): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ������ �� �����.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ������ �� �����.
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsWallGetComments): Boolean; overload;
    /// <summary>
    /// ��������� �������� ������ �������� �������� ������.
    /// �������� ��������, �������� ������ �������� ����� ������ ��� ������, ��������� ������� �������������, ��� � ����������, ��� ������� ������������ �������� ���������������.
    /// </summary>
    function GetReposts(var Items: TVkPosts; PostId: Integer; Offset: Integer = 0; Count: Integer = 0; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// �������� ��������������� ������
    /// �������� ������ � ����������� ��������, ��������������� ������� ���� ��������� � ������� wall.closeComments
    /// </summary>
    function OpenComments(const OwnerId, PostId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ������ �� ����� (������ ����� ������������ ���� ���������).
    /// </summary>
    function Pin(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ��������� ������� ������ �� �����, ���������� ������ �� ����� ��������� ��������, ������������ ������������ ���������� ������.
    /// ����� ������� ������������ ������, ���������� �������� � owner_id ������������� ��������� ��������, � ������� ������� ������������ �� �������� �������������.
    /// ��� ���������� ������������ � ���������� ������� ����������� �������� post_id, �������� ��� �������� ����� �������� ������� wall.get � filter=suggests � postponed ��������������.
    /// </summary>
    function Post(var PostId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ������� ������ �� �����, ���������� ������ �� ����� ��������� ��������, ������������ ������������ ���������� ������.
    /// ����� ������� ������������ ������, ���������� �������� � owner_id ������������� ��������� ��������, � ������� ������� ������������ �� �������� �������������.
    /// ��� ���������� ������������ � ���������� ������� ����������� �������� post_id, �������� ��� �������� ����� �������� ������� wall.get � filter=suggests � postponed ��������������.
    /// </summary>
    function Post(const Message: string; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ������� ������ �� �����, ���������� ������ �� ����� ��������� ��������, ������������ ������������ ���������� ������.
    /// ����� ������� ������������ ������, ���������� �������� � owner_id ������������� ��������� ��������, � ������� ������� ������������ �� �������� �������������.
    /// ��� ���������� ������������ � ���������� ������� ����������� �������� post_id, �������� ��� �������� ����� �������� ������� wall.get � filter=suggests � postponed ��������������.
    /// </summary>
    function Post(var PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ������� ������ �� �����, ���������� ������ �� ����� ��������� ��������, ������������ ������������ ���������� ������.
    /// ����� ������� ������������ ������, ���������� �������� � owner_id ������������� ��������� ��������, � ������� ������� ������������ �� �������� �������������.
    /// ��� ���������� ������������ � ���������� ������� ����������� �������� post_id, �������� ��� �������� ����� �������� ������� wall.get � filter=suggests � postponed ��������������.
    /// </summary>
    function Post(const Message: string; OwnerId: Integer; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ������� ������ �� �����, ���������� ������ �� ����� ��������� ��������, ������������ ������������ ���������� ������.
    /// ����� ������� ������������ ������, ���������� �������� � owner_id ������������� ��������� ��������, � ������� ������� ������������ �� �������� �������������.
    /// ��� ���������� ������������ � ���������� ������� ����������� �������� post_id, �������� ��� �������� ����� �������� ������� wall.get � filter=suggests � postponed ��������������.
    /// </summary>
    function Post(var PostId: Integer; Params: TVkParamsWallPost): Boolean; overload;
    /// <summary>
    /// ��������� ������� ������ �� �����, ���������� ������ �� ����� ��������� ��������, ������������ ������������ ���������� ������.
    /// ����� ������� ������������ ������, ���������� �������� � owner_id ������������� ��������� ��������, � ������� ������� ������������ �� �������� �������������.
    /// ��� ���������� ������������ � ���������� ������� ����������� �������� post_id, �������� ��� �������� ����� �������� ������� wall.get � filter=suggests � postponed ��������������.
    /// </summary>
    function Post(Params: TVkParamsWallPost): Boolean; overload;
    /// <summary>
    /// ��������� ������� ������� ������, ������� �� �������� �� ����� ���������� � � ���������� ����� ���� ������������ ��� �������� ���������� ���������� ���� "������ � ����������".
    /// �������� ������� ������� �������� ������ � ����������� �� ����� ������, ��������� �������� ��� �����������; ������������ ������ �������� ������� �������������� ��� ���������. �������� �������� � � ����� ����� ������� �� ����� 100 ������� �������.
    /// </summary>
    function PostAdsStealth(var PostId: Integer; Params: TVkParamsWallPostAdsStealth): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� ����������� � ������.
    /// </summary>
    function ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason = TVkMediaReportReason.Spam): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� ������.
    /// </summary>
    function ReportPost(const OwnerId, PostId: Integer; Reason: TVkMediaReportReason = TVkMediaReportReason.Spam): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� ����������� � ������.
    /// </summary>
    function Repost(var Info: TVkRepostInfo; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� ����������� � ������.
    /// </summary>
    function Repost(var Info: TVkRepostInfo; Params: TVkParamsWallRepost): Boolean; overload;
    /// <summary>
    /// ��������������� ��������� ������ �� ����� ������������ ��� ����������.
    /// </summary>
    function Restore(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ��������������� ��������� ����������� � ������ �� �����.
    /// </summary>
    function RestoreComment(const CommentId: Integer; const OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ��������� ������ ������ �� ����� � ������������ � ��������� ����������.
    /// 1000 ������� � �����
    /// </summary>
    function Search(var Items: TVkPosts; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ������ ������ �� ����� � ������������ � ��������� ����������.
    /// 1000 ������� � �����
    /// </summary>
    function Search(var Items: TVkPosts; Params: TVkParamsWallSearch): Boolean; overload;
    /// <summary>
    /// �������� ����������� ������ �� �����.
    /// </summary>
    function Unpin(const PostId: Integer; const OwnerId: Integer = 0): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils;

{ TWallController }

function TWallController.Post(var PostId: Integer; Message: string; Attachments: TAttachmentArray = []): Boolean;
var
  Params: TVkParamsWallPost;
begin
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  if not Message.IsEmpty then
    Params.Message(Message);
  Result := Post(PostId, Params);
end;

function TWallController.Post(const Message: string; Attachments: TAttachmentArray): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Message, Attachments);
end;

function TWallController.Post(const Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Message, OwnerId, Attachments);
end;

function TWallController.Post(Params: TVkParamsWallPost): Boolean;
var
  PostId: Integer;
begin
  Result := Post(PostId, Params);
end;

function TWallController.PostAdsStealth(var PostId: Integer; Params: TVkParamsWallPostAdsStealth): Boolean;
begin
  Result := Handler.Execute('wall.postAdsStealth', Params.List).GetValue('post_id', PostId);
end;

function TWallController.ReportComment(const OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('owner_id', OwnerId);
  Params.Add('reason', Reason.ToString);
  Result := Handler.Execute('wall.reportComment', Params).ResponseIsTrue;
end;

function TWallController.ReportPost(const OwnerId, PostId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  Params.Add('owner_id', OwnerId);
  Params.Add('reason', Reason.ToString);
  Result := Handler.Execute('wall.reportPost', Params).ResponseIsTrue;
end;

function TWallController.Repost(var Info: TVkRepostInfo; Params: TVkParamsWallRepost): Boolean;
begin
  Result := Repost(Info, Params.List);
end;

function TWallController.Restore(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.restore', Params).ResponseIsTrue;
end;

function TWallController.RestoreComment(const CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.restoreComment', Params).ResponseIsTrue;
end;

function TWallController.Search(var Items: TVkPosts; Params: TVkParamsWallSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TWallController.Unpin(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.unpin', Params).ResponseIsTrue;
end;

function TWallController.Search(var Items: TVkPosts; Params: TParams): Boolean;
begin
  Result := Handler.Execute('wall.search', Params).GetObject(Items);
end;

function TWallController.Repost(var Info: TVkRepostInfo; Params: TParams): Boolean;
begin
  Result := Handler.Execute('wall.repost', Params).GetObject(Info);
end;

function TWallController.Post(var PostId: Integer; Params: TVkParamsWallPost): Boolean;
begin
  Result := Handler.Execute('wall.post', Params.List).GetValue('post_id', PostId);
end;

function TWallController.Post(var PostId: Integer; Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  Params: TVkParamsWallPost;
begin
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  if not Message.IsEmpty then
    Params.Message(Message);
  Params.OwnerId(OwnerId);
  Result := Post(PostId, Params);
end;

function TWallController.CreateComment(var CommentInfo: TVkCommentInfo; Params: TVkCommentCreateParams): Boolean;
begin
  Result := Handler.Execute('wall.createComment', Params.List).GetObject(CommentInfo);
end;

function TWallController.CreateComment(Params: TVkCommentCreateParams): Boolean;
var
  CommentInfo: TVkCommentInfo;
begin
  Result := CreateComment(CommentInfo, Params);
  if Result then
    CommentInfo.Free;
end;

function TWallController.CloseComments(const OwnerId, PostId: Integer): Boolean;
begin
  Result := Handler.Execute('wall.closeComments', [
    ['owner_id', OwnerId.ToString],
    ['post_id', PostId.ToString]]).
    ResponseIsTrue;
end;

function TWallController.CreateComment(const PostId: Integer; const Message: string; OwnerId: Integer; Attachments: TAttachmentArray): Boolean;
var
  CommentInfo: TVkCommentInfo;
  Params: TVkCommentCreateParams;
begin
  Params.PostId(PostId);
  Params.Message(Message);
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  if not Attachments.IsEmpty then
    Params.Attachments(Attachments);
  Result := CreateComment(CommentInfo, Params);
  if Result then
    CommentInfo.Free;
end;

function TWallController.Delete(const PostId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.delete', Params).ResponseIsTrue;
end;

function TWallController.DeleteComment(const CommentId: Integer; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.deleteComment', Params).ResponseIsTrue;
end;

function TWallController.Edit(var PostId: Integer; Params: TVkParamsWallEdit): Boolean;
begin
  Result := Handler.Execute('wall.edit', Params.List).ResponseAsInt(PostId);
end;

function TWallController.EditAdsStealth(Params: TVkParamsWallEditAdsStealth): Boolean;
begin
  Result := Handler.Execute('wall.editAdsStealth', Params.List).ResponseIsTrue;
end;

function TWallController.EditComment(Params: TVkParamsWallEditComment): Boolean;
begin
  Result := Handler.Execute('wall.editComment', Params.List).ResponseIsTrue;
end;

function TWallController.Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: Integer): Boolean;
var
  Params: TVkParamsWallGet;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Params.Offset(Offset);
  Params.Count(Count);
  Result := Get(Items, Params);
end;

function TWallController.GetById(var Items: TVkPosts; Params: TVkParamsWallGetById): Boolean;
begin
  Result := Handler.Execute('wall.getById', Params.List).GetObjects(Items);
end;

function TWallController.GetComment(var Items: TVkComments; Params: TVkParamsWallGetComment): Boolean;
begin
  Result := GetComment(Items, Params.List);
end;

function TWallController.GetComments(var Items: TVkComments; Params: TVkParamsWallGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TWallController.GetReposts(var Items: TVkPosts; PostId, Offset, Count, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if Offset <> 0 then
    Params.Add('offset', Offset);
  if Count <> 0 then
    Params.Add('count', Count);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.getReposts', Params).GetObject(Items);
end;

function TWallController.OpenComments(const OwnerId, PostId: Integer): Boolean;
begin
  Result := Handler.Execute('wall.openComments', [
    ['owner_id', OwnerId.ToString],
    ['post_id', PostId.ToString]]).
    ResponseIsTrue;
end;

function TWallController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('wall.getComments', Params).GetObject(Items);
end;

function TWallController.GetComment(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('wall.getComment', Params).GetObject(Items);
end;

function TWallController.Get(var Items: TVkPosts; Params: TVkParamsWallGet): Boolean;
begin
  Result := Handler.Execute('wall.get', Params.List).GetObject(Items);
end;

function TWallController.Pin(const PostId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.pin', Params).ResponseIsTrue;
end;

function TWallController.�heckCopyrightLink(const Link: string): Boolean;
begin
  Result := Handler.Execute('wall.checkCopyrightLink', ['link', Link]).ResponseIsTrue;
end;

{ TVkWallParams }

function TVkParamsWallPost.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPost.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPost.CloseComments(const Value: Boolean): Integer;
begin
  Result := List.Add('close_comments', Value);
end;

function TVkParamsWallPost.Copyright(const Value: string): Integer;
begin
  Result := List.Add('copyright', Value);
end;

function TVkParamsWallPost.DonutPaidDuration(const Value: TVkDonutPaidDuration): Integer;
begin
  Result := List.Add('donut_paid_duration', Ord(Value));
end;

function TVkParamsWallPost.FriendsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkParamsWallPost.FromGroup(const Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsWallPost.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsWallPost.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallPost.MarkAsAds(const Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallPost.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallPost.MuteNotifications(const Value: Boolean): Integer;
begin
  Result := List.Add('mute_notifications', Value);
end;

function TVkParamsWallPost.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallPost.PlaceId(const Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallPost.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallPost.PublishDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('publish_date', Value);
end;

function TVkParamsWallPost.Services(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('services', Value);
end;

function TVkParamsWallPost.Signed(const Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

{ TVkCommentCreateParams }

function TVkCommentCreateParams.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkCommentCreateParams.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkCommentCreateParams.FromGroup(const Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkCommentCreateParams.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkCommentCreateParams.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkCommentCreateParams.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkCommentCreateParams.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkCommentCreateParams.ReplyToComment(const Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkCommentCreateParams.StickerId(const Value: Integer): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

{ TVkWallGetParams }

function TVkParamsWallGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallGet.Domain(const Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsWallGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGet.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGet.Filter(const Value: TVkPostTypeFilter): Integer;
begin
  Result := List.Add('value', Value.ToString);
end;

function TVkParamsWallGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallGet.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsWallEdit }

function TVkParamsWallEdit.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEdit.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallEdit.FriendsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('friends_only', Value);
end;

function TVkParamsWallEdit.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEdit.Services(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('services', Value);
end;

function TVkParamsWallEdit.Signed(const Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallEdit.PublishDate(const Value: TDateTime): Integer;
begin
  Result := List.Add('publish_date', Value);
end;

function TVkParamsWallEdit.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallEdit.PlaceId(const Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallEdit.MarkAsAds(const Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallEdit.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEdit.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEdit.CloseComments(const Value: Boolean): Integer;
begin
  Result := List.Add('close_comments', Value);
end;

function TVkParamsWallEdit.PosterBkgId(const Value: Integer): Integer;
begin
  Result := List.Add('poster_bkg_id', Value);
end;

function TVkParamsWallEdit.PosterBkgOwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('poster_bkg_owner_id', Value);
end;

function TVkParamsWallEdit.PosterBkgAccessHash(const Value: string): Integer;
begin
  Result := List.Add('poster_bkg_access_hash', Value);
end;

function TVkParamsWallEdit.Copyright(const Value: string): Integer;
begin
  Result := List.Add('copyright', Value);
end;

{ TVkParamsWallEditAdsStealth }

function TVkParamsWallEditAdsStealth.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEditAdsStealth.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallEditAdsStealth.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEditAdsStealth.Signed(const Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallEditAdsStealth.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditAdsStealth.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditAdsStealth.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallEditAdsStealth.PlaceId(const Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallEditAdsStealth.LinkButton(const Value: string): Integer;
begin
  Result := List.Add('link_button', Value);
end;

function TVkParamsWallEditAdsStealth.LinkTitle(const Value: string): Integer;
begin
  Result := List.Add('link_title', Value);
end;

function TVkParamsWallEditAdsStealth.LinkVideo(OwnerId, MediaId: Integer): Integer;
begin
  Result := List.Add('link_video', OwnerId.ToString + '_' + MediaId.ToString);
end;

function TVkParamsWallEditAdsStealth.LinkImage(const Value: string): Integer;
begin
  Result := List.Add('link_image', Value);
end;

function TVkParamsWallEditAdsStealth.LinkVideo(const Value: string): Integer;
begin
  Result := List.Add('link_video', Value);
end;

{ TVkParamsWallEditComment }

function TVkParamsWallEditComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallEditComment.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallEditComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallEditComment.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallEditComment.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

{ TVkParamsWallGetById }

function TVkParamsWallGetById.CopyHistoryDepth(const Value: Integer): Integer;
begin
  Result := List.Add('copy_history_depth', Value);
end;

function TVkParamsWallGetById.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetById.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetById.Posts(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('posts', Value);
end;

{ TVkParamsWallGetComment }

function TVkParamsWallGetComment.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallGetComment.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetComment.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsWallGetComments }

function TVkParamsWallGetComments.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallGetComments.PostId(const Value: Integer): Integer;
begin
  Result := List.Add('post_id', Value);
end;

function TVkParamsWallGetComments.NeedLikes(const Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsWallGetComments.StartCommentId(const Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

function TVkParamsWallGetComments.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallGetComments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallGetComments.Sort(const Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsWallGetComments.PreviewLength(const Value: Integer): Integer;
begin
  Result := List.Add('preview_length', Value);
end;

function TVkParamsWallGetComments.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallGetComments.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

function TVkParamsWallGetComments.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsWallGetComments.ThreadItemsCount(const Value: Integer): Integer;
begin
  Result := List.Add('thread_items_count', Value);
end;

{ TVkParamsWallPostAdsStealth }

function TVkParamsWallPostAdsStealth.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallPostAdsStealth.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallPostAdsStealth.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPostAdsStealth.Signed(const Value: Boolean): Integer;
begin
  Result := List.Add('signed', Value);
end;

function TVkParamsWallPostAdsStealth.LatLong(Lat, Long: Extended): Integer;
begin
  List.Add('lat', Lat);
  Result := List.Add('long', Long);
end;

function TVkParamsWallPostAdsStealth.PlaceId(const Value: Integer): Integer;
begin
  Result := List.Add('place_id', Value);
end;

function TVkParamsWallPostAdsStealth.Attachments(const Value: TAttachment): Integer;
begin
  Result := List.Add('attachments', Value);
end;

function TVkParamsWallPostAdsStealth.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsWallPostAdsStealth.LinkButton(const Value: string): Integer;
begin
  Result := List.Add('link_button', Value);
end;

function TVkParamsWallPostAdsStealth.LinkTitle(const Value: string): Integer;
begin
  Result := List.Add('link_title', Value);
end;

function TVkParamsWallPostAdsStealth.LinkButton(const Value: TVkPostLinkButton): Integer;
begin
  Result := List.Add('link_button', Value.ToString);
end;

function TVkParamsWallPostAdsStealth.LinkImage(const Value: string): Integer;
begin
  Result := List.Add('link_image', Value);
end;

function TVkParamsWallPostAdsStealth.LinkVideo(const Value: string): Integer;
begin
  Result := List.Add('link_video', Value);
end;

{ TVkParamsWallRepost }

function TVkParamsWallRepost.&Object(const Value: string): Integer;
begin
  Result := List.Add('object', Value);
end;

function TVkParamsWallRepost.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsWallRepost.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsWallRepost.MarkAsAds(const Value: Boolean): Integer;
begin
  Result := List.Add('mark_as_ads', Value);
end;

function TVkParamsWallRepost.MuteNotifications(const Value: Boolean): Integer;
begin
  Result := List.Add('mute_notifications', Value);
end;

{ TVkParamsWallSearch }

function TVkParamsWallSearch.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsWallSearch.Domain(const Value: string): Integer;
begin
  Result := List.Add('domain', Value);
end;

function TVkParamsWallSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('query', Value);
end;

function TVkParamsWallSearch.OwnersOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('owners_only', Value);
end;

function TVkParamsWallSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsWallSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsWallSearch.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsWallSearch.Fields(UserFields: TVkProfileFields; GroupFields: TVkGroupFields): Integer;
begin
  Result := List.Add('fields', [GroupFields.ToString, UserFields.ToString]);
end;

end.

