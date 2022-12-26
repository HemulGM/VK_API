unit VK.Wall;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, System.JSON, VK.Entity.Media, VK.Entity.Info;

type
  TVkParamsWallPost = record
    List: TParams;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallPost; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallPost; overload;
    /// <summary>
    /// True � ����������� � ������ ���������.
    /// False � ����������� � ������ ��������.
    /// </summary>
    function CloseComments(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// �������� ���������. �������������� ������� � ���������� ������
    /// </summary>
    function Copyright(const Value: string): TVkParamsWallPost;
    /// <summary>
    /// ������ ������� � ������� �������� ������ ����� �������� ��� ����� � ������� ����������� VK Donut
    /// </summary>
    function DonutPaidDuration(const Value: TVkDonutPaidDuration): TVkParamsWallPost;
    /// <summary>
    /// True � ������ ����� �������� ������ �������, False � ���� �������������.
    /// �� ��������� ����������� ������ �������� ���� �������������
    /// </summary>
    function FriendsOnly(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// ������ �������� �����������, ���� OwnerId ������ 0 (������ ����������� �� ����� ������).
    /// True � ������ ����� ������������ �� ����� ������,
    /// False � ������ ����� ������������ �� ����� ������������ (�� ���������)
    /// </summary>
    function FromGroup(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ���������� ������.
    /// ��������� � ������� ������ ����
    /// </summary>
    function Guid(const Value: string): TVkParamsWallPost;
    /// <summary>
    /// �������������� �������
    /// Lat - ������, �������� � �������� (�� -90 �� 90).
    /// Long - �������, �������� � �������� (�� -180 �� 180).
    /// </summary>
    function LatLong(Lat, Long: Extended): TVkParamsWallPost;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������",
    /// False � ����� ��������� �� �����.
    /// � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function MarkAsAds(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// ����� ��������� (�������� ������������, ���� �� ����� �������� Attachments)
    /// </summary>
    function Message(const Value: string): TVkParamsWallPost;
    /// <summary>
    /// True � ����������� � ������ ���������.
    /// False � ����������� � ������ ��������.
    /// </summary>
    function MuteNotifications(const Value: Boolean = True): TVkParamsWallPost;
    /// <summary>
    /// ������������� ������������ ��� ����������, �� ����� �������� ������ ���� ������������ ������
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallPost;
    /// <summary>
    /// ������������� �����, � ������� ������� ������������
    /// </summary>
    function PlaceId(const Value: Integer): TVkParamsWallPost;
    /// <summary>
    /// ������������� ������, ������� ���������� ������������.
    /// ������ �������� ������������ ��� ���������� ���������� ������� � ������������ ��������
    /// </summary>
    function PostId(const Value: Integer): TVkParamsWallPost;
    /// <summary>
    /// ���� ���������� ������. ���� �������� ������, ���������� ������ ����� �������� �� ���������� �������
    /// </summary>
    function PublishDate(const Value: TDateTime): TVkParamsWallPost;
    /// <summary>
    /// ������ �������� ��� ������, �� ������� ���������� �������������� ������,
    /// � ������ ���� ������������ �������� ��������������� �����. ��������, twitter, facebook
    /// </summary>
    function Services(const Value: TArrayOfString): TVkParamsWallPost;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� �������
    /// (��� ������������, ������������� ������), False � ������� ��������� �� �����.
    /// �������� ����������� ������ ��� ���������� �� ����� ���������� � �������� ��������� FromGroup.
    /// �� ��������� ������� �� �����������
    /// </summary>
    function Signed(const Value: Boolean = True): TVkParamsWallPost;
  end;

  TVkParamsWallEdit = record
    List: TParams;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallEdit; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallEdit; overload;
    /// <summary>
    /// True � ����������� � ������ ���������.
    /// False � ����������� � ������ ��������.
    /// </summary>
    function CloseComments(const Value: Boolean = True): TVkParamsWallEdit;
    /// <summary>
    /// �������� ���������. �������������� ������� � ���������� ������
    /// </summary>
    function Copyright(const Value: string): TVkParamsWallEdit;
    /// <summary>
    /// True � ������ ����� �������� ������ �������, False � ���� �������������.
    /// �� ��������� ����������� ������ �������� ���� �������������
    /// </summary>
    function FriendsOnly(const Value: Boolean = True): TVkParamsWallEdit;
    /// <summary>
    /// �������������� �������
    /// Lat - ������, �������� � �������� (�� -90 �� 90).
    /// Long - �������, �������� � �������� (�� -180 �� 180).
    /// </summary>
    function LatLong(Lat, Long: Extended): TVkParamsWallEdit;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������",
    /// False � ����� ��������� �� �����. � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function MarkAsAds(const Value: Boolean = True): TVkParamsWallEdit;
    /// <summary>
    /// ����� ��������� (�������� ������������, ���� �� ����� �������� attachments)
    /// </summary>
    function Message(const Value: string): TVkParamsWallEdit;
    /// <summary>
    /// ������������� ������������ ��� ����������, �� ����� �������� ������ ���� ������������ ������
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallEdit;
    /// <summary>
    /// ������������� �����, � ������� ������� ������������
    /// </summary>
    function PlaceId(const Value: Integer): TVkParamsWallEdit;
    function PosterBkgAccessHash(const Value: string): TVkParamsWallEdit;
    function PosterBkgId(const Value: Integer): TVkParamsWallEdit;
    function PosterBkgOwnerId(const Value: Integer): TVkParamsWallEdit;
    /// <summary>
    /// ������������� ������, ������� ���������� ������������.
    /// ������ �������� ������������ ��� ���������� ���������� ������� � ������������ ��������
    /// </summary>
    function PostId(const Value: Integer): TVkParamsWallEdit;
    /// <summary>
    /// ���� ���������� ������. ���� �������� ������, ���������� ������ ����� �������� �� ���������� �������
    /// </summary>
    function PublishDate(const Value: TDateTime): TVkParamsWallEdit;
    /// <summary>
    /// ������ �������� ��� ������, �� ������� ���������� �������������� ������,
    /// � ������ ���� ������������ �������� ��������������� �����. ��������, twitter, facebook
    /// </summary>
    function Services(const Value: TArrayOfString): TVkParamsWallEdit;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� �������
    /// (��� ������������, ������������� ������), False � ������� ��������� �� �����.
    /// �������� ����������� ������ ��� ���������� �� ����� ���������� � �������� ��������� FromGroup.
    /// �� ��������� ������� �� �����������
    /// </summary>
    function Signed(const Value: Boolean = True): TVkParamsWallEdit;
  end;

  TVkParamsWallEditAdsStealth = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ����� (������������� ���������� ����� ��������� �� ������ ������)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PostId(const Value: Integer): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// ����� ������
    /// </summary>
    function Message(const Value: string): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallEditAdsStealth; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallEditAdsStealth; overload;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� �������
    /// (��� ������������, ������������� ������), False � ������� ��������� �� �����.
    /// �������� ����������� ������ ��� ���������� �� ����� ���������� � �������� ��������� FromGroup.
    /// �� ��������� ������� �� �����������
    /// </summary>
    function Signed(const Value: Boolean = True): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������",
    /// False � ����� ��������� �� �����.
    /// � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function LatLong(Lat, Long: Extended): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// ������������� �����, � ������� ������� ������������
    /// </summary>
    function PlaceId(const Value: Integer): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// ������������� ������, ������� ���������� �������� � �������� ��� ������
    /// </summary>
    function LinkButton(const Value: string): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// ���������, ������� ������ ���� ����������� ��� ��������.
    /// ���� �� ������, ����� ������������� ������� � ������� ������.
    /// ����������� ��������� � ������, ���� ������ �������� ������� ��������
    /// </summary>
    function LinkTitle(const Value: string): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// ������ �� �����������, ������� ������ ���� ������������ ��� ��������.
    /// ����������� ����������: 537x240. ���� �� �������, ����� ������������� ��������� � ������� ������.
    /// ����������� ��������� � ������, ���� ������ �������� ������� ��������.
    /// ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� LinkVideo
    /// </summary>
    function LinkImage(const Value: string): TVkParamsWallEditAdsStealth;
    /// <summary>
    /// ������������� ����� � ������� "OwnerId_MediaId".
    /// ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� LinkVideo.
    /// ����� ����, �������� LinkVideo ����� ���� ������ ������ ������ � ����������� LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(const Value: string): TVkParamsWallEditAdsStealth; overload;
    /// <summary>
    /// ������������� �����. ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� .
    /// ����� ����, �������� LinkVideo ����� ���� ������ ������ ������ � ����������� LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(OwnerId: TVkPeerId; MediaId: Integer): TVkParamsWallEditAdsStealth; overload;
  end;

  TVkParamsWallEditComment = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �����
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallEditComment;
    /// <summary>
    /// ������������� �����������, ������� ���������� ���������������
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsWallEditComment;
    /// <summary>
    /// ����� ����� �����������. ������������ ��������, ���� �� ������� �������� Attachments
    /// </summary>
    function Message(const Value: string): TVkParamsWallEditComment;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallEditComment; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallEditComment; overload;
  end;

  TVkParamsWallGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �� ����� ��������
    /// ���������� �������� ������ (�� ��������� � ������� ������������)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallGet;
    /// <summary>
    /// �������� ����� ������������ ��� ����������
    /// </summary>
    function Domain(const Value: string): TVkParamsWallGet;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ �������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsWallGet;
    /// <summary>
    /// ���������� �������, ������� ���������� ��������. ������������ ��������: 100
    /// </summary>
    function Count(const Value: Integer): TVkParamsWallGet;
    /// <summary>
    /// ����������, ����� ���� ������� �� ����� ���������� ��������
    /// </summary>
    function Filter(const Value: TVkPostTypeFilter = TVkPostTypeFilter.All): TVkParamsWallGet;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallGet;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������.
    /// �������� ��������, ���� �������� ����������� ������ ��� Extended = True
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsWallGet;
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
    function Posts(const Value: TArrayOfString): TVkParamsWallGetById;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallGetById;
    /// <summary>
    /// ���������� ������ ������� copy_history, ������������� � ������, ���� ������ �������� �������� ������ � ������ �����
    /// ��������, copy_history_depth=1 � copy_history ����� ��������� ���� ������� � ����������� � ������, ������ �������� ������� �������� �������.
    /// copy_history_depth=2 � copy_history ����� ��������� ��� ��������, ����������� ���������� � ������, �������� ������� �������� ������ �������, � ��� ����� (��� �������, ��� �������� �������� ��������� ������� ��� ������� ������ ����������).
    /// </summary>
    function CopyHistoryDepth(const Value: Integer = 2): TVkParamsWallGetById;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � �����, ������� ���������� �������
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsWallGetById;
  end;

  TVkParamsWallGetComment = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ����� (��� ��������� � �� ������ ������)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallGetComment;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsWallGetComment;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallGetComment;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(Value: TVkExtendedFields = []): TVkParamsWallGetComment;
  end;

  TVkCommentCreateParams = record
    List: TParams;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PostId(const Value: Integer): TVkCommentCreateParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �� ���� ����� ��������� ������, � ������� ���������� �������� �����������
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkCommentCreateParams;
    /// <summary>
    /// ����� �����������. ������������ ��������, ���� �� ������� �������� attachments
    /// </summary>
    function Message(const Value: string): TVkCommentCreateParams;
    /// <summary>
    /// ������ �������� �����������, ���� OwnerId ������ 0 (������ ����������� �� ����� ������).
    /// True � ������ ����� ������������ �� ����� ������,
    /// False � ������ ����� ������������ �� ����� ������������ (�� ���������)
    /// </summary>
    function FromGroup(const Value: Boolean = True): TVkCommentCreateParams;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ���������� ������.
    ///  ��������� � ������� ������ ����
    /// </summary>
    function Guid(const Value: string): TVkCommentCreateParams;
    /// <summary>
    /// ������������� �����������, � ����� �� ������� ������ ���� �������� ����� �����������
    /// </summary>
    function ReplyToComment(const Value: Integer): TVkCommentCreateParams;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkCommentCreateParams; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): TVkCommentCreateParams; overload;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function StickerId(const Value: Integer): TVkCommentCreateParams;
  end;

  TVkParamsWallGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������� (������������ ��� ����������)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallGetComments;
    /// <summary>
    /// ������������� ������
    /// </summary>
    function PostId(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// True � ���������� ���������� � ������
    /// </summary>
    function NeedLikes(const Value: Boolean = True): TVkParamsWallGetComments;
    /// <summary>
    /// ������������� �����������, ������� � �������� ����� ������� ������
    /// </summary>
    function StartCommentId(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// �����, ����������� ��� ��������� ���������� ������� �����������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// ����� ������������, ������� ���������� �������� (������������ ��������: 100)
    /// </summary>
    function Count(const Value: Integer = 10): TVkParamsWallGetComments;
    /// <summary>
    /// ������� ���������� ������������
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsWallGetComments;
    /// <summary>
    /// ���������� ��������, �� �������� ����� �������� ����� �����������. ������� 0, ���� �� �� ������ �������� �����
    /// </summary>
    function PreviewLength(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallGetComments;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkExtendedFields = []): TVkParamsWallGetComments;
    /// <summary>
    /// ������������� �����������, ����� �������� ����� ��������
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsWallGetComments;
    /// <summary>
    /// ������������ ����� ��������� � ���� thread
    /// </summary>
    function ThreadItemsCount(const Value: Integer = 0): TVkParamsWallGetComments;
  end;

  TVkParamsWallPostAdsStealth = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� ����� (������������� ���������� ����� ��������� �� ������ ������)
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// ����� ������
    /// </summary>
    function Message(const Value: string): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsWallPostAdsStealth; overload;
    /// <summary>
    /// ������ ��� ��������� ��������, ����������� � ������
    /// </summary>
    function Attachments(const Value: TAttachment): TVkParamsWallPostAdsStealth; overload;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� �������
    /// (��� ������������, ������������� ������), False � ������� ��������� �� �����.
    /// �������� ����������� ������ ��� ���������� �� ����� ���������� � �������� ��������� FromGroup.
    /// �� ��������� ������� �� �����������
    /// </summary>
    function Signed(const Value: Boolean = True): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������",
    /// False � ����� ��������� �� �����.
    /// � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function LatLong(Lat, Long: Extended): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// ������������� �����, � ������� ������� ������������
    /// </summary>
    function PlaceId(const Value: Integer): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// ���������� �������������, ��������������� ��� �������������� ��������� �������� ���������� ������.
    /// ��������� � ������� ������ ����
    /// </summary>
    function Guid(const Value: string): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// ������������� ������, ������� ���������� �������� � �������� ��� ������
    /// </summary>
    function LinkButton(const Value: string): TVkParamsWallPostAdsStealth; overload;
    /// <summary>
    /// ������������� ������, ������� ���������� �������� � �������� ��� ������
    /// </summary>
    function LinkButton(const Value: TVkPostLinkButton): TVkParamsWallPostAdsStealth; overload;
    /// <summary>
    /// ���������, ������� ������ ���� ����������� ��� ��������.
    /// ���� �� ������, ����� ������������� ������� � ������� ������.
    /// ����������� ��������� � ������, ���� ������ �������� ������� ��������
    /// </summary>
    function LinkTitle(const Value: string): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// ������ �� �����������, ������� ������ ���� ������������ ��� ��������.
    /// ����������� ����������: 537x240. ���� �� �������, ����� ������������� ��������� � ������� ������.
    /// ����������� ��������� � ������, ���� ������ �������� ������� ��������.
    /// ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� LinkVideo
    /// </summary>
    function LinkImage(const Value: string): TVkParamsWallPostAdsStealth;
    /// <summary>
    /// ������������� ����� � ������� "[owner_id]_[media_id]".
    /// ������������ ����� ���� ������ ���� �������� LinkImage, ���� �������� LinkVideo.
    /// ����� ����, �������� LinkVideo ����� ���� ������ ������ ������ � ����������� LinkButton, LinkTitle
    /// </summary>
    function LinkVideo(const Value: string): TVkParamsWallPostAdsStealth;
  end;

  TVkParamsWallRepost = record
    List: TParams;
    /// <summary>
    /// ��������� ������������� �������, ������� ���������� ���������� �� �����, ��������, wall66748_3675 ��� wall-1_340364.
    /// ����������� �� ���� ������� (wall, photo, video � �.�.), �������������� ��������� ������� � �������������� ������ �������
    /// </summary>
    function &Object(const Value: string): TVkParamsWallRepost;
    /// <summary>
    /// ���������������� �����, ������� ����� �������� � ������ � ��������
    /// </summary>
    function Message(const Value: string): TVkParamsWallRepost;
    /// <summary>
    /// ������������� ����������, �� ����� �������� ����� ��������� ������ � ��������.
    /// ���� �� ������, ������ ����� ��������� �� ����� �������� ������������
    /// </summary>
    function GroupId(const Value: TVkPeerId): TVkParamsWallRepost;
    /// <summary>
    /// True � � ������, ����������� �� ����� ����������, ����� ��������� ����� "��� �������", False � ����� ��������� �� �����. � ����� ����� ���� ������������ �� ����� ���� ��������� �������, �� ������� �� ����� ��� � ��� ����� ���������
    /// </summary>
    function MarkAsAds(const Value: Boolean = True): TVkParamsWallRepost;
    /// <summary>
    /// True � ����������� � ������ ���������.
    /// False � ����������� � ������ ��������.
    /// </summary>
    function MuteNotifications(const Value: Boolean): TVkParamsWallRepost;
  end;

  TVkParamsWallSearch = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsWallSearch;
    /// <summary>
    /// �������� ����� ������������ ��� ����������
    /// </summary>
    function Domain(const Value: string): TVkParamsWallSearch;
    /// <summary>
    /// ��������� ������. ��� ������� ���������� ������ ���������� ���������� � ������� ��������
    /// </summary>
    function Query(const Value: string): TVkParamsWallSearch;
    /// <summary>
    /// True � ���������� ������ ������ �� ����� ��������� �����
    /// </summary>
    function OwnersOnly(const Value: Boolean): TVkParamsWallSearch;
    /// <summary>
    /// ���������� �������, ������� ���������� �������
    /// </summary>
    function Count(const Value: Integer): TVkParamsWallSearch;
    /// <summary>
    /// ��������, ���������� ��� ��������� ������������� ������������ �����������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsWallSearch;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean = True): TVkParamsWallSearch;
    /// <summary>
    /// ������ �������������� ����� ��� �������� � ���������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkExtendedFields = []): TVkParamsWallSearch;
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
    function CloseComments(const OwnerId: TVkPeerId; PostId: Integer): Boolean;
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
    function CreateComment(const PostId: Integer; const Message: string; OwnerId: TVkPeerId = 0; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ������� ������ �� �����.
    /// </summary>
    function Delete(const PostId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// ������� ����������� � ������ �� �����.
    /// </summary>
    function DeleteComment(const CommentId: Integer; OwnerId: TVkPeerId = 0): Boolean;
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
    function Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: TVkPeerId = 0): Boolean; overload;
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
    function GetReposts(var Items: TVkPosts; PostId: Integer; Offset: Integer = 0; Count: Integer = 0; OwnerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// �������� ��������������� ������
    /// �������� ������ � ����������� ��������, ��������������� ������� ���� ��������� � ������� wall.closeComments
    /// </summary>
    function OpenComments(const OwnerId: TVkPeerId; PostId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ������ �� ����� (������ ����� ������������ ���� ���������).
    /// </summary>
    function Pin(const PostId: Integer; const OwnerId: TVkPeerId = 0): Boolean; overload;
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
    function Post(var PostId: Integer; Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray = []): Boolean; overload;
    /// <summary>
    /// ��������� ������� ������ �� �����, ���������� ������ �� ����� ��������� ��������, ������������ ������������ ���������� ������.
    /// ����� ������� ������������ ������, ���������� �������� � owner_id ������������� ��������� ��������, � ������� ������� ������������ �� �������� �������������.
    /// ��� ���������� ������������ � ���������� ������� ����������� �������� post_id, �������� ��� �������� ����� �������� ������� wall.get � filter=suggests � postponed ��������������.
    /// </summary>
    function Post(const Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray = []): Boolean; overload;
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
    function ReportComment(const OwnerId: TVkPeerId; CommentId: Integer; Reason: TVkMediaReportReason = TVkMediaReportReason.Spam): Boolean; overload;
    /// <summary>
    /// ��������� ������������ �� ������.
    /// </summary>
    function ReportPost(const OwnerId: TVkPeerId; PostId: Integer; Reason: TVkMediaReportReason = TVkMediaReportReason.Spam): Boolean; overload;
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
    function Restore(const PostId: Integer; const OwnerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// ��������������� ��������� ����������� � ������ �� �����.
    /// </summary>
    function RestoreComment(const CommentId: Integer; const OwnerId: TVkPeerId = 0): Boolean; overload;
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
    function Unpin(const PostId: Integer; const OwnerId: TVkPeerId = 0): Boolean; overload;
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

function TWallController.Post(const Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray): Boolean;
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

function TWallController.ReportComment(const OwnerId: TVkPeerId; CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
var
  Params: TParams;
begin
  Params.Add('comment_id', CommentId);
  Params.Add('owner_id', OwnerId);
  Params.Add('reason', Reason.ToString);
  Result := Handler.Execute('wall.reportComment', Params).ResponseIsTrue;
end;

function TWallController.ReportPost(const OwnerId: TVkPeerId; PostId: Integer; Reason: TVkMediaReportReason): Boolean;
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

function TWallController.Restore(const PostId: Integer; const OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.restore', Params).ResponseIsTrue;
end;

function TWallController.RestoreComment(const CommentId: Integer; const OwnerId: TVkPeerId): Boolean;
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

function TWallController.Unpin(const PostId: Integer; const OwnerId: TVkPeerId): Boolean;
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

function TWallController.Post(var PostId: Integer; Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray): Boolean;
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

function TWallController.CloseComments(const OwnerId: TVkPeerId; PostId: Integer): Boolean;
begin
  Result := Handler.Execute('wall.closeComments', [
    ['owner_id', OwnerId.ToString],
    ['post_id', PostId.ToString]]).
    ResponseIsTrue;
end;

function TWallController.CreateComment(const PostId: Integer; const Message: string; OwnerId: TVkPeerId; Attachments: TAttachmentArray): Boolean;
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

function TWallController.Delete(const PostId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  Params.Add('post_id', PostId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('wall.delete', Params).ResponseIsTrue;
end;

function TWallController.DeleteComment(const CommentId: Integer; OwnerId: TVkPeerId): Boolean;
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

function TWallController.Get(var Items: TVkPosts; Offset, Count: Integer; OwnerId: TVkPeerId): Boolean;
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

function TWallController.GetReposts(var Items: TVkPosts; PostId, Offset, Count: Integer; OwnerId: TVkPeerId): Boolean;
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

function TWallController.OpenComments(const OwnerId: TVkPeerId; PostId: Integer): Boolean;
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

function TWallController.Pin(const PostId: Integer; const OwnerId: TVkPeerId): Boolean;
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

function TVkParamsWallPost.Attachments(const Value: TAttachmentArray): TVkParamsWallPost;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallPost.Attachments(const Value: TAttachment): TVkParamsWallPost;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallPost.CloseComments(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('close_comments', Value);
  Result := Self;
end;

function TVkParamsWallPost.Copyright(const Value: string): TVkParamsWallPost;
begin
  List.Add('copyright', Value);
  Result := Self;
end;

function TVkParamsWallPost.DonutPaidDuration(const Value: TVkDonutPaidDuration): TVkParamsWallPost;
begin
  List.Add('donut_paid_duration', Ord(Value));
  Result := Self;
end;

function TVkParamsWallPost.FriendsOnly(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('friends_only', Value);
  Result := Self;
end;

function TVkParamsWallPost.FromGroup(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkParamsWallPost.Guid(const Value: string): TVkParamsWallPost;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkParamsWallPost.LatLong(Lat, Long: Extended): TVkParamsWallPost;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsWallPost.MarkAsAds(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('mark_as_ads', Value);
  Result := Self;
end;

function TVkParamsWallPost.Message(const Value: string): TVkParamsWallPost;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallPost.MuteNotifications(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('mute_notifications', Value);
  Result := Self;
end;

function TVkParamsWallPost.OwnerId(const Value: TVkPeerId): TVkParamsWallPost;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallPost.PlaceId(const Value: Integer): TVkParamsWallPost;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsWallPost.PostId(const Value: Integer): TVkParamsWallPost;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkParamsWallPost.PublishDate(const Value: TDateTime): TVkParamsWallPost;
begin
  List.Add('publish_date', Value);
  Result := Self;
end;

function TVkParamsWallPost.Services(const Value: TArrayOfString): TVkParamsWallPost;
begin
  List.Add('services', Value);
  Result := Self;
end;

function TVkParamsWallPost.Signed(const Value: Boolean): TVkParamsWallPost;
begin
  List.Add('signed', Value);
  Result := Self;
end;

{ TVkCommentCreateParams }

function TVkCommentCreateParams.Attachments(const Value: TAttachmentArray): TVkCommentCreateParams;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkCommentCreateParams.Attachments(const Value: TAttachment): TVkCommentCreateParams;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkCommentCreateParams.FromGroup(const Value: Boolean): TVkCommentCreateParams;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkCommentCreateParams.Guid(const Value: string): TVkCommentCreateParams;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkCommentCreateParams.Message(const Value: string): TVkCommentCreateParams;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkCommentCreateParams.OwnerId(const Value: TVkPeerId): TVkCommentCreateParams;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkCommentCreateParams.PostId(const Value: Integer): TVkCommentCreateParams;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkCommentCreateParams.ReplyToComment(const Value: Integer): TVkCommentCreateParams;
begin
  List.Add('reply_to_comment', Value);
  Result := Self;
end;

function TVkCommentCreateParams.StickerId(const Value: Integer): TVkCommentCreateParams;
begin
  List.Add('sticker_id', Value);
  Result := Self;
end;

{ TVkWallGetParams }

function TVkParamsWallGet.Count(const Value: Integer): TVkParamsWallGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsWallGet.Domain(const Value: string): TVkParamsWallGet;
begin
  List.Add('domain', Value);
  Result := Self;
end;

function TVkParamsWallGet.Extended(const Value: Boolean): TVkParamsWallGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallGet.Fields(Value: TVkExtendedFields): TVkParamsWallGet;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGet.Filter(const Value: TVkPostTypeFilter): TVkParamsWallGet;
begin
  List.Add('value', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGet.Offset(const Value: Integer): TVkParamsWallGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsWallGet.OwnerId(const Value: TVkPeerId): TVkParamsWallGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsWallEdit }

function TVkParamsWallEdit.OwnerId(const Value: TVkPeerId): TVkParamsWallEdit;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PostId(const Value: Integer): TVkParamsWallEdit;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.FriendsOnly(const Value: Boolean): TVkParamsWallEdit;
begin
  List.Add('friends_only', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Message(const Value: string): TVkParamsWallEdit;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Services(const Value: TArrayOfString): TVkParamsWallEdit;
begin
  List.Add('services', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Signed(const Value: Boolean): TVkParamsWallEdit;
begin
  List.Add('signed', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PublishDate(const Value: TDateTime): TVkParamsWallEdit;
begin
  List.Add('publish_date', Value);
  Result := Self;
end;

function TVkParamsWallEdit.LatLong(Lat, Long: Extended): TVkParamsWallEdit;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsWallEdit.PlaceId(const Value: Integer): TVkParamsWallEdit;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.MarkAsAds(const Value: Boolean): TVkParamsWallEdit;
begin
  List.Add('mark_as_ads', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Attachments(const Value: TAttachment): TVkParamsWallEdit;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Attachments(const Value: TAttachmentArray): TVkParamsWallEdit;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEdit.CloseComments(const Value: Boolean): TVkParamsWallEdit;
begin
  List.Add('close_comments', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PosterBkgId(const Value: Integer): TVkParamsWallEdit;
begin
  List.Add('poster_bkg_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PosterBkgOwnerId(const Value: Integer): TVkParamsWallEdit;
begin
  List.Add('poster_bkg_owner_id', Value);
  Result := Self;
end;

function TVkParamsWallEdit.PosterBkgAccessHash(const Value: string): TVkParamsWallEdit;
begin
  List.Add('poster_bkg_access_hash', Value);
  Result := Self;
end;

function TVkParamsWallEdit.Copyright(const Value: string): TVkParamsWallEdit;
begin
  List.Add('copyright', Value);
  Result := Self;
end;

{ TVkParamsWallEditAdsStealth }

function TVkParamsWallEditAdsStealth.OwnerId(const Value: TVkPeerId): TVkParamsWallEditAdsStealth;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.PostId(const Value: Integer): TVkParamsWallEditAdsStealth;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.Message(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.Signed(const Value: Boolean): TVkParamsWallEditAdsStealth;
begin
  List.Add('signed', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.Attachments(const Value: TAttachment): TVkParamsWallEditAdsStealth;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.Attachments(const Value: TAttachmentArray): TVkParamsWallEditAdsStealth;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LatLong(Lat, Long: Extended): TVkParamsWallEditAdsStealth;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.PlaceId(const Value: Integer): TVkParamsWallEditAdsStealth;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkButton(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_button', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkTitle(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_title', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkVideo(OwnerId: TVkPeerId; MediaId: Integer): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_video', OwnerId.ToString + '_' + MediaId.ToString);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkImage(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_image', Value);
  Result := Self;
end;

function TVkParamsWallEditAdsStealth.LinkVideo(const Value: string): TVkParamsWallEditAdsStealth;
begin
  List.Add('link_video', Value);
  Result := Self;
end;

{ TVkParamsWallEditComment }

function TVkParamsWallEditComment.OwnerId(const Value: TVkPeerId): TVkParamsWallEditComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallEditComment.CommentId(const Value: Integer): TVkParamsWallEditComment;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsWallEditComment.Message(const Value: string): TVkParamsWallEditComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallEditComment.Attachments(const Value: TAttachment): TVkParamsWallEditComment;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallEditComment.Attachments(const Value: TAttachmentArray): TVkParamsWallEditComment;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

{ TVkParamsWallGetById }

function TVkParamsWallGetById.CopyHistoryDepth(const Value: Integer): TVkParamsWallGetById;
begin
  List.Add('copy_history_depth', Value);
  Result := Self;
end;

function TVkParamsWallGetById.Extended(const Value: Boolean): TVkParamsWallGetById;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallGetById.Fields(Value: TVkExtendedFields): TVkParamsWallGetById;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGetById.Posts(const Value: TArrayOfString): TVkParamsWallGetById;
begin
  List.Add('posts', Value);
  Result := Self;
end;

{ TVkParamsWallGetComment }

function TVkParamsWallGetComment.CommentId(const Value: Integer): TVkParamsWallGetComment;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComment.Extended(const Value: Boolean): TVkParamsWallGetComment;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallGetComment.Fields(Value: TVkExtendedFields): TVkParamsWallGetComment;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGetComment.OwnerId(const Value: TVkPeerId): TVkParamsWallGetComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsWallGetComments }

function TVkParamsWallGetComments.OwnerId(const Value: TVkPeerId): TVkParamsWallGetComments;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.PostId(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('post_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.NeedLikes(const Value: Boolean): TVkParamsWallGetComments;
begin
  List.Add('need_likes', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.StartCommentId(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('start_comment_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Offset(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Count(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Sort(const Value: TVkSort): TVkParamsWallGetComments;
begin
  List.Add('sort', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGetComments.PreviewLength(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('preview_length', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Extended(const Value: Boolean): TVkParamsWallGetComments;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.Fields(const Value: TVkExtendedFields): TVkParamsWallGetComments;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsWallGetComments.CommentId(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsWallGetComments.ThreadItemsCount(const Value: Integer): TVkParamsWallGetComments;
begin
  List.Add('thread_items_count', Value);
  Result := Self;
end;

{ TVkParamsWallPostAdsStealth }

function TVkParamsWallPostAdsStealth.OwnerId(const Value: TVkPeerId): TVkParamsWallPostAdsStealth;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Message(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Attachments(const Value: TAttachmentArray): TVkParamsWallPostAdsStealth;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Signed(const Value: Boolean): TVkParamsWallPostAdsStealth;
begin
  List.Add('signed', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LatLong(Lat, Long: Extended): TVkParamsWallPostAdsStealth;
begin
  List.Add('lat', Lat).Add('long', Long);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.PlaceId(const Value: Integer): TVkParamsWallPostAdsStealth;
begin
  List.Add('place_id', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Attachments(const Value: TAttachment): TVkParamsWallPostAdsStealth;
begin
  List.Add('attachments', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.Guid(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkButton(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_button', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkTitle(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_title', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkButton(const Value: TVkPostLinkButton): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_button', Value.ToString);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkImage(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_image', Value);
  Result := Self;
end;

function TVkParamsWallPostAdsStealth.LinkVideo(const Value: string): TVkParamsWallPostAdsStealth;
begin
  List.Add('link_video', Value);
  Result := Self;
end;

{ TVkParamsWallRepost }

function TVkParamsWallRepost.&Object(const Value: string): TVkParamsWallRepost;
begin
  List.Add('object', Value);
  Result := Self;
end;

function TVkParamsWallRepost.Message(const Value: string): TVkParamsWallRepost;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsWallRepost.GroupId(const Value: TVkPeerId): TVkParamsWallRepost;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsWallRepost.MarkAsAds(const Value: Boolean): TVkParamsWallRepost;
begin
  List.Add('mark_as_ads', Value);
  Result := Self;
end;

function TVkParamsWallRepost.MuteNotifications(const Value: Boolean): TVkParamsWallRepost;
begin
  List.Add('mute_notifications', Value);
  Result := Self;
end;

{ TVkParamsWallSearch }

function TVkParamsWallSearch.OwnerId(const Value: TVkPeerId): TVkParamsWallSearch;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Domain(const Value: string): TVkParamsWallSearch;
begin
  List.Add('domain', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Query(const Value: string): TVkParamsWallSearch;
begin
  List.Add('query', Value);
  Result := Self;
end;

function TVkParamsWallSearch.OwnersOnly(const Value: Boolean): TVkParamsWallSearch;
begin
  List.Add('owners_only', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Count(const Value: Integer): TVkParamsWallSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Offset(const Value: Integer): TVkParamsWallSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Extended(const Value: Boolean): TVkParamsWallSearch;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsWallSearch.Fields(const Value: TVkExtendedFields): TVkParamsWallSearch;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

end.

