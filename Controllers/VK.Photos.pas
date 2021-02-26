unit VK.Photos;

interface

uses
  System.SysUtils, System.Types, System.Generics.Collections, System.Classes,
  VK.Controller, VK.Types, VK.Entity.Album, REST.Json, VK.Entity.Photo.Upload,
  VK.Entity.Photo, VK.Entity.Media, VK.Entity.Group, VK.Entity.Common;

type
  TVkParamsPhotosGetAll = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, ���������� �������� ����� ��������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// True � ���������� ����������� ���������� � �����������
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// ����� ����������, ���������� � ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����������. �� ��������� � 0
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// True� ����� ���������� ������� ���������� � ����������� �������
    /// </summary>
    function PhotoSizes(const Value: Boolean): Integer;
    /// <summary>
    /// False � ������� ��� ����������, ������� ����������� � ��������� ��������, ����� ��� "���������� �� ���� �����" (�� ���������);
    /// True � ������� ���������� ������ �� ����������� �������� ������������ ��� ����������
    /// </summary>
    function NoServiceAlbums(const Value: Boolean): Integer;
    /// <summary>
    /// True � ���������� ���������� �� ���, ������ �� ���������� �� ����� ��� ������ ������������
    /// </summary>
    function NeedHidden(const Value: Boolean): Integer;
    /// <summary>
    /// True � �� ���������� ����������, ������� �� ����� ��� ������ ������������
    /// (�������� ����������� ������ ��� OwnerId ������ 0, �������� NoServiceAlbums ������������)
    /// </summary>
    function SkipHidden(const Value: Boolean): Integer;
  end;

  TVkParamsPhotosGet = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: Integer): Integer; overload;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: TVkPhotoSystemAlbum): Integer; overload;
    /// <summary>
    /// �������������� ����������, ���������� � ������� ���������� �������
    /// </summary>
    function PhotoIds(const Value: TArrayOfInteger): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes, comments, tags, can_comment, reposts. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean): Integer;
    /// <summary>
    /// ���������� �������, ������� ����� ��������
    /// </summary>
    function Count(const Value: Integer = 50): Integer;
    /// <summary>
    /// ������, ����������� ��� ��������� ������������� ������������ �������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// True � ���������� ��������� ������� ���������� � ����������� �������. �� ���������: False
    /// </summary>
    function PhotoSizes(const Value: Boolean): Integer;
    /// <summary>
    /// ��� �������, ���������� � ���� type ������ newsfeed.get, ��� ���������
    /// ������ ����������� ������������� ����������, ���� ������ ����������, ��
    /// ������� �� ��� �������. ����� ��������� �������� photo, photo_tag
    /// </summary>
    function FeedType(const Value: TVkPhotoFeedType): Integer;
    /// <summary>
    /// ����� � �������, ������� ����� ���� ������� ������� newsfeed.get � ���� date,
    /// ��� ��������� ���� ���������� ����������� ������������� � ����������� ����
    /// ���� �� ������� ������������ ��� �������. ����� ����� ������� �������� uid ������������, � ������� ��������� �������.
    /// �������� ������ ���������� �� �������� ������� �� �����, ��� �� �����.
    /// </summary>
    function Feed(const Value: Integer): Integer;
    /// <summary>
    /// ������� ���������� ����������
    ///  True � �������������������;
    ///  False � ���������������.
    /// </summary>
    function Rev(const Value: Boolean): Integer;
    /// <summary>
    /// [��� ��������]
    /// </summary>
    function Uid(const Value: Integer): Integer;
  end;

  TVkParamsAlbumsGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����� ������� �������������� �������� (�� ����� 1000)
    /// </summary>
    function AlbumIds(const Value: TArrayOfInteger): Integer; overload;
    /// <summary>
    /// ������������� ����� ������� �������������� ��������
    /// </summary>
    function AlbumIds(const Value: Integer): Integer; overload;
    /// <summary>
    /// ���������� ��������, ������� ����� �������. (�� ��������� ������������ ��� �������)
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ��������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// True � ������� ���������� ����� ���������� � ����������� �������
    /// </summary>
    function PhotoSizes(const Value: Boolean): Integer;
    /// <summary>
    /// True � ����� ���������� ��������� �������, ������� ������������� ��������������.
    /// �������� ��������, ��� ���������� � ��������� �������� ������������ ���� � ��� ������, ���� ��� �� �������� ����������
    /// </summary>
    function NeedSystem(const Value: Boolean): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� thumb_src � ������� �����������-�������. �� ��������� ���� thumb_src �� ������������
    /// </summary>
    function NeedCovers(const Value: Boolean): Integer;
  end;

  TVkParamsPhotosCreateAlbum = record
    List: TParams;
    /// <summary>
    /// �������� �������
    /// </summary>
    function Title(const Value: string): Integer;
    /// <summary>
    /// ������������� ����������, � ������� �������� ������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ����� �������� �������
    /// </summary>
    function Description(const Value: string): Integer;
    /// <summary>
    /// ��������� ����������� ��������� ������� � ����������� �������
    /// </summary>
    function PrivacyView(const Value: TVkPrivacySettings): Integer;
    /// <summary>
    /// ��������� ����������� ��������������� ������� � ����������� �������
    /// </summary>
    function PrivacyComment(const Value: TVkPrivacySettings): Integer;
    /// <summary>
    /// ��� ����� ��������� ���������� � ������ (������ ��� ������� ����������)
    /// False � ���������� ����� ��������� ��� ������������;
    /// True � ���������� ����� ��������� ������ ��������� � ��������������
    /// </summary>
    function UploadByAdminsOnly(const Value: Boolean): Integer;
    /// <summary>
    /// ��������� �� ��������������� ������� (������ ��� ������� ����������)
    /// False � ��������������� ��������;
    /// True � ��������������� ���������
    /// </summary>
    function CommentsDisabled(const Value: Boolean): Integer;
  end;

  TVkParamsPhotosEditAlbum = record
    List: TParams;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// �������� �������
    /// </summary>
    function Title(const Value: string): Integer;
    /// <summary>
    /// ����� �������� �������
    /// </summary>
    function Description(const Value: string): Integer;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ��������� ����������� ��������� ������� � ����������� �������
    /// </summary>
    function PrivacyView(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ��������� ����������� ��������������� ������� � ����������� �������
    /// </summary>
    function PrivacyComment(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ��� ����� ��������� ���������� � ������ (������ ��� ������� ����������)
    /// False � ���������� ����� ��������� ��� ������������;
    /// True � ���������� ����� ��������� ������ ��������� � ��������������
    /// </summary>
    function UploadByAdminsOnly(const Value: Boolean): Integer;
    /// <summary>
    /// ��������� �� ��������������� ������� (������ ��� ������� ����������)
    /// False � ��������������� ��������;
    /// True � ��������������� ���������
    /// </summary>
    function CommentsDisabled(const Value: Boolean): Integer;
  end;

  TVkParamsPhotosCreateComment = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PhotoId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ����������� (�������� ������������, ���� �� ����� �������� Attachments).
    /// ������������ ���������� ��������: 2048
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer;
    /// <summary>
    /// ������ �������� �����������, ���� OwnerId ������ 0 (����������� � ���������� ������)
    ///  True � ����������� ����� ����������� �� ����� ������;
    ///  False � ����������� ����� ����������� �� ����� ������������
    /// </summary>
    function FromGroup(const Value: Boolean = False): Integer;
    /// <summary>
    /// ������������� �����������, � ����� �� ������� ����� �������� �������
    /// </summary>
    function ReplyToComment(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ������� ����� ���������� � �����������
    /// </summary>
    function StickerId(const Value: Cardinal): Integer;
    /// <summary>
    /// ���� �������
    /// </summary>
    function AccessKey(const Value: string): Integer;
    /// <summary>
    /// ���������� �������� ��� �������������� ��������� �������� ������ � ���� �� �����������
    /// </summary>
    function Guid(const Value: string): Integer;
  end;

  TVkParamsPhotosEdit = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PhotoId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ����� �������� � ����������. ���� �������� �� �����, �� ���������, ��� �� ����� ������ ������
    /// </summary>
    function Caption(const Value: string): Integer;
    /// <summary>
    /// �������������� ������
    /// </summary>
    function Latitude(const Value: Extended): Integer;
    /// <summary>
    /// �������������� �������
    /// </summary>
    function Longitude(const Value: Extended): Integer;
    /// <summary>
    /// �������� �����
    /// </summary>
    function PlaceStr(const Value: string): Integer;
    /// <summary>
    /// Id � Foursquare
    /// </summary>
    function FoursquareId(const Value: string): Integer;
    /// <summary>
    /// ������� ����� (False � �� �������, True � �������)
    /// </summary>
    function DeletePlace(const Value: Boolean = False): Integer;
  end;

  TVkParamsPhotosSave = record
    List: TParams;
    /// <summary>
    /// ������������� �������, � ������� ���������� ��������� ����������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������, � ������� ���������� ��������� ����������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Server(const Value: string): Integer;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function PhotosList(const Value: TArrayOfString): Integer;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Hash(const Value: string): Integer;
    /// <summary>
    /// �������������� ������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    function Latitude(const Value: Extended): Integer;
    /// <summary>
    /// �������������� �������, �������� � �������� (�� -180 �� 180)
    /// </summary>
    function Longitude(const Value: Extended): Integer;
    /// <summary>
    /// ����� �������� ���������� (�������� 2048 ��������)
    /// </summary>
    function Caption(const Value: string): Integer;
  end;

  TVkParamsPhotosEditComment = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function CommentId(const Value: Integer): Integer;
    /// <summary>
    /// ����� ����� �����������. ������������ ��������, ���� �� ����� �������� attachments.
    /// ������������ ���������� ��������: 2048
    /// </summary>
    function Message(const Value: string): Integer;
    /// <summary>
    /// ����� ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): Integer;
  end;

  TVkParamsPhotosGetAlbumsCount = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, ���������� �������� �������� ���������� ��������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������, ���������� �������� �������� ���������� ��������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
  end;

  TVkParamsPhotosGetAllComments = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������. ���� �������� �� �����, �� ���������, ��� ���������� �������� ����������� �� ���� �������� ������������ ��� ����������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes. �� ��������� ���� likes �� ������������
    /// </summary>
    function NeedLikes(const Value: Boolean): Integer;
    /// <summary>
    /// ���������� ������������, ������� ���������� ��������. ���� �������� �� �����, �� ��������� ��� �� ����� 20. ������������ �������� ��������� 100.
    /// �������� ��������, ���� ��� ������������� ��������� offset ��� ��������� �������� ������ ������ 10000 ������������.
    /// </summary>
    function Count(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� 0
    /// </summary>
    function Offset(const Value: Integer): Integer;
  end;

  TVkParamsPhotosGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PhotoId(const Value: Integer): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes. �� ���������: False
    /// </summary>
    function NeedLikes(const Value: Boolean = False): Integer;
    /// <summary>
    /// ���������� ������������, ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������������. �� ���������: 0
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// ������������� �����������, ������� � �������� ����� ������� ������
    /// </summary>
    function StartCommentId(const Value: Integer): Integer;
    /// <summary>
    /// ������� ���������� ������������
    /// </summary>
    function Sort(const Value: TVkSort): Integer;
    /// <summary>
    /// ���� ������� � ����������
    /// </summary>
    function AccessKey(const Value: string): Integer;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): Integer;
  end;

  TVkParamsPhotosGetMarketUploadServer = record
    List: TParams;
    /// <summary>
    /// ������������� ����������, ��� �������� ���������� ��������� ���������� ������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// �������� �� ���������� �������� ������ (True � ���������� ��� �������, False � �������������� ����������)
    /// </summary>
    function MainPhoto(const Value: Boolean): Integer;
    /// <summary>
    /// ���������� ��� ������� ���������� (������� ������ ����)
    /// </summary>
    function Crop(const Value: TPoint): Integer;
    /// <summary>
    /// ������ ���������� ����� ������� � px (����������� �������� 400)
    /// </summary>
    function CropWidth(const Value: Integer): Integer;
  end;

  TVkParamsPhotosGetUserPhotos = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, ������ ���������� ��� �������� ����� ��������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes, comments, tags, can_comment.
    /// ���� comments � tags �������� ������ ���������� ��������. �� ��������� ������ ���� �� ������������
    /// </summary>
    function Extended(const Value: Boolean = False): Integer;
    /// <summary>
    /// ���������� ����������, ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): Integer;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����������
    /// </summary>
    function Offset(const Value: Integer = 0): Integer;
    /// <summary>
    /// ���������� ����������� (�� ���� ���������� �������)
    /// </summary>
    function Sort(const Value: TVkSort): Integer;
  end;

  TVkParamsPhotosReorderAlbums = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ����� ������� ������� ��������� ������
    /// </summary>
    function Before(const Value: Integer): Integer;
    /// <summary>
    /// ������������� �������, ����� �������� ������� ��������� ������
    /// </summary>
    function After(const Value: Integer): Integer;
  end;

  TVkParamsPhotosReorderPhotos = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PhotoId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������, ����� ������� ������� ��������� ����������. ���� �������� �� ������, ���������� ����� �������� ���������
    /// </summary>
    function Before(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������, ����� ������� ������� ��������� ����������. ���� �������� �� ������, ���������� ����� �������� ������
    /// </summary>
    function After(const Value: Integer): Integer;
  end;

  TVkParamsPhotosSaveMarketPhoto = record
    List: TParams;
    /// <summary>
    /// ������������ ������, ��� ������� ����� ��������� ����������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// �������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Photo(const Value: string): Integer;
    /// <summary>
    /// �������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Server(const Value: Integer): Integer;
    /// <summary>
    /// �������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Hash(const Value: string): Integer;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������.
    /// ������������ ��������, ���� �� ����� �������� ���� ��� ������� MainPhoto = True
    /// </summary>
    function CropData(const Value: string): Integer;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������.
    /// ������������ ��������, ���� �� ����� �������� ���� ��� ������� MainPhoto = True
    /// </summary>
    function CropHash(const Value: string): Integer;
  end;

  TVkParamsPhotosSaveWallPhoto = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, �� ����� �������� ����� ��������� ����������
    /// </summary>
    function UserId(const Value: Integer): Integer;
    /// <summary>
    /// ������������� ����������, �� ����� �������� ����� ��������� ����������
    /// </summary>
    function GroupId(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Photo(const Value: string): Integer;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Server(const Value: Integer): Integer;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Hash(const Value: string): Integer;
    /// <summary>
    /// �������������� ������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    function Latitude(const Value: Extended): Integer;
    /// <summary>
    /// �������������� �������, �������� � �������� (�� -180 �� 180)
    /// </summary>
    function Longitude(const Value: Extended): Integer;
    /// <summary>
    /// ����� �������� ���������� (�������� 2048 ��������)
    /// </summary>
    function Caption(const Value: string): Integer;
  end;

  TVkParamsPhotosSearch = record
    List: TParams;
    /// <summary>
    /// ������ ���������� �������, ��������
    /// </summary>
    function Query(const Value: string): Integer;
    /// <summary>
    /// �������������� ������ �������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    function Latitude(const Value: Extended): Integer;
    /// <summary>
    /// �������������� ������� �������, �������� � �������� (�� -180 �� 180)
    /// </summary>
    function Longitude(const Value: Extended): Integer;
    /// <summary>
    /// ����� � ������� unixtime, �� ������ �������� ������ ���� ���� ��������� ��������� ����������
    /// </summary>
    function StartTime(const Value: TDateTime): Integer;
    /// <summary>
    /// ����� � ������� unixtime, �� ����� �������� ������ ���� ���� ��������� ��������� ����������
    /// </summary>
    function EndTime(const Value: TDateTime): Integer;
    /// <summary>
    /// ���������� �����������
    /// </summary>
    function Sort(const Value: TVkPhotoSort): Integer;
    /// <summary>
    /// �������� ������������ ������ ��������� ���������� ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): Integer;
    /// <summary>
    /// ���������� ������������ ���������� (������������ �������� 1000)
    /// </summary>
    function Count(const Value: Integer = 100): Integer;
    /// <summary>
    /// ������ ������ � ������. (�������� ����� �����������, ������� �������� ���������� �� ���� ����� ���������� �� ���������). ����� ��������� ��������: 10, 100, 800, 6000, 50000
    /// </summary>
    function Radius(const Value: Integer = 5000): Integer;
  end;

  TPhotosController = class(TVkController)
  public
    /// <summary>
    /// ������������ ������� �� ����������.
    /// </summary>
    function �onfirmTag(PhotoId, TagId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ������������ �������.
    /// </summary>
    function ConfirmTags(Tags: TIdList): Boolean;
    /// <summary>
    /// ��������� ����������� ���������� � ������ "����������� ����������"
    /// </summary>
    function Copy(var Id: Integer; OwnerId, PhotoId: Integer; AccessKey: string = ''): Boolean;
    /// <summary>
    /// ������� ������ ������ ��� ����������.
    /// </summary>
    function CreateAlbum(var Item: TVkPhotoAlbum; Params: TParams): Boolean; overload;
    /// <summary>
    /// ������� ������ ������ ��� ����������.
    /// </summary>
    function CreateAlbum(var Item: TVkPhotoAlbum; Params: TVkParamsPhotosCreateAlbum): Boolean; overload;
    /// <summary>
    /// ������� ����� ����������� � ����������.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TParams): Boolean; overload;
    /// <summary>
    /// ������� ����� ����������� � ����������.
    /// </summary>
    function CreateComment(var Id: Integer; Params: TVkParamsPhotosCreateComment): Boolean; overload;
    /// <summary>
    /// �� �� ������)
    /// https://vk.com/dev/photos.declineTags
    /// </summary>
    function DeclineTags: Boolean;
    /// <summary>
    /// �������� ���������� �� �����.
    /// </summary>
    function Delete(OwnerId, PhotoId: Integer): Boolean;
    /// <summary>
    /// ������� ��������� ������ ��� ���������� � �������� ������������
    /// </summary>
    function DeleteAlbum(AlbumId: Integer; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ����������� � ����������.
    /// </summary>
    function DeleteComment(OwnerId, CommentId: Integer): Boolean;
    /// <summary>
    /// ����������� �������� ��� �������� � ����������.
    /// </summary>
    function Edit(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� �������� ��� �������� � ����������.
    /// </summary>
    function Edit(Params: TVkParamsPhotosEdit): Boolean; overload;
    /// <summary>
    /// ����������� ������ ������� ��� ����������.
    /// </summary>
    function EditAlbum(Params: TParams): Boolean; overload;
    /// <summary>
    /// ����������� ������ ������� ��� ����������.
    /// </summary>
    function EditAlbum(Params: TVkParamsPhotosEditAlbum): Boolean; overload;
    /// <summary>
    /// �������� ����� ����������� � ����������.
    /// �������� ��������, ��� �������������� ����������� �������� ������ � ������� ����� ����� ��� ��������.
    /// </summary>
    function EditComment(Params: TParams): Boolean; overload;
    /// <summary>
    /// �������� ����� ����������� � ����������.
    /// �������� ��������, ��� �������������� ����������� �������� ������ � ������� ����� ����� ��� ��������.
    /// </summary>
    function EditComment(Params: TVkParamsPhotosEditComment): Boolean; overload;
    /// <summary>
    /// ���������� ������ ���������� � �������.
    /// </summary>
    function Get(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ���������� � �������.
    /// </summary>
    function Get(var Items: TVkPhotos; Params: TVkParamsPhotosGet): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ ������������ ��� ����������.
    /// </summary>
    function GetAlbums(var Items: TVkPhotoAlbums; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ ������������ ��� ����������.
    /// </summary>
    function GetAlbums(var Items: TVkPhotoAlbums; Params: TVkParamsAlbumsGet): Boolean; overload;
    /// <summary>
    /// ���������� ���������� ��������� �������� ������������ ��� ����������.
    /// </summary>
    function GetAlbumsCount(var Count: Integer; Params: TVkParamsPhotosGetAlbumsCount): Boolean;
    /// <summary>
    /// ���������� ��� ���������� ������������ ��� ���������� � ������������������� �������.
    /// </summary>
    function GetAll(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ��� ���������� ������������ ��� ���������� � ������������������� �������.
    /// </summary>
    function GetAll(var Items: TVkPhotos; Params: TVkParamsPhotosGetAll): Boolean; overload;
    /// <summary>
    /// ���������� ��������������� � ������������������� ������� ������ ���� ������������ � ����������� ������� ��� �� ���� �������� ������������.
    /// </summary>
    function GetAllComments(var Items: TVkComments; Params: TVkParamsPhotosGetAllComments): Boolean;
    /// <summary>
    /// ���������� ���������� � ����������� �� �� ���������������.
    /// <b>Photos</b> - ������������� ����� ������� ��������������, ������� ������������ ����� ������ ����� ���� ������������� id �������������, ������������ ����������, � id ����� ����������. ����� �������� ���������� � ���������� � ������� ������, ������ id ������������ ������� ������� -id ������. ������ �������� photos: 1_263219656,6492_456239863,-1_456239099
    /// ��������� ����������, �������������� ������� ����� ���� �������� ����� API, ������� ������������, � �� ����� ��������. � ���� ������ ������� ������������ ���� ������� ���������� (access_key) � � ��������������.
    /// ������ �������� photos: 1_129207899_220df2876123d3542f, 6492_135055734_e0a9bcc31144f67fbd
    /// ���� access_key ����� ���������� ������ � ���������� ������� ���������� � �������, ������� ���������� ����������, �������� ������������ �� ��������� � ������ ���������. �������� ������ ���� ����� ����������, ������������ ������� newsfeed.get.
    /// </summary>
    function GetById(var Items: TVkPhotos; Photos: TArrayOfString; Extended: Boolean = False; PhotoSizes: Boolean = False): Boolean; overload;
    /// <summary>
    /// ��������� �������� ����� ��� �������� ������� ����.
    /// </summary>
    function GetChatUploadServer(var UploadUrl: string; ChatId: Integer; Crop: TPoint; CropWidth: Integer = 0): Boolean;
    /// <summary>
    /// ���������� ������ ������������ � ����������.
    /// ���� ��� ������� �������� StartCommentId, ����� ������� ������� ����������� � ������ (��� ��������� � ���� ����� ������). ������� � ���� ������� ����� ���������� Count ������������. �������� Offset � ���� ������ ����� ������������� �� ���� ������� (��� ����� ���� �������������)
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������������ � ����������.
    /// ���� ��� ������� �������� StartCommentId, ����� ������� ������� ����������� � ������ (��� ��������� � ���� ����� ������). ������� � ���� ������� ����� ���������� Count ������������. �������� Offset � ���� ������ ����� ������������� �� ���� ������� (��� ����� ���� �������������)
    /// </summary>
    function GetComments(var Items: TVkComments; Params: TVkParamsPhotosGetComments): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ���������� �������� ������� � ����������.
    /// ����������� ������ ���������� � 1280x720 ��������.
    /// </summary>
    function GetMarketAlbumUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ���������� ������.
    /// </summary>
    function GetMarketUploadServer(var UploadUrl: string; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ���������� ������.
    /// </summary>
    function GetMarketUploadServer(var UploadUrl: string; Params: TVkParamsPhotosGetMarketUploadServer): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ���������� � ������ ���������.
    /// </summary>
    function GetMessagesUploadServer(var UploadUrl: string; PeerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ���������� � ������ ���������.
    /// </summary>
    function GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ������ ����������, �� ������� ���� ��������������� �������.
    /// </summary>
    function GetNewTags(var Items: TVkPhotos; Count: Integer = 20; Offset: Integer = 0): Boolean;
    /// <summary>
    /// �������� ����� ��� �������� ������� ����������.
    /// </summary>
    function GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: Integer; CropLeft: TPoint; CropRight: TPoint): Boolean; overload;
    /// <summary>
    /// �������� ����� ��� �������� ������� ����������.
    /// </summary>
    function GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ������� ���������� �� �������� ������������ ��� ����������.
    /// </summary>
    function GetOwnerPhotoUploadServer(var UploadUrl: string; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ������ ������� �� ����������.
    /// </summary>
    function GetTags(var Items: TVkPhotoTags; PhotoId: Integer; OwnerId: Integer = 0; AccessKey: string = ''): Boolean;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ����������.
    /// </summary>
    function GetUploadServer(var UploadData: TVkPhotoGetUploadResponse; AlbumId: Integer = 0; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ���������� ������ ����������, �� ������� ������� ������������.
    /// </summary>
    function GetUserPhotos(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ���������� ������ ����������, �� ������� ������� ������������.
    /// </summary>
    function GetUserPhotos(var Items: TVkPhotos; Params: TVkParamsPhotosGetUserPhotos): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ���������� �� ����� ������������ ��� ����������.
    /// </summary>
    function GetWallUploadServer(var UploadData: TVkPhotoGetUploadResponse; GroupId: Integer = 0): Boolean;
    /// <summary>
    /// ������ ���������� �������� �������.
    /// </summary>
    function MakeCover(PhotoId, AlbumId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ��������� ���������� �� ������ ������� � ������.
    /// </summary>
    function Move(PhotoId, TargetAlbumId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ��������� ������� �� ����������.
    /// </summary>
    function PutTag(var TagId: Integer; PhotoId, UserId: Integer; Left, Right: TPoint; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ������� ������� � ����������.
    /// </summary>
    function RemoveTag(PhotoId, TagId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ������ ������� ������� � ������ �������� ������������.
    /// </summary>
    function ReorderAlbums(Params: TVkParamsPhotosReorderAlbums): Boolean;
    /// <summary>
    /// ������ ������� ���������� � ������ ���������� ������� ������������.
    /// </summary>
    function ReorderPhotos(Params: TVkParamsPhotosReorderPhotos): Boolean;
    /// <summary>
    /// ��������� ������������ �� ����������.
    /// </summary>
    function Report(OwnerId, PhotoId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// ��������� ������������ �� ����������� � ����������.
    /// </summary>
    function ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
    /// <summary>
    /// ��������������� ��������� ����������.
    /// </summary>
    function Restore(PhotoId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ��������������� ��������� ����������� � ����������.
    /// </summary>
    function RestoreComment(CommentId: Integer; OwnerId: Integer = 0): Boolean;
    /// <summary>
    /// ��������� ���������� ����� �������� ��������.
    /// </summary>
    function Save(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ���������� ����� �������� ��������.
    /// </summary>
    function Save(var Items: TVkPhotos; Params: TVkParamsPhotosSave): Boolean; overload;
    /// <summary>
    /// ��������� ���������� ����� �������� �������� �� URI, ���������� ������� photos.getMarketAlbumUploadServer.
    /// </summary>
    function SaveMarketAlbumPhoto(var Items: TVkPhotos; GroupId: Integer; Photo, Server, Hash: string): Boolean;
    /// <summary>
    /// ��������� ���������� ����� �������� �������� �� URI, ���������� ������� photos.getMarketUploadServer.
    /// </summary>
    function SaveMarketPhoto(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ���������� ����� �������� �������� �� URI, ���������� ������� photos.getMarketUploadServer.
    /// </summary>
    function SaveMarketPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveMarketPhoto): Boolean; overload;
    /// <summary>
    /// ��������� ���������� ����� �������� �������� �� URI, ���������� ������� photos.getMessagesUploadServer.
    /// </summary>
    function SaveMessagesPhoto(var Items: TVkPhotos; Data: TVkPhotoUploadResponse): Boolean; overload;
    /// <summary>
    /// ��������� ���������� ����� �������� �������� �� URI, ���������� ������� photos.getMessagesUploadServer.
    /// </summary>
    function SaveMessagesPhoto(var Items: TVkPhotos; Server: Integer; Photo, Hash: string): Boolean; overload;
    /// <summary>
    /// ��������� ����������� ��� ������� ���������� ����� �������� ��������.
    /// </summary>
    function SaveOwnerCoverPhoto(var Items: TVkCoverImages; Photo, Hash: string): Boolean;
    /// <summary>
    /// ��������� ��������� ������� ���������� ������������ ��� ����������. ����� ��� �������� ���������� �� ������ �������� � ������� ������ photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(var Info: TVkOwnerPhoto; Server: Integer; Photo, Hash: string): Boolean; overload;
    /// <summary>
    /// ��������� ��������� ������� ���������� ������������ ��� ����������. ����� ��� �������� ���������� �� ������ �������� � ������� ������ photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(var Info: TVkOwnerPhoto; Data: TVkPhotoUploadResponse): Boolean; overload;
    /// <summary>
    /// ��������� ��������� ������� ���������� ������������ ��� ����������. ����� ��� �������� ���������� �� ������ �������� � ������� ������ photos.getOwnerPhotoUploadServer.
    /// </summary>
    function SaveOwnerPhoto(FileName: string): Boolean; overload;
    /// <summary>
    /// ��������� ���������� ����� �������� �������� �� URI, ���������� ������� photos.getWallUploadServer.
    /// </summary>
    function SaveWallPhoto(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ��������� ���������� ����� �������� �������� �� URI, ���������� ������� photos.getWallUploadServer.
    /// </summary>
    function SaveWallPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveWallPhoto): Boolean; overload;
    /// <summary>
    /// ������������ ����� ����������� �� �������������� ��� ��������.
    /// </summary>
    function Search(var Items: TVkPhotos; Params: TParams): Boolean; overload;
    /// <summary>
    /// ������������ ����� ����������� �� �������������� ��� ��������.
    /// </summary>
    function Search(var Items: TVkPhotos; Params: TVkParamsPhotosSearch): Boolean; overload;
    /// <summary>
    /// �������� ����������
    /// </summary>
    function Upload(const UploadUrl: string; const FileNames: array of string; var Response: TVkPhotoUploadResponse): Boolean; overload;
    /// <summary>
    /// �������� ����������
    /// </summary>
    function Upload(const UploadUrl: string; Stream: TStream; const FileName: string; var Response: TVkPhotoUploadResponse): Boolean; overload;
    /// <summary>
    /// �������� ���������� ��� �������� � ���������
    /// </summary>
    function UploadForMessage(var Photos: TVkPhotos; const PeerId: Integer; const FileNames: array of string): Boolean;
    /// <summary>
    /// �������� ���������� ��� ���������� �� ����� ������������ ��� ����������
    /// </summary>
    function UploadForWall(var Photos: TVkPhotos; const FileNames: array of string; Params: TVkParamsPhotosSaveWallPhoto; const GroupId: Cardinal = 0): Boolean;
    /// <summary>
    /// �������� ���������� ��� ���������� �� ����� ����������
    /// </summary>
    function UploadForGroupWall(var Photos: TVkPhotos; const GroupId: Cardinal; const FileNames: array of string): Boolean;
    /// <summary>
    /// �������� ���������� ��� ���������� �� ����� ������������
    /// </summary>
    function UploadForUserWall(var Photos: TVkPhotos; const UserId: Integer; const FileNames: array of string): Boolean;
  end;

implementation

uses
  VK.API, VK.CommonUtils, System.DateUtils, System.Net.HttpClient,
  System.Net.Mime;

{ TPhotosController }

function TPhotosController.Upload(const UploadUrl: string; Stream: TStream; const FileName: string; var Response: TVkPhotoUploadResponse): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
begin
  Result := False;
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    Data.AddStream('file', Stream, ExtractFileName(FileName));
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      Response := TVkPhotoUploadResponse.FromJsonString<TVkPhotoUploadResponse>(ResStream.DataString);
      Result := True;
    end;
  finally
    ResStream.Free;
    Data.Free;
    HTTP.Free;
  end;
end;

function TPhotosController.Upload(const UploadUrl: string; const FileNames: array of string; var Response: TVkPhotoUploadResponse): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
  FileName: string;
  i: Integer;
begin
  Result := False;
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    i := 1;
    for FileName in FileNames do
    begin
      Data.AddFile('file' + i.ToString, FileName);
      Inc(i);
    end;
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      Response := TVkPhotoUploadResponse.FromJsonString<TVkPhotoUploadResponse>(ResStream.DataString);
      Result := True;
    end;
  finally
    ResStream.Free;
    Data.Free;
    HTTP.Free;
  end;
end;

function TPhotosController.GetMessagesUploadServer(var UploadUrl: string; PeerId: Integer): Boolean;
var
  Upload: TVkPhotoGetUploadResponse;
begin
  Result := GetMessagesUploadServer(Upload, PeerId);
  if Result then
  begin
    try
      UploadUrl := Upload.UploadUrl;
    finally
      Upload.Free;
    end;
  end;
end;

function TPhotosController.Get(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.get', Params).GetObject(Items);
end;

function TPhotosController.GetAll(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.getAll', Params).GetObject(Items);
end;

function TPhotosController.GetAlbums(var Items: TVkPhotoAlbums; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.getAlbums', Params).GetObject(Items);
end;

function TPhotosController.GetAlbums(var Items: TVkPhotoAlbums; Params: TVkParamsAlbumsGet): Boolean;
begin
  Result := GetAlbums(Items, Params.List);
end;

function TPhotosController.GetAlbumsCount(var Count: Integer; Params: TVkParamsPhotosGetAlbumsCount): Boolean;
begin
  Result := Handler.Execute('photos.getAlbumsCount', Params.List).ResponseAsInt(Count);
end;

function TPhotosController.ConfirmTags(Tags: TIdList): Boolean;
begin
  Result := Handler.Execute('photos.confirmTag', ['tags', Tags.ToString]).ResponseIsTrue;
end;

function TPhotosController.Copy(var Id: Integer; OwnerId, PhotoId: Integer; AccessKey: string): Boolean;
begin
  Result := Handler.Execute('photos.confirmTag', [
    ['owner_id', OwnerId.ToString],
    ['photo_id', PhotoId.ToString],
    ['access_key', AccessKey]]).
    ResponseAsInt(Id);
end;

function TPhotosController.CreateAlbum(var Item: TVkPhotoAlbum; Params: TVkParamsPhotosCreateAlbum): Boolean;
begin
  Result := CreateAlbum(Item, Params.List);
end;

function TPhotosController.CreateComment(var Id: Integer; Params: TVkParamsPhotosCreateComment): Boolean;
begin
  Result := CreateComment(Id, Params.List);
end;

function TPhotosController.DeclineTags: Boolean;
begin
  Result := Handler.Execute('photos.declineTags').ResponseIsTrue;
end;

function TPhotosController.Delete(OwnerId, PhotoId: Integer): Boolean;
begin
  Result := Handler.Execute('photos.delete', [['owner_id', OwnerId.ToString], ['photo_id', PhotoId.ToString]]).ResponseIsTrue;
end;

function TPhotosController.DeleteAlbum(AlbumId, GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('album_id', AlbumId);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('photos.deleteAlbum', Params).ResponseIsTrue;
end;

function TPhotosController.DeleteComment(OwnerId, CommentId: Integer): Boolean;
begin
  Result := Handler.Execute('photos.deleteComment', [
    ['owner_id', OwnerId.ToString],
    ['comment_id', CommentId.ToString]]).
    ResponseIsTrue;
end;

function TPhotosController.Edit(Params: TVkParamsPhotosEdit): Boolean;
begin
  Result := Edit(Params.List);
end;

function TPhotosController.EditAlbum(Params: TVkParamsPhotosEditAlbum): Boolean;
begin
  Result := EditAlbum(Params.List);
end;

function TPhotosController.EditComment(Params: TVkParamsPhotosEditComment): Boolean;
begin
  Result := EditComment(Params.List);
end;

function TPhotosController.EditComment(Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.editComment', Params).ResponseIsTrue;
end;

function TPhotosController.EditAlbum(Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.editAlbum', Params).ResponseIsTrue;
end;

function TPhotosController.Edit(Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.edit', Params).ResponseIsTrue;
end;

function TPhotosController.CreateComment(var Id: Integer; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.createComment', Params).ResponseAsInt(Id);
end;

function TPhotosController.CreateAlbum(var Item: TVkPhotoAlbum; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.createAlbum', Params).GetObject(Item);
end;

function TPhotosController.Get(var Items: TVkPhotos; Params: TVkParamsPhotosGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TPhotosController.GetAll(var Items: TVkPhotos; Params: TVkParamsPhotosGetAll): Boolean;
begin
  Result := GetAll(Items, Params.List);
end;

function TPhotosController.GetAllComments(var Items: TVkComments; Params: TVkParamsPhotosGetAllComments): Boolean;
begin
  Result := Handler.Execute('photos.getAllComments', Params.List).GetObject(Items);
end;

function TPhotosController.GetById(var Items: TVkPhotos; Photos: TArrayOfString; Extended, PhotoSizes: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('photos', Photos);
  if Extended then
    Params.Add('extended', Extended);
  if PhotoSizes then
    Params.Add('photo_sizes', PhotoSizes);
  Result := Handler.Execute('photos.getById', Params).GetObject(Items);
end;

function TPhotosController.GetChatUploadServer(var UploadUrl: string; ChatId: Integer; Crop: TPoint; CropWidth: Integer): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Params.Add('chat_id', ChatId);
  if not Crop.IsZero then
  begin
    Params.Add('crop_x', Crop.X);
    Params.Add('crop_y', Crop.Y);
  end;
  if CropWidth <> 0 then
    Params.Add('crop_width', CropWidth);
  with Handler.Execute('photos.getChatUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString<TVkPhotoGetUploadResponse>(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetComments(var Items: TVkComments; Params: TVkParamsPhotosGetComments): Boolean;
begin
  Result := GetComments(Items, Params.List);
end;

function TPhotosController.GetComments(var Items: TVkComments; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.getComments', Params).GetObject(Items);
end;

function TPhotosController.GetMarketAlbumUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
var
  Item: TVkPhotoGetUploadResponse;
begin
  with Handler.Execute('photos.getMarketAlbumUploadServer', ['group_id', GroupId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString<TVkPhotoGetUploadResponse>(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetMarketUploadServer(var UploadUrl: string; Params: TVkParamsPhotosGetMarketUploadServer): Boolean;
begin
  Result := GetMarketUploadServer(UploadUrl, Params.List);
end;

function TPhotosController.GetMarketUploadServer(var UploadUrl: string; Params: TParams): Boolean;
var
  Item: TVkPhotoGetUploadResponse;
begin
  with Handler.Execute('photos.getMarketUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString<TVkPhotoGetUploadResponse>(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: Integer): Boolean;
var
  Params: TParams;
begin
  if PeerId <> 0 then
    Params.Add('peer_id', PeerId);
  Result := Handler.Execute('photos.getMessagesUploadServer', Params).GetObject(Upload);
end;

function TPhotosController.GetNewTags(var Items: TVkPhotos; Count, Offset: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('count', Count);
  Params.Add('offset', Offset);
  Result := Handler.Execute('photos.getNewTags', Params).GetObject(Items);
end;

function TPhotosController.GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
begin
  Result := GetOwnerCoverPhotoUploadServer(UploadUrl, GroupId, TPoint.Zero, TPoint.Zero);
end;

function TPhotosController.GetOwnerPhotoUploadServer(var UploadUrl: string; OwnerId: Integer): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Params.Add('owner_id', OwnerId);
  with Handler.Execute('photos.getOwnerPhotoUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString<TVkPhotoGetUploadResponse>(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.GetTags(var Items: TVkPhotoTags; PhotoId, OwnerId: Integer; AccessKey: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo_id', PhotoId);
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('access_key', AccessKey);
  Result := Handler.Execute('photos.getTags', Params).GetObject(Items);
end;

function TPhotosController.GetUploadServer(var UploadData: TVkPhotoGetUploadResponse; AlbumId, GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  if AlbumId <> 0 then
    Params.Add('album_id', AlbumId);
  Result := Handler.Execute('photos.getUploadServer', Params).GetObject(UploadData);
end;

function TPhotosController.GetUserPhotos(var Items: TVkPhotos; Params: TVkParamsPhotosGetUserPhotos): Boolean;
begin
  Result := GetUserPhotos(Items, Params.List);
end;

function TPhotosController.GetWallUploadServer(var UploadData: TVkPhotoGetUploadResponse; GroupId: Integer): Boolean;
var
  Params: TParams;
begin
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  Result := Handler.Execute('photos.getWallUploadServer', Params).GetObject(UploadData);
end;

function TPhotosController.MakeCover(PhotoId, AlbumId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('album_id', AlbumId);
  Result := Handler.Execute('photos.makeCover', Params).ResponseIsTrue;
end;

function TPhotosController.Move(PhotoId, TargetAlbumId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('target_album_id', TargetAlbumId);
  Result := Handler.Execute('photos.move', Params).ResponseIsTrue;
end;

function TPhotosController.PutTag(var TagId: Integer; PhotoId, UserId: Integer; Left, Right: TPoint; OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('user_id', UserId);
  Params.Add('x', Left.X);
  Params.Add('y', Left.y);
  Params.Add('x2', Right.X);
  Params.Add('y2', Right.y);
  Result := Handler.Execute('photos.putTag', Params).ResponseAsInt(TagId);
end;

function TPhotosController.RemoveTag(PhotoId, TagId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('tag_id', TagId);
  Result := Handler.Execute('photos.removeTag', Params).ResponseIsTrue;
end;

function TPhotosController.ReorderAlbums(Params: TVkParamsPhotosReorderAlbums): Boolean;
begin
  Result := Handler.Execute('photos.reorderAlbums', Params.List).ResponseIsTrue;
end;

function TPhotosController.ReorderPhotos(Params: TVkParamsPhotosReorderPhotos): Boolean;
begin
  Result := Handler.Execute('photos.reorderPhotos', Params.List).ResponseIsTrue;
end;

function TPhotosController.Report(OwnerId, PhotoId: Integer; Reason: TVkMediaReportReason): Boolean;
begin
  Result := Handler.Execute('photos.report', [
    ['owner_id', OwnerId.ToString],
    ['photo_id', PhotoId.ToString],
    ['reason', Reason.ToConst.ToString]]).
    ResponseIsTrue;
end;

function TPhotosController.ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
begin
  Result := Handler.Execute('photos.reportComment', [
    ['owner_id', OwnerId.ToString],
    ['comment_id', CommentId.ToString],
    ['reason', Reason.ToConst.ToString]]).
    ResponseIsTrue;
end;

function TPhotosController.Restore(PhotoId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Result := Handler.Execute('photos.restore', Params).ResponseIsTrue;
end;

function TPhotosController.RestoreComment(CommentId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('comment_id', CommentId);
  Result := Handler.Execute('photos.restoreComment', Params).ResponseIsTrue;
end;

function TPhotosController.GetUserPhotos(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.getUserPhotos', Params).GetObject(Items);
end;

function TPhotosController.GetOwnerCoverPhotoUploadServer(var UploadUrl: string; GroupId: Integer; CropLeft, CropRight: TPoint): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Params.Add('group_id', GroupId);
  Params.Add('crop_x', CropLeft.X);
  Params.Add('crop_y', CropLeft.y);
  Params.Add('crop_x2', CropRight.X);
  Params.Add('crop_y2', CropRight.y);
  with Handler.Execute('photos.getOwnerCoverPhotoUploadServer', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Item := TVkPhotoGetUploadResponse.FromJsonString<TVkPhotoGetUploadResponse>(Response);
        try
          UploadUrl := Item.UploadUrl;
        finally
          Item.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

function TPhotosController.Save(var Items: TVkPhotos; Params: TVkParamsPhotosSave): Boolean;
begin
  Result := Save(Items, Params.List);
end;

function TPhotosController.Save(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.ExecutePost('photos.save', Params).GetObjects(Items);
end;

function TPhotosController.SaveMarketAlbumPhoto(var Items: TVkPhotos; GroupId: Integer; Photo, Server, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('group_id', GroupId);
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  Result := Handler.ExecutePost('photos.saveMarketAlbumPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveMarketPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveMarketPhoto): Boolean;
begin
  Result := SaveMarketPhoto(Items, Params.List);
end;

function TPhotosController.SaveMessagesPhoto(var Items: TVkPhotos; Server: Integer; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  Result := Handler.ExecutePost('photos.saveMessagesPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveOwnerCoverPhoto(var Items: TVkCoverImages; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('hash', Hash);
  Result := Handler.ExecutePost('photos.saveOwnerCoverPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveOwnerPhoto(FileName: string): Boolean;
var
  Server: string;
  Response: TVkPhotoUploadResponse;
  Info: TVkOwnerPhoto;
begin
  Result := False;
  if GetOwnerPhotoUploadServer(Server) then
  begin
    if Upload(Server, FileName, Response) then
    begin
      try
        if SaveOwnerPhoto(Info, Response) then
        begin
          try
            Result := Info.Saved;
          finally
            Info.Free;
          end;
        end;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TPhotosController.SaveOwnerPhoto(var Info: TVkOwnerPhoto; Data: TVkPhotoUploadResponse): Boolean;
begin
  Result := SaveOwnerPhoto(Info, Data.Server, Data.Photo, Data.Hash);
end;

function TPhotosController.SaveOwnerPhoto(var Info: TVkOwnerPhoto; Server: Integer; Photo, Hash: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('photo', Photo);
  Params.Add('server', Server);
  Params.Add('hash', Hash);
  Result := Handler.Execute('photos.saveOwnerPhoto', Params).GetObject(Info);
end;

function TPhotosController.SaveWallPhoto(var Items: TVkPhotos; Params: TVkParamsPhotosSaveWallPhoto): Boolean;
begin
  Result := SaveWallPhoto(Items, Params.List);
end;

function TPhotosController.Search(var Items: TVkPhotos; Params: TVkParamsPhotosSearch): Boolean;
begin
  Result := Search(Items, Params.List);
end;

function TPhotosController.UploadForGroupWall(var Photos: TVkPhotos; const GroupId: Cardinal; const FileNames: array of string): Boolean;
var
  SaveParams: TVkParamsPhotosSaveWallPhoto;
begin
  SaveParams.GroupId(GroupId);
  Result := UploadForWall(Photos, FileNames, SaveParams, GroupId);
end;

function TPhotosController.UploadForMessage(var Photos: TVkPhotos; const PeerId: Integer; const FileNames: array of string): Boolean;
var
  Url: string;
  Response: TVkPhotoUploadResponse;
begin
  Result := False;
  if GetMessagesUploadServer(Url, PeerId) then
  begin
    if Upload(Url, FileNames, Response) then
    begin
      try
        Result := SaveMessagesPhoto(Photos, Response);
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TPhotosController.UploadForUserWall(var Photos: TVkPhotos; const UserId: Integer; const FileNames: array of string): Boolean;
var
  SaveParams: TVkParamsPhotosSaveWallPhoto;
begin
  SaveParams.UserId(UserId);
  Result := UploadForWall(Photos, FileNames, SaveParams);
end;

function TPhotosController.UploadForWall(var Photos: TVkPhotos; const FileNames: array of string; Params: TVkParamsPhotosSaveWallPhoto; const GroupId: Cardinal): Boolean;
var
  Response: TVkPhotoUploadResponse;
  PhotoUpload: TVkPhotoGetUploadResponse;
begin
  Result := False;
  if GetWallUploadServer(PhotoUpload, GroupId) then
  begin
    try
      if Upload(PhotoUpload.UploadUrl, FileNames, Response) then
      begin
        try
          Params.Photo(Response.Photo);
          Params.Hash(Response.Hash);
          Params.Server(Response.Server);
          Result := SaveWallPhoto(Photos, Params);
        finally
          Response.Free;
        end;
      end;
    finally
      PhotoUpload.Free;
    end;
  end;
end;

function TPhotosController.Search(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.search', Params).GetObject(Items);
end;

function TPhotosController.SaveWallPhoto(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.ExecutePost('photos.saveWallPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveMarketPhoto(var Items: TVkPhotos; Params: TParams): Boolean;
begin
  Result := Handler.Execute('photos.saveMarketPhoto', Params).GetObjects(Items);
end;

function TPhotosController.SaveMessagesPhoto(var Items: TVkPhotos; Data: TVkPhotoUploadResponse): Boolean;
begin
  Result := SaveMessagesPhoto(Items, Data.Server, Data.Photo, Data.Hash);
end;

function TPhotosController.�onfirmTag(PhotoId, TagId, OwnerId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('photo_id', PhotoId);
  Params.Add('tag_id', TagId);
  Result := Handler.Execute('photos.confirmTag', Params).ResponseIsTrue;
end;

{ TVkGetAllParams }

function TVkParamsPhotosGetAll.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetAll.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPhotosGetAll.NeedHidden(const Value: Boolean): Integer;
begin
  Result := List.Add('need_hidden', Value);
end;

function TVkParamsPhotosGetAll.NoServiceAlbums(const Value: Boolean): Integer;
begin
  Result := List.Add('no_service_albums', Value);
end;

function TVkParamsPhotosGetAll.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGetAll.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosGetAll.PhotoSizes(const Value: Boolean): Integer;
begin
  Result := List.Add('photo_sizes', Value);
end;

function TVkParamsPhotosGetAll.SkipHidden(const Value: Boolean): Integer;
begin
  Result := List.Add('skip_hidden', Value);
end;

{ TVkParamsAlbumsGet }

function TVkParamsAlbumsGet.AlbumIds(const Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsAlbumsGet.AlbumIds(const Value: Integer): Integer;
begin
  Result := List.Add('album_ids', Value);
end;

function TVkParamsAlbumsGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsAlbumsGet.NeedCovers(const Value: Boolean): Integer;
begin
  Result := List.Add('need_covers', Value);
end;

function TVkParamsAlbumsGet.NeedSystem(const Value: Boolean): Integer;
begin
  Result := List.Add('need_system', Value);
end;

function TVkParamsAlbumsGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsAlbumsGet.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsAlbumsGet.PhotoSizes(const Value: Boolean): Integer;
begin
  Result := List.Add('photo_sizes', Value);
end;

{ TVkPhotosGetParams }

function TVkParamsPhotosGet.AlbumId(const Value: TVkPhotoSystemAlbum): Integer;
begin
  Result := List.Add('album_id', Value.ToString);
end;

function TVkParamsPhotosGet.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosGet.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGet.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPhotosGet.Feed(const Value: Integer): Integer;
begin
  Result := List.Add('feed', Value);
end;

function TVkParamsPhotosGet.FeedType(const Value: TVkPhotoFeedType): Integer;
begin
  Result := List.Add('feed_type', Value.ToString);
end;

function TVkParamsPhotosGet.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGet.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosGet.PhotoIds(const Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('photo_ids', Value);
end;

function TVkParamsPhotosGet.PhotoSizes(const Value: Boolean): Integer;
begin
  Result := List.Add('photo_sizes', Value);
end;

function TVkParamsPhotosGet.Rev(const Value: Boolean): Integer;
begin
  Result := List.Add('rev', Value);
end;

function TVkParamsPhotosGet.Uid(const Value: Integer): Integer;
begin
  Result := List.Add('uid', Value);
end;

{ TVkParamsPhotosCreateAlbum }

function TVkParamsPhotosCreateAlbum.CommentsDisabled(const Value: Boolean): Integer;
begin
  Result := List.Add('comments_disabled', Value);
end;

function TVkParamsPhotosCreateAlbum.Description(const Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsPhotosCreateAlbum.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosCreateAlbum.PrivacyComment(const Value: TVkPrivacySettings): Integer;
begin
  Result := List.Add('privacy_comment', Value.ToString);
end;

function TVkParamsPhotosCreateAlbum.PrivacyView(const Value: TVkPrivacySettings): Integer;
begin
  Result := List.Add('privacy_view', Value.ToString);
end;

function TVkParamsPhotosCreateAlbum.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsPhotosCreateAlbum.UploadByAdminsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('upload_by_admins_only', Value);
end;

{ TVkParamsPhotosCreateComment }

function TVkParamsPhotosCreateComment.AccessKey(const Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsPhotosCreateComment.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToStrings);
end;

function TVkParamsPhotosCreateComment.FromGroup(const Value: Boolean): Integer;
begin
  Result := List.Add('from_group', Value);
end;

function TVkParamsPhotosCreateComment.Guid(const Value: string): Integer;
begin
  Result := List.Add('guid', Value);
end;

function TVkParamsPhotosCreateComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsPhotosCreateComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosCreateComment.PhotoId(const Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPhotosCreateComment.ReplyToComment(const Value: Integer): Integer;
begin
  Result := List.Add('reply_to_comment', Value);
end;

function TVkParamsPhotosCreateComment.StickerId(const Value: Cardinal): Integer;
begin
  Result := List.Add('sticker_id', Value);
end;

{ TVkParamsPhotosEdit }

function TVkParamsPhotosEdit.Caption(const Value: string): Integer;
begin
  Result := List.Add('caption', Value);
end;

function TVkParamsPhotosEdit.DeletePlace(const Value: Boolean): Integer;
begin
  Result := List.Add('delete_place', Value);
end;

function TVkParamsPhotosEdit.FoursquareId(const Value: string): Integer;
begin
  Result := List.Add('foursquare_id', Value);
end;

function TVkParamsPhotosEdit.Latitude(const Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsPhotosEdit.Longitude(const Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsPhotosEdit.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosEdit.PhotoId(const Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPhotosEdit.PlaceStr(const Value: string): Integer;
begin
  Result := List.Add('place_str', Value);
end;

{ TVkParamsPhotosEditAlbum }

function TVkParamsPhotosEditAlbum.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosEditAlbum.CommentsDisabled(const Value: Boolean): Integer;
begin
  Result := List.Add('comments_disabled', Value);
end;

function TVkParamsPhotosEditAlbum.Description(const Value: string): Integer;
begin
  Result := List.Add('description', Value);
end;

function TVkParamsPhotosEditAlbum.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosEditAlbum.PrivacyComment(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_comment', Value);
end;

function TVkParamsPhotosEditAlbum.PrivacyView(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('privacy_view', Value);
end;

function TVkParamsPhotosEditAlbum.Title(const Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

function TVkParamsPhotosEditAlbum.UploadByAdminsOnly(const Value: Boolean): Integer;
begin
  Result := List.Add('upload_by_admins_only', Value);
end;

{ TVkParamsPhotosEditComment }

function TVkParamsPhotosEditComment.Attachments(const Value: TAttachmentArray): Integer;
begin
  Result := List.Add('attachments', Value.ToStrings);
end;

function TVkParamsPhotosEditComment.CommentId(const Value: Integer): Integer;
begin
  Result := List.Add('comment_id', Value);
end;

function TVkParamsPhotosEditComment.Message(const Value: string): Integer;
begin
  Result := List.Add('message', Value);
end;

function TVkParamsPhotosEditComment.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsPhotosGetAlbumsCount }

function TVkParamsPhotosGetAlbumsCount.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosGetAlbumsCount.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsPhotosGetAllComments }

function TVkParamsPhotosGetAllComments.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosGetAllComments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetAllComments.NeedLikes(const Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsPhotosGetAllComments.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGetAllComments.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsPhotosGetComments }

function TVkParamsPhotosGetComments.AccessKey(const Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsPhotosGetComments.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetComments.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPhotosGetComments.Fields(const Value: TVkProfileFields): Integer;
begin
  Result := List.Add('fields', Value.ToString);
end;

function TVkParamsPhotosGetComments.NeedLikes(const Value: Boolean): Integer;
begin
  Result := List.Add('need_likes', Value);
end;

function TVkParamsPhotosGetComments.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGetComments.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetComments.PhotoId(const Value: Integer): Integer;
begin
  Result := List.Add('photo_id', Value);
end;

function TVkParamsPhotosGetComments.Sort(const Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Value.ToString);
end;

function TVkParamsPhotosGetComments.StartCommentId(const Value: Integer): Integer;
begin
  Result := List.Add('start_comment_id', Value);
end;

{ TVkParamsPhotosGetMarketUploadServer }

function TVkParamsPhotosGetMarketUploadServer.Crop(const Value: TPoint): Integer;
begin
  List.Add('crop_x', Value.X);
  Result := List.Add('crop_y', Value.Y);
end;

function TVkParamsPhotosGetMarketUploadServer.CropWidth(const Value: Integer): Integer;
begin
  Result := List.Add('crop_width', Value);
end;

function TVkParamsPhotosGetMarketUploadServer.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosGetMarketUploadServer.MainPhoto(const Value: Boolean): Integer;
begin
  Result := List.Add('main_photo', Value);
end;

{ TVkParamsPhotosGetUserPhotos }

function TVkParamsPhotosGetUserPhotos.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosGetUserPhotos.Extended(const Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsPhotosGetUserPhotos.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPhotosGetUserPhotos.Sort(const Value: TVkSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsPhotosGetUserPhotos.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsPhotosReorderAlbums }

function TVkParamsPhotosReorderAlbums.After(const Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

function TVkParamsPhotosReorderAlbums.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosReorderAlbums.Before(const Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsPhotosReorderAlbums.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsPhotosReorderPhotos }

function TVkParamsPhotosReorderPhotos.After(const Value: Integer): Integer;
begin
  Result := List.Add('after', Value);
end;

function TVkParamsPhotosReorderPhotos.Before(const Value: Integer): Integer;
begin
  Result := List.Add('before', Value);
end;

function TVkParamsPhotosReorderPhotos.OwnerId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsPhotosReorderPhotos.PhotoId(const Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkParamsPhotosSave }

function TVkParamsPhotosSave.AlbumId(const Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsPhotosSave.Caption(const Value: string): Integer;
begin
  Result := List.Add('caption', Value);
end;

function TVkParamsPhotosSave.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosSave.Hash(const Value: string): Integer;
begin
  Result := List.Add('hash', Value);
end;

function TVkParamsPhotosSave.Latitude(const Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsPhotosSave.Longitude(const Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsPhotosSave.PhotosList(const Value: TArrayOfString): Integer;
begin
  Result := List.Add('photos_list', Value);
end;

function TVkParamsPhotosSave.Server(const Value: string): Integer;
begin
  Result := List.Add('server', Value);
end;

{ TVkParamsPhotosSaveMarketPhoto }

function TVkParamsPhotosSaveMarketPhoto.CropData(const Value: string): Integer;
begin
  Result := List.Add('crop_data', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.CropHash(const Value: string): Integer;
begin
  Result := List.Add('crop_hash', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.Hash(const Value: string): Integer;
begin
  Result := List.Add('hash', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.Photo(const Value: string): Integer;
begin
  Result := List.Add('photo', Value);
end;

function TVkParamsPhotosSaveMarketPhoto.Server(const Value: Integer): Integer;
begin
  Result := List.Add('server', Value);
end;

{ TVkParamsPhotosSaveWallPhoto }

function TVkParamsPhotosSaveWallPhoto.Caption(const Value: string): Integer;
begin
  Result := List.Add('caption', Value);
end;

function TVkParamsPhotosSaveWallPhoto.GroupId(const Value: Integer): Integer;
begin
  Result := List.Add('group_id', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Hash(const Value: string): Integer;
begin
  Result := List.Add('hash', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Latitude(const Value: Extended): Integer;
begin
  Result := List.Add('latitude', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Longitude(const Value: Extended): Integer;
begin
  Result := List.Add('longitude', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Photo(const Value: string): Integer;
begin
  Result := List.Add('photo', Value);
end;

function TVkParamsPhotosSaveWallPhoto.Server(const Value: Integer): Integer;
begin
  Result := List.Add('server', Value);
end;

function TVkParamsPhotosSaveWallPhoto.UserId(const Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

{ TVkParamsPhotosSearch }

function TVkParamsPhotosSearch.Count(const Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPhotosSearch.EndTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('end_time', Value);
end;

function TVkParamsPhotosSearch.Latitude(const Value: Extended): Integer;
begin
  Result := List.Add('lat', Value);
end;

function TVkParamsPhotosSearch.Longitude(const Value: Extended): Integer;
begin
  Result := List.Add('long', Value);
end;

function TVkParamsPhotosSearch.Offset(const Value: Integer): Integer;
begin
  Result := List.Add('offse', Value);
end;

function TVkParamsPhotosSearch.Query(const Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsPhotosSearch.Radius(const Value: Integer): Integer;
begin
  Result := List.Add('radius', Value);
end;

function TVkParamsPhotosSearch.Sort(const Value: TVkPhotoSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

function TVkParamsPhotosSearch.StartTime(const Value: TDateTime): Integer;
begin
  Result := List.Add('start_time', Value);
end;

end.

