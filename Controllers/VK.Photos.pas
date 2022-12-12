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
    function OwnerId(const Value: Integer): TVkParamsPhotosGetAll;
    /// <summary>
    /// True � ���������� ����������� ���������� � �����������
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsPhotosGetAll;
    /// <summary>
    /// ����� ����������, ���������� � ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsPhotosGetAll;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����������. �� ��������� � 0
    /// </summary>
    function Offset(const Value: Integer): TVkParamsPhotosGetAll;
    /// <summary>
    /// True� ����� ���������� ������� ���������� � ����������� �������
    /// </summary>
    function PhotoSizes(const Value: Boolean): TVkParamsPhotosGetAll;
    /// <summary>
    /// False � ������� ��� ����������, ������� ����������� � ��������� ��������, ����� ��� "���������� �� ���� �����" (�� ���������);
    /// True � ������� ���������� ������ �� ����������� �������� ������������ ��� ����������
    /// </summary>
    function NoServiceAlbums(const Value: Boolean): TVkParamsPhotosGetAll;
    /// <summary>
    /// True � ���������� ���������� �� ���, ������ �� ���������� �� ����� ��� ������ ������������
    /// </summary>
    function NeedHidden(const Value: Boolean): TVkParamsPhotosGetAll;
    /// <summary>
    /// True � �� ���������� ����������, ������� �� ����� ��� ������ ������������
    /// (�������� ����������� ������ ��� OwnerId ������ 0, �������� NoServiceAlbums ������������)
    /// </summary>
    function SkipHidden(const Value: Boolean): TVkParamsPhotosGetAll;
  end;

  TVkParamsPhotosGet = record
    List: TParams;
    /// <summary>
    /// ������������� ��������� �������
    /// </summary>
    function OwnerId(const Value: TVkPeerId): TVkParamsPhotosGet;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: Int64): TVkParamsPhotosGet; overload;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: TVkPhotoSystemAlbum): TVkParamsPhotosGet; overload;
    /// <summary>
    /// �������������� ����������, ���������� � ������� ���������� �������
    /// </summary>
    function PhotoIds(const Value: TArrayOfInteger): TVkParamsPhotosGet;
    /// <summary>
    /// ������������� ����������, ���������� � ������� ���������� �������
    /// </summary>
    function PhotoId(const Value: Int64): TVkParamsPhotosGet;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes, comments, tags, can_comment, reposts. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean): TVkParamsPhotosGet;
    /// <summary>
    /// ���������� �������, ������� ����� ��������
    /// </summary>
    function Count(const Value: Integer = 50): TVkParamsPhotosGet;
    /// <summary>
    /// ������, ����������� ��� ��������� ������������� ������������ �������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsPhotosGet;
    /// <summary>
    /// True � ���������� ��������� ������� ���������� � ����������� �������. �� ���������: False
    /// </summary>
    function PhotoSizes(const Value: Boolean): TVkParamsPhotosGet;
    /// <summary>
    /// ��� �������, ���������� � ���� type ������ newsfeed.get, ��� ���������
    /// ������ ����������� ������������� ����������, ���� ������ ����������, ��
    /// ������� �� ��� �������. ����� ��������� �������� photo, photo_tag
    /// </summary>
    function FeedType(const Value: TVkPhotoFeedType): TVkParamsPhotosGet;
    /// <summary>
    /// ����� � �������, ������� ����� ���� ������� ������� newsfeed.get � ���� date,
    /// ��� ��������� ���� ���������� ����������� ������������� � ����������� ����
    /// ���� �� ������� ������������ ��� �������. ����� ����� ������� �������� uid ������������, � ������� ��������� �������.
    /// �������� ������ ���������� �� �������� ������� �� �����, ��� �� �����.
    /// </summary>
    function Feed(const Value: Integer): TVkParamsPhotosGet;
    /// <summary>
    /// ������� ���������� ����������
    ///  True � �������������������;
    ///  False � ���������������.
    /// </summary>
    function Rev(const Value: Boolean): TVkParamsPhotosGet;
    /// <summary>
    /// [��� ��������]
    /// </summary>
    function Uid(const Value: Integer): TVkParamsPhotosGet;
  end;

  TVkParamsAlbumsGet = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsAlbumsGet;
    /// <summary>
    /// ������������� ����� ������� �������������� �������� (�� ����� 1000)
    /// </summary>
    function AlbumIds(const Value: TArrayOfInteger): TVkParamsAlbumsGet; overload;
    /// <summary>
    /// ������������� ����� ������� �������������� ��������
    /// </summary>
    function AlbumIds(const Value: Integer): TVkParamsAlbumsGet; overload;
    /// <summary>
    /// ���������� ��������, ������� ����� �������. (�� ��������� ������������ ��� �������)
    /// </summary>
    function Count(const Value: Integer): TVkParamsAlbumsGet;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ��������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsAlbumsGet;
    /// <summary>
    /// True � ������� ���������� ����� ���������� � ����������� �������
    /// </summary>
    function PhotoSizes(const Value: Boolean): TVkParamsAlbumsGet;
    /// <summary>
    /// True � ����� ���������� ��������� �������, ������� ������������� ��������������.
    /// �������� ��������, ��� ���������� � ��������� �������� ������������ ���� � ��� ������, ���� ��� �� �������� ����������
    /// </summary>
    function NeedSystem(const Value: Boolean): TVkParamsAlbumsGet;
    /// <summary>
    /// True � ����� ���������� �������������� ���� thumb_src � ������� �����������-�������. �� ��������� ���� thumb_src �� ������������
    /// </summary>
    function NeedCovers(const Value: Boolean): TVkParamsAlbumsGet;
  end;

  TVkParamsPhotosCreateAlbum = record
    List: TParams;
    /// <summary>
    /// �������� �������
    /// </summary>
    function Title(const Value: string): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// ������������� ����������, � ������� �������� ������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// ����� �������� �������
    /// </summary>
    function Description(const Value: string): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// ��������� ����������� ��������� ������� � ����������� �������
    /// </summary>
    function PrivacyView(const Value: TVkPrivacySettings): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// ��������� ����������� ��������������� ������� � ����������� �������
    /// </summary>
    function PrivacyComment(const Value: TVkPrivacySettings): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// ��� ����� ��������� ���������� � ������ (������ ��� ������� ����������)
    /// False � ���������� ����� ��������� ��� ������������;
    /// True � ���������� ����� ��������� ������ ��������� � ��������������
    /// </summary>
    function UploadByAdminsOnly(const Value: Boolean): TVkParamsPhotosCreateAlbum;
    /// <summary>
    /// ��������� �� ��������������� ������� (������ ��� ������� ����������)
    /// False � ��������������� ��������;
    /// True � ��������������� ���������
    /// </summary>
    function CommentsDisabled(const Value: Boolean): TVkParamsPhotosCreateAlbum;
  end;

  TVkParamsPhotosEditAlbum = record
    List: TParams;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// �������� �������
    /// </summary>
    function Title(const Value: string): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// ����� �������� �������
    /// </summary>
    function Description(const Value: string): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� �������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// ��������� ����������� ��������� ������� � ����������� �������
    /// </summary>
    function PrivacyView(const Value: TArrayOfString): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// ��������� ����������� ��������������� ������� � ����������� �������
    /// </summary>
    function PrivacyComment(const Value: TArrayOfString): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// ��� ����� ��������� ���������� � ������ (������ ��� ������� ����������)
    /// False � ���������� ����� ��������� ��� ������������;
    /// True � ���������� ����� ��������� ������ ��������� � ��������������
    /// </summary>
    function UploadByAdminsOnly(const Value: Boolean): TVkParamsPhotosEditAlbum;
    /// <summary>
    /// ��������� �� ��������������� ������� (������ ��� ������� ����������)
    /// False � ��������������� ��������;
    /// True � ��������������� ���������
    /// </summary>
    function CommentsDisabled(const Value: Boolean): TVkParamsPhotosEditAlbum;
  end;

  TVkParamsPhotosCreateComment = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPhotosCreateComment;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PhotoId(const Value: Integer): TVkParamsPhotosCreateComment;
    /// <summary>
    /// ����� ����������� (�������� ������������, ���� �� ����� �������� Attachments).
    /// ������������ ���������� ��������: 2048
    /// </summary>
    function Message(const Value: string): TVkParamsPhotosCreateComment;
    /// <summary>
    /// ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsPhotosCreateComment;
    /// <summary>
    /// ������ �������� �����������, ���� OwnerId ������ 0 (����������� � ���������� ������)
    ///  True � ����������� ����� ����������� �� ����� ������;
    ///  False � ����������� ����� ����������� �� ����� ������������
    /// </summary>
    function FromGroup(const Value: Boolean = False): TVkParamsPhotosCreateComment;
    /// <summary>
    /// ������������� �����������, � ����� �� ������� ����� �������� �������
    /// </summary>
    function ReplyToComment(const Value: Integer): TVkParamsPhotosCreateComment;
    /// <summary>
    /// ������������� �������, ������� ����� ���������� � �����������
    /// </summary>
    function StickerId(const Value: Cardinal): TVkParamsPhotosCreateComment;
    /// <summary>
    /// ���� �������
    /// </summary>
    function AccessKey(const Value: string): TVkParamsPhotosCreateComment;
    /// <summary>
    /// ���������� �������� ��� �������������� ��������� �������� ������ � ���� �� �����������
    /// </summary>
    function Guid(const Value: string): TVkParamsPhotosCreateComment;
  end;

  TVkParamsPhotosEdit = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPhotosEdit;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PhotoId(const Value: Integer): TVkParamsPhotosEdit;
    /// <summary>
    /// ����� ����� �������� � ����������. ���� �������� �� �����, �� ���������, ��� �� ����� ������ ������
    /// </summary>
    function Caption(const Value: string): TVkParamsPhotosEdit;
    /// <summary>
    /// �������������� ������
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsPhotosEdit;
    /// <summary>
    /// �������������� �������
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsPhotosEdit;
    /// <summary>
    /// �������� �����
    /// </summary>
    function PlaceStr(const Value: string): TVkParamsPhotosEdit;
    /// <summary>
    /// Id � Foursquare
    /// </summary>
    function FoursquareId(const Value: string): TVkParamsPhotosEdit;
    /// <summary>
    /// ������� ����� (False � �� �������, True � �������)
    /// </summary>
    function DeletePlace(const Value: Boolean = False): TVkParamsPhotosEdit;
  end;

  TVkParamsPhotosSave = record
    List: TParams;
    /// <summary>
    /// ������������� �������, � ������� ���������� ��������� ����������
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsPhotosSave;
    /// <summary>
    /// ������������� ����������, � ������� ���������� ��������� ����������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPhotosSave;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Server(const Value: string): TVkParamsPhotosSave;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function PhotosList(const Value: TArrayOfString): TVkParamsPhotosSave;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Hash(const Value: string): TVkParamsPhotosSave;
    /// <summary>
    /// �������������� ������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsPhotosSave;
    /// <summary>
    /// �������������� �������, �������� � �������� (�� -180 �� 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsPhotosSave;
    /// <summary>
    /// ����� �������� ���������� (�������� 2048 ��������)
    /// </summary>
    function Caption(const Value: string): TVkParamsPhotosSave;
  end;

  TVkParamsPhotosEditComment = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPhotosEditComment;
    /// <summary>
    /// ������������� �����������
    /// </summary>
    function CommentId(const Value: Integer): TVkParamsPhotosEditComment;
    /// <summary>
    /// ����� ����� �����������. ������������ ��������, ���� �� ����� �������� attachments.
    /// ������������ ���������� ��������: 2048
    /// </summary>
    function Message(const Value: string): TVkParamsPhotosEditComment;
    /// <summary>
    /// ����� ������ ��������, ����������� � �����������
    /// </summary>
    function Attachments(const Value: TAttachmentArray): TVkParamsPhotosEditComment;
  end;

  TVkParamsPhotosGetAlbumsCount = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, ���������� �������� �������� ���������� ��������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPhotosGetAlbumsCount;
    /// <summary>
    /// ������������� ����������, ���������� �������� �������� ���������� ��������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPhotosGetAlbumsCount;
  end;

  TVkParamsPhotosGetAllComments = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPhotosGetAllComments;
    /// <summary>
    /// ������������� �������. ���� �������� �� �����, �� ���������, ��� ���������� �������� ����������� �� ���� �������� ������������ ��� ����������
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsPhotosGetAllComments;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes. �� ��������� ���� likes �� ������������
    /// </summary>
    function NeedLikes(const Value: Boolean): TVkParamsPhotosGetAllComments;
    /// <summary>
    /// ���������� ������������, ������� ���������� ��������. ���� �������� �� �����, �� ��������� ��� �� ����� 20. ������������ �������� ��������� 100.
    /// �������� ��������, ���� ��� ������������� ��������� offset ��� ��������� �������� ������ ������ 10000 ������������.
    /// </summary>
    function Count(const Value: Integer): TVkParamsPhotosGetAllComments;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������������.
    /// ���� �������� �� �����, �� ���������, ��� �� ����� 0
    /// </summary>
    function Offset(const Value: Integer): TVkParamsPhotosGetAllComments;
  end;

  TVkParamsPhotosGetComments = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPhotosGetComments;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PhotoId(const Value: Integer): TVkParamsPhotosGetComments;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes. �� ���������: False
    /// </summary>
    function NeedLikes(const Value: Boolean = False): TVkParamsPhotosGetComments;
    /// <summary>
    /// ���������� ������������, ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsPhotosGetComments;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ������������. �� ���������: 0
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsPhotosGetComments;
    /// <summary>
    /// ������������� �����������, ������� � �������� ����� ������� ������
    /// </summary>
    function StartCommentId(const Value: Integer): TVkParamsPhotosGetComments;
    /// <summary>
    /// ������� ���������� ������������
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsPhotosGetComments;
    /// <summary>
    /// ���� ������� � ����������
    /// </summary>
    function AccessKey(const Value: string): TVkParamsPhotosGetComments;
    /// <summary>
    /// True � � ������ ����� ���������� �������������� ���� profiles � groups,
    /// ���������� ���������� � ������������� � �����������. �� ���������: False
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsPhotosGetComments;
    /// <summary>
    /// ������ �������������� ����� ��������, ������� ���������� �������
    /// </summary>
    function Fields(const Value: TVkProfileFields): TVkParamsPhotosGetComments;
  end;

  TVkParamsPhotosGetMarketUploadServer = record
    List: TParams;
    /// <summary>
    /// ������������� ����������, ��� �������� ���������� ��������� ���������� ������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPhotosGetMarketUploadServer;
    /// <summary>
    /// �������� �� ���������� �������� ������ (True � ���������� ��� �������, False � �������������� ����������)
    /// </summary>
    function MainPhoto(const Value: Boolean): TVkParamsPhotosGetMarketUploadServer;
    /// <summary>
    /// ���������� ��� ������� ���������� (������� ������ ����)
    /// </summary>
    function Crop(const Value: TPoint): TVkParamsPhotosGetMarketUploadServer;
    /// <summary>
    /// ������ ���������� ����� ������� � px (����������� �������� 400)
    /// </summary>
    function CropWidth(const Value: Integer): TVkParamsPhotosGetMarketUploadServer;
  end;

  TVkParamsPhotosGetUserPhotos = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, ������ ���������� ��� �������� ����� ��������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPhotosGetUserPhotos;
    /// <summary>
    /// True � ����� ���������� �������������� ���� likes, comments, tags, can_comment.
    /// ���� comments � tags �������� ������ ���������� ��������. �� ��������� ������ ���� �� ������������
    /// </summary>
    function Extended(const Value: Boolean = False): TVkParamsPhotosGetUserPhotos;
    /// <summary>
    /// ���������� ����������, ������� ���������� ��������
    /// </summary>
    function Count(const Value: Integer = 20): TVkParamsPhotosGetUserPhotos;
    /// <summary>
    /// ��������, ����������� ��� ������� ������������� ������������ ����������
    /// </summary>
    function Offset(const Value: Integer = 0): TVkParamsPhotosGetUserPhotos;
    /// <summary>
    /// ���������� ����������� (�� ���� ���������� �������)
    /// </summary>
    function Sort(const Value: TVkSort): TVkParamsPhotosGetUserPhotos;
  end;

  TVkParamsPhotosReorderAlbums = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPhotosReorderAlbums;
    /// <summary>
    /// ������������� �������
    /// </summary>
    function AlbumId(const Value: Integer): TVkParamsPhotosReorderAlbums;
    /// <summary>
    /// ������������� �������, ����� ������� ������� ��������� ������
    /// </summary>
    function Before(const Value: Integer): TVkParamsPhotosReorderAlbums;
    /// <summary>
    /// ������������� �������, ����� �������� ������� ��������� ������
    /// </summary>
    function After(const Value: Integer): TVkParamsPhotosReorderAlbums;
  end;

  TVkParamsPhotosReorderPhotos = record
    List: TParams;
    /// <summary>
    /// ������������� ������������ ��� ����������, �������� ����������� ����������
    /// </summary>
    function OwnerId(const Value: Integer): TVkParamsPhotosReorderPhotos;
    /// <summary>
    /// ������������� ����������
    /// </summary>
    function PhotoId(const Value: Integer): TVkParamsPhotosReorderPhotos;
    /// <summary>
    /// ������������� ����������, ����� ������� ������� ��������� ����������. ���� �������� �� ������, ���������� ����� �������� ���������
    /// </summary>
    function Before(const Value: Integer): TVkParamsPhotosReorderPhotos;
    /// <summary>
    /// ������������� ����������, ����� ������� ������� ��������� ����������. ���� �������� �� ������, ���������� ����� �������� ������
    /// </summary>
    function After(const Value: Integer): TVkParamsPhotosReorderPhotos;
  end;

  TVkParamsPhotosSaveMarketPhoto = record
    List: TParams;
    /// <summary>
    /// ������������ ������, ��� ������� ����� ��������� ����������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// �������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Photo(const Value: string): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// �������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Server(const Value: Integer): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// �������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Hash(const Value: string): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������.
    /// ������������ ��������, ���� �� ����� �������� ���� ��� ������� MainPhoto = True
    /// </summary>
    function CropData(const Value: string): TVkParamsPhotosSaveMarketPhoto;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������.
    /// ������������ ��������, ���� �� ����� �������� ���� ��� ������� MainPhoto = True
    /// </summary>
    function CropHash(const Value: string): TVkParamsPhotosSaveMarketPhoto;
  end;

  TVkParamsPhotosSaveWallPhoto = record
    List: TParams;
    /// <summary>
    /// ������������� ������������, �� ����� �������� ����� ��������� ����������
    /// </summary>
    function UserId(const Value: Integer): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// ������������� ����������, �� ����� �������� ����� ��������� ����������
    /// </summary>
    function GroupId(const Value: Integer): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Photo(const Value: string): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Server(const Value: Integer): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// ��������, ������������ � ���������� �������� ���������� �� ������
    /// </summary>
    function Hash(const Value: string): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// �������������� ������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// �������������� �������, �������� � �������� (�� -180 �� 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsPhotosSaveWallPhoto;
    /// <summary>
    /// ����� �������� ���������� (�������� 2048 ��������)
    /// </summary>
    function Caption(const Value: string): TVkParamsPhotosSaveWallPhoto;
  end;

  TVkParamsPhotosSearch = record
    List: TParams;
    /// <summary>
    /// ������ ���������� �������, ��������
    /// </summary>
    function Query(const Value: string): TVkParamsPhotosSearch;
    /// <summary>
    /// �������������� ������ �������, �������� � �������� (�� -90 �� 90)
    /// </summary>
    function Latitude(const Value: Extended): TVkParamsPhotosSearch;
    /// <summary>
    /// �������������� ������� �������, �������� � �������� (�� -180 �� 180)
    /// </summary>
    function Longitude(const Value: Extended): TVkParamsPhotosSearch;
    /// <summary>
    /// ����� � ������� unixtime, �� ������ �������� ������ ���� ���� ��������� ��������� ����������
    /// </summary>
    function StartTime(const Value: TDateTime): TVkParamsPhotosSearch;
    /// <summary>
    /// ����� � ������� unixtime, �� ����� �������� ������ ���� ���� ��������� ��������� ����������
    /// </summary>
    function EndTime(const Value: TDateTime): TVkParamsPhotosSearch;
    /// <summary>
    /// ���������� �����������
    /// </summary>
    function Sort(const Value: TVkPhotoSort): TVkParamsPhotosSearch;
    /// <summary>
    /// �������� ������������ ������ ��������� ���������� ��� ������� ������������� ������������
    /// </summary>
    function Offset(const Value: Integer): TVkParamsPhotosSearch;
    /// <summary>
    /// ���������� ������������ ���������� (������������ �������� 1000)
    /// </summary>
    function Count(const Value: Integer = 100): TVkParamsPhotosSearch;
    /// <summary>
    /// ������ ������ � ������. (�������� ����� �����������, ������� �������� ���������� �� ���� ����� ���������� �� ���������). ����� ��������� ��������: 10, 100, 800, 6000, 50000
    /// </summary>
    function Radius(const Value: Integer = 5000): TVkParamsPhotosSearch;
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
    function Upload(var Response: TVkPhotoUploadResponse; const UploadUrl: string; const FileNames: array of string): Boolean; overload;
    /// <summary>
    /// �������� ����������
    /// </summary>
    function Upload(var Response: TVkPhotoUploadResponse; const UploadUrl: string; Stream: TStream; const FileName: string): Boolean; overload;
    /// <summary>
    /// �������� ���������� ��� �������� � ���������
    /// </summary>
    function UploadForMessage(var Photos: TVkPhotos; const PeerId: Integer; const FileNames: array of string): Boolean; overload;
    /// <summary>
    /// �������� ���������� ��� �������� � ���������
    /// </summary>
    function UploadForMessage(var Photos: TAttachmentArray; const PeerId: Integer; const FileNames: array of string): Boolean; overload;
    /// <summary>
    /// �������� ���������� ��� �������� � ���������
    /// </summary>
    function UploadForMessage(var Photos: TAttachmentArray; const PeerId: Integer; const FileName: string; Stream: TStream): Boolean; overload;
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

function TPhotosController.Upload(var Response: TVkPhotoUploadResponse; const UploadUrl: string; Stream: TStream; const FileName: string): Boolean;
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

function TPhotosController.Upload(var Response: TVkPhotoUploadResponse; const UploadUrl: string; const FileNames: array of string): Boolean;
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
  Result := Handler.Execute('photos.getById', Params).GetObjects(Items);
end;

function TPhotosController.GetChatUploadServer(var UploadUrl: string; ChatId: Integer; Crop: TPoint; CropWidth: Integer): Boolean;
var
  Params: TParams;
  Item: TVkPhotoGetUploadResponse;
begin
  Result := False;
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
    if GetObject(Item) then
    begin
      try
        try
          UploadUrl := Item.UploadUrl;
          Result := not UploadUrl.IsEmpty;
        except
          Result := False;
        end;
      finally
        Item.Free;
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
  Result := False;
  with Handler.Execute('photos.getMarketAlbumUploadServer', ['group_id', GroupId.ToString]) do
  begin
    if GetObject(Item) then
    begin
      try
        try
          UploadUrl := Item.UploadUrl;
          Result := not UploadUrl.IsEmpty;
        except
          Result := False;
        end;
      finally
        Item.Free;
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
  Result := False;
  with Handler.Execute('photos.getMarketUploadServer', Params) do
  begin
    if GetObject(Item) then
    begin
      try
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
  Result := False;
  Params.Add('owner_id', OwnerId);
  with Handler.Execute('photos.getOwnerPhotoUploadServer', Params) do
  begin
    if GetObject(Item) then
    begin
      try
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
    ['reason', Ord(Reason).ToString]]).
    ResponseIsTrue;
end;

function TPhotosController.ReportComment(OwnerId, CommentId: Integer; Reason: TVkMediaReportReason): Boolean;
begin
  Result := Handler.Execute('photos.reportComment', [
    ['owner_id', OwnerId.ToString],
    ['comment_id', CommentId.ToString],
    ['reason', Ord(Reason).ToString]]).
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
  Result := False;
  Params.Add('group_id', GroupId);
  Params.Add('crop_x', CropLeft.X);
  Params.Add('crop_y', CropLeft.y);
  Params.Add('crop_x2', CropRight.X);
  Params.Add('crop_y2', CropRight.y);
  with Handler.Execute('photos.getOwnerCoverPhotoUploadServer', Params) do
  begin
    if GetObject(Item) then
    begin
      try
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
    if Upload(Response, Server, FileName) then
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

function TPhotosController.UploadForMessage(var Photos: TAttachmentArray; const PeerId: Integer; const FileName: string; Stream: TStream): Boolean;
var
  Items: TVkPhotos;
var
  Url: string;
  Response: TVkPhotoUploadResponse;
begin
  Result := False;
  if GetMessagesUploadServer(Url, PeerId) then
  begin
    if Upload(Response, Url, Stream, FileName) then
    begin
      try
        Result := SaveMessagesPhoto(Items, Response);
      finally
        Response.Free;
      end;
    end;
  end;
  if Result then
  begin
    Photos := Items.ToAttachments;
    Items.Free;
  end;
end;

function TPhotosController.UploadForMessage(var Photos: TAttachmentArray; const PeerId: Integer; const FileNames: array of string): Boolean;
var
  Items: TVkPhotos;
begin
  Result := UploadForMessage(Items, PeerId, FileNames);
  if Result then
  begin
    Photos := Items.ToAttachments;
    Items.Free;
  end;
end;

function TPhotosController.UploadForMessage(var Photos: TVkPhotos; const PeerId: Integer; const FileNames: array of string): Boolean;
var
  Url: string;
  Response: TVkPhotoUploadResponse;
begin
  Result := False;
  if GetMessagesUploadServer(Url, PeerId) then
  begin
    if Upload(Response, Url, FileNames) then
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
      if Upload(Response, PhotoUpload.UploadUrl, FileNames) then
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

function TVkParamsPhotosGetAll.Count(const Value: Integer): TVkParamsPhotosGetAll;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.Extended(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.NeedHidden(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('need_hidden', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.NoServiceAlbums(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('no_service_albums', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.Offset(const Value: Integer): TVkParamsPhotosGetAll;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.OwnerId(const Value: Integer): TVkParamsPhotosGetAll;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.PhotoSizes(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('photo_sizes', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAll.SkipHidden(const Value: Boolean): TVkParamsPhotosGetAll;
begin
  List.Add('skip_hidden', Value);
  Result := Self;
end;

{ TVkParamsAlbumsGet }

function TVkParamsAlbumsGet.AlbumIds(const Value: TArrayOfInteger): TVkParamsAlbumsGet;
begin
  List.Add('album_ids', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.AlbumIds(const Value: Integer): TVkParamsAlbumsGet;
begin
  List.Add('album_ids', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.Count(const Value: Integer): TVkParamsAlbumsGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.NeedCovers(const Value: Boolean): TVkParamsAlbumsGet;
begin
  List.Add('need_covers', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.NeedSystem(const Value: Boolean): TVkParamsAlbumsGet;
begin
  List.Add('need_system', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.Offset(const Value: Integer): TVkParamsAlbumsGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.OwnerId(const Value: Integer): TVkParamsAlbumsGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsAlbumsGet.PhotoSizes(const Value: Boolean): TVkParamsAlbumsGet;
begin
  List.Add('photo_sizes', Value);
  Result := Self;
end;

{ TVkPhotosGetParams }

function TVkParamsPhotosGet.AlbumId(const Value: TVkPhotoSystemAlbum): TVkParamsPhotosGet;
begin
  List.Add('album_id', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosGet.AlbumId(const Value: Int64): TVkParamsPhotosGet;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Count(const Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Extended(const Value: Boolean): TVkParamsPhotosGet;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Feed(const Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('feed', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.FeedType(const Value: TVkPhotoFeedType): TVkParamsPhotosGet;
begin
  List.Add('feed_type', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosGet.Offset(const Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.OwnerId(const Value: TVkPeerId): TVkParamsPhotosGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.PhotoId(const Value: Int64): TVkParamsPhotosGet;
begin
  List.Add('photo_ids', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.PhotoIds(const Value: TArrayOfInteger): TVkParamsPhotosGet;
begin
  List.Add('photo_ids', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.PhotoSizes(const Value: Boolean): TVkParamsPhotosGet;
begin
  List.Add('photo_sizes', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Rev(const Value: Boolean): TVkParamsPhotosGet;
begin
  List.Add('rev', Value);
  Result := Self;
end;

function TVkParamsPhotosGet.Uid(const Value: Integer): TVkParamsPhotosGet;
begin
  List.Add('uid', Value);
  Result := Self;
end;

{ TVkParamsPhotosCreateAlbum }

function TVkParamsPhotosCreateAlbum.CommentsDisabled(const Value: Boolean): TVkParamsPhotosCreateAlbum;
begin
  List.Add('comments_disabled', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.Description(const Value: string): TVkParamsPhotosCreateAlbum;
begin
  List.Add('description', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.GroupId(const Value: Integer): TVkParamsPhotosCreateAlbum;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.PrivacyComment(const Value: TVkPrivacySettings): TVkParamsPhotosCreateAlbum;
begin
  List.Add('privacy_comment', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.PrivacyView(const Value: TVkPrivacySettings): TVkParamsPhotosCreateAlbum;
begin
  List.Add('privacy_view', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.Title(const Value: string): TVkParamsPhotosCreateAlbum;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateAlbum.UploadByAdminsOnly(const Value: Boolean): TVkParamsPhotosCreateAlbum;
begin
  List.Add('upload_by_admins_only', Value);
  Result := Self;
end;

{ TVkParamsPhotosCreateComment }

function TVkParamsPhotosCreateComment.AccessKey(const Value: string): TVkParamsPhotosCreateComment;
begin
  List.Add('access_key', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.Attachments(const Value: TAttachmentArray): TVkParamsPhotosCreateComment;
begin
  List.Add('attachments', Value.ToStrings);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.FromGroup(const Value: Boolean): TVkParamsPhotosCreateComment;
begin
  List.Add('from_group', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.Guid(const Value: string): TVkParamsPhotosCreateComment;
begin
  List.Add('guid', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.Message(const Value: string): TVkParamsPhotosCreateComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.OwnerId(const Value: Integer): TVkParamsPhotosCreateComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.PhotoId(const Value: Integer): TVkParamsPhotosCreateComment;
begin
  List.Add('photo_id', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.ReplyToComment(const Value: Integer): TVkParamsPhotosCreateComment;
begin
  List.Add('reply_to_comment', Value);
  Result := Self;
end;

function TVkParamsPhotosCreateComment.StickerId(const Value: Cardinal): TVkParamsPhotosCreateComment;
begin
  List.Add('sticker_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosEdit }

function TVkParamsPhotosEdit.Caption(const Value: string): TVkParamsPhotosEdit;
begin
  List.Add('caption', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.DeletePlace(const Value: Boolean): TVkParamsPhotosEdit;
begin
  List.Add('delete_place', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.FoursquareId(const Value: string): TVkParamsPhotosEdit;
begin
  List.Add('foursquare_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.Latitude(const Value: Extended): TVkParamsPhotosEdit;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.Longitude(const Value: Extended): TVkParamsPhotosEdit;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.OwnerId(const Value: Integer): TVkParamsPhotosEdit;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.PhotoId(const Value: Integer): TVkParamsPhotosEdit;
begin
  List.Add('photo_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEdit.PlaceStr(const Value: string): TVkParamsPhotosEdit;
begin
  List.Add('place_str', Value);
  Result := Self;
end;

{ TVkParamsPhotosEditAlbum }

function TVkParamsPhotosEditAlbum.AlbumId(const Value: Integer): TVkParamsPhotosEditAlbum;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.CommentsDisabled(const Value: Boolean): TVkParamsPhotosEditAlbum;
begin
  List.Add('comments_disabled', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.Description(const Value: string): TVkParamsPhotosEditAlbum;
begin
  List.Add('description', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.OwnerId(const Value: Integer): TVkParamsPhotosEditAlbum;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.PrivacyComment(const Value: TArrayOfString): TVkParamsPhotosEditAlbum;
begin
  List.Add('privacy_comment', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.PrivacyView(const Value: TArrayOfString): TVkParamsPhotosEditAlbum;
begin
  List.Add('privacy_view', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.Title(const Value: string): TVkParamsPhotosEditAlbum;
begin
  List.Add('title', Value);
  Result := Self;
end;

function TVkParamsPhotosEditAlbum.UploadByAdminsOnly(const Value: Boolean): TVkParamsPhotosEditAlbum;
begin
  List.Add('upload_by_admins_only', Value);
  Result := Self;
end;

{ TVkParamsPhotosEditComment }

function TVkParamsPhotosEditComment.Attachments(const Value: TAttachmentArray): TVkParamsPhotosEditComment;
begin
  List.Add('attachments', Value.ToStrings);
  Result := Self;
end;

function TVkParamsPhotosEditComment.CommentId(const Value: Integer): TVkParamsPhotosEditComment;
begin
  List.Add('comment_id', Value);
  Result := Self;
end;

function TVkParamsPhotosEditComment.Message(const Value: string): TVkParamsPhotosEditComment;
begin
  List.Add('message', Value);
  Result := Self;
end;

function TVkParamsPhotosEditComment.OwnerId(const Value: Integer): TVkParamsPhotosEditComment;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetAlbumsCount }

function TVkParamsPhotosGetAlbumsCount.GroupId(const Value: Integer): TVkParamsPhotosGetAlbumsCount;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAlbumsCount.UserId(const Value: Integer): TVkParamsPhotosGetAlbumsCount;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetAllComments }

function TVkParamsPhotosGetAllComments.AlbumId(const Value: Integer): TVkParamsPhotosGetAllComments;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAllComments.Count(const Value: Integer): TVkParamsPhotosGetAllComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAllComments.NeedLikes(const Value: Boolean): TVkParamsPhotosGetAllComments;
begin
  List.Add('need_likes', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAllComments.Offset(const Value: Integer): TVkParamsPhotosGetAllComments;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGetAllComments.OwnerId(const Value: Integer): TVkParamsPhotosGetAllComments;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetComments }

function TVkParamsPhotosGetComments.AccessKey(const Value: string): TVkParamsPhotosGetComments;
begin
  List.Add('access_key', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Count(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Extended(const Value: Boolean): TVkParamsPhotosGetComments;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Fields(const Value: TVkProfileFields): TVkParamsPhotosGetComments;
begin
  List.Add('fields', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosGetComments.NeedLikes(const Value: Boolean): TVkParamsPhotosGetComments;
begin
  List.Add('need_likes', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Offset(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.OwnerId(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.PhotoId(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('photo_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetComments.Sort(const Value: TVkSort): TVkParamsPhotosGetComments;
begin
  List.Add('sort', Value.ToString);
  Result := Self;
end;

function TVkParamsPhotosGetComments.StartCommentId(const Value: Integer): TVkParamsPhotosGetComments;
begin
  List.Add('start_comment_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetMarketUploadServer }

function TVkParamsPhotosGetMarketUploadServer.Crop(const Value: TPoint): TVkParamsPhotosGetMarketUploadServer;
begin
  List.Add('crop_x', Value.X).Add('crop_y', Value.Y);
  Result := Self;
end;

function TVkParamsPhotosGetMarketUploadServer.CropWidth(const Value: Integer): TVkParamsPhotosGetMarketUploadServer;
begin
  List.Add('crop_width', Value);
  Result := Self;
end;

function TVkParamsPhotosGetMarketUploadServer.GroupId(const Value: Integer): TVkParamsPhotosGetMarketUploadServer;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosGetMarketUploadServer.MainPhoto(const Value: Boolean): TVkParamsPhotosGetMarketUploadServer;
begin
  List.Add('main_photo', Value);
  Result := Self;
end;

{ TVkParamsPhotosGetUserPhotos }

function TVkParamsPhotosGetUserPhotos.Count(const Value: Integer): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosGetUserPhotos.Extended(const Value: Boolean): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('extended', Value);
  Result := Self;
end;

function TVkParamsPhotosGetUserPhotos.Offset(const Value: Integer): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPhotosGetUserPhotos.Sort(const Value: TVkSort): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

function TVkParamsPhotosGetUserPhotos.UserId(const Value: Integer): TVkParamsPhotosGetUserPhotos;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosReorderAlbums }

function TVkParamsPhotosReorderAlbums.After(const Value: Integer): TVkParamsPhotosReorderAlbums;
begin
  List.Add('after', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderAlbums.AlbumId(const Value: Integer): TVkParamsPhotosReorderAlbums;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderAlbums.Before(const Value: Integer): TVkParamsPhotosReorderAlbums;
begin
  List.Add('before', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderAlbums.OwnerId(const Value: Integer): TVkParamsPhotosReorderAlbums;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosReorderPhotos }

function TVkParamsPhotosReorderPhotos.After(const Value: Integer): TVkParamsPhotosReorderPhotos;
begin
  List.Add('after', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderPhotos.Before(const Value: Integer): TVkParamsPhotosReorderPhotos;
begin
  List.Add('before', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderPhotos.OwnerId(const Value: Integer): TVkParamsPhotosReorderPhotos;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsPhotosReorderPhotos.PhotoId(const Value: Integer): TVkParamsPhotosReorderPhotos;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosSave }

function TVkParamsPhotosSave.AlbumId(const Value: Integer): TVkParamsPhotosSave;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Caption(const Value: string): TVkParamsPhotosSave;
begin
  List.Add('caption', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.GroupId(const Value: Integer): TVkParamsPhotosSave;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Hash(const Value: string): TVkParamsPhotosSave;
begin
  List.Add('hash', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Latitude(const Value: Extended): TVkParamsPhotosSave;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Longitude(const Value: Extended): TVkParamsPhotosSave;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.PhotosList(const Value: TArrayOfString): TVkParamsPhotosSave;
begin
  List.Add('photos_list', Value);
  Result := Self;
end;

function TVkParamsPhotosSave.Server(const Value: string): TVkParamsPhotosSave;
begin
  List.Add('server', Value);
  Result := Self;
end;

{ TVkParamsPhotosSaveMarketPhoto }

function TVkParamsPhotosSaveMarketPhoto.CropData(const Value: string): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('crop_data', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.CropHash(const Value: string): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('crop_hash', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.GroupId(const Value: Integer): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.Hash(const Value: string): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('hash', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.Photo(const Value: string): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('photo', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveMarketPhoto.Server(const Value: Integer): TVkParamsPhotosSaveMarketPhoto;
begin
  List.Add('server', Value);
  Result := Self;
end;

{ TVkParamsPhotosSaveWallPhoto }

function TVkParamsPhotosSaveWallPhoto.Caption(const Value: string): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('caption', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.GroupId(const Value: Integer): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('group_id', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Hash(const Value: string): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('hash', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Latitude(const Value: Extended): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('latitude', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Longitude(const Value: Extended): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('longitude', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Photo(const Value: string): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('photo', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.Server(const Value: Integer): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('server', Value);
  Result := Self;
end;

function TVkParamsPhotosSaveWallPhoto.UserId(const Value: Integer): TVkParamsPhotosSaveWallPhoto;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsPhotosSearch }

function TVkParamsPhotosSearch.Count(const Value: Integer): TVkParamsPhotosSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.EndTime(const Value: TDateTime): TVkParamsPhotosSearch;
begin
  List.Add('end_time', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Latitude(const Value: Extended): TVkParamsPhotosSearch;
begin
  List.Add('lat', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Longitude(const Value: Extended): TVkParamsPhotosSearch;
begin
  List.Add('long', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Offset(const Value: Integer): TVkParamsPhotosSearch;
begin
  List.Add('offse', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Query(const Value: string): TVkParamsPhotosSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Radius(const Value: Integer): TVkParamsPhotosSearch;
begin
  List.Add('radius', Value);
  Result := Self;
end;

function TVkParamsPhotosSearch.Sort(const Value: TVkPhotoSort): TVkParamsPhotosSearch;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

function TVkParamsPhotosSearch.StartTime(const Value: TDateTime): TVkParamsPhotosSearch;
begin
  List.Add('start_time', Value);
  Result := Self;
end;

end.

