unit VK.Entity.Counters;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkCounters = class(TVkEntity)
  private
    FApp_requests: Integer;
    FEvents: Integer;
    FFriends: Integer;
    FFriends_recommendations: Integer;
    FGifts: Integer;
    FGroups: Integer;
    FMenu_discover_badge: Integer;
    FMessages: Integer;
    FNotes: Integer;
    FNotifications: Integer;
    FPhotos: Integer;
    FSdk: Integer;
    FVideos: Integer;
    FUser_videos: integer;
    FOnline_friends: integer;
    FAlbums: integer;
    FAudios: integer;
    FPages: integer;
    FFollowers: integer;
    FMutual_friends: integer;
  public
    property AppRequests: Integer read FApp_requests write FApp_requests;
    property Events: Integer read FEvents write FEvents;
    /// <summary>
    /// ���������� ������;
    /// </summary>
    property Friends: Integer read FFriends write FFriends;
    property FriendsRecommendations: Integer read FFriends_recommendations write FFriends_recommendations;
    property Gifts: Integer read FGifts write FGifts;
    /// <summary>
    /// ���������� ���������;
    /// </summary>
    property Groups: Integer read FGroups write FGroups;
    property MenuDiscoverBadge: Integer read FMenu_discover_badge write FMenu_discover_badge;
    property Messages: Integer read FMessages write FMessages;
    /// <summary>
    /// ���������� �������;
    /// </summary>
    property Notes: Integer read FNotes write FNotes;
    property Notifications: Integer read FNotifications write FNotifications;
    /// <summary>
    /// ���������� ����������;
    /// </summary>
    property Photos: Integer read FPhotos write FPhotos;
    property SDK: Integer read FSdk write FSdk;
    /// <summary>
    /// ���������� ������������;
    /// </summary>
    property Videos: Integer read FVideos write FVideos;
    /// <summary>
    /// ���������� ������������;
    /// </summary>
    property albums: integer read FAlbums write FAlbums;
    /// <summary>
    /// ���������� ������������;
    /// </summary>
    property audios: integer read FAudios write FAudios;
    /// <summary>
    /// ���������� ������ ������;
    /// </summary>
    property online_friends: integer read FOnline_friends write FOnline_friends;
    /// <summary>
    /// ���������� ����� ������;
    /// </summary>
    property mutual_friends: integer read FMutual_friends write FMutual_friends;
    /// <summary>
    /// ���������� ������������ � �������������;
    /// </summary>
    property user_videos: integer read FUser_videos write FUser_videos;
    /// <summary>
    /// ���������� �����������;
    /// </summary>
    property followers: integer read FFollowers write FFollowers;
    /// <summary>
    /// ���������� �������� � ����� ����������� ���������.
    /// </summary>
    property pages: integer read FPages write FPages;
  end;

implementation

end.

