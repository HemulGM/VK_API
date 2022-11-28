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
    FCalls: Integer;
    FMessages_unread_unmuted: Integer;
    FMenu_new_clips_badge: Integer;
    FMenu_superapp_friends_badge: Integer;
    FMenu_clips_badge: Integer;
  public
    /// <summary>
    /// ���������� ������������;
    /// </summary>
    property Albums: integer read FAlbums write FAlbums;
    property AppRequests: Integer read FApp_requests write FApp_requests;
    /// <summary>
    /// ���������� ������������;
    /// </summary>
    property Audios: integer read FAudios write FAudios;
    property Calls: Integer read FCalls write FCalls;
    property Events: Integer read FEvents write FEvents;
    /// <summary>
    /// ���������� �����������;
    /// </summary>
    property Followers: integer read FFollowers write FFollowers;
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
    property MenuClipsBadge: Integer read FMenu_clips_badge write FMenu_clips_badge;
    property MenuDiscoverBadge: Integer read FMenu_discover_badge write FMenu_discover_badge;
    property MenuNewClipsBadge: Integer read FMenu_new_clips_badge write FMenu_new_clips_badge;
    property MenuSuperappFriendsBadge: Integer read FMenu_superapp_friends_badge write FMenu_superapp_friends_badge;
    property Messages: Integer read FMessages write FMessages;
    property MessagesUnreadUnmuted: Integer read FMessages_unread_unmuted write FMessages_unread_unmuted;
    /// <summary>
    /// ���������� ����� ������;
    /// </summary>
    property MutualFriends: integer read FMutual_friends write FMutual_friends;
    /// <summary>
    /// ���������� �������;
    /// </summary>
    property Notes: Integer read FNotes write FNotes;
    property Notifications: Integer read FNotifications write FNotifications;
    /// <summary>
    /// ���������� ������ ������;
    /// </summary>
    property OnlineFriends: integer read FOnline_friends write FOnline_friends;
    /// <summary>
    /// ���������� ����������;
    /// </summary>
    property Photos: Integer read FPhotos write FPhotos;
    /// <summary>
    /// ���������� �������� � ����� ����������� ���������.
    /// </summary>
    property Pages: integer read FPages write FPages;
    property SDK: Integer read FSdk write FSdk;
    /// <summary>
    /// ���������� ������������ � �������������;
    /// </summary>
    property UserVideos: integer read FUser_videos write FUser_videos;
    /// <summary>
    /// ���������� ������������;
    /// </summary>
    property Videos: Integer read FVideos write FVideos;
    //property messages_folders: TArray<?> read FMessages_folders write FMessages_folders;
  end;

implementation

end.

