unit VK.Entity.Message.Chat;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Group,
  VK.Entity.Common, VK.Types, VK.Entity.Common.List;

type
  TVkChat = class(TVkObject)
  private
    FAdmin_id: Integer;
    FIs_default_photo: Boolean;
    FMembers_count: Integer;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FTitle: string;
    FType: string;
    FUsers: TArray<TVkProfile>;
  public
    property AdminId: Integer read FAdmin_id write FAdmin_id;
    property IsDefaultPhoto: Boolean read FIs_default_photo write FIs_default_photo;
    property MembersCount: Integer read FMembers_count write FMembers_count;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property Title: string read FTitle write FTitle;
    property&Type: string read FType write FType;
    property Users: TArray<TVkProfile> read FUsers write FUsers;
    destructor Destroy; override;
  end;

  TVkChats = TVkEntityList<TVkChat>;

  TVkChatPhoto = class
  private
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
  public
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
  end;

  TVkChatInfoMessage = class(TVkEntity)
  private
    FChat: TVkChat;
    FMessage_id: Integer;
  public
    property Chat: TVkChat read FChat write FChat;
    property MessageId: Integer read FMessage_id write FMessage_id;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVkChatPreviewInfo = class
  private
    FAdmin_id: Integer;
    FMembers: TArrayOfInteger;
    FTitle: string;
    FPhoto: TVkChatPhoto;
    FLocal_id: Integer;
  public
    property AdminId: Integer read FAdmin_id write FAdmin_id;
    property LocalId: Integer read FLocal_id write FLocal_id;
    property Members: TArrayOfInteger read FMembers write FMembers;
    property Title: string read FTitle write FTitle;
    property Photo: TVkChatPhoto read FPhoto write FPhoto;
  end;

  TVkEmail = class(TVkObject)
  private
    FAddress: string;
  public
    property Address: string read FAddress write FAddress;
  end;

  TVkChatPreview = class(TVkEntity)
  private
    FChat: TVkChatPreviewInfo;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
    FEmails: TArray<TVkEmail>;
  public
    property Preview: TVkChatPreviewInfo read FChat write FChat;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Emails: TArray<TVkEmail> read FEmails write FEmails;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkChat}

destructor TVkChat.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FUsers);
  inherited;
end;

{TVkChatInfoMessage}

constructor TVkChatInfoMessage.Create;
begin
  inherited;
  FChat := TVkChat.Create();
end;

destructor TVkChatInfoMessage.Destroy;
begin
  FChat.Free;
  inherited;
end;

{ TVkChatPreview }

destructor TVkChatPreview.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkEmail>(FEmails);
  inherited;
end;

end.

