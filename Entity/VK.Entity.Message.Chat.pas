unit VK.Entity.Message.Chat;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common, VK.Types;

type
  TVkChat = class
  private
    FAdmin_id: Integer;
    FId: Extended;
    FIs_default_photo: Boolean;
    FMembers_count: Extended;
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
    FTitle: string;
    FType: string;
    FUsers: TArray<TVkProfile>;
  public
    property AdminId: Integer read FAdmin_id write FAdmin_id;
    property Id: Extended read FId write FId;
    property IsDefaultPhoto: Boolean read FIs_default_photo write FIs_default_photo;
    property MembersCount: Extended read FMembers_count write FMembers_count;
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    property Title: string read FTitle write FTitle;
    property&Type: string read FType write FType;
    property Users: TArray<TVkProfile> read FUsers write FUsers;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChat;
  end;

  TVkChats = class
  private
    FItems: TArray<TVkChat>;
    FCount: Integer;
  public
    property Items: TArray<TVkChat> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChats;
  end;

  TVkChatPhoto = class
  private
    FPhoto_100: string;
    FPhoto_200: string;
    FPhoto_50: string;
  public
    property Photo100: string read FPhoto_100 write FPhoto_100;
    property Photo200: string read FPhoto_200 write FPhoto_200;
    property Photo50: string read FPhoto_50 write FPhoto_50;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatPhoto;
  end;

  TVkChatInfoMessage = class
  private
    FChat: TVkChat;
    FMessage_id: Extended;
  public
    property Chat: TVkChat read FChat write FChat;
    property MessageId: Extended read FMessage_id write FMessage_id;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatInfoMessage;
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatPreviewInfo;
  end;

  TVkChatPreview = class
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
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkChatPreview;
  end;

implementation

uses
  VK.CommonUtils;

{TVkChat}

function TVkChat.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

destructor TVkChat.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FUsers);
  inherited;
end;

class function TVkChat.FromJsonString(AJsonString: string): TVkChat;
begin
  result := TJson.JsonToObject<TVkChat>(AJsonString)
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

function TVkChatInfoMessage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkChatInfoMessage.FromJsonString(AJsonString: string): TVkChatInfoMessage;
begin
  result := TJson.JsonToObject<TVkChatInfoMessage>(AJsonString)
end;

{ TVkChats }

destructor TVkChats.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkChat>(FItems);
  inherited;
end;

class function TVkChats.FromJsonString(AJsonString: string): TVkChats;
begin
  result := TJson.JsonToObject<TVkChats>(AJsonString)
end;

function TVkChats.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkChatPreviewInfo }

class function TVkChatPreviewInfo.FromJsonString(AJsonString: string): TVkChatPreviewInfo;
begin
  result := TJson.JsonToObject<TVkChatPreviewInfo>(AJsonString)
end;

function TVkChatPreviewInfo.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkChatPhoto }

class function TVkChatPhoto.FromJsonString(AJsonString: string): TVkChatPhoto;
begin
  result := TJson.JsonToObject<TVkChatPhoto>(AJsonString)
end;

function TVkChatPhoto.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkChatPreview }

destructor TVkChatPreview.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkEmail>(FEmails);
  inherited;
end;

class function TVkChatPreview.FromJsonString(AJsonString: string): TVkChatPreview;
begin
  result := TJson.JsonToObject<TVkChatPreview>(AJsonString)
end;

function TVkChatPreview.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

