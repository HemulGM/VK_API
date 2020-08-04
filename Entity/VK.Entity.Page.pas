unit VK.Entity.Page;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkPage = class
  private
    FCreated: Int64;
    FEdited: Int64;
    FGroup_id: Integer;
    FId: Integer;
    FParent2: string;
    FTitle: string;
    FView_url: string;
    FViews: integer;
    FWho_can_edit: Integer;
    FWho_can_view: Integer;
    FCreator_id: Integer;
    FCurrent_user_can_edit: Integer;
    FCurrent_user_can_edit_access: Integer;
    FEditor_id: Integer;
    FParent: string;
    FSource: string;
    FHtml: string;
  public
    property Id: Integer read FId write FId;
    property GroupId: Integer read FGroup_id write FGroup_id;
    property CreatorId: Integer read FCreator_id write FCreator_id;
    property Title: string read FTitle write FTitle;
    property CurrentUserCanEdit: Integer read FCurrent_user_can_edit write FCurrent_user_can_edit;
    property CurrentUserCanEditAccess: Integer read FCurrent_user_can_edit_access write FCurrent_user_can_edit_access;
    property WhoCanView: Integer read FWho_can_view write FWho_can_view;
    property WhoCanEdit: Integer read FWho_can_edit write FWho_can_edit;
    property Edited: Int64 read FEdited write FEdited;
    property Created: Int64 read FCreated write FCreated;
    property EditorId: Integer read FEditor_id write FEditor_id;
    property Views: Integer read FViews write FViews;
    property Parent: string read FParent write FParent;
    property Parent2: string read FParent2 write FParent2;
    property Source: string read FSource write FSource;
    property Html: string read FHtml write FHtml;
    property ViewUrl: string read FView_url write FView_url;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPage;
  end;

  TVkPages = class
  private
    FItems: TArray<TVkPage>;
    FCount: Integer;
  public
    property Items: TArray<TVkPage> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPages;
  end;

  TVkPageVersion = class
  private
    FDate: Int64;
    FEditor_id: Integer;
    FEditor_name: string;
    FId: Integer;
    FLength: Integer;
  public
    property Date: Int64 read FDate write FDate;
    property EditorId: Integer read FEditor_id write FEditor_id;
    property EditorName: string read FEditor_name write FEditor_name;
    property Id: Integer read FId write FId;
    property Length: Integer read FLength write FLength;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPageVersion;
  end;

  TVkPageVersions = class
  private
    FItems: TArray<TVkPageVersion>;
    FCount: Integer;
  public
    property Items: TArray<TVkPageVersion> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPageVersions;
  end;

implementation

{TVkPage}

function TVkPage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPage.FromJsonString(AJsonString: string): TVkPage;
begin
  result := TJson.JsonToObject<TVkPage>(AJsonString)
end;

{ TVkPageVersion }

class function TVkPageVersion.FromJsonString(AJsonString: string): TVkPageVersion;
begin
  result := TJson.JsonToObject<TVkPageVersion>(AJsonString)
end;

function TVkPageVersion.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkPageVersions }

destructor TVkPageVersions.Destroy;
var
  LItemsItem: TVkPageVersion;
begin
  for LItemsItem in FItems do
    LItemsItem.Free;
  inherited;
end;

class function TVkPageVersions.FromJsonString(AJsonString: string): TVkPageVersions;
begin
  result := TJson.JsonToObject<TVkPageVersions>(AJsonString)
end;

function TVkPageVersions.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkPages }

destructor TVkPages.Destroy;
var
  LItemsItem: TVkPage;
begin
  for LItemsItem in FItems do
    LItemsItem.Free;
  inherited;
end;

class function TVkPages.FromJsonString(AJsonString: string): TVkPages;
begin
  result := TJson.JsonToObject<TVkPages>(AJsonString)
end;

function TVkPages.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

