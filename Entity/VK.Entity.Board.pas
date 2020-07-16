unit VK.Entity.Board;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.User;

type
  TVkBoardTopic = class
  private
    FComments: Integer;
    FCreated: Int64;
    FCreated_by: Integer;
    FId: Integer;
    FIs_closed: Integer;
    FIs_fixed: Integer;
    FTitle: string;
    FUpdated: Int64;
    FUpdated_by: Integer;
  public
    property Comments: Integer read FComments write FComments;
    property Created: Int64 read FCreated write FCreated;
    property CreatedBy: Integer read FCreated_by write FCreated_by;
    property Id: Integer read FId write FId;
    property IsClosed: Integer read FIs_closed write FIs_closed;
    property IsFixed: Integer read FIs_fixed write FIs_fixed;
    property Title: string read FTitle write FTitle;
    property Updated: Int64 read FUpdated write FUpdated;
    property UpdatedBy: Integer read FUpdated_by write FUpdated_by;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkBoardTopic;
  end;

  TVkBoardTopics = class
  private
    FCount: Integer;
    FItems: TArray<TVkBoardTopic>;
    FDefault_order: Integer;
    FCan_add_topics: Integer;
    FProfiles: TArray<TVkUser>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkBoardTopic> read FItems write FItems;
    property DefaultOrder: Integer read FDefault_order write FDefault_order;
    property CanAddTopics: Integer read FCan_add_topics write FCan_add_topics;
    property Profiles: TArray<TVkUser> read FProfiles write FProfiles;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkBoardTopics;
  end;

implementation

{TVkBoardTopic}

function TVkBoardTopic.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkBoardTopic.FromJsonString(AJsonString: string): TVkBoardTopic;
begin
  result := TJson.JsonToObject<TVkBoardTopic>(AJsonString)
end;

{TVkBoardTopics}

destructor TVkBoardTopics.Destroy;
var
  LitemsItem: TVkBoardTopic;
  User: TVkUser;
begin

  for User in FProfiles do
    User.Free;

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkBoardTopics.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkBoardTopics.FromJsonString(AJsonString: string): TVkBoardTopics;
begin
  result := TJson.JsonToObject<TVkBoardTopics>(AJsonString)
end;

end.

