unit VK.Entity.Board;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Profile, VK.Entity.Common, VK.Entity.Common.List;

type
  TVkBoardTopic = class(TVkObject)
  private
    FComments: Integer;
    FCreated: Int64;
    FCreated_by: Integer;
    FIs_closed: Integer;
    FIs_fixed: Integer;
    FTitle: string;
    FUpdated: Int64;
    FUpdated_by: Integer;
    function GetCreated: TDateTime;
    procedure SetCreated(const Value: TDateTime);
  public
    property Comments: Integer read FComments write FComments;
    property Created: TDateTime read GetCreated write SetCreated;
    property CreatedBy: Integer read FCreated_by write FCreated_by;
    property IsClosed: Integer read FIs_closed write FIs_closed;
    property IsFixed: Integer read FIs_fixed write FIs_fixed;
    property Title: string read FTitle write FTitle;
    property Updated: Int64 read FUpdated write FUpdated;
    property UpdatedBy: Integer read FUpdated_by write FUpdated_by;
  end;

  TVkBoardTopics = class(TVkEntityList<TVkBoardTopic>)
  private
    FDefault_order: Integer;
    FCan_add_topics: Integer;
    FProfiles: TArray<TVkProfile>;
  public
    property DefaultOrder: Integer read FDefault_order write FDefault_order;
    property CanAddTopics: Integer read FCan_add_topics write FCan_add_topics;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    destructor Destroy; override;
  end;

implementation

uses
  System.DateUtils, VK.CommonUtils;

{TVkBoardTopic}

function TVkBoardTopic.GetCreated: TDateTime;
begin
  Result := UnixToDateTime(FCreated, False);
end;

procedure TVkBoardTopic.SetCreated(const Value: TDateTime);
begin
  FCreated := DateTimeToUnix(Value, False);
end;

{TVkBoardTopics}

destructor TVkBoardTopics.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

end.

