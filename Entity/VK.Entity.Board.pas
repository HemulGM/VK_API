unit VK.Entity.Board;

interface

uses
  Generics.Collections, VK.Wrap.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Profile, VK.Entity.Common, VK.Entity.Common.List, VK.Types;

type
  TVkBoardTopic = class(TVkObject)
  private
    FComments: Integer;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FCreated: TDateTime;
    FCreated_by: TVkPeerId;
    FIs_closed: Boolean;
    FIs_fixed: Boolean;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FUpdated: TDateTime;
    FUpdated_by: TVkPeerId;
  public
    property Comments: Integer read FComments write FComments;
    property Created: TDateTime read FCreated write FCreated;
    property CreatedBy: TVkPeerId read FCreated_by write FCreated_by;
    property IsClosed: Boolean read FIs_closed write FIs_closed;
    property IsFixed: Boolean read FIs_fixed write FIs_fixed;
    property Title: string read FTitle write FTitle;
    property Updated: TDateTime read FUpdated write FUpdated;
    property UpdatedBy: TVkPeerId read FUpdated_by write FUpdated_by;
  end;

  TVkBoardTopics = class(TVkEntityList<TVkBoardTopic>)
  private
    FDefault_order: Boolean;
    FCan_add_topics: Boolean;
    FProfiles: TArray<TVkProfile>;
  public
    property DefaultOrder: Boolean read FDefault_order write FDefault_order;
    property CanAddTopics: Boolean read FCan_add_topics write FCan_add_topics;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    destructor Destroy; override;
  end;

implementation

uses
  System.DateUtils, VK.CommonUtils;

{TVkBoardTopics}

destructor TVkBoardTopics.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  inherited;
end;

end.

