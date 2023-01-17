unit VK.Entity.Streaming;

interface

uses
  VK.Entity.Common, VK.Entity.Common.List, System.Generics.Collections,
  REST.Json.Types;

type
  TVkStreamServer = class(TVkEntity)
  private
    FEndpoint: string;
    FKey: string;
  public
    property Endpoint: string read FEndpoint write FEndpoint;
    property Key: string read FKey write FKey;
  end;

  TVkStreamLimit = class(TVkEntity)
  private
    FMonthly_Limit: string;
  public
    property MonthlyLimit: string read FMonthly_Limit write FMonthly_Limit;
  end;

  TVkStreamStatItem = class
  private
    FTimestamp: Integer;
    FValue: Integer;
  public
    property Timestamp: Integer read FTimestamp write FTimestamp;
    property Value: Integer read FValue write FValue;
  end;

  TVkStreamStat = class(TVkEntity)
  private
    FEvent_Type: string;
    FStats: TArray<TVkStreamStatItem>;
  public
    property EventType: string read FEvent_Type write FEvent_Type;
    property Stats: TArray<TVkStreamStatItem> read FStats write FStats;
    destructor Destroy; override;
  end;

  TVkStreamStats = TVkEntityList<TVkStreamStat>;

implementation

uses
  VK.CommonUtils;

{ TVkStreamStat }

destructor TVkStreamStat.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkStreamStatItem>(FStats);
  inherited;
end;

end.

