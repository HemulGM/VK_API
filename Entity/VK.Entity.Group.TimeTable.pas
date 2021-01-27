unit VK.Entity.Group.TimeTable;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkTimeTableDay = class
  private
    FBreak_close_time: Integer;
    FBreak_open_time: Integer;
    FClose_time: Integer;
    FOpen_time: Integer;
  public
    property BreakCloseTime: Integer read FBreak_close_time write FBreak_close_time;
    property BreakOpenTime: Integer read FBreak_open_time write FBreak_open_time;
    property CloseTime: Integer read FClose_time write FClose_time;
    property OpenTime: Integer read FOpen_time write FOpen_time;
  end;

  TVkTimeTable = class(TVkEntity)
  private
    FFri: TVkTimeTableDay;
    FMon: TVkTimeTableDay;
    FSat: TVkTimeTableDay;
    FThu: TVkTimeTableDay;
    FTue: TVkTimeTableDay;
    FWed: TVkTimeTableDay;
    FSun: TVkTimeTableDay;
  public
    property Friday: TVkTimeTableDay read FFri write FFri;
    property Monday: TVkTimeTableDay read FMon write FMon;
    property Saturday: TVkTimeTableDay read FSat write FSat;
    property Thursday: TVkTimeTableDay read FThu write FThu;
    property Tuesday: TVkTimeTableDay read FTue write FTue;
    property Wednesday: TVkTimeTableDay read FWed write FWed;
    property Sunday: TVkTimeTableDay read FSun write FSun;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{TVkTimeTable}

constructor TVkTimeTable.Create;
begin
  inherited;
  FMon := TVkTimeTableDay.Create();
  FTue := TVkTimeTableDay.Create();
  FWed := TVkTimeTableDay.Create();
  FThu := TVkTimeTableDay.Create();
  FFri := TVkTimeTableDay.Create();
  FSat := TVkTimeTableDay.Create();
  FSun := TVkTimeTableDay.Create();
end;

destructor TVkTimeTable.Destroy;
begin
  FMon.Free;
  FTue.Free;
  FWed.Free;
  FThu.Free;
  FFri.Free;
  FSat.Free;
  FSun.Free;
  inherited;
end;

end.

