unit VK.Entity.Note;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkNote = class
  private
    FComments: Integer;
    FDate: Int64;
    FId: Integer;
    FOwner_id: Integer;
    FRead_comments: Integer;
    FTitle: string;
    FView_url: string;
    FText: string;
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
  public
    property Id: Integer read FId write FId;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Comments: Integer read FComments write FComments;
    property Date: TDateTime read GetDate write SetDate;
    property ReadComments: Integer read FRead_comments write FRead_comments;
    property Title: string read FTitle write FTitle;
    property Text: string read FText write FText;
    property ViewUrl: string read FView_url write FView_url;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNote;
  end;

  TVkNotes = class
  private
    FCount: Integer;
    FItems: TArray<TVkNote>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkNote> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNotes;
  end;

implementation

uses
  System.DateUtils;

{TVkNote}

function TVkNote.GetDate: TDateTime;
begin
  Result := UnixToDateTime(FDate, False);
end;

procedure TVkNote.SetDate(const Value: TDateTime);
begin
  FDate := DateTimeToUnix(Value, False);
end;

function TVkNote.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNote.FromJsonString(AJsonString: string): TVkNote;
begin
  result := TJson.JsonToObject<TVkNote>(AJsonString)
end;

{TVkNotes}

destructor TVkNotes.Destroy;
var
  LitemsItem: TVkNote;
begin

  for LitemsItem in FItems do
    LitemsItem.Free;

  inherited;
end;

function TVkNotes.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNotes.FromJsonString(AJsonString: string): TVkNotes;
begin
  result := TJson.JsonToObject<TVkNotes>(AJsonString)
end;

end.

