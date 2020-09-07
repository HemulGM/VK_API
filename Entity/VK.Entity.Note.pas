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
    FPrivacy: Integer;
    FComment_privacy: Integer;
    FCan_comment: Integer;
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
    property Privacy: Integer read FPrivacy write FPrivacy;
    property CommentPrivacy: Integer read FComment_privacy write FComment_privacy;
    property CanComment: Integer read FCan_comment write FCan_comment;
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

  TVkNoteComment = class
  private
    FDate: Int64;
    FId: Integer;
    FMessage: string;
    FNid: Integer;
    FOid: Integer;
    FReply_to: Integer;
    FUid: Integer;
  public
    property Date: Int64 read FDate write FDate;
    property Id: Integer read FId write FId;
    property Message: string read FMessage write FMessage;
    property NoteId: Integer read FNid write FNid;
    property OwnerId: Integer read FOid write FOid;
    property ReplyTo: Integer read FReply_to write FReply_to;
    property UserId: Integer read FUid write FUid;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNoteComment;
  end;

  TVkNoteComments = class
  private
    FCount: Integer;
    FItems: TArray<TVkNoteComment>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkNoteComment> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNoteComments;
  end;

implementation

uses
  System.DateUtils, VK.CommonUtils;

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
begin
  TArrayHelp.FreeArrayOfObject<TVkNote>(FItems);
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

{ TVkNoteComment }

class function TVkNoteComment.FromJsonString(AJsonString: string): TVkNoteComment;
begin
  result := TJson.JsonToObject<TVkNoteComment>(AJsonString)
end;

function TVkNoteComment.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkNoteComments }

destructor TVkNoteComments.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkNoteComment>(FItems);
  inherited;
end;

class function TVkNoteComments.FromJsonString(AJsonString: string): TVkNoteComments;
begin
  result := TJson.JsonToObject<TVkNoteComments>(AJsonString)
end;

function TVkNoteComments.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

