unit VK.Entity.Note;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkNote = class(TVkObject)
  private
    FComments: Integer;
    FDate: Int64;
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
  end;

  TVkNotes = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkNote>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkNote> read FItems write FItems;
    destructor Destroy; override;
  end;

  TVkNoteComment = class(TVkObject)
  private
    FDate: Int64;
    FMessage: string;
    FNid: Integer;
    FOid: Integer;
    FReply_to: Integer;
    FUid: Integer;
  public
    property Date: Int64 read FDate write FDate;
    property Message: string read FMessage write FMessage;
    property NoteId: Integer read FNid write FNid;
    property OwnerId: Integer read FOid write FOid;
    property ReplyTo: Integer read FReply_to write FReply_to;
    property UserId: Integer read FUid write FUid;
  end;

  TVkNoteComments = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkNoteComment>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkNoteComment> read FItems write FItems;
    destructor Destroy; override;
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

{TVkNotes}

destructor TVkNotes.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkNote>(FItems);
  inherited;
end;

{ TVkNoteComments }

destructor TVkNoteComments.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkNoteComment>(FItems);
  inherited;
end;

end.

