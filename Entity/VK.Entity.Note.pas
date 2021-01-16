unit VK.Entity.Note;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Common.List;

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
    FCan_comment: Boolean;
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
    property CanComment: Boolean read FCan_comment write FCan_comment;
  end;

  TVkNotes = TVkEntityList<TVkNote>;

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

  TVkNoteComments = TVkEntityList<TVkNoteComment>;

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

end.

