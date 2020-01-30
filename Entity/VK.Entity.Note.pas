unit VK.Entity.Note;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkNote = class
  private
    FComments: Extended;
    FDate: Extended;
    FId: Extended;
    FOwner_id: Extended;
    FRead_comments: Extended;
    FTitle: string;
    FView_url: string;
    FText: string;
  public
    property Comments: Extended read FComments write FComments;
    property Date: Extended read FDate write FDate;
    property Id: Extended read FId write FId;
    property OwnerId: Extended read FOwner_id write FOwner_id;
    property ReadComments: Extended read FRead_comments write FRead_comments;
    property Title: string read FTitle write FTitle;
    property Text: string read FText write FText;
    property ViewUrl: string read FView_url write FView_url;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkNote;
  end;

implementation

{TVkNote}

function TVkNote.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkNote.FromJsonString(AJsonString: string): TVkNote;
begin
  result := TJson.JsonToObject<TVkNote>(AJsonString)
end;

end.

