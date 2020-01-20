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
    property comments: Extended read FComments write FComments;
    property date: Extended read FDate write FDate;
    property id: Extended read FId write FId;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property read_comments: Extended read FRead_comments write FRead_comments;
    property title: string read FTitle write FTitle;
    property text: string read FText write FText;
    property view_url: string read FView_url write FView_url;
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

