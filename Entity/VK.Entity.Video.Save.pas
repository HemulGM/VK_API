unit VK.Entity.Video.Save;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkVideoSaved = class
  private
    FAccess_key: string;
    FDescription: string;
    FOwner_id: Integer;
    FTitle: string;
    FUpload_url: string;
    FVideo_id: Integer;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Description: string read FDescription write FDescription;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Title: string read FTitle write FTitle;
    property UploadUrl: string read FUpload_url write FUpload_url;
    property VideoId: Integer read FVideo_id write FVideo_id;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkVideoSaved;
  end;

implementation

{TVkVideoSaved}

function TVkVideoSaved.ToJsonString: string;
begin
  Result := TJson.ObjectToJsonString(self);
end;

class function TVkVideoSaved.FromJsonString(AJsonString: string): TVkVideoSaved;
begin
  Result := TJson.JsonToObject<TVkVideoSaved>(AJsonString)
end;

end.

