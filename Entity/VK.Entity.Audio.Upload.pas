unit VK.Entity.Audio.Upload;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkAudioUploadResponse = class
  private
    FAudio: string;
    FHash: string;
    FRedirect: string;
    FServer: integer;
  public
    property Audio: string read FAudio write FAudio;
    property Hash: string read FHash write FHash;
    property Redirect: string read FRedirect write FRedirect;
    property Server: integer read FServer write FServer;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioUploadResponse;
  end;

implementation

{TVkAudioUploadResponse}

function TVkAudioUploadResponse.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioUploadResponse.FromJsonString(AJsonString: string): TVkAudioUploadResponse;
begin
  result := TJson.JsonToObject<TVkAudioUploadResponse>(AJsonString)
end;

end.

