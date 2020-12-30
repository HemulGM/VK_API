unit VK.Entity.AudioMessage;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkAudioMessage = class(TVkObject)
  private
    FAccess_key: string;
    FDuration: Int64;
    FLink_mp3: string;
    FLink_ogg: string;
    FOwner_id: Integer;
    FWaveform: TArray<Integer>;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Duration: Int64 read FDuration write FDuration;
    property LinkMp3: string read FLink_mp3 write FLink_mp3;
    property LinkOgg: string read FLink_ogg write FLink_ogg;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property WaveForm: TArray<Integer> read FWaveform write FWaveform;
    function ToJsonString: string;
    function ToAttachment: string;
    class function FromJsonString(AJsonString: string): TVkAudioMessage;
  end;

implementation

uses
  VK.Types, System.SysUtils;

{TVkAudioMessage}

function TVkAudioMessage.ToAttachment: string;
begin
  Result := Attachment.Doc(FId, OwnerId, FAccess_key);
end;

function TVkAudioMessage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioMessage.FromJsonString(AJsonString: string): TVkAudioMessage;
begin
  result := TJson.JsonToObject<TVkAudioMessage>(AJsonString)
end;

end.

