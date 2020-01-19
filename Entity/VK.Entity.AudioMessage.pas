unit VK.Entity.AudioMessage;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkAudioMessage = class
  private
    FAccess_key: string;
    FDuration: Extended;
    FId: Extended;
    FLink_mp3: string;
    FLink_ogg: string;
    FOwner_id: Extended;
    FWaveform: TArray<Extended>;
  public
    property access_key: string read FAccess_key write FAccess_key;
    property duration: Extended read FDuration write FDuration;
    property id: Extended read FId write FId;
    property link_mp3: string read FLink_mp3 write FLink_mp3;
    property link_ogg: string read FLink_ogg write FLink_ogg;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property waveform: TArray<Extended> read FWaveform write FWaveform;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkAudioMessage;
  end;

implementation

{TVkAudioMessage}

function TVkAudioMessage.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkAudioMessage.FromJsonString(AJsonString: string): TVkAudioMessage;
begin
  result := TJson.JsonToObject<TVkAudioMessage>(AJsonString)
end;

end.

