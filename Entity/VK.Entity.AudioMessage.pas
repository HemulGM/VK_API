unit VK.Entity.AudioMessage;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkAudioMessage = class
  private
    FAccess_key: string;
    FDuration: Extended;
    FId: Integer;
    FLink_mp3: string;
    FLink_ogg: string;
    FOwner_id: Integer;
    FWaveform: TArray<Extended>;
  public
    property access_key: string read FAccess_key write FAccess_key;
    property duration: Extended read FDuration write FDuration;
    property id: Integer read FId write FId;
    property link_mp3: string read FLink_mp3 write FLink_mp3;
    property link_ogg: string read FLink_ogg write FLink_ogg;
    property owner_id: Integer read FOwner_id write FOwner_id;
    property waveform: TArray<Extended> read FWaveform write FWaveform;
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
  Result := CreateAttachment('doc', owner_id, FId, FAccess_key);
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

