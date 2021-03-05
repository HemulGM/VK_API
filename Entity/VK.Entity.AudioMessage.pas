unit VK.Entity.AudioMessage;

interface

uses
  Generics.Collections, Rest.Json, VK.Types, VK.Entity.Common;

type
  TVkAudioMessage = class(TVkObject, IAttachment)
  private
    FAccess_key: string;
    FDuration: Int64;
    FLink_mp3: string;
    FLink_ogg: string;
    FOwner_id: Integer;
    FWaveform: TArray<Integer>;
    FTranscript: string;
    FTranscript_state: string;
  public
    property Id;
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Длительность аудиосообщения в секундах
    /// </summary>
    property Duration: Int64 read FDuration write FDuration;
    /// <summary>
    /// URL .mp3-файла
    /// </summary>
    property LinkMp3: string read FLink_mp3 write FLink_mp3;
    /// <summary>
    /// URL .ogg-файла
    /// </summary>
    property LinkOgg: string read FLink_ogg write FLink_ogg;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    /// <summary>
    /// Массив значений (integer) для визуального отображения звука
    /// </summary>
    property WaveForm: TArray<Integer> read FWaveform write FWaveform;
    property Transcript: string read FTranscript write FTranscript;
    property TranscriptState: string read FTranscript_state write FTranscript_state;
    function ToAttachment: TAttachment;
  end;

implementation

uses
  System.SysUtils;

{TVkAudioMessage}

function TVkAudioMessage.ToAttachment: TAttachment;
begin
  Result := TAttachment.Doc(OwnerId, Id, FAccess_key);
end;

end.

