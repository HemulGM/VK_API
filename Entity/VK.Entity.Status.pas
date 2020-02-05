unit VK.Entity.Status;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Audio;

type
  TVkStatus = class
  private
    FAudio: TVkAudio;
    FText: string;
  public
    property Audio: TVkAudio read FAudio write FAudio;
    property Text: string read FText write FText;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkStatus;
  end;

implementation


{TVkStatus}

constructor TVkStatus.Create;
begin
  inherited;
end;

destructor TVkStatus.Destroy;
begin
  if Assigned(FAudio) then
    FAudio.Free;
  inherited;
end;

function TVkStatus.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkStatus.FromJsonString(AJsonString: string): TVkStatus;
begin
  result := TJson.JsonToObject<TVkStatus>(AJsonString)
end;

end.

