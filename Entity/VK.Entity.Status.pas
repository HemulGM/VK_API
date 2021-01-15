unit VK.Entity.Status;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Audio, VK.Entity.Common;

type
  TVkStatus = class(TVkEntity)
  private
    FAudio: TVkAudio;
    FText: string;
  public
    property Audio: TVkAudio read FAudio write FAudio;
    property Text: string read FText write FText;
    destructor Destroy; override;
  end;

implementation

{TVkStatus}

destructor TVkStatus.Destroy;
begin
  if Assigned(FAudio) then
    FAudio.Free;
  inherited;
end;

end.

