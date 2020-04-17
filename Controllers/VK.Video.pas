unit VK.Video;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Status, VK.Entity.Video.Save;

type
  TVideoController = class(TVkController)
  public
    /// <summary>
    /// ������������� ����� ������ �������� ������������ ��� ����������.
    /// </summary>
    /// <param name="Text">����� ������ �������</param>
    /// <param name="GroupId">������������� ����������, � ������� ����� ���������� ������. �� ��������� ������ ��������������� �������� ������������</param>
    function Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean; overload;
  end;

implementation

uses
  VK.API;

{ TVideoController }

function TVideoController.Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean;
var
  Params: TParams;
  SaveResp: string;
begin
  Params.Add('link', Link);
  Result := False;
  with Handler.Execute('video.save', Params) do
  begin
    if Success then
    begin
      try
        VideoSaved := TVkVideoSaved.FromJsonString(Response);
        Result := True;
      except
        Result := False;
      end;
    end;
  end;
  if Result then
  begin
    Result := False;
    if TCustomVK(VK).Uploader.Upload(VideoSaved.UploadUrl, '', SaveResp) then
    begin
      Result := not SaveResp.IsEmpty;
    end
    else
      TCustomVK(VK).DoError(Self, TVkException.Create(SaveResp), -1, SaveResp);
  end;
end;

end.

