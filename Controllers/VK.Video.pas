unit VK.Video;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Video, System.JSON,
  VK.Entity.Status, VK.Entity.Video.Save;

type
  TVkParamsVideosGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Videos(Value: TArrayOfString): Integer;
  end;

  TVkParamsVideoAlbumsGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Extended(Value: Boolean): Integer;
    function NeedSystem(Value: Boolean): Integer;
  end;

  TVideoController = class(TVkController)
  public
    /// <summary>
    /// Устанавливает новый статус текущему пользователю или сообществу.
    /// </summary>
    /// <param name="Text">текст нового статуса</param>
    /// <param name="GroupId">идентификатор сообщества, в котором будет установлен статус. По умолчанию статус устанавливается текущему пользователю</param>
    function Save(var VideoSaved: TVkVideoSaved; Link: string): Boolean; overload;
    function Get(var Videos: TVkVideos; Params: TParams): Boolean; overload;
    function Get(var Videos: TVkVideos; Params: TVkParamsVideosGet): Boolean; overload;
    function GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean; overload;
    function GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoAlbumsGet): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TVideoController }

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TParams): Boolean;
begin
  with Handler.Execute('video.getAlbums', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkVideoAlbums.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TVideoController.GetAlbums(var Items: TVkVideoAlbums; Params: TVkParamsVideoAlbumsGet): Boolean;
begin
  Result := GetAlbums(Items, Params.List);
end;

function TVideoController.Get(var Videos: TVkVideos; Params: TParams): Boolean;
begin
  with Handler.Execute('video.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Videos := TVkVideos.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TVideoController.Get(var Videos: TVkVideos; Params: TVkParamsVideosGet): Boolean;
begin
  Result := Get(Videos, Params.List);
end;

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

{ TVkVideosGetParams }

function TVkParamsVideosGet.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsVideosGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideosGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideosGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideosGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsVideosGet.Videos(Value: TArrayOfString): Integer;
begin
  Result := List.Add('videos', Value.ToString);
end;

{ TVkParamsVideoAlbumsGet }

function TVkParamsVideoAlbumsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsVideoAlbumsGet.Extended(Value: Boolean): Integer;
begin
  Result := List.Add('extended', Value);
end;

function TVkParamsVideoAlbumsGet.NeedSystem(Value: Boolean): Integer;
begin
  Result := List.Add('need_system', Value);
end;

function TVkParamsVideoAlbumsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsVideoAlbumsGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

end.

