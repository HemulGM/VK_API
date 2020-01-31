unit VK.Photos;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types,
  VK.Entity.Audio, System.JSON, REST.Json, VK.Entity.Photo.Upload, VK.Entity.Photo;

type
  TPhotosController = class(TVkController)
  public
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии в личное сообщение
    /// </summary>
    /// <param name="var UploadUrl">искомый адрес сервера</param>
    /// <param name="PeerId">идентификатор назначения</param>
    function GetMessagesUploadServer(var UploadUrl: string; PeerId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки фотографии в личное сообщение
    /// </summary>
    /// <param name="var Upload: TVkPhotoGetUploadResponse">Объект с полями UploadUrl, AlbumId (id альбома), GroupId (идентификатор сообщества, если используются сообщения сообщества).</param>
    /// <param name="PeerId: Integer">идентификатор назначения</param>
    function GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: Integer): Boolean; overload;
    /// <summary>
    /// Сохраняет фотографию после успешной загрузки на URI, полученный методом Photos.GetMessagesUploadServer.
    /// </summary>
    /// <param name="PeerId: Integer">идентификатор назначения</param>
    function SaveMessagesPhoto(PhotoSaveData: TVkPhotoUploadResponse; var Photos: TVkPhotos): Boolean;
  end;

implementation

uses
  VK.API, VK.Utils;

{ TPhotosController }

function TPhotosController.GetMessagesUploadServer(var UploadUrl: string; PeerId: Integer): Boolean;
var
  Upload: TVkPhotoGetUploadResponse;
begin
  Result := GetMessagesUploadServer(Upload, PeerId);
  if Result then
  begin
    UploadUrl := Upload.UploadUrl;
    Upload.Free;
  end;
end;

function TPhotosController.GetMessagesUploadServer(var Upload: TVkPhotoGetUploadResponse; PeerId: Integer): Boolean;
var
  Params: TParams;
begin
  Params.Add('peer_id', PeerId);
  with Handler.Execute('photos.getMessagesUploadServer', Params) do
  begin
    if Success then
    begin
      try
        Upload := TVkPhotoGetUploadResponse.FromJsonString(Response);
        Result := True;
      except
        Result := False;
      end;
    end
    else
      Result := False;
  end;
end;

function TPhotosController.SaveMessagesPhoto(PhotoSaveData: TVkPhotoUploadResponse; var Photos: TVkPhotos): Boolean;
var
  Params: TParams;
  JArr: TJSONArray;
  i: Integer;
begin
  Params.Add('photo', PhotoSaveData.Photo);
  Params.Add('server', PhotoSaveData.Server);
  Params.Add('hash', PhotoSaveData.Hash);
  with Handler.Execute('photos.saveMessagesPhoto', Params) do
  begin
    if Success then
    begin
      JArr := TJSONArray(TJSONObject.ParseJSONValue(Response));
      if Assigned(JArr) then
      begin
        SetLength(Photos, JArr.Count);
        for i := 0 to JArr.Count - 1 do
        begin
          Photos[i] := TVkPhoto.FromJsonString(JArr.Items[i].ToJSON);
        end;
        Result := True;
        JArr.Free;
      end
      else
        Result := False;
    end
    else
      Result := False;
  end;
end;

end.

