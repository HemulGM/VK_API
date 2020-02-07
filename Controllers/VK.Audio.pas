unit VK.Audio;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types,
  VK.Entity.Audio, System.JSON, REST.Json, VK.Entity.Playlist, VK.Entity.Audio.Upload;

type
  TVkAudioParams = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function AudioIds(Value: TIds): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkPlaylistParams = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TAudioController = class(TVkController)
  public
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="OwnerId: Integer">Идентификатор пользователя или сообщества</param>
    function Get(var Audios: TVkAudios; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Params: TVkAudioParams">Параметры запроса</param>
    function Get(var Audios: TVkAudios; Params: TVkAudioParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Params: TVkAudioParams">Параметры запроса</param>
    function GetRecommendations(var Audios: TVkAudios; Params: TVkAudioParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="OwnerId: Integer">Идентификатор пользователя или сообщества</param>
    function GetRecommendations(var Audios: TVkAudios; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Params: TVkAudioParams">Параметры запроса</param>
    function GetPlaylists(var Items: TVkPlaylists; Params: TVkPlaylistParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Params: TVkAudioParams">Параметры запроса</param>
    function GetPlaylists(var Items: TVkPlaylists; OwnerID: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="List: TVkAudioIndexes">Список идентификаторов аудиозаписей</param>
    function GetById(var Audios: TVkAudios; List: TVkAudioIndexes): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписи
    /// </summary>
    /// <param name="var Audio: TVkAudio">Аудиозапись</param>
    /// <param name="Item: TVkAudioIndexe">Идентфикатор аудиозаписи</param>
    function GetById(var Audio: TVkAudio; OwnerId, AudioId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки аудиозаписей
    /// </summary>
    function GetUploadServer(var UploadUrl: string): Boolean;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии с заданным критерием поиска
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Query: string">Поисковый запрос</param>
    function Search(var Audios: TVkAudios; Query: string): Boolean; overload;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии с заданным критерием поиска
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Query: string">Поисковый запрос</param>
    function Search(var Audios: TVkAudios; Query: string; AutoComplete, PerformerOnly: Boolean): Boolean; overload;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии с заданным критерием поиска
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Query: string">Поисковый запрос</param>
    function Search(var Audios: TVkAudios; Query: string; Params: TParams): Boolean; overload;
    function Save(AudioSaveData: TVkAudioUploadResponse; var Audio: TVkAudio): Boolean;
  end;

implementation

uses
  VK.API;

{ TAudioController }

function TAudioController.Save(AudioSaveData: TVkAudioUploadResponse; var Audio: TVkAudio): Boolean;
var
  Params: TParams;
begin
  Params.Add('audio', AudioSaveData.Audio);
  Params.Add('server', AudioSaveData.Server);
  Params.Add('hash', AudioSaveData.Hash);
  with Handler.Execute('audio.save', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Audio := TVkAudio.FromJsonString(Response);
    end;
  end;
end;

function TAudioController.Search(var Audios: TVkAudios; Query: string; Params: TParams): Boolean;
begin
  Params.Add('q', Query);
  with Handler.Execute('audio.search', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Audios := TVkAudios.FromJsonString(Response);
    end
  end;
end;

function TAudioController.Search(var Audios: TVkAudios; Query: string; AutoComplete, PerformerOnly: Boolean): Boolean;
var
  Params: TParams;
begin
  if AutoComplete then
    Params.Add('auto_complete', AutoComplete);
  if PerformerOnly then
    Params.Add('performer_only', PerformerOnly);
  Result := Search(Audios, Query, Params);
end;

function TAudioController.Get(var Audios: TVkAudios; OwnerId: Integer): Boolean;
var
  Params: TVkAudioParams;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Result := Get(Audios, Params);
end;

function TAudioController.Get(var Audios: TVkAudios; Params: TVkAudioParams): Boolean;
begin
  with Handler.Execute('audio.get', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Audios := TVkAudios.FromJsonString(Response);
    end;
  end;
end;

function TAudioController.GetPlaylists(var Items: TVkPlaylists; OwnerID: Integer): Boolean;
var
  Params: TVkPlaylistParams;
begin
  Params.OwnerId(OwnerID);
  Params.Count(100);
  Result := GetPlaylists(Items, Params);
end;

function TAudioController.GetRecommendations(var Audios: TVkAudios; OwnerId: Integer): Boolean;
var
  Params: TVkAudioParams;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Result := GetRecommendations(Audios, Params);
end;

function TAudioController.GetRecommendations(var Audios: TVkAudios; Params: TVkAudioParams): Boolean;
begin
  with Handler.Execute('audio.getRecommendations', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Audios := TVkAudios.FromJsonString(Response);
    end;
  end;
end;

function TAudioController.GetPlaylists(var Items: TVkPlaylists; Params: TVkPlaylistParams): Boolean;
begin
  with Handler.Execute('audio.getPlaylists', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Items := TVkPlaylists.FromJsonString(Response);
    end
  end;
end;

function TAudioController.GetById(var Audio: TVkAudio; OwnerId, AudioId: Integer): Boolean;
var
  JArray: TJSONArray;
  i: Integer;
begin
  with Handler.Execute('audio.getById', [['count', '1'], ['audios', OwnerId.ToString + '_' + AudioId.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      JArray := TJSONArray(TJSONObject.ParseJSONValue(Response));
      try
        for i := 0 to JArray.Count - 1 do
          Audio := TVkAudio.FromJsonString(JArray.Items[i].ToString);
      finally
        JArray.Free;
      end;
    end;
  end;
end;

function TAudioController.GetById(var Audios: TVkAudios; List: TVkAudioIndexes): Boolean;
var
  ListStr: string;
  i: Integer;
begin
  for i := Low(List) to High(List) do
  begin
    if i <> Low(List) then
      ListStr := ListStr + ',';
    ListStr := ListStr + List[i].OwnerId.ToString + '_' + List[i].AudioId.ToString;
  end;
  with Handler.Execute('audio.getById', ['audios', ListStr]) do
  begin
    Result := Success;
    if Result then
    begin
      Audios := TVkAudios.FromJsonString(Response);
    end;
  end;
end;

function TAudioController.GetUploadServer(var UploadUrl: string): Boolean;
var
  JSONItem: TJSONValue;
begin
  with Handler.Execute('audio.getUploadServer') do
  begin
    Result := Success;
    if Result then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      try
        UploadUrl := JSONItem.GetValue<string>('upload_url', '');
      finally
        JSONItem.Free;
      end;
      Result := not UploadUrl.IsEmpty;
    end;
  end;
end;

function TAudioController.Search(var Audios: TVkAudios; Query: string): Boolean;
begin
  Result := Search(Audios, Query, True, False);
end;

{ TVkAudioParams }

function TVkAudioParams.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value.ToString);
end;

function TVkAudioParams.AudioIds(Value: TIds): Integer;
begin
  Result := List.Add('audio_ids', Value.ToString);
end;

function TVkAudioParams.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value.ToString);
end;

function TVkAudioParams.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value.ToString);
end;

function TVkAudioParams.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value.ToString);
end;

{ TVkPlaylistParams }

function TVkPlaylistParams.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value.ToString);
end;

function TVkPlaylistParams.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value.ToString);
end;

end.

