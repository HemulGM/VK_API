unit VK.Audio;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types,
  VK.Entity.Audio, System.JSON, REST.Json, VK.Entity.Playlist;

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
    function Get(var Audios: TVkAudios; Params: TVkAudioParams): Boolean; overload;
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
    function GetById(var Audio: TVkAudio; Item: TVkAudioIndex): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписи
    /// </summary>
    /// <param name="var Audio: TVkAudio">Аудиозапись</param>
    /// <param name="Item: TVkAudioIndexe">Не полная аудиозапись</param>
    function GetById(var Audio: TVkAudio; Item: TVkAudio): Boolean; overload;
  end;

implementation

uses
  VK.API;

{ TAudioController }

function TAudioController.Get(var Audios: TVkAudios; OwnerId: Integer): Boolean;
var
  Params: TVkAudioParams;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Result := Get(Audios, Params);
end;

function TAudioController.Get(var Audios: TVkAudios; Params: TVkAudioParams): Boolean;
var
  JSONItem: TJSONValue;
  JArray: TJSONArray;
  i: Integer;
begin
  with Handler.Execute('audio.get', Params.List) do
  begin
    if Success then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      JArray := JSONItem.GetValue<TJSONArray>('items', nil);
      if Assigned(JArray) then
      begin
        SetLength(Audios, JArray.Count);
        for i := 0 to JArray.Count - 1 do
          Audios[i] := TVkAudio.FromJsonString(JArray.Items[i].ToString);
      end;
      JSONItem.Free;
      Result := True;
    end
    else
      Result := False;
  end;
end;

function TAudioController.GetById(var Audio: TVkAudio; Item: TVkAudio): Boolean;
var
  Audios: TVkAudios;
begin
  Result := GetById(Audios, [[Item.OwnerId, Item.Id]]);
  if Length(Audios) > 0 then
    Audio := Audios[0];
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
var
  JSONItem: TJSONValue;
  JArray: TJSONArray;
  i: Integer;
begin
  with Handler.Execute('audio.getRecommendations', Params.List) do
  begin
    if Success then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      JArray := JSONItem.GetValue<TJSONArray>('items', nil);
      if Assigned(JArray) then
      begin
        SetLength(Audios, JArray.Count);
        for i := 0 to JArray.Count - 1 do
          Audios[i] := TVkAudio.FromJsonString(JArray.Items[i].ToString);
      end;
      JSONItem.Free;
      Result := True;
    end
    else
      Result := False;
  end;
end;

function TAudioController.GetPlaylists(var Items: TVkPlaylists; Params: TVkPlaylistParams): Boolean;
var
  JSONItem: TJSONValue;
  JArray: TJSONArray;
  i: Integer;
begin
  with Handler.Execute('audio.getPlaylists', Params.List) do
  begin
    if Success then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      JArray := JSONItem.GetValue<TJSONArray>('items', nil);
      if Assigned(JArray) then
      begin
        SetLength(Items, JArray.Count);
        for i := 0 to JArray.Count - 1 do
          Items[i] := TVkAudioPlaylist.FromJsonString(JArray.Items[i].ToString);
      end;
      JSONItem.Free;
      Result := True;
    end
    else
      Result := False;
  end;
end;

function TAudioController.GetById(var Audio: TVkAudio; Item: TVkAudioIndex): Boolean;
var
  Audios: TVkAudios;
begin
  Result := GetById(Audios, [Item]);
  if Length(Audios) > 0 then
    Audio := Audios[0];
end;

function TAudioController.GetById(var Audios: TVkAudios; List: TVkAudioIndexes): Boolean;
var
  JArray: TJSONArray;
  ListStr: string;
  i: Integer;
begin
  for i := Low(List) to High(List) do
  begin
    if i <> Low(List) then
      ListStr := ListStr + ',';
    ListStr := ListStr + List[i][0].ToString + '_' + List[i][1].ToString;
  end;
  with Handler.Execute('audio.getById', ['audios', ListStr]) do
  begin
    if Success then
    begin
      JArray := TJSONArray(TJSONObject.ParseJSONValue(Response));
      if Assigned(JArray) then
      begin
        SetLength(Audios, JArray.Count);
        for i := 0 to JArray.Count - 1 do
          Audios[i] := TVkAudio.FromJsonString(JArray.Items[i].ToString);
      end;
      JArray.Free;
      Result := True;
    end
    else
      Result := False;
  end;
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

