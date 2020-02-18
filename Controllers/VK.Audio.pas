unit VK.Audio;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types,
  VK.Entity.Audio, System.JSON, REST.Json, VK.Utils, VK.Entity.Playlist, VK.Entity.Audio.Upload;

type
  TVkAudioParams = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function AudioIds(Value: TIds): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkPopAudioParams = record
    List: TParams;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function OnlyEng(Value: Boolean): Integer;
    function GenreId(Value: TAudioGenre): Integer;
  end;

  TVkPlaylistParams = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkAudioSort = (asDateAdd, asDuration, asPopular);

  TVkAudioSearchParams = record
    List: TParams;
    function Query(Value: string): Integer;
    function AutoComplete(Value: Boolean): Integer;
    function PerformerOnly(Value: Boolean): Integer;
    function Lyrics(Value: string): Integer;
    function Sort(Value: TVkAudioSort): Integer;
    function SearchOwn(Value: Boolean): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkAudioEditParams = record
    List: TParams;
    function AudioId(Value: Integer): Integer;
    function OwnerId(Value: Integer): Integer;
    function Artist(Value: string): Integer;
    function Title(Value: string): Integer;
    function Text(Value: string): Integer;
    function GenreId(Value: TAudioGenre): Integer;
    function NoSearch(Value: Boolean): Integer;
  end;

  TAudioController = class(TVkController)
  private
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
    /// Returns a list of audio files from the "Popular".
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="OwnerId: Integer">Идентификатор пользователя или сообщества</param>
    function GetPopular(var Audios: TVkAudios; Params: TVkPopAudioParams): Boolean; overload;
    /// <summary>
    /// Returns a list of audio files from the "Popular".
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="OnlyEng: Boolean">Только иностранные</param>
    /// <param name="GenreId: TAudioGenre">Жанр</param>
    /// <param name="Count: Integer">Количество</param>
    /// <param name="Offset: Integer">Смещение</param>
    function GetPopular(var Audios: TVkAudios; OnlyEng: Boolean = False; GenreId: TAudioGenre =
      agNone; Count: Integer = 0; Offset: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    /// <param name="var Items: TVkPlaylists">Список плейлистов</param>
    /// <param name="Params: TVkAudioParams">Параметры запроса</param>
    function GetPlaylists(var Items: TVkPlaylists; Params: TVkPlaylistParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об плейлистах
    /// </summary>
    /// <param name="var Items: TVkPlaylists">Список плейлистов</param>
    /// <param name="OwnerId: Integer">Идентификатор пользователя или сообщества</param>
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
    /// <param name="OwnerId: Integer">Идентификатор пользователя или сообщества</param>
    /// <param name="AudioId: Integer">Идентификатор аудиозаписи</param>
    function GetById(var Audio: TVkAudio; OwnerId, AudioId: Integer; AccessKey: string = ''): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки аудиозаписей
    /// </summary>
    /// <param name="var UploadUrl: string">Полученный сервер</param>
    function GetUploadServer(var UploadUrl: string): Boolean;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии (нестрогий поиск)
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Query: string">Поисковый запрос</param>
    function Search(var Audios: TVkAudios; Query: string): Boolean; overload;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии с заданным критерием поиска
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Query: string">Поисковый запрос</param>
    /// <param name="PerformerOnly: Boolean">Поиск только по названию</param>
    /// <param name="AutoComplete: Boolean">Нестрогий поиск</param>
    function Search(var Audios: TVkAudios; Query: string; AutoComplete, PerformerOnly: Boolean): Boolean; overload;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии с заданным критерием поиска
    /// </summary>
    /// <param name="var Audios: TVkAudios">Список аудиозаписей</param>
    /// <param name="Params: TParams"></param>
    function Search(var Audios: TVkAudios; Params: TVkAudioSearchParams): Boolean; overload;
    /// <summary>
    /// Сохраняет аудиозаписи после успешной загрузки.
    /// </summary>
    /// <param name="var Audio: TVkAudio">Загруженная аудиозапись</param>
    /// <param name="AudioSaveData: TVkAudioUploadResponse">Данные для сохренения</param>
    function Save(var Audio: TVkAudio; AudioSaveData: TVkAudioUploadResponse): Boolean;
    /// <summary>
    /// Копирует аудиозапись на страницу пользователя или группы.
    /// </summary>
    /// <param name="var Id: Integer">Ид полученной аудиозаписи</param>
    /// <param name="AudioId: Integer">Идентификатор аудиозаписи</param>
    /// <param name="OwnerId: Integer">Идентификатор пользователя или сообщества</param>
    /// <param name="GroupId: Integer">Идентификатор сообщества (если аудиозапись необходимо скопировать в список сообщества)</param>
    /// <param name="AlbumId: Integer">Идентификатор альбома, в который нужно переместить аудиозапись</param>
    function Add(var Id: Integer; AudioId, OwnerId: Integer; GroupId: Integer = 0; AlbumId: Integer = -1): Boolean;
    /// <summary>
    /// Удаляет аудиозапись со страницы пользователя или сообщества.
    /// </summary>
    /// <param name="AudioId: Integer">Идентификатор аудиозаписи</param>
    /// <param name="OwnerId: Integer">Идентификатор пользователя или сообщества</param>
    function Delete(AudioId, OwnerId: Integer): Boolean;
    /// <summary>
    /// Редактирует данные аудиозаписи на странице пользователя или сообщества.
    /// </summary>
    /// <param name="AudioId: Integer">Идентификатор аудиозаписи</param>
    /// <param name="OwnerId: Integer">Идентификатор пользователя или сообщества</param>
    /// <param name="Params: TVkAudioEditParams">Параметры</param>
    function Edit(AudioId, OwnerId: Integer; Params: TVkAudioEditParams): Boolean;
  end;

implementation

uses
  VK.API;

{ TAudioController }

function TAudioController.Save(var Audio: TVkAudio; AudioSaveData: TVkAudioUploadResponse): Boolean;
var
  Params: TParams;
begin
  Params.Add('audio', AudioSaveData.Audio);
  Params.Add('server', AudioSaveData.Server);
  Params.Add('hash', AudioSaveData.Hash);
  if not AudioSaveData.Artist.IsEmpty then
    Params.Add('artist', AudioSaveData.Artist);
  if not AudioSaveData.Title.IsEmpty then
    Params.Add('title', AudioSaveData.Title);
  with Handler.Execute('audio.save', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Audio := TVkAudio.FromJsonString(Response);
    end;
  end;
end;

function TAudioController.Search(var Audios: TVkAudios; Params: TVkAudioSearchParams): Boolean;
begin
  with Handler.Execute('audio.search', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Audios := TVkAudios.FromJsonString(Response);
    end
  end;
end;

function TAudioController.Search(var Audios: TVkAudios; Query: string): Boolean;
var
  Params: TVkAudioSearchParams;
begin
  Params.Query(Query);
  Params.AutoComplete(True);
  Params.PerformerOnly(False);
  Result := Search(Audios, Params);
end;

function TAudioController.Search(var Audios: TVkAudios; Query: string; AutoComplete, PerformerOnly: Boolean): Boolean;
var
  Params: TVkAudioSearchParams;
begin
  Params.AutoComplete(AutoComplete);
  Params.PerformerOnly(PerformerOnly);
  Params.Query(Query);
  Result := Search(Audios, Params);
end;

function TAudioController.Get(var Audios: TVkAudios; OwnerId: Integer): Boolean;
var
  Params: TVkAudioParams;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Result := Get(Audios, Params);
end;

function TAudioController.Add(var Id: Integer; AudioId, OwnerId, GroupId, AlbumId: Integer): Boolean;
var
  Params: TParams;
begin
  Id := -1;
  Params.Add('audio_id', AudioId);
  Params.Add('owner_id', OwnerId);
  if GroupId <> 0 then
    Params.Add('group_id', Abs(GroupId));
  if AlbumId > -1 then
    Params.Add('album_id', AlbumId);
  with Handler.Execute('audio.add', Params) do
  begin
    Result := Success;
    if Result then
    begin
      Result := TryStrToInt(Response, Id);
    end;
  end;
end;

function TAudioController.Delete(AudioId, OwnerId: Integer): Boolean;
begin
  with Handler.Execute('audio.delete', [['audio_id', AudioId.ToString], ['owner_id', OwnerId.ToString]]) do
  begin
    Result := Success;
    if Result then
    begin
      Result := Response = '1';
    end;
  end;
end;

function TAudioController.Edit(AudioId, OwnerId: Integer; Params: TVkAudioEditParams): Boolean;
begin
  Params.AudioId(AudioId);
  Params.OwnerId(OwnerId);
  with Handler.Execute('audio.edit', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Result := Response <> '0';
    end;
  end;
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

function TAudioController.GetPopular(var Audios: TVkAudios; Params: TVkPopAudioParams): Boolean;
begin
  with Handler.Execute('audio.getPopular', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      Audios := TVkAudios.FromJsonString(AppendItemsTag(Response));
    end;
  end;
end;

function TAudioController.GetPopular(var Audios: TVkAudios; OnlyEng: Boolean; GenreId: TAudioGenre;
  Count, Offset: Integer): Boolean;
var
  Params: TVkPopAudioParams;
begin
  Params.OnlyEng(OnlyEng);
  if GenreId <> agNone then
    Params.GenreId(GenreId);
  if Count > 0 then
    Params.Count(Count);
  if Offset > 0 then
    Params.Offset(Offset);
  Result := GetPopular(Audios, Params);
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

function TAudioController.GetById(var Audio: TVkAudio; OwnerId, AudioId: Integer; AccessKey: string): Boolean;
var
  i: Integer;
  ItemStr: string;
  JArray: TJSONArray;
begin
  if AccessKey.IsEmpty then
    ItemStr := OwnerId.ToString + '_' + AudioId.ToString
  else
    ItemStr := OwnerId.ToString + '_' + AudioId.ToString + '_' + AccessKey;
  with Handler.Execute('audio.getById', [['count', '1'], ['audios', ItemStr]]) do
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

{ TVkPopAudioParams }

function TVkPopAudioParams.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkPopAudioParams.GenreId(Value: TAudioGenre): Integer;
begin
  Result := List.Add('genre_id', Value.ToConst);
end;

function TVkPopAudioParams.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkPopAudioParams.OnlyEng(Value: Boolean): Integer;
begin
  Result := List.Add('only_eng', Value);
end;

{ TVkAudioEditParams }

function TVkAudioEditParams.Artist(Value: string): Integer;
begin
  Result := List.Add('artist', Value);
end;

function TVkAudioEditParams.AudioId(Value: Integer): Integer;
begin
  Result := List.Add('audio_id', Value);
end;

function TVkAudioEditParams.GenreId(Value: TAudioGenre): Integer;
begin
  Result := List.Add('genre_id', Value.ToConst);
end;

function TVkAudioEditParams.NoSearch(Value: Boolean): Integer;
begin
  Result := List.Add('no_search', Value);
end;

function TVkAudioEditParams.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkAudioEditParams.Text(Value: string): Integer;
begin
  Result := List.Add('text', Value);
end;

function TVkAudioEditParams.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

{ TVkAudioSerachParams }

function TVkAudioSearchParams.AutoComplete(Value: Boolean): Integer;
begin
  Result := List.Add('auto_complete', Value);
end;

function TVkAudioSearchParams.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkAudioSearchParams.Lyrics(Value: string): Integer;
begin
  Result := List.Add('lirycs', Value);
end;

function TVkAudioSearchParams.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkAudioSearchParams.PerformerOnly(Value: Boolean): Integer;
begin
  Result := List.Add('performer_only', Value);
end;

function TVkAudioSearchParams.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkAudioSearchParams.SearchOwn(Value: Boolean): Integer;
begin
  Result := List.Add('search_own', Value);
end;

function TVkAudioSearchParams.Sort(Value: TVkAudioSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

end.

