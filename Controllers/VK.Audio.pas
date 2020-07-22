unit VK.Audio;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  REST.Json, VK.CommonUtils, VK.Entity.Playlist, VK.Entity.Audio.Upload;

type
  TVkParamsAudioGet = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function AlbumId(Value: Integer): Integer;
    function PlaylistId(Value: Integer): Integer;
    function AudioIds(Value: TIds): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function AccessKey(Value: string): Integer;
  end;

  TVkParamsPopAudio = record
    List: TParams;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function OnlyEng(Value: Boolean): Integer;
    function GenreId(Value: TAudioGenre): Integer;
  end;

  TVkParamsPlaylist = record
    List: TParams;
    function OwnerId(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
  end;

  TVkAudioSort = (asDateAdd, asDuration, asPopular);

  TVkParamsAudioSearch = record
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

  TVkParamsAudioEdit = record
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
  public
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function Get(var Audios: TVkAudios; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function Get(var Audios: TVkAudios; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function Get(var Audios: TVkAudios; Params: TVkParamsAudioGet): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function GetRecommendations(var Audios: TVkAudios; Params: TVkParamsAudioGet): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function GetRecommendations(var Audios: TVkAudios; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// Returns a list of audio files from the "Popular".
    /// </summary>
    function GetPopular(var Audios: TVkAudios; Params: TVkParamsPopAudio): Boolean; overload;
    /// <summary>
    /// Returns a list of audio files from the "Popular".
    /// </summary>
    function GetPopular(var Audios: TVkAudios; OnlyEng: Boolean = False; GenreId: TAudioGenre = agNone; Count: Integer =
      0; Offset: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function GetPlaylists(var Items: TVkPlaylists; Params: TVkParamsPlaylist): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об плейлистах
    /// </summary>
    function GetPlaylists(var Items: TVkPlaylists; OwnerID: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function GetById(var Audios: TVkAudios; List: TVkAudioIndexes): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписи
    /// </summary>
    function GetById(var Audio: TVkAudio; OwnerId, AudioId: Integer; AccessKey: string = ''): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки аудиозаписей
    /// </summary>
    function GetUploadServer(var UploadUrl: string): Boolean;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии (нестрогий поиск)
    /// </summary>
    function Search(var Audios: TVkAudios; Query: string): Boolean; overload;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии с заданным критерием поиска
    /// </summary>
    function Search(var Audios: TVkAudios; Query: string; AutoComplete, PerformerOnly: Boolean): Boolean; overload;
    /// <summary>
    /// Возвращает список аудиозаписей в соответствии с заданным критерием поиска
    /// </summary>
    function Search(var Audios: TVkAudios; Params: TVkParamsAudioSearch): Boolean; overload;
    /// <summary>
    /// Сохраняет аудиозаписи после успешной загрузки.
    /// </summary>
    function Save(var Audio: TVkAudio; AudioSaveData: TVkAudioUploadResponse): Boolean;
    /// <summary>
    /// Копирует аудиозапись на страницу пользователя или группы.
    /// </summary>
    function Add(var Id: Integer; AudioId, OwnerId: Integer; GroupId: Integer = 0; AlbumId: Integer = -1): Boolean;
    /// <summary>
    /// Удаляет аудиозапись со страницы пользователя или сообщества.
    /// </summary>
    function Delete(AudioId, OwnerId: Integer): Boolean;
    /// <summary>
    /// Редактирует данные аудиозаписи на странице пользователя или сообщества.
    /// </summary>
    function Edit(AudioId, OwnerId: Integer; Params: TVkParamsAudioEdit): Boolean;
  end;

implementation

uses
  VK.API;

{
		var parameters = new VkParameters
			{
				{ "extended", extended ,
				{ "count", count ,
				{ "fields", fields
			;
			return _vk.Call<AudioGetCatalogResult>("audio.getCatalog", parameters);
}
{
  var parameters = new VkParameters
  {
    { "audio", audio ,
    { "target_ids", targetIds
  ;
  return _vk.Call<ReadOnlyCollection<long>>("audio.setBroadcast", parameters);
}
{
public AudioPlaylist CreatePlaylist(long ownerId, string title, string description = null, IEnumerable<string> audioIds = null)
			var parameters = new VkParameters
				 "owner_id", ownerId ,
				 "title", title ,
				 "description", description ,
				 "audio_ids", audioIds;
			return _vk.Call<AudioPlaylist>("audio.createPlaylist", parameters);
}

{
var parameters = new VkParameters
			{
				{ "owner_id", ownerId ,
				{ "playlist_id", playlistId
			;

			return _vk.Call<bool>("audio.deletePlaylist", parameters);
}

{
var parameters = new VkParameters
			{
				{ "owner_id", ownerId ,
				{ "playlist_id", playlistId ,
				{ "title", title ,
				{ "description", description ,
				{ "audio_ids", audioIds
			;

			return _vk.Call<bool>("audio.editPlaylist", parameters);
}

{
var parameters = new VkParameters
			{
				{ "owner_id", ownerId ,
				{ "playlist_id", playlistId
			;
			return _vk.Call<AudioPlaylist>("audio.getPlaylistById", parameters);
}

{
var parameters = new VkParameters
			{
				{ "filter", filter ,
				{ "active", active
			;
			return _vk.Call<ReadOnlyCollection<object>>("audio.getBroadcastList", parameters);
}
{
var parameters = new VkParameters
			{
				{ "lyrics_id", lyricsId
			;
			return _vk.Call<Lyrics>("audio.getLyrics", parameters);
}
{
GetRecommendations
{ "target_audio", targetAudio ,
				{ "user_id", userId ,
				{ "offset", offset ,
				{ "count", count ,
				{ "shuffle", shuffle
}
{
  var parameters = new VkParameters
  {
    { "owner_id", ownerId ,
    { "playlist_id", playlistId ,
    { "audio_ids", audioIds
  ;
  return _vk.Call("audio.addToPlaylist", parameters).ToReadOnlyCollectionOf<long>(x => x["audio_id"]);
}
{
var parameters = new VkParameters
			{
				{ "audio_id", audioId ,
				{ "owner_id", ownerId ,
				{ "before", before ,
				{ "after", after
			;
			return _vk.Call<bool>("audio.reorder", parameters);
}
{
			var parameters = new VkParameters
			{
				{ "audio_id", audioId ,
				{ "owner_id", ownerId
			;
			return _vk.Call<Audio>("audio.restore", parameters);
}

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
      try
        Audio := TVkAudio.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAudioController.Search(var Audios: TVkAudios; Params: TVkParamsAudioSearch): Boolean;
begin
  with Handler.Execute('audio.search', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Audios := TVkAudios.FromJsonString(Response);
      except
        Result := False;
      end;
    end
  end;
end;

function TAudioController.Search(var Audios: TVkAudios; Query: string): Boolean;
var
  Params: TVkParamsAudioSearch;
begin
  Params.Query(Query);
  Params.AutoComplete(True);
  Params.PerformerOnly(False);
  Result := Search(Audios, Params);
end;

function TAudioController.Search(var Audios: TVkAudios; Query: string; AutoComplete, PerformerOnly: Boolean): Boolean;
var
  Params: TVkParamsAudioSearch;
begin
  Params.AutoComplete(AutoComplete);
  Params.PerformerOnly(PerformerOnly);
  Params.Query(Query);
  Result := Search(Audios, Params);
end;

function TAudioController.Get(var Audios: TVkAudios; OwnerId: Integer): Boolean;
var
  Params: TVkParamsAudioGet;
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
  //access_key
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
    Result := Success and (Response = '1');
  end;
end;

function TAudioController.Edit(AudioId, OwnerId: Integer; Params: TVkParamsAudioEdit): Boolean;
begin
  Params.AudioId(AudioId);
  Params.OwnerId(OwnerId);
  with Handler.Execute('audio.edit', Params.List) do
  begin
    Result := Success and (Response <> '0');
  end;
end;

function TAudioController.Get(var Audios: TVkAudios; Params: TParams): Boolean;
begin
  with Handler.Execute('audio.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Audios := TVkAudios.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAudioController.Get(var Audios: TVkAudios; Params: TVkParamsAudioGet): Boolean;
begin
  Result := Get(Audios, Params.List);
end;

function TAudioController.GetPlaylists(var Items: TVkPlaylists; OwnerID: Integer): Boolean;
var
  Params: TVkParamsPlaylist;
begin
  Params.OwnerId(OwnerID);
  Params.Count(100);
  Result := GetPlaylists(Items, Params);
end;

function TAudioController.GetPopular(var Audios: TVkAudios; Params: TVkParamsPopAudio): Boolean;
begin
  with Handler.Execute('audio.getPopular', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Audios := TVkAudios.FromJsonString(AppendItemsTag(Response));
      except
        Result := False;
      end;
    end;
  end;
end;

function TAudioController.GetPopular(var Audios: TVkAudios; OnlyEng: Boolean; GenreId: TAudioGenre; Count, Offset:
  Integer): Boolean;
var
  Params: TVkParamsPopAudio;
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
  Params: TVkParamsAudioGet;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Result := GetRecommendations(Audios, Params);
end;

function TAudioController.GetRecommendations(var Audios: TVkAudios; Params: TVkParamsAudioGet): Boolean;
begin
  with Handler.Execute('audio.getRecommendations', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Audios := TVkAudios.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAudioController.GetPlaylists(var Items: TVkPlaylists; Params: TVkParamsPlaylist): Boolean;
begin
  with Handler.Execute('audio.getPlaylists', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkPlaylists.FromJsonString(Response);
      except
        Result := False;
      end;
    end
  end;
end;

function TAudioController.GetById(var Audio: TVkAudio; OwnerId, AudioId: Integer; AccessKey: string): Boolean;
var
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
      try
        JArray := TJSONArray(TJSONObject.ParseJSONValue(Response));
        try
          if JArray.Count > 0 then
            Audio := TVkAudio.FromJsonString(JArray.Items[0].ToString);
        finally
          JArray.Free;
        end;
      except
        Result := False;
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
      try
        Audios := TVkAudios.FromJsonString(Response);
      except
        Result := False;
      end;
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
      try
        JSONItem := TJSONObject.ParseJSONValue(Response);
        try
          UploadUrl := JSONItem.GetValue<string>('upload_url', '');
        finally
          JSONItem.Free;
        end;
        Result := not UploadUrl.IsEmpty;
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkAudioParams }

function TVkParamsAudioGet.AccessKey(Value: string): Integer;
begin
  Result := List.Add('access_key', Value);
end;

function TVkParamsAudioGet.AlbumId(Value: Integer): Integer;
begin
  Result := List.Add('album_id', Value);
end;

function TVkParamsAudioGet.AudioIds(Value: TIds): Integer;
begin
  Result := List.Add('audio_ids', Value);
end;

function TVkParamsAudioGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsAudioGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsAudioGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsAudioGet.PlaylistId(Value: Integer): Integer;
begin
  Result := List.Add('playlist_id', Value);
end;

{ TVkPlaylistParams }

function TVkParamsPlaylist.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPlaylist.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPlaylist.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

{ TVkPopAudioParams }

function TVkParamsPopAudio.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsPopAudio.GenreId(Value: TAudioGenre): Integer;
begin
  Result := List.Add('genre_id', Value.ToConst);
end;

function TVkParamsPopAudio.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsPopAudio.OnlyEng(Value: Boolean): Integer;
begin
  Result := List.Add('only_eng', Value);
end;

{ TVkAudioEditParams }

function TVkParamsAudioEdit.Artist(Value: string): Integer;
begin
  Result := List.Add('artist', Value);
end;

function TVkParamsAudioEdit.AudioId(Value: Integer): Integer;
begin
  Result := List.Add('audio_id', Value);
end;

function TVkParamsAudioEdit.GenreId(Value: TAudioGenre): Integer;
begin
  Result := List.Add('genre_id', Value.ToConst);
end;

function TVkParamsAudioEdit.NoSearch(Value: Boolean): Integer;
begin
  Result := List.Add('no_search', Value);
end;

function TVkParamsAudioEdit.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsAudioEdit.Text(Value: string): Integer;
begin
  Result := List.Add('text', Value);
end;

function TVkParamsAudioEdit.Title(Value: string): Integer;
begin
  Result := List.Add('title', Value);
end;

{ TVkAudioSerachParams }

function TVkParamsAudioSearch.AutoComplete(Value: Boolean): Integer;
begin
  Result := List.Add('auto_complete', Value);
end;

function TVkParamsAudioSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsAudioSearch.Lyrics(Value: string): Integer;
begin
  Result := List.Add('lirycs', Value);
end;

function TVkParamsAudioSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsAudioSearch.PerformerOnly(Value: Boolean): Integer;
begin
  Result := List.Add('performer_only', Value);
end;

function TVkParamsAudioSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsAudioSearch.SearchOwn(Value: Boolean): Integer;
begin
  Result := List.Add('search_own', Value);
end;

function TVkParamsAudioSearch.Sort(Value: TVkAudioSort): Integer;
begin
  Result := List.Add('sort', Ord(Value));
end;

end.

