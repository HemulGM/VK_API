unit VK.Audio;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller,
  VK.Types, VK.Entity.Audio, System.JSON, REST.Json, VK.CommonUtils,
  VK.Entity.Playlist, VK.Entity.Audio.Upload, VK.Entity.Audio.Catalog;

type
  TVkParamsAudioGet = record
    List: TParams;
    function OwnerId(const Value: TVkPeerId): TVkParamsAudioGet;
    function AlbumId(const Value: Integer): TVkParamsAudioGet;
    function PlaylistId(const Value: Integer): TVkParamsAudioGet;
    function AudioIds(const Value: TIdList): TVkParamsAudioGet;
    function Offset(const Value: Integer): TVkParamsAudioGet;
    function Count(const Value: Integer): TVkParamsAudioGet;
    function AccessKey(const Value: string): TVkParamsAudioGet;
    class function Create: TVkParamsAudioGet; static;
  end;

  TVkParamsAudioGetRecomendations = record
    List: TParams;
    function TargetAudio(const Value: Integer): TVkParamsAudioGetRecomendations;
    function UserId(const Value: TVkPeerId): TVkParamsAudioGetRecomendations;
    function Offset(const Value: Integer): TVkParamsAudioGetRecomendations;
    function Count(const Value: Integer): TVkParamsAudioGetRecomendations;
    function Shuffle(const Value: Boolean): TVkParamsAudioGetRecomendations;
  end;

  TVkParamsPopAudio = record
    List: TParams;
    function Offset(const Value: Integer): TVkParamsPopAudio;
    function Count(const Value: Integer): TVkParamsPopAudio;
    function OnlyEng(const Value: Boolean): TVkParamsPopAudio;
    function GenreId(const Value: TVkAudioGenre): TVkParamsPopAudio;
  end;

  TVkParamsPlaylist = record
    List: TParams;
    function OwnerId(const Value: TVkPeerId): TVkParamsPlaylist;
    function Offset(const Value: Integer): TVkParamsPlaylist;
    function Count(const Value: Integer): TVkParamsPlaylist;
  end;

  TVkParamsByArtist = record
    List: TParams;
    function ArtistId(const Value: string): TVkParamsByArtist;
    function Offset(const Value: Integer): TVkParamsByArtist;
    function Count(const Value: Integer): TVkParamsByArtist;
  end;

  TVkParamsAudioSearch = record
    List: TParams;
    function Query(const Value: string): TVkParamsAudioSearch;
    function AutoComplete(const Value: Boolean): TVkParamsAudioSearch;
    function PerformerOnly(const Value: Boolean): TVkParamsAudioSearch;
    function Lyrics(const Value: string): TVkParamsAudioSearch;
    function Sort(const Value: TVkMediaSort): TVkParamsAudioSearch;
    function SearchOwn(const Value: Boolean): TVkParamsAudioSearch;
    function Offset(const Value: Integer): TVkParamsAudioSearch;
    function Count(const Value: Integer): TVkParamsAudioSearch;
  end;

  TVkParamsAudioBasicSearch = record
    List: TParams;
    function Query(const Value: string): TVkParamsAudioBasicSearch;
    function Offset(const Value: Integer): TVkParamsAudioBasicSearch;
    function Count(const Value: Integer): TVkParamsAudioBasicSearch;
  end;

  TVkParamsAudioPlaylistSearch = record
    List: TParams;
    function Query(const Value: string): TVkParamsAudioPlaylistSearch;
    function Offset(const Value: Integer): TVkParamsAudioPlaylistSearch;
    function Count(const Value: Integer): TVkParamsAudioPlaylistSearch;
    function Filters(const Value: TVkAudioPlaylistFilter = TVkAudioPlaylistFilter.All): TVkParamsAudioPlaylistSearch;
  end;

  TVkParamsAudioEdit = record
    List: TParams;
    function AudioId(const Value: Integer): TVkParamsAudioEdit;
    function OwnerId(const Value: TVkPeerId): TVkParamsAudioEdit;
    function Artist(const Value: string): TVkParamsAudioEdit;
    function Title(const Value: string): TVkParamsAudioEdit;
    function Text(const Value: string): TVkParamsAudioEdit;
    function GenreId(const Value: TVkAudioGenre): TVkParamsAudioEdit;
    function NoSearch(const Value: Boolean): TVkParamsAudioEdit;
  end;

  TVkParamsAudioEditPlaylist = record
    List: TParams;
    function PlaylistId(const Value: Integer): TVkParamsAudioEditPlaylist;
    function OwnerId(const Value: Integer): TVkParamsAudioEditPlaylist;
    function Description(const Value: string): TVkParamsAudioEditPlaylist;
    function Title(const Value: string): TVkParamsAudioEditPlaylist;
    function AudioIds(const Value: TArrayOfString): TVkParamsAudioEditPlaylist;
  end;

  TVkParamsAudioReorder = record
    List: TParams;
    function OwnerId(const Value: TVkPeerId): TVkParamsAudioReorder;
    function AudioId(const Value: Integer): TVkParamsAudioReorder;
    function Before(const Value: Integer): TVkParamsAudioReorder;
    function After(const Value: Integer): TVkParamsAudioReorder;
  end;

  TAudioController = class(TVkController)
  public
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function Get(var Audios: TVkAudios; OwnerId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function Get(var Audios: TVkAudios; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function Get(var Audios: TVkAudios; Params: TVkParamsAudioGet): Boolean; overload;
    /// <summary>
    ///
    /// </summary>
    function GetArtistById(var Item: TVkAudioArtist; const ArtistId: string; Extended: Boolean = False): Boolean;
    /// <summary>
    ///
    /// </summary>
    function GetAudiosByArtist(var Items: TVkAudios; Params: TVkParamsByArtist): Boolean;
    /// <summary>
    ///
    /// </summary>
    function GetAlbumsByArtist(var Items: TVkPlaylists; Params: TVkParamsByArtist): Boolean;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function GetRecommendations(var Audios: TVkAudios; Params: TVkParamsAudioGetRecomendations): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function GetRecommendations(var Audios: TVkAudios; UserId: TVkPeerId = 0): Boolean; overload;
    /// <summary>
    /// Returns a list of audio files from the "Popular".
    /// </summary>
    function GetPopular(var Audios: TVkAudios; Params: TVkParamsPopAudio): Boolean; overload;
    /// <summary>
    /// Returns a list of audio files from the "Popular".
    /// </summary>
    function GetPopular(var Audios: TVkAudios; OnlyEng: Boolean = False; GenreId: TVkAudioGenre = TVkAudioGenre.None; Count: Integer = 0; Offset: Integer = 0): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function GetPlaylists(var Items: TVkPlaylists; Params: TVkParamsPlaylist): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об плейлистах
    /// </summary>
    function GetPlaylists(var Items: TVkPlaylists; OwnerId: TVkPeerId): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписях
    /// </summary>
    function GetById(var Audios: TVkAudios; List: TVkAudioIndexes): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписи
    /// </summary>
    function GetById(var Audio: TVkAudio; OwnerId, AudioId: Integer; AccessKey: string = ''): Boolean; overload;
    /// <summary>
    /// Возвращает информацию об аудиозаписи
    /// </summary>
    function GetById(var Audio: TVkAudio; FromAudio: TVkAudio): Boolean; overload;
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
    /// Возвращает список альбомов в соответствии с заданным критерием поиска
    /// </summary>
    function SearchAlbums(var Items: TVkPlaylists; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список альбомов в соответствии с заданным критерием поиска
    /// </summary>
    function SearchAlbums(var Items: TVkPlaylists; Params: TVkParamsAudioBasicSearch): Boolean; overload;
    /// <summary>
    /// Возвращает список артистов в соответствии с заданным критерием поиска
    /// </summary>
    function SearchArtists(var Items: TVkAudioArtists; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список артистов в соответствии с заданным критерием поиска
    /// </summary>
    function SearchArtists(var Items: TVkAudioArtists; Params: TVkParamsAudioBasicSearch): Boolean; overload;
    /// <summary>
    /// Возвращает список плейлистов в соответствии с заданным критерием поиска
    /// </summary>
    function SearchPlaylists(var Items: TVkPlaylists; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает список плейлистов в соответствии с заданным критерием поиска
    /// </summary>
    function SearchPlaylists(var Items: TVkPlaylists; Params: TVkParamsAudioPlaylistSearch): Boolean; overload;
    /// <summary>
    /// Сохраняет аудиозаписи после успешной загрузки.
    /// </summary>
    function Save(var Audio: TVkAudio; AudioSaveData: TVkAudioUploadResponse): Boolean;
    /// <summary>
    /// Копирует аудиозапись на страницу пользователя или группы.
    /// </summary>
    function Add(var Id: Integer; AudioId: integer; OwnerId: TVkPeerId; GroupId: TVkPeerId = 0; AlbumId: Integer = -1; AccessKey: string = ''): Boolean;
    /// <summary>
    /// Удаляет аудиозапись со страницы пользователя или сообщества.
    /// </summary>
    function Delete(const AudioId: integer; OwnerId: TVkPeerId): Boolean;
    /// <summary>
    /// Редактирует данные аудиозаписи на странице пользователя или сообщества.
    /// </summary>
    function Edit(const Params: TVkParamsAudioEdit): Boolean;
    /// <summary>
    /// Создать плейлист
    /// </summary>
    function CreatePlaylist(var Item: TVkAudioPlaylist; const OwnerId: TVkPeerId; const Title: string; Description: string = ''; AudioIds: TArrayOfString = []): Boolean;
    /// <summary>
    /// Удалить плейлист
    /// </summary>
    function DeletePlaylist(const PlaylistId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Изменить плейлист
    /// </summary>
    function EditPlaylist(Params: TVkParamsAudioEditPlaylist): Boolean;
    /// <summary>
    /// Возвращает информацию об плейлисте
    /// </summary>
    function GetPlaylistById(var Item: TVkAudioPlaylist; const PlaylistId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Восстанавливает аудиозапись
    /// </summary>
    function Restore(const AudioId: Integer; OwnerId: TVkPeerId = 0): Boolean;
    /// <summary>
    /// Получить каталог аудиозаписей
    /// </summary>
    function GetCatalog(var Items: TVkAudioCatalog; Params: TParams = []): Boolean;
    /// <summary>
    /// Позволяет изменить порядок аудио.
    /// </summary>
    function Reorder(Params: TVkParamsAudioReorder): Boolean;
    /// <summary>
    /// Удалить плейлист
    /// </summary>
    function AddToPlaylist(var Items: TVkAudioInfoItems; const OwnerId: TVkPeerId; const PlaylistId: Integer; AudioIds: TArrayOfString): Boolean;
    /// <summary>
    ///  Метод загружает на указанный URL адрес аудиозапись, точнее, просто файл, но его можно использовать, чтобы загрузить аудиозапись в профиль в связке с методом audio.getUploadServer(), чтобы получить URL для загрузки.
    /// </summary>
    function Upload(const UploadUrl, FileName: string; var Response: TVkAudioUploadResponse): Boolean; overload;
    /// <summary>
    /// Метод загружает на указанный URL адрес аудиозапись
    /// </summary>
    function Upload(var Audio: TVkAudio; const FileName: string): Boolean; overload;
    /// <summary>
    /// Метод возвращает количество аудиозаписей пользователя или группы
    /// </summary>
    function GetCount(var Value: Integer; OwnerId: TVkPeerId): Boolean;
    /// <summary>
    /// Метод добавляет плейлист в подписки
    /// </summary>
    function FollowPlaylist(var NewPlaylistId: Integer; OwnerId: TVkPeerId; PlaylistId: Integer): Boolean;
  end;

implementation

uses
  VK.API, System.Classes, System.Net.HttpClient, System.Net.Mime;

{
  var parameters = new VkParameters
  {
    { "audio", audio ,
    { "target_ids", targetIds
  ;
  return _vk.Call<ReadOnlyCollection<long>>("audio.setBroadcast", parameters);
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
  var parameters = new VkParameters
  {
    { "owner_id", ownerId ,
    { "playlist_id", playlistId ,
    { "audio_ids", audioIds
  ;
  return _vk.Call("audio.addToPlaylist", parameters).ToReadOnlyCollectionOf<long>(x => x["audio_id"]);
}

{ TAudioController }

function TAudioController.Upload(const UploadUrl, FileName: string; var Response: TVkAudioUploadResponse): Boolean;
var
  HTTP: THTTPClient;
  Data: TMultipartFormData;
  ResStream: TStringStream;
begin
  Result := False;
  Data := TMultipartFormData.Create;
  HTTP := THTTPClient.Create;
  ResStream := TStringStream.Create;
  try
    Data.AddFile('file', FileName);
    if HTTP.Post(UploadUrl, Data, ResStream).StatusCode = 200 then
    begin
      Response := TVkAudioUploadResponse.FromJsonString<TVkAudioUploadResponse>(ResStream.DataString);
      Result := True;
    end;
  finally
    ResStream.Free;
    Data.Free;
    HTTP.Free;
  end;
end;

function TAudioController.Upload(var Audio: TVkAudio; const FileName: string): Boolean;
var
  UploadServer: string;
  Response: TVkAudioUploadResponse;
begin
  Result := False;
  if GetUploadServer(UploadServer) then
  begin
    if Upload(UploadServer, FileName, Response) then
    begin
      try
        Result := Save(Audio, Response);
      finally
        Response.Free;
      end;
    end;
  end;
end;

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
  Result := Handler.Execute('audio.save', Params).GetObject(Audio);
end;

function TAudioController.Search(var Audios: TVkAudios; Params: TVkParamsAudioSearch): Boolean;
begin
  Result := Handler.Execute('audio.search', Params.List).GetObject(Audios);
end;

function TAudioController.SearchAlbums(var Items: TVkPlaylists; Params: TParams): Boolean;
begin
  Result := Handler.Execute('audio.searchAlbums', Params).GetObject(Items);
end;

function TAudioController.SearchAlbums(var Items: TVkPlaylists; Params: TVkParamsAudioBasicSearch): Boolean;
begin
  Result := SearchAlbums(Items, Params.List);
end;

function TAudioController.SearchArtists(var Items: TVkAudioArtists; Params: TVkParamsAudioBasicSearch): Boolean;
begin
  Result := SearchArtists(Items, Params.List);
end;

function TAudioController.SearchArtists(var Items: TVkAudioArtists; Params: TParams): Boolean;
begin
  Result := Handler.Execute('audio.searchArtists', Params).GetObject(Items);
end;

function TAudioController.SearchPlaylists(var Items: TVkPlaylists; Params: TVkParamsAudioPlaylistSearch): Boolean;
begin
  Result := SearchPlaylists(Items, Params.List);
end;

function TAudioController.SearchPlaylists(var Items: TVkPlaylists; Params: TParams): Boolean;
begin
  Result := Handler.Execute('audio.searchPlaylists', Params).GetObject(Items);
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

function TAudioController.Get(var Audios: TVkAudios; OwnerId: TVkPeerId): Boolean;
var
  Params: TVkParamsAudioGet;
begin
  if OwnerId <> 0 then
    Params.OwnerId(OwnerId);
  Result := Get(Audios, Params);
end;

function TAudioController.Add(var Id: Integer; AudioId: integer; OwnerId, GroupId: TVkPeerId; AlbumId: Integer; AccessKey: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('audio_id', AudioId);
  Params.Add('owner_id', OwnerId);
  if not AccessKey.IsEmpty then
    Params.Add('access_key', AccessKey);
  if GroupId <> 0 then
    Params.Add('group_id', GroupId);
  if AlbumId > -1 then
    Params.Add('album_id', AlbumId);
  Result := Handler.Execute('audio.add', Params).ResponseAsInt(Id);
end;

function TAudioController.AddToPlaylist(var Items: TVkAudioInfoItems; const OwnerId: TVkPeerId; const PlaylistId: Integer; AudioIds: TArrayOfString): Boolean;
begin
  Result := Handler.Execute('audio.addToPlaylist', [
    ['playlist_id', PlaylistId.ToString],
    ['owner_id', OwnerId.ToString],
    ['audio_ids', AudioIds.ToString]]).
    GetObjects(Items);
end;

function TAudioController.CreatePlaylist(var Item: TVkAudioPlaylist; const OwnerId: TVkPeerId; const Title: string; Description: string; AudioIds: TArrayOfString): Boolean;
var
  Params: TParams;
begin
  if not AudioIds.IsEmpty then
    Params.Add('audio_ids', AudioIds);
  Params.Add('owner_id', OwnerId);
  Params.Add('title', Title);
  Params.Add('description', Description);
  Result := Handler.Execute('audio.createPlaylist', Params).GetObject(Item);
end;

function TAudioController.Delete(const AudioId: integer; OwnerId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('audio.delete', [
    ['audio_id', AudioId.ToString],
    ['owner_id', OwnerId.ToString]]).
    ResponseIsTrue;
end;

function TAudioController.DeletePlaylist(const PlaylistId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('playlist_id', PlaylistId);
  Result := Handler.Execute('audio.deletePlaylist', Params).ResponseIsTrue;
end;

function TAudioController.Edit(const Params: TVkParamsAudioEdit): Boolean;
begin
  Result := not Handler.Execute('audio.edit', Params.List).ResponseIsFalse;
end;

function TAudioController.EditPlaylist(Params: TVkParamsAudioEditPlaylist): Boolean;
begin
  Result := Handler.Execute('audio.editPlaylist', Params.List).ResponseIsTrue;
end;

function TAudioController.FollowPlaylist(var NewPlaylistId: Integer; OwnerId: TVkPeerId; PlaylistId: Integer): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('playlist_id', PlaylistId);
  Result := Handler.Execute('audio.followPlaylist', Params).GetValue('playlist_id', NewPlaylistId);
end;

function TAudioController.Get(var Audios: TVkAudios; Params: TParams): Boolean;
begin
  Result := Handler.Execute('audio.get', Params).GetObject(Audios);
end;

function TAudioController.Get(var Audios: TVkAudios; Params: TVkParamsAudioGet): Boolean;
begin
  Result := Get(Audios, Params.List);
end;

function TAudioController.GetAlbumsByArtist(var Items: TVkPlaylists; Params: TVkParamsByArtist): Boolean;
begin
  Result := Handler.Execute('audio.getAlbumsByArtist', Params.List).GetObject(Items);
end;

function TAudioController.GetArtistById(var Item: TVkAudioArtist; const ArtistId: string; Extended: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('artist_id', ArtistId);
  Params.Add('extended', Extended);
  Result := Handler.Execute('audio.getArtistById', Params).GetObject(Item);
end;

function TAudioController.GetAudiosByArtist(var Items: TVkAudios; Params: TVkParamsByArtist): Boolean;
begin
  Result := Handler.Execute('audio.getAudiosByArtist', Params.List).GetObject(Items);
end;

function TAudioController.GetPlaylistById(var Item: TVkAudioPlaylist; const PlaylistId: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Params.Add('playlist_id', PlaylistId);
  Result := Handler.Execute('audio.getPlaylistById', Params).GetObject(Item);
end;

function TAudioController.GetPlaylists(var Items: TVkPlaylists; OwnerId: TVkPeerId): Boolean;
var
  Params: TVkParamsPlaylist;
begin
  Params.OwnerId(OwnerId);
  Params.Count(100);
  Result := GetPlaylists(Items, Params);
end;

function TAudioController.GetPopular(var Audios: TVkAudios; Params: TVkParamsPopAudio): Boolean;
begin
  Result := Handler.Execute('audio.getPopular', Params.List).GetObjects(Audios);
end;

function TAudioController.GetPopular(var Audios: TVkAudios; OnlyEng: Boolean; GenreId: TVkAudioGenre; Count, Offset: Integer): Boolean;
var
  Params: TVkParamsPopAudio;
begin
  Params.OnlyEng(OnlyEng);
  if GenreId <> TVkAudioGenre.None then
    Params.GenreId(GenreId);
  if Count > 0 then
    Params.Count(Count);
  if Offset > 0 then
    Params.Offset(Offset);
  Result := GetPopular(Audios, Params);
end;

function TAudioController.GetRecommendations(var Audios: TVkAudios; UserId: TVkPeerId): Boolean;
var
  Params: TVkParamsAudioGetRecomendations;
begin
  if UserId <> 0 then
    Params.UserId(UserId);
  Result := GetRecommendations(Audios, Params);
end;

function TAudioController.GetRecommendations(var Audios: TVkAudios; Params: TVkParamsAudioGetRecomendations): Boolean;
begin
  Result := Handler.Execute('audio.getRecommendations', Params.List).GetObject(Audios);
end;

function TAudioController.GetPlaylists(var Items: TVkPlaylists; Params: TVkParamsPlaylist): Boolean;
begin
  Result := Handler.Execute('audio.getPlaylists', Params.List).GetObject(Items);
end;

function TAudioController.GetById(var Audio: TVkAudio; FromAudio: TVkAudio): Boolean;
begin
  Result := GetById(Audio, FromAudio.OwnerId, FromAudio.Id, FromAudio.AccessKey);
end;

function TAudioController.GetById(var Audio: TVkAudio; OwnerId, AudioId: Integer; AccessKey: string): Boolean;
var
  ItemStr: string;
  JArray: TJSONArray;
begin
  Result := False;
  if AccessKey.IsEmpty then
    ItemStr := OwnerId.ToString + '_' + AudioId.ToString
  else
    ItemStr := OwnerId.ToString + '_' + AudioId.ToString + '_' + AccessKey;
  with Handler.Execute('audio.getById', [['count', '1'], ['audios', ItemStr]]) do
  begin
    if GetValue(JArray) then
    begin
      try
        try
          if JArray.Count > 0 then
            Audio := TVkAudio.FromJsonString<TVkAudio>(JArray.Items[0].ToString);
        finally
          JArray.Free;
        end;
        Result := True;
      except
        Result := False;
      end;
    end;
  end;
end;

function TAudioController.GetCatalog(var Items: TVkAudioCatalog; Params: TParams): Boolean;
begin
  Result := Handler.Execute('audio.getCatalog', Params).GetObject(Items);
end;

function TAudioController.GetCount(var Value: Integer; OwnerId: TVkPeerId): Boolean;
var
  Params: TParams;
begin
  if OwnerId <> 0 then
    Params.Add('owner_id', OwnerId);
  Result := Handler.Execute('audio.getCount', Params).ResponseAsInt(Value);
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
  Result := Handler.Execute('audio.getById', ['audios', ListStr]).GetObject(Audios);
end;

function TAudioController.GetUploadServer(var UploadUrl: string): Boolean;
begin
  Result := Handler.Execute('audio.getUploadServer').GetValue('upload_url', UploadUrl);
end;

function TAudioController.Reorder(Params: TVkParamsAudioReorder): Boolean;
begin
  Result := Handler.Execute('audio.reorder', Params.List).ResponseIsTrue;
end;

function TAudioController.Restore(const AudioId: Integer; OwnerId: TVkPeerId): Boolean;
begin
  Result := Handler.Execute('audio.restore', [
    ['audio_id', AudioId.ToString],
    ['owner_id', OwnerId.ToString]]).
    ResponseIsTrue;
end;

{ TVkAudioParams }

function TVkParamsAudioGet.AccessKey(const Value: string): TVkParamsAudioGet;
begin
  List.Add('access_key', Value);
  Result := Self;
end;

function TVkParamsAudioGet.AlbumId(const Value: Integer): TVkParamsAudioGet;
begin
  List.Add('album_id', Value);
  Result := Self;
end;

function TVkParamsAudioGet.AudioIds(const Value: TIdList): TVkParamsAudioGet;
begin
  List.Add('audio_ids', Value);
  Result := Self;
end;

function TVkParamsAudioGet.Count(const Value: Integer): TVkParamsAudioGet;
begin
  List.Add('count', Value);
  Result := Self;
end;

class function TVkParamsAudioGet.Create: TVkParamsAudioGet;
begin
  //
end;

function TVkParamsAudioGet.Offset(const Value: Integer): TVkParamsAudioGet;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsAudioGet.OwnerId(const Value: TVkPeerId): TVkParamsAudioGet;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsAudioGet.PlaylistId(const Value: Integer): TVkParamsAudioGet;
begin
  List.Add('playlist_id', Value);
  Result := Self;
end;

{ TVkPlaylistParams }

function TVkParamsPlaylist.Count(const Value: Integer): TVkParamsPlaylist;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPlaylist.Offset(const Value: Integer): TVkParamsPlaylist;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPlaylist.OwnerId(const Value: TVkPeerId): TVkParamsPlaylist;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkPopAudioParams }

function TVkParamsPopAudio.Count(const Value: Integer): TVkParamsPopAudio;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsPopAudio.GenreId(const Value: TVkAudioGenre): TVkParamsPopAudio;
begin
  List.Add('genre_id', Value.ToConst);
  Result := Self;
end;

function TVkParamsPopAudio.Offset(const Value: Integer): TVkParamsPopAudio;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsPopAudio.OnlyEng(const Value: Boolean): TVkParamsPopAudio;
begin
  List.Add('only_eng', Value);
  Result := Self;
end;

{ TVkAudioEditParams }

function TVkParamsAudioEdit.Artist(const Value: string): TVkParamsAudioEdit;
begin
  List.Add('artist', Value);
  Result := Self;
end;

function TVkParamsAudioEdit.AudioId(const Value: Integer): TVkParamsAudioEdit;
begin
  List.Add('audio_id', Value);
  Result := Self;
end;

function TVkParamsAudioEdit.GenreId(const Value: TVkAudioGenre): TVkParamsAudioEdit;
begin
  List.Add('genre_id', Value.ToConst);
  Result := Self;
end;

function TVkParamsAudioEdit.NoSearch(const Value: Boolean): TVkParamsAudioEdit;
begin
  List.Add('no_search', Value);
  Result := Self;
end;

function TVkParamsAudioEdit.OwnerId(const Value: TVkPeerId): TVkParamsAudioEdit;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsAudioEdit.Text(const Value: string): TVkParamsAudioEdit;
begin
  List.Add('text', Value);
  Result := Self;
end;

function TVkParamsAudioEdit.Title(const Value: string): TVkParamsAudioEdit;
begin
  List.Add('title', Value);
  Result := Self;
end;

{ TVkAudioSerachParams }

function TVkParamsAudioSearch.AutoComplete(const Value: Boolean): TVkParamsAudioSearch;
begin
  List.Add('auto_complete', Value);
  Result := Self;
end;

function TVkParamsAudioSearch.Count(const Value: Integer): TVkParamsAudioSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsAudioSearch.Lyrics(const Value: string): TVkParamsAudioSearch;
begin
  List.Add('lirycs', Value);
  Result := Self;
end;

function TVkParamsAudioSearch.Offset(const Value: Integer): TVkParamsAudioSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsAudioSearch.PerformerOnly(const Value: Boolean): TVkParamsAudioSearch;
begin
  List.Add('performer_only', Value);
  Result := Self;
end;

function TVkParamsAudioSearch.Query(const Value: string): TVkParamsAudioSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

function TVkParamsAudioSearch.SearchOwn(const Value: Boolean): TVkParamsAudioSearch;
begin
  List.Add('search_own', Value);
  Result := Self;
end;

function TVkParamsAudioSearch.Sort(const Value: TVkMediaSort): TVkParamsAudioSearch;
begin
  List.Add('sort', Ord(Value));
  Result := Self;
end;

{ TVkParamsAudioBasicSearch }

function TVkParamsAudioBasicSearch.Count(const Value: Integer): TVkParamsAudioBasicSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsAudioBasicSearch.Offset(const Value: Integer): TVkParamsAudioBasicSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsAudioBasicSearch.Query(const Value: string): TVkParamsAudioBasicSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

{ TVkParamsAudioPlaylistSearch }

function TVkParamsAudioPlaylistSearch.Count(const Value: Integer): TVkParamsAudioPlaylistSearch;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsAudioPlaylistSearch.Filters(const Value: TVkAudioPlaylistFilter): TVkParamsAudioPlaylistSearch;
begin
  List.Add('filters', Ord(Value));
  Result := Self;
end;

function TVkParamsAudioPlaylistSearch.Offset(const Value: Integer): TVkParamsAudioPlaylistSearch;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsAudioPlaylistSearch.Query(const Value: string): TVkParamsAudioPlaylistSearch;
begin
  List.Add('q', Value);
  Result := Self;
end;

{ TVkParamsAudioEditPlaylist }

function TVkParamsAudioEditPlaylist.AudioIds(const Value: TArrayOfString): TVkParamsAudioEditPlaylist;
begin
  List.Add('audio_ids', Value);
  Result := Self;
end;

function TVkParamsAudioEditPlaylist.Description(const Value: string): TVkParamsAudioEditPlaylist;
begin
  List.Add('description', Value);
  Result := Self;
end;

function TVkParamsAudioEditPlaylist.OwnerId(const Value: Integer): TVkParamsAudioEditPlaylist;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

function TVkParamsAudioEditPlaylist.PlaylistId(const Value: Integer): TVkParamsAudioEditPlaylist;
begin
  List.Add('playlist_id', Value);
  Result := Self;
end;

function TVkParamsAudioEditPlaylist.Title(const Value: string): TVkParamsAudioEditPlaylist;
begin
  List.Add('title', Value);
  Result := Self;
end;

{ TVkParamsAudioGetRecomendations }

function TVkParamsAudioGetRecomendations.Count(const Value: Integer): TVkParamsAudioGetRecomendations;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsAudioGetRecomendations.Offset(const Value: Integer): TVkParamsAudioGetRecomendations;
begin
  List.Add('offset', Value);
  Result := Self;
end;

function TVkParamsAudioGetRecomendations.Shuffle(const Value: Boolean): TVkParamsAudioGetRecomendations;
begin
  List.Add('shuffle', Value);
  Result := Self;
end;

function TVkParamsAudioGetRecomendations.TargetAudio(const Value: Integer): TVkParamsAudioGetRecomendations;
begin
  List.Add('target_audio', Value);
  Result := Self;
end;

function TVkParamsAudioGetRecomendations.UserId(const Value: TVkPeerId): TVkParamsAudioGetRecomendations;
begin
  List.Add('user_id', Value);
  Result := Self;
end;

{ TVkParamsAudioReorder }

function TVkParamsAudioReorder.After(const Value: Integer): TVkParamsAudioReorder;
begin
  List.Add('after', Value);
  Result := Self;
end;

function TVkParamsAudioReorder.AudioId(const Value: Integer): TVkParamsAudioReorder;
begin
  List.Add('audio_id', Value);
  Result := Self;
end;

function TVkParamsAudioReorder.Before(const Value: Integer): TVkParamsAudioReorder;
begin
  List.Add('before', Value);
  Result := Self;
end;

function TVkParamsAudioReorder.OwnerId(const Value: TVkPeerId): TVkParamsAudioReorder;
begin
  List.Add('owner_id', Value);
  Result := Self;
end;

{ TVkParamsByArtist }

function TVkParamsByArtist.ArtistId(const Value: string): TVkParamsByArtist;
begin
  List.Add('artist_id', Value);
  Result := Self;
end;

function TVkParamsByArtist.Count(const Value: Integer): TVkParamsByArtist;
begin
  List.Add('count', Value);
  Result := Self;
end;

function TVkParamsByArtist.Offset(const Value: Integer): TVkParamsByArtist;
begin
  List.Add('offset', Value);
  Result := Self;
end;

end.

