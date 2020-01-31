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
    /// ���������� ���������� �� ������������
    /// </summary>
    /// <param name="var Audios: TVkAudios">������ ������������</param>
    /// <param name="OwnerId: Integer">������������� ������������ ��� ����������</param>
    function Get(var Audios: TVkAudios; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ���������� �� ������������
    /// </summary>
    /// <param name="var Audios: TVkAudios">������ ������������</param>
    /// <param name="Params: TVkAudioParams">��������� �������</param>
    function GetRecommendations(var Audios: TVkAudios; Params: TVkAudioParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� �� ������������
    /// </summary>
    /// <param name="var Audios: TVkAudios">������ ������������</param>
    /// <param name="OwnerId: Integer">������������� ������������ ��� ����������</param>
    function GetRecommendations(var Audios: TVkAudios; OwnerId: Integer = 0): Boolean; overload;
    /// <summary>
    /// ���������� ���������� �� ������������
    /// </summary>
    /// <param name="var Audios: TVkAudios">������ ������������</param>
    /// <param name="Params: TVkAudioParams">��������� �������</param>
    function Get(var Audios: TVkAudios; Params: TVkAudioParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� �� ������������
    /// </summary>
    /// <param name="var Audios: TVkAudios">������ ������������</param>
    /// <param name="Params: TVkAudioParams">��������� �������</param>
    function GetPlaylists(var Items: TVkPlaylists; Params: TVkPlaylistParams): Boolean; overload;
    /// <summary>
    /// ���������� ���������� �� ������������
    /// </summary>
    /// <param name="var Audios: TVkAudios">������ ������������</param>
    /// <param name="Params: TVkAudioParams">��������� �������</param>
    function GetPlaylists(var Items: TVkPlaylists; OwnerID: Integer): Boolean; overload;
    /// <summary>
    /// ���������� ���������� �� ������������
    /// </summary>
    /// <param name="var Audios: TVkAudios">������ ������������</param>
    /// <param name="List: TVkAudioIndexes">������ ��������������� ������������</param>
    function GetById(var Audios: TVkAudios; List: TVkAudioIndexes): Boolean; overload;
    /// <summary>
    /// ���������� ���������� �� �����������
    /// </summary>
    /// <param name="var Audio: TVkAudio">�����������</param>
    /// <param name="Item: TVkAudioIndexe">������������ �����������</param>
    function GetById(var Audio: TVkAudio; Item: TVkAudioIndex): Boolean; overload;
    /// <summary>
    /// ���������� ���������� �� �����������
    /// </summary>
    /// <param name="var Audio: TVkAudio">�����������</param>
    /// <param name="Item: TVkAudioIndexe">�� ������ �����������</param>
    function GetById(var Audio: TVkAudio; Item: TVkAudio): Boolean; overload;
    /// <summary>
    /// ���������� ����� ������� ��� �������� ������������
    /// </summary>
    function GetUploadServer(var UploadUrl: string): Boolean;
    /// <summary>
    /// ���������� ������ ������������ � ������������ � �������� ��������� ������
    /// </summary>
    /// <param name="var Audios: TVkAudios">������ ������������</param>
    /// <param name="Query: string">��������� ������</param>
    function Search(var Audios: TVkAudios; Query: string): Boolean;
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
    if Success then
    begin
      try
        Audio := TVkAudio.FromJsonString(Response);
        Result := True;
      except
        Result := False;
      end;
    end
    else
      Result := False;
  end;
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

function TAudioController.GetUploadServer(var UploadUrl: string): Boolean;
var
  JSONItem: TJSONValue;
begin
  with Handler.Execute('audio.getUploadServer') do
  begin
    if Success then
    begin
      JSONItem := TJSONObject.ParseJSONValue(Response);
      UploadUrl := JSONItem.GetValue<string>('upload_url', '');
      JSONItem.Free;
      Result := not UploadUrl.IsEmpty;
    end
    else
      Result := False;
  end;
end;

function TAudioController.Search(var Audios: TVkAudios; Query: string): Boolean;
var
  JArray: TJSONArray;
  i: Integer;
begin
  with Handler.Execute('audio.search', ['q', Query]) do
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

