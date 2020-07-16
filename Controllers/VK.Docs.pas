unit VK.Docs;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Doc.Save, VK.Entity.Video.Save, VK.Entity.Doc, VK.Entity.Doc.Types;

type
  TVkDocUploadType = (dutDoc, dutAudioMessage);

  TVkDocTypeFilter = (vdtAll, vdtText, vdtArchives, vdtGIF, vdtPictures, vdtAudios, vdtVideos, vdtBooks, vdtOther);

  TVkParamsDocsGet = record
    List: TParams;
    /// <summary>
    /// Идентификатор пользователя или сообщества, которому принадлежат документы.
    /// </summary>
    function OwnerId(Value: Integer): Integer;
    /// <summary>
    /// Фильтр по типу документа.
    /// </summary>
    function &Type(Value: TVkDocTypeFilter): Integer;
    /// <summary>
    /// Возвращать теги
    /// </summary>
    function ReturnTags(Value: Boolean): Integer;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества документов.
    /// </summary>
    function Offset(Value: Integer): Integer;
    /// <summary>
    /// Количество документов, информацию о которых нужно вернуть.
    /// </summary>
    function Count(Value: Integer): Integer;
  end;

  TVkParamsDocsSearch = record
    List: TParams;
    /// <summary>
    /// True — искать среди собственных документов пользователя.
    /// </summary>
    function SearchOwn(Value: Boolean): Integer;
    /// <summary>
    /// Строка поискового запроса. Например, зеленые тапочки.
    /// </summary>
    function Query(Value: string): Integer;
    /// <summary>
    /// Количество документов, информацию о которых нужно вернуть.
    /// </summary>
    function Count(Value: Integer): Integer;
    /// <summary>
    /// Смещение, необходимое для выборки определенного подмножества документов.
    /// </summary>
    function Offset(Value: Integer): Integer;
    /// <summary>
    /// Возвращать теги.
    /// </summary>
    function ReturnTags(Value: Boolean): Integer;
  end;

  TDocController = class(TVkController)
  public
    /// <summary>
    /// Получает адрес сервера для загрузки документа в личное сообщение.
    /// </summary>
    function GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType; PeerId: Integer): Boolean; overload;
    /// <summary>
    /// ППолучает адрес сервера для загрузки документа в личное сообщение.
    /// </summary>
    function GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType): Boolean; overload;
    /// <summary>
    /// Сохраняет документ после его успешной загрузки на сервер.
    /// </summary>
    function Save(var Doc: TVkDocSaved; FileData: string; Title, Tags: string; ReturnTags: Boolean = False): Boolean;
    /// <summary>
    /// Сохраняет аудиосообщение
    /// </summary>
    function SaveAudioMessage(var Doc: TVkDocSaved; FileName: string; Title, Tags: string; PeerId: Integer = 0;
      ReturnTags: Boolean = False): Boolean;
    /// <summary>
    /// Возвращает расширенную информацию о документах пользователя или сообщества.
    /// </summary>
    function Get(var Items: TVkDocuments; Params: TParams): Boolean; overload;
    /// <summary>
    /// Возвращает расширенную информацию о документах пользователя или сообщества.
    /// </summary>
    function Get(var Items: TVkDocuments; Params: TVkParamsDocsGet): Boolean; overload;
    /// <summary>
    /// Возвращает информацию о документах по их идентификаторам.
    /// </summary>
    function GetById(var Items: TVkDocuments; Docs: TArrayOfString; ReturnTags: Boolean): Boolean; overload;
    /// <summary>
    /// Копирует документ в документы текущего пользователя.
    /// </summary>
    function Add(var Id: Integer; OwnerId, DocId: Integer; AccessKey: string): Boolean; overload;
    /// <summary>
    /// Удаляет документ пользователя или группы.
    /// </summary>
    function Delete(OwnerId, DocId: Integer): Boolean; overload;
    /// <summary>
    /// Редактирует документ пользователя или группы.
    /// </summary>
    function Edit(OwnerId, DocId: Integer; Title: string; Tags: TArrayOfString = []): Boolean; overload;
    /// <summary>
    /// Возвращает доступные для пользователя типы документов.
    /// </summary>
    function GetTypes(var Items: TVkDocTypes; OwnerId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки документов.
    /// </summary>
    function GetUploadServer(var UploadUrl: string; GroupId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает адрес сервера для загрузки документов в папку Отправленные, для последующей отправки документа на стену или личным сообщением.
    /// </summary>
    function GetWallUploadServer(var UploadUrl: string; GroupId: Integer): Boolean; overload;
    /// <summary>
    /// Возвращает результаты поиска по документам.
    /// </summary>
    function Search(var Items: TVkDocuments; Params: TVkParamsDocsSearch): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TDocController }

function TDocController.GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType; PeerId: Integer): Boolean;
var
  Params: TParams;
  JSONItem: TJSONValue;
begin
  case&Type of
    dutDoc:
      Params.Add('type', 'doc');
    dutAudioMessage:
      Params.Add('type', 'audio_message');
  end;
  if PeerId <> 0 then
    Params.Add('peer_id', PeerId);
  with Handler.Execute('docs.getMessagesUploadServer', Params) do
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

function TDocController.Get(var Items: TVkDocuments; Params: TParams): Boolean;
begin
  with Handler.Execute('docs.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkDocuments.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TDocController.Add(var Id: Integer; OwnerId, DocId: Integer; AccessKey: string): Boolean;
var
  Params: TParams;
begin
  Params.Add('owner_id', OwnerId);
  Params.Add('doc_id', DocId);
  if not AccessKey.IsEmpty then
    Params.Add('access_key', AccessKey);
  with Handler.Execute('docs.Add', Params) do
  begin
    Result := Success and TryStrToInt(Response, Id);
  end;
end;

function TDocController.Delete(OwnerId, DocId: Integer): Boolean;
begin
  with Handler.Execute('docs.delete', [['doc_id', DocId.ToString], ['owner_id', OwnerId.ToString]]) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TDocController.Edit(OwnerId, DocId: Integer; Title: string; Tags: TArrayOfString): Boolean;
var
  Params: TParams;
begin
  Params.Add('doc_id', DocId);
  Params.Add('owner_id', OwnerId);
  Params.Add('title', Title);
  Params.Add('tags', Tags);
  with Handler.Execute('docs.edit', Params) do
  begin
    Result := Success and (Response = '1');
  end;
end;

function TDocController.Get(var Items: TVkDocuments; Params: TVkParamsDocsGet): Boolean;
begin
  Result := Get(Items, Params.List);
end;

function TDocController.GetById(var Items: TVkDocuments; Docs: TArrayOfString; ReturnTags: Boolean): Boolean;
begin
  with Handler.Execute('docs.getById', [['docs', Docs.ToString], ['return_tags', BoolToString(ReturnTags)]]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkDocuments.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TDocController.GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType): Boolean;
var
  Params: TParams;
  JSONItem: TJSONValue;
begin
  case&Type of
    dutDoc:
      Params.Add('type', 'doc');
    dutAudioMessage:
      Params.Add('type', 'audio_message');
  end;
  with Handler.Execute('docs.getMessagesUploadServer', Params) do
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

function TDocController.GetTypes(var Items: TVkDocTypes; OwnerId: Integer): Boolean;
begin
  with Handler.Execute('docs.getTypes', ['owner_id', OwnerId.ToString]) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkDocTypes.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TDocController.GetUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
var
  JSONItem: TJSONValue;
begin
  with Handler.Execute('docs.getUploadServer', ['group_id', GroupId.ToString]) do
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

function TDocController.GetWallUploadServer(var UploadUrl: string; GroupId: Integer): Boolean;
var
  JSONItem: TJSONValue;
begin
  with Handler.Execute('docs.getWallUploadServer', ['group_id', GroupId.ToString]) do
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

function TDocController.Save(var Doc: TVkDocSaved; FileData: string; Title, Tags: string; ReturnTags: Boolean): Boolean;
var
  Params: TParams;
begin
  Params.Add('file', FileData);
  Params.Add('title', Title);
  Params.Add('tags', Tags);
  if ReturnTags then
    Params.Add('return_tags', BoolToString(ReturnTags));
  with Handler.Execute('docs.save', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Doc := TVkDocSaved.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TDocController.SaveAudioMessage(var Doc: TVkDocSaved; FileName, Title, Tags: string; PeerId: Integer;
  ReturnTags: Boolean): Boolean;
var
  Url, Response: string;
begin
  Result := False;
  if GetMessagesUploadServer(Url, dutAudioMessage, PeerId) then
  begin
    try
      if TCustomVK(VK).Uploader.Upload(Url, FileName, Response) then
      begin
        Result := Save(Doc, Response, Title, Tags);
      end
      else
        TCustomVK(VK).DoError(Self, TVkException.Create(Response), -1, Response);
    except
      Result := False;
    end;
  end;
end;

function TDocController.Search(var Items: TVkDocuments; Params: TVkParamsDocsSearch): Boolean;
begin
  with Handler.Execute('docs.search', Params.List) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Items := TVkDocuments.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

{ TVkParamsDocsGet }

function TVkParamsDocsGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsDocsGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsDocsGet.OwnerId(Value: Integer): Integer;
begin
  Result := List.Add('owner_id', Value);
end;

function TVkParamsDocsGet.ReturnTags(Value: Boolean): Integer;
begin
  Result := List.Add('return_tags', Value);
end;

function TVkParamsDocsGet.&Type(Value: TVkDocTypeFilter): Integer;
begin
  Result := List.Add('type', Ord(Value));
end;

{ TVkParamsDocsSearch }

function TVkParamsDocsSearch.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsDocsSearch.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsDocsSearch.Query(Value: string): Integer;
begin
  Result := List.Add('q', Value);
end;

function TVkParamsDocsSearch.ReturnTags(Value: Boolean): Integer;
begin
  Result := List.Add('return_tags', Value);
end;

function TVkParamsDocsSearch.SearchOwn(Value: Boolean): Integer;
begin
  Result := List.Add('search_own', Value);
end;

end.

