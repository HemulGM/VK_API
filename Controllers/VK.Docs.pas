unit VK.Docs;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Doc.Save, VK.Entity.Video.Save;

type
  TVkDocUploadType = (dutDoc, dutAudioMessage);

  TDocController = class(TVkController)
  public
    /// <summary>
    /// Получает адрес сервера для загрузки документа в личное сообщение
    /// </summary>
    /// <param name="var UploadUrl">искомый адрес сервера</param>
    /// <param name="Type">тип документа</param>
    /// <param name="PeerId">идентификатор назначения</param>
    function GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType; PeerId: Integer): Boolean; overload;
    /// <summary>
    /// Получает адрес сервера для загрузки документа в личное сообщение
    /// </summary>
    /// <param name="var UploadUrl">искомый адрес сервера</param>
    /// <param name="Type">тип документа</param>
    function GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType): Boolean; overload;
    //
    function Save(var Doc: TVkDocSaved; FileData: string; Title, Tags: string; ReturnTags: Boolean = False): Boolean;
    //
    function SaveAudioMessage(var Doc: TVkDocSaved; FileName: string; Title, Tags: string; PeerId: Integer = 0;
      ReturnTags: Boolean = False): Boolean;
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
      Doc := TVkDocSaved.FromJsonString(Response);
      Result := True;
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
    if TCustomVK(VK).Uploader.Upload(Url, FileName, Response) then
    begin
      if Save(Doc, Response, Title, Tags) then
      begin
        Result := True;
      end;
    end
    else
      TCustomVK(VK).DoError(Self, TVkException.Create(Response), -1, Response);
  end;
end;

end.

