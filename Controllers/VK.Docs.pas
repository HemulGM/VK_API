unit VK.Docs;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types,
  VK.Entity.Audio, System.JSON, VK.Entity.Doc.Save;

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
    function GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType; PeerId: Integer):
      Boolean; overload;
    /// <summary>
    /// Получает адрес сервера для загрузки документа в личное сообщение
    /// </summary>
    /// <param name="var UploadUrl">искомый адрес сервера</param>
    /// <param name="Type">тип документа</param>
    function GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType): Boolean; overload;
    //
    function Save(var Doc: TVkDocSaved; FileData: string; Title, Tags: string; ReturnTags: Boolean = False): Boolean;
  end;

implementation

uses
  VK.API, VK.Utils;

{ TDocController }

function TDocController.GetMessagesUploadServer(var UploadUrl: string; &Type: TVkDocUploadType;
  PeerId: Integer): Boolean;
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
  Params.Add('peer_id', PeerId);
  with Handler.Execute('docs.getMessagesUploadServer', Params) do
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
    if Success then
    begin
      Doc := TVkDocSaved.FromJsonString(Response);
      Result := True;
    end
    else
      Result := False;
  end;
end;

end.

