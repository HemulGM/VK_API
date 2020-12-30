unit VK.Entity.Doc.Save;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.AudioMessage,
  VK.Entity.Doc, VK.Entity.Graffiti;

type
  TVkDocSaved = class
  private
    FType: string;
    FAudio_message: TVkAudioMessage;
    FDoc: TVkDocument;
    FGraffiti: TVkGraffiti;
  public
    property&Type: string read FType write FType;
    property AudioMessage: TVkAudioMessage read FAudio_message write FAudio_message;
    property Doc: TVkDocument read FDoc write FDoc;
    property Graffiti: TVkGraffiti read FGraffiti write FGraffiti;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkDocSaved;
  end;

implementation

{TVkDocSaved}

constructor TVkDocSaved.Create;
begin
  //Не создаем объекты вложений, их создаст JSON парсер
  //Создан будет только один объект
  inherited;
end;

destructor TVkDocSaved.Destroy;
begin
  //Ну а тут, уничтожим, то что было создано
  if Assigned(FAudio_message) then
    FAudio_message.Free;
  if Assigned(FDoc) then
    FDoc.Free;
  if Assigned(FGraffiti) then
    FGraffiti.Free;

  inherited;
end;

function TVkDocSaved.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkDocSaved.FromJsonString(AJsonString: string): TVkDocSaved;
begin
  result := TJson.JsonToObject<TVkDocSaved>(AJsonString);
end;

end.

