unit VK.Asr;

interface

uses
  System.SysUtils, System.Generics.Collections, VK.Controller, VK.Types,
  VK.Entity.Asr;

type
  /// <summary>
  /// Методы для работы с сервисом распознавания речи
  /// </summary>
  TAsr = class(TVkController)
  public
    /// <summary>
    /// Метод проверяет статус задачи на обработку аудиозаписи и возвращает текстовую расшифровку аудиозаписи
    /// </summary>
    function CheckStatus(out Status: TVkAsrStatus; const TaskId: string): Boolean; overload;
    /// <summary>
    /// Метод возвращает ссылку на адрес сервера для загрузки аудиозаписи.
    /// </summary>
    function GetUploadUrl(out UploadUrl: string): Boolean; overload;
    /// <summary>
    /// Метод возвращает ссылку на адрес сервера для загрузки аудиозаписи.
    /// audio - JSON-ответ из запроса на отправку файла аудиозаписи на адрес сервера загрузки
    /// Суммарная максимальная длительность файлов аудиозаписей в сутки: 100 минут.
    /// </summary>
    function Process(out TaskId: string; const Audio: string; Model: TVkAsrModel): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TAsr }

function TAsr.CheckStatus(out Status: TVkAsrStatus; const TaskId: string): Boolean;
begin
  Result := Handler.Execute('asr.checkStatus').GetObject(Status);
end;

function TAsr.GetUploadUrl(out UploadUrl: string): Boolean;
begin
  Result := Handler.Execute('asr.getUploadUrl').GetValue('upload_url', UploadUrl);
end;

function TAsr.Process(out TaskId: string; const Audio: string; Model: TVkAsrModel): Boolean;
begin
  Result := Handler.Execute('asr.process', [
    ['audio', Audio],
    ['model', Model.ToString]
    ]).GetValue('task_id', TaskId);
end;

end.

