unit VK.Entity.Asr;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  Vk.Wrap.Interceptors, VK.Entity.Common, VK.Entity.Common.List,
  VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common.ExtendedList, VK.Types;

type
  TVkAsrStatus = class(TVkEntity)
  private
    FId: string;
    [JsonReflectAttribute(ctString, rtString, TAsrStateInterceptor)]
    FStatus: TVkAsrState;
    FText: string;
  public
    /// <summary>
    /// Идентификатор созданной задачи на обработку аудиозаписи в формате UUID
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Статус задачи на обработку аудиозаписи
    /// </summary>
    property Status: TVkAsrState read FStatus write FStatus;
    /// <summary>
    /// Расшифровка текста. Имеет значение, если параметр status имеет значение finished
    /// </summary>
    property Text: string read FText write FText;
  end;

implementation

uses
  VK.CommonUtils;

end.

