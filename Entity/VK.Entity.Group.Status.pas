unit VK.Entity.Group.Status;

interface

uses
  Generics.Collections, Rest.Json, REST.JsonReflect, Vk.Types, VK.Wrap.Interceptors, VK.Entity.Common;

type
  TVkGroupStatus = class(TVkEntity)
  private
    FMinutes: Integer;
    [JsonReflectAttribute(ctString, rtString, TGroupStatusTypeInterceptor)]
    FStatus: TVkGroupStatusType;
  public
    /// <summary>
    /// Оценка времени ответа в минутах (для status = answer_mark)
    /// </summary>
    property Minutes: Integer read FMinutes write FMinutes;
    /// <summary>
    /// Cтатус сообщества
    /// </summary>
    /// <returns>none — сообщество не онлайн; online — сообщество онлайн (отвечает мгновенно); answer_mark — сообщество отвечает быстро.</returns>
    property Status: TVkGroupStatusType read FStatus write FStatus;
  end;

implementation

end.

