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
    /// ќценка времени ответа в минутах (дл€ status = answer_mark)
    /// </summary>
    property Minutes: Integer read FMinutes write FMinutes;
    /// <summary>
    /// Cтатус сообщества
    /// </summary>
    /// <returns>none Ч сообщество не онлайн; online Ч сообщество онлайн (отвечает мгновенно); answer_mark Ч сообщество отвечает быстро.</returns>
    property Status: TVkGroupStatusType read FStatus write FStatus;
  end;

implementation

end.

