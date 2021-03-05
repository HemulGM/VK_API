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
    /// ������ ������� ������ � ������� (��� status = answer_mark)
    /// </summary>
    property Minutes: Integer read FMinutes write FMinutes;
    /// <summary>
    /// C����� ����������
    /// </summary>
    /// <returns>none � ���������� �� ������; online � ���������� ������ (�������� ���������); answer_mark � ���������� �������� ������.</returns>
    property Status: TVkGroupStatusType read FStatus write FStatus;
  end;

implementation

end.

