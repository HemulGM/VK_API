unit VK.Entity.Graffiti;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkGraffiti = class(TVkObject)
  private
    FPhoto_604: string;
    FOwner_id: integer;
    FPhoto_130: string;
    FAccess_key: string;
  public
    /// <summary>
    /// ������������� ��������
    /// </summary>
    property Id;
    /// <summary>
    /// ���� �������
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ������������� ������ ��������
    /// </summary>
    property OwnerId: integer read FOwner_id write FOwner_id;
    /// <summary>
    /// URL ����������� ��� �������������
    /// </summary>
    property Photo130: string read FPhoto_130 write FPhoto_130;
    /// <summary>
    /// URL ��������������� �����������
    /// </summary>
    property Photo604: string read FPhoto_604 write FPhoto_604;
  end;

implementation

end.

