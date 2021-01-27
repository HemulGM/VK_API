unit VK.Entity.OldApp;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkOldApp = class(TVkBasicObject)
  private
    FPhoto_604: string;
    FPhoto_130: string;
    FAccess_key: string;
  public
    /// <summary>
    /// ������������� ����������
    /// </summary>
    property Id;
    /// <summary>
    /// �������� ����������
    /// </summary>
    property Name;
    /// <summary>
    /// ���� �������
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
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

