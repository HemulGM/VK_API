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
    /// Идентификатор приложения
    /// </summary>
    property Id;
    /// <summary>
    /// Название приложения
    /// </summary>
    property Name;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// URL изображения для предпросмотра
    /// </summary>
    property Photo130: string read FPhoto_130 write FPhoto_130;
    /// <summary>
    /// URL полноразмерного изображения
    /// </summary>
    property Photo604: string read FPhoto_604 write FPhoto_604;
  end;

implementation

end.

