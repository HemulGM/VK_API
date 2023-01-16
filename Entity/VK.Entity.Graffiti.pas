unit VK.Entity.Graffiti;

interface

uses
  Generics.Collections, Vk.Types, Rest.Json, VK.Entity.Common;

type
  TVkGraffiti = class(TVkObject)
  private
    FOwner_id: TVkPeerId;
    FAccess_key: string;
    FHeight: Integer;
    FWidth: Integer;
    FUrl: string;
  public
    /// <summary>
    /// Идентификатор граффити
    /// </summary>
    property Id;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Идентификатор автора граффити
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property Url: string read FUrl write FUrl;
  end;

implementation

end.

