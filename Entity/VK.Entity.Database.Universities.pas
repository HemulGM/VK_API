unit VK.Entity.Database.Universities;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkUniversity = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
  end;

  TVkUniversities = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkUniversity>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkUniversity> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkUniversities}

destructor TVkUniversities.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkUniversity>(FItems);
  inherited;
end;

end.

