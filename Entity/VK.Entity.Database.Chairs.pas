unit VK.Entity.Database.Chairs;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkChair = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
  end;

  TVkChairs = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkChair>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkChair> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkChairs}

destructor TVkChairs.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkChair>(FItems);
  inherited;
end;

end.

