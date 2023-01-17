unit VK.Entity.Database.Faculties;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkFaculty = class(TVkObject)
  private
    FTitle: string;
  public
    property Title: string read FTitle write FTitle;
  end;

  TVkFaculties = class(TVkEntity)
  private
    FCount: Integer;
    FItems: TArray<TVkFaculty>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkFaculty> read FItems write FItems;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkFaculties}

destructor TVkFaculties.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkFaculty>(FItems);
  {$ENDIF}
  inherited;
end;

end.

