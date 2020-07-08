unit VK.Notes;

interface

uses
  System.SysUtils, System.Generics.Collections, REST.Client, VK.Controller, VK.Types, VK.Entity.Audio, System.JSON,
  VK.Entity.Note;

type
  TVkParamsNotesGet = record
    List: TParams;
    function NoteIds(Value: TArrayOfInteger): Integer;
    function UserId(Value: Integer): Integer;
    function Offset(Value: Integer): Integer;
    function Count(Value: Integer): Integer;
    function Sort(Value: Boolean): Integer;
  end;

  TNotesController = class(TVkController)
  public
    function Get(var Notes: TVkNotes; Params: TParams): Boolean; overload;
    function Get(var Notes: TVkNotes; Params: TVkParamsNotesGet): Boolean; overload;
  end;

implementation

uses
  VK.API, VK.CommonUtils;

{ TNotesController }

function TNotesController.Get(var Notes: TVkNotes; Params: TParams): Boolean;
begin
  with Handler.Execute('notes.get', Params) do
  begin
    Result := Success;
    if Result then
    begin
      try
        Notes := TVkNotes.FromJsonString(Response);
      except
        Result := False;
      end;
    end;
  end;
end;

function TNotesController.Get(var Notes: TVkNotes; Params: TVkParamsNotesGet): Boolean;
begin
  Result := Get(Notes, Params.List);
end;

{ TVkParamsNotesGet }

function TVkParamsNotesGet.Count(Value: Integer): Integer;
begin
  Result := List.Add('count', Value);
end;

function TVkParamsNotesGet.NoteIds(Value: TArrayOfInteger): Integer;
begin
  Result := List.Add('notes_ids', Value.ToString);
end;

function TVkParamsNotesGet.Offset(Value: Integer): Integer;
begin
  Result := List.Add('offset', Value);
end;

function TVkParamsNotesGet.Sort(Value: Boolean): Integer;
begin
  Result := List.Add('sort', Value);
end;

function TVkParamsNotesGet.UserId(Value: Integer): Integer;
begin
  Result := List.Add('user_id', Value);
end;

end.

