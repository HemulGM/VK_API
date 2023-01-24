unit VK.Entity.Audio.Upload;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkAudioUploadResponse = class(TVkEntity)
  private
    FAudio: string;
    FHash: string;
    FRedirect: string;
    FServer: integer;
    FTitle: string;
    FArtist: string;
  public
    property Audio: string read FAudio write FAudio;
    property Hash: string read FHash write FHash;
    property Redirect: string read FRedirect write FRedirect;
    property Server: integer read FServer write FServer;
    property Artist: string read FArtist write FArtist;
    property Title: string read FTitle write FTitle;
  end;

implementation

end.

