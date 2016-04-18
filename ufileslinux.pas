unit ufileslinux;


interface

uses
  ufilescore, ufilesutf8;

implementation

initialization
  //todo: check if Linux uses utf8 as locale
  RegisterUtf8AsAnsiNames;

end.
