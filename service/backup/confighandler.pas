unit ConfigHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TConfigLine = record
    Keyword: string;
    Value: string;
    Command: string;
  end;
  PConfigLine = ^TConfigLine; // Pointer to TConfigLine

  TConfigHandler = class
  private
    FFileName: string;
    FLines: TList; // Stores pointers to TConfigLine records
    function LineToString(const ALine: TConfigLine): string;
    function StringToLine(const AString: string): TConfigLine;
    function KeywordExists(const AKeyword: string): Boolean;
    function FindLineIndex(const AKeyword: string): Integer;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure AddLine(const AKeyword, AValue, ACommand: string);
    procedure UpdateLine(const AKeyword, ANewValue, ANewCommand: string);
    procedure DeleteLine(const AKeyword: string);
    function GetLine(const AKeyword: string): TConfigLine;
  end;

implementation

constructor TConfigHandler.Create(const AFileName: string);
begin
  FFileName := AFileName;
  FLines := TList.Create;
  LoadConfig;
end;

destructor TConfigHandler.Destroy;
var
  I: Integer;
begin
  for I := 0 to FLines.Count - 1 do
    Dispose(PConfigLine(FLines[I])); // Free memory for each TConfigLine
  FLines.Free; // Free the TList
  inherited Destroy;
end;

procedure TConfigHandler.LoadConfig;
var
  ConfigFile: TextFile;
  Line: string;
  ConfigLine: PConfigLine;
begin
  if not FileExists(FFileName) then
    Exit;

  AssignFile(ConfigFile, FFileName);
  Reset(ConfigFile);
  try
    while not EOF(ConfigFile) do
    begin
      ReadLn(ConfigFile, Line);
      New(ConfigLine); // Allocate memory for a new TConfigLine
      ConfigLine^ := StringToLine(Line); // Convert string to TConfigLine
      FLines.Add(ConfigLine); // Add the pointer to the list
    end;
  finally
    CloseFile(ConfigFile);
  end;
end;

procedure TConfigHandler.SaveConfig;
var
  ConfigFile: TextFile;
  I: Integer;
begin
  AssignFile(ConfigFile, FFileName);
  Rewrite(ConfigFile);
  try
    for I := 0 to FLines.Count - 1 do
      WriteLn(ConfigFile, LineToString(PConfigLine(FLines[I])^)); // Write each line
  finally
    CloseFile(ConfigFile);
  end;
end;

procedure TConfigHandler.AddLine(const AKeyword, AValue, ACommand: string);
var
  ConfigLine: PConfigLine;
begin
  if KeywordExists(AKeyword) then
    raise Exception.Create('Keyword already exists: ' + AKeyword);

  New(ConfigLine); // Allocate memory for a new TConfigLine
  ConfigLine^.Keyword := AKeyword;
  ConfigLine^.Value := AValue;
  ConfigLine^.Command := ACommand;
  FLines.Add(ConfigLine); // Add the pointer to the list
end;

procedure TConfigHandler.UpdateLine(const AKeyword, ANewValue, ANewCommand: string);
var
  Index: Integer;
  ConfigLine: PConfigLine;
begin
  Index := FindLineIndex(AKeyword);
  if Index = -1 then
    raise Exception.Create('Keyword not found: ' + AKeyword);

  ConfigLine := PConfigLine(FLines[Index]); // Get the pointer to the TConfigLine
  ConfigLine^.Value := ANewValue; // Update the value
  ConfigLine^.Command := ANewCommand; // Update the command
end;

procedure TConfigHandler.DeleteLine(const AKeyword: string);
var
  Index: Integer;
begin
  Index := FindLineIndex(AKeyword);
  if Index = -1 then
    raise Exception.Create('Keyword not found: ' + AKeyword);

  Dispose(PConfigLine(FLines[Index])); // Free the memory for the TConfigLine
  FLines.Delete(Index); // Remove the pointer from the list
end;

function TConfigHandler.GetLine(const AKeyword: string): TConfigLine;
var
  I: Integer;
begin
  for I := 0 to FLines.Count - 1 do
  begin
    if PConfigLine(FLines[I])^.Keyword = AKeyword then
      Exit(PConfigLine(FLines[I])^); // Return the TConfigLine record
  end;
  raise Exception.Create('Keyword not found: ' + AKeyword);
end;

function TConfigHandler.LineToString(const ALine: TConfigLine): string;
begin
  Result := ALine.Keyword + ';' + ALine.Value + ';' + ALine.Command;
end;

function TConfigHandler.StringToLine(const AString: string): TConfigLine;
var
  Parts: TStringList;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ';';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := AString;
    if Parts.Count <> 3 then
      raise Exception.Create('Invalid config line format: ' + AString);
    Result.Keyword := Parts[0];
    Result.Value := Parts[1];
    Result.Command := Parts[2];
  finally
    Parts.Free;
  end;
end;

function TConfigHandler.KeywordExists(const AKeyword: string): Boolean;
begin
  Result := FindLineIndex(AKeyword) <> -1;
end;

function TConfigHandler.FindLineIndex(const AKeyword: string): Integer;
var
  I: Integer;
begin
  for I := 0 to FLines.Count - 1 do
  begin
    if PConfigLine(FLines[I])^.Keyword = AKeyword then
      Exit(I); // Return the index of the matching keyword
  end;
  Result := -1; // Keyword not found
end;

end.
