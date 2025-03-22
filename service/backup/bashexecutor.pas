 unit BashExecutor;

{$mode objfpc}{$H+}


(*
 program TestBashExecutor;

uses
  BashExecutor, SysUtils;

var
  Executor: TBashExecutor;
begin
  Executor := TBashExecutor.Create;
  try
    // Execute a simple command
    Executor.ExecuteCommand('echo "Hello, World!"');
    WriteLn('Output: ', Executor.OutputText);
    WriteLn('Exit Code: ', Executor.ExitCode);

    // Execute a command that lists files in the current directory
    Executor.ExecuteCommand('ls -l');
    WriteLn('Output: ', Executor.OutputText);
    WriteLn('Exit Code: ', Executor.ExitCode);

    // Execute a command that fails (e.g., trying to list a non-existent directory)
    Executor.ExecuteCommand('ls /nonexistent');
    WriteLn('Output: ', Executor.OutputText);
    WriteLn('Exit Code: ', Executor.ExitCode);
  finally
    Executor.Free;
  end;
end.
*)

interface

uses
  Classes, SysUtils, Process;

type
  TBashExecutor = class
  private
    FCommand: string;
    FOutput: TStringList;
    FExitCode: Integer;
    function GetOutputText: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteCommand(const ACommand: string);
    property OutputText: string read GetOutputText;
    property ExitCode: Integer read FExitCode;
  end;

implementation

constructor TBashExecutor.Create;
begin
  FOutput := TStringList.Create;
end;

destructor TBashExecutor.Destroy;
begin
  FOutput.Free;
  inherited Destroy;
end;

procedure TBashExecutor.ExecuteCommand(const ACommand: string);
var
  Process: TProcess;
begin
  FCommand := ACommand;
  FOutput.Clear;
  FExitCode := -1;

  Process := TProcess.Create(nil);
  try
    Process.Executable := '/bin/bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(FCommand);
    Process.Options := [poUsePipes, poStderrToOutPut]; // Redirect output to pipes
    Process.ShowWindow := swoHIDE; // Hide the command window (if applicable)
    Process.Execute;

    // Read the output
    FOutput.LoadFromStream(Process.Output);
    FExitCode := Process.ExitCode; // Get the exit code
  finally
    Process.Free;
  end;
end;

function TBashExecutor.GetOutputText: string;
begin
  Result := FOutput.Text;
end;

end.
