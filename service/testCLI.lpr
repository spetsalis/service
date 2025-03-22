program testCLI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,ConfigHandler,CustApp, BashExecutor
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    function CountParam:integer; //returns count of command line parameters
    procedure WriteAllParam;  //print on screen the command line parameters

  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

Function TMyApplication.CountParam:integer;
begin

   CountParam:=  ParamCount;
end;

Procedure TMyApplication.WriteAllParam;
Var i:integer;
Begin
writeln ('Parameters passed: ', CountParam);
writeln ('These are:');
for i := 1 to CountParam
 do writeln('Param.',i,' ',ParamStr(i));

end;


constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('');
  writeln('Usage: ', ExeName, ' -h');
  writeln('');
  writeln('Commands');
  writeln('init    : will create a config file ');
  writeln('add     : will add a new entry to the config file');
  writeln('        : must be followed by "service alias;0;service name"');
  writeln('remove  : will remove a config entry ');
  writeln('        : must be followed by service alias');
  writeln('start   : followed by service alias');
  writeln('stop    : followed by service alias');
  writeln('disable : followed by service alias ');
  writeln('enable  : followed by service alias');
  writeln('');
end;

var
  Application: TMyApplication;
  MyConfig: TConfigHandler;
  Cfilename: string;

  procedure init(fname:string);
  begin
  Myconfig:=TConfigHandler.Create(Cfilename);
  try
    Myconfig.LoadConfig;
    Myconfig.AddLine('Keyname','0','Service_name');
    Myconfig.SaveConfig;
  finally
    Myconfig.Free;
   end;
  end;

  procedure add(fname,param:string);
  var passpar:TConfigLine;
      Parts:TStringList;
  begin

  Myconfig:=TConfigHandler.Create(Cfilename);
     Parts := TStringList.Create;

  //parse string
  try
    Parts.Delimiter := ';';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := param;
    if Parts.Count <> 3 then
      raise Exception.Create('Invalid config line format: ' + param);
    passpar.Keyword := Parts[0];
    passpar.Value := Parts[1];
    passpar.Command := Parts[2];
  finally
    Parts.Free;
  end;

   Myconfig:=TConfigHandler.Create(Cfilename);
    try
    Myconfig.AddLine(passpar.Keyword,passpar.Value,passpar.Command);
    Myconfig.SaveConfig;
  finally
    Myconfig.Free;
   end;
  end;

  procedure remove(fname,param:string);
  begin
   Myconfig:=TConfigHandler.Create(Cfilename);
   try
    Myconfig.DeleteLine(param);
    Myconfig.SaveConfig;
  finally
    Myconfig.Free;
   end;
  end;

  procedure service (fname,param,flag:string) ;
  var
  Executor: TBashExecutor;
  command:string;
  line:TConfigLine;

  begin
  Myconfig:=TConfigHandler.Create(Cfilename);
  Executor := TBashExecutor.Create;

  //read line
  line:=Myconfig.GetLine(param);

  //setup command
  command:= 'systemctl '+flag+' '+line.Command;

  try
    // Execute a simple command
    Executor.ExecuteCommand(command);
    writeln ('executed comand: ',command);
    WriteLn('Output: ', Executor.OutputText);
    WriteLn('Exit Code: ', Executor.ExitCode);
    finally
    Executor.Free;
    end;
  end;

procedure show;

 begin
   Myconfig:=TConfigHandler.Create(Cfilename);
   try
     Myconfig.Show;
   finally
     Myconfig.Free;
    end;
   end;


//   Application Code Starts Here
//  ==============================


begin
  Cfilename := '/home/spyros/Documents/Pascal/testCLI/config.txt'; //init filename
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  //Application.WriteAllParam;
  if Application.CountParam >=1 then
     begin
      case   ParamStr(1) of


//action init
         'init':
                         begin
                           writeln ('action selected:init');
                           try
                             init(Cfilename);
                           except
                           writeln('No action. File already initialized');exit;
                           Application.WriteHelp;
                           end;

                         end;
//action add

          'add':
                         begin
                           writeln ('action selected:add');
                          if  Application.CountParam <2 then
                          begin
                          writeln('no parameters passed');
                          exit;
                          end
                         else
                            try
                              writeln ('adding ',ParamStr(2));
                              add (Cfilename, ParamStr(2));
                            except
                                   writeln ('Something went wrong');
                                   Application.WriteHelp;
                           end;
                         end ;


//action list

                    'list':
                                   begin
                                     writeln ('action selected:list');
                                    if  Application.CountParam <1 then
                                    begin
                                    writeln('no parameters passed');
                                    exit;
                                    end
                                   else
                                      try
                                       show;
                                      except
                                             writeln ('Something went wrong');
                                             Application.WriteHelp;
                                     end;
                                   end ;


//action remove

         'remove':
                         begin
                          writeln ('action selected:remove');
                          if  Application.CountParam <2 then
                          begin
                          writeln('no parameters passed');
                          exit;
                          end
                         else
                            try
                              writeln ('removing ',ParamStr(2));
                              remove (Cfilename, ParamStr(2));
                            except
                                   writeln ('Something went wrong');
                                   Application.WriteHelp;
                           end;
                         end;


//action start
         'start':
                         begin
                          writeln ('action selected:start ',ParamStr(2));
                          if  Application.CountParam <2 then
                          begin
                          writeln('no parameters passed');
                          exit;
                          end
                         else
                          try service(Cfilename, ParamStr(2),'start');
                          except
                            writeln ('Something went wrong');
                            Application.WriteHelp;
                          end;
                         end;
//action stop
         'stop':
                         begin
                          writeln ('action selected:stop ',ParamStr(2));
                          if  Application.CountParam <2 then
                          begin
                          writeln('no parameters passed');
                          exit;
                          end
                         else
                          try service(Cfilename, ParamStr(2),'stop');
                          except
                            writeln ('Something went wrong');
                            Application.WriteHelp;
                          end;
                         end;

//action disable
         'disable':
                         begin
                          writeln ('action selected:disable ',ParamStr(2));
                          if  Application.CountParam <2 then
                          begin
                          writeln('no parameters passed');
                          exit;
                          end
                         else
                          try service(Cfilename, ParamStr(2),'disable');
                          except
                            writeln ('Something went wrong');
                            Application.WriteHelp;
                          end;
                         end;

//action enable
         'enable':       begin
                          writeln ('action selected:enable ',ParamStr(2));
                          if  Application.CountParam <2 then
                          begin
                          writeln('no parameters passed');
                          exit;
                          end
                         else
                          try service(Cfilename, ParamStr(2),'enable');
                          except
                            writeln ('Something went wrong');
                            Application.WriteHelp;
                          end;
                         end;

//action status
                  'status':       begin
                                   writeln ('action selected:enable ',ParamStr(2));
                                   if  Application.CountParam <2 then
                                   begin
                                   writeln('no parameters passed');
                                   exit;
                                   end
                                  else
                                   try service(Cfilename, ParamStr(2),'status');
                                   except
                                     writeln ('Something went wrong');
                                     Application.WriteHelp;
                                   end;
                                  end;

//help
         '-h': writeln ('action selected: help')

       else
          begin
            writeln('');
            writeln ('Unknown command: "',ParamStr(1),'"');
            writeln('');
            Application.WriteHelp;
          end;
     end;
    end;
  Application.Free;
end.

