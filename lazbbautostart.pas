{****************************************************************************** }
{ lazbbautostart - alllow autostart of application in Windows and Linux                            }
{ bb - sdtp - november 2019                                                     }
{*******************************************************************************}

unit lazbbautostart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, registry, dialogs
  {$IFDEF Linux}
    , BaseUnix
  {$ENDIF}
  ;

  function SetAutostart(progname, exename: string): boolean;
  function UnsetAutostart(progname: string): boolean;


implementation

// Windows Create entry in registry
// HKey_CURRENT_USER\Software\Microsoft\Windows\CurentVersion\Run
// linux Create progname'.desktop' file in HomeDir+'.config/autostart/'

function SetAutostart(progname, exename: string): boolean;
var
  {$IFDEF WINDOWS}
    reg: TRegistry;
    RunRegKeyVal, RunRegKeySz: string;
  {$ENDIF}
  {$IFDEF Linux}
    HomeDir: string;
  {$ENDIF}
begin
  Result:= true;
  {$IFDEF Linux}
    HomeDir:= GetUserDir;
    if not DirectoryExists(HomeDir+'.config/autostart/') then CreateDir(HomeDir+'.config/autostart/');
    If not FileExists(HomeDir+'.config/autostart/contactmgr.desktop') then
    begin
      StrDskFile:= TstringList.Create;
      StrDskFile.Add('[Desktop Entry]');
      StrDskFile.Add('Type=Application');
      StrDskFile.Add('Encoding=UTF-8');
      StrDskFile.Add('Name='+progname);
      StrDskFile.Add('Comment=Contacts manager');
      StrDskFile.Add('Exec='+exename);
      Icon=HomeDir+'.config/autostart/'+progname+'.png';
      StrDskFile.Add('Terminal=false');
      StrDskFile.SaveToFile(HomeDir+'.config/autostart/'+progname+'.desktop');
      // Important ! Give the file all permissions
      fpChmod (HomeDir+'.config/autostart/'+progname+'.desktop',&777);
    end;
  {$ENDIF}
  {$IFDEF WINDOWS}
    // Check if the values are already in the registry, if no, create them
    RunRegKeyVal:= UTF8ToAnsi(ProgName);
    RunRegKeySz:= UTF8ToAnsi('"'+ExeName+'"');
    Reg:= TRegistry.Create;
    Reg.RootKey:= HKEY_CURRENT_USER;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', True);
    if not Reg.ValueExists(RunRegKeyVal) then
    try
      reg.WriteString(RunRegKeyVal, RunRegKeySz);
      reg.CloseKey;
    except
      result:= False;
    end;
    Reg.Free;
  {$ENDIF}
end;

// Windows delete entry in registry
// HKey_CURRENT_USER\Software\Microsoft\Windows\CurentVersion\Run
// linux delete progname+'.desktop' file in HomeDir+'.config/autostart/'

function UnsetAutostart(progname: string): boolean;
var
  {$IFDEF WINDOWS}
    reg: TRegistry;
    RunRegKeyVal: string;
  {$ENDIF}
  {$IFDEF Linux}
    HomeDir: string;
  {$ENDIF}
begin
  result:= true;
  {$IFDEF Linux}
    HomeDir:= GetUserDir;
    If FileExists(HomeDir+'.config/autostart/'+progname+'.desktop') then DeleteFile(HomeDir+'.config/autostart/'+progname+'.desktop');
  {$ENDIF}
  {$IFDEF WINDOWS}
    //On vérifie que ces valeurs sont bien dans le registre
    Reg:= TRegistry.Create;
    Reg.RootKey:= HKEY_CURRENT_USER;
    Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', True);
    RunRegKeyVal:= UTF8ToAnsi(ProgName);
    if Reg.ValueExists(RunRegKeyVal) then
    try
      Reg.DeleteValue(RunRegKeyVal);
      Reg.CloseKey;
    except
      result:= false;
    end;
    Reg.Free;
  {$ENDIF}
end;

end.

