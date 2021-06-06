{******************************************************************************}
{ lazbbosver - Returns OS version information (Windows, Linux and Mac          }
{ Replacement for lazbbosversion unit, uses class instead record               }
{ sdtp - bb - may 2021                                                    }
{ Localization data in lazbbosver.lng to copy in application .lng file         }
{******************************************************************************}
unit lazbbosver;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
    Windows,
  {$ELSE}
    process,
  {$ENDIF}
  Classes, SysUtils, lazbbutils, lazbbinifiles;

Type

  TOSVersion = class
  private
    fOSName: string;
    fArchitecture: string;
    // Windows
    FPID : Integer; {platform ID}
    FVerTyp, FVerPro : Integer;
    FVerProd: String;
    fVerProEx: DWORD;
    FVerSup : String;
    FVerMaj, FVerMin, FVerBuild: Integer;
    FVerMask : Integer;
    FSrvPMaj: Word;
    fSrvPMin: Word;
    fProdTyp: BYTE;
    fReserved: BYTE;
    FVerDetail: string;     //Description of the OS, with version, build etc.
        // Unix
    fKernelName: string;
    fKernelRelease: string;
    fKernelVersion: string;
    fNetworkNode: string;
    ProdStr: array of String;
    Win10Build: array of array of String;
     procedure localize(lang: string; LangFile: TBbInifile);
  public
    constructor Create (lang: string='en'; LangFile: TBbInifile=nil); overload;
    destructor Destroy; override;
    procedure GetSysInfo;
    {$IFDEF WINDOWS}
      procedure GetNT32Info;
    {$ENDIF}
    property OSName: string read FOSName;
    property Architecture: string read fArchitecture;
    property KernelName: string read FKernelName;
    property KernelRelease: string read FKernelRelease;
    property KernelVersion: string read FKernelVersion;
    property NetworkNode: string read FNetworkNode;
    property VerTyp: integer read FVerTyp;      // Windows type
    property VerMaj: integer read FVerMaj;      // major version number
    property VerMin: integer read FVerMin;      // Minor version number
    property VerProd : String read FVerProd;    // Version type
    property VerSup : String read FVerSup;      // Additional version information
    property VerMask : Integer read FVerMask;   // Product suite mask;
    property VerBuild: integer read FVerBuild;  // Build number
    property VerDetail: string read FVerDetail; //Description of the OS, with version, build etc.
  end;

  {$IFDEF WINDOWS}
  POSVersionInfoExA = ^TOSVersionInfoExA;
  POSVersionInfoExW = ^TOSVersionInfoExW;
  POSVersionInfoEx = POSVersionInfoExA;
  _OSVERSIONINFOEXA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance string for PSS usage }
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  _OSVERSIONINFOEXW = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar; { Maintenance string for PSS usage }
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  _OSVERSIONINFOEX = _OSVERSIONINFOEXA;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  TOSVersionInfoExW = _OSVERSIONINFOEXW;
  TOSVersionInfoEx = TOSVersionInfoExA;
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  OSVERSIONINFOEX = OSVERSIONINFOEXA;

const
    VER_NT_WORKSTATION       = 1;
    VER_NT_DOMAIN_CONTROLLER = 2;
    VER_NT_SERVER            = 3;
    VER_SUITE_SMALLBUSINESS            = $0001;
    VER_SUITE_ENTERPRISE               = $0002;
    VER_SUITE_BACKOFFICE               = $0004;
    VER_SUITE_TERMINAL                 = $0010;
    VER_SUITE_SMALLBUSINESS_RESTRICTED = $0020;
    VER_SUITE_DATACENTER               = $0080;
    VER_SUITE_PERSONAL                 = $0200;

    StatStr: array of String = ('Microsoft Windows 32',
                                    'Microsoft Windows 95',
                                    'Microsoft Windows 95-OSR2',
                                    'Microsoft Windows 98',
                                    'Microsoft Windows 98 SE',
                                    'Microsoft Windows ME',
                                    'Microsoft Windows NT 3.5',
                                    'Microsoft Windows NT 4',
                                    'Microsoft Windows 2000',
                                    'Microsoft Windows XP',
                                    'Microsoft Windows Server 2003',
                                    'Microsoft Windows Vista',
                                    'Microsoft Windows Server 2008',
                                    'Microsoft Windows Server 2008 R2',
                                    'Microsoft Windows 7',
                                    'Microsoft Windows 8',
                                    'Microsoft Windows Server 2012',
                                    'Microsoft Windows 8.1',
                                    'Windows Server 2012 R2',
                                    'Microsoft Windows 10',
                                    'Windows Server 2016',
                                    'Windows Server 2019',
                                    'Système inconnu');




      // Valeurs en hexa pour info
   ProdStrEx: array [0..$A3] of String =('Unknown product',                                     //00
                                         'Ultimate Edition',                                    //01
                                         'Home Basic Edition',                                  //02
                                         'Home Premium Edition',                                //03
                                         'Enterprise',                                          //04
                                         'Home Basic Edition',                                  //05
                                         'Business',                                            //06
                                         'Server Standard Edition (full installation)',         //07
                                         'Server Datacenter (full installation)',               //08
                                         'Small Business Server',                               //09
                                         'Server Enterprise Edition (full installation)',       //0A
                                         'Starter Edition',                                     //0B
                                         'Server Datacenter (core installation)',               //0C
                                         'Server Standard Edition (core installation)',         //0D
                                         'Server Enterprise Edition (core installation)',       //0E
                                         'Server Enterprise Edition for Itanium-based Systems', //0F
                                         'Business N',                                          //10
                                         'Web Server Edition',                                  //11
                                         'Cluster Server',                                      //12
                                         'Home Server Edition',                                 //13
                                         'Storage Server Express Edition',                      //14
                                         'Storage Server Standard Edition',                     //15
                                         'Storage Server Workgroup Edition',                    //16
                                         'Storage Server Enterprise Edition',                   //17
                                         'Server for Small Business Edition',                   //18
                                         'Small Business Server Premium Edition',               //19
                                         'Home Premium Edition',                                //1A
                                         'Enterprise N',                                        //1B
                                         'Ultimate Edition',                                    //1C
                                         'Web Server (core installation)',                      //1D
                                         'Windows Essential Business Server Management Server', //1E
                                         'Windows Essential Business Server Security Server',   //1F
                                         'Windows Essential Business Server Messaging Server',  //20
                                         'Server Foundation',                                   //21
                                         'Windows Home Server 2011',                            //22
                                         'Windows Server 2008 without Hyper-V for Windows Essential Server Solutions',   //23
                                         'Server Standard without Hyper-V',                       //24
                                         'Server Datacenter without Hyper-V (full installation)', //25
                                         'Server Enterprise without Hyper-V (full installation)', //26
                                         'Server Datacenter without Hyper-V (core installation)', //27
                                         'Server Standard without Hyper-V (core installation)',   //28
                                         'Server Enterprise without Hyper-V (core installation)', //29
                                         'Microsoft Hyper-V Server',                              //2A
                                         'Storage Server Express (core installation)',            //2B
                                         'Storage Server Standard (core installation)',           //2C
                                         'Storage Server Workgroup (core installation)',          //2D
                                         'Storage Server Enterprise (core installation)',         //2E
                                         'Starter N',                                             //2F
                                         'Professional',                                          //30
                                         'Professional N',                                        //31
                                         'Windows Small Business Server 2011 Essentials',         //32
                                         'Server For SB Solutions',                               //33
                                         'Server Solutions Premium',                              //34
                                         'Server Solutions Premium (core installation)',          //35
                                         'Server For SB Solutions EM',                            //36
                                         'Server For SB Solutions EM',                            //37
                                         'Windows MultiPoint Server',                             //38
                                         'Unknown',                                               //39
                                         'Unknown',                                               //3A
                                         'Windows Essential Server Solution Management',          //3B
                                         'Windows Essential Server Solution Additional',          //3C
                                         'Windows Essential Server Solution Management SVC',      //3D
                                         'Windows Essential Server Solution Additional SVC',      //3E
                                         'Small Business Server Premium (core installation)',     //3F
                                         'Server Hyper Core V',                                   //40
                                         'Unknown',                                               //41
                                         'Not supported',                                         //42
                                         'Not supported',                                         //43
                                         'Not supported',                                         //44
                                         'Not supported',                                         //45
                                         'Enterprise E',                                          //46
                                         'Not supported',                                         //47
                                         'Enterprise (evaluation)',                               //48
                                         'Unknown',                                               //49
                                         'Unknown',                                               //4A
                                         'Unknown',                                               //4B
                                         'Windows MultiPoint Server Standard (full)',             //4C
                                         'Windows MultiPoint Server Premium (full)',              //4D
                                         'Unknown',                                               //4E
                                         'Server Standard (evaluation)',                          //4F
                                         'Server Datacenter (evaluation)',                        //50
                                         'Unknown',                                               //51
                                         'Unknown',                                               //52
                                         'Unknown',                                               //53
                                         'Enterprise N (evaluation)',                             //54
                                         'Unknown',                                               //55
                                         'Unknown',                                               //56
                                         'Unknown',                                               //57
                                         'Unknown',                                               //58
                                         'Unknown',                                               //59
                                         'Unknown',                                               //5A
                                         'Unknown',                                               //5B
                                         'Unknown',                                               //5C
                                         'Unknown',                                               //5D
                                         'Unknown',                                               //5E
                                         'Storage Server Workgroup (evaluation)',                 //5F
                                         'Storage Server Standard (evaluation)',                  //60
                                         'Unknown',                                               //61
                                         'Home N',                                                //62
                                         'Home China',                                            //63
                                         'Home Single Language',                                  //64
                                         'Home',                                                  //65
                                         'Unknown',                                               //66
                                         'Professional with Media Center',                        //67
                                         'Unlicensed product',                                    //68
                                          'Unknown',                                              //69
                                         'Unknown',                                               //6A
                                         'Unknown',                                               //6B
                                         'Unknown',                                               //6C
                                         'Unknown',                                               //6D
                                         'Unknown',                                               //6E
                                         'Unknown',                                               //6F
                                         'Unknown',                                               //70
                                         'Unknown',                                               //71
                                         'Unknown',                                               //72
                                         'Unknown',                                               //73
                                         'Unknown',                                               //74
                                         'Unknown',                                               //75
                                         'Unknown',                                               //76
                                         'Unknown',                                               //77
                                         'Unknown',                                               //78
                                         'Education',                                             //79
                                         'Education N',                                           //7A
                                         'Unknown',                                               //7B
                                         'Unknown',                                               //7C
                                         'Enterprise 2015 LTSB',                                  //7D
                                         'Enterprise 2015 LTSB N',                                //7E
                                         'Unknown',                                               //7F
                                         'Unknown',                                               //80
                                         'Enterprise 2015 LTSB (evaluation)',                     //81
                                          'Unknown',                                              //82
                                         'Unknown',                                               //83
                                         'Unknown',                                               //84
                                         'Unknown',                                               //85
                                         'Unknown',                                               //86
                                         'Unknown',                                               //87
                                         'Unknown',                                               //88
                                         'Unknown',                                               //89
                                         'Unknown',                                               //8A
                                         'Unknown',                                               //8B
                                         'Unknown',                                               //8C
                                         'Unknown',                                               //8D
                                         'Unknown',                                               //8E
                                         'Unknown',                                               //8F
                                         'Unknown',                                               //90
                                         'Server Datacenter, Semi-Annual Channel (core)',         //91
                                         'Server Standard, Semi-Annual Channel (core)',           //92
                                         'Unknown',                                               //93
                                         'Unknown',                                               //94
                                         'Unknown',                                               //95
                                         'Unknown',                                               //96
                                         'Unknown',                                               //97
                                         'Unknown',                                               //98
                                         'Unknown',                                               //99
                                         'Unknown',                                               //9A
                                         'Unknown',                                               //9B
                                         'Unknown',                                               //9C
                                         'Unknown',                                               //9D
                                         'Unknown',                                               //9E
                                         'Unknown',                                               //9F
                                         'Unknown',                                               //A0
                                         'Pro for Workstations',                                  //A1
                                         'Windows 10 Pro for Workstations',                       //A2
                                         'Unknown');                                              //A3



    ProductStr: array [0..3] of String =   ('',
                                         'Home',
                                         'Professional',
                                         'Server');
    // First element: build number, second element: english, third element: french
    Windows10Build: array [0..12,0..1] of String =(('00000',    'Unknown version'),
                                              ('10240', 'v 1507 "July 2015 update"'),
                                              ('10586', 'v 1511 "November 2015 update"'),
                                              ('14393', 'v 1607 "July 2016 (Anniversary update)"'),
                                              ('15063', 'v 1703 "April 2017 (Creators update)"'),
                                              ('16299', 'v 1709 "October 2017 (Fall Creators update)"'),
                                              ('17134', 'v 1803 "April 2018 update"'),
                                              ('17763', 'v 1809 "October 2018 update"'),
                                              ('18362', 'v 1903 "May 2019 update"'),
                                              ('18363', 'v 1909 "November 2019 update"'),
                                              ('19041', 'v 2004 "May 2020 update"'),
                                              ('19042', 'v 20H2 "October 2020 update"'),
                                              ('19043', 'v 21H1 "May 2021 updsate"'));


    var
    fVerProEx: DWORD;
    // GetProductInfo doesn't exists before Vista, so we load it dynamically
    // We need to test it is assigned before use it in the unit
    GetProductInfo: function (dwOSMajorVersion, dwOSMinorVersion,
                            dwSpMajorVersion, dwSpMinorVersion: DWORD;
                            var pdwReturnedProductType: DWORD): BOOL stdcall = NIL;




     {$ENDIF}

implementation



// lang: "en", "fr" see content of .lng files
// LangFile: Tbbinifile
// Default language english

constructor TOSVersion.Create (lang: string='en'; LangFile: TBbInifile=nil);
begin
  inherited Create;
  // Initialize variables
  FOSName:='';
  fArchitecture:='';
  FKernelName:='';
  FKernelRelease:='';
  FKernelVersion:='';
  FNetworkNode:='';
  FVerTyp:=0 ;
  FVerMaj:=0;
  FVerMin:=0;
  FVerProd:='';
  FVerSup:='';
  FVerMask:=0;
  FVerBuild:=0;
  FVerDetail:='';
  {$IFDEF WINDOWS}
    Pointer(GetProductInfo) := GetProcAddress(GetModuleHandle('KERNEL32.DLL'),
                                    'GetProductInfo');
    localize(lang, LangFile);
  {$ENDIF}

  GetSysInfo;
end;

// Windows 10 strings localization  in lazbbosver.lng
// Copy the content in the application localization file: lngfile


procedure TOSVersion.localize(lang:string; LangFile: TBbInifile);
var
  i: integer;
begin
  {$IFDEF WINDOWS}
  // populate dynamic arrays for product details and versions with default values
  SetLength(ProdStr, Length(ProductStr));
  for i:= 0 to high(ProdStr) do ProdStr[i]:= ProductStr[i];
  SetLength(Win10Build, length(Windows10Build), length(Windows10Build[0]));
  for i:= 0 to high(Win10Build) do Win10Build[i,0]:= Windows10Build[i,0];
  for i:= 0 to high(Win10Build) do Win10Build[i,1]:= Windows10Build[i,1];
  if assigned (Langfile) then
  try
    for i:= 1 to high(ProdStr) do ProdStr[i]:= LangFile.ReadString(lang,ProductStr[i],ProductStr[i]);
    for i:= 0 to high(Win10Build) do Win10Build[i,1]:= LangFile.ReadString(lang,Windows10Build[i,0],Windows10Build[i,1]);
  except
  end;
  {$ENDIF}
end;

destructor TOSVersion.Destroy;
begin
  inherited;
  {$IFDEF WINDOWS}
  if Assigned(GetProductInfo) then  FreeAndNil(GetProductInfo);
  {$ENDIF}
end;


{$IFDEF WINDOWS}
function IsWin64: Boolean;
  {$IFDEF WIN32}
  type
    TIsWow64Process = function(Handle: Windows.THandle; var Res: Windows.BOOL): Windows.BOOL; stdcall;
  var
    IsWOW64: Windows.BOOL;
    IsWOW64Process: TIsWow64Process;
  {$ENDIF}
begin
  {$IFDEF WIN32}
  // Try to load required function from kernel32
  IsWOW64Process := TIsWow64Process(Windows.GetProcAddress(Windows.GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(IsWOW64Process) then
    begin
      // Function exists
      if not IsWOW64Process(Windows.GetCurrentProcess, IsWOW64) then
        Result:=False
      else
        Result:=IsWOW64;
    end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
  {$ELSE} //if were running 64bit code, OS must be 64bit !)
     Result := True;
  {$ENDIF}
end;

procedure TOSVersion.GetSysInfo;
var
  OsViEx : TOSVersionInfoEx;
  OsVi : TOSVersionInfo;
begin
  fVerProEx:= 0;
  // Free Pascal GetVersionEx function use OSVersionInfo structure instead OSVersionInfoEx
  // We call it with an OSVersionInfo variable with OSVersionInfoEx size
  OsViEx:= Default(TOSVersionInfoEx);
  OsVi.dwOSVersionInfoSize:= SizeOf(TOSVersionInfoEx);
  GetVersionEx (Osvi);
  // Then we move memory to the OSVersionInfoEx variable and we get extra items.
  Move(OsVi, OsViEx,SizeOf(TOSVersionInfoEx));
  With OsViEx do
  begin
    fVerMaj:=dwMajorVersion;
    fVerMin:=dwMinorVersion;
    fVerBuild:= dwBuildNumber and $FFFF;
    fVerSup:= StrPas(szCSDVersion);
    fPid:= dWPlatformID;
    fSrvPMaj:= wServicePackMajor;
    fSrvPMin:= wServicePackMinor;
    fVerMask:= wSuiteMask;
    fProdTyp:= wProductType;
    fReserved:= wReserved;
        // Inconnu par défaut
    fVerTyp:= High(StatStr);
    Case fPid of
    0 : fVerTyp:= 0;                                                       // Win32s
    1 : If fVerMin < 10 then
        begin
          If fVerBuild <= 1000 then fVerTyp:= 1                            // win95 4.00 build 950
          else fVerTyp:= 2;                                                // Win95-OSR2 4.00 950c
        end else
        begin
          if (fVerBuild >= 0) and (fVerBuild < 2000) then fVerTyp:= 3;     // Win98 4.10 build 1999
          if (fVerBuild >= 2000) and (fVerBuild < 3000) then fVerTyp:= 4;  // Win98 SE 4.10 build 2222
          if fVerBuild >= 3000 then fVerTyp:= 5 ;                          //Win ME 4.90 build 3000
        end;
    2: begin                                                               //VER_PLATFORM_WIN32_NT
         GetNT32Info;
       end;
    end;

  end;
  FOSName:= StatStr[High(StatStr)];
  if (fVerTyp < High(StatStr)) then FOSName:= StatStr[fVerTyp];
  try
    if fVerProEx > 0 then fVerProd:=  ProdStrEx[fVerProEx] else
    fVerProd:= ProdStr[fVerPro];
  except
    fVerProd:= ProdStr[0];
  end;
  //s:= GetEnvironmentVariable('PROCESSOR_ARCHITECTURE');
  //if s='AMD64' then s:= 'x86_64';
  if IsWin64 then
  fArchitecture:= 'x86_64' else
  fArchitecture:= 'x86';

  fVerDetail:= fOSName+' '+fVerProd+' - '+IntToStr(fVerMaj)+'.'+IntToStr(fVerMin)
                 +'.'+IntToStr(fVerBuild)+' - '+fVerSup+' - '+fArchitecture;
end;

procedure TOSVersion.GetNT32Info ;
var
  dwOSMajorVersion, dwOSMinorVersion,
  dwSpMajorVersion, dwSpMinorVersion: DWORD;
  i: integer;
begin
  dwOSMajorVersion:= 0;
  dwOSMinorVersion:= 0;
  dwSpMajorVersion:= 0;
  dwSpMinorVersion:= 0;
  case fVerMaj of
            3: fVerTyp:= 6;  //NT 3.5
            4: fVerTyp:= 7;  //NT 4
            5: begin
                 case fVerMin of
                   0: begin
                        fVerTyp:= 8; // W2000
                        case fProdTyp of
                          VER_NT_WORKSTATION: fVerPro:= 2;    // Professional
                          else fVerPro:= 3;    // Server
                        end;
                      end;
                   1: begin
                        fVerTyp:= 9; // Windows XP
                        if (fVerMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL then
                          fVerPro:= 1     //Home Edition
                        else
                          fVerPro:= 2;     //Professional
                      end;
                   2: fVerTyp:= 10; // Windows Server 2003
                 end;
               end;
            6: begin
                 if Assigned(GetProductInfo) then
                 begin
                   GetProductInfo( dwOSMajorVersion, dwOSMinorVersion,
                             dwSpMajorVersion, dwSpMinorVersion,
                             fVerProEx );
                   if fVerProEx = $ABCDABCD then fVerProEx:= High(ProdStrEx);
                 end;
                 case fVerMin of       // Windows Vista
                   0: begin
                        case fProdTyp of
                          VER_NT_WORKSTATION:
                          begin
                            fVerTyp:= 11;    // Windows Vista
                             if (fVerMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL
                            then fVerPro:= 1     //Home Edition
                            else fVerPro:= 2;     //Professional
                          end else
                          begin
                            fVerTyp:= 12; // Windows Server 2008
                          end;
                        end;
                      end;
                   1: begin
                        case fProdTyp of
                          VER_NT_WORKSTATION:
                          begin
                            fVerTyp:= 14;    // Windows 7
                            if (fVerMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL
                            then fVerPro:= 1     //Home Edition
                            else fVerPro:= 2;     //Professional
                          end else
                          begin
                            fVerTyp:= 13; // Windows Server 2008 RC2
                          end;
                        end;
                      end;
                   2: begin
                        case fProdTyp of
                          VER_NT_WORKSTATION:
                          begin
                            fVerTyp:= 15;    // Windows 8
                            if (fVerMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL
                            then fVerPro:= 1     //Home Edition
                            else fVerPro:= 2;     //Professional
                          end else
                          begin
                            fVerTyp:= 16; // Windows Server 2012
                          end;
                        end;
                      end;
                   3: begin
                       case fProdTyp of
                          VER_NT_WORKSTATION:
                          begin
                            fVerTyp:= 17;    // Windows 8.1
                            if (fVerMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL
                            then fVerPro:= 1     //Home Edition
                            else fVerPro:= 2;     //Professional
                          end else
                          begin
                            fVerTyp:= 18; // Windows 2012 Server R2
                          end;
                        end;
                      end;
                 end;     //case fVermin
               end;
          10: begin
                if Assigned(GetProductInfo) then
                 begin
                   GetProductInfo( dwOSMajorVersion, dwOSMinorVersion,
                             dwSpMajorVersion, dwSpMinorVersion,
                             fVerProEx );
                  if fVerProEx = $ABCDABCD then fVerProEx:= High(ProdStrEx);
                end;
                case fVerMin of       // Windows 10
                   0: begin
                        case fProdTyp of
                          VER_NT_WORKSTATION:
                          begin
                            fVerTyp:= 19;    // Windows 10
                            if (fVerMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL
                            then fVerPro:= 1     //Home Edition
                            else fVerPro:= 2;     //Professional
                            // Match builds to Win 10 version commercial name
                            // Build numbers are in Win10build array
                            // The array can be updated from the calling application
                            FVersup:= Win10Build[0, 1]; //'Unknown version';
                            for i:= 0 to length(Win10build)-1 do
                              if FVerBuild=StringToInt(Win10build[i,0]) then
                              begin
                                FVersup:= Win10Build[i, 1];
                                break;
                              end;
                          end else
                          begin
                            if fVerbuild < 14394 then
                            begin
                              fVerTyp:= 20; // Windows Server 2016
                            end else
                            begin
                              fVerTyp:= 21; // Windows Server 2019
                            end;
                          end;
                        end;
                      end;
                    end;
               end;
          end;            //case fVermaj
end;

// End of Windows code, begin Linux, Unix or Mac code


{$ELSE}
  procedure TOSVersion.GetSysInfo;
  var
    P: TProcess;
    Function ExecParam(Param: String): String;
        Begin
          P.Parameters[0]:= '-' + Param;
          P.Execute;
          SetLength(Result, 1000);
          SetLength(Result, P.Output.Read(Result[1], Length(Result)));
          While (Length(Result) > 0) And (Result[Length(Result)] In [#8..#13,#32]) Do
            SetLength(Result, Length(Result) - 1);
        End;
    Begin
      //Default(OSInfo);
      P:= TProcess.Create(Nil);
      P.Options:= [poWaitOnExit, poUsePipes];
      P.Executable:= 'uname';
      P.Parameters.Add('');
      fOSName:= ExecParam('o');
      fKernelName:= ExecParam('s');
      fKernelRelease:= ExecParam('r');
      fKernelVersion:= ExecParam('v');
      fNetworkNode:= ExecParam('n');
      fArchitecture:= ExecParam('m');
      P.Free;
      fVerDetail:= fOSName+' '+fKernelName+' '+fKernelVersion;
    End;
{$ENDIF}

end.

