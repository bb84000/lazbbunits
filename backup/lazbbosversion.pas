unit lazbbosversion;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
    Windows,
  {$ENDIF}
  Classes, SysUtils;

Type

  TOSInfo = record
    OSName: string;
    // Unix
    KernelName: string;
    KernelRelease: string;
    KernelVersion: string;
    NetworkNode: string;
    Architecture: string;
    // Windows
    VerTyp: Integer;       // Windows type
    VerMaj    : Integer;   // : Major version number
    VerMin    : Integer;   // Minor version number
    VerBuild  : Integer;   // Build version number
    VerProd   : String;    // Version type
    VerSup    : String;    // Additional version information
    VerMask   : Integer;   // Product suite mask;
    VerDetail: string;     //Description of the OS, with version, build etc.
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

      StatStr: array[0..21] of String = ('Microsoft Windows 32',
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
                                    'Windows 8.1',
                                    'Windows Server 2012 R2',
                                    'Microsoft Windows 10',
                                    'Windows Server 2016',
                                    'Système inconnu');

   ProdStr: array [0..3] of String = ('',
                                      'Home',
                                      'Professional',
                                      'Server');

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

    var
    fVerProEx: DWORD;
    // GetProductInfo doesn't exists before Vista, so we load it dynamically
    // We need to test it is assigned before use it in the unit
    GetProductInfo: function (dwOSMajorVersion, dwOSMinorVersion,
                            dwSpMajorVersion, dwSpMinorVersion: DWORD;
                            var pdwReturnedProductType: DWORD): BOOL stdcall = NIL;
  {$ENDIF}

  procedure GetSysInfo(var OsInfo: TOSInfo);

implementation


{$IFDEF WINDOWS}
  procedure GetSysInfo(var OsInfo: TOSInfo);
  var
    I : Integer;
    OsViEx : TOSVersionInfoEx;
    OsVi : TOSVersionInfo;
    dwOSMajorVersion, dwOSMinorVersion,
    dwSpMajorVersion, dwSpMinorVersion: DWORD;
    FPID : Integer; {platform ID}
    FVerTyp, FVerPro : Integer;
    fVerProEx: DWORD;
    FVerName, FVerDetail, FVerProd, FVerSup : String;
    FVerMaj, FVerMin, FVerBuild: Integer;
    FVerMask : Integer;
    FSrvPMaj: Word;
    fSrvPMin: Word;
    fProdTyp: BYTE;
    fReserved: BYTE;
    fVer64bit: Bool;
    FValues: TStrings;
    FValProd: TStrings;
    FValProdEx: TStrings;
    s: string;
  begin
    Pointer(GetProductInfo) := GetProcAddress(GetModuleHandle('KERNEL32.DLL'),
                                     'GetProductInfo');
    fVerProEx:= 0;
    // Free Pascal GetVersionEx function use OSVersionInfo structure instead OSVersionInfoEx
    // We call it with an OSVersionInfo variable with OSVersionInfoEx size
    OsViEx:= Default(TOSVersionInfoEx);
    dwOSMajorVersion:= 0;
    dwOSMinorVersion:= 0;
    dwSpMajorVersion:= 0;
    dwSpMinorVersion:= 0;
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
    0 : fVerTyp:= 0;                                          // Win32s
    1 : If fVerMin < 10 then
        begin
          If fVerBuild <= 1000 then fVerTyp:= 1                // win95 4.00 build 950
          else fVerTyp:= 2;                                     // Win95-OSR2 4.00 950c
        end else
        begin
          // Win98 4.10 build 1999
          if (fVerBuild >= 0) and (fVerBuild < 2000) then fVerTyp:= 3;
          // Win98 SE 4.10 build 2222
          if (fVerBuild >= 2000) and (fVerBuild < 3000) then fVerTyp:= 4;
          //Win ME 4.90 build 3000
          if fVerBuild >= 3000 then fVerTyp:= 5 ;
        end;
    2 : begin                                                 //VER_PLATFORM_WIN32_NT
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
                case fVerMin of       // Windows Vista
                   0: begin
                        case fProdTyp of
                          VER_NT_WORKSTATION:
                          begin
                            fVerTyp:= 19;    // Windows 10
                            if (fVerMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL
                            then fVerPro:= 1     //Home Edition
                            else fVerPro:= 2;     //Professional
                             case FVerBuild of
                               10240: FVerSup:= 'v 1507 "Initial Update"';
                               10586: FVerSup:= 'v 1511 "November Update"';
                               14393: FVersup:= 'v 1607 "Anniversary Update"';
                               15063: FVersup:= 'v 1703 "Creators Update"';
                               16299: FVersup:= 'v 1709 "Fall Creators Update"';
                               17134: FVersup:= 'v 1803 "April 2018 Update"';
                               17763: FVerSup:= 'v 1809 "October 2018 Update"';
                             end;
                          end else
                          begin
                            fVerTyp:= 20; // Windows Server 2016
                          end;
                        end;
                      end;
                    end;
               end;
          end;            //case fVermaj
        end;              //fPid= 2
    end;

  end;
    OSInfo:= Default(TOSInfo);
    OSInfo.OSName:= StatStr[fVerTyp];
    try
      if fVerProEx > 0 then OSInfo.VerProd:=  ProdStrEx[fVerProEx] else
      OSInfo.VerProd:= ProdStr[fVerPro];
    except
      OSInfo.VerProd:= ProdStr[0];
    end;
    s:= GetEnvironmentVariable('PROCESSOR_ARCHITECTURE');
    if s='AMD64' then s:= x86_64;
    OSInfo.Architecture:= s;
    OSInfo.VerMaj:= FVerMaj;
    OSInfo.VerTyp:=FVerTyp;
    OSInfo.VerMaj:= FVerMaj;
    OSInfo.VerMin:= FVerMin;
    OSInfo.VerBuild:= FVerBuild;
    OSInfo.VerSup:= FVerSup;
    OSInfo.VerMask:= FVerMask;
    OSInfo.VerDetail:= OSInfo.OSName+' '+OSInfo.VerProd+' - '+IntToStr(OSInfo.VerMaj)+'.'+IntToStr(OSInfo.VerMin)
                 +'.'+IntToStr(OSInfo.VerBuild)+' - '+OSInfo.VerSup+' - '+OsInfo.Architecture;
 end;
{$ELSE}
  procedure GetSysInfo(var OsInfo: TOSInfo);
  var
    OSInfo: TOSInfo;
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
      OSInfo.OSName:= ExecParam('o');
      OSInfo.KernelName:= ExecParam('s');
      OSInfo.KernelRelease:= ExecParam('r');
      OSInfo.KernelVersion:= ExecParam('v');
      OSInfo.NetworkNode:= ExecParam('n');
      OSInfo.Architecture:= ExecParam('m');
      P.Free;
      OSInfo.VerDetail:= OSInfo.OSName+' '+OSInfo.KernelName+' '+OSInfo.KernelVersion;
    End;
{$ENDIF}


end.

