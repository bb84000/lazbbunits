{*********************************************************************************}
{ Unité lazbbutils : cross platform utilities                                     }
{ bb - sdtp - october 2019                                                        }
{ Syntax just before the function implementation                                  }
{*********************************************************************************}

unit lazbbutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls, FileInfo,
  lclintf, LazUTF8, math,  base64, Process;

type

  TCharSet = (UTF8BOM,UTF16BEBOM,UTF16LEBOM,UTF32BEBOM,UTF32LEBOM,SCSUBOM,UTF7BOM,UTFEBCDICBOM,BOCU1BOM,UTF1BOM,UTF8NOBOM,UTF16BENOBOM,UTF16LENOBOM,ANSI,UNK);

  TVersionInfo = record
   Comments: string;
   CompanyName: string;
   FileDescription: String;
   FileVersion: string;
   InternalName: string;
   LegalCopyright: string;
   LegalTrademarks: string;
   OriginalFilename: string;
   ProductName: string;
   ProductVersion: string;
 end;

  function IsAnsi2Utf8(st: string): string;
  function IsUtf82Ansi(st: string): string;
  function MsgDlg(const Capt, Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; Captions: ARRAY OF string; HelpCtx: Longint=0; Pos: TPosition=poMainFormCenter): Integer;
  function GetVersionInfo(): TVersionInfo;
  function Str2Date (s, format: String): TDateTime;
  function TrimFileExt(FileName: String): String;
  function FormatS(const Fmt: String; const Args: array of Const): String;
  function Charset(s: string): TCharset;
  function RPos (Substr: string; S: string): Integer;
  procedure ImageFitToSize(var img: TImage; imgw, imgh: integer);
  procedure CropBitmap(InBitmap, OutBitMap : TBitmap; Enabled: Boolean);//X, Y, W, H :Integer);
  procedure ResPngToGlyph(Instance: THandle; ResName: string; glyph: Tbitmap);
  function TranslateHttpErrorMsg(ErrMsg: string; HttpErrMsgs: array of string):string;
  function BoolToString(b: boolean): string;
  function StringToBool(s: string): boolean;
  function StringToFloat(s: String; DecSepar: char='.' ): Float;
  function FloatToString(f: Float; DecSepar: char='.'): string;
  function StringToTimeDate(s: string; fmt: string=''): TDateTime;
  function TimeDateToString(datim: TDateTime; fmt: string=''): string;
  function StringToInt(s: String): Int64;
  function FindSeparator(s: string): Char ;
  function StringEncrypt(S: String; Key: DWord): String;
  function StringDecrypt(S: String; Key: DWord): String;
  function DarkColor(col : TColor) : Boolean;
  procedure Execute(exec: string; param: TStrings);

  const
  SRCCOPY = $00CC0020;
  Charsets: array [0..14] of TCharSet = (UTF8BOM,UTF16BEBOM,UTF16LEBOM,UTF32BEBOM,UTF32LEBOM,SCSUBOM,UTF7BOM,UTFEBCDICBOM,BOCU1BOM,UTF1BOM,UTF8NOBOM,UTF16BENOBOM,UTF16LENOBOM,ANSI,UNK);


  CharSetArray : array[0..14] of String = ('UTF-8 with BOM','UTF-16 Big Endian vith BOM','UTF-16 Little Endian with BOM','UTF-32 Big Endian with BOM',
                  'UTF-32 Little Endian with BOM','SCSU with BOM','UTF-7 with BOM','UTF EBCDIC with BOM','BOCU-1 with BOM',
                  'UTF-1 with BOM','UTF-8 without BOM','UTF-16 Big Endian without BOM','UTF-16 Little Endian without BOM',
                  'ANSI','Unknown');
  CharsetBom : array [0..9] of String = (#$EF#$BB#$BF,#$FE#$FF,#$FF#$FE,#00#00#$FE#$FF,#$FF#$FE#00#00,#$0E#$FE#$FF,#$2B#$2F#$76,
                                         #$DD#$73#$66#$73,#$FB#$EE#$28,#$F7#$64#$4C);

  UTF8= UTF8NOBOM;

  // encryption values
  C1 = 34419;
  C2 = 23602;

implementation


// Convert Ansi string to UTF8 string if string is ansi, do nothing if string is already UTF8

function IsAnsi2Utf8(st: string):string;
var
  i: integer;
  b1, b2: byte;
begin
  result:= st;
  // if length (byte) equal to UTF8 char number
  // Then we are in ANSI
  if Length(st) = UTF8Length(st) then
  begin
    for i := Length(Result) downto 1 do
      if Result[i] >= #127 then
      begin
        b1 := $C0 or (ord(Result[i]) shr 6);
        b2 := $80 or (ord(Result[i]) and $3F);
        Result[i] := chr(b1);
        Insert(chr(b2), Result, i + 1);
      end;
  end;
end;

// Convert UTF8 string to Ansi string if string is UTF8, do nothing if string is already Ansi

function IsUtf82Ansi(st: string): string;
var
  i: integer;
  b1, b2: byte;
begin
  result:= st;
  // if length (byte) greater to UTF8 char number
  // Then we are in UTF8
  if Length(st) > UTF8Length(st) then
  begin
    i := 1;
    while i <= Length(Result) do
    begin
      if (ord(Result[i]) and $80) <> 0 then
      begin
        b1 := ord(Result[i]);
        b2 := ord(Result[i + 1]);
        // C'est un C quelque chose, toujour suivi d'un char sans bit 6
        if ((b1 and $F0)=$C0) and ((b2 and $40)<>$40)  then
        begin
          Result[i] := Chr((b1 shl 6) or (b2 and $3F));
          Delete(Result, i + 1, 1);
        end;
      end;
      inc(i);
    end;
  end;
end;

// Customized message box
// Derived from CreateMessageDialog, with extra parameters :
// Capt: Title bar
// Msg: text to display
// DlgType: See Dialogs unit
// Captions: array of buttons captions
// HelpCtx : Help context, default 0= none
// Pos : Position on desktop, default main form center
// Result: Modal result

function MsgDlg(const Capt, Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Captions: ARRAY OF string; HelpCtx: Longint=0; Pos: TPosition=poMainFormCenter	): Integer;
var
  aMsgdlg: TForm;
  i: Integer;
  Dlgbutton: TBitBtn;
  Captionindex: Integer;
begin
  aMsgdlg:= CreateMessageDialog(Msg, DlgType, Buttons);
  try
    aMsgdlg.Caption:= Capt;
    aMsgdlg.HelpContext := HelpCtx;
    aMsgdlg.Position:= Pos;
    Case DlgType of
      mtWarning: Beep; 
      mtError  : Beep; 
    end;
    CaptionIndex:= 0;
    for i := 0 to aMsgdlg.componentcount - 1 Do
    begin
    if (aMsgdlg.components[i] is TBitBtn) then
    Begin
      Dlgbutton := TBitBtn(aMsgdlg.components[i]);
      if Captionindex <= High(Captions)  then
      begin
        Dlgbutton.Caption := Captions[Captionindex];
         inc(Captionindex);
      end;
    end;
  end;
    Result := aMsgdlg.ShowModal;
  finally
    FreeAndNil(aMsgdlg);
  end;
end;

// Retrieve infos embedded in version resource

function GetVersionInfo(): TVersionInfo;
var
 FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:= TFileVersionInfo.Create(nil);
  with FileVerInfo do
  try
    ReadFileInfo;
    result.CompanyName:= VersionStrings.Values['CompanyName'];
    result.FileDescription:= VersionStrings.Values['FileDescription'];
    result.FileVersion:= VersionStrings.Values['InternalName'];
    result.LegalCopyright:= VersionStrings.Values['LegalCopyright'];
    result.LegalTrademarks:= VersionStrings.Values['LegalTrademarks'];;
    result.OriginalFilename:= VersionStrings.Values['OriginalFilename'];
    result.ProductName:= VersionStrings.Values['ProductName'];
    result.ProductVersion:= VersionStrings.Values['ProductVersion'];
    result.InternalName:= VersionStrings.Values['InternalName'];
    result.Comments:= VersionStrings.Values['Comments'];
  except
  end;
  FreeAndNil(FileVerInfo);
end;

// StrToDate with temproary format string ( dd/MM/yyyy

function Str2Date(s, Format : String) : TDateTime;
var
  sDateFmt: String;
begin
  sDateFmt:=  DefaultFormatSettings.ShortDateFormat;
  DefaultFormatSettings.ShortDateFormat:= Format;
  Result:= 0;
  try
    Result:= StrToDate(s);
  except

  end;
  DefaultFormatSettings.ShortDateFormat:= sDateFmt;
end;

// Trim file extension
function TrimFileExt(FileName: String): String;
var
  p: Integer;
begin
  result:= FileName;
  p:= Pos(ExtractFileExt(FileName), FileName);
  if p > 0 then result:= Copy(FileName, 1, p-1) else result:='';
end;

// modified format function to trap exception

function FormatS(const Fmt: String; const Args: array of Const): String;
begin
  try
    result:= Format(Fmt, Args);
  except
    result:= Fmt+' Format Error';
  end;
end;

//Detect charset
function Charset(s: string): TCharset;
var
  bom: string;
  i : integer;
  ss: TStringStream;
begin
   // if length (byte) equal to UTF8 char number
  // Then we are in ANSI
  if Length(s) = UTF8Length(s) then
  begin
    result:= ANSI;
    exit;
  end;
  result:= UTF8NOBOM;
  ss:= TStringStream.Create(s);
  SetLength(bom,4);
  ss.ReadBuffer(Pointer(bom)^, 4);

  // search charset in BOM list
  for i:= 0 to length(CharsetBom)-1 do
    if pos(CharsetBom[i], bom)=1 then
    begin
      result:= Charsets[i];
      break;
    end;
    // Not found BOM
    if result= UTF8NOBOM then
    begin
      // search zeros if unicode 16
      for i:= 0 to 10 do
      begin
        if ss.ReadByte = 0 then
        begin
          if (i mod 2)=0 then
          begin
            result:= UTF16BENOBOM;
          end else
          begin
            result:= UTF16LENOBOM;
          end;
          break;
        end;
      end;

  end;
  ss.free;
end;

{ Fonction de recherche de sous-chaîne à partir de la fin}
{ Syntaxe et résultat identique à celuis de la focntion Pos}

Function RPos (Substr: string; S: string): Integer;
var
  RSub, RS : String;
  I : Integer;
begin
  SetLength(RSub, Length(Substr));
  SetLength(RS, Length(S));
  For I:= 0 to Length(Substr)-1 do
  begin
    Rsub[I+1]:= Substr[Length(SubStr)-I];
  end;
  For I:= 0 to Length(S)-1 do
  begin
    RS[I+1]:= S[Length(S)-I];
  end;
  I:= Pos(RSub, RS);
  If I = 0 then Result:= 0
  else Result:= Length(S)-I+1-Length(Substr)+1;
end;

// resize image in fixed size, respect aspect ratio
// Img : Image to resize
// imgw, imgh : new size

procedure ImageFitToSize(var img: TImage; imgw, imgh: integer);
var
  bmp:TBitmap;
  //w, h: integer;
  sar, tar : double;
begin
   bmp := TBitmap.Create;
   sar:= Img.Width/Img.Height;
   tar:= imgw/imgh;
   if(sar >= tar) then  //source is wider than target in proportion
   begin
     bmp.width:= imgw;
     bmp.Height:= round(imgw/sar);
   end else
   begin
     bmp.Height:= imgh;
     bmp.width:= round(imgh*sar)
   end;
   bmp.Canvas.StretchDraw(rect(0,0,bmp.width,bmp.height), Img.Picture.Graphic);
   Img.Picture.Assign(bmp);
   bmp.free;
end;

// Crop speedbutton images to popup menu images
// Enabled image or disabled image
procedure CropBitmap(InBitmap, OutBitMap : TBitmap; Enabled: Boolean);//X, Y, W, H :Integer);
begin
  OutBitMap.PixelFormat := InBitmap.PixelFormat;
  OutBitmap.Width:= InBitMap.Height;  // as we can have double width or not in sbuttons
  OutBitmap.Height:= InBitMap.Height;
  // First or second image
  if Enabled then BitBlt(OutBitMap.Canvas.Handle, 0, 0, OutBitmap.Width, OutBitmap.Height, InBitmap.Canvas.Handle, 0, 0, SRCCOPY)
  else BitBlt(OutBitMap.Canvas.Handle, 0, 0, OutBitmap.Width, OutBitmap.Height, InBitmap.Canvas.Handle, OutBitmap.Height, 0, SRCCOPY);
end;

// Load png from resource in Bitmap (for instance buttons glyphs. Use RCDATA in RC file
// Instance: Handle of the instance of the application, usually HInstance
// Resname: resource name (must be defined as RCDATA resource
// Glyph: TBitmap variable, i.e. Speedbutton.glyph

procedure ResPngToGlyph(Instance: THandle; ResName: string; glyph: Tbitmap);
var
    png: TPortableNetworkGraphic;
begin
  Png:= TPortableNetworkGraphic.Create;
  try
    Png.LoadFromResourceName(Instance,resname);
    glyph.Assign (Png);
  finally
    Png.Free;
  end;
end;

// Translation HTTP and Socket error messages
// HttpErrMsgs : array of translated messages done by application
// If there are variables, extract them to use in the translated messages
// Use a minimum part of the original message to to avoid waste of time

function TranslateHttpErrorMsg(ErrMsg: string; HttpErrMsgs: array of string):string;
var
  A: TStringarray;
  n: integer;
begin
  result:= ErrMsg;
  if length(ErrMsg) > 0 then
  try
    // HttpClient errors
    //SErrInvalidProtocol='Invalid protocol : "%s"';
    if Pos('Invalid protocol :', ErrMsg)=1 then
    begin
      A:= ErrMsg.split('"');
      if length(HttpErrMsgs[0])>0 then result:= Format(HttpErrMsgs[0], [A[1]]);
      exit;
    end;
    // SErrReadingSocket = 'Error reading data from socket';
    if Pos('Error', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[1])>0 then result:= HttpErrMsgs[1];
      exit;
    end;
    // SErrInvalidProtocolVersion = 'Invalid protocol version in response: "%s"';
    if Pos('Invalid protocol version', ErrMsg)=1 then
    begin
      A:= ErrMsg.split('"');
      if length(HttpErrMsgs[2])>0 then result:= Format(HttpErrMsgs[2], [A[1]]);
      exit;
    end;
    // SErrInvalidStatusCode = 'Invalid response status code: %s';
    if Pos('Invalid response', ErrMsg)=1 then
    begin
    if length(HttpErrMsgs[3])>0 then result:= Format(HttpErrMsgs[3], [copy(ErrMsg, 30)]);
      exit;
    end;
    // SErrUnexpectedResponse = 'Unexpected response status code: %d';
    // As %d is converted in message,replace with %s
    if Pos('Unexpected', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[4])>0 then result:= Format(HttpErrMsgs[4], [copy(ErrMsg, 33)]);
      exit;
    end;
    // SErrChunkTooBig = 'Chunk too big';
    if Pos('Chunk too', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[5])>0 then result:= HttpErrMsgs[5];
      exit;
    end;
    // SErrChunkLineEndMissing = 'Chunk line end missing';
    if Pos('Chunk line', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[6])>0 then result:= HttpErrMsgs[6];
      exit;
    end;
    // SErrMaxRedirectsReached = 'Maximum allowed redirects reached : %d';
    // As %d is converted in message,replace with %s
    if Pos('Maximum', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[7])>0 then result:= Format(HttpErrMsgs[7], [copy(ErrMsg, 37)]);
      exit;
    end;
    // Socket errors
    // strHostNotFound = 'Host name resolution for "%s" failed.';
    if Pos('Host', ErrMsg)=1 then
    begin
      A:= ErrMsg.split('"');
      if length(HttpErrMsgs[8])>0 then result:= Format(HttpErrMsgs[8], [A[1]]);
      exit;
    end;
    // strSocketCreationFailed = 'Creation of socket failed: %s';
    if Pos('Creation', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[9])>0 then result:= Format(HttpErrMsgs[9], [Copy(ErrMsg, 28)]);
      exit;
    end;
    // strSocketBindFailed = 'Binding of socket failed: %s';
    if Pos('Binding', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[10])>0 then result:= Format(HttpErrMsgs[10], [Copy(ErrMsg, 27)]);
      exit;
    end;
    // strSocketListenFailed = 'Listening on port #%d failed, error: %d';
    // As %d is converted in message,replace with %s
    if Pos('Listening', ErrMsg)=1 then
    begin
      n:= Pos('failed', ErrMsg);
      if length(HttpErrMsgs[11])>0 then result:= Format(HttpErrMsgs[11], [Copy(ErrMsg, 20, n-21), Copy(ErrMsg, n+13)]);
      exit;
    end;
    // strSocketConnectFailed = 'Connect to %s failed.';
    if Pos('Connect', ErrMsg)=1 then
    begin
      n:= Pos('failed', ErrMsg);
      if length(HttpErrMsgs[12])>0 then result:= Format(HttpErrMsgs[12],
                     [Copy(ErrMsg, 12, n-13)]);
      exit;
    end;
    // strSocketAcceptFailed = 'Could not accept a client connection on socket: %d, error %d';
    // As %d is converted in message,replace with %s ';
    if Pos('Could', ErrMsg)=1 then
    begin
      n:= pos('error', ErrMsg);
      if length(HttpErrMsgs[13])>0 then result:= Format(HttpErrMsgs[13],
                      [Copy(ErrMsg, 49, n-51), copy(ErrMsg, n+6)]);
      exit;
    end;
    // strSocketAcceptWouldBlock = 'Accept would block on socket: %d';
    // As %d is converted in message,replace with %s ';
    if Pos('Accept', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[14])>0 then result:= Format(HttpErrMsgs[14], [Copy(ErrMsg, 31)]);
      exit;
    end;
    // strSocketIOTimeOut = 'Failed to set IO Timeout to %d';
    // Do not use initial string :  'Failed to set IO Timeout to %d';
    if Pos('Failed', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[15])>0 then result:= Format(HttpErrMsgs[15], [copy(ErrMsg, 29)]);
      exit;
    end;
    // strErrNoStream = 'Socket stream not assigned';
    if Pos('Socket', ErrMsg)=1 then
    begin
      if length(HttpErrMsgs[16])>0 then result:= HttpErrMsgs[16];
      exit;
    end;
  except
  end;
end;

function BoolToString(b: boolean): string;
begin
  if b then result:= 'true' else result:= 'false';
end;

function StringToBool(s: string): Boolean;
begin
  if uppercase(s)='TRUE' then result:= true else result:= false;
end;

// String to float, default decimal separator is dot '.'

function StringToFloat(s: String; DecSepar: char='.' ): Float;
var
  fmtsetting: TFormatSettings;
begin
  fmtsetting.DecimalSeparator:= DecSepar;
  try
    result:= StrToFloat(s, fmtsetting);
  except
    result:= 0;
  end;
end;

// Float to string, default decimal separator is dot '.'

function FloatToString(f: Float; DecSepar: char='.' ): string;
var
  fmtsetting: TFormatSettings;
begin
  fmtsetting.DecimalSeparator:= DecSepar;
  try
    result:= FloatToStr(f, fmtsetting);
  except
    result:= '';
  end;
end;


// Find separator in date or time format strings
// As format string conains only letters and separators, if not a letter, it is separator
// Searching on Uppercase simplify the process

function FindSeparator(s: string): Char ;
var
  i: integer;
begin
  result:= Chr(0);
  s:= uppercase(s);
  for i:= 1 to length(s) do
    if (Ord(s[i])<64) or (Ord(s[i])>90) then // It is not a letter likely separator
    begin
      result:= s[i];
      break;
    end;
end;

// String to date time, return now on error
// fmt string ie : 'yyyy/mm/dd hh:nn:ss'

function StringToTimeDate(s: string; fmt: string=''): TDateTime;
var
  A: TStringArray;
  fmtsetting: TFormatSettings;
begin
  try
    if length(fmt)=0 then result:= StrToDateTime(s)
    else
    begin
      A:= fmt.split(' ');
      fmtsetting.ShortDateFormat := A[0];
      // search date separator
      fmtsetting.DateSeparator:= FindSeparator(A[0]);
      // search time separator
      fmtsetting.TimeSeparator:= FindSeparator(A[1]);
      fmtsetting.LongTimeFormat:= A[1];
      result:= StrToDateTime(s, fmtsetting);
    end;
  except
    result:=now();
  end;
end;

//DateTime to string, see above

function TimeDateToString(datim: TDateTime; fmt: string=''): string;
var
  A: TStringArray;
  fmtsetting: TFormatSettings;
begin
  if length(fmt)=0 then result:= DateTimeTostr(datim)
  else
  begin
    A:= fmt.split(' ');
    fmtsetting.shortDateFormat := A[0];
    // search date separator
    fmtsetting.DateSeparator:= FindSeparator(A[0]);
    // search time separator
    fmtsetting.TimeSeparator:= FindSeparator(A[1]);
    fmtsetting.LongTimeFormat:= A[1];
    result:= DateTimeTostr(datim, fmtsetting);
  end;
end;

// String to integer 64, return 0 on error
function StringToInt(s: String): Int64;
begin
  try
    result:= StrToInt64(s);
  except
    result:= 0;
  end;
end;

// String encryption and decryption
//   C1 = 34419; C2 = 23602;
function StringEncrypt(S: String; Key: DWord): String;
var
  I: byte;
begin
  SetLength(Result,Length(S));
  for I := 1 to Length(S) do begin
     Result[I] := char(byte(S[I]) xor (Key shr 8));
    Key := (byte(Result[I]) + Key) * C1 + C2;
  end;
  Result:= EncodeStringBase64(Result);
end;

function StringDecrypt(S: String; Key: DWord): String;
var
  I: byte;
begin
  if length (s) = 0 then
  begin
    result:= '';
    exit;
  end;
  s:= DecodeStringBase64(s);
  SetLength(Result,Length(S));
  for I := 1 to Length(S) do begin
    Result[I] := char(byte(S[I]) xor (Key shr 8));
    Key := (byte(S[I]) + Key) * C1 + C2;
  end;
end;

// color is it dark or light ?

function DarkColor(col : TColor) : Boolean;
var
  r: integer;
begin
  Col:= ColorToRGB(Col);
  r:= (30*Red(Col)+59*Green(Col)+11*Blue(Col)) div 255;
  Result:= r < 50;
end;

procedure Execute(exec: string; param: TStrings);
var
  Process: TProcess;
  i: integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.InheritHandles := False;
    Process.Options := [];
    Process.ShowWindow := swoShow;
    // Copy default environment variables including DISPLAY variable for GUI application to work
    for I := 1 to GetEnvironmentVariableCount do
      Process.Environment.Add(GetEnvironmentString(I));
    Process.Executable := exec;
    Process.Parameters.Assign (param);
    Process.Execute;
  finally
    Process.Free;
  end;
end;

end.
