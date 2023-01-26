//******************************************************************************
// lazbbinifiles : Read/write ini files with UTF16/UTF8 w or W/O BOM compatibility
// sdtp - bb - september 2022
//   Same syntax as TiniFiles
//   Added Text property, ini file text  (UTF-8)
//   Added Charset property string
//   Added Create from resource and create from stream
//******************************************************************************

unit lazbbinifiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, LazUTF8, LConvEncoding, Dialogs, lazbbutils;


type
 // TCharSet = (UTF8BOM,UTF16BEBOM,UTF16LEBOM,UTF32BEBOM,UTF32LEBOM,SCSUBOM,UTF7BOM,UTFEBCDICBOM,BOCU1BOM,UTF1BOM,UTF8NOBOM,UTF16BENOBOM,UTF16LENOBOM,ANSI,UNK);

  TBbInifile = Class(TIniFile)
      fchrset: TCharSet;
      fcharset: string;
      fchange: boolean;
    private
      inifile: TIniFile;
      MS: TMemoryStream;
      bfilename: string;
      function GetStream: TStream;
      function GetText: String;
      procedure DetectCharset(var BS: TMemoryStream);
      procedure ApplyCharset;
    public
      constructor Create(const AFileName: string; AOptions : TIniFileoptions = []); overload; override;
      constructor Create(AStream: TStream; AOptions : TIniFileoptions = []); overload;
      constructor Create(Instance: TFPResourceHMODULE; ResName: String; AOptions : TIniFileOptions = []);
      destructor destroy; override;
      function ReadString(const Section, Ident, Default: string): string; override;
      procedure WriteString(const Section, Ident, Value: String); override;
      procedure ReadSection(const Section: string; Strings: TStrings); override;
      procedure ReadSectionRaw(const Section: string; Strings: TStrings);


      procedure ReadSections(Strings: TStrings); override;
      procedure ReadSectionValues(const Section: string; Strings: TStrings; AOptions : TSectionValuesOptions = [svoIncludeInvalid]); overload; override;
      procedure EraseSection(const Section: string); override;
      procedure DeleteKey(const Section, Ident: String); override;
      procedure UpdateFile; override;
      property Stream: TStream read GetStream;
      property Text: string read GetText;
      property CharSet: string read fcharset;
  end;

  const
  Charsets: array [0..14] of TCharSet = (UTF8BOM,UTF16BEBOM,UTF16LEBOM,UTF32BEBOM,UTF32LEBOM,SCSUBOM,UTF7BOM,UTFEBCDICBOM,BOCU1BOM,UTF1BOM,UTF8NOBOM,UTF16BENOBOM,UTF16LENOBOM,ANSI,UNK);
  RT_RCDATA = MAKEINTRESOURCE(10);

  CharSetArray : array[0..14] of String = ('UTF-8 with BOM','UTF-16 Big Endian vith BOM','UTF-16 Little Endian with BOM','UTF-32 Big Endian with BOM',
                  'UTF-32 Little Endian with BOM','SCSU with BOM','UTF-7 with BOM','UTF EBCDIC with BOM','BOCU-1 with BOM',
                  'UTF-1 with BOM','UTF-8 without BOM','UTF-16 Big Endian without BOM','UTF-16 Little Endian without BOM',
                  'ANSI','Unknown');
  CharsetBom : array [0..9] of String = (#$EF#$BB#$BF,#$FE#$FF,#$FF#$FE,#00#00#$FE#$FF,#$FF#$FE#00#00,#$0E#$FE#$FF,#$2B#$2F#$76,
                                         #$DD#$73#$66#$73,#$FB#$EE#$28,#$F7#$64#$4C);
implementation

procedure TBbIniFile.DetectCharset(var BS: TMemoryStream);
var
  bom: string;
  i : integer;
begin
 fchrset:= UTF8NOBOM;
 if BS.Size = 0 then exit else
 begin
    BS.Position := 0;
    SetLength(bom,4);
    BS.ReadBuffer(Pointer(bom)^, 4);
    BS.Position := 0;
    // search charset in BOM list
    for i:= 0 to length(CharsetBom)-1 do
      if pos(CharsetBom[i], bom)=1 then
      begin
        fchrset:= Charsets[i];
        break;
      end;
    // Not found BOM
    if fchrset= UTF8NOBOM then
    begin
      // search zeros if unicode 16
      for i:= 0 to 10 do
      begin
        //if MS.ReadByte = 0 then
        if BS.ReadByte = 0 then
        begin
          if (i mod 2)=0 then
          begin
            fChrset:= UTF16BENOBOM;
          end else
          begin
            fChrset:= UTF16LENOBOM;
          end;
          break;
        end;
      end;
  end;

  end;
  fcharset:= CharSetArray[ord(fchrset)];
end;

procedure TBbIniFile.ApplyCharset;
var
  S: String;
begin
 try
   MS.Position := 0;
   Case fchrset of
      UTF16BEBOM, UTF16LEBOM : // Unicode with BOM
        begin;
          S:= UTF16ToUTF8(PWideChar(MS.Memory), MS.Size div SizeOf(WideChar));
          S:= UTF8BOMToUTF8(S);
          MS.Clear;
          MS.WriteBuffer(Pointer(S)^, length(S));
        end;
      UTF8BOM:
        begin
          SetLength(S, MS.Size);
          MS.ReadBuffer(Pointer(S)^, MS.Size);
          S:= UTF8BOMToUTF8(S);
          MS.Clear;
          MS.WriteBuffer(Pointer(S)^, length(S));
        end;
      UTF16BENOBOM, UTF16LENOBOM:
        begin
          S := UTF16ToUTF8(PWideChar(MS.Memory), MS.Size div SizeOf(WideChar));
          MS.Clear;
          MS.WriteBuffer(Pointer(S)^, length(S));
        end;
     // else likely UTF-8 w/o BOM
   end;
 finally
 end;
end;

constructor TBbIniFile.Create(const AFileName: string; AOptions : TIniFileOptions = []);
begin
 fchange:= false;
 bfilename:= AFileName;
 MS:= TMemoryStream.Create;
 fchrset:= UTF8NOBOM;              // Default UTF-8 w/o BOM
 if FileExists(bfilename) then
 try
   MS.LoadFromFile(bfilename);
   DetectCharset(MS);
   ApplyCharset;
 except
 end;
 MS.Position:= 0;
 IniFile:= TIniFile.Create(MS, AOptions);
end;

constructor TBbIniFile.Create(AStream: TStream; AOptions : TIniFileoptions = []); overload;
begin
  fchange:= false;
  MS:= TMemoryStream.Create;
  try
    MS.CopyFrom(AStream, AStream.Size);
    DetectCharset(MS);
    ApplyCharset;
  except
  end;
  MS.Position:= 0;
  IniFile:= TIniFile.Create(MS, AOptions);
end;

constructor TBbIniFile.Create(Instance: TFPResourceHMODULE; ResName: String; AOptions : TIniFileOptions = []); overload;
var
  RS: TResourceStream;
begin
  fchange:= false;
  MS:= TMemoryStream.Create;
  try
    RS:= TResourceStream.Create(Instance, ResName, RT_RCDATA );
    MS.CopyFrom(RS, RS.Size);
    DetectCharset(MS);
    ApplyCharset;
    RS.Free;
  except
  end;
  MS.Position:= 0;
  IniFile:= TIniFile.Create(MS, AOptions);
end;

function TBbIniFile.ReadString(const Section, Ident, Default: string): string;
begin
   result:= IniFile.ReadString(Section, Ident, Default);

end;


procedure TBbIniFile.WriteString(const Section, Ident, Value: String);
begin
  IniFile.WriteString(Section, Ident, Value);
  fchange:= true;
end;

procedure TBbIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
  IniFile.ReadSection(Section, Strings);
end;

procedure TBbIniFile.ReadSectionRaw(const Section: string; Strings: TStrings);
begin
  IniFile.ReadSectionRaw(Section, Strings);
end;

procedure TBbIniFile.ReadSections(Strings: TStrings);
begin
  IniFile.ReadSections(Strings);
end;

procedure TBbIniFile.ReadSectionValues(const Section: string; Strings: TStrings; AOptions : TSectionValuesOptions = [svoIncludeInvalid]);
begin
 IniFile.ReadSectionValues(Section, Strings, AOptions);
end;

procedure TBbIniFile.EraseSection(const Section: string);
begin
  IniFile.EraseSection(Section);
  fchange:= true;
end;

procedure TBbIniFile.DeleteKey(const Section, Ident: String);
begin
  IniFile.DeleteKey(Section, Ident);
  fchange:= true;
end;

procedure TBbIniFile.UpdateFile;
begin
  IniFile.UpdateFile;
  fchange:= true;
end;

function TBbIniFile.GetStream: TStream;
begin
  result:= inifile.Stream;
end;

function TBbIniFile.GetText: String;
begin
 SetLength(result, MS.size);
 MS.Position:= 0;
 MS.ReadBuffer(Pointer(result)^, MS.size);
end;

destructor TBbIniFile.destroy;
var
  S: String;
  WS: WideString;
  bom: string;
begin
  try
    IniFile.destroy;
    Case fchrset of
      UTF16BEBOM, UTF16LEBOM : // Unicode with BOM
        begin
          SetLength(S, MS.Size);
          MS.Position := 0;
          MS.ReadBuffer(Pointer(S)^, MS.Size);
          ShowMessage(S);
          MS.Clear;
          WS:= UTF8ToUTF16(S);
          bom:= CharsetBom [ord(fchrset)];
          MS.WriteBuffer(Pointer(bom)^, length(bom));
          MS.WriteBuffer(Pointer(WS)^, length(WS)*2);
        end;
       UTF8BOM:
         begin                              // UTF6 with BOM
          SetLength(S, MS.Size);
          MS.Position := 0;
          MS.ReadBuffer(Pointer(S)^, MS.Size);
          MS.Clear;
          bom:= CharsetBom [ord(fchrset)];
          MS.WriteBuffer(Pointer(bom)^, length(bom));
          MS.WriteBuffer(Pointer(S)^, length(S));
        end;
       UTF16BENOBOM, UTF16LENOBOM:
         begin                              // unicode w/o BOM
           SetLength(S, MS.Size);
           MS.Position := 0;
           MS.ReadBuffer(Pointer(S)^, MS.Size);
           MS.Clear;
           WS:= UTF8ToUTF16(S);
           MS.WriteBuffer(Pointer(WS)^, length(WS)*2);
         end;
       // else, UTF-8 or Ansi save the stream w/o change
    end;
    if fchange then MS.SaveToFile(bfilename);
  except
    // Eat exception. Compatible to D7 behaviour, see comments to bug 19046
  end;
  MS.Free;
  inherited destroy;
end;


end.

