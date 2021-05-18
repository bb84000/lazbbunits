{ Astro unit for calendar application                                           }
{ GetPaques(Year: Word): TDateTime; Frm Wikipedia                               }
{ Moon functions : original author alantell - november 2004 from astro books    }
{ bb: Added one cycle to avoid bug in january                                   }
{ rewritten to avoid "goto" loops                                               }
{ sunrise and sunset based on parts of code from NOAA Solar Calculator          }
{ at http://www.srrb.noaa.gov/highlights/sunrise/sunrise.html                   }
{ bb - sdtp - april 2021                                                        }

unit lazbbastro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, math, dateUtils;


type
  TMoonRecord = record
    MDays: TDateTime;
    MType: string;
  end;

  TMoonDays = array[1..56] of TMoonRecord;

  TypSunRise = (standard, civil, nautic, astro, none);

var
  MoonDays: TMoonDays;

const
  leapyear: array [1..13] of integer = (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366);
  noleapyear: array [1..13] of integer =     (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365);


  function GetPaques(Year: Word): TDateTime;
  // Moon functions
  function Get_MoonDays(gDate: TDateTime): TMoonDays; // retourne dates lune sur 52 sem
  function isMoon(gDate: TDateTime): Boolean;         // retourne vrai si NL,PQ,PL,DQ
  function Get_MoonType(gDate: TDateTime): string;    // retourne NL,PQ,PL,DQ
  // Seasons function
  function GetSeasonDate(year: word; Num: Integer): TDateTime;
  // Get the 1st of a month
  function GetBegMonth(dDate: TDateTime): word;
  // French special days
  Function GetDeportes(Year: Word): TDateTime;
  function GetFetMeres (Year: Word): TDateTime;
  // DST
  function IsDST (dDate: TDateTime): Boolean;
  // Sunrise and sunset time
  // dt: Date for sunrise and sunset
  // latitude and longitude (West negative)
  // Type sun standard, civil, nautic, astro (default: standard)
  function Sunrise(dt: TDateTime; latitude, longitude: double; TypeSun: TypSunRise=standard): TDateTime;
  function Sunset(dt: TDateTime; latitude, longitude: double; TypeSun: TypSunRise=standard): TDateTime;


implementation

const
  aSunTypDeg: array [0..4] of Double = (90.83333, 96, 102, 108, 90);

function Periodic24(t: Extended ): Extended; forward;
function calcSunrise(dt: TDateTime; latitude, longitude: double; rise: Boolean; TypeSun: TypSunRise=standard): TDateTime; forward;
function calcTimeJulianCent(jd: double): double; forward;
function calcJDFromJulianCent(t: double): double; forward;
function calcEquationOfTime(t: double): double; forward;
function calcObliquityCorrection(t: double):double; forward;
function calcGeomMeanLongSun(t: double): double; forward;
function calcEccentricityEarthOrbit(t: double): double; forward;
function calcGeomMeanAnomalySun(t: double): double; forward;
function calcMeanObliquityOfEcliptic(t: double): double; forward;
function calcSunDeclination(t: double): double; forward;
function calcSunApparentLong(t: double): double; forward;
function calcSunTrueLong(t: double): double; forward;
function calcSunEqOfCenter(t: double): double; forward;
function calcHourAngleSunrise(lat, solarDec: double; TypeSun: TypSunRise=standard): double; forward;
function calcSolNoonUTC(t, longitude: double): double; forward;

Function GetPaques(Year: Word): TDateTime;     // Wikipedia
var
  nMonth, nDay, nMoon, nEpact, nSunday, nGold, nCent, nCorx, nCorz: Integer;
begin
  nGold := (Year mod 19) + 1;    // The Golden Number of the year in the 19 year Metonic Cycle
  nCent := (Year div 100) + 1;   // Calculate the Century
  { Number of years in which leap year was dropped in order... }
  { to keep in step with the sun: }
  nCorx := (3 * nCent) div 4 - 12;
  nCorz := (8 * nCent + 5) div 25 - 5; // Special correction to syncronize Easter with moon's orbit
  nSunday := (Longint(5) * Year) div 4 - nCorx - 10;  // Find Sunday
  { ^ To prevent overflow at year 6554}
  { Set Epact - specifies occurrence of full moon: }
  nEpact := (11 * nGold + 20 + nCorz - nCorx) mod 30;
  if nEpact < 0 then
    nEpact := nEpact + 30;
  if ((nEpact = 25) and (nGold > 11)) or (nEpact = 24) then nEpact := nEpact + 1;
  { Find Full Moon: }
  nMoon := 44 - nEpact;
  if nMoon < 21 then
    nMoon := nMoon + 30;
  { Advance to Sunday: }
  nMoon := nMoon + 7 - ((nSunday + nMoon) mod 7);
  if nMoon > 31 then
  begin
    nMonth := 4;
    nDay   := nMoon - 31;
  end
  else
  begin
    nMonth := 3;
    nDay   := nMoon;
  end;
  try
    result := EncodeDate(Year, nMonth, nDay);
  except
    result:= 0;
  end;
end;

function Get_MoonDays(gDate: TDateTime): TMoonDays;
// const du jour julien du 01-01-2000 à 12h TU
// ou 0h GMT 01-01-2000 12:00:00 TU 2451545 JJ
// const JJ2000 = 2451545;
var
  TUJrLun: TDateTime; K, TT: integer; TDatL, TxtL, THorL: string;
  AnDecim, T, T2, T3, Rd, AMSol, AMLun, LatLune, PhMoyL: double;
  NoLune, tLune, J, gLunes, PhLun, CurM, CurM2, HrLun, MnLun: byte;
  AnPh, MoPh, AJour, LunAnW, LunMsW, JrLunEntW: word;
  CptLMax, CptL, PentPhL, PentPhMoyL, PfracPhMoyL, Alpha, B, C, D, E: single;
  LunAn, LunMs, JrLun, JrLunFrac, TotHeu, TotMin, TotSec: single;
  ListDatLun: array[1..56] of string;
  //ListHeuLun: array[1..56] of string;
  gNbrLune: array[1..56] of byte;
  AnBis,  Found : boolean;

begin
  DecodeDate(gDate, AnPh, MoPh, AJour);
  gLunes:= 0;
  AJour:= 1;  // AJour = 1 pour éviter AJour + 3 > 31
  if MoPh = 0 then begin MoPh:= 12; AnPh:= AnPh - 1; end;
  CptLMax:= 14; // définit le nb phase de lune ex: 13 lunes => 56 phases
  // valeur année décimale par mois si année bissextile ou pas
  AnBis:= ((AnPh Mod 4)= 0);    // si = 0 année bissextile
  if AnBis then begin
      case MoPh of
       1: AnDecim:= 4.24375935815675E-02;
       2: AnDecim:= 0.124574871481376;
       3: AnDecim:= 0.20534319474952;
       4: AnDecim:= 0.288849427280992;
       5: AnDecim:= 0.372355659812463;
       6: AnDecim:= 0.455861892343935;
       7: AnDecim:= 0.539368124875406;
       8: AnDecim:= 0.624243312038541;
       9: AnDecim:= 0.707749544570013;
      10: AnDecim:= 0.791255777101484;
      11: AnDecim:= 0.874762009632956;
      12: AnDecim:= 0.958268242164428;
      end;
    end
  else begin
    case MoPh of
       1: AnDecim:= 4.24375935815675E-02;
       2: AnDecim:= 0.123205916849712;
       3: AnDecim:= 0.203974240117857;
       4: AnDecim:= 0.287480472649328;
       5: AnDecim:= 0.3709867051808;
       6: AnDecim:= 0.454492937712271;
       7: AnDecim:= 0.537999170243743;
       8: AnDecim:= 0.622874357406878;
       9: AnDecim:= 0.706380589938349;
      10: AnDecim:= 0.789886822469821;
      11: AnDecim:= 0.873393055001292;
      12: AnDecim:= 0.956899287532764;
    end;
  end;
  // calcul nb de lunaison CptL nb de lunes en 1 an 12.3685 => 365.25 / 29.53058
  // nombre de lunaisons depuis le 1/1/1900
  CptL:= Trunc(((AnPh + AnDecim) - 1900) * 12.3685);
  CptLMax:= CptL + CptLMax;

//CalculDesPh:
  while  (CptL < CptLMax) do
  begin
    T:= CptL / 1236.85; T2:= T*T; T3:= T*T*T; Rd:= PI / 180;
    // anomalie moyenne du soleil : AMSol
    AMSol:= 359.2242 + (29.10535608 * CptL) - (0.0000333 * T2) - (0.00000347 * T3);
    if AMSol > 360 then AMSol:= frac(AMSol/360) * 360; // intervalle 0-360°
    // anomalie moyenne de la lune : AMLun
    AMLun:= 306.0253 + (385.81691806 * CptL) + (0.0107306 * T2) + (0.00001236 * T3);
    if AMLun > 360 then AMLun:= frac(AMLun/360) * 360; // intervalle 0-360°
    // Latitude de la lune
    LatLune:= 21.2964 + (390.67050646 * CptL)-(0.0016528 * T2)-(0.00000239 * T3);
    if LatLune > 360 then LatLune:= frac(LatLune/360) * 360; // intervalle 0-360°
    // Phase moyenne de la Lune 2415020.75933 1er jour julien 1/1/-4711
    PhMoyL:= 2415020.75933 + (29.53058868 * CptL) + (0.0001178*T2)-(0.000000155*T3);
    PhMoyL:= PhMoyL + (0.00033*Sin((Rd*166.56) + (Rd*132.87)*T)-((Rd*0.009173*T2)));
    // degrés en radian
    AMSol:= AMSol * Rd;
    AMLun:= AMLun * Rd;
    LatLune:= LatLune * Rd;
    // correction de la phase vraie pour nouvelle et pleine lune
    if (frac(CptL) = 0.0) Or (frac(CptL) = 0.5) Or (frac(CptL) = -0.5) then
    begin
      PhMoyL:= PhMoyL + ((0.1734 - 0.000393 * T) * Sin(AMSol));
      PhMoyL:= PhMoyL + (0.0021 * Sin(2 * AMSol));
      PhMoyL:= PhMoyL - (0.4068 * Sin(AMLun));
      PhMoyL:= PhMoyL + (0.0161 * Sin(2 * AMLun));
      PhMoyL:= PhMoyL - (0.0004 * Sin(3 * AMLun));
      PhMoyL:= PhMoyL + (0.0104 * Sin(2 * LatLune));
      PhMoyL:= PhMoyL - (0.0051 * Sin(AMSol + AMLun));
      PhMoyL:= PhMoyL - (0.0074 * Sin(AMSol - AMLun));
      PhMoyL:= PhMoyL + (0.0004 * Sin((2 * LatLune) + AMSol));
      PhMoyL:= PhMoyL - (0.0004 * Sin((2 * LatLune) - AMSol));
      PhMoyL:= PhMoyL - (0.0006000001 * Sin((2 * LatLune) + AMLun));
      PhMoyL:= PhMoyL + (0.001 * Sin((2 * LatLune) - AMLun));
      PhMoyL:= PhMoyL + 0.0005 * Sin(AMSol + (2 * AMLun));
    end
    else begin
      // correction de la phase vraie pour premier et dernier quartier lune
      PhMoyL:= PhMoyL + (0.1721 - 0.0004 * T) * Sin(AMSol);
      PhMoyL:= PhMoyL + 0.0021 * Sin(2 * AMSol);
      PhMoyL:= PhMoyL - 0.628  * Sin(AMLun);
      PhMoyL:= PhMoyL + 0.0089 * Sin(2 * AMLun);
      PhMoyL:= PhMoyL - 0.0004 * Sin(3 * AMLun);
      PhMoyL:= PhMoyL + 0.0079 * Sin(2 * LatLune);
      PhMoyL:= PhMoyL - 0.0119 * Sin(AMSol + AMLun);
      PhMoyL:= PhMoyL - 0.0047 * Sin(AMSol - AMLun);
      PhMoyL:= PhMoyL + 0.0003 * Sin(2 * LatLune + AMSol);
      PhMoyL:= PhMoyL - 0.0004 * Sin(2 * LatLune - AMSol);
      PhMoyL:= PhMoyL - 0.0006000001 * Sin(2 * LatLune + AMLun);
      PhMoyL:= PhMoyL + 0.0021 * Sin(2 * LatLune - AMLun);
      PhMoyL:= PhMoyL + 0.0003 * Sin(AMSol + 2 * AMLun);
      PhMoyL:= PhMoyL + 0.0004 * Sin(AMSol - 2 * AMLun);
      PhMoyL:= PhMoyL - 0.0003 * Sin(2 * AMSol - AMLun);
      // ajustement suivant le quartier
      if (CptL >= 0)  then
      begin
        if (frac(CptL) = 0.25) then PhMoyL:= PhMoyL + 0.0028 - 0.0004 * Cos(AMSol) + 0.0003 * Cos(AMLun);     //1er quartier
        if (frac(CptL) = 0.75) then PhMoyL:= PhMoyL - 0.0028 + 0.0004 * Cos(AMSol) - 0.0003 * Cos(AMLun);     // dernier quartier
      end else
      begin
        if (frac(CptL) = -0.25) then PhMoyL:= PhMoyL - 0.0028 + 0.0004 * Cos(AMSol) - 0.0003 * Cos(AMLun);
        if (frac(CptL) = -0.75) then PhMoyL:= PhMoyL + 0.0028 - 0.0004 * Cos(AMSol) + 0.0003 * Cos(AMLun);
      end;
    end;
    // calcul des dates de lune calendrier
    PhMoyL:= PhMoyL + 0.5;
    PentPhMoyL:= Trunc(PhMoyL);
    PfracPhMoyL:= frac(PhMoyL);

    if PentPhMoyL <  2299161 then PentPhL:= PentPhMoyL
    else
    begin
      ALPHA:= Trunc((PentPhMoyL - 1867216.25) / 36524.25);
      PentPhL:= PentPhMoyL + 1 + ALPHA - Trunc(ALPHA / 4);
    end;
    B:= PentPhL + 1524;
    C:= Trunc((B - 122.1) / 365.25);
    D:= Trunc(365.25 * C);
    E:= Trunc((B - D) / 30.6001);
    JrLun:= B - D - Trunc(30.6001 * E) + PfracPhMoyL;
    LunMs:= 1; // initialisation
    if E < 13.5 then LunMs:= E - 1;
    if E > 13.5 then LunMs:= E - 13;
    if LunMs > 2.5 then LunAn:= C - 4716;
    if LunMs < 2.5 then LunAn:= C - 4715;
    LunAnW:= Trunc(LunAn);
    LunMsW:= Trunc(LunMs);
    JrLunEntW:= Trunc(JrLun);
    JrLunFrac:= frac(JrLun);
    TotSec:= JrLunFrac * 86400;
    TotHeu:= (TotSec / 3600);
    HrLun:= Trunc(TotHeu);
    TotMin:= frac(TotHeu) * 60;
    MnLun:= Trunc(TotMin);
    // horaire de la lune
    TUJrLun:= EncodeTime(HrLun, MnLun, 0, 0);
    //THorL:= FormatDateTime('hh"h "mm',TUJrLun);
    PhLun:= 0;
    if CptL >= 0 then
    begin
      if frac(CptL) =  0.0   then PhLun:= 6;// NL
      if frac(CptL) =  0.125 then PhLun:= 5;
      if frac(CptL) =  0.25  then PhLun:= 4;// DQ
      if frac(CptL) =  0.375 then PhLun:= 3;
      if frac(CptL) =  0.5   then PhLun:= 2;// PL
      if frac(CptL) =  0.625 then PhLun:= 1;
      if frac(CptL) =  0.75  then PhLun:= 8;// PQ
      if frac(CptL) =  0.875 then PhLun:= 7;
    end
    else begin
      if frac(CptL) = -0.875 then PhLun:= 7;
      if frac(CptL) = -0.75  then PhLun:= 8;// PQ
      if frac(CptL) = -0.625 then PhLun:= 1;
      if frac(CptL) = -0.5   then PhLun:= 2;// PL
      if frac(CptL) = -0.375 then PhLun:= 3;
      if frac(CptL) = -0.25  then PhLun:= 4;// DQ
      if frac(CptL) = -0.125 then PhLun:= 5;
      if frac(CptL) =  0.0   then PhLun:= 6;// NL
    end;
    TT:= PhLun;
    try
      EncodeDate(LunAnW,LunMsW,JrLunEntW); // jour de lune
    except
      MessageDlg('pb1' + inttostr(LunAnW) + inttostr(LunMsW) + inttostr(JrLunEntW),
                mtInformation,[mbOk], 0);
    end;
    try
      EncodeDate(LunAnW,LunMsW,JrLunEntW); // jour de lune
    except
      MessageDlg('pb2' + inttostr(AnPh) +  inttostr(MoPh) + inttostr(AJour),
                mtInformation,[mbOk], 0);
    end;
    // CurM2
    // Phase de lune paire  (pl, pq, dq ou nl)
    if (TT <> 0) and ((TT div 2) = (TT / 2)) then
    begin
      if (EncodeDate(LunAnW,LunMsW,JrLunEntW) <= EncodeDate(AnPh,MoPh,AJour + 3))
         and (PhLun <> 0) then CurM2:= PhLun;
    end;
    // CurM
    if (EncodeDate(LunAnW,LunMsW,JrLunEntW) <= EncodeDate(AnPh,MoPh,AJour + 1))
        and (PhLun <> 0 ) then CurM:= PhLun;
    Found:= False;
    TDatL:= DateToStr(EncodeDate(LunAnW,LunMsW,JrLunEntW)); // jour de lune
    NoLune:= PhLun;
    case NoLune of
      1: tLune:= 36;
      2: tLune:= 35; // PL
      3: tLune:= 42;
      4: tLune:= 41; // PQ
      5: tLune:= 40;
      6: tLune:= 39; // NL
      7: tLune:= 38;
      8: tLune:= 37; // DQ
    end;
    // enregistrement des jours de lune
    for J:= gLunes downTo 1 do
      if ListDatLun[J] = TDatL then
      begin
        Found:= True;
        //Exit;
      end;
    // End for
    if not Found then
    begin
      gLunes:= gLunes + 1;
      ListDatLun[gLunes]:= TDatL;
      //ListHeuLun[gLunes]:= THorL;
      MoonDays[gLunes].MDays:= EncodeDate(LunAnW,LunMsW,JrLunEntW)+EncodeTime(HrLun,MnLun,0,0);// date de lune
      //MoonDays[gLunes].MTime:= EncodeTime(HrLun,MnLun,0,0);        // horaire de lune
      gNbrLune[gLunes]:= tLune;
    end; // else exit;
    CptL:= CptL + 0.250;
  end;
    for J:= 1 To 56 do
    begin
      case gNbrLune[J] of
        35: TxtL:= 'PL';   //36:   '1
        37: TxtL:= 'DQ';   //38:   '3
        39: TxtL:= 'NL';   //40:   '5
        41: TxtL:= 'PQ';   //42:   '7
      end;
      MoonDays[J].MType:= TxtL; // type de lune
    end;
  //end;
  result:= MoonDays;
end;

// GetMoonday doit avoir été lancé !!!

function isMoon(gDate: TDateTime): Boolean;
var L: integer;
begin
  result:= false;
  for L:= 1 to 56 do
    if gDate = Trunc (MoonDays[L].MDays) then result:= true;
end;

function Get_MoonType (gDate: TDateTime): string;
var L: integer;
begin
  for L:= 1 to 56 do
    if gDate = Trunc(MoonDays[L].MDays) then result:= MoonDays[L].MType;
end;


// Meeus Astronmical Algorithms Chapter 27

function GetSeasonDate(year: word; Num: Integer): TDateTime;
var
  dDate: TDateTime;
  jdeo, yr, t, w, dl, s, julDay: Extended;
  deltaT: Extended;
  scl: Extended;
begin
  // Caclul initial du jour julien
  yr:=(Year-2000)/1000;
    Case num of
      0: jdeo:= 2451623.80984 + 365242.37404*yr + 0.05169*power(yr,2) - 0.00411*power(yr,3) - 0.00057*power(yr,4);
      1: jdeo:= 2451716.56767 + 365241.62603*yr + 0.00325*power(yr,2) + 0.00888*power(yr,3) - 0.00030*power(yr,4);
      2: jdeo:= 2451810.21715 + 365242.01767*yr - 0.11575*power(yr,2) + 0.00337*power(yr,3) + 0.00078*power(yr,4);
      3: jdeo:= 2451900.05952 + 365242.74049*yr - 0.06223*power(yr,2) - 0.00823*power(yr,3) + 0.00032*power(yr,4);
    else
      jdeo:= 0;
    end;
    t:= (jdeo - 2451545.0)/36525;
    w:= (35999.373*t) - 2.47;
    dl:= 1 + 0.0334*cos(DegToRad(w))  + 0.0007*cos(DegToRad(2*w));
    // Correction périodique
    s:= Periodic24(t);
    julDay:= jdeo + ( (0.00001*s) / dL ); 	// This is the answer in Julian Emphemeris Days
    // écart entre UTC et DTD en secondes entre les années from Meeus Astronmical Algroithms Chapter 10
    scl:= (Year - 2000) / 100;
    deltaT:= 102 + 102*scl + 25.3*power(scl,2);
    // Special correction to avoid discontinurity in 2000
    if (Year >=2000) and (Year <=2100) then deltaT:= deltaT+ 0.37 * ( Year - 2100 );
    // Ecart en jour fractionnaire
    deltaT:= deltaT/86400;
    // On y est ! Conversion en date réelle
    dDate:= julDay-deltaT-693594-1721425+0.5;  //DateDelta= 693594 + 1721425-0,5;
    Result:= dDate;
end;


function Periodic24(t: Extended ): Extended;
const
  A: array[0..23] of integer = (485,203,199,182,156,136,77,74,70,58,52,50,45,44,29,18,17,16,14,12,12,12,9,8);
  B: array[0..23] of real = (324.96,337.23,342.08,27.85,73.14,171.52,222.54,296.72,243.58,119.81,297.17,21.02,
		     247.54,325.15,60.93,155.12,288.79,198.04,199.76,95.39,287.11,320.81,227.73,15.45);
  C: array[0..23] of real = (1934.136,32964.467,20.186,445267.112,45036.886,22518.443,
			     65928.934,3034.906,9037.513,33718.147,150.678,2281.226,
                             29929.562,31555.956,4443.417,67555.328,4562.452,62894.029,
			     31436.921,14577.848,31931.756,34777.259,1222.114,16859.074);
var
  i: Integer;
begin
  result:= 0;
  for i:= 0 to 23 do
    //result:= result +  A[i]*degCOS(;
    result:= result +  A[i]*cos(DegToRad(B[i] + (C[i]*T)));
end;



// Jour de début du mois

function GetBegMonth(dDate: TDateTime): word;
var
  CurrYear, CurMonth, CurDay: word;
begin
  DecodeDate(dDate, CurrYear, CurMonth, CurDay);
  if IsLeapYear(CurrYear) then
    Result := leapyear[CurMonth]
  else
    Result := noleapyear[CurMonth];
end;

Function GetDeportes(Year: Word): TDateTime;
var
  d: TDateTime;
begin
  // dernier dimanche d'avril
  d:= EncodeDate(Year, 4, 30);           // On prend la fin du mois
  result:= d- DayOfWeek(d)+1;            // Et on retire les jours nécessaires
end;

function GetFetMeres (Year: Word): TDateTime;
var
  d: TDateTime;
begin
  // Dernier dimanche de mai
    d:= EncodeDate(Year, 5, 31);           // On prend la fin du mois
    result:= d- DayOfWeek(d)+1;            // Et on retire les jours nécessaires
    if result = GetPaques (Year)+49        // Tombe en même temps que la pentecôte ?
    then result:= result+7;                // Alors une semaine plus tard
end;




function GetDebHeureEte(Year: Word): TDateTime;
var
 DebHeureEte: TDateTime;
begin
  DebHeureEte:= EncodeDate(Year, 3, 31);   //31 Mars
  Result:= DebHeureEte-DayOfWeek(DebHeureEte)+1;      // on cherche le dimanche
end;

function GetDebHeureHiver(Year: Word): TDateTime;
var
  DebHeureHiver: TDateTime;
begin
  DebHeureHiver:= EncodeDate(Year, 10, 31);   //31 octobre
  Result:=  DebHeureHiver-DayOfWeek(DebHeureHiver)+1;
end;

// Est-on en heure d'été ?

function IsDST (dDate: TDateTime): Boolean;
var
  Year, Month, Day: Word;
begin
  Result:= True;
  DecodeDate(dDate, Year, Month, Day);
  If (dDate < GetDebHeureEte(Year)) or (dDate >= GetDebHeureHiver(Year)) then result:= False;
end;


//***********************************************************************/
// Sunrise, Sunset : solar functions
//   dt  :  day
//   latitude : latitude of observer in degrees
//   longitude : longitude of observer in degrees
// rise = true sunrise else sunset
// Return value: time
//***********************************************************************/

function Sunrise(dt: TDateTime; latitude, longitude: double; TypeSun: TypSunRise=standard): TDateTime;
begin
  Result:= calcSunrise(dt, latitude, longitude, True, TypeSun);
end;

function Sunset(dt: TDateTime; latitude, longitude: double; TypeSun: TypSunRise=standard): TDateTime;
begin
  Result:= calcSunrise(dt, latitude, longitude, False, TypeSun);
end;

//***********************************************************************/
// calcSunrise  : calculate the Universal Coordinated Time (UTC) of sunrise/sunset
//   dt  :  day
//   latitude : latitude of observer in degrees
//   longitude : longitude of observer in degrees
// rise = true sunrise else sunset
// Return value: time
//***********************************************************************/

function calcSunrise(dt: TDateTime; latitude, longitude: double; rise: Boolean; TypeSun: TypSunRise=standard): TDateTime;
var
  r: integer;
  JD, t: double;
  noonmin, tnoon, eqTime, solarDec, hourAngle: double;
  delta, timeDiff,timeUTC, newt : double;
  hr, mn, y, m, d : Integer;
begin
  // check if sunrise or sunset
  if rise then r:= 1 else r:= -1;
  // longitudes are inversed (east negative)
  longitude:= -1*longitude;
  JD:= DateTimeToJulianDate(dt);
  t:= calcTimeJulianCent(JD);
  noonmin:= calcSolNoonUTC(t, longitude);
  // *** Find the time of solar noon at the location, and use
  //     that declination. This is better than start of the
  //     Julian day
  tnoon:= calcTimeJulianCent (JD+noonmin/1440.0);
  // *** First pass to approximate sunrise or sunset (using solar noon)
  eqTime:= calcEquationOfTime(tnoon);
  solarDec:= calcSunDeclination(tnoon);
  hourAngle:= calcHourAngleSunrise(latitude, solarDec, TypeSun)*r;
  delta:= longitude - radToDeg(hourAngle);
  timeDiff:= 4 * delta;	// in minutes of time
  timeUTC:= 720 + timeDiff - eqTime;	// in minutes
  // *** Second pass includes fractional jday in gamma calc
  newt:= calcTimeJulianCent(calcJDFromJulianCent(t) + timeUTC/1440.0);
  eqTime:= calcEquationOfTime(newt);
  solarDec:= calcSunDeclination(newt);
  hourAngle:= calcHourAngleSunrise(latitude, solarDec, TypeSun)*r;
  delta:= longitude - radToDeg(hourAngle);
  timeDiff:= 4 * delta;
  timeUTC:= 720 + timeDiff - eqTime; // in minutes
  hr:= floor(timeUTC /60);
  mn:= floor(timeUTC- hr*60);
  y:= YearOf(dt);
  m:= MonthOf(dt);
  d:= DayOfTheMonth(dt);
  result:= EncodeDateTime(y,m,d,hr,mn,0,0);
end;

//***********************************************************************
// calcSolNoonUTC
//   calculate the Universal Coordinated Time (UTC) of solar
//		noon for the given day at the given location on earth
//   t : number of Julian centuries since J2000.0
//   longitude : longitude of observer in degrees
// Return value: time in minutes from zero Z
//***********************************************************************/

function calcSolNoonUTC(t, longitude: double): double;
var
  tnoon, eqTime, solNoonUTC, newt: double;
begin
  //First pass uses approximate solar noon to calculate eqtime
  tnoon:= calcTimeJulianCent(calcJDFromJulianCent(t) + longitude/360.0);
  eqTime:= calcEquationOfTime(tnoon);
  solNoonUTC:= 720 + (longitude * 4) - eqTime; // min
  newt:= calcTimeJulianCent(calcJDFromJulianCent(t) -0.5 + solNoonUTC/1440.0);
  eqTime:= calcEquationOfTime(newt);
  // var solarNoonDec = calcSunDeclination(newt);
  solNoonUTC:= 720 + (longitude * 4) - eqTime; // min
  result:= solNoonUTC;
end;

//***********************************************************************/
//* Name:    calcTimeJulianCent
//* Type:    Function
//* Purpose: convert Julian Day to centuries since J2000.0.
//* Arguments:
//*   jd : the Julian Day to convert
//* Return value:
//*   the T value corresponding to the Julian Day
//***********************************************************************/

function calcTimeJulianCent(jd: double): double;
begin
  result:=  (jd-2451545.0)/36525.0;
end;

//***********************************************************************/
//* Name:    calcJDFromJulianCent
//* Type:    Function
//* Purpose: convert centuries since J2000.0 to Julian Day.
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   the Julian Day corresponding to the t value
//***********************************************************************/

function calcJDFromJulianCent(t: double): double;
var
  JD: double;
begin
  JD:= (t*36525.0) + 2451545.0;
result:= JD;
end;

//***********************************************************************/
//* Name:    calcEquationOfTime
//* Type:    Function
//* Purpose: calculate the difference between true solar time and mean solar time
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   equation of time in minutes of time
//***********************************************************************/

function calcEquationOfTime(t: double): double;
var
 epsilon, l0, e, m, y : double;
 sin2l0, sinm, cos2l0, sin4l0, sin2m : double;
 Etime: Double;
begin
  epsilon:= calcObliquityCorrection(t);
  l0:= calcGeomMeanLongSun(t);
  e:= calcEccentricityEarthOrbit(t);
  m:= calcGeomMeanAnomalySun(t);
  y:= sqr(tan(degToRad(epsilon)/2.0));
  sin2l0:= sin(2.0 * degToRad(l0));
  sinm:= sin(degToRad(m));
  cos2l0:= cos(2.0 * degToRad(l0));
  sin4l0:= sin(4.0 * degToRad(l0));
  sin2m:= sin(2.0 * degToRad(m));
  Etime:= y * sin2l0 - 2.0 * e * sinm + 4.0 * e * y * sinm * cos2l0 - 0.5 * y * y * sin4l0 - 1.25 * e * e * sin2m;
  result:=  radToDeg(Etime)*4.0;	// in minutes of time
end;


//***********************************************************************/
//* Name:    calcObliquityCorrection
//* Type:    Function
//* Purpose: calculate the corrected obliquity of the ecliptic
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   corrected obliquity in degrees
//***********************************************************************/

function calcObliquityCorrection(t: double):double;
var
 e0, omega, e: double;
begin
  e0:= calcMeanObliquityOfEcliptic(t);
  omega:= 125.04 - 1934.136 * t;
  e:= e0 + 0.00256 * cos(degToRad(omega));
  result:= e;		// in degrees
end;

//***********************************************************************/
//* Name:    calcMeanObliquityOfEcliptic
//* Type:    Function
//* Purpose: calculate the mean obliquity of the ecliptic
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   mean obliquity in degrees
//***********************************************************************/

function calcMeanObliquityOfEcliptic(t: double): double;
var
  seconds, e0 : double;
begin
  seconds:= 21.448 - t*(46.8150 + t*(0.00059 - t*(0.001813)));
  e0:= 23.0 + (26.0 + (seconds/60.0))/60.0;
  result:= e0;    //in degrees
end;


//***********************************************************************/
//* Name:    calGeomMeanLongSun
//* Type:    Function
//* Purpose: calculate the Geometric Mean Longitude of the Sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   the Geometric Mean Longitude of the Sun in degrees
//***********************************************************************/

function calcGeomMeanLongSun(t: double): double;
var
  L0: double;
begin
  L0:= 280.46646 + t * (36000.76983 + 0.0003032 * t);
  while(L0 > 360.0) do L0:= L0 - 360.0;
  while(L0 < 0.0) do L0:= L0 + 360.0;
  result:= L0; // in degrees
end;

//***********************************************************************/
//* Name:    calcEccentricityEarthOrbit
//* Type:    Function
//* Purpose: calculate the eccentricity of earth's orbit
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   the unitless eccentricity
//***********************************************************************/

function calcEccentricityEarthOrbit(t: double): double;
var
  e: double;
begin
  e:= 0.016708634 - t * (0.000042037 + 0.0000001267 * t);
  result:= e;		// unitless
end;

//***********************************************************************/
//* Name:    calGeomAnomalySun
//* Type:    Function
//* Purpose: calculate the Geometric Mean Anomaly of the Sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   the Geometric Mean Anomaly of the Sun in degrees
//***********************************************************************/

function calcGeomMeanAnomalySun(t: double): double;
var
  M: double;
begin
  M:= 357.52911 + t * (35999.05029 - 0.0001537 * t);
  result:= M;		// in degrees
end;

//***********************************************************************/
//* Name:    calcSunDeclination
//* Type:    Function
//* Purpose: calculate the declination of the sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   sun's declination in degrees
//***********************************************************************/

function calcSunDeclination(t: double): double;
var
  e, lambda, sint, theta: double;
begin
  e:= calcObliquityCorrection(t);
  lambda:= calcSunApparentLong(t);
  sint:= sin(degToRad(e)) * sin(degToRad(lambda));
  theta:= radToDeg(arcsin(sint));
  result:= theta;		// in degrees
end;

//***********************************************************************/
//* Name:    calcSunApparentLong
//* Type:    Function
//* Purpose: calculate the apparent longitude of the sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   sun's apparent longitude in degrees
//***********************************************************************/

function calcSunApparentLong(t: double): double;
var
  o, omega, lambda: double;
begin
  o:= calcSunTrueLong(t);
  omega:= 125.04 - 1934.136 * t;
  lambda:= o - 0.00569 - 0.00478 * sin(degToRad(omega));
result:= lambda;		// in degrees
end;

//***********************************************************************/
//* Name:    calcSunTrueLong
//* Type:    Function
//* Purpose: calculate the true longitude of the sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   sun's true longitude in degrees
//***********************************************************************/

function calcSunTrueLong(t: double): double;
var
  l0, c, O: double;
begin
  l0:= calcGeomMeanLongSun(t);
  c:= calcSunEqOfCenter(t);
  O:= l0 + c;
  result:= O;		// in degrees
end;

//***********************************************************************/
//* Name:    calcSunEqOfCenter
//* Type:    Function
//* Purpose: calculate the equation of center for the sun
//* Arguments:
//*   t : number of Julian centuries since J2000.0
//* Return value:
//*   in degrees
//***********************************************************************/

function calcSunEqOfCenter(t: double): double;
var
  m, mrad, sinm, sin2m, sin3m, C : double;
begin
  m:= calcGeomMeanAnomalySun(t);
  mrad:= degToRad(m);
  sinm:= sin(mrad);
  sin2m:= sin(mrad+mrad);
  sin3m:= sin(mrad+mrad+mrad);
  C:= sinm * (1.914602 - t * (0.004817 + 0.000014 * t)) + sin2m * (0.019993 - 0.000101 * t) + sin3m * 0.000289;
  Result:= C;		// in degrees
end;

//***********************************************************************/
//* Name:    calcHourAngleSunrise
//* Purpose: calculate the hour angle of the sun at sunrise for the latitude
//* Arguments:
//*   lat : latitude of observer in degrees
//*   solarDec : declination angle of sun in degrees
//*   TypSun : standard, civil, nautic, astro (values are in aSunTypDeg array)
//* Return value:
//*   hour angle of sunrise in radians
//***********************************************************************/

function calcHourAngleSunrise(lat, solarDec: double; TypeSun: TypSunRise=standard): double;
var
  latRad, sdRad, HA: double;
  ZenithDistance: Double;
begin
  try
    ZenithDistance:= aSunTypDeg[Ord(TypeSun)];
  except
    ZenithDistance:= 90.83333;
  end;
  latRad:= degToRad(lat);
  sdRad:= degToRad(solarDec);
  HA:= (arccos(cos(degToRad(ZenithDistance))/(cos(latRad)*cos(sdRad))-tan(latRad) * tan(sdRad)));
  Result:= HA;		// in radians
end;




end.

