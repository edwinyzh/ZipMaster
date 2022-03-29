(* **************************************************************** *)
(* SFX for DelZip v1.8 *)
(* Copyright 1997, Microchip Systems / Carl Bunton *)
(* e-mail: Twojags@cris.com *)
(* Web-page: http://www.concentric.net/~twojags *)
(* *)
(* This code is not for redistribution in whole or in part.  It *)
(* may be used in compiled program format only. *)
(* *)
(* modified by Markus Stephany *)
(* modified by Russell Peters, Roger Aelbrecht
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License (licence.txt) for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

  contact: problems AT delphizip DOT org
  updates: http://www.delphizip.org

  modified 30-Jan-2008
  --------------------------------------------------------------------------- *)

unit ZMSFXInflate;

{
  INFLATE method

}

interface

uses ZMSFXDefs, Windows;
{$DEFINE PKZIP_BUG_WORKAROUND}
procedure Inflate(mem: pointer; size: Integer);

implementation

uses ZMSFXVars, ZMSFXProcs, ZMSFXStructs;

var
  hufts: WORD;
  bb: LONGINT; { bit buffer (Static) }
  bk: WORD; { bits in bit buffer (Static) }
  InBuf: BufPtr;
  Slide: BufPtr;
  InPTR: WORD; { Index FOR ZipFile input buffer }
  ZipCount: Cardinal; { Count OF bytes in ZipFile input buffer }
  InfMem: boolean;

  WP: WORD; { Static Global }
  fixed_tl, fixed_td: PT; { Static Global }

function GetNextByte: Integer;
var
  i: Integer;
begin
  { EndFile := false; }
  if InPTR >= ZipCount then
  begin
    if not InfMem then
      ZipCount := FRead(InBuf^, Min(VInt_BytesToGo, WSIZE));

    if ((VRec_ZipHeader.Flag and 1) = 1) and (ZipCount > 0) then
    begin
      for i := 0 to ZipCount - 1 do
      begin
        InBuf[i] := InBuf[i] xor decrypt_byte;
        UpdateKeys(BYTE(InBuf[i]));
      end;
    end;
    InPTR := 0;
  end;

  if ZipCount = 0 then
  begin
    { EndFile := TRUE; }
    VInt_BytesToGo := 0;
    InPTR := 0;
    GetNextByte := -1;
  end
  else
  begin
    GetNextByte := InBuf^[InPTR];
    Inc(InPTR);
  end;
end;
(* -------------------------------------------------------------------------- *)

function Get_Compressed: Integer;
begin
  (* Unshrink & UnReduce & Explode & Inflate *)

  if VInt_BytesToGo <= 0 then
    Result := -1
  else
  begin
    Result := GetNextByte;
    Dec(VInt_BytesToGo);
  end;
end;
(* -------------------------------------------------------------------------- *)

procedure NEEDBITS(n: WORD; var b: LONGINT; var k: WORD);
var
  c: Integer;
begin
  while (k < n) do
  begin
    c := Get_Compressed;
    b := b or LONGINT(c) shl k; { no parens!! }
    Inc(k, 8);
  end;
end;
(* -------------------------------------------------------------------------- *)

procedure DUMPBITS(n: WORD; var b: LONGINT; var k: WORD);
begin
  b := b shr n;
  k := k - n;
end;
(* -------------------------------------------------------------------------- *)

{ "decompress" an inflated type 0 (stored) block. }

procedure inflate_stored;
var
  n: WORD; { number OF bytes in block }
  w: WORD; { current window position }
  b: LONGINT; { bit buffer }
  k: WORD; { number OF bits in bit buffer }
begin

  { make local copies OF globals }
  b := bb; { initialize bit buffer }
  k := bk;
  w := WP; { initialize window position }

  { go TO BYTE boundary }
  n := k and 7;
  DUMPBITS(n, b, k);

  { get the length AND its complement }
  NEEDBITS(16, b, k);
  n := (WORD(b) and $FFFF);
  DUMPBITS(16, b, k);
  NEEDBITS(16, b, k);

  if (n <> (not WORD(b)) and $FFFF) then
  begin
    WP := 0;

    (* ***********  REM'D RAISE  ************* *)
    // raise E_RAISE.Create(LoadStr(E_BADBLOCK));
  end;

  DUMPBITS(16, b, k);

  { read AND output the compressed data }
  while (n <> 0) do
  begin
    Dec(n);
    NEEDBITS(8, b, k);
    Slide^[w] := WORD(b);
    Inc(w);

    if (w = WORD(WSIZE)) then
    begin
      CheckFWrite(VH_OutFile, Slide^, w, VStr_OutFile);

      crc32_buf(PByte(Slide), w, VDW_CRC32Val);
      w := 0;
    end;
    DUMPBITS(8, b, k);
  end;

  { restore the globals from the locals }
  WP := w; { restore global window pointer }
  bb := b; { restore global bit buffer }
  bk := k;
end;
(* -------------------------------------------------------------------------- *)

function huft_free(t: PT): Integer;
{ Free the malloc'ed tables built by huft_build(), which makes a linked
  list OF the tables it made, with the links in a dummy first entry OF
  each table. }
var
  p, q: PT;
begin
  { Go through linked list, freeing from the malloced (t[-1]) address. }
  p := t;
  { p :=t.next; }
  while (p <> nil) do
  begin
    Dec(p);
    q := p^.Next;
    FreeMem(p);
    p := q;
  end;
  Result := 0;
end;
(* -------------------------------------------------------------------------- *)

function huft_build(var b: array of WORD; n, s: WORD; const d: array of WORD;
  const e: array of BYTE; var t, HF: PT; var m: Integer): Integer;
const
  BMAX = 16;
var
  a: WORD;
  c: array [0 .. BMAX] of WORD; { bit length count table }
  el: WORD;
  f: WORD;
  g: Integer; { maximum code length }
  h: Integer; { table level }
  i: WORD; { counter, current code / counter }
  j: WORD; { counter }
  k: Integer; { number OF bits in current code }
  lx: array [-1 .. BMAX + 1] of Integer;
  p: PWORD;
  q: PT;
  r: Thuft;
  u: array [0 .. BMAX] of PT;
  v: array [0 .. N_MAX] of WORD; { values in order OF bit length }
  w: WORD;
  x: array [0 .. BMAX + 1] of WORD; { bit offsets, THEN code stack }
  xp: PWORD;
  y: Integer;
  z: WORD;
begin
  { Generate counts FOR each bit length }
  if n > 256 then { set length OF EOB code, IF any }
    el := b[256]
  else
    el := BMAX;

  FillChar(c[0], SizeOf(c), 0);

  p := PWORD(@b);
  i := n;

  repeat
    Inc(c[p^]); { assume all entries <= BMAX }
    Inc(p);
    Dec(i);
  until (i = 0);

  { null input--all zero length codes }
  if c[0] = n then
  begin
    t := nil;
    m := 0;
    Result := 0;
    Exit;
  end;

  { Find minimum AND maximum length, bound *m by those }
  for j := 1 to BMAX do
    if c[j] <> 0 then
      Break;

  k := j; { minimum code length }

  if (WORD(m) < j) then
    { m := INTEGER(j); }
    m := j;

  for i := BMAX downto 1 do
    if c[i] <> 0 then
      Break;

  g := i; { maximum code length }

  if WORD(m) > i then
    m := Integer(i);

  { Adjust last length count TO fill out codes, IF needed }
  y := 1 shl j;
  for j := j to i - 1 do
  begin
    y := y - c[j];
    if y < 0 then
    begin
      Result := 2; { bad input: more codes than bits }
      Exit;
    end;
    y := y shl 1;
  end;

  y := y - c[i];
  if y < 0 then
  begin
    Result := 2;
    Exit;
  end;

  Inc(c[i], y);

  { Generate starting offsets into the value table FOR each length }
  x[1] := 0;
  j := 0;

  p := @c[1];
  xp := @x[2];

  Dec(i); { note that i = g from above }
  while (i > 0) do
  begin
    Inc(j, p^);
    xp^ := j;
    Inc(p);
    Inc(xp);
    Dec(i);
  end;

  { Make a table OF values in order OF bit lengths }
  p := PWORD(@b);
  i := 0;
  repeat
    j := p^;
    if (j <> 0) then
    begin
      v[x[j]] := i;
      Inc(x[j]);
    end;
    Inc(p);
    Inc(i);
  until i >= n;

  { Generate the Huffman codes AND FOR each, make the table entries }
  h := -1; { no tables yet--level -1 }
  i := 0;
  lx[-1] := 0; { ditto }
  p := PWORD(@v); { grab values in bit order }
  q := nil; { ditto }
  t := nil;
  u[0] := nil; { just TO keep compilers happy }
  w := 0; { no bits decoded yet }
  x[0] := 0; { first Huffman code is zero }
  z := 0; { ditto }

  { go through the bit lengths (k already is bits in shortest code) }
  for k := k to g do
  begin
    a := c[k];
    while (a <> 0) do
    begin
      Dec(a);

      { here i is the Huffman code OF length k bits FOR value *p }
      { make tables up TO required level }
      { WHILE k > INTEGER(w + lx[h]) DO }
      while k > (w + lx[h]) do
      begin
        Inc(w, lx[h]); { add bits already decoded }
        Inc(h);

        { compute minimum size table less than or equal TO *m bits }
        z := g - w; { upper limit }
        { IF z > WORD(m) }
        if z > m then
          { z :=WORD(m); }
          z := m;

        { j := WORD(k - w); }
        j := k - w;
        f := 1 shl j;
        if f > (a + 1) then { TRY a k-w bit table }
        begin { too few codes FOR k-w bit table }
          Dec(f, a + 1); { deduct codes from patterns left }
          xp := @c[k];
          Inc(j);
          while (j < z) do { TRY smaller tables up TO z bits }
          begin
            Inc(xp);
            f := f shl 1;
            if f <= xp^ then
              Break; { enough codes TO use up j bits }
            f := f - xp^; { ELSE deduct codes from patterns }
            Inc(j);
          end;
        end;

        if ((w + j > el) and (w < el)) then
          j := el - w; { make EOB code END at table }

        z := 1 shl j; { table entries FOR j-bit table }
        lx[h] := j; { set table size in stack }

        { allocate AND link in new table }
        GetMem(q, (z + 1) * SizeOf(Thuft));
        if q = nil then
        begin
          if (h > 0) then
            huft_free(u[0]);
          Result := 3;
          Exit;
        end;

        Inc(hufts, z + 1); { track memory usage }
        r.Next := HF;
        q^.Next := HF;
        Inc(q);
        HF := q;
        u[h] := q;

        // first block link
        if t = nil then
          t := q;

        { connect TO last table, IF there is one }
        if h > 0 then
        begin
          x[h] := i; { save pattern FOR backing up }

          r.b := lx[h - 1]; { bits TO dump before this table }
          r.e := (16 + j); { bits in this table }
          r.Next := q; { pointer TO this table }
          j := (i and ((1 shl w) - 1)) shr (w - lx[h - 1]);

          { connect TO last table }
          { ****************************************************************
            Use the following line in the debugger TO verify the allocated
            memory boundries with data being inserted.
            * u[h - 1][j] = q; *
            *->   (LONGINT(u[h-1])+(j*sizeof(Thuft))) - LONGINT(u[h-1])   <-*
            **************************************************************** }
          Move(r, pointer(Cardinal(u[h - 1]) + (j * SizeOf(Thuft)))^, SizeOf(r));
        end;
      end;

      { set up table entry in r }
      r.b := ShortInt(k - w);

      { IF (LONGINT(addr(p^)) >= LONGINT(addr(v[n]))) }
      if (LONGINT(p) >= LONGINT(@v[n])) then
        r.e := 99 { out OF values--invalid code }
      else if (p^ < s) then
      begin
        if p^ < 256 then { 256 is END-OF-block code }
          r.e := 16
        else
          r.e := 15;

        r.n := p^; { simple code is just the value }
        Inc(p);
      end
      else
      begin
        r.e := e[p^ - s]; { non-simple--look up in lists }
        r.n := d[p^ - s];
        Inc(p);
      end;

      { fill code-like entries with r }
      f := 1 shl (k - w);
      j := i shr w;
      while (j < z) do
      begin
        { ****************************************************************
          Use the following line in the debugger TO verify the allocated
          memory boundries with data being inserted.
          * q[j] = r;  *
          *->   (LONGINT(q)+(j*sizeof(Thuft))) - LONGINT(q)    <-*
          **************************************************************** }
        Move(r, pointer(Cardinal(q) + (j * SizeOf(Thuft)))^, SizeOf(r));

        Inc(j, f);
      end;

      { backwards increment the k-bit code i }
      j := 1 shl (k - 1);
      while (i and j) <> 0 do { added...   <> 0 }
      begin
        i := i xor j; { bitwise exclusive or }
        j := j shr 1;
      end;

      i := i xor j; { bitwise exclusive or }

      { backup over finished tables }
      while ((i and ((1 shl w) - 1)) <> x[h]) do
      begin
        Dec(h);
        Dec(w, lx[h]); { don't need TO update q }
      end;
    end;
  end;

  { return actual size OF base table }
  m := Integer(lx[0]);

  if (y <> 0) then
    y := 1
  else
    y := 0;

  if (g <> 1) then
    g := 1
  else
    g := 0;
  Result := (y and g);

  { Return true (1) IF we were given an incomplete table }
  { result := (y <> 0) AND  (g <> 1); }
end;
(* -------------------------------------------------------------------------- *)

function inflate_codes(var tl, td: PT; bl, bd: Integer): Integer;
(* tl,td:   literal/length AND distance decoder tables *)
(* bl,bd:   number OF bits decoded by tl[] AND td[] *)

(* inflate (decompress) the codes in a deflated (compressed) block.
  Return an error code or zero IF it all goes ok. *)
var
  e: WORD; { table entry flag/number OF extra bits }
  n, d: WORD; { length AND index FOR copy }
  w: WORD; { current window position }
  t: PT; { Thuft } { pointer TO table entry }
  ml, md: WORD; { masks FOR bl AND bd bits }
  b: LONGINT; { bit buffer }
  k: WORD; { number OF bits in bit buffer }
begin

  { make local copies OF globals }
  b := bb; { initialize bit buffer }
  k := bk;
  w := WP; { initialize window position }

  { inflate the coded data }
  ml := maskr[bl]; { precompute masks FOR speed }
  md := maskr[bd];
  repeat
    NEEDBITS(bl, b, k);
    t := pointer(LONGINT(tl) + ((WORD(b) and ml) * SizeOf(Thuft)));
    { t :=ptr(seg(tl^), ofs(tl^)+ ((WORD(b) AND ml) * sizeof(Thuft))); }

    (* Inflate_Fixed & Inflate_Dynamic *)
    { with CentralZipHeader DO
      IF CalcProgress(False, PMode, Percent, UnpackedSize - Bytes_To_Go, UnpackedSize) THEN
      DoProgress(Percent); }

    e := t^.e;
    if (e > 16) then
      while (e > 16) do
      begin
        if (e = 99) then
        begin
          Result := 1;
          Exit;
        end;
        DUMPBITS(t^.b, b, k);
        Dec(e, 16);
        NEEDBITS(e, b, k);

        t := pointer(LONGINT(t^.Next) + ((b and maskr[e]) * SizeOf(Thuft)));
        e := t^.e;
      end;

    DUMPBITS(t^.b, b, k);
    if (e = 16) then { THEN it's a literal }
    begin
      Slide^[w] := t^.n;
      Inc(w);
      // Dec(Bytes_To_Go);

      if (w = WORD(WSIZE)) then
      begin
        // Inc(ExtCount);
        CheckFWrite(VH_OutFile, Slide^, w, VStr_OutFile);

        crc32_buf(PByte(Slide), w, VDW_CRC32Val);
        w := 0;
      end;
    end
    else
    begin { it's an EOB or a length }
      { exit IF END OF block }
      if (e = 15) then
        Break;

      { get length OF block TO copy }
      NEEDBITS(e, b, k);
      n := t^.n + (WORD(b) and maskr[e]);
      { n := t^.n + (b AND maskr[e]); }
      DUMPBITS(e, b, k);

      { decode distance OF block TO copy }
      NEEDBITS(WORD(bd), b, k);
      { NEEDBITS(bd,b,k); }

      t := pointer(LONGINT(td) + ((b and md) * SizeOf(Thuft)));

      e := t^.e;
      if e > 16 then
        repeat
          if (e = 99) then
          begin
            Result := 1;
            Exit;
          end;
          DUMPBITS(t^.b, b, k);
          Dec(e, 16);
          NEEDBITS(e, b, k);
          t := pointer(LONGINT(t^.Next) + ((WORD(b) and maskr[e]) * SizeOf
                (Thuft)));
          { t := pointer(LONGINT(t^.next) + ((b AND maskr[e]) * sizeof(Thuft))); }
          e := t^.e;
        until (e <= 16);

        DUMPBITS(t^.b, b, k);
      NEEDBITS(e, b, k);

      d := WORD(w - t^.n - (b and maskr[e]));

      DUMPBITS(e, b, k);

      { DO the copy }
      repeat
        d := (d and (WSIZE - 1));

        if (d > w) then
          e := WSIZE - d
        else
          e := WSIZE - w;

        if (e > n) then
          e := n;

        Dec(n, e);

        (* incrementing w by e bytes below... DO same with bytes_to_go
          prior TO value e changing *)
        // Dec(Bytes_To_Go, e);

        if ((w - d) >= e) then { this test assumes unsigned comparison }
        begin
          Move(Slide^[d], Slide^[w], e);
          Inc(w, e);
          Inc(d, e);
        end
        else
        begin { DO it slow TO avoid memcpy() overlap }
          repeat
            Slide^[w] := Slide^[d];
            Inc(w);
            Inc(d);
            Dec(e);
          until (e <= 0);
        end;

        if (w = WORD(WSIZE)) then
        begin
          CheckFWrite(VH_OutFile, Slide^, w, VStr_OutFile);

          crc32_buf(PByte(Slide), w, VDW_CRC32Val);
          w := 0;
        end;
      until n = 0;
    end;

  until (1 <> 1);

  { restore the globals from the locals }
  WP := w; { restore global window pointer }
  bb := b; { restore global bit buffer }
  bk := k;

  Result := 0;
end;
(* -------------------------------------------------------------------------- *)

(*
  mofified nov 23, 2002 (changes contributed by James Turner)
  procedure inflate_fixed;
  { decompress an inflated type 1 (fixed Huffman codes) block.  We should
  either replace this with a custom decoder, or at least precompute the
  Huffman tables. }
  var
  i: INTEGER; { temporary variable }
  l: array[0..287] of WORD; { length list FOR huft_build }
  fixed_bl, fixed_bd: INTEGER;
  HFTD, HFTL: PT;
  begin

  { IF first time, set up tables FOR fixed blocks }
  if (fixed_tl = nil) then
  begin
  { literal table }
  for i := 0 to 287 do
  begin
  case i of
  0..143: l[i] := 8;
  144..255: l[i] := 9;
  256..279: l[i] := 7;
  280..287: l[i] := 8; { make a complete, but wrong code set }
  end;
  end;

  fixed_bl := 7;
  i := huft_build(l, 288, 257, cplens, cplext, fixed_tl, HFTL, fixed_bl);
  if (i <> 0) then
  begin
  fixed_tl := nil;

  { ********** REM'D RAISE ************ }
  //          raise E_RAISE.Create(LoadStr(E_CODESET));
  end;

  { distance table }
  for i := 0 to 29 do { make an incomplete code set }
  l[i] := 5;

  fixed_bd := 5;

  i := huft_build(l, 30, 0, cpdist, cpdext, fixed_td, HFTD, fixed_bd);
  if (i > 1) then
  begin
  {ErrCode := IncompleteCodeSet;}
  huft_free(HFTL);
  fixed_tl := nil;

  { ********** REM'D RAISE ************ }
  //raise E_RAISE.Create(LoadStr(E_CODESET));
  end;
  end;

  { decompress UNTIL an END-OF-block code }
  i := inflate_codes(fixed_tl, fixed_td, fixed_bl, fixed_bd);

  if i <> 0 then

  { ********** REM'D RAISE ************ }
  //raise E_RAISE.Create(LoadStr(E_BADBLOCK));
  ;

  end; *)

procedure inflate_fixed;
begin
  { decompress until an end-of-block code }
  inflate_codes(fixed_tl, fixed_td, 7, 5);
end;
(* -------------------------------------------------------------------------- *)
var
  HFTD, HFTL: PT;

procedure InitFixedTables;
var
  i: Integer;
  L: array [0 .. 287] of WORD; { length list FOR huft_build }
  fixed_bl, fixed_bd: Integer;
//  HFTD, HFTL: PT;
begin
  HFTL := nil;
  HFTD := nil;

  { create literal table }
  for i := 0 to 287 do
    case i of
      0 .. 143:
        L[i] := 8;
      144 .. 255:
        L[i] := 9;
      256 .. 279:
        L[i] := 7;
      280 .. 287:
        L[i] := 8; { make a complete, but wrong code set }
    end; { case / for }

  fixed_bl := 7;

  huft_build(L, 288, 257, cplens, cplext, fixed_tl, HFTL, fixed_bl);

  { distance table }
  for i := 0 to 29 do
    L[i] := 5; { make an incomplete code set }

  fixed_bd := 5;

  huft_build(L, 30, 0, cpdist, cpdext, fixed_td, HFTD, fixed_bd);
end;
(* -------------------------------------------------------------------------- *)

procedure FreeFixedTables;
begin
  huft_free(HFTD);
  huft_free(HFTL);
//  huft_free(fixed_tl);
//  huft_free(fixed_td);
end;
(* -------------------------------------------------------------------------- *)

procedure inflate_dynamic;
var
  i: Integer; { temporary variables }
  j: WORD; { }
  L: WORD; { last length }
  m: WORD; { mask FOR bit lengths table }
  n: WORD; { number OF lengths TO get }
  tl: PT; { literal/length code table }
  td: PT; { distance code table }
  HFTL, HFTD: PT;
  bl: Integer; { lookup bits FOR tl }
  bd: Integer; { lookup bits FOR td }
  nb: WORD; { number OF bit length codes }
  nl: WORD; { number OF literal/length codes }
  nd: WORD; { number OF distance codes }
{$IFDEF PKZIP_BUG_WORKAROUND}
  ll: array [0 .. 288 + 32] of WORD;
{$ELSE}
  ll: array [0 .. 286 + 30] of WORD;
{$ENDIF}
  b: LONGINT; { bit buffer }
  k: WORD; { number OF bits in bit buffer }
const
  border: array [0 .. 18] of BYTE = { Order OF the bit length code lengths }
  (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);
  dummy1: array [0 .. 30] of WORD = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  dummy2: array [0 .. 30] of BYTE = { Extra bits FOR literal codes 257..285 }
  (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0); { 99==invalid }
begin
  { make local bit buffer }
  b := bb;
  k := bk;

  { read in table lengths }
  NEEDBITS(5, b, k);
  nl := 257 + (WORD(b) and $1F); { number OF literal/length codes }
  DUMPBITS(5, b, k);
  NEEDBITS(5, b, k);
  nd := 1 + (WORD(b) and $1F); { number OF distance codes }
  DUMPBITS(5, b, k);
  NEEDBITS(4, b, k);
  nb := 4 + (WORD(b) and $F); { number OF bit length codes }
  DUMPBITS(4, b, k);
{$IFDEF PKZIP_BUG_WORKAROUND}
  if ((nl > 288) or (nd > 32)) then
{$ELSE}
    if ((nl > 286) or (nd > 30)) then
{$ENDIF}
      (* ***********  REM'D RAISE  ************* *)
      // raise E_RAISE.Create(LoadStr(E_INVALIDLEN));
      ;

  { read in bit-length-code lengths }
  for j := 0 to nb - 1 do
  begin
    NEEDBITS(3, b, k);
    ll[border[j]] := WORD(b) and 7;
    DUMPBITS(3, b, k);
  end;

  for j := nb to 18 do
    ll[border[j]] := 0;

  { build decoding table FOR trees--single level, 7 bit lookup }
  bl := 7;
  HFTL := nil;
  i := huft_build(ll, 19, 19, dummy1, dummy2, tl, HFTL, bl);
  if (i <> 0) then
  begin
    if (i = 1) then
      { huft_free(tl); }
      huft_free(HFTL);

    (* ***********  REM'D RAISE  ************* *)
    // raise E_RAISE.Create(LoadStr(E_CODESET));

  end;

  { read in literal AND distance code lengths }
  n := nl + nd;
  m := maskr[bl];
  i := 0;
  L := 0;
  while (WORD(i) < n) do
  begin
    NEEDBITS(WORD(bl), b, k);

    td := pointer(LONGINT(tl) + ((b and m) * SizeOf(Thuft)));

    j := td^.b;
    DUMPBITS(j, b, k);

    j := td^.n;
    if (j < 16) then { length OF code in bits (0..15) }
    begin
      ll[i] := j;
      L := j; { * save last length in l }
      Inc(i);
    end
    else if (j = 16) then { REPEAT last length 3 TO 6 times }
    begin
      NEEDBITS(2, b, k);
      j := 3 + (WORD(b) and 3);
      DUMPBITS(2, b, k);
      if (WORD(i) + j > n) then

        (* ***********  REM'D RAISE  ************* *)
        // raise E_RAISE.Create(LoadStr(E_CODESET));
        ;

      while (j <> 0) do
      begin
        ll[i] := L;
        Inc(i);
        Dec(j);
      end
    end
    else if (j = 17) then { 3 TO 10 zero length codes }
    begin
      NEEDBITS(3, b, k);
      j := 3 + (WORD(b) and 7);
      DUMPBITS(3, b, k);
      if (WORD(i) + j > n) then

        (* ***********  REM'D RAISE  ************* *)
        // raise E_RAISE.Create(LoadStr(E_CODESET));
        ;

      while (j <> 0) do
      begin
        ll[i] := 0;
        Inc(i);
        Dec(j);
      end;
      L := 0;
    end
    else
    begin { j == 18: 11 TO 138 zero length codes }
      NEEDBITS(7, b, k);
      j := 11 + (WORD(b) and $7F);
      DUMPBITS(7, b, k);
      if (WORD(i) + j > n) then

        (* ***********  REM'D RAISE  ************* *)
        // raise E_RAISE.Create(LoadStr(E_CODESET));
        ;

      while (j <> 0) do
      begin
        ll[i] := 0;
        Inc(i);
        Dec(j);
      end;
      L := 0;
    end;
  end;

  { free decoding table FOR trees }
  { huft_free(tl); }
  huft_free(HFTL);

  { restore the global bit buffer }
  bb := b;
  bk := k;

  { build the decoding tables for literal/length AND distance codes }
  bl := lbits;
  HFTL := nil;
  i := huft_build(ll, nl, 257, cplens, cplext, tl, HFTL, bl);
  if (i <> 0) then
  begin
    if i = 1 then
      huft_free(HFTL);

    (* ***********  REM'D RAISE  ************* *)
    // raise E_RAISE.Create(LoadStr(E_CODESET));

  end;

  bd := dbits;
  HFTD := nil;
  i := huft_build(ll[nl], nd, 0, cpdist, cpdext, td, HFTD, bd);
  if (i <> 0) then
  begin
    if i = 1 then
    begin

      (* ***********  REM'D RAISE  ************* *)
      // raise E_RAISE.Create(LoadStr(E_CODESET));
{$IFDEF PKZIP_BUG_WORKAROUND}
      { i := 0;   ********************** return as result?????? }
    end;
{$ELSE}
    huft_free(HFTD);

    (* ***********  REM'D RAISE  ************* *)
    // raise E_RAISE.Create(E_CODESET);

  end;
  { huft_free(tl); }
  huft_free(HFTL);
  { result := i; }{ incomplete code set }
  Result := IncompleteCodeSet;
  Exit;
{$ENDIF}
end;

{ decompress UNTIL an END-OF-block code }
if (inflate_codes(tl, td, bl, bd)) <> 0 then
  // raise E_RAISE.Create(LoadStr(E_CODESET));
  ;

{ free the decoding tables, return }
huft_free(HFTL); { ******** IF inflate_codes fails above, }
huft_free(HFTD); { ******** memory is not released!!! }

{ result :=0; }
{ result := None; }{ 100% correct result???? }
end;
(* -------------------------------------------------------------------------- *)

{ decompress an inflated block }

procedure inflate_block(var e: Integer); { e = last block flag }
var
  t: WORD; { block type }
  k: WORD; { number OF bits in bit buffer }
  b: LONGINT; { bit buffer }
begin
  { make local bit buffer }
  b := bb;
  k := bk;

  { read in last block bit }
  NEEDBITS(1, b, k);
  e := Integer(b) and 1;
  DUMPBITS(1, b, k);

  { read in block type */ }
  NEEDBITS(2, b, k);
  t := WORD(b) and 3;
  DUMPBITS(2, b, k);

  { restore the global bit buffer }
  bb := b;
  bk := k;

  { inflate that block type }
  case t of
    0:
      inflate_stored;
    1:
      inflate_fixed;
    2:
      inflate_dynamic;
  else
    (* ***********  REM'D RAISE  ************* *)
    // raise E_RAISE.Create(LoadStr(E_BADBLOCK));
    ;
  end;
end;
(* -------------------------------------------------------------------------- *)

{ decompress an inflated entry }

procedure Inflate(mem: pointer; size: Integer);
var
  e: Integer; { last block flag }
  h: WORD; { maximum struct huft's malloc'ed }
begin
  InPTR := 0;
  ZipCount := 0;

  { initialize window, bit buffer }
  WP := 0;
  bk := 0;
  bb := 0;

  { decompress UNTIL the last block }
  h := 0;

  // InBuf := nil;
  Slide := nil;
  // GetMem(InBuf, sizeof(InBuf^) + 1);
  GetMem(Slide, SizeOf(Slide^) + 1);
  if (mem = nil) or (size <= 0) then
  begin
    InBuf := nil;
    GetMem(InBuf, SizeOf(InBuf^) + 1);
    InfMem := False;
  end
  else
  begin
    InBuf := mem;
    ZipCount := Cardinal(size);
    InfMem := True;
  end;

  try
    // fixed_tl := nil; //changes contributed by James Turner, done nov 23, 2002
    // fixed_td := nil; //changes contributed by James Turner, done nov 23, 2002
    try
      repeat
        hufts := 0;
        inflate_block(e);
        if (hufts > h) then
          h := hufts;
      until (e <> 0);

      // with LocalZipHeader DO
      // IF CalcProgress(False, PMode, Percent, PackedSize - Bytes_To_Go, PackedSize) THEN
      // DoProgress(Percent);

      if WP > 0 then
      begin
        CheckFWrite(VH_OutFile, Slide^, WP, VStr_OutFile);

        crc32_buf(PByte(Slide), WP, VDW_CRC32Val);
        WP := 0;
      end;
    except
      // MessageBox(0, 'Error...', 'Error', mb_OK)
    end;

  finally
    if not InfMem then
      FreeMem(InBuf);
    FreeMem(Slide);
  end;
end;
(* -------------------------------------------------------------------------- *)

initialization

InitFixedTables;

finalization

FreeFixedTables;

end.
