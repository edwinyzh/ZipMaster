unit tziplist;
(* ***************************************************************************
  TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
 Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
 Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
 Copyright (C) 1992-2008 Eric W. Engler
 Copyright (C) 2009, 2010, 2011, 2012, 2013 Russell Peters and Roger Aelbrecht
 Copyright (C) 2014 Russell Peters and Roger Aelbrecht

   This file is part of TZipMaster Version 1.9.2
   
All rights reserved.
For the purposes of Copyright and this license "DelphiZip" is the current
 authors, maintainers and developers of its code:
  Russell Peters and Roger Aelbrecht.

Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
* DelphiZip reserves the names "DelphiZip", "ZipMaster", "ZipBuilder",
   "DelZip" and derivatives of those names for the use in or about this
   code and neither those names nor the names of its authors or
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL DELPHIZIP, IT'S AUTHORS OR CONTRIBUTERS BE
 LIABLE FOR ANYDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

contact: problems AT delphizip DOT org
updates: http://www.delphizip.org
 *************************************************************************** *)

interface

uses
  Wintypes, Winprocs, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Grids, ExtCtrls, SortGrid, ZipMstr;

type
  TZipForm = class(TForm)
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Button2: TButton;
    ZipFNameLabel: TLabel;
//    StringGrid1: TSortGrid;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure StringGrid1BeginSort(Sender: TObject; Col: Longint;
                                  var SortOptions: TSortOptions);
    procedure FillGrid;
  private
    { Private declarations }
  public
   { Public declarations } 
    StringGrid1: TSortGrid;
end;

var
  ZipForm: TZipForm;

implementation

uses Unit1, printers;
{$R *.DFM}

procedure TZipForm.FormCreate(Sender: TObject);
begin
  StringGrid1 := TSortGrid.Create(self);
  StringGrid1.Parent := self;
  with StringGrid1 do
  begin
    Left := 0;
    Top := 46;
    Width := 591;
    Height := 318;
    Align := alClient;
    ColCount := 1;
    DefaultRowHeight := 20;
    FixedCols := 0;
    RowCount := 2;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clBlack;
    Font.Height := -13;
    Font.Name := 'Arial';
    Font.Style := [fsBold];
    Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goThumbTracking];
    ParentFont := False;
    TabOrder := 2;
    CaseSensitive := False;
    AlignmentHorz := taLeftJustify;
    AlignmentVert := taTopJustify;
    ProportionalScrollBars := True;
    ExtendedKeys := False;
    SortOnClick := True;
    FooterFont.Charset := DEFAULT_CHARSET;
    FooterFont.Color := clWindowText;
    FooterFont.Height := -11;
    FooterFont.Name := 'MS Sans Serif';
    FooterFont.Style := [];
    PrintOptions.Orientation := poPortrait;
    PrintOptions.PageTitleMargin := 0;
    PrintOptions.PageFooter := 'date|time|page';
    PrintOptions.HeaderSize := 10;
    PrintOptions.FooterSize := 7;
    PrintOptions.DateFormat := 'd-mmm-yyyy';
    PrintOptions.TimeFormat := 'h:nn';
    PrintOptions.FromRow := 0;
    PrintOptions.ToRow := 0;
    PrintOptions.BorderStyle := bsNone;
    PrintOptions.MarginBottom := 0;
    PrintOptions.MarginLeft := 0;
    PrintOptions.MarginTop := 0;
    PrintOptions.MarginRight := 0;
    WordWrap := False;
    OnBeginSort := StringGrid1BeginSort;
    RowCount:=1;  { first row is fixed, and used for titles }
    ColCount:=4;
    Cells[0,0] := 'File Name';
    Cells[1,0] := 'Compr Size';
    Cells[2,0] := 'Uncmpr Size';
    Cells[3,0] := 'Date/Time';
  end;
end;

procedure TZipForm.FillGrid;
var
  i: Integer;
begin
  with StringGrid1 do
  begin
    { Empty data from string grid }
    FixedRows:=0;
    RowCount:=1; { remove everything from grid except col titles }
    if Form1.ZipMaster1.Count = 0 then
       Exit;

    for i:=0 to Form1.ZipMaster1.Count-1 do
    begin
       RowCount := RowCount + 1;
       { We have to set fixed rows after the rowcount is more than 1}
       FixedRows:=1;
//       with TZipDirEntry(Form1.ZipMaster1.ZipContents[i]^) do
       with Form1.ZipMaster1[i] do
       begin
          { The "-1" below is an offset for the row titles }
          Cells[0,RowCount-1] := FileName;
          Cells[1,RowCount-1] := IntToStr(CompressedSize);
          Cells[2,RowCount-1] := IntToStr(UncompressedSize);
          Cells[3,RowCount-1] := FormatDateTime('ddddd  t',FileDateToDateTime(DateTime));
       end; // end with
    end; // end for
  end; // end with
end;

procedure TZipForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TZipForm.FormActivate(Sender: TObject);
begin
   Width:=Form1.Width;
   Height:=Form1.Height;
   Top:=Form1.Top;
   Left:=Form1.Left;
   ZipFNameLabel.Caption:=Form1.ZipFName.Caption;
   with StringGrid1 do
   begin
      FixedRows:=0;
      RowCount:=1; { remove everything from grid except col titles }
      ColWidths[0]:=316;
      ColWidths[1]:=84;
      ColWidths[2]:=84;
      ColWidths[3]:=120;
   end;

   if FileExists(Form1.ZipFName.Caption) then
      { This assignment causes zipfile to be read: }
      Form1.ZipMaster1.ZipFileName := Form1.ZipFName.Caption
   else
   begin
      ShowMessage('Error - file not found: ' + Form1.ZipFName.Caption);
      Close;
   end;
   FillGrid;
end;

{ This just shows you which column, datatype, and sort order will be used. }
{ This is keyed from the SortGrid's OnBeginSort event. }
{ You can remove this if you want. }
procedure TZipForm.StringGrid1BeginSort(Sender: TObject; Col: Longint;
          var SortOptions: TSortOptions);
var
  Order: String;
  ColName: String;
begin
  if SortOptions.SortDirection=sdAscending then
     Order:='Ascending'
  else
     Order:='Descending';
  ColName:=StringGrid1.Cells[Col,0];
  case SortOptions.SortStyle of
     ssNumeric:  ShowMessage('Sorting By ' + ColName + ', Numeric, ' + Order);
     ssDateTime: ShowMessage('Sorting By ' + ColName + ', Datetime, ' + Order);
     ssTime:     ShowMessage('Sorting By ' + ColName + ', Time, ' + Order);
     ssCustom:   ShowMessage('Sorting By ' + ColName + ', Custom, ' + Order);
  else
     ShowMessage('Sorting By ' + ColName + ', Alpha, ' + Order);
  end;
end;

end.
