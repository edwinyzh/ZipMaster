program MakeExe;
(************************************************************************
 Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht,
      Eric W. Engler and Chris Vleghert.

   This file is part of TZipMaster Version 1.9.

    TZipMaster is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    TZipMaster is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with TZipMaster.  If not, see <http://www.gnu.org/licenses/>.

    contact: problems@delphizip.org (include ZipMaster in the subject).
    updates: http://www.delphizip.org
    DelphiZip maillist subscribe at http://www.freelists.org/list/delphizip 
************************************************************************)

uses
  Forms,
  mainunit in 'mainunit.pas' {Mainform},
  Addunit in 'Addunit.pas' {AddForm};

{$R *.RES}
{$R '..\..\res\ZMRes192_all.res'}
{.$R '..\..\res\ZMRes192_lng.res'}
{.$R '..\..\res\ZMRes192_sfx.res'}

begin
  Application.Initialize;
  Application.Title := 'Make Exe Archive';
  Application.CreateForm(TMainform, Mainform);
  Application.CreateForm(TAddForm, AddForm);
  Application.Run;
end.
