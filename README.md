# ZipMaster
TZipMaster is a Delphi component for writing/reading .zip archives. This repository is based on `ZipMaster 1.9.2.0021 29Mar2016 10:55` and contains my fixes.

- TZipMaster has a full set of features for writing/reading zip archives, most of the logics are implemented in Delphi, but it relies on a 300+KB DLL for the underlying operations.
- TZipMaster supports both 64bit and 32bit Delphi compilers.
- TZipMaster supports Zip64 (for zip files larger than 4GB).

# History
Russell Peters was the maintainer of ZipMaster when I started using it around 2005). Before that, Chris Vleghert and Eric W. Engler were the original authors I guess. 

On 2019, Russell Peters posted to the DelphiZip/ZipMaster mail list (https://www.freelists.org/post/delphizip/Anyone-willing-to-take-over-development-and-maintenance-of-ZipMaster):

> Unfortunately, I have very little time left (cancer) so cannot continue 
> development or maintenance of ZipMaster.
> 
> Is there someone else willing to take it on?
> 
> Russell Peters

That's a sad news and I always want to thank Russell (and other contributors) for his contribution to the Delphi community. Unfortunately, no one has taken over the project.

This year (2022), I need to upgrade one of my projects which uses ZipMaster, and during the process I encountered several minor issues and I fixed them. Since there is nowhere I can send my PR's to (delphizip.org is unreachable now), I had to create this repository. Anyone is welcomed to summit your chances, as long as your code meets the following.

# Some notes
- My comments in the code starts with "Edwin:" for keeping track of my changes before putting this project to github.

# Requirements for code contribution
- Please don't break old Delphi compiler - I use XE4 but maybe we should keep the code compi-able by all Unicode Delphi compilers.
