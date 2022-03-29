
                           Delphi Demo 4 

This is a simple little "ViewZip" application, but it supports
automatic install features.  The reason I have designed this demo
is to show you how to write programs that can install and un-install
themselves.


If VIEWZIP.EXE were being distributed in a Delphi Zip Self-Extracting 
Archive, you could tell it to run this command line after extraction:
'><viewzip /install'.  This would bring up the install menu.



Ways to test this program from a DOS box:

C>viewzip /install       Automatically bring up the install menu

C>viewzip test.zip       Run it with a zip file parameter
 
C>start test.zip         Check the File Extension assoc. in Registry

C>start viewzip          Check the Application Path in Registry
 
C>viewzip /uninstall     Automatically perform uninstall and quit


The uninstall can also be performed from the Control Panel.
Select Start, Settings, Control Panel, Add/Remove Programs.
