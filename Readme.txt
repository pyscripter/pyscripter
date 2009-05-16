INSTRUCTIONS FOR COMPILIING PyScripter
======================================

I am using Borland Delphi 2006 to compile PyScripter but with few changes it could compile with other versions.



Use TortoiseSVN to download the code from pyscripter.googlecode.com

To compile the PythonIDE.dpr, you first need to install the following components:            

   - Python for Delphi (http://python4delphi.googlecode.com/ - use SVN)

   - JVCL version 3.36 (jvcl.sf.net) and JCL version 1.1104 (jvcl.sf.net)

   - Toolbar2000 (http://www.innosetup.com/tb2k.php)
   - Tntware Delphi Unicode Controls (http://www.silverpointdevelopment.com/sptbxlib/TntUnicodeControls.zip)
   - SpTBXLib version (Use SVN svn://www.soft-gems.net/mustangpeak/SpTBXLib)
     
   - Install the PyScripterCustom package and the Tntware Delphi LX Controls (can be found in the Components folder of PyScripter)

   - Unicode version of SynEdit (synedit.sf.net) at http://mh-nexus.de/unisynedit.htm (latest version needed)
   - SynWeb Highlighters at http://flatdev.ovh.org/

   - VirtualTreeView (http://www.delphi-gems.com/) 
   
   - MustangPeak Components: Common Library, EasyListView and VirtualShellTools.  
     Use SVN (svn://www.soft-gems.net/mustangpeak)
     In all three packages you need to modify the files Addins.inc the "Include" directory and the Options.ini 
     in the Source directory to enable TB2K and SpTBX

If you have managed to do all the above then you should be able to compile PyScripter! :) 
