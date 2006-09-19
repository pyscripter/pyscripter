To compile the PythonIDE.dpr, you first need to install the following components:            

   - Python for Delphi (www.mmm-experts.com)
     In the Definitions.inc file activate the conditional define PREFER_UNICODE

   - JVCL version 3.2 built 2172 (jvcl.sf.net) and JCL versuib 1.97 (jcl.sf.net)
     Downolad the latest version
   - Download FastMM from http://sourceforge.net/projects/fastmm/
     Rename FastMM4Options.inc to say OriginalFastMM4Options.inc so that project specific options will apply
   - Toolbar2000 version 2.1.8 (www.jrsoftware.org/tb2k.php)
   - TBX version 2.1b (from www.g32.org/tbx)
   - Download from http://club.telepolis.com/silverpointdev and apply the TB2k v2.1.8 - TBX v2.1 patch before installing Toolbar 2000 and TBX
     Note that you can download the patch.exe tool from http://unxutils.sourceforge.net/
     Note also that If you want a painless TB2k+TBX installation for Delphi you can download the Multiinstaller from 
     http://club.telepolis.com/silverpointdev/multiinstaller/index.htm and follow the instructions provided in that page.          You do not need TNT or SpTBXLib.

   - Download and install JvTBXLib.zip from mxs.bergsoft.net (no need to bother with the rest of TBXLib).

   - Download from http://www.rmklever.com/delphitbx.html the themes by Roy Magne Klever
     (Only two of these themes NexosX and Whidbey are currently used)
   - From the same site download the Office2003 theme by Yury Plashenkov 
   - Copy the above themes to the TBX folder.  No need to install them.

   - Unicode version of SynEdit (synedit.sf.net) at http://mh-nexus.de/unisynedit.htm
   - VirtualTreeView (www.delphi-gems.com) at http://www.delphi-gems.com/VirtualTreeview/
   - VirtualShellTools (www.mustangpeak.net)
     In the file VSToolsAddins.inc in the "Include" directory enable the USE_TBX flag before installing

If you have managed to do all the above then you should be able to compile PyScripter! (:)) 

 
   

