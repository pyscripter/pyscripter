&lt;wiki:gadget url="http://pyscripter.googlecode.com/svn/wiki/files/adsense3.xml" border="0" width="468" height="60" /&gt;

# Frequently Asked Questions #



### Which Python versions are supported? ###

PyScripter supports Python versions 2.4-2.7, 3.0-3.4.

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### How can I debug Django applications? ###

Here is how you can debug [Django](http://www.djangoproject.com/) applications with PyScripter in six simple steps:

  1. In the File explorer locate the root directory of your Django application. You may want to right click on the directory name and select File Explorer, Explore here.
  1. Open the project files in PyScripter (e.g., models,py, views.py etc.) and set whatever breakpoints you want.
  1. Select Run, Command Line Parameters... and set the command line to "runserver --noreload". Also check the "Use Command line" checkbox.
  1. Make sure the remote engine is selected (Run, Python Engine, Remote).
  1. Open the manage.py file and press the debug button (or press F9).
  1. Start a web browser and test your application. PyScripter should now stop at whatever breakpoints you have set and you can use the various debugging facilities (call stack, variables, interpreter prompt etc.)

To stop debugging, right-click on the interpreter window and select "Reinitialize Interpreter", then go to the browser and reload the document.

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### How do I use PyScripter with Portable (Movable) Python? ###

You can very easily setup PyScripter so that it can work with unregistered versions of Python such as for example [Portable Python](http://www.portablepython.com/) and [Movable Python](http://www.voidspace.org.uk/python/movpy/) which are typically residing in a USB stick or a portable hard disk.

Steps:

a) Download the zip-only distribution from the [PyScripter downloads page](https://sourceforge.net/projects/pyscripter/files/) and extract it to a directory of your choice in the portable drive.

b)  From an existing PyScripter installation copy PyScripter.ini (this file is typically located in the %APPDATA%\PyScripter directory) to the same directory as the PyScripter.exe file..

c) In the same directory create a batch or command file that will set the PYTHONHOME environment variable and start PyScripter with appropriate command-line options e.g.

```
SET PYTHONHOME=E:\PortablePython 
PyScripter --PYTHON25 --PYTHONDLLPATH "E:\PortablePython" %1 %2 %3 %4 %5
```

d) Start PyScripter using the created batch file.

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### How do I use PyScripter with an unregistered version of Python (e.g. WinPython)? ###

See answer above

For WinPython in particular the following command file works.  It assumes WinPython version is 2.7.9.amd64
and that it is located at C:\WinPython\python-2.7.9.amd64. Adjust this according to your WinPython version and installation directory.

```
SET PYTHONHOME=C:\WinPython\python-2.7.9.amd64
SET PYTHONPATH=C:\WinPython\python-2.7.9.amd64\Lib
SET HOME=C:\WinPython\settings
SET PATH=C:\WinPython\python-2.7.9.amd64\Lib\site-packages\PyQt4;C:\WinPython\python-2.7.9.amd64\;%PATH%
SET WINPYDIR=C:\WinPython\python-2.7.9.amd64
SET WINPYVER=2.7.9.3
PyScripter.exe --PYTHON27 --PYTHONDLLPATH "C:\WinPython\python-2.7.9.amd64" %1 %2 %3 %4 %5
```

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### When should I use the remote interpreter/debugger? ###

The remote interpreter/debugger is strongly recommended for all uses of PyScripter.
It is more robust since it isolates the IDE from the Python engine that runs your scripts.
It also allows you to start afresh by reinitializing the Python engine.

When you run GUI scripts such wxPython, Tkinter etc. it is necessary to use the remote Python engine and reinitialize the engine before each run.  This last step
is done automatically by default.

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### How do I use the remote interpreter/debugger? ###

See [Remote Python Engines](RemoteEngines.md)

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### How do I use Matplotlib with PyScripter? ###

To work with [matplotlib](http://matplotlib.sourceforge.net/) within PyScripter you should use the [Remote Python Engines](RemoteEngines.md).

You can also run matplotlib in interactive mode. Here is how:

  * Choose the right Python engine (Run, Python Engine menu option) for the backend of your choice e.g. Remote engine Tk for the "TkAgg" backend or Remote Engine wx for the "WX" and "WxAgg" backends.

  * Assuming that you have selected the Remote Engine wx, issue the following commands in the interpreter:
```
  >>> import matplotlib
  >>> matplotlib.interactive(True)
  >>> matplotlib.use("WXAgg")
  >>> from matplotlib.pylab import *
  >>> plot([1,2,3])
  >>> xlabel('time (s)') 
```
  * You can set the backend and interactive mode in the matplotlibrc file. If this is done, the following is sufficient for the above example:
```
  >>> from pylab import *
  >>> plot([1,2,3])
  >>> xlabel('time (s)')
```

  * Issue more pylab commands, or close the pylab window and call plot again for a new plot, etc.


**Sample script by heylam**
```
#Tested on: PyScripter 1.9.9.2, Matplotlib 0.91.2
 #Assumed setup:
 #In site-packages\matplotlib\mpl-data\matplotlibrc set backend to WXAgg.

def demoplot():
   '''This represents your work'''
   close('all') #closes figures
   t = c_[0:1:20j]
   s = cos(2*pi*t)
   for ampl in c_[0.1:1:10j]:
     plot(t, ampl*s, 'o-', lw=2)
   xlabel('time (s)')
   ylabel('voltage (mV)')
   title('demo', color='b')
   grid(True)
   draw() #update figure if already shown

#Select a demonstration:
 if 0: #Normal session
   #Starts non-interactive.
   #Figures have toolbar for zooming and panning.
   #Disadvantage: You can't re-run your script with PyScripter Remote
   # engine without first reinitializing the Remote interpreter.
   #Best use Remote(Wx) engine. This also allows interactive mode using
   # ion() and ioff(). For disadvantages: see the PyScripter help file.

#from numpy import *
   #from scipy import * #includes numpy
   from pylab import *  #includes scipy

demoplot()
   show() #Open the figure, let figure GUI take over.
     #This should be last line of script.
     #You can also type this at command line after the script exits.

if 0:
     ion() #turns interactive mode on  (needs Remote(Wx) engine!)
     ylabel('interactive modification')
     plot( rand(200), rand(200), 'go' )
     ioff() #turns interactive mode off

elif 0: #Same but use WX instead
   try:
     type(matplotlib)
   except NameError:
     import matplotlib
     matplotlib.use('WX')
     from matplotlib.pylab import *

demoplot()
   show() #Open the figure, let figure GUI take over.
     #This should be last line of script.
     #You can also type this at command line after the script exits.

elif 0: #Same but start as interactive session, needs Remote(Wx)engine.
   try:
     type(matplotlib)
   except NameError:
     import matplotlib
     matplotlib.interactive(True)
     from matplotlib.pylab import *

demoplot()
   show() #Open the figure, let figure GUI take over.
     #This should be last line of script.
     #You can also type this at command line after the script exits.

elif 0:#pdf output, allows use of Remote engine without re-initialization.
   #Disadvantage: no figure toolbar.
   #WARNING: close the file in acrobat reader before the next run.
   #(Maybe other pdf viewers don't block file overwrite)
   try:
     type(matplotlib)
   except NameError:
     import matplotlib
     matplotlib.use('PDF')
     from pylab import *

demoplot()
   filename='demo_plot'
   savefig(filename)

#view the file:
   import win32api
   win32api.ShellExecute(0, "open", filename+'.pdf', None, "", 1)

elif 1:#png output, allows use of Remote engine without re-initialization.
   #Disadvantage: no figure toolbar.
   #Tip: make Irfanview your standard viewer.
   from pylab import *

demoplot()
   filename='demo_plot'
   savefig(filename)

#view the file:
   import win32api
   win32api.ShellExecute(0, "open", filename+'.png', None, "", 1) 
```

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)


### How do I use PyScripter with TortoiseHg installed? ###

[TortoiseHg](http://bitbucket.org/tortoisehg/stable/wiki/Home) is Windows Explorer extention
for the [Mercurial](http://www.selenic.com/mercurial/wiki/) distributed revision control system.
When installed it affects PyScripter in two ways.
  * It contains a custom python25.dll in its directory which is placed on the windows path
  * It loads various python library dlls in the running space of PyScripter through the PyScripter File Explorer

The above create problems that can be resolved as follows:
  * Solution 1
> > Use PyScripter with a python version different than 2.5 which comes with TortoiseHg.
> > You can force PyScripter to use a specific version of Python using the -PYTHONxx start up flags e.g.
> > ` PyScripter --PYTHON26 `
  * Solution 2
> > If you really need to use Python 2.5 then create a command file with the following
```
        regsvr32 /u "C:\Program Files\TortoiseHg\tortoisehg.dll" 
	PyScripter --PYTHON25 --PYTHONDLLPATH=c:\windows\system32
	regsvr32 "C:\Program Files\TortoiseHg\tortoisehg.dll" 
```
> > Then run PyScripter using this command file.  Note that the PYTHONDLLPATH should point
> > to the directory in which the originall python25.dll that came from the python installation program
> > resides.

**_Update_**
The incompatibility between PyScripter and the TortoiseHg Mercurial addon has finally been fixed with the release
of TortoiseHg 0.8, which replaces the Python Windows shell extensions with C++ based ones.

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### How do I use PyScripter in Ubuntu? ###

This how-to is based on the XFCE 4.8 desktop running on top of Ubuntu 11.04.  The easiest way to get this configuration is to
install [Xubuntu](http://www.xubuntu.org/).  Or from any Ubuntu distribution one can install the XFCE meta package
from Synaptic and choose XFCE from the boot manager at login.

  * First, install Wine.
> > I installed version 1.3.15 via Synaptic.

  * Download the Python 2.7.2 Windows installer. (Other versions should work as well.) Accept the "open with Wine"

> option and proceed with the installation as normal.

  * Download PyScripter.zip from http://code.google.com/p/pyscripter/downloads/list. Unzip it and drag and drop the exe onto your desktop.
> Double click on the exe and Wine will intercept it and run it automagically.

  * Or download the current PyScripter Setup.exe from http://code.google.com/p/pyscripter/downloads/list.  Accept the "open with Wine" option
> and proceed with the installation as normal.  Applications menu>Wine>Programs>PyScripter> "your choice"

Both configurations seem to work flawlessly with the internal and remote python engines.

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### How do I have autocompletion with PyQt4 or PyGtk? ###

  * PyQt4
    * Go to Tools, Options, IDE Options, Special packages and add PyQt4 to the list
    * Tools, Edit Startup Scripts.  Add to the end of pyscripter\_init.pythe following:
> > > `from PyQt4 import QtCore, QtGui`
    * Save and restart PyScripter, go to a new editor and type
> > > `from PyQt4 import QtCore, QtGui`
> > > `QtGui.`


> and voila.  Code completion is available.  See [Support Group message](http://groups.google.com/group/pyscripter/msg/20afb8ec66d80bd5?hl=en) for explanations.

  * PyGtk
    * Similar to the above.  See [Support Group message](https://groups.google.com/d/msg/pyscripter/j6YY3VUiyTk/mY9jIZG_24UJ) for details.



[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)

### Why am I getting UnicodeEncodeError/UnicodeDecodeError? ###

Most likely you are using scripts with file names containing non-ascii characters.  Please see my [blog post](http://pyscripter.blogspot.com/2012/02/dreaded-unicode-encodedecode-errors.html)
explaining the issue and providing advice for dealing with it.

[Back to Top](http://code.google.com/p/pyscripter/wiki/FAQ#Frequently_Asked_Questions)