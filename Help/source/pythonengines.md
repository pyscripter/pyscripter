:::{index} Python Engines
:::

# Python Engines

In addition  to using the internal integrared Python engine, PyScripter offers you the option 
to use one of three remote Python engines. These remote engines run in  a separate process, 
so, when using them, script errors should not affect the stability of PyScripter. You can select 
the python engine that will  be active from the Python Engine submenu of the 
[Run menu](runmenu).  Here is a brief explanation of the Python engine options:  

  
:::{index} Python Engines; Internal
:::
## Internal (deprecated)

It is faster than the other options however if there are problems with the scripts you are 
running or debugging they could affect the stability of PyScripter and could 
cause crashes. Another limitation of this engine is that it cannot run or debug 
GUI scripts nor it can be reinitialized. Since version 3.1 the internal 
Python engine is hidden by default. This is controled by the
[IDE option](ideoptions) Python Interpreter, "Internal Interpreter hidden".

  
:::{index} Python Engines; Remote
:::
 ## Remote

This the default engine of PyScripter and is the recommended engine for most Python development 
tasks. It runs in a child process and communicates with PyScripter using
[rpyc](https://github.com/tomerfiliba/rpyc).  Rpyc is bundled with the PyScripter destribution 
and no separate  installation is required. It can be used to run and debug any kind of script. 
However *if you  run or debug GUI scripts it is a good idea to reinitialize the engine before 
each run. This is done automatically by default.*

:::{index} Python Engines; Remote Tk
:::
## Remote Tk

This Python engine is specifically designed to run and debug Tkinter applications 
including [matplotlib](http://matplotlib.org/) 
using the TkAgg backend. It also supports running pylab in interactive mode. 
The engine activates a Tkinter mainloop and replaces the mainloop with a dummy 
function so that the Tkinter scripts you are running or debugging do not block 
the engine. You may even develop and test Tkinter widgets using the interactive 
console.

:::{index} Python Engines; Remote wx
:::
## Remote Wx

This Python engine is specifically  designed to run and debug 
[wxPython](http://www.wxpython.org/) applications including 
[matplotlib](http://matplotlib.org/) using the Wx and WxAgg backends. 
It also supports running pylab in interactive  mode. The engine activates a 
wx MainLoop and replaces the MainLoop with a dummy function so that the 
wxPython scripts you are running or debugging do not block the engine. 
You may even develop and test wxPython Frames and Apps using the interactive 
console.
 
:::{index} Python Engines; SSH
:::
## SSH Engine

This engine type runs a python interpreter in a remote Windows or Linux machine 
or inside a virtual environment (servers). You first need to define 
one or more SSH servers as explained in the topic [Working with Remote Files](remotefiles). This topic also describes the requirements for using SSH with PyScripter. 
Once you choose this type of engine you need to select a defined SSH server. PyScripter starts a python engine on the remote server using SSH and communicates with it using 
[rpyc](https://github.com/tomerfiliba/rpyc).

You can then run and debug remote or local scripts on the SSH server as if the scripts 
were running locally. You can also use python running inside the SSH 
server with the [Python Interactive Interpreter](interpreter). While debugging, tracing into remote modules works transparently for the user. The local and remote versions do not need to be the same.


**Note:**\
When using the Tk and Wx remote engines you can of course run or debug any other non-GUI 
Python script. However bear in mind that these engines may be slightly 
slower than the standard remote engine since they also contain a GUI main loop. 
Also note that these two engines override the sys.exit function with a dummy 
procedure.

**Debugging Wx and Tkinter scirpts using the remote Wx and Tk engines**

As mentioned above the Wx and Tk engines activate a main loop and replace the MainLoop with 
a dummy function. Therefore, when debugging Gui scripts using these engines, 
as soon as you reach the MainLoop statement debugging ends and you can then 
test the running application but without further debugging support. This 
means two things: 
- Breakpoints and debugging would work up to the point the script enters the MainLoop routine
- You  will not be able to debug event triggered code using these two engines.

To debug event code of Wx and Tkinter scripts use the standard remote engine. You may 
wonder why should you ever use the Wx and Tk specific remote engines. Here is 
a few reasons:

- These engine allow you to interactively develop and test frames and widgets. 
  (possible because they run their own main loop).
- They support running pylab in interactive mode like IPython does, which was a request 
  from many Pyscripter users. 
- There  is no need to reinitialize the engines after running Gui scripts.
- Pyscripter  does not stay in running mode while the Gui Windows are showing but instead 
  it returns in ready mode allowing further work and runs.

**Troubleshooting**

If the remote Python engine becomes unresponsive you can try to reinitialize the 
engine from the Python Engine submenu of the [Run menu](runmenu) 
(also available in the context menu of the [Interactive Interpreter](interpreter)).

If Pyscripter fails to start or appears locked when starting this may be due to 
remote python engines from earlier runs still being active. This could 
happen after a Pyscripter crash. In such cases you should kill the python 
engines using the Windows Task Managers. Look in the Processes tab for 
processes with image name "python.exe".



