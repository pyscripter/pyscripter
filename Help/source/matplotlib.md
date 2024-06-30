:::{index} matplotlib
:::

# Using matplotlib with PyScripter

You can run python scripts using  [matplotlib](https://matplotlib.org/) with the 
default [Remote Python Engine](pythonengines) without any issues.  The python
engine needs to be reinitialized each time you run a script, but PyScripter does this
automatically.

You can also run matplotlib in interactive mode by using the specialized remote engines:
- Remote Tk
- Remote Wx

Here is how:

1. Choose the right Python engine (Run, Python Engine menu option) for the backend of your 
   choice e.g. Remote engine Tk for the "TkAgg" backend or Remote Engine wx for the "WX" and 
   "WxAggg" backends.  
2. Assuming that you have selected the Remote Engine wx, issue the following commands in the 
   interpreter:
   ```python
   >>> import matplotlib
   >>> matplotlib.use("WxAgg")  # You could use instead the TkAgg backend
   >>> matplotlib.interactive(True)
   >>> from matplotlib.pylab import *
   >>> plot([1,2,3])
   >>> xlabel('time (s)') 
   ```
  
3. You can set the backend and interactive mode in the matplotlibrc file. If this is done, the following 
  is sufficient for the above example:  
   ```python
   >>> from pylab import *
   >>> plot([1,2,3])
   >>> xlabel('time (s)')
   ```

3. Issue more pylab commands, or close the pylab window and call plot again for a new plot, etc.   

---

**Sample script by heylam**  

The following python script shows different ways of using matplotlib with PyScripter.

```python
#Assumed setup: matplotlib and wxpython are installed with pip
import matplotlib
from pylab import *
matplotlib.use("WxAgg")

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

    from pylab import *

    demoplot()
    show() #Open the figure, let figure GUI take over.
    #This should be last line of script.
    #You can also type this at command line after the script exits.

elif 0:
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

    demoplot()
    filename='demo_plot'
    savefig(filename)

    #view the file:
    import win32api
    win32api.ShellExecute(0, "open", filename+'.pdf', None, "", 1)

elif 1:#png output, allows use of Remote engine without re-initialization.
    #Disadvantage: no figure toolbar.
    demoplot()
    filename='demo_plot'
    savefig(filename)

    #view the file:
    import win32api
    win32api.ShellExecute(0, "open", filename+'.png', None, "", 1)
```
