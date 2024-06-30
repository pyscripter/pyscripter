:::{index} Django
:::

# Debugging Django Applications

Here is how you can debug [Django](http://www.djangoproject.com/ "http://www.djangoproject.com/") 
applications with Pyscripter in six simple steps:

1. In the File explorer locate the root directory of your Django application. You may want 
to right click on the directory name and select File Explorer, Explore here.

2. Open the project files in Pyscripter (e.g., models,py, views.py etc.) and set whatever 
breakpoints you want.

3. Select Run, Command Line Parameters... and set the command line to "runserver --noreload". 
Also check the "Use Command line" checkbox.

4. Make sure the remote engine is selected (Run, Python Engine, Remote).

5. Open the manage.py file and press the debug button (or press F9).

6. Start a web browser and test your application. Pyscripter should now stop at whatever 
breakpoints you have set and you can use the various debugging facilities (call stack, 
variables, interpreter prompt etc.)  To stop debugging, right-click on the interpreter 
window and select "Reinitialize Interpreter", then go to the browser and reload the document.
