object ResourcesDataModule: TResourcesDataModule
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 376
  Width = 746
  object dlgFileOpen: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 20
    Top = 22
  end
  object dlgFileSave: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 103
    Top = 22
  end
  object PrintDialog: TPrintDialog
    MaxPage = 10000
    Options = [poPageNums]
    Left = 187
    Top = 22
  end
  object PythonScripts: TJvMultiStringHolder
    MultipleStrings = <
      item
        Name = 'InitScript'
        Strings.Strings = (
          'import sys'
          'import code'
          ''
          'class PythonInteractiveInterpreter(code.InteractiveInterpreter):'
          '    debugIDE = __import__("DebugIDE")'
          ''
          '    class IDEDebugger(__import__('#39'bdb'#39').Bdb):'
          '        debugIDE = __import__("DebugIDE")'
          ''
          '        def do_clear(self, arg):'
          '            numberlist = arg.split()'
          '            for i in numberlist:'
          '                self.clear_bpbynumber(i)'
          ''
          '        def stop_here(self, frame):'
          '            import bdb'
          '            if not self.InitStepIn:'
          '                self.InitStepIn = True'
          '                self.set_continue()'
          '                return 0'
          '            return bdb.Bdb.stop_here(self, frame)'
          ''
          '        def user_call(self, frame, args):'
          '            self.debugIDE.user_call(frame, args)'
          ''
          '        def user_line(self, frame):'
          '            self.debugIDE.user_line(frame)'
          ''
          '        def user_exception(self, frame, exc_stuff):'
          '            self.debugIDE.user_exception(frame, exc_stuff)'
          ''
          '        def trace_dispatch(self, frame, event, arg):'
          '            self.tracecount += 1'
          
            '            if self.tracecount == 30:  #yield processing every 3' +
            '0 steps'
          '                self.tracecount = 0'
          '                self.debugIDE.user_yield()'
          
            '                if self.quitting: raise __import__('#39'bdb'#39').BdbQui' +
            't'
          
            '            return __import__('#39'bdb'#39').Bdb.trace_dispatch(self, fr' +
            'ame, event, arg)'
          ''
          '        def run(self, cmd, globals=None, locals=None):'
          '            import bdb'
          '            import sys'
          '            import types'
          ''
          '            if globals is None:'
          '                globals = self.locals'
          ''
          '            self.saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          ''
          '            if locals is None:'
          '                locals = globals'
          ''
          '            globals["__name__"] = '#39'__main__'#39
          '            if isinstance(cmd, types.CodeType):'
          '                globals["__file__"] = cmd.co_filename'
          '            else:'
          '                cmd = cmd+'#39'\n'#39
          ''
          '            try:'
          '                try:'
          '                    bdb.Bdb.run(self, cmd, globals, locals)'
          '                except SystemExit as e:'
          '                    if isinstance(e.code, str):'
          '                        print(e.code)'
          '                    elif isinstance(e.code, int):'
          '                        print("Exit code: ", e.code)'
          '            finally:'
          
            '                sys.stdin, sys.stdout, sys.stderr = self.saveStd' +
            'io'
          '                if '#39'__file__'#39' in globals:'
          '                    del globals['#39'__file__'#39']'
          '                __import__("gc").collect()'
          ''
          '    class IDETestResult(__import__('#39'unittest'#39').TestResult):'
          '        debugIDE = __import__("DebugIDE")'
          ''
          '        def startTest(self, test):'
          
            '            __import__('#39'unittest'#39').TestResult.startTest(self, te' +
            'st)'
          '            self.debugIDE.testResultStartTest(test)'
          ''
          '        def stopTest(self, test):'
          
            '            __import__('#39'unittest'#39').TestResult.stopTest(self, tes' +
            't)'
          '            self.debugIDE.testResultStopTest(test)'
          ''
          '        def addError(self, test, err):'
          
            '            __import__('#39'unittest'#39').TestResult.addError(self, tes' +
            't, err)'
          
            '            self.debugIDE.testResultAddError(test, self._exc_inf' +
            'o_to_string(err, test))'
          ''
          '        def addFailure(self, test, err):'
          
            '            __import__('#39'unittest'#39').TestResult.addFailure(self, t' +
            'est, err)'
          
            '            self.debugIDE.testResultAddFailure(test, self._exc_i' +
            'nfo_to_string(err, test))'
          ''
          '        def addSuccess(self, test):'
          '            self.debugIDE.testResultAddSuccess(test)'
          
            '            __import__('#39'unittest'#39').TestResult.addSuccess(self, t' +
            'est)'
          ''
          '    def __init__(self, locals = None):'
          '        code.InteractiveInterpreter.__init__(self, locals)'
          '        self.locals["__name__"] = "__main__"'
          ''
          '        self.debugger = self.IDEDebugger()'
          '        self.debugger.InitStepIn = False'
          '        self.debugger.tracecount = 0'
          '        self.debugger.currentframe = None'
          '        self.debugger.locals = self.locals'
          ''
          '        try:'
          '            pyrepr = __import__('#39'repr'#39').Repr()'
          '        except:'
          '            pyrepr = __import__('#39'reprlib'#39').Repr()'
          '        pyrepr.maxstring = 60'
          '        pyrepr.maxother = 60'
          '        self._repr = pyrepr.repr'
          ''
          '        self.commontypes = frozenset(['
          '              '#39'NoneType'#39','
          '              '#39'NotImplementedType'#39','
          '              '#39'bool'#39','
          '              '#39'buffer'#39','
          '              '#39'builtin_function_or_method'#39','
          '              '#39'code'#39','
          '              '#39'complex'#39','
          '              '#39'dict'#39','
          '              '#39'dictproxy'#39','
          '              '#39'ellipsis'#39','
          '              '#39'file'#39','
          '              '#39'float'#39','
          '              '#39'frame'#39','
          '              '#39'function'#39','
          '              '#39'generator'#39','
          '              '#39'getset_descriptor'#39','
          '              '#39'instancemethod'#39','
          '              '#39'int'#39','
          '              '#39'list'#39','
          '              '#39'long'#39','
          '              '#39'member_descriptor'#39','
          '              '#39'method-wrapper'#39','
          '              '#39'object'#39','
          '              '#39'slice'#39','
          '              '#39'str'#39','
          '              '#39'traceback'#39','
          '              '#39'tuple'#39','
          '              '#39'unicode'#39','
          '              '#39'xrange'#39'])'
          ''
          '    def _import(self, name, code):'
          '        import imp'
          '        import sys'
          '        mod = imp.new_module(name)'
          '        mod.__file__ = code.co_filename'
          ''
          '        try:'
          '            exec(code, mod.__dict__)'
          '            sys.modules[name] = mod'
          '            return mod'
          '        except SystemExit:'
          '            pass'
          ''
          '    def run_nodebug(self, cmd, globals=None, locals=None):'
          '        import types'
          '        import sys'
          ''
          '        if globals is None:'
          '            globals = self.locals'
          ''
          '        self.saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          ''
          '        if locals is None:'
          '            locals = globals'
          ''
          '        globals["__name__"] = '#39'__main__'#39
          '        if isinstance(cmd, types.CodeType):'
          '            globals["__file__"] = cmd.co_filename'
          '        else:'
          '            cmd = cmd+'#39'\n'#39
          ''
          '        try:'
          '            try:'
          '                exec(cmd, globals, locals)'
          '            except SystemExit as e:'
          '                if isinstance(e.code, str):'
          '                    print(e.code)'
          '                elif isinstance(e.code, int):'
          '                    print("Exit code: ", e.code)'
          '        finally:'
          '            sys.stdin, sys.stdout, sys.stderr = self.saveStdio'
          '            if '#39'__file__'#39' in globals:'
          '                del globals['#39'__file__'#39']'
          '            __import__("gc").collect()'
          ''
          '    def objecttype(self, ob):'
          '        try:'
          '            try:'
          '                return ob.__class__.__name__'
          '            except:'
          '                return type(ob).__name__'
          '        except:'
          '            return "Unknown type"'
          ''
          '    def objectinfo(self, ob):'
          '        res = 1'
          '        try:'
          '            import inspect'
          
            '            if hasattr(ob, "__dict__") and isinstance(ob.__dict_' +
            '_, dict):'
          '                res = res | 2'
          '            if inspect.ismodule(ob):'
          '                res = res | 4'
          
            '            elif inspect.ismethod(ob) or inspect.ismethoddescrip' +
            'tor(ob):'
          '                res = res | 8'
          
            '            elif inspect.isfunction(ob) or inspect.isbuiltin(ob)' +
            ':'
          '                res = res | 16'
          '            elif inspect.isclass(ob):'
          '                res = res | 32'
          '            elif isinstance(ob, dict):'
          '                res = res | 64'
          '            return res'
          '        except:'
          '            return res'
          ''
          '    def saferepr(self, x):'
          '        try:'
          '            return self._repr(x)'
          '        except:'
          '            return '#39'<unprintable %s object>'#39' % type(x).__name__'
          ''
          
            '    def membercount(self, ob, dictitems = False, expandcommontyp' +
            'es = True, sequenceitems = False):'
          
            '        #  dictitems will be True when used in the Variables win' +
            'dow'
          
            '        #  expandcommontypes will be False when used in the Vari' +
            'ables window'
          '        try:'
          '            if sequenceitems and isinstance(ob, (list, tuple)):'
          '                return len(ob)'
          '            elif dictitems and isinstance(ob, dict):'
          '                return len(ob)'
          
            '            elif not expandcommontypes and (self.objecttype(ob) ' +
            'in self.commontypes):'
          '                return 0'
          '            else:'
          '                return len(set(dir(ob)))'
          '        except:'
          '            return 0'
          ''
          
            '    def _getmembers(self, ob, dictitems = False, expandcommontyp' +
            'es = True, sequenceitems = False):'
          '        result = {}'
          '        if sequenceitems and isinstance(ob, (list, tuple)):'
          '            for i in range(len(ob)):'
          '                result[str(i)] = ob[i]'
          '        elif dictitems and isinstance(ob, dict):'
          '            for (i,j) in ob.items():'
          '                result[self.safestr(i)] = j'
          
            '        elif not expandcommontypes and (self.objecttype(ob) in s' +
            'elf.commontypes):'
          '            pass'
          '        else:'
          '            for i in dir(ob):'
          '                try :'
          '                    result[self.safestr(i)] = getattr(ob, i)'
          '                except:'
          '                    result[self.safestr(i)] = None'
          '        return result'
          ''
          
            '    def safegetmembers(self, ob, dictitems = False, expandcommon' +
            'types = True, sequenceitems = False):'
          '        try:'
          
            '            return self._getmembers(ob, dictitems, expandcommont' +
            'ypes, sequenceitems)'
          '        except:'
          '            return {}'
          ''
          
            '    def safegetmembersfullinfo(self, ob, dictitems = False, expa' +
            'ndcommontypes = True, sequenceitems = False):'
          '        try:'
          
            '            members = self._getmembers(ob, dictitems, expandcomm' +
            'ontypes, sequenceitems)'
          '            d = {}'
          '            for (i,j) in members.items():'
          
            '                d[i] = (j, self.objecttype(j), self.objectinfo(j' +
            '))'
          '            if sequenceitems and isinstance(ob, (list, tuple)):'
          
            '                return tuple(sorted(d.items(), key = lambda r: i' +
            'nt(r[0])))'
          '            else:'
          '                return tuple(d.items())'
          '        except:'
          '            return ()'
          ''
          '    def safestr(self, value):'
          '        try:'
          '            return str(value)'
          '        except:'
          '            return self.saferepr(value)'
          ''
          '    def find_dotted_module(self, name, path=None):'
          '        import imp'
          '        segs = name.split('#39'.'#39')'
          '        file = None'
          '        while segs:'
          '            if file: file.close()'
          
            '            file, filename, desc = imp.find_module(segs[0], path' +
            ')'
          '            del segs[0]'
          '            path = [filename]'
          '        return file, filename, desc'
          ''
          ''
          '    def findModuleOrPackage(self, modName, path=None):'
          '        if path is None:'
          '            import sys'
          '            path = sys.path'
          '        try:'
          
            '            f, filename, (ext, mode, type) =  self.find_dotted_m' +
            'odule(modName, path)'
          '        except ImportError as err:'
          '            return None'
          ''
          '        if f is not None:'
          '            f.close()'
          '        if filename:'
          '            import imp, os'
          '            if type == imp.PKG_DIRECTORY:'
          '                return os.path.join(filename, '#39'__init__.py'#39')'
          '            elif type in (imp.PY_SOURCE, imp.C_EXTENSION):'
          '                return filename'
          ''
          '    def getmodules(self, path=None):'
          '        import sys'
          '        try:'
          '            import pkgutil'
          '            l = [i[1] for i in pkgutil.iter_modules(path)]'
          '        except:'
          '            l = []'
          '        if path is None:'
          '            l.extend(sys.builtin_module_names)'
          '        return l'
          ''
          '    def runcode(self, code):'
          '        import sys'
          '        self.saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          '        try:'
          '            if self.debugger.currentframe:'
          
            '                exec(code, self.debugger.currentframe.f_globals,' +
            ' self.debugger.currentframe.f_locals)'
          '                # save locals'
          '                try:'
          '                    import ctypes'
          
            '                    ctypes.pythonapi.PyFrame_LocalsToFast(ctypes' +
            '.py_object(self.debugger.currentframe), 0)'
          '                except :'
          '                    pass'
          '            else:'
          '                exec(code, self.locals)'
          '        except SystemExit as e:'
          '            if isinstance(e.code, str):'
          '                print(e.code)'
          '            elif isinstance(e.code, int):'
          '                print("Exit code: ", e.code)'
          '        except:'
          '            self.showtraceback()'
          '        sys.stdin, sys.stdout, sys.stderr = self.saveStdio'
          ''
          '    def evalcode(self, code):'
          '        # may raise exceptions'
          '        try:'
          '            if self.debugger.currentframe:'
          
            '                return eval(code, self.debugger.currentframe.f_g' +
            'lobals, self.debugger.currentframe.f_locals)'
          '            else:'
          '                return eval(code, self.locals)'
          '        except SystemExit:'
          '            return None'
          ''
          '    def get_arg_text(self, ob):'
          
            '        "Get a string describing the arguments for the given obj' +
            'ect - From IDLE"'
          '        import types'
          
            '        from inspect import isclass, isroutine, signature, getdo' +
            'c'
          '        argText = ""'
          '        if ob is not None:'
          '            argOffset = 0'
          '            if isclass(ob) and (ob.__module__ != '#39'builtins'#39'):'
          
            '                # Look for the highest __init__ in the class cha' +
            'in.'
          '                fob = getattr(ob, '#39'__init__'#39', ob)'
          '                argOffset = 1'
          '            elif type(ob)==types.MethodType:'
          
            '                # bit of a hack for methods - turn it into a fun' +
            'ction'
          '                # but we drop the "self" param.'
          '                fob = ob.__func__'
          '                argOffset = 1'
          '            else:'
          '                fob = ob'
          '            # Try and build one for Python defined functions'
          '            if isroutine(fob):'
          '                try:'
          '                    argText = str(signature(fob))[1:]'
          '                    idx = argText.rfind(") ->")'
          '                    if idx < 0:'
          '                        idx = argText.rfind(")")'
          '                    argText = argText[0:idx]'
          '                except:'
          '                    pass'
          '            return (argText, getdoc(fob))'
          ''
          '    def Win32RawInput(self, prompt=None):'
          '        "Provide raw_input() for gui apps"'
          '        # flush stderr/out first.'
          '        debugIDE = __import__("DebugIDE")'
          '        import sys'
          '        try:'
          '            sys.stdout.flush()'
          '            sys.stderr.flush()'
          '        except:'
          '            pass'
          '        if prompt is None:'
          '            prompt = ""'
          '        else:'
          '            sys.stdout.write(prompt)'
          '            sys.stdout.flush'
          '        ret = debugIDE.InputBox('#39'Python input'#39', str(prompt),"")'
          '        if ret is not None:'
          '            sys.stdout.write(ret)'
          '        sys.stdout.write("\n")'
          '        sys.stdout.flush'
          '        if ret is None:'
          '            raise KeyboardInterrupt("Operation cancelled")'
          '        return ret'
          ''
          '    def setupdisplayhook(self):'
          '        import sys, pprint, builtins'
          '        def pphook(value, show=pprint.pprint, bltin=builtins):'
          '            if value is not None:'
          '                bltin._ = value'
          '                show(value)'
          '        sys.displayhook = pphook'
          ''
          '    def system_command(self, cmd):'
          '        import subprocess'
          
            '        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, ' +
            'shell=True)'
          '        print(process.communicate()[0].decode())'
          '        if process.returncode != 0:'
          '            print('#39'Error code: '#39', process.returncode)'
          ''
          '    def htmldoc(self, module):'
          '        import sys'
          '        import pydoc'
          '        class _HTMLDoc(pydoc.HTMLDoc):'
          '            def page(self, title, contents):'
          '                """Format an HTML page."""'
          '                css = '#39#39
          '                if sys.version_info[0:2] >= (3, 11):'
          '                    import os'
          
            '                    css_path = os.path.join(sys.base_prefix, "Li' +
            'b", "pydoc_data", "_pydoc.css")'
          '                    if os.path.exists(css_path):'
          '                        with open(css_path, '#39'r'#39') as file:'
          
            '                            css = "<style>\n" + file.read() + "\' +
            'n</style>"'
          ''
          '                return '#39#39#39'\'
          '<!DOCTYPE>'
          '<html lang="en">'
          '<head>'
          '<meta charset="utf-8">'
          '<title>Pydoc: %s</title>'
          '%s'
          '</head>'
          '<body>'
          '%s'
          '</body>'
          '</html>'#39#39#39' % (title, css, contents)'
          ''
          '        htmldoc = _HTMLDoc()'
          
            '        html = htmldoc.page(pydoc.describe(module), htmldoc.docu' +
            'ment(module))'
          '        return html'
          ''
          '_II = PythonInteractiveInterpreter(globals())'
          ''
          'sys.modules['#39'builtins'#39'].input=_II.Win32RawInput'
          ''
          'import os'
          'try:'
          '    sys.path.remove(os.path.dirname(sys.executable))'
          '    sys.path.remove(os.path.dirname(sys.executable))'
          'except:'
          '    pass'
          ''
          'import warnings'
          'warnings.simplefilter("ignore", DeprecationWarning)'
          ''
          'del DebugOutput'
          'del code'
          'del PythonInteractiveInterpreter'
          'del sys'
          'del os'
          'del warnings')
      end
      item
        Name = 'RpyC_Init'
        Strings.Strings = (
          'import sys'
          'import code'
          'import threading'
          '#import logging'
          ''
          '##logging.basicConfig(level=logging.DEBUG,'
          '##                    filename = "test.log",'
          '##                    filemode = "a",'
          '##                    format='#39'(%(threadName)-10s) %(message)s'#39')'
          ''
          'class RemotePythonInterpreter(code.InteractiveInterpreter):'
          '    class DebugManager:'
          '        # Debugger commands'
          
            '        dcNone, dcRun, dcStepInto, dcStepOver, dcStepOut, dcRunT' +
            'oCursor, dcPause, dcAbort = range(8)'
          '        debug_command = dcNone'
          ''
          '        _threading = threading'
          '        # IDE synchronization lock'
          '        user_lock = threading.Lock()'
          ''
          '        # Thread status'
          '        thrdRunning, thrdBroken, thrdFinished = range(3)'
          '        main_thread_id = threading.current_thread().ident'
          ''
          '        # module for communication with the IDE'
          '        debugIDE = None #will be set to P4D module'
          ''
          '        # shared debugger breakpoints'
          '        breakpoints = {}'
          ''
          '        # main debugger will be set below'
          '        main_debugger = None'
          ''
          '        #active debugger objects'
          '        active_thread = active_frame = None'
          ''
          '        #sleep function'
          '        import time'
          '        _sleep = time.sleep'
          ''
          '        @classmethod'
          '        def thread_status(cls, ident, name, status):'
          '            with cls.user_lock:'
          '                cls.debugIDE.user_thread(ident, name, status)'
          ''
          '    class ThreadWrapper(threading.Thread):'
          '        """ Wrapper class for threading.Thread. """'
          ''
          '        def run(self):'
          
            '            self.debug_manager.thread_status(self.ident, self.na' +
            'me, self.debug_manager.thrdRunning)'
          ''
          
            '            self.debugger = self.debug_manager.main_debugger.__c' +
            'lass__()'
          '            self.debugger.reset()'
          
            '            self.debugger._sys.settrace(self.debugger.trace_disp' +
            'atch)'
          ''
          '            try:'
          '                super().run()'
          '            finally:'
          '                self.debugger._sys.settrace(None)'
          
            '                self.debug_manager.thread_status(self.ident, sel' +
            'f.name, self.debug_manager.thrdFinished)'
          '                self.debugger = None'
          ''
          '    ThreadWrapper.debug_manager = DebugManager'
          ''
          '    class IDEDebugger(__import__('#39'bdb'#39').Bdb):'
          '        def __init__(self):'
          '            super().__init__()'
          '            self.locals = globals()'
          '            self.breaks = self.debug_manager.breakpoints'
          '            self.InitStepIn = False'
          '            self.tracecount = 0'
          '            self._sys = __import__("sys")'
          ''
          '        def showtraceback(self):'
          '            """Display the exception that just occurred.'
          
            '            We remove the first two stack items because it is ou' +
            'r own code.'
          '            """'
          '            import sys, traceback'
          '            try:'
          
            '                sys.last_type, sys.last_value, last_tb = ei = sy' +
            's.exc_info()'
          '                tblist = traceback.extract_tb(ei[2])'
          '                del tblist[:2]'
          '                lines = traceback.format_list(tblist)'
          '                if lines:'
          
            '                    lines.insert(0, "Traceback (most recent call' +
            ' last):\n")'
          
            '                lines.extend(traceback.format_exception_only(ei[' +
            '0], ei[1]))'
          '            finally:'
          '                tblist = tb = None'
          '            sys.stderr.write('#39#39'.join(lines))'
          ''
          '        def do_clear(self, arg):'
          '            numberlist = arg.split()'
          '            for i in numberlist:'
          '                self.clear_bpbynumber(i)'
          ''
          '        def stop_here(self, frame):'
          '            if not self.InitStepIn:'
          '                self.InitStepIn = True'
          '                self.set_continue()'
          '                return 0'
          '            return super().stop_here(frame)'
          ''
          '        def user_line(self, frame):'
          '            try:'
          '                self._sys.stdout.flush()'
          '                self._sys.stderr.flush()'
          '            except:'
          '                pass'
          ''
          '            dbg_manager = self.debug_manager'
          
            '            thread_id = dbg_manager._threading.current_thread().' +
            'ident'
          ''
          '            with dbg_manager.user_lock:'
          
            '                if not dbg_manager.debugIDE.user_line(thread_id,' +
            ' frame, self.botframe):'
          '                    self.set_return(frame)'
          '                    return'
          ''
          '            dbg_manager.debug_command = dbg_manager.dcNone'
          
            '            conn = object.__getattribute__(dbg_manager.debugIDE,' +
            ' "____conn__")'
          ''
          '            while (((thread_id != dbg_manager.active_thread) or'
          
            '                    (dbg_manager.debug_command == dbg_manager.dc' +
            'None)) and'
          
            '                    (dbg_manager.debug_command != dbg_manager.dc' +
            'Run)):'
          '                with dbg_manager.user_lock:'
          '                    conn.poll_all(0.01)'
          '                if thread_id != dbg_manager.active_thread:'
          '                    dbg_manager._sleep(0.1)'
          ''
          '            if dbg_manager.debug_command == dbg_manager.dcRun:'
          '                self.set_continue()'
          
            '            elif dbg_manager.debug_command == dbg_manager.dcStep' +
            'Into:'
          '                self.set_step()'
          
            '            elif dbg_manager.debug_command == dbg_manager.dcStep' +
            'Over:'
          '                self.set_next(frame)'
          
            '            elif dbg_manager.debug_command == dbg_manager.dcStep' +
            'Out:'
          '                self.set_return(frame)'
          
            '            elif dbg_manager.debug_command == dbg_manager.dcRunT' +
            'oCursor:'
          '                self.set_continue()'
          
            '            elif dbg_manager.debug_command == dbg_manager.dcPaus' +
            'e:'
          '                self.set_step()'
          
            '            elif dbg_manager.debug_command == dbg_manager.dcAbor' +
            't:'
          '                self.set_quit()'
          ''
          '            if dbg_manager.debug_command != dbg_manager.dcRun:'
          '                dbg_manager.debug_command = dbg_manager.dcNone'
          
            '            dbg_manager.thread_status(thread_id, "", dbg_manager' +
            '.thrdRunning)'
          ''
          '        def dispatch_call(self, frame, arg):'
          '            self.tracecount += 1'
          '            #check for Pause'
          '            if self.tracecount > 20000:'
          '                self.tracecount = 0'
          '                with self.debug_manager.user_lock:'
          
            '                    cmd = self.debug_manager.debugIDE.user_yield' +
            '()'
          '                if cmd == self.debug_manager.dcPause:'
          '                    self.set_step()'
          '                    return self.trace_dispatch'
          ''
          '            res = super().dispatch_call(frame, arg)'
          '            if res:'
          
            '                if not frame.f_globals.get('#39'__traceable__'#39', True' +
            '):'
          '                    return'
          
            '                #logging.debug("dispatch_call " + frame.f_code.c' +
            'o_filename + " " + frame.f_code.co_name)'
          '            return res'
          ''
          '        def clear_all_breaks(self):'
          '            super().clear_all_breaks()'
          '            self.breaks = self.debug_manager.breakpoints'
          '            self.breaks.clear()'
          ''
          '##        def trace_dispatch(self, frame, event, arg):'
          
            '##            logging.debug(frame.f_code.co_filename + " " + eve' +
            'nt+ " " + frame.f_code.co_name)'
          '##            return super().trace_dispatch(frame, event, arg)'
          ''
          '        def run(self, cmd, globals=None, locals=None):'
          '            import bdb'
          '            import types'
          '            import sys'
          '            import threading'
          ''
          '            if globals is None:'
          '                globals = self.locals'
          ''
          '            saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          ''
          '            if locals is None:'
          '                locals = globals'
          ''
          '            globals["__name__"] = '#39'__main__'#39
          '            if isinstance(cmd, types.CodeType):'
          '                globals["__file__"] = cmd.co_filename'
          '            else:'
          '                cmd = cmd+'#39'\n'#39
          ''
          '            old_thread_class = threading.Thread'
          '            threading.Thread = self.thread_wrapper'
          ''
          '            self.exc_info = None'
          '            try:'
          '                try:'
          '                    bdb.Bdb.run(self, cmd, globals, locals)'
          '                    for t in threading.enumerate():'
          
            '                        if (t != threading.main_thread()) and no' +
            't t.daemon:'
          '                            try:'
          '                                t.join()'
          '                            except:'
          '                                pass'
          ''
          '                except SystemExit as e:'
          '                    if isinstance(e.code, str):'
          '                        print(e.code)'
          '                    elif isinstance(e.code, int):'
          '                        print("Exit code: ", e.code)'
          '                except:'
          '                    self.showtraceback()'
          '                    exc_info = sys.exc_info()'
          '                    if hasattr(exc_info[0], "__name__"):'
          '                        name = exc_info[0].__name__'
          '                    elif type(exc_info[0]) == str:'
          '                        name = exc_info[0]'
          '                    else:'
          '                        name = ""'
          
            '                    self.exc_info = (name, exc_info[1], exc_info' +
            '[2])'
          '            finally:'
          '                sys.stdin, sys.stdout, sys.stderr = saveStdio'
          '                if '#39'__file__'#39' in globals:'
          '                    del globals['#39'__file__'#39']'
          '                __import__("gc").collect()'
          '                threading.Thread = old_thread_class'
          '                try:'
          '                    sys.stdout.flush()'
          '                    sys.stderr.flush()'
          '                except:'
          '                    pass'
          ''
          '    IDEDebugger.debug_manager = DebugManager'
          '    IDEDebugger.thread_wrapper = ThreadWrapper'
          '    DebugManager.main_debugger = IDEDebugger()'
          ''
          '    class IDETestResult(__import__('#39'unittest'#39').TestResult):'
          ''
          '        def startTest(self, test):'
          
            '            __import__('#39'unittest'#39').TestResult.startTest(self, te' +
            'st)'
          
            '            self.debug_manager.debugIDE.testResultStartTest(test' +
            ')'
          ''
          '        def stopTest(self, test):'
          
            '            __import__('#39'unittest'#39').TestResult.stopTest(self, tes' +
            't)'
          '            self.debug_manager.debugIDE.testResultStopTest(test)'
          ''
          '        def addError(self, test, err):'
          
            '            __import__('#39'unittest'#39').TestResult.addError(self, tes' +
            't, err)'
          
            '            self.debug_manager.debugIDE.testResultAddError(test,' +
            ' self._exc_info_to_string(err, test))'
          ''
          '        def addFailure(self, test, err):'
          
            '            __import__('#39'unittest'#39').TestResult.addFailure(self, t' +
            'est, err)'
          
            '            self.debug_manager.debugIDE.testResultAddFailure(tes' +
            't, self._exc_info_to_string(err, test))'
          ''
          '        def addSuccess(self, test):'
          
            '            self.debug_manager.debugIDE.testResultAddSuccess(tes' +
            't)'
          
            '            __import__('#39'unittest'#39').TestResult.addSuccess(self, t' +
            'est)'
          '    IDETestResult.debug_manager = DebugManager'
          ''
          '    def __init__(self, locals = None):'
          '        code.InteractiveInterpreter.__init__(self, locals)'
          '        self.locals["__name__"] = "__main__"'
          '        self.inspect = __import__("inspect")'
          '        self.exc_info = None'
          ''
          '        try:'
          '            pyrepr = __import__('#39'repr'#39').Repr()'
          '        except:'
          '            pyrepr = __import__('#39'reprlib'#39').Repr()'
          '        pyrepr.maxstring = 60'
          '        pyrepr.maxother = 60'
          '        self._repr = pyrepr.repr'
          ''
          '        self.commontypes = frozenset(['
          '              '#39'NoneType'#39','
          '              '#39'NotImplementedType'#39','
          '              '#39'bool'#39','
          '              '#39'buffer'#39','
          '              '#39'builtin_function_or_method'#39','
          '              '#39'code'#39','
          '              '#39'complex'#39','
          '              '#39'dict'#39','
          '              '#39'dictproxy'#39','
          '              '#39'ellipsis'#39','
          '              '#39'file'#39','
          '              '#39'float'#39','
          '              '#39'frame'#39','
          '              '#39'function'#39','
          '              '#39'generator'#39','
          '              '#39'getset_descriptor'#39','
          '              '#39'instancemethod'#39','
          '              '#39'int'#39','
          '              '#39'list'#39','
          '              '#39'long'#39','
          '              '#39'member_descriptor'#39','
          '              '#39'method-wrapper'#39','
          '              '#39'object'#39','
          '              '#39'slice'#39','
          '              '#39'str'#39','
          '              '#39'traceback'#39','
          '              '#39'tuple'#39','
          '              '#39'unicode'#39','
          '              '#39'xrange'#39'])'
          ''
          ''
          '    def saferepr(self, ob):'
          '        try:'
          '            return self._repr(ob)'
          '        except:'
          '            return '#39'<unprintable %s object>'#39' % type(ob).__name__'
          ''
          
            '    def membercount(self, ob, dictitems = False, expandcommontyp' +
            'es = True, sequenceitems = False):'
          
            '        #  dictitems will be True when used in the Variables win' +
            'dow'
          
            '        #  expandcommontypes will be False when used in the Vari' +
            'ables window'
          '        try:'
          '            if sequenceitems and isinstance(ob, (list, tuple)):'
          '                return len(ob)'
          '            elif dictitems and isinstance(ob, dict):'
          '                return len(ob)'
          
            '            elif not expandcommontypes and (self.objecttype(ob) ' +
            'in self.commontypes):'
          '                return 0'
          '            else:'
          '                return len(set(dir(ob)))'
          '        except:'
          '            return 0'
          ''
          
            '    def _getmembers(self, ob, dictitems = False, expandcommontyp' +
            'es = True, sequenceitems = False):'
          '        result = {}'
          '        if sequenceitems and isinstance(ob, (list, tuple)):'
          '            for i in range(len(ob)):'
          '                result[str(i)] = ob[i]'
          '        elif dictitems and isinstance(ob, dict):'
          '            for (i,j) in ob.items():'
          '                result[self.safestr(i)] = j'
          
            '        elif not expandcommontypes and (self.objecttype(ob) in s' +
            'elf.commontypes):'
          '            pass'
          '        else:'
          '            for i in dir(ob):'
          '                try :'
          '                    result[self.safestr(i)] = getattr(ob, i)'
          '                except:'
          '                    result[self.safestr(i)] = None'
          '        return result'
          ''
          
            '    def safegetmembers(self, ob, dictitems = False, expandcommon' +
            'types = True, sequenceitems = False):'
          '        try:'
          
            '            return self._getmembers(ob, dictitems, expandcommont' +
            'ypes, sequenceitems)'
          '        except:'
          '            return {}'
          ''
          
            '    def safegetmembersfullinfo(self, ob, dictitems = False, expa' +
            'ndcommontypes = True, sequenceitems = False):'
          '        try:'
          
            '            members = self._getmembers(ob, dictitems, expandcomm' +
            'ontypes, sequenceitems)'
          '            d = {}'
          '            for (i,j) in members.items():'
          
            '                d[i] = (j, self.objecttype(j), self.objectinfo(j' +
            '))'
          '            if sequenceitems and isinstance(ob, (list, tuple)):'
          
            '                return tuple(sorted(d.items(), key = lambda r: i' +
            'nt(r[0])))'
          '            else:'
          '                return tuple(d.items())'
          '        except:'
          '            return ()'
          ''
          '    def safestr(self, value):'
          '        try:'
          '            return str(value)'
          '        except:'
          '            return self.saferepr(value)'
          ''
          '    def find_dotted_module(self, name, path=None):'
          '        import imp'
          '        segs = name.split('#39'.'#39')'
          '        file = None'
          '        while segs:'
          '            if file: file.close()'
          
            '            file, filename, desc = imp.find_module(segs[0], path' +
            ')'
          '            del segs[0]'
          '            path = [filename]'
          '        return file, filename, desc'
          ''
          '    def findModuleOrPackage(self, modName, path=None):'
          '        if path is None:'
          '            import sys'
          '            path = sys.path'
          '        try:'
          
            '            f, filename, (ext, mode, type) =  self.find_dotted_m' +
            'odule(modName, path)'
          '        except ImportError as err:'
          '            return None'
          ''
          '        if f is not None:'
          '            f.close()'
          '        if filename:'
          '            import imp, os'
          '            if type == imp.PKG_DIRECTORY:'
          '                return os.path.join(filename, '#39'__init__.py'#39')'
          '            elif type in (imp.PY_SOURCE, imp.C_EXTENSION):'
          '                return filename'
          ''
          '    def getmodules(self, path=None):'
          '        import sys'
          '        try:'
          '            import pkgutil'
          '            l = [i[1] for i in pkgutil.iter_modules(path)]'
          '        except:'
          '            l = []'
          '        if path is None:'
          '            l.extend(sys.builtin_module_names)'
          '        return tuple(l)'
          ''
          '    def showsyntaxerror(self, filename=None):'
          '        import sys, code'
          '        old_excepthook = sys.excepthook'
          '        sys.excepthook = sys.__excepthook__'
          '        try:'
          '            super().showsyntaxerror(filename)'
          '        finally:'
          '            sys.excepthook = old_excepthook'
          '            try:'
          '                sys.stdout.flush()'
          '                sys.stderr.flush()'
          '            except:'
          '                pass'
          ''
          
            '    def runsource(self, source, filename="<input>", symbol="sing' +
            'le"):'
          '        import sys'
          '        saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          '        try:'
          '            return super().runsource(source, filename, symbol)'
          '        finally:'
          '            sys.stdin, sys.stdout, sys.stderr = saveStdio'
          '            try:'
          '                sys.stdout.flush()'
          '                sys.stderr.flush()'
          '            except:'
          '                pass'
          ''
          '    def runcode(self, code):'
          '        import sys'
          ''
          '        try:'
          '            if self.DebugManager.active_frame:'
          
            '                exec(code, self.DebugManager.active_frame.f_glob' +
            'als, self.DebugManager.active_frame.f_locals)'
          '                # save locals'
          '                try:'
          '                    import ctypes'
          
            '                    ctypes.pythonapi.PyFrame_LocalsToFast(ctypes' +
            '.py_object(self.DebugManager.active_frame), 0)'
          '                except :'
          '                    pass'
          '            else:'
          '              exec(code, self.locals)'
          '        except SystemExit as e:'
          '            if isinstance(e.code, str):'
          '                print(e.code)'
          '            elif isinstance(e.code, int):'
          '                print("Exit code: ", e.code)'
          '        except:'
          '            self.showtraceback()'
          ''
          '    def evalcode(self, code):'
          '        # may raise exceptions'
          '        try:'
          '            if self.DebugManager.active_frame:'
          
            '                return eval(code, self.DebugManager.active_frame' +
            '.f_globals, self.DebugManager.active_frame.f_locals)'
          '            else:'
          '                return eval(code, self.locals)'
          '        except SystemExit:'
          '            return None'
          ''
          '    def get_arg_text(self, ob):'
          
            '        "Get a string describing the arguments for the given obj' +
            'ect - From IDLE"'
          '        import types'
          
            '        from inspect import isclass, isroutine, signature, getdo' +
            'c'
          '        argText = ""'
          '        if ob is not None:'
          '            argOffset = 0'
          '            if isclass(ob) and (ob.__module__ != '#39'builtins'#39'):'
          
            '                # Look for the highest __init__ in the class cha' +
            'in.'
          '                fob = getattr(ob, '#39'__init__'#39', ob)'
          '                argOffset = 1'
          '            elif type(ob)==types.MethodType:'
          
            '                # bit of a hack for methods - turn it into a fun' +
            'ction'
          '                # but we drop the "self" param.'
          '                fob = ob.__func__'
          '                argOffset = 1'
          '            else:'
          '                fob = ob'
          '            # Try and build one for Python defined functions'
          '            if isroutine(fob):'
          '                try:'
          '                    argText = str(signature(fob))[1:]'
          '                    idx = argText.rfind(") ->")'
          '                    if idx < 0:'
          '                        idx = argText.rfind(")")'
          '                    argText = argText[0:idx]'
          '                except:'
          '                    pass'
          '            return (argText, getdoc(fob))'
          ''
          '    def rem_compile(self, source, fname):'
          '        import sys'
          '        self.exc_info = None'
          '        try:'
          '            return compile(source, fname, "exec")'
          '        except (OverflowError, SyntaxError, ValueError) as e:'
          '            print()'
          '            self.showsyntaxerror(fname)'
          '            exc_info = sys.exc_info()'
          
            '            self.exc_info = (exc_info[0].__name__, exc_info[1], ' +
            'exc_info[2], issubclass(exc_info[0], SyntaxError))'
          ''
          '    def rem_import(self, name, code):'
          '        import imp'
          '        import sys'
          '        mod = imp.new_module(name)'
          '        mod.__file__ = code.co_filename'
          ''
          '        self.exc_info = None'
          '        try:'
          '            exec(code, mod.__dict__)'
          '            sys.modules[name] = mod'
          '            return mod'
          '        except SystemExit:'
          '            pass'
          '        except:'
          '            self.showtraceback()'
          '            exc_info = sys.exc_info()'
          '            if hasattr(exc_info[0], "__name__"):'
          '                name = exc_info[0].__name__'
          '            elif type(exc_info[0]) == str:'
          '                name = exc_info[0]'
          '            else:'
          '                name = ""'
          '            self.exc_info = (name, exc_info[1], exc_info[2])'
          ''
          '    def run_nodebug(self, cmd, globals=None, locals=None):'
          '        import types'
          '        import sys'
          '        import threading'
          ''
          '        if globals is None:'
          '            globals = self.locals'
          ''
          '        saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          ''
          '        if locals is None:'
          '            locals = globals'
          ''
          '        globals["__name__"] = '#39'__main__'#39
          '        if isinstance(cmd, types.CodeType):'
          '            globals["__file__"] = cmd.co_filename'
          '        else:'
          '            cmd = cmd+'#39'\n'#39
          ''
          '        self.exc_info = None'
          '        try:'
          '            try:'
          '                exec(cmd, globals, locals)'
          '                for t in threading.enumerate():'
          
            '                    if (t != threading.main_thread()) and not t.' +
            'daemon:'
          '                        try:'
          '                            t.join()'
          '                        except:'
          '                            pass'
          '            except SystemExit as e:'
          '                if isinstance(e.code, str):'
          '                    print(e.code)'
          '                elif isinstance(e.code, int):'
          '                    print("Exit code: ", e.code)'
          '            except:'
          '                self.showtraceback()'
          '                exc_info = sys.exc_info()'
          '                if hasattr(exc_info[0], "__name__"):'
          '                    name = exc_info[0].__name__'
          '                elif type(exc_info[0]) == str:'
          '                    name = exc_info[0]'
          '                else:'
          '                    name = ""'
          '                self.exc_info = (name, exc_info[1], exc_info[2])'
          '        finally:'
          '            sys.stdin, sys.stdout, sys.stderr = saveStdio'
          '            if '#39'__file__'#39' in globals:'
          '                del globals['#39'__file__'#39']'
          '            __import__("gc").collect()'
          '            try:'
          '                sys.stdout.flush()'
          '                sys.stderr.flush()'
          '            except:'
          '                pass'
          ''
          '    def objecttype(self, ob):'
          '        try:'
          '            try:'
          '                return ob.__class__.__name__'
          '            except:'
          '                return type(ob).__name__'
          '        except:'
          '            return "Unknown type"'
          ''
          '    def objectinfo(self, ob):'
          '        res = 1'
          '        try:'
          '            inspect = self.inspect'
          
            '            if hasattr(ob, "__dict__") and isinstance(ob.__dict_' +
            '_, dict):'
          '                res = res | 2'
          '            if inspect.ismodule(ob):'
          '                res = res | 4'
          
            '            elif inspect.ismethod(ob) or inspect.ismethoddescrip' +
            'tor(ob):'
          '                res = res | 8'
          
            '            elif inspect.isfunction(ob) or inspect.isbuiltin(ob)' +
            ':'
          '                res = res | 16'
          '            elif inspect.isclass(ob):'
          '                res = res | 32'
          '            elif isinstance(ob, dict):'
          '                res = res | 64'
          '            return res'
          '        except:'
          '            return res'
          ''
          '    def rem_chdir(self, path):'
          '        import os'
          '        try:'
          '            os.chdir(path)'
          '        except:'
          '            pass'
          ''
          '    def rem_getcwdu(self):'
          '        import os'
          '        try:'
          '            return os.getcwd()'
          '        except:'
          '            return '#39#39
          ''
          '    def setupdisplayhook(self):'
          '        import sys, pprint, builtins'
          '        def pphook(value, show=pprint.pprint, bltin=builtins):'
          '            if value is not None:'
          '                bltin._ = value'
          '                show(value)'
          '        sys.displayhook = pphook'
          ''
          '    def system_command(self, cmd):'
          '        import sys, os'
          '        res = os.system(cmd)'
          '        if res != 0:'
          '            print('#39'Error code: '#39', res)'
          '        try:'
          '            sys.stdout.flush()'
          '            sys.stderr.flush()'
          '        except:'
          '            pass'
          ''
          '    def Win32RawInput(self, prompt=None, quiet = False):'
          '        "Provide input() for gui apps"'
          '        # flush stderr/out first.'
          '        import sys'
          ''
          '        if sys.stdin != sys.__stdin__:'
          '            return __input__(prompt)'
          ''
          '        try:'
          '            sys.stdout.flush()'
          '            sys.stderr.flush()'
          '        except:'
          '            pass'
          '        if prompt is None:'
          '            prompt = ""'
          '        else:'
          '            if not quiet:'
          '                sys.stdout.write(prompt)'
          '                sys.stdout.flush'
          '        with self.DebugManager.user_lock:'
          
            '            ret = self.DebugManager.debugIDE.InputBox('#39'Python in' +
            'put'#39', str(prompt), "")'
          ''
          '        if not quiet:'
          '            if ret is not None:'
          '                sys.stdout.write(ret)'
          '            sys.stdout.write("\n")'
          '            sys.stdout.flush'
          '        if ret is None:'
          '            raise KeyboardInterrupt("Operation cancelled")'
          '        return ret'
          ''
          '    def readline(self, size=-1):'
          '        ret = self.Win32RawInput(None, True)  + "\n"'
          '        if (size is not None) and (size >= 0):'
          '            ret = ret[:size]'
          '        return ret'
          ''
          '    def htmldoc(self, module):'
          '        import sys'
          '        import pydoc'
          '        class _HTMLDoc(pydoc.HTMLDoc):'
          '            def page(self, title, contents):'
          '                """Format an HTML page."""'
          '                css = '#39#39
          '                if sys.version_info[0:2] >= (3, 11):'
          '                    import os'
          
            '                    css_path = os.path.join(sys.base_prefix, "Li' +
            'b", "pydoc_data", "_pydoc.css")'
          '                    if os.path.exists(css_path):'
          '                        with open(css_path, '#39'r'#39') as file:'
          
            '                            css = "<style>\n" + file.read() + "\' +
            'n</style>"'
          ''
          '                return '#39#39#39'\'
          '<!DOCTYPE>'
          '<html lang="en">'
          '<head>'
          '<meta charset="utf-8">'
          '<title>Pydoc: %s</title>'
          '%s'
          '</head>'
          '<body>'
          '%s'
          '</body>'
          '</html>'#39#39#39' % (title, css, contents)'
          ''
          '        htmldoc = _HTMLDoc()'
          
            '        html = htmldoc.page(pydoc.describe(module), htmldoc.docu' +
            'ment(module))'
          '        return html'
          ''
          '_RPI = RemotePythonInterpreter(globals())'
          ''
          'import os'
          'try:'
          '    sys.path.remove(os.path.dirname(sys.argv[0]))'
          'except:'
          '    pass'
          'sys.path.insert(0, "")'
          ''
          'import builtins'
          'builtins.__input__ = input'
          'builtins.input=_RPI.Win32RawInput'
          'sys.stdin.readline = _RPI.readline'
          ''
          'if sys.version_info >= (3, 7):'
          
            '    sys.stdout.reconfigure(line_buffering=True, write_through=Fa' +
            'lse)'
          
            '    sys.stderr.reconfigure(line_buffering=True, write_through=Fa' +
            'lse)'
          ''
          'del code'
          'del RemotePythonInterpreter'
          'del sys'
          'del os'
          'del threading'
          'del builtins')
      end
      item
        Name = 'SimpleServer'
        Strings.Strings = (
          'import sys'
          'if len(sys.argv) > 2:'
          '    sys.path.insert(0, sys.argv[2])'
          ''
          'from rpyc.utils.server import Server'
          'from rpyc.utils.classic import DEFAULT_SERVER_PORT'
          'from rpyc.core import SlaveService'
          'from rpyc.core.stream import NamedPipeStream'
          'from rpyc.utils.factory import connect_stream'
          'import threading'
          ''
          '__traceable__ = 0'
          ''
          'class SimpleServer(Server):'
          '    def _accept_method(self, sock):'
          '        try:'
          '            self._serve_client(sock, None)'
          '        finally:'
          '            self.close()'
          ''
          'class ModSlaveService(SlaveService):'
          '    __slots__ = []'
          ''
          '    def on_connect(self, conn):'
          '        import types'
          '        from rpyc.core.service import ModuleNamespace'
          ''
          '        sys.modules["__oldmain__"] = sys.modules["__main__"]'
          '        sys.modules["__main__"] = types.ModuleType("__main__")'
          '        self.namespace = sys.modules["__main__"].__dict__'
          ''
          '        conn._config.update(dict('
          '            allow_all_attrs = True,'
          '            allow_pickle = True,'
          '            allow_getattr = True,'
          '            allow_setattr = True,'
          '            allow_delattr = True,'
          '            allow_exposed_attrs = False,'
          '            import_custom_exceptions = True,'
          '            instantiate_custom_exceptions = True,'
          '            instantiate_oldstyle_exceptions = True,'
          '            sync_request_timeout = None,'
          '        ))'
          ''
          '        # disable compression'
          '        conn._channel.compress = False'
          '        self._conn = conn'
          ''
          '        # shortcuts'
          '        conn.modules = ModuleNamespace(conn.root.getmodule)'
          '        conn.eval = conn.root.eval'
          '        conn.execute = conn.root.execute'
          '        conn.namespace = conn.root.namespace'
          '        conn.builtin = conn.modules.builtins'
          ''
          '##def printcrt(s):'
          '##    import msvcrt'
          '##    for c in s:'
          '##        msvcrt.putwch(c)'
          ''
          'def main():'
          '    import warnings'
          '    warnings.simplefilter("ignore", DeprecationWarning)'
          ''
          '    try:'
          '        port = int(sys.argv[1])'
          '    except:'
          '        port = DEFAULT_SERVER_PORT'
          ''
          '    conn = None'
          '    if port > 19000:'
          '        # Named server'
          '        try:'
          '            __import__("win32file")'
          '            np_server = NamedPipeStream.create_server(str(port))'
          '            conn = connect_stream(np_server, ModSlaveService)'
          '            try:'
          '                conn.serve_all()'
          '            except Exception:'
          '                pass'
          '            finally:'
          '                if (conn is not None) and not conn.closed:'
          '                    conn.close()'
          '                exit()'
          '        except SystemExit:'
          '            raise'
          '        except:'
          '            conn = None'
          ''
          '    if conn is None:'
          
            '        t = SimpleServer(ModSlaveService, port = port, auto_regi' +
            'ster = False)'
          '        t.start()'
          ''
          'if __name__ == "__main__":'
          '    main()')
      end
      item
        Name = 'TkServer'
        Strings.Strings = (
          'import sys'
          'if len(sys.argv) > 2:'
          '    sys.path.insert(0, sys.argv[2])'
          ''
          'try:'
          '    import tkinter'
          'except:'
          '    import Tkinter as tkinter'
          'import threading'
          'import gc'
          ''
          'from rpyc.utils.server import Server'
          'from rpyc.utils.classic import DEFAULT_SERVER_PORT'
          'from rpyc.core import SlaveService'
          ''
          '__traceable__ = 0'
          ''
          'class SimpleServer(Server):'
          '    def _accept_method(self, sock):'
          '        from rpyc.core import SocketStream, Channel, Connection'
          '        config = dict(self.protocol_config, credentials = None)'
          
            '        self.conn = self.service._connect(Channel(SocketStream(s' +
            'ock)), config)'
          ''
          'class ModSlaveService(SlaveService):'
          '    __slots__ = []'
          ''
          '    def on_connect(self, conn):'
          '        import types'
          '        from rpyc.core.service import ModuleNamespace'
          ''
          '        sys.modules["__oldmain__"] = sys.modules["__main__"]'
          '        sys.modules["__main__"] = types.ModuleType("__main__")'
          '        self.namespace = sys.modules["__main__"].__dict__'
          ''
          '        conn._config.update(dict('
          '            allow_all_attrs = True,'
          '            allow_pickle = True,'
          '            allow_getattr = True,'
          '            allow_setattr = True,'
          '            allow_delattr = True,'
          '            allow_exposed_attrs = False,'
          '            import_custom_exceptions = True,'
          '            instantiate_custom_exceptions = True,'
          '            instantiate_oldstyle_exceptions = True,'
          '            sync_request_timeout = None,'
          '        ))'
          ''
          '        # disable compression'
          '        conn._channel.compress = False'
          '        self._conn = conn'
          ''
          '        # shortcuts'
          '        conn.modules = ModuleNamespace(conn.root.getmodule)'
          '        conn.eval = conn.root.eval'
          '        conn.execute = conn.root.execute'
          '        conn.namespace = conn.root.namespace'
          '        conn.builtin = conn.modules.builtins'
          ''
          'running = False'
          ''
          'class GuiPart:'
          '    def __init__(self, master):'
          '        self.processing = False'
          ''
          '    def processIncoming(self, conn):'
          '        """'
          '        Handle messages currently in the queue (if any).'
          '        """'
          '        if self.processing:'
          '            return'
          '        else:'
          '            self.processing = True'
          '        try:'
          '            conn.poll_all()'
          '        except EOFError:'
          '            global running'
          '            running = False'
          ''
          '        self.processing = False'
          ''
          'class GuiServer:'
          '    def __init__(self, master):'
          '        self.master = master'
          ''
          '        import warnings'
          '        warnings.simplefilter("ignore", DeprecationWarning)'
          ''
          '        try:'
          '            port = int(sys.argv[1])'
          '        except:'
          '            port = DEFAULT_SERVER_PORT'
          ''
          
            '        self._server = SimpleServer(ModSlaveService, port = port' +
            ', auto_register = False)'
          '##        self._server.start()'
          '        self._server.listener.listen(self._server.backlog)'
          '        self._server.active = True'
          '        self._server.listener.settimeout(0.5)'
          '        self._server.accept()'
          ''
          '        # Set up the GUI part'
          '        self.gui = GuiPart(master)'
          ''
          '        global running'
          '        running = True'
          
            '        # Start the periodic call in the GUI to check if the que' +
            'ue contains'
          '        # anything'
          '        self.periodicCall()'
          ''
          '    def periodicCall(self):'
          '        """'
          
            '        Check every 100 ms if there is something new in the queu' +
            'e.'
          '        """'
          '        global running'
          '        if not running:'
          '            import sys'
          '            self._server.conn.close()'
          '            self._server.close()'
          '            gc.collect()'
          '            sys.exit(1)'
          '        else:'
          '            if not self.gui.processing:'
          '                self.gui.processIncoming(self._server.conn)'
          ''
          '            self.master.after(1, self.periodicCall)'
          ''
          'def hijack_tk():'
          
            '    """Modifies tkinter'#39's mainloop with a dummy so when a module' +
            ' calls'
          '    mainloop, it does not block.'
          ''
          '    """'
          '    def misc_mainloop(self, n=0):'
          '        pass'
          '    def tkinter_mainloop(n=0):'
          '        pass'
          '    def dummy_exit(arg=0):'
          '        pass'
          '    def dummy_quit(self):'
          '        if self.master:'
          '            self.master.destroy()'
          '        else:'
          '            self.destroy()'
          ''
          '    tkinter.oldmainloop = tkinter.mainloop'
          '    tkinter.Misc.oldmainloop = tkinter.Misc.mainloop'
          '    tkinter.Misc.mainloop = misc_mainloop'
          '    tkinter.mainloop = tkinter_mainloop'
          '    tkinter.Misc.oldquit = tkinter.Misc.quit'
          '    tkinter.Misc.quit = dummy_quit'
          '    import sys'
          '    sys.exit = dummy_exit'
          ''
          'def main():'
          '    root = tkinter.Tk()'
          '    server = GuiServer(root)'
          '    root.withdraw()'
          '    hijack_tk()'
          '    root.oldmainloop()'
          ''
          'if __name__ == "__main__":'
          '    main()')
      end
      item
        Name = 'WxServer'
        Strings.Strings = (
          'import sys'
          'if len(sys.argv) > 2:'
          '    sys.path.insert(0, sys.argv[2])'
          ''
          'import wx'
          'import threading'
          'import gc'
          ''
          '__traceable__ = 0'
          'running = False'
          ''
          'from rpyc.utils.server import Server'
          'from rpyc.utils.classic import DEFAULT_SERVER_PORT'
          'from rpyc.core import SlaveService'
          ''
          '__traceable__ = 0'
          ''
          'class SimpleServer(Server):'
          '    def _accept_method(self, sock):'
          '        from rpyc.core import SocketStream, Channel, Connection'
          '        config = dict(self.protocol_config, credentials = None)'
          
            '        self.conn = self.service._connect(Channel(SocketStream(s' +
            'ock)), config)'
          ''
          'class ModSlaveService(SlaveService):'
          '    __slots__ = []'
          ''
          '    def on_connect(self, conn):'
          '        import types'
          '        from rpyc.core.service import ModuleNamespace'
          ''
          '        sys.modules["__oldmain__"] = sys.modules["__main__"]'
          '        sys.modules["__main__"] = types.ModuleType("__main__")'
          '        self.namespace = sys.modules["__main__"].__dict__'
          ''
          '        conn._config.update(dict('
          '            allow_all_attrs = True,'
          '            allow_pickle = True,'
          '            allow_getattr = True,'
          '            allow_setattr = True,'
          '            allow_delattr = True,'
          '            allow_exposed_attrs = False,'
          '            import_custom_exceptions = True,'
          '            instantiate_custom_exceptions = True,'
          '            instantiate_oldstyle_exceptions = True,'
          '             sync_request_timeout = None,'
          '       ))'
          ''
          '        # disable compression'
          '        conn._channel.compress = False'
          '        self._conn = conn'
          ''
          '        # shortcuts'
          '        conn.modules = ModuleNamespace(conn.root.getmodule)'
          '        conn.eval = conn.root.eval'
          '        conn.execute = conn.root.execute'
          '        conn.namespace = conn.root.namespace'
          '        conn.builtin = conn.modules.builtins'
          ''
          'class GuiPart:'
          '    def __init__(self, master):'
          '        self.processing = False'
          '        self.connected = False'
          '        self.conn = None'
          ''
          '    def connect(self):'
          '        import warnings'
          '        warnings.simplefilter("ignore", DeprecationWarning)'
          ''
          '        try:'
          '            port = int(sys.argv[1])'
          '        except:'
          '            port = DEFAULT_SERVER_PORT'
          ''
          
            '        self._server = SimpleServer(ModSlaveService, port = port' +
            ', auto_register = False)'
          '##        self._server.start()'
          '        self._server.listener.listen(self._server.backlog)'
          '        self._server.active = True'
          '        self._server.listener.settimeout(0.5)'
          '        self._server.accept()'
          ''
          '        self.conn = self._server.conn'
          '        self.connected = True'
          '        self.processIncoming()'
          ''
          '    def processIncoming(self):'
          '        """'
          '        Handle messages currently in the queue (if any).'
          '        """'
          '        global running'
          '        if self.processing:'
          '            return'
          '        else:'
          '            self.processing = True'
          '        try:'
          '            self.conn.poll_all()'
          '        except EOFError:'
          '            running = False'
          ''
          '        self.processing = False'
          ''
          'class GuiServer:'
          '    def __init__(self, master):'
          ''
          '        global running'
          ''
          '        self.master = master'
          ''
          '        # Set up the GUI part'
          '        self.gui = GuiPart(master)'
          ''
          '        running = True'
          
            '        # Start the periodic call in the GUI to check if the que' +
            'ue contains'
          '        # anything'
          '        wx.CallAfter(self.setupPeriodicCall)'
          ''
          '    def setupPeriodicCall(self):'
          '        self.calllater = wx.CallLater(1, self.periodicCall)'
          ''
          '    def periodicCall(self):'
          '        """'
          '        Check every 1 ms if there is something new in the queue.'
          '        """'
          '        global running'
          '        if not running:'
          '            import sys'
          '            self.gui.conn.close()'
          '            gc.collect()'
          '            sys.exit(1)'
          '        if not self.gui.connected:'
          '            self.gui.connect()'
          '        elif not self.gui.processing:'
          '            self.gui.processIncoming()'
          ''
          '        self.calllater.Restart()'
          ''
          'def hijack_wx():'
          '    """Modifies wx mainloop with a dummy so when a module calls'
          '    mainloop, it does not block.'
          ''
          '    """'
          '    def MainLoop(self):'
          '        pass'
          ''
          '    def RedirectStdio(self, filename=None):'
          '        pass'
          ''
          '    def RestoreStdio(self):'
          '        pass'
          ''
          '    def dummy_exit(arg=0):'
          '        pass'
          ''
          '    wx.PyApp.OldMainLoop = wx.PyApp.MainLoop'
          '    wx.PyApp.MainLoop = MainLoop'
          '    wx.App.OldRedirectStdio = wx.App.RedirectStdio'
          '    wx.App.RedirectStdio = RedirectStdio'
          '    wx.App.OldRestoreStdio = wx.App.RestoreStdio'
          '    wx.App.RestoreStdio = RestoreStdio'
          '    import sys'
          '    sys.exit = dummy_exit'
          ''
          'def main():'
          '    app = wx.App()'
          '    frame = wx.Frame(None, title='#39'PyScripter Debug Server'#39')'
          '    app.SetTopWindow(frame)'
          '    #frame.Show()'
          '    hijack_wx()'
          '    server = GuiServer(app)'
          '    app.OldMainLoop(app)'
          ''
          'if __name__ == "__main__":'
          '    main()')
      end>
    Left = 23
    Top = 82
  end
  object ParameterCompletion: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 350
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Title = 'Parameters'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Shell Dlg 2'
    TitleFont.Style = [fsBold]
    GripperFont.Charset = DEFAULT_CHARSET
    GripperFont.Color = clBtnText
    GripperFont.Height = -36
    GripperFont.Name = 'Segoe UI'
    GripperFont.Style = []
    Columns = <
      item
        ColumnWidth = 120
        DefaultFontStyle = [fsBold]
      end
      item
        ColumnWidth = 230
      end>
    Resizeable = True
    OnExecute = ParameterCompletionExecute
    ShortCut = 24656
    OnCodeCompletion = ParameterCompletionCodeCompletion
    Left = 335
    Top = 20
  end
  object ModifierCompletion: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 400
    EndOfTokenChr = '()[].- '
    TriggerChars = '.'
    Title = 'Modifiers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    GripperFont.Charset = DEFAULT_CHARSET
    GripperFont.Color = clBtnText
    GripperFont.Height = -36
    GripperFont.Name = 'Segoe UI'
    GripperFont.Style = []
    Columns = <
      item
        ColumnWidth = 120
        DefaultFontStyle = [fsBold]
      end
      item
        ColumnWidth = 280
      end>
    Resizeable = True
    OnExecute = ModifierCompletionExecute
    ShortCut = 24653
    OnCodeCompletion = ModifierCompletionCodeCompletion
    Left = 335
    Top = 82
  end
  object CodeTemplatesCompletion: TSynAutoComplete
    AutoCompleteList.Strings = (
      'hdr'
      '|Python Module header'
      
        '=#--------------------------------------------------------------' +
        '-----------------'
      '=# Name:        $[ActiveDoc-Name]'
      '=# Purpose:     |'
      '=#'
      '=# Author:      $[UserName]'
      '=#'
      '=# Created:     $[DateTime-'#39'DD/MM/YYYY'#39'-DateFormat]'
      '=# Copyright:   (c) $[UserName] $[DateTime-'#39'YYYY'#39'-DateFormat]'
      '=# Licence:     <your licence>'
      
        '=#--------------------------------------------------------------' +
        '-----------------'
      'cl'
      '|Comment Line'
      
        '=#--------------------------------------------------------------' +
        '-----------------'
      '=|'
      'pyapp'
      '|Python application'
      '=def main():'
      '=    |pass'
      '='
      '=if __name__ == '#39'__main__'#39':'
      '=    main()'
      'cls'
      '|Python class'
      '=class |(object):'
      '=    """'
      '='#9#9'class comment'
      '=    """'
      '='
      '=    def __init__(self):'
      '=        pass'
      'fec'
      '|File encoding comment'
      '=# -*- coding: UTF-8 -*-'
      '=|'
      'she'
      '|Sheband'
      '=#!/usr/bin/env python$[($[PythonVersion]>='#39'3.0'#39')'#39'3'#39':'#39#39']'
      '=|')
    EndOfTokenChr = '()[]. '
    ShortCut = 0
    Options = [scoLimitToMatchedText, scoUseInsertList, scoCompleteWithTab, scoCompleteWithEnter]
    Left = 335
    Top = 148
  end
  object icGutterGlyphs: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'EditorGutter\Break'
        SVGText = 
          '<svg viewBox="0 0 11 14" stroke="black">'#13#10'  <circle stroke-width' +
          '="0.5" fill="#E24444" cx="5.5" cy="7" r="4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'EditorGutter\BreakDisabled'
        SVGText = 
          '<svg viewBox="0 0 11 14" stroke="black">'#13#10'  <circle stroke-width' +
          '="0.5" fill="#8E8E8E" cx="5.5" cy="7" r="4"/>'#13#10'</svg>'
      end
      item
        IconName = 'EditorGutter\BreakInvalid'
        SVGText = 
          '<svg viewBox="0 0 11 14" stroke="black">'#13#10'  <circle stroke-width' +
          '="0.5" fill="#E24444" cx="5.5" cy="7" r="4"/>'#13#10'  <line stroke="#' +
          'FFCE00" x1="3.5" x2="7.5" y1="5" y2="9" />'#13#10'  <line stroke="#FFC' +
          'E00" x1="3.5" x2="7.5" y1="9" y2="5" />'#13#10'</svg>'
      end
      item
        IconName = 'EditorGutter\Current'
        SVGText = 
          '<svg viewBox="0 0 11 14" stroke="black">'#13#10'  <path stroke-width="' +
          '0.5" fill="#4488FF" d="M 0.5 5 h 4 v -3.5 l 5 5.5 l -5 5.5 v-3.5' +
          ' h -4z"/>'#13#10'</svg>'
      end
      item
        IconName = 'EditorGutter\CurrentBreak'
        SVGText = 
          '<svg viewBox="0 0 11 14" stroke="black">'#13#10'  <circle stroke-width' +
          '="0.5" fill="#E24444" cx="5.5" cy="7" r="4"/>'#13#10'  <path stroke-wi' +
          'dth="0.5" fill="#4488FF" d="M 0.5 5 h 4 v -3.5 l 5 5.5 l -5 5.5 ' +
          'v-3.5 h -4z"/>'#13#10'</svg>'
      end
      item
        IconName = 'EditorGutter\Executable'
        SVGText = 
          '<svg  viewBox="0 0 11 14" width="11" height="14" stroke="black" ' +
          '>'#13#10'  <circle stroke-width="0.5" fill="#4488FF" cx="5.5" cy="7" r' +
          '="2"/>'#13#10'</svg>'
      end>
    ApplyFixedColorToRootOnly = True
    Left = 120
    Top = 224
  end
  object icCodeImages: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'CodeImages\Class'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10#9'<path d="M23.5,23.1c0,0.8-0.7,1.5-1' +
          '.5,1.5h-8.2c-0.8,0-1.5-0.7-1.5-1.5V12.7H8.8c-0.8,0-1.5-0.7-1.5-1' +
          '.5c0-0.8,0.7-1.5,1.5-1.5h12'#13#10#9#9'c0.8,0,1.5,0.7,1.5,1.5c0,0.8-0.7,' +
          '1.5-1.5,1.5h-5.5v8.9H22C22.8,21.6,23.5,22.3,23.5,23.1z"/>'#13#10#9'<pat' +
          'h fill="#4488FF"  d="M23.5,18.7c-0.4,0-0.9-0.2-1.2-0.5l-3.8-3.8c' +
          '-0.7-0.7-0.7-1.8,0-2.5L25,5.3c0.7-0.7,1.8-0.7,2.5,0l3.8,3.8'#13#10#9#9'c' +
          '0.7,0.7,0.7,1.8,0,2.5l-6.6,6.6C24.4,18.5,24,18.7,23.5,18.7z M22.' +
          '1,13.1l1.4,1.4l4.1-4.1L26.2,9L22.1,13.1z"/>'#13#10#9'<path fill="#FFCE0' +
          '0"  d="M6.3,15.7c-0.4,0-0.9-0.2-1.2-0.5l-4.5-4.5c-0.7-0.7-0.7-1.' +
          '8,0-2.5l7.7-7.7c0.7-0.7,1.8-0.7,2.5,0L15.2,5'#13#10#9#9'c0.7,0.7,0.7,1.8' +
          ',0,2.5l-7.7,7.7C7.2,15.5,6.7,15.7,6.3,15.7z M4.3,9.5l2,2l5.2-5.2' +
          'l-2-2L4.3,9.5z"/>'#13#10#9'<path fill="#E24444"  d="M23,32c-0.4,0-0.9-0' +
          '.2-1.2-0.5l-3.8-3.8c-0.7-0.7-0.7-1.8,0-2.5l6.6-6.6c0.7-0.7,1.8-0' +
          '.7,2.5,0l3.8,3.8'#13#10#9#9'c0.7,0.7,0.7,1.8,0,2.5l-6.6,6.6C23.9,31.8,23' +
          '.5,32,23,32z M21.7,26.4l1.4,1.4l4.1-4.1l-1.4-1.4L21.7,26.4z"/>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'CodeImages\Field'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#E24444" d="M13.6,29.3c-' +
          '0.6,0-1.1-0.2-1.5-0.6l-8.8-8.8c-0.8-0.8-0.8-2.2,0-3.1L16.8,3.3c0' +
          '.8-0.8,2.2-0.8,3.1,0l8.8,8.8'#13#10#9#9'c0.8,0.8,0.8,2.2,0,3.1L15.2,28.7' +
          'C14.7,29.1,14.2,29.3,13.6,29.3z M7.9,18.4l5.7,5.7l10.4-10.4l-5.7' +
          '-5.7L7.9,18.4z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeImages\Function'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g fill="#4488FF">'#13#10#9'<rect x="3.9" y=' +
          '"9.7" width="6.5" height="3.5"/>'#13#10#9'<rect x="0.5" y="16.1" width=' +
          '"9.9" height="3.5"/>'#13#10#9'<rect x="0.5" y="22.6" width="11.9" heigh' +
          't="3.5"/>'#13#10#9'<path d="M31.2,13.9l-8.7-8.3c-0.6-0.6-1.6-0.6-2.3-0.' +
          '1L14,10.4c-0.1,0.1-0.3,0.2-0.4,0.3c-0.1,0.1-0.2,0.3-0.3,0.4'#13#10#9#9'c' +
          '0,0-0.1,0.1-0.1,0.1c0,0,0,0,0,0.1c-0.1,0.2-0.1,0.4-0.1,0.6l0,5.2' +
          'c0,0.5,0.2,0.9,0.5,1.3l8.3,8c0.3,0.3,0.8,0.5,1.2,0.5'#13#10#9#9'c0.4,0,0' +
          '.8-0.2,1.2-0.5l5.8-5.3c0.3-0.3,0.5-0.6,0.5-1l0.9-4.6C31.8,15,31.' +
          '6,14.4,31.2,13.9z M21.2,9.2l6.3,6l-3.4,2.9L17.6,12'#13#10#9#9'L21.2,9.2z' +
          '"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeImages\Keyword'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#FFCE00" d="M16.9,12.2c-' +
          '1.4-3.1-4.6-5.1-8-5.1C3.9,7.2,0,11,0,16s3.9,8.8,8.9,8.8c3.4,0,6.' +
          '6-2,8-5.1h4.2v5.1h7.5v-5H32'#13#10#9#9'v-7.7H16.9z M8.9,19.8c-2.1,0-3.8-' +
          '1.8-3.8-3.8s1.7-3.8,3.8-3.8c2,0,3.8,1.8,3.8,3.8S10.9,19.8,8.9,19' +
          '.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeImages\List'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10'<g fill="#4488FF">'#13#10#9'<path d="M13.2,' +
          '4.4H8.8v23.3h4.4v3.2H4.5V1.1h8.7V4.4z"/>'#13#10#9'<path d="M18.8,1.1h8.' +
          '7v29.7h-8.7v-3.2h4.4V4.4h-4.4V1.1z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeImages\Method'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g fill="#E24444"> '#13#10#9'<rect x="3.8" y' +
          '="9.7" width="6.5" height="3.5"/>'#13#10#9'<rect x="0.4" y="16.1" width' +
          '="9.9" height="3.5"/>'#13#10#9'<rect x="0.4" y="22.6" width="11.9" heig' +
          'ht="3.5"/>'#13#10#9'<path d="M31.1,13.9l-8.7-8.3c-0.6-0.6-1.6-0.6-2.3-0' +
          '.1l-6.2,4.9c-0.1,0.1-0.3,0.2-0.4,0.3c-0.1,0.1-0.2,0.3-0.3,0.4'#13#10#9 +
          #9'c0,0-0.1,0.1-0.1,0.1c0,0,0,0,0,0.1C13.1,11.5,13,11.7,13,12l0,5.' +
          '2c0,0.5,0.2,0.9,0.5,1.3l8.3,8c0.3,0.3,0.8,0.5,1.2,0.5'#13#10#9#9'c0.4,0,' +
          '0.8-0.2,1.2-0.5l5.8-5.3c0.3-0.3,0.5-0.6,0.5-1l0.9-4.6C31.7,15,31' +
          '.5,14.3,31.1,13.9z M21.1,9.2l6.3,6L24,18.1L17.5,12'#13#10#9#9'L21.1,9.2z' +
          '"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeImages\Module'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10#9'<path d="M30.5,26.3l-2.9,2.6c1.7,0,' +
          '3.1-1.4,3.2-3C30.8,26,30.7,26.2,30.5,26.3z M27.7,5.8h-4.8V3.1c0-' +
          '1.8-1.4-3.2-3.2-3.2h-8.1'#13#10#9#9'c-1.8,0-3.2,1.4-3.2,3.2v2.7h-5C1.5,5' +
          '.8,0.1,7.2,0.1,9v16.7c0,1.8,1.4,3.2,3.2,3.2h16.5l-3.5-3.4H3.5v-1' +
          '.3H1.3v-3.1h2.2V9.3h23.7v6'#13#10#9#9'l3.7,3.5V9C30.9,7.2,29.5,5.8,27.7,' +
          '5.8z M19.3,5.8h-7.6V3.3h7.6V5.8z"/>'#13#10#9'<g fill="#4488FF">'#13#10#9#9'<rec' +
          't x="4.7" y="15.1" width="6.4" height="3.1"/>'#13#10#9#9'<rect x="1.3" y' +
          '="21.2" width="9.8" height="3.1"/>'#13#10#9#9'<path d="M31.5,19.3l-0.6-0' +
          '.5l-3.7-3.5l-4.3-4.1c-0.6-0.6-1.6-0.6-2.3-0.1l-6,4.8c-0.1,0.1-0.' +
          '3,0.2-0.4,0.3'#13#10#9#9#9'c-0.1,0.1-0.2,0.3-0.3,0.4c0,0,0,0.1-0.1,0.1c0,' +
          '0,0,0,0,0.1c-0.1,0.2-0.1,0.4-0.1,0.6l0,5.1c0,0.5,0.2,0.9,0.5,1.2' +
          'l1.9,1.9'#13#10#9#9#9'l3.5,3.4l2.7,2.6c0.3,0.3,0.8,0.5,1.2,0.5c0.4,0,0.8-' +
          '0.1,1.2-0.4l2.9-2.6l2.9-2.6c0.1-0.1,0.3-0.3,0.3-0.4'#13#10#9#9#9'c0.1-0.2' +
          ',0.1-0.3,0.2-0.5l0.9-4.5C32.1,20.3,31.9,19.7,31.5,19.3z M24.5,23' +
          '.4l-6.4-6l3.5-2.8l6.2,5.9L24.5,23.4z"/>'#13#10#9'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeImages\Namespace'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g fill="#4488FF">'#13#10#9'<path d="M10.6,3' +
          '1.4c-3.7-1-5.7-3.5-5.7-7.4v-3.1c0-2.2-2.2-3.4-4.1-3.4v-3.2c1.8,0' +
          ',4-1,4.1-3.1V8'#13#10#9#9'c0-1.9,0.5-3.5,1.4-4.8c0.9-1.3,2.3-2.1,4.3-2.6' +
          'l0.9,2.6C10.7,3.5,10,4,9.7,4.8C9.3,5.5,9,6.4,9,7.7v3.4c0,2.3-0.9' +
          ',4-2.8,5'#13#10#9#9'C8.1,17,9,18.7,9,21.2v3.4c0,2.5,0.9,4,2.5,4.5L10.6,3' +
          '1.4z"/>'#13#10#9'<path d="M20.6,28.8c1.5-0.5,2.5-2.1,2.5-4.5v-3.4c0-2.3' +
          ',0.9-4,2.8-4.9c-1.9-0.9-2.8-2.6-2.8-5V7.7c0-2.5-0.9-4-2.5-4.5'#13#10#9 +
          #9'l0.9-2.6c1.9,0.5,3.4,1.4,4.3,2.6c0.9,1.2,1.4,2.7,1.4,4.6v3.4c0,' +
          '2.1,2.3,3.1,4.1,3.1v3.2c-1.8,0-4.1,1.2-4.1,3.4v3.4'#13#10#9#9'c-0.1,3.6-' +
          '1.9,6.1-5.7,7.1L20.6,28.8z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeImages\Python'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10'<g transform="translate(-275.1 -318.' +
          '4)">'#13#10#9'<path d="M290.9,318.8h-11.3c-1.6,0-2.9,1.3-2.8,2.9v23.4c0' +
          ',1.6,1.2,2.9,2.8,2.9h17c1.6,0,2.9-1.3,2.8-2.9'#13#10#9#9'v-17.6L290.9,31' +
          '8.8z M279.6,345.2v-23.4h9.9v7.3h7.1v16.1L279.6,345.2L279.6,345.2' +
          'z"/>'#13#10'</g>'#13#10'<g transform="translate(-16.5,-18) scale(1.45)">'#13#10#9'<' +
          'g fill="#4488FF">'#13#10#9'<path d="M20.8,22.5H25c1.2,0,2.1-1,2.1-2.2v-' +
          '4c-0.1-1.2-1-2.1-2.1-2.2c0,0,0,0-0.1,0c-0.6-0.1-3.4-0.2-4.2,0'#13#10#9 +
          #9'c-1.2,0.3-1.8,0.6-2,1.2c-0.1,0.3-0.1,0.6-0.1,1v1.6h4.2v0.6h-5.9' +
          'c-1.3,0-2.4,0.9-2.7,2.1c-0.1,0.3-0.1,0.5-0.2,0.8'#13#10#9#9'c0,0.1,0,0.2' +
          '-0.1,0.3c-0.1,0.4-0.1,0.8-0.1,1.2c0,0.7,0.1,1.4,0.3,2.1c0.1,0.3,' +
          '0.2,0.6,0.3,0.8c0.3,0.6,0.6,1,1.1,1.2'#13#10#9#9'c0.3,0.1,0.5,0.2,0.8,0.' +
          '2h1.5v-2C18.1,23.6,19.3,22.5,20.8,22.5z M20.5,16.7c-0.4,0-0.8-0.' +
          '4-0.8-0.8s0.4-0.8,0.8-0.8'#13#10#9#9's0.8,0.4,0.8,0.8C21.4,16.3,21,16.7,' +
          '20.5,16.7z"/>'#13#10#9'</g>'#13#10#9'<path fill="#FFCE00" d="M24.9,23.5h-4.2c-' +
          '1.2,0-2.1,1-2.1,2.2v4c0.1,1.2,1,2.1,2.1,2.2c0,0,0,0,0.1,0c0.6,0.' +
          '1,3.4,0.2,4.2,0'#13#10#9#9'c1.2-0.3,1.8-0.6,2-1.2c0.1-0.3,0.1-0.6,0.1-1v' +
          '-1.6h-4.2v-0.6h5.9c1.3,0,2.4-0.9,2.7-2.1c0.1-0.3,0.1-0.5,0.2-0.8' +
          #13#10#9#9'c0-0.1,0-0.2,0.1-0.3c0.1-0.4,0.1-0.8,0.1-1.2c0-0.7-0.1-1.4-0' +
          '.3-2.1c-0.1-0.3-0.2-0.6-0.3-0.8c-0.3-0.6-0.6-1-1.1-1.2'#13#10#9#9'c-0.3-' +
          '0.1-0.5-0.2-0.8-0.2h-1.5v2C27.6,22.4,26.4,23.5,24.9,23.5z M25.2,' +
          '29.3c0.4,0,0.8,0.4,0.8,0.8c0,0.4-0.4,0.8-0.8,0.8'#13#10#9#9's-0.8-0.4-0.' +
          '8-0.8C24.3,29.7,24.7,29.3,25.2,29.3z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeImages\Variable'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#4488FF" d="M13.6,29.3c-' +
          '0.6,0-1.1-0.2-1.5-0.6l-8.8-8.8c-0.8-0.8-0.8-2.2,0-3.1L16.8,3.3c0' +
          '.8-0.8,2.2-0.8,3.1,0l8.8,8.8'#13#10#9#9'c0.8,0.8,0.8,2.2,0,3.1L15.2,28.7' +
          'C14.7,29.1,14.2,29.3,13.6,29.3z M7.9,18.4l5.7,5.7l10.4-10.4l-5.7' +
          '-5.7L7.9,18.4z"/>'#13#10'</svg>'#13#10
      end>
    ApplyFixedColorToRootOnly = True
    Left = 117
    Top = 152
  end
  object icBrowserImages: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Browser\Back'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M28,16c0,6.6-5.4,12-12,12S4' +
          ',22.6,4,16S9.4,4,16,4S28,9.4,28,16 M31,16c0-8.3-6.7-15-15-15S1,7' +
          '.7,1,16s6.7,15,15,15'#13#10#9#9'S31,24.3,31,16L31,16z M16,17.5h6v-3h-6V1' +
          '0l-6,6l6,6V17.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Browser\Cancel'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M19.8,10.1L15.9,14L12,10.1l' +
          '-2.1,2.1l3.9,3.9L9.9,20l2.1,2.1l3.9-3.9l3.9,3.9l2.1-2.1L18,16.1l' +
          '3.9-3.9L19.8,10.1z'#13#10#9#9' M15.9,1.1c-8.3,0-15,6.7-15,15s6.7,15,15,1' +
          '5s15-6.7,15-15S24.2,1.1,15.9,1.1z M15.9,28.1c-6.6,0-12-5.4-12-12' +
          's5.4-12,12-12'#13#10#9#9's12,5.4,12,12S22.5,28.1,15.9,28.1z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Browser\Forward'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M4,16C4,9.4,9.4,4,16,4s12,5' +
          '.4,12,12s-5.4,12-12,12S4,22.7,4,16 M1,16c0,8.3,6.7,15,15,15s15-6' +
          '.7,15-15S24.3,1,16,1'#13#10#9#9'S1,7.8,1,16L1,16z M16,14.5h-6v3h6V22l6-6' +
          'l-6-6V14.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Browser\PageSetup'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M19,1H7C5.3,1,4,2.3,4,4v24c0' +
          ',1.7,1.3,3,3,3h18c1.7,0,3-1.3,3-3V10L19,1z M25,23.9h-2.3V11.5H25' +
          'V23.9z M11.6,23.9V8.2h5.9'#13#10#9'v3.3h3v12.4C20.5,23.9,11.6,23.9,11.6' +
          ',23.9z M20.5,26.1V28h-8.9v-1.9H20.5z M17.5,6h-5.9V4h5.9V6z M9.4,' +
          '4v2H7V4H9.4z M7,8.2h2.4'#13#10#9'v15.7H7v2.2h2.4V28H7V8.2z M22.7,28v-1.' +
          '9H25V28H22.7z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Browser\Preview'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M7.5,25.6l2.1,2.1l3.6-3.6c1' +
          ',0.6,2.3,1.1,3.6,1.1c3.8,0,6.8-3,6.8-6.8s-3-6.8-6.8-6.8s-6.8,3-6' +
          '.8,6.8c0,1.3,0.5,2.5,1,3.6'#13#10#9#9'L7.5,25.6z M13,18.4c0-2.1,1.7-3.8,' +
          '3.8-3.8s3.8,1.6,3.8,3.8c0,2.1-1.7,3.8-3.8,3.8S13,20.5,13,18.4z M' +
          '7.5,25.6l2.1,2.1l3.6-3.6'#13#10#9#9'c1,0.6,2.3,1.1,3.6,1.1c3.8,0,6.8-3,6' +
          '.8-6.8s-3-6.8-6.8-6.8s-6.8,3-6.8,6.8c0,1.3,0.5,2.5,1,3.6L7.5,25.' +
          '6z M13,18.4'#13#10#9#9'c0-2.1,1.7-3.8,3.8-3.8s3.8,1.6,3.8,3.8c0,2.1-1.7,' +
          '3.8-3.8,3.8S13,20.5,13,18.4z M7.5,25.6l2.1,2.1l3.6-3.6c1,0.6,2.3' +
          ',1.1,3.6,1.1'#13#10#9#9'c3.8,0,6.8-3,6.8-6.8s-3-6.8-6.8-6.8s-6.8,3-6.8,6' +
          '.8c0,1.3,0.5,2.5,1,3.6L7.5,25.6z M13,18.4c0-2.1,1.7-3.8,3.8-3.8'#13 +
          #10#9#9's3.8,1.6,3.8,3.8c0,2.1-1.7,3.8-3.8,3.8S13,20.5,13,18.4z"/>'#13#10#9 +
          '<path d="M19,1H7C5.3,1,4,2.3,4,4v24c0,1.7,1.3,3,3,3h18c1.7,0,3-1' +
          '.3,3-3V10L19,1z M25,28H7V4h10.5v7.5H25V28z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Browser\Print'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M26.5,10H25V2.5H7V10H5.5C3,' +
          '10,1,11.9,1,14.5v9h6v6h18v-6h6v-9C31,11.9,28.9,10,26.5,10z M10,5' +
          '.5h12V10H10V5.5z M22,23.5v3'#13#10#9#9'H10v-6h12V23.5z M25,20.5v-3H7v3H4' +
          'v-6C4,13.7,4.8,13,5.5,13h21c0.9,0,1.5,0.8,1.5,1.5v6H25z"/>'#13#10#9'<ci' +
          'rcle cx="25" cy="15.2" r="1.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Browser\Save'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M23.5,2.5h-18c-1.7,0-3,1.4-' +
          '3,3v21c0,1.6,1.3,3,3,3h21c1.6,0,3-1.4,3-3v-18L23.5,2.5z M26.5,26' +
          '.5h-21v-21h16.8l4.2,4.2'#13#10#9#9'C26.5,9.7,26.5,26.5,26.5,26.5z M16,16' +
          'c-2.5,0-4.5,1.9-4.5,4.5s2,4.5,4.5,4.5s4.5-1.9,4.5-4.5S18.6,16,16' +
          ',16z M7,7h13.5v6H7V7z"/>'#13#10'</svg>'#13#10
      end>
    ApplyFixedColorToRootOnly = True
    Left = 24
    Top = 224
  end
  object icSVGImages: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Abort'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<rect  fill="#E24444" x="3" y="3" wid' +
          'th="10" height="10" rx="2"/>'#13#10'</svg>'
      end
      item
        IconName = 'AppSettings'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M7 22H9V24H7V22M11 22H13' +
          'V24H11V22M15 22H17V24H15V22M5 4H19A2 2 0 0 1 21 6V18A2 2 0 0 1 1' +
          '9 20H5A2 2 0 0 1 3 18V6A2 2 0 0 1 5 4M5 8V18H19V8H5" />'#13#10'</svg>'
      end
      item
        IconName = 'ArrowLeft'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M20,9V15H12V19.84L4.16,1' +
          '2L12,4.16V9H20Z" />'#13#10'</svg>'
      end
      item
        IconName = 'ArrowRight'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M4,15V9H12V4.16L19.84,12' +
          'L12,19.84V15H4Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Assembly'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'  <path d="M19,1H7C5.4,1,4,2.3,4,4v24' +
          'c0,1.6,1.4,3,3,3h18c1.6,0,3-1.4,3-3V10L19,1z M25,28H7V4h10.5v7.5' +
          'H25V28z"/>'#13#10'  <path transform="scale(0.65)" d="M 14 20 L 14 22 L' +
          ' 15 22 L 15 29 L 17 29 L 17 20 L 14 20 z M 23.5 20 C 21.578812 2' +
          '0 20 21.578812 20 23.5 L 20 25.5 C 20 27.421188 21.578812 29 23.' +
          '5 29 C 25.421188 29 27 27.421188 27 25.5 L 27 23.5 C 27 21.57881' +
          '2 25.421188 20 23.5 20 z M 32.5 20 C 30.578812 20 29 21.578812 2' +
          '9 23.5 L 29 25.5 C 29 27.421188 30.578812 29 32.5 29 C 34.421188' +
          ' 29 36 27.421188 36 25.5 L 36 23.5 C 36 21.578812 34.421188 20 3' +
          '2.5 20 z M 23.5 22 C 24.340812 22 25 22.659188 25 23.5 L 25 25.5' +
          ' C 25 26.340812 24.340812 27 23.5 27 C 22.659188 27 22 26.340812' +
          ' 22 25.5 L 22 23.5 C 22 22.659188 22.659188 22 23.5 22 z M 32.5 ' +
          '22 C 33.340812 22 34 22.659188 34 23.5 L 34 25.5 C 34 26.340812 ' +
          '33.340812 27 32.5 27 C 31.659188 27 31 26.340812 31 25.5 L 31 23' +
          '.5 C 31 22.659188 31.659188 22 32.5 22 z M 17.5 33 C 15.578812 3' +
          '3 14 34.578812 14 36.5 L 14 38.5 C 14 40.421188 15.578812 42 17.' +
          '5 42 C 19.421188 42 21 40.421188 21 38.5 L 21 36.5 C 21 34.57881' +
          '2 19.421188 33 17.5 33 z M 26.5 33 C 24.578812 33 23 34.578812 2' +
          '3 36.5 L 23 38.5 C 23 40.421188 24.578812 42 26.5 42 C 28.421188' +
          ' 42 30 40.421188 30 38.5 L 30 36.5 C 30 34.578812 28.421188 33 2' +
          '6.5 33 z M 32 33 L 32 35 L 33 35 L 33 42 L 35 42 L 35 33 L 32 33' +
          ' z M 17.5 35 C 18.340812 35 19 35.659188 19 36.5 L 19 38.5 C 19 ' +
          '39.340812 18.340812 40 17.5 40 C 16.659188 40 16 39.340812 16 38' +
          '.5 L 16 36.5 C 16 35.659188 16.659188 35 17.5 35 z M 26.5 35 C 2' +
          '7.340812 35 28 35.659188 28 36.5 L 28 38.5 C 28 39.340812 27.340' +
          '812 40 26.5 40 C 25.659188 40 25 39.340812 25 38.5 L 25 36.5 C 2' +
          '5 35.659188 25.659188 35 26.5 35 z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Back'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M28,16c0,6.6-5.4,12-12,12S4' +
          ',22.6,4,16S9.4,4,16,4S28,9.4,28,16 M31,16c0-8.3-6.7-15-15-15S1,7' +
          '.7,1,16s6.7,15,15,15'#13#10#9#9'S31,24.3,31,16L31,16z M16,17.5h6v-3h-6V1' +
          '0l-6,6l6,6V17.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Breakpoint'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#E24444" d="M16,28.9C8.9' +
          ',28.9,3.1,23.1,3.1,16S8.9,3.1,16,3.1S28.9,8.9,28.9,16S23.1,28.9,' +
          '16,28.9z M16,6.1c-5.5,0-9.9,4.4-9.9,9.9'#13#10#9#9's4.4,9.9,9.9,9.9s9.9-' +
          '4.4,9.9-9.9S21.4,6.1,16,6.1z"/>'#13#10'</svg>'
      end
      item
        IconName = 'BreakpointsRemove'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path  fill="#E24444"  d="M22.6,9.7h' +
          '-0.2c-0.1-4.8-4-8.6-8.8-8.6c-4.9,0-8.8,4-8.8,8.8c0,1.5,0.4,3,1.1' +
          ',4.2l0,0c0.8,1.5,2,2.7,3.5,3.5'#13#10#9#9'c0.3,0.2,0.6,0.3,0.9,0.4l2.4-2' +
          '.4c-2.8-0.4-4.9-2.8-4.9-5.8c0-3.2,2.6-5.8,5.8-5.8c3.2,0,5.8,2.6,' +
          '5.8,5.8c0,0.1,0,0.2,0,0.4'#13#10#9#9'c-1.8,0.7-3.4,2-4.4,3.7l2.2-1c0.2-0' +
          '.1,0.4-0.1,0.6-0.2h0.1l0,0c0.2,0,0.5,0.1,0.7,0.2c0.3,0.2,0.6,0.4' +
          ',0.7,0.8'#13#10#9#9'c0.1,0.2,0.1,0.4,0.1,0.6c0,0.3-0.1,0.5-0.2,0.7L18,17' +
          '.6c1.9-1.1,3.4-2.8,4.1-4.9c0.2,0,0.4,0,0.7,0c3.2,0,5.8,2.6,5.8,5' +
          '.8'#13#10#9#9's-2.6,5.8-5.8,5.8c-3,0-5.5-2.3-5.8-5.3l-2.5,2.5c1.2,3.4,4.' +
          '5,5.8,8.3,5.8c4.9,0,8.8-4,8.8-8.8C31.4,13.6,27.4,9.7,22.6,9.7z"/' +
          '>'#13#10#9'<polygon points="18.2,28.7 11.6,22.2 13.9,19.9 15.1,18.6 15.' +
          '7,18 16.3,17.5 16.8,16.4 17.9,14.3 17.8,14.3 17.8,14.3 16.6,14.9' +
          ' '#13#10#9#9'15.5,15.4 14.5,15.9 11.8,18.6 10,20.5 2.1,12.6 0.4,14.2 0.4' +
          ',14.2 0.4,14.2 4.4,18.2 8.3,22.1 5,25.4 1.7,28.7 1.7,28.7 1.7,28' +
          '.7 '#13#10#9#9'3.3,30.4 3.4,30.4 7.8,26 10,23.8 13.3,27.1 16.6,30.4 18.6' +
          ',30.8 19,30.9 19,30.9 19,30.9 '#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'BreakpointsWin'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="translate(-538.75 -769' +
          '.336)">'#13#10#9#9'<path fill="#E24444" d="M547,783.8c-1.8,0-3.4,0.6-4.7' +
          ',1.6c-0.1,0.1-0.2,0.2-0.3,0.2c-1.3,1-2.1,2.4-2.5,4.1'#13#10#9#9#9'c-0.1,0' +
          '.6-0.2,1.2-0.2,1.8c0,0.6,0.1,1.1,0.2,1.7c0.2,0.7,0.4,1.3,0.7,1.9' +
          'c0.1,0.2,0.2,0.4,0.3,0.6c1.4,2.1,3.8,3.6,6.5,3.6'#13#10#9#9#9'c2.5,0,4.7-' +
          '1.2,6.1-3c0.1-0.1,0.2-0.2,0.2-0.3c0.5-0.8,0.9-1.6,1.2-2.5c0-0.1,' +
          '0.1-0.2,0.1-0.3c0.1-0.5,0.2-1.1,0.2-1.6'#13#10#9#9#9'C554.7,787.3,551.3,7' +
          '83.8,547,783.8z M551.2,794.8c-0.4,0.5-0.8,0.9-1.3,1.2c-0.8,0.5-1' +
          '.8,0.8-2.9,0.8c-1,0-2-0.3-2.8-0.8'#13#10#9#9#9'c-0.7-0.4-1.3-1-1.7-1.7c-0' +
          '.6-0.8-0.9-1.8-0.9-2.9c0-0.7,0.1-1.4,0.4-2c0.3-0.9,0.9-1.6,1.7-2' +
          '.2c0.9-0.7,2-1.1,3.2-1.1'#13#10#9#9#9'c2.9,0,5.3,2.4,5.3,5.3c0.1,0.5,0.1,' +
          '0.9,0,1.3c0,0.3-0.1,0.5-0.2,0.8C551.8,794,551.5,794.4,551.2,794.' +
          '8z"/>'#13#10#9'</g>'#13#10#9'<path d="M28.6,2.5H3.4c-1.5,0-2.7,1.2-2.7,2.7v10.' +
          '8c0.4-0.5,1-1,1.5-1.4c0.3-0.2,0.7-0.4,1-0.6V5.2C3.3,5.1,3.3,5,3.' +
          '4,5h25.2'#13#10#9#9'c0.1,0,0.2,0.1,0.2,0.2V24c0,0.1-0.1,0.2-0.2,0.2H17.9' +
          'c0,0.1,0,0.1,0,0.2c-0.1,0.5-0.2,1-0.4,1.5c0,0.1-0.1,0.3-0.2,0.4'#13 +
          #10#9#9'c-0.1,0.1-0.1,0.3-0.2,0.4c0,0,0,0,0,0h11.6c1.5,0,2.7-1.2,2.7-' +
          '2.7V5.2C31.3,3.7,30.1,2.5,28.6,2.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bug'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path transform="rotate(-45) transla' +
          'te(-16,6)" d="M28,9.9h-4.2c-0.7-1.2-1.6-2.2-2.7-2.9l2.4-2.4l-2.1' +
          '-2.1l-3.3,3.3c-0.7-0.2-1.4-0.3-2.1-0.3s-1.4,0.1-2.1,0.3l-3.3-3.3' +
          #13#10#9#9'L8.5,4.7l2.4,2.4c-1.1,0.8-2,1.8-2.7,2.9H4v3h3.1C7,13.4,7,13.' +
          '9,7,14.4v1.5H4v3h3v1.5c0,0.5,0.1,1,0.1,1.5H4v3h4.2'#13#10#9#9'c1.6,2.7,4' +
          '.5,4.5,7.8,4.5s6.2-1.8,7.8-4.5H28v-3h-3.1c0.1-0.5,0.1-1,0.1-1.5v' +
          '-1.5h3v-3h-3v-1.5c0-0.5-0.1-1-0.1-1.5H28V9.9z'#13#10#9#9' M22,15.9v4.5c0' +
          ',0.3,0,0.7-0.1,1l-0.1,1l-0.6,1c-1.1,1.9-3.1,3-5.2,3c-2.1,0-4.1-1' +
          '.2-5.2-3l-0.6-1l-0.1-1C10,21.2,10,20.7,10,20.3'#13#10#9#9'v-6c0-0.3,0-0.' +
          '7,0.1-1l0.1-1l0.6-1c0.5-0.8,1.1-1.5,1.8-2l0.9-0.6l1.1-0.3c0.5-0.' +
          '1,0.9-0.2,1.4-0.2s0.9,0.1,1.4,0.2l1,0.2l0.9,0.6'#13#10#9#9'c0.8,0.5,1.4,' +
          '1.2,1.8,2l0.6,1l0.2,1c0.1,0.3,0.1,0.7,0.1,1C22,14.4,22,15.9,22,1' +
          '5.9z M13,18.9h6v3h-6V18.9z M13,12.9h6v3h-6V12.9z"/>'#13#10'</svg>'
      end
      item
        IconName = 'CallStack'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#4488FF" d="M27.5,18.2h-' +
          '3.4v-3.7c0-0.7-0.6-1.3-1.3-1.3H19V9.6c0-0.7-0.6-1.3-1.3-1.3H4.5c' +
          '-0.7,0-1.3,0.6-1.3,1.3v4.9'#13#10#9#9'c0,0.7,0.6,1.3,1.3,1.3h3.9v3.7c0,0' +
          '.7,0.6,1.3,1.3,1.3H13v3.7c0,0.7,0.6,1.3,1.3,1.3h13.2c0.7,0,1.3-0' +
          '.6,1.3-1.3v-4.9'#13#10#9#9'C28.7,18.7,28.2,18.2,27.5,18.2z M5.7,10.8h10.' +
          '7v2.4H5.7V10.8z M10.9,15.8h10.7v2.4H10.9V15.8z M26.2,23.1H15.5v-' +
          '2.4h10.7V23.1z"/>'#13#10#9'<path d="M28.6,28.1H3.4c-1.5,0-2.7-1.2-2.7-2' +
          '.7V6.6c0-1.5,1.2-2.7,2.7-2.7h25.2c1.5,0,2.7,1.2,2.7,2.7v18.8'#13#10#9#9 +
          'C31.3,26.9,30.1,28.1,28.6,28.1z M3.4,6.4c-0.1,0-0.2,0.1-0.2,0.2v' +
          '18.8c0,0.1,0.1,0.2,0.2,0.2h25.2c0.1,0,0.2-0.1,0.2-0.2V6.6'#13#10#9#9'c0-' +
          '0.1-0.1-0.2-0.2-0.2H3.4z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Check'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#FFCE00" d="M10.2,23.1l-' +
          '7.6-7.6L0.1,18l10.1,10.1L31.9,6.4l-2.5-2.5L10.2,23.1z"/>'#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'CmdOuputWin'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="translate(3 2) scale(1' +
          '.2)">'#13#10#9#9'<path fill="#22AA22" d="M6,14.9c0.5,0,1-0.1,1.3-0.5c0.3' +
          '-0.3,0.5-0.7,0.6-1.2h2c0,0.6-0.2,1.2-0.6,1.7s-0.8,0.9-1.4,1.2'#13#10#9 +
          #9#9'c-0.6,0.3-1.3,0.5-1.9,0.4c-1.2,0.1-2.5-0.4-3.3-1.3c-0.8-1-1.2-' +
          '2.3-1.2-3.7v-0.2C1.4,10,1.9,8.8,2.7,7.7c0.8-0.9,2-1.4,3.3-1.3'#13#10#9 +
          #9#9'c1,0,2,0.3,2.9,1C9.5,8.2,10,9.1,10,10.1H8c0-0.5-0.2-1-0.6-1.3C' +
          '6.9,8.4,6.5,8.3,6,8.3c-0.6,0-1.2,0.3-1.6,0.8'#13#10#9#9#9'C4,9.8,3.8,10.6' +
          ',3.8,11.4v0.3c-0.1,0.8,0.1,1.6,0.6,2.3C4.6,14.6,5.3,14.9,6,14.9z' +
          '"/>'#13#10#9'</g>'#13#10#9'<path fill="#22AA22"  d="M20,10h3l4,13h-3z"/>'#13#10#9'<ci' +
          'rcle fill="#22AA22" cx="18" cy="13" r="2"/>'#13#10#9'<circle fill="#22A' +
          'A22" cx="18" cy="19" r="2"/>'#13#10#9'<path d="M28.6,28.1H3.4c-1.5,0-2.' +
          '7-1.2-2.7-2.7V6.6c0-1.5,1.2-2.7,2.7-2.7h25.2c1.5,0,2.7,1.2,2.7,2' +
          '.7v18.8'#13#10#9#9'C31.3,26.9,30.1,28.1,28.6,28.1z M3.4,6.4c-0.1,0-0.2,0' +
          '.1-0.2,0.2v18.8c0,0.1,0.1,0.2,0.2,0.2h25.2c0.1,0,0.2-0.1,0.2-0.2' +
          'V6.6'#13#10#9#9'c0-0.1-0.1-0.2-0.2-0.2H3.4z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CodeComment'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M15.9,14v-2h-4v-4h-2v4H6v-4' +
          'H4v4H0v2h4v4H0v2h4v4h2v-4h4v4h2v-4h4v-2h-4v-4H15.9z M9.9,18H6v-4' +
          'h4V18z"/>'#13#10#9'<path d="M32,14v-2h-4v-4h-2v4h-4v-4h-2v4h-4v2h4v4h-4' +
          'v2h4v4h2v-4h4v4h2v-4h4v-2h-4v-4H32z M26,18h-4v-4h4V18z"/>'#13#10'</svg' +
          '>'#13#10
      end
      item
        IconName = 'CodeExplorer'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<polygon fill="#E24444" points="26.4' +
          ',24 22.8,20.5 20.3,23.1 18.9,24.5 17.2,26.2 18.4,27.4 20.7,29.7 ' +
          '23,27.4 26.1,24.3 "/>'#13#10#9'<path d="M28.6,2.6H11.8l2.5,2.5h8.5c0.3-' +
          '0.2,0.6-0.3,1-0.3c0.3,0,0.6,0.1,0.9,0.3h4c0.1,0,0.2,0.1,0.2,0.1v' +
          '3.9l1.3,1.3'#13#10#9#9'c0.6,0.6,0.6,1.5,0,2.1l-1.3,1.3V24c0,0.2-0.2,0.3-' +
          '0.2,0.3h-0.8c0,0.3-0.2,0.6-0.4,0.8l-1.6,1.6h2.8c1.5,0,2.7-1.2,2.' +
          '7-2.7V5.2'#13#10#9#9'C31.3,3.8,30.1,2.6,28.6,2.6z M3.4,24.2c-0.1,0-0.2-0' +
          '.1-0.2-0.2V13.9l-1.5-1.5c-0.6-0.6-0.6-1.5,0-2.1l1.5-1.5V5.2'#13#10#9#9'c' +
          '0-0.1,0.1-0.1,0.2-0.1h3.4l2.5-2.5H3.4c-1.5,0-2.7,1.2-2.7,2.6V24c' +
          '0,1.5,1.2,2.7,2.7,2.7h12.3c-0.1-0.2-0.1-0.4-0.1-0.6'#13#10#9#9'c0-0.4,0.' +
          '1-0.8,0.4-1.1l0.6-0.7l0.1-0.1H3.4z"/>'#13#10#9'<polygon fill="#FFCE00" ' +
          'points="15.1,8 12.6,5.4 10.6,3.5 8.7,5.4 3.4,10.6 2.8,11.3 3.4,1' +
          '2 7.3,15.8 10.1,13 10.1,13 11.5,11.6"/>'#13#10#9'<polygon points="19.5,' +
          '21.7 18.3,22.9 18.1,23.1 12.9,23.1 12.9,13 10.1,13 11.5,11.6 18.' +
          '4,11.6 17,13 14.3,13 14.3,21.7"/>'#13#10#9'<rect fill="#4488FF" x="17.1' +
          '" y="9" width="10.9" height="7.6" transform="matrix(0.7071 -0.70' +
          '71 0.7071 0.7071 -2.4116 19.7039)"/>'#13#10'</svg>'
      end
      item
        IconName = 'Collapse'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M25.5,22.1v-1.7H6.4v1.7H3.7' +
          'v-5.2h24.5v5.2H25.5z"/>'#13#10#9'<path d="M28.3,9.9v5.2H3.8V9.9h2.7v1.7' +
          'h19V9.9H28.3z"/>'#13#10#9'<path d="M19,3.1l-3,4.6l-3-4.6H19 M22.2,1.4H9' +
          '.8L16,11L22.2,1.4z"/>'#13#10#9'<path d="M13,28.9l3-4.6l3,4.6H13 M9.8,30' +
          '.6h12.3L16,21L9.8,30.6z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Copy'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(69.2 -212)">'#13 +
          #10#9'<path d="M-46.8,212.2h-17.2c-1.6,0-2.9,1.2-2.9,2.9v20.1h2.9v-2' +
          '0.1h17.2V212.2z M-42.5,218h-15.8'#13#10#9#9'c-1.6,0-2.9,1.2-2.9,2.9v20.1' +
          'c0,1.6,1.2,2.9,2.9,2.9h15.8c1.6,0,2.9-1.2,2.9-2.9v-20.1C-39.6,21' +
          '9.2-40.9,218-42.5,218L-42.5,218z'#13#10#9#9' M-42.5,240.9h-15.8v-20.1h15' +
          '.8V240.9z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Cut'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M12.5,9.5C12.8,8.7,13,7.9,1' +
          '3,7c0-3.3-2.7-6-6-6S1,3.7,1,7s2.7,6,6,6c0.9,0,1.7-0.2,2.5-0.5L13' +
          ',16l-3.5,3.5'#13#10#9#9'C8.7,19.2,7.9,19,7,19c-3.3,0-6,2.7-6,6s2.7,6,6,6' +
          's6-2.7,6-6c0-0.9-0.2-1.7-0.5-2.5L16,19l10.5,10.5H31V28L12.5,9.5z' +
          ' M7,10'#13#10#9#9'c-1.6,0-3-1.3-3-3s1.4-3,3-3c1.7,0,3,1.3,3,3S8.6,10,7,1' +
          '0z M7,28c-1.6,0-3-1.3-3-3s1.4-3,3-3c1.7,0,3,1.3,3,3S8.6,28,7,28z' +
          #13#10#9#9' M16,16.7c-0.4,0-0.8-0.3-0.8-0.8c0-0.4,0.3-0.8,0.8-0.8c0.4,0' +
          ',0.8,0.3,0.8,0.8C16.7,16.4,16.4,16.7,16,16.7z M26.5,2.5l-9,9l3,3' +
          #13#10#9#9'L31,4V2.5H26.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Debug'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-262.8 -667.1' +
          '95)">'#13#10#9'<path d="M291.7,682.1l-0.4-2.5l-3.8,0.6c-0.2-0.4-0.4-0.9' +
          '-0.7-1.3l3.2-2.4l-1.6-2l-3.2,2.5'#13#10#9#9'c-0.3-0.4-0.7-0.7-1.1-1l1.4-' +
          '3.6l-2.4-0.9l-1.3,3.3c-0.7-0.3-1.3-0.4-2.1-0.5c-0.6-1.2-1.7-2.1-' +
          '2.9-2.5l0.4-3l-2.5-0.3l-0.4,3.1'#13#10#9#9'c-1.7,0.3-3.1,1.4-3.8,3l-3.3-' +
          '0.4l-0.3,2.5l3,0.3c0.1,1.3,0.7,2.6,1.7,3.4c0,0.2-0.1,0.5-0.1,0.7' +
          'l2.8,1.8c-0.6-1.9,0-4,1.5-5.3'#13#10#9#9'c2.2-1.8,5.8-1,8.1,1.9c2.2,2.8,' +
          '2.2,6.3,0.1,8.1l-4.7,3.2c2.1,0.5,4.3,0,6-1.2c2-1.7,3-4.4,2.6-7L2' +
          '91.7,682.1z M274.5,675.5'#13#10#9#9'c-0.7,0.5-1.2,1.2-1.6,1.9c-0.5-1.3,0' +
          '.1-2.8,1.5-3.3c0.8-0.3,1.7-0.2,2.4,0.3C275.9,674.7,275.2,675,274' +
          '.5,675.5L274.5,675.5z"/>'#13#10#9'<path d="M283.5,684.9l-4.8-6.1l-2,1.6' +
          'l4.8,6.1L283.5,684.9z"/>'#13#10#9'<path fill="#22AA22" d="M280.6,688.4l' +
          '-1.4-0.9l-3.6-2.3l-4.1-2.6l-5.6-3.6v18.8l9-5.7l5.6-3.6L280.6,688' +
          '.4z M268.5,692.9v-9'#13#10#9#9'l2.2,1.4l2.1,1.4l2.8,1.8l-0.4,0.2L268.5,6' +
          '92.9z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'DebugLast'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="translate(-262.8 -667.' +
          '195)">'#13#10#9#9'<path d="M290.4,679.7l-0.3-2.1l-3.2,0.5c-0.2-0.3-0.3-0' +
          '.7-0.6-1.1l2.7-2l-1.3-1.7l-2.7,2.1'#13#10#9#9#9'c-0.2-0.3-0.6-0.6-0.9-0.8' +
          'l1.2-3l-2-0.7l-1.1,2.7c-0.6-0.2-1.1-0.3-1.7-0.4c-0.5-1-1.4-1.7-2' +
          '.4-2.1l0.3-2.5l-2.1-0.2L276,671'#13#10#9#9#9'c-1.4,0.2-2.6,1.2-3.2,2.5l-2' +
          '.7-0.3l-0.2,2.1l2.5,0.2c0.1,1.1,0.6,2.2,1.4,2.8c0,0.2-0.1,0.4-0.' +
          '1,0.6l2.3,1.5'#13#10#9#9#9'c-0.5-1.6,0-3.3,1.2-4.4c1.8-1.5,4.8-0.8,6.7,1.' +
          '6c1.8,2.3,1.8,5.2,0.1,6.7l-1.8,1.2l1.7,1.1c0.4-0.2,0.8-0.4,1.2-0' +
          '.7'#13#10#9#9#9'c1.7-1.4,2.5-3.7,2.2-5.8L290.4,679.7z M276.2,674.3c-0.6,0' +
          '.4-1,1-1.3,1.6c-0.4-1.1,0.1-2.3,1.2-2.7c0.7-0.2,1.4-0.2,2,0.2'#13#10#9 +
          #9#9'C277.4,673.5,276.7,673.9,276.2,674.3z"/>'#13#10#9#9'<path d="M283.7,68' +
          '2l-4-5.1l-1.7,1.3l4,5.1L283.7,682z"/>'#13#10#9'</g>'#13#10#9'<path fill="#22AA' +
          '22" d="M4.4,8.9v18l0.7-0.4c0.1-2.8,2.4-5.1,5.2-5.1H13l3.1-1.9l1.' +
          '6-1l1-0.7L4.4,8.9z M6.9,22.3v-8.6l6.8,4.3L6.9,22.3z"/>'#13#10#9'<path d' +
          '="M18.6,28.8c0.1,0.1,0.1,2,0.1,2h-8.4c-2.2,0-4-1.7-4-4c0-0.3,0.1' +
          '-0.7,0.2-1.1c0.5-1.7,2-2.9,3.8-2.9h7v-3l4,4l-4,4v-3h-7'#13#10#9#9'c-1.1,' +
          '0-2,0.9-2,2s0.9,2,2,2H18.6L18.6,28.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dedent'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M2.5,5.5v3h21v-3H2.5z M2.5,' +
          '26.5h21v-3h-21V26.5z M15.2,15.1h16.1v-3H15.2V15.1z M15.2,20.7H26' +
          'v-3H15.2V20.7z"/>'#13#10#9'<path id="Trazado_1730_10_" d="M12.7,14.9H6v' +
          '-4.2L0.7,16L6,21.3v-4.2h6.6V14.9z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Delete'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#E24444" d="M24.9,10.2L2' +
          '7,5.9L22.5,8l-6.2,6.2L5.6,3.5L3.4,5.8l10.7,10.7l-9,9l2.2,2.2l9-9' +
          'l9,9l3.3,0.7l-1.2-3l-8.9-8.9L24.9,10.2z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Down'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M9,4H15V12H19.84L12,19.8' +
          '4L4.16,12H9V4Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Download'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M25.7,13.4c-0.9-4.6-5-8.1-9' +
          '.7-8.1c-3.8,0-7.2,2.2-8.9,5.4c-4,0.4-7.1,3.8-7.1,7.9c0,4.4,3.5,8' +
          ',8,8h17.3'#13#10#9#9'c3.6,0,6.6-3,6.6-6.6C31.9,16.4,29.2,13.6,25.7,13.4z' +
          ' M25.3,24H8c-2.9,0-5.3-2.4-5.3-5.3c0-2.7,2-5,4.7-5.3l1.4-0.2l0.7' +
          '-1.2'#13#10#9#9'c1.2-2.4,3.7-4,6.5-4c3.5,0,6.5,2.5,7.2,5.8l0.4,2l2,0.2c2' +
          ',0.1,3.7,1.9,3.7,3.9C29.3,22.2,27.5,24,25.3,24z M17.9,13.3h-3.8v' +
          '4h-3.4'#13#10#9#9'l5.3,5.3l5.3-5.3H18L17.9,13.3L17.9,13.3z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Edit'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M14.06,9L15,9.94L5.92,19H' +
          '5V18.08L14.06,9M17.66,3C17.41,3 17.15,3.1 16.96,3.29L15.13,5.12L' +
          '18.88,8.87L20.71,7.04C21.1,6.65 21.1,6 20.71,5.63L18.37,3.29C18.' +
          '17,3.09 17.92,3 17.66,3M14.06,6.19L3,17.25V21H6.75L17.81,9.94L14' +
          '.06,6.19Z" />'#13#10'</svg>'
      end
      item
        IconName = 'EditorMax'
        SVGText = 
          '<svg viewBox="0 0 512 512">'#13#10'  <path d="M224.971,224.971a24,24,0' +
          ',0,1-33.942,0L88,121.941V88h33.941l103.03,103.029A24,24,0,0,1,22' +
          '4.971,224.971ZM424,424V390.059L320.971,287.029a24,24,0,0,0-33.94' +
          '2,33.942L390.059,424ZM72,176V72H176a24,24,0,0,0,0-48H48A24,24,0,' +
          '0,0,24,48V176a24,24,0,0,0,48,0ZM488,464V336a24,24,0,0,0-48,0V440' +
          'H336a24,24,0,0,0,0,48H464A24,24,0,0,0,488,464Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Editor'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M10 20H6V4H13V9H18V12.1L' +
          '20 10.1V8L14 2H6C4.9 2 4 2.9 4 4V20C4 21.1 4.9 22 6 22H10V20M20.' +
          '2 13C20.3 13 20.5 13.1 20.6 13.2L21.9 14.5C22.1 14.7 22.1 15.1 2' +
          '1.9 15.3L20.9 16.3L18.8 14.2L19.8 13.2C19.9 13.1 20 13 20.2 13M2' +
          '0.2 16.9L14.1 23H12V20.9L18.1 14.8L20.2 16.9Z" />'#13#10'</svg>'
      end
      item
        IconName = 'EditOptions'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M14.06,9L15,9.94L5.92,19H' +
          '5V18.08L14.06,9M17.66,3C17.41,3 17.15,3.1 16.96,3.29L15.13,5.12L' +
          '18.88,8.87L20.71,7.04C21.1,6.65 21.1,6 20.71,5.63L18.37,3.29C18.' +
          '17,3.09 17.92,3 17.66,3M14.06,6.19L3,17.25V21H6.75L17.81,9.94L14' +
          '.06,6.19Z" />'#13#10'<path d="M7,22H9V24H7V22M11,22H13V24H11V22M15,22H' +
          '17V24H15V22Z" />'#13#10'</svg>'
      end
      item
        IconName = 'EditorMin'
        SVGText = 
          '<svg viewBox="0 0 512 512">'#13#10'  <path d="M134.059,168,31.029,64.9' +
          '71A24,24,0,0,1,64.971,31.029L168,134.059V168ZM480.971,447.029,37' +
          '7.941,344H344v33.941l103.029,103.03a24,24,0,0,0,33.942-33.942ZM2' +
          '32,208V80a24,24,0,0,0-48,0V184H80a24,24,0,0,0,0,48H208A24,24,0,0' +
          ',0,232,208Zm96,224V328H432a24,24,0,0,0,0-48H304a24,24,0,0,0-24,2' +
          '4V432a24,24,0,0,0,48,0Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Execute'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#22AA22" d="M14.2,32h-1.' +
          '8l1.8-12.5H7.9c-1.5,0-0.6-1.3-0.6-1.4C9.6,14.1,13,8,17.7,0h1.8l-' +
          '1.7,12.5h6.3c0.7,0,1.1,0.4,0.7,1.2'#13#10#9#9'C17.7,25.9,14.2,32,14.2,32' +
          'z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Exit'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M24.3,9.4L22,11.7l2.7,2.7H1' +
          '1v3.3h13.6L22,20.3l2.3,2.3l6.6-6.6L24.3,9.4z M4.4,4.4H16V1.1H4.4' +
          'c-1.9,0-3.3,1.4-3.3,3.3'#13#10#9#9'v23.2c0,1.9,1.4,3.3,3.3,3.3H16v-3.3H4' +
          '.4V4.4z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Expand'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M25.7,6.7V5H6.3v1.7H3.6V1.3' +
          'h24.9v5.3H25.7z"/>'#13#10#9'<path d="M28.3,25.3v5.3H3.5v-5.3h2.7V27h19.' +
          '3v-1.7H28.3z"/>'#13#10#9'<path d="M18.9,19.3l-3,4.7l-3-4.7H18.9 M22.3,1' +
          '7.4H9.7l6.3,9.8L22.3,17.4z"/>'#13#10#9'<path d="M12.9,12.7l3-4.7l3,4.7H' +
          '12.9 M9.7,14.4h12.5l-6.3-9.8L9.7,14.4z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ExternalRun'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M30.6,2.6H1.4c-0.7,0-1.3,0.' +
          '6-1.2,1.3v2L1,6.4l1.9,1.2V5.3h26.3v18.8H11.6l-0.2,0.1l-4.3,2.7h2' +
          '3.4c0.7,0,1.3-0.6,1.3-1.4'#13#10#9#9'V3.9C31.9,3.2,31.4,2.6,30.6,2.6z"/>' +
          #13#10#9'<path fill="#22AA22" d="M2.9,9.4L0.1,7.7l0,0v21.7l4-2.5l4.4-2' +
          '.8l2-1.3l3.7-2.3l1.9-1.2l1.2-0.8L2.9,9.4z M3.2,23.8V13.4l8.2,5.2' +
          #13#10#9#9'L3.2,23.8z"/>'#13#10'</svg>'
      end
      item
        IconName = 'ExternalRunLast'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M30.6,0.3h-29c-0.8,0-1.4,0.' +
          '6-1.4,1.4v2.2c0.1,0,0.2,0,0.3,0c0.3,0,0.6,0.1,0.8,0.3l1.5,1V3h26' +
          '.3v18.8h-7.7l0.4,0.4'#13#10#9#9'c0.6,0.6,0.6,1.6,0,2.3v0.1h8.7c0.7,0,1.3' +
          '-0.6,1.3-1.4V1.6C31.9,0.9,31.3,0.3,30.6,0.3z"/>'#13#10#9'<path fill="#2' +
          '2AA22" d="M0.1,5.4v21.7l0.8-0.5c0.1-3.4,2.9-6.1,6.3-6.1h3.3l3.7-' +
          '2.3l1.9-1.2l1.2-0.8L0.1,5.4z M3.2,21.5V11.1l8.2,5.2'#13#10#9#9'L3.2,21.5' +
          'z"/>'#13#10#9'<path d="M17.2,29.3c0.1,0.1,0.1,2.4,0.1,2.4H7.2c-2.6,0-4.' +
          '8-2.1-4.8-4.8c0-0.4,0.1-0.9,0.2-1.3c0.6-2,2.4-3.5,4.6-3.5h8.4v-3' +
          '.6'#13#10#9#9'l4.8,4.8l-4.8,4.8v-3.6H7.2c-1.3,0-2.4,1.1-2.4,2.4s1.1,2.4,' +
          '2.4,2.4L17.2,29.3L17.2,29.3z"/>'#13#10'</svg>'
      end
      item
        IconName = 'ExternalRunSetup'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M19.9,31.2v-2.6h2.6v2.6H19.' +
          '9 M14.7,31.2v-2.6h2.6v2.6H14.7 M9.5,31.2v-2.6h2.6v2.6H9.5z"/>'#13#10#9 +
          '<path d="M30.6,1.7H1.3C0.6,1.7,0,2.3,0.1,3v2l0.8,0.5l1.9,1.2V4.4' +
          'h26.3v18.8H11.5l-0.2,0.1L7.1,26h23.4c0.7,0,1.3-0.6,1.3-1.4V3'#13#10#9#9 +
          'C31.9,2.3,31.3,1.7,30.6,1.7z"/>'#13#10#9'<path fill="#22AA22" d="M2.8,8' +
          '.6L0.1,6.9l0,0v21.7l4-2.5l4.4-2.8l2-1.3l3.7-2.3l1.9-1.2l1.2-0.8L' +
          '2.8,8.6z M3.2,23V12.6l8.2,5.2'#13#10#9#9'L3.2,23z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Favorite'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M16,9.5l1.5,3.4l0.7,1.7l1.8' +
          ',0.1l3.7,0.3l-2.8,2.4l-1.4,1.2l0.4,1.8l0.8,3.6l-3.2-1.9l-1.5-1L1' +
          '4.5,22l-3.2,1.9l0.8-3.6'#13#10#9#9'l0.4-1.8l-1.4-1.2l-2.8-2.4l3.7-0.3l1.' +
          '8-0.2l0.7-1.7L16,9.5 M16,1.8l-4.2,9.9L1,12.6l8.2,7.1L6.7,30.2l9.' +
          '3-5.6l9.3,5.6l-2.5-10.5'#13#10#9#9'l8.2-7.1l-10.8-0.9L16,1.8z"/>'#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'FileExplorer'
        SVGText = 
          '<svg viewBox="0 0 512 512">'#13#10#9#9'<polygon points=" 331.636,270.546' +
          ' 171.636,270.546 171.636,200 302.545,200 '#13#10#9#9#9'130.909,200 130.90' +
          '9,456.728 331.636,456.728 331.636,494.546 331.636,416 171.63,416' +
          ' 171.636,311.273 331.636,311.273"/>'#13#10#13#10#9#9'<polygon fill="#FFCE00"' +
          ' points=" 280,20 20,20 20,200 280,200"/>'#13#10#9#9'<polygon points="490' +
          ',349.091 490,230 331.636,230 331.636,349.091"/>'#13#10#9#9'<polygon poin' +
          'ts="490,494.546 490,378.183  330,378.183 330,494.546"/>'#13#10#9#9' '#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'FileNew'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-272.1 -317.4' +
          ')">'#13#10#9'<path d="M290.9,318.8h-11.3c-1.6,0-2.9,1.3-2.8,2.9v23.4c0,' +
          '1.6,1.2,2.9,2.8,2.9h17c1.6,0,2.9-1.3,2.8-2.9v-17.6'#13#10#9#9'L290.9,318' +
          '.8z M279.6,345.2v-23.4h9.9v7.3h7.1v16.1H279.6z"/>'#13#10'</g>'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = 'FileOpen'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M22.9,9.8c0.7,1.2,1.3,2.7,1' +
          '.7,4.3h0.4v-1.8C24.9,10.9,24,9.9,22.9,9.8z"/>'#13#10#9'<path d="M31.6,1' +
          '9.1l-4.2,4.7l-2.5,2.7c-0.3,0.3-0.8,0.6-1.2,0.6h-0.6c-0.2,0.1-0.4' +
          ',0.1-0.7,0.1H2.5c-1.4,0-2.5-1.1-2.5-2.5v-15'#13#10#9#9'c0-1.3,1.1-2.5,2.' +
          '5-2.5H10l2.5,2.5h3.2c0.1,0.2,0.2,0.3,0.2,0.5c0.2,0.7,0.5,1.3,0.6' +
          ',2h-14v10.2l5.1-5.6c0.3-0.3,0.8-0.6,1.2-0.6'#13#10#9#9'h4.6l0.8,0.8l1.6,' +
          '1.7H9.1l-5.2,5.8h19.4l1.7-1.8l2.5-2.7l1.2-1.2h-3l0.3-0.3l1.2-1.3' +
          'l0.3-0.3l0.5-0.5h2.3c0.7,0,1.2,0.4,1.6,1'#13#10#9#9'C32.1,17.8,32,18.6,3' +
          '1.6,19.1z"/>'#13#10#9'<g transform="translate(119.5 -231.7)">'#13#10#9#9'<path ' +
          'd="M-92.4,247l-0.9,0.9l-1.2,1.3l-1.2,1.2l-1.3,1.4l-1.7,1.8l-3.1-' +
          '3.2l-2.4-2.5l-0.9-0.9h3.7c0-1.1-0.1-2.2-0.2-3.1'#13#10#9#9#9'c-0.2-0.9-0.' +
          '4-1.7-0.7-2.5c-0.7-1.9-2-3.5-3.7-4.8c3.1,0.7,6.1,2.1,8.1,4.8c1.1' +
          ',1.5,1.7,3.3,2.1,5.6H-92.4z"/>'#13#10#9'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'FileProperties'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="translate(-272.1 -317.' +
          '4)">'#13#10#9#9'<path d="M303.4,327.5v17.6c0.1,1.6-1.2,2.9-2.8,2.9h-15.3' +
          'l0.5-2.8h14.9v-16.1h-7.1v-7.3h-9.9v5.1h-2.9v-5.2'#13#10#9#9#9'c-0.1-1.6,1' +
          '.2-2.9,2.8-2.9h11.3L303.4,327.5z"/>'#13#10#9'</g>'#13#10#9'<path d="M11.5,17.1' +
          'c-0.5-0.3-1-0.4-1.6-0.4c-0.4,0-0.8,0.1-1.2,0.2c-1.5,0.5-2.6,1.9-' +
          '2.6,3.5c0,1.7,1.1,3.1,2.6,3.5'#13#10#9#9'c0.4,0.1,0.8,0.2,1.2,0.2c0.6,0,' +
          '1.1-0.1,1.6-0.4c1.3-0.6,2.1-1.9,2.1-3.4C13.6,19,12.7,17.7,11.5,1' +
          '7.1z M11.5,21.4'#13#10#9#9'c-0.3,0.6-0.9,0.9-1.6,0.9c-0.4,0-0.9-0.2-1.2-' +
          '0.4C8.3,21.5,8,21,8,20.5c0-0.6,0.3-1.1,0.7-1.4c0.3-0.3,0.7-0.4,1' +
          '.2-0.4'#13#10#9#9'c0.7,0,1.3,0.4,1.6,0.9c0.2,0.3,0.3,0.6,0.3,0.9C11.7,20' +
          '.8,11.6,21.1,11.5,21.4z"/>'#13#10#9'<path d="M16.8,21.4c0.1-0.3,0.1-0.6' +
          ',0.1-0.9s0-0.6-0.1-0.9l2-1.6c0.2-0.1,0.3-0.4,0.1-0.6L17,14.2c-0.' +
          '1-0.1-0.3-0.3-0.4-0.3h-0.2'#13#10#9#9'l-2.3,0.9c-0.5-0.4-1-0.7-1.6-0.9l-' +
          '0.4-2.5c0-0.3-0.3-0.4-0.4-0.4H8c-0.3,0-0.4,0.2-0.4,0.4l-0.4,2.4c' +
          '-0.6,0.3-1.1,0.6-1.6,0.9'#13#10#9#9'l-2.3-0.9H3.1c-0.2,0-0.3,0.1-0.4,0.3' +
          'l-1.9,3.2c-0.1,0.2-0.1,0.4,0.1,0.6l2,1.6c-0.1,0.3-0.1,0.6-0.1,0.' +
          '9s0,0.6,0.1,0.9l-2,1.6'#13#10#9#9'c-0.2,0.1-0.3,0.4-0.1,0.6l1.9,3.2C2.8,' +
          '26.8,3,27,3.1,27h0.2l2.3-0.9c0.5,0.4,1,0.7,1.6,0.9l0.4,2.5c0,0.3' +
          ',0.3,0.4,0.4,0.4h3.8'#13#10#9#9'c0.3,0,0.4-0.2,0.4-0.4l0.3-1.8l0.1-0.8c0' +
          '.6-0.3,1.1-0.6,1.6-0.9l2.3,0.9h0.2c0.2,0,0.3-0.1,0.4-0.3l1.9-3.2' +
          #13#10#9#9'c0.1-0.2,0.1-0.4-0.1-0.6L16.8,21.4z M15,19.8c0.1,0.3,0.1,0.5' +
          ',0.1,0.7s0,0.4-0.1,0.7l-0.1,1.1l0.8,0.7l1,0.8L16,24.9l-1.2-0.5'#13#10 +
          #9#9'l-1-0.4L13,24.6c-0.4,0.3-0.8,0.5-1.2,0.7l-0.3,0.1l-0.7,0.3l-0.' +
          '1,1.1l-0.2,1.3H9.2L9,26.8l-0.1-1.1l-0.2-0.1l-0.8-0.3'#13#10#9#9'c-0.4-0.' +
          '2-0.8-0.4-1.1-0.7L5.9,24l-1,0.4l-1.2,0.5l-0.6-1.1l1-0.8l0.8-0.6l' +
          '-0.1-1.1c0-0.3-0.1-0.5-0.1-0.7s0-0.4,0.1-0.7l0.1-1.1'#13#10#9#9'l-0.8-0.' +
          '7l-1-0.8l0.6-1.1l1.2,0.5l1,0.4l0.9-0.6C7.2,16.2,7.6,16,8,15.8l0.' +
          '8-0.3l0.3-0.1l0.1-1.1L9.4,13h1.3l0.2,1.3l0.1,1.1'#13#10#9#9'l0.6,0.2l0.4' +
          ',0.1c0.4,0.2,0.8,0.4,1.1,0.7L14,17l1-0.4l1.2-0.5l0.6,1.1l-1,0.8l' +
          '-1,0.8L15,19.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Filter'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M15,19.88C15.04,20.18 14.' +
          '94,20.5 14.71,20.71C14.32,21.1 13.69,21.1 13.3,20.71L9.29,16.7C9' +
          '.06,16.47 8.96,16.16 9,15.87V10.75L4.21,4.62C3.87,4.19 3.95,3.56' +
          ' 4.38,3.22C4.57,3.08 4.78,3 5,3V3H19V3C19.22,3 19.43,3.08 19.62,' +
          '3.22C20.05,3.56 20.13,4.19 19.79,4.62L15,10.75V19.88M7.04,5L11,1' +
          '0.06V15.58L13,17.58V10.05L16.96,5H7.04Z" />'#13#10'</svg>'
      end
      item
        IconName = 'FindNext'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path id="Trazado_1710_3_" d="M19.8,' +
          '0C13.6,0,8.6,4.9,8.6,11.1c0,2.7,1,5.2,2.8,7.2l-0.5,0.5H9.6L1,27.' +
          '4l2.6,2.6l8.6-8.6V20'#13#10#9#9'l0.5-0.5c0.7,0.7,1.5,1.1,2.4,1.6h4.8l-0.' +
          '4-0.4l-1.2-1.2l-1.3-1.2c-0.2-0.1,0,0-0.2-0.1c-2.7-1.1-4.6-3.9-4.' +
          '6-6.9v-0.1'#13#10#9#9'c0-4.3,3.4-7.7,7.7-7.7s7.7,3.4,7.7,7.7c0,2.2-1,4.2' +
          '-2.4,5.6l2.5,2.5c2.1-2,3.4-4.8,3.4-8C30.9,4.9,25.9,0,19.8,0z"/>'#13 +
          #10'  <g transform="translate(-149.74 -448.98)">'#13#10#9#9#9'<polygon point' +
          's="180.2,473.5 179.5,474.2 173.4,480.4 172.7,481 170.3,478.5 171' +
          ',477.9 173.5,475.2 165.7,475.2 165.7,471.8 '#13#10#9#9#9#9'173.5,471.8 171' +
          ',469.1 170.3,468.5 172,466.8 172.7,466 "/>'#13#10#9'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'FindPrevious'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M19.7,0C13.7,0,8.8,4.9,8.8,' +
          '11c0,2.6,1,5.2,2.7,7.1L11,18.6H9.5L1.2,27l2.6,2.6l8.4-8.4v-1.4l0' +
          '.6-0.6'#13#10#9#9'c1.2,1.1,2.8,2,4.5,2.3l3.1-3c-0.2,0-0.4,0-0.6,0c-4.2,0' +
          '-7.6-3.4-7.6-7.6s3.4-7.7,7.6-7.7s7.6,3.4,7.6,7.7c0,2.2-0.9,4.2-2' +
          '.5,5.6'#13#10#9#9'l2.2,2.2l0.3,0.3c2.2-2,3.5-4.9,3.5-8C30.7,4.8,25.7,0,1' +
          '9.7,0z"/>'#13#10#9'<polygon points="30.4,26.3 22.6,26.3 25.3,28.8 25.9,' +
          '29.6 24.1,31.2 23.5,32 16.1,24.6 16.7,23.9 18.9,21.8 23.5,17.2 2' +
          '5.9,19.6 '#13#10#9#9'25.3,20.3 25.3,20.4 22.6,22.8 30.4,22.8 '#9#9'"/>'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'FindRefresh'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M27.9,11.2h3.6C30.6,5.1,25.' +
          '4,0.3,19.1,0.3c-3.6,0-6.7,1.4-9,3.8L6.5,0.3v10.8h10.8l-4.5-4.5C1' +
          '4.4,5.1,16.7,4,19.2,4'#13#10#9#9'C23.4,4,27.3,7.1,27.9,11.2z M0.3,29l2.7' +
          ',2.7l8.8-8.6c2,1.6,4.7,2.5,7.4,2.5c3.6,0,6.7-1.4,9-3.8l3.6,3.8V1' +
          '4.8H20.9l4.5,4.5'#13#10#9#9'c-1.6,1.6-3.8,2.7-6.3,2.7c-4.3,0-7.9-3.1-8.8' +
          '-7.2H6.8c0.4,2,1.1,4,2.3,5.6C9.1,20.4,0.3,29.2,0.3,29z"/>'#13#10'</svg' +
          '>'#13#10
      end
      item
        IconName = 'FindResults'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M28.6,28.1H3.4c-1.5,0-2.7-1' +
          '.2-2.7-2.7V6.6c0-1.5,1.2-2.7,2.7-2.7h25.2c1.5,0,2.7,1.2,2.7,2.7v' +
          '18.8'#13#10#9#9'C31.3,26.9,30.1,28.1,28.6,28.1z M3.4,6.4c-0.1,0-0.2,0.1-' +
          '0.2,0.2v18.8c0,0.1,0.1,0.2,0.2,0.2h25.2c0.1,0,0.2-0.1,0.2-0.2V6.' +
          '6'#13#10#9#9'c0-0.1-0.1-0.2-0.2-0.2H3.4z"/>'#13#10#9'<path transform="scale(1.1' +
          ') translate(-2,-2)" fill="#4488FF" d="M15.3,22.1c1.4,0,2.6-0.5,3' +
          '.7-1.1l3.7,3.7l2.1-2.1l-3.7-3.7c0.6-1.1,1.1-2.3,1.1-3.7c0-3.8-3.' +
          '1-6.9-6.9-6.9'#13#10#9#9's-6.9,3.1-6.9,6.9C8.4,19,11.4,22.1,15.3,22.1z M' +
          '15.3,11.4c2.1,0,3.8,1.7,3.8,3.8S17.3,19,15.3,19s-3.8-1.7-3.8-3.8' +
          #13#10#9#9'S13.1,11.4,15.3,11.4z "/>'#13#10'</svg>'
      end
      item
        IconName = 'FolderAdd'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M12.6,22.8H3.3V9.1h22.1v1c0' +
          '.1,0,0.2,0.1,0.2,0.1c0.9,0.3,1.7,0.8,2.5,1.3V9.2c0-1.6-1.3-2.7-2' +
          '.7-2.7H14.4l-2.7-2.7H3.4'#13#10#9#9'C1.8,3.7,0.7,5,0.7,6.4V23c0,1.6,1.2,' +
          '2.7,2.7,2.7h10.9L12.6,22.8z"/>'#13#10#9'<path d="M22.5,28.3c-4.8,0-8.8-' +
          '3.9-8.8-8.8c0-4.8,3.9-8.8,8.8-8.8s8.8,3.9,8.8,8.8S27.3,28.3,22.5' +
          ',28.3z M22.5,12.7'#13#10#9#9'c-3.8,0-6.8,3.1-6.8,6.8c0,3.8,3.1,6.8,6.8,6' +
          '.8c3.8,0,6.8-3.1,6.8-6.8S26.3,12.7,22.5,12.7z"/>'#13#10#9'<polygon poin' +
          'ts="27.8,18.6 27.8,20.3 23.3,20.4 23.3,24.9 21.6,24.9 21.6,20.4 ' +
          '17.2,20.4 17.2,18.6 21.6,18.6 21.6,14.3 23.3,14.3 '#13#10#9#9'23.3,18.6 ' +
          #9'"/>'#13#10'</svg>'
      end
      item
        IconName = 'Folders'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M16.4,19.9h10.4c1,0,1.7-0.8,' +
          '1.7-1.8v-5.2c0-1-0.8-1.8-1.7-1.8H16.4c-1,0-1.8,0.8-1.8,1.8v1.2h-' +
          '4.4V10h5.2'#13#10#9'c1,0,1.8-0.8,1.8-1.8V3c0-1-0.8-1.8-1.8-1.8H5C4,1.2,' +
          '3.3,2,3.3,3v5.2c0,1,0.8,1.8,1.7,1.8h2.5v20.4c0,0.2,0.2,0.4,0.4,0' +
          '.4H10'#13#10#9'c0.2,0,0.4-0.2,0.4-0.4v-3.2h4.4v1.2c0,1,0.8,1.8,1.8,1.8H' +
          '27c1,0,1.7-0.8,1.7-1.8v-5.2c0-1-0.8-1.8-1.7-1.8H16.6'#13#10#9'c-1,0-1.8' +
          ',0.8-1.8,1.8v1.2h-4.4v-7.5h4.4v1.3C14.6,19.1,15.4,19.9,16.4,19.9' +
          'z M17.3,13.9h8.5v3.3h-8.5V13.9z M5.9,4h8.5v3.3H5.9V4z'#13#10#9' M17.3,2' +
          '4.1h8.5v3.3h-8.5V24.1z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Font'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M17,8H20V20H21V21H17V20H1' +
          '8V17H14L12.5,20H14V21H10V20H11L17,8M18,9L14.5,16H18V9M5,3H10C11.' +
          '11,3 12,3.89 12,5V16H9V11H6V16H3V5C3,3.89 3.89,3 5,3M6,5V9H9V5H6' +
          'Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Forward'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M4,16C4,9.4,9.4,4,16,4s12,5' +
          '.4,12,12s-5.4,12-12,12S4,22.7,4,16 M1,16c0,8.3,6.7,15,15,15s15-6' +
          '.7,15-15S24.3,1,16,1'#13#10#9#9'C7.7,1,1,7.8,1,16L1,16z M16,14.5h-6v3h6V' +
          '22l6-6l-6-6V14.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Function'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M12.42,5.29C11.32,5.19 10' +
          '.35,6 10.25,7.11L10,10H12.82V12H9.82L9.38,17.07C9.18,19.27 7.24,' +
          '20.9 5.04,20.7C3.79,20.59 2.66,19.9 2,18.83L3.5,17.33C3.83,18.38' +
          ' 4.96,18.97 6,18.63C6.78,18.39 7.33,17.7 7.4,16.89L7.82,12H4.82V' +
          '10H8L8.27,6.93C8.46,4.73 10.39,3.1 12.6,3.28C13.86,3.39 15,4.09 ' +
          '15.66,5.17L14.16,6.67C13.91,5.9 13.23,5.36 12.42,5.29M22,13.65L2' +
          '0.59,12.24L17.76,15.07L14.93,12.24L13.5,13.65L16.35,16.5L13.5,19' +
          '.31L14.93,20.72L17.76,17.89L20.59,20.72L22,19.31L19.17,16.5L22,1' +
          '3.65Z" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'GoToError'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'    <defs>'#13#10'        <clipPath id="cli' +
          'p">'#13#10'            <path d="M0 0v32h14v-20h18v-12z"/>'#13#10'        </c' +
          'lipPath>'#13#10'    </defs>'#13#10'    <g clip-path="url(#clip)">'#13#10#9#9'<path d' +
          '="M9,23.3h13.9v2.9H9V23.3z"/>'#13#10#9#9'<path d="M9,5.9h13.9v2.9H9V5.9z' +
          '"/>'#13#10#9#9#9'<path d="M6.2,12.8V3.1c0-0.1,0.1-0.2,0.2-0.2h19.3c0.1,0,' +
          '0.2,0.1,0.2,0.2v9.7h2.8V3.1c0-1.6-1.4-3.1-3.1-3.1H6.4'#13#10#9#9#9#9'C4.8,' +
          '0,3.3,1.4,3.3,3.1v9.7H6.2z"/>'#13#10#9#9#9'<path d="M25.9,19.2l-0.1,9.7c0' +
          ',0.1-0.1,0.3-0.2,0.3H6.4c-0.1,0-0.2-0.1-0.2-0.2v-9.8H3.3v9.6c0,1' +
          '.6,1.4,3.1,3.1,3.1h19.3'#13#10#9#9#9#9'c1.6,0,2.9-1.2,3.1-2.9v-9.8H25.9z"/' +
          '>'#13#10'    </g>'#13#10'    <g transform="matrix(.41205 -.43746 .39483 .419' +
          '17 9.85 23.27)">'#13#10'        <path d="M28 9.9h-4.2c-.7-1.2-1.6-2.2-' +
          '2.7-2.9l2.4-2.4-2.1-2.1-3.3 3.3a7.61 7.61 0 0 0-4.2 0l-3.3-3.3-2' +
          '.1 2.2 2.4 2.4c-1.1.8-2 1.8-2.7 2.9H4v3h3.1c-.1.4-.1.9-.1 1.4v1.' +
          '5H4v3h3v1.5c0 .5.1 1 .1 1.5H4v3h4.2c1.6 2.7 4.5 4.5 7.8 4.5s6.2-' +
          '1.8 7.8-4.5H28v-3h-3.1c.1-.5.1-1 .1-1.5v-1.5h3v-3h-3v-1.5c0-.5-.' +
          '1-1-.1-1.5H28zm-6 6v4.5c0 .3 0 .7-.1 1l-.1 1-.6 1a6 6 0 0 1-5.2 ' +
          '3c-2.1 0-4.1-1.2-5.2-3l-.6-1-.1-1c-.1-.2-.1-.7-.1-1.1v-6c0-.3 0-' +
          '.7.1-1l.1-1 .6-1c.5-.8 1.1-1.5 1.8-2l.9-.6 1.1-.3c.5-.1.9-.2 1.4' +
          '-.2.5 0 .9.1 1.4.2l1 .2.9.6c.8.5 1.4 1.2 1.8 2l.6 1 .2 1c.1.3.1.' +
          '7.1 1v1.7zm-9 3h6v3h-6zm0-6h6v3h-6z" id="path2-2"/>'#13#10'    </g>'#13#10' ' +
          '   <path fill="#4488FF" d="M2.6,12.8H1.3H0v6.4l12.7,0.1H14v-6.6H' +
          '2.6z M4.9,18c-1.1,0-2-0.9-2-2c0-1.1,0.9-2,2-2c1.1,0,2,0.9,2,2'#13#10' ' +
          '       C6.9,17.1,6,18,4.9,18z"/>'#13#10'</svg>'
      end
      item
        IconName = 'GoToLine'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<rect x="9" y="23.3" width="13.9" hei' +
          'ght="2.9"/>'#13#10'<rect x="9" y="5.9" width="13.9" height="2.9"/>'#13#10'<p' +
          'ath d="M28.7,3.1v9.7h-2.8V3.1c0-0.1-0.1-0.2-0.2-0.2H6.4C6.3,2.9,' +
          '6.2,3,6.2,3.1v9.7H3.3V3.1C3.3,1.4,4.8,0,6.4,0h19.2'#13#10#9'C27.3,0,28.' +
          '7,1.5,28.7,3.1z"/>'#13#10'<path d="M25.9,19.1v0.1h0l0,3.5l-0.1,6.2c0,0' +
          '.1-0.1,0.3-0.2,0.3H6.4c-0.1,0-0.2-0.1-0.2-0.2v-9.9H3.3v9.7c0,1.6' +
          ',1.4,3.1,3.1,3.1'#13#10#9'h19.3c1.6,0,2.9-1.2,3.1-2.9v-9.9H25.9z"/>'#13#10'<p' +
          'ath fill="#4488FF" d="M28.7,12.9v-0.1h-2.8H6.2H3.3h-3V19h3v0.1h2' +
          '5.5h3v-6.2H28.7z M6.1,17.5c-0.3,0.2-0.7,0.3-1,0.3'#13#10#9'c-0.9,0-1.6-' +
          '0.6-1.8-1.4c-0.1-0.1-0.1-0.2-0.1-0.4s0-0.3,0.1-0.5c0.2-0.8,1-1.4' +
          ',1.8-1.4c0.4,0,0.8,0.1,1,0.4'#13#10#9'c0.5,0.4,0.8,0.9,0.8,1.5S6.6,17.1' +
          ',6.1,17.5z M28.7,16.5c-0.2,0.8-1,1.3-1.8,1.3c-0.4,0-0.8-0.1-1-0.' +
          '3c-0.5-0.4-0.8-0.9-0.8-1.5'#13#10#9's0.3-1.1,0.8-1.5c0.3-0.2,0.7-0.4,1-' +
          '0.4c0.9,0,1.6,0.6,1.8,1.3c0,0.2,0.1,0.3,0.1,0.5C28.8,16.2,28.7,1' +
          '6.3,28.7,16.5z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Help'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M14.5,25.1h3v-3h-3V25.1z M16' +
          ',0.8C7.6,0.8,0.8,7.6,0.8,16S7.6,31.2,16,31.2S31.2,24.4,31.2,16S2' +
          '4.3,0.8,16,0.8z M16,28.2'#13#10#9'C9.3,28.2,3.8,22.7,3.8,16S9.3,3.8,16,' +
          '3.8S28.2,9.3,28.2,16S22.7,28.2,16,28.2z M16,6.9c-3.3,0-6.1,2.7-6' +
          '.1,6.1h3c0-1.7,1.4-3,3-3'#13#10#9'c1.7,0,3,1.3,3,3c0,3-4.6,2.6-4.6,7.6h' +
          '3c0-3.4,4.6-3.8,4.6-7.6C22.1,9.6,19.3,6.9,16,6.9z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Highlight'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<rect fill="#FFCE00" x="2.3" y="26.9' +
          '" width="24.9" height="5"/>'#13#10#9'<path d="M29.3,13.1L17.6,0.7C16.9,' +
          '0,15.7,0,15,0.6l-3.9,3.7c-0.4,0.4-0.6,0.7-0.6,1.2s0.1,1,0.5,1.4l' +
          '1.7,1.9l-1,1'#13#10#9#9'c-0.5,0.5-0.7,1.2-0.5,1.9c-0.2,0.1-0.6,0.2-0.9,0' +
          '.5l-7.1,6.7c-0.4,0.4-0.6,0.9-0.6,1.4v1.4c0,0.5,0.2,1,0.5,1.2L4.3' +
          ',24'#13#10#9#9'c0.4,0.4,0.9,0.6,1.4,0.6h7.6c0.5,0,1.1-0.2,1.4-0.6l3.4-4c' +
          '0.2-0.2,0.4-0.5,0.4-0.7c0.1,0,0.2,0,0.4,0l0,0c0.5,0,1-0.1,1.2-0.' +
          '5'#13#10#9#9'l1-1l1.4,1.4c0.4,0.4,0.9,0.6,1.4,0.6c0.5,0,0.9-0.1,1.2-0.5l' +
          '4.1-3.9c0.4-0.4,0.4-0.6,0.4-1.1C29.9,13.9,29.7,13.4,29.3,13.1z'#13#10 +
          #9#9#9'M12.6,20.9H6.8l5-4.7l2.6,2.7L12.6,20.9z M24.2,15.4l-9.1-9.6l1' +
          '.2-1.1l9.1,9.6L24.2,15.4z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Indent'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M2.6,5.5v3h21v-3H2.6z M2.6,' +
          '26.5h21v-3h-21V26.5z M15.2,15.1h16.1v-3H15.2V15.1z M15.2,20.7h10' +
          '.9v-3H15.2V20.7z"/>'#13#10#9'<path id="Trazado_1730_9_" d="M0.7,17.2h6.' +
          '6v4.2l5.3-5.3l-5.3-5.3V15H0.7V17.2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Info'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M14.5,8.5h3v3h-3V8.5z M14.5' +
          ',14.5h3v9h-3V14.5z M16,1C7.7,1,1,7.7,1,16s6.7,15,15,15s15-6.7,15' +
          '-15S24.2,1,16,1z M16,28'#13#10#9#9'C9.4,28,4,22.6,4,16S9.4,4,16,4s12,5.4' +
          ',12,12S22.6,28,16,28z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Keyboard'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M7,22H9V24H7V22M11,22H13' +
          'V24H11V22M15,22H17V24H15V22M4,5A2,2 0 0,0 2,7V17A2,2 0 0,0 4,19H' +
          '20A2,2 0 0,0 22,17V7A2,2 0 0,0 20,5H4M4,7H20V17H4V7M5,8V10H7V8H5' +
          'M8,8V10H10V8H8M11,8V10H13V8H11M14,8V10H16V8H14M17,8V10H19V8H17M5' +
          ',11V13H7V11H5M8,11V13H10V11H8M11,11V13H13V11H11M14,11V13H16V11H1' +
          '4M17,11V13H19V11H17M8,14V16H16V14H8Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Layouts'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M26.5,2.3h-7.8H5.5L1,6.8v5.' +
          '1v13.4l4.5,4.5h9.8h11.1l4.5-4.5V15.1V6.8L26.5,2.3z M28.4,24.2l-3' +
          ',3h-10H6.7l-3-3V13.3V7.9'#13#10#9#9'l3-3h12h6.7l3,3v5.9V24.2z"/>'#13#10#9'<path' +
          ' d="M7.1,8h17.7c0.3,0,0.5,0.2,0.5,0.5v2c0,0.3-0.2,0.5-0.5,0.5H7.' +
          '1c-0.3,0-0.5-0.2-0.5-0.5v-2C6.6,8.2,6.9,8,7.1,8z"/>'#13#10#9'<path d="M' +
          '7.1,13.9h5.4c0.3,0,0.5,0.2,0.5,0.5v9.4c0,0.3-0.2,0.5-0.5,0.5H7.1' +
          'c-0.3,0-0.5-0.2-0.5-0.5v-9.4'#13#10#9#9'C6.6,14.2,6.9,13.9,7.1,13.9z"/>'#13 +
          #10#9'<path d="M16.5,13.9h8.4c0.3,0,0.5,0.2,0.5,0.5v9.4c0,0.3-0.2,0.' +
          '5-0.5,0.5h-8.4c-0.3,0-0.5-0.2-0.5-0.5v-9.4'#13#10#9#9'C16,14.1,16.2,13.9' +
          ',16.5,13.9z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'LineNumbers'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M0.7,24h3.2v0.9H2.3v1.6h1.6' +
          'v0.9H0.7v1.6h4.8v-6.4H0.7V24z M2.3,9.5h1.6V3.1H0.7v1.6h1.6V9.5z ' +
          'M0.7,14.3h2.9l-2.9,3.4v1.4'#13#10#9#9'h4.8v-1.6H2.6l2.9-3.4v-1.4H0.7V14.' +
          '3z M8.7,4.7v3.2h22.6V4.7H8.7z M8.7,27.2h22.6V24H8.7V27.2z M8.7,1' +
          '7.6h22.6v-3.2H8.7V17.6z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Link'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M23.5,8.5h-6v3h6c2.5,0,4.5,' +
          '2,4.5,4.5s-2,4.5-4.5,4.5h-6v3h6c4.1,0,7.5-3.4,7.5-7.5S27.6,8.5,2' +
          '3.5,8.5z M14.5,20.5h-6'#13#10#9#9'C6,20.5,4,18.5,4,16s2-4.5,4.5-4.5h6v-3' +
          'h-6C4.4,8.5,1,11.9,1,16s3.4,7.5,7.5,7.5h6V20.5z M10,14.5h12v3H10' +
          'V14.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Lock'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10#9'<path'#13#10#9#9'd="M12,17C10.89,17 10,16.1 ' +
          '10,15C10,13.89 10.89,13 12,13A2,2 0 0,1 14,15A2,2 0 0,1 12,17M18' +
          ',20V10H6V20H18M18,8A2,2 0 0,1 20,10V20A2,2 0 0,1 18,22H6C4.89,22' +
          ' 4,21.1 4,20V10C4,8.89 4.89,8 6,8H7V6A5,5 0 0,1 12,1A5,5 0 0,1 1' +
          '7,6V8H18M12,3A3,3 0 0,0 9,6V8H15V6A3,3 0 0,0 12,3Z" />'#13#10'</svg>'
      end
      item
        IconName = 'MessagesWin'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path fill="#FFCE00" d="M20.5,4v10.5H' +
          '5.8L4,16.3V4H20.5 M22,1H2.5C1.7,1,1,1.7,1,2.5v21l6-6h15c0.8,0,1.' +
          '5-0.7,1.5-1.5V2.5C23.5,1.6,22.9,1,22,1z"/>'#13#10'<path d="M29.5,7h-3v' +
          '13.5H7v3C7,24.3,7.7,25,8.5,25H25l6,6V8.5C31,7.6,30.4,7,29.5,7z"/' +
          '>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Minus'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M20 14H4V10H20V14Z" />'#13#10 +
          '</svg>'
      end
      item
        IconName = 'OpenFolder'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(119.5 -231.7)' +
          '">'#13#10#9'<path d="M-98.6,243.1h-3.7l1.5-1.6l5-5.1l3.8,3.9l2.7,2.7h-3' +
          '.6c-0.2,1.8-0.7,3.3-1.5,4.6c-0.1,0.2-0.2,0.2-0.2,0.4'#13#10#9#9'c-0.6,1-' +
          '1.3,1.8-2.2,2.5l-0.1,0.1c-1.8,1.5-4,2.3-6.2,2.8c1.1-0.8,2-1.8,2.' +
          '7-2.9c0.5-0.7,0.9-1.6,1.2-2.5c0.4-1.2,0.7-2.6,0.7-4.1'#13#10#9#9'C-98.6,' +
          '243.7-98.6,243.4-98.6,243.1z"/>'#13#10#9'<path d="M-88,250.9l-4.1,4.6l-' +
          '2.5,2.7c-0.3,0.3-0.8,0.6-1.3,0.6h-0.5c-0.2,0.1-0.4,0.1-0.7,0.1H-' +
          '117c-1.4,0-2.5-1.1-2.5-2.5v-14.9'#13#10#9#9'c0-1.3,1.1-2.5,2.5-2.5h7.5l2' +
          '.5,2.5h4.5l-0.7,0.7c-0.3,0.3-0.5,0.9-0.2,1.3c0.1,0.2,0.2,0.3,0.3' +
          ',0.4H-117v10.2l5-5.6'#13#10#9#9'c0.3-0.3,0.7-0.6,1.2-0.6h10c-0.3,0.9-0.8' +
          ',1.7-1.4,2.5h-8.2l-5.2,5.8h19.3l1.7-1.8l2.5-2.7l1.1-1.2h-4.1c0.2' +
          '-0.2,0.4-0.4,0.6-0.7'#13#10#9#9'c0.4-0.6,0.8-1.2,1.2-1.8h4.1c0.7,0,1.2,0' +
          '.4,1.6,1C-87.5,249.7-87.5,250.4-88,250.9z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PageSetup'
        SVGText = 
          '<svg viewBox="0 0 510 510">'#13#10#9'<g>'#13#10#9#9'<path'#13#10#9#9#9'd="m510 388.114v-' +
          '56.229l-40.849-11.954c-1.092-2.971-2.307-5.898-3.64-8.774l20.435' +
          '-37.344-39.76-39.759-37.343 20.435c-2.876-1.334-5.805-2.549-8.77' +
          '5-3.641l-11.953-40.848h-13.115v-210h-375v465h252.868l20.946 20.9' +
          '45 37.343-20.435c2.877 1.334 5.805 2.549 8.775 3.641l11.953 40.8' +
          '49h56.229l11.953-40.849c2.97-1.092 5.898-2.307 8.775-3.641l37.34' +
          '3 20.435 39.76-39.759-20.435-37.344c1.333-2.876 2.548-5.804 3.64' +
          '-8.774zm-480-358.114h315v180h-13.115l-11.953 40.849c-2.97 1.092-' +
          '5.898 2.307-8.775 3.641l-37.343-20.435-39.76 39.759 20.435 37.34' +
          '4c-1.333 2.876-2.548 5.804-3.64 8.774l-40.849 11.954v56.229l40.8' +
          '49 11.954c1.092 2.971 2.307 5.898 3.64 8.774l-14.313 26.157h-210' +
          '.176zm418.837 410.868-7.97 7.97-31.816-17.411c-5.031 2.569-12.48' +
          '6 7.851-25.284 11.508l-7.952 2.273-10.18 34.792h-11.271l-10.181-' +
          '34.792-7.952-2.273c-12.939-3.698-20.81-9.224-25.284-11.508l-31.8' +
          '16 17.411-7.97-7.97 17.41-31.816-4.018-7.23c-6.597-11.872-8.208-' +
          '21.212-9.764-26.005l-34.789-10.181v-11.271l34.79-10.181c1.746-5.' +
          '379 3.248-14.277 9.764-26.005l4.018-7.23-17.41-31.816 7.97-7.97 ' +
          '31.816 17.411c5.031-2.57 12.486-7.85 25.284-11.508l7.952-2.273 1' +
          '0.181-34.793h11.271l10.181 34.792c5.385 1.747 14.278 3.245 26.00' +
          '5 9.763l7.23 4.019 31.816-17.411 7.97 7.97-17.41 31.816c2.561 5.' +
          '013 7.846 12.479 11.509 25.286l2.273 7.949 34.79 10.18v11.271l-3' +
          '4.791 10.181-2.273 7.949c-3.708 12.965-9.229 20.822-11.509 25.28' +
          '6z">'#13#10#9#9'</path>'#13#10#9#9'<path'#13#10#9#9#9'd="m360 315c-24.813 0-45 20.186-45 ' +
          '45 2.472 59.699 87.538 59.681 90 0 0-24.814-20.187-45-45-45zm0 6' +
          '0c-8.271 0-15-6.728-15-15 .824-19.9 29.179-19.894 30 0 0 8.272-6' +
          '.729 15-15 15z">'#13#10#9#9'</path>'#13#10#9#9'<path d="m60 360h105v30h-105z"></' +
          'path>'#13#10#9#9'<path d="m60 300h105v30h-105z"></path>'#13#10#9#9'<path d="m60 ' +
          '240h105v30h-105z"></path>'#13#10#9#9'<path d="m60 180h105v30h-105z"></pa' +
          'th>'#13#10#9#9'<path d="m60 120h255v30h-255z"></path>'#13#10#9#9'<path d="m60 60' +
          'h255v30h-255z"></path>'#13#10#9'</g>'#13#10'</svg>'
      end
      item
        IconName = 'Paste'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-180.6 -325.9' +
          ')">'#13#10#9'<path d="M206.8,329h-6.2c-0.9-2.3-3.3-3.4-5.6-2.6c-1.2,0.5' +
          '-2.2,1.4-2.6,2.6h-6.2c-1.6,0-2.9,1.2-2.9,2.8v23'#13#10#9#9'c0,1.6,1.3,2.' +
          '8,2.9,2.8h20.5c1.6,0,2.9-1.2,2.9-2.8v-23C209.7,330.2,208.4,329,2' +
          '06.8,329z M196.6,329c0.8,0,1.4,0.7,1.4,1.4'#13#10#9#9's-0.7,1.4-1.4,1.4s' +
          '-1.4-0.7-1.4-1.4S195.8,329,196.6,329z M206.8,354.8h-20.5v-23h2.9' +
          'v4.3h14.6v-4.3h2.9V354.8z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Pause'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M14,19H18V5H14M6,19H10V5' +
          'H6V19Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Pin'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M19,4v7.5c0,1.6,0.6,3.3,1.5' +
          ',4.5h-9c0.9-1.4,1.5-2.9,1.5-4.5V4H19 M23.5,1h-15C7.7,1,7,1.8,7,2' +
          '.5S7.8,4,8.5,4l0,0H10v7.5'#13#10#9#9'c0,2.6-1.9,4.5-4.5,4.5v3h9v10.5L16,' +
          '31l1.5-1.5V19h9v-3l0,0c-2.6,0-4.5-1.9-4.5-4.5V4h1.5l0,0C24.3,4,2' +
          '5,3.2,25,2.5S24.2,1,23.5,1'#13#10#9#9'L23.5,1z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Plus'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M20 14H14V20H10V14H4V10H' +
          '10V4H14V10H20V14Z" />'#13#10'</svg>'
      end
      item
        IconName = 'PostMortem'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M18.6,14.9v0.2V14.9L18.6,14' +
          '.9z M14.4,9.6l-1.8,1.5l4.3,5.4l0.3-0.2v-2.9c0-0.1,0-0.2,0-0.3l0,' +
          '0L14.4,9.6z"/>'#13#10#9#9'<path d="M17.2,13.1L17.2,13.1c0,0.1,0,0.2,0,0.' +
          '3v2.9l-0.3,0.2l-4.3-5.4l1.8-1.5L17.2,13.1z"/>'#13#10#9#9'<polygon points' +
          '="18.7,15 18.6,15.1 18.6,14.9"/>'#13#10#9'<g transform="translate(94.5 ' +
          '-231.7)">'#13#10#9#9'<path d="M-80.1,241.3l2.8,3.5l0,0c0,0.1,0,0.2,0,0.3' +
          'v2.9l-0.3,0.2l-4.3-5.4L-80.1,241.3z"/>'#13#10#9#9'<path d="M-68.4,244.4l' +
          '-0.3-2.2l-3.4,0.5c-0.2-0.4-0.4-0.8-0.6-1.2l2.8-2.2l0,0l-0.9-1.2l' +
          '-0.5-0.7l-0.9,0.7l-1.9,1.6'#13#10#9#9#9'c-0.3-0.3-0.7-0.6-1-0.9l0.3-0.7l1' +
          '.1-2.5l-2.1-0.9l-1.2,2.9c-0.6-0.2-1.3-0.4-1.8-0.5c-0.3-0.6-1.1-1' +
          '.6-2.6-2.2l0.3-2.7l-2.3-0.3'#13#10#9#9#9'l-0.4,2.8c-2.1,0.4-3,1.9-3.4,2.7' +
          'l-2.8-0.3l-0.1,1l-0.2,1.3l2.7,0.3c0.1,1.6,1.1,2.6,1.6,3c-0.1,0.6' +
          '-0.1,1.3,0,1.8l-1.6,0.2'#13#10#9#9#9'c1.6,1.6,3.6,2.8,5.9,3.4c-0.2-0.2-0.' +
          '4-0.5-0.6-0.7c-1.9-2.5-1.9-5.8,0.1-7.4c2-1.6,5.2-0.8,7.2,1.6c0.8' +
          ',1,1.3,2.1,1.4,3.2'#13#10#9#9#9'l2.2,2.2c0.2-0.8,0.2-1.6,0.1-2.4L-68.4,24' +
          '4.4z M-83.9,238.4c-0.6,0.5-1.1,1.1-1.5,1.6c-0.3-0.7-0.2-1.4,0.2-' +
          '1.9'#13#10#9#9#9'c0.1-0.2,0.3-0.4,0.6-0.6c0.8-0.6,1.8-0.7,2.7-0.1c-0.6,0.' +
          '2-1.1,0.4-1.6,0.7C-83.6,238.2-83.7,238.3-83.9,238.4z"/>'#13#10#9#9'<poly' +
          'gon points="-75.8,246.7 -75.9,246.8 -75.9,246.6"/>'#13#10#9#9'<polygon p' +
          'oints="-75.8,246.7 -75.9,246.8 -75.9,246.6"/>'#13#10#9#9'<path d="M-75.9' +
          ',263.5v-5.2c-0.4-0.1-0.8-0.1-1.2-0.2c-8.5-1.6-12.2-7.9-13.5-14.2' +
          'c0.4,0.6,0.9,1.1,1.4,1.6'#13#10#9#9#9'c0.6,0.7,1.3,1.3,1.9,1.7c0.6,0.5,1.' +
          '3,0.9,1.9,1.3c2.6,1.5,5.7,2.1,9.5,2.1v-3.7l0.1-0.1l-0.1-0.1v-1.6' +
          'l2,1.9l1.8,1.8l3,2.9'#13#10#9#9#9'l2.5,2.5L-75.9,263.5z"/>'#13#10#9'</g>'#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'Print'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M26.5,10H25V2.5H7V10H5.5C3,' +
          '10,1,11.9,1,14.5v9h6v6h18v-6h6v-9C31,11.9,28.9,10,26.5,10z M10,5' +
          '.5h12V10H10V5.5z M22,23.5v3'#13#10#9#9'H10v-6h12V23.5z M25,20.5v-3H7v3H4' +
          'v-6C4,13.7,4.8,13,5.5,13h21c0.9,0,1.5,0.8,1.5,1.5v6H25z"/>'#13#10#9'<ci' +
          'rcle cx="25" cy="15.2" r="1.5"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PrintPreview'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M19,1H7C5.3,1,4,2.4,4,4v24c' +
          '0,1.6,1.3,3,3,3h18c1.6,0,3-1.4,3-3V10L19,1z M25,28H7V4h10.5v7.5H' +
          '25V28z"/>'#13#10#9'<path d="M11.6,14.6c-2.1,2.1-2.1,5.4,0,7.4c1.7,1.7,4' +
          '.2,2,6.2,0.9l2.8,2.8l2.1-2.1l-2.8-2.8c1.1-2,0.8-4.5-0.9-6.2'#13#10#9#9'C' +
          '16.9,12.5,13.6,12.5,11.6,14.6z M16.9,19.9c-0.9,0.9-2.3,0.9-3.2,0' +
          's-0.9-2.3,0-3.2c0.9-0.9,2.3-0.9,3.2,0'#13#10#9#9'C17.8,17.6,17.8,19,16.9' +
          ',19.9z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PrintSetup'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M19 8C20.66 8 22 9.34 22 ' +
          '11V17H18V21H6V17H2V11C2 9.34 3.34 8 5 8H6V3H18V8H19M8 5V8H16V5H8' +
          'M16 19V15H8V19H16M18 15H20V11C20 10.45 19.55 10 19 10H5C4.45 10 ' +
          '4 10.45 4 11V15H6V13H18V15M19 11.5C19 12.05 18.55 12.5 18 12.5C1' +
          '7.45 12.5 17 12.05 17 11.5C17 10.95 17.45 10.5 18 10.5C18.55 10.' +
          '5 19 10.95 19 11.5M15,24V22H17V24H15M11,24V22H13V24H11M7,24V22H9' +
          'V24H7Z" />'#13#10'</svg>'
      end
      item
        IconName = 'ProjectAdd'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="m 25.6,7.3 c 0,0 0,-0.1 0,0' +
          ' C 25.5,7.1 25.4,7.1 25.4,7 L 25.3,6.9 C 25.2,6.9 25.2,6.8 25.1,' +
          '6.8 L 13.1,0.5 C 12.7,0.3 12.3,0.3 11.9,0.5 L 0.8,6.3 H 0.7 L 0.' +
          '6,6.4 0.5,6.5 0.3,6.7 0.2,6.8 C 0.2,6.9 0.1,7 0.1,7 v 12.1 c 0,0' +
          '.5 0.3,0.9 0.6,1.1 l 11.7,6.5 c 0.1,0 0.2,0.1 0.2,0.1 h 0.5 C 13' +
          ',26.6 12.9,26.4 12.9,26.3 v -0.1 c -0.3,-0.9 -0.5,-1.7 -0.6,-2.5' +
          ' -0.1,-0.9 -0.1,-1.7 0,-2.5 0.2,-1.8 0.9,-3.5 1.9,-5 V 14.4 L 23' +
          '.3,10 v 2 c 0.8,0.1 1.6,0.2 2.3,0.5 V 7.7 c 0.1,-0.2 0,-0.3 0,-0' +
          '.4 z M 22.811236,7.5707865 13.307866,11.764044 3.8303371,7.07528' +
          '09 12.750562,2.3932584 Z M 11.8,23.6 2.6,18.4 V 9.6 l 9.2,4.8 z"' +
          ' />'#13#10#9#9'<path d="M22.7,31.6c-5.1,0-9.1-4.1-9.1-9.1c0-5.1,4.1-9.1,' +
          '9.1-9.1s9.1,4.1,9.1,9.1C31.9,27.6,27.8,31.6,22.7,31.6z M22.7,15.' +
          '3'#13#10#9#9#9'c-4,0-7.2,3.2-7.2,7.2s3.2,7.2,7.2,7.2c4,0,7.2-3.2,7.2-7.2C' +
          '29.9,18.5,26.7,15.3,22.7,15.3z"/>'#13#10#9'<polygon points="28.3,21.5 2' +
          '8.3,23.3 23.7,23.4 23.7,28 21.8,28 21.8,23.4 17.3,23.4 17.3,21.5' +
          ' 21.8,21.5 21.8,16.9 23.7,16.9 23.7,21.5 '#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ProjectExplorer'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="scale(1.15) translate(1' +
          '.5,1.75)">'#13#10#9'<path fill="#4488FF" d="M19.5,14.9V9.2c0-0.7-0.4-1.' +
          '4-1-1.7l-5-2.9c-0.3-0.2-0.6-0.3-1-0.3s-0.7,0.1-1,0.3l-5,2.9c-0.6' +
          ',0.4-1,1-1,1.7'#13#10#9#9'v5.7c0,0.7,0.4,1.4,1,1.7l5,2.9c0.3,0.2,0.6,0.3' +
          ',1,0.3s0.7-0.1,1-0.3l5-2.9C19.2,16.3,19.5,15.6,19.5,14.9z M11.5,' +
          '17.2l-4-2.3v-4.6'#13#10#9#9'l4,2.3V17.2z M12.5,10.9l-4-2.3l4-2.3l4,2.3L1' +
          '2.5,10.9z M17.5,14.9l-4,2.3v-4.6l4-2.3V14.9z"/>'#13#10'</g>'#13#10#9'<path d=' +
          '"M28.6,28.1H3.4c-1.5,0-2.7-1.2-2.7-2.7V6.6c0-1.5,1.2-2.7,2.7-2.7' +
          'h25.2c1.5,0,2.7,1.2,2.7,2.7v18.8'#13#10#9#9'C31.3,26.9,30.1,28.1,28.6,28' +
          '.1z M3.4,6.4c-0.1,0-0.2,0.1-0.2,0.2v18.8c0,0.1,0.1,0.2,0.2,0.2h2' +
          '5.2c0.1,0,0.2-0.1,0.2-0.2V6.6'#13#10#9#9'c0-0.1-0.1-0.2-0.2-0.2H3.4z"/>'#13 +
          #10'</svg>'
      end
      item
        IconName = 'ProjectFile'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="translate(-272.1 -317.' +
          '4)">'#13#10#9#9'<path d="M294,318.8h-11.3c-1.6,0-2.9,1.3-2.8,2.9v5.6l1.4' +
          '-0.7c0.3-0.2,0.7-0.3,1.1-0.3c0.1,0,0.2,0,0.3,0'#13#10#9#9#9'v-4.5h9.9v7.3' +
          'h7.1v16.1h-11.1L284,348h15.7c1.6,0,2.9-1.3,2.8-2.9v-17.6L294,318' +
          '.8z"/>'#13#10#9'</g>'#13#10#9'<g transform="scale(1.3) translate(-4,3.5)">'#13#10#9#9 +
          '<path fill="#4488FF" d="M19,14.87V9.13c0-0.72-0.38-1.38-1-1.73l-' +
          '5-2.88c-0.31-0.18-0.65-0.27-1-0.27s-0.69,0.09-1,0.27L6,7.39 C5.3' +
          '8,7.75,5,8.41,5,9.13v5.74c0,0.72,0.38,1.38,1,1.73l5,2.88c0.31,0.' +
          '18,0.65,0.27,1,0.27s0.69-0.09,1-0.27l5-2.88 C18.62,16.25,19,15.5' +
          '9,19,14.87z M11,17.17l-4-2.3v-4.63l4,2.33V17.17z M12,10.84L8.04,' +
          '8.53L12,6.25l3.96,2.28L12,10.84z M17,14.87l-4,2.3v-4.6l4-2.33V14' +
          '.87z"/>'#13#10#9'</g>'#13#10'</svg>'
      end
      item
        IconName = 'ProjectOpen'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M31.7,15.1l-2.3-5.4c0-0.1-0' +
          '.1-0.2-0.1-0.2l0,0c0,0,0-0.1-0.1-0.1c0-0.1-0.1-0.1-0.1-0.2L29,9.' +
          '1c-0.1,0-0.1-0.1-0.2-0.1'#13#10#9#9'L16.2,2.4c-0.4-0.2-0.8-0.2-1.2,0L3.5' +
          ',8.5l0,0l0,0c0,0,0,0-0.1,0l0,0l0,0l0,0l0,0l0,0c0,0,0,0-0.1,0l0,0' +
          'L3.2,8.6'#13#10#9#9'C3.1,8.7,3.1,8.7,3,8.8L2.9,8.9l0,0C2.8,9,2.7,9.1,2.6' +
          ',9.3l-2.3,5.4c-0.3,0.6,0,1.3,0.5,1.6l2,1.1v4.3c0,0.5,0.3,0.9,0.6' +
          ',1.1'#13#10#9#9'l12.2,6.8c0.1,0,0.2,0.1,0.2,0.1h0.6c0,0,0,0,0.1,0s0.1,0,' +
          '0.2-0.1l12-6.5c0.4-0.3,0.6-0.7,0.6-1.1v-4.3l1.9-1'#13#10#9#9'C31.8,16.3,' +
          '32,15.7,31.7,15.1z M3,14.7l1.3-3.1l0.9,0.5l9.2,4.8L13,20.3L5.2,1' +
          '6L3,14.7z M14.9,26.4L5.3,21v-2.2l7.8,4.3'#13#10#9#9'c0.2,0.1,0.4,0.2,0.6' +
          ',0.2c0.1,0,0.3,0,0.4-0.1c0.3-0.1,0.6-0.4,0.7-0.7v-0.1v4L14.9,26.' +
          '4L14.9,26.4z M15.1,13.8L15.1,13.8'#13#10#9#9'L15,13.7L11.7,12l-1.4-0.7L6' +
          '.8,9.5l1.7-1l1.6-0.8l4.7-2.5l0,0L15,5.1v8.4L15.1,13.8L15.1,13.8L' +
          '15.1,13.8z M28.9,9.1'#13#10#9#9'c0,0-0.1,0-0.2-0.1C28.8,9,28.8,9.1,28.9,' +
          '9.1z M17.4,5.8l5.9,3.1l2,1l-2.1,1l-5.8,2.7L17.4,5.8L17.4,5.8z M2' +
          '6.9,21.3l-7.4,4'#13#10#9#9'l-0.3,0.1l-1.8,0.9v-3.2c0.1,0.1,0.3,0.2,0.5,0' +
          '.3c0.1,0,0.3,0.1,0.4,0.1c0.2,0,0.4-0.1,0.6-0.2l8-4.4V21.3z M26.9' +
          ',16.2L26.9,16.2'#13#10#9#9'l-8,4.4l-1.4-3.4l9.3-4.9l0.8-0.4l1.3,3.1L26.9' +
          ',16.2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ProjectRemove'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="m 25.6,7.3 c 0,0 0,-0.1 0,0' +
          ' C 25.5,7.1 25.4,7.1 25.4,7 L 25.3,6.9 C 25.2,6.9 25.2,6.8 25.1,' +
          '6.8 L 13.1,0.5 C 12.7,0.3 12.3,0.3 11.9,0.5 L 0.8,6.3 H 0.7 L 0.' +
          '6,6.4 0.5,6.5 0.3,6.7 0.2,6.8 C 0.2,6.9 0.1,7 0.1,7 v 12.1 c 0,0' +
          '.5 0.3,0.9 0.6,1.1 l 11.7,6.5 c 0.1,0 0.2,0.1 0.2,0.1 h 0.5 C 13' +
          ',26.6 12.9,26.4 12.9,26.3 v -0.1 c -0.3,-0.9 -0.5,-1.7 -0.6,-2.5' +
          ' -0.1,-0.9 -0.1,-1.7 0,-2.5 0.2,-1.8 0.9,-3.5 1.9,-5 V 14.4 L 23' +
          '.3,10 v 2 c 0.8,0.1 1.6,0.2 2.3,0.5 V 7.7 c 0.1,-0.2 0,-0.3 0,-0' +
          '.4 z M 22.811236,7.5707865 13.307866,11.764044 3.8303371,7.07528' +
          '09 12.750562,2.3932584 Z M 11.8,23.6 2.6,18.4 V 9.6 l 9.2,4.8 z"' +
          ' />'#13#10#9'<polygon points="28.2,21.5 28.2,23.3 23.7,23.4 21.8,23.4 1' +
          '7.3,23.4 17.3,21.5 21.8,21.5 23.7,21.5 '#9#9#9'"/>'#13#10#9'<path d="M29.1,1' +
          '5.9c-1-1-2.2-1.7-3.5-2.2c-0.8-0.3-1.5-0.5-2.3-0.5l0,0c-0.2,0-0.4' +
          ',0-0.7,0c-2.3,0-4.6,0.9-6.5,2.7'#13#10#9#9'c-0.9,0.9-1.4,1.7-1.9,2.8c-0.' +
          '4,0.8-0.6,1.6-0.7,2.4c-0.1,0.8-0.1,1.4,0,2.3c0.1,0.8,0.3,1.4,0.5' +
          ',2.2c0.1,0.2,0.2,0.4,0.2,0.5'#13#10#9#9'c0.2,0.4,0.4,0.8,0.7,1.3c0.4,0.5' +
          ',0.7,1,1.2,1.4c0.2,0.2,0.5,0.5,0.7,0.6c0.2,0.2,0.4,0.3,0.6,0.5c0' +
          '.8,0.5,1.5,1,2.4,1.3'#13#10#9#9'c0.9,0.3,1.7,0.5,2.7,0.5c0.9,0,1.7-0.1,2' +
          '.5-0.4c0.7-0.2,1.4-0.5,2.1-1c0.6-0.4,1.3-0.8,1.7-1.4C32.7,25.3,3' +
          '2.7,19.5,29.1,15.9z'#13#10#9#9#9'M27.9,27.5c-0.2,0.2-0.4,0.4-0.5,0.5c-0.6' +
          ',0.5-1.4,1-2.1,1.3c-0.8,0.3-1.5,0.5-2.3,0.5h-0.3c-1.2,0-2.2-0.3-' +
          '3.2-0.7'#13#10#9#9'c-0.7-0.4-1.4-0.8-1.9-1.4c-0.7-0.7-1.2-1.4-1.5-2.2c-0' +
          '.3-0.8-0.5-1.5-0.6-2.4l0,0c-0.1-0.9,0-1.7,0.3-2.6c0.4-1.2,1-2.2,' +
          '1.8-3.1'#13#10#9#9'c1.4-1.4,3.3-2.1,5.1-2.1c0.2,0,0.4,0,0.7,0c0,0,0,0,0.' +
          '1,0c0.8,0.1,1.5,0.3,2.3,0.6c0.8,0.4,1.5,0.9,2.1,1.4'#13#10#9#9'C30.7,20.' +
          '1,30.7,24.7,27.9,27.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ProjectSave'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M9.1,20.5c0,1.1,0.3,2,0.9,2' +
          '.8v-5.5C9.5,18.5,9.1,19.4,9.1,20.5z M4.6,7v6h6.2c0.7-0.8,1.6-1.3' +
          ',2.7-1.3h4.6V7H4.6z'#13#10#9#9#9'M21.1,2.5h-18c-1.7,0-3,1.4-3,3v21c0,1.6,' +
          '1.3,3,3,3h7.5c-0.4-0.6-0.6-1.3-0.6-2v-1H3.1v-21h16.8l4.2,4.2v4.1' +
          'h3V8.5L21.1,2.5z'#13#10#9#9#9'M9.1,20.5c0,1.1,0.3,2,0.9,2.8v-5.5C9.5,18.5' +
          ',9.1,19.4,9.1,20.5z M4.6,7v6h6.2c0.7-0.8,1.6-1.3,2.7-1.3h4.6V7H4' +
          '.6z M9.1,20.5'#13#10#9#9'c0,1.1,0.3,2,0.9,2.8v-5.5C9.5,18.5,9.1,19.4,9.1' +
          ',20.5z M4.6,7v6h6.2c0.7-0.8,1.6-1.3,2.7-1.3h4.6V7H4.6z"/>'#13#10#9'<g t' +
          'ransform="scale(1.25) translate(3.5,5.5)">'#13#10#9#9'<path fill="#4488F' +
          'F" d="M19,14.87V9.13c0-0.72-0.38-1.38-1-1.73l-5-2.88c-0.31-0.18-' +
          '0.65-0.27-1-0.27s-0.69,0.09-1,0.27L6,7.39 C5.38,7.75,5,8.41,5,9.' +
          '13v5.74c0,0.72,0.38,1.38,1,1.73l5,2.88c0.31,0.18,0.65,0.27,1,0.2' +
          '7s0.69-0.09,1-0.27l5-2.88 C18.62,16.25,19,15.59,19,14.87z M11,17' +
          '.17l-4-2.3v-4.63l4,2.33V17.17z M12,10.84L8.04,8.53L12,6.25l3.96,' +
          '2.28L12,10.84z M17,14.87l-4,2.3v-4.6l4-2.33V14.87z"/>'#13#10#9'</g>'#13#10'</' +
          'svg>'
      end
      item
        IconName = 'PyActivate'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M27.1,17c0.1-0.5,0.1-1,0.1-' +
          '1.5c0-0.5,0-1-0.1-1.5l3.2-2.5c0.3-0.2,0.4-0.6,0.2-1l-3-5.2c-0.1-' +
          '0.2-0.4-0.4-0.7-0.4h-0.3'#13#10#9#9'l-3.7,1.5c-0.8-0.6-1.6-1.1-2.5-1.5l-' +
          '0.6-4c0-0.4-0.4-0.6-0.7-0.6h-6c-0.4,0-0.7,0.3-0.7,0.6l-0.6,4c-0.' +
          '9,0.4-1.8,0.9-2.5,1.5'#13#10#9#9'L5.3,4.8H5c-0.3,0-0.5,0.1-0.6,0.4l-3,5.' +
          '2c-0.2,0.3-0.1,0.7,0.2,1l3.2,2.5c-0.1,0.5-0.1,1-0.1,1.5c0,0.5,0,' +
          '1,0.1,1.5l-3.2,2.5'#13#10#9#9'c-0.3,0.2-0.4,0.6-0.2,1l3,5.2c0.1,0.2,0.4,' +
          '0.4,0.7,0.4h0.3l3.7-1.5c0.8,0.6,1.6,1.1,2.5,1.5l0.6,4c0,0.4,0.4,' +
          '0.6,0.7,0.6h4.2'#13#10#9#9'l1.2-1.4l1.4-1.4h-5.4v-3.2l-0.1-0.9l-1.6-0.6c' +
          '-0.6-0.3-1.2-0.6-1.8-1.1l-1.4-1l-1.6,0.6l-1.9,0.8l-1-1.8l1.6-1.3' +
          'l1.3-1l-0.2-1.7'#13#10#9#9'c0-0.5-0.1-0.8-0.1-1.1c0-0.3,0-0.6,0.1-1.1l0.' +
          '2-1.7l-1.3-1.1l-1.6-1.3l1-1.8l1.9,0.8l1.6,0.6l1.4-1c0.6-0.5,1.3-' +
          '0.8,1.9-1.1'#13#10#9#9'l1.6-0.6l0.2-1.7l0.3-2H17l0.3,2l0.2,1.7l1.6,0.6c0' +
          '.6,0.3,1.2,0.6,1.8,1.1l1.4,1l1.6-0.6l1.9-0.8l1,1.8l-1.6,1.3l-1.3' +
          ',1.1'#13#10#9#9'c-0.1-0.1,0.2,1.6,0.2,1.6c0.1,0.5,0.1,0.8,0.1,1.1c0,0.2,' +
          '0,0.5,0,0.8l3.5,3.3l0,0l0,0l2,2.1l0.8-1.3c0.2-0.3,0.1-0.7-0.2-1'#13 +
          #10#9#9'L27.1,17z"/>'#13#10#9'<path d="M15.9,9.4c-3.3,0-6,2.7-6,6c0,2.9,2,5.' +
          '3,4.7,5.9c0.4,0.1,0.8,0.1,1.3,0.1s0.9,0,1.3-0.1c0.7-0.2,1.3-0.5,' +
          '1.3-0.5l2.8-2.6'#13#10#9#9'c0.3-0.5,0.5-1.1,0.6-1.6c0.1-0.4,0.1-0.8,0.1-' +
          '1.1C21.9,12.1,19.2,9.4,15.9,9.4z M15.9,18.5c-1.6,0-3-1.4-3-3c0-1' +
          '.7,1.4-3,3-3'#13#10#9#9's3,1.3,3,3S17.5,18.5,15.9,18.5z"/>'#13#10#9'<polygon po' +
          'ints="30.4,24.6 23.1,31.8 20.5,29.3 22.3,27.6 23.4,26.4 15.9,26.' +
          '4 15.9,22.8 23.4,22.8 22.6,22 21.9,21.3 20.5,20 '#13#10#9#9'23.1,17.4 23' +
          '.9,18.2 25.2,19.5 25.2,19.5 26.6,20.8 29.8,24 '#9#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PyDoc'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M30.4,14.7v-2.9l1.1-1c0.4-0.' +
          '3,0.5-0.7,0.4-1.2s-0.4-0.8-0.8-0.9L16.6,3.9c-0.5-0.2-0.9-0.1-1.2' +
          ',0.2L2.2,14.2'#13#10#9'C2.1,14.3,2,14.4,2,14.4c-0.9,0.7-2,2.1-2,4.3C0,2' +
          '1.5,2,23,2.1,23c0.1,0.1,0.3,0.2,0.4,0.2c0.1,0.1,0.2,0.2,0.4,0.2L' +
          '18,28.6'#13#10#9'c0.1,0.1,0.3,0.1,0.5,0.1c0.3,0,0.7-0.1,0.9-0.4l12.1-11' +
          '.6c0.6-0.5,0.6-1.3,0-1.9C31.2,14.8,30.8,14.7,30.4,14.7z M7.9,18.' +
          '5'#13#10#9'L7.9,18.5l0.7,0.3l6.8,2.6V25L4.5,21.1v-3.9L7.9,18.5z M3.1,20' +
          '.3c-0.2-0.4-0.4-0.9-0.4-1.6c0-1.1,0.3-1.7,0.6-2v3.6l0,0'#13#10#9'C3.2,2' +
          '0.4,3.1,20.4,3.1,20.3z M28.3,16.4l-10.6,9.4c0-0.1,0-0.1,0-0.2v-3' +
          '.3h0.1c0.2,0.1,0.3,0.1,0.5,0.1s0.3,0,0.5-0.1'#13#10#9'c0,0,0.1,0,0.1-0.' +
          '1c0.1,0,0.2-0.1,0.3-0.2c0,0,0,0,0.1,0l9.1-8.1L28.3,16.4z"/>'#13#10'<pa' +
          'th fill="#4488FF" d="M11.5,12.4l0.9,0.4l0.6-0.5c0.4-0.4,1.6-0.4,' +
          '2.5,0l2.8,1c0.7,0.3,1.7,0.3,2.1,0l1.1-1c0.3-0.3-0.1-0.7-0.8-1'#13#10#9 +
          'c-0.5-0.2-0.9-0.4-1.4-0.6c-0.5-0.2-0.9-0.3-1.4-0.5c-1.3-0.4-1.7-' +
          '0.3-2.1,0l-0.5,0.4l2.8,1l-0.2,0.1l-2.8-1l-1-0.4'#13#10#9'c-0.8-0.3-1.8-' +
          '0.4-2.3-0.1c-0.7,0.3-1,0.6-1.2,1C10.5,11.7,10.7,12.1,11.5,12.4z ' +
          'M17.7,10.8c-0.1,0.1-0.5,0.1-0.7,0'#13#10#9'c-0.3-0.1-0.5-0.3-0.3-0.4c0.' +
          '1-0.1,0.5-0.1,0.7,0C17.7,10.5,17.8,10.7,17.7,10.8z"/>'#13#10'<path fil' +
          'l="#FFCE00" d="M20.7,13.4c-0.4,0.4-1.6,0.4-2.5,0l-2.8-1c-0.7-0.3' +
          '-1.7-0.3-2.1,0l-1.1,1c-0.4,0.3,0.2,0.7,0.7,1'#13#10#9'c0.4,0.2,0.8,0.5,' +
          '1.3,0.6c0.5,0.2,0.9,0.3,1.5,0.5c0.8,0.2,1.7,0.3,2.1,0l0.5-0.4l-2' +
          '.9-1l0.2-0.1l2.9,1l1.4,0.6'#13#10#9'c0.8,0.3,1.4,0.2,2.1,0s1-0.6,1.2-1.' +
          '1c0.1-0.4,0-0.7-0.8-1l-1.1-0.4L20.7,13.4z M16.1,14.9c0.1-0.1,0.5' +
          '-0.1,0.7,0'#13#10#9'c0.3,0.1,0.4,0.3,0.3,0.4c-0.1,0.1-0.5,0.1-0.7,0C16.' +
          '1,15.2,16,15,16.1,14.9z"/>'#13#10'</svg>'
      end
      item
        IconName = 'PySetup'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M27.1,17.5c0.1-0.5,0.1-1,0.' +
          '1-1.5s0-1-0.1-1.5l3.2-2.5c0.3-0.2,0.4-0.6,0.2-1l-3-5.2c-0.1-0.2-' +
          '0.4-0.4-0.7-0.4'#13#10#9#9'c-0.1,0-0.2,0-0.3,0l-3.7,1.5c-0.8-0.6-1.6-1.1' +
          '-2.5-1.5l-0.6-4c0-0.4-0.4-0.6-0.7-0.6h-6c-0.4,0-0.7,0.3-0.7,0.6l' +
          '-0.6,4'#13#10#9#9'c-0.9,0.4-1.8,0.9-2.5,1.5L5.4,5.4c-0.1,0-0.2,0-0.3,0c-' +
          '0.3,0-0.5,0.1-0.6,0.4l-3,5.2c-0.2,0.3-0.1,0.7,0.2,1l3.2,2.5'#13#10#9#9'c' +
          '-0.1,0.5-0.1,1-0.1,1.5s0,1,0.1,1.5L1.7,20c-0.3,0.2-0.4,0.6-0.2,1' +
          'l3,5.2c0.1,0.2,0.4,0.4,0.7,0.4c0.1,0,0.2,0,0.3,0l3.7-1.5'#13#10#9#9'c0.8' +
          ',0.6,1.6,1.1,2.5,1.5l0.6,4c0,0.4,0.4,0.6,0.7,0.6h6c0.4,0,0.7-0.3' +
          ',0.7-0.6l0.6-4c0.9-0.4,1.8-0.9,2.5-1.5l3.7,1.5'#13#10#9#9'c0.1,0,0.2,0,0' +
          '.3,0c0.3,0,0.5-0.1,0.6-0.4l3-5.2c0.2-0.3,0.1-0.7-0.2-1L27.1,17.5' +
          'z M24.1,15c0.1,0.5,0.1,0.8,0.1,1.1'#13#10#9#9'c0,0.3,0,0.6-0.1,1.1l-0.2,' +
          '1.7l1.3,1.1l1.6,1.3l-1.1,1.8l-1.9-0.8l-1.6-0.6l-1.3,1c-0.6,0.5-1' +
          '.3,0.8-1.9,1.1l-1.6,0.6l-0.2,1.7'#13#10#9#9'l-0.3,2h-2.1l-0.3-2l-0.2-1.7' +
          'l-1.6-0.6c-0.6-0.3-1.2-0.6-1.8-1.1l-1.4-1l-1.6,0.6L6,23.1l-1-1.8' +
          'L6.6,20l1.3-1l-0.2-1.7'#13#10#9#9'c0-0.5-0.1-0.8-0.1-1.1c0-0.3,0-0.6,0.1' +
          '-1.1l0.2-1.7l-1.3-1.1L5,11l1-1.8L7.9,10l1.6,0.6l1.4-1c0.6-0.5,1.' +
          '3-0.8,1.9-1.1l1.6-0.6'#13#10#9#9'l0.2-1.7l0.3-2H17l0.3,2l0.2,1.7l1.6,0.6' +
          'c0.6,0.3,1.2,0.6,1.8,1.1l1.4,1l1.6-0.6l1.9-0.8l1,1.8l-1.6,1.3l-1' +
          '.3,1.1'#13#10#9#9'C23.8,13.3,24.1,15,24.1,15z M15.9,10c-3.3,0-6,2.7-6,6s' +
          '2.7,6,6,6s6-2.7,6-6S19.2,10,15.9,10z M15.9,19c-1.6,0-3-1.4-3-3'#13#10 +
          #9#9'c0-1.7,1.4-3,3-3s3,1.3,3,3S17.6,19,15.9,19z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PySetupAdd'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M14.3,9.1c-3.3,0-5.9,2.7-5.' +
          '9,5.9c0,2.5,1.6,4.6,3.8,5.5c0.1-0.8,0.4-1.6,0.7-2.3c0.1-0.2,0.2-' +
          '0.4,0.3-0.6'#13#10#9#9'c-1-0.5-1.8-1.5-1.8-2.7c0-1.7,1.4-3,3-3c1.1,0,2.1' +
          ',0.6,2.6,1.5c0.8-0.6,1.8-1,2.7-1.3C18.6,10.4,16.6,9.1,14.3,9.1z ' +
          'M14.3,9.1'#13#10#9#9'c-3.3,0-5.9,2.7-5.9,5.9c0,2.5,1.6,4.6,3.8,5.5c0.1-0' +
          '.8,0.4-1.6,0.7-2.3c0.1-0.2,0.2-0.4,0.3-0.6c-1-0.5-1.8-1.5-1.8-2.' +
          '7'#13#10#9#9'c0-1.7,1.4-3,3-3c1.1,0,2.1,0.6,2.6,1.5c0.8-0.6,1.8-1,2.7-1.' +
          '3C18.6,10.4,16.6,9.1,14.3,9.1z M14.3,9.1c-3.3,0-5.9,2.7-5.9,5.9'#13 +
          #10#9#9'c0,2.5,1.6,4.6,3.8,5.5c0.1-0.8,0.4-1.6,0.7-2.3c0.1-0.2,0.2-0.' +
          '4,0.3-0.6c-1-0.5-1.8-1.5-1.8-2.7c0-1.7,1.4-3,3-3'#13#10#9#9'c1.1,0,2.1,0' +
          '.6,2.6,1.5c0.8-0.6,1.8-1,2.7-1.3C18.6,10.4,16.6,9.1,14.3,9.1z M1' +
          '4.3,9.1c-3.3,0-5.9,2.7-5.9,5.9'#13#10#9#9'c0,2.5,1.6,4.6,3.8,5.5c0.1-0.8' +
          ',0.4-1.6,0.7-2.3c0.1-0.2,0.2-0.4,0.3-0.6c-1-0.5-1.8-1.5-1.8-2.7c' +
          '0-1.7,1.4-3,3-3'#13#10#9#9'c1.1,0,2.1,0.6,2.6,1.5c0.8-0.6,1.8-1,2.7-1.3C' +
          '18.6,10.4,16.6,9.1,14.3,9.1z M28.7,10.1l-3-5.1c-0.1-0.2-0.4-0.4-' +
          '0.7-0.4h-0.3'#13#10#9#9'L21,6c-0.8-0.6-1.6-1.1-2.5-1.5l-0.6-4C18,0.2,17.' +
          '6,0,17.3,0h-5.9c-0.4,0-0.7,0.3-0.7,0.6l-0.6,4C9.2,4.9,8.3,5.4,7.' +
          '6,6L3.9,4.5'#13#10#9#9'H3.6C3.3,4.5,3.1,4.6,3,4.9l-3,5.1c-0.2,0.3-0.1,0.' +
          '7,0.2,1l3.2,2.5c-0.1,0.5-0.1,1-0.1,1.5c0,0.5,0,1,0.1,1.5L0.3,19'#13 +
          #10#9#9'c-0.3,0.2-0.4,0.6-0.2,1l3,5.1c0.1,0.2,0.4,0.4,0.7,0.4H4L7.7,2' +
          '4c0.8,0.6,1.6,1.1,2.5,1.5l0.6,4c0,0.4,0.4,0.6,0.7,0.6h3.5'#13#10#9#9'c-0' +
          '.4-0.5-0.8-0.9-1.2-1.5l0,0l0,0c-0.3-0.5-0.5-0.9-0.7-1.4c-0.1-0.1' +
          '-0.1-0.3-0.2-0.4l-0.1-0.2l0,0l0,0'#13#10#9#9'c-0.3-0.9-0.5-1.7-0.6-2.6c0' +
          '-0.3-0.1-0.6-0.1-0.8l-0.9-0.3c-0.6-0.3-1.2-0.6-1.8-1.1l-1.4-1l-1' +
          '.6,0.6L4.5,22l-1-1.8L5.1,19l1.3-1'#13#10#9#9'l-0.2-1.7c0-0.5-0.1-0.8-0.1' +
          '-1.1c0-0.3,0-0.6,0.1-1.1l0.2-1.7l-1.3-1.1l-1.6-1.3l1-1.8l1.9,0.8' +
          'L8,9.7l1.4-1'#13#10#9#9'c0.6-0.5,1.3-0.8,1.9-1.1L12.8,7L13,5.3l0.3-2h2.1' +
          'l0.3,2L15.9,7l1.6,0.6c0.6,0.3,1.2,0.6,1.8,1.1l1.4,1l1.6-0.6l1.9-' +
          '0.8l1,1.8'#13#10#9#9'l-1.6,1.3L23,11.7c0.1,0,0.3,0,0.5,0l0,0h0.1c1,0.1,1' +
          '.9,0.3,2.8,0.6c0.1,0,0.2,0.1,0.3,0.1l1.9-1.5'#13#10#9#9'C28.8,10.9,28.9,' +
          '10.5,28.7,10.1z M14.3,9.1c-3.3,0-5.9,2.7-5.9,5.9c0,2.5,1.6,4.6,3' +
          '.8,5.5c0.1-0.8,0.4-1.6,0.7-2.3'#13#10#9#9'c0.1-0.2,0.2-0.4,0.3-0.6c-1-0.' +
          '5-1.8-1.5-1.8-2.7c0-1.7,1.4-3,3-3c1.1,0,2.1,0.6,2.6,1.5c0.8-0.6,' +
          '1.8-1,2.7-1.3'#13#10#9#9'C18.6,10.4,16.6,9.1,14.3,9.1z"/>'#13#10#9'<path d="M22' +
          '.7,32c-5.1,0-9.3-4.2-9.3-9.3c0-5.1,4.2-9.3,9.3-9.3c5.1,0,9.3,4.2' +
          ',9.3,9.3C32,27.8,27.9,32,22.7,32z M22.7,15.3'#13#10#9#9'c-4.1,0-7.3,3.3-' +
          '7.3,7.3s3.3,7.3,7.3,7.3s7.3-3.3,7.3-7.3C30,18.7,26.8,15.3,22.7,1' +
          '5.3z"/>'#13#10#9'<polygon points="28.3,21.6 28.3,23.4 23.6,23.5 23.6,28' +
          '.3 21.7,28.3 21.7,23.5 17.1,23.5 17.1,21.6 21.7,21.6 21.7,17 23.' +
          '6,17 23.6,21.6 '#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PySetupRemove'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M14.3,9.2c-3.3,0-5.9,2.7-5.' +
          '9,5.9c0,2.5,1.6,4.6,3.7,5.5c0.1-0.8,0.4-1.6,0.7-2.3c0.1-0.2,0.2-' +
          '0.4,0.3-0.6'#13#10#9#9'c-1-0.5-1.8-1.5-1.8-2.7c0-1.7,1.4-3,3-3c1.1,0,2.1' +
          ',0.6,2.6,1.5c0.8-0.6,1.8-1,2.7-1.3C18.5,10.4,16.5,9.2,14.3,9.2z ' +
          'M14.3,9.2'#13#10#9#9'c-3.3,0-5.9,2.7-5.9,5.9c0,2.5,1.6,4.6,3.7,5.5c0.1-0' +
          '.8,0.4-1.6,0.7-2.3c0.1-0.2,0.2-0.4,0.3-0.6c-1-0.5-1.8-1.5-1.8-2.' +
          '7'#13#10#9#9'c0-1.7,1.4-3,3-3c1.1,0,2.1,0.6,2.6,1.5c0.8-0.6,1.8-1,2.7-1.' +
          '3C18.5,10.4,16.5,9.2,14.3,9.2z M14.3,9.2c-3.3,0-5.9,2.7-5.9,5.9'#13 +
          #10#9#9'c0,2.5,1.6,4.6,3.7,5.5c0.1-0.8,0.4-1.6,0.7-2.3c0.1-0.2,0.2-0.' +
          '4,0.3-0.6c-1-0.5-1.8-1.5-1.8-2.7c0-1.7,1.4-3,3-3'#13#10#9#9'c1.1,0,2.1,0' +
          '.6,2.6,1.5c0.8-0.6,1.8-1,2.7-1.3C18.5,10.4,16.5,9.2,14.3,9.2z M1' +
          '4.3,9.2c-3.3,0-5.9,2.7-5.9,5.9'#13#10#9#9'c0,2.5,1.6,4.6,3.7,5.5c0.1-0.8' +
          ',0.4-1.6,0.7-2.3c0.1-0.2,0.2-0.4,0.3-0.6c-1-0.5-1.8-1.5-1.8-2.7c' +
          '0-1.7,1.4-3,3-3'#13#10#9#9'c1.1,0,2.1,0.6,2.6,1.5c0.8-0.6,1.8-1,2.7-1.3C' +
          '18.5,10.4,16.5,9.2,14.3,9.2z M28.6,10.1l-3-5.1c-0.1-0.2-0.4-0.4-' +
          '0.7-0.4h-0.3'#13#10#9#9'L21,6.1c-0.8-0.6-1.6-1.1-2.5-1.5l-0.6-3.9c0-0.4-' +
          '0.4-0.6-0.7-0.6h-5.9c-0.4,0-0.7,0.3-0.7,0.6L10,4.6C9.2,5,8.3,5.5' +
          ',7.6,6.1'#13#10#9#9'L3.9,4.6H3.6C3.3,4.6,3.1,4.7,3,5l-3,5.1c-0.2,0.3-0.1' +
          ',0.7,0.2,1l3.2,2.5c-0.1,0.5-0.1,1-0.1,1.5c0,0.5,0,1,0.1,1.5L0.3,' +
          '19'#13#10#9#9'c-0.3,0.2-0.4,0.6-0.2,1l3,5.1c0.1,0.2,0.4,0.4,0.7,0.4H4L7.' +
          '7,24c0.8,0.6,1.6,1.1,2.5,1.5l0.6,3.9c0,0.4,0.4,0.6,0.7,0.6h3.4'#13#10 +
          #9#9'c-0.4-0.5-0.8-0.9-1.2-1.5l0,0l0,0c-0.3-0.5-0.5-0.9-0.7-1.4c-0.' +
          '1-0.1-0.1-0.3-0.2-0.4l-0.1-0.2l0,0l0,0c-0.3-0.9-0.5-1.7-0.6-2.6'#13 +
          #10#9#9'c0-0.3-0.1-0.6-0.1-0.8l-0.9-0.3c-0.6-0.3-1.2-0.6-1.8-1.1l-1.4' +
          '-1l-1.6,0.6l-1.9,0.6l-1-1.8L5.1,19l1.3-1l-0.2-1.7'#13#10#9#9'c0-0.5-0.1-' +
          '0.8-0.1-1.1c0-0.3,0-0.6,0.1-1.1l0.2-1.7l-1.3-1.1l-1.6-1.3l1-1.8l' +
          '1.9,0.8L8,9.7l1.4-1c0.6-0.5,1.3-0.8,1.9-1.1'#13#10#9#9'l1.6-0.6L13,5.4l0' +
          '.3-2h2.1l0.3,2l0.2,1.7l1.6,0.6C18,8,18.6,8.3,19.2,8.8l1.4,1l1.6-' +
          '0.6L24,8.4l1,1.8l-1.6,1.3l-0.5,0.4'#13#10#9#9'c0.1,0,0.3,0,0.5,0l0,0h0.1' +
          'c1,0.1,1.9,0.3,2.8,0.6c0.1,0,0.2,0.1,0.3,0.1l1.9-1.5C28.7,10.9,2' +
          '8.8,10.5,28.6,10.1z M14.3,9.2'#13#10#9#9'c-3.3,0-5.9,2.7-5.9,5.9c0,2.5,1' +
          '.6,4.6,3.7,5.5c0.1-0.8,0.4-1.6,0.7-2.3c0.1-0.2,0.2-0.4,0.3-0.6c-' +
          '1-0.5-1.8-1.5-1.8-2.7'#13#10#9#9'c0-1.7,1.4-3,3-3c1.1,0,2.1,0.6,2.6,1.5c' +
          '0.8-0.6,1.8-1,2.7-1.3C18.5,10.4,16.5,9.2,14.3,9.2z"/>'#13#10#9'<rect x=' +
          '"17.1" y="21.8" width="11.2" height="1.9"/>'#13#10#9'<path d="M22.8,13.' +
          '4c-5.1,0-9.3,4.1-9.3,9.3c0,5.1,4.1,9.3,9.3,9.3s9.3-4.1,9.3-9.3C3' +
          '2,17.5,27.8,13.4,22.8,13.4z M22.8,29.9'#13#10#9#9'c-4,0-7.3-3.3-7.3-7.3s' +
          '3.3-7.3,7.3-7.3s7.3,3.3,7.3,7.3S26.7,29.9,22.8,29.9z"/>'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = 'Python'
        SVGText = 
          '<svg viewBox="0 0 18 18">'#13#10#9#9'<path fill="#4488FF" d="M11.298,8.0' +
          '2c1.295-0.587,1.488-5.055,0.724-6.371c-0.998-1.718-5.742-1.373-7' +
          '.24-0.145'#13#10#9#9#9'C4.61,2.114,4.628,3.221,4.636,4.101h4.702v0.412H4.' +
          '637c0,0.006-2.093,0.013-2.093,0.013c-3.609,0-3.534,7.838,1.228,7' +
          '.838'#13#10#9#9#9'c0,0,0.175-1.736,0.481-2.606C5.198,7.073,9.168,8.986,11' +
          '.298,8.02z M6.375,3.465c-0.542,0-0.981-0.439-0.981-0.982'#13#10#9#9#9'c0-' +
          '0.542,0.439-0.982,0.981-0.982c0.543,0,0.982,0.44,0.982,0.982C7.3' +
          '58,3.025,6.918,3.465,6.375,3.465z"/>'#13#10#9#9'<path fill="#FFCE00" d="' +
          'M13.12,4.691c0,0-0.125,1.737-0.431,2.606c-0.945,2.684-4.914,0.77' +
          '2-7.045,1.738'#13#10#9#9#9'C4.35,9.622,4.155,14.09,4.92,15.406c0.997,1.71' +
          '9,5.741,1.374,7.24,0.145c0.172-0.609,0.154-1.716,0.146-2.596H7.6' +
          '03v-0.412h4.701'#13#10#9#9#9'c0-0.006,2.317-0.013,2.317-0.013C17.947,12.5' +
          '3,18.245,4.691,13.12,4.691z M10.398,13.42c0.542,0,0.982,0.439,0.' +
          '982,0.982'#13#10#9#9#9'c0,0.542-0.44,0.981-0.982,0.981s-0.981-0.439-0.981' +
          '-0.981C9.417,13.859,9.856,13.42,10.398,13.42z"/>'#13#10'</svg>'
      end
      item
        IconName = 'PythonScript'
        SVGText = 
          '<svg viewBox="0 0 32 32" >'#13#10'<g transform="translate(-275.1 -318.' +
          '4)">'#13#10#9'<path d="M290.9,318.8h-11.3c-1.6,0-2.9,1.3-2.8,2.9v23.4c0' +
          ',1.6,1.2,2.9,2.8,2.9h17c1.6,0,2.9-1.3,2.8-2.9'#13#10#9#9'v-17.6L290.9,31' +
          '8.8z M279.6,345.2v-23.4h9.9v7.3h7.1v16.1L279.6,345.2L279.6,345.2' +
          'z"/>'#13#10'</g>'#13#10'<g transform="translate(-16.5,-18) scale(1.45)">'#13#10#9'<' +
          'path fill="#4488FF" d="M20.8,22.5H25c1.2,0,2.1-1,2.1-2.2v-4c-0.1' +
          '-1.2-1-2.1-2.1-2.2c0,0,0,0-0.1,0c-0.6-0.1-3.4-0.2-4.2,0'#13#10#9#9'c-1.2' +
          ',0.3-1.8,0.6-2,1.2c-0.1,0.3-0.1,0.6-0.1,1v1.6h4.2v0.6h-5.9c-1.3,' +
          '0-2.4,0.9-2.7,2.1c-0.1,0.3-0.1,0.5-0.2,0.8'#13#10#9#9'c0,0.1,0,0.2-0.1,0' +
          '.3c-0.1,0.4-0.1,0.8-0.1,1.2c0,0.7,0.1,1.4,0.3,2.1c0.1,0.3,0.2,0.' +
          '6,0.3,0.8c0.3,0.6,0.6,1,1.1,1.2'#13#10#9#9'c0.3,0.1,0.5,0.2,0.8,0.2h1.5v' +
          '-2C18.1,23.6,19.3,22.5,20.8,22.5z M20.5,16.7c-0.4,0-0.8-0.4-0.8-' +
          '0.8s0.4-0.8,0.8-0.8'#13#10#9#9's0.8,0.4,0.8,0.8C21.4,16.3,21,16.7,20.5,1' +
          '6.7z"/>'#13#10#9'<path fill="#FFCE00" d="M24.9,23.5h-4.2c-1.2,0-2.1,1-2' +
          '.1,2.2v4c0.1,1.2,1,2.1,2.1,2.2c0,0,0,0,0.1,0c0.6,0.1,3.4,0.2,4.2' +
          ',0'#13#10#9#9'c1.2-0.3,1.8-0.6,2-1.2c0.1-0.3,0.1-0.6,0.1-1v-1.6h-4.2v-0.' +
          '6h5.9c1.3,0,2.4-0.9,2.7-2.1c0.1-0.3,0.1-0.5,0.2-0.8'#13#10#9#9'c0-0.1,0-' +
          '0.2,0.1-0.3c0.1-0.4,0.1-0.8,0.1-1.2c0-0.7-0.1-1.4-0.3-2.1c-0.1-0' +
          '.3-0.2-0.6-0.3-0.8c-0.3-0.6-0.6-1-1.1-1.2'#13#10#9#9'c-0.3-0.1-0.5-0.2-0' +
          '.8-0.2h-1.5v2C27.6,22.4,26.4,23.5,24.9,23.5z M25.2,29.3c0.4,0,0.' +
          '8,0.4,0.8,0.8c0,0.4-0.4,0.8-0.8,0.8'#13#10#9#9's-0.8-0.4-0.8-0.8C24.3,29' +
          '.7,24.7,29.3,25.2,29.3z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Quit'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M23.6,3l5.2,5.2l-6.1,6.1c0.2' +
          ',0.7,0.3,1.3,0.3,2.1c0,0.5,0,0.8-0.1,1.2l6.3,6.3L23.9,29l-5.9-5.' +
          '9c-0.7,0.2-1.4,0.3-2.1,0.3'#13#10#9'c-0.7,0-1.4-0.1-2.1-0.3L8,29l-5.2-5' +
          '.2l6.1-6.1c-0.1-0.5-0.1-0.9-0.1-1.4c0-0.7,0.1-1.5,0.3-2.1l-5.9-6' +
          'L8.3,3l6.3,6.3'#13#10#9'c0.4-0.1,0.8-0.1,1.3-0.1s0.9,0.1,1.4,0.1L23.6,3' +
          ' M23.6,0.2c-0.7,0-1.4,0.3-2,0.8l-5.3,5.3c-0.1,0-0.2,0-0.4,0c-0.1' +
          ',0-0.2,0-0.2,0'#13#10#9'L10.4,1c-0.6-0.6-1.3-0.8-2-0.8S7,0.5,6.5,1L1.2,' +
          '6.2c-0.6,0.6-0.8,1.2-0.8,2c0,0.7,0.3,1.5,0.8,2L6.1,15C6,15.4,6,1' +
          '5.9,6,16.2'#13#10#9'c0,0.1,0,0.2,0,0.4l-5.1,5.1C0.3,22.3,0,23,0,23.7s0.' +
          '3,1.5,0.8,2l5.2,5.2c0.6,0.6,1.2,0.8,2,0.8s1.5-0.3,2-0.8l4.8-4.8'#13 +
          #10#9'c0.4,0,0.7,0.1,1.1,0.1c0.4,0,0.8,0,1.2-0.1L22,31c0.6,0.6,1.2,0' +
          '.8,2,0.8s1.5-0.3,2-0.8l5.2-5.2c1.1-1.1,1.1-2.9,0-3.9l-5.3-5.3'#13#10#9 +
          'c0-0.1,0-0.1,0-0.2c0-0.4,0-0.7-0.1-1.1l5-5c1.1-1.1,1.1-2.9,0-3.9' +
          'L25.5,1C25,0.5,24.3,0.2,23.6,0.2L23.6,0.2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Redo'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(119.5 -231.7)' +
          '">'#13#10#9'<path d="M-100.6,242.7v-5.6l10.2,9.9l-10.2,9.9v-5.8c-7.2,0-' +
          '12.3,2.3-16,7.2 C-115.1,251.1-110.8,244.1-100.6,242.7z"/>'#13#10'</g>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'Refresh'
        SVGText = 
          '<svg viewBox="0 0 64 64">'#13#10#9#9'<path'#13#10#9#9#9'd="m59.008 34a2.921 2.921' +
          ' 0 0 0 -2.208-1.023h-.116a3.032 3.032 0 0 0 -2.966 2.616 21.653 ' +
          '21.653 0 0 1 -15.968 17.678 22.064 22.064 0 0 1 -23.792-8.7 1 1 ' +
          '0 0 1 .822-1.571h5.22a2 2 0 0 0 0-4h-16v16a2 2 0 0 0 4 0v-5.34a1' +
          ' 1 0 0 1 1.793-.608 28 28 0 0 0 49.874-12.721 2.9 2.9 0 0 0 -.65' +
          '9-2.331z" />'#13#10#9#9'<path'#13#10#9#9#9'd="m44 25h16v-16a2 2 0 0 0 -4 0v5.34a1' +
          ' 1 0 0 1 -1.793.608 28 28 0 0 0 -49.853 12.582 3.006 3.006 0 0 0' +
          ' 2.971 3.47 3.029 3.029 0 0 0 2.966-2.588 21.993 21.993 0 0 1 39' +
          '.751-8.983 1 1 0 0 1 -.822 1.571h-5.22a2 2 0 0 0 0 4z" />'#13#10'</svg' +
          '>'
      end
      item
        IconName = 'RegExp'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="matrix(1.0212803,0,0,1' +
          '.0213893,322.73762,227.28249)">'#13#10#9#9'<path d="M-312-205.6c-1.2-1.8' +
          '-1.1-3.4,0.2-5l1.1-1.3c0.8-0.9,0.8-1.7,0-2.4l1.1-1.4c0.8,0.6,1.5' +
          ',0.5,2.3-0.3l1.2-1.4'#13#10#9#9#9'c0.7-0.8,1.4-1.3,2.2-1.5c0.8-0.2,1.7,0,' +
          '2.7,0.4l-0.5,1.4c-0.4-0.2-0.9-0.2-1.3,0s-0.9,0.5-1.3,1l-1.3,1.5'#13 +
          #10#9#9#9'c-0.8,1-1.8,1.3-2.9,1.1c0.4,1.1,0.2,2.1-0.6,3.1l-1.2,1.4c-0.' +
          '8,1-1,1.9-0.6,2.7L-312-205.6z"/>'#13#10#9#9'<path d="M-300.5-208.9c-0.2-' +
          '0.3-0.4-0.5-0.6-0.6c-0.7-0.6-1.4-0.8-2-0.5l-3.9,4.6l-1.9-1.6l6-7' +
          '.1l1.8,1.5l-0.8,0.9'#13#10#9#9#9'c0.9-0.3,1.7-0.2,2.4,0.4c0.2,0.2,0.4,0.4' +
          ',0.5,0.6L-300.5-208.9z"/>'#13#10#9#9'<path d="M-301.1-200.1c-1-0.9-1.6-1' +
          '.9-1.7-3.1c-0.1-1.2,0.3-2.3,1.2-3.4l0.2-0.2c0.6-0.7,1.3-1.2,2-1.' +
          '6c0.7-0.3,1.5-0.4,2.3-0.3'#13#10#9#9#9'c0.8,0.1,1.5,0.5,2.1,1c1,0.8,1.5,1' +
          '.8,1.5,2.8c0,1.1-0.4,2.2-1.4,3.3l-0.7,0.9l-4.5-3.8c-0.3,0.5-0.5,' +
          '1-0.4,1.6'#13#10#9#9#9'c0.1,0.5,0.3,1,0.8,1.4c0.7,0.6,1.5,0.8,2.4,0.6l0.1' +
          ',1.8c-0.6,0.2-1.3,0.2-1.9,0C-299.8-199.3-300.5-199.6-301.1-200.1' +
          'z'#13#10#9#9#9#9'M-296.4-206.1c-0.4-0.3-0.8-0.4-1.2-0.4c-0.4,0.1-0.9,0.3-1' +
          '.4,0.7l2.6,2.2l0.1-0.2c0.3-0.4,0.5-0.8,0.5-1.3'#13#10#9#9#9'C-295.8-205.4' +
          '-296-205.8-296.4-206.1z"/>'#13#10#9#9'<path d="M-299.2-196.6c0.9,0.3,1.7' +
          ',0,2.6-1l1.2-1.4c0.8-1,1.8-1.3,2.9-1c-0.5-1.1-0.3-2.1,0.6-3.1l1.' +
          '2-1.4c0.8-1,1-1.9,0.6-2.8'#13#10#9#9#9'l1.3-0.7c0.6,0.9,0.9,1.7,0.9,2.5s-' +
          '0.4,1.6-1,2.4l-1.2,1.4c-0.7,0.9-0.7,1.6,0.1,2.3l-1.1,1.4c-0.8-0.' +
          '7-1.5-0.5-2.3,0.4l-1.2,1.4'#13#10#9#9#9'c-1.3,1.5-2.9,1.8-4.8,1L-299.2-19' +
          '6.6z"/>'#13#10#9'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Rename'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M17,7H22V17H17V19A1,1 0 ' +
          '0,0 18,20H20V22H17.5C16.95,22 16,21.55 16,21C16,21.55 15.05,22 1' +
          '4.5,22H12V20H14A1,1 0 0,0 15,19V5A1,1 0 0,0 14,4H12V2H14.5C15.05' +
          ',2 16,2.45 16,3C16,2.45 16.95,2 17.5,2H20V4H18A1,1 0 0,0 17,5V7M' +
          '2,7H13V9H4V15H13V17H2V7M20,15V9H17V15H20Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Replace'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M29.8,4.7c-2.2-2.8-5.5-4.6-' +
          '9.5-4.6S13,1.7,10.8,4.7c-0.2,0.2-0.3,0.4-0.4,0.6l1.9,4.9c0.5-2.4' +
          ',2-4.3,3.9-5.4'#13#10#9#9'c1.3-0.6,2.7-1.1,4.1-1.1c1.6,0,2.8,0.3,4.1,1.1' +
          'c2.5,1.4,4.1,4.1,4.1,7.1c0.1,1.8-0.5,3.5-1.5,4.9c0.5,0.7,0.9,1.7' +
          ',0.9,3'#13#10#9#9'c0,0.6-0.1,1.1-0.3,1.6c2.6-2.2,4.3-5.4,4.3-9.2C32.3,9.' +
          '3,31.4,6.7,29.8,4.7z M5,24.4L0.3,29l-0.2,0.2L2.9,32l4.5-4.5'#13#10#9#9'c' +
          '-1-0.9-1.8-2.1-2.1-3.4L5,24.4z"/>'#13#10#9'<path d="M9.5,6.9L8.9,5.4H6.' +
          '2L0.9,19.7H4l1-2.9h5.1l0.7,2l0.3,0.9l0,0h3.1L9.5,6.9z M5.8,14.4l' +
          '1.8-5.3l0.9,2.8l0.9,2.5H5.8z"/>'#13#10#9'<path d="M26.1,23.9c-0.3-0.4-0' +
          '.6-0.7-1.1-0.9c-0.2-0.1-0.4-0.2-0.6-0.2c0.6-0.3,1.1-0.6,1.5-1.2c' +
          '0.3-0.5,0.5-1.1,0.5-1.8'#13#10#9#9's-0.1-1.4-0.4-1.9c-0.2-0.4-0.5-0.7-0.' +
          '9-1c-0.9-0.7-2.2-1-3.9-1h-4.9V30h5.5c1.6,0,2.8-0.4,3.7-1.1c0.9-0' +
          '.7,1.3-1.7,1.3-3.1'#13#10#9#9'C26.8,25.1,26.6,24.5,26.1,23.9z M19.2,18.3' +
          'h2c0.8,0,1.4,0.1,1.8,0.4c0.3,0.2,0.4,0.5,0.5,0.8c0.1,0.2,0.1,0.4' +
          ',0.1,0.6'#13#10#9#9'c0,1.1-0.7,1.7-2.2,1.8h-2.2L19.2,18.3L19.2,18.3z M23' +
          '.3,27.3c-0.4,0.3-0.9,0.5-1.6,0.5h-2.5V24h2.7H22c1.3,0,1.9,0.7,1.' +
          '9,2'#13#10#9#9'C23.9,26.5,23.7,26.9,23.3,27.3z"/>'#13#10#9'<path d="M14.8,27.5L' +
          '14.8,27.5L11.6,30l0.2-2.2c-1.3-0.1-2.5-0.7-3.4-1.5l0,0c-1-0.9-1.' +
          '7-2.2-2-3.6l0,0c-0.1-0.5-0.1-1-0.1-1.5'#13#10#9#9'C6.4,20,6.9,19,7.5,18.' +
          '2l1.1,1.3C8.3,20,8.1,20.5,8,21l0,0c0,0.1,0,0.3,0,0.4c-0.2,1.5,0.' +
          '4,2.8,1.4,3.7l0,0c0.7,0.6,1.5,1,2.5,1.1'#13#10#9#9'l0.2-2.2L14.8,27.5L14' +
          '.8,27.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ReplaceAll'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M21.2,14.8l4.4,4.4c-1.2,1.2' +
          '-2.9,2.2-4.7,2.5c0.2,0.5,0.3,1,0.3,1.6c0,0.7-0.2,1.3-0.5,1.9c2.9' +
          '-0.3,5.5-1.6,7.5-3.6'#13#10#9#9'l3.5,3.7l0.1-10.5H21.2z M19.5,0.8'#13#10#9#9'c-3' +
          '.5,0-6.5,1.3-8.8,3.6L7.2,0.7v10.5h10.5l-4.4-4.4c1.6-1.5,3.7-2.6,' +
          '6.1-2.6c4.2,0,7.8,2.9,8.6,7h3.5C30.6,5.5,25.6,0.9,19.5,0.8z"/>'#13#10 +
          #9'<path d="M9.4,20.5l-0.7-1.7l-1.1-3l-0.9-2.2l-0.5-1.1H4.2l-4,10.' +
          '7h2.4L3.3,21h3.9l0.4,1.1l0.1,0.3L8,23.2h2.4L9.4,20.5z'#13#10#9#9#9'M4,19.' +
          '2l1.3-4l0.8,2.1l0.7,1.9H4z"/>'#13#10#9'<path d="M10.8,29.2L10.8,29.2l-0' +
          '.3,0.3l-2.1,1.8l0.2-1.7c-0.9-0.1-1.6-0.4-2.3-0.9c-0.1-0.1-0.2-0.' +
          '1-0.3-0.2l0,0'#13#10#9#9'c-0.8-0.7-1.3-1.6-1.5-2.7l0,0c0-0.1-0.1-0.3-0.1' +
          '-0.4c0-0.2,0-0.5,0-0.8c0.1-0.8,0.4-1.5,0.9-2.2c0-0.1,0.1-0.1,0.1' +
          '-0.2l0.2,0.2'#13#10#9#9'l0.7,0.8c-0.2,0.2-0.3,0.5-0.4,0.7c-0.1,0.2-0.1,0' +
          '.4-0.1,0.6l0,0c0,0.1,0,0.2,0,0.3c-0.2,1.1,0.3,2.2,1,2.9'#13#10#9#9'c0.1,' +
          '0.1,0.2,0.2,0.3,0.2c0.5,0.3,1,0.5,1.6,0.6l0.2-1.7l1.7,2.1L10.8,2' +
          '9.2L10.8,29.2z"/>'#13#10#9'<path d="M19.5,26.4c-0.2-0.3-0.5-0.5-0.9-0.7' +
          'c-0.2-0.1-0.3-0.1-0.5-0.2c0.2-0.1,0.4-0.2,0.6-0.3c0.2-0.2,0.4-0.' +
          '3,0.6-0.6'#13#10#9#9'c0.3-0.4,0.4-0.9,0.4-1.4s-0.1-1-0.4-1.4l-0.1-0.1c-0' +
          '.2-0.3-0.4-0.5-0.6-0.7c-0.7-0.5-1.6-0.8-2.9-0.8H12V31h4.2'#13#10#9#9'c1.' +
          '2,0,2.2-0.3,2.8-0.9c0.6-0.6,1-1.3,1-2.3C19.9,27.4,19.8,26.9,19.5' +
          ',26.4z M14.1,22.2h1.5c0.6,0,1,0.1,1.3,0.3'#13#10#9#9'c0.2,0.2,0.3,0.4,0.' +
          '4,0.6c0,0.1,0.1,0.3,0.1,0.5c0,0.8-0.4,1.1-1.2,1.3c-0.1,0-0.3,0-0' +
          '.4,0h-1.6L14.1,22.2L14.1,22.2z M17.3,29'#13#10#9#9'c-0.3,0.3-0.7,0.4-1.2' +
          ',0.4h-1.9v-2.9h2h0.1c1,0,1.4,0.6,1.4,1.5C17.8,28.4,17.6,28.7,17.' +
          '3,29z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Run'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path fill="#22AA22" d="M28.5,16.1l-2' +
          '.4-1.5L20,10.6l-7-4.5L3.5,0V32l15.2-9.6l9.5-6.1L28.5,16.1z M8,23' +
          '.9V8.7l3.8,2.4'#13#10#9'l3.6,2.4l4.7,3.1l-0.7,0.3L8,23.9z"/>'#13#10'</svg>'
      end
      item
        IconName = 'RunConfigAdd'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'    <path d="M16 28.4l-1.1.6v-8.8l.9-' +
          '.5c-1.1-.6-2.1-1.2-3-2.1l-5.4-2.8-1.83-.9L7 13.1l2.7-1.5c-.2-.7-' +
          '.2-1.6-.2-2.3l-7 3.7c-.1 0-.1.1-.2.1l-.1.1-.1.1c-.2.2-.3.4-.3.6v' +
          '10.9c0 .4.3.8.6 1l10.7 6c.1 0 .2.1.3.1.1 0 .2.1.2.1h.4c.1 0 .2 0' +
          ' .2-.1h.1c.1 0 .1-.1.2-.1l3.9-2.1-2.1-1.3zm-3.4.5L4 24.1v-8.2l8.' +
          '6 4.4z"/>'#13#10'    <path d="M25.6 19.7c-.7.4-1.5.6-2.2.8v1l2.3 2.3-.' +
          '1-4.1z"/>'#13#10'    <polygon fill="#22AA22" points="23.3,23.3 21,21 1' +
          '9.2,22.8 21.2,24.7 16.1,24.7 16.1,27.2 21.2,27.2 19.2,29.1 21,30' +
          '.9 25.9,26 25.4,25.6"/>'#13#10'    <polygon points="26.4,8.8 21.6,8.8 ' +
          '21.6,3.9 19.7,3.9 19.7,8.8 16.1,8.8 14.8,8.8 14.8,10.7 19.7,10.7' +
          ' 19.7,13.3 19.7,15.6 21.6,15.5 21.6,11.6 21.6,10.7 26.4,10.6"/>'#13 +
          #10'    <path d="M27.4 2.9A9.78 9.78 0 0 0 20.5 0c-2.5 0-4.9.9-6.8 ' +
          '2.9A9 9 0 0 0 11 8.4a9.64 9.64 0 0 0 .6 4.7l.9 1.8c.4.6.7 1 1.2 ' +
          '1.5.2.2.5.5.7.6.2.2.4.3.6.5.8.6 1.7 1 2.6 1.3a8.8 8.8 0 0 0 5.6.' +
          '1c.7-.2 1.6-.6 2.2-1 .6-.4 1.3-.8 1.8-1.5 4-3.7 4-9.8.2-13.5zm-1' +
          '.3 12.2c-.2.2-.4.4-.6.5-.6.6-1.4 1-2.2 1.3s-1.7.5-2.5.5h-.3c-1.2' +
          ' 0-2.3-.3-3.4-.7-.7-.4-1.4-.8-2-1.5a7.8 7.8 0 0 1 0-10.9C16.6 2.' +
          '8 18.6 2 20.5 2s4 .7 5.4 2.2c3.1 3.1 3.1 8 .2 10.9z"/>'#13#10'</svg>'
      end
      item
        IconName = 'RunConfigEdit'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'    <polygon points="30.6,9.1 29.1,10' +
          '.5 29,10.6 27.8,11.7 27.8,18.4 30.7,21.4 30.6,9.3 30.6,9.2"/>'#13#10' ' +
          '   <path d="M15.35 3.15l5.18 2.35 1.87-1.6L15.7.4c-.4-.2-.9-.2-1' +
          '.4 0l-13.2 7H1c-.1 0-.1.1-.2.1l-.1.1-.2.1c-.2.3-.3.6-.3.9v13.9c0' +
          ' .5.3 1 .7 1.3l13.7 7.8.5.2h.6l.5-.2 4.9-2.7-2.7-1.7c.1 0 0 0 0 ' +
          '0L17 28v-6.6l-2.9-.1v6.6l-11-6.2V11.1l7.31 3.86L13 12.9 5 8.75"/' +
          '>'#13#10'    <polygon fill="#22AA22" points="27.8,20.7 24.7,17.8 22.5,' +
          '20 25.1,22.5 18.5,22.5 18.5,25.7 25.1,25.7 22.5,28.2 24.7,30.4 3' +
          '1,24.1 30.5,23.6"/>'#13#10'    <path d="M20.5 8l-2.3 2.2-1.7 1.6-3.7 3' +
          '.6v.8l-.1 3.7h1.5l2.5.1h.5l.2-.2 2.5-2.4.6-.6 3.1-3 2.2-2.1 1-.9' +
          ' 1-.9-.1-.2-.1-.1-3.9-4.4zm4.2 1.3c-2.37 2.18-4.79 4.57-6.8 6.5-' +
          '.52.43-1.06.96-1.5 1.4h-1v-1l4-3.8 4-3.8.4-.4c.25.41.59.74.9 1.1' +
          'z"/>'#13#10'    <path d="M31.5 4.6l-.1-.2-2.5-2.6c-.2-.2-.4-.3-.8-.3H2' +
          '8c-.1 0-.3.1-.4.1-.1 0-.2.1-.2.2l-2.1 2.1a86.11 86.11 0 0 1 4.1 ' +
          '4.4l.4-.4 1.6-1.7c.5-.4.5-1.1.1-1.6z"/>'#13#10'</svg>'
      end
      item
        IconName = 'RunLast'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#22AA22" d="M5.9,2.9v21.' +
          '7l0.8-0.5c0.1-3.4,2.9-6.1,6.3-6.1h3.3l3.7-2.3l1.9-1.2l1.2-0.8L5.' +
          '9,2.9z M9,19V8.6l8.2,5.2L9,19z"/>'#13#10#9'<path d="M23,26.8c0.1,0.1,0.' +
          '1,2.4,0.1,2.4H13c-2.6,0-4.8-2.1-4.8-4.8c0-0.4,0.1-0.9,0.2-1.3c0.' +
          '6-2,2.4-3.5,4.6-3.5h8.4V16l4.8,4.8'#13#10#9#9'l-4.8,4.8v-3.6h-8.4c-1.3,0' +
          '-2.4,1.1-2.4,2.4s1.1,2.4,2.4,2.4L23,26.8L23,26.8z"/>'#13#10'</svg>'
      end
      item
        IconName = 'RunScript'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M9 14H14V15.7C13.9 15.8 1' +
          '3.9 15.9 13.8 16H9V14M9 12H14V10H9V12M9 8H14V6H9V8M7 5C7 4.4 7.4' +
          ' 4 8 4H16V13.8C16.6 13.4 17.3 13.2 18 13.1V5C18 4.4 18.4 4 19 4S' +
          '20 4.4 20 5V6H22V5C22 3.3 20.7 2 19 2H8C6.3 2 5 3.3 5 5V16H7V5M1' +
          '3 19V18.4 18H2V19C2 20.7 3.3 22 5 22H13.8C13.3 21.1 13 20.1 13 1' +
          '9Z" />'#13#10'   <path fill="#22AA22" d="M16 15v8l6 -4z" />'#13#10'</svg>'
      end
      item
        IconName = 'RunToCursor'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#22AA22" d="M20.6,15.9l-' +
          '1.7-1.1L14.5,12l-5-3.2L2.7,4.4v22.9l10.9-6.9l6.8-4.4L20.6,15.9z ' +
          'M5.8,21.5V10.7'#13#10#9#9'l2.7,1.7l2.6,1.7l3.4,2.2L14,16.4L5.8,21.5z"/>'#13 +
          #10#9'<polygon points="29.3,3.2 29.3,0.2 19.9,0.2 19.9,3.2 23.2,3.2 ' +
          '23.2,28.8 19.9,28.8 19.9,31.8 29.3,31.8 29.3,28.8 26.2,28.8 '#13#10#9#9 +
          '26.2,3.2 '#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Save'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M23.5,2.5h-18c-1.7,0-3,1.4-' +
          '3,3v21c0,1.6,1.3,3,3,3h21c1.6,0,3-1.4,3-3v-18L23.5,2.5z M26.5,26' +
          '.5h-21v-21h16.8l4.2,4.2'#13#10#9#9'V26.5z M16,16c-2.5,0-4.5,1.9-4.5,4.5s' +
          '2,4.5,4.5,4.5s4.5-1.9,4.5-4.5S18.6,16,16,16z M7,7h13.5v6H7V7z"/>' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = 'SaveAll'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-567.64 -367.' +
          '91)">'#13#10#9'<path d="M575.4,377.8v3.7h1v-3.7H575.4z M584.5,368.2H570' +
          'c-1.3,0-2.4,1.1-2.4,2.4v16.9c0,1.3,1.1,2.4,2.4,2.4'#13#10#9#9'h0.4v-2.4H' +
          '570v-16.9h13.5l1,1h3.4L584.5,368.2z M589.9,374.2l-0.6-0.6l-0.6-0' +
          '.7h-2.8h-3.9h-10.3v17l0,0v2.4c0,1.3,1.1,2.4,2.4,2.4'#13#10#9#9'h2.2v-2.4' +
          'h-2.2v-16.9h13.5l1,1h3.4L589.9,374.2z M597.1,380.3l-2.4-2.4h-1.2' +
          'h-3.4h-3.9h-8.4v17l0,0v2.4c0,1.3,1.1,2.4,2.4,2.4h17'#13#10#9#9'c1.3,0,2.' +
          '4-1.1,2.4-2.4v-14.5L597.1,380.3z M597.1,397.3h-16.9v-17h13.5l3.4' +
          ',3.4V397.3z"/>'#13#10#9'<path d="M589,389.1c-0.1,0-0.2,0-0.3,0c-1.9,0-3' +
          '.5,1.5-3.5,3.4v0.1c0,0.4,0.1,0.8,0.2,1.2s0.4,0.8,0.7,1.2'#13#10#9#9'c1.4' +
          ',1.5,3.6,1.6,5.1,0.2c0.1-0.1,0.2-0.2,0.2-0.3c0.3-0.3,0.5-0.7,0.6' +
          '-1.2c0.1-0.4,0.2-0.8,0.2-1.2'#13#10#9#9'C592.3,390.8,590.9,389.2,589,389' +
          '.1L589,389.1z"/>'#13#10#9'<path d="M589.3,381.6h-7.6v4.7h10.5v-4.7H589.' +
          '3L589.3,381.6z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Search'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M-0.1,29.3l2.8,2.8l9.2-9.2v' +
          '-1.5l0.6-0.6c2,1.8,4.8,2.9,7.7,2.9c6.6,0,11.9-5.3,11.9-11.9s-5.4' +
          '-11.9-12-11.9'#13#10#9#9'S8.2,5.2,8.2,11.8c0,2.9,1.1,5.7,2.9,7.7l-0.6,0.' +
          '6H9L-0.1,29.3z M11.8,11.8c0-4.6,3.7-8.3,8.3-8.3c4.6,0,8.3,3.7,8.' +
          '3,8.3'#13#10#9#9's-3.7,8.3-8.3,8.3C15.5,20.1,11.8,16.4,11.8,11.8z"/>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'SearchFolder'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="translate(-289.9 -109.' +
          '8)">'#13#10#9#9'<path d="M318.1,116.8h-12.2l-3-3h-9.1c-1.7,0-3,1.3-3,3V1' +
          '34l3-3v-11.2h3c1.5-1.2,3.4-2,5.5-2s4,0.7,5.5,2'#13#10#9#9#9'h10.3v14.9h-2' +
          '0.5l-3,3h23.5c1.7,0,3-1.3,3-3v-14.9C321.1,118.2,319.8,116.8,318.' +
          '1,116.8z"/>'#13#10#9'</g>'#13#10#9'<path d="M14.9,10c-0.8-0.3-1.6-0.5-2.5-0.5S' +
          '10.7,9.7,9.9,10c-2.6,1-4.4,3.5-4.4,6.4c0,1.7,0.6,3.3,1.7,4.4L7,2' +
          '1.2H6.2L4,23.3'#13#10#9#9'L1.2,26l-0.4,0.4L2.4,28l5.3-5.3v-0.9l0.4-0.3c1' +
          '.1,1.1,2.8,1.7,4.5,1.7c3.8,0,6.9-3,6.9-6.9C19.3,13.5,17.5,11,14.' +
          '9,10z'#13#10#9#9#9'M12.4,21.2c-2.7,0-4.8-2.1-4.8-4.8s2.1-4.8,4.8-4.8s4.8,' +
          '2.1,4.8,4.8S15.1,21.2,12.4,21.2z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Setup'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M15.9,18.45C17.25,18.45 ' +
          '18.35,17.35 18.35,16C18.35,14.65 17.25,13.55 15.9,13.55C14.54,13' +
          '.55 13.45,14.65 13.45,16C13.45,17.35 14.54,18.45 15.9,18.45M21.1' +
          ',16.68L22.58,17.84C22.71,17.95 22.75,18.13 22.66,18.29L21.26,20.' +
          '71C21.17,20.86 21,20.92 20.83,20.86L19.09,20.16C18.73,20.44 18.3' +
          '3,20.67 17.91,20.85L17.64,22.7C17.62,22.87 17.47,23 17.3,23H14.5' +
          'C14.32,23 14.18,22.87 14.15,22.7L13.89,20.85C13.46,20.67 13.07,2' +
          '0.44 12.71,20.16L10.96,20.86C10.81,20.92 10.62,20.86 10.54,20.71' +
          'L9.14,18.29C9.05,18.13 9.09,17.95 9.22,17.84L10.7,16.68L10.65,16' +
          'L10.7,15.31L9.22,14.16C9.09,14.05 9.05,13.86 9.14,13.71L10.54,11' +
          '.29C10.62,11.13 10.81,11.07 10.96,11.13L12.71,11.84C13.07,11.56 ' +
          '13.46,11.32 13.89,11.15L14.15,9.29C14.18,9.13 14.32,9 14.5,9H17.' +
          '3C17.47,9 17.62,9.13 17.64,9.29L17.91,11.15C18.33,11.32 18.73,11' +
          '.56 19.09,11.84L20.83,11.13C21,11.07 21.17,11.13 21.26,11.29L22.' +
          '66,13.71C22.75,13.86 22.71,14.05 22.58,14.16L21.1,15.31L21.15,16' +
          'L21.1,16.68M6.69,8.07C7.56,8.07 8.26,7.37 8.26,6.5C8.26,5.63 7.5' +
          '6,4.92 6.69,4.92A1.58,1.58 0 0,0 5.11,6.5C5.11,7.37 5.82,8.07 6.' +
          '69,8.07M10.03,6.94L11,7.68C11.07,7.75 11.09,7.87 11.03,7.97L10.1' +
          '3,9.53C10.08,9.63 9.96,9.67 9.86,9.63L8.74,9.18L8,9.62L7.81,10.8' +
          '1C7.79,10.92 7.7,11 7.59,11H5.79C5.67,11 5.58,10.92 5.56,10.81L5' +
          '.4,9.62L4.64,9.18L3.5,9.63C3.41,9.67 3.3,9.63 3.24,9.53L2.34,7.9' +
          '7C2.28,7.87 2.31,7.75 2.39,7.68L3.34,6.94L3.31,6.5L3.34,6.06L2.3' +
          '9,5.32C2.31,5.25 2.28,5.13 2.34,5.03L3.24,3.47C3.3,3.37 3.41,3.3' +
          '3 3.5,3.37L4.63,3.82L5.4,3.38L5.56,2.19C5.58,2.08 5.67,2 5.79,2H' +
          '7.59C7.7,2 7.79,2.08 7.81,2.19L8,3.38L8.74,3.82L9.86,3.37C9.96,3' +
          '.33 10.08,3.37 10.13,3.47L11.03,5.03C11.09,5.13 11.07,5.25 11,5.' +
          '32L10.03,6.06L10.06,6.5L10.03,6.94Z" />'#13#10'</svg>'
      end
      item
        IconName = 'SpecialChars'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M27.8,3.3H12.1c-4.3,0-7.8,3.' +
          '5-7.8,7.8s3.5,7.8,7.8,7.8v9.8h3.9V7.2h3.9v21.5h3.9V7.2h3.9L27.8,' +
          '3.3z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'SpellCheck'
        SVGText = 
          '<svg viewBox="0 0 24 24"><path d="M21.59,11.59L13.5,19.68L9.83,1' +
          '6L8.42,17.41L13.5,22.5L23,13M6.43,11L8.5,5.5L10.57,11M12.45,16H1' +
          '4.54L9.43,3H7.57L2.46,16H4.55L5.67,13H11.31L12.45,16Z" /></svg>'
      end
      item
        IconName = 'SplitHorizontal'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M26.2,6.3L26.2,6.3l-0.1-1.1H' +
          '5.9v1.1l-2,2v16.6l2,2h20.1l2-2V8.3L26.2,6.3z M25.2,23.9H6.9v-5.8' +
          'h18.2V23.9z M25.2,15.1H6.9'#13#10#9'V9.3h18.2V15.1z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'SplitVertical'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M27.4,8V6.1h-9.7V8l-1.2,1.2L' +
          '15.3,8V6.1H5.6V8L4.4,9.3v15.6l2,2h8.1l2-2l2,2h8.1l2-2V9.3L27.4,8' +
          'z M13.5,23.8H7.3V10.1h6.2'#13#10#9'V23.8z M25.7,23.8h-6.2V10.1h6.2V23.8' +
          'z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'StepIn'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-254.935 -437' +
          '.323)">'#13#10#9'<path d="M269.1,439.1v10.2h-6.4l8.2,8.2l8.2-8.2h-6.4v-' +
          '10.2H269.1z"/>'#13#10#9'<circle cx="270.9" cy="463.6" r="4"/>'#13#10'</g>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'StepOut'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-254.935 -437' +
          '.323)">'#13#10#9'<path d="M272.7,467.6v-10.2h6.4l-8.2-8.2l-8.2,8.2h6.4v' +
          '10.2H272.7z"/>'#13#10#9'<circle cx="270.9" cy="443.1" r="4"/>'#13#10'</g>'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'StepOver'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-515.151 -51.' +
          '247)">'#13#10#9'<g transform="translate(768.215 145.747) rotate(90)">'#13#10 +
          #9#9'<path d="M-76,228.1v-5.3l8.6,9.3l-8.6,9.3V236c-6.2,0-10.5,2.1-' +
          '13.6,6.8C-88.3,236-84.6,229.4-76,228.1z"/>'#13#10#9'</g>'#13#10#9'<circle cx="' +
          '520.6" cy="74.4" r="3.7"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Stop'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M16,1C7.7,1,1,7.8,1,16s6.8,' +
          '15,15,15s15-6.8,15-15S24.4,1,16,1z M16,28C9.4,28,4,22.6,4,16S9.4' +
          ',4,16,4s12,5.4,12,12'#13#10#9#9'S22.6,28,16,28z M21.4,8.5L16,13.9l-5.4-5' +
          '.4l-2.1,2.1l5.4,5.4l-5.4,5.4l2.1,2.1l5.4-5.4l5.4,5.4l2.1-2.1L18.' +
          '1,16l5.4-5.4L21.4,8.5z'#13#10#9#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Styles'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M28.3,3.8H3.7C2,3.8,0.6,5.1' +
          ',0.6,6.9v18.3c0,1.7,1.4,3.1,3.1,3.1h24.6c1.7,0,3.1-1.4,3.1-3.1V6' +
          '.8C31.4,5,30,3.8,28.3,3.8z'#13#10#9#9#9'M28.3,25.1H3.7V6.8h24.6V25.1z"/>'#13 +
          #10#9'<rect fill="#4488FF" x="6.7" y="17.5" width="4.3" height="4.4"' +
          '/>'#13#10#9'<rect x="6.7" y="10.2" width="4.3" height="4.4"/>'#13#10#9'<rect x' +
          '="13.8" y="17.5" width="4.3" height="4.4"/>'#13#10#9'<rect fill="#FFCE0' +
          '0" x="13.8" y="10.2" width="4.3" height="4.4"/>'#13#10#9'<rect fill="#E' +
          '24444" x="21" y="17.5" width="4.3" height="4.4"/>'#13#10#9'<rect x="21"' +
          ' y="10.2" width="4.3" height="4.4"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'TabClose'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<rect fill="#E24444" x="18" y="1" wi' +
          'dth="13" height="4"/>'#13#10#9'<g transform="translate(-1820 -863.725)"' +
          '>'#13#10#9#9'<path d="M1845.2,869.7h-10.4v-2.3l-1-1h-11.7l-1,1v4.5c0,0.1' +
          ',0,0.2,0,0.3v7.3l0,0v12c0,1,0.8,1.6,1.6,1.6'#13#10#9#9#9'h26.7c1,0,1.6-0.' +
          '8,1.6-1.6l0,0v-16.9L1845.2,869.7z M1847.5,889.8h-23.4v-15.9h20l3' +
          '.4,2.9V889.8z"/>'#13#10#9'</g>'#13#10'</svg>'
      end
      item
        IconName = 'TabNext'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-1820 -863.72' +
          '5)">'#13#10#9'<path d="M1845.2,869.7h-10.4v-2.3l-1-1h-11.7l-1,1v4.5c0,0' +
          '.1,0,0.2,0,0.3v7.3l0,0v12c0,1,0.8,1.6,1.6,1.6'#13#10#9#9'h26.7c1,0,1.6-0' +
          '.8,1.6-1.6l0,0v-16.9L1845.2,869.7z M1847.5,889.8h-23.4v-15.9h20l' +
          '3.4,2.9V889.8z"/>'#13#10'</g>'#13#10'<polygon points="21,15 18.2,12.2 16.1,1' +
          '4.3 18.4,16.6 12.2,16.6 12.2,19.7 18.4,19.7 16.1,22 18.2,24.2 24' +
          '.1,18.2 23.6,17.6 '#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'TabPrevious'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="translate(-1820 -863.7' +
          '25)">'#13#10#9#9'<path id="Trazado_1760_2_" d="M1845.2,869.7h-10.4v-2.3l' +
          '-1-1h-11.7l-1,1v4.5c0,0.1,0,0.2,0,0.3v7.3l0,0v12c0,1,0.8,1.6,1.6' +
          ',1.6'#13#10#9#9#9'h26.7c1,0,1.6-0.8,1.6-1.6l0,0v-16.9L1845.2,869.7z M1847' +
          '.4,889.8H1824v-15.9h20l3.4,2.9V889.8z"/>'#13#10#9'</g>'#13#10#9'<polygon point' +
          's="10.7,21.4 13.4,24.2 15.6,22 13.2,19.7 19.3,19.7 19.3,16.6 13.' +
          '2,16.6 15.6,14.3 13.4,12.2 7.4,18.1 8.1,18.7 '#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Tabs'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M26.3,12.4h-2.6v-4l-4.3-3.6h' +
          '-7.7V3.1l-0.8-0.8H2.2L1.4,3.1V21c0,0.7,0.6,1.2,1.2,1.2h5.7v6.3c0' +
          ',0.7,0.6,1.2,1.2,1.2h19.8'#13#10#9'c0.7,0,1.2-0.6,1.2-1.2V15.9L26.3,12.' +
          '4z M3.8,19.8V8h14.8l2.5,2.1v2.3h-2.6v-1.7l-0.8-0.8H9l-0.8,0.8v9.' +
          '1H3.8z M28,27.4H10.6v-5.1'#13#10#9'v-2.4v-4.4h10.5h2.6h1.8l2.5,2.1V27.4' +
          'z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'TabsClose'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<rect fill="#E24444" x="18" y="1" wi' +
          'dth="13" height="4"/>'#13#10#9'<path d="M26.2,13h-2.6V9l-4.3-3.6h-7.7V3' +
          '.7l-0.8-0.8H2.1L1.3,3.7v17.9c0,0.7,0.6,1.2,1.2,1.2h5.7v6.3c0,0.7' +
          ',0.6,1.2,1.2,1.2h19.8'#13#10#9#9'c0.7,0,1.2-0.6,1.2-1.2V16.5L26.2,13z M3' +
          '.8,20.4V8.6h14.8l2.5,2.1V13h-2.6v-1.7l-0.8-0.8H9l-0.8,0.8v9.1H3.' +
          '8z M28,28H10.6v-5.1'#13#10#9#9'v-2.4v-4.4h10.5h2.6h1.8l2.5,2.1L28,28L28,' +
          '28z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'TestsFailed'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10#9'<polygon fill="#E24444" points="15.3' +
          ',4.8 13.8,6.3 16,8.6 13.8,10.8 15.3,12.3 17.6,10.1 19.8,12.3 21.' +
          '3,10.8 19.1,8.6 21.3,6.3 19.8,4.8 17.6,7 "/>'#13#10#9'<rect x="4.5" y="' +
          '7.5" width="7.5" height="3"/>'#13#10#9'<polygon fill="#E24444" points="' +
          '15.3,12.7 13.8,14.3 16,16.5 13.8,18.7 15.3,20.3 17.6,18.1 19.8,2' +
          '0.3 21.3,18.7 19.1,16.5 21.3,14.3 19.8,12.7 17.6,14.9 "/>'#13#10#9'<rec' +
          't x="4.5" y="15" width="7.5" height="3"/>'#13#10'</svg>'
      end
      item
        IconName = 'ThreadPaused'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M26.3,29.5h-6.8c-0.8,0-1.5-' +
          '0.7-1.5-1.5V4c0-0.8,0.7-1.5,1.5-1.5h6.8c0.8,0,1.5,0.7,1.5,1.5V28' +
          #13#10#9#9'C27.8,28.8,27.1,29.5,26.3,29.5z M20.9,26.5h3.8V5.5h-3.8V26.5' +
          'z M12.6,29.5H5.7c-0.8,0-1.5-0.7-1.5-1.5V4c0-0.8,0.7-1.5,1.5-1.5'#13 +
          #10#9#9'h6.8c0.8,0,1.5,0.7,1.5,1.5V28C14.1,28.8,13.4,29.5,12.6,29.5z ' +
          'M7.2,26.5h3.8V5.5H7.2V26.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ThreadRunning'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M4,16C4,9.4,9.4,4,16,4s12,5' +
          '.4,12,12s-5.4,12-12,12S4,22.7,4,16 M1,16c0,8.3,6.7,15,15,15s15-6' +
          '.7,15-15S24.3,1,16,1'#13#10#9#9'S1,7.8,1,16L1,16z"/>'#13#10#9'<path d="M14,12.2' +
          'l6.1,3.9L14,19.9V12.2 M11.7,8v16.1L24.4,16L11.7,8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'TodoWin'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'    <path fill="#FFCE00" transform="t' +
          'ranslate(4 6)" d="M9,20.42L2.79,14.21L5.62,11.38L9,14.77L18.88,4' +
          '.88L21.71,7.71L9,20.42Z" />'#9#13#10#9'<g transform="translate(-180.6 -3' +
          '25.9)">'#13#10#9#9'<path d="M206.7,329.2h-6.1c-0.8-2.2-3.3-3.4-5.5-2.5c-' +
          '1.2,0.5-2.2,1.4-2.5,2.5h-6.1c-1.6,0-2.9,1.2-2.9,2.8'#13#10#9#9#9'v22.7c0,' +
          '1.6,1.3,2.8,2.9,2.8h20.2c1.6,0,2.9-1.2,2.9-2.8V332C209.5,330.4,2' +
          '08.2,329.2,206.7,329.2z M196.6,329.2'#13#10#9#9#9'c0.7,0,1.4,0.7,1.4,1.4c' +
          '0,0.7-0.7,1.4-1.4,1.4s-1.4-0.7-1.4-1.4C195.2,329.8,195.9,329.2,1' +
          '96.6,329.2z M206.7,354.6h-20.2V332h2.9'#13#10#9#9#9'v4.2h14.4V332h2.9V354' +
          '.6z"/>'#13#10#9'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Lock'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10#9'<path'#13#10#9#9'd="M12,17C10.89,17 10,16.1 ' +
          '10,15C10,13.89 10.89,13 12,13A2,2 0 0,1 14,15A2,2 0 0,1 12,17M18' +
          ',20V10H6V20H18M18,8A2,2 0 0,1 20,10V20A2,2 0 0,1 18,22H6C4.89,22' +
          ' 4,21.1 4,20V10C4,8.89 4.89,8 6,8H7V6A5,5 0 0,1 12,1A5,5 0 0,1 1' +
          '7,6V8H18M12,3A3,3 0 0,0 9,6V8H15V6A3,3 0 0,0 12,3Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Tools'
        SVGText = 
          '<svg viewBox="0 0 512 512">'#13#10#9'<path d="m289.648 159.19c-3.136 3.' +
          '236-86.784 70.442-89.836 72.36l31.519 39.064 90.026-72.35z" />'#13#10 +
          #9'<path d="m498.989 417.204-158.733-195.642-90.096 72.4 158.423 1' +
          '96.372c20.142 24.99 56.738 28.99 81.816 8.719 25.074-20.282 28.8' +
          '27-56.906 8.59-81.849z" />'#13#10#9'<path d="m269.066 118.356-18.665-23' +
          '.022c34.142-25.443 57.552-32.168 108.839-34.007 6.665-.239 12.37' +
          '1-4.848 14.005-11.313 1.634-6.464-1.196-13.232-6.947-16.608-78.1' +
          '72-45.903-151.412-44.477-211.803 4.126l-63.725 51.29c-6.473 5.20' +
          '9-7.476 14.699-2.232 21.146 5.128 6.306 4.307 15.647-2.281 20.94' +
          '9-6.453 5.194-15.937 4.189-21.14-2.242-5.2-6.426-14.618-7.434-21' +
          '.059-2.257l-27.292 21.936c-19.372 15.59-22.413 43.867-6.737 63.2' +
          '04l37.74 46.549c15.605 19.248 43.997 22.237 63.279 6.719l27.284-' +
          '21.93c6.47-5.201 7.485-14.681 2.254-21.134-5.202-6.416-4.188-15.' +
          '771 2.243-20.947 6.461-5.199 15.957-4.197 21.173 2.235 5.204 6.4' +
          '19 14.618 7.418 21.057 2.239l81.76-65.802c6.466-5.205 7.475-14.6' +
          '82 2.247-21.131z" />'#13#10'</svg>'
      end
      item
        IconName = 'ToolsSetup'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<rect x="22.5" y="17.5" transform="m' +
          'atrix(0.7071 -0.7071 0.7071 0.7071 -9.6262 24.476)" width="4.5" ' +
          'height="12.7"/>'#13#10#9'<path d="M24.3,13.2c2.9,0,5.3-2.4,5.3-5.3c0-0.' +
          '9-0.2-1.7-0.6-2.4l-4.1,4.1l-2.2-2.2l4.1-4.1c-0.7-0.4-1.5-0.6-2.4' +
          '-0.6'#13#10#9#9'c-2.9,0-5.3,2.4-5.3,5.3c0,0.6,0.1,1.2,0.3,1.7l-2.8,2.8l-' +
          '2.7-2.7L15,8.7l-2.1-2.1l3.2-3.2c-1.8-1.8-4.6-1.8-6.4,0L4.4,8.7'#13#10 +
          #9#9'l2.1,2.1H2.3l-1.1,1.1l5.3,5.3l1.1-1.1v-4.2L9.7,14l1.1-1.1l2.7,' +
          '2.7L2.4,26.7l3.2,3.2l17.1-17.1C23.1,13,23.6,13.2,24.3,13.2z"/>'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'TreeDeselectAll'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M9.1,12h1.7c0.1,0,0.1,0,0.2' +
          ',0h0.3v1.8h2.6V12h0.2c0.1,0,0.1,0,0.2,0h2.3c1,0,1.8-0.8,1.8-1.8V' +
          '2.7c0-1-0.8-1.8-1.8-1.8H9.1'#13#10#9#9'c-1,0-1.8,0.8-1.8,1.8v2.5v4.6v0.5' +
          'C7.3,11.2,8.1,12,9.1,12z M10,3.7h5.6v5.6H10V3.7z"/>'#13#10#9'<path d="M' +
          '16.9,20H9.4c-1,0-1.8,0.8-1.8,1.8v2.3c0,0,0,3.4,0,3.5v1.7c0,1,0.8' +
          ',1.8,1.8,1.8h7.5c1,0,1.8-0.8,1.8-1.8v-7.5'#13#10#9#9'C18.7,20.8,17.9,20,' +
          '16.9,20z M15.9,28.3h-5.6v-5.6h5.6V28.3z"/>'#13#10#9'<path d="M30,10.5h-' +
          '7.5c-1,0-1.8,0.8-1.8,1.8v2.6l0,0.7H20v2.6h0.8l0,1.3v0.3c0,1,0.8,' +
          '1.8,1.8,1.8h7.5c1,0,1.8-0.8,1.8-1.8v-7.5'#13#10#9#9'C31.7,11.3,30.9,10.5' +
          ',30,10.5z M29,18.8h-5.6v-5.6H29V18.8z"/>'#13#10#9'<rect x="0.1" y="11" ' +
          'width="2.6" height="2.6"/>'#13#10#9'<rect x="0.1" y="15.4" width="2.6" ' +
          'height="2.6"/>'#13#10#9'<rect x="0.1" y="6.6" width="2.6" height="2.6"/' +
          '>'#13#10#9'<rect x="0.1" y="24.2" width="2.6" height="2.6"/>'#13#10#9'<rect x=' +
          '"0.1" y="19.8" width="2.6" height="2.6"/>'#13#10#9'<rect x="0.1" y="2.2' +
          '" width="2.6" height="2.6"/>'#13#10#9'<rect x="4.5" y="6.6" width="2.6"' +
          ' height="2.6"/>'#13#10#9'<rect x="4.5" y="24.2" width="2.6" height="2.6' +
          '"/>'#13#10#9'<rect x="11.2" y="15.5" width="2.6" height="2.6"/>'#13#10#9'<rect' +
          ' x="15.6" y="15.5" width="2.6" height="2.6"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'TreeSelectAll'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<rect x="7.5" y="1" width="10" heigh' +
          't="10" rx="1"/>'#13#10#9'<rect x="7.5" y="20.5" width="10" height="10" ' +
          'rx="1"/>'#13#10#9'<rect x="21" y="12" width="10" height="10" rx="1"/>'#13#10 +
          #13#10#9'<rect x="0.1" y="11" width="2.6" height="2.6"/>'#13#10#9'<rect x="0.' +
          '1" y="15.4" width="2.6" height="2.6"/>'#13#10#9'<rect x="0.1" y="6.6" w' +
          'idth="2.6" height="2.6"/>'#13#10#9'<rect x="0.1" y="24.2" width="2.6" h' +
          'eight="2.6"/>'#13#10#9'<rect x="0.1" y="19.8" width="2.6" height="2.6"/' +
          '>'#13#10#9'<rect x="0.1" y="2.2" width="2.6" height="2.6"/>'#13#10#9'<rect x="' +
          '4.5" y="6.6" width="2.6" height="2.6"/>'#13#10#9'<rect x="4.5" y="24.2"' +
          ' width="2.6" height="2.6"/>'#13#10#9'<rect x="11.2" y="11" width="2.6" ' +
          'height="2.6"/>'#13#10#9'<rect x="11.2" y="15.5" width="2.6" height="2.6' +
          '"/>'#13#10#9'<rect x="15.6" y="15.5" width="2.6" height="2.6"/>'#13#10#9'<rect' +
          ' x="20" y="15.5" width="2.6" height="2.6"/>'#13#10'</svg>'
      end
      item
        IconName = 'UnCodeComment'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M3.7,14.8H0v2h4V20l2-2v-0.9' +
          'L3.7,14.8z M11.9,8.9v-4h-2v4h-4v-4H4v2.5l3.5,3.5h2.5v2.5l0.4,0.4' +
          'l1.6-1.6v-1.2h1.2'#13#10#9#9'l1.3-1.3l0.2-0.1L15.9,9L11.9,8.9L11.9,8.9z"' +
          '/>'#13#10#9'<path d="M32,10.9v-2h-4v-4h-2v4h-4v-4h-2v2L22,6l-2,3.9l-0.5' +
          ',1.1H20v4h-3.2L16,15.7v1.2h4v4h2v-4h4v4h2v-4h4v-2h-4v-4L32,10.9'#13 +
          #10#9#9'L32,10.9z M26.1,14.8h-4v-4h4V14.8z"/>'#13#10#9'<polygon fill="#E2444' +
          '4" points="20,27.1 17.4,26.5 10.3,19.4 9.9,19.7 3.2,26.5 1.4,24.' +
          '6 5.3,20.8 5.9,20.1 8.5,17.6 0,9.1 0,8.9 0.2,8.9 '#13#10#9#9'1.8,7.3 3.4' +
          ',8.9 10.3,15.7 11.9,14.1 15.2,10.9 15.2,10.9 15.9,10.6 16.1,10.4' +
          ' 18.7,9.2 17.9,10.9 17,12.5 14.9,14.8 12.9,16.8 '#13#10#9#9'12.1,17.6 19' +
          '.1,24.6 '#9#9#9'"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Undo'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(94.5 -231.7)"' +
          '>'#13#10#9'<path d="M-81.4,242.6v-5.8L-91.8,247l10.3,10.2v-6c7.4,0,12.5' +
          ',2.3,16.2,7.4C-66.6,251.3-71.1,244-81.4,242.6z"/>'#13#10'</g>'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = 'UnitTestWin'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M 12 0 C 10.695 0 9.5838' +
          '75 0.835 9.171875 2 L 5 2 C 3.91 2 3 2.911 3 4 L 3 20 C 3 21.089' +
          ' 3.911 22 5 22 L 19 22 C 20.09 22 21 21.089 21 20 L 21 3.9726562' +
          ' C 21 2.8826562 20.09 2 19 2 L 14.828125 2 C 14.416125 0.835 13.' +
          '305 0 12 0 z M 12 2 C 12.552 2 13 2.448 13 3 C 13 3.552 12.552 4' +
          ' 12 4 C 11.448 4 11 3.552 11 3 C 11 2.448 11.448 2 12 2 z M 5 4 ' +
          'L 7 4 L 7 7 L 17 7 L 17 4 L 19 4 L 19 20 L 5 20 L 5 4 z M 7 10 L' +
          ' 7 12 L 12 12 L 12 10 L 7 10 z z M 7 15 L 7 17 L 12 17 L 12 15 L' +
          ' 7 15 z"/>'#13#10'    <path fill="#22AA22" d=" M 17.091797 8.8613281 L' +
          ' 14.970703 11.017578 L 13.908203 9.9570312 L 12.847656 11.017578' +
          ' L 14.970703 13.138672 L 18.152344 9.921875 L 17.091797 8.861328' +
          '1 z" />'#13#10'    <path  fill="#E24444" d=" M 14.25 13.470703 L 13.18' +
          '9453 14.529297 L 14.658203 16 L 13.189453 17.470703 L 14.25 18.5' +
          '29297 L 15.720703 17.060547 L 17.189453 18.529297 L 18.25 17.470' +
          '703 L 16.78125 16 L 18.25 14.529297 L 17.189453 13.470703 L 15.7' +
          '20703 14.939453 L 14.25 13.470703 z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Up'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M15,20H9V12H4.16L12,4.16' +
          'L19.84,12H15V20Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Upload'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M25.8,13.4c-0.9-4.6-5-8.1-9' +
          '.8-8.1c-3.8,0-7.2,2.2-8.9,5.4c-4,0.4-7.1,3.8-7.1,7.9c0,4.4,3.5,8' +
          ',8,8h17.3'#13#10#9#9'c3.6,0,6.7-3,6.7-6.7C32,16.4,29.2,13.6,25.8,13.4z M' +
          '25.3,24H8c-2.9,0-5.3-2.4-5.3-5.3c0-2.8,2-5,4.7-5.3l1.4-0.2l0.7-1' +
          '.2'#13#10#9#9'c1.2-2.4,3.7-4,6.5-4c3.5,0,6.5,2.5,7.2,5.9l0.4,2l2,0.2c2,0' +
          '.1,3.7,1.9,3.7,3.9C29.3,22.2,27.5,24,25.3,24z M10.7,17.3H14v4H18' +
          'v-4'#13#10#9#9'h3.4L16,12L10.7,17.3z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'VariablesWin'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<g transform="translate(1.44)">'#13#10#9#9'<' +
          'path fill="#E24444" d="M22.1,22.3l-3.2,3.2l-2.4,2.4l-0.6,0.6h-0.' +
          '3l-3-3l-0.4-0.4c-0.1-0.1-0.1-0.1,0-0.3l6.2-6.2h0.3l3.4,3.4'#13#10#9#9#9'C' +
          '22.2,22.2,22.2,22.3,22.1,22.3z"/>'#13#10#9#9'<path fill="#4488FF" d="M20' +
          '.8,10.1l-7.1,7.1c-0.1,0.1-0.3,0.1-0.4,0l-3.5-3.5c-0.1-0.1-0.1-0.' +
          '2,0-0.4l7.1-7.1h0.4l3.5,3.5'#13#10#9#9#9'C20.9,9.8,20.9,10,20.8,10.1z"/>'#13 +
          #10#9#9'<path fill="#FFCE00" d="M10.6,20.2l-7.1,7.1l-0.2,0.1l-3.6-3.6' +
          'c-0.1-0.1-0.1-0.2,0-0.3l1.9-2l5.2-5.1h0.3l3.4,3.4'#13#10#9#9#9'C10.7,19.8' +
          ',10.7,20.1,10.6,20.2z"/>'#13#10#9'</g>'#13#10#9'<path d="M28.6,3.7H3.4c-1.5,0-' +
          '2.7,1.2-2.7,2.7V21l2.4-2.4v-12c0-0.1,0.1-0.5,0.2-0.38V6.2h12l1.6' +
          '-1.6c1-1,2.5-1,3.5,0L22,6.2h6.5'#13#10#9#9'c0.1,0,0.2,0.1,0.2,0.2v18.9c0' +
          ',0.1-0.1,0.2-0.2,0.2h-5l-2.4,2.4h7.4c1.4,0,2.6-1.2,2.7-2.7V6.4C3' +
          '1.3,4.9,30.1,3.7,28.6,3.7z'#13#10#9#9'M10.8,25.5H10l-2.4,2.4h5.6L10.8,25' +
          '.5z"/>'#13#10'</svg>'
      end
      item
        IconName = 'ViewAny'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M25.5,31.8H6.5c-1.7,0-3-1.3' +
          '-3-3V3.2c0-1.7,1.3-3,3-3h19.1c1.7,0,3,1.3,3,3v25.6C28.5,30.4,27.' +
          '1,31.8,25.5,31.8z M6.5,3'#13#10#9#9'C6.4,3,6.3,3.1,6.3,3.2v25.6c0,0.1,0.' +
          '1,0.2,0.2,0.2h19.1c0.1,0,0.2-0.1,0.2-0.2V3.2c0-0.1-0.1-0.2-0.2-0' +
          '.2H6.5z"/>'#13#10#9'<rect x="9.1" y="5.9" width="13.8" height="2.8"/>'#13#10 +
          #9'<rect x="11.4" y="11.6" width="9.2" height="2.8"/>'#13#10#9'<rect x="9' +
          '.1" y="17.4" width="13.8" height="2.8"/>'#13#10#9'<rect x="11.7" y="23"' +
          ' width="8.6" height="2.8"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ViewStart'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M25.6,31.9H6.4c-1.7,0-3-1.3' +
          '-3-3V3.1c0-1.7,1.3-3,3-3h19.2c1.7,0,3,1.3,3,3v25.8C28.6,30.5,27.' +
          '3,31.9,25.6,31.9z M6.5,2.9'#13#10#9#9'C6.4,2.9,6.3,3,6.3,3.1v25.8c0,0.1,' +
          '0.1,0.2,0.2,0.2h19.2c0.1,0,0.2-0.1,0.2-0.2V3.1c0-0.1-0.1-0.2-0.2' +
          '-0.2H6.5z"/>'#13#10#9'<rect x="9.2" y="5.8" width="13.9" height="2.9"/>' +
          #13#10#9'<rect x="9.2" y="11.6" width="9.5" height="2.9"/>'#13#10#9'<rect x="' +
          '9.2" y="17.4" width="7.2" height="2.9"/>'#13#10#9'<rect x="9.2" y="23.1' +
          '" width="8.7" height="2.9"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Watch'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M3,10C2.76,10 2.55,10.09' +
          ' 2.41,10.25C2.27,10.4 2.21,10.62 2.24,10.86L2.74,13.85C2.82,14.5' +
          ' 3.4,15 4,15H7C7.64,15 8.36,14.44 8.5,13.82L9.56,10.63C9.6,10.5 ' +
          '9.57,10.31 9.5,10.19C9.39,10.07 9.22,10 9,10H3M7,17H4C2.38,17 0.' +
          '96,15.74 0.76,14.14L0.26,11.15C0.15,10.3 0.39,9.5 0.91,8.92C1.43' +
          ',8.34 2.19,8 3,8H9C9.83,8 10.58,8.35 11.06,8.96C11.17,9.11 11.27' +
          ',9.27 11.35,9.45C11.78,9.36 12.22,9.36 12.64,9.45C12.72,9.27 12.' +
          '82,9.11 12.94,8.96C13.41,8.35 14.16,8 15,8H21C21.81,8 22.57,8.34' +
          ' 23.09,8.92C23.6,9.5 23.84,10.3 23.74,11.11L23.23,14.18C23.04,15' +
          '.74 21.61,17 20,17H17C15.44,17 13.92,15.81 13.54,14.3L12.64,11.5' +
          '9C12.26,11.31 11.73,11.31 11.35,11.59L10.43,14.37C10.07,15.82 8.' +
          '56,17 7,17M15,10C14.78,10 14.61,10.07 14.5,10.19C14.42,10.31 14.' +
          '4,10.5 14.45,10.7L15.46,13.75C15.64,14.44 16.36,15 17,15H20C20.5' +
          '9,15 21.18,14.5 21.25,13.89L21.76,10.82C21.79,10.62 21.73,10.4 2' +
          '1.59,10.25C21.45,10.09 21.24,10 21,10H15Z" />'#13#10'</svg>'
      end
      item
        IconName = 'WatchesWin'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#4488FF" d="M26.6,15.4L2' +
          '6.6,15.4l-4.7-6.9l-1.5,1l2.6,3.9c-0.2,0-0.4-0.1-0.6-0.1c-2,0-3.7' +
          ',1.2-4.5,2.9'#13#10#9#9'c-0.6-0.4-1.3-0.6-2-0.6s-1.4,0.2-2,0.6c-0.8-1.7-' +
          '2.5-2.9-4.5-2.9c-0.2,0-0.3,0-0.5,0.1l2.6-3.9l-1.5-1l-4.6,6.7'#13#10#9#9 +
          'c-0.7,0.9-1.1,2-1.1,3.1c0,2.8,2.3,5.1,5.1,5.1s5.1-2.3,5.1-5.1c0-' +
          '0.1,0-0.2,0-0.2c0.3-0.5,0.9-0.8,1.5-0.8s1.2,0.3,1.5,0.8'#13#10#9#9'c0,0.' +
          '1,0,0.1,0,0.2c0,2.8,2.3,5.1,5.1,5.1s5.1-2.3,5.1-5.1C27.6,17.5,27' +
          '.2,16.5,26.6,15.4z M9.4,21.2c-1.4,0-2.6-1.1-2.6-2.6'#13#10#9#9's1.1-2.6,' +
          '2.6-2.6S12,17,12,18.5S10.8,21.2,9.4,21.2z M22.5,21.2c-1.4,0-2.6-' +
          '1.1-2.6-2.6s1.1-2.6,2.6-2.6s2.6,1.1,2.6,2.6'#13#10#9#9'S23.9,21.2,22.5,2' +
          '1.2z"/>'#13#10#9'<path d="M28.6,28.1H3.4c-1.5,0-2.7-1.2-2.7-2.7V6.6c0-1' +
          '.5,1.2-2.7,2.7-2.7h25.2c1.5,0,2.7,1.2,2.7,2.7v18.8'#13#10#9#9'C31.2,26.9' +
          ',30,28.1,28.6,28.1z M3.3,6.5c-0.1,0-0.2,0.1-0.2,0.2v18.8c0,0.1,0' +
          '.1,0.2,0.2,0.2h25.2c0.1,0,0.2-0.1,0.2-0.2V6.6'#13#10#9#9'c0-0.1-0.1-0.2-' +
          '0.2-0.2H3.3V6.5z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Web'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M16.36,14C16.44,13.34 16.' +
          '5,12.68 16.5,12C16.5,11.32 16.44,10.66 16.36,10H19.74C19.9,10.64' +
          ' 20,11.31 20,12C20,12.69 19.9,13.36 19.74,14M14.59,19.56C15.19,1' +
          '8.45 15.65,17.25 15.97,16H18.92C17.96,17.65 16.43,18.93 14.59,19' +
          '.56M14.34,14H9.66C9.56,13.34 9.5,12.68 9.5,12C9.5,11.32 9.56,10.' +
          '65 9.66,10H14.34C14.43,10.65 14.5,11.32 14.5,12C14.5,12.68 14.43' +
          ',13.34 14.34,14M12,19.96C11.17,18.76 10.5,17.43 10.09,16H13.91C1' +
          '3.5,17.43 12.83,18.76 12,19.96M8,8H5.08C6.03,6.34 7.57,5.06 9.4,' +
          '4.44C8.8,5.55 8.35,6.75 8,8M5.08,16H8C8.35,17.25 8.8,18.45 9.4,1' +
          '9.56C7.57,18.93 6.03,17.65 5.08,16M4.26,14C4.1,13.36 4,12.69 4,1' +
          '2C4,11.31 4.1,10.64 4.26,10H7.64C7.56,10.66 7.5,11.32 7.5,12C7.5' +
          ',12.68 7.56,13.34 7.64,14M12,4.03C12.83,5.23 13.5,6.57 13.91,8H1' +
          '0.09C10.5,6.57 11.17,5.23 12,4.03M18.92,8H15.97C15.65,6.75 15.19' +
          ',5.55 14.59,4.44C16.43,5.07 17.96,6.34 18.92,8M12,2C6.47,2 2,6.5' +
          ' 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2Z" />'#13 +
          #10'</svg>'
      end
      item
        IconName = 'WordWrap'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M21,5H3V7H21V5M3,19H10V1' +
          '7H3V19M3,13H18C19,13 20,13.43 20,15C20,16.57 19,17 18,17H16V15L1' +
          '2,18L16,21V19H18C20.95,19 22,17.73 22,15C22,12.28 21,11 18,11H3V' +
          '13Z" />'#13#10'</svg>'
      end
      item
        IconName = 'ZoomIn'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M21.7,19.3h-1.2l-0.4-0.4c1.' +
          '5-1.7,2.4-3.9,2.4-6.3c0-5.4-4.4-9.8-9.8-9.8s-9.8,4.4-9.8,9.8s4.4' +
          ',9.8,9.8,9.8'#13#10#9#9'c2.4,0,4.6-0.9,6.3-2.4l0.4,0.4v1.2l7.5,7.5l2.2-2' +
          '.2L21.7,19.3z M12.7,19.3c-3.7,0-6.8-3-6.8-6.8s3-6.8,6.8-6.8'#13#10#9#9'c' +
          '3.7,0,6.8,3,6.8,6.8S16.5,19.3,12.7,19.3z M13.5,8.8H12v3H9v1.5h3v' +
          '3h1.5v-3h3v-1.5h-3V8.8z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ZoomOut'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<path d="M21.7,19.3h-1.2l-0.4-0.4c1.5' +
          '-1.7,2.4-3.9,2.4-6.3c0-5.4-4.4-9.8-9.8-9.8s-9.8,4.4-9.8,9.8s4.4,' +
          '9.8,9.8,9.8'#13#10#9'c2.4,0,4.6-0.9,6.3-2.4l0.4,0.4v1.2l7.5,7.5l2.2-2.2' +
          'L21.7,19.3z M12.7,19.3c-3.7,0-6.8-3-6.8-6.8s3-6.8,6.8-6.8c3.7,0,' +
          '6.8,3,6.8,6.8'#13#10#9'S16.4,19.3,12.7,19.3z M9,11.8h7.5v1.5H9V11.8z"/>' +
          #13#10'</svg>'
      end
      item
        IconName = 'ZoomReset'
        SVGText = 
          '<svg viewBox="0 0 32 32" stroke="black">'#13#10#9'<path stroke-width="0' +
          '" d="M20.1,19l-0.4-0.4l-1.1,1.1'#13#10#9'l0.4,0.4v1.2l7.5,7.5l2.2-2.2L2' +
          '1.7,19.3z" />'#13#10#9'<path transform="translate(2.2,2.2)" fill="none"' +
          ' stroke-width="2.7" d="M17.502 7.001C16.273 4.41 13.596 2.625 10' +
          '.5 2.625C6.15 2.625 2.625 6.15 2.625 10.5C2.625 14.849 6.15 18.3' +
          '75 10.5 18.375L10.5 18.375C14.849 18.375 18.375 14.849 18.375 10' +
          '.5M18.375 2.625L18.375 7.875L13.125 7.875"/>'#13#10'</svg>'
      end>
    ApplyFixedColorToRootOnly = True
    Left = 24
    Top = 152
  end
  object SynWebEngine: TSynWebEngine
    Options.HtmlVersion = shvHtml5
    Left = 496
    Top = 216
  end
  object SynWebEsSyn: TSynWebEsSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Left = 684
    Top = 152
  end
  object SynWebPhpPlainSyn: TSynWebPhpPlainSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Left = 587
    Top = 152
  end
  object SynWebCssSyn: TSynWebCssSyn
    Brackets = '()[]{}<>'
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Options.HtmlVersion = shvHtml401Transitional
    Left = 496
    Top = 152
  end
  object SynWebXmlSyn: TSynWebXmlSyn
    Brackets = '()[]{}<>'
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Left = 684
    Top = 88
  end
  object SynYAMLSyn: TSynYAMLSyn
    Left = 587
    Top = 216
  end
  object SynWebHtmlSyn: TSynWebHtmlSyn
    Brackets = '()[]{}<>'
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Options.HtmlVersion = shvHtml5
    Options.UseEngineOptions = True
    Left = 587
    Top = 88
  end
  object SynIniSyn: TSynIniSyn
    Left = 496
    Top = 88
  end
  object SynCppSyn: TSynCppSyn
    Left = 684
    Top = 24
  end
  object SynJSONSyn: TSynJSONSyn
    Left = 587
    Top = 24
  end
  object SynGeneralSyn: TSynGeneralSyn
    DefaultFilter = 'Text Files(*.txt,*.*)|*.txt;*.*'
    DetectPreprocessor = False
    SpaceAttri.Foreground = clSilver
    Left = 496
    Top = 24
  end
end
