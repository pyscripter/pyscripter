object CommandsDataModule: TCommandsDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 392
  Width = 675
  object SynPythonSyn: TSynPythonSyn
    DefaultFilter = 'Python Files (*.py;*.pyw)|*.py;*.pyw'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    IdentifierAttri.Foreground = clBlack
    KeyAttri.Foreground = clNavy
    NonKeyAttri.Style = []
    NumberAttri.Foreground = clTeal
    HexAttri.Foreground = clTeal
    OctalAttri.Foreground = clTeal
    FloatAttri.Foreground = clTeal
    SpaceAttri.Background = clWhite
    SpaceAttri.Foreground = clSilver
    StringAttri.Foreground = clOlive
    DocStringAttri.Foreground = 16711884
    SymbolAttri.Foreground = clMaroon
    Left = 616
    Top = 212
  end
  object SynEditPrint: TSynEditPrint
    Copies = 1
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Arial'
    Header.DefaultFont.Style = []
    Footer.DefaultFont.Charset = DEFAULT_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Arial'
    Footer.DefaultFont.Style = []
    Margins.Left = 25.000000000000000000
    Margins.Right = 15.000000000000000000
    Margins.Top = 25.000000000000000000
    Margins.Bottom = 25.000000000000000000
    Margins.Header = 18.000000000000000000
    Margins.Footer = 18.000000000000000000
    Margins.LeftHFTextIndent = 2.000000000000000000
    Margins.RightHFTextIndent = 2.000000000000000000
    Margins.HFInternalMargin = 0.500000000000000000
    Margins.MirrorMargins = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Highlighter = SynPythonSyn
    TabWidth = 8
    Color = clWhite
    Left = 188
    Top = 68
  end
  object PrintDialog: TPrintDialog
    MaxPage = 10000
    Options = [poPageNums]
    Left = 187
    Top = 22
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 100
    Top = 68
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
    Columns = <
      item
        ColumnWidth = 120
        DefaultFontStyle = [fsBold]
      end
      item
        ColumnWidth = 230
      end>
    Resizeable = False
    OnExecute = ParameterCompletionExecute
    ShortCut = 24656
    OnCodeCompletion = ParameterCompletionCodeCompletion
    Left = 335
    Top = 12
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
    Columns = <
      item
        ColumnWidth = 120
        DefaultFontStyle = [fsBold]
      end
      item
        ColumnWidth = 280
      end>
    Resizeable = False
    OnExecute = ModifierCompletionExecute
    ShortCut = 24653
    OnCodeCompletion = ModifierCompletionCodeCompletion
    Left = 338
    Top = 58
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
    Left = 340
    Top = 108
  end
  object ShellImages: TImageList
    DrawingStyle = dsTransparent
    ShareImages = True
    Left = 91
    Top = 202
  end
  object SynEditSearch: TSynEditSearch
    Left = 195
    Top = 123
  end
  object SynEditRegexSearch: TSynEditRegexSearch
    Left = 105
    Top = 124
  end
  object ProgramVersionCheck: TJvProgramVersionCheck
    AllowedReleaseType = prtAlpha
    AppStoragePath = 'Check for Updates'
    CheckFrequency = 0
    LocalDirectory = 'Updates'
    LocalVersionInfoFileName = 'versioninfo.ini'
    LocationHTTP = ProgramVersionHTTPLocation
    LocationType = pvltHTTP
    UserOptions = [uoCheckFrequency, uoLocalDirectory, uoAllowedReleaseType, uoLocationHTTP]
    VersionHistoryFileOptions.INIOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    VersionHistoryFileOptions.INIOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    VersionHistoryFileOptions.INIOptions.SetAsString = True
    VersionHistoryFileOptions.INIOptions.FloatAsString = True
    VersionHistoryFileOptions.INIOptions.DefaultIfReadConvertError = True
    VersionHistoryFileOptions.XMLOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    VersionHistoryFileOptions.XMLOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    VersionHistoryFileOptions.XMLOptions.SetAsString = True
    VersionHistoryFileOptions.XMLOptions.FloatAsString = True
    VersionHistoryFileOptions.XMLOptions.DefaultIfReadConvertError = True
    VersionHistoryFileOptions.XMLOptions.UseOldItemNameFormat = False
    VersionHistoryFileOptions.XMLOptions.WhiteSpaceReplacement = '_'
    VersionHistoryFileOptions.XMLOptions.InvalidCharReplacement = '_'
    Left = 491
    Top = 23
  end
  object ProgramVersionHTTPLocation: TJvProgramVersionHTTPLocation
    OnLoadFileFromRemote = ProgramVersionHTTPLocationLoadFileFromRemote
    VersionInfoLocationPathList.Strings = (
      'https://raw.githubusercontent.com/pyscripter/pyscripter/master')
    VersionInfoFileName = 'PyScripterVersionInfo.ini'
    Left = 494
    Top = 72
  end
  object SynIniSyn: TSynIniSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 344
    Top = 324
  end
  object JvMultiStringHolder: TJvMultiStringHolder
    MultipleStrings = <
      item
        Name = 'InitScript'
        Strings.Strings = (
          'import sys'
          'import code'
          'import pyscripter'
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
          '        def canonic(self, filename):'
          '            import bdb'
          '            if type(filename) is unicode:'
          '                filename = filename.encode("mbcs")'
          '            return bdb.Bdb.canonic(self, filename)'
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
          '                except SystemExit, e:'
          '                    if isinstance(e.code, basestring):'
          '                        print e.code'
          '                    elif isinstance(e.code, int):'
          '                        print "Exit code: ", e.code'
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
          '        import repr'
          '        pyrepr = repr.Repr()'
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
          '            exec code in mod.__dict__'
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
          '                exec cmd in globals, locals'
          '            except SystemExit, e:'
          '                if isinstance(e.code, basestring):'
          '                    print e.code'
          '                elif isinstance(e.code, int):'
          '                    print "Exit code: ", e.code'
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
          '                return len(dir(ob))'
          '        except:'
          '            return 0'
          ''
          
            '    def _getmembers(self, ob, dictitems = False, expandcommontyp' +
            'es = True, sequenceitems = False):'
          '        if sequenceitems and isinstance(ob, (list, tuple)):'
          '            result = {}'
          '            for i in range(len(ob)):'
          '                result[str(i)] = ob[i]'
          '        elif dictitems and isinstance(ob, dict):'
          '            result = {}'
          '            for (i,j) in ob.items():'
          '                result[self.safestr(i)] = j'
          
            '        elif not expandcommontypes and (self.objecttype(ob) in s' +
            'elf.commontypes):'
          '            result = {}'
          '        else:'
          '            result = {}'
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
          '        except ImportError, err:'
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
          '        def softspace(file, newvalue):'
          '            oldvalue = 0'
          '            try:'
          '                oldvalue = file.softspace'
          '            except AttributeError:'
          '                pass'
          '            try:'
          '                file.softspace = newvalue'
          '            except (AttributeError, TypeError):'
          
            '                # "attribute-less object" or "read-only attribut' +
            'es"'
          '                pass'
          '            return oldvalue'
          ''
          '        self.saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          '        try:'
          '            if self.debugger.currentframe:'
          
            '                exec code in self.debugger.currentframe.f_global' +
            's, self.debugger.currentframe.f_locals'
          '                # save locals'
          '                try:'
          '                    import ctypes'
          
            '                    ctypes.pythonapi.PyFrame_LocalsToFast(ctypes' +
            '.py_object(self.debugger.currentframe), 0)'
          '                except :'
          '                    pass'
          '            else:'
          '                exec code in self.locals'
          '        except SystemExit, e:'
          '            if isinstance(e.code, basestring):'
          '                print e.code'
          '            elif isinstance(e.code, int):'
          '                print "Exit code: ", e.code'
          '        except:'
          '            self.showtraceback()'
          '        else:'
          '            if softspace(sys.stdout, 0):'
          '                print'
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
          
            '        from inspect import isclass, isroutine, getargspec, form' +
            'atargspec, getdoc'
          '        argText = ""'
          '        if ob is not None:'
          '            argOffset = 0'
          '            if isclass(ob) and (ob.__module__ != '#39'__builtin__'#39'):'
          
            '                # Look for the highest __init__ in the class cha' +
            'in.'
          '                fob = getattr(ob, '#39'__init__'#39', ob)'
          '                argOffset = 1'
          '            elif type(ob)==types.MethodType:'
          
            '                # bit of a hack for methods - turn it into a fun' +
            'ction'
          '                # but we drop the "self" param.'
          '                fob = ob.im_func'
          '                argOffset = 1'
          '            else:'
          '                fob = ob'
          '            # Try and build one for Python defined functions'
          '            if isroutine(fob):'
          '                try:'
          
            '                    args, varargs, varkw, defaults = getargspec(' +
            'fob)'
          
            '                    argText = formatargspec(args[argOffset:], va' +
            'rargs, varkw, defaults)[1:-1]'
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
          
            '        ret = debugIDE.InputBox(u'#39'Python input'#39', unicode(prompt)' +
            ',u"")'
          '        if ret is not None:'
          '            sys.stdout.write(ret)'
          '        sys.stdout.write("\n")'
          '        sys.stdout.flush'
          '        if ret is None:'
          '            raise KeyboardInterrupt, "'#927'peration cancelled"'
          '        return ret'
          ''
          '    def Win32Input(self, prompt=None):'
          '        "Provide input() for gui apps"'
          '        return eval(raw_input(prompt))'
          ''
          '    def setupdisplayhook(self):'
          '        if pyscripter.IDEOptions.PrettyPrintOutput:'
          '            import sys, pprint, __builtin__'
          
            '            def pphook(value, show=pprint.pprint, bltin=__builti' +
            'n__):'
          '                if value is not None:'
          '                    bltin._ = value'
          '                    show(value)'
          '            sys.displayhook = pphook'
          ''
          '    def system_command(self, cmd):'
          '        import subprocess'
          
            '        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, ' +
            'shell=True)'
          '        print process.communicate()[0]'
          '        if process.returncode != 0:'
          '            print '#39'Error code: '#39', process.returncode'
          ''
          '_II = PythonInteractiveInterpreter(globals())'
          ''
          'sys.modules['#39'__builtin__'#39'].raw_input=_II.Win32RawInput'
          'sys.modules['#39'__builtin__'#39'].input=_II.Win32Input'
          ''
          'import os'
          'try:'
          '    sys.path.remove(os.path.dirname(sys.executable))'
          '    sys.path.remove(os.path.dirname(sys.executable))'
          'except:'
          '    pass'
          'sys.path.insert(0, "")'
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
        Name = 'InitScript3000'
        Strings.Strings = (
          'import sys'
          'import code'
          'import pyscripter'
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
          '                return len(dir(ob))'
          '        except:'
          '            return 0'
          ''
          
            '    def _getmembers(self, ob, dictitems = False, expandcommontyp' +
            'es = True, sequenceitems = False):'
          '        if sequenceitems and isinstance(ob, (list, tuple)):'
          '            result = {}'
          '            for i in range(len(ob)):'
          '                result[str(i)] = ob[i]'
          '        elif dictitems and isinstance(ob, dict):'
          '            result = {}'
          '            for (i,j) in ob.items():'
          '                result[self.safestr(i)] = j'
          
            '        elif not expandcommontypes and (self.objecttype(ob) in s' +
            'elf.commontypes):'
          '            result = {}'
          '        else:'
          '            result = {}'
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
          '        if pyscripter.IDEOptions.PrettyPrintOutput:'
          '            import sys, pprint, builtins'
          
            '            def pphook(value, show=pprint.pprint, bltin=builtins' +
            '):'
          '                if value is not None:'
          '                    bltin._ = value'
          '                    show(value)'
          '            sys.displayhook = pphook'
          ''
          '    def system_command(self, cmd):'
          '        import subprocess'
          
            '        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, ' +
            'shell=True)'
          '        print(process.communicate()[0].decode())'
          '        if process.returncode != 0:'
          '            print('#39'Error code: '#39', process.returncode)'
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
          'sys.path.insert(0, "")'
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
          ''
          '__import__('#39'bdb'#39').__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.netref.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.protocol.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.stream.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.brine.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.channel.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.vinegar.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.service.__traceable__ = 0'
          '__import__('#39'rpyc'#39').utils.classic.__traceable__ = 0'
          '__import__('#39'rpyc'#39').utils.server.__traceable__ = 0'
          'threading.__traceable__ = 0'
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
          '        main_thread_id = threading.currentThread().ident'
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
          '        def _Thread__bootstrap(self):'
          '            self._set_ident()'
          
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
          '                self._Thread__bootstrap_inner()'
          '            finally:'
          '                self.debugger._sys.settrace(None)'
          
            '                self.debug_manager.thread_status(self.ident, sel' +
            'f.name, self.debug_manager.thrdFinished)'
          '                self.debugger = None'
          '    ThreadWrapper.debug_manager = DebugManager'
          ''
          '    class IDEDebugger(__import__('#39'bdb'#39').Bdb):'
          '        def __init__(self):'
          '            __import__('#39'bdb'#39').Bdb.__init__(self)'
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
          '        def isTraceable(self, frame):'
          '            return frame.f_globals.get('#39'__traceable__'#39', 1)'
          ''
          '        def stop_here(self, frame):'
          '            if not self.InitStepIn:'
          '                self.InitStepIn = True'
          '                self.set_continue()'
          '                return 0'
          '            return __import__('#39'bdb'#39').Bdb.stop_here(self, frame)'
          ''
          '        def canonic(self, filename):'
          '            if type(filename) is unicode:'
          
            '                filename = filename.encode(self._sys.getfilesyst' +
            'emencoding())'
          '            return __import__('#39'bdb'#39').Bdb.canonic(self, filename)'
          ''
          '        def user_line(self, frame):'
          '            try:'
          '                self._sys.stdout.print_queue.join()'
          '            except:'
          '                pass'
          ''
          '            dbg_manager = self.debug_manager'
          
            '            thread_id = dbg_manager._threading.currentThread().i' +
            'dent'
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
          '        def user_call(self, frame, arg):'
          '            self.tracecount += 1'
          '            #yield processing every 1000 calls'
          
            '            if (self.tracecount > 1000) and not (hasattr(self._s' +
            'ys.stdout, "writing") and self._sys.stdout.writing):'
          '                self.tracecount = 0'
          '                with self.debug_manager.user_lock:'
          
            '                    cmd = self.debug_manager.debugIDE.user_yield' +
            '()'
          '                if cmd == self.debug_manager.dcAbort:'
          '                    self.set_quit()'
          '                elif cmd == self.debug_manager.dcPause:'
          '                    self.set_step()'
          ''
          '        def dispatch_call(self, frame, arg):'
          
            '            res = __import__('#39'bdb'#39').Bdb.dispatch_call(self, fram' +
            'e, arg)'
          '            if res:'
          '                if self.isTraceable(frame) == 0:'
          '                    return'
          '            return res'
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
          '                except SystemExit, e:'
          '                    if isinstance(e.code, basestring):'
          '                        print e.code'
          '                    elif isinstance(e.code, int):'
          '                        print "Exit code: ", e.code'
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
          '                sys.stdout.print_queue.join()'
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
          '        import repr'
          '        pyrepr = repr.Repr()'
          '        pyrepr.maxstring = 60'
          '        pyrepr.maxother = 60'
          '        self._repr = pyrepr.repr'
          '        self._str = str'
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
          '                return len(dir(ob))'
          '        except:'
          '            return 0'
          ''
          
            '    def _getmembers(self, ob, dictitems = False, expandcommontyp' +
            'es = True, sequenceitems = False):'
          '        if sequenceitems and isinstance(ob, (list, tuple)):'
          '            result = {}'
          '            for i in range(len(ob)):'
          '                result[self._str(i)] = ob[i]'
          '        elif dictitems and isinstance(ob, dict):'
          '            result = {}'
          '            for (i,j) in ob.items():'
          '                result[self.safestr(i)] = j'
          
            '        elif not expandcommontypes and (self.objecttype(ob) in s' +
            'elf.commontypes):'
          '            result = {}'
          '        else:'
          '            result = {}'
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
          '            return self._str(value)'
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
          '        except ImportError, err:'
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
          
            '            code.InteractiveInterpreter.showsyntaxerror(self, fi' +
            'lename)'
          '        finally:'
          '            sys.excepthook = old_excepthook'
          '            sys.stdout.print_queue.join()'
          ''
          
            '    def runsource(self, source, filename="<input>", symbol="sing' +
            'le"):'
          '        import sys, code'
          '        saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          '        try:'
          
            '            return code.InteractiveInterpreter.runsource(self, s' +
            'ource, filename, symbol)'
          '        finally:'
          '            sys.stdin, sys.stdout, sys.stderr = saveStdio'
          '            sys.stdout.print_queue.join()'
          ''
          '    def runcode(self, code):'
          '        import sys'
          '        def softspace(file, newvalue):'
          '            oldvalue = 0'
          '            try:'
          '                oldvalue = file.softspace'
          '            except AttributeError:'
          '                pass'
          '            try:'
          '                file.softspace = newvalue'
          '            except (AttributeError, TypeError):'
          
            '                # "attribute-less object" or "read-only attribut' +
            'es"'
          '                pass'
          '            return oldvalue'
          '        try:'
          '            if self.DebugManager.active_frame:'
          
            '                exec code in self.DebugManager.active_frame.f_gl' +
            'obals, self.DebugManager.active_frame.f_locals'
          '                # save locals'
          '                try:'
          '                    import ctypes'
          
            '                    ctypes.pythonapi.PyFrame_LocalsToFast(ctypes' +
            '.py_object(self.DebugManager.active_frame), 0)'
          '                except :'
          '                    pass'
          '            else:'
          '              exec code in self.locals'
          '        except SystemExit, e:'
          '            if isinstance(e.code, basestring):'
          '                print e.code'
          '            elif isinstance(e.code, int):'
          '                print "Exit code: ", e.code'
          '        except:'
          '            self.showtraceback()'
          '        else:'
          '            import sys'
          '            if softspace(sys.stdout, 0):'
          '                print'
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
          
            '        from inspect import isclass, isroutine, getargspec, form' +
            'atargspec, getdoc'
          '        argText = ""'
          '        if ob is not None:'
          '            argOffset = 0'
          '            if isclass(ob) and (ob.__module__ != '#39'__builtin__'#39'):'
          
            '                # Look for the highest __init__ in the class cha' +
            'in.'
          '                fob = getattr(ob, '#39'__init__'#39', ob)'
          '                argOffset = 1'
          '            elif type(ob)==types.MethodType:'
          
            '                # bit of a hack for methods - turn it into a fun' +
            'ction'
          '                # but we drop the "self" param.'
          '                fob = ob.im_func'
          '                argOffset = 1'
          '            else:'
          '                fob = ob'
          '            # Try and build one for Python defined functions'
          '            if isroutine(fob):'
          '                try:'
          
            '                    args, varargs, varkw, defaults = getargspec(' +
            'fob)'
          
            '                    argText = formatargspec(args[argOffset:], va' +
            'rargs, varkw, defaults)[1:-1]'
          '                except:'
          '                    pass'
          '            return (argText, getdoc(fob))'
          ''
          '    def rem_compile(self, source, fname):'
          '        import sys'
          '        self.exc_info = None'
          '        try:'
          '            return compile(source, fname, "exec")'
          '        except (OverflowError, SyntaxError, ValueError), e:'
          '            print'
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
          '            exec code in mod.__dict__'
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
          '                exec cmd in globals, locals'
          '            except SystemExit, e:'
          '                if isinstance(e.code, basestring):'
          '                    print e.code'
          '                elif isinstance(e.code, int):'
          '                    print "Exit code: ", e.code'
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
          '            sys.stdout.print_queue.join()'
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
          '            elif inspect.ismethod(ob) or inspect.isbuiltin(ob):'
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
          '            return os.getcwdu()'
          '        except:'
          '            return '#39#39
          ''
          '    class AsyncStream(object):'
          '        softspace=0'
          '        encoding=None'
          '        def __init__(self, stream):'
          '            import Queue'
          '            import time'
          '            self._stream = stream'
          '            self.writing = False'
          '            self.print_queue = Queue.Queue()'
          '            self._sleep = time.sleep'
          ''
          '            import threading'
          
            '            print_thread = threading.Thread(target = self.proces' +
            's_print_queue)'
          '            print_thread.daemon = True'
          '            print_thread.start()'
          ''
          '##        def __getattr__(self, attr):'
          '##            return getattr(self._stream, attr)'
          ''
          '        def flush(self):'
          '            import sys'
          
            '            if sys._getframe(1).f_globals["__name__"] != "loggin' +
            'g":'
          '                self.print_queue.join()'
          ''
          '        def readline(self, size=None):'
          '            try:'
          '                self.print_queue.join()'
          '                return self._stream.readline(size)'
          '            except KeyboardInterrupt:'
          '                raise KeyboardInterrupt, "Operation Cancelled"'
          ''
          '        def write(self, message):'
          '            if isinstance(message, basestring):'
          '                self.print_queue.put(message)'
          '            else:'
          
            '                raise TypeError("write() argument must be str or' +
            ' unicode")'
          ''
          '        def process_print_queue(self):'
          '            def rem_write(l):'
          '                self.writing = True'
          '                try:'
          '                    self._stream.write("".join(l))'
          '                except UnicodeDecodeError:'
          
            '                    self._stream.write("".join([s.decode(errors=' +
            #39'replace'#39') if type(s) == str else s for s in l]))'
          '                self.writing = False'
          '                del l[:]'
          ''
          '            while True:'
          '                self._sleep(0.01)'
          '                l = [self.print_queue.get()]'
          '                while not self.print_queue.empty():'
          '                    if len(l) > 10000:'
          '                        rem_write(l)'
          '                    l.append(self.print_queue.get())'
          '                    self.print_queue.task_done()'
          '                rem_write(l)'
          
            '                # matches the first get so that join waits for w' +
            'riting to finesh'
          '                self.print_queue.task_done()'
          ''
          '    def asyncIO(self):'
          '        import sys'
          
            '        sys.stdin = sys.stderr = sys.stdout = self.AsyncStream(s' +
            'ys.stdout)'
          ''
          '    def setupdisplayhook(self):'
          '        import sys, pprint, __builtin__'
          
            '        def pphook(value, show=pprint.pprint, bltin=__builtin__)' +
            ':'
          '            if value is not None:'
          '                bltin._ = value'
          '                show(value)'
          '        sys.displayhook = pphook'
          ''
          '    def system_command(self, cmd):'
          '        import sys, os'
          '        res = os.system(cmd)'
          '        if res != 0:'
          '            print '#39'Error code: '#39', res'
          '        sys.stdout.print_queue.join()'
          ''
          '    def Win32RawInput(self, prompt=None):'
          '        "Provide raw_input() for gui apps"'
          '        # flush stderr/out first.'
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
          '        with self.DebugManager.user_lock:'
          
            '            ret = self.DebugManager.debugIDE.InputBox(u'#39'Python i' +
            'nput'#39', unicode(prompt),u"")'
          '        if ret is not None:'
          '            sys.stdout.write(ret)'
          '        sys.stdout.write("\n")'
          '        sys.stdout.flush'
          '        if ret is None:'
          '            raise KeyboardInterrupt, "Operation cancelled"'
          '        return ret'
          ''
          '    def Win32Input(self, prompt=None):'
          '        "Provide input() for gui apps"'
          '        return eval(raw_input(prompt))'
          ''
          '_RPI = RemotePythonInterpreter(globals())'
          ''
          'sys.modules['#39'__builtin__'#39'].raw_input = _RPI.Win32RawInput'
          'sys.modules['#39'__builtin__'#39'].input = _RPI.Win32Input'
          ''
          'import os'
          'try:'
          '    sys.path.remove(os.path.dirname(sys.argv[0]))'
          'except:'
          '    pass'
          'sys.path.insert(0, "")'
          ''
          'del code'
          'del RemotePythonInterpreter'
          'del sys'
          'del os'
          'del threading')
      end
      item
        Name = 'RpyC_Init3000'
        Strings.Strings = (
          'import sys'
          'import code'
          'import threading'
          '#import logging'
          ''
          '##logging.basicConfig(level=logging.DEBUG,'
          
            '##                    filename = "c:/users/kiriakos/desktop/test' +
            '.log",'
          '##                    filemode = "a",'
          '##                    format='#39'(%(threadName)-10s) %(message)s'#39')'
          ''
          '__import__('#39'bdb'#39').__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.netref.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.protocol.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.stream.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.brine.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.channel.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.vinegar.__traceable__ = 0'
          '__import__('#39'rpyc'#39').core.service.__traceable__ = 0'
          '__import__('#39'rpyc'#39').utils.classic.__traceable__ = 0'
          '__import__('#39'rpyc'#39').utils.server.__traceable__ = 0'
          'threading.__traceable__ = 0'
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
          '        def _bootstrap(self):'
          '            self._set_ident()'
          
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
          '                self._bootstrap_inner()'
          '            finally:'
          '                self.debugger._sys.settrace(None)'
          
            '                self.debug_manager.thread_status(self.ident, sel' +
            'f.name, self.debug_manager.thrdFinished)'
          '                self.debugger = None'
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
          '        def isTraceable(self, frame):'
          '            return frame.f_globals.get('#39'__traceable__'#39', 1)'
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
          '                self._sys.stdout.print_queue.join()'
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
          '        def user_call(self, frame, arg):'
          '            self.tracecount += 1'
          '            #yield processing every 1000 calls'
          
            '            if (self.tracecount > 1000) and not (hasattr(self._s' +
            'ys.stdout, "writing") and self._sys.stdout.writing):'
          '                self.tracecount = 0'
          '                with self.debug_manager.user_lock:'
          
            '                    cmd = self.debug_manager.debugIDE.user_yield' +
            '()'
          '                if cmd == self.debug_manager.dcAbort:'
          '                    self.set_quit()'
          '                elif cmd == self.debug_manager.dcPause:'
          '                    self.set_step()'
          ''
          '        def dispatch_call(self, frame, arg):'
          '            res = super().dispatch_call(frame, arg)'
          '            if res:'
          '                if self.isTraceable(frame) == 0:'
          '                    return'
          
            '                #logging.debug("dispatch_call " + frame.f_code.c' +
            'o_filename + " " + frame.f_code.co_name)'
          '            return res'
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
          '                sys.stdout.print_queue.join()'
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
          '                return len(dir(ob))'
          '        except:'
          '            return 0'
          ''
          
            '    def _getmembers(self, ob, dictitems = False, expandcommontyp' +
            'es = True, sequenceitems = False):'
          '        if sequenceitems and isinstance(ob, (list, tuple)):'
          '            result = {}'
          '            for i in range(len(ob)):'
          '                result[str(i)] = ob[i]'
          '        elif dictitems and isinstance(ob, dict):'
          '            result = {}'
          '            for (i,j) in ob.items():'
          '                result[self.safestr(i)] = j'
          
            '        elif not expandcommontypes and (self.objecttype(ob) in s' +
            'elf.commontypes):'
          '            result = {}'
          '        else:'
          '            result = {}'
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
          '            sys.stdout.print_queue.join()'
          ''
          ''
          
            '    def runsource(self, source, filename="<input>", symbol="sing' +
            'le"):'
          '        import sys'
          '        saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
          '        try:'
          '            return super().runsource(source, filename, symbol)'
          '        finally:'
          '            sys.stdin, sys.stdout, sys.stderr = saveStdio'
          '            sys.stdout.print_queue.join()'
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
          '            sys.stdout.print_queue.join()'
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
          '    class AsyncStream(object):'
          '        #softspace=0'
          '        encoding=None'
          '        def __init__(self, stream):'
          '            import queue'
          '            import time'
          '            self._stream = stream'
          '            self.writing = False'
          '            self.print_queue = queue.Queue()'
          '            self._sleep = time.sleep'
          ''
          '            import threading'
          
            '            print_thread = threading.Thread(target = self.proces' +
            's_print_queue)'
          '            print_thread.daemon = True'
          '            print_thread.start()'
          ''
          '##        def __getattr__(self, attr):'
          '##            return getattr(self._stream, attr)'
          ''
          '        def flush(self):'
          '            import sys'
          
            '            if sys._getframe(1).f_globals["__name__"] != "loggin' +
            'g":'
          '                self.print_queue.join()'
          ''
          '        def readline(self, size=None):'
          '            try:'
          '                self.print_queue.join()'
          '                return self._stream.readline(size)'
          '            except KeyboardInterrupt:'
          '                raise KeyboardInterrupt("Operation Cancelled")'
          ''
          '        def write(self, message):'
          '            if isinstance(message, str):'
          '                self.print_queue.put(message)'
          '            else:'
          '                raise TypeError("write() argument must be str")'
          ''
          '        def process_print_queue(self):'
          '            def rem_write(l):'
          '                self.writing = True'
          '                self._stream.write("".join(l))'
          '                self.writing = False'
          '                del l[:]'
          ''
          '            while True:'
          '                self._sleep(0.01)'
          '                l = [self.print_queue.get()]'
          '                while not self.print_queue.empty():'
          '                    if len(l) > 10000:'
          '                        rem_write(l)'
          '                    l.append(self.print_queue.get())'
          '                    self.print_queue.task_done()'
          '                rem_write(l)'
          
            '                # matches the first get so that join waits for w' +
            'riting to finesh'
          '                self.print_queue.task_done()'
          ''
          '    def asyncIO(self):'
          '        import sys'
          
            '        sys.stdin = sys.stderr = sys.stdout = self.AsyncStream(s' +
            'ys.stdout)'
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
          '            print ('#39'Error code: '#39', res)'
          '        sys.stdout.print_queue.join()'
          ''
          '    def Win32RawInput(self, prompt=None):'
          '        "Provide raw_input() for gui apps"'
          '        # flush stderr/out first.'
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
          '        with self.DebugManager.user_lock:'
          
            '            ret = self.DebugManager.debugIDE.InputBox('#39'Python in' +
            'put'#39', str(prompt), "")'
          '        if ret is not None:'
          '            sys.stdout.write(ret)'
          '        sys.stdout.write("\n")'
          '        sys.stdout.flush'
          '        if ret is None:'
          '            raise KeyboardInterrupt("Operation cancelled")'
          '        return ret'
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
          'sys.modules['#39'builtins'#39'].input=_RPI.Win32RawInput'
          'del code'
          'del RemotePythonInterpreter'
          'del sys'
          'del os'
          'del threading')
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
          '        import imp'
          '        from rpyc.core.service import ModuleNamespace'
          ''
          '        sys.modules["__oldmain__"] = sys.modules["__main__"]'
          '        sys.modules["__main__"] = imp.new_module("__main__")'
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
          '        ))'
          '        # shortcuts'
          '        conn.modules = ModuleNamespace(conn.root.getmodule)'
          '        conn.eval = conn.root.eval'
          '        conn.execute = conn.root.execute'
          '        conn.namespace = conn.root.namespace'
          '        if sys.version_info[0] > 2:'
          '            conn.builtin = conn.modules.builtins'
          '        else:'
          '            conn.builtin = conn.modules.__builtin__'
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
          '        import imp'
          '        from rpyc.core.service import ModuleNamespace'
          ''
          '        sys.modules["__oldmain__"] = sys.modules["__main__"]'
          '        sys.modules["__main__"] = imp.new_module("__main__")'
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
          '        ))'
          '        # shortcuts'
          '        conn.modules = ModuleNamespace(conn.root.getmodule)'
          '        conn.eval = conn.root.eval'
          '        conn.execute = conn.root.execute'
          '        conn.namespace = conn.root.namespace'
          '        if sys.version_info[0] > 2:'
          '            conn.builtin = conn.modules.builtins'
          '        else:'
          '            conn.builtin = conn.modules.__builtin__'
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
          '        import imp'
          '        from rpyc.core.service import ModuleNamespace'
          ''
          '        sys.modules["__oldmain__"] = sys.modules["__main__"]'
          '        sys.modules["__main__"] = imp.new_module("__main__")'
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
          '        ))'
          '        # shortcuts'
          '        conn.modules = ModuleNamespace(conn.root.getmodule)'
          '        conn.eval = conn.root.eval'
          '        conn.execute = conn.root.execute'
          '        conn.namespace = conn.root.namespace'
          '        if sys.version_info[0] > 2:'
          '            conn.builtin = conn.modules.builtins'
          '        else:'
          '            conn.builtin = conn.modules.__builtin__'
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
    Left = 343
    Top = 176
  end
  object dlgFileOpen: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 20
    Top = 16
  end
  object dlgFileSave: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 20
    Top = 68
  end
  object SynWebHtmlSyn: TSynWebHtmlSyn
    Options.HtmlVersion = shvHtml5
    Options.UseEngineOptions = True
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Left = 432
    Top = 272
  end
  object SynWebXmlSyn: TSynWebXmlSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Left = 436
    Top = 328
  end
  object SynWebCssSyn: TSynWebCssSyn
    Options.HtmlVersion = shvHtml401Transitional
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Left = 528
    Top = 324
  end
  object SynCppSyn: TSynCppSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 344
    Top = 272
  end
  object SynWebEngine: TSynWebEngine
    Options.HtmlVersion = shvHtml5
    Left = 616
    Top = 324
  end
  object actlMain: TActionList
    Images = Images
    Left = 21
    Top = 125
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      Enabled = False
      HelpContext = 310
      Hint = 'Save|Save active file'
      ImageIndex = 4
      ShortCut = 16467
      OnExecute = actFileSaveExecute
    end
    object actFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save &As...'
      Enabled = False
      HelpContext = 310
      Hint = 'Save As|Save active file under different name'
      OnExecute = actFileSaveAsExecute
    end
    object actFileClose: TAction
      Category = 'File'
      Caption = '&Close'
      Enabled = False
      HelpContext = 310
      Hint = 'Close|Close active file'
      ImageIndex = 149
      ShortCut = 16499
      OnExecute = actFileCloseExecute
    end
    object actEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Enabled = False
      HelpContext = 320
      HelpType = htContext
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 11
      ShortCut = 16472
    end
    object actEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Enabled = False
      HelpContext = 320
      HelpType = htContext
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 12
      ShortCut = 16451
    end
    object actEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      HelpContext = 320
      HelpType = htContext
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 13
      ShortCut = 16470
    end
    object actEditDelete: TEditDelete
      Category = 'Edit'
      Caption = 'De&lete'
      Enabled = False
      HelpContext = 320
      HelpType = htContext
      Hint = 'Delete|Delete selection'
      ImageIndex = 14
    end
    object actEditUndo: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Enabled = False
      HelpContext = 320
      HelpType = htContext
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 9
      ShortCut = 16474
    end
    object actEditRedo: TAction
      Category = 'Edit'
      Caption = '&Redo'
      Enabled = False
      HelpContext = 320
      Hint = 'Redo| Redo last action'
      ImageIndex = 10
      ShortCut = 24666
      OnExecute = actEditRedoExecute
    end
    object actEditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      HelpContext = 320
      HelpType = htContext
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object actEditReadOnly: TAction
      Category = 'Edit'
      Caption = 'Read Only'
      HelpContext = 320
      Hint = 'Enable/disable editing'
      OnExecute = actEditReadOnlyExecute
    end
    object actSearchFind: TAction
      Category = 'Search'
      Caption = '&Find...'
      Enabled = False
      HelpContext = 330
      Hint = 'Search|Search for a string'
      ImageIndex = 15
      ShortCut = 16454
      OnExecute = actSearchFindExecute
    end
    object actSearchFindNext: TAction
      Category = 'Search'
      Caption = 'Find &Next'
      Enabled = False
      HelpContext = 330
      Hint = 'Find next|Find next match'
      ImageIndex = 16
      ShortCut = 114
      OnExecute = actSearchFindNextExecute
    end
    object actSearchFindPrev: TAction
      Category = 'Search'
      Caption = 'Find &Previous'
      Enabled = False
      HelpContext = 330
      Hint = 'Find previous|Find Previous match'
      ImageIndex = 121
      ShortCut = 8306
      OnExecute = actSearchFindPrevExecute
    end
    object actSearchReplace: TAction
      Category = 'Search'
      Caption = '&Replace...'
      Enabled = False
      HelpContext = 330
      Hint = 'Replace|Search  & Replace'
      ImageIndex = 17
      ShortCut = 16456
      OnExecute = actSearchReplaceExecute
    end
    object actFileSaveAll: TAction
      Category = 'File'
      Caption = 'Save &All'
      HelpContext = 310
      Hint = 'Save all|Save project and all open files'
      ImageIndex = 5
      OnExecute = actFileSaveAllExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print...'
      Enabled = False
      HelpContext = 310
      Hint = 'Print|Print active file'
      ImageIndex = 8
      ShortCut = 16464
      OnExecute = actFilePrintExecute
    end
    object actPrinterSetup: TAction
      Category = 'File'
      Caption = 'Printer Set&up...'
      HelpContext = 310
      Hint = 'Printer setup'
      ImageIndex = 6
      OnExecute = actPrinterSetupExecute
    end
    object actPrintPreview: TAction
      Category = 'File'
      Caption = 'Print Pre&view'
      HelpContext = 310
      Hint = 'Print preview'
      ImageIndex = 7
      OnExecute = actPrintPreviewExecute
    end
    object actPageSetup: TAction
      Category = 'File'
      Caption = 'Pa&ge Setup...'
      HelpContext = 310
      Hint = 'Page setup'
      ImageIndex = 78
      OnExecute = actPageSetupExecute
    end
    object actEditorOptions: TAction
      Category = 'Options'
      Caption = '&Editor Options...'
      HelpContext = 620
      Hint = 'Set Editor Options'
      ImageIndex = 22
      OnExecute = actEditorOptionsExecute
    end
    object actIDEOptions: TAction
      Category = 'Options'
      Caption = '&IDE Options...'
      HelpContext = 610
      Hint = 'Set IDE Options'
      ImageIndex = 24
      OnExecute = actIDEOptionsExecute
    end
    object actEditIndent: TAction
      Category = 'Source Code'
      Caption = '&Indent Block'
      HelpContext = 320
      Hint = 'Indent block|Indent selected block of code'
      ImageIndex = 69
      ShortCut = 24649
      OnExecute = actEditIndentExecute
    end
    object actEditDedent: TAction
      Category = 'Source Code'
      Caption = '&Unindent Block'
      HelpContext = 320
      Hint = 'Unindent|Unindent selected block of code'
      ImageIndex = 70
      ShortCut = 24661
      OnExecute = actEditDedentExecute
    end
    object actEditCommentOut: TAction
      Category = 'Source Code'
      Caption = '&Comment out'
      HelpContext = 320
      Hint = 'Comment out| Comment out block of code'
      ImageIndex = 73
      ShortCut = 49342
      OnExecute = actEditCommentOutExecute
    end
    object actEditUncomment: TAction
      Category = 'Source Code'
      Caption = '&Uncomment'
      HelpContext = 320
      Hint = 'Uncomment| Uncomment block of code'
      ImageIndex = 74
      ShortCut = 49340
      OnExecute = actEditUncommentExecute
    end
    object actSearchMatchingBrace: TAction
      Category = 'Search'
      Caption = '&Matching Brace'
      HelpContext = 330
      Hint = 'Find Matching Brace'
      ShortCut = 16605
      OnExecute = actSearchMatchingBraceExecute
    end
    object actEditTabify: TAction
      Category = 'Source Code'
      Caption = '&Tabify'
      HelpContext = 320
      Hint = 'Tabify|Convert spaces to tabs'
      ShortCut = 32990
      OnExecute = actEditTabifyExecute
    end
    object actEditUntabify: TAction
      Category = 'Source Code'
      Caption = 'U&ntabify'
      HelpContext = 320
      Hint = 'Untabify|Convert tabs to spaces'
      ShortCut = 41182
      OnExecute = actEditUntabifyExecute
    end
    object actPythonPath: TAction
      Category = 'Tools'
      Caption = 'Python &Path...'
      HelpContext = 870
      Hint = 'Python Path|View or edit the Python path'
      ImageIndex = 25
      OnExecute = actPythonPathExecute
    end
    object actHelpContents: THelpContents
      Category = 'Help'
      Caption = '&Contents'
      Enabled = False
      HelpContext = 370
      HelpType = htContext
      Hint = 'Help Contents'
      ImageIndex = 71
      OnExecute = actHelpContentsExecute
    end
    object actPythonManuals: THelpContents
      Category = 'Help'
      Caption = '&Python Manuals'
      HelpContext = 370
      HelpType = htContext
      Hint = 'Show Python Manuals'
      ImageIndex = 77
      OnExecute = actPythonManualsExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      HelpContext = 370
      Hint = 'About|Info about the application'
      ImageIndex = 30
      OnExecute = actAboutExecute
    end
    object actSearchGoToLine: TAction
      Category = 'Search'
      Caption = 'Go To &Line...'
      HelpContext = 330
      Hint = 'Go to line number'
      ImageIndex = 32
      ShortCut = 32839
      OnExecute = actSearchGoToLineExecute
    end
    object actSearchGoToSyntaxError: TAction
      Category = 'Search'
      Caption = 'Go To Syntax &Error'
      HelpContext = 330
      Hint = 'Jump to the position of the first syntax error'
      ImageIndex = 123
      ShortCut = 24645
      OnExecute = actSearchGoToSyntaxErrorExecute
    end
    object actFindInFiles: TAction
      Category = 'Search'
      Caption = '&Find in Files...'
      HelpContext = 330
      Hint = 'Search in Files|Search for a string in Files'
      ImageIndex = 86
      ShortCut = 24646
      OnExecute = actFindInFilesExecute
    end
    object actParameterCompletion: TAction
      Category = 'Parameters'
      Caption = 'Insert &parameter'
      HelpContext = 720
      Hint = 'Insert parameter to the edited file'
      ShortCut = 24656
      OnExecute = actParameterCompletionExecute
    end
    object actModifierCompletion: TAction
      Category = 'Parameters'
      Caption = 'Insert &modifier'
      HelpContext = 720
      Hint = 'Insert parameter to the edited file'
      ShortCut = 24653
      OnExecute = actModifierCompletionExecute
    end
    object actReplaceParameters: TAction
      Category = 'Parameters'
      Caption = '&Replace parameters'
      HelpContext = 720
      Hint = 'Replace parameters with their values'
      ShortCut = 24658
      OnExecute = actReplaceParametersExecute
    end
    object actHelpParameters: TAction
      Category = 'Help'
      Caption = '&Parameters'
      HelpContext = 370
      Hint = 'Help on custom parameters'
      OnExecute = actHelpParametersExecute
    end
    object actInsertTemplate: TAction
      Category = 'Edit'
      Caption = 'Insert &Template'
      HelpContext = 320
      Hint = 'Insert a Code Template'
      ShortCut = 16458
      OnExecute = actInsertTemplateExecute
    end
    object actCustomizeParameters: TAction
      Category = 'Parameters'
      Caption = 'Custom &Parameters...'
      HelpContext = 720
      Hint = 'Add/Remove custom parameters'
      OnExecute = actCustomizeParametersExecute
    end
    object actIDEShortcuts: TAction
      Category = 'Options'
      Caption = 'IDE &Shortcuts...'
      HelpContext = 615
      Hint = 'Customize IDE shortcuts'
      ImageIndex = 102
      OnExecute = actIDEShortcutsExecute
    end
    object actCodeTemplates: TAction
      Category = 'Options'
      Caption = '&Code Templates...'
      HelpContext = 540
      Hint = 'Add/Remove code templates'
      OnExecute = actCodeTemplatesExecute
    end
    object actConfigureTools: TAction
      Category = 'Tools'
      Caption = 'Configure &Tools...'
      HelpContext = 710
      Hint = 'Configure Tools|Add/remove/edit command-line tools'
      ImageIndex = 83
      OnExecute = actConfigureToolsExecute
    end
    object actHelpExternalTools: TAction
      Category = 'Help'
      Caption = 'External &Tools'
      HelpContext = 370
      Hint = 'Help on External Tools'
      OnExecute = actHelpExternalToolsExecute
    end
    object actFindFunction: TAction
      Category = 'Search'
      Caption = 'Find F&unction...'
      HelpContext = 330
      Hint = 'Find Function|Find function from function list'
      ImageIndex = 26
      ShortCut = 16455
      OnExecute = actFindFunctionExecute
    end
    object actEditLineNumbers: TAction
      Category = 'Edit'
      Caption = 'Line &Numbers'
      HelpContext = 320
      Hint = 'Show/Hide line numbers'
      ImageIndex = 43
      OnExecute = actEditLineNumbersExecute
    end
    object actEditShowSpecialChars: TAction
      Category = 'Edit'
      Caption = 'Special &Characters'
      HelpContext = 320
      Hint = 'Show/Hide special characters'
      ImageIndex = 95
      OnExecute = actEditShowSpecialCharsExecute
    end
    object actFindPreviousReference: TAction
      Tag = 1
      Category = 'Search'
      Caption = 'Find Previous Reference'
      HelpContext = 330
      Hint = 'Find previous identifier reference'
      ShortCut = 49190
      OnExecute = actFindNextReferenceExecute
    end
    object actFindNextReference: TAction
      Category = 'Search'
      Caption = 'Find Next Reference'
      HelpContext = 330
      Hint = 'Find next identifier reference'
      ShortCut = 49192
      OnExecute = actFindNextReferenceExecute
    end
    object actEditLBDos: TAction
      Category = 'Edit'
      Caption = '&DOS/Windows'
      Checked = True
      HelpContext = 320
      Hint = 'DOS/Windows|Convert to DOS line break'
      OnExecute = actEditLBExecute
    end
    object actEditLBUnix: TAction
      Tag = 1
      Category = 'Edit'
      Caption = '&UNIX'
      HelpContext = 320
      Hint = 'UNIX|Convert to UNIX line break'
      OnExecute = actEditLBExecute
    end
    object actEditLBMac: TAction
      Tag = 2
      Category = 'Edit'
      Caption = '&Mac'
      HelpContext = 320
      Hint = 'Mac|Convert to Mac line break'
      OnExecute = actEditLBExecute
    end
    object actEditAnsi: TAction
      Category = 'Edit'
      Caption = 'ANSI'
      Checked = True
      HelpContext = 320
      Hint = 'Use ANSI encoding'
      OnExecute = actEditFileEncodingExecute
    end
    object actHelpEditorShortcuts: TAction
      Category = 'Help'
      Caption = 'Editor &Shortcuts'
      HelpContext = 370
      Hint = 'Help on editor shortcuts'
      OnExecute = actHelpEditorShortcutsExecute
    end
    object actCheckForUpdates: TAction
      Category = 'Tools'
      Caption = 'Check For &Updates'
      HelpContext = 350
      Hint = 'Check whether a newer version of PyScripter is available'
      OnExecute = actCheckForUpdatesExecute
    end
    object actUnitTestWizard: TAction
      Category = 'Tools'
      Caption = '&Unit Test Wizard...'
      HelpContext = 930
      Hint = 'Unit test wizard|Create unit test for active module'
      ImageIndex = 103
      OnExecute = actUnitTestWizardExecute
    end
    object actInterpreterEditorOptions: TAction
      Category = 'Options'
      Caption = '&Interpreter Editor Options...'
      HelpContext = 620
      Hint = 'Set Interpreter Editor Options'
      ImageIndex = 22
      OnExecute = actInterpreterEditorOptionsExecute
    end
    object actEditToggleComment: TAction
      Category = 'Source Code'
      Caption = 'Toggle &Comment'
      HelpContext = 320
      Hint = 'Toggle Comment| Comment/Uncomment block of code'
      ImageIndex = 73
      ShortCut = 16606
      OnExecute = actEditToggleCommentExecute
    end
    object actFileTemplates: TAction
      Category = 'Options'
      Caption = '&File Templates...'
      HelpContext = 640
      Hint = 'Add/Remove file templates'
      OnExecute = actFileTemplatesExecute
    end
    object actEditUTF8: TAction
      Tag = 1
      Category = 'Edit'
      Caption = 'UTF-8'
      HelpContext = 320
      Hint = 'Use UTF-8 encoding when saving the file'
      OnExecute = actEditFileEncodingExecute
    end
    object actEditUTF8NoBOM: TAction
      Tag = 2
      Category = 'Edit'
      Caption = 'UTF-8 (No BOM)'
      HelpContext = 320
      Hint = 'Use UTF-8 encoding without BOM'
      OnExecute = actEditFileEncodingExecute
    end
    object actEditUTF16LE: TAction
      Tag = 3
      Category = 'Edit'
      Caption = 'UTF-16LE'
      HelpContext = 320
      Hint = 'Use UTF-16LE encoding'
      OnExecute = actEditFileEncodingExecute
    end
    object actEditUTF16BE: TAction
      Tag = 4
      Category = 'Edit'
      Caption = 'UTF-16BE'
      HelpContext = 320
      Hint = 'Use UTF-16BE encoding'
      OnExecute = actEditFileEncodingExecute
    end
    object actFileReload: TAction
      Category = 'File'
      Caption = '&Reload'
      Enabled = False
      HelpContext = 310
      Hint = 'Reload|Reload active file'
      ImageIndex = 120
      OnExecute = actFileReloadExecute
    end
    object actImportShortcuts: TAction
      Category = 'Import/Export'
      Caption = 'Import Shortcuts'
      Hint = 'Import Shortcuts'
      OnExecute = actImportShortcutsExecute
    end
    object actExportShortCuts: TAction
      Category = 'Import/Export'
      Caption = 'Export Shortcuts'
      Hint = 'Export Shortcuts'
      OnExecute = actExportShortCutsExecute
    end
    object actImportHighlighters: TAction
      Category = 'Import/Export'
      Caption = 'Import Highlighters'
      Hint = 'Import Syntax Highlighters'
      OnExecute = actImportHighlightersExecute
    end
    object actExportHighlighters: TAction
      Category = 'Import/Export'
      Caption = 'Export Highlighters'
      Hint = 'Export Syntax Highlighters'
      OnExecute = actExportHighlightersExecute
    end
    object actSearchReplaceNow: TAction
      Category = 'Search'
      Caption = 'Replace'
      Enabled = False
      HelpContext = 330
      Hint = 'Replace with existing  settings'
      ImageIndex = 17
      OnExecute = actSearchReplaceNowExecute
    end
    object actSearchHighlight: TAction
      Category = 'Search'
      AutoCheck = True
      Caption = '&Highlight Search Text'
      HelpContext = 330
      Hint = 'Highlight the search text in the current editor'
      ImageIndex = 122
      ShortCut = 24648
      OnExecute = actSearchHighlightExecute
    end
    object actEditWordWrap: TAction
      Category = 'Edit'
      Caption = 'Word &Wrap'
      HelpContext = 320
      Hint = 'Turn word wrap on/off'
      ImageIndex = 124
      OnExecute = actEditWordWrapExecute
    end
    object actSearchGoToDebugLine: TAction
      Category = 'Search'
      Caption = 'Go To &Debugger Position'
      HelpContext = 330
      Hint = 'Go to the current position of the debugger'
      OnExecute = actSearchGoToDebugLineExecute
    end
    object actHelpWebProjectHome: TAction
      Category = 'Help'
      Caption = '&Project Home'
      HelpContext = 370
      Hint = 'Go to the project home page'
      ImageIndex = 147
      OnExecute = actHelpWebProjectHomeExecute
    end
    object actHelpWebGroupSupport: TAction
      Category = 'Help'
      Caption = '&Group Support'
      HelpContext = 370
      Hint = 'Go to the PyScripter Internet group'
      ImageIndex = 147
      OnExecute = actHelpWebGroupSupportExecute
    end
    object actFileCloseAllOther: TAction
      Tag = 1
      Category = 'File'
      Caption = 'Close All &Other'
      HelpContext = 310
      Hint = 'Close all files except the active one'
      OnExecute = actFileCloseWorkspaceTabsExecute
    end
    object actFileCloseAllToTheRight: TAction
      Category = 'File'
      Caption = 'Close All to the &Right'
      HelpContext = 310
      Hint = 'Close all files to the right'
      OnExecute = actFileCloseWorkspaceTabsExecute
    end
    object actEditCopyFileName: TAction
      Category = 'Edit'
      Caption = 'Copy File Name'
      HelpContext = 320
      HelpType = htContext
      Hint = 'Copy file name of active file to clipboard'
      ImageIndex = 12
      OnExecute = actEditCopyFileNameExecute
    end
    object actToolsEditStartupScripts: TAction
      Category = 'Tools'
      Caption = 'Edit Start&up Scripts'
      HelpContext = 350
      HelpType = htContext
      Hint = 'Edit PyScripter initialization files'
      OnExecute = actToolsEditStartupScriptsExecute
    end
    object actHelpWebBlog: TAction
      Category = 'Help'
      Caption = '&Blog'
      HelpContext = 370
      Hint = 'Go to the PyScripter Blog'
      ImageIndex = 147
      OnExecute = actHelpWebBlogExecute
    end
    object actFoldVisible: TAction
      Category = 'Code Folding'
      Caption = 'Code Folding'
      Hint = 'Show/Hide Code Folding'
      OnExecute = actFoldVisibleExecute
    end
    object actFoldAll: TAction
      Category = 'Code Folding'
      Caption = 'All'
      Hint = 'Fold all'
      ImageIndex = 29
      OnExecute = actFoldAllExecute
    end
    object actUnfoldAll: TAction
      Category = 'Code Folding'
      Caption = 'All'
      Hint = 'Unfold all'
      ImageIndex = 28
      OnExecute = actUnfoldAllExecute
    end
    object actFoldNearest: TAction
      Category = 'Code Folding'
      Caption = 'Nearest'
      Hint = 'Collapse nearest fold'
      OnExecute = actFoldNearestExecute
    end
    object actUnfoldNearest: TAction
      Category = 'Code Folding'
      Caption = 'Nearest'
      Hint = 'Expand nearest fold'
      OnExecute = actUnfoldNearestExecute
    end
    object actFoldRegions: TAction
      Category = 'Code Folding'
      Caption = 'Regions'
      Hint = 'Fold Ranges'
      OnExecute = actFoldRegionsExecute
    end
    object actUnfoldRegions: TAction
      Category = 'Code Folding'
      Caption = 'Regions'
      Hint = 'Unfold ranges'
      OnExecute = actUnfoldRegionsExecute
    end
    object actFoldLevel1: TAction
      Category = 'Code Folding'
      Caption = 'Level 1'
      Hint = 'Fold level 1'
      OnExecute = actFoldLevel1Execute
    end
    object actUnfoldLevel1: TAction
      Category = 'Code Folding'
      Caption = 'Level 1'
      Hint = 'Unfold level 1'
      OnExecute = actUnfoldLevel1Execute
    end
    object actFoldLevel2: TAction
      Category = 'Code Folding'
      Caption = 'Level 2'
      Hint = 'Fold level 2'
      OnExecute = actFoldLevel2Execute
    end
    object actUnfoldLevel2: TAction
      Category = 'Code Folding'
      Caption = 'Level 2'
      Hint = 'Unfold level 2'
      OnExecute = actUnfoldLevel2Execute
    end
    object actFoldLevel3: TAction
      Category = 'Code Folding'
      Caption = 'Level 3'
      Hint = 'Fold level 3'
      OnExecute = actFoldLevel3Execute
    end
    object actUnfoldLevel3: TAction
      Category = 'Code Folding'
      Caption = 'Level 3'
      Hint = 'Unfold level 3'
      OnExecute = actUnfoldLevel3Execute
    end
    object actFoldClasses: TAction
      Category = 'Code Folding'
      Caption = 'Classes'
      Hint = 'Fold classes'
      OnExecute = actFoldClassesExecute
    end
    object actUnfoldClasses: TAction
      Category = 'Code Folding'
      Caption = 'Classes'
      Hint = 'Unfold classes'
      OnExecute = actUnfoldClassesExecute
    end
    object actFoldFunctions: TAction
      Category = 'Code Folding'
      Caption = 'Functions'
      Hint = 'Fold functions'
      OnExecute = actFoldFunctionsExecute
    end
    object actUnfoldFunctions: TAction
      Category = 'Code Folding'
      Caption = 'Functions'
      Hint = 'Unfold functions'
      OnExecute = actUnfoldFunctionsExecute
    end
    object actFileSaveToRemote: TAction
      Category = 'File'
      Caption = 'Save to Remote File'
      Hint = 'Save to remote file with SSH'
      ImageIndex = 162
      OnExecute = actFileSaveToRemoteExecute
    end
    object actDonate: TAction
      Category = 'Help'
      Caption = '&Donate'
      HelpContext = 370
      Hint = 'Donate to the PyScripter project'
      ImageIndex = 147
      OnExecute = actDonateExecute
    end
  end
  object Images: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 16
    Top = 200
    Bitmap = {
      494C0101A3001401040010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009002000001002000000000000090
      0200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000010101020000000100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000010F0F0F110F0F0F1100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000002020203786767A17160609A00000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001818181BD58686F1C97676EB0F0F0F10000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000010101027769699EE86C6CFFE24D4DFF765E5EA0010101020000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001818181BE97C7CFED46464F50F0F0F10000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001717171A1D1D1D221D1D1D221D1D
      1D221D1D1D221D1D1D221D1D1D221D1D1D221D1D1D221D1D1D221D1D1D221D1D
      1D221D1D1D221D1D1D221D1D1D22161616190000000000000000000000000000
      0000000000005D56567D9C8080C4E97070FFDB5555FB966565C4565050740000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001818181BE97C7CFED46464F50F0F0F10000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000717171D7A2A2A2ECA2A2A2ECA2A2
      A2ECA2A2A2ECA2A2A2ECA2A2A2ECA2A2A2ECA3A3A3ECA3A3A3ECA4A4A4ECA2A2
      A0EC9E9E89EFA2A29CEDA4A4A4EC707070D30000000000000000000000000000
      000000000000000000001818181BE97C7CFED46464F50F0F0F10000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001818181BE97C7CFED46464F50F0F0F10000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000797979D1F2F0EEFFE9D7CAFFE9D7
      CAFFEAD8CBFFEAD8CBFFEAD8CBFFEDDDD3FFF6F6F6FFF6F6F6FFF7F7F7FFEFEF
      E7FFB0B060FFEBEBDBFFF8F8F8FF777777CC00000000060606074948406B5B59
      468F5B59468F5B59488F6463519BEA7B7BFFDB6665FA5B5B4E8A5B5A498F5B5B
      4A8F5B5B4A8F504F45790B0B0B0C0000000000000000060606074848406A5B59
      468F5B59468F5B59488F6463519BEA7B7BFFDB6665FA5B5B4E8A5B5A498F5B5B
      4A8F5B5B4A8F505045790C0C0C0D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000797979D1EFE7E2FFD69D74FFD69D
      74FFD69D74FFD69D74FFD69D74FFDBAC8BFFF6F6F6FFF6F6F6FFF7F7F7FFF7F7
      F7FFBEBE80FFF8F8F8FFF8F8F8FF777777CC0F0F0F117E7749C6E8CC95FEF4D8
      ABFFF5DBB2FFF6DEB9FFF8E5C7FFEB7C7CFFE76B6AFFF9ECD8FFFAEFDCFFFBF2
      E3FFFCF5EAFFF8F4E7FF8D8C63D21818181B0E0E0E0F7D7649C5E8CD95FEF4D8
      ABFFF5DBB2FFF6DEB9FFF8E5C7FFEB7C7CFFE76B6AFFF9ECD8FFFAEFDCFFFBF2
      E3FFFCF5EAFFF8F4E7FF8D8D64D31919181C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000797979D1EFE7E2FFD69D74FFD69D
      74FFD69D74FFD69D74FFD69D74FFDBAC8BFFF6F6F6FFF6F6F6FFF7F7F7FFF7F7
      F7FFBEBE80FFF8F8F8FFF8F8F8FF777777CC5C5A4691EFCF99FFF3D5A4FFF4D8
      ABFFF5DBB2FFF6DEB9FFF8E5C7FFEB7C7CFFE76B6AFFF9ECD8FFFAEFDCFFFBF2
      E3FFFCF5EAFFFDF9F1FFFDFBF6FF60604E985B59468FEFCF99FFF3D5A4FFF4D8
      ABFFF5DBB2FFFAD3C1FFF6BDB7FFE96F6FFFE35554FFEB918CFFF7D8CEFFFBF2
      E3FFFCF5EAFFFDF9F1FFFDFBF6FF61614F990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000797979D1EFE7E2FFD69D74FFD69D
      74FFD69D74FFD69D74FFD69D74FFDBAC8BFFF6F6F6FFF6F6F6FFF7F7F7FFF7F7
      F7FFBEBE80FFF8F8F8FFF8F8F8FF777777CC6D694AACF2D19DFFF3D5A4FFF4D8
      ABFFF5DBB2FFF6DEB9FFF8E5C7FFEB7C7CFFE76B6AFFF9ECD8FFFAEFDCFFFBF2
      E3FFFCF5EAFFFDF9F1FFFEFCF8FF676753A06C684AABF2D19DFFF3D5A4FFF4D8
      ABFFF5DBB2FFF6DEB9FFF7D1C2FFE97070FFE25050FFF1B7AEFFFAEFDCFFFBF2
      E3FFFCF5EAFFFDF9F1FFFEFCF8FF686854A30000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000797979D1F0EBE7FFDFB79CFFDFB7
      9CFFE0B89DFFE0B89DFFE0B89DFFE4C3ADFFF6F6F6FFF6F6F6FFF7F7F7FFF7F7
      F7FFBEBE80FFF8F8F8FFF8F8F8FF777777CC5150447AE2C58BFCF3D5A4FFF4D8
      ABFFF5DBB2FFF6DEB9FFF8E5C6FFF09C9BFFEC9190FFF9ECD7FFFAEFDCFFFBF2
      E3FFFCF5EAFFFDF9F1FFBEBD9EEC37373549504F4379E0C58CFBF3D5A4FFF4D8
      ABFFF5DBB2FFF6DEB9FFF7E2C0FFF5C7BBFFF3C2B6FFF9EBD5FFFAEFDCFFFBF2
      E3FFFCF5EAFFFDF9F1FFC0BFA2ED3939364C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000797979D1F3F3F3FFF3F3F3FFF3F3
      F3FFF4F4F4FFF4F4F4FFF5F5F5FFF5F5F5FFF6F6F6FFF6F6F6FFF7F7F7FFEDED
      E1FFB6B66EFFE5E5CFFFF8F8F8FF777777CC010101026260479DC2AE78EFF4D8
      ABFFF5DBB2FFF6DEB9FFF7E2C0FFF8E5C7FFF8E8CEFFF9EBD5FFFAEFDCFFFBF2
      E3FFF6EFDFFF7C7B56C23D3D3A530000000000000001625F479CC2AD76EFF4D8
      ABFFF5DBB2FFF6DEB9FFF7E2C0FFF8E5C7FFF8E8CEFFF9EBD5FFFAEFDCFFFBF2
      E3FFF7F0E0FF7D7D57C23E3E3A54000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000059595997636363AA636363AA6363
      63AA636363AA636363AA636363AA636363AA636363AA636363AA636363AA6363
      63AA636363AA636363AA636363AA5757579200000000000000006A664AA8F2D7
      A8FFF5DBB2FFF6DEB9FFF7E2C0FFF8E5C7FFF8E8CEFFF9EBD5FFFAEFDCFFFBF2
      E3FFD7D2B3F82F2F2D3B0000000000000000000000000000000069664AA7F2D7
      A8FFF5DBB2FFF6DEB9FFF7E2C0FFF8E5C7FFF8E8CEFFF9EBD5FFFAEFDCFFFBF2
      E3FFD9D4B6F830302F3E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000101010126A67
      4AAA7B7554BB74714BBBF0DDB5FFF8E5C7FFF8E8CEFFF9EBD5FFFAEFDCFFFBF2
      E3FF818059C70000000100000000000000000000000000000000101010126966
      49A97B7554BB74704ABAF0DDB5FFF8E5C7FFF8E8CEFFF9EBD5FFFAEFDCFFFBF2
      E3FF82805AC80000000100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000050505066A684BACD3C598F7F8E8CEFFF9EBD5FFEEE4C8FE8482
      5BCA2525242C0000000000000000000000000000000000000000000000000000
      000000000000050505066A684BAAD1C496F6F8E8CEFFF9EBD5FFEEE4C9FE8583
      5BCA2525252D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000353532455B5A4A8E61604D97474740680404
      0405000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000343331435B5A4A8E61604D97474740680505
      0506000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000050505063F3F
      517237377BAC3E3E608B15151518000000000000000000000000000000000000
      000000000000000000001414141640474FA61B1B1B1F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002F31323F346287B5346787B5346C
      87B5347187B5347687B5347B87B53E4C4D6B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004F654FA52828283100000000000000000000000000000000000000000000
      0000010101023232324400000000000000004545457B4C4C4CA72D2D8FC80000
      FFFF0000FFFF0000FFFF1717BEE5111111130000000000000000000000000000
      000001010102323232441818181B417AB0FF245F88FE30535CD7333333450000
      0000000000000000000000000000000000000000000000000000000000000000
      00000101010232323244000000000000000045474883118DCAF200B6FFFF007C
      A2FF005D6EFF00E9FFFF06E8ECF82D2F2F3B0000000000000000000000000000
      0000010101023232324400000000000000004545457B505850BA4C574C94444A
      446C287928E92E742EDF28282831000000000000000000000000000000001818
      181B474747CB858585F44444447C303030424E4E4ED69191B8FF0000FFFF2A2A
      FFFF3333FFFF3333FFFF0404FFFF3F3F53750000000000000000000000001818
      181B474747CB858585F4444444861F7AA0FF00A4CAFF009ABCFF3B494CCF0000
      0000000000000000000000000000000000000000000000000000000000001818
      181B474747CB858585F44444447C313131434E4E4ED672B0BBFF00D7FCFF008A
      96FF005A5BFF00FFFFFF3E666690000000000000000000000000000000001818
      181B474747CB848484F44444447C313131434E4E4ED77CB67CFF009E00FF009D
      00FF009C00FF009B00FF2C8D2CE32A2B2A350000000000000000000000000505
      0506525252D2CACACAFF989898F6868686FD9B9B9BFF8585C1FF3333FDFF9999
      FFFFB6B6FFFFB6B6FFFF1414FFFF3E3E618D0000000000000000000000000505
      0506525252D3CACACAFF989898F62E757FFE00A7C9FF00B5E0FF0193B1FD2121
      2127383838510808080900000000000000000000000000000000000000000606
      0607525252D3CACACAFF989898F6868686FD9C9C9CFFB8B8B8FF1CA0A2EC0089
      89FF004747FF1CB9B9DE12121214000000000000000000000000000000000606
      0607535353D3CACACAFF989898F6868686FD9C9C9CFF81C981FF00CE00FF00CD
      00FF00CC00FF00CB00FF1BBB1BF1383C384E0000000000000000000000000707
      07084C4C4CCBB1B1B1FF535353FF2E2E2EFF292929FF393948FF4949F7FF1B1B
      FDFF0000FFFF0000FFFF0000FFFF333338480000000000000000000000000707
      07084C4C4CCBB1B1B1FF525252FF2E2E2EFF097B8FFF00A1C5FF00AFD8FF097C
      97FB828282F74A4A4AAA00000000000000000000000000000000000000000707
      07084C4C4CCBB1B1B1FF525252FF2E2E2EFF292929FF3E3E3EFF729090FF0389
      89FD005D5DFF3B6868C800000000000000000000000000000000000000000707
      07084C4C4CCBB0B0B0FF525252FF2E2E2EFF292929FF446844FF53B153FF3889
      38EE1ACF1AFE15C815FA393D394F00000000000000002F2F2F3E4F4F4FE14F4F
      4FD5B1B1B1FE454545FF373737FF353535F5323232F5272727FF1C1C61FF2E2E
      F7FF1313FEFF0000FBFE3F3F5B8000000000000000002F2F2F3F4F4F4FE14F4F
      4FD5B1B1B1FE454545FF373737FF353535F52C3233F6038FA7FF009BBFFF00A0
      C5FF318395FF444444D40000000100000000000000002F2F2F3F4F4F4FE14F4F
      4FD5B1B1B1FE454545FF373737FF353535F5313131F5272727FF272727FF50B9
      B9FF35D3D3FF434343D4010101020000000000000000303030404F4F4FE14F4F
      4FD5B1B1B1FE444444FF373737FF353535F5313131F5272727FF272727FF9D9D
      9DFF5FD55FFF4A694AE201010102000000000000000047474792DCDCDCFFE4E4
      E4FF7C7C7CFF3F3F3FFF424242C50202020300000000414141B4262626FF4E4E
      54FF434361E22020202600000000000000000000000047474792DCDCDCFFE4E4
      E4FF7C7C7CFF3F3F3FFF424242C50202020300000000324649CF009FBDFF00A5
      CCFF009ABCFF3F4B4D8300000000000000000000000047474793DDDDDDFFE4E4
      E4FF7C7C7CFF3F3F3FFF434343C40202020300000000414141B6262626FF5151
      51FF4A5050DD1414141700000000000000000000000047474793DDDDDDFFE4E4
      E4FF7D7D7DFF3F3F3FFF434343C40202020300000000414141B6262626FF5151
      51FF4D4D4DDC1414141700000000000000000000000029292933494949AE9999
      99F0646464FF424242FF2727273000000000000000000E0E0E0F282828FF3737
      37FF575757E11E1E1E2300000000000000000000000029292933494949AE9999
      99F0646464FF424242FF2727273000000000000000000E0E0E0F105761FF00A7
      C9FF00B7E3FF0296B8FC2D2E2E3B000000000000000029292933494949AE9A9A
      9AF0646464FF424242FF2727273000000000000000000E0E0E0F292929FF3737
      37FF575757E11E1E1E2300000000000000000000000029292934494949AF9A9A
      9AF0646464FF424242FF2525252E00000000000000000F0F0F10292929FF3737
      37FF575757E11E1E1E2300000000000000000000000000000000000000016161
      61DB747474FF464646FF4141416C0000000000000000373737502C2C2CFF4646
      46FFBDBDBDFF6A6A6AEA43434377000000000000000000000000000000016161
      61DB747474FF464646FF4141416D00000000000000003636364C2C2C2CFF0D83
      95FF00A6C9FF0CB3DAFF6B7C80F80F0F0F100000000000000000010101026161
      61DB747474FF464646FF4141416C0000000000000000383838522C2C2CFF4646
      46FFBDBDBDFF6A6A6AEA43434376000000000000000000000000010101026161
      61DC737373FF454545FF4141416B00000000000000003636364E2C2C2CFF4646
      46FFBDBDBDFF6A6A6AEA434343750000000000000000010101024848488BA6A6
      A6ECB3B3B3FF494949FF444444FA474747A44545459D373737F9303030FF8989
      89FFBFBFBFFFA1A1A1FD3C3C3C5D0000000000000000010101024848488BA6A6
      A6ECB3B3B3FF494949FF444444FA464646A44545459D383838F8303030FF7982
      83FF4193A0FF909394FF9191A9FF4B4B5CB000000000010101024848488BA6A6
      A6ECB3B3B3FF494949FF444444FA464646A44545459E383838F9303030FF8989
      89FFBFBFBFFFA1A1A1FD3B3B3B5B0000000000000000010101024848488BA6A6
      A6ECB3B3B3FF494949FF444444FA464646A44545459E383838F8303030FF8989
      89FFC1C1C1FFA1A1A1FD3B3B3B5B00000000000000000F0F0F115D5D5DDCF9F9
      F9FFE4E4E4FF919191FF474747FF424242FF3D3D3DFF383838FF6E6E6EFF6A6A
      6AE543434375444444770808080900000000000000000F0F0F115D5D5DDCF9F9
      F9FFE4E4E4FF919191FF474747FF424242FF3D3D3DFF383838FF6E6E6EFF6A6A
      6AE54A4A4AA97474A6FE54548DF020202026000000000F0F0F115E5E5EDCF9F9
      F9FFE4E4E4FF919191FF474747FF424242FF3D3D3DFF393939FF6F6F6FFF6A6A
      6AE543434374444444770808080900000000000000000F0F0F115E5E5EDCF9F9
      F9FFE4E4E4FF919191FF474747FF424242FF3D3D3DFF393939FF6F6F6FFF6A6A
      6AE54343437444444477080808090000000000000000000000004545457A5C5C
      5CB74646469D787878E2C7C7C7FF919191FF8C8C8CFFB3B3B3FFE1E1E1FF4949
      49C5000000000000000000000000000000000000000000000000444444775B5B
      5BB84646469E787878E2C6C6C6FF919191FF8C8C8CFFB3B3B3FFE0E0E0FF4949
      49C50000000021212128040404050000000000000000000000004545457A5C5C
      5CB74646469E787878E3C6C6C6FF919191FF8C8C8CFFB3B3B3FFE0E0E0FF4848
      48C6000000000000000000000000000000000000000000000000454545795B5B
      5BB84747479D787878E3C6C6C6FF919191FF8C8C8CFFB3B3B3FFE0E0E0FF4949
      49C5000000000000000000000000000000000000000000000000000000000000
      0000000000004D4D4DAAF8F8F8FF9A9A9AEE515151BB545454CAE0E0E0FF6464
      64EA1F1F1F250000000000000000000000000000000000000000000000000000
      0000000000004C4C4CABF8F8F8FF9A9A9AEF515151BB545454CAE0E0E0FF6464
      64EA1F1F1F250000000000000000000000000000000000000000000000000000
      0000000000004C4C4CABF8F8F8FF9A9A9AEE515151BB545454CAE0E0E0FF6464
      64EA1F1F1F250000000000000000000000000000000000000000000000000000
      0000000000004D4D4DABF8F8F8FF9A9A9AEF505050BA545454CAE0E0E0FF6262
      62EA1E1E1E240000000000000000000000000000000000000000000000000000
      0000000000004B4B4B9B8F8F8FED5C5C5CB0000000001919191D434343CA3C3C
      3C5B000000000000000000000000000000000000000000000000000000000000
      0000000000004C4C4C9C8F8F8FED5C5C5CAF000000001919191D434343CA3C3C
      3C5B000000000000000000000000000000000000000000000000000000000000
      0000000000004B4B4B9C909090EE5C5C5CAF000000001919191D434343CA3C3C
      3C5B000000000000000000000000000000000000000000000000000000000000
      0000000000004C4C4C9C909090EE5C5C5CAF000000001919191D434343CA3C3C
      3C5B000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E1E1E241919191D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E1E1E241919191D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E1E1E241919191D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E1E1E241919191D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A2B2A363E513E90286228CD146C14E8156C15E7295F29CB3E4F3E8C2626
      262F000000000000000000000000000000000000000000000000040404050F0F
      0F110F0F0F110F0F0F110B0B0B0C00000000000000000C0C0C0D0F0F0F110F0F
      0F110F0F0F110303030400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000001393E
      39583E543E8D3C443C6405050506000000000000000000000000080808093D51
      3D97058A05F900A200FF00AC00FF00B100FF00AF00FF00A700FF009A00FF087D
      08F63E4F3E8D04040405000000000000000000000000000000003E474A6B0C74
      CFF20C72CFF20C6FCFF22F6181C000000000000000002A6389C90C6FCFF20C72
      CFF20C75CFF23B42435F00000000000000000000000000000000000000000000
      0000010101023232324400000000000000004545457D4B4B4BA7323232430000
      0000000000000000000000000000000000000000000000000000000000000000
      0000010101023232324400000000000000004545457D4C4C4CA7316A31C000BC
      00FF6BD96BFF00BC00FF2C6E2CC604040405000000000909090A315C31BB00A2
      00FF00B800FF00BC00FF12C012FF00BC00FF00BC00FF00BC00FF00BC00FF00AA
      00FF009200FF365736B0040404050000000000000000000000003E484B6C008A
      FFFF0085FFFF0081FFFF306486BE00000000000000002A6892C90081FFFF0085
      FFFF008AFFFF3C43446100000000000000000000000000000000000000001919
      191D484848CC848484F343434379313131434F4F4FD8B3B3B3FF484848B70000
      0000000000000000000000000000000000000000000000000000000000001919
      191D484848CC848484F343434379313131434F4F4FD88BA88BFF00BC00FF00BC
      00FF97E497FF00BC00FF00BC00FF3A403A5B000000003C533C9900A600FF00BC
      00FF00BC00FF00BC00FF58CF58FF7DD37DFF00BC00FF00BC00FF00BC00FF00BC
      00FF00AF00FF009100FF3E4E3E8B0000000000000000000000003E484B6C0099
      FFFF0094FFFF0090FFFF306786BE00000000000000002A6D92C90090FFFF0095
      FFFF0099FFFF3C43446100000000000000000000000000000000000000000707
      0708535353D4CACACAFF979797F6858585FD9B9B9BFFB8B8B8FF444444C30B0B
      0B0C383838520606060700000000000000000000000000000000000000000707
      0708545454D5CACACAFF979797F6858585FD9C9C9CFF74AA74FF62D562FF96E3
      96FFD4F4D4FF96E396FF58D458FF3F4F3F7E2B2C2B38049604FA00BC00FF00BC
      00FF00BC00FF00BC00FF5CD35CFFF3F3F3FF80D480FF00BC00FF00BC00FF00BC
      00FF00BC00FF00A800FF097709F52424242C00000000000000003E484B6C00A8
      FFFF00A4FFFF009FFFFF306C86BE00000000000000002A7192C900A0FFFF00A4
      FFFF00A8FFFF3C43446100000000000000000000000000000000000000000808
      08094C4C4CCDAFAFAFFF515151FF2E2E2EFF292929FF404040FF8A8A8AFF4B4B
      4BDD858585F84A4A4AA700000000000000000000000000000000000000000808
      08094C4C4CCDAFAFAFFF515151FF2E2E2EFF292929FF364A36FF4DCE4DFF0EBF
      0EFF97E497FF00BC00FF00BC00FF313431443D543D9500B600FF00BC00FF00BC
      00FF00BC00FF00BC00FF5ED55EFFFCFCFCFFF7F7F7FF82D682FF00BC00FF00BC
      00FF00BC00FF00BB00FF009500FF3F4E3F8700000000000000003E494B6C00B7
      FFFF00B3FFFF00AEFFFF307086BE00000000000000002A7692C900AFFFFF00B3
      FFFF00B8FFFF3C43446100000000000000000000000032323245515151E14F4F
      4FD6B1B1B1FE444444FF363636FF343434F5313131F5272727FF292929FF9F9F
      9FFFAFAFAFFF424242D300000001000000000000000032323245515151E15050
      50D6B2B2B2FE444444FF363636FF343434F5313131F5272727FF1B6A1BFF2EC6
      2EFF53D353FF00BB00FF3E573E8E00000000247124D300BC00FF00BC00FF00BC
      00FF00BC00FF00BC00FF5DD45DFFFEFEFEFFFDFDFDFFFAFAFAFF84D884FF00BC
      00FF00BC00FF00BC00FF00A200FF2C5D2CC500000000000000003E494B6C00C6
      FFFF00C2FFFF00BEFFFF307386BE00000000000000002A7C92C900BEFFFF00C2
      FFFF00C7FFFF3C44446100000000000000000000000047474797DFDFDFFFE4E4
      E4FF7A7A7AFF3F3F3FFF434343C102020203000000003F3F3FBB262626FF5454
      54FF4B4B4BDA1313131500000000000000000000000047474798DFDFDFFFE4E4
      E4FF7A7A7AFF3F3F3FFF434343C102020203000000003F3F3FBB262626FF4A5B
      4AFF395D39E52C2D2C390000000000000000108910ED20C320FF00BC00FF00BC
      00FF00BC00FF00BC00FF5CD35CFFFBFBFBFFFEFEFEFFFEFEFEFFFAFAFAFF74D7
      74FF00BC00FF00BC00FF00A900FF1B651BDF00000000000000003E4A4B6C00D6
      FFFF00D1FFFF00CDFFFF307886BE00000000000000002A8092C900CDFFFF00D2
      FFFF00D6FFFF3C4444610000000000000000000000002A2A2A35494949B09E9E
      9EF2626262FF424242FF2323232B000000000000000012121214282828FF3A3A
      3AFF555555E01C1C1C210000000000000000000000002A2A2A35494949B09E9E
      9EF2626262FF424242FF2323232A000000000000000012121214282828FF3A3A
      3AFF555555E01C1C1C2100000000000000000F8E0FEE67D367FF2FC72FFF00BC
      00FF00BC00FF00BC00FF5AD15AFFF8F8F8FFFBFBFBFFFDFDFDFFE8F9E8FF27C6
      27FF00BC00FF00BC00FF00AB00FF1B661BE000000000000000003E4A4B6C00E5
      FFFF00E1FFFF00DCFFFF307C86BE00000000000000002A8592C900DCFFFF00E1
      FFFF00E5FFFF3C44446100000000000000000000000000000000020202036464
      64DD707070FF454545FF3F3F3F6500000000000000003A3A3A572C2C2CFF4949
      49FFBCBCBCFF686868E942424272000000000000000000000000020202036464
      64DD707070FF454545FF3F3F3F650000000000000000393939532C2C2CFF4949
      49FFBCBCBCFF686868E94242427100000000237923D455D055FF76D676FF03BD
      03FF00BC00FF00BC00FF59D059FFF4F4F4FFF7F7F7FFE3F4E3FF28C728FF00BC
      00FF00BC00FF00BC00FF00A700FF2C5D2CC500000000000000003E4B4B6C00F4
      FFFF00F0FFFF00EBFFFF308086BE00000000000000002A8A92C900ECFFFF00F0
      FFFF00F4FFFF3C444461000000000000000000000000010101024848488EAAAA
      AAEEB1B1B1FF494949FF444444FA464646A34545459F383838F9303030FF8C8C
      8CFFBFBFBFFF9C9C9CFC3A3A3A560000000000000000010101024848488FAAAA
      AAEEB0B0B0FF494949FF444444FA464646A34545459F393939F8303030FF8C8C
      8CFFC1C1C1FF9C9C9CFC3A3A3A56000000003D593D961FC31FFF87DA87FF3FCB
      3FFF00BC00FF00BC00FF59D059FFF2F2F2FFDDEEDDFF27C627FF00BC00FF00BC
      00FF00BC00FF00BC00FF009E00FF3F4E3F8900000000000000003E4B4B6C00FF
      FFFF00FFFFFF00FBFFFF308486BE00000000000000002A8E92C900FBFFFF00FF
      FFFF00FFFFFF3C44446100000000000000000000000011111113606060DEF9F9
      F9FFE4E4E4FF8F8F8FFF474747FF424242FF3D3D3DFF393939FF717171FF6767
      67E3424242724444447707070708000000000000000011111113616161DEF9F9
      F9FFE4E4E4FF8F8F8FFF474747FF424242FF3D3D3DFF393939FF717171FF6666
      66E3424242724444447707070708000000002C2D2C3A04AF04FA5DD15DFF73D6
      73FF2CC72CFF00BC00FF59D059FFDCEDDCFF26C526FF00BC00FF00BC00FF00BC
      00FF00BC00FF00B700FF088308F62525252D00000000000000003E4B4B6C00FF
      FFFF00FFFFFF00FFFFFF308686BE00000000000000002A9292C900FFFFFF00FF
      FFFF00FFFFFF3C444461000000000000000000000000000000004646467E5A5A
      5AB64646469E7A7A7AE3C5C5C5FF909090FF8C8C8CFFB5B5B5FFE0E0E0FF4949
      49C30000000000000000000000000000000000000000000000004646467C5A5A
      5AB74646469E7A7A7AE3C5C5C5FF909090FF8C8C8CFFB5B5B5FFE0E0E0FF4848
      48C300000000000000000000000000000000000000003C5B3C9B08BE08FF5CD1
      5CFF61D261FF3CCA3CFF4BCD4BFF22C422FF00BC00FF00BC00FF00BC00FF00BC
      00FF00BC00FF00A200FF3E4F3E8D0000000000000000000000003E4B4B6C00FF
      FFFF00FFFFFF00FFFFFF308686BE00000000000000002A9292C900FFFFFF00FF
      FFFF00FFFFFF3C44446100000000000000000000000000000000000000000000
      0000000000004D4D4DADF8F8F8FF969696ED505050BA545454CBE1E1E1FF6060
      60E91C1C1C210000000000000000000000000000000000000000000000000000
      0000000000004D4D4DADF8F8F8FF959595ED505050BA555555CCE1E1E1FF5F5F
      5FE81C1C1C20000000000000000000000000000000000A0A0A0B306F30BD02BD
      02FF2EC62EFF4BCD4BFF3CC93CFF24C424FF0BBF0BFF00BC00FF00BC00FF00BC
      00FF00AB00FF355B35B2050505060000000000000000000000003E4B4B6C00FF
      FFFF00FFFFFF00FFFFFF308686BE00000000000000002A9292C900FFFFFF00FF
      FFFF00FFFFFF3C44446100000000000000000000000000000000000000000000
      0000000000004B4B4B9F919191EF5B5B5BAC000000001B1B1B1F444444CB3B3B
      3B58000000000000000000000000000000000000000000000000000000000000
      0000000000004C4C4C9F919191EF5A5A5AAC000000001B1B1B1F434343CA3B3B
      3B580000000000000000000000000000000000000000000000000A0A0A0B3C5B
      3C9904AF04FA03BD03FF12C112FF12C012FF09BE09FF00BC00FF00BC00FF079A
      07F73E513E9006060607000000000000000000000000000000002F31313F3B68
      689F3B68689F3B68689F3F57578400000000000000003E5B5B8C3B68689F3B68
      689F3B68689F2A2B2B3500000000000000000000000000000000000000000000
      000000000000000000001F1F1F251818181C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001F1F1F251818181C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002B2C2B373E583E92267D26CF149014E8158B15E7297329CB3E533E8E2728
      2731000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000059575397B2A48AF1B2A2
      88F1B2A185F1B2A083F1B1A081F1B19E7EF1B09E7CF1B09D79F1B09C76F1B09B
      75F1B09A71F1B09A71F156524D8F00000001525244815757448C5757448C5757
      448C5757448C5757448C5757448C5757448C5757448C5757448C5757448C5757
      448C5757448C5757448C5757448C4F4F437C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005E5B5794F8E4BFFFF8E2
      BCFFF7E1B8FFF7DFB5FFF7DEB2FFF6DCAEFFBCBD73FFAF8051FFC47A5DFFE3B6
      89FFF4D59DFFF4D39AFF59554F8C0404040590904EE5EEEDDBFFEEEDDAFFEEEC
      DAFFEEECD9FFEEECD7FFEDEBD6FFEDEBD5FFEDEBD4FFEDEBD3FFEDEBD3FFEDEA
      D2FFEDEAD1FFEDEAD1FFEDEAD1FF8B8B47E20000000000000000000000000000
      00004A4A426D878766C9878766C9878766C9878766C9878765C9878665C98786
      63C9878663C9878663C97B7B4CCA010101020000000000000000000000000000
      00004A4A426D878766C9878766C9878766C9878766C9878765C9878665C98786
      63C9878663C9878663C97B7B4CCA01010102000000005E5B5794F9E5C3FFF8E4
      C0FFF8E2BDFFF7E1B9FFF4D9B1FF906A34FF00E900FF04E300FF6E1C00FF8C00
      00FFB35741FFF1D19AFF59554F8C04040405919158E1FFFFFEFFFFFEFDFFFFFE
      FCFFFFFDFBFFFFFDF9FFFFFDF8FFFEFCF7FFFEFCF6FFFEFBF5FFFEFBF4FFFEFB
      F3FFFEFAF1FFFEFAF1FFFEFAF1FF8C8B4FDE0000000000000000000000000000
      000057574987FFFFFEFFFFFEFCFFFFFDFBFFFFFDF9FFFEFCF7FFFEFCF5FFFEFB
      F4FFFEFAF2FFFEFAF1FFA6A47AE3020202030000000000000000000000000000
      000057574987FFFFFEFFFFFEFCFFFFFDFBFFFFFDF9FFFEFCF7FFFEFCF5FFFEFB
      F4FFFEFAF2FFFEFAF1FFA6A47AE302020203000000005E5B5894F9E7C8FFF9E6
      C4FFF8E4C1FFF8E3BEFFB35B4BFF2BA700FF2BAF00FF921200FF890000FF9E05
      00FF881800FF838D34FF59554F8C04040405919158E1FFFFFFFFFFFFFEFFFFFE
      FDFFFFFEFCFFFFFDFAFFFFFDF9FFFFFDF8FFFEFCF7FFFEFCF6FFFEFBF5FFFEFB
      F3FFFEFAF2FFFEFAF1FFFEFAF1FF8C8B4FDE0000000000000000000000000000
      000057574987FFFFFFFFFFFFFEFFFFFEFCFFFFFDFAFFFFFDF9FFFEFCF7FFFEFB
      F5FFFEFBF3FFFEFAF2FFA6A47AE3020202030000000000000000000000000000
      000057574987FFFFFFFFFFFFFEFFFFFEFCFFFFFDFAFFFFFDF9FFFEFCF7FFFEFB
      F5FFFEFBF3FFFEFAF2FFA6A47AE302020203000000005E5C5994F9E9CCFFF9E7
      C8FFF9E6C5FFEFD2B2FF890F00FF882F00FFC20100FFD60000FFD50000FF7161
      00FF07E100FF0BD800FF5C59509A04040405919158E1FFFFFFFFFFFFFFFFFFFF
      FEFFFFFEFDFFFFFEFBFFFFFDFAFFFFFDF9FFFEFCF8FFFEFCF7FFFEFCF6FFFEFB
      F4FFFEFBF3FFFEFAF2FFFEFAF1FF8C8B4FDE000000000F0F0F100F0F0F110F0F
      0F115B5B4A8FFFFFFFFFFFFFFFFFFFFFFEFFFFFEFCFFFFFDFAFFFFFDF8FFFEFC
      F7FFFEFBF5FFFEFBF3FFA6A47AE302020203000000000F0F0F100F0F0F110F0F
      0F115B5B4A8FFFFFFFFFFFFFFFFFFFFFFEFFFFFEFCFFFFFDFAFFFFFDF8FFFEFC
      F7FFFEFBF5FFFEFBF3FFA6A47AE302020203000000005E5C5994FAEBD0FFFAE9
      CDFFF9E8C9FFDAAA92FF694400FF8A3500FFCA0000FFE70000FFED0000FFA72D
      00FF00EA00FF00ED00FF5B6446B504040405919158E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFEFDFFFFFEFCFFFFFEFBFFFFFDFAFFFFFDF9FFFEFCF8FFFEFCF7FFFEFC
      F5FFFEFBF4FFFEFBF3FFFEFAF2FF8C8B4FDE0D0D0D0E95955CE3B3B28DE9B3B2
      8DE9ACAB69F5FFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFFFFFEFCFFFFFDFAFFFFFD
      F8FFFEFCF6FFFEFBF5FFA6A57BE3020202030E0E0E0F95955DE3B3B28DE9B3B2
      8DE9ACAB69F5FFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFFFFFEFCFFFFFDFAFFFFFD
      F8FFFEFCF6FFFEFBF5FFA6A57BE302020203000000005E5D5994FAEDD4FFFAEB
      D1FFFAEACDFFDEB59EFF399000FF09DD00FF4E9100FF831700FF9E0000FF6E46
      00FF5C6E00FF27AC00FF5D5D4CA704040405919158E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFEFFFFFEFDFFFFFEFCFFFFFEFBFFFFFDFAFFFFFDF9FFFEFCF8FFFEFC
      F6FFFEFCF5FFFEFBF4FFFEFBF3FF8C8B4FDE0E0E0E0FB4B491E9FFFEFDFFFFFE
      FCFFCECD9BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFFFFFEFBFFFFFD
      FAFFFEFCF8FFFEFCF6FFA6A57CE3020202030F0F0F10B4B492E9FFFEFDFFFFFE
      FCFFCECD9BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFFFFFEFBFFFFFD
      FAFFFEFCF8FFFEFCF6FFA6A57CE302020203000000005E5D5A94FBEED8FFFAED
      D5FFFAEBD2FFF8E5CAFF605F0BFF05E300FF4E8800FF735100FF871A00FF7450
      00FF11D000FF28B205FF5956518C04040405919158E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFEFFFFFEFDFFFFFEFCFFFFFEFBFFFFFDFAFFFFFDF9FFFEFC
      F7FFFEFCF6FFFEFBF5FFFEFBF4FF8C8B4FDE0E0E0E0FB4B491E9FFFFFFFFFFFE
      FDFFCBCB94FFEEEEDDFFEEEEDDFFEEEEDDFFEEEEDDFFEEEEDDFFEEEDDBFFEEED
      DAFFEEECD8FFEDEBD7FFA5A56FEA020202030F0F0F10B4B492E9FFFFFFFFFFFE
      FDFFCBCB94FFEEEEDDFFEEEEDDFFEEEEDDFFEEEEDDFFEEEEDDFFEEEDDBFFEEED
      DAFFEEECD8FFEDEBD7FFA5A56FEA03030304000000005E5D5A94FBF0DDFFFBEF
      D9FFFBEDD6FFFAECD2FFD39F8CFF4B7300FF30A500FF596800FF813200FF477C
      00FF07DF00FF96AB55FF5956528C04040405919158E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFEFDFFFFFEFCFFFFFDFBFFFFFDF9FFFFFD
      F8FFFEFCF7FFFEFCF6FFFEFBF5FF8C8B4FDE0E0E0E0FB4B491E9FFFFFFFFFFFF
      FFFFC3BB78FFC09F47FFBF9E44FFBE9C41FFBD9B3FFFB89838FFB69534FFB594
      32FFB59331FFB59331FF887D27E8010101020F0F0F10B4B492E9FFFFFFFFFFFF
      FFFFC3BB78FFC09F47FFBF9E44FFBE9C41FFBD9B3FFFB89838FFB69534FFB594
      32FFB59331FFB59331FF887D27E802020203000000005E5D5B94FCF2E1FFFBF1
      DDFFFBEFDAFFFBEED7FFFAECD3FFD6AD97FF6E681CFF683C00FF4F6600FF30A9
      08FF99A257FFF8E2BCFF5956528C04040405919158E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFEFDFFFFFEFCFFFFFDFAFFFFFD
      F9FFFFFDF8FFFEFCF7FFFEFCF6FF8C8B50DE0E0E0E0FB4B491E9FFFFFFFFFFFF
      FFFFCCC890FFE9CA9BFFEDC799FFEAC08BFFD5B169FFB6AE5BFFD9D2A5FF6865
      43AE42413C5D42413C5D2525252D000000000F0F0F10B4B492E9FFFFFFFFFFFF
      FFFFCCC890FFE9CA9BFFEDC799FFEAC08BFFD5B169FFB6AE5BFFD9D2A4FF6765
      43AC42413C5D42413C5D2525252D00000000000000005E5D5B94FCF4E5FFFCF2
      E2FFFBF1DEFFFBEFDBFFFBEED7FFFAECD4FFFAEBD1FFF6E1C5FFEBDFBAFFF9E7
      C7FFF9E5C3FFF8E4C0FF5956538C04040405929247ECE6E6CCFFE6E6CCFFE6E6
      CCFFE6E6CCFFE6E6CCFFE6E6CCFFE6E6CCFFE6E6CBFFE6E5CAFFE6E5C9FFE6E4
      C8FFE6E4C7FFE5E4C6FFE5E4C6FF8D8D42E90E0E0E0FB4B491E9FFFFFFFFFFFF
      FFFFF9F9F3FFD2D09FFFD4CE9DFFD4CD9AFFDBD7ADFFFEFCF7FFFDFBF4FF5352
      467F000000000000000000000000000000000F0F0F10B4B492E9FFFFFFFFFFFF
      FFFFF9F9F3FFD2D09FFFD4CE9DFFD4CD9AFFDBD7ADFFFEFCF7FFFDFBF3FF5252
      467D00000000000000000000000000000000000000005E5D5C94FDF6E9FFFCF4
      E6FFFCF3E2FFFCF1DFFFFBF0DCFFFBEED8FFFAEDD5FFFAEBD2FFFAEACEFFF9E8
      CBFFF9E7C7FFF9E6C4FF5956538C0606060787811FF0A59236F8A59135F8A490
      34F8A39033F8A38F32F8A38E31F8A28E30F8A28D2FF8A18D2EF8A18D2EF8A18D
      2EF8A18D2EF8A18D2EF8A18D2EF8847D1CEE1111111399984EEFB0B07BF1B0B0
      7BF1B0B07BF1B0B07BF1B0B07BF1B0B07AF1B0AF79F1B0AF77F1B0AF76F15858
      458E00000000000000000000000000000000121212149A994FEFB0B07BF1B0B0
      7BF1B0B07BF1B0B07BF1B0B07BF1B0B07AF1B0AF79F1B0AF77F1B0AF76F15858
      468C00000000000000000000000000000000000000005E5E5D94FDF7EDFFFDF6
      EAFFFCF4E7FFFCF3E3FFFCF2E0FFFBF0DCFFFBEFD9FFFBEDD6FFE5D3B8FFB990
      64FFB99064FFB98F66FE504E4A7D040404058C823BE1E8BB84FFE6B679FFE4B1
      71FFE2AD69FFE1AB67FFDEA861FF8F812EE98A7C2DE4897B2DE4897B2DE4897A
      2CE4897A2BE4887A2AE4867A28E4585640900E0E0E0FA6904FE8E2AE6CFFE1AB
      66FFDFA862FFC1974EF6A18740E8A1863EE8A0843CE89F833AE8947D38E23837
      344A000000000000000000000000000000000F0F0F10A88F50E9E2AE6CFFE1AB
      66FFDFA862FFC0984EF5A18740E8A1863EE8A0843CE89F833AE8937D37E23737
      344900000000000000000000000000000000000000005E5E5D94FEF9F2FFFDF8
      EEFFFDF6EBFFFCF5E7FFFCF3E4FFFCF2E1FFFBF0DDFFFBEFDAFFDFCCB3FFE3A1
      5EFFE1A05EFF6F6456B600000000040404058C8737E8EED0A7FFF0CEA3FFEEC9
      9BFFECC492FFE8BD86FF988A3FE9595840940000000035353949424268964242
      68964242689642426896424268964747618A0B0B0B0C8F894DDEEBCEA0FFE8C6
      93FFCCAE72F8676443AB0F0F0F110F0F0F110F0F0F1143434D6D43436A9B4242
      6896424268964242689642426896444453730C0C0C0D91894EDEEBCEA0FFE8C6
      93FFCCAE72F8676443AB0F0F0F110F0F0F110F0F0F110F0F0F110C0C0C0D0000
      000000000000000000000000000000000000000000005E5E5E94FEFBF6FFFEFA
      F2FFFDF8EFFFFDF7ECFFFDF5E8FFFCF4E5FFFCF2E1FFFBF1DEFFDFCDB5FFE09F
      5EFF6D6357B400000000000000000000000043433D617F7B3AD98A8445DD8984
      43DD898342DD807C37DB32323040040404050000000047475A782B2BE0EF2B2B
      E0EF2B2BE0EF2B2BE0EF2B2BE0EF4444BDD9000000001B1B1B1F4949416C4949
      416C3F3F3B58000000000000000000000000000000004B4B769C2B2BE0EF2B2B
      E0EF2B2BE0EF2B2BE0EF2B2BE0EF4A4A92B8000000001B1B1B1F4949416C4949
      416C3F3F3B580000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A5A5A97B6B5B3F1B6B3
      B2F1B6B3B0F1B5B2ADF1B5B2AAF1B5B1A8F1B4B0A6F1B4B0A4F1AC9F8FF26A60
      55AE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000464646691D1D1D21000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001717171A5C6158AC6C665BD1715B5BD0674E4EC24A3F3F733E3D3D595656
      568B5A5A5A925959598E3B3B3B52000000000000000000000000000000000000
      000000000000000000001818181B44443D6846463E6C46463E6C45453E6A1111
      111300000000000000000000000000000000000000003D453D69375837AE1A1A
      1A1E000000004F4F4F882B2B2B350000000010101012494946644C4C48694C4C
      48694646435F0000000000000000000000000D0D0D0E42423D6946463F774646
      3F7746463F7746463F7746463F7746463F7746463F7746463F7746463F774749
      3E8C434A3AA246463F7741413D680A0A0A0B0000000000000000000000002928
      28338C9680F9A7BFA7FF81B381FF8AA982FFBFBAB9FF8D6363FFBAB2B2FF9B8E
      8EF3817E7ED0848484CEC9C9C9F93E3E3E580000000000000000000000000000
      0000000000004F4F3F8977770FEE949400FF9C9C00FFA6A600FFAFAF00FF5858
      3D9600000000000000000000000000000000000000003E563E9200A300FF2065
      20E21818181B4F4F4F882B2B2B352F2F2E3A8A8A49DD949448E395954EDA9999
      4EDA98985BD10F0F0F1100000000000000005C5C43D4727245FF707015FF7575
      0EFF7C7C0EFF818111FF9A9A80FFB7B7A2FFB7B7A2FFB7B7A2FFB7B7A2FF6B89
      5FFF027001FF658559FFB7B7A2FF5F5F49D200000000000000003C3838536138
      10FAD2D2D2FF22C522FF02E301FF53B24BFF8F8F79FFAAA6A6FFABA4A4FFA05C
      5CFF802F2FF2323130427A7A7ABF6363639C0000000000000000222222292C58
      2CC63D5928CD7C7C06F85B5B37AD3D3D39583A3A37503A3A37503939364F0909
      090A00000000000000000000000000000000000000003E563E9200CA00FF00A9
      00FF2A5F2AC83A3B3A52606060AC717156B5767658B60E0E0E0F000000000000
      000000000000000000000000000002020203585824F9797918FFA3A386FFADAD
      96FFAFAF96FFB6B6A0FFFBFBFBFFFFFFFFFFA2BBA2FF74A674FF74A674FF4E8B
      4EFF007300FF037003FF8DAB8DFF75754BF1000000002B2A2A36840807F71FB1
      0DFFDADADAFF26BD21FF5D6309FF9E7B7BFFA99B9BFFB5B3B3FFB6B2B2FFB692
      92FF964646FF66360EF07F827CCE6464649D000000000000000030313040008F
      00FE3E7900FF6A7105F91111111300000000000000000F0F0F1046463E6B0101
      010200000000000000000000000000000000000000003E543E9200C200FF00D2
      00FF00B100FF305C30BE48484476979741FB9F9F40FEA0A003FB5A5A38AA4646
      435F7F7F5BB41010101200000000030303045A5A15FC949463FFFFFFFFFFFFFF
      FFFFDFDFD9FFABAB85FFFEFEFEFFFFFFFFFF60A860FF009B00FF009B00FF009A
      00FF009900FF009900FF039403FF44692BF8000000016A2B2BC7910100FF20B7
      00FF9CB896FFBDAFA7FFB17171FFAB7474FFCCBFBFFFA17272FFC0BABAFF939B
      7DFF8D896BFF9E857AFFCED1CDFE4444446300000000000000003031304000A3
      00FE398600FF727D00FF325829CA06060607000000002C2C2B38A8A800FE5B5B
      3BA001010102000000000000000000000000000000003E523E9200A900FF00BD
      00FF00C800FE31632DCE797904FC9E9E48FF9A9A2FFFC5C53FFF9B9B3FFF9898
      64DBBFBF00FF8C8C57C02626262E3C3C3C5459591DFB818132FFE3E3E0FFFFFF
      FFFFBEBEB0FFB1B100FF9E9E64FFFEFEFEFF60B960FF00C100FF00C000FF00BF
      00FF00BF00FF00BE00FF00BD00FF10920AFD363434498C0000FF822400FF961A
      00FF9F1F0EFFB25B5BFFB56565FFB76666FFB64E4EFFC60404FF933F23FF5CBC
      5CFF60BA60FF67B461FF35C233FF3637354B00000000000000003031304000AE
      00FE10B400FF718300FF678100FE506518E358583C9A5C5C38AAB4B400FFBFBF
      00FE5D5D3BA0010101020000000000000000000000003E513E92008B00FF00A0
      00FF326732C86F701BE7A5A51DFF747429FFB6B653FFACAC40FFBABA3FFFBBBB
      40FDC1C100FFB4B435E87C7C6FC72424242B6A6A3CF67C7C39FF7A7A0AFF8383
      17FF8B8B15FFBEBE00FFC0C000FFABAB89FF60C160FF00D200FF00D200FF00D2
      00FF00D200FF00D200FF1FC01FFF5F723DF5503F3F828F0200FF546C00FF9A1D
      00FFBB0400FFC70000FFDC0000FFE50000FFE40000FFDB0000FFB31D00FF00EA
      00FF00ED00FF00EB00FF00ED00FF485F39A600000000000000003031304000A1
      00FE00CC00FF1EAD00FF5C8A00FF7B8A00FE848907F7888815E7BABA00FFC7C7
      00FF797926CF101010120000000000000000000000003E513E92007D00FF335C
      33C14040405B8F8F0CFB949425FFA1A122FF747406FF9F9F02FFA0A041FFA9A9
      44F8BDBD2FF8828270CA3A3A3A5003030304737342F5FCFCFCFFC3C3B8FFB3B3
      A2FF87876EFFBABA00FF99994DFFF9F9F9FFD7E4D7FFC0D8C0FFC0D8C0FF78B6
      78FF00D200FF1FBE1FFFCDE0CDFF75754AF35C3A3AA2575C00FF9A0D00FF912D
      00FFB90000FFD30000FFE60000FFEA0000FFF20000FFE40000FFD30000FF34AC
      00FF00ED00FF00ED00FF00ED00FF41732EC20000000000000000303130400094
      00FE00BB00FF00C900FF00D000FF00BA00FF0D790DF143483F74B3B300FF7A7A
      27CE1D1D1D22000000000000000000000000000000003B403B5E3B653BDA5354
      539A4646466C93930DF6CACA24FF7C7C0BFFA4A414FFCDCD66FFC3C394FF7777
      48CB3131303D000000000000000003030304737342F5FFFFFFFFFFFFFFFFFFFF
      FFFFD5D5CCFFA0A066FFF9F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9AC3
      9AFF29AD29FFCDE0CDFFFFFFFFFF75754AF35C3A3AA3467700FF28B000FF04E5
      00FF588100FFC80F00FFD40000FF9A0000FFE10000FFD30000FFA90D00FF3E9F
      00FF25B900FF19C600FF00EA00FF466633B80000000000000000303130400086
      00FE00AA00FF00B800FF00C600FF06B306F83A553AA21F1F1F25626236B01C1C
      1C200000000000000000000000000000000000000000545454863838384C0000
      000042423E6A626207F7757511FFC7C756FFBEBE87FFDFDF9BFFB2B26CF51515
      151800000000000000000000000003030304737342F5FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3503F3F834C6B00FF00EC00FF05E3
      00FF22BF00FF25C000FF606000FF800000FF820000FF810000FF7C0600FF34AA
      00FF803D00FF793400FF1CBB00FF4A533E8F0000000000000000303130400077
      00FE009600FF00A600FF03A103FB375837AE0101010200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000316E4EEB696900FF757502FF989844F2AAAA59F673734BC7616160B91313
      13153030303C4F4F4F863131313F03030304727242F5EEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFF747449F3302E2E3D673900FF00ED00FF04E4
      00FF1BC700FF29BA00FF844C00FF731A00FF800000FF800000FF714E00FF26B9
      00FF08DF00FF329C00FF11CC00FF282928320000000000000000303130400073
      00FE008100FF018B01FD335833B7030303040000000000000000000000000000
      0000000000000000000000000000000000002D2D2D3841414161464646695959
      599B565B39CB37874CFF525233B6020202035151518932323241292929326363
      63B1505050883232324000000001060606077A633FFC92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF775F3DFA000000016C2828CC2CA300FF00E9
      00FF21BC00FF824200FF795300FF3D9900FF734C00FF9C1500FF784A00FF3E90
      00FF02E600FF309C00FF339015E7000000000000000000000000303130400073
      00FE007A00FE2F562FBF07070708000000000000000000000000000000000000
      0000000000000000000000000000000000002323232A3E3E3E583D3D3D562626
      262E1919191D575B59B100000001000000004242425F42424261000000000000
      000000000000000000000000000001010102A78F6FFBE9C2A5FFE8C1A3FFE8C0
      A2FFE7BFA0FFE7BE9EFFE6BD9CFFE6BB9AFFE5BA99FFE5B997FFE5B895FFE4B7
      93FFE4B691FFE3B58FFFE3B48EFF9C805AF9000000002C2B2B37820806F828AA
      00FF08DD00FF2AAF00FF655F00FF28AC00FF498000FFAA0000FF3D8D00FF10D0
      00FF00EA00FF16C300FE3A3B3853000000000000000000000000282928332659
      26CF335533B70A0A0A0B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000484848703E3E3E5500000000000000004E4E4E862C2C2C37000000000000
      00000000000000000000000000000000000055534C9EA19581F6A79988F9A698
      86F9A69785F9A69684F9A69682F9A69582F9A59481F9A5947EF9A4937DF9A493
      7BF9A4927AF9A49179F99D8C72F55250499700000000000000003C383854800E
      07F71AC000FF4F7000FF892000FF803100FF990D00FF911100FF664600FF00EB
      00FF05DF00FF4A503E8C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000424242624141415D00000000000000003939394D03030304000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002A29
      2934574B23D4545900FF821500FF8F0300FF811A00FF3D8600FF07DD00FF24A9
      06F840413C620000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000111111131E1E1E2300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002A2929344B593AA3456C2BC73C7F23D4436C30BE44453E6D0000
      0000000000000000000000000000000000006B4941DE926E67E49B705CE69E67
      43E89F6643E8A06541E8A06440E8A0633FE8A0633EE8A0633CE8A1633BE8A162
      3AE8A16139E8995930E59E7C63E4674941D70000000000000000000000000000
      000000000000000000000202020343484A823334344600000000000000001111
      11132A2A2A340000000000000000000000000000000000000000000000000000
      000000000000000000000202020343484A823334344600000000000000001111
      1113292A29330000000000000000000000000000000059565296B2A48AF1B2A2
      88F1B2A185F1B2A083F1B1A081F1AF9B7CF17D7260F893856BF5AB9672F16964
      5BFC776D5DF9B09A71F156534D9100000000945B4BF0DB9E90FFEEC8B6FFF9F9
      F9FFEAF5EAFFE8F4E8FFE8F4E8FFE8F4E8FFE8F4E8FFE8F4E8FFE8F4E8FFEDF6
      EDFFF9F9F9FFEA9860FFF2B58BFF89594AE40000000000000000000000000000
      0000000000003738384D4D6C77BC3D7489EA41B3D9FA45616CAD2E2E2E3C3334
      3343177717F53D413D5900000000000000000000000000000000000000000000
      0000000000003738384D4D6C77BC3D7489EA41B3D9FA45616CAD2E2E2E3C3435
      3444177717F53D403D580000000000000000000000005C5A5692F8E4BFFFF8E2
      BCFFF7E1B9FFF7DFB5FFF7DEB2FFCEB993FF999795FF9F9B95FF837C72FFAEAE
      AEFF7D7567FFEECE96FF5A564F9A00000000945B4BF0DB9E90FFEEC8B6FFF9F9
      F9FF18BC18FF00B500FF00B500FF00B500FF00B500FF00B500FF00B500FF3EC6
      3EFFF9F9F9FFEA9860FFF2B58BFF89594AE40000000000000000000000003132
      32424D656DB05FC7ECFD61D1F8FF3E768AEA42C4EFFF47B387FF2B9B46FE2C8D
      37F4008400FF198319F33B3D3B53000000000000000000000000000000003132
      32424D656DB05FC7ECFD61D1F8FF3E768AEA42C4EFFF47B386FF2B9B46FE2C8D
      37F4008400FF1A831AF23B3D3B5300000000000000005C5B5792DFCC93FFDCBE
      6EFFDCBD6EFFDCBD6DFFBFA55FFFB19959FF928F86FF6D6D6DFF363636FF3737
      37FF747473FF706C62FF6F6D6AEE12121214945B4BF0DB9E90FFEEC8B6FFF9F9
      F9FFCEEDCEFFC8EBC8FFC8EBC8FFC8EBC8FFC8EBC8FFC8EBC8FFC8EBC8FFD5EF
      D5FFF9F9F9FFEA9860FFF2B58BFF89594AE400000000282828324D5F65A061BC
      DEF867D3FAFF64D2F9FF61D1F8FF3E768AEA42C4EFFF30C35FFF00B500FF00B4
      00FF00B400FF00B300FF1EAB1FF32324232A00000000282828324D5F65A061BC
      DEF867D3FAFF64D2F9FF61D1F8FF3E768AEA42C4EFFF2FC25EFF00B500FF00B4
      00FF00B400FF00B300FF1EAB1FF32424242B000000005C5B5792E5D4A2FFDBC2
      78FFDBC177FFDBC176FF837C6EFFADACA8FF7B7B7BFF403F3EFF696046FF524C
      3DFF262626FF909090FF585551E413131315945B4BF0DB9E90FFEEC8B6FFF9F9
      F9FF27C027FF0DB90DFF0DB90DFF0DB90DFF0DB90DFF0DB90DFF0DB90DFF4ACA
      4AFFF9F9F9FFEA9860FFF2B58BFF89594AE43D3F405D64B7D7F66DD6FDFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E768AEA41C3EEFF3AD567FF0FD50FFF0DD5
      0DFF00D200FF04D305FF43A371EF000000003D3F405D64B7D7F66DD6FDFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E768AEA41C3EEFF3AD566FF0FD50FFF0DD5
      0DFF00D200FF04D206FF44A074ED00000000000000005D5B5792F9E9CCFFF9E7
      C8FFF9E6C5FFF8E5C2FF8F8778FFBAB9B6FF515151FF716A5EFFF7DDB1FFF6DC
      ADFF3C3B38FF656565FF514F4AD100000000945B4BF0DB9E90FFEEC8B6FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFEA9860FFF2B58BFF89594AE443484A7070D7FEFF6DD6FDFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E768AEA41C3EEFF49C4EEFF4DC3EEFF58CB
      D1FF04D305FF47CD9EFF387D9AE60000000043484A7070D7FEFF6DD6FDFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E768AEA40C1EBFF49C4EEFF4DC3EEFF58CC
      D0FF05D206FF49CBA4FF397C99E500000000000000005D5B5892FAEBD0FFFAE9
      CDFFF9E8C9FFF9E6C6FFF8E5C3FF9C9890FF626262FF605C56FFF7DFB5FFE1CB
      A4FF363635FF787878FFA3A2A2FC4949498B945B4BF0DB9E90FFE39A75FFE084
      46FFE18344FFE28242FFE28140FFE3803EFFE47D3CFFE47D3AFFE47B38FFE57A
      36FFE57934FFE57228FFF2B58BFF89594AE443484A7070D7FEFF6DD6FDFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E768AEA41C3EEFF40C1EDFF40BFEDFF48C1
      DFFF4AC9B7FF3DB8EAFF387D9AE60000000043484A7070D7FEFF6DD6FDFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E768AEA2A5B7BFF225678FF38AED8FF48C1
      DFFF49C8B7FF3DB8EAFF397C99E500000000000000005D5B5892FAEDD4FFFAEB
      D1FFFAEACEFFF9E8CAFF938C7DFFE5E5E4FFB9B9B9FF484848FF4E4D4BFF4040
      3FFF434343FF9D9A96FF716F6BDD39393952945B4BF0DB9E90FFE19169FFDB6A
      21FFDC681FFFDD671CFFDD661AFFDE6517FFDF6415FFDF6312FFE06110FFE160
      0EFFE15F0BFFE46B1DFFF2B58BFF89594AE443484A7070D7FEFF6DD6FDFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF356477ED3FB5DBFC40C0EDFF3FBEECFF3FBC
      ECFF3EBAEBFF3DB8EAFF387D9AE60000000043484A7070D7FEFF6DD6FDFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF356477ED336B92FF0B82A5FF048EAEFF3DB8
      E6FF3EBAEBFF3DB8EAFF397C99E500000000000000005D5B5992FBEED8FFFAED
      D5FFFAEBD2FFFAEACEFFC5BAA4FFA49F96FF8F897DFFC1C1C1FF959595FF9999
      99FFD3D3D3FF8B8271FF5957528E00000000945B4BF0DB9E90FFE19169FFDB6A
      21FFDC681FFFDD671CFFDD661AFF6F5A44FF4E90A0FF4D919EFF4C919FFF4A91
      9EFF48929DFF46939EFF479BABFF4A7989F743484A7070D7FEFF6DD6FDFF6AD4
      FBFF67D3FAFF56ACC6F63B6B7ED9256D8AE6346B7ED1386271D33EA9D1F83FBC
      ECFF3EBAEBFF3DB7EAFF387C9AE60000000043484A7070D7FEFF6DD6FDFF6AD4
      FBFF67D3FAFF56ACC6F63B6B7ED9256D8AE61F697AE8009EBEFF00A6CDFF0E89
      A8FF3EBAEBFF3DB7EAFF397C99E500000000000000005D5C5992FBF0DDFFFBEF
      D9FFFBEDD6FFFAECD3FFFAEACFFFF9E9CCFFDBCBB0FFC5C4C3FFA7A39DFF9C92
      81FFA09E9AFF81786CFF5957528E00000000945B4BF0DB9E90FFE19169FFDB6A
      21FFDC681FFFDD671CFFD9651AFF3C7D97FF3ACDF3FF38D0F4FF35D4F5FF33D7
      F6FF30DAF7FF2EDDF8FF2CE0F9FF33A7BEFE43484A7070D7FEFF6DD6FDFF5FB8
      D8F9447283DA3E7286CF39849FD12583A4E8328BABD93E7D93C841656FBD2C6A
      53E021916EFB30ACBBFF387C9AE60000000043484A7070D7FEFF6DD6FDFF5FB8
      D8F9447283DA3E7387CF39849FD12583A4E8328BABD9167D93F2009CBEFF009C
      C0FF1B7E9FFC3DB7EAFF397C99E500000000000000005D5C5A92FCF2E1FFFBF1
      DDFFFBEFDAFFFBEED7FFFAECD3FFFAEBD0FFF3E3C7FFA29A8BFFCBBEA8FFF8E5
      C2FFCDBDA0FFF6E0BBFF5957538E00000000945B4BF0DB9E90FFBA886EFFC1AB
      9EFFA89688FF907D72FFBBA497FF3D8099FF3BCCF3FF3ACEF4FF37D1F4FF35D4
      F5FF32D7F6FF30DBF7FF2DDEF8FF33A8BEFE43484A7066BCDEFA4B7383D7476A
      76BE47707FB5407B90C63589A6D61E8CB2EF2D8FB1E0398297D0119925F300BC
      00FF5CD55CFF00BA00FF1E7250F00000000043484A7066BDDEFA4B7383D7476A
      76BE47707FB5407B91C63589A6D61E8CB2EF2D8FB1E03A7F98CF0B889EF9009D
      C1FF0094B5FE365F6ED4346A84E400000000000000005D5C5B92FCF4E5FFFCF2
      E2FFFBF1DEFFFBEFDBFFFBEED8FFFAEDD4FFFAEBD1FFFAEACDFFF9E8CAFFF9E7
      C7FFF9E5C3FFF8E4C0FF5A57538E00000000945B4BF0DB9E90FFBB8E76FFF0F0
      F0FF878787FF212121FFE7E7E7FF3D8199FF3BCCF3FF3BCCF3FF39CEF4FF37D2
      F5FF34D5F6FF32D8F6FF2FDBF7FF35A7BEFE3F404163495C64BB4B595D904A60
      68A2456773B53C6D7EC531758CD5176C8DF0207FA0EE1C8459EE00BC00FF01BD
      01FF94E394FF00BC00FF02A802FC101010123F414265495C64BB4B595D904A60
      68A2456773B53C6D7EC531758CD5176C8DF0207FA0EE337C96DE367185D90294
      B1FE00A7CFFF058BA8FA434A4C9300000000000000005D5C5B92FDF6E9FFFCF4
      E6FFFCF3E2FFFCF1DFFFFBF0DCFFFBEED8FFFAEDD5FFFAEBD2FFFAEACEFFF9E8
      CBFFF9E7C7FFF9E6C4FF5A58548E00000000945B4BF0DB9E90FFBB8E76FFF1F1
      F1FF878787FF212121FFE7E7E7FF3C7D99FF3B91A9FF3B90A8FF399FBBFF39CF
      F4FF36D2F5FF34D5F6FF31D9F7FF36A7BFFE000000001818181B43484A764862
      6BAB467282B93E7F96CA318DADDB2D6B82CE308EB0DC2AA253EF77DB77FF97E4
      97FFD3F3D3FF96E396FF2FC92FFF2B2C2B37000000001818181B43484A764962
      6AAA467282B93E7F96CA318DADDB2D6B81CE308EB0DC3C8099CC457484BB3666
      71CC009EBEFF00AFD9FF3F6B75D901010102000000005D5C5B92FDF7EDFFFDF6
      EAFFFCF5E7FFFCF3E3FFFCF2E0FFFBF0DDFFFBEFD9FFFBEDD6FFE6D5BBFFB990
      64FFB99064FFB98F66FE504E4B7E00000000945B4BF0DB9E90FFBB8E76FFF1F1
      F1FF888888FF212121FFE7E7E7FF3B849EFF5AD0FDFF5BD0FEFF52BCE5FF449B
      B9FF429CB8FF409BB7FF3F9AB6FF4198B2FD0000000000000000000000001919
      191D454C50853F6D7FC23688A4D533667AC63489A7D7308256D84ECE4EFF02BD
      02FF94E394FF00BC00FF08AB08F60E0E0E0F0000000000000000000000001919
      191D454C50853F6D7FC23688A5D5346679C53489A7D7416876B743494B7B1818
      181B37626ABA69949EFF9898A3FF4A4A5293000000005D5D5C92FEF9F2FFFDF8
      EEFFFDF6EBFFFCF5E7FFFCF3E4FFFCF2E1FFFBF0DDFFFBEFDAFFDFCDB3FFE3A1
      5EFFE1A05EFF6F6356BA0000000000000000945B4BF0DB9E90FFB78A73FFD4D2
      D1FFA5A4A3FF767575FFCCCBCAFF3A839EFF57CFFCFF5BD0FEFF5AD0FEFF52A9
      CAFF5793AAFF537D89FF5A8D9BFF535659BB0000000000000000000000000000
      0000000000002121212742505593355562C1424A4D841E1E1E24388A38C92CC6
      2CFF56D356FF04B504FA3C443C63000000000000000000000000000000000000
      0000000000002121212742505694365563C0424A4D841919191D000000000000
      00000404040566667AE857578DF02525262E000000005D5D5C92FEFBF6FFFEFA
      F2FFFDF8EFFFFDF7ECFFFDF5E8FFFCF4E5FFFCF2E2FFFBF1DEFFDFCEB7FFE09F
      5EFF6E6356B60000000000000000000000006B4941DE926E67E4926551E58C4F
      29E68D4F28E68D4E27E68D4D25E65C5145F45B615CF55C615CF55D615CF58551
      2FE98E4A1DE6955128E45D544E9E010101020000000000000000000000000000
      0000000000000000000000000000020202030000000000000000020202033337
      33483C453C621F1F1F2500000000000000000000000000000000000000000000
      0000000000000000000000000000020202030000000000000000000000000000
      000000000000050505060101010200000000000000005A5A5996B6B5B3F1B6B3
      B2F1B6B3B0F1B5B2ADF1B5B2AAF1B5B1A8F1B4B0A6F1B4B0A4F1ACA191F36B62
      56B0000000000000000000000000000000000000000000000000000000000000
      000000000000000000000E0E0E0F434B4E8B2424242C00000000000000001E1E
      1E2338383F542424252C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001415
      1417373B37512D2E2D3B00000001000000000000000000000000000000000000
      00000000000000000000000000003D3F405F4044467100000001000000000101
      0102333433420000000000000000000000000000000000000000000000000000
      0000000000000202020343444574464545704F4D4AB0484441DA615B56DA504E
      4ACA1B1B1B1F0000000000000000060606070000000000000000000000000000
      0000040404054246486F4C7E90DB3F7080DA3FA3C4F3455A62A43F405D8B0202
      F9FC0000FFFF0000FFFF3D3D66940000000016161619464F537A464F537A464F
      527A464F527A464F527A454F527A454F527A454F527A454F527A415D52A206AD
      09F843CE43FF05BE05FF21782CDD3C40415C0000000000000000000000000000
      000000000000313232424A626AB54DA8C5FC3A9EBFF9406F7FCA3D4041610808
      0809218121F74951497A000000000000000000000000000000013D3D3C584E4D
      4AAB5C5853D2655D55E3376793FF1F5574FF8C8B76FF463D32FF685B4CFF9481
      68FD4746467A0000000000000000060606070000000000000000000000013F41
      42624C7584D163D0F7FF61D1F8FF3F7080DA41C4EEFF36A3E9FF0000FEFF0000
      FFFF0000FFFF0000FFFF0000FFFF3333374749565B8842B1D1FF3AC3E6FF39C5
      E6FF38C6E7FF37C8E7FF36C9E8FF33CBE8FF32CCE9FF2FCCE4FF02BD0FFF00BC
      00FF89E089FF0CC00CFF00BC00FF317051D00000000000000000000000002828
      28324A5D63A85CB6D7F862D1F8FF53B5D8FF3BA2C3F94AB8A0FF2A9E47FF2F8E
      3DF4048404FE088308FC485148780000000000000000555350BB8F7761F7CCA9
      81FFDCB181FFDCB181FF26728EFF00A0C5FF088FACFF3F3E39FF64594DFF8576
      64FC4141416300000000000000000606060700000000383A3A514C6974C167CE
      F4FF67D3FAFF64D2F9FF61D1F8FF3F7080DA41C3EEFF297AE7FF1717FEFFCACA
      FFFFDDDDFFFFDDDDFFFF0909FFFF3F3F587E4E6E7BB13D9EBAFF3ACDF3FF39CF
      F4FF37D1F4FF36D3F5FF35D4F5FF33D6F6FF32D8F6FF29CBCEFF3DCC3DFF95E3
      95FFCEF2CEFF9AE49AFF75DB75FF207C36E4000000001E1F1F244A575C9B5DAB
      C8F268D3FAFF65D2F9FF62D1F8FF53B5D8FF3BA1C3F93BC487FF00B500FF00B5
      00FF00B400FF00B300FF07B007FD3C3F3C5400000000514D4BD2AF9675FDA088
      6BFF7B6956FF796C5DFF4E6A67FF009CBCFF00A9D2FF1A8496FFDDBA92FF605B
      53E9060606070000000000000000060606074951558A6BCBF0FD6DD5FCFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3F7080DA41C3EEFF3297E9FF5252FBFF0F0F
      FEFF0C0CFFFF0C0CFFFF0000FFFF3A3A42594E6E7BB13D9EBAFF3BCCF3FF3ACE
      F3FF38CFF4FF37D1F4FF36D3F5FF34D5F6FF33D7F6FF30D4EBFF4ACD4DFF0DBF
      0DFF89E089FF0CC00CFF00BC00FF2C7349D72B2C2C385EA4BEEC6ED6FDFF6BD5
      FCFF68D3FAFF65D2F9FF62D1F8FF53B5D8FF3BA1C3F941D28BFF0FD50FFF0FD5
      0FFF00D200FF00D200FF41C575FD272727300000000022222228464545765551
      4BEDD4B592FFE2BD94FFE2BE95FF4F8B88FF009EBFFF00A4CAFF4C888CFF524F
      4CC4000000000000000000000000060606074E5C629B70D7FEFF6DD5FCFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3F7080DA41C3EEFF40C1EDFF2851F9FF3838
      FCFF0E0EFEFF0000FFFF1623C4EE030303044E6E7BB13D9EBAFF3BCCF3FF3BCC
      F3FF39CEF4FF38D0F4FF37D2F5FF35D3F5FF34D5F6FF33D7F6FF20C98AFF38C8
      39FF6CD96CFF07BE07FF03B719FF46676CAF3536364A71D7FEFF6ED6FDFF6BD5
      FCFF68D3FAFF65D2F9FF62D1F8FF53B5D8FF3AA1C2F948C3EFFF4DC3EEFF52C3
      EEFF17D51AFF42D183FF3CB0E0FF2A2A2B3600000000000000000C0C0C0D615A
      54E6E4C29CFFE4C39EFFE5C49FFFE3C29FFF208F9DFF009BBEFF0097BAFF4252
      54AE000000000000000000000000060606074E5C629B70D7FEFF6DD5FCFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3F7080DA41C2EEFF40C0EDFF3FBDECFF2A80
      ECFF1F58F0FF2671E6FF426374BB000000004E6F7BB13D9EBBFF3BCCF3FF3BCC
      F3FF3BCDF3FF39CEF4FF38D0F4FF36D2F5FF35D4F5FF34D6F6FF32D7F6FF25CB
      BCFF18C578FF19C18EFF2ACEDDFF49656FA73536364A71D7FEFF6ED6FDFF6BD5
      FCFF68D3FAFF65D2F9FF62D1F8FF53B5D8FF3AA0C2F941C1EEFF40BFEDFF42BE
      ECFF4ECDAFFF3FB9EAFF39AEDFFF2A2A2B3600000000000000002D2D2D39766B
      5FEFE7C8A6FFE7C9A7FFE8CAA9FFE8CAAAFFD1BEA2FF0B94ACFF00A2C7FF048E
      ACFA222222290000000000000000060606074E5C629B70D7FEFF6DD5FCFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF335F6EE440BFEAFF40C0EDFF3FBEECFF3EBC
      EBFF3EB9EBFF3DB7EAFF426473BA000000004E6E7BB13E9EBBFF3BCCF3FF3BCC
      F3FF3BCCF3FF3ACDF3FF39CFF4FF38D1F4FF36D2F5FF35D4F5FF33D6F6FF32D8
      F6FF31D9F7FF2FDBF7FF2ED2ECFF49656FA73536364A71D7FEFF6ED6FDFF6BD5
      FCFF68D3FAFF65D2F9FF62D1F8FF4899B4FA368DA8F440C1EDFF40BFEDFF3FBC
      ECFF3EBAEBFF3DB8EAFF39AEDFFF2A2A2B36000000000000000041404063A796
      84FEEACEB0FFEACFB1FFEAD0B2FFEBD0B4FFEBD1B5FF9FAD9FFF009BB9FF10A8
      CBFF697679EC0606060700000000060606074E5C629B70D7FEFF6DD5FCFF6AD4
      FBFF67D3FAFF4E97B3F1376B7FDC266C87E1376578D036687ADE3EB5E3FE3EBB
      EBFF3EB9EBFF3DB7EAFF426473BA000000004E6E7BB13D96B1FF3AB8DAFF3AB8
      DAFF3AB8DAFF3AB8DAFF3ACBF1FF39CFF4FF37D1F4FF36D3F5FF35D4F5FF33D6
      F6FF32D8F6FF30DAF7FF2FD1EBFF49656FA73536364A71D7FEFF6ED6FDFF6BD5
      FCFF68D3FAFF5CBEE3FD3C6F83E2287A98E82D687FD9375F6ED43A96B9F33FBC
      ECFF3EBAEBFF3DB8EAFF39AEDFFF2A2A2B3600000000000000004D4C4B95C5B3
      9FFFECD4BAFFEDD5BBFFEDD6BCFFEED7BEFFEED7BFFFEED8C0FF42686DF4999B
      9DFF7272A6FF3E3E416200000000060606074E5C629B70D7FEFF6DD5FCFF569D
      B7F2406B7BD93D788ECE3787A3D4287998E03688A5D541768BC442616CBD3D69
      7AD43CB1E0FE3DB7EAFF426473BA000000004E6E7BB155BBE2FF52B1D6FF52B0
      D5FF51B0D4FF4FACD0FF399BB6FF3ACEF3FF38D0F4FF37D1F5FF36D3F5FF34D5
      F6FF33D7F6FF32D8F7FF30CFEBFF496570A73536364A71D7FEFF6ED6FDFF64C5
      E9FD477688DB3D6F82CE3A829BCE2797BFE92881A0E43D8097CB436875BA4162
      6EC43A93B5F13DB7EAFF39ADDFFF2A2A2B3600000000000000004F4F4CB3DECC
      B9FFEFDAC3FFF0DBC5FFF0DCC6FFF0DDC7FFF1DDC9FFF1DECAFF605D5BD85151
      63BB47474E890000000000000000060606074E5C629B5FABC6F34C6E7CCC486B
      78BA467282B93E7E95C9328CACDA207FA1E8318EAEDB3E7F96CA457383BA4A68
      72A9485D65B23B6273D63F5F6EC0000000004D6D7BB158CFFDFF5AD0FEFF5CD1
      FFFF5AD0FEFF59CFFDFF4AA1C0FF41869DFF40859DFF3F869DFF3F859CFF3D84
      9CFF3C849BFF3C849BFF3E91ACFF496570A73536364A6FCFF4FE4D7A8CDD456A
      76CB486E7BB241798DC33786A2D3229CC7EE2285A9EA3985A0D143788AC0496D
      79B0496067A9405D69C63681A1EE2A2A2B360000000000000000535250C7F2E0
      CEFFF2E0CDFFF2E1CEFFF3E2D0FFF3E3D1FFF3E3D2FFF4E4D4FF5D5B5AD22424
      242C3F3F3F62444444701414141706060607464E50924B5D63AD4C5B61944962
      6AA6456977B83B6E81C82E768FD8156D8EF22482A2EB357A93DB407487CD486C
      78B94D646DAA4C585D9C4042426F000000004D6D7BB157CFFCFF59CFFDFF5BD0
      FEFF5CD1FFFF5AD0FEFF58CFFCFF57CEFBFF55CCFAFF53CBF9FF52CAF8FF50C9
      F7FF4FC8F6FF4DC7F5FF4ABBE4FB4147496A2B2B2B37475C64C44B585D8E4B5F
      67A0466672B23E6C7DC234738BD21B7090ED1A789DF22F7C9AE13B768BD3466F
      7EC04C6772B14C5D63A1454D4F901414141700000000000000004E4D4CCEF5E6
      D7FFF5E6D7FFF5E7D8FFF6E8D9FFF6E9DBFFF6E9DCFFF7EADDFF4D4C4CE02D2D
      2DE0454545743D3D3D5A484848B306060607000000002323242B464D4F814867
      72B1447587BD3C829ACD2E8FB3DE2E6E87CF328BAAD93F7D93C8477180B8485A
      609D404243680808080900000000000000004C6D7AB155CEFBFF57CFFCFF59D0
      FDFF5BD1FEFF5BD0FEFF59CFFDFF52B1D6F34B5E66984A5E66984A5E66984A5E
      65984A5E6598495E6598404546650000000000000000080808093F424365485F
      67AB47717FB63F7D92C7348AA8D7296E88D82D91B5DF3B839CCF447688BE4863
      6DA9454A4D7C21212127000000000000000000000000000000004F4E4DC3F6EA
      DFFFF8ECE0FFF8EDE2FFF8EEE3FFF9EFE4FFF9EFE6FFF9F0E7FF5F5E5DD74949
      49AA4D4D4DBB4B4B4BAC4D4D4DB4060606070000000000000000000000002A2A
      2A35455256913E7588C7338BAAD834697DC63786A1D443606AAC3F4143670909
      090A0000000000000000000000000000000049656FAF4AA1C1F04BA1C2F04DA2
      C3F04FA3C3F050A4C4F04FA3C3F049565B8A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000A0A
      0A0B42464870416674BB3885A0D231697ECE328CACDA3F6E7FBD444F528A2829
      29330000000000000000000000000000000000000000000000004A494994DBD4
      CDFEFAF2EAFFFBF3EBFFFBF4EDFFFCF5EEFFFCF5EFFFFCF6F1FF7D7B79E84F4F
      4F954E4E4EC14D4D4DB54949498A060606070000000000000000000000000000
      0000000000003031314140575FA437545FBB414446720A0A0A0B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F11424B4F86345562C4414F549227272730000000000000
      000000000000000000000000000000000000000000000000000032323242827F
      7EEAFDF8F4FFFEF9F5FFFCF8F4FFDBD9D7FFB1AFAEFF838281F6747373E84B4B
      4ACA464646883A3A3A5200000001060606070000000000000000000000000000
      0000000000000000000000000000020202030000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000020202030000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010101025858
      58C1797877F4737272E3626262CE5555559E4040405C15151517000000000000
      0000000000000000000000000000060606070000000000000000000000000000
      000000000000000000000000000000000000111111133F51478A264F36CF3F44
      417B080808090000000000000000000000000000000059565296B2A48AF1B2A2
      88F1B2A185F1B2A083F1B1A081F1B19E7EF1B09E7CF1B09D79F1B09C76F1B09B
      75F1B09A71F1B09A71F156534D90000000000000000000000000000000000000
      0000000000000000000000000000020202030303030400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000013F42436A1616161900000000060606073E47
      3E6C3C5A3C9B3E473E6C06060607000000000000000000000000000000000000
      00000000000000000000040404053E47416F1E8547DB00D453FF205F39D80053
      20FF204A31D93F44417A0303030400000000000000005D5B5793F8E4BFFFF8E2
      BCFFF7E1B9FFF7DFB5FFF7DEB2FFF6DCAEFFF6DBABFFF6D9A8FFF5D8A4FFF5D6
      A1FFF4D59DFFF4D39AFF59554F8D000000000000000000000000000000000000
      000000000000000000003A3C3D58315F72CE286174D73D414263010101020000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003536374A4B6B76BE3E6D7FDE4092ADE7455054862A762AC800BC
      00FF73DB73FF00BC00FF2D6D2DC3020202030000000000000000000000000000
      000001010102414C4194227D46D500D353FF00D453FF00D453FF205F39D80053
      20FF005320FF005220FF284734CC2323232B000000005D5B5793F9E5C3FFF8E4
      C0FFF8E2BDFFE7D5B1FFF7DFB6FFF7DEB3FFF6DDAFFFF6DBACFFF6DAA8FFF5D8
      A5FFF5D7A2FFF5D59EFF59554F8D000000000000000000000000000000000000
      0000303131403F5F6CAF25ACDFFC199ACBFF1594BEF912A9DBFF33677AC63B3E
      3F5D000000010000000000000000000000000000000000000000000000002C2D
      2D394D666EAD5EC1E3FB61D1F8FF3E7082DE42C4EFFF2DAEA5FF00BC00FF00BC
      00FF97E497FF00BC00FF00BC00FF3639364E0000000000000000000000003A3A
      375170702EC1407926ED00D453FF00D453FF00D453FF00D453FF205F39D80053
      20FF005320FF005320FF005320FF37373750000000005D5B5793F9E7C8FFE0D7
      BBFF95BFC3FF5CA2B3FF77B3BDFFD3CCADFFF7DEB3FFF6DDB0FFF6DBADFFF6DA
      A9FFF5D9A6FFF5D7A2FF59554F8D00000000000000000000000027282831435B
      65A02EA9D9F828B9F0FF23B5EDFF1A93C0FF1396C0FB12A9DCFF12A0D2FF1392
      C2FE345C6DC137393A520000000000000000000000002323232A4D5F65A05FB7
      D7F767D3FAFF64D2F9FF61D1F8FF3E7082DE41C4EEFF25B08AFF6DD86DFF96E3
      96FFD4F4D4FF96E396FF4FD14FFF3E473E6C0000000031313040676734B5DADA
      02FCEDED00FF417B26ED00D453FF00D453FF00D453FF00D453FF205E38D90053
      20FF005320FF005320FF005320FF37373750000000005F5D5A9BA2C5C7FF6BC8
      E9FF63D1F8FF61AEC1FF41C1EEFF43B5DEFF83B1B8FFE2D3AEFFF7DDB1FFF6DC
      ADFFF6DAAAFFF5D9A7FF5955508D00000000000000004450568A36A6D1F533C0
      F7FF2DBCF4FF29B9EFFF2385ABFF236077FF1294BEFC12A7D9FF129FD1FF1197
      C8FF118EBFFF137DAAFC404C5295000000004347486D61AFCDF26DD5FCFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E7082DE41C3EEFF34B7C1FF4FCE4FFF0DBF
      0DFF97E497FF00BC00FF00BB00FE2627262F3636344ACECE06F8EDED00FFEDED
      00FFEDED00FF417B26ED00D453FF00D453FF00A340FF005B23FF0A5829F41F4A
      2FDA074821F7005320FF005320FF3737375000000000626E70BA6ED6FDFF69D4
      FBFF63D1F8FF61AEC2FF40C1EDFF3FBDECFF3DB8EAFFB1BEAEFFF7DFB5FFF7DE
      B2FFF6DCAEFFF6DBABFF5956508D0000000000000000436B7BBB38C3FAFF33C0
      F7FF2BA1CFFF3289AAFF40AFD9FF245E74FF1195BFFD268DB3FF1B8DB6FF1092
      C1FF118DBDFF1185B4FF3A525EB0000000004B565A8E70D7FEFF6DD5FCFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E7082DE41C3EEFF40C1EDFF27BC86FF2AC6
      2DFF48CF48FF00BC03FF2A7754E0000000003C3C3956EDED00FFEDED00FFEDED
      00FFEDED00FF417B26ED01A045FF1C4366FF0B2058FF004E41FF007D31FF0080
      31FF0D6530F11F4A30DA0A4722F43737375000000000626E70BA6ED6FDFF69D4
      FBFF63D1F8FF5094A9FF40C0EDFF3FBCECFF3DB8EAFFB2BFB2FFF7E1B9FFF7DF
      B6FFF7DEB2FFF6DCAFFF5956518D0000000000000000436B7BBB37B5E7FF3A97
      BBFF49B7E1FF4BC6F3FF47C3F0FF2B7896FF1888AFFF40BDECFF3CBBEAFF2695
      BFFF1378A2FF1183B2FF355766BF000000004B565A8E70D7FEFF6DD5FCFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3E7082DE41C3EEFF40C0EDFF3FBEECFF39B7
      D6FF2EB0B1FF37B3D4FF406B7DC7000000003C3C3956EDED00FFEDED00FFEDED
      00FFD4D401FE2F3D4AF73B44BAFF5353FFFF0F1E83FF0000CFFF001883FF004D
      3CFF008031FF017831FD2A4836C82525252E00000000626E71BA6ED6FDFF67B5
      CFFF5BA4B9FF3DA4C3FF60A5B6FF549DB2FF3DB6E8FFB2C0B5FFF8E3BDFFF8E1
      BAFFF7E0B7FFF7DEB3FF5957528D000000000B0B0B0C40778DE24CB3DBFF55CC
      FAFF50C9F7FF4BC6F3FF47C3F0FF3496B9FF2991B7FF40BEEDFF3CBBEAFF34B6
      E6FF2CAEDDFF1A85ADFF20637DEB000000004B565A8E70D7FEFF6DD5FCFF6AD4
      FBFF67D3FAFF64D2F9FF61D1F8FF3A697BE241C2EEFF40C0EDFF3FBEECFF3FBC
      ECFF3EBAEBFF3DB7EAFF406A7DC7000000003C3C3956EDED00FFE9E900FF6262
      39EA4848A8FB5353FFFF5353FFFF5353FFFF0F1C81FF0000D4FF0000D4FF0000
      C9FF0E1E67F03D3F3E650000000100000000000000005E6768BA79A8B4FF79B7
      C7FF56B0CBFF2E9CBFFF51ADC9FF79B6C3FF789EA4FFBFBFAEFFF8E4C2FFF8E3
      BEFFF8E2BBFFF7E0B7FF5957528D000000004B7282C55BD1FFFF59CFFDFF55CC
      FAFF50C9F7FF47BDE8FF2C86A6FF0D5771FF1D9AC5FF248EB2FF36ABD7FF34B6
      E6FF2DB1E1FF26ACDCFF1D9CCAFE405359954B565A8E70D7FEFF6DD5FCFF6AD4
      FBFF67D3FAFF59B2D4F9396B7EDE24667FE7366373CF38778DE23FBDEBFF3EBB
      EBFF3EB9EBFF3DB7EAFF406A7DC7000000003D3D3958767618E4727218E36262
      69FF5353FFFF5353FFFF5353FFFF5353FFFF1F1F7CFE0000D4FF0000D4FF0000
      D4FF0808ABF60F0F0F110000000000000000000000005E5C5A93DAD8CDFF95BB
      C2FF4BB4D8FF47ADCCFF62B6CEFFA5B9B3FFE4D9BFFFF9E8C9FFF9E6C6FFF8E5
      C2FFF8E3BFFFF8E2BCFF5957538D0000000049565B885BD1FFFF59CFFDFF52C6
      F4FF3597BDFF0F5871FF056386FF07698EFF22BEF4FF22BEF4FF1B9AC5FF1C7C
      A1FF269CC6FF26ACDCFF1FA7D8FF415056894B565A8E70D7FEFF6DD5FCFF62BE
      DFFA467486D93C7287D03886A0D2277B98E13589A6D6407588C340616DBF3A72
      89E23CB7E7FF3DB7EAFF406A7DC7000000001C1C1C2052523D98898911EC6262
      69FF5353FFFF5353FFFF5353FFFF5353FFFF21217CFC0000D4FF0000D4FF0000
      D4FF0808ABF60F0F0F110000000000000000000000005E5D5B93FCF4E5FFFCF2
      E2FFBFC8BFFF8BACAFFFE3DAC7FFFAECD4FFFAEBD1FFFAEACDFFF9E8CAFFF9E7
      C7FFF9E5C3FFF8E4C0FF5957538D000000001C1C1C2051B6DCFB3E98BBFD1459
      71FF056386FF056386FF056386FF0B6E90FC22BEF4FF22BEF4FF22BEF4FF22BE
      F4FF1C9DCCFF167AA1FF1B7598F9121212144B565A8E69C7EAFD4B7788DD456B
      79C8467080B73F7D92C7338AAAD82181A4E8318EAEDB3D7F97CB457384BA4966
      70A9445B63B93B687CD83F697CC90000000000000000000000001E1E1E244949
      6DD75353FFFF5353FFFF4040B4FD2E2E87EE2A2A98E7242477DE0101C0FD0000
      D4FF0808ABF60F0F0F110000000000000000000000005E5D5B93FDF6E9FFFCF4
      E6FFFCF3E2FFFCF1DFFFFBF0DCFFFBEED8FFFAEDD5FFFAEBD2FFFAEACEFFF9E8
      CBFFF9E7C7FFF9E6C4FF5957548D00000000000000003E494DA4205162DE0563
      86FF056386FF056386FF056386FF186C8CEF22BEF4FF22BEF4FF22BEF4FF22BE
      F4FF26B6E8F9406C7BAF2E2F303E00000000464E508F4B6067BB4B5A61944960
      68A3456876B43C6E80C6327590D3146A8CF4287C9EE5357A92D9417284C9486B
      78BA4C636AA64C5C61A142474A90000000000000000000000000000000004848
      65A14141C0F72D2D7CED2828CBF42828FFFF2828FFFF2828FFFF2D2DACE62424
      77DE080896F60F0F0F110000000000000000000000005E5D5C93FDF7EDFFFDF6
      EAFFFCF4E7FFFCF3E3FFFCF2E0FFFBF0DCFFFBEFD9FFFBEDD6FFE5D3B9FFB990
      64FFB99064FFB98F66FE504E4B7D0000000000000000000000000C0C0C0D3F45
      47771F5163DF056386FF056386FF216C88E322BEF4FF22BEF4FF26B7EAFA3F71
      82B53132334200000000000000000000000000000000313232434751558C4869
      75B0457486BC3C819ACD2E8FB2DE2E6F85CE328CADDA3E7F96CA467282B94A5F
      67A244494B791E1E1E2300000000000000000000000000000000000000004141
      519D2929AFEE2828FFFF2828FFFF2828FFFF2828FFFF2828FFFF2828FFFF2929
      E5FA33335BC5080808090000000000000000000000005E5E5D93FEF9F2FFFDF8
      EEFFFDF6EBFFFCF5E7FFFCF3E4FFFCF2E1FFFBF0DDFFFBEFDAFFDFCDB4FFE3A1
      5EFFE1A05EFF6F6456B600000000000000000000000000000000000000000000
      00000D0D0D0E3F45477D1D4F62E1296D84D726B7EAFA3F7182B5323334440000
      0000000000000000000000000000000000000000000000000000000000003536
      364B45575D9E3E7A8FCA338AAAD833697CC63688A4D5426877B8444B4E802020
      2026000000000000000000000000000000000000000000000000000000000000
      00002D2D2E3B41416AA72929E0F82828FFFF2828FFFF2828F0FD3B3B78BE3636
      384E00000000000000000000000000000000000000005E5E5D93FEFBF6FFFEFA
      F2FFFDF8EFFFFDF7ECFFFDF5E8FFFCF4E5FFFCF2E1FFFBF1DEFFDFCEB6FFE09F
      5EFF6E6356B50000000000000000000000000000000000000000000000000000
      000000000000000000001212121440484A783233344400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000383A3A53405A64AB355866C1424D508A1F1F1F25000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000333334463D3D71B63C3C76BC37373A50000000000000
      000000000000000000000000000000000000000000005A5A5996B6B5B3F1B6B3
      B2F1B6B3B0F1B5B2ADF1B5B2AAF1B5B1A8F1B4B0A6F1B4B0A4F1ACA091F36B61
      56AF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000161616190000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003739
      394F0B0B0B0C0000000000000000000000000000000000000000000000000000
      000000000000000000001919191D3E3F3F523939394915151518000000000000
      000000000000000000000000000000000000000000002E2E2E3B050505060000
      00000000000000000000000000001B1B1B1F5F5F5FAC727272CD717171CC5D5D
      5DA813131315000000000000000000000000000000002E2E2E3A060606070000
      00000000000000000000000000001919191C666666AA707070CD717171CC6666
      66AA151515170000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003031313F50707CB7467D
      8CD34393ADE2444E527A0909090A000000000000000000000000000000000000
      0000010101026F787BA182BECDE564D9F9FE64D6F6FCA4CAD5EA6B74769C0000
      00000000000000000000000000000000000000000000696969C66F6F6FD14242
      42601919191D2424242B5959599A878787E1E0EAEDFFC5E5EFFFC5E5EFFFC1CC
      D0FD3333334300000000000000000000000000000000737373C27A7A7AD24343
      435D131313152323232A5E5E5E97757575E0666E71FF7C9FA7FF7B9EA8FF7C8A
      8EFF373737480000000000000000000000000000000000000000000000000000
      00000000000000000000000000001F1F1F24536D78AE66CBF1FD64D2F9FF487C
      8DD241C2EEFF3FBEECFF42819BD636393A4D0000000000000000000000000000
      00003232323E7DDEF5FA50DCFFFF48D9FFFF48D9FFFF8C9294B9AFD4DFEE1F1F
      1F240000000000000000000000000000000000000000696969BEF9F9F9FFD0D0
      D0FD989898E8A5A5A5EDE5E5E5FFF2F7F7FF82E5FEFF47D9FFFF68DDFEFFC3E9
      F4FF5154547A000000000000000000000000000000006E6E6EBC404040FF5F5F
      5FF9717171E5696969EC4A4A4AFF565859FF79DCF4FF48D9FFFF5DD6F7FFAFD5
      E0FF595C5D82000000000000000000000000000000012626262F282727312827
      2731282727312827273128272731484F517F6ED6FDFF69D4FBFF64D2F9FF477F
      92DB41C1EEFF3FBDECFF3EB9EBFF465961920000000000000000000000000000
      00003D3E3E4F7DE8FFFF5AE0FFFF64DDFBFD86BAC7E09EC9D4E89EC5D1E62929
      29310000000000000000000000000000000000000000696969BEFAFAFAFFF5F5
      F5FFEFEFEFFFEEEEEEFFEEEDEBFFE8EEEDFF6AE4FFFF60E0FFFFC3F2FEFFC9EF
      F9FF7E888BB82F30303B0000000000000000000000006E6E6EBC424242FF4848
      48FF464646FF454545FF61605EFF959998FF6DE4FFFF5FE1FFFFACDCE8FFBEE4
      EEFF848F92BB3030313C00000000000000004F4B487AE7C4A7FFE9C6A9FFE9C6
      A9FFE9C6A9FFE9C6A9FFE9C6A9FFAFB4ABFF6ED6FDFF69D4FBFF64D2F9FF4886
      9AFF3FB8E1FF3FBDECFF3DB9EAFF465961920000000000000000787470A4A696
      82DEAEADA7D787EAFFFF65E4FFFF6DE0FBFD8ABCC7E087BBC7E085BAC7E082B9
      C7E082B6C4DE656D70940000000000000000000000006A6A6ABEFBFBFBFFEBEB
      EBFFDBDBDBFFDFDCD9FFC19F73FFD5D0C1FF79E9FFFF66E4FFFF66E2FFFF5CDE
      FFFF51DBFFFF5FCEEDF63C3D3E4F00000000000000006E6E6EBC434343FF5959
      59FF545454FF716F6CFFB9986CFFC1B8A7FF7CEAFFFF67E4FFFF66E2FFFF5CDE
      FFFF52DBFFFF5ED0EFF73F4142550000000061595399F9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFB9D4DDFF6ED6FDFF69B9D6FF58A4BFFF3DA7
      CDFF57AAC8FF457587DD3DA6D0F746596192000000003030303CB6986EF5A976
      3AFFA59F96D58DEBFDFE6FE7FFFF68E5FFFF60E2FFFF59DFFFFF51DDFFFF4ADA
      FFFF42D7FFFF7CC1D4E81B1B1B1F00000000000000006A6A6ABEFCF9F9FFFDD3
      D3FFFCD2D2FFEED0C2FFB07C3EFFCDB391FFBDF3FDFF95EDFEFF91EBFEFF73E5
      FEFF54DEFFFF4ADAFFFF5F6E719600000000000000006E6E6EBC494747FF6D58
      58FF725A5AFFAD978AFFB07C3FFFB89C77FFA5D9E3FF8CE4F5FF87E1F5FF73E4
      FCFF54DEFFFF4ADAFFFF607074990000000061595399F9F9F9FFE3E3E3FFE1E1
      E1FFE1E1E1FFE1E1E1FFE1E1E1FFA7B8BFFF73A3B4FF6FB1C6FF55B1D1FF2791
      B7FF4DAFD1FF497787D24B5C63A342484A73000000005E5C5B80B6874AFFAD7A
      3DFFAE997FE7A1BABFDB8AECFFFF86EBFFFF80E9FFFF75E6FFFF5DE0FFFF54DE
      FFFF4DDBFFFF5ADCFFFF4B4C4D6600000000000000006A6A6ABEFDFCFCFFFCEF
      EFFFFCEFEFFFEFDDCEFFB68443FFAF7B3EFFC1A074FFCBB190FFC5AC8DFFCEC6
      B7FE83C5D4EA59DFFFFF616E729700000000000000006E6E6EBC474646FF524B
      4BFF524B4BFFA09183FFB68443FFAF7B3EFFB59267FFB29775FFAD9371FFBEB6
      A5FF87C7D5EA59DFFFFF6272769B0000000061595399F9F9F9FFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE7E7E7FFE7E7E7FFC5CBCEFF89B3C2FF4BB3D7FF47AC
      CFFF53B5D4FF506063B42121212700000000000000006B696692B78544FFB180
      40FFAC7A3DFFAC987DE6A4A199D5A3A19AD4A3A09AD4A3A39ED5A5B7BCDB68E4
      FFFF57DFFFFF5BDEFFFF575A5B7A00000000000000006A6A6ABEFDFDFDFFE8E8
      E8FFD2D2D2FFDAD6D1FFC8A06BFFB58342FFAD7A3DFFA67438FF9F6E33FF9868
      2FFF94B6BBDF73E3FBFD474A4A6200000000000000006E6E6EBC484848FF6464
      64FF606060FF8A8682FFC49C68FFB58342FFAE7A3DFFA77438FFA06E33FF9867
      2EFF98B7BDDF73E5FDFE4A4D4E670000000061595399F9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFC2D0D6FF80AD
      BFFFCCCCC9FF4947446D0000000000000000000000004B4B4A65C4975DFFB684
      43FFB17D3FFFAC793CFFA77438FFA27035FF9D6B31FF98672EFFA99580E794BE
      C7E062E3FFFF7BE2FBFD3636364400000000000000006A6A6ABEFEFEFEFFFCFC
      FCFFFAFAFAFFF9F9F9FFF6F3F0FFF2EAE0FFEADDCDFFE6D7C5FFB08750FF9E6D
      32FF828887BC4B4D4E670000000100000000000000006E6E6EBC494949FF4A4A
      4AFF484848FF474747FF6E6C68FFB0A89EFFDACDBDFFD3C5B2FFB08651FF9E6D
      33FF898E8CBF4C4E4F69010101020000000061595399F9F9F9FFD1D1D1FFCECE
      CEFFCECECEFFCECECEFFCECECEFFCECECEFFCECECEFFCECECEFFCECECEFFE6E6
      E6FFF5ECE4FF4947446D0000000000000000000000000E0E0E0FAB9F8ED8BB8A
      49FFB58342FFB07D3FFFAB783BFFA67438FFA16F34FF9C6B31FFAD8D63F997B4
      BAD76CE6FFFF93B2B8D60707070800000000000000006A6A6ABEFFFFFFFFE9E9
      E9FFD5D5D5FFD5D5D5FFD5D5D5FFDDDAD6FFE3D4C1FFC8A87DFFAB783CFFAB7C
      46FF5A595785000000000000000000000000000000006E6E6EBC4B4B4BFF6464
      64FF606060FF5F5F5FFF5D5D5DFF8A8885FFD2C3B1FFC09F75FFAC793CFFA87A
      42FF63615F8D00000000000000000000000061595399F9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF5ECE4FF4947446D000000000000000000000000000000001E1E1E236E6C
      6896928E89BCE5E1DDF5E2DFDAF4E1DFD9F4B38B57FFA06E34FFAF8E65F9777C
      7EA46F77789E292929320000000000000000000000006A6A6ABEFFFDFDFFFFDF
      DFFFFEDEDEFFFEDEDEFFFDF1F1FFFCFCFCFFF1E8DCFFE6D5C1FFE3D2BDFFD3CB
      C1FE33333344000000000000000000000000000000006E6E6EBC4E4C4CFF6A5A
      5AFF6C5A5AFF6B5959FF474747FF464646FF978E82FFA69581FFA3937BFF9F97
      8CFF3838384900000000000000000000000061595399F9F9F9FFE1E1E1FFDEDE
      DEFFDEDEDEFFDEDEDEFFDEDEDEFFDEDEDEFFDEDEDEFFDEDEDEFFDEDEDEFFEDED
      EDFFF5ECE4FF4947446D00000000000000000000000000000000000000000000
      00003D3C3C4EC2AA8AF29E9383D0AE7B3EFFA9773AFFA47237FFB09066F82828
      283000000000000000000000000000000000000000006A6A6ABEFFFEFEFFFFE8
      E8FFFFE8E8FFFEE7E7FFFEF5F5FFFDFDFDFFFCFCFCFFF4F4F4FFA8A8A8FF9C9C
      9CFC32323242000000000000000000000000000000006E6E6EBC4D4C4CFF6155
      55FF635656FF635656FF484848FF474747FF454545FF4B4B4BFF919191FF8D8D
      8DFD3737374800000000000000000000000061595399F9F9F9FFE9E9E9FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFF1F1
      F1FFF5ECE4FF4947446D00000000000000000000000000000000000000000000
      00002424242BC7B8A7EB9A9185CAB38142FFAE7B3DFFA9763AFFA69887DC0D0D
      0D0E00000000000000000000000000000000000000006A6A6ABEFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFEFEFEFFFEFEFEFFFDFDFDFFEEEEEEFFA2A2A2FF6E6E
      6ECB00000000000000000000000000000000000000006E6E6EBC4B4B4BFF4B4B
      4BFF4B4B4BFF4B4B4BFF4A4A4AFF484848FF474747FF515151FFA1A1A1FF7979
      79D20000000000000000000000000000000059534F8EEBCCB1FFEBCCB1FFEBCC
      B1FFEBCCB1FFEBCCB1FFEBCCB1FFEBCCB1FFEBCCB1FFEBCCB1FFEBCCB1FFEBCC
      B1FFE7C5A7FF4341406100000000000000000000000000000000000000000000
      0000000000004747465F7F7A74AC878077B7867F77B77B7671A82E2E2E390000
      00000000000000000000000000000000000000000000606060B27E7E7EDD7E7E
      7EDD7E7E7EDD7E7E7EDD7E7E7EDD7E7E7EDD7E7E7EDD808080E0747474DB0101
      010200000000000000000000000000000000000000006A6A6AAE7C7C7CDD7C7C
      7CDD7C7C7CDD7C7C7CDD7C7C7CDD7C7C7CDD7C7C7CDD808080DF7C7C7CDE0202
      0203000000000000000000000000000000000B0B0B0C6D6A67AB9A918CD39A91
      8CD39A918CD39A928CD39A928CD39A928CD39A928CD39A928CD39A928CD39A92
      8CD35958568C0000000100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003B6E7CAC3A70
      7FAF0909090A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000358BA0CE44D2
      F9FE347F92C30808080900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000001553F3F756A3E
      3E926A3E3E926A3E3E926A3E3E924D3D3D68513E3E6F6A3E3E926A3E3E926A3E
      3E926A3E3E92513E3E6F00000000000000000000000007070708673E3E91663E
      3E91663E3E91663E3E91663E3E91663E3E91663E3E91663E3E91663E3E91663E
      3E91663E3E91643E3E8E01010102000000000000000000000000000000000000
      000000000000000000000000000000000000141414164154597C3399B4DE5FDE
      FFFF51D3F7FD367D8FC00707070800000000000000001A1A1A1E343434413434
      344134333341343333412424242B0000000000000000000000001E1D1D226C54
      54961E1D1D220000000000000000000000000000000041383855C05749FAD2A0
      80FFD2A07CFFD2A07AFFD3A178FFCA3125F7C25143FAD2A080FFD2A07CFFD2A0
      7AFFD3A178FFBC4737F835313143000000000000000014141416A32D2DF1D7B2
      AFFFD7B2ABFFD8B3A6FFD8B3A3FFD9B49EFFD9B49BFFDAB597FFDAB593FFDAB5
      8FFFDBB68AFFA4322AEA07070708000000000000000000000000000000000000
      0000363636470000000000000000333637463B9BB3DD6ADEFAFF77E4FFFF73E3
      FFFF6FE2FFFF5CD7F6FD377C8EBF060606073231313EDF6E6EF1EF9090FFEA75
      75FFE45B5BFFDE4040FFD23636F94D474769000000001E1E1E23A16262CED40A
      0AFF524B4B71000000000000000000000000000000004A3C3C63D08E75FFFDFD
      D1FFFDFDCDFFFEFECAFFFEFEC7FFD34434FDCE816AFFFDFDD1FFFDFDCEFFFEFE
      CAFFFEFEC7FFC86E54FF3D36364E000000000000000014141416A93B3BF2F9F9
      F5FFFAFAEFFFFBFBE9FFFBFBE4FFFCFCDEFFFCFCD9FFFDFDD3FFFDFDCEFFFEFE
      C8FFFFFFC2FFA93F33EB07070708000000000000000000000000000000000000
      00005656568F00000000080808093D94A9D695ECFFFF91EBFFFF8DEAFFFF88E9
      FFFF84E7FFFF7CE5FFFF3699B1DD1B1B1B1F1919191C8B6666B2917474BB8F6C
      6CBB8E6464BB8C5B5BBB875656B73130303D1A1A1A1EA97B7BD2DF4040FFD71B
      1BFF615656851E1E1E221313131500000000000000004A3C3C63CF8D78FFFDFD
      D6FFFDFDD3FFFDFDD0FFFEFECCFFD34436FDCD806DFFFDFDD6FFFDFDD3FFFDFD
      D0FFFEFECDFFC86E56FF3D36364E000000000000000014141416A93B3BF2F9F9
      F5FFFAFAEFFFFBFBE9FFFBFBE4FFFCFCDEFFFCFCD9FFFDFDD3FFFDFDCEFFFEFE
      C8FFFFFFC2FFA93F33EB070707080000000000000000000000004C4C4C750000
      000057575793040404053A40415776D7EEFB86E0F3FC4797ABD651ABC0E49CEE
      FFFF96ECFFFF3D9BB3DD27282830000000000000000000000000000000000000
      000000000000000000000000000000000000A67C7CCAEB7676FFE35151FFDB2C
      2CFFE87575FFE04949FFCC4747F33130303D000000004A3C3C63CF8D7BFFFCFC
      DBFFFCFCD8FFFDFDD5FFFDFDD2FFD24338FDCD8070FFFCFCDCFFFCFCD8FFFDFD
      D5FFFDFDD2FFC76D59FF3D36364E000000000000000014141416A93B3BF2F9F9
      F5FFFAFAEFFFFBFBE9FFFBFBE4FFFCFCDEFFFCFCD9FFFDFDD3FFFDFDCEFFFEFE
      C8FFFFFFC2FFA93F33EB070707080000000000000000000000005656568F0000
      00002727272F5A5A56AA43827ED575D9EDFD40737FB506060607448EA0CE9FEE
      FFFF419DB5DE2829293200000000000000001F1F1F24A26F6FC7B78B8BDAB480
      80DAB27373DAAF6767DAA05A5ACF3836364758505077E69393FAE66262FFDF3D
      3DFF8F6E6EBC826464AFD92121FF48454560000000004A3C3C63CF8D80FFFBFB
      E1FFFCFCDEFFFCFCDAFFFCFCD7FFD24338FDCD8072FFFBFBE1FFFCFCDEFFFCFC
      DBFFFCFCD7FFC76D5BFF3D36364E0000000000000000141414169F2626F0C48B
      88FFC48B85FFC58C82FFC58C7DFFC58C7AFFC58C77FFC68D73FFC68D71FFC68D
      6DFFC78E6AFFA02B26E907070708000000000000000000000000565656912F2F
      2F3B3A3A3852747406FA288585FF42BCB4FF67681DDE1E1E1E23338DA3D43C99
      AFDB383C3D510000000000000000000000002C2C2C36CA7575E4D99595F1D584
      84F1D17070F1CD5C5CF1C14E4EEB4743435F00000000524E4E6ED98080F4E24E
      4EFF544D4D72554F4F74DB2F2FFF48454560000000004A3C3C63CF8D83FFFBFB
      E6FFFBFBE3FFFCFCE0FFFCFCDDFFD2433BFDCD8075FFFBFBE7FFFBFBE3FFFCFC
      E0FFFCFCDDFFC76D5EFF3D36364E000000000000000014141416A43232F1E4CE
      CBFFE5CFC6FFE6D0C1FFE6D0BDFFE7D1B8FFE7D1B4FFE7D1AFFFE7D1AAFFE8D2
      A5FFE9D3A1FFA5372EEA070707080000000000000000000000001C1C1C204343
      43647B7B14F1999921FF619660FF6B9239FF949400FF5B5B3AA40B0B0B0C5355
      558D2A2A2A3400000000000000000000000000000000070707080F0F0F110F0F
      0F110F0F0F110F0F0F110A0A0A0B000000000000000000000000514E4E6DBB7A
      7ADF3C3A3A4D55505074DE3D3DFF48454560000000004A3C3C63CE8C86FFFAFA
      ECFFFBFBE9FFFBFBE5FFFBFBE2FFD2433CFDCC7D78FFFAFAECFFFBFBE9FFFBFB
      E6FFFBFBE2FFC76D60FF3D36364E000000000000000014141416A93B3BF2F9F9
      F5FFFAFAEFFFFBFBE9FFFBFBE4FFFCFCDEFFFCFCD9FFFDFDD3FFFDFDCEFFFEFE
      C8FFFFFFC2FFA93F33EB070707080000000000000000000000013A3A3A4F4C4C
      4C80929210FEB1B125FF7D7D0CFFA0A00BFFA0A04AFF6B6B37CB585858923D3D
      3D5600000000000000000000000000000000000000013E3C3C4F4F4B4B694F4C
      4C694E4C4C694E4B4B694E4B4B694E4A4A694E4A4A694E494969353434430101
      010200000000504C4C6BE24F4FFF3F3D3D52000000004A3C3C63CE8C89FFFAFA
      F1FFFAFAEEFFFAFAEBFFFBFBE8FFD1423DFDCC7D7BFFFAFAF1FFFAFAEEFFFAFA
      EBFFFBFBE8FFC66C63FF3D36364E000000000000000014141416A93B3BF2F9F9
      F5FFFAFAEFFFFBFBE9FFFBFBE4FFFCFCDEFFFCFCD9FFFDFDD3FFFDFDCEFFFEFE
      C8FFFFFFC2FFA93F33EB07070708000000000606060757575795424242622B2B
      2B3692920CF8909018FF98982FFFD2D27AFFC8C882FE2626262F000000000000
      0000000000000000000000000000000000003A39394AF85050FEF46C6CFFEE88
      88FFEA7474FFE66262FFE35050FFDF3E3EFFDB2C2CFFD71A1AFFCA3636F51A19
      191D00000000080808094643435C02020203000000004A3C3C63CE8C8CFFF9F9
      F7FFFAFAF3FFFAFAF0FFFAFAEDFFD1423FFDCC7D7DFFF9F9F7FFFAFAF4FFFAFA
      F0FFFAFAEDFFC66C65FF3D36364E000000000000000014141416A32D2DF0D6B0
      ADFFD6B0A9FFD7B1A4FFD7B1A1FFD8B29DFFD8B299FFD9B395FFD9B391FFD9B3
      8DFFDAB489FFA3312AEA0707070800000000040404051919191C000000004858
      3FBE606000FF9F9F33FDCFCF77FFB8B86FF75E5E4DA7000000001919191D2424
      242C000000000000000000000000000000000D0D0D0E715D5D967F6969A87F6E
      6EA87E6A6AA87E6767A87D6464A87C6161A87C5E5EA87B5B5BA86454548B0000
      000100000000000000000000000000000000000000001B1B1B1FA52121DF8E0B
      0BFF8E0B0BFF8E0B0BFF8E0B0AFFA82222DAA62121DD8E0B0BFF8E0B0BFF8E0B
      0BFF8E0B0AFFA32222DB141414160000000000000000060606077C3838AA8700
      00FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF8000
      00FF8E0000FF723C3C9D01010102000000002D2D2D393939394E4646466C525E
      3ECC4D7C31FD46463F75424241633E3E3E584747476B595959974D4D4D7E1F1F
      1F25000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000513E3E6E6A3E
      3E926A3E3E926A3E3E926A3E3E92473B3B5F4C3D3D666A3E3E926A3E3E926A3E
      3E926A3E3E924C3D3D670000000000000000000000000000000021202026663E
      3E92663E3E91663E3E91663E3E91663E3E91663E3E91663E3E91663E3E91663E
      3E91673E3E931918181C00000000000000002D2D2D383B3B3B512F2F2F3B2727
      272F4E504F8B000000001D1D1D214B4B4B7B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004545
      456C2A2A2A3400000000373737493737374B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000020202032E2E
      2E3B2323232A0000000000000001000000000000000000000000000000000E0E
      0E0F0C0C0C0D0101010200000000000000000000000059565296B2A48AF1B2A2
      88F1B2A185F1B2A083F1B1A081F1B19E7EF1B09E7CF1B09D79F1B09C76F1B09B
      75F1B09A71F1B09A71F156534D90000000001819191C378BA8DA4041415D0000
      0000000000000000000000000000000000000000000000000000000000000000
      00004044405D4042405900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000717171D012121214000000000000000000000000000000000000
      000000000000000000000000000000000000000000005D5B5793F8E4BFFFF8E2
      BCFFF7E1B9FFF7DFB5FFF7DEB2FFF6DCAEFFF6DBABFFF6D9A8FFF5D8A4FFF5D6
      A1FFF4D59DFFF4D39AFF59554F8D000000005094B3D827C2FFFF25AEDDFE4344
      4463000000000000000000000000000000000000000000000000000000004043
      405C1D931DF34044405D0000000000000000338282B6229F9FD5229F9FD5229F
      9FD51C9696EA2BBCBCF732A1A1E42D9B9BE4279696E4219292E4208484DD3638
      384E000000000000000000000000000000000000000000000000000000000000
      000000000000727272E01A1A1A1D000000000000000000000000000000000000
      000000000000000000000000000000000000000000005D5B5793F9E5C3FFF8E4
      C0FFF8E2BDFFF7E1B9FFF7DFB6FFF7DEB3FFF6DDAFFFF6DBACFFF6DAA8FFF5D8
      A5FFF5D7A2FFF5D59EFF59554F8D000000002728283051B5E6F52CC2FFFF27AF
      E0FE4546476800000000000000000000000000000000000000003F433F5B1CA2
      1CF300A700FF2F9B2FE8349734E2519451D50000000000000000000000001313
      13151CC6C6FB2AF3F3FF38FEFEFF2FF7F7FF25EEEEFF1AE5E5FF10DBDBFF14A6
      A6ED3739395000000000000000000000000000000000000000005353538A4A4A
      4A6E00000000727272E01A1A1A1D000000000000000000000000000000000000
      000000000000000000000000000000000000000000005D5B5793F9E7C8FFF9E6
      C4FFF8E4C1FFF8DDB9FFF8DDB6FFFAA187FFFAA085FFF6D1A6FFF6DBADFFF6DA
      A9FFF5D9A6FFF5D7A2FF59554F8D00000000000000002223232955B3E0F331C3
      FFFF29B1E2FF494A4B7200000000000000000000000000000000343634452CA9
      2CE800BC00FF3C953CD3418841C4528652BC0000000000000000000000000707
      07082E7A7ACB1CE8E8FF2AF3F3FF39FFFFFF2FF6F6FF24EDEDFF1AE4E4FF0FDB
      DBFF14A6A6ED373A3A50000000000000000000000000000000005A5A5A9D5B5B
      5BA4000000005B5B5BA1767676DD0D0D0D0E0000000000000000000000000000
      000000000000000000000000000000000000000000005E5B5893F9E9CCFFF9E7
      C8FFF9E6C5FFFD4A3FFFFD5E53FFFF5D5DFFFF7D7DFFFE8C86FFF9AC8EFFF6DC
      ADFFF6DAAAFFF5D9A7FF5955508D000000000000000000000000222222285AB2
      E0F236C3FFFF29B3E5FF4A4C4D76101010124C4C4493504E35D0514E32D85968
      4DC82BBB2BED4046405D00000000000000000000000000000000000000000000
      00001818181C2F7C7CC91DE8E8FF2BF4F4FF39FFFFFF2EF6F6FF24EDEDFF19E4
      E4FF0FDBDBFF295B5BD1393939570E0E0E0F0000000000000000555555835A5A
      5ABA000000000000000050505BA719198EF50404A4FA0606BBF8393966B20000
      000000000000000000000000000000000000000000005E5B5893FAEBD0FFFAE9
      CDFFF9E8C9FFFE3C36FFFF3E3EFFFF5C5CFFFC8B79FFFAA68EFFFD756BFFF9B3
      90FFF6DCAEFFF6DBABFF5956508D000000000000000000000000000000002222
      22285DB2E0F23CC4FFFF4096AEF77A781AFCB1B162FFC6C58DFFCCCB9CFFB8B7
      70FF85A238FF5E7251D70B0B0B0C000000000000000000000000000000000000
      0000000000001717171A307B7BC81DE9E9FF2CF4F4FF38FEFEFF2EF5F5FF23EC
      ECFF386C6DD52F4148F92F3436D53C464AE000000000000000003C3C3C4E6464
      64EA565656863F3F44620303B5FB0707A9FF00003AFF0000FEFF08088BFF3D3D
      579D00000000000000000000000054545485000000005E5C5993FAEDD4FFFAEB
      D1FFFAEACDFFFD4A42FFFF3E3EFFFE5C59FFF9C9AAFFF8E2BDFFF7D9B3FFF9A8
      89FFF7DEB2FFF6DCAFFF5956518D000000000000000000000000000000000000
      00001F2020255E8DA2DF989A37FFDBD8B0FFECE9D3FFF1F0E3FFF3F2E8FFF4F4
      EDFFEAE9D7FFA2A247FF4E4D3BD2010101020000000000000000000000000000
      0000000000000000000014141417327C7CC61EE9E9FF2CF5F5FF37F9F9FF4668
      6BD565818EFE465C66F241494DF0444F52E70000000000000000000000005454
      54865A5A5AB810109BF81C1CE6FF2525AAFF2525DEFF0808AFFF0F0FABFF1919
      D0FC0E0E0E0F000000005A5A5A9B6F6F6FD1000000005E5C5A93FBEED8FFFAED
      D5FFFAEBD2FFFD7D6FFFFD7C6FFFFC9484FFFBB59CFFF8E4C1FFF8E3BDFFF8E1
      BAFFF7E0B7FFF7DEB3FF5957528D000000000000000000000000000000000000
      0000070707087B7A20F8DFDBB6FFEEE9D2FFF0EEDEFFF0EEE1FFF1F0E4FFF3F2
      E9FFF4F4EDFFE8E7D3FF909027FF4343426A0000000000000000000000000000
      000000000000000000000000000014141417337676C320D2D2FB3F5050D25458
      5AFF697A83FA454B4EDE586063FF464F51E70000000000000000000000000000
      0000292929320B0BCAFE2525D1FF2424A8FF0D0DA2FF0D0DC2FF3434E5FF1F1F
      E4FF59595AAC626262D37F7F7FE707070708000000005E5C5A93FBF0DDFFFBEF
      D9FFFBEDD6FFFAECD2FFFAEACFFFFBBFA8FFFC9081FFFD7669FFFD5E52FFF8E3
      BEFFF8E2BBFFF7E0B7FF5957528D000000000000000000000000000000000000
      00004444406FB9B66EFFF0E9D0FFF1EBD5FFF0EDDBFFF0EEDEFFF1EFE2FFF1F0
      E5FFF3F3EAFFF3F2EAFFBAB976FF4F4D36DA0000000000000000000000000000
      000000000000000000000000000000000000151515183C4747BC3E4547FF4041
      42E4595D5FE36B6D6EFF5C6061FF494F50E70000000007070708747474DB7F7F
      7FE665656AE50B0BE3FF2525EAFF1111B2FF0A0ACEFF5555FCFF9292B2FF4848
      B8F55B5B5B9F3D3D3D510101010200000000000000005E5C5A93FCF2E1FFFBF1
      DDFFFCBFAEFFFBD0BDFFFAECD3FFFAE5CAFFFE6862FFFF4444FFFE312DFFF8E5
      C2FFF8E3BFFFF8E2BCFF5957538D000000000000000000000000000000000000
      000051503AB1D1CD9CFFF3EBD3FFF0EAD0FFF1EDD9FFF0EDDCFFF1EEDFFFF2F0
      E2FFF2F1E6FFF2F1E8FFCFCE9DFF5A581DF80000000000000000000000000000
      0000000000000000000000000000000000000000000020202026374248E76183
      91FF96A9B2FF949CA0FF646565FF4B4F4FE7161616197F7F7FEA565656930000
      00003636374D1717A8F91E1EABFF3F3FDFFF7474BBFF9D9DFCFFA2A2F6FF4545
      507C00000000000000000000000000000000000000005E5D5B93FCF4E5FFFCF2
      E2FFFBE4D2FFFE6A65FFFDA79CFFFD978CFFFF5F5FFFFF4444FFFF2726FFF9DB
      BDFFF9E5C3FFF8E4C0FF5957538D000000000000000000000000000000000000
      0000525237BAD7D2A5FFF5ECD5FFF2EBD2FFF0E9D1FFF1EDDAFFF1EEDDFFF1EE
      DFFFF1F0E3FFEEECDAFFD4D2A7FF5D5B18FA0000000000000000000000000000
      0000000000000000000000000000000000000000000005050506414A4FC95271
      81FF6B8C9AFF9EAFB6FF8D9497FF4F4F4FE70000000015151517000000000F0F
      0F102B2B54EA121257FF2B2BACFE8080FCFF9999FCFF8181DAFB575786E70101
      010200000000151515173F3F3F5500000000000000005E5D5B93FDF6E9FFFCF4
      E6FFFCF3E2FFFCD0C1FFFE918DFFFF8080FFFF6363FFFD5B55FFFD6258FFFAC4
      ABFFF9E7C7FFF9E6C4FF5957548D000000000000000000000000000000000000
      00004B4B408EC9C48AFFF7EED8FFF5EDD5FFF2EBD2FFF0E9CFFFEFEBD5FFF1EE
      DDFFF0EDDBFFE6E4C4FFC8C58FFF52502AE60000000000000000000000000000
      00000000000000000000000000000000000000000000000000001818181B424E
      52CC557485FF73929EFFA6B4BBFF646668E70000000000000000000000005656
      579B424295FA313199FF2F2F74E537373A52585871E9333333425E5E5EA96E6E
      6EE57C7C7CE7797979E65353539100000000000000005E5D5C93FDF7EDFFFDF6
      EAFFFCF4E7FFFCF3E3FFFCF0DEFFFCBCADFFFDB1A2FFFBE8D2FFE5D3B9FFBA8F
      63FFB99064FFB98F66FE504D4B7D000000000000000000000000000000000000
      000021212127A1A040FFF7EED8FFF7EED7FFF4ECD4FFF2EAD1FFEFE9CEFFECE7
      CBFFEAE5C7FFE7E3C5FFA9A853FF4C4C46910000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001E1E
      1E23434F55D3587888FF7B97A3FF798285E77E7E7EDA808080E37F7F7FE56666
      66C748484E7E5D5D8EEB2E2E2F3D000000005A5A5A945A5A5AB3000000004343
      435C0F0F0F10000000000000000000000000000000005E5E5D93FEF9F2FFFDF8
      EEFFFDF6EBFFFCF5E7FFFCF3E4FFFCF2E1FFFBF0DDFFFBEFDAFFDFCDB4FFE3A1
      5EFFE1A05EFF6F6456B600000000000000000000000000000000000000000000
      000000000000525039B2C3BF81FFF8EFDAFFF7EED7FFF4ECD4FFF2EBD1FFEFE9
      CEFFEDE7CAFFC6C58CFF5A591EF0060606070000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000020202026455358D55B7C8CFF627078E70000000000000000000000000000
      0000616161BB5A5A5A9600000000000000005A5A5AAA59595997000000000000
      000000000000000000000000000000000000000000005E5E5D93FEFBF6FFFEFA
      F2FFFDF8EFFFFDF7ECFFFDF5E8FFFCF4E5FFFCF2E2FFFBF1DEFFDFCEB6FFE09F
      5EFF6E6356B50000000000000000000000000000000000000000000000000000
      0000000000000606060757552DCBB3B062FFE3DCB9FFF6EDD7FFF3ECD4FFE2DD
      BAFFB7B56CFF615E1AF321212127000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002626262F454F54C4434F54D50000000000000000000000000000
      0000696969DB2F2F2F3A0000000000000000676767D03A3A3A4D000000000000
      000000000000000000000000000000000000000000005A5A5996B6B5B3F1B6B3
      B2F1B6B3B0F1B5B2ADF1B5B2AAF1B5B1A8F1B4B0A6F1B4B0A4F1ACA091F36B61
      56AF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000101010244443F6E5F5E22E18C8B26FC8F8F28FD6563
      1BEE4E4C41A00808080900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005D5D5DB24B4B4B6D00000000000000000909090A00000000000000000000
      00000000000000000000000000000000000000000000000000000C0C0C0D2020
      202520202025202020251818181B00000000000000001A1A1A1D202020252020
      2025202020250A0A0A0B000000000000000000000000000000003F4447A02828
      283230303042393A3A5814141416000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001313
      13153F7E94CC1F1F1F2400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000101
      0102457587BB3C3E3F5600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000002020203917A78B6FF24
      0EFFFF1F0EFFFF1A0EFFEB605CF50303030404040405F35651F9FF1A0EFFFF1F
      0EFFFF240EFF817472A70000000000000000000000000000000036648EFE3164
      95FF1B4758FF1B8A57FF1E8E5CFE315448CD395049B9246C51EE31544ACD0C0C
      0C0D000000000000000000000000000000000909090A41413D6846463F774D50
      4A8C49B6E8FA2FA6CEFA4D4E458C46463F7746463F7746463F7746463F774646
      3F7746463F7746463F7741413D65070707080909090A41413D6846463F774849
      417C55A1C3ED2AB4E8FE515952AA46463F7746463F7746463F7746463F774646
      3F7746463F7746463F7741413D65060606070000000002020203967F79BBFF26
      00FFFF2100FFFF1C00FFF16254F80303030404040405F55646FAFF1C00FFFF21
      00FFFF2600FF877976AD000000000000000000000000000000003B6A95FE3472
      A5FF0A698AFF008CA9FF1E9061FF22A268FF21A069FF1F9173FF1F9674FF2560
      4EE7404B4894286451E44145457900000000595943CAB7B7A2FFB7B7A2FFB7B7
      A2FFB1BAADFF55BFF0FF34AFDBFFA9AE9FFFAAAA95FF8E8D71FFA3A38FFFB7B7
      A2FFB7B7A2FFB7B7A2FFB7B7A2FF5B5B47C9595943CAB7B7A2FFB7B7A2FFB7B7
      A2FFB6B8A6FF6BBBE2FF2FB7ECFF94A8A2FFB0B09CFF919075FF9B9B86FFB5B5
      A0FFB7B7A2FFB7B7A2FFB7B7A2FF5B5B46C90000000002020203968179BBFF36
      00FFFF3200FFFF2D00FFF16D54F80303030404040405F56246FAFF2D00FFFF32
      00FFFF3700FF877A76AD00000000000000000000000000000000245F79F8028A
      A9FF00BFEDFF0090B1FF01677BFF156060FF1B7A7AFF1C7D7DFF1D8383FF1E88
      85FF208E85FF1F8E8CFF1C766BFC1E1E1E246A6A3DEDFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFEFF6FDFF5FC2F4FF5B9391FFB3B383FFD7D6C2FFC1C09DFF7775
      51FFF7F7F7FFFFFFFFFFFFFFFFFF6D6D46EC6A6A3DEDFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFBFDFFFF80C7F0FF4CA4BEFFA7A66DFFD3D2BAFFCECDB0FF8685
      4EFFDEDEDAFFFFFFFFFFFFFFFFFF6E6E44EB0000000002020203968279BBFF47
      00FFFF4300FFFF3E00FFF17854F80303030404040405F56E46FAFF3E00FFFF43
      00FFFF4800FF877B76AD000000000000000000000000080808093F474878018E
      A7FF0094B4FF00C1EFFF009BBDFF075D69FF135656FF175555F93F4545923247
      47C61D5353F12E4949D01B7878FE374B4AB96A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFA3A490FFD5D2BAFFFCF9F5FFB7B76DFFFAF9F8FFE6E5
      DAFF818064FFFFFFFFFFFFFFFFFF6E6E45ED6A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFD6D6CFFFB8B88DFFFCF9F4FFFCFAF8FFFBFAF9FFF7F6
      F3FF7A7A47FFFFFFFFFFFFFFFFFF6D6D45EC0000000002020203968479BBFF58
      00FFFF5300FFFF4F00FFF18354F80303030404040405F57A46FAFF4F00FFFF54
      00FFFF5900FF877D76AD00000000000000003534384A3F3E4867030303043059
      5FC2009FBFFF008DAEFF00AED7FF00A1C4FF106368FF395252BA111111130000
      000000000000000000003B3D3D5E3F4747926A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFAAA96FFFF9F5E7FFF4F2E5FFA7A64CFFF4F3E8FFF9F8
      F4FF949459FFFFFFFFFFFFFFFFFF6E6E45ED6A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF8F8D66FFF5F1E5FFDEDDBAFFDCDBB7FFDCDBB7FFEEEE
      DFFFB0AF81FFE6E6E5FFFFFFFFFF6D6D45EC0000000002020203968679BBFF69
      00FFFF6400FFFF5F00FFF18E54F80303030404040405F58646FAFF6000FFFF65
      00FFFF6900FF877D76AD000000000000000016161619282F92EA1E8A63FF1F95
      5DFE177151FD026F80FF009DC1FF009DC2FF0099BAFF187476FF365757C51010
      1012000000000000000000000000000000006A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB0AF76FFD8D5ABFFBBBA72FF949426FFBABA74FFE6E5
      CBFF9D9C64FFFFFFFFFFFFFFFFFF6E6E45ED6A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF949263FFFAF5EAFFAFAE5BFFAAA952FFAAA952FFD9D9
      B2FFB5B388FFDAD9D6FFFFFFFFFF6D6D45EC0000000002020203968879BBFF7A
      00FFFF7500FFFF7000FFF19954F80303030404040405F59246FAFF7100FFFF76
      00FFFF7A00FF877F76AD00000000000000000000000028514DDF18259AFF134E
      51FF1E915BFF20A061FF0C5E55FF00B2DCFF008AAAFF00839EFF1E8888FF4148
      487E000000000000000000000000000000006A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF99966CFFF1ECDCFFFCF7EBFFAAA951FFFDF9F2FFF1EF
      E4FF848253FFFFFFFFFFFFFFFFFF6E6E45ED6A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFB7B5A0FFDAD6BBFFFDF6EAFFFDF8EEFFFDF9F2FFFCF9
      F2FF989757FFFDFDFDFFFFFFFFFF6D6D45EC0000000002020203968A79BBFF8D
      00FFFF8800FFFF8300FFF1A254F80303030404040405F59E46FAFF8400FFFF88
      00FFFF8D00FF877F76AD0000000000000000414543781E8E5CFE22A262FF1E91
      60FF166F4CFF136444FF21A264FF10614FFF00C6F4FF0093B5FF056A7BFF414F
      4F93000000000000000000000000000000006A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF4F4F0FFA6A562FFE7E1CFFFE2DFBFFFE7E3D3FF9D9B
      59FFE3E2DEFFFFFFFFFFFFFFFFFF6E6E45ED6A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF9E9D66FFD9D5BBFFF9F3E8FFF0ECE0FFADAC
      74FFB8B7A9FFFFFFFFFFFFFFFFFF6D6D45EC0000000002020203968C79BBFF9E
      00FFFF9900FFFF9400FFF1AD54F80303030404040405F5A846FAFF9400FFFF99
      00FFFF9E00FF878176AD000000000000000026674FE91F9869FF1D8F6DFF1E91
      6DFF20A067FF209C64FF18764FFF21A266FF0B5D56FF00C4F2FF009DBEFF2A53
      5ACE000000000000000000000000000000006A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAF8FF9E9C80FF8D8A5AFF939173FFF3F3
      F1FFFFFFFFFFFFFFFFFFFFFFFFFF6E6E45ED6A6A3BEFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB3B19BFF8E8C5DFF8C8965FFDEDD
      D6FFFFFFFFFFFFFFFFFFFFFFFFFF6D6D45EC0000000002020203968E7CBBFFAF
      0AFFFFAA00FFFFA500FFF1B854F80303030404040405F5B446FAFFA500FFFFAA
      00FFFFB00BFF878278AD0000000000000000384946B4114D4DFF166161FF1769
      69FF1C8079FF1F9470FF21A26BFF21A16AFF157159FF0093B5FF00AFD9FF009D
      C0FE3536364C00000000000000000000000069693BEFEEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFF6D6D44ED69693BEFEEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFF6C6C44EC0000000002020203969081BBFFC3
      34FFFFBD28FFFFB81CFFF1C15FF80303030404040405F5C053FAFFB81DFFFFBE
      29FFFFC335FF87837AAD000000000000000006060607304444C71A7272FF1B7A
      7AFF1D8181FF1C7B7BFF1E8A7BFF1F9177FF0C4B4DFE00ADD2FF00ACD5FF0397
      B8FF6A8388F61717171A0000000000000000755F3DFA92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF74603DF7755F3DFA92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF735F3DF70000000002020203969286BBFFD6
      5FFFFFD052FFFFCB46FFF1CD79F80303030404040405F5CF6FFAFFCB47FFFFD1
      53FFFFD65FFF87857DAD00000000000000000000000000000000314343CB578E
      90FF375959FF589192FF1F6162FF1C4A4AF03E43446E048DA5FE1198B5FF9EAC
      AEFF7B7B7BFF5E5E6CDB02020203000000009F8869F8E9C2A5FFE8C1A3FFE8C0
      A2FFE7BFA0FFE7BE9EFFE6BD9CFFE6BB9AFFE5BA99FFE5B997FFE5B895FFE4B7
      93FFE4B691FFE3B58FFFE3B48EFF957957F59F8869F8E9C2A5FFE8C1A3FFE8C0
      A2FFE7BFA0FFE7BE9EFFE6BD9CFFE6BB9AFFE5BA99FFE5B997FFE5B895FFE4B7
      93FFE4B691FFE3B58FFFE3B48EFF957956F50000000002020203949389BAFFE9
      8BFFFFE47DFFFFDE70FFEFD892F70303030404040405F5DA8DFAFFDE71FFFFE4
      7DFFFFE98CFF86857FAC00000000000000000000000000000000394242BC5070
      70FF3D4040FF51504DFF2A4445FF2D4545CF00000000485759AD8F9090FF9191
      91FF7D7DAFFF5C5CA9FC0E0E0E0F0000000052504B929E9480F5A79988F9A698
      86F9A69785F9A69684F9A69682F9A69582F9A59481F9A5947EF9A4937DF9A493
      7BF9A49279F9A49179F99B8B71F54F4E488B52504B949E9480F5A79988F9A698
      86F9A69785F9A69684F9A69682F9A69582F9A59481F9A5947EF9A4937DF9A493
      7BF9A49279F9A49179F99B8B71F54F4D488A00000000010101025C5C5A7BD4D4
      BAE8D5D4B7E9D5D1B2E9B0AD9CD00303030403030304BAB6A2D7D5D1B3E9D5D4
      B7E9D4D4BAE85050506B0000000000000000000000000000000040444483518A
      8BFF315E5EFF4B8081FF226A6AFF394141AC0000000001010102666672DC7070
      C3FF59599FF93838395200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010101020303
      0304030303040303030403030304000000000000000003030304030303040303
      0304030303040101010200000000000000000000000000000000101010123246
      46C5374848BC275252E02C4747D31F1F1F250000000000000000141414174444
      487E141414160000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004040405272828303031
      313D131313150000000100000000000000000000000000000001131313154E81
      82AB404949610202020300000000000000013C646FC543555A9F1717171A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003E3F405946626C9A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003E3F405946626C9A00000000000000000000000008080809495B5B7B0BF0
      F4FA516E6F961212121400000001000000000909090A2B2B2B3540C8CCE603FA
      FDFE3D4242570202020300000000000000012C96ADF248CBF8FF3BBBE6FF3299
      BDFA37758BD94150558908080809000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004042
      425E23B0E2FE33C3FFFF383A3C4D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004042
      425E23B0E2FE33C3FFFF383A3C4D0000000000000000000000001E1E1E2311EC
      F5FB0CD4F8FC32CBD1E837393949212121274447475F28D8DEEF07CFFDFE17E6
      EFF829292932000000010000000000000001269AACEB42AECEFF4CCEFAFF44CB
      F9FF3CC8F9FF33C5F9FF2AAEDEFE278FB4F2386A7BBE3C404261010101020000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004143436028B0
      E2FE3AC3FFFF5568749E00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004143436028B0
      E2FE3AC3FFFF5568749E000000000000000000000000000000001919191C47AD
      B2D400B6FFFF00B5FFFF13E7F3FA54A8A9CF19E4ECF700C0FFFF00BBFFFF2ECA
      CFE72424242A000000000000000000000001269CACEB26C9E5FF56C8EEFF51CF
      FAFF49CCF9FF40C9F9FF38C6F9FF30C3F9FF27C0F9FF1FBBF5FF1A9FD0FB257F
      A0DE3D57609B0000000000000000000000000000000000000000000000000000
      0000080808093636354A3C3C3B571414141600000000424444622DB1E3FE40C4
      FFFF5568739C0000000000000000000000000000000000000000000000000000
      0000080808093636354A3C3C3B571313131500000000424444622DB1E3FE40C4
      FFFF5568739C00000000000000000000000000000000000000000E0E0E0F4D55
      557512D9F3FA009FFFFF00A6FFFF04CCFDFE00B2FFFF00AEFFFF02CEFDFE4F80
      81AA13131315000000000000000000000001279EACEB20E0FBFF43A3B9FF5ED4
      FAFF56D1FAFF4DCEFAFF45CBF9FF3DC8F9FF34C5F9FF2CC2F9FF24BFF9FF1BBC
      F8FF13B7F4FF2020202600000000000000000000000000000000000000014C4C
      46946D6C24F98B8B50EE888859E488873DF4545436D532B1E1FE46C5FFFF525F
      688E000000000000000000000000000000000000000000000000000000014D4C
      47956D6C24F98B8B50EE888859E488873DF4545436D532B1E1FE46C5FFFF525F
      678E000000000000000000000000000000000000000000000000020202032C2C
      2C3623DEE6F400A5FFFF00A2FFFF00A6FFFF00A9FFFF00ADFFFF13E0F5FB4045
      455C0A0A0A0B000000000000000000000001289FACEB22E3FBFF24CDE6FF5FBD
      D9FF63D6FAFF5AD3FAFF52D0FAFF4ACDF9FF41CAF9FF39C7F9FF31C4F9FF28C1
      F9FF20BEF9FF4253597E000000000000000000000000000000004E4D45A79090
      4FF4B5B5B2D9C9C9C8E3C2C2AAEC8B8B89B384826EC8658059F4525C638A0000
      00000000000000000000000000000000000000000000000000004E4E45A89090
      4FF4B5B5B2D9C8C8C7E3B7B6B4D78B8B89B384826EC8658059F4515B62880000
      00000000000000000000000000000000000000000000000000000A0A0A0B4143
      435826E4ECF700B2FFFF00A1FFFF00A5FFFF00A9FFFF00ACFFFF09D0FCFE23E0
      E6F44850506C1E1E1E23050505060000000129A2ACEB23E6FBFF21E2FBFF4196
      A6FF6FDAFAFF67D7FAFF5FD4FAFF56D1FAFF4ECEFAFF46CBF9FF3EC8F9FF35C5
      F9FF2DC2F9FF368BAAD7070707080000000000000000222222297D7D28FAB0B0
      AED5CBCBCBE5BEBDBADD8C8C17FFA2A09FC782807DA988876AD44D4C41960000
      000000000000000000000000000000000000000000002323232A7E7E28FAB2B1
      AFD6CBCBCBE5BCBBB9DAADADABD0A2A09FC782807DA988876AD44D4C41950000
      00000000000000000000000000000000000000000001050505064247475E1DDE
      E6F401B4FEFF009CFFFF00A0FFFF00A4FFFF00A8FFFF00ACFFFF00AFFFFF00BD
      FFFF12E5F4FA47A5A8CC353535430D0D0D0E2BA3ACEB25E9FBFF23E5FBFF23CE
      E4FF5A9CADFF64B9D1FF5DB6D1FF57B3D1FF3A88F9FF45AEFBFF4ACDF9FF2882
      F6FF2480F7FF2DB4F9FF363B89B303030304000000004D4C4695838360D7C7C6
      C6E2BDBCBCDBB2B2ADD48C8C17FF9A9996C07F7E7AA77E7C78A76D6C2FE40000
      000000000000000000000000000000000000000000004D4C4697838360D7C8C8
      C7E3C3C2B6E5B6B6A8DFAEADA0D9A8A799D48D8B84B77E7C78A76E6C30E40000
      0000000000000000000000000000050505061E1E1E22536E6E9514E2F1F900A2
      FFFF0097FFFF009BFFFF009FFFFF00A3FFFF00A7FFFF00ABFFFF00AFFFFF00B5
      FFFF00BDFFFF02DEFDFE14E5EAF54751516D2CA6ADEC27ECFBFF25E8FBFF23E5
      FBFF23BDD2FF23B0C6FF22AEC5FF277688FF4C8DA3FF2859FDFF1D51F2FF052E
      E3FF0834E7FF0C33F7FF455E6E9900000000000000004F4D41B57D7D6AC2BBBB
      A4E6979731FE97962FFE828203FF979731FD979630FD9C998CC8828240E70000
      000000000000000000000000000000000000000000004F4E41B67D7D6AC2C2C1
      B9E2808000FF808000FF808000FF808000FFACAB5CFC82807AAA828141E60000
      0000000000000000000000000000101010124C7A7CA401FBFDFE0ED3F9FD13D2
      F2F913D7F1F914DFF3FA0CCCFBFE00A2FFFF00A6FFFF10E0F9FD1FE3EAF628DF
      E5F431CED4EA3ABCC1DE43A5A8CD4A6060842EA7ADEC28EFFCFF26EBFBFF24E8
      FBFF22E4FBFF20E0FBFF1EDDFAFF1DD0EFFF34464CFF295FEAFF0025D9FF0050
      E2FF0363E9FF093AE9FF2E7EE0F306060607000000004D4C4595838264D37D7D
      7AA5AAA9A2D1A3A298CD8B8B14FF8A887FB58F8C82B988857EB0747334E40000
      000000000000000000000000000000000000000000004D4C4597838264D3807F
      7DA8B2B297E3AAA98EDE9F9D84D69B9A7FD29A9888C786837CAE747334E30000
      00000000000000000000000000000C0C0C0D3E44445A48575777434B4B644348
      48603E414155404141551DE3EDF700A4FFFF00A6FFFF1DE2EBF62626262D0202
      02031919191C222222281E1E1E230C0C0C0D2DA8ACEB2AF2FCFF28EEFCFF26EB
      FBFF24E7FBFF22E3FBFF20E0FBFF1EDBFAFF1448C6FF0724DDFF0027DAFF00E3
      FAFF00F6FEFF0758ECFF1E6EFCFF3E4577A1000000001F1F1F24848431F87271
      6E9A7472709A7D7B77A58A8A17FE807E79A885837DAD8A8872CC4E4C3F9C0000
      0000000000000000000000000000000000000000000020202026848431F87271
      6E9A7472709A7775729E7C7A76A3807E79A885837DAD8A8772CB4E4C3F9B0000
      0000000000000000000000000000000000010808080906060607010101020505
      05060404040506060607519699C102C4FEFF00B9FFFF44ADB1D30E0E0E0F0000
      0000000000000000000000000000000000012DA8ACEB2CF5FCFF2AF1FCFF28ED
      FCFF26EAFBFF23E6FBFF21E3FBFF1FDFFBFF1BCFFAFF0742ECFF032CDFFF00B6
      F4FF00C9F8FF0946ECFF3C669FC41818181B00000000000000004E4D43A48B8A
      52EB787673A27F7D78A7ADAC74F085827CAC878578BF72702EE70C0C0C0D0000
      00000000000000000000000000000000000000000000000000004E4D43A48B8A
      52EB787673A17C7A75A3807E79A885827CAC878578BF73712DE60B0B0B0C0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000030303043B3C3C4E15DFF1F90ACFF8FC5065658A0C0C0C0D0000
      0000000000000000000000000000000000012DA8ACEB2CF5FCFF2BF4FCFF29F0
      FCFF27EDFBFF25E9FBFF23E6FBFF21E2FBFF1FDEFBFF0F71F7FF0631E5FF0633
      E4FF0838E8FF1A5FEBFB41496385000000000000000000000000000000004D4C
      4394828130F48A8962DE898968D68B8A54E75D5B2FD415151518000000000000
      0000000000000000000000000000000000000000000000000000000000004D4C
      4394828130F48A8962DE898968D68B8A54E75D5B2FD414141417000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002929293130C8CFE70CEAF4FA37383848020202030000
      0000000000000000000000000000000000003F6768B02CF5FCFF2CF5FCFF2BF3
      FCFF29F0FCFF26E7F6FF3C6C71B642585A8F414F5A8C2F3890C138417DB1093A
      E9FF1648DEF63C3F49603F3F66891818181B0000000000000000000000000000
      0000040404053838374F3F3E3C5E1B1B1B1F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040404053838374F3F3E3C5E1B1B1B1F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000121212144F64648826C9CDE51E1E1E23000000000000
      00000000000000000000000000000000000007070708416163A53A8386C93A83
      86C93A8386C94257599400000001000000000D0D0D0E0606060738394053294B
      79FF2F4D76F80303030400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000001080808093334344206060607000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001818191C3F44
      4F693F434F690000000100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000050505061515151811111113000000010000000000000000000000000000
      00000000000000000000000000000000000000000000535353988A8A8AF18888
      88F1858585F1838383F1818181F17E7E7EF17C7C7CF1797979F1767676F17575
      75F1717171F1717171F14D4D4D8E00000000000000011919191D000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000D0D0D0E5E57509E695E55AD695E
      55AD695E55AD695E55AD695E55AD695E55AD695E55AD695E55AD695E55AD695E
      55AD695E55AD695E55AD5C554F9B0909090A00000001403D3D65543B3BAC6B2E
      2ED4882929E7B02C2CF2AC2A2AEF8F2929E3753232C9593D3D9E2E2D2D3B0000
      0000000000000000000000000000000000000000000057575795BFBFBFFFBCBC
      BCFFB8B8B8FFB5B5B5FFB2B2B2FFAEAEAEFFABABABFFA8A8A8FFA4A4A4FFA1A1
      A1FF9D9D9DFF9A9A9AFF4F4F4F8B000000004D6D7CAA26ADDCFC363737490000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000049474570EDD3BEFFF1E0D1FFF1E0
      D1FFF1E0D1FFF1E0D1FFF1E0D1FFF1E0D1FFF1E0D1FFF1E0D1FFF1E0D1FFF1E0
      D1FFF1E0D1FFF1E0D1FFEDD1BBFF454342684B3A3AA3930C0CFDB51414FFCB1D
      1DFFE12626FFF72F2FFFFF2C2CFFFF2323FFFF1A1AFFFF1111FFBF1313F1443D
      3D68202021263B70799F00000000000000000000000057575795C3C3C3FF9797
      97FF747474FFB9B9B9FFB6B6B6FF545454FF9F9F9FFFACACACFFA8A8A8FFA5A5
      A5FFA2A2A2FF9E9E9EFF4F4F4F8B000000003F41425C46BDF4FD27B1E3FE545B
      4CB44E4E3E924E4E3E924E4E3E924E4E3E924E4E3E924E4E3E924E4E3E924E4E
      3E924E4E3E924E4E3E924A4A3F831919191D4A474571F4E7DCFFE7D6C7FFDFC5
      B1FFDFC5B1FFDFC5B1FFE4DAD1FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7C7FFC7C7
      C7FFC7C7C7FFE6E6E6FFF3E4D8FF464442694E3232B99E0B0BFFB51414FFAA19
      1CFF582145FF3F3272FF4B306DFF73213FFFD51618FFFF1111FFEB0909FD4D3F
      3F7F07070708383F415431343543272829310000000058585895939393FF5F5F
      5FFF606060FF777777FFBABABAFF191919FF989898FFB0B0B0FFADADADFFA9A9
      A9FFA6A6A6FFA2A2A2FF4F4F4F8B00000000626245E0B5C4C9FF4EC1F7FF2EB4
      E6FFA4B1B1FFC1C1B9FF979685FF908F7AFFB6B6AEFFC5C5BDFFC5C5BDFFC5C5
      BDFFC5C5BDFFC5C5BDFFC5C5BDFF64644ADB4A474571F4E7DCFFD6B194FFD8B3
      92FFD8B392FFD8B392FFC5AE9BFFC9C9C9FFE0E0E0FFE0E0E0FFE0E0E0FFE0E0
      E0FFD9D9D9FFC7C7C7FFF3E4D8FF464443690D0D0D0E493F3F8B29264EF50846
      B3FF0047B3FF001A42FF003381FF0066FFFF1052D0FF422A52EE433D3D680000
      0001000000003F525B7A222323291C1C1C200000000059595995A2A2A2FF6363
      63FF5F5F5FFF8A8A8AFFBEBEBEFF191919FF9D9D9DFFB4B4B4FFB1B1B1FFADAD
      ADFFAAAAAAFFA7A7A7FF4F4F4F8B04040405747443F6FFFFFFFFE3F1FCFF57C4
      F9FF47A4C1FF959548FFBDBC93FFC8C8A8FF9A9A57FF8C8A74FFFEFEFEFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF747449F14A474571F4E7DCFFD6B396FFD9B6
      96FFD9B696FFD9B696FFC5AE9BFFCECECEFFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
      E6FFE0E0E0FFC7C7C7FFF3E4D8FF464443690000000000000000203773EE1B39
      92FF3E2B6DFF612050FF592763FF41429BFF0D56DAFF464452A0000000000000
      00000000000000000000111111133E5B6A8D0000000059595995D0D0D0FFB2B2
      B2FF999999FFC6C6C6FFC2C2C2FF7D7D7DFFB2B2B2FFB8B8B8FFB5B5B5FFB2B2
      B2FFAEAEAEFFABABABFF5050508B05050506757543F6FFFFFFFFFFFFFFFFCAD4
      D5FFA1A66CFFF9F6EFFFFCFAF8FFFAFAF9FFFAF9F9FFB9B990FF9F9D8FFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF747449F14A474571F4E7DCFFD6B396FFD9B6
      96FFD9B696FFD9B696FFC5AE9BFFCECECEFFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
      E6FFE0E0E0FFC7C7C7FFF3E4D8FF4644436900000000000000004D393AAA9300
      00FFA10000FFB20A0AFFE7A6A6FFDA5151FF811A1CE10E0E0E0F05050506363B
      3E4F020202033F5258760000000002020203000000005A5A5A95D4D4D4FF9292
      92FF737373FFCACACAFFC7C7C7FF494949FFADADADFFBDBDBDFFB9B9B9FFB6B6
      B6FFB2B2B2FFAFAFAFFF5050508B05050506757543F6FFFFFFFFFFFFFFFF8684
      48FFF4F0E3FFFCF9F2FFFBF9F5FFFCFAF8FFFBFAF9FFF8F8F6FF7B7A36FFFEFE
      FEFFFFFFFFFFFFFFFFFFFFFFFFFF747449F14A474571F4E7DCFFD6B396FFD9B6
      96FFD9B696FFD9B696FFC5AE9BFFCECECEFFE6E6E6FFE6E6E6FFE6E6E6FFE6E6
      E6FFE0E0E0FFC7C7C7FFF3E4D8FF464443690000000000000000483F3F809300
      00FFA10000FFBC2929FFF5D9D9FFE58585FF583D3D9D00000000000000002F31
      333F0F0F0F103E62698C0808080900000000000000005A5A5A95D8D8D8FF7A7A
      7AFF545454FFCCCCCCFF4A4A4AFF787878FF4B4B4BFFA1A1A1FFBDBDBDFFBABA
      BAFFB7B7B7FFB3B3B3FF5151518B05050506757543F6FFFFFFFFFFFFFFFFA2A1
      59FFFCF7EBFFFCF7EDFFFBF8F3FFFBF9F6FFFAF9F7FFFCFBF9FF989854FFF0F0
      F0FFFFFFFFFFFFFFFFFFFFFFFFFF747449F14A474571F4E7DCFFD5B295FFD3AC
      8CFFD3AC8CFFD3AC8CFFCAB4A2FFAEAEAEFFBABABAFFBABABAFFBABABAFFBABA
      BAFFB6B6B6FFCCCCCCFFF3E4D8FF4644436900000000000000003D3B3B5D9200
      00FFA40505FFB41010FFC93232FFB50404FB3433334700000000000000003F56
      5F8121222228000000000000000000000000000000005B5B5B95DDDDDDFF7D7D
      7DFF565656FFD2D2D2FF959595FF4F4F4FFF7A7A7AFFBBBBBBFFC2C2C2FFBEBE
      BEFFBBBBBBFFB7B7B7FF5151518B05050506757543F6FFFFFFFFFFFFFFFF9594
      4BFFFCF6E9FFFCF7EBFFFCF8EFFFFCF9F3FFFBF9F6FFFDFAF6FF919146FFFBFB
      FBFFFFFFFFFFFFFFFFFFFFFFFFFF747449F14A474571F4E7DCFFEDCBA7FFE9BE
      92FFE9BE92FFE9BE92FFE9BE92FFE9BE92FFE9BE92FFE9BE92FFE9BE92FFE9BE
      92FFE9BD91FFF2DFCDFFF3E4D8FF4644436900000000000000001919191D7707
      07F7CC7272FFE5AEAEFFBB0000FF761E1EDB010101022B2A2A364C3535B30F0F
      0F1000000000000000000000000000000000000000005B5B5B95E1E1E1FFC5C5
      C5FFB9B9B9FFD7D7D7FFD3D3D3FFA0A0A0FFC9C9C9FFC9C9C9FFC6C6C6FFC2C2
      C2FFBFBFBFFFBCBCBCFF5151518B05050506757543F6FFFFFFFFFFFFFFFFA7A5
      8AFFD2CEB0FFFCF6E9FFFCF7ECFFFDF9EFFFFDFAF1FFDCD9C4FF797651FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF747449F14A484571F4E7DDFFEAC095FFEEC9
      A4FFEECAA5FFEECAA6FFEECBA6FFEECBA7FFEFCBA8FFEFCCA8FFEFCCA9FFEFCD
      AAFFEEC9A5FFEDCFB0FFF3E4D8FF464443690000000000000000000000005627
      27D1D18282FFECC2C2FFC62626FFBA0101FD533636B0A70F0FEE513F3F8A0000
      000000000000000000000000000000000000000000005C5C5C95E5E5E5FF9B9B
      9BFF666666FFDBDBDBFFD7D7D7FF373737FFB7B7B7FFCDCDCDFFCACACAFFC7C7
      C7FFC3C3C3FFC0C0C0FF5252528B05050506757543F6FFFFFFFFFFFFFFFFFEFE
      FEFF949157FFCAC7A4FFF6F0E4FFF6F1E6FFCECCAFFF868541FFF4F3F2FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF747449F14A484571F4E7DDFFEBC299FFEBC0
      94FFEBC095FFEBC095FFEBC195FFEBC195FFEBC195FFEBC296FFEBC296FFECC2
      96FFEBBF93FFF1D8BEFFF3E4D8FF464443690000000000000000000000001818
      181C503535B1B12727FDBB0000FFD02121FFDB2020FF941717E5151515180000
      000000000000000000000000000000000000000000005D5D5D958B8B8BFF6161
      61FF818181FF636363FFDCDCDCFF1D1D1DFFB5B5B5FFD2D2D2FFCECECEFFCBCB
      CBFFC7C7C7FFC4C4C4FF5353538B050505066B6B39F6C3C3BAFFC3C3BAFFC3C3
      BAFFC3C3BAFF908E71FF737135FF757335FF7D7B5DFFC1C1B8FFC3C3BAFFC3C3
      BAFFC3C3BAFFC3C3BAFFC3C3BAFF6B6B40F24A484571F1DECEFFF6EEE7FFF6EE
      E7FFF6EEE7FFF6EEE7FFF6EEE7FFF6EEE7FFF6EEE7FFF6EEE7FFF6EEE7FFF6EE
      E7FFF6EEE7FFF6EEE7FFF0DBCAFF464443690000000000000000000000000000
      000000000001463F3F78891313EAD33232FFDD3131FF533E3E93000000000000
      000000000000000000000000000000000000000000005D5D5D95DDDDDDFF8888
      88FF646464FFCCCCCCFFE0E0E0FF1E1E1EFFB9B9B9FFD6D6D6FFBBBBBBFF7777
      77FF777777FF787878FE4B4B4B7C0B0B0B0C886A49FEAE885FFFAE885FFFAE88
      5FFFAE885FFFAE885FFFAE885FFFAE885FFFAE885FFFAE885FFFAE885FFFAE88
      5FFFAE885FFFAE885FFFAE885FFF816645FC2C2C2C38AB8C71E8E5B790FFE5B7
      90FFE5B790FFE5B790FFE5B790FFE5B790FFE6B791FFE6B791FFE6B891FFE6B8
      91FFE6B891FFE6B892FFA58A70E4282828320000000000000000000000000000
      000000000000000000002C2C2C39642929CB971111EC1D1D1D22000000000000
      000000000000000000000000000000000000000000005E5E5E95F2F2F2FFE4E4
      E4FFD4D4D4FFE7E7E7FFE4E4E4FFC7C7C7FFD9D9D9FFDADADAFFB7B7B7FF8080
      80FF7D7D7DFF5C5C5CB60000000001010102A38E6DF9EAC6ACFFEAC5AAFFE9C4
      A8FFE9C3A6FFE8C1A4FFE8C0A3FFE7BFA1FFE7BE9FFFE7BD9DFFE6BC9BFFE6BB
      9AFFE5BA98FFE5B996FFE4B894FF967C59F500000000373636496F6E6DAD6F6E
      6DAD6F6E6DAD6F6E6DAD6F6E6DAD6F6E6DAD6F6E6DAD6F6E6DAD6F6E6DAD6F6E
      6DAD6F6E6DAD6F6E6DAD32323242000000000000000000000000000000000000
      00000000000000000000000000000A0A0A0B403D3D6500000000000000000000
      000000000000000000000000000000000000000000005E5E5E95F6F6F6FFF2F2
      F2FFEFEFEFFFECECECFFE8E8E8FFE5E5E5FFE1E1E1FFDEDEDEFFBABABAFF7D7D
      7DFF5C5C5CB40000000000000000000000004C4B48808C836FE9918873ED9188
      72ED918871ED918771ED918670ED90866FED90856EED90846EED90846DED9084
      6CED90846BED90846BED877A64E7484744760000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A5A5A98B3B3B3F1B2B2
      B2F1B0B0B0F1ADADADF1AAAAAAF1A8A8A8F1A6A6A6F1A4A4A4F1939393F25A5A
      5AAD000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000292929343434344A3434344A3434344A3333334703030304000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000292929343434344A3434344A3434344A3333334703030304000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000525252C9CCCCCCFFCCCCCCFFCCCCCCFF707070EE1717171A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000525252C9CCCCCCFFCCCCCCFFCCCCCCFF707070EE1717171A000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000020202026303030413030
      3041303030413030304130303041303030413030304130303041303030413030
      304130303041303030411E1E1E23000000001818181B2A2A2A35282828310101
      0102585858C29D9D9DFF2C2C2CFF616161FF898989EA1717171A000000000000
      0000000000000000000000000000000000001818181B2A2A2A35282828310101
      0102585858C2FFFFFFFFFFFFFFFFFFFFFFFF898989EA1717171A000000000000
      0000000000000000000000000000000000000000000000000000493E3E60744F
      4F9B7357579B715A5A9B7156569B7052529B704F4F9B6E4B4B9B6E47479B6C43
      439B6C40409B433A3A5B0000000000000000545345B197925FFD9B965BFF9B94
      58FF9A9254FF9A9151FF9A904FFF9A8E4BFF998D48FF998B45FF988A41FF9788
      3FFF97883FFF97883FFF928241FD535140B04444446B3A3A3A5B353535460303
      0304585858C2919191FF000000FF4E4E4EFF898989EA1717171A000000000000
      0000000000000000000000000000000000004444446B3A3A3A5B343434450202
      0203585858C2FFFFFFFFFFFFFFFFFFFFFFFF898989EA1717171A000000000000
      00000000000000000000000000000000000000000000000000007946469FF850
      50FFF27575FFED8585FFEA7373FFE66262FFE35252FFDF4141FFDC3131FFD820
      20FFD51010FF683D3D970000000001010102707040F1FBF3E5FFC5E7E4FF4CD6
      F2FF4CD5F0FF96DBDDFFBFC2C4FF4B80CBFF4B7DC9FF97A5BAFF9986C4FF4A3F
      E0FF4A3FDEFFB79DAFFFF1CE96FF706933EF4343436C0D0D0D0E000000000000
      0000585858C2E4E4E4FFB3B3B3FFCCCCCCFF898989EA1717171A000000000000
      0000000000000000000000000000000000004343436D0C0C0C0D000000000000
      0000585858C2FFFFFFFFFFFFFFFFFFFFFFFF898989EA1717171A000000000000
      00000000000000000000000000000000000000000000000000007946469FF850
      50FFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFDFDFFFEF7
      F7FFD51010FF683D3D97000000000303030473723FF5FCF5EBFFACE6ECFF00CC
      FFFF00CCFFFF66D7EAFFA5B4CBFF0053D4FF0053D4FF6990C4FF6D61D7FF0000
      FFFF0000FFFF9C85BBFFF1CE96FF726930F3434343680B0B0B0C000000000000
      00004848488B4E4E4EAB4E4E4EAB4E4E4EAB494949AC0C0C0C0D1C1C1C204848
      48984848489848484898484848983A3A3A54444444690A0A0A0B000000000000
      00004848488B4E4E4EAB4E4E4EAB4E4E4EAB494949AC0C0C0C0D1C1C1C204848
      48984848489848484898484848983A3A3A5400000000000000007946469FF851
      51FFFFFFFFFFFFFFFFFFBDBDBDFF989898FFFFFFFFFFFFFFFFFFFFFFFFFFFEFA
      FAFFD51010FF683D3D970000000003030304737341F5FDF8F1FFADE8F0FF00CC
      FFFF00CCFFFF66D9EDFFA5B6CFFF0053D4FF0053D4FF6A91C7FF6E62DAFF0000
      FFFF0000FFFF9C86BCFFF1CE96FF726930F34343436F0C0C0C0D000000000000
      000000000000000000000000000000000000000000000000000038383851CFCF
      CFFFFFFFFFFFFFFFFFFFFFFFFFFF4E4E4EAA434343700C0C0C0D000000000000
      000000000000000000000000000000000000000000000000000038383851CFCF
      CFFFFFFFFFFFFFFFFFFFFFFFFFFF4E4E4EAA00000000000000007946469FF851
      51FFFFFFFFFFC2C2C2FF0A0A0AFF000000FF939393FFFFFFFFFFFFFFFFFFFEFA
      FAFFD51010FF683D3D970000000003030304737342F5FEFBF7FFEDF5F0FFC9EC
      EDFFC8EAE7FFDDE9DDFFE8E0D2FFC5C9CDFFC5C6C6FFDAD1BFFFDAC4BCFFC2AD
      BEFFC1AAB8FFE1C3A5FFF1CE97FF726930F3434343670A0A0A0B000000000000
      000000000000030303043E3E3EA23D3D3D503A3A3AA34C4C4CB94141416BCFCF
      CFFF3A3A3AFF0E0E0EFFD6D6D6FF4E4E4EAA434343670909090A000000000000
      000000000000030303043E3E3EA23C3C3C4F3A3A3AA34C4C4CB94242426CCFCF
      CFFFFFFFFFFFFFFFFFFFFFFFFFFF4E4E4EAA00000000000000007946469FF851
      51FFFFFFFFFF101010FF070707FF0F0F0FFF000000FF959595FFFFFFFFFFFEFA
      FAFFD51010FF683D3D970000000003030304737342F5FFFEFDFFEFEDE9FFCECE
      CDFFCECECDFFE1DCD5FFFCA293FFFF0807FFFF0807FFFB6759FF76AE59FF0983
      06FF098305FFA2B96BFFF2D19DFF726930F3454545750D0D0D0E000000000000
      0000000000000A0A0A0B4A4A4ACE000000001A1A1A1E1E1E1E2338383851CFCF
      CFFF444444FF1C1C1CFFD9D9D9FF4E4E4EAA454545750D0D0D0E000000000000
      0000000000000B0B0B0C4B4B4BCD000000001A1A1A1E1E1E1E2338383851CFCF
      CFFFFFFFFFFFFFFFFFFFFFFFFFFF4E4E4EAA00000000000000007946469FF851
      51FFFFFFFFFF333333FFBCBCBCFFCDCDCDFF101010FF000000FF959595FFFEFA
      FAFFD51010FF683D3D970000000003030304737342F5FFFFFFFFEFEEECFFCCCC
      CCFFCCCCCCFFE0DCD6FFFCA094FFFF0000FFFF0000FFFC6356FF6FAD57FF0080
      00FF008000FF9EB96CFFF3D4A3FF736A31F3585858870C0C0C0D000000000000
      000000000000000000004040405700000000000000000000000038383851C1C1
      C1FFEEEEEEFFEEEEEEFFEEEEEEFF4D4D4DAB585858870C0C0C0D000000000000
      000000000000000000004040405700000000000000000000000038383851C1C1
      C1FFEEEEEEFFEEEEEEFFEEEEEEFF4D4D4DAB00000000000000007946469FF851
      51FFFFFFFFFFE5E5E5FFFFFFFFFFFFFFFFFFCDCDCDFF101010FF292929FFFEFA
      FAFFD51010FF683D3D970000000003030304737342F5FFFFFFFFF0F0F0FFCFCF
      CFFFCFCFCEFFE2DFDAFFFCA79DFFFF0C0BFFFF0C0BFFFC6C60FF78B260FF0C85
      09FF0C8509FFA4BC75FFF4D7A9FF736B33F3424242710E0E0E0F000000000000
      00004848489E4F4F4FAE424242E14F4F4FAC484848A3000000001B1B1B1F4646
      468046464680464646804646468033333346424242720D0D0D0E000000000000
      00004848489E4F4F4FAF424242E14F4F4FAC484848A3000000001B1B1B1F4646
      46804646468046464680464646803333334600000000000000007946469FF851
      51FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFCFFF3C3C3CFFFEFA
      FAFFD51010FF683D3D970000000003030304737342F5FFFFFFFFFFFFFFFFFFFF
      FFFFFEFCF9FFFDF9F1FFFCF5EAFFFBF2E3FFFAEEDBFFF9EBD4FFF8E7CDFFF7E4
      C5FFF6E1BEFFF5DDB7FFF4DAAFFF736C35F3434343640A0A0A0B000000000000
      0000636363D5EBEBEBFFD1D1D1FFEAEAEAFF666666DA00000000000000000000
      000000000000000000000000000000000000434343650909090A000000000000
      0000636363D6FFFFFFFFFFFFFFFFFFFFFFFF666666DA00000000000000000000
      00000000000000000000000000000000000000000000000000007946469FF850
      50FFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFE4E2E2FFFEF7
      F7FFD51010FF683D3D9700000000060606076D6138FB8D8158FF8D8158FF8D81
      58FF8D8158FF8D8057FF8C7D55FF8C7C54FF8C7C51FF8C7B50FF8B7A4EFF8B79
      4CFF8B784BFF8A7849FF8A7748FF6C5D35F83B3B3BAB262626CF454545834949
      49755D5D5DF7828282FF000000FF787878FF666666DA00000000000000000000
      0000000000000000000000000000000000003B3B3BAC262626CF444444804A4A
      4A785E5E5EF7FFFFFFFFFFFFFFFFFFFFFFFF666666DA00000000000000000000
      00000000000000000000000000000000000000000000000000007946469FF850
      50FFF27575FFED8585FFEA7373FFE66262FFE35252FFDF4141FFDC3131FFD820
      20FFD51010FF683D3D970000000003030304AC906DFDE6BB99FFE5BA97FFE5B9
      96FFE4B894FFE4B692FFE3B590FFE3B48EFFE3B38CFFE2B28BFFE2B189FFE1B0
      87FFE1AF85FFE0AE83FFE0AD82FFA28159FB4747476A0909090A000000000000
      0000636363D5989898FF3B3B3BFF939393FF666666DA00000000000000000000
      0000000000000000000000000000000000004747476B08080809000000000000
      0000636363D6FFFFFFFFFFFFFFFFFFFFFFFF666666DA00000000000000000000
      0000000000000000000000000000000000000000000000000000493E3E60744F
      4F9B7357579B715A5A9B7156569B7052529B704F4F9B6E4B4B9B6E47479B6C43
      439B6C40409B433A3A5B0000000000000000676152D0DDC6B7FFF1D6C8FFF0D5
      C6FFF0D4C4FFF0D3C2FFEFD2C0FFEFD1BFFFEED0BDFFEECFBBFFEDCEB9FFEDCD
      B7FFEDCCB5FFECCBB4FFD5B6A0FF625B4CC93E3E3E6D0F0F0F10000000000000
      0000565656DCCCCCCCFFCCCCCCFFCCCCCCFF585858E100000000000000000000
      0000000000000000000000000000000000003E3E3E6E0E0E0E0F000000000000
      0000575757DDCCCCCCFFCCCCCCFFCCCCCCFF585858E100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000034333347424241714242
      4171424241714242417142424171424241714242417142424171424241714242
      4171424241714242417131313041000000000000000000000000000000000000
      0000313131433434344A3434344A3434344A3030304100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000313131433434344A3434344A3434344A3030304100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000101
      01026E6E6E9BA1A1A1DDA0A0A0D66969699455555576858585B99F9F9FDA9E9E
      9ED5343434410000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000303030400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002E2E2E3D5B5B5BDA737373EB7373
      73EB737373EB737373EB737373EB737373EB737373EB737373EB737373EB5D5D
      5DDB2A2A2A350000000000000000000000000000000000000000000000007878
      78A7585858F5000000FF000000FF2F2F2FFF424242FF141414FF000000FF0606
      06FFA5A5A5E42525252C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000A0A0A0B5D5B5B7D1A1A1A1D0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006C6C6CED686868E44B4C4BA74649
      46A6454B45AC454B45AD454B45AD495049B14C514CB34D514DB04D4E4DA96B6B
      6BE8626262E300000000000000000000000000000000000000003F3F3F538282
      82ED000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF101010FFA3A3A3D602020203000000000000000000000000000000000000
      000000000000000000000000000000000000161616197E534EB6C67559E01212
      1214000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000001010102828282F847474796388338A918CD
      18E40FDF0FEE00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF388438AA4848
      48A87C7C7CF60000000000000000000000000000000000000000A2A2A2D60606
      06FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF636363F54C4C4C67000000000000000000000000000000000000
      0000000000000000000000000000000000000202020300000000584B497CBD77
      5EDA605C5B800000000000000000000000002B2A2A364B41416C4B41416C4B41
      416C4B41416C4B41416C4B41416C4B41416C4B41416C4B41416C4B41416C4B41
      416C4B41416C4B41416C4B41416C2726262F4B4B4BAB848484F7394139573196
      31BB3F543F7000FF00FF00FF00FF00FF00FF00FF00FF28DC28FF4C5F4CA18181
      81F9484848A0000000000000000000000000000000003131313D7A7A7AEE0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF6B6B6BF1737373A0000000000000000000000000000000000000
      00000000000000000000000000003F3F3F527192C9E736373848000000003B37
      364DCB603CE40000000000000000000000009E3D39E2D03D3AFFD03D3AFFD03D
      3AFFD65653FFE49290FFE49290FFE49290FFE49290FFE49290FFE49290FFD551
      4FFFD03D3AFFD03D3AFFD03D3AFF90403BD51E1E1E24929292FD494949952AA6
      2AC82B2D2B3714D514E900FF00FF00FF00FF37C637FF40A740E1545454CE8181
      81FB1A1A1A1E00000000000000000000000000000000696969932F2F2FFF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF626262F26868689200000000000000000000000000000000000000000000
      00000000000000000000202020253B81DBF5424D5E8360646C915D5F658B0000
      0000915B4FC16E625E920000000000000000A23F35E7C23B3BFFC23B3BFFC23B
      3BFFD16B6BFFF3DADAFFF3DADAFFF3DADAFFF3DADAFFF3DADAFFF3DADAFFCE61
      61FFC23B3BFFC23B3BFFC23B3BFF944138DA00000000484848A08B8B8BFB3B43
      3B5C3B7A3BA03F5B3F7A00FF00FF00FF00FF55A855FF414C417221218DFF0606
      BEFA3F3F4A7200000001000000000000000000000000959595C9010101FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FFA4A4A4D90000000100000000000000000000000000000000000000000000
      0000000000000000000045494E682A81E0F976808EB72863C4E82264E3FC0000
      00000C0C0C0DBD6148E32525252C000000009D4031E7CA5353FFDB8C8CFFC950
      50FFDB8C8CFFCC5A5AFFD88383FFD16B6BFFD37070FFD77C7CFFCD5F5FFFDB8C
      8CFFC95151FFDB8C8CFFC84F4FFF904235DA0000000016161619878787FA4B4B
      4BA6388438AB202120263A7E3AA43F633F855B705BC6333394F51C1CB2FF0E0E
      BFFF00007DFF3E3E486F000000005656569100000000969696CC000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0808
      08FF8A8A8ABE000000000000000000000000000000000F0F0F11363636440404
      0405696E759B0F0F0F11101010121378DFF9307ED9F4585A5E7E1460DCF90303
      0304000000006E5C63AF29282832000000009B432EE7D57979FFFFFFFFFFD26F
      6FFFFFFFFFFFDA8B8BFFF9EAEAFFE8B4B4FFEBC1C1FFF5DEDEFFDE9797FFFFFF
      FFFFD37171FFFFFFFFFFD26C6CFF8E4532DA00000000000000004646468F9090
      90FE2222222922232229000000001EC01EDB4B4D4B7D1F1FB7FF2121D3FF0B0B
      8BFF19199DFF2727B2F9616161B54D4D4D7C000000008B8B8BBF0A0A0AFF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF9F9F9FD300000000000000000000000000000000564F4D756B616DAE0202
      02031E63DCF46B8BB7D904040405373A3C4E1E72C9EB236EC6E93A3E43590000
      00000000000006060607000000000000000098452AE7C74949FFD06868FFC646
      46FFD06868FFC84C4CFFCE6363FFCA5656FFCB5A5AFFCE6060FFC85050FFD068
      68FFC54848FFD06868FFC54646FF8B462FDA00000000000000000F0F0F107878
      78F54D4D4DBA00000000343934495D5E5DA45F5F5FE7353593FB2424AFFE3434
      C9FF4F4FF4FF464691CE0000000000000000000000004F4F4F6C5D5D5DF60000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF7C7C7CED4F4F4F6C00000000000000000000000015141417BC6550E52B2B
      2C352D2E303B0B5DE9FC7899C9E574A4DBF13F41435800000000000000000000
      000000000000000000000000000000000000954727E7D57979FFFFFFFFFFD26F
      6FFFFFFFFFFFDA8B8BFFF9EAEAFFE8B4B4FFEBC1C1FFF5DEDEFFDE9797FFFFFF
      FFFFD37171FFFFFFFFFFD26C6CFF88482CDA0000000000000000000000004444
      447C939393FF1E1E1E241B1B1B1F2626262F959595FF282863F83C3C94E94D4D
      BFF15959A0DE5D5D61B1616161B4292929320000000004040405A3A3A3D51E1E
      1EFF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FFBEBEBEEF2222222800000000000000000000000038353447AA6E
      62D8000000003F434B640259ECFE405879A41A1A1A1E00000000000000000000
      000000000000000000000000000000000000934924E7C95050FFD78181FFC84D
      4DFFD78181FFCA5656FFD57777FFCF6464FFD06969FFD37373FFCC5A5AFFD781
      81FFC84D4DFFD78181FFC74C4CFF864A28DA0000000000000000000000003636
      364E939393FF28282833272727305B585BC4868686FF59596DD32525262E4F4F
      4F83474747673838384A010101020000000000000000000000001818181B9898
      98CB888888E9646464F7909090E7B0B0B0DEDBDBDBF1969696E65E5E5EFA6969
      69F5A6A6A6E3525252720000000000000000000000000000000000000000C765
      44E0857674B00808080935373A4B060606070000000000000000000000000000
      000000000000000000000000000000000000644B36B3C4590BFFC65A0BFFC65A
      0BFFC65A0BFFC65A0BFFC65A0BFFC65A0BFFC65A0BFFC65A0BFFC65A0BFFC65A
      0BFFC65A0BFFC65A0BFFC2590CFE5E4B3AA70000000000000000000000003737
      374F939393FF3E3E3E66040404052D2D2D3B919191FF656565C7000000005151
      518A141414160000000000000000000000000000000000000000000000000000
      0000232323293A3A3A4A1D1D1D213A3A3A4AA3A3A3F5BFBFBFE75E5E5E7F3838
      3847080808090000000000000000000000000000000000000000000000002C2B
      2A368B5441B1937A77C315151518000000000000000000000000000000000000
      000000000000000000000000000000000000000000013F3D3A594A443E6C4A44
      3E6C4A443E6C4A443E6C4A443E6C4A443E6C4A443E6C4A443E6C4A443E6C4A44
      3E6C4A443E6C4A443E6C3D3A3854000000000000000000000000222222296767
      67F1B1B1B1FF6A6A6AFF3535354A2A2A2A358D8D8DFF6A6A6AEF141414170000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000161616199A9A9AE50D0D0DFFA8A8A8DE0C0C
      0C0D000000000000000000000000000000000000000000000000000000000000
      0000000000018F5541B5956462D10909090A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000474747958282
      82F82F2F2F3F2121212800000001000000002F2F2F3E939393FF414141750000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000068686892707070F1575757FA4747
      475F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000008080809000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000434343749E9E
      9EFF4E4E4EB2474747A0484848A14D4D4DA7515151BB9F9F9FFF3B3B3B590000
      0000000000000000000000000000000000000000000006060607000000000000
      000000000000000000000000000000000000000000005050506EA7A7A7DA5C5C
      5C80000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000050505064747
      479B656565E6737373EB737373EB737373EB6B6B6BE54848488F020202030000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000634646A73734344900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003E3A3A575F47479C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000141515172D30313C3C46
      49603F5A6384397081A8288DABCC2295B8D60000000000000000000000000000
      0001323333453B3E405D000000010000000000000000000000003435354A3739
      3A52000000000000000000000000000000000000000000000000000000000000
      000000000000000000016047479EC48E8EF83A37374F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000433D3D5FC99999F85F48489A000000000000
      00000000000000000000000000000000000000000000020202031C1C1C203635
      31444D493D67655B3F8A424039573E626E920CB2E5F200C3FFFF00C3FFFF00C3
      FFFF00C3FFFF00C3FFFF00C3FFFF17A4CFE5000000001818181B316F87C008A8
      E0FD08B6EFFF06AFEAFF152025EB2C2A2AE0191818E80F0D0DF20D88C0FF13A6
      E3FF336075BF0606060700000000000000000000000000000000000000000000
      0000000000016149499FE89191FBF28585FF3A37374F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000433D3D5FF58383FFE68A8AFA6049499C0000
      000000000000000000000000000000000000A58528CDE5AE05F9F1B400FFF1B4
      00FFF1B400FFF1B400FF635A3F8735778BB100C3FFFF00C3FFFF00C3FFFF00C3
      FFFF00C3FFFF00C3FFFF00C3FFFF17A4CFE5000000002425252C0ABDF5FF0ABD
      F5FF0ABDF5FF0CA5D8FFD3D4D4FFFBFDFDFFE2E4E4FF484646FF1193C8FF1FC0
      F5FF1EBEF3FF18ACE4FF21212127000000000000000000000000000000000000
      0001624949A0EA8989FBFF2929FFF28282FF3A37374F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000433D3D5FF68181FFFF2929FFE68989FA6049
      499C00000000000000000000000000000000D8A40DF1F1B400FFF1B400FFF1B4
      00FFF1B400FFF1B400FF62593F8635778BB100C3FFFF00C3FFFF00C3FFFF00C3
      FFFF00C3FFFF00C3FFFF00C3FFFF17A4CFE5000000001919191D0ABDF5FF0ABD
      F5FF0ABCF3FF0D2934FFBEBEBEFFFBFDFDFFFBFDFDFFEDEFEFFF1790C1FF19AB
      E4FF1EBEF4FF2692BBE802020203000000000000000000000000000000016349
      49A4EA8D8DFBFB3D3DFFFC3C3CFFF18C8CFF5C4444994942426C4942426C4942
      426C4942426C4942426C4942426C443E3E62463F3F654942426C4942426C4942
      426C4942426C4942426C4942426C604444A3F48B8BFFFC3C3CFFFB3E3EFFE78B
      8BFB6049499C000000000000000000000000D8A40DF1F1B400FFF1B400FFF1B4
      00FFF1B400FFF1B400FF62593F8635788CB200C3FFFF00C3FFFF00C3FFFF00C3
      FFFF00C3FFFF00C3FFFF00C3FFFF17A4CFE50000000001010102383E40550FA6
      D9F7077096FFA7A9A9FFFAFCFCFFFBFDFDFFFBFDFDFFF8FAFAFF29A3D3FF0414
      1BFF0C394CFF2122222800000000000000000000000000000001634949A4E8A1
      A1FCF27575FFF37474FFF37373FFF69696FFF1A4A4FFF1A4A4FFF1A3A3FFF1A3
      A3FFF1A2A2FFF1A2A2FFF1A1A1FF955B5BE29E6464E6F1A1A1FFF1A2A2FFF1A2
      A2FFF1A3A3FFF1A3A3FFF1A4A4FFF2A5A5FFF69494FFF37373FFF37474FFF275
      75FFE5A0A0FB6048489D0000000100000000D8A40DF1F1B400FFF1B400FFF1B4
      00FFF1B400FFF1B400FF62593F8635788DB300C3FFFF00C3FFFF00C3FFFF00C3
      FFFF00C3FFFF00C3FFFF00C3FFFF17A4CFE50000000000000000000000002830
      32D15B5C5CFFFBFDFDFFF7F9F9FFFBFDFDFFFBFDFDFFFBFDFDFFF2F4F4FF0D0C
      0CFF080606FF1C1C1C21000000000000000000000001634949A3E49F9FFCEC7A
      7AFFEC7B7BFFEC7B7BFFEC7B7BFFEC7C7CFFEC7C7CFFEC7D7DFFEC7D7DFFEC80
      80FFEC8080FFED8181FFED8181FF976666DFA17070E3ED8181FFED8181FFEC80
      80FFEC8080FFEC7D7DFFEC7D7DFFEC7C7CFFEC7C7CFFEC7B7BFFEC7B7BFFEC7B
      7BFFEC7A7AFFE39F9FFC634747A400000001D8A40DF1F1B400FFF1B400FFF1B4
      00FFF1B400FFF1B400FF62593F8635788DB300C3FFFF00C3FFFF00C3FFFF00C3
      FFFF00C3FFFF00C3FFFF00C3FFFF17A4CFE50000000000000000000000003F3F
      3F7E545353FCFBFDFDFFF7F9F9FFFBFDFDFFFBFDFDFFFBFDFDFFF7F9F9FF0604
      04FF020000FF1A1A1A1E0000000000000000654848A6E39292FCE66161FFE661
      61FFE66262FFE66262FFE76363FFE76363FFE76464FFE76464FFE76565FFE765
      65FFE76565FFE76666FFE76666FF976666DFA16E6EE3E76666FFE76666FFE765
      65FFE76565FFE76464FFE76464FFE76464FFE76363FFE76363FFE66262FFE662
      62FFE66161FFE66161FFE18E8EFC634A4AA278683BA1806D38AA7F6C38A97163
      3C9971633C9971633C993E3D37513C4649603F5C66883F5C66883F5C66883F5C
      66883F5C66883F5C66883F5C66883F545B7A0000000000000000000000000D0D
      0D0E3E3C3CDAF3F5F5FFFAFCFCFFFBFDFDFFFBFDFDFFFBFDFDFFC9C9C9FF100E
      0EFF1A1919E3000000000000000000000000654848A6E08383FCE14848FFE149
      49FFE14949FFE14949FFE14A4AFFE14A4AFFE14B4BFFE24B4BFFE24C4CFFE24C
      4CFFE24D4DFFE24D4DFFE24E4EFF976565DFA06D6DE3E24E4EFFE24D4DFFE24D
      4DFFE24C4CFFE24C4CFFE24B4BFFE14B4BFFE14A4AFFE14A4AFFE14949FFE149
      49FFE14949FFE14848FFDF8080FC634A4AA2455780A9455682AA455682AA4556
      82AA455682AA455682AA3C3E445A3F514D75387263AA396F61A63C665C993C66
      5C993C665C993C665C993C665C993F5D55890000000000000000000000000000
      0000393838B2797979FFFAFCFCFFF7F9F9FFF2F4F4FFDCDEDEFF464444FF1311
      11FC3F3F3F7600000000000000000000000000000001634949A3DC7676FCDC30
      30FFDC3030FFDC3131FFDC3131FFDC3232FFDC3232FFDC3333FFDC3333FFDC33
      33FFDD3434FFDD3434FFDD3535FF966565DFA06C6CE3DD3535FFDD3434FFDD34
      34FFDC3333FFDC3333FFDC3333FFDC3232FFDC3232FFDC3131FFDC3131FFDC30
      30FFDC3030FFDD7A7AFC644949A5000000013163DEF12A66F8FF2A66F8FF2A66
      F8FF2A66F8FF2A66F8FF464E6386367665AF00C391FF00C391FF00C391FF00C3
      91FF00C391FF00C391FF00C391FF17A480E50000000000000000000000000000
      000016161619313131EDF9FBFBFFEFF1F1FFFBFDFDFFE0E2E2FF020000FF1D1A
      1AE1040404050000000000000000000000000000000000000001634949A4DA71
      71FCD61818FFD71818FFD71818FFE15252FFE27373FFE27474FFE27474FFE274
      74FFE27474FFE27575FFE27575FF955B5BE29D6262E6E27575FFE27575FFE274
      74FFE27474FFE27474FFE27474FFE37474FFE04E4EFFD71818FFD71818FFD617
      17FFD87474FB6048489D00000001000000003163DEF12A66F8FF2A66F8FF2A66
      F8FF2A66F8FF2A66F8FF464E6386367665AF00C391FF00C391FF00C391FF00C3
      91FF00C391FF00C391FF00C391FF17A480E50000000000000000000000000000
      0000000000003A3A3A5C89959DFF3E84A1FFA0B2BBFF808080FF121010FD2626
      262F000000000000000000000000000000000000000000000000000000016349
      49A4D76D6DFBD10000FFD10000FFDD6F6FFF5C4444994942426C4942426C4942
      426C4942426C4942426C4942426C443E3E62463F3F654942426C4942426C4942
      426C4942426C4942426C4942426C604444A3DF6B6BFFD10000FFD10000FFD56D
      6DFB6049499C0000000000000000000000003164E0F22A66F8FF2A66F8FF2A66
      F8FF2A66F8FF2A66F8FF464E6386367665AF00C391FF00C391FF00C391FF00C3
      91FF00C391FF00C391FF00C391FF17A480E50000000000000000000000000000
      000000000000222222290882B2FF27CCEBFF0EA3C7FF17313EFF2E2D2DCC0000
      0000000000000000000000000000000000000000000000000000000000000000
      0001624949A0D77575FBD10000FFDD6D6DFF3A37374F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000433D3D5FDF6A6AFFD10000FFD57878FA6049
      499C000000000000000000000000000000003164E0F22A66F8FF2A66F8FF2A66
      F8FF2A66F8FF2A66F8FF464E6386367665AF00C391FF00C391FF00C391FF00C3
      91FF00C391FF00C391FF00C391FF17A480E50000000000000000000000000000
      0000000000002626262F647881FF147386FF487A83FF2E2F2FFF3F3F3F7B0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000001624949A0D78282FBDE7171FF3A37374F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000433D3D5FDF6D6DFFD57B7BFA6049499C0000
      000000000000000000000000000000000000415A9BC23363D8EE2A66F8FF2A66
      F8FF2A66F8FF2A66F8FF464E6386367665AF00C391FF00C391FF00C391FF00C3
      91FF00C391FF00C391FF00C391FF17A480E50000000000000000000000000000
      0000000000002A2A2A35828282FF151313FF9FA0A0FF1C1B1BFF3C3C3C640000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000016149499FC69191F93A37374F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000433D3D5FC79797F85F48489A000000000000
      0000000000000000000000000000000000000000000000000000101010122A2A
      2C353B3D4358444A5A7A37383D4E3F5E568A14A883E900C391FF00C391FF00C3
      91FF00C391FF00C391FF00C391FF17A480E50000000000000000000000000000
      0000000000000F0F0F10040202FD020000FF0E0C0CFF0E0C0CFC2323232A0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000634646A73734344900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003E3A3A575F47479C00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000070707082324242B383F
      3D553F534E773C665C992D856EC4229577D60000000000000000000000000000
      000000000000000000003F3F3F70110F0FEF1F1D1DE63C3C3C61000000000000
      000000000000000000000000000000000000000000000000000048483E6F9B82
      37ED857247CB5F5947933A39364D070707080000000000000000000000000000
      000000000000000000000000000000000000000000003E3E3D5A77746FD87F7B
      74E07F7971E07E7870E07E776DE07E776AE07E7769E07E7567E07E7465E07E72
      63E07D7261E073695BD73B3A3A53000000000000000040404060787570D97F7B
      74E07F7971E07E786FE07E776DE07E776AE07E7769E07E7567E07E7465E07E72
      63E07D7261E0746A5BD73D3C3C58000000000000000000000000000000000000
      00001F1F1F24D5CBCBEAC9B4B4E4D5CACAEA2727272EC2B6B6DEBCA9A9DBA399
      99C700000000000000000000000000000000000000000000000054543E8CC496
      4AFFECC79DFFE6B88AFFE0AC79FFC49A5BFA9B844CE2786C4BBB4F4C44732121
      21270000000000000000000000000000000000000000686664B3FCF3E6FFFBF1
      E1FFFAEEDBFFF9ECD6FFF9E9D0FFF8E7CBFFF7E4C5FFF7E1C0FFF6DFBAFFF5DC
      B4FFF4DAAFFFF4D7A9FF635D55AE00000000000000006B6A68BAFCF3E6FFFBF1
      E1FFFAEEDBFFF9ECD6FFF9E9D0FFF8E7CBFFF7E4C5FFF7E1C0FFF6DFBAFFF5DC
      B5FFF4DAAFFFF4D7AAFF656056B3000000000000000000000000000000000000
      00002727272FCE9E9EEADF3F3FFFC99D9DE735353543C89494E7D40B0BFFBDA5
      A5DD00000000000000000000000000000000000000000000000054543E8CC496
      4BFFF7E4C6FFF7E2C0FFF6DFBAFFF4D9B1FFEFCCA1FFEAC093FFE7B98CFFD6AA
      76FEA68D58E64A4A46680000000000000000000000006D6C6ABAFCF5EAFFFBF3
      E5FFFBF0DFFFFAEEDAFFF9EBD4FFF9E8CFFFF8E6C9FFF7E3C4FFF6E1BEFFF6DE
      B9FFF5DCB3FFF4D9ADFF676258B5000000000000000071716EC1FCF5EAFFFBF3
      E5FFFBF0DFFFFAEEDAFFF9EBD4FFF9E8CFFFF8E6C9FFF7E3C4FFF6E1BEFFF6DE
      B9FFF5DCB3FFF4D9AEFF6B655ABB000000000000000000000000000000000000
      00002727272FCE9F9FEAE04545FFCA9E9EE735353543C89797E7D51111FFBDA5
      A5DD00000000000000000000000000000000000000000000000054543E8CC497
      4BFFF8E8CEFFF8E5C8FFF7E3C2FFF6E0BCFFA39D8DFF737C79FFD5C5A0FFF3D5
      A4FFEDC798FF5F5F578A0000000000000000000000006D6C6BBAFDF7EEFFFCF5
      E9FFFBF2E3FFD6A79BFFB96D63FFB86B60FFB86A5EFFB8695BFFE8C7AAFFF6E0
      BDFFF5DDB7FFF5DBB1FF676259B5000000000000000071716FC1F9EFE7FFBA70
      6BFFB96F68FFB96E66FFB96D63FFC78A7AFFF8E8CDFFF8E5C8FFF7E3C2FFF6E0
      BDFFF5DDB7FFF5DBB2FF6B655BBB000000000000000000000000000000000000
      00002727272FCFA1A1EAE24B4BFFCAA0A0E735353543C89999E7D61717FFBDA6
      A6DD00000000000000000000000000000000000000000000000054543E8CC497
      4BFFFAECD7FFF9E9D1FFF8E7CBFFF7E4C5FF677D99FF117CA1FF0292B3FF4D90
      92FFE8C99EFF5F5F578A0000000000000000000000006D6C6CBAFDF9F2FFFDF7
      EDFFFCF4E7FFFBF1E2FFFAEFDCFFFAECD7FFF9EAD1FFF8E7CCFFF7E4C6FFF7E2
      C1FFF6DFBBFFF5DDB6FF67625BB50000000000000000717171C1FDF9F2FFFDF6
      EDFFFCF4E7FFFBF1E2FFFAEFDCFFFAECD7FFF9EAD1FFF8E7CCFFF7E4C6FFF7E2
      C1FFF6DFBBFFF5DDB6FF6B665CBB000000000000000000000000000000000000
      00002727272FCFA3A3EAE35151FFCBA2A2E735353543C99A9AE7D81D1DFFBDA7
      A7DD00000000000000000000000000000000000000000000000054543E8CC497
      4BFFFBF0DFFFFAEDD9FFF9EBD3FF97B5E1FF0265FBFF3D8AA3FF0491ACFF009B
      C0FF0B8FACFF516C6AC00000000000000000000000006D6D6CBAFEFBF6FFB76B
      68FFB2605CFFB25F5AFFB15E58FFB15D56FFB15C53FFB15B51FFB05A4FFFB059
      4DFFC17A67FFF6DFBAFF67625BB50000000000000000717171C1FAF3EEFFB364
      61FFB2605CFFB25F5AFFB15E58FFB15D56FFB15C53FFB15B51FFB05A4FFFB059
      4DFFC5836DFFF6DFBAFF6B665CBB000000000000000000000000000000000000
      00002727272FD0A5A5EAE45757FFCBA4A4E735353543C99C9CE7D92323FFBEA8
      A8DD00000000000000000000000000000000000000000000000054543E8CC497
      4CFFFCF4E8FFF9F0E2FF70A5EFFF0065FFFF2F80F6FF478CEFFFD7D0B5FF3791
      9AFF009ABAFF009FC4FF366D7ADF35353548000000006D6D6DBAFEFDFAFFFEFA
      F5FFFDF8EFFFFCF5EAFFFBF3E4FFFBF0DFFFFAEDD9FFF9EBD4FFF8E8CEFFF8E6
      C9FFF7E3C3FFF6E0BEFF68635BB50000000000000000717171C1FEFDFAFFFEFA
      F5FFFDF8EFFFFCF5EAFFFBF3E4FFFBF0DFFFFAEDD9FFF9EBD4FFF9E8CEFFF8E6
      C9FFF7E3C3FFF6E1BEFF6C665EBB000000000000000000000000000000000F0F
      0F116866668BD9A9A9F0E55D5DFFCBA4A4E735353543CA9D9DE7DA2929FFBEA9
      A9DD00000000000000000000000000000000000000000000000054543E8CC497
      4CFFFDF8F0FFDAE2EDFF4F93F7FFC0D0E7FFEEE7DBFF1470FBFFE6DFD1FFF8E5
      C7FF9EB0A0FF17899DF6899498FF555576DB000000006D6D6DBAFFFFFEFFFEFC
      F9FFFDFAF3FFDAB0AAFFB2605BFFB15F59FFB15E56FFB6665DFFF9EAD2FFF8E8
      CDFFF8E5C7FFF7E2C2FF68645DB50000000000000000717171C1FBF7F6FFB466
      65FFB2625FFFB2615DFFB2605BFFDEB8ADFFFBEFDDFFFAEDD8FFF9EAD2FFF8E8
      CDFFF8E5C7FFF7E2C2FF6C675FBB0000000000000000000000002C2C2C35C7BC
      BCE1E9A5A5FAEA7474FFE76464FFCBA6A6E735353543CA9F9FE7DC3030FFBEAA
      AADD00000000000000000000000000000000000000000000000054543E8CC497
      4CFFFEFCF9FFFDF9F3FFFDF7EDFFFCF4E7FFFBF1E1FF76A7EEFF95B7E6FFF9E9
      D0FFF4DCBEFF605F578B4F4F54952D2D2D3A000000006D6D6DBAFFFFFFFFFFFE
      FDFFFEFBF7FFFDF9F2FFFDF6ECFFFCF4E7FFFBF1E1FFFAEFDCFFFAECD6FFF9E9
      D1FFF8E7CBFFF7E4C6FF68645EB50000000000000000717171C1FFFFFFFFFFFE
      FDFFFEFBF7FFFDF9F2FFFDF6ECFFFCF4E7FFFBF1E1FFFAEFDCFFFAECD6FFF9E9
      D1FFF8E7CBFFF7E4C6FF6C6760BB00000000000000000F0F0F10CAB9B9E2F286
      86FFEE8686FFEB7777FFE86A6AFFCBA8A8E735353543CBA1A1E7DD3636FFBEAA
      AADD00000000000000000000000000000000000000000000000054543E8CC497
      4CFFFFFFFFFFFFFDFCFFFEFBF6FFFDF8F0FFFCF5EAFFE5E6E6FF458DF6FFFAED
      D8FFF5DFC4FF5F5F578A0000000000000000000000006D6D6DBAF5EBEBFFC488
      88FFC48786FFC38683FFC38480FFC2837BFFC28278FFC28075FFC17D72FFC17B
      70FFCE9685FFF8E6CAFF68655EB50000000000000000717171C1FBF7F7FFC489
      89FFC48786FFC38683FFC38480FFC2837BFFC28278FFC28075FFC17D72FFC17B
      70FFC58473FFF8E6CAFF6C6861BB00000000000000006B67678FF48080FCF46D
      6DFFEF8C8CFFEC7D7DFFE97070FFCCAAAAE735353543CBA3A3E7DE3C3CFFBFAB
      ABDD00000000000000000000000000000000000000000000000054543E8CC497
      4CFFFFFFFFFFFFFFFFFFFFFFFEFFFEFCF8FFFDF9F2FFFDF6ECFF639FF5FFDFE2
      E4FFF6E2CBFF5F5F578A0000000000000000000000006D6D6DBAFEFCFCFFEEDD
      DDFFEEDDDDFFEDDBD9FFEDD9D4FFECD7CFFFEBD4CAFFEBD2C6FFEBD0C0FFEACD
      BCFFEED5BFFFF8E8CEFF68655FB50000000000000000717171C1FEFEFEFFEFDE
      DEFFEEDDDDFFEDDBD9FFEDD9D3FFECD7CFFFEBD4CAFFEBD2C6FFEBD0C0FFEACD
      BCFFECD0BBFFF8E8CEFF6C6861BB0000000000000000AB9C9CCCFB4747FFF660
      60FFF17D7DFFED8585FFEB7676FFCCACACE735353543CBA4A4E7E04242FFBFAB
      ABDD00000000000000000000000000000000000000000000000054543E8CC497
      4CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFBFFFEFAF5FFD7E2F1FFA3C3
      F1FFF7E6D2FF5F5F578A0000000000000000000000006D6D6DBAFFFFFFFFFFFF
      FFFFFFFFFFFFCA9594FFB36362FFB2625FFFB2615DFFB2605BFFEBD2C4FFFAEF
      DDFFFAEDD7FFF9EAD2FF686560B50000000000000000717171C1FBF7F7FFB467
      67FFB36464FFB36464FFB36362FFBA716EFFFDF7EEFFFCF4E8FFFBF2E3FFFAEF
      DDFFFAEDD8FFF9EAD2FF6C6963BB0000000000000000B1A1A1D1FD3636FFF852
      52FFF36F6FFFEF8B8BFFEC7C7CFFCDADADE735353543CBA6A6E7E14848FFBFAC
      ACDD00000000000000000000000000000000000000000000000054543E8CC496
      4BFFFAF1EAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFFFEFCF8FFFDF9
      F2FFF8E9D9FF5F5F578A0000000000000000000000006D6D6DBAFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFEFDFFFEFBF7FFFDF9F2FFFCF6ECFFFCF4E7FFFBF1
      E1FFFAEEDBFFFAECD6FF696560B50000000000000000717171C1FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFEFDFFFEFBF7FFFDF9F2FFFCF6ECFFFCF4E7FFFBF1
      E1FFFAEEDCFFFAECD6FF6C6963BB0000000000000000817A7AA8FF4B4BFFFA44
      44FFF66161FFF18080FFED8484FFCFAFAFE8716F6F96CEA8A8E9E24E4EFFC2B0
      B0DF1E1E1E221E1E1E221313131500000000000000000000000042423C609D85
      30F4C2954AFFDDA772FFE7BC98FFEFD3BBFFF7E8DCFFFEFCFAFFFFFFFFFFFEFD
      FAFFF9ECDFFF5F5F578A0000000000000000000000006D6D6DBAFDFAFAFFB568
      68FFB36464FFB36464FFB36464FFB36362FFB36260FFB2615EFFB2605CFFB25F
      5AFFC98E84FFFAEEDAFF696661B50000000000000000717171C1FBF7F7FFB467
      67FFB36464FFB36464FFB36464FFB36362FFB36260FFB2615EFFB2605CFFB25F
      5AFFC88B82FFFAEEDAFF6C6965BB000000000000000021212126D4AAAAE8FC38
      38FFF85454FFF37171FFEF8A8AFFEF9191FFF0A0A0FFEA7979FFE35454FFD893
      93F2D0A4A4EBCFA1A1EBB3A8A8D4000000000000000000000000000000000000
      0000101010124A49406F6F6740B895813EE5B59648FDD4A76CFFE8BF9DFFEFD3
      BBFFF2D9C6FF5F5F578A000000000000000000000000686868B4FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFAFFFEFAF4FFFDF7EFFFFCF5
      E9FFFBF2E4FFFBF0DEFF63625EAE00000000000000006C6C6CBAFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFAFFFEFAF4FFFDF7EFFFFCF5
      E9FFFBF2E4FFFBF0DEFF676561B400000000000000000000000054525270D5A4
      A4E9FA5656FFF56363FFF18282FFED8383FFEA7474FFE86767FFE55A5AFFE24D
      4DFFDF4040FFDC3333FFB3A4A4D5000000000000000000000000000000000000
      000000000000000000000000000000000000040404053D3D3854656242A9897B
      42DAA48E50EB5B5B51870000000000000000000000003E3E3E5B787878D88181
      81E0818181E0818181E0818181E0818181E0818181E081807EE0807E7CE0807E
      7AE0807D78E077736FD73B3B3B530000000000000000414141617A7A7AD98181
      81E0818181E0818181E0818181E0818181E0818181E081807EE0807E7CE0807E
      7AE0807D78E077746FD83D3D3C58000000000000000000000000000000003434
      3441A09595C5CDBABAE5CDBDBDE6CDC0C0E6CDBEBEE6CCBDBDE6CBBABAE5CAB8
      B8E4CAB6B6E4C9B4B4E49E9696C3000000000000000000000000000000000000
      0000000000000000000011111113050505060000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002C2C2C39373737523C3C3C632E2E
      2E3E1C1C1C203D3D3D6739393956313131430B0B0B0C000000005F5F5FDB6C6C
      6CFC6C6C6CCB545454FB676767FB696969E30E0E0E0F42423E6A46463F774646
      3F7746463F7746463F77434A45B8315B83FC344448D447473F7A46463F774646
      3F7746463F7746463F7741413D67080808090F0F0F1042423E6A46463F774646
      3F7746463F7746463F7746463F7746463F7746463F7746463F7746463F774646
      3F7746463F7746463F7741413D67080808090000000006060607737338AB9191
      1DDD87881CDE83861CEF70701BDF64641DDD58581DDD4F4F29CB1D1D1D220000
      000000000000000000000000000000000000151544E7000067FF09095AF53737
      49AD2E2E2E3D0D0D48F100006BFF030362FB39393B5700000000545454DB4E4E
      4EDD34343443313131FF4E4E4EE3000000005E5E45D5B7B7A2FFB7B7A2FFB7B7
      A2FFB7B7A2FFB7B7A2FF6F7B7BFF367BAEFF038AADFF2B8694FFB7B7A2FFB7B7
      A2FFB7B7A2FFB7B7A2FFB7B7A2FF5D5D48CF4C4628D6654320FF654320FF6543
      20FF654320FF654320FF654320FF654320FF654320FF654320FF654320FF6543
      20FF654320FF6B4A29FF745536FF4C452DCE00000000000000000F0F0F108484
      29CBA4A400FF939300FF838300FF707000FF5C5C14E82626262F000000000000
      00000000000000000000000000000000000035354BB10000ADFF0D0D62F10000
      000000000000323244B90404BAFF10107AF10404040500000000545454DB4E4E
      4EDD34343443313131FF4E4E4EE300000000757544F5FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFCFD7DAFF0694AEFF00A0C6FF009ABDFF79ADB8FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F0594B16F5814934FF814934FF824B
      36FFA3796AFFB18F82FFA07565FF814934FF8D5A47FFA3796AFF8B5744FF8149
      34FF814934FFBB9E93FFC6ADA4FF54491AF000000000000000000D0D0D0E919A
      31EAA7A700FF969600FF868600FF6F7416F92727273000000000000000000000
      0000000000000000000000000000000000001717171A05058BFC04049CFF1515
      44EA151546EA090966FA0C0CC8FF3C3C53A200000000000000005A5A5ABB5252
      52F75353538A313131FF4E4E4EE300000000767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF87B5BBFF00A3C4FF00B0DAFF009BBDFFB8CD
      D2FFFFFFFFFFFFFFFFFFFFFFFFFF707047F1584C14F7814934FF86503CFFE6DB
      D7FFFDFCFCFFE7DCD8FFF4EFEDFF814934FFAD8A7AFFFFFFFFFFA78170FF8149
      34FF814934FFE2D6D1FF9F7363FF554919F1000000000D0D0D0E9AA430EABBBB
      00FFAAAA00FF9A9A00FF848A13FF6593A7FF5C768DC60D0D0D0E000000000000
      000000000000000000000000000000000000000000002A2A59CE0D0DCAFF0F0F
      CDFF1010D1FF1212D4FF1414ADFB1818181C0000000000000000030303045E5E
      5ED04C4C4CFE313131FF525252DB00000000767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3E9AA9FF00A8CAFF00B8E3FF089A
      BAFFE6EBECFFFFFFFFFFFFFFFFFF707047F1584C14F7814934FFB29184FFFFFF
      FFFFB29184FF814934FF86513DFF814934FF956654FFBC9F94FF926250FF8149
      34FF8D5A47FFF3EDEBFF824A36FF554919F100000000929F3DE1CFCF00FFBEBE
      00FFA3A911FF839B58FF598FB5FF5B94C7FF6199CBFF5C768DC64F4F4FAC3F3F
      3F5F0000000000000000000000000000000000000000373739521111C3FF1818
      9EF53B3B59AC1818D6FF333366C6000000000000000000000000000000000505
      05065F5F5FD2565656F22424242C00000000767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F8F8FF1895AAFF009FC2FF00AE
      D7FF2193ACFFFDFDFDFFFFFFFFFF707047F1584C14F7814934FFBCA095FFFFFF
      FFFF9D7161FF814934FF814934FF814934FF814934FF814934FF814934FF8149
      34FFB39386FFCEB8B0FF814934FF554919F1000000003536374945799BF86A99
      B5FF4883B8FF508BBEFF6EA0CBFF558FC3FF5B94C7FF576778ED9B9B9BFE7777
      77FC404040620000000000000000000000000000000000000000222275E61717
      95FA26263FD61D1DC5FD32323446000000000000000000000000000000000000
      000006060607151515170000000000000000767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD3E0E2FF0799B3FF009D
      C1FF009FC4FF509FB0FFFFFFFFFF707047F1584C14F7814934FF9D7161FFFFFE
      FEFFDBCCC6FFA27869FFB7988CFF814934FFAA8675FFF7F3F2FFA47B6CFF8149
      34FFDACBC5FFA6806FFF814934FF554919F10000000000000000353637493B71
      A6F76496C2FF4883B8FF508BBEFF6EA0CBFF546574EEA4A4A4FEDADADAFFAAAA
      AAFF5C5C5CE3000000000000000000000000000000000000000040404B8E2525
      F7FF2727FDFF2C2C87E10000000057505099644B4BDB574E4EB4000000000606
      0607735C5CD16B4343E86B4343E8604E4ECE767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF91BDC3FF00A2
      BFFF00A9D2FF0098BAFFB2BFC1FF707047F1584C14F7814934FF814934FFAE8C
      7DFFE7DDD9FFF6F2F0FFE2D5D0FF814934FFA47B6CFFE6DBD7FF9F7464FF8752
      3EFFF5F1EFFF854F3AFF814934FF554919F10000000000000000000000003536
      37493B71A6F76496C2FF4883B8FF586979EEACACACFEDCDCDCFFBABABAFFAEAE
      AEFF626262E30000000000000000000000000000000000000000020202033030
      A1F54A4AFFFF43434C7C000000006D4E4EC2800000FF6B4242E8020202030000
      00005950508E800000FF7F3E3EF2514B4B7D767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF46A1
      AEFF0AA7C5FF839BA0FF8B8B96FF646444F4584C14F7814934FF814934FF8149
      34FF814934FF814934FF814934FF814934FF814934FF814934FF814934FFAB88
      78FFD5C3BCFF814934FF814934FF554919F10000000000000000000000000000
      0000353637493B71A6F75F6F7EF0B9B9B9FEE0E0E0FFC7C7C7FFBEBEBEFFA5A5
      A5FF5050509A0000000000000000000000000000000000000000000000004242
      59B85151A8F408080809000000001717171A7C4141E1842020FE635252D55D52
      52B9755B5BD6800000FF6E4242E700000000737342F7EEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFE8E9
      E9FF898E8FFF9191A8FF6363AFFF5E5E44F5564B13F7764430FF764430FF7644
      30FF764430FF764430FF764430FF764430FF764430FF764430FF764430FFA17D
      6FFF976E5FFF764430FF764430FF534819F10000000000000000000000000000
      000000000000595E61C4D3D3D3FEE4E4E4FFDEDEDEFFD9D9D9FFD6D6D6FFB2B2
      B2FF515151A50000000000000000000000000000000000000000000000001F1F
      1F25464653A50000000000000000000000001818181B7D3F3FE3800000FF8000
      00FF820303FF800000FF6E4242E7000000007E6540FC92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF9274
      4CFF836A4AFF585279FF745F47FF715A3AFA7D633FFC8D6E45FF8D6E45FF8D6E
      45FF8D6E45FF8D6E45FF8D6E45FF8D6E45FF8D6E45FF8D6E45FF8D6E45FF8D6E
      45FF8D6E45FF8D6E45FF8D6E45FF765E3CFA0000000000000000000000000000
      0000000000004747476DDDDDDDFEEAEAEAFFECECECFFD1D1D1FFE7E7E7FFEAEA
      EAFFC5C5C5FF535353A500000000000000000000000000000000000000000000
      000002020203000000000000000000000000000000001B1B1B1F7E3D3DE68A1F
      1FFF635C5CB0830707FF6E4242E700000000AA9073FCE9C2A5FFE8C1A3FFE8C0
      A2FFE7BFA0FFE7BE9EFFE6BD9CFFE6BB9AFFE5BA99FFE5B997FFE5B895FFE4B7
      93FFE4B691FFE3B58FFFE3B48EFF997B56F9AB9073FCE9C2A5FFE8C1A3FFE8C0
      A2FFE7BFA0FFE7BE9EFFE6BD9CFFE6BB9AFFE5BA99FFE5B997FFE5B895FFE4B7
      93FFE4B691FFE3B58FFFE3B48EFF9B7E59F80000000000000000000000000000
      000000000000000000004747476C949494E6939393E85454549B626262B1EBEB
      EBFFEEEEEEFFC8C8C8FF535353A5000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001D1D1D22803D
      3DE78B2929FF800000FF6E4242E70000000056544CA1A19581F6A79988F9A698
      86F9A69785F9A69684F9A69682F9A69582F9A59481F9A5947EF9A4937DF9A493
      7BF9A4927AF9A49179F99C8B71F5514F489456544CA1A19581F6A79988F9A698
      86F9A69785F9A69684F9A69682F9A69582F9A59481F9A5947EF9A4937DF9A493
      7BF9A4927AF9A49179F99B8A71F5504F48930000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006262
      62B1EEEEEEFFF2F2F2FF767676E90909090A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001F1F
      1F247F3939EA800000FF6E4242E7000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000626262B17F7F7FE91919191D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000202020267F3838EA6E4242E7000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000202020300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002424242B6C5D5DB8000000000000000000000000000000000000
      0000000000005757579E54545495000000010B0B0B0C0808080929292934374C
      51C13A5055BB3A5155BB3A4749BF0B0B0B0C0000000008080809665743DC9171
      4BFA906E47FA8F6C44FA8E6C42FA8D693FFA8C683DFA8B663AFA8A6538FA8963
      36FA896336FA514536D200000001000000000000000008080809485458820202
      020300000000000000001E1E1E234A5659B01B1B1B1F00000000000000000000
      0000000000000000000000000000000000004B5F68922FA0C7F42425252C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000028282831414141DF50505085585858C1575757B5373737502FB1
      D2FF3BDAFFFF3CDDFFFF447B87E813131315000000005F5549C7E6B87DFFE5B4
      76FFE3B170FFE2AD6AFFE0AA66FFDFA862FFDDA55EFFDBA35AFFDAA056FFD89E
      52FFD79C4FFFC9924AFF4B443BBB000000000000000050626C9525BFFCFF4E70
      7DC20F0F0F1145595DA143B1C8FB5B9DA9FF2424242C00000000000000000000
      000000000000000000000000000000000000464B4E6C3CC0FBFE30A6CEFB4E4F
      469146463F7746463F7746463F7746463F7746463F7746463F7746463F774646
      3F7746463F7746463F7741413D67080808090000000000000000000000000000
      000000000000424242603C3C3CE93F3F3F59434343613E3E3E583636364E2869
      7CF0308194ED308394ED325158E61212121400000000866F58EBE8BC85FFE6B8
      7DFFE5B477FFE0AF72FFCF9E77FFE0AB66FFDFA862FFDDA65EFFDCA35AFFDAA0
      56FFD89E52FFD79C4FFF5D4B33E500000000000000000404040559869EC92BC0
      FCFF449AB5FE3DDAFDFF4DE0FFFF599CA9FF393A3A5327272730272727302727
      2730272727301414141700000000000000005E5E45D5A0B9BCFF44C3FCFF32AF
      DAFFA8AE9FFFB7B7A2FFA6A591FFA09F8AFFB5B5A0FFB7B7A2FFB7B7A2FFB7B7
      A2FFB7B7A2FFB7B7A2FFB7B7A2FF5E5E49D00000000000000000000000000000
      000000000000444444663D3D3D55000000000000000000000000040404051D1D
      1D221D1D1D221D1D1D221C1C1C200000000000000000867159EBE9BF8CFFE8BC
      86FFDBAF85FF4536D5FF221AE9FFD2A175FFE1AB66FFDFA862FFDDA65EFFDCA3
      5AFFDAA156FFD89E52FF5D4B33E50000000000000000000000002121212859BD
      EAFF32C3FEFF44A7C5FF4DB4BFFF4D6244FF47653FFF3A8073FF23DBF2FF25E9
      FBFF28EFFCFF435656860000000004040405737343F5FFFFFFFFD2E9F9FF4BC4
      FCFF46A7C7FF888842FFA7A76CFFB2B280FF878740FFAAA99CFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F00000000000000000000000000000
      0000000000004747476D3939394C000000000000000000000000151515184044
      457D4044457D4044457D41434378040404050000000087745EEBEBC393FFCAA6
      9BFF251EEBFF0000FFFF0000FFFF3528DEFFDCA86FFFE1AB67FFDFA863FFDDA6
      5FFFDCA35BFFDAA157FF5D4C34E50000000000000000000000001F1F1F2533BD
      E1FF61C4F2FF4AA2BAFF95A674FFBED6CFFFC4E9EAFFB7D1C1FF6E7A3AFF2BCF
      DBFF28EFFCFF5C6D36D55B5B39A606060607757542F7FFFFFFFFFFFFFFFFCADB
      E4FF96A064FFF5F2EAFFFCFAF8FFFAF9F8FFF8F8F6FFAAA971FFB4B3A9FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F10000000000000000000000000000
      0000000000004646466A3C3C3CE83B3B3B50505050894D4D4D7E373737502CA5
      C4FF37CBEEFF38CEEEFF40747FEA1313131500000000897761EBC6A7A9FF100D
      F7FF0000FFFF0403FCFF0B08F8FF0000FFFF4E3CCEFFE1AD6CFFE1AB67FFDFA8
      63FFDDA65FFFDCA35BFF5D4C35E50000000000000000000000001F1F1F2532BD
      E1FF3FAFBFFF97AD82FFC4F0F3FFD8EBECFFD7F3F7FFE3F6F8FFC7E8E5FF5267
      3DFF28EFFCFF6C7D35D6656538A906060607757542F7FFFFFFFFFFFFFFFF8382
      4DFFEEEADCFFFDFAF4FFFBF9F6FFFBFAF8FFFAF9F8FFF6F5F2FF737235FFFEFE
      FEFFFFFFFFFFFFFFFFFFFFFFFFFF707047F10000000000000000000000000000
      00000000000022222228434343DC5151518B575757A454545498373737502FB1
      D2FF3BDAFFFF3CDDFFFF447B87E81313131500000000897964EBEBC9A1FF4D40
      DDFF0000FFFF8E73BAFFBB979FFF120EF4FF0000FFFF6C54BCFFE2AE6CFFE1AB
      67FFDFA963FFDEA65FFF5E4D37E50000000000000000000000001F1F1F2532BD
      E1FF537448FFB2E3E1FFC3EEF0FFD3EAEAFFCFF2F6FFD9F5F8FFDCF7F9FF8A98
      54FF2DD6E0FF5F6D65DE5F5F59B806060607757542F7FFFFFFFFFFFFFFFFA09F
      57FFFCF6EAFFFDF8EFFFFCF9F4FFFBF9F6FFFBFAF8FFFCFBF9FF979650FFF1F1
      F1FFFFFFFFFFFFFFFFFFFFFFFFFF707047F10000000000000000000000000000
      000000000000555555945555559E0202020300000000000000001C1C1C203F46
      48983F4648983F4648984146469206060607000000008A7B68EBEFCFA7FFE7C5
      A3FF947BC0FFEBC494FFEAC08DFFC5A099FF1914F0FF0000FFFF8B6AABFFE2AE
      6CFFE1AB67FFDFA963FF5F4D39E50000000000000000000000001F1F1F2532BD
      E1FF6B864DFFBCECEBFFBEECECFFCAE4E3FFC8F1F5FFD1F4F7FFC8F4F7FF8DA6
      6EFF32C4CAFF768788D66A6A6AA906060607757542F7FFFFFFFFFFFFFFFF9B99
      51FFFDF6E9FFFDF7ECFFFCF9EFFFFBF9F3FFFBFAF6FFFDFBF6FF939248FFF9F9
      F9FFFFFFFFFFFFFFFFFFFFFFFFFF707047F1080808093E41416A3F41426C3F41
      426C3F41426C3F41426C3F41426C3F41426C3F41426C3F41426C3F41426C2727
      273100000000000000000000000000000000000000008B7D6CEBF1D4AFFFF0CF
      A7FFEECBA1FFEDC89AFFEBC494FFEAC18EFFD1AA93FF271FE9FF0100FDFF9F7A
      9FFFE3AE6CFFE1AB67FF5F4E3AE50000000000000000000000001F1F1F2532BD
      E1FF4C7858FFB4DDD5FFC1ECEAFFBFDFDCFFB0EBEEFFB8EFF3FFA4F0F4FF8994
      4CFF2BE5F1FF768788D66A6A6AA906060607757542F7FFFFFFFFFFFFFFFF9795
      72FFDDD9C0FFFCF6E9FFFDF8EDFFFDF9F0FFFBF8F0FFE3E1D2FF757347FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F11C1C1C202C8197F136C8EEFF35C7
      EEFF35C7EEFF35C6EEFF35C6EEFF35C6EEFF35C6EEFF35C6EEFF35C6EEFF4049
      4C9700000000000000000000000000000000000000008C8070EBF3D9B8FFF1D4
      B0FFF0D0A8FFEECCA1FFEDC89BFFEBC494FFEAC18EFFD8B090FF3227E3FF0A07
      F8FFB9908DFFE3AF6DFF5F503CE50000000000000000000000001F1F1F2532BD
      E1FF3CC7E1FF90A467FFC2EAE6FFC2DFDAFFABEAEBFFA6EDEFFF96C4AEFF4078
      60FF28EFFCFF4B6162A63333334306060607757542F7FFFFFFFFFFFFFFFFFCFB
      FAFF98964FFFDCD8C0FFFCF6EAFFFCF7EDFFE1DECAFF919141FFE9E9E6FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F11C1C1C20308BA1F03AD8FFFF3AD8
      FFFF3AD7FFFF3AD6FFFF3AD6FFFF39D5FFFF39D5FFFF39D4FFFF39D4FFFF4049
      4C9600000000000000000000000000000000000000008D8374EBF5DEC0FFF3D9
      B8FFF2D4B0FFF0D0A8FFEECCA1FFEDC89BFFEBC595FFEAC18EFFDEB58DFF3D31
      DDFF1511EBFFC99D81FF60513CE50000000000000000000000001F1F1F2532BD
      E1FF3AD8FFFF41ACB3FF8B9C57FFA6B798FF9BC4AFFF91A671FF4A7654FF25E5
      F7FF28EFFCFF435656860000000006060607737341F7EEEEEEFFEEEEEEFFEEEE
      EEFFEBEBEAFF8D8B69FF919046FF919147FF7A7751FFE4E4E2FFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFEEEEEEFF6D6D45F21C1C1C20308DA1F03BDAFFFF3BDA
      FFFF3BD9FFFF3BD9FFFF3AD8FFFF3AD7FFFF3AD7FFFF3AD6FFFF3AD6FFFF4049
      4C9600000000000000000000000000000000000000008E8579EBF7E3C9FFF5DE
      C1FFC8B49DFF9F9C99FF9F9C98FF9F9B97FF9F9A95FF9E9994FF9E9993FFBE9D
      78FF4739D2FF3025CBFF5F5040E60000000000000000000000001F1F1F2532BD
      E1FF3AD8FFFF3BDBFFFF4DDBF6FF4F8F88FF31948FFF26C2D1FF21E3FBFF25E9
      FBFF28EFFCFF43565686000000000C0C0C0D7D6540FC92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF92744CFF9274
      4CFF92744CFF92744CFF92744CFF705A3AFA1C1C1C20308DA1F03CDCFFFF3CDC
      FFFF3BDBFFFF3BDBFFFF3BDAFFFF3BDAFFFF3BD9FFFF3AD8FFFF3AD8FFFF4149
      4C9600000000000000000000000000000000000000008F877DEBF9E7D1FFF7E3
      C9FFEAD5B9FFADA69EFFBEBEBEFFA8A5A1FFA9A6A1FFBDBDBDFFAA9F92FFE3BC
      8BFFE7BD8AFFE3B785FF625341E50000000000000000000000001F1F1F2532BD
      E1FF3AD8FFFF3BDBFFFF4DE0FFFF51B9CEFF1AD7FAFF1EDDFAFF21E3FBFF25E9
      FBFF28EFFCFF435656860000000003030304A99072FCE9C2A5FFE8C1A3FFE8C0
      A2FFE7BFA0FFE7BE9EFFE6BD9CFFE6BB9AFFE5BA99FFE5B997FFE5B895FFE4B7
      93FFE4B691FFE3B58FFFE3B48EFF997B56F91C1C1C204791A1F059E2FFFF4FE1
      FFFF44DFFFFF3CDDFFFF3CDCFFFF3CDCFFFF3BDBFFFF3BDBFFFF3BDAFFFF4149
      4C96000000000000000000000000000000000000000064625ED2FAEBD9FFF9E8
      D2FFF7E3CAFFCCBBA7FFD5D5D5FFC1AB91FFBDA68BFFD5D5D5FFC7AC89FFECC5
      96FFEAC28FFFD9B180FF4B463ECA0000000000000000000000001F1F1F2532BD
      E1FF3AD8FFFF3BDBFFFF44CBE8FF36A6BEFF1AD7FAFF1EDDFAFF21E3FBFF25E9
      FBFF28EFFCFF43565686000000000000000056544CA1A19581F6A79988F9A698
      86F9A69785F9A69684F9A69682F9A69582F9A59481F9A5947EF9A4937DF9A493
      7BF9A4927AF9A49179F99B8A71F5514F48941C1C1C205B919BEF82EAFFFF76E8
      FFFF69E2FBFF448996EC3E818FE8397F8FE8347E8FE8317E8FE8317D8FE84047
      4994000000000000000000000000000000000000000007070708736E69E3EADD
      CBFFE8D9C4FFC5B7A7FFEBEBEBFFDBDBDBFFDDDDDDFFEAEAEAFFBEA688FFDDBC
      92FFD7B388FF52483EE10C0C0C0D0000000000000000000000001F1F1F2532BD
      E1FF2FBBDFFF1EA6CAFF14B2D9FF17D1FAFF1AD7FAFF1EDDFAFF21E3FBFF25E9
      FBFF28EFFCFF4356568600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000001454A4CA14F7279E15072
      78E0435255C81D1D1D220F0F0F110F0F0F110F0F0F110F0F0F110F0F0F110505
      05060000000000000000000000000000000000000000000000000C0C0C0D4242
      4170424241704444447B858584DABEBEBEF6BEBEBEF67F7F7FD6444343794241
      40704140406E0F0F0F1100000000000000000000000000000000151515182D67
      7ADB3A6471B13D6672A83D6772A83E6972A83F6A72A83F6B72A8406C72A8416E
      72A8426F72A83C42425F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002626252E29292833292928332929
      2833292928332929283329292833292928332929283329292833292928332929
      28332929283329292833292928332424242C0000000000000000000000000000
      00000000000000000000000000002525252D3A3A3A5500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000533F3F85C7B177F3C19364F3BD7351F3B9553EF3B4382CF35F3C3BA90000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000736B54CAA77856EBA04D3AEB5A3D3CA4000000000000
      000000000000000000000000000000000000878737E8B6B688F1B6B687F1B6B5
      87F1B6B586F1B6B586F1B6B486F1B6B485F1B6B485F1B6B484F1B6B483F1B6B3
      82F1B6B382F1B6B382F1B6B382F1858434E70000000000000000000000000000
      000000000000202020264D4D4DAC878787FA9E9E9EFF525252CE272727310000
      000000000000000000003F4547780D0D0D0E0000000000000000000000000000
      0000543F3F83FDDF94FFF6B678FFF08E5EFFE96444FFE33B2BFF653C3CA90000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008D8262CFF4A66EFFE85E41FF633E3DA5000000000000
      000000000000000000000000000000000000919157E1FFFFFEFFFFFEFDFFFFFE
      FCFFFFFDFAFFF6F4EBFFF3F1E5FFFEFCF7FFFEFCF6FFFEFBF5FFFEFBF3FFFEFA
      F2FFFEFAF1FFFEFAF1FFFEFAF1FF8D8C50DF0000000000000000000000001818
      181C4C4C4C9F828282F7D6D6D6FFF2F2F2FFF2F2F2FFE0E0E0FF7F7F7FFA4B4B
      4BA00909090A3F4D50821A6173E1335761B80000000000000000000000000000
      0000543F3F83FDDF94FFF6B678FFF08E5EFFE96444FFE33B2BFF653C3CA90000
      00000000000000000000000000000000000000000000464646814B4B4BB94B4B
      4BB927272730000000008D8262CFF4A66EFFE85E41FF633E3DA5000000000000
      000000000000000000000000000000000000919157E1FFFFFFFFFFFFFEFFFFFE
      FCFFE6E6D2FF888729FF939242FFFEFCF8FFFEFCF7FFFEFCF6FFFEFBF4FFFEFB
      F3FFFEFAF2FFFEFAF1FFFEFAF1FF8D8C50DF00000000151515184949499B7575
      75F7CECECEFFF2F2F2FFF2F2F2FFC9C9C9FF6A6A6AFFC9C9C9FFF2F2F2FFC2C2
      C2FF366670F3146D84EA0AAAD3F4226476D50000000000000000000000000000
      00004E3F3F7B9F8F67E3A17C5BE99D654BE9984F3CE9913A31E2593E3E9F0000
      000000000000000000000000000000000000161616199E9E9EFDE6E6E6FFE6E6
      E6FF717171F2272727308D8262CFF4A66EFFE85E41FF633E3DA5000000000000
      000000000000000000000000000000000000919157E1FFFFFFFFFFFFFFFFE4E3
      CFFF888826FF808000FF89882EFFB1B075FFB0B075FFBAB986FFFEFCF5FFFEFB
      F4FFFEFBF3FFFEFAF2FFFEFAF1FF8D8C50DF48484896757575F5CCCCCCFFF2F2
      F2FFF2F2F2FFDFDFDFFF787878FF858585FFD6D6D6FFF2F2F2FFF2F2F2FFF2F2
      F2FF3A8494FF03BBE7FF08A5C9FD404647750000000000000000000000000000
      0000000000005A5A5A91F6F6F6FFE8E8E8FFDADADAFF4E4E4E7F000000000000
      00000000000000000000000000000000000027272730828282EA878787E8D6D6
      D6FFF1F1F1FF7D7D7DF4A49568F5F4A66EFFE85E41FF772D2CEB505050C75050
      50C74F4F4FC74E4E4EC74E4E4EC734343449919157E1FFFFFFFFE3E3CFFF8D8D
      25FF909000FF909000FF909000FF909000FF909000FF8F8E25FFFEFCF6FFFEFB
      F5FFFEFBF4FFFEFBF3FFFEFAF2FF8D8C50DF5A5A5AB3DFDFDFFEF2F2F2FFE9E9
      E9FF878787FF747474FFCFCFCFFF767676FF878787FFF0F0F0FFF2F2F2FFC9C9
      A5FF95AB41FF2FA18CFFA1BA24FF51513ACD0000000000000000000000000000
      0000000000005A5A5A91F6F6F6FFE8E8E8FFDADADAFF4E4E4E7F000000000000
      000000000000000000000000000000000000000000000000000000000000B0B0
      B0FDF2F2F2FFEEEEEEFFC7B687FFF4A66EFFE85E41FFB05756FFDADADAFFD6D6
      D6FFD2D2D2FFCECECEFFCCCCCCFF46464687919157E1FEFEFDFF8F8F2BFFA7A7
      00FFA7A700FFA7A700FFA7A700FFA7A700FFA7A700FF999825FFFEFCF7FFFEFC
      F6FFFEFBF5FFFEFBF4FFFEFBF3FF8D8C50DF00000000363636498D8D8DE2B4B4
      B4FFCACACAFF7C7C7CFF7C7C7CFFE3E3E3FFF2F2F2FFD6D6C5FFBEBE4FFF8686
      19FFCFCF06FFB4B422FB50504686000000000000000000000000000000000000
      0000000000005A5A5A91F6F6F6FFE8E8E8FFDADADAFF4E4E4E7F000000000000
      00000000000000000000000000000000000034343448515151C7505050C7B6B6
      B6FEE6E6E6FF9D9D9DF4897D5FF8AF805CFDA6503EFD763E3DF1727272DE7171
      71DE717171DE717171DE717171DE3C3C3C59919157E1FFFFFFFFDDDDBFFFA5A5
      21FFBEBE00FFBEBE00FFBEBE00FFBEBE00FFBEBE00FFA3A225FFFFFDF8FFFEFC
      F7FFFEFCF6FFFEFBF5FFFEFBF4FF8D8C50DF00000000000000000909090A5656
      5692BBBBBBFACFCFCFFFC2C2C2FFE1E1D9FFBEBE61FFDEDE0AFF464672FF1515
      7CFF141455F32525252E00000000000000000000000000000000000000000000
      0000000000005A5A5A91F6F6F6FFE8E8E8FFDADADAFF4E4E4E7F000000000000
      0000000000000000000000000000000000002626262FB7B7B7FCE2E2E2FFDEDE
      DEFF888888EF2020202643434365F1F1F1FF949494E900000000000000000000
      000000000000000000000000000000000000919157E1FFFFFFFFFFFFFFFFDDDD
      C0FF9E9E21FFB6B600FF9B9B2EFFB5B46EFFB5B46DFFB9B777FFFFFDF9FFFFFD
      F8FFFEFCF7FFFEFCF6FFFEFBF5FF8D8C50DF0000000000000000000000001616
      16195B5B5B9BC0C0C0FAB5B59EFFD9D916FFFFFF00FFC1C11CFD393963E10000
      FCFF1C1C65DE262656D014141417000000000000000000000000000000000000
      0000000000005A5A5A91F6F6F6FFE8E8E8FFDADADAFF4E4E4E7F000000000000
      00000000000000000000000000000000000000000000323232425D5D5DBA6262
      62CB202020260000000043434365F1F1F1FF949494E900000000000000000000
      000000000000000000000000000000000000919157E1FFFFFFFFFFFFFFFFFFFF
      FFFFDDDDC6FF8E8E23FF979740FFFFFFFEFFFFFEFCFFFFFEFBFFFFFDFAFFFFFD
      F9FFFEFCF8FFFEFCF7FFFEFCF6FF8D8C50DF000000001C1C1C2159595991B1B1
      B1E9F5F5F5FFF9F9F9FF919181EBB1B11BFD6B6B3FCA14141416010101023939
      66A81010A7ED1C1C62DE38383A55000000000000000000000000000000000000
      000000000000565656AC9B9B9BEA939393EA8B8B8BEA4F4F4F9D000000000000
      00000000000000000000030303043D3D3D650000000000000000000000000000
      0000000000000000000043434365F1F1F1FF949494E900000000000000000000
      000000000000000000000000000000000000919157E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFF0F0E7FFEBEBDEFFFFFFFFFFFFFEFDFFFFFEFCFFFFFEFBFFFFFD
      FAFFFFFDF9FFFEFCF8FFFEFCF7FF8D8D51DF00000000929292DDF0F0F0FF8E8E
      8EFF7D7D7DFFF9F9F9FF6C6C6BB4000000000000000000000000000000000707
      07083F3F4A822A2A2B3613131315000000004444448C474747A43E3E3E620000
      00002525252E4D4D4DE66A6A6AFF6A6A6AFF696969FF525252EC3D3D3D600000
      0000000000001D1D1D22434343B9444444A40000000000000000000000000000
      0000000000000000000043434365F1F1F1FF949494E900000000000000000000
      00000000000000000000000000000000000089891FF7928E2DF6928E2DF6928E
      2CF6918E2CF6918D2CF6918D2CF6918D2BF6918D2BF6918D2BF6918D2BF6918D
      2BF6918C2BF6918C2BF6918C2BF689871FF600000000929292E4747474FFD9D9
      D9FFD7D7C4FFC6C659FF8B8B34E5000000000000000000000000000000000000
      0000000000002B2C2C38205764D83F484B764C4C4CC3818181FF4E4E4ED54545
      4584474747C27D7D7DFF7D7D7DFF7C7C7CFF7C7C7CFF7C7C7CFF494949C94848
      4896494949AF545454E55C5C5CED2C2C2C390000000000000000000000000000
      0000000000000000000049494995616161E4505050E12D2D2D3B000000000000
      0000060606073737374F00000000000000008A7F39E0E6B578FFE3B06FFFE2AC
      69FFE0AB66FFDFA964FFDDA760FFBF9A3FFFBE983DFFBD973BFFBC9639FFBC95
      37FFBB9435FFBA9334FFB49033FD6C6835C0000000009F9F9FE1D7D7C0FFC6C6
      50FFECEC04FFFFFF00FF878734E2000000000000000000000000000000000000
      0000313233431B7386E01A778FE1215A68D75B5B5BC3BDBDBDFFBCBCBCFFBCBC
      BCFFBCBCBCFFBBBBBBFFBBBBBBFFBBBBBBFFBABABAFFBABABAFFBABABAFFB9B9
      B9FFB9B9B9FFB4B4B4FF4A4A4A99000000000000000000000000343434485656
      56D64646467B32323244666666F2757575FF757575FF585858E12C2C2C393E3E
      3E5F525252C33F3F3F6100000000000000008C8543E0F0CDA2FFEEC899FFEBC3
      91FFE9BE88FFE7B97DFFCBA656FF666537B83434314434343144343431443434
      314434343144343431443333314307070708000000009C9C40F5ECEC04FFFDFD
      00FFBABA21F8646444B611111113000000000000000000000000000000000000
      0000315861BC1886A3E300C3F6FE3E535993606060C3D3D3D3FFD3D3D3FFD2D2
      D2FFD2D2D2FFD2D2D2FFD1D1D1FFD1D1D1FFD1D1D1FFD0D0D0FFD0D0D0FFD0D0
      D0FFCACACAFF535353B9070707080000000000000000000000003838384FB6B6
      B6FFABABABFFA4A4A4FEB7B7B7FFB8B8B8FFB8B8B8FFB8B8B8FFB0B0B0FFBABA
      BAFF656565BF01010102000000000000000063633EAD99914DE8B8AC6CF5B7A9
      68F5B6A762F59C9247EC5655418C1E1E1E230000000000000000000000000000
      00000000000000000000000000000000000000000000BDBD14FCAEAE1FF66161
      42B7070707080000000000000000000000000000000000000000000000000000
      00003F4E528A04B4E1FA3F50558A00000000666666C3E9E9E9FF9F9F9FEC5858
      58A69A9A9AE9E8E8E8FFE8E8E8FFE7E7E7FFE7E7E7FFE7E7E7FFE6E6E6FFA6A6
      A6F2515151A606060607000000000000000000000000000000003838384FD2D2
      D2FFBCBCBCF9BBBBBBF8D3D3D3FFD4D4D4FFD4D4D4FFD5D5D5FFD5D5D5FF7777
      77CC1C1C1C21000000000000000000000000000000001F1F1E24292928332929
      2833292928332222212800000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004E4E4392030303040000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000333535480000000000000000535353C0878787E3575757C50000
      00003E3E3E5D6B6B6BC76E6E6EC66E6E6EC66E6E6EC66E6E6EC6606060BF2A2A
      2A36000000000000000000000000000000000000000000000000353535489A9A
      9AE4575757983939394F878787D28E8E8ED68E8E8ED68E8E8ED6515151840808
      0809000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000005050506282829320000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000333331432D2D2C39000000000000
      0000000000000000000000000000000000000000000059565296B2A48AF1B2A2
      88F1B2A185F1B2A083F1B1A081F1B19E7EF1B09E7CF1B09D79F1B09C76F1B09B
      75F1B09A71F1B09A71F157534E91000000002626252E29292833292928332929
      2833292928332929283329292833292928332929283329292833292928332929
      28332929283329292833292928332424242C00000000080808090F0F0F110F0F
      0F110F0F0F110F0F0F110F0F0F110F0F0F1114141417325387BA155AC5E73A3D
      42590F0F0F110F0F0F1107070708000000000000000000000000000000000000
      000000000000000000003D3C3B58566558E96694A1FF6C847DFA0F0F0F100000
      000000000000000000000000000000000000000000005C5A5692F8E4BFFFF0DB
      B6FFA19276FFF7DFB5FF98896CFFF6DCAEFFF6DBABFFF6D9A8FFF5D8A4FFF5D6
      A1FF94815DFFF4D39AFF59564F8E02020203878737E8B6B688F1B6B687F1B6B5
      87F1B6B586F1B6B586F1B6B486F1B6B485F1B6B485F1B6B484F1B6B483F1B6B3
      82F1B6B382F1B6B382F1B6B382F1858434E7525246A487875CF98D8D5AFF8D8D
      5AFF8D8D5AFF8D8D5AFF8D8D5AFF8A8C5DFF2969BCFF0B6DFBFF0B6EFFFF1368
      DFFF607B85FF8D8D5AFF85855DF8515148990000000000000000000000000101
      010247474475576C62F46B9DB0FF74AFCDFF74AFCDFF8DB1BEFF677269DA0000
      000000000000000000000000000000000000000000005C5B5792AA9C85FF7C72
      60FF464035FF847661FF40392EFF84745DFF83745BFF83735AFF837258FF8371
      56FF3F3729FFAA946CFF59564F8E02020203919157E1FFFFFEFFFFFEFDFFFFFE
      FCFFFFFDFAFFFFFDF9FFFFFDF8FFFEFCF7FFFEFCF6FFDCDABEFFD1CFABFFFEFA
      F2FFFEFAF1FFFEFAF1FFFEFAF1FF8D8C4FDF737345F2FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF8F8F8FFDBE3EDFF3782E8FF2784FFFF2684FFFF2684
      FFFF3182F1FFB5D0F5FFFFFFFFFF71714AEC00000000000000001A1A1A1E5E77
      6EF570A7C0FF74AFCDFF74AFCDFF74AFCDFF74AFCDFF8AAFBBFF99C1D5FF4F51
      4E8000000000000000000000000000000000000000005C5B5792F9E7C8FFEDDB
      BBFF877A67FFF8E3BEFF7A6E5BFFF7E0B7FFF7DEB3FFF6DDB0FFF6DBADFFF6DA
      A9FF786A51FFF5D7A2FF5956508E02020203919157E1FFFFFFFFFFFFFEFFFFFE
      FCFFFFFEFBFFFFFDFAFFFFFDF9FFFEFCF8FFFEFCF7FFA1A157FF7D7D0AFFB5B4
      78FFFEFAF2FFFEFAF1FFFEFAF1FF8D8C4FDF767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF757575FF828282FF717B8DFF2A71D5FF3790FFFF3791
      FFFF3791FFFF3C84E8FFFEFEFFFF737349F100000000000000005A6142D173AF
      CBFF74AFCDFF74AFCDFF74AFCDFF74AFCDFF74AFCDFF89ACB8FF9CC5DBFF95B9
      C9FE3839384D000000000000000000000000000000005D5B5792F9E9CCFFEDDC
      BFFF887B69FFF8E5C2FFC3B396FFF8E2BBFFF7E0B8FFF7DFB4FFF7DDB1FFF6DC
      ADFFC0AA85FFF5D9A7FF5956508E02020203919157E1FFFFFFFFFFFFFFFFFFFE
      FDFFFFFEFCFFFFFEFBFFAAA967FF9D9C4EFF9C9B4DFF89882BFF808000FF8080
      0BFFB2B075FFFEFAF2FFFEFAF1FF8D8C4FDF767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF888888FFFFFFFFFFFFFFFFFFE1EBFAFF418AEBFF3791
      FFFF3F8BEFFFCCDEF8FFFFFFFFFF737349F1000000001818181B608379FF74AF
      CDFF74AFCDFF74AFCDFF74AFCDFF74AFCDFF74AFCDFF87AAB5FF9CC5DBFF9CC5
      DBFF8CA8AFFA222221280000000000000000000000005D5B5892FAEBD0FFEEDE
      C3FF887C6CFFF9E6C6FFF8E5C3FFF8E3BFFFF8E2BCFFF7E1B8FFF7DFB5FFF7DE
      B2FFF6DCAEFFF6DBABFF5957518E02020203919157E1FFFFFFFFFFFFFFFFFFFF
      FEFFFFFEFDFFFFFEFCFF9A9A3AFF969600FF969600FF969600FF969600FF9696
      00FF919109FFB0AF6DFFFEFAF2FF8D8C4FDF767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF888888FFFFFFFFFFFFFFFFFFFFFFFFFFE0EAFAFF468A
      E7FFCFE1F8FFFFFFFFFFFFFFFFFF737349F1000000004A4A40776B9FB3FF74AF
      CDFF74AFCDFF74AFCDFF74AFCDFF74AFCDFF73ADCAFF84A6B0FF9CC5DBFF9CC5
      DBFF9CC5DBFF7C8E8CEE0F0F0F1000000000000000005D5B5892FAEDD4FFEEE0
      C7FF88806EFFF9E8CAFFF9E7C7FFF9E5C3FFF8E4C0FFF8E2BDFFF7E1B9FFF7DF
      B6FFF7DEB2FFF6DDAFFF5957528E02020203919157E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFEFFFFFEFDFFA1A13AFFADAD00FFADAD00FFADAD00FFADAD00FFADAD
      00FFADAD00FF939324FFF4F1E4FF8D8C50DF767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF888888FFFFFFFFFFFFFFFFFFF5F4E9FFB3A849FFE2DF
      BDFFFFFFFFFFFFFFFFFFFFFFFFFF737349F1000000005B6040C774AFCDFF74AF
      CDFF74AFCDFF6DA2B9FF618377FF647037FF6F7212FF80821AFF99BDC8FF9CC5
      DBFF9CC5DBFF9CC5DBFF4647476800000000000000005D5B5992FBEED8FFEEE2
      CBFF888070FFFAEACEFFF9E9CBFFF9E7C8FFF9E6C4FFF8E4C1FFF8E3BDFFF8E1
      BAFFF7E0B7FFF7DEB3FF5957528E02020203919157E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFEFFA3A335FFB7B700FFB7B700FFB9B900FFC4C400FFC3C3
      00FFA2A124FFE5E3C9FFFEFBF4FF8D8C50DF767643F7FFFFFFFFD5F4FEFF73DB
      FBFFFFFFFFFFFFFFFFFF888888FFFFFFFFFFF7F7EDFFB9AC49FFD7AD36FFBFA8
      3DFFE2DEBDFFFFFFFFFFFFFFFFFF737349F1000000005D6D50E6679096FF6277
      4EFF686F23F8757613F1888800FE8D8D00FF909000FF929200FF737635D498BF
      D2FC9CC5DBFF9CC5DBFF4042435900000000000000005D5C5992FBF0DDFFEFE4
      CFFF898173FFFAECD3FFFAEACFFFF9E9CCFFF9E7C8FFF9E6C5FFF8E5C2FFF8E3
      BEFFF8E2BBFFF7E0B7FF5957528E02020203919157E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFDEDEC4FFD1D1ADFFD1D1ACFF9C9B44FFACAC00FF9998
      27FFE6E5CFFFFEFCF6FFFEFBF5FF8D8C50DF767643F7D6F4FEFF25BFF0FF06A7
      DEFF5ED2F5FFF0F0F0FF909090FFF0F0F0FFB8B550FFEAD718FFECD81AFFECD7
      1AFFCABF2EFFE3E2BCFFFFFFFFFF737349F10000000057583EA27F7F03FC8585
      00FF878700FF898900FF8E9517FF87B165FF9CB053FF929200FF949400FF6D71
      49C19CC5DBFF9CC5DBFF494C4E6B00000000000000005D5C5A92FCF2E1FFEFE6
      D3FF898275FFFBEED7FFFAECD3FFFAEBD0FFFAE9CDFFF9E8C9FFF9E6C6FFF8E5
      C2FFF8E3BFFFF8E2BCFF5957538E02020203919157E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFEFDFFA8A765FF8E8E33FFEAE8
      D6FFFEFCF8FFFEFCF7FFFEFCF6FF8D8C50DF64835FF825C7F4FF00B5DEFF00B3
      DEFF02B4E1FF289EC0FF545454FF6A6A6AFF6F6F66FFC0C033FFFDFD00FFFFFF
      00FFFFFF00FFCECE1CFFECECD5FF737349F100000000000000004C4C40798484
      01FD878700FF898900FF7ABF97FF51DAFBFF80D9DDFF68D4D9FF6ACFCDFF93A6
      31FF707857C49CC5DBFF494C4E6B00000000000000005D5C5B92FCF4E5FFF0E7
      D7FF898377FFFBEFDBFFFBEED8FFFAEDD4FFFAEBD1FFFAEACDFFF9E8CAFFF9E7
      C7FFF9E5C3FFF8E4C0FF5A57538E02020203919157E1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFDFFFFFEFCFFFFFEFBFFFFFD
      FAFFFFFDF9FFFEFCF8FFFEFCF7FF8D8D50DF3B9598FD05D3EDFF00DDF0FF00DC
      F0FF00DBEFFF03D3EDFF39AEBFFF928964FF928964FF91895EFFACAA16FFFDFD
      00FFF7F700FFA3A01EFF928962FF6C613CF50000000000000000000000004646
      3E69858503FB908F0DFFB28E4BFF96AD79FF99AC73FFA99A52FF9BB080FF83C3
      96FF929204FA73806CCB3D3F3F5400000000000000005D5C5B92FDF6E9FFF0E9
      DBFF898479FFFCF1DFFFFBF0DCFFFBEED8FFFAEDD5FFFAEBD2FFFAEACEFFF9E8
      CBFFF9E7C7FFF9E6C4FF5A58548E0303030489891FF7928E2DF6928E2DF6928E
      2CF6918E2CF6918D2CF6918D2CF6918D2BF6918D2BF6918D2BF6918D2BF6918D
      2BF6918C2BF6918C2BF6918C2BF689871FF6B1926FFD6AC4CDFF0AECF9FF00FF
      FFFF00FFFFFF00FFFFFF06F1FBFF68C1C8FFE1B188FFE1B086FFDBAC7AFFBCB1
      1CFFBAAA26FFDEAC80FFE0AD82FFA18158FB0000000000000000000000000000
      000046463E6A848404FAA2912BFFB08944FFB4934DFFBB995BFFAD793AFFA49C
      21FF989800FF8E8F0DF33E3E3B5500000000000000005D5C5B92FDF7EDFFF1EA
      DFFF89867CFFFCF3E3FFFCF2E0FFFBF0DDFFFBEFD9FFFBEDD6FFE6D5BBFFB990
      64FFB99064FFB98F66FE504E4B7E020202038A7F39E0E6B578FFE3B06FFFE2AC
      69FFE0AB66FFDFA964FFDDA760FFBF9A3FFFBE983DFFBD973BFFBC9639FFBC95
      37FFBB9435FFBA9334FFB49033FD6C6834C1716856DEEBD1C1FF71D2E2FF0BEE
      FBFF00FFFFFF00FEFFFF24D8F4FFCCCDC4FFEDCDB8FFEDCCB7FFECCBB5FFE8C7
      ABFFE9C8AEFFEBC8AFFFE3C0A8FF675E4BD30000000000000000000000000000
      00000000000042423C60858506F88D8D00FF949309FFA99C32FFA29A20FF9696
      00FF898811EE5D5D3E9B2B2B2A3600000000000000005D5D5C92B9B6B1FF817C
      77FF484542FF8E8A82FFFCF3E4FFFCF2E1FFFBF0DDFFFBEFDAFFDFCDB3FFE3A1
      5EFFE1A05EFF6F6356BA00000000020202038C8543E0F0CDA2FFEEC899FFEBC3
      91FFE9BE88FFE7B980FFCBA656FF676536B93434314434343144343431443434
      31443434314434343144333331430A0A0A0B0303030445444374494946853776
      82C407DCECF816B3CBEB47515197494846854948468549484685494846854948
      468549484685494846854343416E000000000000000000000000000000000000
      000000000000000000003E3E395785850AF5909000FF87870EF2656537B04545
      3D650F0F0F10000000000000000000000000000000005D5D5C92FEFBF6FFF4F0
      E9FF8F8C87FFFDF7ECFFFDF5E8FFFCF4E5FFFCF2E2FFFBF1DEFFDFCEB7FFE09F
      5EFF6E6356B600000000000000000000000163633EAD98934CE9B8AC6CF5B7A9
      68F5B6A762F59C9247EC5655418C1F1F1E240000000000000000000000000000
      0000000000000000000000000000000000010000000000000000000000000000
      00003F5459771D1D1E2200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000353533473D3D38540D0D0D0E000000000000
      000000000000000000000000000000000000000000005A5A5996B6B5B3F1B6B3
      B2F1B6B3B0F1B5B2ADF1B5B2AAF1B5B1A8F1B4B0A6F1B4B0A4F1ACA191F36B62
      56B000000000000000000000000000000000000000001F1F1E24292928332929
      2833292928332222212800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000444444696363
      63CD6A6A6ACD6A6A6ACD6A6A6ACD6A6A6ACD686868C9676767C8676767C86767
      67C8676767C85E5E5EC73737374B000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002525262D4B4B
      56731919191C0000000000000000000000000000000000000000000000000000
      000000000000060606076E5D59A91717171A0000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C4C4C93A8A8
      A8F5D9D9D9FFBFBFBFFFABABABFFB8B8B8FF898989FFC6C6C6FFA9A9A9FF8787
      87FFD9D9D9FFA7A7A7FF46464671000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004848506D5757729E1111
      111300000000000000000000000000000000000000003131323E4343C1DD5A5A
      7FA6020202030000000000000000000000000000000000000000000000000000
      00000000000000000000917269D1A78578E0403D3D5600000000000000000000
      00000000000000000000000000000000000000000000000000004C4C4C93B1B1
      B1FDC9C9C9FF606060FF787878FF979797FFADADADFFDDDDDDFF696969FFECEC
      ECFFECECECFFA7A7A7FF4848487A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005252729F0000D8FF5454
      94C11414141600000000000000000000000038383A4A3C3CCAE25C5C88AF0303
      0304000000000000000000000000000000000000000000000000000000000000
      0000000000000000000061575391F3E0C8FFB49387E730302F3D000000000000
      00000000000000000000000000000000000000000000000000004E4E4E99B4B4
      B4FFC9C9C9FF818181FF606060FF979797FF939393FFD4D4D4FF6C6C6CFFECEC
      ECFFECECECFFA7A7A7FF4C4C4C8300000000000000006C5C5C91806767A80000
      000056515174997676C2000000000000000007070708B37777D6403E3E530000
      0000996A6AC1564D4D74000000000000000000000000766379B42925D3FA0000
      E2FF605BAED8906E6FBC000000004242475D3030DBEC8B6DC2F0353434430000
      00008D5B5BB648424260000000000000000000000000000000003F3B3B5B3A37
      375000000000000000004441405DEAD1BBFFF9EBD4FF8D7169CB000000000000
      0000000000000000000000000000020202030000000000000000515151A0B4B4
      B4FFD7D7D7FF727272FF9E9E9EFFB9B9B9FF777777FFC0C0C0FFAFAFAFFF8888
      88FFD5D5D5FFA7A7A7FF4D4D4D89000000000000000065555587A56B6BCB0000
      00004E4B4B69C97777E8050505060000000000000000B96666DC584F4F770000
      0000955C5CBD6F565695000000000000000000000000584B4B77A06B79D34747
      ABD40000EBFF755DD0F94F4F59782525E3F25A5A8AB0B25A5AD84B4444640000
      00008A5050B4614C4C8500000000000000000000000000000000312F2F3F8057
      57F1644C49B179655FB48C6F67CBECD4BDFFFAECD5FFC6AB9AEE856D65C1846C
      64C0846C63C0806961BC524C4A77080808090000000000000000545454AB8989
      89FFA6A6A6FFA6A6A6FFA3A3A3FE9B9B9BF89B9B9BF89B9B9BF79B9B9BF79A9A
      9AF69A9A9AF6797979FA4D4D4D87000000000F0F0F106E626292C76E6EE43434
      34415A575779E87272FC4544445B0F0F0F112626262DA56A6ACC7A6363A23433
      33418C6464B58E6060B72C2C2C36000000000C0C0C0D61565683C26060E14342
      43586764AFD60101F5FF0707FBFF5C5B8FB62726262E9D5E5EC66D5858943433
      3341825A5AAB835454AC28282830000000000000000000000000000000017051
      51D2C19485FFF7E7CDFFF9EAD2FFF9EBD3FFFAECD5FFFAECD4FFF9E8CCFFF8E4
      C4FFF7E0BBFFF5DCB3FFD1AD8DF5524B4A7800000000000000004C4C4CA44363
      63FE299E9EFF337878FF252727FF3F9393FF2C9292FE2A3A3AFE456565FD2E84
      84F8396969F7181818FF4949497C00000000504A4A6BD16A6AE8F44E4EFFC97A
      7AE7C58080E6E56060FCCD7676EB4C484866987272C0D75858F1DB4D4DF3C861
      61E6D44F4FEDE33030F8A86C6CD000000000443E3E58CA5555E4F34848FFC169
      69E3A179B4F20B0AF8FF100FFEFF5D59B2D68B6366B8D34B4BEFD84141F2C04A
      4AE2CE3D3DEBE12626F7945050C1000000000000000000000000000000007253
      50CBE2C3AEFFF9E9D0FFF9EAD2FFF9EBD3FFFAECD5FFFAECD5FFF9E8CCFFF8E4
      C4FFF7E0BBFFF6DCB3FFF4D7AAFF7C665CBC00000000000000004C4C4C9B1C1D
      1DFF339D9DFF27CCCCFF4B7D7DFF2D4D4DFF28C5C5FF32AFAFFF263030FF3398
      98FF23BEBEFF365C5CFD4A4A4A8C00000000000000001B1B1B1FEB6363FA3C3B
      3B4D00000001BD7777E0544F4F7200000000000000006D595994966060BF0000
      0000584E4E76BC5858DD00000000000000000000000012121214EB5A5AFA6562
      86B10707F5FE8166CEF6665F6C9B3B3BC4E154549BC36854568F8B5353B60000
      00004B434364B44646D80000000000000000000000004640406A5D4A4AA88158
      54E2E6C9B3FFF9E9D0FFF9EAD2FFF9EBD3FFFAECD5FFFAECD5FFF9E8CCFFF8E4
      C4FFF7E0BBFFF6DCB3FFF4D8AAFF81685EC0000000000B0B0B0C4A4A54D54344
      4BEE565959B14B4E4E904A4D4D894A4C4C854949497D4949497D464646724646
      467246464672464646701E1E1E23000000000000000002020203CA6D6DE5504C
      4C6B000000009F6F6FC86558588900000000000000005B51517CB26262D60000
      00004743435DDF4545F40F0F0F11000000000000000007070708926ABDF50202
      F5FF595995BB996869C4584D4D792121212757579AC36158B0DCAB5B5DD40000
      000039373748DD3838F305050506000000004841416E8D7C7CF8999999FFA18E
      8AFFE6C9B3FFF9E9D0FFF9EAD2FFF9EBD3FFFAECD5FFFAEDD5FFF9E8CDFFF8E4
      C4FFF7E0BCFFF6DCB3FFF4D8ABFF81685EC000000000474747701D1D1DFF3030
      E4FF45456AE74E4E539E11111113000000000000000000000000000000000000
      000000000000000000000000000000000000010101028F6868B6D65C5CEEA96D
      6DD08F6969BBC26A6AE4B56868DA7360609B49454561A56262CDDE4545F59157
      57BB9B5959C3F12424FF925252BF3E3C3C51101010127763BBE90000F3FF7B64
      CBF2926C6DBFBF6666E3B26363D96957578F423F3F578F6BACEBA66CB1FC9157
      57BB995858C2F01919FF915151BE333131405F4A4AAE999999FF999999FFA18E
      8AFFE6C8B3FFF9E9D0FFF9EAD2FFF9EBD3FFFAECD5FFFAEDD5FFF9E8CDFFF8E4
      C4FFF7E0BCFFF6DCB3FFF5D8ABFF80685DC000000000040404054B4B4E824F4F
      90E22B2BDDFF181818FF494959E74040405E0000000000000000000000000000
      000000000000000000000000000000000000000000004F4A4A69A46D6DCA9874
      74C0575252768F7070B9A97474D03F3D3D5223232329695E5E8DF04A4AFF6C5F
      5F91584F4F77D64747EF7E6767A71919191C51515F800000F1FF765FC4EF8B66
      68B74642425D806060AB9D6262C83230303E1B1B1B1F544C4C72EE4244FF564C
      4C744640405DD33939ED6651518B13131315624B4BB2999999FF999999FF9D92
      90FFD6B2A0FFF9E9D0FFF9EAD2FFF9EBD3FFFAECD5FFFAEDD5FFF9E9CDFFF8E4
      C5FFF7E0BCFFF6DCB4FFF2D4A8FF746059B10000000000000000000000001C1C
      1C20555566AF202023FF3232B9FF3F3F8AEA4C4C56C122222228000000000000
      0000000000000000000000000000000000000000000000000000775F5F9E8A68
      68B40000000060565683A96F6FD000000000000000000E0E0E0FDA5757F24744
      445E00000000B35858D75B4F4F7C0000000019191A1D46464F6A71595A997F5E
      5EA900000000544B4B72A06161CA000000000000000004040405D74A4AF13A38
      384A00000000AC4747D24E45456A00000000624B4BB2999999FF999999FF9999
      99FFB38E84FFD5B19EFFE7CAB4FFE7CAB5FFE7CBB6FFE7CCB7FFE7C9B0FFE6C5
      AAFFE5C2A4FFD4B193F7987567D72626262E0000000000000000000000000000
      0000000000003B3B3C53565675D01A1AE4F9242442FF3C3C4EF24B4B4D820505
      05060000000000000000000000000000000000000000000000005B52527B9273
      73BB000000004745455EAB7E7ED0000000010000000000000000A16E6EC8514B
      4B6C00000000886161B1635454870000000000000000000000004E4747688666
      66B0000000003A38384A9E6C6CC7000000000000000000000000966060BF423E
      3E56000000007D5555A55549497500000000624B4BB2999999FF999999FF9999
      99FF999999FF9D9391FFA08E8AFFA08E8AFFA08E8AFFA08E8AFFA08E8AFF936D
      69F8433F3F5F2E2D2D3A0909090A0909090A0000000000000000000000000000
      000000000000000000000707070852525A90373753F623238BFF3636A9F14C4C
      5CD8292929320000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005B4848A1989898FF999999FF9999
      99FF999999FF999999FF999999FF999999FF999999FF999999FF999999FF7757
      57E10A0A0A0B0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002020202655556BBD2E2EC4EE3535
      6BF14040405F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001D1D1D226C4B4BCD8B7171F89078
      78FF907878FF907878FF907878FF907878FF907878FF907878FF7A5B5BE54D45
      457C000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004040425E5555
      62B2070707080000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000030303042323232A2928
      28332928283329282833292828332928283329282833292828330F0F0F110000
      0000000000000000000000000000000000000000000000000000000000000000
      0000030303045858589C54545496131313150000000000000000000000000000
      0000000000000000000000000000000000000000000000000000020202030000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000B0B0B0C355355B2206975D9384D52AA05050506000000000000
      0000000000000000000000000000000000000000000000000000000000000404
      04055D5D5DAA4B4B4B774040405B636363C34A4A4A7401010102000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003F4B4B8900E5F7FF00D9FFFF01B9EEFE3F444678000000000000
      0000000000000000000000000000000000000000000000000000060606075F5F
      5FB04B4B4B731212121408080809060606074C4C4C7C5D5D5DBD3838384C0000
      000000000000000000000000000000000000000000001A1A1A1E1F1F1F241F1F
      1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F
      1F241F1F1F24040404050000000000000000000000001A1A1A1E1F1F1F241F1F
      1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F
      1F241F1F1F240404040500000000000000000000000000000000000000000000
      0000000000003C51519A00ECFFFF00D9FFFF00C6FEFF3E494D90000000000000
      00000000000000000000000000000000000000000000060606075F5F5FB24747
      476B2525252D606060C4646464C342424260000000001D1D1D21585858A35E5E
      5EB22323232A00000000000000000000000000000000B97171D7F96A6AFFF48A
      8AFFF09595FFED8888FFEA7878FFE86B6BFFE45E5EFFE25050FFDF4343FFDB35
      35FFDC3838FF2E2E2E39000000000000000000000000B97171D7F96A6AFFF48A
      8AFFF09595FFED8888FFEA7878FFE86B6BFFE45E5EFFE25050FFDF4343FFDB35
      35FFDC3939FF2D2D2D3700000000000000000000000000000000000000000000
      0000000000002525252D206D73D900B6D6FF225B6CD51C1C1C21000000000000
      000000000000000000000000000000000000000000003C3C3C53606060CC4949
      496C646464C52C2C2C370E0E0E0F5353538E5F5F5FBA2F2F2F3C000000003333
      3343616161BB5151518B0A0A0A0B0000000000000000524C4C6D5B52527A5A54
      547A5A55557A5A54547A5A53537A5A52527A5952527A5951517A5951517A5950
      507A5A50507C12121214000000000000000000000000524C4C6D5B52527A5A54
      547A5A55557A5A54547A5A53537A5A52527A5952527A5951517A5951517A5950
      507A5A50507C1212121400000000000000000000000000000000000000000000
      000000000000000000000E0E0E0F303030410C0C0C0D00000000000000000000
      00000000000000000000000000000000000000000000000000003C3C3C516767
      67E9666666C02E2E2E3A00000000000000002626262E5C5C5CAE5A5A5AA61919
      191C0F0F0F10616161CD35353546000000000000000000000000000000005E69
      5E9B0000000100000000000000004E4848687360609A7160609A715A5A9A6F55
      559A5E5151820000000000000000000000000000000000000000383A384C4346
      43610000000000000000000000004E4848687360609A7160609A715A5A9A6F55
      559A5E5151820000000000000000000000000000000000000000000000000000
      00000000000013131315344B4EB4384B4EAB3435354900000000000000000000
      0000000000000000000000000000000000000000000029292933606060C42222
      22282A2A2A345E5E5EB35D5D5DAF2525252D00000000000000003D3D3D546666
      66C6696969DD4343436100000000000000000000000000000000000000003A95
      3AEB5367539C0101010200000000765A5A9CF27575FFE97070FFE24C4CFFDA28
      28FF905555C10000000000000000000000000000000034363445278C27E9454B
      4569000000000000000000000000765A5A9CF27575FFE96F6FFFE24C4CFFDA28
      28FF905555C10000000000000000000000000000000000000000000000000000
      0000000000003131314301ADC0FD00DEFFFF33494EB600000000000000000000
      000000000000000000000000000000000000000000002525252D5F5F5FB65757
      579E2D2D2D39424242D6464646E0585858D05A5A5AA01A1A1A1E1A1A1A1E6363
      63C35A5A5AA4616161C02929293300000000506650963B8A3BD33B8A3BD30FA6
      0FFA01A601FF52675299000000013231313E4B4949644B4949644B4848644B47
      47643D3B3B4F000000000000000000000000383A384C269C26EC00A500FF3494
      34DF3B8A3BD34A904AD30C0C0C0D3231313E4B4949644B4949644B4848644B47
      47643D3B3B4F0000000000000000000000000000000000000000000000000000
      0000000000000D0D0D0E1E5F67DC00DEFFFF0E94AFF13B4649A1070707080000
      0000000000000000000000000000000000000000000000000000000000004242
      4263424242F4E2E2E2FFF1F1F1FF928C8BEF494544D75D5D5DC9606060C83232
      3241343434455E5E5EC01C1C1C2000000000506850963B943BD33B943BD30FB6
      0FFA01BB01FF526B529900000001554D4D72926B6BB9907878B98F7272B98E6D
      6DB98C6767B98B6161B98A5B5BB95F535384383B384C26AB26EC00BA00FF349F
      34DF3B943BD34A984AD30C0C0C0D554D4D72926B6BB9907878B98F7272B98E6D
      6DB98C6767B98B6161B98A5B5BB95F5353840000000000000000000000000000
      000000000000000000003F44457B06ADC5F800D5FFFF01B8E5FE294750CB1919
      191D000000000000000000000000000000000000000000000000151515184343
      43D4E2E2E2FFF2F2F2FFF2EAE9FFCD994DFFE3AD46FF6A6664E4454545AC3B3B
      3B535F5F5FBD1818181B00000000000000000000000000000000000000003ABC
      3AEB5470549D01010102000000006D585891F86161FFF09494FFEB7C7CFFE766
      66FFE25050FFDD3B3BFFD92525FF795858A7000000003436344527B627E9454D
      45690000000000000000000000006D585891F86161FFF09494FFEB7C7CFFE766
      66FFE25050FFDD3B3BFFD92525FF795858A70000000000000000000000000000
      00000000000000000000000000012E4A4EC200D5FFFF00CDFFFF00BFF9FF2E45
      4EC2000000000000000000000000000000000000000015151518434343D4E4E4
      E4FFF2F2F2FFF2ECEBFFC7964EFF9C9C00FFD3D300FFEFB648FFC9C2C0FE4949
      49F54242426D0000000000000000000000000000000000000000000000005E73
      5E9B0000000100000000000000001E1E1E233736364637373746373737463736
      36463736364637363646373636462423232A0000000000000000383B384C4349
      43610000000000000000000000001E1E1E233736364637373746373737463736
      36463736364637363646373636462423232A0000000000000000000000000000
      00000000000000000000000000003D40406800B8DCFF00CDFFFF00C4FFFF0E74
      9BF12525252D0000000000000000000000000000000013131315444444B57272
      72EAEBE5E4FFC2914EFF929200FF9C9C00FFD1D100FFE4E400FFEFB649FFF2EB
      E9FF919191F2434343C3151515180000000002020203625757835F5656815F58
      58815F5959815F5757815E5757815E5656815E5555815E5454815E5353815D52
      52815E54548202020203000000000000000003030304625757835F5656815F58
      58815F5959815F5757815E5757815E5656815E5555815E5454815E5353815D52
      52815E5454820101010200000000000000000000000000000000000000002323
      232B274A4BCE3F4646832C2C2C39374446AC00C7EEFF00CDFFFF00C4FFFF01A0
      DAFF3C3E3E630000000000000000000000000000000000000000000000002A2A
      2A364A413EC9A17063F4E9B6A7FFB48A12FFDBAF18FFF3C0A8FFF3C0A8FFF2E5
      E0FFD5D5D5FE414141CB0E0E0E0F0000000008080809E07474EFF86E6EFFF38E
      8EFFEF9494FFED8686FFE97777FFE76969FFE45C5CFFE14E4EFFDE4040FFDB33
      33FFC75A5AEF0707070800000000000000000909090AE17373F0F86E6EFFF38E
      8EFFEF9494FFED8686FFE97777FFE76969FFE45C5CFFE14E4EFFDE4040FFDB33
      33FFC75B5BEE0606060700000000000000000000000000000000000000003D3F
      3F6700D6DCFF00DAE8FF03A7B9FB00C9E7FF00D5FFFF00CDFFFF00C4FFFF0197
      CEFE393A3A580000000000000000000000000000000000000000000000000000
      0000000000003636364C444444CBAC820FFED9AF1CFFF2F2F2FFF2F2F2FFD4D4
      D4FE414141C90D0D0D0E0000000000000000000000011E1E1E231F1F1F241F1F
      1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F
      1F241E1E1E23000000000000000000000000000000011E1E1E231F1F1F241F1F
      1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F1F241F1F
      1F241E1E1E230000000000000000000000000000000000000000000000003F45
      457B00E3E9FF00F0FFFF00E7FFFF00DEFFFF00D5FFFF00CDFFFF00C4FFFF1C54
      6ADE0F0F0F110000000000000000000000000000000000000000000000000000
      00000000000000000000010101029A730EF1BF970AFAC2C2C2FDD2D2D2FE4242
      42C70C0C0C0D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003A3C
      3C5B09A2A7F500F0FFFF00E7FFFF00DEFFFF00D5FFFF00CDFFFF107495ED3F42
      4373000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000006A553AA2715B3BA04545458F424242B80808
      0809000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003A3B3C5A344D4FB52A575BC9205A63D926565FD0324A50BA3536364D0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000F0F0F11434357923434
      77CD323278CF4242599614141417000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000070707080F0F0F110F0F
      0F110F0F0F110F0F0F110F0F0F11101010123C4F6D9C323436460F0F0F110F0F
      0F110F0F0F110F0F0F11070707080000000000000000070707080F0F0F110F0F
      0F110F0F0F110F0F0F110F0F0F110F0F0F110F0F0F110F0F0F110F0F0F110F0F
      0F110F0F0F110F0F0F1107070708000000000F0F0F11383879D10D0DBAFF0D0D
      BAFF0D0DBAFF0D0DBAFF2E2E7CD8212121270F0F0F110F0F0F110F0F0F110F0F
      0F110F0F0F110F0F0F1107070708000000004F4F4697797951F17C7C4FF87C7C
      4FF87C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C
      4FF87C7C4FF87C7C4FF8797951F14D4D478D5151469F87875CF98D8C4FFF8D8D
      51FF8D8D5AFF8D8D5AFF8C8C5BFF336BAFFF0163F7FF1263D8FF6C8175FF8D8D
      5AFF8D8D5AFF8D8D5AFF86865DF85252489E5151469F87875CF98D8D5AFF8D8D
      5AFF8D8D5AFF8D8D5AFF8D8D5AFF8D8D5AFF8D8D5AFF8D8D5AFF8D8D5AFF8D8D
      5AFF8D8D5AFF8D8D5AFF86865DF85252489E494969DA4242C7FF1515BCFF0D0D
      BAFF0D0DBAFF0D0DBAFF0D0DBAFF474774FF8D8D5AFF8D8D5AFF8D8D5AFF8D8D
      5AFF8D8D5AFF8D8D5AFF86865DF85252489E737347F1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF70704BEB707043F0EEEDDBFFB9A743FFB8A7
      44FFF1F0E1FFFFFFFFFF6DA2ECFF1875FAFF1676FFFF1575FFFF2D7BEEFFC7DA
      F7FFFFFFFFFFFFFFFFFFFFFFFFFF74744DEE707044F0FFFFFFFFFFFFFFFFFFFF
      FFFFEBEBEBFF9F9F9FFFE1E1E1FFFFFFFFFFFFFFFFFFE3E3E3FF9D9D9DFFE5E5
      E5FFFFFFFFFFFFFFFFFFFFFFFFFF74744DEE5959A7FD5E5ED0FF4545C9FF1111
      BBFF0D0DBAFF0D0DBAFF0D0DBAFF3E3EACFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF73734CED767644F7C9E4C9FFB6DBB6FFF8F8
      F8FFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEEEEFFEEEE
      EEFFEEEEEEFFEEEEEEFFF4F4F4FF737349F1767639F6BDB13DFFE1C12AFFE0BF
      2BFFBBAF3EFFF1F1E1FFD0E1F8FF3D88EEFF318DFFFF318CFFFF308CFFFF3987
      EEFFC9DCF7FFFFFFFFFFFFFFFFFF75754AF3737342F5FFFFFFFFFFFFFFFFEAEA
      EAFF8E8E8EFFF7F7F7FF919191FFE1E1E1FFE7E7E7FF999999FFFAFAFAFF9393
      93FFE7E7E7FFFFFFFFFFFFFFFFFF74744AF36262AFFE7878DBFF6868D5FF4C4C
      CBFF1919BCFF0D0DBAFF0D0DBAFF3E3EACFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3767644F78BC58BFF66B466FFA5A5
      A5FF535353FF535353FF535353FF535353FF535353FF535353FF535353FF5353
      53FF535353FF535353FF757575FF737349F1787737F6CAC52CFFF5EB0CFFF5EA
      0DFFF4E80DFFC3BF33FFF2F2E1FFD0E1F8FF3E89EEFF3791FFFF3791FFFF3B8A
      F1FFB7D1F5FFFFFFFFFFFFFFFFFF75754AF3737342F5FFFFFFFFFFFFFFFF9797
      97FFF7F7F7FFFFFFFFFFFEFEFEFF636363FF5E5E5EFFFCFCFCFFFFFFFFFFFCFC
      FCFF989898FFFFFFFFFFFFFFFFFF74744AF35D5D89FB8E8EE8FF8282E0FF7272
      D8FF6363D2FF4242C7FF2828C0FF7676B7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3767644F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF737349F1727241F5EBEBCFFFCECE26FFFFFF
      00FFFFFF00FFF1F102FFCACA70FFFCFEFFFFD1E2F8FF408BEEFF3A8AF2FFB3CE
      F5FFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3737342F5FFFFFFFFF0F0F0FF5757
      57FF999999FF999999FF999999FF676767FF686868FF999999FF999999FF9999
      99FF585858FFF2F2F2FFFFFFFFFF74744AF36F6F42F57B7BD5FF9696EBFF8A8A
      E4FF7C7CDEFF6C6CD7FF6464BDFFEAEAEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3767644F7A0D0A0FF83C183FF8F8F
      8FFF424242FF424242FF424242FF424242FF424242FF424242FF434343FFCBCB
      CBFF6E6E6EFF646464FF777777FF737349F1727241F5FFFFFFFFECECD0FFCECE
      26FFF1F101FFC5C558FFBCEDFCFF2AC2F3FFB9EDFDFFD1E2F8FFBBD3F6FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3737342F5FFFFFFFFFFFFFFFF9494
      94FFF3F3F3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4F4
      F4FF979797FFFFFFFFFFFFFFFFFF74744AF3737342F5F1F1F4FF8A8ACDFF7A7A
      D4FF7979D1FF8989C7FFEBEBF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3767644F746A446FF209120FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF737349F1727241F5FFFFFFFFFFFFFFFFEBEB
      CFFFC9C96CFFC0EDFDFF21BCEBFF00A4D7FF1EB9EAFFBAECFDFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3737342F5FFFFFFFFFFFFFFFFF3F3
      F3FF969696FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9999
      99FFF3F3F3FFFFFFFFFFFFFFFFFF74744AF3737342F5FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3767644F747A447FF219121FF9696
      96FF535353FF535353FF535353FF535353FF535353FF535353FF535353FFC9C9
      C9FF6D6D6DFF646464FF777777FF737349F1727241F5FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF33D4FAFF00D5EDFF00D4ECFF00D3ECFF1ED3F5FFBAF0FEFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3737342F5FFFFFFFFFFFFFFFFFFFF
      FFFFA8A8A8FFE2E2E2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3E3E3FFA9A9
      A9FFFFFFFFFFFFFFFFFFFFFFFFFF74744AF3737342F5FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3767644F7BADDBAFFA3D1A3FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF737349F1727241F5FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFDAF7FFFF2AE5FFFF00FFFFFF00FFFFFF00FFFFFF1CE6FFFFD9F7
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF3737342F5FFFFFFFFFFFFFFFFFFFF
      FFFFFDFDFDFF919191FFA8A8A8FFFFFFFFFFFFFFFFFFADADADFF969696FFFDFD
      FDFFFFFFFFFFFFFFFFFFFFFFFFFF74744AF3737342F5FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF75754AF36E633AFA968F6CFF968F6CFF968F
      6CFF968F6CFF968F6CFF968F6CFF968F6CFF968F6CFF968F6CFF968F6CFF968F
      6CFF968F6CFF968F6CFF968F6CFF6B613CF56E6139FA928964FF928964FF9289
      64FF928964FF928964FF7C937AFF18D0E4FF00FFFFFF00F9FEFF2EBCCDFF908A
      66FF928964FF928964FF928964FF6B5F3CF76D6139FA928964FF928964FF9289
      64FF928964FF928964FF928964FF928964FF928964FF928964FF928964FF9289
      64FF928964FF928964FF928964FF6B5F3CF76E623AFA928964FF928964FF9289
      64FF928964FF928964FF928964FF928964FF928964FF928964FF928964FF9289
      64FF928964FF928964FF928964FF6D623DF7B0906CFDE4B692FFE3B590FFE3B4
      8EFFE2B38CFFE2B28AFFE2B189FFE1B087FFE1AF85FFE0AE83FFE0AD82FFE0AD
      82FFE0AD82FFE0AD82FFE0AD82FFA28159FBAC8E6BFDE5B895FFE4B793FFE4B6
      91FFE3B58FFFE3B48EFFE2B38CFFC2B69BFF25D6EBFF49C8D6FFDEAF86FFE0AD
      83FFE0AD82FFE0AD82FFE0AD82FFA37F58FCAD8E6CFDE5B895FFE4B793FFE4B6
      91FFE3B58FFFE3B48EFFE2B38CFFE2B28AFFE2B188FFE1B086FFE1AF85FFE0AD
      83FFE0AD82FFE0AD82FFE0AD82FFA37F58FCAD8E6CFDE5B895FFE4B793FFE4B6
      91FFE3B58FFFE3B48EFFE2B38CFFE2B28AFFE2B188FFE1B086FFE1AF85FFE0AD
      83FFE0AD82FFE0AD82FFE0AD82FFA48059FC796E5BE5EFD2C1FFEFD2C0FFEFD1
      BEFFEED0BCFFEECFBBFFEDCEB9FFEDCDB7FFECCBB5FFECCAB3FFECC9B2FFEBC8
      B0FFEBC7AEFFEAC6ACFFE8C3A9FF6C624CDB706656DBEAD0C0FFF0D4C3FFEFD3
      C1FFEFD2C0FFEFD1BEFFEED0BCFFEECEBAFFDACCBEFFEACCB8FFECCBB5FFECCA
      B3FFEBC9B1FFEBC8AFFFE4C1A8FF685F4DD7706656DBEAD0C0FFF0D4C3FFEFD3
      C1FFEFD2C0FFEFD1BEFFEED0BCFFEECEBAFFEDCDB8FFEDCCB7FFECCBB5FFECCA
      B3FFEBC9B1FFEBC8AFFFE4C1A8FF685F4DD7706756DBEBD1C1FFF0D4C3FFEFD3
      C1FFEFD2C0FFEFD1BEFFEED0BCFFEECEBAFFEDCDB8FFEDCCB7FFECCBB5FFECCA
      B3FFEBC9B1FFEBC8AFFFE4C1A8FF68604CD60909090A4D4C498E504F4A97504F
      4A97504F4A97504F4A97504F4A97504F4997504F4997504F4997504F4997504F
      4997504F4997504F49974B4A4788030303040101010245444273494946854949
      4685494946854949468549484685494846854948468549484685494846854948
      4685494846854948468543434270010101020101010245444273494946854949
      4685494946854949468549484685494846854948468549484685494846854948
      4685494846854948468543434270010101020101010245444273494946854949
      4685494946854949468549484685494846854948468549484685494846854948
      4685494846854948468543434270010101020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002A2A2A33070707080000
      00000000000000000000030303045A5A6E9249494E6700000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000039393A524343569F3C3C63BC3B3B66C141415BAB434348750606
      060700000000000000000000000000000000000000006060AAD662628BB70909
      090A0000000007070708636384AB666691B80505050600000000000000000000
      0000000000000000000000000000000000000000000046453F835B532CC65B53
      2CC65B532CC65B532CC6474433B6000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001111
      111341415AB31818A0F80D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0E0EB5FF3636
      69CF32323445000000000000000000000000000000005B5B6E962121D9F86363
      8FB81818181B646497BD64648EB5060606070404040540404569424259964242
      59953E3E42620202020300000000000000004F4F4696534D1EFAF1BE00FFCE84
      00FFCE8400FFE0A100FF66590FFD7C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C
      4FF87C7C4FF87C7C4FF8797951F14E4E478E4F4F4697797951F17C7C4FF87C7C
      4FF87C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C4FF87C7C
      4FF87C7C4FF87C7C4FF8797951F14D4D478D00000000000000000C0C0C0D3E3E
      66C91111B9FF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0D
      BAFF282883E6313133430000000000000000000000000000000049494E694343
      C7E55454CBE5636391B7070707081E1E1F2437376FC70D0DB9FF0D0DBAFF0D0D
      BAFF0E0EB8FF3A3A69BF1818181B02020203727246F1817858FFE6AB00FFAA44
      00FFAA4400FFC87700FF615310FF5D5D5DFF585858FFF9F9F9FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF6F6F4AEB727246F1FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFE0E6E0FFB9C7B9FFB9C7B9FFB9C7B9FFB9C7B9FFB9C7
      B9FFB9C6B9FFC5CDC5FFFFFFFFFF6F6F4AEB00000000000000004646569F3737
      C2FF2424C0FF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0D
      BAFF0D0DBAFF373768CD05050506000000000000000000000000232323295959
      ACCF5353D0E6636392B90B0B0B0C444472C42222BFFF0D0DBAFF0D0DBAFF0D0D
      BAFF0D0DBAFF0D0DBAFF3F3F62B007070708767643F7817858FFEDB800FFC36E
      00FFC36E00FFD89500FF756825FFCCCCCCFF595959FFF0F0F0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F1767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF7CB87CFF00B300FF00AF00FF00AF00FF00AF00FF00AF
      00FF009C00FF2D852DFFFFFFFFFF707047F1000000000F0F0F11474795EC4A4A
      CAFF3F3FC6FF1717BCFF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0D
      BAFF0D0DBAFF0E0EB4FF4141456F00000000000000002A2A2B345454B3D56262
      99BE1717171A61617CA46F6FA8D35A5ACBFE4747C8FF1515BCFF0D0DBAFF0D0D
      BAFF0D0DBAFF0D0DBAFF1414ABFB2E2E2E3C767643F7817858FFE9B100FFB557
      00FFB55700FFCF8500FF756825FFCCCCCCFF595959FFF0F0F0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F1767643F7FFFFFFFFFFFFFFFFFFFF
      FFFFEAEBEAFFDADCDAFF669F66FF009400FF008E00FF008E00FF008E00FF008E
      00FF007D00FF2A742AFFFFFFFFFF707047F1000000004141446B5E5ECEFF5B5B
      CFFF5151CBFF3D3DC7FF1010BBFF0D0DBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0D
      BAFF0D0DBAFF0D0DBAFF42425AA600000000333335424949BDDE626297BD0B0B
      0B0C0505050638383A5166669BDB9393E6FF5F5FD2FF4848C9FF1313BBFF0D0D
      BAFF0D0DBAFF0D0DBAFF0D0DBAFF4040466C767643F7817858FFEFBB00FFC979
      00FFC97900FFDC9B00FF756825FFCCCCCCFF595959FFF0F0F0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F1767643F7FFFFFFFFFFFFFFFFFFFF
      FFFF5AAA5AFF00AF00FF00AA00FF00A300FF00A300FF00A300FF00A300FF4499
      44FFEEEEEEFFF1F1F1FFFFFFFFFF707047F100000000464650887272D9FF6A6A
      D5FF6161D2FF5757CEFF3B3BC7FF0F0FBAFF0D0DBAFF0D0DBAFF0D0DBAFF0D0D
      BAFF0D0DBAFF0D0DBAFF3D3D63BB00000000404043585D5D7FA60E0E0E0F3434
      3648313178D30E0EB8FF2121A3FF8484E1FF7676DAFF6868D5FF5252CCFF2020
      BFFF0D0DBAFF0D0DBAFF0D0DB9FF3E3E4162767643F7817858FFE7AE00FFAE4C
      00FFAE4C00FFCB7C00FF756825FFCCCCCCFF595959FFF0F0F0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F1767643F7FFFFFFFFFFFFFFFFFFFF
      FFFF5DB05DFF009B00FF009600FF009600FF009600FF009600FF009C00FF4DAC
      4DFFFFFFFFFFFFFFFFFFFFFFFFFF707047F10000000046464D828282E0FF7979
      DBFF7070D8FF6767D4FF5E5ED0FF4545C9FF1515BCFF0D0DBAFF0D0DBAFF0D0D
      BAFF0D0DBAFF0D0DBAFF3F3F60B5000000000000000000000000282828323434
      99EB0E0EBBFF0D0DBAFF1111ADFF7979DAFF8B8BE4FF7D7DDFFF7171D8FF6363
      D2FF4A4ACAFF2E2EC3FF2E2E8DE615151518767643F7B8B5A7FF88815CFF544D
      2BFF696240FF736C4AFF79755EFFCCCCCCFF595959FFF0F0F0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F1767643F7FFFFFFFF318C31FF0C9A
      0CFF0A880AFF087B08FF087B08FF087B08FF087B08FF588B58FFBEC8BEFFD3DA
      D3FFFFFFFFFFFFFFFFFFFFFFFFFF707047F1000000003636374C7878D9FE8787
      E2FF7D7DDFFF7575DAFF6C6CD7FF6363D2FF5757CFFF3131C3FF1414BCFF0D0D
      BAFF0D0DBAFF0D0DBAFF444454980000000000000000000000004A4A68AE4848
      C9FF2525BFFF0D0DBAFF0D0DBAFF3131AAFF9595ECFF9292E8FF8787E2FF7979
      DCFF6C6CD6FF5656C6FD44444C7807070708767643F7FFFFFFFFFFFFFFFF7777
      77FFB1B1B1FFCCCCCCFFCCCCCCFFCCCCCCFF595959FFF0F0F0FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F1767643F7FFFFFFFF2FA02FFF0096
      00FF009600FF009600FF009600FF009600FF00A500FF7AB77AFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F100000000030303045A5A98E29292
      EAFF8C8CE5FF8585E1FF7B7BDDFF7272D8FF6969D5FF5F5FD2FF5555CEFF4242
      C8FF3030C3FF29299EF531313243000000000000000000000000565696DF6262
      D2FF5252CCFF1D1DBEFF0D0DBAFF0D0DBAFF2B2BAAFF6F6FD5FF8A8AE6FF7B7B
      DCFF5D5DA1E543434A710000000007070708767643F7FFFFFFFFFFFFFFFF9595
      95FF444444FF4A4A4AFF4A4A4AFF4A4A4AFF363636FFF6F6F6FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F1767643F7FFFFFFFFB8C9B8FFADC4
      ADFFADC4ADFFADC4ADFFADC4ADFFADC4ADFFADC4ADFFD7E0D7FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF707047F100000000000000004242466D8383
      DEFD9797EDFF9090E8FF8989E3FF8181DFFF7777DCFF6F6FD7FF6666D3FF5C5C
      D0FF4E4ECAFF45455BAD000000000000000000000000000000005E5EA4E57878
      DBFF6A6AD5FF5A5ACEFF2727C1FF0D0DBAFF0D0DBAFF0E0EB9FF34346FD42323
      242B0101010200000000000000000A0A0A0B6E633AFA968F6CFF968F6CFF968F
      6CFF968F6CFF968F6CFF968F6CFF968F6CFF968F6CFF968F6CFF968F6CFF968F
      6CFF968F6CFF968F6CFF968F6CFF665E38F66F6134FA8F7C54FF8F7C54FF8F7C
      54FF8F7C54FF8F7C54FF8F7C54FF8F7C54FF8F7C54FF8F7C54FF8F7C54FF8F7C
      54FF8F7C54FF8F7C54FF8F7C54FF665A33F50000000000000000000000004A4A
      59998585E2FD9A9AEFFF9595EBFF8E8EE7FF8686E2FF7C7CDDFF7474DAFF6464
      D2FF464668C70C0C0C0D0000000000000000000000000000000055557EC18D8D
      E6FF8181DFFF7272D9FF6565D3FF4A4ACAFF2929C1FF1919BDFF42425B9D0000
      00000000000000000000000000000909090AB0906CFDE4B692FFE3B590FFE3B4
      8EFFE2B38CFFE2B28AFFE2B189FFE1B087FFE1AF85FFE0AE83FFE0AD82FFE0AD
      82FFE0AD82FFE0AD82FFE0AD82FF9D7B54FCB0906CFDE4B692FFE3B590FFE3B4
      8EFFE2B38CFFE2B28AFFE2B189FFE1B087FFE1AF85FFE0AE83FFE0AD82FFE0AD
      82FFE0AD82FFE0AD82FFE0AD82FF9C7A55FB0000000000000000000000000000
      00004141466B5C5C9CE38383E2FE9696EDFF8F8FE8FF7979DDFF5858A0EC4848
      569A0606060700000000000000000000000000000000000000002D2D2E3B8080
      D7F99393E9FF8888E4FF7B7BDDFF6E6ED7FF5F5FD2FF4A4AA4ED2626262F0000
      000000000000000000000000000000000000786E5AE5EFD2C1FFEFD2C0FFEFD1
      BEFFEED0BCFFEECFBBFFEDCEB9FFEDCDB7FFECCBB5FFECCAB3FFECC9B2FFEBC8
      B0FFEBC7AEFFEAC6ACFFE8C3A9FF6D614DDC796E5BE5EFD2C1FFEFD2C0FFEFD1
      BEFFEED0BCFFEECFBBFFEDCEB9FFEDCDB7FFECCBB5FFECCAB3FFECC9B2FFEBC8
      B0FFEBC7AEFFEAC6ACFFE7C3A8FF6C614DDC0000000000000000000000000000
      000000000000020202033333334544444B7C46464D823E3E40600B0B0B0C0000
      0000000000000000000000000000000000000000000000000000000000003D3D
      415C7070C2F19797EDFF8F8FE7FF8181E0FF57579DE53737384D000000000000
      0000000000000000000000000000000000000909090A4C4C488C504F4A97504F
      4A97504F4A97504F4A97504F4A97504F4997504F4997504F4997504F4997504F
      4997504F4997504F49974B4A4788030303040909090A4D4C498E504F4A97504F
      4A97504F4A97504F4A97504F4A97504F4997504F4997504F4997504F4997504F
      4997504F4997504F49974B4A4788030303040000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000909090A40404567484853833D3D415F0707070800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000222222287774749E7571719B1E1E1E23000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000817C7CA8958B8BBB948A8ABB797474A0000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000021212127D4C9C9E9EE9090FFE97777FFCFBEBEE71B1B1B1F0000
      0000000000000000000000000000000000000000000000000000151515180D0D
      0D0E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B7A1A1D8E76363FFE35151FFAF9494D3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000746F6F99F5A2A2FFE86B6BFFE14848FFE36666FE6663638A0000
      0000000000000000000000000000000000000A0A0A0BA49F9FC7D0BABAE8D4C1
      C1EA6C69699100000000000000000000000000000000000000003332323F4947
      4760000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B7A1A1D8E76363FFE35151FFAF9494D3000000000000
      000000000000000000000000000000000000000000001E1E1E2445454BFF7272
      84FF727283FF727283FF727283FF727282FF727282FF727282FF727281FF7272
      81FF727282FF414146FF15151518000000000000000000000000000000000000
      000000000000746F6F99F5A2A2FFE86B6BFFE14848FFE46666FF6663638A0000
      0000000000000000000000000000000000006A66668DEAB1B1F9E86767FFE044
      44FFD4A5A5ED3131313D0000000000000000000000003030303CC3A0A0E1CA94
      94E94A4949620000000000000000000000000000000000000000000000000000
      00000000000000000000B7A1A1D8E76363FFE35151FFAF9494D3000000000000
      000000000000000000000000000000000000000000001F1F1F258E8EB4FF2020
      F8FF2020F3FF2020EEFF2020E8FF2020E2FF2020DDFF2020D8FF2020D3FF2020
      CFFF2020DAFF8D8DA9FF16161619000000000000000000000000000000000000
      00000000000021212127D4C8C8E9ED9090FFE97777FFCFBFBFE71C1C1C200000
      000000000000000000000000000000000000A09696C4F19797FFE86767FFE044
      44FFE57272FE5453537100000000000000002B2B2B34C3A4A4E1E04141FFDC32
      32FFC99090E84B4A4A64000000000000000000000000000000000A0A0A0B6863
      638B6E6868926E6A6A92C6ACACE2E76363FFE35151FFC1A2A2E06C6666926C66
      66926662628A060606070000000000000000000000001F1F1F258C8CB3FF0404
      F8FF1A1AF6FF1212F1FF0A0AECFF0606E5FF0101DDFF0000D5FF0000CDFF0000
      C7FF0000D4FF8C8CA8FF16161619000000000000000000000000000000000000
      00000000000000000000222222287774749E7571719B1E1E1E23000000000000
      0000000000000000000000000000000000005F5C5C7FE5B5B5F6E86767FFE047
      47FFD4AFAFEC2B2B2B340000000028282830C2A8A8E0E24E4EFFDF3D3DFFDC32
      32FFDA2626FFC68A8AE74443435A00000000000000000000000000000001877F
      7FADE99F9FF7F09999FFEB8080FFE76363FFE35151FFE14C4CFFE04949FFD97A
      7AF67C7474A4000000000000000000000000000000001F1F1F258C8CB3FF0707
      FAFF2B2BF9FF1F1FF5FF1414EFFF0C0CE9FF0606E2FF0101DAFF0000D2FF0000
      C8FF0000D0FF8C8CA8FF16161619000000000000000000000000000000000000
      00000000000000000000020202038D8585B4857E7EAD01010102000000000000
      00000000000000000000000000000000000006060607868383ADD2C4C4E8D1C4
      C4E857555575000000002727272EC1A9A9DFE45959FFE14949FFDF3D3DFFDC32
      32FFDA2626FFD71C1CFFC58E8EE63F3F3F520000000000000000000000000000
      0001817B7BA8E19F9FF6EA7575FFE76363FFE35151FFDF3F3FFFD46565F57A73
      73A200000000000000000000000000000000000000001F1F1F258C8CB3FF0909
      FBFF3636FAFF2929F6FF1E1EF1FF1414ECFF0C0CE5FF0505DDFF0101D5FF0000
      CDFF0000CCFF8C8CA8FF16161619000000000000000000000000000000000000
      000000000000010101028B8585B2E08686F8DB7777F7857D7DAD010101020000
      0000000000000000000000000000030303047E7C7CA400000000040404050000
      000100000000000000006E6B6B939E9393C4B1A5A5D2E38080FBDF3D3DFFDC32
      32FFDA2626FFC3A2A2E29B8989C2898181B10000000000000000000000000000
      000000000001827E7EA9E09393F7E76363FFE35151FFD96C6CF7837B7BAB0000
      000100000000000000000000000000000000000000001F1F1F258C8CB3FF0D0D
      FDFF4545FCFF3636F8FF2828F3FF1C1CEEFF1313E8FF0B0BE1FF0505DAFF0101
      D1FF0000C9FF8C8CA8FF16161619000000000000000000000000000000000000
      000000000001827E7EA9E09393F7E76363FFE35151FFD96C6CF7837B7BAB0000
      000100000000000000000000000000000000C0BABADB08080809000000000000
      0000000000000000000000000000000000007F7878A6E45C5CFFDF3D3DFFDC32
      32FFDB2A2AFF918484B900000000000000000000000000000000000000000000
      000000000000010101028C8686B3E18888F9DB7777F7857D7DAD010101020000
      000000000000000000000000000000000000000000001F1F1F258C8CB3FF0F0F
      FEFF5454FCFF4444F9FF3434F5FF2727F1FF1B1BEBFF1212E4FF0A0ADDFF0404
      D5FF0000C9FF8C8CA8FF16161619000000000000000000000000000000000000
      0001807A7AA7E1A0A0F6EA7575FFE76363FFE35151FFDF3F3FFFD86666F7837A
      7AAB00000001000000000000000000000000948989BA908888B6000000010000
      00000000000000000000000000003B3B3B4CCB9A9AE8E14949FFDF3D3DFFDC32
      32FFDB6464FA4B49496300000000000000000000000000000000000000000000
      00000000000000000000020202038E8686B5857D7DAC01010102000000000000
      000000000000000000000000000000000000000000001F1F1F258C8CB3FF1313
      FFFF6565FDFF5353FBFF4242F7FF3333F3FF2626EEFF1A1AE8FF1111E1FF0909
      DAFF0000CAFF8C8CA7FF1616161900000000000000000000000000000001867E
      7EACE9A0A0F7F09999FFEB8080FFE76363FFE35151FFE14C4CFFE04949FFD979
      79F67D7575A500000000000000000000000029292931E4CCCCF2A49898C73131
      313D0F0F0F111D1D1D21736F6F99C7A1A1E5E35555FFE14949FFDF3D3DFFDC33
      33FFB79E9ED90808080900000000000000000000000000000000000000000000
      00000000000000000000222222287673739D736F6F991E1E1E22000000000000
      000000000000000000000000000000000000000000001F1F1F258C8CB3FF1515
      FFFF7878FEFF6464FCFF5151F9FF4040F5FF3131F1FF2424EBFF1919E4FF1010
      DDFF0101CDFF8C8CA7FF161616190000000000000000000000000A0A0A0B6863
      638B6E6868926E6A6A92C6ADADE2E76363FFE35151FFC1A2A2E06C6666926C66
      66926662628A06060607000000000000000000000000746E6E99E99898F7DBA6
      A6F0C5A8A8E2CEA6A6E9EA8484FEE66060FFE35454FFE14949FFDF3D3DFFCA93
      93E93D3C3C4E0000000000000000000000000000000000000000000000000000
      00000000000021212127D4C9C9E9EE9191FFE97878FFCEBEBEE71A1A1A1E0000
      000000000000000000000000000000000000000000001F1F1F258C8CB3FF1414
      FFFF6F6FFEFF5E5EFCFF4D4DF8FF3F3FF4FF3232F0FF2727EBFF1C1CE5FF1313
      DEFF0101CEFF8C8CA6FF16161619000000000000000000000000000000000000
      00000000000000000000B7A1A1D7E76363FFE35151FFAF9494D3000000000000
      00000000000000000000000000000000000000000000010101027A7474A0D6A9
      A9EDED8686FFEB7777FFE86B6BFFE66060FFE35454FFE35858FFC59E9EE34B49
      4963000000000000000000000000000000000000000000000000000000000000
      000000000000746F6F99F5A1A1FFE86B6BFFE14848FFE36767FE666363890000
      000000000000000000000000000000000000000000001F1F1F258E8EB4FF2020
      FFFF2121FEFF2121F9FF2121F4FF2121EFFF2020E9FF2020E3FF2020DEFF2020
      D9FF2020D4FF8D8DA7FF16161619000000000000000000000000000000000000
      00000000000000000000B7A1A1D7E76363FFE35151FFAF9494D3000000000000
      0000000000000000000000000000000000000000000000000000000000003837
      3746A09696C5C3A8A8E0CD9F9FE9CB9E9EE8BAA0A0DB8B8282B21E1E1E220000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007670709BF5A1A1FFE86B6BFFE14848FFE36767FE666363890000
      000000000000000000000000000000000000000000001E1E1E2445454BFF7272
      85FF727285FF727284FF727284FF727283FF727283FF727283FF727282FF7272
      82FF727282FF414145FF15151518000000000000000000000000000000000000
      00000000000000000000B7A1A1D7E76363FFE35151FFAF9494D3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000080808091F1F1F241919191C0303030400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000022222228D4C8C8E9ED8F8FFFE97676FFCFBEBEE71B1B1B1F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000101010202020203817C7CA8958D8DBC958A8ABC7B7575A2020202030202
      0203020202030202020302020203010101020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002424242A797676A07571719B1E1E1E23000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000038383B4B6060
      95BC1717171A0000000000000000000000000000000000000000000000001919
      1A1D61618FBE3636384700000000000000003F4A3F8A228122D6346D34B92A2A
      2A3500000000575757910B0B0B0C000000000000000000000000000000000000
      0000000000000000000000000000000000001717171A58565676000000000000
      000000000000000000000000000000000000000000000000000055504E74DC6D
      55FAE16753FBDC6D54FAC97F68EE000000000000000000000000000000000000
      00000000000000000000050505060F0F0F110F0F0F1100000001000000000000
      00000000000000000000000000000000000000000000363638473E3ECCE40000
      F7FF5454A3C916161619000000000000000000000000000000001818181B5757
      9BC90000D5FF3B3BB7E739393B4C000000003D4F3D9700D300FF07DB07FF18B8
      18F24054408E5D5D5DA910101012000000000000000000000000000000000000
      0000000000000000000000000000000000002626262DE3C2C2F1656060870000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CF786DEB5B55527E00000000000000000000000000000000000000000000
      000000000000000000006269628F4ABA4AF34AA74AF26F826FBB1C1C1C200000
      000000000000000000000000000000000000363638474242CDE40000FFFF0000
      FBFF0000F7FF5454A3C91616161900000000000000001818181B59599DC80000
      DCFF0000D8FF0000D5FF3B3BB6E5353537463D4F3D9700D300FF2CBE2CFF2DD0
      2DFF17EB17FF5C865CED383A384F000000000000000000000000000000000000
      0000000000000000000000000000000000002626262DDD8181EDE17878F06B64
      648E000000000000000000000000000000000000000000000000000000000000
      0000CF786DEB5B55527E00000000000000000000000000000000000000000000
      00000000000000000000636B639201C301FF00BD00FF00A500FF578E57DB4C4E
      4C6B0000000000000000000000000000000067679EC20C0CFFFF0505FFFF0000
      FFFF0000FBFF0000F7FF55559EC552526A8F52526A8F54549EC80000E4FF0000
      E0FF0000DCFF0000D8FF0000D5FF61618FBE3D4F3D9700D300FF36BB36FF3AB7
      3AFF17EB17FF43C643FF64A264FB434D43840000000000000000000000000000
      0000000000000000000000000000000000002626262DDD8181EDFF2727FFE178
      78F06B66668F0000000000000000000000000000000000000000000000000000
      0000CD8074EB5B56537E00000000000000000000000000000000000000000000
      00000000000000000000636C639201C901FF00C400FF00BF00FF00B100FF2C9F
      2CF76F7D6FB21010101200000000000000001C1C1C205858ADCE0C0CFFFF0505
      FFFF0000FFFF0000FBFF0000F7FF0000F3FF0000EFFF0000ECFF0000E8FF0000
      E4FF0000E0FF0000DCFF50509DCB1D1D1D213D4F3D9700D300FF36CC36FF3AA7
      3AFF17EB17FF1FEB1FFE4B7040D465651AEE757505F9959506F85C5C34B40000
      0000000000000000000000000000000000002626262DDD8181EDFC3737FFF851
      51FFDD9B9BF06B68688F00000000000000000000000000000000000000000000
      0000CB8E84EB5B57547E00000000000000000000000000000000000000000000
      00000000000000000000636C639201D001FF00CA00FF00C600FF00C100FF00BC
      00FF02A102FF659065D43D3E3D5100000000000000001F1F1F245B5BB0D00C0C
      FFFF0505FFFF0000FFFF0000FBFF0000F7FF0000F3FF0000EFFF0000ECFF0000
      E8FF0000E4FF4F4FA5D11E1E1E23000000003D4F3D9700D300FF26D926FF518D
      51FF45A145EB4954419E717103FB707007FF282800FFC4C400FF747400FF5353
      3B9E0000000000000000000000003E3E3E552626262DDA9090EDF56363FFF180
      80FFEE8686FFD8A1A1F06B67678F000000000000000000000000000000000000
      0000C99C92EB5B58557E0000000000000000333333455A5A66BA5A5A66BA5A5A
      65BA5A5A65BA5A5A65BA8DA293E101D601FF00D100FF00CC00FF00C800FF00C3
      00FF00BE00FF00AD00FF60A660EC3E3E3E510000000000000000202020255757
      AFCF0C0CFFFF0505FFFF0000FFFF0000FBFF0000F7FF0000F3FF0000EFFF0000
      ECFF5050AAD22020202500000000000000003D4F3D9700D300FF09CF09FD4A74
      4AC64D4E4D87727213F1A2A21BFF828225FFABAB25FF8C8C04FF808000FF9696
      02FC0F0F0F11000000004141415D585858952626262DD6A2A2EDEE8989FFEC7B
      7BFFE97070FFE76464FFD59191F16A67678F0000000000000000000000000000
      0000C89B91EB5B58557E00000000000000004C4C4E871515F9FF0000F1FF0000
      EAFF0000E3FF0000DBFF718ECEFF01DC01FF00D800FF00D300FF00CE00FF00CA
      00FF00C500FF57AD57E153585378000000000000000000000000000000004E4E
      59771313FFFF0C0CFFFF0505FFFF0000FFFF0000FBFF0000F7FF0000F3FF0000
      F0FF4B4B54710000000000000000000000003536354B3F4B3F77383B38530000
      00002020202697970BFEA0A025FF848424FF82820EFF8E8E00FFABAB1EFFA5A5
      05FF4949487753535395646464B1030303042626262DD5A7A7EDEA7373FFE867
      67FFE55B5BFFE25050FFE04444FFD38D8DF1514F4F6C00000000000000000000
      0000C6958BEB5B57547E00000000000000004C4C4E871717FAFF2323F8FF1717
      F2FF0D0DEBFF0505E3FF7290D0FF01E301FF00DE00FF00DA00FF00D500FF14D2
      14FE739873C42323232900000000000000000000000000000000000000004C4C
      54711A1AFFFF1313FFFF0C0CFFFF0505FFFF0000FFFF0000FBFF0000F7FF0000
      F3FF4949506B00000000000000000000000000000000020202035B5B5B9E6464
      64AF5B5B5AB2B1B10BFFBBBB25FF8E8E11FF9A9A00FFC4C43DFFA7A78DFF9696
      3BF3444444642626262E00000001000000002626262DD39D9DEDE65F5FFFE353
      53FFE14747FFDE3C3CFFDF4242FFB7A1A1D81616161900000000000000000000
      0000C58F85EB5B57547E00000000000000004C4C4E871818FCFF3131FAFF2121
      F4FF1515EEFF0C0CE7FF7492D3FF01E901FF00E500FF00E000FF48CC48F45D67
      5D8C020202030000000000000000000000000000000000000000101010126464
      99BE2222FFFF1A1AFFFF1313FFFF0C0CFFFF0505FFFF0000FFFF0000FBFF0000
      F7FF5A5A9CC30F0F0F1000000000000000000B0B0B0C666666B53F3F3F5B0000
      00003636354B7E7E06F8848413FFADAD2FFFA4A46BFFDCDC90FFDADA97FF4E4E
      437E000000000000000000000000000000002626262DD19595EDE14B4BFFDF3F
      3FFFDC3333FFDD3939FFB79F9FD8161616190000000000000000000000000000
      0000C3887DEB5A56537E00000000000000004C4C4E871717FEFF4040FBFF2F2F
      F7FF2121F1FF1414EBFF7696D4FF23F223FF22EE22FF6FCA94FF5A5F5A9E0000
      000000000000000000000000000000000000000000000F0F0F116B6B9DC13030
      FFFF2929FFFF2222FFFF1B1BFFFF1313FFFF0C0CFFFF0505FFFF0000FFFF0000
      FBFF0000F7FF595995BC0E0E0E0F00000000000000000B0B0B0C000000000D0D
      0D0E44622FEB5D5D00FF818114FED2D26DFFDADA8BFFBEBE72FB6D6D4FCC0000
      0001000000000A0A0A0B2727272F000000002626262DCF8C8CEDDD3737FFDB2B
      2BFFDB3232FFB29C9CD516161619000000000000000000000000000000000000
      0000C28378EB5A56537E00000000000000004C4C4E871818FFFF5252FCFF3F3F
      F9FF2D2DF4FF1F1FEDFF2529E4FF3A47D9FF3541D3FF2020D0FF4A4A4B800000
      0000000000000000000000000000000000000C0C0C0D6D6D99BE3E3EFFFF3737
      FFFF3030FFFF2929FFFF2222FFFF1B1BFFFF1313FFFF0C0CFFFF0505FFFF0000
      FFFF0000FBFF0000F7FF595993BB0E0E0E0F0000000000000000000000004343
      436541733AF95E8321FF616116E63A3A38555D5D4FBF222222294747476A5D5D
      5DAE646464B0626262AE3E3E3E58000000002626262DCD8484EDD92323FFDA2B
      2BFFB29C9CD51616161900000000000000000000000000000000000000000000
      0000C17B6FEB5A55527E00000000000000004C4C4E871919FFFF6666FDFF5050
      FAFF3C3CF6FF2C2CF0FF1E1EEAFF1212E2FF0909D9FF1919CEFF4A4A4B800000
      0000000000000000000000000000000000006C6C84AB4D4DFFFF4646FFFF3F3F
      FFFF3737FFFF3030FFFF5F5FB6D448484C6549494D665252BAD70C0CFFFF0505
      FFFF0000FFFF0000FBFF0000F7FF5F5F7FA65F5F5F9D646464AB646464AC5252
      52884444406B3D6852DE2F2F2E3E00000000404040594A4A4A76000000002A2A
      2A33070707080000000000000000000000002626262DCB7A7AEDD92424FFB29B
      9BD5161616190000000000000000000000000000000000000000000000000000
      0000BF7369EB5A55527E00000000000000004C4C4E871A1AFFFF7C7CFEFF6363
      FBFF4D4DF8FF3A3AF3FF2A2AEDFF1C1CE6FF1111DEFF1A1AD1FF4A4A4B800000
      0000000000000000000000000000000000002727272F7171B8D64D4DFFFF4646
      FFFF3F3FFFFF6262BCD82626272E0000000000000000282828305555BAD70C0C
      FFFF0505FFFF0000FFFF4949B8D72626272E0000000000000000000000000000
      00004C4C4C794141415D00000000000000004646466A4141415D000000000000
      0000000000000000000000000000000000002626262DD18585EFB29A9AD51515
      1518000000000000000000000000000000000000000000000000000000000000
      0000BE6E64EB5A55527E00000000000000004C4C4E871818FFFF4747FEFF3A3A
      F9FF2F2FF4FF2424EFFF1B1BE9FF1313E1FF0C0CDAFF1A1AD2FF4A4A4B800000
      00000000000000000000000000000000000000000000282828307171BAD74D4D
      FFFF6A6ABEDA2A2A2A3300000000000000000000000000000000282828305656
      BCD80C0CFFFF4B4BBCD82626272E000000000000000000000000000000000000
      00005757579D1D1D1D2200000000000000005353538E2525252C000000000000
      0000000000000000000000000000000000002525252CC6BBBBE1131313150000
      00000000000000000000000000000000000000000000000000003D3C3B50A46F
      68D3D0493EFAB56C61E58D6C66BF000000004848497F5454C2FF4949BEFF4949
      BBFF4949B7FF4949B4FF4949B1FF4949AEFF4949ABFF5656ADFF464647790000
      00000000000000000000000000000000000000000000000000002A2A2A337575
      9CC0282828300000000000000000000000000000000000000000000000002A2A
      2A3366669CC02727272F00000000000000000000000000000000000000000000
      0000484848703232324000000000000000000404040500000000000000000000
      000000000000000000000000000000000000010101020A0A0A0B000000000000
      00000000000000000000000000000000000000000000000000002423232A6B5D
      5A956B5D5A956B5D5A955B53517E000000000707070811111113111111131111
      1113111111131111111311111113111111131111111311111113060606070000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A2B2A363E513E90286228CD146C14E8156C15E7295F29CB3E4F3E8C2626
      262F000000000000000000000000000000000000000000000000000000000000
      000000000000000000001E791EDB188218ED136C13EA195419E2060606070000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003E443E6F1670
      16E6166416E62A4D2AC91B1B1B1F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000080808093D51
      3D97058A05F900A200FF00AC00FF00B100FF00AF00FF00A700FF009A00FF087D
      08F63E4F3E8D0404040500000000000000000000000000000000000000000000
      0000000000000000000013A713EA10BC10FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F453F7000BE
      00FF00B000FF009800FF285228CD1C1C1C200000000000000000000000000000
      000000000000000000000000000000000000000000000909090A315C31BB00A2
      00FF00B800FF00BC00FF00BC00FF00BC00FF00BC00FF00BC00FF00BC00FF00AA
      00FF009200FF365736B004040405000000000000000000000000000000000000
      0000000000000000000013A713EA10BC10FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F453F7000C2
      00FF00BE00FF00B000FF009800FF285228CC1B1B1B1F00000000000000000000
      000000000000000000000000000000000000000000003C533C9900A600FF00BC
      00FF00BC00FF00BC00FF00BC00FF63D763FF57D357FF00BC00FF00BC00FF00BC
      00FF00AF00FF009100FF3E4E3E8B000000000000000000000000000000000000
      0000000000000000000013A713EA10BD10FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F453F7000C5
      00FF00C200FF00BE00FF00B000FF009800FF285228CC1B1B1B1F000000000000
      0000000000000000000000000000000000002B2C2B38049604FA00BC00FF00BC
      00FF00BC00FF00BC00FF60D660FFFDFEFDFFFCFEFCFF5AD45AFF00BC00FF00BC
      00FF00BC00FF00A800FF097709F52424242C0000000000000000000000000000
      0000000000000000000013A713EA10BE10FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F463F7000C9
      00FF00C500FF00C200FF00BE00FF00AF00FF009700FF295229CA1717171A0000
      0000000000000000000000000000000000003D543D9500B600FF00BC00FF00BC
      00FF00BC00FF60D660FFFDFEFDFFFFFFFFFFFFFFFFFFFCFEFCFF5AD45AFF00BC
      00FF00BC00FF00BB00FF009500FF3F4E3F870F0F0F110F0F0F110F0F0F110F0F
      0F110F0F0F110F0F0F1113A613EA10BE10FF00A600FF0F770FEE151515180F0F
      0F110F0F0F110F0F0F110F0F0F110F0F0F100000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F463F7000CD
      00FF00C900FF00C500FF00C200FF00BE00FF00AF00FF009700FF2A522AC91616
      161900000000000000000000000000000000247124D300BC00FF00BC00FF00BC
      00FF60D660FFFDFEFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFEFCFF5AD4
      5AFF00BC00FF00BC00FF00A200FF2B5B2BC7145E14E80A7D0AF40A7D0AF40A7D
      0AF40A7D0AF40A7D0AF401C601FD10BE10FF00A600FF009E00FE0A7D0AF40A7D
      0AF40A7D0AF40A7D0AF40A7D0AF41A591AE16C6CB8DE4C4CBCE54C4CBBE54C4C
      BBE54C4CBAE54C4CBAE54C4CB9E54C4CB9E54C4CB7E54C4CB7E54C4CB7E54C4C
      B7E54C4CB7E54C4CB7E54C4CB7E56E6EACD700000000000000003F463F7000D1
      00FF00CD00FF00C900FF00C500FF00C200FF00BE00FF00AF00FF009700FF2D51
      2DC415151518000000000000000000000000108910ED20C320FF00BC00FF63D7
      63FFFEFFFEFFFFFFFFFFA1E6A1FFFFFFFFFFFFFFFFFFA1E6A1FFFFFFFFFFFCFE
      FCFF57D357FF00BC00FF00A900FF1B631BE00F770FEE00A600FF00A600FF00A6
      00FF00A600FF00A600FF00CE00FF10BF10FF00A600FF00A600FF00A600FF00A6
      00FF00A600FF00A600FF00A600FF186D18E43E3EDDEE0000FBFF0000FAFF0000
      F9FF0000F8FF0000F7FF0000F6FF0000F5FF0000F3FF0000F2FF0000F1FF0000
      F0FF0000EFFF0000EEFF0000ECFF4949C0E200000000000000003F463F7000D5
      00FF00D100FF00CD00FF00C900FF00C500FF00C200FF00BE00FF00AF00FF0097
      00FF2C502CC51313131500000000000000000F8E0FEE67D367FF2FC72FFFA1E6
      A1FFFFFFFFFF80DE80FF21C521FFFFFFFFFFFFFFFFFF13C213FF8EE18EFFFFFF
      FFFF93E393FF00BC00FF00AB00FF1A641AE1189618F311C111FF11C211FF11C2
      11FF11C211FF11C311FF11DD11FF1CD31CFF11C411FF11C411FF11C411FF11C5
      11FF11C511FF11C611FF11C511FF1C8C1CEB3E3EDFEE0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFF4949CBE200000000000000003F463F7000D8
      00FF00D500FF00D100FF00CD00FF00C900FF00C500FF00C200FF00BE00FF00AF
      00FF2C562CC5131313150000000000000000237923D455D055FF76D676FF06BE
      06FF4FD14FFF00BC00FF21C521FFFFFFFFFFFFFFFFFF13C213FF00BC00FF51D2
      51FF00BC00FF00BC00FF00A700FF2C5C2CC6208120D81B9A1BE01B9A1BE01B9A
      1BE01B9A1BE01B9A1BE003D203FB07D507FF00C900FF02BD02FC1B991BE01B9A
      1BE01B9A1BE01B9A1BE01B9A1BE0247624D26C6CC4DE4C4CCFE54C4CCFE54C4C
      CFE54C4CCFE54C4CCFE54C4CCFE54C4CCFE54C4CCFE54C4CCFE54C4CCFE54C4C
      CFE54C4CCFE54C4CCFE54C4CCFE56E6EBAD700000000000000003F463F7000DC
      00FF00D800FF00D500FF00D100FF00CD00FF00C900FF00C500FF00C100FF2D64
      2DC4151515180000000000000000000000003D593D961FC31FFF87DA87FF3FCB
      3FFF00BC00FF00BC00FF21C521FFFFFFFFFFFFFFFFFF13C213FF00BC00FF00BC
      00FF00BC00FF00BC00FF009E00FF3F4E3F880000000000000000000000000000
      0000000000000000000013A713EA10BF10FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F463F7000E0
      00FF00DC00FF00D800FF00D400FF00D100FF00CD00FF00C900FF2A692AC91616
      1619000000000000000000000000000000002C2D2C3A04AF04FA5DD15DFF73D6
      73FF2CC72CFF00BC00FF21C521FFFFFFFFFFFFFFFFFF13C213FF00BC00FF00BC
      00FF00BC00FF00B700FF098409F52525252D0000000000000000000000000000
      0000000000000000000013A713EA10C010FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F463F7000E4
      00FF00E000FF00DC00FF00D800FF00D400FF00D100FF296C29CA1717171A0000
      000000000000000000000000000000000000000000003C5B3C9B08BE08FF5CD1
      5CFF61D261FF3CCA3CFF27C727FFCCF2CCFFCCF2CCFF0FC00FFF00BC00FF00BC
      00FF00BC00FF00A200FF3E4F3E8D000000000000000000000000000000000000
      0000000000000000000013A713EA10C010FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F463F7000E7
      00FF00E400FF00E000FF00DC00FF00D800FF286D28CC1A1A1A1E000000000000
      000000000000000000000000000000000000000000000A0A0A0B316E31BC02BD
      02FF2EC62EFF4BCD4BFF3CC93CFF24C424FF0BBF0BFF00BC00FF00BC00FF00BC
      00FF00AB00FF355B35B104040405000000000000000000000000000000000000
      0000000000000000000013A713EA10C110FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F463F7000EB
      00FF00E700FF00E400FF00E000FF287128CC1B1B1B1F00000000000000000000
      00000000000000000000000000000000000000000000000000000A0A0A0B3D5A
      3D9805B005F903BD03FF12C112FF12C012FF09BE09FF00BC00FF00BC00FF079A
      07F73E513E8F0404040500000000000000000000000000000000000000000000
      0000000000000000000013A713EA10BF10FF00A600FF0F770FEE070707080000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003F473F7000EF
      00FF00EB00FF00E700FF287428CD1C1C1C200000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A2B2A363E583E91287C28CD149014E8158B15E7297229CB3E533E8D2727
      2730000000000000000000000000000000000000000000000000000000000000
      00000000000000000000188D18E31C8E1CEF146C14E91A551AE1060606070000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003E453E6F1695
      16E6169216E62A6D2AC91B1B1B1F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000100000000000000000000
      0000000000000000000033323242544E4EA74240406000000000000000000000
      000000000000000000000000000003030304377282DC3E6875CD3F4344690101
      0102000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000006060607676F679F000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A2B2A363E513E90286228CD146C14E8156C15E7295F29CB3E4F3E8C2626
      262F000000000000000000000000000000000303030400000000000000000000
      00013938384E544E4FB24B4444FB454141FF413F3F5F00000000000000000000
      000000000000000000000000000004040405299BB0F247CAF5FF40C8F7FF34A9
      D2FE3087A5F03B626FBD37393A52000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000050505065B6F5BAB527E52D0000000000000
      0000000000000000000000000000000000000000000000000000080808093D51
      3D97058A05F900A200FF00AC00FF00B100FF00AF00FF00A700FF009A00FF087D
      08F63E4F3E8D04040405000000000000000003030304000000003F3E3E5B5B4F
      51CC4A4344FF454141FF454141FF454141FF403E3F5D00000000000000000000
      000000000000000000000000000004040405259EB0ED3EADCBFF4ECEFAFF46CB
      F9FF3EC8F9FF36C6F9FF2DBDF1FF259EC8FB2B7B97E03D5760A22626262F0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000060606075B745BB2007900FF517D51D0000000000000
      000000000000000000000000000000000000000000000909090A315C31BB00A2
      00FF00B800FF00BC00FF00BC00FF00BC00FF00BC00FF00BC00FF00BC00FF00AA
      00FF009200FF365736B0040404050000000000000000000000005D5152AC604C
      4CFF574949FF4F4545FF464141FF454141FF774F56EE785157E4785157E47851
      57E4785157E453494A9C000000000404040525A0B0ED23CFECFF56C4E7FF52D0
      FAFF4ACDF9FF42CAF9FF3AC7F9FF32C4F9FF2AC1F9FF22BFF9FF1AB1E9FF1B90
      BDF2306D84C52C2D2E3A00000000000000000000000000000000000000000000
      000000000000000000005E775EB0008200FF007A00FF517E51D0000000000000
      000000000000000000000000000000000000000000003C533C9900A600FF00BC
      00FF00BC00FF00BC00FF1CC41CFFDDF6DDFFDDF6DDFF10C110FF00BC00FF00BC
      00FF00AF00FF009100FF3E4E3E8B000000000000000000000000605253AC6F52
      52FF664F4FFF5D4B4BFF544747FF4B4444FF396B27FF197910FF197910FF1979
      10FF197910FF505247B8000000000404040526A2B0ED20E0FBFF3FA3B9FF5FD4
      FAFF57D1FAFF4FCFFAFF47CCF9FF3FC9F9FF37C6F9FF2EC3F9FF26C0F9FF1EBD
      F9FF16BAF8FF3A6372AE00000000000000000000000000000000000000000000
      000000000000000000004E914ED9008700FF007C00FF517E51D0000000000000
      0000000000000000000000000000000000002B2C2B38049604FA00BC00FF00BC
      00FF00BC00FF00BC00FF21C521FFFFFFFFFFFFFFFFFF13C213FF00BC00FF00BC
      00FF00BC00FF00A800FF097809F52424242C0000000000000000645455AC8059
      59FF755555FF6C5151FF634D4DFF5A4A4AFF417413FF008000FF008000FF0080
      00FF008000FF4E5444BC000000000404040527A3B0ED22E3FBFF21D5EEFF5EB7
      D2FF64D6FAFF5BD3FAFF88DBEBFF95D8D0FF7DD2BFFF84DACBFF77D9E3FF3AC6
      F9FF23BFF9FF2791B7E600000001000000000000000000000000000000000000
      000000000000000000004E934ED9008D00FF007D00FF517F51D0000000000000
      0000000000000000000000000000000000003D543D9500B600FF00BC00FF00BC
      00FF00BC00FF00BC00FF21C521FFFFFFFFFFFFFFFFFF13C213FF00BC00FF00BC
      00FF00BC00FF00BB00FF009500FF3F4E3F880101010200000000665556AC8E5F
      5FFF855B5BFF7A5757FF725353FF695050FF647B06FF2B8000FF068000FF0080
      00FF008000FF4E5444BC000000000404040528A6B0ED23E6FBFF21E3FBFF3894
      A5FF70D9F9FF68D8FAFF78DBFAFF8DDBE4FF8ED6BBFF2FB937FF12BF12FF6DDF
      88FF6CD6E1FF27C0F9FF2E30303D000000000000000000000000000000000000
      000000000000000000004E964ED9009200FF008300FF518051D0000000000000
      000000000000000000000000000000000000247124D300BC00FF00BC00FF03BD
      03FF5DD55DFF00BC00FF21C521FFFFFFFFFFFFFFFFFF13C213FF00BD00FF5ED5
      5EFF01BD01FF00BC00FF00A200FF2C5D2CC503030304000000006A5758AC9D65
      65FF946161FF9F7A7AFF977676FF775656FF667B06FF6F8000FF458000FF0880
      00FF008000FF4E5444BC000000000404040529A7B0ED25E9FBFF23E5FBFF22D8
      EFFF5998AAFF68C2DCFF62C0DCFF59BDDCFF5ACDF2FF92DEDEFF1EC121FF00CA
      00FF50D75EFF5ED0EFFF45606B96000000000000000000000000000000000000
      000000000000000000004E974ED9009700FF008800FF518151D0000000000000
      000000000000000000000000000000000001108910ED20C320FF00BC00FFA9E8
      A9FFFFFFFFFF8EE18EFF21C521FFFFFFFFFFFFFFFFFF14C214FF9AE49AFFFFFF
      FFFF9CE59CFF00BC00FF00A900FF1B651BE002020203000000006D595AACAC6B
      6BFFA36767FF9F6B6BFF966868FF885C5CFF9B732FFF374C28FF222E57FF292B
      62FF6A4D57FF63544AB900000000040404052BA8B1ED27ECFBFF25E8FBFF23E5
      FBFF24BACEFF25A5B8FF23A2B8FF258697FF447181FF62D5FAFF86DCA7FF00C8
      00FF00CA00FF89DABDFF3CA5C9EA080808090000000000000000000000000000
      000000000000000000004E9A4ED9009D00FF008E00FF518151D0000000000000
      0000000000000000000000000000000000010F8E0FEE67D367FF2FC72FFF57D3
      57FFFCFEFCFFFFFFFFFFADE9ADFFFFFFFFFFFFFFFFFFACE9ACFFFFFFFFFFF9FD
      F9FF4CD04CFF00BC00FF00AB00FF1A661AE10000000000000000705A5BACBA71
      71FFB16D6DFFA86969FFA06666FF976262FFE29239FFFEA527FFE39A25FFE796
      30FFFFA428FF6B554CB600000000040404052BABB1ED28EFFCFF26EBFBFF24E8
      FBFF22E4FBFF20E1FBFF1EDDFBFF1CD9F9FF2D4F56FF5DB2CBFF94E1CAFF00C6
      00FF00CD00FF70D489FF4BCDF9FF393D3E530000000000000000000000000000
      000000000000000000004E9B4ED900A200FF009300FF518251D0000000000000
      000000000000000000000000000000000000237923D455D055FF77D777FF03BD
      03FF54D354FFFBFEFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFEFAFF4ED1
      4EFF00BC00FF00BC00FF00A700FF2C5E2CC60000000000000000745B5CACC977
      77FFC07373FFB77070FFAE6C6CFFA56868FFE4A530FFFDD367FFFCE2A2FFFDD0
      63FFFFBF22FF6B584AB600000000040404052DADB1ED2AF1FCFF28EEFCFF26EA
      FBFF24E7FBFF22E3FBFF20E0FBFF1EDCFAFF239EB4FF3D5157FF92C4B5FF00C5
      00FF00D000FF33C234FF78DAF7FF4C707CAB0000000000000000000000000000
      0000000000004C544C731DB41DF700A700FF009800FF248E24F3474B47680000
      0000000000000000000000000000000000003D593D961FC31FFF87DA87FF3FCA
      3FFF00BC00FF54D354FFFBFEFBFFFFFFFFFFFFFFFFFFFAFEFAFF4ED14EFF00BC
      00FF00BC00FF00BC00FF009E00FF3F4E3F880000000000000000775D5EACD87D
      7DFFCF7979FFC67676FFBD7272FFB46E6EFFEFD587FFFAF6E7FFF9F6E9FFFAF6
      E2FFFDE261FF6B5A49B600000000040404052DADB1ED2CF4FCFF2AF1FCFF28ED
      FCFF26EAFBFF23E6FBFF21E3FBFF1FDFFBFF1DDAF8FF7DD7D3FF93D8B7FF0CC6
      0CFF00D300FF0EBA0EFF7F9882CB74817BB40000000000000000000000000000
      00004A514A6E21C021F400BC00FF00AD00FF009E00FF008E00FF228822F5494E
      496E000000000000000000000000000000002C2D2C3A04AF04FA5DD15DFF73D6
      73FF2CC72CFF00BC00FF54D354FFFBFEFBFFFAFEFAFF4ED14EFF00BC00FF00BC
      00FF00BC00FF00B700FF098409F52525252D01010102000000007B5E5FADE685
      85FFDD8181FFD47C7CFFCB7878FFC37474FFEAD23DFFFCF469FFFDF038FFFFEF
      08FFFFEE06FF6B5D48B600000000040404052DADB1ED2CF5FCFF2BF4FCFF29F0
      FCFF27EDFBFF25E9FBFF23E6FBFF21E2FBFF1FDFFBFF51E0EEFF4FCB62FF00C1
      00FF00D300FF00B800FF45A545EF4647465F0000000000000000000000004C56
      4C741ED61EF700D000FF00C100FF00B200FF00A300FF009400FF008400FF2185
      21F54A4E4A6F000000000000000000000000000000003C5B3C9B08BE08FF5BD1
      5BFF61D261FF3CC93CFF0DC00DFF58D458FF4CD04CFF00BC00FF00BC00FF00BC
      00FF00BC00FF00A300FF3E4F3E8D000000000303030400000000232222296C57
      5999D17C7DF3E38484FFDA8080FFD17A7AFF947749E28E7A40D98E7A40D98E7A
      40D98E7A40D9534D49900000000001010102378083CE2CF5FCFF2CF5FCFF2BF3
      FCFF29EFFCFF27ECFBFF2DA3AEE539767EC33E686EAD405C609B6E8C81C722C2
      23FC00D100FF17BE17FD636C63960000000000000000000000004C574C751EE5
      1EF700E500FF00D600FF00C700FF00B700FF00A800FF009900FF008A00FF007C
      00FF218321F64D524D750000000000000000000000000A0A0A0B316E31BC02BD
      02FF2EC62EFF4BCD4BFF3CC93CFF24C424FF0BBF0BFF00BC00FF00BC00FF00BC
      00FF00AB00FF355B35B104040405000000000202020300000000000000000000
      000028282831715A5BA0D88081F7DE8182FF2E2D2D3A00000000000000000000
      0000000000000000000000000000000000001D1D1D22378386CE26D4DAFF26D4
      DAFF24D2DAFF328B91D82324242B00000000000000000000000000000001738B
      73B807D107FF759075BE030303040000000000000000505D507C1EF11EF800F9
      00FF00EA00FF00DB00FF00CC00FF00BD00FF00AE00FF009E00FF008F00FF0080
      00FF007900FF218021F64E524E760000000000000000000000000A0A0A0B3D5A
      3D9805B005F903BD03FF12C112FF12C012FF09BE09FF00BC00FF00BC00FF079A
      07F73E513E8F0404040500000000000000000000000000000000000000000000
      000000000000000000003130303E7E5F61B32A29293300000000000000000000
      000000000000000000000000000000000000000000000C0C0C0D1D1D1D221D1D
      1D221D1D1D220D0D0D0E00000000000000000000000000000000000000001313
      131593A893CE1818181B00000000000000004F544F705FBD5FD950BE50DA50BE
      50DA50B750DA50B150DA50AA50DA50A450DA509E50DA509750DA509050DA508A
      50DA508650DA508450DA638C63D8484A48650000000000000000000000000000
      00002A2B2A363E583E91287C28CD149014E8158B15E7297229CB3E533E8D2727
      2730000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000F0F0F11434343741818181B378BA8DA4142425F0000
      00000000000003030304373937513D453D682F302F3F00000000000000000000
      0000000000000000000000000000000000005E52527EB58989D9AA6C6CD51A1A
      1A1E524F4F6E776B6B9D766F6F9D7670709D766F6F9D766D6D9D766D6D9D756B
      6B9D756A6A9D7569699D7568689D534F4F71060606074E4E5DE9626283EF6262
      83EF626282EF626281EF626281EF62627FEF62627FEF62627EEF62627DEF6262
      7DEF62627DEF62627FEF4D4D5AE9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000202020265A5A5AB0111111135094B3D727C2FFFF24ADDDFE4344
      44630F0F0F113F473F81385E38AB06A706F801C001FD366636AF0F0F0F100000
      00000000000000000000000000000000000016161619000000008E6767BB6554
      548CC08888DBF94D4DFFF37272FFEE8686FFEA7474FFE76363FFE35353FFE043
      43FFDC3232FFD92222FFD51212FFB36868DD070707085B5BABFF0000F7FF0000
      F2FF0000EDFF0000E8FF0000E3FF0000DEFF0000D9FF0000D3FF0000CEFF0000
      C9FF0000CBFF0000D4FF5C5C9BFF000000010000000000000000000000000000
      0000000000000000000002020203030303040000000000000000000000000000
      000034343445797979CC2525252D000000002728283051B5E6F527B4E5FF25AD
      DCFF4547476900000000000000003D443D6900C700FF00CB00FF3D573D980000
      00000000000000000000000000000000000000000000AB8787D1CD6565F01C1C
      1C20BE8F8FDAF77A7AFDF39797FDEFA5A5FDEC9999FDEA8D8DFDE78181FDE573
      73FDE26767FDDF5B5BFDDC5151FDB67A7ADD070707085B5BABFF0000F8FF1414
      F6FF0F0FF2FF0A0AEDFF0707E8FF0202E3FF0101DDFF0000D7FF0000D1FF0000
      CBFF0000C7FF0000D4FF5C5C9BFF000000010000000000000000000000000000
      000000000000000000002424242C614E40C82929283300000000000000004646
      466CAAAAAAEA3C3C3C540000000000000000000000003132324012AD32FC10A5
      58FF28AEDDFF494B4C743D4E3D953B593BA105B205F900CB00FF0F910FEE3B53
      3BA12C2C2C390000000000000000000000002323232928282830AB6A6AD64642
      425D040404050606060706060607060606070606060706060607060606070606
      060706060607060606070606060704040405070707085B5BABFF0202FAFF2727
      F9FF2020F5FF1717F1FF0F0FECFF0909E7FF0505E1FF0101DBFF0000D4FF0000
      CDFF0000C7FF0000D2FF5C5C9BFF000000010000000000000000000000000000
      000000000000000000002B2B2B37A76D3EFF865E3EEB3C3B3A585151518BD0D0
      D0FA5050507D0000000000000000000000001818181B297329CA00D200FF00D2
      00FF12A558FF28B0E0FF4B545095188C18E400C900FF00CB00FF00AA00FF3155
      28DB3333334500000000000000000000000039373749AE8585D49B6A6AC70D0D
      0D0E000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000070707085B5BABFF0202FBFF3131
      FAFF2828F7FF1F1FF3FF1616EEFF0E0EE9FF0808E4FF0404DEFF0101D7FF0000
      D0FF0000C8FF0000CEFF5C5C9BFF000000010000000000000000000000000000
      0000000000000000000030303040AB6F3FFFAC7040FF9F7350FAB2AAA4FF6464
      64A50000000100000000000000001C1C1C20276027CE00B700FF00D200FF00D0
      00FF00C400FF129650FF3F95ABF7607115FC12B20AFF00C600FF5F9B49FFB8B7
      70FF86861EFF4F4E40C60B0B0B0C000000001A1A1A1E3332323F3332323F1818
      181B020202030303030403030304030303040303030403030304030303040303
      030403030304030303040303030401010102070707085B5BABFF0303FCFF3B3B
      FBFF3232F8FF2727F5FF1D1DF1FF1515ECFF0E0EE7FF0808E0FF0404DAFF0101
      D3FF0000CCFF0000CBFF5C5C9BFF000000010000000000000000000000000000
      0000000000000000000034343347AC7040FFB78458FFCBA788FFC1AE9FFB3A3A
      3A51000000000000000000000000060606070F0F0F112E6D2EC200D200FF02C3
      02FC3B3D3C5759869AE1979937FFDBD8B0FFB5CBA1FF7BB774FFF3F2E8FFF4F4
      EDFFEAE9D7FFA2A247FF4F4D3BD2010101025D54548EE18C8CF8A36969CF524A
      4A70605A5A808A7676B1897B7BB1897D7DB1887B7BB1887878B1887777B18775
      75B1877272B1867171B1866F6FB161595985070707085B5BABFF0505FDFF4747
      FCFF3D3DF9FF3131F6FF2626F2FF1C1CEEFF1414E9FF0E0EE4FF0707DEFF0303
      D7FF0101CFFF0000C9FF5C5C9BFF000000010000000000000000000000000000
      0000000000000000000046434174B8865BFFCCA88BFFE1CBB9FFF0E4DBFF9A94
      90DC1616161900000000000000000000000000000000343634490BB30BF300D2
      00FF2C7A2CC55D7118FAC6C7A2FFDAD8C1FFF0EEDEFFF0EEE1FFF1F0E4FFF3F2
      E9FFF4F4EDFFE8E7D3FF909027FF4343416A000000008C7676B67E6464A90303
      0304C08888DBF94D4DFFF37272FFEE8686FFEA7474FFE76363FFE35353FFE043
      43FFDC3232FFD92222FFD51212FFB36868DD070707085B5BABFF0505FFFF5353
      FDFF4949FAFF3B3BF7FF2F2FF4FF2525F0FF1B1BEBFF1414E6FF0D0DE1FF0707
      DAFF0303D3FF0000C8FF5C5C9AFF000000010000000000000000000000000000
      000008080809504A4591A87B57F9CEAA8EFFE2CDBCFFF0E4DBFFF0E4DBFFF0E4
      DBFF5E5C5A9A000000000000000000000000000000000000000030313040376A
      37AE189014E829A718FF82AD6FFFF1EBD5FFF0EDDBFFF0EEDEFFF1EFE2FFF1F0
      E5FFF3F3EAFFF3F2EAFFBBBB76FF4F4C35DA0000000000000000B46B6BDD3736
      3646B38B8BD2EF8585F9EB9B9BF9E8A7A7F9E69D9DF9E39292F9E18989F9DF7D
      7DF9DC7171F9DB6969F9D75E5EF9AB7878D4070707085B5BABFF0707FFFF6161
      FEFF5656FCFF4747F9FF3A3AF5FF2E2EF2FF2424EDFF1A1AE8FF1313E3FF0C0C
      DDFF0707D7FF0000CAFF5C5C99FF000000012828283242414164131313152C2C
      2C396A5341CBBA8A60FFCFAC90FFCAB9AAF84C4C4B7B44444466454444664C4B
      4A744B4A49770404040500000000000000000000000000000000000000000000
      000051503AB1D1CD9CFFF3EBD3FFF0EAD0FFF1EDD9FFF0EDDCFFF1EEDFFFF2F0
      E2FFF2F1E6FFF2F1E8FFCFCE9FFF5A581CF847444462A48181CCCA6262EF3C3A
      3A4E040404050606060706060607060606070606060706060607060606070606
      060706060607060606070606060704040405070707085B5BABFF0808FFFF7070
      FEFF6464FCFF5454FAFF4646F7FF3939F4FF2D2DF0FF2323EBFF1A1AE6FF1212
      E0FF0C0CDAFF0000CCFF5C5C98FF00000001736356D7AE8867FF897160E76452
      43DEBB8C63FFD0AE93FFE1CFBFFF535252880000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000525237BAD7D2A5FFF5ECD5FFF2EBD2FFF0E9D1FFF1ECDAFFF1EEDDFFF1EE
      DFFFF1F0E3FFEDEBDAFFD4D2A8FF5E5B18FA000000003E3D3D50363535440000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000070707085B5BABFF0909FFFF8080
      FEFF7171FDFF6060FBFF5252F8FF4444F5FF3737F2FF2B2BEDFF2121E9FF1818
      E3FF1111DDFF0101CDFF5C5C97FF00000001756253DA996740FFA67A57FFAB8F
      76F97B6C60E1E6D3C4FF696665AF010101020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004B4B408EC9C48AFFF7EED8FFF5EDD5FFF2EBD2FFF0EAD0FFF0EBD6FFF1EE
      DDFFF0EDDBFFE6E4C4FFC9C690FF53502AE6000000002F2F2F3A695656910000
      0000020202030303030403030304030303040303030403030304030303040303
      030403030304030303040303030402020203070707085B5BABFF0505FFFF5858
      FEFF5050FCFF4444F9FF3A3AF6FF3131F2FF2828EEFF2121EAFF1A1AE5FF1313
      E0FF0E0EDBFF0101CEFF5C5C97FF000000015954509BAE8765FFAD8664FFBD9D
      83FFA29285EE77736FD412121214000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000021212127A1A040FFF7EED8FFF7EED7FFF4ECD4FFF2EAD1FFEFE9CEFFECE7
      CBFFEAE5C7FFE7E3C5FFAAA953FF4D4C4691000000004745455E9D5656CD0101
      01026B61618F987878BE987F7FBF978383BF978080BF957C7CBF957A7ABF9576
      76BF947373BF936F6FBF916C6CBE6A5C5C91070707085B5BABFF0000FFFF0101
      FFFF0101FBFF0101F6FF0101F0FF0000EBFF0000E6FF0000E1FF0000DCFF0000
      D7FF0000D2FF0000CDFF5C5C97FF0000000120202026907F72E3C4A78FFFCBB1
      9CFFDAC7B6FF5757568F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000525039B2C3BF81FFF8EFDAFFF7EED7FFF4ECD4FFF2EBD1FFEFE9
      CEFFEDE7CAFFC6C58CFF5A581DF106060607000000004745455E9D5656CD0303
      0304C08888DBF94D4DFFF37272FFEE8686FFEA7474FFE76363FFE35353FFE043
      43FFDC3232FFD92222FFD51212FFB36868DD060606074E4E5DE9626285EF6262
      85EF626284EF626283EF626283EF626282EF626281EF626281EF626280EF6262
      7FEF62627FEF62627DEF4D4D59E900000000000000003938384EAA9E94ECE0CF
      C1FFEADCD2FF706D6BB100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000606060756542DCBB3B062FFE3DCB9FFF6EDD7FFF3ECD4FFE2DD
      BAFFB7B56CFF615F1AF321212128000000002C2B2B35A58080CD9D5656CD0303
      0304A48484C7E48A8AF3E09D9DF3DFA8A8F3DD9F9FF3DB9696F3D88D8DF3D785
      85F3D47B7BF3D37171F3D06969F39F7474CA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002D2D2D3A6E6C
      6AB199938FE14C4C4C7605050506050505060505050605050506050505060505
      0506050505060505050600000000000000000000000000000000000000000000
      000000000000000000000101010244443F6E5F5E22E18C8B26FC8F8F28FD6563
      1BEE4E4C41A0080808090000000000000000000000007B6A6AA5815B5BAF0000
      0000040404050505050605050506050505060505050605050506050505060505
      0506050505060505050605050506030303040000000000000000000000000000
      00002A2B2A363E503E91286228CD146C14E8156C15E7295F29CB3E4F3E8C2626
      262F000000000000000000000000000000000000000000000000000000000000
      000000000000000000001F1F1F254D5360A969738BE6494E558D1C1C1C210000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000333232454948488501010102000000000000
      0000000000000000000000000000000000000000000059565296B2A48AF1B2A2
      88F1B2A185F1B2A083F1B1A081F1B19E7EF1B09E7CF1B09D79F1B09C76F1B09B
      75F1B09A71F1B09A71F156534D910000000000000000000000000909090A3D51
      3D97058A05F900A200FF00AC00FF00B100FF00AF00FF00A700FF009A00FF087D
      08F63E4F3E8D0404040500000000000000000000000000000000000000000000
      0000161616194A5059978A8C9EF9D0BCBDFFB7B4BFFFDED1D1FF7685A7FE4F5F
      82E13F4245670000000100000000000000000000000000000000000000000000
      000000000000000000002F2E2E3D743741EBC5C1C2FF8D8D8DF24A4A4A820202
      020300000000000000000000000000000000000000005C5A5692F8E4BFFFF8E2
      BCFFF7E1B9FFF7DFB5FFF7DEB2FFF6DCAEFFF6DBABFFF6D9A8FFF5D8A4FFF5D6
      A1FFF4D59DFFF4D39AFF59564F8E00000000000000000A0A0A0B315D31BC00A2
      00FF00B800FF00BC00FF00BC00FF00BC00FF00BC00FF00BC00FF00BC00FF00AA
      00FF009200FF365736B004040405000000000000000000000000141414174B50
      599683869BF8CDBBBCFFD2BEBEFFD2BEBEFFB8B6C2FFE4D8D8FF7D93BBFF356F
      C8FF5072AEFF6F6D7FEA494C538A050505060000000000000000000000000000
      0000000000002F2E2E3D793E47E7923745FFD2CECFFFF2F2F2FFE6E6E6FF8D8D
      8DF14A4A4A82010101020000000000000000000000005C5B5792F9E5C3FFF8E4
      C0FFF8E2BDFFF7E1B9FFF7DFB6FFF7DEB3FFF6DDAFFFF6DBACFFF6DAA8FFF5D8
      A5FFF5D7A2FFF5D59EFF59564F8E00000000000000003C533C9A00A600FF00BC
      00FF00BC00FF00BC00FF03BD03FFABE9ABFF57D357FF00BC00FF00BC00FF00BC
      00FF00AF00FF009100FF3E4E3E8B000000000F0F0F10484B52857D8095F4CCBA
      BCFFD2BEBEFFD2BEBEFFD2BEBEFFD2BEBEFFB9B8C3FFE8DDDDFF8296BFFF356F
      C8FF5176B6FFBDA0A0FF6A6266B5000000000000000000000000000000000000
      00002F2E2E3D7D434AE79C414DFF943B48FFD2CFCFFFF2F2F2FFF2F2F2FFF2F2
      F2FFE6E6E6FF8D8D8DF14848487A00000001000000005C5B5792F9E7C8FFF9E6
      C4FFF8E4C1FFCDD69DFFD2D89EFF69C94DFF67C64AFFB4CD81FFF6DBADFFF6DA
      A9FFF5D9A6FFF5D7A2FF5956508E000000002C2D2C3A049604FA00BC00FF00BC
      00FF00BC00FF00BC00FF5FD55FFFFFFFFFFFFCFEFCFF57D357FF00BC00FF00BC
      00FF00BC00FF00A800FF097709F52424242C595C63A6CBB9BAFED2BEBEFFD2BE
      BEFFD2BEBEFFD2BEBEFFD3BDBDFFD6ABABFFAEACB5FFECE3E3FF889BC0FF356F
      C8FF4F76B7FFC1A5A5FF655E5E9B000000000000000000000000000000002F2E
      2E3D81494FE7A14953FF9E4550FF973F4BFFC1BEBEFFF2F2F2FFF2F2F2FFF2F2
      F2FFF2F2F2FFF2F2F2FFE4E4E4FF7A7A7AE7000000005D5B5792F9E9CCFFF9E7
      C8FFF9E6C5FF32C527FF2BD121FF00D800FF00CB00FF08BB05FF74B952FFF6DC
      ADFFF6DAAAFFF5D9A7FF5956508E000000003D543D9600B600FF00BC00FF00BC
      00FF00BC00FF00BC00FF00BC00FF87DF87FFFFFFFFFFFCFEFCFF5AD45AFF00BC
      00FF00BC00FF00BB00FF009500FF3F4E3F874E4C4C6CD2BEBEFFD2BEBEFFD2BE
      BEFFD3BDBDFFD6A7A7FFE59697FFF09999FFB08C93FFECE6E6FF91A1C3FF376E
      C5FF4F76B8FFC5ABABFF6B6363A3000000000000000000000000302F2F3F854E
      53E7A65159FFA34D56FFA04852FF68484CD57A5F61DA757273E3CACACAFFF2F2
      F2FFF2F2F2FFF2F2F2FFF2F2F2FFC4C4C4FD000000005D5B5892FAEBD0FFFAE9
      CDFFF9E8C9FF30D626FF00E200FF0BD107FFA1CC78FFCED49AFF5EB444FFA3BC
      74FFF6DCAEFFF6DBABFF5957518E00000000237123D400BC00FF00BC00FF1BC4
      1BFF20C520FF20C520FF20C520FF20C520FFACE9ACFFFFFFFFFFFCFEFCFF57D3
      57FF00BC00FF00BC00FF00A200FF2C5D2CC54C4A4A69D2BEBEFFD3BCBCFFCFA3
      A3F5DE9595F8F09999FFF09999FFF09999FFE79596FDAFA4A4EEEAE0E0FFBEBA
      C4FF9197B2FFC9B1B1FF706868AA0000000000000000302F2F3E8A5456E7AB59
      5FFFA8555CFFA15159FE6F4F52D3E09EA2FFDE959BFFD88B91FF8D6165E66D67
      67E2B8B8B8FDF2F2F2FFF2F2F2FFC4C4C4FD000000005D5B5892FAEDD4FFFAEB
      D1FFFAEACEFF40D534FF00E200FF10CE0CFFA2CD7CFFF8E2BDFFF7E1B9FFDED6
      A3FFF7DEB2FFF6DDAFFF5957528E000000000F8B0FEE20C320FF00BC00FFDDF6
      DDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFE
      FCFF5AD45AFF00BC00FF00A900FF1B651BDF494747649F7F7FCEDA9191F3F099
      99FFF09999FFF09999FFF09999FFF09999FFF09999FFB18080E3DED6D6FFE3D6
      D6FFD8C6C6FFCEB7B7FF6D6463B44F4C4B8400000000645051B0B06265FFAE5D
      62FF9F565BFB75595AD3EBAEB1FFE6A4A8FFC292A8FF6F75BCFFD5858CFFCF79
      83FFA36268F26C6061DE9E9D9DFAC1C1C1FD000000005D5B5992FBEED8FFFAED
      D5FFFAEBD2FFA8D88AFF9ED382FFBAD495FFEAE0B8FFF8E4C1FFF8E3BDFFF8E1
      BAFFF7E0B7FFF7DEB3FF5957528E000000000E8F0EEF67D267FF2FC72FFFDDF6
      DDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9FD
      F9FF4ED14EFF00BC00FF00AB00FF1B661BE07F6565AAF09999FFF09999FFF099
      99FFF09999FFF09999FFF09999FFF09999FFF09999FFF09999FF988181DDE5DA
      DAFFDCCCCCFFC0AFAEF56A5855E53030303E00000000655151B0B36668FFA05B
      5FF9826566D9F3BCBEFFEEB2B5FFC39CB5FF396AD0FFB088A9FFD78990FFD17D
      86FFCC737BFFC66972FFAC5A62F85C5152CD000000005D5C5992FBF0DDFFFBEF
      D9FFE6E5C4FFFAECD3FFFAEACFFF9ACC7DFF51CA41FF4EC23DFF70BC57FFF8E3
      BEFFF8E2BBFFF7E0B7FF5957528E00000000237923D456D056FF76D676FF12C1
      12FF0FC00FFF0FC00FFF0FC00FFF11C111FFA9E8A9FFFFFFFFFFF9FDF9FF4CD0
      4CFF00BC00FF00BC00FF00A700FF2C5D2CC513131315C58686E6F09999FFF099
      99FFF09999FFEF9A9AFFE5A9A9FFE89797FFF09999FFF09999FFD28D8DF2B8AF
      AFF4E0D2D2FF6C6362BF5A5351AA0000000000000000655152B09B6061F28B71
      71DDFBCBCBFFF6C0C2FFC3A5C2FF346AD4FF6479C9FF5A72C7FFCF8C98FFD483
      8AFFCE7781FFC86D76FF85565AD322212128000000005D5C5A92FCF2E1FFFBF1
      DDFF88D274FF92D67BFFFAECD3FFD2DFAFFF11C30DFF00C400FF49B639FFF8E5
      C2FFF8E3BFFFF8E2BCFF5957538E000000003D593D9620C320FF87DA87FF3FCB
      3FFF00BC00FF00BC00FF00BC00FF95E395FFFFFFFFFFF9FDF9FF4ED14EFF00BC
      00FF00BC00FF00BC00FF009E00FF3F4E3F880000000033323240EA9696FCF099
      99FFF09999FFED9C9CFFD4C1C1FFBB7B7BFFF09999FFF09999FFF09999FFA27C
      7CDEA9A09FE46B5955F01F1F1F2400000000000000005E4E4EB4997E7EE1FFD1
      D1FFFECFCFFFC9B2CDFF2365DAFFC6A2BCFFE7A6AAFF7C80BDFF8A7BB3FFD688
      8EFFD07B85FF815B5EC71818181B00000000000000005D5C5B92FCF4E5FFFCF2
      E2FFF2EDD6FF35D22EFF33D12BFF1BCE17FF00CE00FF00C400FF52B742FFF9E7
      C7FFF9E5C3FFF8E4C0FF5A57538E000000002C2D2C3A04AF04FA5DD15DFF73D6
      73FF2CC72CFF00BC00FF60D660FFFFFFFFFFF9FDF9FF4CD04CFF00BC00FF00BC
      00FF00BC00FF00B700FF098409F52525252D000000000000000063565688F099
      99FFF09999FFEE9C9CFFD4C1C1FF9B6767FFCF8888FFF09999FFF09999FFED97
      97FE766261CB504C4C87000000000000000000000000484947778C8179D4F2C7
      C7FCFFD1D1FFF2C5CAFF8A95D2FF858DCBFFA593BFFF3669D1FFBF8EA6FFD88C
      92FF775C5FBA111111130000000000000000000000005D5C5B92FDF6E9FFFCF4
      E6FFFCF3E2FFE3E8C9FF56D04BFF0FCB0DFF1DC219FF81C66BFF62B851FFF9E8
      CBFFF9E7C7FFF9E6C4FF5A58548E00000000000000003C5C3C9C08BE08FF5CD1
      5CFF61D261FF3CCA3CFF10C110FF9DE59DFF4CD04CFF00BC00FF00BC00FF00BC
      00FF00BC00FF00A200FF3E4F3E8D00000000000000000000000004040405A075
      75C9F09999FFEF9C9CFFD4C1C1FF946363FF846363FFEE9898FFF09999FFDD90
      90F4725F5E9F0D0D0D0E00000000000000002424242C1A6A1AE1008000FF2F6B
      29E8767668CFD3AFAEF2F7C3C4FFA39BC8FF7C87C7FFC79AB1FFE09A9FFF715D
      5FAF0A0A0A0B000000000000000000000000000000005D5C5B92FDF7EDFFFDF6
      EAFFFCF5E7FFFCF3E3FFFCF2E0FFF4EDD7FFF7EDD6FFFBEDD6FFDFD3B5FFB990
      64FFB99064FFB98F66FE504E4B7E00000000000000000A0A0A0B306F30BD02BD
      02FF2EC62EFF4BCD4BFF3CC93CFF24C424FF0BBF0BFF00BC00FF00BC00FF00BC
      00FF00AB00FF355B35B104040405000000000000000000000000000000001E1D
      1D22D78F8FF1EF9B9BFFD4C1C1FFA06969FF7F6262EE685A5A9B363434440606
      0607000000000000000000000000010101021F611FDA008000FF008000FF0080
      00FF007D00FF4249427958535382B49294E3EEB3B5FFE7A7ABFF685B5CA00606
      060700000000000000000000000000000000000000005D5D5C92FEF9F2FFFDF8
      EEFFFDF6EBFFFCF5E7FFFCF3E4FFFCF2E1FFFBF0DDFFFBEFDAFFDFCDB3FFE3A1
      5EFFE1A05EFF6F6356BA000000000000000000000000000000000A0A0A0B3C5B
      3C9904AF04FA03BD03FF12C112FF12C012FF09BE09FF00BC00FF00BC00FF079A
      07F73E513E8F0404040500000000000000000000000000000000000000000000
      0000413E3E55907474BCB69595DE946262C13D3A3A5000000000000000000000
      000000000000000000000000000000000000385238AB226422D6008000FF007C
      00FE3F4D3F8A00000001000000000C0C0C0D4B48486D56505080000000010000
      000000000000000000000000000000000000000000005D5D5C92FEFBF6FFFEFA
      F2FFFDF8EFFFFDF7ECFFFDF5E8FFFCF4E5FFFCF2E2FFFBF1DEFFDFCEB7FFE09F
      5EFF6E6356B60000000000000000000000000000000000000000000000000000
      00002B2C2B373E583E92287C28CD149014E8158B15E7297229CB3E533E8D2727
      2730000000000000000000000000000000000000000000000000000000000000
      0000000000001E1E1E235849499E5C4848A83331314000000000000000000000
      00000000000000000000000000000000000000000000212121283A553AA43F49
      3F78000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A5A5996B6B5B3F1B6B3
      B2F1B6B3B0F1B5B2ADF1B5B2AAF1B5B1A8F1B4B0A6F1B4B0A4F1ACA191F36B62
      56B000000000000000000000000000000000000000001C1C1C20D0C0AFEDCBB9
      A5EBCAB7A3EBCAB7A2EBCAB69EEBC9B39CEBC8B39AEBC7B198EBC7AF96EBC6AE
      93EBC5AC91EBC7AE94EC29292931000000000000000000000000000000000000
      00002E2E2E396B6662929D9182CABBA691E2B8A48DE19A8B7CC868635D902929
      2831000000000000000000000000000000000000000000000000000000000000
      000000000000020202032B2B2B354544446F4F4D4AB0484441DA615B56DA504E
      4BCA1B1B1B1F0000000000000000000000000000000000000000000000000000
      00002A2B2A363E513E90286228CD146C14E8156C15E7295F29CB3E4F3E8C2626
      262F00000000000000000000000000000000000000001E1E1E22ECD4B9FFE9CD
      AFFFE8CBABFFE7C9A7FFE6C6A4FFE5C4A0FFE4C29CFFE3C099FFE2BD95FFE1BB
      91FFE0B98EFFE1B990FF2B2B2B3400000000000000000000000007070708716D
      6999DBC2A7F6E6C7A5FFBF9E7BFFB38964FFB38761FFBF9A73FFDFB88CFFCFAE
      8BF468635E9004040405000000000000000000000000000000013D3D3C584F4D
      4AAA5C5853D2655D55E3716251EFAA8D6CFFB99E80FF515942FF6A7557FF9582
      69FD4746467A0000000000000000000000000000000000000000080808093D51
      3D97058A05F900A200FF00AC00FF00B100FF00AF00FF00A700FF009A00FF087D
      08F63E4F3E8D040404050000000000000000000000001E1E1E22EED7BDFFA290
      7AFF8C7A68FF8C7865FF8B7763FF8A7561FF89745FFF89735CFF88715AFF8870
      58FFB19471FFE1BC94FF2B2B2B3400000000000000000909090A918A83BBEBD1
      B5FFBA9575FFA25C34FFBB5724FFDEA587FFC76737FFB44B18FF9E562DFFB489
      61FFDEB588FF857B71B304040405000000000000000054524FBA8E7860F7CCA9
      82FFDCB181FFDCB181FFDCB282FFD6B086FFB39676FF497144FF0E7A0EFF7C91
      69FD41414163000000000000000000000000000000000909090A315C31BB00A2
      00FF00B800FF00BC00FF00BC00FF00BC00FF00BC00FF00BC00FF00BC00FF00AA
      00FF009200FF365736B00404040500000000000000001E1E1E22EFDAC2FFECD3
      B8FFEBD1B5FFEACFB1FFE9CCADFFE8CAAAFFE7C8A6FFE6C6A2FFE5C39FFFE3C1
      9BFFE2BF97FFE2BF98FF2B2B2B34000000000000000074706E9BEED8C0FFAE83
      65FFBD5725FFC45923FFC96736FFFDFBFAFFD68F69FFBF511BFFBE4F19FFB24C
      18FFA8754FFFDEB689FF68635E8F0000000000000000514E4BD3AF9675FDA088
      6BFF7B6956FF796C5DFF6A8E5DFF4A853EFF548D43FF47923AFF007600FF107D
      0FFE474C476D000000000000000000000000000000003C533C9900A600FF00BC
      00FF00BC00FF00BC00FF00BC00FF64D764FFA1E6A1FF01BD01FF00BC00FF00BC
      00FF00AF00FF009100FF3E4E3E8B00000000000000001E1E1E22F0DCC6FFA494
      82FF8E7D6EFF8D7C6BFF8C7A69FF8C7966FF8B7865FF8B7762FF8A7560FF8974
      5EFFB49979FFE4C29DFF2B2B2B34000000003030303CE5D6C5F8BE9F87FFC05B
      2AFFC85D28FFC65C26FFC55B26FFC86331FFC45923FFC1541EFFC0521CFFBF51
      1AFFB24C1AFFB78B64FFCEAF8CF2282828300000000022222228464545765551
      4BEDD4B592FFE2BD94FF63B84FFF009F00FF009E00FF009E00FF009D00FF009C
      00FF129C12F9484F487000000000000000002B2C2B38049604FA00BC00FF00BC
      00FF00BC00FF00BC00FF64D764FFFEFFFEFFFFFFFFFF50D250FF00BC00FF00BC
      00FF00BC00FF00A800FF097709F52424242C0B0B0B0C2A292933E8CEBBFFE6C8
      B3FFE5C6AFFFE4C4ACFFE3C2A9FFE3C0A5FFE1BEA2FFE0BC9FFFDFB99BFFDEB7
      98FFDDB694FFDDB695FF353434440A0A0A0B706E6C96F2E2D1FFAA6844FFCB62
      2EFFCA612CFFC95F2AFFC75D28FFF7E8E1FFD3845AFFC45721FFC2561FFFC154
      1DFFC0521BFF9F5930FFE0BB91FF645F5B8A00000000000000000C0C0C0D615A
      54E6E4C29CFFE4C39EFF64CE52FF00C400FF00C400FF00C300FF00C200FF00C2
      00FF00C100FF4F864FBC01010102000000003D543D9500B600FF00BC00FF00BC
      00FF00BC00FF60D660FFFDFEFDFFFFFFFFFF88E088FF00BC00FF00BC00FF00BC
      00FF00BC00FF00BB00FF009500FF3F4E3F87644440A4A12918FFB14D3FFFA42C
      1BFFA42B19FFA52C1AFFA62E1BFFA72F1BFFA8301CFFA9311DFFAA321EFFAA33
      1EFFAB351FFFB85543FFAF3C26FF62484499AAA5A0CECBB8A8FFC96532FFCD65
      31FFCC642FFFCB622DFFC9602BFFF3DED3FFE2AD93FFC65A25FFC45923FFC357
      21FFC2551FFFB6501DFFC4A07CFF96897BC300000000000000002D2D2D39766B
      5FEFE7C8A6FFE7C9A7FF6CD95CFF0FD50FFF0FD50FFF08D408FF00D200FF00D2
      00FF489048C3121212140000000000000000247124D300BC00FF00BC00FF00BC
      00FF63D763FFFEFFFEFFFFFFFFFFA0E6A0FF20C520FF20C520FF20C520FF20C5
      20FF19C319FF00BC00FF00A200FF2C5D2CC572564AA4DB8662FFF8F7F6FFDE8D
      6AFFD76C40FFD86D41FFD96E42FFDA6F42FFDB7043FFDC7244FFDD7344FFDE74
      45FFE18052FFF8F5F3FFEAAA8CFF6D574D99CEC8C2E7C1A593FFD16A37FFCF68
      35FFCE6733FFCD6531FFCC632EFFDE9F7CFFFAF1ECFFD88E67FFC65C26FFC55A
      24FFC45822FFC35620FFB38C69FFB1A08EDB000000000000000041404063A796
      84FEEACEB0FFEACFB1FFE2D5B2FFE0D6B4FFE0D7B4FF9CDC86FF00D200FF4B91
      4BC411111113000000000000000000000001108910ED20C320FF00BC00FF60D6
      60FFFDFEFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFCEF2CEFF00BC00FF00A900FF1B651BDF7A4E46A4F14C2DFFF24E31FFF248
      2BFFF2472AFFF34529FFF34428FFF34227FFF34026FFF43F25FFF43D24FFF43C
      23FFF43A22FFF53D26FFF53720FF72484499D0CBC6E7C4A898FFD36D3AFFD26B
      38FFD06A36FFCF6834FFCE6632FFCC6430FFE3AC91FFF8EAE3FFD17649FFC75D
      28FFC65B26FFC55923FFB6906EFFB4A391DC00000000000000004C4C4B94C5B3
      9FFFECD4BAFFEDD5BBFFEDD6BCFFEED7BEFFEED7BFFFAFDE96FF4FB24DF71212
      1214000000000000000000000000000000010F8E0FEE67D367FF2FC72FFF54D3
      54FFFBFEFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFCEF2CEFF00BC00FF00AB00FF1B661BE00F0F0F102E2D2D39F8D2C6FFF5CE
      BFFFF4CCBDFFF3CAB9FFF2C7B5FFF2C6B3FFF1C4AFFFF0C2ACFFEFBFA9FFEEBD
      A5FFEDBCA2FFEDBBA2FF393737490E0E0E0FADAAA8CED0C4B9FFD37D4DFFD46E
      3BFFD26D39FFD26F3CFFD06935FFCF6733FFCD6531FFEFCEBDFFE8BAA2FFC960
      2BFFC85E29FFBB5826FFC7AA8BFF988D82C400000000000000004F4E4DB2DDCB
      B8FFEFDAC3FFF0DBC5FFF0DCC6FFF0DDC7FFF1DDC9FFF1DECAFF5F5D5CD90000
      000000000000000000000000000000000000237923D455D055FF76D676FF03BD
      03FF57D357FFFCFEFCFFFFFFFFFF9DE59DFF0FC00FFF0FC00FFF0FC00FFF0FC0
      0FFF0CC00CFF00BC00FF00A700FF2C5D2CC5000000001E1E1E22F8EDE1FFAAA0
      96FF938A81FF92897DFF92877AFF918678FF908576FF8F8374FF8F8271FF8E80
      6FFFBAA792FFECD3B8FF2B2B2B340000000073727298FBF5F0FFBE9977FFE094
      60FFE3A17CFFF8E9E2FFD36F3EFFD16A37FFCF6835FFEABEA8FFECC5B1FFCC63
      2EFFCA612CFFA86541FFE9CEB0FF66625F8B0000000000000000535150C7F2E0
      CEFFF2E0CDFFF2E1CEFFF3E2D0FFF3E3D1FFF3E3D2FFF4E4D4FF5C5B59D22424
      242C3F3F3F624444447014141417000000003D593D961FC31FFF87DA87FF3FCB
      3FFF00BC00FF54D354FFFBFEFBFFFFFFFFFF8FE28FFF00BC00FF00BC00FF00BC
      00FF00BC00FF00BC00FF009E00FF3F4E3F88000000001E1E1E22F9EFE5FFF7EB
      DFFFF6E9DBFFF5E7D8FFF4E4D4FFF3E2D0FFF2E0CDFFF1DEC9FFF0DBC5FFEFD9
      C2FFEED7BEFFEED6BDFF2B2B2B34000000003131313DF0ECE9F8D0C7BAFFE2B7
      88FFE5A672FFF7E3D6FFF0CDBBFFDE936AFFE4AA8CFFFAF1ECFFDF9D79FFCE66
      32FFC06031FFC0A086FFD6C2ADF22828283000000000000000004D4C4BCEF4E5
      D6FFF5E6D7FFF5E7D8FFF6E8D9FFF6E9DBFFF6E9DCFFF7EADDFF4D4D4CE02F2F
      2FE1454545743D3D3D5A484848B3000000002C2D2C3A04AF04FA5DD15DFF73D6
      73FF2CC72CFF00BC00FF58D458FFFCFEFCFFFFFFFFFF51D251FF00BC00FF00BC
      00FF00BC00FF00B700FF098409F52525252D000000001E1E1E22FAF2EAFFE1D7
      CEFFD7CDC2FFD5CBC0FFD4C8BCFFD3C6B9FFD3C5B5FFD2C3B3FFD1C0AFFFD0BF
      ACFFDFCBB6FFEFD9C2FF2B2B2B3400000000000000007676769CFFFBF9FFC5BE
      ACFFE8C699FFEEBF94FFF4D9C6FFF8E5DAFFF4DACDFFE4A98AFFD16B37FFC465
      35FFB58F73FFEFD9C1FF6A67648F0000000000000000000000004E4E4EC2F5E9
      DEFFF8ECE0FFF8EDE2FFF8EEE3FFF9EFE4FFF9EFE6FFF9F0E7FF5F5E5DD74949
      49A94E4E4EBC4B4B4BAD4D4D4DB500000000000000003C5B3C9B08BE08FF5CD1
      5CFF61D261FF3CCA3CFF0DC00DFF58D458FF92E292FF00BC00FF00BC00FF00BC
      00FF00BC00FF00A200FF3E4F3E8D00000000000000001E1E1E22FBF5EFFFC5BE
      B8FFB7AFA8FFB6AEA5FFB5ACA3FFB4AAA0FFB4A99DFFB3A79BFFB2A598FFB1A4
      95FFCDBDABFFF0DCC7FF2B2B2B3400000000000000000909090A969595BBFFFC
      FAFFD0CABFFFC7B89AFFE7BF90FFE8AD7AFFDF9460FFCD7343FFAF714FFFC5AA
      96FFF2E0CCFF8B8781B4040404050000000000000000000000004A4A4A92DBD4
      CEFEFAF2EAFFFBF3EBFFFBF4EDFFFCF5EEFFFCF5EFFFFCF6F1FF7C7B79E74F4F
      4F954E4E4EC14D4D4DB54A4A4A8B00000000000000000A0A0A0B316E31BC02BD
      02FF2EC62EFF4BCD4BFF3CC93CFF24C424FF0BBF0BFF00BC00FF00BC00FF00BC
      00FF00AB00FF355B35B10404040500000000000000001E1E1E22FCF8F2FFFBF4
      EEFFFAF2EAFFF9F0E6FFF8EDE3FFF7EBDFFFF6E9DBFFF5E7D7FFF4E4D4FFF3E2
      D0FFF2E0CCFFF2DFCCFF2B2B2B340000000000000000000000000909090A7675
      759BEFECEAF7FEF9F6FFD4CFC7FFCDC2B3FFCABCACFFD3C5B9FFF7EADDFFE0D6
      CBF46C6B6892050505060000000000000000000000000000000032323242817F
      7EEAFDF8F4FFFEF9F5FFFCF8F4FFDBD9D7FFB1AFAEFF838281F6747373E84B4B
      4ACA464646873A3A3A52000000010000000000000000000000000A0A0A0B3D5A
      3D9805B005F903BD03FF12C112FF12C012FF09BE09FF00BC00FF00BC00FF079A
      07F73E513E8F040404050000000000000000000000001D1D1D21DAD8D5ECD6D4
      D0EAD5D2CEEAD5D0CCEAD5CFCAEAD4CDC8EAD4CCC5EAD3CBC2EAD2CAC1EAD1C9
      BFEAD0C7BCEAD2C8BEEB29292931000000000000000000000000000000000000
      00002F2F2F3A6E6E6E93A8A8A6CBC9C6C3E2C6C2BEE0A4A19EC96C6A69902A2A
      2A33000000000000000000000000000000000000000000000000000000015858
      58C1787777F4737373E3626262CE5555559E4040405C15151517000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A2B2A363E583E91287C28CD149014E8158B15E7297229CB3E533E8D2727
      27300000000000000000000000000000000000000000050505066F6F6F9D7B7B
      7BB17B7B7BB17B7B7BB17B7B7BB17B7B7BB17B7B7BB17B7B7BB17B7B7BB17B7B
      7BB17B7B7BB16C6C6C9A05050506000000000000000000000000000000000000
      0000000000000000000000000000635F5F865E5B5B7E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000070707080000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000D0D0D0E393939FE0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF3F3F3FFE0D0D0D0E000000000000000000000000000000000000
      0000000000000000000049484860E38585FBEAA2A2FB4645455C000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000058504BA93C3A3A5400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000F0F0F113F42476B222222290000
      000000000000000000000000000000000000000000000D0D0D0E383838FE8484
      84FA7F7F7FB77C7C7CB57C7C7CB5828282BB808080B87C7C7CB57C7C7CB57F7F
      7FB77E7E7EFA3F3F3FFE0D0D0D0E000000000000000000000000000000000000
      0000000000003131313DDF8C8CF8EA7272FFF66161FFF17B7BF82E2E2E380000
      0000000000000000000000000000000000000000000000000000000000000000
      00007D6E64CF8E725EFE413F3E5F000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002525262E383A
      3C543D40446341474F7D3F4C61AE3B4E69C6485D7DE397A0ACFD3B5377E03132
      3343000000000000000000000000000000000000000005050506737373AB5151
      51710909090A0505050613131315D3A2A2EDC3A8A8E00909090A050505060909
      090A53535374727272A805050506000000000000000000000000040404050303
      03041E1E1E22DD9494F5E86A6AFFF27474FFFF2727FFFF2727FFE98E8EF41919
      191C030303040404040500000000000000000000000000000000000000000000
      00007E7065CFE9CEB0FF8E735DFE4E484681444140663A3838501D1D1D220000
      00000000000000000000000000000000000000000000000000003A5377DDA0A9
      B6FFABB1BBFFB5BAC1FFCCCCCDFFDCDCDCFFEFEFEFFFE1E1E1FFDEDFDFFF4A60
      82E73D4044630000000000000000000000000000000000000000050505060303
      03040000000004040405C39D9DE2E76767FFF28989FFB09191D0000000000000
      00000303030405050506000000000000000000000000040404056D6D6DBF5757
      57824747475E948989BB958C8CBB968484BB968484BB968484BB968484BB4443
      43585A5A5A886C6C6CBA04040405000000000000000000000000000000013E3C
      3B58907A6BF3ECD3B7FFE9CDAEFFAF9478FFA58669FF9D795DFF8B654BFF684E
      3FE34643416B0505050600000000000000000000000022222229808EA2F1F2F2
      F2FFF2F2F2FFF2F2F2FFE9E9E9FFD5D5D5FFE9E9E9FFEAEAEAFFF9F9F9FFE8E8
      E8FF60708DED4249538800000001000000000000000000000000000000000000
      000000000001AB9090D0E66666FFEE8888FFFD3535FFFD4444FE918080B70000
      00000000000000000000000000000000000000000000060606073C3C3CF45858
      58EC6A6A6ACC696969CB696969CB696969CB696969CB696969CB696969CB6A6A
      6ACC555555ED424242F10606060700000000000000000D0D0D0E67564ECCAF9B
      8CFFECDAC5FFEED7BEFFEBD0B4FFE8CAAAFFE5C4A0FFE2BE96FFDFB88CFFD9AF
      81FF9E7758FF6A5043DC1918181C000000000000000042485080C5C8CEFFD6D6
      D6FFD8D8D8FFD5D5D5FFDCDCDCFFD6D6D6FFDCDCDCFFE5E5E5FFE5E5E5FFF9F9
      F9FFF1F1F1FF8F9AA6F83B4C66BD101010120000000000000000000000000000
      000064616186E4A8A8F8E0ADADF5E89292F5EB8282F5EB8282F5EFA1A1F74444
      44590000000000000000000000000000000000000000060606075B5B5BEB3333
      33F7333333F7333333F7333333F7333333F7333333F7333333F7333333F73333
      33F7333333F7606060E8060606070000000001010102726159DAE6DBCFFFF5E7
      D8FFF2E1CEFFEFDAC4FFDBB29CFFD19D86FFCF987CFFCC9475FFDDB58DFFDEB6
      89FFDCB181FFC79F72FF715543E80A0A0A0B000000003A4C68C3E5E5E5FFD6D6
      D6FFD8D8D8FFD6D6D6FFD4D4D4FFDFDFDFFFE0E0E0FFD7D7D7FFE5E5E5FFDDDD
      DDFFF9F9F9FFF9F9F9FFCBCDD1FF374F73D60000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000012E2E2E383232
      323E3232323E3232323E3232323E3232323E3232323E3232323E3232323E3232
      323E3232323E2C2C2C3600000001000000004F4C4B82E8E0DBFFFAF1E8FFF7EB
      DEFFF4E4D4FFF1DECAFFC68978FF8D1815FF800000FFA74B3DFFDAAE8BFFE0BA
      8FFFDDB485FFDCB181FFC29C6FFF564D489C03030304526686E2E2E2E2FFD5D5
      D5FFD4D4D4FFD5D5D5FFD6D6D6FFEBEBEBFFE5E5E5FFDDDDDDFFD5D5D5FFE7E7
      E7FFD8D8D8FFE8E8E8FFF9F9F9FF5B687BC30000000000000000000000000000
      0000040404050F0F0F110F0F0F110F0F0F110F0F0F110F0F0F110F0F0F110404
      0405000000000000000000000000000000000000000006060607434343F30808
      08FE080808FE080808FE080808FE080808FE080808FE080808FE080808FE0808
      08FE080808FE494949F006060607000000008C837FDFFEFBF8FFFBF4EEFFF9EE
      E4FFF6E8DAFFF3E2D0FFF0DCC6FF9D3730FF800000FFD7A88CFFE5C49FFFE2BE
      95FFDFB78BFFDCB181FFDCB181FF846650EC2F30313F9AA3B4F8DDDDDDFFD5D5
      D5FFD6D6D6FFD4D4D4FFD5D5D5FFDEDEDEFFDEDEDEFFE5E5E5FFD9D9D9FFE1E1
      E1FFD8D8D8FFF6F6F6FFD7DBE1FB2323242B0000000000000000000000000000
      000055545473E9A4A4FBEAA4A4FBF28F8FFBF77070FBF77070FBF79696FB4F4D
      4D690000000000000000000000000000000000000000060606073C3C3CF46C6C
      6CE56C6C6CAC6B6B6BAB6B6B6BAB6B6B6BAB6B6B6BAB6B6B6BAB6B6B6BAB6C6C
      6CAC676767E6424242F10606060700000000ABA4A0F0FFFCFAFFFDF8F4FFFAF2
      EAFFF8ECE0FFF5E6D7FFDEB8A9FF962924FF800000FFD8AB91FFE6C7A5FFE4C1
      9BFFE1BB92FFDEB588FFDCB181FF9D7B5DF9414B5A9BCFD1D3FFD6D6D6FFD3D3
      D3FFD5D5D5FFD5D5D5FFD6D6D6FFE0E0E0FFDBDBDBFFDADADAFFE6E6E6FFD7D7
      D7FFE4E4E4FFF9F9F9FF728196DC000000000000000000000000000000000000
      000000000000897D7DB2E66F6FFEED8383FFFB4141FFFD4545FE897979AF0000
      00000000000000000000000000000000000000000000030303046363639A4545
      455F58575777E2A3A3F7DFA5A5F4E58C8CF4E97777F4E97777F4EF9292F75251
      516E484848656161619603030304000000009D9795E2FFFCFAFFFFFCFAFFFCF6
      F0FFF9F0E7FFF7EADDFFA54641FF800000FF800000FFD9AE96FFE8CBABFFE5C5
      A2FFE3BF98FFE0B98EFFDDB384FF8F735BE93D516FCDE4E4E4FFD6D6D6FFD4D4
      D4FFD4D4D4FFD5D5D5FFD4D5D6FFD4D7DAFDE3E3E3FFD2D2D2FFE2E2E2FFD8D8
      D8FFECECECFFECEDEFFE3A3C3E56000000000000000000000000050505060303
      03040000000000000000A38D8DC9E86C6CFFF09595FFA98D8DCB000000000000
      0000030303040505050600000000000000000000000000000000030303040202
      020300000000A18C8CC8E76868FFEE8787FFFC3838FFFF3737FFA88B8BCA0000
      0000020202030303030400000000000000005D5D5C94FFFCFAFFFFFCFAFFFEFA
      F7FFFBF4EDFFF8EEE3FFF0DFD0FFEDD5C3FFEACEB9FFECD3BBFFEACFB2FFE7C9
      A8FFE4C39EFFE2BD94FFDFB78AFF58534E94465B7CD8D7DADFFCE4E5E8FECFD3
      D9FBA4AEBCF166758AD43C3E415C535E6FB6D8D8DAFEE3E3E3FFD7D7D7FFE3E3
      E3FFECECECFF8E9AAEEB00000001000000000000000005050506737373AB5151
      51710909090A050505060909090ABF9D9DDFBCA4A4DB08080809050505060909
      090A53535374727272A805050506000000000000000000000000000000000000
      00000000000002020203BB9B9BDCE86969FFF08787FFC19393DC020202030000
      00000000000000000000000000000000000011111113A09E9CDEFFFCFAFFFFFC
      FAFFFDF8F3FFFAF2E9FFF7EBDFFFC5877BFFB15F57FFE5C5B0FFECD3B8FFE9CD
      AEFFE6C7A4FFE3C19AFF8E7B65DE0F0F0F110000000020202026282929331818
      191C010101020000000000000000000000014C535D96BBC1C8F9D6D6D6FFEAEA
      EAFFEAEAEAFF494F57890000000000000000000000000D0D0D0E383838FE8484
      84FA7F7F7FB77C7C7CB57C7C7CB57F7F7FB77F7F7FB77C7C7CB57C7C7CB57F7F
      7FB77E7E7EFA3F3F3FFE0D0D0D0E000000000000000000000000000000000000
      000000000000000000000A0A0A0BD0A0A0EBCDABABE808080809000000000000
      000000000000000000000000000000000000000000002323232A747372B36363
      6298B5B2B0E7FCF5EFFFF9EFE6FFA03C39FF800000FFDEB8A6FFEED7BEFFEBD1
      B4FFE7CAA9FF847667D220202026000000000000000000000000000000000000
      000000000000000000000000000000000000000000002B2C2D38748399DFCCCD
      D1FDAAB5C4F40A0A0A0B0000000000000000000000000D0D0D0E393939FE0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF3F3F3FFE0D0D0D0E000000000000000000000000000000000000
      0000000000000000000000000000131313151010101200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003838384CF4EFECFFFBF3ECFFF0DED4FFEBD2C5FFF1DECCFFE4D1BBFE9085
      78D94F4D4B7A0404040500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002C2D
      2E3A3D3F425D00000000000000000000000000000000050505066E6E6E9C7B7B
      7BAF7B7B7BAF7B7B7BAF7B7B7BAF7B7B7BAF7B7B7BAF7B7B7BAF7B7B7BAF7B7B
      7BAF7B7B7BAF6C6C6C9A05050506000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002F2F2F3C4949486B5251517D5251507C4847476B2C2C2C380000
      0001000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002E2E2E3D32323245323232453232
      3245323232453232324532323245323232453232324532323245323232453232
      32453232324532323245323232452C2C2C390000000000000000000000001313
      13159B8989CC7B7272AA00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000292929343F47
      3F770909090A0000000000000000000000005C5C5CE5BEBEBEFFBEBEBEFFBEBE
      BEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBEBEFFBEBE
      BEFFBEBEBEFFBEBEBEFFBEBEBEFF5A5A5ADE0000000000000000000000001A1A
      1A1EA26363EB8C7676C300000000000000000000000000000000000000000000
      000000000000000000000000000000000000040404054C493E6C4B483D691A1A
      1A1E0000000000000000544F3F79514D3F760000000000000000000000000000
      0000000000006B623AA434333042000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003D413D680080
      00FF345234B40A0A0A0B00000000000000006D6D6DDDF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EB8C7676C3000000004E4D4D6AA18484D2A08181D0A18181D0A481
      81D0A58181D0B29292D82626262D000000000D0D0D0ED49A0BF3FFAE00FF3C3B
      3751000000003938354B9D7F23D41D1D1D229B801BDF48463D67000000002C2C
      2B37AC8B12EB4A483E6C9D7F22D61212121455504C8E89725ED689725ED68972
      5ED689725ED689725ED67F6C58DA4D6134EB4D6235EB4D6235EB365B25F20080
      00FF008000FF305F21F3826E5AD741403F5F6D6D6DDDF9F9F9FFF9F9F9FFF3F3
      F3FFF3F3F3FFF9F9F9FFD9D9D9FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF3F3
      F3FFF3F3F3FFDADADAFFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EB9F8787D45B57577D988F8FC2A60D0DFFB4A4A4D97E7474A87E74
      74A8B29090D9D54242FD2F2F2F3A000000000D0D0D0ED49A0BF3FFAE00FF3C3B
      37510000000089722FBF49473D6807070708BD9013EA49483E6A000000002E2E
      2C3AE5A500FF282827316D613B9F5E573F8AB79E8AF2F5EDE6FFF5EDE6FFF5ED
      E6FFF5EDE6FFF5EDE6FFBEC0B2FF009500FF009900FF009900FF009900FF0099
      00FF009900FF009900FF408E3CFF75675AC46D6D6DDDF9F9F9FFF9F9F9FF8181
      81FF888888FFF9F9F9FFCDCDCDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFDADADAFFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EBA25C5CEEAE8585E4B58383E9A20303FF7B7272A70F0F0F110F0F
      0F11917777BED54242FD2F2F2F3A000000000D0D0D0ED49A0BF3FFAE00FF3C3B
      37511A1A1A1EAC8818E30606060707070708BD9013EA49483E6A000000002E2E
      2C3AE5A500FF282827313D3C375293782AC9BAA390F3F9F9F9FFFBFBF6FFFBFB
      F6FFFBFBF6FFFBFBF6FFC0CBC0FF00B100FF00B600FF00B600FF00B600FF00B6
      00FF00B600FF00B600FF00B600FF336F26EC6D6D6DDDF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFDCDCDCFFB4B4B4FFB4B4B4FFB4B4B4FFB4B4B4FFB4B4
      B4FFB4B4B4FFAEAEAEFFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EB8C7676C300000000625D5D87B83E3EFFC68F8FF1C79595EDC995
      95EDCE7D7DF3DD6666FD2F2F2F3A000000000D0D0D0ED49A0BF3FFAE00FF3C3B
      37513939354C967A29CA0000000007070708BD9013EA49483E6A000000002E2E
      2C3AE5A500FF282827311A1A1A1EAB8718E4BAA490F3F9F9F9FFFFFEF2FFFFFE
      F2FFFFFEF2FFFFFEF2FFC0CDBFFF00CD00FF00D300FF00D300FF00D300FF00D3
      00FF00D300FF00D300FF0DC10DFF5E6B48D66D6D6DDDF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EB8C7676C3000000001919191C3C3C3C4E3C3C3C4E3C3C3C4E3C3C
      3C4E3D3C3C4E3C3C3C4E0C0C0C0D000000000D0D0D0ED49A0BF3FFAE00FF3C3B
      37512B2B2A36A28120D80000000007070708BD9013EA706438AB02020203403F
      3958D99F05F91B1B1B1F2C2C2B37A28120D9BAA490F3F9F9F9FFFFFEF2FFFFFE
      F2FFFFFEF2FFFFFEF2FFC7D2C6FF28BC28FF29C129FF29C129FF20C620FF03EF
      03FF03EF03FF14D714FFABCFABFF7A6B5EC16D6D6DDDF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFFAFAFAFFFAFAFAFFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EB8C7676C3000000005B57577CAD7E7EE1BA8E8EE6BF9393E7C293
      93E7BD8989E4C49494E62C2C2C3600000000706438ABE8A405F9FFAE00FF7E6C
      32BA2525242CA08120D82A2A293407070708BA8F12EB8C7822D5907729CAA783
      20D989742CC60101010255503F7B736439A6BAA490F3F9F9F9FFFFFEF2FFFFFE
      F2FFFFFEF2FFFFFEF2FFF9F9F8FFF9F9F9FFF9F9F9FFF9F9F9FFABC8ABFF6DF6
      6DFF6CDD6CFFBCD0BCFFF9F9F9FF7A6B5EC16D6D6DDDF9F9F9FFF9F9F9FFD1D1
      D1FFD3D3D3FFF9F9F9FFCECECEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFDADADAFFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EB9E6969E28F7474C6A48484D7A30606FF9B8E8EC54D4B4B684D4B
      4B68A38686CDD54242FD2F2F2F3A00000000937925D1F4AA02FCFFAE00FFAC86
      1EDC2F2F2D3B58533F81726439A70202020349473D691818181B4C4A3E6F524E
      3F78151514170E0E0E0F9B7E23D435353245BBA490F3F9F9F9FFFFFEF2FFFFFE
      F2FFFFFEF2FFFFFEF2FFF9F9F8FFF9F9F9FFF9F9F9FFF9F9F9FFC1C8C1FFB9D1
      B9FFCDD3CDFFF9F9F9FFF9F9F9FF7B6B5EC16D6D6DDDF9F9F9FFF9F9F9FFA3A3
      A3FFA8A8A8FFF9F9F9FFCDCDCDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFDADADAFFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EBA07F7FDA776C6CA6A19090CDA40A0AFF877D7DB22A2A2A332A2A
      2A33967D7DC2D54242FD2F2F2F3A000000000D0D0D0ED49A0BF3FFAE00FF3C3B
      37510000000005050506816E31BC4D4A3E6F0000000000000000000000000000
      000000000000716538A95D563F8903030304BBA490F3F9F9F9FFFFFEF2FFFFFE
      F2FFFFFEF2FFFFFEF2FFF9F9F8FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FF7B6B5EC16D6D6DDDF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFE8E8E8FFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBF
      BFFFBFBFBFFFC0C0C0FFF9F9F9FF666666D40000000000000000000000001A1A
      1A1EA26363EB8C7676C3000000005F5B5B83B26262EFC98F8FF2CE9696F2D096
      96F2CD8383F1D08080F12E2E2E39000000000D0D0D0ED49A0BF3FFAE00FF3C3B
      37510000000000000000131313151D1D1D220000000000000000000000000000
      0000000000002B2B2A350000000103030304BBA490F3F9F9F9FFFCFCF7FFFCFC
      F6FFFCFCF6FFFCFCF6FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FF7B6B5EC16D6D6DDDF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FF666666D400000000131313154A4848625655
      5575A36161ED9B8484CF49484862504F4F6C3232323E1E1E1E221E1E1E221E1E
      1E221E1E1E221E1E1E2203030304000000000D0D0D0ED49A0BF3FFAE00FF3C3B
      3751000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000003030304BBA491F3F9F9F9FFDBDBDBFFDADA
      DAFFDADADAFFDADADAFFDADADAFFDADADAFFDADADAFFDADADAFFDADADAFFDADA
      DAFFDADADAFFECECECFFF9F9F9FF7B6B5FC16D6D6DDDA6A64EFF90901DFF9090
      1DFF90901DFF90901DFF90901DFF90901DFF90901DFF90901DFF90901DFF9090
      1DFF90901DFF90901DFFA0A03FFF666666D40000000041404054E85F5FFFC486
      86F0AA8080E5B18989E8B88585EFB13C3CFF524F4F6F00000000000000000000
      00000000000000000000000000000000000003030304B98D18E4FFAE00FF534F
      3F7A3232304045443C612C2C2B37000000000000000000000000000000000000
      000000000000000000000000000003030304BBA491F3F9F9F9FFE5E5E5FFE4E4
      E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4E4FFE4E4
      E4FFE4E4E4FFF0F0F0FFF9F9F9FF7B6B5FC16D6D6DDDC3C38BFFB5B56BFFB5B5
      6BFFB5B56BFFB5B56BFFB5B56BFFB5B56BFFB5B56BFFB5B56BFFB5B56BFFB5B5
      6BFFB5B56BFFB5B56BFFBEBE80FF666666D40000000041404054E44747FF8877
      77B60F0F0F110F0F0F117F7474AEA51E1EFF524F4F6F00000000000000000000
      000000000000000000000000000000000000000000006C603B9FFFAE00FFF7AA
      00FEEEA704FAF0A703FB42413A5B000000000000000000000000000000000000
      000000000000000000000000000003030304B39883F1F2E0D2FFF2E0D2FFF2E0
      D2FFF2E0D2FFF2E0D2FFF2E0D2FFF2E0D2FFF2E0D2FFF2E0D2FFF2E0D2FFF2E0
      D2FFF2E0D2FFF2E0D2FFF2E0D2FF78695DBF4E4E4EBA5B5B5BBC5B5B5BBC5B5B
      5BBC5B5B5BBC5B5B5BBC5B5B5BBC5B5B5BBC5B5B5BBC5B5B5BBC5B5B5BBC5B5B
      5BBC5B5B5BBC5B5B5BBC5B5B5BBC4E4E4EB10000000041404054E44747FFAA8F
      8FD5726A6A9F7269699FA59191D3A51E1EFF524F4F6F00000000000000000000
      000000000000000000000000000000000000000000000B0B0B0C675D3D97A07F
      24D2977A28CC4D4A3E6D00000000000000000000000000000000000000000000
      00000000000000000000000000000000000054504D8CE3BC9CFEEAC3A2FFEAC3
      A3FFEAC3A3FFEAC3A3FFEAC3A3FFEAC3A3FFEAC4A4FFEAC4A4FFEAC4A4FFEAC4
      A4FFEAC4A4FFEAC4A4FFC7A88EF5434241620000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003232323EA68484CD9676
      76C68E7474C68D7474C68F7474C6977B7BCB3F3E3E5200000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000474747685353537A5353
      537A5353537A5353537A5353537A5353537A5353537A5353537A5353537A5353
      537A5353537A5353537A3A3A3A4F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A2A2B363E3E5C90282886CD1414A1E815159FE7292982CB3E3E5A8C2626
      262F00000000000000000000000000000000000000003E3D3C55BEAE96F0BEAC
      93F0BDAC8FF0BDAA8DF0BDA98AF0BDA888F0BCA785F0BCA683F0BBA57FF0BBA3
      7DF0BAA279F0BAA178F02A2A2934000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000415141733691
      36CA000000000000000000000000000000000000000000000000000000000000
      000000000000373C37520E0E0E0F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000080808093D3D
      5E970505CCF90000EAFF0000F2FF0000F6FF0000F5FF0000EFFF0000E4FF0808
      BFF63E3E598D040404050000000000000000000000003E3D3D54F7E2C2FFF6E1
      BEFFF6DFBAFFF5DDB6FFF5DBB3FFF4DAAFFFF4D8ABFFF3D6A7FFF3D4A4FFF2D3
      A0FFF2D19CFFF1CF98FF2A2A2A35000000000000000000000000000000000000
      0000000000000000000000000000000000003B9840CB418443B61FB624EF09BF
      0BFF407540A926A326E12425242C000000000000000000000000000000000000
      00003D463D66139213EA2A722AC90D0D0D0E0000000000000000000000000000
      000000000000000000000000000000000000000000000909090A313177BB0000
      EAFF0000FCFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      F1FF0000DDFF36366DB00404040500000000000000003E3D3D54F7E5C7FFF7E3
      C3FFF6E1BFFFF6DFBBFFF5DEB7FFF5DCB4FFF4DAB0FFF4D8ACFFF3D7A8FFF3D5
      A5FFF2D3A1FFF2D19DFF2A2A2A35000000000000000000000000000000000000
      0000000000000000000000000000000000003CA042D01AD127FF13C91DFF0CC2
      13FF05BB09FF09BA0AFD1C1D1C21000000000000000000000000000000003C45
      3C64149114E800BC00FF00BA00FF2D6F2DC30C0C0C0D00000000000000000000
      000000000000000000000000000000000000000000003C3C60990000EEFF0000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF0000F5FF0000DCFF3E3E588B00000000000000003E3E3D54F8E7CBFFE2D6
      BBFFC7BE9FFFC7BD9DFFC7BC9BFFC7BC9AFFC6BB98FFC6BA97FFC5B995FFC5B9
      93FFE4CFA8FFF3D3A2FF2A2A2A35000000000000000000000000000000000000
      00000000000000000000000000004351446E34CF41F122D932FF30C037EC4183
      43B50DC314FF0ABF0DFF427142A20F0F0F1000000000000000003C443C631491
      14E800BC00FF00BC00FF00BC00FF00BA00FF2E6E2EC20C0C0C0D000000000000
      0000000000000000000000000000000000002B2B2C380404D7FA0000FFFF0000
      FFFF0000FFFF4747FDFF3535FEFF0000FFFF0000FFFF2A2AFEFF5151FDFF0000
      FFFF0000FFFF0000EFFF0909BBF52424242C000000003E3E3D54F9E9D0FFC1BB
      9CFF505016FF6B6B38FF626248FF6B6B38FF6B6B38FF6B6B38FF6B6B38FF4B4B
      10FFCFC19EFFF3D6A7FF2A2A2A35000000000000000000000000000000000000
      000000000000000000000000000043BD51DF30E948FF2AE23EFF477B4AA71414
      141615CC20FF0EC416FF0DC112FE2324232A000000003A413A5B149114E800BC
      00FF00BC00FF00BC00FF00BC00FF00BC00FF00B900FF2D6E2DC40B0B0B0C0000
      0000000000000000000000000000000000003D3D60950000FAFF0000FFFF0000
      FFFF4747FDFFF3F3F9FFEAEAF9FF3535FEFF2A2AFEFFE4E4F9FFF6F6F9FF5454
      FDFF0000FFFF0000FEFF0000E0FF3F3F5788000000003E3E3D54F9EBD5FFC2BC
      9EFF777747FFB8B8EEFF2323FCFFBDBDEEFFFBFBE8FFFBFBE8FFFBFBE8FF6060
      2AFFCFC3A0FFF4D8ABFF2A2A2A35000000000000000000000000000000001415
      151703030304000000003F525F863F4A5171489B51C332EA49FF2BE43FFF28DB
      37FD1DD42BFF1ED128FD08080809000000003B423B5E159015E700BC00FF00BC
      00FF00BC00FF1A851AE102B202FC00BC00FF00BC00FF00B900FF2E6D2EC10909
      090A00000000000000000000000000000000242496D30000FFFF0000FFFF0000
      FFFF3434FEFFEAEAF9FFF9F9F9FFEAEAF9FFE5E5F9FFF9F9F9FFEFEFF9FF3E3E
      FDFF0000FFFF0000FFFF0000EAFF2C2C7EC5000000003E3E3D54FAEEDAFFC2BD
      A1FF565679FF1919FDFF0000FFFF3C3CFAFFF7F7E8FFFBFBE8FFFBFBE8FF6060
      2AFFCFC4A3FFF4DAB0FF2A2A2A3500000000000000000000000035383A4B0196
      F6FD2D6C92C33F4D55770189F7FD0681DFF845D959ED39F454FF33EB4BFF2CE4
      41FF28DD38FE1ED62DFF3D453D5C00000000267426D000BB00FF00BC00FF00BC
      00FF227B22D520202026366436AF02B302FC00BC00FF00BC00FF00B800FF2F6C
      2FBF080808090000000000000000000000011010BBED1B1BFCFF0000FFFF0000
      FFFF0000FFFF3030FEFFE8E8F9FFF9F9F9FFF9F9F9FFF0F0F9FF4040FDFF0000
      FFFF0000FFFF0000FFFF0000F0FF1B1B96E0000000003E3E3D54FBF0DEFFBDB9
      A5FF1212E2FF1D1DFCFF6666F5FF0505FEFF6B6BF5FFFBFBE8FFFBFBE8FF6060
      2AFFD0C5A5FFF5DCB5FF2A2A2A350000000000000000000000000C0C0C0D009B
      FAFE2CA6F6FF65B1E6FF56A8E6FF128EF1FE417067AA367280BE479954C238EB
      4CFD0A0A0A0B3E493F6100000001000000001B1B1B1F247924D300BB00FF227C
      22D5212121270000000002020203386338AB02B302FC00BC00FF00BC00FF00B8
      00FF2F6C2FBF0808080900000000000000010F0FC0EE6565F9FF2E2EFDFF0000
      FFFF0000FFFF2626FEFFE2E2FAFFF9F9F9FFF9F9F9FFEAEAF9FF3535FEFF0000
      FFFF0000FFFF0000FFFF0000F1FF1A1A97E1000000003E3E3D54817E7ABFA7A4
      8EF14D4D88FFB9B9EEFFFBFBE8FF6D6DF5FF0A0AFEFFA1A1F0FFFBFBE8FF6060
      2AFFD0C6A7FFF6DFBAFF2A2A2A3500000000020202033A6476A42E7394C25EBF
      F4FFDADBDCFFCFD2D4FFC8CACCFFB5BFC7FF1D93F3FF0088FFFF386A78B3373D
      384E00000000000000000000000000000000000000001C1C1C202B6E2BC72222
      222900000000000000000000000003030304356635B301B301FD00BC00FF00BC
      00FF00B800FF316931BC070707080000000023239CD45555FAFF7777F9FF0303
      FFFF2A2AFEFFE4E4F9FFF9F9F9FFEFEFF9FFEBEBF9FFF9F9F9FFEAEAF9FF3434
      FEFF0000FFFF0000FFFF0000EEFF2C2C7EC6000000003E3E3E54716F6CA36767
      5EAC777747FFFBFBE8FFFBFBE8FFFAFAE8FF6767F5FF1515FDFFB5B5EFFF6060
      2AFFD1C7A9FFF6E1BEFF2A2A2A35000000002E30313D00B3FFFF0AB1FEFFE4E9
      ECFF91A7AFDF070707081E1E1E23B3BEC5FC87B3D3FF147DC1E8050505060000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000002020203356635B101B301FD00BC
      00FF00BC00FF00AD00FE326632BA070707083D3D64962323FDFF8888F8FF3C3C
      FBFF5151FDFFF6F6F9FFEFEFF9FF3E3EFDFF3434FEFFEAEAF9FFF8F8F9FF5F5F
      FDFF0000FFFF0000FFFF0000E7FF3F3F5888000000003E3E3E5471706EA36060
      599C777747FFFBFBE8FFFBFBE8FFFBFBE8FFFBFBE8FF7777F4FF2323FCFF4848
      5EFFD1C8ABFFF7E3C3FF2A2A2A3500000000030303043F51577A2EC1FEFFF0F0
      F0FF4445465E00000000000000007B8F98D0AFC1CDFF0C88D5F21E1E1E230000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004040405336733B600B5
      00FE00BC00FF00B200FF009900FE336333B82C2C2E3A0404ECFA6262FAFF7373
      F8FF2727FDFF5353FDFF3E3EFDFF0000FFFF0000FFFF3434FEFF5E5EFDFF0000
      FFFF0000FFFF0000FBFF0909C3F52525252D000000003E3E3E5471706FA36060
      599C505016FF6B6B38FF6B6B38FF6B6B38FF6B6B38FF6B6B38FF494975FF0C0C
      D3FF9892C4FFF8E5C8FF2A2A2A3500000000000000000F0F0F110CBCFCFEEFF4
      F6FF94A8B1DA080808091C1C1C20B9C8CFFA90BEDAFF0099FFFF068BE0F80000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040404053368
      33B701B301FD00B200FF0E8B0EF03F4F3F7D000000003C3C679B0B0BFEFF6161
      F9FF6262FAFF3838FCFF0808FEFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000EBFF3E3E598D00000000000000003E3E3E54717170A34444
      435E5A5A548D5A5A548D63635CA2ACA895F1C9C4ABFFC8C3AAFFB6B198FFA6A0
      9CFF9089C3FFBAAF97F9141414170000000000000000169CC5E600BFFFFF6AD6
      FCFFF2F3F3FFDEE6E9FEE0E1E2FFCCD7DCFF24ABF3FF2F6E8DC03E5866920000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000505
      0506336833B70B960BF33F513F8200000000000000000A0A0A0B313185BC0505
      FFFF3535FBFF4E4EFAFF3D3DFBFF2222FEFF0B0BFFFF0000FFFF0000FFFF0000
      FFFF0000F1FF35356FB10404040500000000000000003E3E3E54717171A30000
      00000000000000000000000000002525252DB6B1A8E0FBF1E0FFD1C7B4FFF6E3
      C2FFE3D2B5FE3B3B3A4F0000000000000000000000003F535A7E317488BC2487
      A6D338CBFEFF7CD5F7FF69CBF4FF16B4FCFF168ABDE600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000060606073A3F3A5A000000000000000000000000000000000A0A0A0B3D3D
      66980505EAF90808FFFF1818FEFF1919FDFF0D0DFEFF0000FFFF0000FFFF0707
      D8F73E3E5B8F040404050000000000000000000000003E3E3E54A8A8A8D47373
      73A1727270A772716FA772706FA6777572A498948FCDFCF3E5FFD3C9B9FFECDA
      BBFF4B4A486C0000000000000000000000000000000000000000000000001C1C
      1C2000C1FFFF09A6DAF53E59618B09A2DEF505A1E2F906060607000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002A2A2B363E3E6391282898CD1414BBE81515B8E7292991CB3E3E5D8D2727
      273000000000000000000000000000000000000000003E3E3E55C4C4C4F0C4C4
      C4F0C4C3C3F0C3C3C0F0C3C1BDF0C3C0BBF0C3BFB7F0C3BDB5F0B3ACA1F25A59
      5689000000000000000000000000000000000000000000000000000000000F0F
      0F10356F82B33F4C5171000000002525262D1111111300000000000000000000
      0000000000000000000000000000000000001818181B378BA8DA4142425F0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000474B476A33343343000000000000000014141416378BA7D9444646670000
      000000000000000000000000000000000000383A4154384683AA384683AA3E47
      698D1A1A1A1E0000000000000000000000001819191C378BA8DA4041415D0000
      000000000000000000000000000000000000384684AB1F3CBCDA263FADCF2940
      A7CA28292A3300000000000000000000000000000000363974AF272728300000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005094B3D727C2FFFF24ADDDFE4344
      4463000000000000000000000000000000000000000000000000000000000000
      000048504875288C28E93435344400000000518CA8CF28C2FFFF22B0E1FF4748
      496D00000000000000000B0B0B0C000000003F455E7E3F4665881D1D1E223D41
      4F691E3CBFDC0303030400000000000000005094B3D827C2FFFF25AEDDFE4344
      4463000000000E0E0E0F2323242A00000000384684AB33343948000000003233
      3746213DB7D7000000000000000000000000000000001435C4E80389F3FB3845
      76A90E0E0E0F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002728283051B5E6F52CC2FFFF26B0
      E1FF4547476900000000000000000000000000000000151515174E974ED94591
      45D6379937E300A600FF2D992DE7323332412222222853B1E0F22DC2FFFF22B2
      E5FF4B4F56833F4663851C3BC2DE222223293F455E7E3F445876000000000000
      0000183ACAE31A1A1A1E00000000000000002728283051B5E6F52CC2FFFF26B0
      E1FF454F73A5253EB0D11B3AC5E00F0F0F11384684AB33343948000000001D1D
      1E221739CDE500000000000000000000000000000000182EBBE400B3FEFF00B0
      FEFF1360CCEA3C3C466000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002223232955B3E0F331C3
      FFFF29B1E2FF494A4B7200000000000000000000000012121214519351CC498E
      49C93E9D3EDA00BB00FF35A035DF2B2B2B35000000002121212756B0DEF130BA
      FFFF0648F8FF475272A83C3E4960000000013F455E7E3A467CA3383A41553E46
      6C902F4399BF000000010000000000000000000000002223232955B3E1F32195
      FEFF0B57F4FF4A4E537E1414141600000000384684AB33448FB63A467BA22941
      A8CB383A41540000000000000000000000000000000039394056085CE4F600A8
      FEFF00B4FEFF009CFDFF294A9DCA2525252D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000222222285AB2
      E0F237C3FFFF29B3E5FF4A4D4E77101010124C4C4493504E35D0514E32D84F4E
      42B45166509A32AB32DF2A2B2A340000000000000000000000001F2020253478
      EAF71F84FEFF29B4E7FF4C4F507C0D0D0D0E38457DC71938BCF3284093EE1638
      C7F1404145660000000000000000000000000000000000000000222222281C58
      F3FB2EAFFFFF29B3E5FF4A4D4E7710101012283FA1DC32437FE63C4762E41B3A
      BBED3F414767000000000000000000000000000000000000000038383F550872
      E5F600AEFDFF00B1FEFF00B4FEFF047DF0FA3C436D9C0909090A000000000000
      0000000000000000000000000000000000000000000000000000000000002121
      21275DB2E0F23CC4FFFF4097AFF77A781AFCB1B162FFC6C58DFFCCCB9CFFB8B7
      70FF89B148FF586549D20B0B0B0C000000000D0D0D0E03030304000000003E4A
      779E4993E1F33CC2FFFF3D96BAF8757419FB586FAFFF687DC0FFCDCB9CFF8390
      9CFF2A4BB6FF4E4D3CCD0D0D0D0E000000003C3E49610909090A000000003247
      99BF53A6E1F332ABFEFF3685BBF87A791AFC3858CAFF8D9BACFFCCCB9CFF667A
      ADFF3A5498FF4F4E40C60B0B0B0C000000000000000000000000000000003838
      3F550874E5F600DEFCFF00A5FDFF00B4FEFF00ACFEFF1459CBE83A3A425A0000
      0000000000000000000000000000000000000000000000000000000000000000
      00001F2020255E8DA2DF989A37FFDBD8B0FFECE9D3FFF1F0E3FFF3F2E8FFF4F4
      EDFFEAE9D7FFA2A247FF4F4D3BD2010101023B46779E3E41516C000000003536
      3B4B393B40533968C8EC3C59AAFFDAD6AFFF758DE7FF6E8AF0FFC4CDEBFF6280
      F5FF5573EFFFA7A74EFF4E4D38D903030304374687AE393B4358000000002A2A
      2C35282829311B4AE2F76A7872FFCCCAB7FF4665EFFF3D5AF6FF4C63F6FF1943
      FAFFA2B0E2FFA2A247FF4E4D3BD2010101020000000000000000000000003A3A
      425A303481BD0072FCFF00FDFCFF00B9FDFF00ACFEFF00B4FEFF0091FBFE3145
      8ABB202020260000000000000000000000000000000000000000000000000000
      0000070707087B7A20F8DFDBB6FFEEE9D2FFF0EEDEFFF0EEE1FFF1F0E4FFF3F2
      E9FFF4F4EDFFE8E7D3FF909027FF4343416A2D2E303C2C42A0C50F0F0F110F0F
      0F1114141416133AD5FEB0B7C1FFEEE9D1FFA2B0E9FF4E6DF3FF5574F3FF7891
      F3FFE4E7EEFFEAE9D8FF93932BFF4646457635373C4D2E439CC1383A4155383A
      41553D404C660D38E2FEB8B6C4FF3B39F4FF3E48F5FF6B79F1FF7687F1FF565D
      F7FF8D8DF5FFE8E7D3FF909027FF4343426A000000000000000000000000393A
      76A700A6F4FF00A6F4FF00C9F9FF00FEFCFF00EEFCFF00AAFDFF00B3FEFF00B2
      FEFF0866E4F63535394B00000000000000000000000000000000000000000000
      00004444406FB9B66EFFF0E9D0FFF1EBD5FFF0EDDBFFF0EEDEFFF1EFE2FFF1F0
      E5FFF3F3EAFFF3F2EAFFBBBB76FF4F4C35DA00000001203DBBD91F3CBCDA2840
      A9CC1539CBEA1E46E3FFF0E9D0FFEFEAD3FFF1EEDCFFF0EEDEFFF0EEE1FFF1F0
      E5FFF3F2E9FFF3F3ECFFBEBC7BFF4E4D31E1030303041537D0E73F47668A3F44
      5877213DB0DE566DBAFFD3CDD6FF2524F8FF7674EDFFF0EEDEFFF1EFE2FFF1F0
      E5FF7777F5FFBCBBEFFFBAB976FF4F4D36DA0000000000000000000000003C3C
      6C9B00A3F4FF00A6F4FF00A6F4FF00B9F9FF00FEFCFF00FEFCFF00CBFDFF00A8
      FEFF00AFFEFF3E3E4E6E00000000000000000000000000000000000000000000
      000051503AB1D1CD9CFFF3EBD3FFF0EAD0FFF1EDD9FFF0EDDCFFF1EEDFFFF2F0
      E2FFF2F1E6FFF2F1E8FFCFCE9FFF5A581CF8000000003F445A793F455C7C0000
      0000304389D87185C4FFF3EBD3FFF1EAD0FFF2EEDAFFF0EDDCFFF1EEDEFFF1F0
      E2FFF2F1E5FFF3F3EAFFD2D1A5FF5D5B17FB000000003F47668A3D414F690000
      00001237CDF3B2B5ABFFF3EBD3FFF0EAD0FFF1EDD9FFF0EDDCFFF0EEDFFFF2F0
      E2FFE5E4E7FF6463F5FFCFCE9DFF5A581DF80000000000000000000000003838
      3F55135ECAEA00C7FDFF00D2FDFF00D2FDFF00F6FCFF00FEFCFF00FEFCFF00F6
      FCFF0090FEFF3F3F507200000000000000000000000000000000000000000000
      0000525237BAD7D2A5FFF5ECD5FFF2EBD2FFF0E9D1FFF1ECDAFFF1EEDDFFF1EE
      DFFFF1F0E3FFEDEBDAFFD4D2A8FF5E5B18FA000000001818181B243EB3D30202
      02030E35D8F6C3C2A7FFF5EDD5FFF2EBD2FFF1EAD2FFF1EDDAFFF0EDDCFFF1EE
      DFFFF1F0E2FFEEECDBFFD6D5ACFF605F13FD00000000222223292E439DC22A2A
      2C351C3ABBEFD7D2A6FF8984E7FFE4DDD5FFF0E9D1FFF1ECDAFFF1EEDDFFF1EE
      DFFFF1F0E3FFAEACE4FFD4D2A7FF5C5A18FA0000000000000000000000000000
      0000101010123C466C9B04A7EEFA00FEFCFF00FEFCFF00F0FCFF00B4FDFF0A42
      DAF43F3F537B1919191D00000000000000000000000000000000000000000000
      00004B4B408EC9C48AFFF7EED8FFF5EDD5FFF2EBD2FFF0EAD0FFF0EBD6FFF1EE
      DDFFF0EDDBFFE6E4C4FFC9C690FF53502AE6000000000000000033448FB63F43
      577429409DD7C5C184FFF8EFD8FFF5EDD5FFF3EBD2FFF0EAD0FFF0ECD5FFF0ED
      DCFFF0EDDBFFE6E3C4FFCBC995FF545225EC00000000000000002D42A0C43846
      82A93F4868BBC9C58BFFA8A1E5FF9B96E4FFF2EBD2FFF0EAD0FFF0EBD6FFAAA8
      E7FF8F8EE9FFD0CFCAFFC8C58FFF52502AE60000000000000000000000000000
      000000000000000000002626272F2F538FC000D3F9FE00FEFCFF00D2FDFF009D
      FEFF104AD4ED35353A4C00000000000000000000000000000000000000000000
      000021212127A1A040FFF7EED8FFF7EED7FFF4ECD4FFF2EAD1FFEFE9CEFFECE7
      CBFFEAE5C7FFE7E3C5FFAAA953FF4D4C46910000000000000000383A41550933
      E8F53D3F47629B9A3AFEF6EDD6FFF7EED7FFF4ECD4FFF2EAD1FFEFE9CEFFEDE7
      CBFFEAE5C7FFE7E3C4FFACAB59FF4D4D459D00000000000000003D3F4C650B34
      E4F328282832A1A041FFF4EBD8FF5451F1FFBBB4DEFFF2EAD1FFEFE9CEFFA4A0
      DBFF1111FAFFA5A2D6FFA9A853FF4C4C46910000000000000000000000000000
      00000000000000000000000000000000000037383D521B70BADF00F4FCFF00CD
      FDFF009FFEFF045EEFFA3F3F5174000000000000000000000000000000000000
      000000000000525039B2C3BF81FFF8EFDAFFF7EED7FFF4ECD4FFF2EBD1FFEFE9
      CEFFEDE7CAFFC6C58CFF5A581DF106060607000000000000000006060607383A
      415503030304504F3CA8BFBC78FFF8EED9FFF7EED7FFF5EDD4FFF1EAD1FFEEE8
      CEFFECE6CAFFC9C791FF5E5C1CF40A0A0A0B0000000000000000000000000000
      000000000000515038B3C3BF82FFF1E8DBFF7C77EBFF5451F0FF5351EFFF6E6C
      E8FFB2ADD7FFA5A49EFF5A581EF0060606070000000000000000000000000000
      00000000000000000000000000000000000000000000040404053F475B8408A5
      E5F600C6FDFF009FFEFF1152D0EC000000000000000000000000000000000000
      0000000000000606060756542DCBB3B062FFE3DCB9FFF6EDD7FFF3ECD4FFE2DD
      BAFFB7B56CFF615F1AF321212128000000000000000000000000000000000000
      00000000000004040405555431C4B1AE5EFFE1DBB7FFF5EDD8FFF3ECD5FFE2DD
      BBFFB9B770FF65631AF52626262F000000000000000000000000000000000000
      0000000000000606060758562ECBB3B063FFE3DCB9FFF6EDD7FFF3ECD4FFE2DD
      BAFFB7B56CFF615E1AF321212127000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001414
      1517345D84B501A8F7FD0F59D3EE000000000000000000000000000000000000
      000000000000000000000101010244443F6E5F5E22E18C8B26FC8F8F28FD6563
      1BEE4E4C41A00808080900000000000000000000000000000000000000000000
      000000000000000000000000000142413E675E5C23E08A8925FC909029FD6664
      1BF04E4D40A60A0A0A0B00000000000000000000000000000000000000000000
      000000000000000000000101010244443F6F5F5E22E18C8B26FC8F8F28FD6563
      1BEE4E4C41A00808080900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000031323544314584BC000000000000000000000000000000000000
      0000000000007A7368C0BFAF94F0BFAD8FF0BFAC8CF0BEAA8AF0BEA986F0BDA8
      83F0BDA67EF0BDA57CF08E7E63DB000000000000000000000000383A3B513082
      A4E72B8EB6F16C9297F98AA5A0FA8AA49EFA8AA39BFA8AA299FA89A197FA899E
      93FA7C796ACE6F685BA75E5A5295000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001818181B4C4C5E7E45454D671818181B378BA8DA424444620505
      0506050505060505050605050506050505060505050605050506050505060505
      0506050505060505050600000000000000000000000000000000000000000000
      000000000000898275C4F8E3BFFFF8E2BBFFF7E0B6FFF7DEB2FFF6DCADFFF6DA
      A8FFF5D8A4FFF5D69FFFA99674E30000000000000000393B3C553A9AC3EE38C5
      FFFF34C4FFFFB1BFB2FFF8E3BDFFF7E1B9FFF7DFB4FFF6DDAFFFF6DBABFFF5D9
      A6FFF5D7A2FFF4D59DFF99896CD600000000000000004141475F4B4B5B7F1919
      1A1D000000000000000000000000000000000000000000000000000000000000
      0000212121274444ADCE2525D8ED38383B4C5194B3D727C2FFFF25ADDDFE4344
      4564000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000898277C4F9E6C5FFF8E4C0FFF8E2BCFFF7E0B7FFF7DEB3FFF6DC
      AEFFF6DAAAFFF5D8A5FFAA9776E30000000000000000437990D444C8FFFF40C7
      FFFF3CC6FFFFB3C0B5FFF9E5C3FFF8E3BEFFF8E1BAFFF7DFB5FFF7DDB1FFF6DB
      ACFFF6D9A7FFF5D7A3FF9C8C6FD800000000000000004B4B80B10000D6FF4444
      9FCE1D1D1D210000000000000000000000000000000000000000000000002929
      2A333E3EB9D72121DFF03C3C4054000000002728283051B5E6F52CC2FFFF26B0
      E1FF454747690000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000089847AC4C7BAA2FFADA089FFAC9E86FFAC9D83FFAB9C80FFAB9B
      7BFFAB9977FFB29F7AFFAA997BE30000000000000000468197D84CCAFFFF48C9
      FFFF45C8FFFFB5C2BAFFF9E7C8FFF9E6C4FFF8E4BFFFF8E2BBFFF7E0B6FFF7DE
      B2FFF6DCADFFF6DAA9FF9B8C72D900000000000000004A4A86B70000D9FF0000
      DDFF4444A1CE1D1D1D21000000000000000000000000000000003131333F3636
      C7E02020E0F13D3D42560000000000000000000000002223232955B3E0F331C3
      FFFF29B1E2FF494B4C7300000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000001746E63C6847C6DC9847B
      6BC9837B69C9B7AD99F2C2B7A1FFA49A86FFA49883FFA39780FFA3957BFFA294
      78FFA29375FFAB9978FFAB9B7FE300000000000000004A8197D854CDFFFF51CC
      FFFF4DCBFFFFB7C5BDFFB8AD98FF9D9280FF9D917AFF9D9077FF9D8F75FF9C8D
      72FF9C8C6FFFAA9876FF9C8E75D900000000000000003F3F445B2C2CBDE80000
      E0FF0000E4FF4444A2CD1B1B1B1F00000000000000003A3A3E502A2AD6EA2020
      E1F13D3D425500000000000000000000000000000000000000002222222859B1
      DFF137C3FFFF29B3E5FF4A4D4E77101010124C4C4493504E35D0514E32D84F4E
      42B43333334500000000000000000000000000000001BEB298F0F8E3BDFFF7E1
      B9FFF7DFB4FFD7C9B2FFBCB2A1FF9C9382FF9C917DFF9B907BFF9B8F78FF9A8D
      75FF9A8C72FFA29376FFAB9C81E300000000000000004E8197D85DCFFFFF59CE
      FFFF55CDFFFFBAC6C0FFB7AD9BFF9C9281FF9B917DFF9B8F7AFF9B8F77FF9B8D
      74FF9A8C71FFA9997AFF9C9078D9000000000000000000000000272728304A4A
      9CC80000E7FF0000EBFF4444A8CE1D1D1D213F3F465C2020E1F11919EAF53F3F
      455B000000000000000000000000000000000000000000000000000000002121
      21275EB2DFF13CC4FFFF4097AFF779781AFCB1B162FFC6C58DFFCCCB9CFFB8B7
      70FF86861EFF4F4E3FC70B0B0B0C0000000000000001BFB39CF0F9E5C3FFF8E3
      BEFFF8E1BAFFD8CCB6FFF5EAD7FFF1E5CFFFF0E3CAFFF0E1C6FFEFDFC1FFEFDD
      BDFFEEDBB8FFEFDAB5FFAB9C85E30000000000000000518397D865D2FFFF61D1
      FFFF5DCFFFFFBAC9C5FFBBB2A2FFA19889FFA19786FFA09683FFA09580FF9F93
      7BFF9F9278FFAD9D82FF9D907AD9000000000000000000000000000000000D0D
      0D0E525278A20606ECFE0000F2FF3F3FB6D71717EBF71C1CE9F44040465C0000
      0000000000000000000000000000000000000000000000000000000000000000
      00001F1F1F245E8DA2DE989A37FFDBD8B0FFECE9D3FFF1F0E3FFF3F2E8FFF4F4
      EDFFEAE9D7FFA3A347FF4F4D3BD20101010200000001C0B4A0F0F9E8C9FFF9E6
      C4FFF8E4C0FFD8CDBCFFC2B6A1FFC7B89EFFC6B69AFFD3C4ABFFFAEACFFFF9E8
      CAFFF9E6C6FFF8E4C1FFAC9E89E30000000000000000568497D86DD4FFFF69D3
      FFFF65D2FFFFBDCBC9FFC3CBC5FFBEC9C4FFBEC8C0FFDBD8C6FFFAE9CDFFF9E7
      C8FFF9E5C4FFF8E4BFFF9D917FD9000000000000000000000000000000000000
      000000000000484854711C1CE2F50000F9FF0101FDFF4B4B5775000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000070707087B7A20F8DFDBB6FFEFEAD3FFF0EEDEFFF1EFE1FFF1F0E4FFF3F2
      E9FFF5F5EEFFE8E7D3FF909027FF4343426B00000001C0B6A5F0FAEACEFFF9E8
      CAFFF9E6C5FFD8D0C0FFD6C6AAFFF7E0B8FFF7DEB3FFD6C6AAFFFAEDD5FFFAEB
      D0FFF9E9CCFFF9E7C7FFACA08CE30000000000000000598597D875D6FFFF71D5
      FFFF6ED4FFFFBFCDCCFF6CB9D8FF63D1FFFF5FD0FFFFA4C3C6FFFAECD3FFFAEA
      CEFFF9E8CAFFF9E6C5FF9D9381D9000000000000000000000000000000000000
      00000101010252526B900707F4FD0101FCFF0000FEFF4343ACCE1C1C1C200000
      0000000000000000000000000000000000000000000000000000000000000000
      00004444406EB8B66DFFF0E9D0FFF0EAD4FFF0EDDBFFF0EEDEFFF1EFE2FFF1F0
      E5FFF3F3EAFFF3F2EAFFBBBB76FF4E4C35DB00000001C1B7A9F0FAECD4FFFAEA
      CFFFF9E9CBFFD8D2C6FFD7C8AEFFF8E3BDFFF7E1B9FFDECAA6FF888071D0BFB5
      A5ECC19363FFD1985CFF89745ED600000000000000005C8697D87DD9FFFF7AD8
      FFFF76D7FFFFC0CED0FF72BBD8FF6BD3FFFF67D2FFFF7DB9CEFF9BC0C9FFE7DD
      C8FFBE9C75FFBD976CFF8F7C65DD000000000000000000000000000000000505
      050653537BA30101F6FF1616EAF74C4C587650508AB10000F7FF4242A8CE1C1C
      1C20000000000000000000000000000000000000000000000000000000000000
      000050503AB1D1CD9CFFF3EBD3FFF0EAD0FFF1EDD9FFF0EDDCFFF1EEDFFFF2F0
      E2FFF2F1E6FFF2F1E8FFCFCE9FFF5A581CF800000001C1B9ACF0FBEFDAFFFAED
      D5FFFAEBD1FFD9D3CAFFCFC6B8FFDACEBAFFD9CDB6FFD9CBB3FF9B9385DDCDC4
      B3F1CB9660FF9A7D5DE3000000010000000000000000608797D888DBFFFF84DA
      FFFF80D9FFFFC2D2D4FF75ABC0FF73BDDAFF70BDDAFF6DBBDAFF69B8D7FFD8D3
      C2FFCF975EFFC39463FF282828320000000000000000000000000C0C0C0D5353
      88B20000F5FF1313EEF94747536F00000000000000014C4C5E7F1A1AE0F64343
      A7CF1E1E1E230000000000000000000000000000000000000000000000000000
      0000535137B9D6D1A4FFF5ECD5FFF2EBD2FFF0E9D1FFF1EDDAFFF1EEDDFFF1EE
      DFFFF0EFE2FFEDEBDAFFD4D2A8FF5E5B18FA00000001BEB7AEF17D7A74B87D7A
      73B57D7972B5B8B3ACF9DBD6CDFFDAD4C9FFDAD3C5FFDAD1C1FFB8B0A4EE8C87
      82C47A6D5EC100000001000000000000000000000000658997D890DEFFFF8CDC
      FFFF88DBFFFFC5D2D8FFFEFBF6FFFEF9F1FFFDF7EDFFFDF5E8FFFCF3E4FFFCF1
      DFFFB99870FF5EA9C7FA111111130000000000000000131313154F4F92BC0000
      F3FF1212EDF9484854710000000000000000000000000000000038383B4C3434
      BBE04343A4CF1E1E1E2300000000000000000000000000000000000000000000
      00004B4B408EC9C589FFF7EED8FFF5EDD5FFF2EBD2FFF0E9CFFFEFEBD5FFF1EE
      DDFFEFECDAFFE6E4C4FFC9C790FF535029E700000001B7B3AAF11717171A0000
      000000000000918B82CBFAECD3FFFAEACEFFF9E8CAFFF9E6C5FF7B766BB50000
      00000000000000000000000000000000000000000000688A97D898E0FFFF94DF
      FFFF91DEFFFF82C6E2FF7CC0DDFF78C0DCFF76BFDCFF73BEDCFF70BCDCFF6CBB
      DBFF70CDF5FF5EB3D6FA11111113000000001D1D1D214747A0C90000F2FF1111
      EBF9484854710000000000000000000000000000000000000000000000002020
      20254C4C92BF515189B600000000000000000000000000000000000000000000
      0000202020269F9E41FEF6EDD8FFF7EED7FFF4ECD4FFF2EAD1FFEFE9CEFFECE7
      CBFFEAE5C7FFE7E3C5FFAAA953FF4C4C469200000001B7B3AEF11717171A0000
      0000000000004848476A5B5A5790F8EAD3FFBC9061FFBD9061FF6D645AB30000
      00000000000000000000000000000000000000000000637A84CAA0E2FFFF9CE1
      FFFF74A9BFFF5E8493FF5C8393FF5B8393FF598293FF578293FF5D96ADFF7DD9
      FFFF79D7FFFF5C99B2EA0C0C0C0D000000004E4EA0C90000F0FF0F0FECFA4949
      5674000000000000000000000000000000000000000000000000000000000000
      0000060606072D2D2E3800000000000000000000000000000000000000000000
      000000000000515039B1C2BE80FFF8EFDAFFF7EED7FFF4ECD4FFF2EBD1FFEFE9
      CEFFEDE7CAFFC7C58DFF5A581DF10606060700000001BAB8B4F14A4A49754646
      466946464669464646694F4E4D84F8ECD8FFE2A05EFF91775EDB000000010000
      00000000000000000000000000000000000000000000272727307294A0DAA5E4
      FFFF71929FFF797979FF7B7B7BFF7D7D7DFF828282FF848484FF6A8894FF87DB
      FFFF6BAAC3EC383A3A50000000000000000056567EA74141C7E44A4A58770000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000606060756552ECAB3B062FFE3DCB8FFF6EED7FFF3ECD4FFE2DD
      BAFFB7B56DFF62601BF32222222900000000000000019E9D9CE6C3C2BEF0C3C0
      BAF0C3BFB7F0C3BDB4F0C3BCB2F0C2B9ACF08A745ED400000000000000000000
      0000000000000000000000000000000000000000000000000000282828326884
      90D8759AAAF38199A3F8819AA3F87F9AA3F87D99A3F87C99A3F86B95A6F4648F
      A2E638393A4F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000101010244443F6E5F5E22E18C8B26FC8F8F28FD6563
      1BEE4D4C41A10808080900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000D0D0D0E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000808
      08095D51519980585AC1403D3E5C000000000000000043414164825959C76252
      549C0808080900000000000000000000000000000000000000003030303F3B3B
      3B55111111130000000005050506696A6AC2B0B4B2FF989B99F6707070CE4E4E
      4E811919191D0000000000000000000000000000000000000000000000000000
      0000000000000909090A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000001080808090000
      000000000000000000000000000000000000000000000000000000000000654A
      4AAEAE6565E8765758B4B63031F6393737513D3A3A58C56D6DF56F5555B0AB4E
      4FE7624343AE0000000000000000000000000000000045454573363636F74040
      40FF222222F6404040BF858786EAD2D7D4FFD3D8D5FFD3D8D5FFD3D8D5FFD3D8
      D5FFBABEBBFF8D8F8EEF626363B6353535480000000000000000000000000000
      00001B1B1B1FD3CFCFEA10101012000000000000000000000000000000000000
      0000000000000000000002020203A09D9DC6A09D9DC600000001000000000000
      000000000000000000000000000000000000000000001D1D1D21C9C3C3E41515
      15180000000000000000000000000000000000000000000000000909090AA44D
      4DEF3A39395000000000664849A9433D3D69463F3F6C655252A4000000003C3B
      3B52922B2DED05050506000000000000000048484883717171FFC3C3C3FFF4F4
      F4FFF0F0F0FFBCBCBCFF636463FF797B7AFF9EA19FFCC4C9C6FDD3D8D5FFD3D8
      D5FFD3D8D5FFD3D8D5FFD3D8D5FF919391D80000000000000000000000001A1A
      1A1DBCB1B1DCCFB7B7EA12121214000000000000000000000000000000000000
      000000000000000000006C676792CBBCBCE7DED2D2F05F5C5C80000000000000
      000000000000000000000000000000000000000000001E1E1E23D1A7A7EDB4A3
      A3D7141414160000000000000000000000000000000000000000000000008E4C
      4CE0302F2F3E00000000664849A6403C3C62433D3D65655252A3000000003535
      3546823335DC0000000000000000000000006C6C6CF8B4B4B4FFCDCDCDFFF5F5
      F5FFF5F5F5FFF4F4F4FFF3F3F3FFC8C8C8FF8D8D8DFF484848FF575857FF7779
      78FFA1A3A1FBC9CDCAFD8A8B8AD82020202600000000000000001818181BBAAE
      AEDAEE8F8FFFC6A9A9E412121214000000000000000000000000000000000000
      000010101012807979A9C97474EEA08E8EC8AD9999D0D59595EB7672729D0D0D
      0D0E00000000000000000000000000000000000000001E1E1E23C69393E7DF49
      49FFB2A1A1D6131313150000000000000000000000000000000000000000794E
      4EC05F52529E1717171A6F4647B83F3C3C60423D3D65705555B61D1D1D226554
      55A46F3F3FBC000000000000000000000000B1B1B1FDB3B3B3FFE2E2E2FFF7F7
      F7FFF6F6F6FFF5F5F5FFCACACAFFCDCDCDFFEBEBEBFFF3F3F3FFF2F2F2FFCECE
      CEFF898989FF868686FF3F3F3FFF444444710000000016161619B9AAAAD9F190
      90FFED8484FFCBABABE7605F5F8342424257414141554646465C615E5E84998D
      8DC2BD9C9CE0DA4848FDDC4545FE5E5B5B80706A6A96F96B6BFFEF8888FCC0AD
      ADDF979090BF5F5D5D804444445A41414155444444596765658CCB9393EBDC31
      31FFDE4141FFAF9F9FD41111111300000000000000000000000000000000423E
      3E5F955E5EDAB65658EFB72F30FB443E3ECB483F3FD2C96E6EFCBA6262F29053
      56D43D3A3B57000000000000000000000000B4B4B4FCBABABAFFF6F6F6FFF8F8
      F8FFF7F7F7FFD2D2D2FFABABABFFA5A5A5FF9A9A9AFFBEBEBEFFC0C0C0FFDDDD
      DDFFF3F3F3FF7AF8F8FF6EF9F9FF616161A715151517B5A6A6D6F47878FFF084
      84FFED8484FFEC8282FFEC8E8EFFEC9292FFEA8C8CFFE88181FFE46262FFDD38
      38FFDA2626FFD71A1AFFBE9999E1151515181E1E1E22C9A2A2E4F46868FFF084
      84FFEE8989FFEE9494FFED9999FFEC9494FFEA8989FFE77171FFE14848FFDC31
      31FFDA2626FFDA3A3AFEAD9C9CD30F0F0F110000000000000000000000000000
      000006060607464344655C4446CFA4A4A4FE575757FE594949C3413F3F5D0303
      030400000000000000000000000000000000ADADADF9E6E6E6FFF9F9F9FFF9F9
      F9FFF7F7F7FFC6C6C6FFC7C7C7FFC0C0C0FFB9B9B9FFB2B2B2FFABABABFFA1A1
      A1FFC5C5C5FFE5F4F4FFBDF6F6FF54545490B9ABABD9F96262FFF46868FFF084
      84FFED8484FFEB7777FFE86B6BFFE66060FFE35454FFE14949FFDF3D3DFFDC32
      32FFDA2626FFCF6F6FF1605D5D840000000000000000706B6B96E09292F4F084
      84FFED8484FFEB7777FFE86B6BFFE66060FFE35454FFE14949FFDF3D3DFFDC31
      31FFDA2626FFD71A1AFFD83232FEACA0A0D10000000000000000000000000000
      0000000000000B0B0B0C515151D4595959FFC4C4C4FF4E4E4ECC050505060000
      0000000000000000000000000000000000006C6C6CBFC0C0C0FAF2F2F2FFF9F9
      F9FFD7D7D7FFE9E9E9FFE3E3E3FFDCDCDCFFD5D5D5FFCECECEFFC7C7C7FFC0C0
      C0FFCCCCCCFFF5F5F5FFF0F0F0FF4040405F5C59597CD79797EDF46868FFF084
      84FFED8484FFEB7777FFE86B6BFFE66060FFE35454FFE14949FFDF3D3DFFDE3B
      3BFFCA8E8EEB716B6B98000000000000000000000000000000017E7979A6D4AB
      ABEDED8787FFEB7777FFE86B6BFFE66060FFE35454FFE14949FFDF3D3DFFDC31
      31FFDA2626FFD71A1AFFC88989EA504E4E6C0000000000000000000000000000
      000000000000484848A1E2E2E2FF717171F1999999F4D2D2D2FD474747920000
      00000000000000000000000000000000000000000000030303044E4E4E7AA3A3
      A3FBB5B5B5FBD0D0D0FEE3E3E3FFF2F2F2FFF0F0F0FFE9E9E9FFE2E2E2FFD1D1
      D1FFEBEBEBFFF6F6F6FFA6A6A6F712121214000000005E5B5B7FD5A0A0EDF084
      84FFED8484FFED8989FFEC9292FFEB8A8AFFE98686FFD58E8EF2C09E9EE2A093
      93C83332323F0000000000000000000000000000000000000000000000003A3A
      3A4AA69D9DCBC4ABABE3DB9D9DF3EB8E8EFFE88181FFE77777FFE25252FFDC31
      31FFDA2626FFC98A8AEB53515170000000000000000000000000000000000000
      0000212121277F7F7FE7E7E7E7FF484848A84A4A4AB3E9E9E9FF6C6C6CE11717
      171A00000000000000000000000000000000000000000000000044444461DFDF
      DFFFBCBCBCFFBCBCBCFFCACACAFFACACACFDB0B0B0FAC9C9C9FDD8D8D8FFE6E6
      E6FFF7F7F7FFDBDBDBFD43434361000000000000000000000000615F5F84D5A9
      A9EEED8484FFC6A9A9E458575777535252704A4949623030303C0E0E0E0F0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000111111133232323E4B4B4B64535252705E5C5C7FC69393E7DC31
      31FFCA8C8CEC5654547400000000000000000000000000000000000000000000
      00004A4A4AA8E6E6E6FF5D5D5DD9101010121818181B686868DEE6E6E6FF4848
      489B00000000000000000000000000000000000000000000000062626293CCCC
      CCFFBCBCBCFFCECECEFFECECECFFECECECFFECECECFF575757821D1D1D214848
      486F656565B2363636470000000000000000000000000000000000000000605F
      5F83D4AAAAEEC6A9A9E412121214000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001E1E1E23C69393E7CD8E
      8EED5A57577B0000000000000000000000000000000000000000000000000303
      03045D5D5DD7B8B8B8F943434374000000000000000046464685C8C8C8FC5858
      58D1000000000000000000000000000000000000000014141417B7B7B7ECBDBD
      BDFFD0D0D0FFDADADAFFBCBCBCFFBCBCBCFFCFCFCFFE2F2F2F3B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006967678EE2DBDBF412121214000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001E1E1E23E3D7D7F55E5B
      5B80000000000000000000000000000000000000000000000000000000002727
      2731989898F34C4C4CC5000000010000000000000000030303044F4F4FCC8484
      84EB1A1A1A1E000000000000000000000000000000003E3E3E568E8E8ECAB5B5
      B5EBD4D4D4FFBDBDBDFFBDBDBDFFBDBDBDFF7A7A7ABC00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005252526F07070708000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000F0F0F104D4C4C670000
      0000000000000000000000000000000000000000000000000000000000004141
      416D696969ED2A2A2A3600000000000000000000000000000000333333476969
      69F2383838520000000000000000000000000000000000000000000000000000
      00011F1F1F2537373749494949695C5C5C892A2A2A3400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004545
      457F474747AA0000000000000000000000000000000000000000000000004343
      43BB3F3F3F660000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002B2B
      2B380D0D0D0E0000000000000000000000000000000000000000000000001313
      13152727273100000000000000000000000064483FD7916D65E49B715EE69F68
      43E8A06743E8A06642E8A06540E8A06440E8A0633FE8A0633DE8A1633CE8A163
      3AE8A1623AE8995A30E59D7A63E4644941D00000000000000000000000000000
      0000000000006E5249C8B59086F1C79674F6C79572F6C89470F6C8936EF6C993
      6CF6C9926AF6CA9269F6B87145F1855C4DE30000000000000000000000000000
      0000000000000000000000000000000000002A2A2A33737350EE6B6B45ED6B6B
      45ED6B6B45ED6B6B45ED6A6A45ED6A6A5FB2000000003D3D3C54BEAE96F0BEAC
      93F0BDAC90F0BDAA8DF0BDA98AF0BDA888F0BCA785F0BCA683F0BBA57FF0BBA3
      7DF0BAA279F0BAA178F02A2A293400000000855649E4DA9C8EFFECC4B1FFF9F9
      F9FFEBF5EBFFE8F4E8FFE8F4E8FFE8F4E8FFE8F4E8FFE8F4E8FFE8F4E8FFEDF6
      EDFFF9F9F9FFEA965CFFF1B38BFF815649DC0000000000000000000000000000
      0000000000007C584FCDE8B7ADFFF3F7F3FF5CCF5CFF5CCF5CFF5CCF5CFF5CCF
      5CFF5CCF5CFFE7F4E7FFEB965DFF9F6A55EC00000000000000003030303F3B3B
      3B55111111130000000005050506686968C1B8BCB8FF6A6A39FFA4A4F0FF4C4C
      F8FFFAFAE8FFFBFBE8FFC9C9AAFF616152B3000000003D3C3C53F7E2C2FFF6E1
      BEFFF6DFBAFFF5DDB6FFF5DBB3FFF4DAAFFFF4D8ABFFF3D6A7FFF3D4A4FFF2D3
      A0FFF2D19CFFF1CF98FF2A2A2A3500000000855649E4DA9C8EFFECC4B1FFF9F9
      F9FF21BE21FF00B500FF00B500FF00B500FF00B500FF00B500FF00B500FF41C7
      41FFF9F9F9FFEA965CFFF1B38BFF815649DC0000000000000000524946888D71
      6BD38C593BD39E5C45F6E8B7ADFFF6F8F6FF98DF98FF98DF98FF98DF98FF98DF
      98FF98DF98FFEDF6EDFFEB965DFF9F6A55EC0000000045454573363636F74040
      40FF222222F6404040BF848785EAD2D7D4FFD6DAD6FF44447DFF1111FDFF0F0F
      FDFF8787F3FFFBFBE8FFC9C9AAFF676754C8000000003D3D3C53DFDFCBFF84B7
      C0FFF4DFBEFFF6DFBBFFF5DEB7FFF5DCB4FFF4DAB0FFF4D8ACFFF3D7A8FFF3D5
      A5FFF2D3A1FFF2D19DFF2A2A2A3500000000855649E4DA9C8EFFECC4B1FFF9F9
      F9FFD0EED0FFC8EBC8FFC8EBC8FFC8EBC8FFC8EBC8FFC8EBC8FFC8EBC8FFD6EF
      D6FFF9F9F9FFEA965CFFF1B38BFF815649DC00000000000000005C4E499ADEA3
      96FFDC7130FFAE5F42FFE8B7ADFFF4F8F4FF74D574FF74D574FF74D574FF74D5
      74FF74D574FFEAF5EAFFEB965DFF9F6A55EC48484882707070FFC3C3C3FFF4F4
      F4FFF0F0F0FFBCBCBCFF626362FF797B7AFFA9ACA9FC3A3A92FF7C7CF4FFA0A0
      F1FF1818FDFFC9C9EDFFC9C9AAFF87886DF3000000003D3D3C53B8D1D8FF2EC1
      FDFF69ACBDFFF5E1BFFFF6E0BCFFF6DEB8FFF5DCB5FFF5DBB1FFF4D9ADFFF4D7
      A9FFF3D5A6FFF3D4A2FF2A2A2A3500000000855649E4DA9C8EFFECC4B1FFF9F9
      F9FF2FC32FFF0DB90DFF0DB90DFF0DB90DFF0DB90DFF0DB90DFF0DB90DFF4DCB
      4DFFF9F9F9FFEA965CFFF1B38BFF805549DC4844436E4946456C6F4F43C5DEA3
      96FFDC7130FFAE5F42FFE7B1A6FFE8A578FFE8A476FFE9A374FFE9A272FFEAA1
      70FFEBA06DFFEB9F6BFFEA8A4AFF9F6A56EC6C6C6CF7B4B4B4FFCECECEFFF5F5
      F5FFF5F5F5FFF4F4F4FFF3F3F3FFC8C8C8FF9B9B99FF6A6A39FFFBFBE8FFFBFB
      E8FF8F8FF2FF2B2BFBFFAAAAB7FF666656BE000000003D3D3C53F9E9D0FFB5D1
      D9FF36C2FDFF63A9BDFFF4E0C0FFF6E0BDFFEDD7B3FFF5DDB6FFF5DBB2FFF4D9
      AEFFF4D7AAFFF3D6A7FF2A2A2A3501010102855649E4DA9C8EFFECC4B1FFF9F9
      F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFF9F9F9FFEA965CFFF1B38BFF7F5448DDA5695AF4E39F86FFAE572FFFDEA3
      96FFDC7130FFAE5F42FFE5ACA0FFDB6A22FFDC681EFFDD661AFFDE6417FFDF63
      13FFE0610FFFE15F0CFFE8813CFF9F6A55ECB1B1B1FDB3B3B3FFE1E1E1FFF7F7
      F7FFF6F6F6FFF5F5F5FFCACACAFFCDCDCDFFEAEAE9FF676733FFDFDFC6FFDFDF
      C6FFDFDFC6FF9090DBFF2727E6FF5D5D64DB000000003D3D3C53F9EBD5FFF9EA
      D1FFBAD3DAFF3FC3FDFF668F89FF9D9C4EFFB7B771FF9E9E54FF908460FFF4DA
      B3FFF4DAAFFFF4D8ABFF2A2A2A3500000000855649E4DA9C8EFFE39C79FFE084
      46FFE18444FFE28242FFE28140FFE3803EFFE47D3CFFE47D3AFFE47B38FFE57A
      35FFE57934FFE67429FFF1B38BFF7F5448DDA5695AF4E39F86FFAE572FFFDEA3
      96FFDC7130FFAE5F42FFE5ACA0FFDB6A22FFDC681EFFDD661AFFDE6417FFDF63
      13FFE0610FFFE15F0CFFE8813CFF9F6A55ECB4B4B4FCBABABAFFF6F6F6FFF8F8
      F8FFF7F7F7FFD2D2D2FFABABABFFA5A5A5FFA4A4A3FF979777FF90906DFF9595
      73FF9A9A78FF839B78FF69838FFF4747B6EF000000003D3D3C53FAEEDAFFFAEC
      D6FFF9EAD2FF9BA699FFC6C58AFFEDEBD9FFF1F0E5FFF4F3EBFFCDCC9DFF9589
      66FFF5DCB4FFF4DAB0FF2A2A2A3500000000855649E4DA9C8EFFE1936DFFDB6A
      21FFDC691FFFDD671CFFDD661AFFDE6517FFDF6415FFDF6312FFE06110FFE160
      0DFFE15F0BFFE56D1FFFF1B38BFF7F5448DDA5695AF4E39F86FFAE572FFFDEA3
      96FFDC7130FFAE5F42FFDEA79BFFC4A794FF968070FFC2A592FFD5B5A1FFD2B2
      9CFFD1AF9AFFCCAC96FFDC7A3BFF9F6A55ECADADADF9E5E5E5FFF9F9F9FFF9F9
      F9FFF7F7F7FFC6C6C6FFC7C7C7FFC0C0C0FFB9B9B9FFB2B2B2FFABABABFFA1A1
      A1FFC5C5C5FFE5F4F4FFBDF6F6FF55555591000000003D3D3C53FBF0DEFFFAEE
      DBFFFAECD7FFA4A255FFF0E9D0FFF0EDDBFFF1EFE0FFF2F1E6FFF1F0E4FF8F8E
      43FFF6DEB9FFF5DCB5FF2A2A2A3500000000855649E4DA9C8EFFE1936DFFDB6A
      21FFDC691FFFDD671CFFDD661AFFDE6517FFDF6415FFDF6312FFE06110FFE160
      0DFFE15F0BFFE56D1FFFF1B38BFF7F5448DDA5695AF4E39F86FFAE572FFFDEA3
      96FFDC7130FFAE5F42FFDAA499FFDEDCDBFF3D3D3DFFCCCCCCFFF6F6F6FFFBFB
      FBFFF3F3F3FFE9E9E9FFD8783BFF9F6A55EC6C6C6CBFC0C0C0FAF2F2F2FFF9F9
      F9FFD8D8D8FFE9E9E9FFE3E3E3FFDCDCDCFFD5D5D5FFCECECEFFC7C7C7FFC0C0
      C0FFCBCBCBFFF5F5F5FFF1F1F1FF41414160000000003D3D3D53817E7ABFAEA7
      9EDBFAEEDCFFB6B267FFF3ECD3FFF0EBD3FFF1EEDDFFF1EFE1FFEFEDDDFFA3A3
      55FFF2DDBBFFF6DFBAFF2A2A2A3500000000855649E4DA9C8EFFE1936DFFDB6A
      21FFDC691FFFDD671CFFDD661AFFDE6517FFDF6415FFDF6312FFE06110FFE160
      0DFFE15F0BFFE56D1FFFF1B38BFF7F5448DDA5695AF4E39F86FFAE572FFFDEA3
      96FFB8896AFFA46451FFDAA499FFDFDDDCFF3D3D3DFFCCCCCCFFF1F1F1FFFEFE
      FEFFF3F3F3FFE9E9E9FFD8783BFF9F6A55EC00000000030303044D4D4D79A4A4
      A4FBB5B5B5FBD0D0D0FEE3E3E3FFF2F2F2FFF0F0F0FFE9E9E9FFE2E2E2FFD0D0
      D0FFEBEBEBFFF6F6F6FFA6A6A6F712121214000000003D3D3D5372706DA42020
      1F25ACA69DD9A7A458FFF7EED8FFF3ECD3FFEFEAD0FFEFEBD4FFE8E4C6FF9291
      45FFF7E3C2FFF6E1BEFF2A2A2A3500000000855649E4DA9C8EFFBD896FFFC1AB
      9EFFA9978AFF8E7C71FFBFA89AFFC5AE9FFFCAB3A3FFC5AE9EFFC3AB9BFFC5AC
      9CFFC0A898FFCE6724FFF1B38BFF7F5448DDA5695AF4E39F86FFAE572FFFDEA3
      96FFCFB9ABFF9A6253FFDEA79BFFC4A794FF998272FFC3A693FFCCAC98FFD6B6
      A0FFD3B19CFFCBAA95FFDC7A3BFF5A4F4B95000000000000000044444460DFDF
      DFFFBCBCBCFFBCBCBCFFCACACAFFACACACFDB0B0B0FAC9C9C9FDD7D7D7FFE5E5
      E5FFF7F7F7FFDCDCDCFE4343436100000000000000003D3D3D5372716EA40000
      00002323232A8C876EE9CFCA93FFF7EED7FFF2EAD2FFEEE8CDFFC0BD7BFFA49B
      79FFF8E5C7FFF7E3C3FF2A2A2A3500000000855649E4DA9C8EFFBD8E75FFF0F0
      F0FF8E8E8EFF1A1A1AFFECECECFFF3F3F3FFFEFEFEFFF8F8F8FFF2F2F2FFF5F5
      F5FFEAEAEAFFCA6627FFF1B38BFF7F5448DDA5695AF4DC9C84FFA55835FFDEA3
      96FFCFB9ABFF73625DFF998885FFC9A994FFD2B19CFFCDAC95FFCDAB94FFB763
      2DFFC37449FF5749419146423F6601010102000000000000000061616192CCCC
      CCFFBCBCBCFFCDCDCDFFECECECFFECECECFFECECECFF585858831C1C1C204848
      486F656565B2363636470000000000000000000000003D3D3D53727170A40000
      0000000000002727272F8B866CEDA7A457FFB8B46AFFA29F52FFA09876FFD3C6
      B1FFD1C4ADFFD1C2ABFF2B2B2B3600000000855649E4DA9C8EFFBD8E75FFF1F1
      F1FF8E8E8EFF1A1A1AFFEDEDEDFFF0F0F0FFFAFAFAFFFBFBFBFFF3F3F3FFF4F4
      F4FFE9E9E9FFCA6627FFF1B38BFF7F5448DDA5695AF4CA9987FFB78E82FFDEA3
      96FFBE9E8AFF9F9189FFAB9C92FFCDBBAFFFD6C3B7FFD6C3B6FFD3BFB2FFCB74
      3DFF8D6C5CD01414141600000000000000000000000014141417B7B7B7ECBDBD
      BDFFD0D0D0FFDADADAFFBCBCBCFFBCBCBCFFCFCFCFFE3030303D000000000000
      000000000000000000000000000000000000000000003D3D3D53727271A40000
      000000000000000000002626262EB4AFA5DFFBF0E0FFFAEFDCFFD0C5B1FFF6E3
      C2FFF6E3C2FFC4B8A1F71313131500000000855649E4DA9C8EFFBD8E75FFF1F1
      F1FF8F8F8FFF1A1A1AFFEDEDEDFFECECECFFF7F7F7FFFDFDFDFFF6F6F6FFF3F3
      F3FFE9E9E9FFCA6627FFF1B38BFF7F5448DDA5695AF4CA9987FFC5B1ACFF674F
      49FFC69C81FFCC9D7DFFCD9E7DFFCB9A79FFBD835CFFC86A2FFF905130E45A4B
      418A20202026000000000000000000000000000000003E3E3E568E8E8ECAB4B4
      B4EBD4D4D4FFBDBDBDFFBDBDBDFFBDBDBDFF7C7C7CBD00000000000000000000
      000000000000000000000000000000000000000000003D3D3D53727272A40000
      00000000000000000000000000002525252DB5AFA7DFFBF1E0FFD1C7B6FFF6E3
      C2FFD1C2A9FC2B2B2B360000000000000000855649E4DA9C8EFFBA8B73FFD4D2
      D1FFA9A7A6FF737272FFD1D0CFFFD0CECDFFD6D5D4FFDFDDDDFFDAD8D7FFD5D3
      D2FFCECCCBFFCA6626FFECAF88FF52494692A5695AF4CA9987FFF1F1F1FF2B2B
      2BFFEDEDEDFFF2F2F2FFFDFDFDFFF2F2F2FFD5C3B7FFEC9458FF6D544CB80000
      0000000000000000000000000000000000000000000000000000000000000000
      00001F1F1F2537373749494949695C5C5C892A2A2A3400000000000000000000
      000000000000000000000000000000000000000000003D3D3D53A8A8A8D47373
      73A1727270A772716FA772706FA6777572A498948FCDFCF3E5FFD3C9B8FFDDCC
      B0FE3938384B00000000000000000000000064483FD7916D65E4926753E58C4F
      29E68D4F28E68D4E27E68D4D25E68D4D25E68D4C23E68D4C22E68D4B21E68E4A
      1EE68E4A1DE6955329E45A524D9905050506875A4EE9AC7D6CF2A77250F5A871
      4EF5A86F4CF5A86E49F5AC714BF5AB6F48F5A8673EF594684BD8212121270000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003D3D3D54C4C4C4F0C4C4
      C4F0C4C3C3F0C3C3C0F0C3C1BDF0C3C0BBF0C3BFB7F0C3BDB5F0B7B0A5F04948
      4768000000000000000000000000000000000000000059565296B2A48AF1B2A2
      88F1B2A185F1B2A083F1B1A081F1B19E7EF1B09E7CF1B09D79F1B09C76F1B09B
      75F1B09A71F1B09A71F156534D910000000042575DA53D404060000000010000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000404A4B7325BCD3F3348696CA4156
      5A891E1F1F240000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005C5A5692F8E4BFFFF8E2
      BCFFF7E1B9FFF7DFB5FFF7DEB2FFF6DCAEFFF6DBABFFF6D9A8FFF5D8A4FFF5D6
      A1FFF4D59DFFF4D39AFF59564F8E000000002F91A8EF45C9F6FF38AAD2FE348A
      A6EF3E6470B72D2E2F3C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000004251538821E3FBFF1FDEFBFF1CDA
      FAFF1AD1F6FF1DB5D8F62E88A0D3405960912323242B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005C5B5792F9E5C3FFF8E4
      C0FFF8E2BDFFF7E1B9FFF7DFB6FFF7DEB3FFF6DDAFFFF6DBACFFF6DAA8FFF5D8
      A5FFF5D7A2FFF5D59EFF59564F8E00000000298E9EE243B1D2FF4BCDFAFF42CA
      F9FF3AC7F9FF31BDEFFF299EC8FB2E7E99DF3E58609C1A1A1A1E000000000000
      0000000000000000000000000000000000004351538823E5FBFF20E1FBFF1EDC
      FAFF1BD8FAFF19D3FAFF16CFFAFF13CAF9FF11C3F5FF17A9D7F62A83A0D33F57
      60912323242B000000000000000000000000000000004C5658893F98B1F93E99
      B2F93E99B2F93E98B2F93E98B2F93D98B2F93D97B2F93D97B2F93D96B2F93D96
      B2F93D96B2F93B94B1F9375F6BBD00000001000000005C5B5792F9E7C8FFF9E6
      C4FFF8E4C1FFF8E3BEFFF8E1BAFFF7E0B7FFF7DEB3FFF6DDB0FFF6DBADFFF6DA
      A9FFF5D9A6FFF5D7A2FF5956508E000000002B909EE228C6E0FF54CAF1FF4FCF
      FAFF47CCF9FF3FC9F9FF37C6F9FF2EC3F9FF26C0F9FF1CAFE7FF1C94C2F52E6F
      8ACB404E54880505050600000000000000004352538825E8FBFF22E4FBFF1FDF
      FBFF1DDBFAFF1AD6FAFF18D2FAFF15CDFAFF12C9F9FF10C4F9FF0DC0F9FF0ABB
      F8FF09B4F4FF149CD0F42C7795CA2424242C3F46476FA4DBE4FC2B99C1FF039D
      D6FF029CD6FF029CD5FF029BD5FF029BD5FF029BD5FF029BD5FF029BD5FF029B
      D5FF029BD5FF029BD5FF1395BDFF3E515695000000005D5B5792F9E9CCFFF9E7
      C8FFF9E6C5FFF8E5C2FFF8E3BEFFF8E2BBFFF7E0B8FFF7DFB4FFF7DDB1FFF6DC
      ADFFF6DAAAFFF5D9A7FF5956508E000000002C919EE220E0FBFF44A4BCFF5CD3
      FAFF53D0FAFF4BCDFAFF43CAF9FF3BC7F9FF33C5F9FF2BC2F9FF23BFF9FF1ABC
      F8FF12B9F8FF3E56609900000000000000004352538826EBFBFF24E6FBFF21E2
      FBFF1EDDFBFF1CD9FAFF19D4FAFF16D0FAFF14CBF9FF14C8F9FF0FC2F9FF0CBE
      F9FF09B9F8FF07B5F8FF07B5F8FF40484B751F8094E3A6E5EFFF2CA0C4FF07AB
      DCFF06AADBFF06AADBFF06A9DBFF06A9DBFF06A8DBFF06A8DAFF06A7DAFF05A6
      DAFF05A6DAFF05A5D9FF1498C0FF21738CDD000000005D5B5892FAEBD0FFFAE9
      CDFFF9E8C9FFF9E6C6FFF8E5C3FFF8E3BFFFF8E2BCFFF7E1B8FFF7DFB5FFF7DE
      B2FFF6DCAEFFF6DBABFF5957518E000000002D939EE221E3FBFF26C9E2FF60C3
      E2FF60D5FAFF58D2FAFF50CFFAFF48CCF9FF3FC9F9FF37C6F9FF2FC3F9FF27C0
      F9FF6FD7EAFF327B97CE00000000000000004452538828EEFCFF25E9FBFF23E5
      FBFF20E0FBFF1DDCFAFF1BD7FAFF18D3FAFF15CEFAFF9FE5EDFF4ED1EBFF0EC0
      F9FF0BBCF8FF08B7F8FF07B5F8FF41494D7A1F8296E4A6E5EFFF2EA7C7FF0BB8
      E2FF0BB8E1FF0AB7E1FF0AB7E1FF0AB6E1FF0AB6E1FF0AB5E0FF0AB5E0FF09B4
      E0FF09B4E0FF09B3DFFF149BC1FF21738CDD000000005D5B5892FAEDD4FFFAEB
      D1FFFAEACEFFF9E8CAFFF9E7C7FFF9E5C3FFF8E4C0FFF8E2BDFFF7E1B9FFF7DF
      B6FFF7DEB2FFF6DDAFFF5957528E000000002D959EE223E5FBFF21E2FBFF4396
      A8FF6DD9FAFF64D6FAFF5CD3FAFF54D0FAFF4CCEFAFF44CBF9FF3CC8F9FF76D8
      DFFF48DC51FF6ED2D2FE1C1C1D21000000004452538829F0FCFF27ECFBFF24E7
      FBFF21E3FBFF1FDEFBFF1CDAFAFF1AD5FAFF17D1FAFF2CD1F9FFB6E0C9FF52D0
      EAFF0DBFF9FF0ABAF8FF07B6F8FF41494D7A1F8296E4A6E5EFFF31AECAFF0FC6
      E8FF0FC6E7FF0EC5E7FF0EC5E7FF0EC4E7FF0EC4E6FF0EC3E6FF0EC3E6FF0EC2
      E6FF0DC2E6FF0DC1E5FF169EC2FF21738CDD000000005D5B5992FBEED8FFFAED
      D5FFFAEBD2FFFAEACEFFF9E9CBFFF9E7C8FFF9E6C4FFF8E4C1FFF8E3BDFFF8E1
      BAFFF7E0B7FFF7DEB3FF5957528E000000002E969EE225E8FBFF23E5FBFF24CB
      DFFF66B5CBFF71DBFAFF69D8FAFF61D5FAFF59D2FAFF50CFFAFF65D3F1FF59D4
      6DFF00D100FF4BCF5CFF56696C9B00000000445253882BF3FCFF28EFFCFF26EA
      FBFF23E5FBFF20E1FBFF2ADEFAFF1FD9FAFF19D3FAFF16CFFAFF77D1B8FF78C6
      9CFF19C4F9FF0CBDF8FF09B8F8FF41494D7A1F8296E4A6E5EFFF32B4CCFF13D4
      EDFF13D4EDFF13D3EDFF12D3EDFF12D2EDFF53DCEFFFC4CED0FFC9D3D5FFCBD6
      D8FF50DCF0FF11CFEBFF16A1C4FF21738CDD000000005D5C5992FBF0DDFFFBEF
      D9FFFBEDD6FFFAECD3FFFAEACFFFF9E9CCFFF9E7C8FFF9E6C5FFF8E5C2FFF8E3
      BEFFF8E2BBFFF7E0B7FF5957528E000000002E989EE226EBFBFF24E8FBFF22E4
      FBFF3292A2FF3D818FFF3A808FFF3A7182FF4D93A9FF65D6FAFF85D9A5FF00C0
      00FF00D300FF00B800FF78C18FF713131315445253882CF5FCFF2AF1FCFF27ED
      FBFF25E8FBFF5DEAEAFFACEEC8FF41E1FBFF1AD6FAFF18D2FAFF7AD5B7FF2098
      21FF5DD3EAFF0DC0F9FF0ABBF8FF41494D7A1F8296E4A6E5EFFF50ADC1FF33C3
      D6FF17E1F3FF17E1F3FF17E0F3FF16E0F2FF41E4F3FF97CDD2FF97CDD2FF98CF
      D4FF40E3F4FF15DDF1FF17A4C5FF21738CDD000000005D5C5A92FCF2E1FFFBF1
      DDFFFBEFDAFFFBEED7FFFAECD3FFFAEBD0FFFAE9CDFFF9E8C9FFF9E6C6FFF8E5
      C2FFF8E3BFFFF8E2BCFF5957538E0000000030989EE228EEFCFF26EAFBFF24E7
      FBFF22E3FBFF20E0FBFF1EDCFAFF1DD2F1FF33494FFFA5DEDDFF89DBA1FF0EC5
      0EFF00D300FF0DB90DFF7DCF9FFF77817CB1445151852CEFF6FF2BF4FCFF29EF
      FCFF70EDE1FF4DDD5EFF48DC52FF42E2FCFF26DBFAFF75E1DFFF3DB949FF0294
      02FF74D7DEFF0FC2F9FF0CBEF9FF414A4D7A1F8296E4A7E6EFFFF9F9F9FF53C2
      D0FF1BEFF9FF1BEFF9FF1BEEF9FF1AEEF8FF1AEDF8FF1AEDF8FF1AECF8FF1AEB
      F7FF1AEBF7FF19EAF7FF19A3C1FE3A555CA6000000005D5C5B92FCF4E5FFFCF2
      E2FFFBF1DEFFFBEFDBFFFBEED8FFFAEDD4FFFAEBD1FFFAEACDFFF9E8CAFFF9E7
      C7FFF9E5C3FFF8E4C0FF5A57538E00000000319B9EE22AF1FCFF28EDFCFF25EA
      FBFF23E6FBFF21E3FBFF1FDFFBFF1DDCFAFF268798FF486F7AFF92D6D5FF0FC7
      0FFF00D000FF3AC43DFF6DD6F9FF495A60883D4141622D8188FF2E7074FF669E
      95FF4CC352FF00C000FF23D825FF80E6A3FF6CDE88FF1ABE1CFF00A900FF30B0
      35FF54D5ECFF10C5F9FF0EC0F9FF414A4D7A1F8296E498E3EDFFF2F4F4FFA1CF
      D8FF86C5D0FF86C4D0FF86C4D0FF86C4D0FF85C4D0FF85C4D0FF85C4D0FF85C4
      D0FF85C4D0FF82C3D0FF1590AFFB11111113000000005D5C5B92FDF6E9FFFCF4
      E6FFFCF3E2FFFCF1DFFFFBF0DCFFFBEED8FFFAEDD5FFFAEBD2FFFAEACEFFF9E8
      CBFFF9E7C7FFF9E6C4FF5A58548E00000000319B9EE22BF4FCFF29F0FCFF27ED
      FBFF25E9FBFF23E5FBFF21E2FBFF1FDEFBFF1ED0EDFF258B9EFF7AB6A2FF00C5
      00FF00CD00FF69B770EE4A565A7F4754587F393C3C5529F0FCFF32EEFCFF7BD4
      A0FF00A100FF00B300FF00C400FF00D300FF00C800FF00BD00FF09B509FF8FD0
      AFFF1DA4C5FF14BCE8FF0FC3F9FF414A4D7A1F8296E412D2ECFF12CFEBFF11CD
      EAFF10CAE9FF0FC7E8FF0EC4E7FF0DC1E5FF0CBFE4FF0CBCE3FF0BB9E2FF0AB6
      E1FF1585A2EF292929342727283101010102000000005D5C5B92FDF7EDFFFDF6
      EAFFFCF5E7FFFCF3E3FFFCF2E0FFFBF0DDFFFBEFD9FFFBEDD6FFE6D5BBFFB990
      64FFB99064FFB98F66FE504E4B7E00000000319B9EE22CF5FCFF2BF3FCFF29EF
      FCFF27ECFBFF25E8FBFF23E5FBFF21E1FBFF1FDEFBFF1DDAFAFF75DFB8FF00C7
      00FF00CA00FF6F8A6FC10000000000000000393C3C552BF4FCFF5AF1F4FF5AB9
      69FF009600FF00A500FF00B700FF44D84BFF55E164FF6FE18EFF84E3C9FF2BB9
      D4FF2197AFFF25798FFF266A7CFF3E4142662D6D7BCB12D2ECFF12CFEBFF11CD
      EAFF10CAE9FF0FC7E8FF15AAC6F639626AB03A616AAD3A6069AD3A6069AD3A5F
      68AD395860AC010101020000000000000000000000005D5D5C92FEF9F2FFFDF8
      EEFFFDF6EBFFFCF5E7FFFCF3E4FFFCF2E1FFFBF0DDFFFBEFDAFFDFCDB3FFE3A1
      5EFFE1A05EFF6F6356BA0000000000000000338F93DB2CF5FCFF2CF5FCFF2AF2
      FCFF28EFFCFF26EBFBFF23E3F6FF21C6DBFC279BAAEA559FA3E341C249FB00C9
      00FF3DCB41FA4345435D0000000000000000393C3C552CF5FCFF2BF4FCFF68EC
      E7FF74CC93FF0B9F0BFF00AA00FF86E3A6FF4CE8FCFF29E1FBFF1EDCFAFF1CD9
      FAFF1AD6FAFF19D3FAFF17D0FAFF3638394F30313241248593E0228A9AE4218A
      9AE421889AE4228293E13D424364000000000000000000000000000000000000
      000000000000000000000000000000000000000000005D5D5C92FEFBF6FFFEFA
      F2FFFDF8EFFFFDF7ECFFFDF5E8FFFCF4E5FFFCF2E2FFFBF1DEFFDFCEB7FFE09F
      5EFF6E6356B60000000000000000000000003638384D31B5B9EA2CF5FCFF2CF5
      FCFF2AF2FCFF2BC5CFF3434A4B763D3D3D52677167A055A555E503B903FF58BC
      58E959605984000000000000000000000000333434472DE1E7FB2CF5FCFF2CF4
      FCFF39F2FCFF7DE8DAFF7BCDA4FA83D097FB94D1C7F5289AA8E923A5B9F124AC
      C1F01AB9D5FD18C6E6FF17C6E9FF3638394F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005A5A5996B6B5B3F1B6B3
      B2F1B6B3B0F1B5B2ADF1B5B2AAF1B5B1A8F1B4B0A6F1B4B0A4F1ACA191F36B62
      56B0000000000000000000000000000000000000000030323241424D4E77424D
      4E77424D4E773639394E3B3B3B4C7B857BB5677A67AE667A66AA5E665E8D2020
      20250000000000000000000000000000000000000000414A4A723E7A7CBB3B84
      87C8379195D6309FA4E341494972000000000000000000000000000000000E0E
      0E0F0F0F0F111515151821212127080808090000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900200000100010000000000801400000000000000000000
      000000000000000000000000FFFFFF00FFFFFE7FFC7F0000FFFFFC3FFC3F0000
      FFFFF81FFC3F00000000F81FFC3F00000000FC3FFC3F00000000800180010000
      0000000000000000000000000000000000000000000000000000000000000000
      00000001000100000000C003C0030000FFFFC003C0030000FFFFF807F8070000
      FFFFFE0FFE0F0000FFFFFFFFFFFF0000FFC1FC7FFF00FFF3F300F01FF300F301
      E000E01FE001E000E000E003E001E000E000E003E003E0018001800180018001
      80838083808380838183818181838183C181C180C181C1818001800080018001
      8001800080018001C00FC009C00FC00FF807F807F807F807F88FF88FF88FF88F
      FCFFFCFFFCFFFCFFFFFFFFFFFFFFFFFFF00FC183FFFFFFC1C003C183F31FF300
      8001C183E01FE0008001C183E003E0000000C183E003E0000000C18380018001
      0000C183808380830000C183818381830000C183C181C1810000C18380018001
      0000C183800180010000C183C00FC00F8001C183F807F8078001C183F88FF88F
      C003C183FCFFFCFFF00FFFFFFFFFFFFF80000000FFFFFFFF80000000F000F000
      80000000F000F00080000000F000F00080000000800080008000000000000000
      8000000000000000800000000000000080000000000000008000000000010001
      80000000000F000F80000000000F000F80000000000F000F800200000000001F
      80070000878087FF800F0000FFFFFFFFFFFFF9FFFFFFF001FC0F89070000E000
      F80F80030000C000C00F803E00008000C18F800200000000C087800000000000
      C003800000000000C003800000000000C007800600000000C00F900E00000000
      C07FF00000000000C0FF000000000001C1FF013E00008001C3FFF33F0000C003
      FFFFF33FFFFFE007FFFFF3FFFFFFF81F0000FC67FC6780010000F803F8038001
      0000E001E0018000000080008000800000000001000180010000000100018000
      0000000100018000000000010001800100000001000180010000000100018001
      0000000100018001000000000001800100008000800080010000E000E0008003
      0000F801F83080070000FEC3FEF9800FFC63FFE1FE27F806F0010000F8038006
      C0000000E00180068000000080008006000000000000800E000000000000C00E
      000100000000C006000100000000C002000100000000C002000100000000C006
      000100000000C000000100000000C000800300018003C000E00F00FFE00FC000
      F83FFFFFF83FC000FEFFFFFFFEFFC03EFF078001FE7FFC41FC018001FC1FF800
      F0008001F007E000E0008001C003800080008001800100000000800180010001
      0000800180010001000080010001000100018001000000010003800100000001
      0003800100000001C003800180010001E0038001C0078003E0038003F01FE00F
      F00F8007FC7FF83FFC3F800FFFFFFEFFFFFFFFFFFFFFFFE7FC3F9E079E07FF81
      F01F80078007FE00F00F800780070000F00F800380030000C003800180010000
      8001800180010000800180018001000180018001800100038001800180010003
      8001800780070003C003800780070003F00F800780070003F00F800F800F0003
      F81F800F800F0003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7FFFFFFFFFFFFFFC3
      FFFF80038001FF0181C780018001F600008780018001F400000180018001D001
      FF0080018001D003000080018001C007008080018001C00781C080018001800F
      000880018001003F000880018001204F000F80018001000FFFFFC003C00304FF
      FFFFFFFFFFFFE4FFFFFFFFFFFFFFC5E380010000FFFFF9FF80010000000FF9FF
      80010000E007C9FF80010000E003C8FF80010000F000CC1F80010000F800C00E
      80010000FC00E00480010000FE00F00080010000FF00800180010000FF80100F
      80010000FF80A00980010000FFC0E00180010000FFE0012780030000FFF0F33F
      80070000FFF8F33F800F0000FFFFF37FC183C1FFE3FFE3FF8003C00F00000000
      8003C001000000008003C0000000000080038000000000008003001C00000000
      8003000F000000008003800F000000008003000F000000008003000F00000000
      800300070000000080030003000000008003C001000000008003C08100000000
      8003C083FFFFFFFFC183C0C7FFFFFFFFFFFFFFFF83820000FFF3FFF381020000
      FFE1FFE1C0020000FFC3FFC3C0060000F087F087C0060000C00FC00FC0060000
      C01FC01FC0000000801F801F00000000801F801E00000000801F801E00000000
      801F801E00000000801F801E001E0000C01FC01FF81E0000E03FE03FFC1F0000
      F0FFF0FFFC3F0000FFFFFFFFFC3F0000FFFFF0FF800100000000001F80010000
      0000000380010000000000008001000000000008800000000000C03C80000000
      0000C002800000000000C061800000000000C067800000000000C00F80000000
      0000E01F800000000000E01F800000000000F03F800000000000FC3F80020000
      8001FE7F80070000FFFFFFFF800F0000F03FF03FFFFFFFFFF03FF03FFFFF8001
      003F003FC0030000003F003FC0020000303F303FC002000030003000C0020000
      3FC03FC0C002000038003800C002000039003900C00200003DC03DC0C0020000
      30403040C0020000307F307FC0020000007F007FC0020000307F307FC0030000
      307F307FFFFF8001F07FF07FFFFFFFFFE007FF7FFFFF0000E003FF1FFFFF0000
      C001FF0FFFFE0000C001FF47000000008001FE27000000008003FC1300000000
      8003FC110000000080078009000000008007801B000000008003807F00000000
      8001C87F00000000C003E0FF00000000F007E1FF00010000FE0FF0FFFFFF0000
      FF0FFDFFFFFF0000BF8FFFFFFFFF0000FE7FFE7FFF80E1CFF87FFE3F80008003
      F07FFE1F00008001E07FFE0F00008001C000000700008003800000010000E003
      000000000000E003000000000000E007000000000000F007000000000000F007
      800000010000F80FC00000070000F81FE07FFE0F0000F81FF07FFE1F0000F81F
      F87FFE3FC000F81FFE7FFE7FFF80FC3FC0FF80018001F00FC00F80018001F00F
      C00380018001F00FC00380018001F00FC00380018001F00FC00380018001F00F
      C00080018001E00FC00080018001C00FC00080018001800FC00380018001800F
      C00380018001800FC00380018001800FC003800180018001C003800180018001
      F00380018001C001FF0380018001E001FCFFFFFFFFFF004000000000801F0041
      00000000C03F184100000000C07F00C100000000803F80C100000000800F81E1
      000000008007C1F300000000C007C22000000000E007C21000000000F007E201
      00000000F807E70100000000F803F78100000000FC01FFC100000000FFE0FFE1
      FFFFFFFFFFF1FFF1FFFFFFFFFFFBFFF9F80080018C7F0000F8008001807F0000
      F800800180030000F9C18001C0020000F9C08001C0000000F8008001C0000000
      F8008001C0000000F8C08001C0000000000F8001C0000000000F8001C0000000
      000F8001C0020000000F8001C0020000000F8001C0020000000F8001C0030000
      000F8001C0030000000FC003C00300000000FE7FF01FFC3F0000F81CF01FFC3F
      0000E000F01F843F00008000F01F003F00000000F83F000000000000F83FE000
      00008001F83F00000000C003F83F007F0000E001F83F847F00008001F83CFC7F
      000081E11018FC7F000081F80000FC33000081F00001C003000081F00001C003
      000087F10003C00700009FFB100FC00FFF9FFF3F800100008001FC1F80000000
      0000E01F800000000000C00F800000000000C007800000000000800380000000
      0000800180000000000080018000000000008001800000000000800180000000
      0000C001800000000000E001800000000000F001800000000000F80180020000
      0001FC0780060000F3FFFE3F800F0000C001FFFFFFC7F8FFC001FFFF8F87FC7F
      C001FFFF870FFC3FC00193138213CC3EC00191938013C000C00100010001C000
      C00100010001E000C001819380138000800189918011000081FF000000000000
      80FF800000000000E03FC98909890000F80FC8C9C9C90000FC07FFFFFFFF0007
      FF07FFFFFFFF000FFFC7FFFFFFFF801FF0FFDFFFFFFFF83FE03FFFFFFFFFF83F
      C01F80038003F83F808780038003F83F802180038003FC7FC301E607CE07F87F
      80C3E2078E07F87F800100070007F81FE00100000000FC0FC003E2008E00FC0F
      8007E600CE00FE07800100030003E007E00100030003E007F80300070007E007
      FC07FFFFFFFFE00FFE0FFFFFFFFFF01FFFFFFFFFFFFF81FFFFFF800180010001
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FFFFFFFFFFFFFFFFFFFF9C7FFFFFFFFFF80F887F81FFFFFF
      E007800300000000C003C00000000000C001C000000000008001800000000000
      800100000000000080010000000000008001C000000000008001C00000000000
      8001C00200000000C003C00600000000E003C01E00000000F007C01F00000000
      F81FE03F00000000FFFFF07FFFFFFFFFFC3FFFFFFC3FFFFFF81FCFFFFC3FFFFF
      F81F07CFFC3F8001F81F0387FC3F8001F81F0303C0038001FC3F0201C0078001
      FC3F0400E00F8001F81E4C00F00F8001F00F3F03F81F8001E0071E03FC3F8001
      C0070003FC3F8001C0038007F81F8001FC3F800FF81F8001FC3FE01FF81F8001
      FC3FF87FF81FFFFFF000FFFFFC3FFFFFC7E300000000FC3F83C100000000FC1F
      018000000000FC0F000000000000FC03000000000000FC018001000000000000
      C003000000000001E007000000000003E007000000000007C00300000000001F
      800100000000001F000000000000001F000000000000001F018000000000001F
      83C100000000001FC7E300000000001FF00FFC1FFFFFC1FFC003FC1FFFFFC0FF
      8001FC1FFFFFC07F8001FC1FFFFFC03F0000FC1FFFFFC01F00000000FFFFC00F
      000000000000C007000000000000C003000000000000C003000000000000C007
      0000FC1FFFFFC00F0000FC1FFFFFC01F8001FC1FFFFFC03F8001FC1FFFFFC07F
      C003FC1FFFFFC0FFF00FFC1FFFFFC1FF00000000FF3FF00F00000000FE3FC003
      00000000FC3F800100000000FC3F800100000000FC3F000000000000FC3F0000
      00000000FC3F000000000000FC3E000000000000FC3E000000000000FC3F0000
      00000000F81F000000000000F00F000000000000E007800100000000C0038001
      000000008001C003000000000000F00FFFFFFFFC000000000001FFF800000000
      0000FCF1000000000000FC63000000000000FC07000000000000FC0600000000
      0000FC0E000000000000FC07000000000000F007000000000000000300000000
      000000FF00000000000000FF00000000000001FF00000000000003FF00000000
      000183FF00000000FFFFC00300000000F00FFC1FFE3F8001C003F003FC0F8001
      8001C000F803800180010001F000800100000001E000800100000001C0008001
      0000000180008001000000008000800100000000800080010000000180008001
      00008001800180010000C003800380018001C003000780018001E00E000F8003
      C003F07F021F8007F00FF87F8FFF800F8001F00FF807F00F8001C0038007C003
      80018001800780018001800180078001800100008003000000000000C0010000
      00000000C003000000000000C006000000000000C00E000000000000C01F0000
      80010000C001000080010000C001000080018001C001800180018001C0018001
      8001C003C001C0038001F00FC03FF00F8001FE7FF7FFFFFF8001FC3FF3FFFF1F
      8001F81FF1FFC00F8001C003F01FC007C8338001C0038001F01F800180018000
      F00F800100008000FFFF800100000000F00F800100000000F00F800100000001
      F81F800100000001CC33C813000000018001F81F000086038001FC3F8001FF83
      8001FE7FF003FFE78001FFFFF80FFFFF0000E3FFFFFFFFC70000E3FF0CF9FFC3
      0000E201082000000000E001082000000000E001002000000000E20102200000
      0000E201020000000000E201000000000000E001000000000000E00108F80000
      0000E2010CF80000000080010FFE00000000807F01FE00000000807F81FE0000
      0000807F83FF00000000807FFFFF8001FFFFF00F8001FFCFF9FFC0038001FF01
      F0FF80018001FF01E07F80018001FE00C03F00008001FE00801F00008001E401
      000F00008001C001000600008001C001040200008001000F8E0100008001001F
      FF0000008001061FFF8000008001801FFFC080018001801FFFE180019E03807F
      FFF3C0038007E03FFFFFF00F800FE27F0000000000009FFF00000000000087FF
      00000000000083FF00000000000080FF000000000000C03F000000000000E01F
      000000000000E007000000000000E003000000000000E003000000000000E003
      000000000000F003000000000000FC03000000000000FF01000000000000FF81
      000000000000FFE1000000000000FFF9F801C001FFF80000F80180018FF00000
      F801800187E10000F801800183C30000000180018187000000018001C00F0000
      00018001E01F000000018001F83F000000018001F01F000000018001E00F0000
      00018001C10700000003800183C30000181F800107E30000181F80010FF30000
      001F80031FFF0000007FC007FFFF0000FF7FFFFFFFFFE187C407FBFFFF9FE007
      8000F1FC3F8FC4230000E1FC3F87E4270000C1F00F83E007000080000001E007
      000000000000F00F000000018000F81F000000038000F81F80008007E001F00F
      C001C01FF803F00FC003E1FFFF87E18F803FF1FFFF8FE187807FF9FFFF9FE3C7
      E07FFFFFFFFFE7E7FFFFFFFFFFFFE7E70000F800FF0080010000F800C4008001
      0000C000800080010000C0000000800100000000000080000000000000008001
      0000000000008001000000000000800100000000000080010000000080008001
      00000000C001900100000000C003980100000003803F9C0100000007807F9E03
      0000001FF07F80070000001FFFFF800F800100000000FFFF800100000000FFFF
      8001000000008000800100000000000080010000000000008001000000000000
      8001000000000000800100000000000080010000000000008001000000000000
      80010000000000008001000000000000800100000000000380030000000001FF
      800700000000FFFF800F00000000FFFF00000000000000000000000000000000
      000000000000}
  end
  object SynWebEsSyn: TSynWebEsSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Left = 616
    Top = 272
  end
  object SynWebPhpPlainSyn: TSynWebPhpPlainSyn
    ActiveHighlighterSwitch = False
    Engine = SynWebEngine
    Left = 528
    Top = 272
  end
  object SynGeneralSyn: TSynGeneralSyn
    DefaultFilter = 'Text Files(*.txt,*.*)|*.txt;*.*'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    DetectPreprocessor = False
    SpaceAttri.Foreground = clSilver
    Left = 264
    Top = 328
  end
  object SynJSONSyn: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 264
    Top = 272
  end
  object icBrowserImages: TImageCollection
    Images = <
      item
        Name = 'Browser\Open'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002884944415478DA63FCFFFF3F032580911403BE7EF96CF1FEED1B7519
              79C5852419F0EDDB579323EB16AC94D4B15060F8FCEE878E8D8B042323E367B8
              01BF7EFD52FFF3FB371F3B3BFB732666E6E740C9BF30CD27F6EF98CCC1C199C6
              2E20C6262C2CCCC0C8C8C070EFEAF9C9E60EEE7960039634E75FB58BCA5667E7
              E266FAFAF1FDDFB74FEE7D7FF5E4C1633D7BEFAEEBA70F8628E85B797371F330
              7EFEFC998187878781939393E1C6B19D6FAD7C22C58016FD635CDC5A78CA31BE
              D8F4D3A74F0CFFFEFD03DAC008F11BC37F065626C67F1C3C7C4CEFDFBF67E0E7
              E767F8FEFD3B03C815ECAC2CFF3F7E789F000C8B458C87B6AD9D2220A990CDC2
              CEC9F0F7EF5F06262626B8DF41B681C4400064FB870F1FC0B4989818C3C1D573
              1EB8466729327EFEF4C9F2C8D6D57BD5CC1C387FFCF801560C0A17904BB8B8B8
              C07C565656862F5FBE80F9DFBE7D6390939363F8F4F6E55F5E21517346A0B399
              97B4E4DF754C2C93077903A411668088880828801940068368414141868F1F3F
              82BD232424C4B077E59C93E05858D1537DC03632CB1E6400333333D8D9200C72
              2EC82090D34186810C92959505BBEACECDEB0CC717F7DE041B70F5DCC98AEFDF
              7FB4730B8981C300A41918A5605BD9D8D8C01814C09292926043F66E5CF1F3EB
              939B67A2CABA82C106FCFEF54B65EB8289A79D23330460010852F8FAF56B0648
              DC333208080830805CB86D6ED7073563FB79E6CE5E2540F1FFF09438AB2CFEB6
              8C89930A1B0717032B10F3088933F00335810C006904A9DB35B3E9956B5259AD
              A2AAC62C8CA40CB451E7E3FBB76A3FBF7FE7FFF8F695D4832B678C3EBE7C2CF9
              FDE30719112D13E917E70F3E89A89E9426282CB293A4CC049417B877EB7AA4B4
              BCD2610E0E8E2B14E5466C0000C1AE36148E700A4B0000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000004BA4944415478DACD950B4C9B5514C7CFD777A1EFF2ACCAAB94874009
              08C9EC16262A5BE29088802E48C29C3353B3B9B92931B24C136573EE695018BA
              69E68B6C4CCDC698A073A4330A5182A458584B610C28EFD2D2D202A50FBF7323
              CDC46DC6A8C94EF2E57BDC7BCFFF9CDF39DFBD94DFEF87FFD3A83B5E805E2F1E
              30F694FED67AA1B8606BC5068AA216FE13017A1DBFAB4D5B693376EC4E7CB098
              CB6533C1EE987B3E3621B9EE5F0BCC39EC9ACB27AA9A320BB78ACD1353944020
              00954A0517DFAD9C7AEC9583617F11F0F97C72C7AC2D7DDEE5943398AC259158
              32C0E5F1AED2E9BA57444DFDA26D39C8713BB7B3EF4AE2E0DA848404989C9C84
              858505A09C564F489472BD582ABB1C10F8BA76FF39119FBB4EB92A8FCD138819
              EE0597DF3169F68E1B3B7D36EBEC60CA438FD7A952D23FA5C52C97CE7C549F92
              BDBAE4FA8495111B1B0B2C160B4C26134444448042A1200E9B8EEFEB2A7CF18D
              8C80C091D255E6BC8A5A85482A83919111A01D910106830191919110C465FB7B
              B5E796CCBD5D33B94F5784F50D8D32D2D2D260626202E6E7E7213939190C0603
              6607494949A06F3DEF4ECD2D486173382622F0FDD9532716171737AF797423B5
              92F5E8E828CCCCCC90E7F0B050E0F2F8101C1C0C636363E48E410C0F0F13111E
              8F0756AB15A41209FCD0587F3AEF89CD1B89C0D4C478FE576FEFFC3CFFE5C322
              B3D94C3270BBDDC064324954788F8F8F273830628C5C269311612E970BD1D1D1
              A0D3E94810A9A9A9647EDBE9EAC59CB25DE1B4AF598A2EB0F09D828491A76A5B
              4562B19814CBE572019BCD06A150081E8F078C4623C8E57222121A1A4A902077
              CC6060600032323260696909C6C7C721262606E69D0EFF90C9B0372D5BF316E9
              A2BA9D25FACC92EDF70AE5616422A64F6303BBDD8E1D46A24341AC098AA330DE
              F1522A9524034484A896ADA5668F237FC7012911E8B8F25DD535DDCFAF6A0A37
              31B0D05EAF97A48F62D8E38801B1A11822B0582CC4495050100C0D0D41666626
              4186CF7171718024A687FBBD4CBEE8492230E770DC7F72475153F99133527470
              A3391C0E181C1C24CF8805C7B14E28EE743A213131113A3B3B09BAA8A8A8C03A
              EBF4245CACDEA32702F4C53AB04169293AD62CF2D1EF88061D20E365E3F3F924
              3A1CC33A619658979E9E1EC8CACA821B03335CED05EDF1BD738290087D60ABF8
              62FF6EAD42BD26E7BE9C87C9FB3212648B86BCFBFBFB0973FC8E59608BA6A7A7
              071C6346DAE646E8FBF2F0AC66CBEBA734EB0B770504FA7A74DBDACE7E7028EF
              D94A2E22C142630761267F6449BA08F71C9BCD06D3D3D3A056AB03CEB1835A3E
              A9F6DA3A9B6D5B3E6C2D95CA43BEFDD3664747AC7A6F534E7BD9B1F332C47133
              5BE64E7EBCF0F0C0F7EEEE6EF8F1B3A38B4C9765F8B99A0B6B695CA337DD4D0F
              15A9A7D4CFEC0B11CB4349B41C0E8774136683DDB45C13E48F6D8B73DADBDBE1
              D7FAA34E494898BEFCCDBA3C1A9DFD96DBF595C686F77FAAAD286370785E9F67
              89C9E0F07DAC6089971124615302A9501E9B4A299232E09EE81822AEEFD681A1
              E1903D21B7F89B47CAB795D1CE3D2BB3BEED79408F09682C0A9B653A65ECBA29
              CDD87669F5356D43364B14EEE7DD9D2C73F577CC3EF042554DD6DA75AFDDCAC7
              3F3E70F04CA0FF1B8DB6E1E44BEADCFCA6A838D5C7B79B7FE71FFA7F67BF0385
              A15B8FF7EA1B940000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000007044944415478DAED570B505465143E77977DC02E01B22C2C8F580481
              62615C8161254D4791C54766253A15A1898E8E3AA6CEE864936689AF344B34CA
              7C66AF2125F3816CA83C1479284A3C5C140294370B2C0BEC82CB63B77BFEBC3B
              2098359335359D999DDDB9F7BFFFF9CEF77DE7FC7729B3D90CFF6450FF037852
              000C7A7DD8B50B3F2EAEBB726A5ED4FAFDB14E2E92D37F0B80EE6E43F099844D
              899E52696880F2558A67238413DBDE52C7BC7F20E0890230994CA2D42FF77D21
              30E9E78444AFA40A8B8AC16834C2C48913A1E064A279EC9C250A814078ED8900
              D069DB22CE6D5F9E3C7DED6EDBC6B60EAAAEAE0EC2C2C240201090FBBD3DDD90
              79F2C8E959716BE78C0880FEB00CFA2E85AEADD55BAFD38A81A2CC427B078D83
              485C6E6323B845519461A4C4F85CFEC573BB0C95056B9E5BB881CACCCC040F0F
              0F1833660CB97FF3E64DE8EBEB23602EEC5B678A5ABDE769369B5D3F04C0AD1B
              79EBF38EED58E7375139CAC5570E3CA11D85370C5A8D597B574D3557149A0DFD
              1CB56C5AF4097F7958328FCFBFF52039F7F4819DA764C18A28275F39EBD2A54B
              101111017C3E1F2A2A2AA0B6B616828383C1CECE8E24EAD0D49BEF5696EF0C9D
              A4DC3004C0CED97EAD7E91AF3B4E895DFDBB3477B53599EF5CFC9EAABF5B5515
              3A6FE5BEB2ABAAC8E0C951517C913B959B9B0B4AA592ACCBC8C818C6824C2603
              2E970BA736C775CFDB7A5C4433DA630170EEF0C749F76E64CD7D63C771D6EDDB
              B7A1ADAD0DACADAD7FD3AEB7171E48042C168B6CEAE8E8089AEA32534F5B23B8
              CB27B1B2B3B3213232120C0603A00453A74E252C141414405757178C1B37CEC2
              424DD15593959D6499D4D7FFA005404D55C582AF57447EF6FAE799D60E0E0E8F
              640041949797434B4B0BE03A17171772CDCDCD0DDADBDB2127270766CE9C093A
              9D0EF2F3F341A15058126361FEFEFEE477D27B4B6A63B61DF3A45920EEA7FAFB
              FBDDE243ADEA947B33C0D9DD93E8C7E170C0CACA8AB411C380B3B3338C1E3DDA
              02E8FEFDFB24416767275CBF7E9D548E89B45A2D8487879335858585D0D1D101
              010101E0E4E444AE15A57C631A3369B6D27E94E3454B17242C9C5CED12324D3A
              7DC1AA4732D0D4D4049595951639B04294A8B5B515BCBDBDA1B4B49450EFE3E3
              4392220B6842946C700CD05D91F2C58EACB96BB64CB600C84E3999703D69DFD2
              857B7FE46A341AA8A9A92149D0383D3D3D74575220140AC1CFCF8FB033303040
              EE63B5AEAEAE446FACD0D3D313CACACA88240C0BC5C5C5649D5C2EB74872F9D0
              16F3F371EF3ECBE5F16E1300AD9AE6599FCF7B3639FA400E17371C29F47A3DA1
              98910525914824440A64C2CBCB8B00C12468D6868606B23E2828084422D190BD
              BA3BB5E6A2ABE90727BDF8DA526610D97E309ED73E6163123B64C214686C6C24
              D4D2E315688F90B642D30D0E640AAB463670E2E5E5E581BBBB3BF920109403DB
              0F43AD5613F3222BC820C6D9EDCBFA5EDE74586C19C547DE5E50005C9BE039AB
              B742737333418D9B6075F5F5F5E41A0222BAD19260659818E945E7A344983C2D
              2D0D42424288F60C0B6842646C70D4A90B4CBDC05D6F0150947779E34FBB57BD
              13BB5FC56766F8A30281A0B6C80C56C430919A9A6A99860FB33038D023AA035B
              40ABCE6FB000D077758DDF13E974E5854FAFB2DDDC3DC804C3C04D5006A61D99
              F5A8332666AEA5A7A7936988E64416424343877500AE4346B28E7E08BAFCE401
              E5D61F3EB200A0BFD9DBA6493A82DE8C174C9E3D1F1E173894D0078181814402
              EC00FAA001954A6561E161135FC94C873B2776F7F53657F52DF9BA30D6D9D52D
              79C8719CBC77734A7D45F1F4055B8F52252525F4CB453719CB482F9E6A0CFDE8
              0F5F5FDF21948AC562387FFE3CCC983183B03038AAABABE1F2B9EFA125EDA051
              F44C58F9A25DDFBE68C5E1545BE60013BF94952E3FB9FE95DDF31354D60FB7CE
              E04047E3C44430D88A52A994B0816399713943394A79ED8743A0CBFEAE3F62E3
              5789E1D35F5E479BB89759330400ED78DF7805EF4ED4271910280F7EAC0C8303
              4732336E31703EE0E154A53A6EEA284C312DFAB634CEC3CBFBF8C3CF0D7B23FA
              287A6C935831CB3974660CD1145B8EC7E391A988DF786DA4C076443618BDD354
              A9507326A1DFDCD9A25F91543443686B9B3BD273C3005C483A7CB4E0587C0C67
              949B899EFA6C8AC365B3780260F105C0B6B10781440A8E1E3EF094BD03997A58
              B5BDBD3DF1888D8D0D3907D252CE42C3D9BD46A14852BBF4B3F311F4C176EF51
              CC0D034053272BCECD88C1DFFDBD466EA7A641DCA9A9137736DE73D25597B8EA
              6BD5225A7C8A2FF1E961DB8AB81C671FAB51CF28C047369600B89193059A94FD
              46675978E9C2ED47A7D1866CFF3DE9FEF44B29BD9ED36B34FAB63437C8AB4B6E
              84FD9C9C384B5B9429B596CAEE733D02F9DD65578C3ECAD8ACE8B5F12FD1F275
              3F6EBFBFE4AD981E54D292FCCB6FA6EF59BD2C287A556AC4FCB8C574F2FE3FF2
              EC7FF7AFD9BF06C0AF4FF29EA9500BF6480000000049454E44AE426082}
          end>
      end
      item
        Name = 'Browser\GoForward'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002C44944415478DA85535D489351187E3E37DB9C8C59CA6C2A4ADD585B
              A6A64118851133AF34B729D38908A224627AA15D8C79230A82483F4817D9B08B
              B11FD9D85584BF84257491FF8A6606F633BF8995E2F6EDC739D7D944FD44A4F7
              703870CEFB3EEF73DEF779A9502804B6310C7373707E5063A36D7766FDB3C9D8
              033239990E55AA6ABC20BBC0102B889D64FB538700E48C317F34F7D4BA6A6BBC
              7CEFB9386E1C28B23C410FBCFBDE888F9016FAF5C9FA3E55BEAA95A228DF1140
              3858F75667EF12743D90C64891C64BC34E7007CE8013F42E0D77D07D9C720BD0
              FAB5831D151D0FC3201100D307D3CBEA4075BD2A5E053E878F55DF2AA6DDD390
              C7CB21BF2047FD977A4291C59B06062403BDA5F74B1B2937E3CE4D7C9F38519C
              5C7C2E899784A1AD21CC79E6A0102B60B9660197E2A27CB11C66A7F91880D445
              B828F4D38FE8DB9475C2FABC39D8FCB8E1620346764630BA3D8A12714924389A
              8A469FA30F75CB7538653F01FB65FB534A6D524FED5ED9CD4EE7A7A39BEE4651
              42D1516683D380C69546B03B1524CBBD476AF21BD06C6A262969BF74232F374F
              3CCD4C2355907A94F92C5B629620FD2405B6818C850C27257B2D7332E94CE29A
              7F0D65896530CA8CE0509CFF03FC25FA58CAA4A98A371593C614E38D43872A49
              15FAAFF6238A8A8269C304ED37ED098040280087CF11E984669B7CC1366E7BA6
              74299BC03B766233E9F9D18396AF2DA7A9AC9022CA4811491B732446C984EB92
              8BC77E6733512FA861D9B01C3FFE0144CB22DF7AEB7A5E444896314BAF7A43DD
              00F1C9246190C2F842542E56623FB47F701916E577C09A617DA1BCA76C3A9432
              BFCDD866EF0C76164640CE6A0291319C804EA47BD75ED95E42A4EC670F13DF3A
              66EDAE99ABA9752590EF08C825170712F693ED22B437453EFD2DFD2B45BEE249
              38F8C4341E8DB387C919FE3CACB1CE5BEFCE6CCDA48401B2CE67FD525D27E39C
              5B60100804536CFF7F9F473F33FEF34CB80000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000004A74944415478DAAD566B4C9B55187E3E7A8132B65AA0308162501097
              B6745CB6B0648A23654B970809898A108662BC6D598690F9C38D1F2A5D368241
              25319168580892700D9730E6481843452185CD52220CB28DD2520AD416684B5B
              7AF1FBDA4206A5A2C4D3BC7FCE39DFF39CF7796F255C2E17FC2D9BCDF6A27C5A
              FEDAED99DBAF4C9A26A3A74DD3117002F1ACF8A523078EA8242F49EE091384DD
              4C2673DA1F06B11BC1C6C6465CD7EF5D2517162F146B9FD186F87D8109E06838
              EB9531954DE7B2CE4949A2993D099EA89EE44B7E917C35C99DE4120481E08060
              70681C30694C385C0E181D46AC3A56B1E1DCF07C407A0403C057F3B5B75EBF75
              29363AB6C92FC18862E4B2F891F80B63883130941E8A445622D83436CC4EB31B
              983283C300BD5D0F9BD3B6FDA924095BCDB6F4BDDA577E2CE958950FC1AC6AF6
              2DE1A8B0CE14620A1485882060096073D9DCA03A870E2A8B0A6BAE35F0027950
              1815BB4BA62349546C8BFC4DF9DB9B9EB80948CD5F486A4DFAED61C4436E063B
              03826001D69DEBD0D975909BE598B5CE82207F2D82168843C538FBC7590CEA07
              7D091CA42D9072CDF1B5F73FB97F82C1603C7613DCECBBF97D31ADF8DDA3ACA3
              38CD390DABCB0AA55589FED57E18EC06D0093ADA846DC80ECF76E3AC39D6903A
              928A69F32EC9B34A9A06A88FABAF2D94147E4058ADD684A8F6A807B6C3B6E0F3
              87CF834130A0B429D1AE6B77CB430FA0A355D08A1C6ECE16469DA60EC57F1603
              BB6538157435C07DCC35A9AFAA45C4B07CF872FA527A65263B13E2436277001B
              740D98B7CEFF77F0CDB5EC219165C9CA0869BBB4A3FC50794E497409B8342E06
              5607D0B7D2870022607FE09B32A90069A8B493C8FB216FAC2DAE2DF946EC0D2C
              DB9751B350E30EF0D39A53CBEC30A351DB08A7CBE917B74A59E5898BD9435060
              291825526B52550ABE22BA8257812E7D17868DC36816346F7BF9BF5DA7C64E61
              403F00AC7B088ECF1F9F23D2BE4E9B1BE58FC69CE19CC1DD95BB6E697A45BDC8
              E064EC9FC0EB41BA265D49E4D7E68F36C636A6100C029B45C7A2B17C48A8CA1D
              340CFE2341D94C19E46B724F0CC82017DA0B6584B449DA7125F04A0E76B4B420
              5A107A443DC8E4646EED552BAB513A5DBAB72BDE2CBA1679AD83908DCB4AD3E4
              695F22D2F7DEBE48EC70171AE681B19CB18F3D85F66DD403DDF3BA601CF81F48
              56E06E175C1559689F918546E95EDF5B5F5BB454F41E22C84386EF373B636275
              5A211A1161CA34B5FD22D560B51E6B486AF8AE4052F0E156B34BAE4EFE752266
              2212E1E4059A7F4F4EB24F22773C173DCB3DDB2F38BCDA2F0122BD4823BB2A3B
              41A7D367B7DAB55AA3CEE537F27F5C797625081C7283B9BB27C90793316418DA
              7E40CD1E3D3CED7A816D19FF68BC8817CD6BA68EB60D1C32E065E29FC4152BE1
              24099BDCA0322B608F8C31C29396D4C0D1B22DFD79FD9FA60852AA378F7D46E6
              9C7AEE0D499DE49B8930522E2AE82CD202BDB121E0E943941C56782A969CCB58
              2365318A34DDEF775FE245F15A9EC6DB75E8DBEDF6E73AEF75965E1CBAF88EE6
              90E6A09B80EEF5C6E54D454A160B10F65798F97AFAF5E62249D1E7D480D98945
              ECF1B7257E7C6A3CFB8EFCCECB8A4545EC8C61864B1124701216859142655652
              D6CFC244612709FCC81FC6DF7CE84165A3875A8F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000006B44944415478DAAD970B4C935714C7FF5F2985D2166B75B391A56C53
              10C1525ED9349953A7A37138D4B84C5996010BEEA50151D9E28CD9B264BA8941
              C168A26E4EA6F111B36593879B89E0648B3A07CAA32234F3C926AF4A694B0B6D
              BFAFBBF7EBD7C202E5B1719A93F6BBBDDF3DBF7BEE39F79ECB783C1E8C47483F
              A5A1D5F0C61FF7FE48BAD9797356ABBD55DDEE680F0707A843D5961859CC23DD
              93BA3F539E4DA98D8D8E3DC5304CEF78C665C60270BBDD4F9FBC78F2E33D0FF6
              BCDAF86CA33A7047A203441D80F69EB6BD405B702E233563A7582CBEFF9F0048
              BBB4FCD7F2CFB6DFDDFE6663C4A0E12026086476422780F370E0188EFFED6B83
              9D681F10F730AE63F7F3BB4B97BFB8FC13F24EFFB80158969DB9EDF4B6D2C219
              85CB406C891811424421500429A0102920099280E338383D4EF473FDB0B136F4
              717D7C1B0436B8885A895A803C57DEA53D397BB246F2C63000ABD5BA60EDD9B5
              47CF479E8FA1330D1385411DACE655C248C0322C06B801385807EC1E3BEC9C1D
              36B71780F5B0FF1E9DF3028044435A775AF3A90F4EBDAD902BAE060420EBAD59
              737C4DE539CDB938B1488C27C44F20461A039558453CEBE167DBC7F6C1C6D960
              612D30BBCDFC6FDA1E30962884D9ABFA2E7D4B5941D92BC1C1C1778601D035DF
              7A626B79D1CCA29724220922432291189608B9580E17E7E2DD6C66CD6877B6A3
              C3DD01ABDB8A79F279D8A2D982ECE66CBE4F40A17FF57875836B43CDFEDCFDA9
              BE98F003945D2E2B4C77A66FA5338F964663BE623E642219EF6E6AB8CDD906A3
              C3C8CFDA453EF1B278542555615AF034FC6CFA19E90DE97072CEC01034301F13
              ED06BE8FFF7EDFEA9757E7FB0168AA2595265D313C63503F25790AA9CA54DEED
              D4B526B70906BB01C67E23EF7E2A3A850E5589555005ABFCE3579A2AB1AA61D5
              E89EA0002640D3A631DFDE7E7B91542A6DE001BEFDE9DBC399C199EB6994EB95
              7ACC91CE014B3E5DAE2E5CB75E8771C0C8071D9578453C6F9CCE7CA89CED3C8B
              754DEBF8B41CD50B26AF17BE89FAE6EBAC9559390C491DA5EEA0AEB939B6591D
              1B1A8B15AA15903252985813AEDBAEE386ED061FE9FFDBB82F20BBBDAAFD4BDB
              5EBFAB3E86696C6EFC406BD41E90296458A55A857961F3F8756FB037A0CA5205
              B3CB3C39C67D6216203A81A6D79BDE678E551E3B9285AC9C19F219C8793207CA
              20251F70E5BDE5B8EBB8CB0F3C69C6A9D030EAF24294C6951E61728FE65E3A10
              7160519C220E19D333889738D4586A70D97A1976D68E0445022E265EFC57C051
              A9B5D6E2BDDBEF0DDF7C46901E770FEE39EE791FFA070136CB365731A97B535B
              AA63ABA39F533E87155357A0DBDD8D33DD67D0E66A43BC7CE4994F54BEEBFC0E
              AF35BEE67D700A0044F5567D0B93FC45725B7D427D448A2A05CBC297E177DBEF
              A8B1D6202A2C6A528C8F08D0ED0548E94C6963927725B7D5E9EA2266CA6662E1
              9485A8EEAD4687AB034B544B50A1AB8054249D5C808121005D0420B530B5E542
              F48568915CC41BA329E7DB1D5F50BE80F309E7210F924F1E8003FE18D0F79125
              C83B92575DAC2C5E8CA923BF1CC813577AAF60C79D1DFC213596743A3BD1646B
              F23E58E1DF0B364F2141585A597A38B327733DE8528B471E2090270EFD7588CF
              8409C9E34180D244928664237A5FFB9BF620A693C6513C1D08A2F8613136B56E
              1A9F717A4C085B31FDBEF5E6AD77E9561CAEDBA96B699C45CA2E9AEA411387D8
              F7701FF25BF3C706B0C07F20E91EEB1EDDD87523863F8C8E571C3FF456FB5BEF
              F000E1A38FF19F215C8271418FE98E7D95B93273BDFF384EF822E1AA21D23003
              4AD2216C74889102F39AE51A16D72D463FDB3FF24BF40CE8F11AD73CD6985B3E
              6D59181A1ADAE42F482A2F557E996648FB90CF86294443C7EF09BA2D2FAD5B8A
              5E7780AB008DFCDE41801F16FD50B472E9CA2DF4AFA12559C847873F2A2FF490
              4A385C580AE9D8109FCFFA1CE9F5E9818DDBE02F4CA917362A365E2EC92DD10F
              2BC9A890A5885C5DB8BAB25C591E0B0569A02AC360A93D116105E33E00A27A37
              294AB7952D2745E95D5FB7E165B9CD3A3F637FC6D10A45C55CDEB84C8889D071
              82D09399CE4DB89CF820D2B8B4E6D39B4E67CBE5F26B43BB07BC98141C2838B1
              D7B577090F201500428806C3BB618904208F60945ECD9C825200870041D63F7F
              7A7EF5EE7777678FEB62E213D21EFAE3C51F77E6FD9297FD60EA03250F201903
              C025000877448D45632E79B9E468FA4BE9DB2774351B2A0E8723FECC8533B945
              578BD21AC3C96625190230D4EDAC00408CEBECBA47F90BF22BD6EAD716D3541B
              6DFC3101867864CAADD65B19B5C6DAA49BF76FCE6E35B5AAFFB6FD4D1316118A
              0873942AAA23F1E94463F2ECE4BAB9D1734F92195BC733EE3F95C277D043CC18
              FE0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Browser\GoBackword'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002B54944415478DA7D535D485361187E8EA6CEC9585138759695A1B969
              6A1A885118817567B9C9A6DBC54484424C6FEC42E78D4C1944542A81D528DCD8
              A69CB9AB489D4A58DEE5CFD434C20BA3F24CCD1FDCCE7ED46D9D8D9C1B56EFC7
              C777F13DCFF3FE137EBF1FE146D3F495A1B9219989325DB37AAC7CEC03B9D1B9
              3FC567C4E3A5F9A5BA0476C264389E381060DE78E347E3E35A7B6D8D8BE58A3D
              7EEC3808E638BD4EB87CAE208643713C1ABEE6A5B844DC4410843B2410202BDF
              2ACD6AB6FA96205E80B4B834EC787760DBB381DAA5E0F03A0E5D6E01CD9EE621
              5595EA4E40242860F86078AED853DC179F148315CDC2927B0956DA0AF505352C
              9B160CAC0D44A4090AE84FEEEFAEB859514F38684721EF3D6FA28C5F169B1297
              82E1AD61CC3A67214D92C2203460DFBF0FC9BC245284A90BE733C743DDA3AE12
              E404F9ACD1DBF8A02EA90E233B2318DD1E0DE13A333A517FBA1E5EBF17F20539
              8C36E3A1C877C07CDEFC84901AA453BB1777F3335999E859EB09824315260874
              6574419E243F1AC92F40B62E9B2404AF05ABC585C589D3F434B4D95A642564E1
              5F1611C93690339F632384AF84363A93E62D7B96B150B4F05F019FDF07C5A202
              5A4A0B6C32F3B1984B11556FAA26F5A9FACB01009FC5470C111341EA48EF4025
              AF3248AE5EAC462FD51BEA846C9B49C1346E7A2AB28B1A1077D4A32A5D8596B3
              2D919E0FEC2B5344215344A68D05C9FAE409FB397B848484278131DB78D473C0
              3600EE17AE7BA569A53838487D637DDDD255691D120F3151441474421D063706
              23C981A1FC06903964A7E886A8E1609459ADFA5673BBB7FD765024067F37668C
              6103945CE5BB3679DB5DA6CD9EF065629163E4A39AD99A5AFB29261DF61F211F
              733DCCB53361AF73DD9A22CD8BF292F2870172C43686D6D94917583E5964E41C
              797D666B2615CC77DE89BC1FE24BCC3A1796EAD86CF65438FE3721F64E331EB3
              264D0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000049C4944415478DAAD566B4C5367187E4E6929F752B0C028B03161684A
              5B412060325C0C48FA632399D9C620C8C6B2C13446A67189537E6CA344C988BB
              2433335B348491E02681920848522463174DCBA540C284A89496CAA5EB45286D
              E965DF6901872D03B3BDCDFBE37CDF39CFF3DEDF526EB71B5B89DD6E7F4935A9
              7AB57BAA3B7F6279823FB93C19031790129CB2B03774AF46B247D22F4C157606
              06064E6E8541F923585D5D4D96FD21AB393E7FBC722E722E6C4B0B9601AE8EBB
              D290D0D07AB4F0A894104D6D4BF050F3B0543220F9728237C1A3280A218C1070
              03B8086404C2497E4BCE25989D66ACBA56BD1F108F6004045AC1DCCD376E9E4C
              E227B56E497077ECEE9982FB059F2F852DB1A39851480B4A0387C981C565F100
              D36A741A6170186077D9379B4A48385A8EB5F795DEDA6C51F6173E04D39AE9B7
              854AE1D5E5B065B6384C8CF4E074D8DD760FA8DEA987C6AA415C501C4696469E
              58FFB4E80989866355BDA57A67DD130F0189F96ED1CFA2DFEFC5DCE31DE41C44
              7A483A565C2BD03BF450595498B64D233B3C1B3D193D18300EA05855EC9FC449
              F41109D78C606EE8E3A13C168BF5C04370ADF7DAF7950195EFED0BDE87C3DCC3
              B0B96D50DBD4909BE5303A8C480D4E85324789F080700F8E6C518623A347E070
              397C49CC4475405372D3957249791565B3D952E3DBE287ED71F6906371C7C0A2
              5850DBD568D3B779C2B32E97F75C4635BF7AE3B94BDFE5DF133AE95A80F780B7
              AC3DAF1553775477CEE42EE4361CE21C424144812781CDFA66CCDA667D8CDB31
              C9A2974451A8384D49DBA4EDB511B5C535FC1AF00278B86DBE8D5E532F5C6E97
              DF3CEE88840E93069046493BA8921F4A066F24DFC8B89874118B8E45749BBA51
              C5AFDAB2B71814036571650866046F9CF9E4C4E22528B39629A9FDDFECD78C09
              C6F875897590196460063021CF94E3596593272B5E829CD99C192AEBABAC19A5
              409950C42D429FA90F799C3CF465F63D3341BFA11F921109569C2B1B1EE4EA72
              D554E99552654B524B26C5A24097AC285C84C694C67F05CB8FCCF78C0EBFE0EB
              3920492E77942B2869ABB4FD1CFB5C31C2B023A9DF5D8FB32F9CDD78A61BAF68
              B80816A7E5C94B6B55541F5BDF4E294615A7B254598D88FD9FC0E93C934603A9
              F2C1E2C18FBC8DF66DFCB0FE457D0842FF23382D2678C6054F431AED53D26874
              DC9BBA9AAE542C54BC8F1872C9F2054F0B4DC348CE08D80CB6FF98AF0B3D60E7
              BCDA2C6AFEAE4C5256BD31EC322E65FC3A9E301E8B5DE485005F92C2E842C844
              3228CC0AFF963BD762BF00880D629DE2BC228FC9644E6F8C6BAD4EFBBAA045F0
              A3E9395310B8E420D097E440E4010C3D1EF2B59C6E6203BCE3FA11C73AFAE168
              45223FF13A7DB569E190849F2EE829A833ED22241C72405716639BCCD3F3902E
              4B7AE1CC71ACF212F92799E99997D6AF7D56E68C76E64DC955C9D7E3D1245C74
              D2E989C05ECB0D45D4BD160E1BBC1D4BF6321E93B02C89759D1F749E4C8C4FFC
              E99F787E97BEC3E178BEA3BFE3D489DF4EBCAB8BD0857B08986BDEB8D74A910E
              8B1588FE2BDA7221F7C2F50A49C567F482791A8BDAE66F4BCAE89FA3AFDD52DD
              7A796C7E2C69CA38C5A30952B9A9F3C258A1BA5054F88B304DD84180EF6F85F1
              37D9255265E140FD370000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000006A24944415478DAB5976B4C935718C7FF6F29A5A52D54A6D88883CD0B
              62B1562E51FCE0DC1469B40CE74866588CE0825BA606869319E7872DD3A81382
              A2C178D99C88F1128D51B96C2119B0B1654E87805819349BCAD804B958DBD242
              2F6F77CEDB97522237DD7C9A276D4F4FCFFFF73CE739979771BBDD9888917E0A
              7DABFEDDDFEEFF16DBF0A86166ABB555D961EB08020B28C54A539434EAA12654
              F347FC8CF83A55A4EA1CC3304F26322E331E80D3E97CE5ECF7673FCD6FCB7FB3
              69469372F48EC40788DB00F57D7547AE3AF75A5A52DA1EA150F8E0B90048BBA4
              ECA7B22F76DEDBB9AE296C48D88FF103898EEF04B06E162CC3729F07DB6025DE
              0744FF15DDB97FD1FEE295AFADFC8CFCA77FC2002E976BDA8EF33B8AF3A6E625
              826809180102040190FBC92117C821F213816559D8DD76F4B3FDB0B82CE863FB
              B836F06C701037133701D98EEC9AFCCCFC8C91B2F11480D96C5EBCF6E2DA93DF
              467C1B45230D140442E9AFE45CC488E0625C1860076073D960755B6165ADB038
              3D002EB76BF8E8AC0700A41A74DDBAE6739BCEBD2797C9AF8F0A40E63B3CB524
              B5E25AF8B568A1408829C2298892442144184232EBE6A2ED73F5C1C25A607299
              60741AB9CFB47DD45AA210468F6BBBB42DA5B9A5ABFCFDFDFF7C0A80CEF9B633
              DBCA0AA6152C130944880888404C600C6442191CAC834BB3D1654487BD039DCE
              4ED8593B8ECD3986FCBFF2516FAE1FB390B9E978ECF1CD8ECDB587B30E270DD6
              8417A0F4C7D2BC147BCA361A79A4241209F2044805522EDD54B8DDDE0E83CDC0
              45CD08185C8CBE88942929DCF7C4FA44D499EAC686A085D94BBC1BB83CFFF2C1
              352BD6E47801E8528B2D8EFD45FFAA5E395D341D498A242EED34B53DCE1EE8AD
              7A18FA0D5CFAFD05FEB832FF0A56BDB4CA3B76AFA317CBEB97A3C1DC30360405
              E801C2DBC38DBFEFFC7DA94422B9CD019CFEEEF4F174FFF48DB4CAB50A2DE648
              E6C0455E5D8E2EDC34DF8461C0C0151D474C0AF3B4EA34D629D70D1B7B4299B0
              7A006816BE99FDCDD719AB333219B274149A239AE66655B35225562139241912
              46821E570F6E5A6EA2DE52CF55FAB0CA7D5E08D6234E5DFDB7BAA3716F6314D3
              D4DCB4496D501749E552BC15F216E605CEE3E6FDB6F536AA4C55303A8C238EF5
              DC10461EE21170E79D3B1F32A72A4E9DC84046E654D95464866642E1A7E00AAE
              EC4919EED9EE713BDD68F65C106487449707A238BAF804937532ABA628AC6869
              B43C1A6993D3489658D49A6AD1E66C83981173EB7F2CA35BF3D1A8A38893C70D
              AFB7D10AB37F0860AB746B15937420A9A55A551DB950B110C99392D1EDECC685
              EE0B289C5388B743DFC67FB1113361E701886BCDDA16266E5F5C7BE382C6B0F8
              9078240625E286E5066ACDB5285195203534F53F018C0861E76B8000C43F8A6F
              67E2F6C6B5DFD2DC0A9B269D8625C14B50FDA41A9D8E4E5C525FFA5F006CAC0D
              BA461DAA7BAB3D0D033E005D0420292FA9A532B232522013402290704B8EEE0D
              FF0700154F6E4C46556F954F23BC35A0ED2353907D22BBBA5051F83A260DFFF3
              3CD93C848A42C71561C86BD78C5D581CBC78ECC807CD0CEF5EB0359814617145
              F1F1F4C7E91BF11269143E7B9407230F22FBE5ECF1231FB4DE2180E218B20CC9
              46F4A1FA67F5114C268DB26713DF376B1FB6476C9FB8383D15F9AD98BEDF5D77
              F703BA150769F6685A9A66926B5708F9C1EF05895333C17B20697A350FEBF7D6
              477187514979C9B1F51DEBDFE700825E90B88317E7FD94E6D457E9ABD3377A8F
              E305FB165CD747E8A742413A048E2E2EF613A326B6068B82160D131FB1E07C8D
              9E018F3DE2E1BDE1C696CF5B9688C5E23BDE0B49454DC5973ABDEE136E350453
              A5D1C7920BE5A85C508984E084F123A7462BFFC910C095A5570A562F5FFD31FD
              C9F74A16B0FDF8F6B23C37B90907F15321191BE2EAFCABD87D7FF7D8E216782F
              A6340B5BE45B7E3C947548FBD4958C1A998A8835796B2ACA14652AC8A90A7129
              86AEDACF622E5E7C1080B8D6492EA53B4A57924BE9BDC16E4F5FCB2DE684B4C3
              6927CBE5E5733971295F13E20982D0D39BC6C63F9C0C42E8585DF3F98FCE6F90
              C964BFFA761FF5C124B728F7CC01C781373800090F1040DC1F9E0D4BC003B979
              51FA6866E79D02D8780832FF399373AAF77FB07FC3841E4C068DB48BAF7E7F75
              4FF60FD91BDA26B5293800D138000E1E807F460C37851B0FAD3874326559CACE
              677A34F3359BCD36FF42E585AC82EB05BAA620B259897C007CD3EEE20188B8C6
              AA7998B338A77CAD766D215D6A638D3F2E804F4682EFB6DE4DAB33D4C5363C68
              98D5DAD3AAFCC7F20F5DB0089387196787CCEE8C7925C610372BEED6DCC8B967
              49C4E6898CFB2FE2A37ED06F2CBFA20000000049454E44AE426082}
          end>
      end
      item
        Name = 'Browser\Stop'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002C34944415478DA85534B4C135114BD6F18B094348014B0E8CA144C68
              E820059B686421065D00D1D292862E0B0B31D505D105291B021B88F113E242D2
              9D29055ABA4308B05041B45A5A160616B8D0B4FC2249B51F4A1918EF9B7E5212
              8D2F3999BCF7EE3D73CFB9EF124110207BC5A2D186CDB939D3A1CB755DB2B676
              FE849E715C50A2D7BFBBD4DCFC5A5A50E0CD8E276902FCE6FB1C8E2717BBBBCD
              853C9F07C5C5201002108900130E8B31DF65B2C33D9B6DAC5EAF7F4408896708
              68B2C76A756B87876FF1B5B500151540422180400072767701A2D1CC1F2995A7
              AF6FEEC6E0E01D4A221278C7C75FD67575DDE33B3A40601860373680F87C400E
              0EE06FEB27E2DBE4E4A8D660B0906824529F282F5F2ED0EBF384D25260676600
              2A2B93A52F2E26E561558246038CCD26EE8F115F518E727BFB1AF1399DCFD5BD
              BD0F8E7A7A20079319990C788703A38E81D5E980ECEFC3D1EC2C405111B0160B
              30636322090A8380DBFD947C301A571B1289CB275555903B3202A05402BFB000
              425919403C9E0426031A99DBDA0A646545244087E0A3C9E425ABD5D5BB2AADB6
              8CF5FB8141DD62C969128522291A4D64DBDA80595A3A65E6FB9A9A1DE251A976
              34A15039130C662E05B53A59764949F2209100D66000869EA5D66FC432C76D93
              B79D9DDE46BBBD2E938C8689C9F80EA891B413D45C2A857A92369676E23395F0
              C5E57A56D3DEFE302F4570D2D404FCF43400CF436E4B0B009AC863922097036B
              360363B78B713F107BD4446CA3664BA1585686C367208B84C46219C3043458E0
              3860A6A6C4FD2FC47A61619CDBDABA2A3EA44F1313A39546E3FDB3F0FF1543EC
              50094EE78B2B5879FA294B16FBFBDDEAA1A1DB9484FD4732356E1FB169B5BEB9
              393070179FF261F630493C4EE788D46CEE3E87722429227A9BA09D4404B16CB0
              D95E35E8748F69F2A969CC1A67CDFAFCBC094B6C04BFFF42AA3301398EB30AC7
              395F2A5DCD8EFF036D0842F98C90186F0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000004BB4944415478DA95566D4C5357187ECFEDBD953580E33B2BB38A5006
              948A404530808225D21F9BC97E4C064118CBB24543641AFFCCCC1F1B1842B66C
              59E28FCD2533C499A021A16010E5637E840D49016975B09500050A2B4C3EABD0
              DEB677E7DC5E0AA574C493BC21B93DE779CEFBBCCFFB1E10C771E06FD9EDF6F8
              31BDFEDD7FDADA72A9E1E16897D1184976737171735C62E254B446F33046A96C
              118BC5467F18683B029665639E363757459D3D5B21B35802FD1D5EC5610C0959
              5DA8AB6B3872FA740D23168FEC48601E1F2F5ED468BE570C0F47004501171A0A
              5C480800C3106640562BC0C202A0B5357E3F39BD8C6350A1B0C4B5B69E93CA64
              0D7E09867B7B2F86A9D55F875BADBB38A914B8A424E0C462000CCA0393BFF3F3
              402D2D11FDBC2EB6826374F7EE35A6BDFDCBA44387BEF1219836993EA495CA5F
              22575777390E1F064E2E07C0B7442B2B40CDCE024C4CF0E00867E16F2D11C930
              8954AF2F5FCF8427C09AC7FE75E0C01F0AA331C2555808CEFDFBB1C0AB3C30D5
              DF0FD4F43480CB053B2DB2E35F1C0358AEFC81812C8661C67882DFAF5FFFF948
              45C5C7CEAC2C701D3D0A1C011F1D05AAB313B8B434A09697010D0E7A81715151
              C0656703D5D8E8F5FDA54062AAAFFF29B7B4F45364B3D9E45352E9D318974B62
              3F7306E784801A1B03A6A909B88307816D6E06703880D16800E16C78F0F07070
              74740097900074652550D7AE79653187E3CF888897D966730A1A7AF2E4624266
              661D5B5000CEBC3C5E67717D3DC0BE7DC0B6B5014824EE93F83B73E20480C5E2
              068F8F17105D40979703D5B0619E451CB86AE0D4E92EA0DF6A6A9A8E5DBE7CD2
              5655C5DF8CC6B2D05D5D0081813C01979EBE913F29320EDC681B36349B81CECF
              07343EEE251321F8BBA6468B3A8A8AFAF31B1B536DB5B5807051C557AFBA2D49
              565010B0ADADC065646C5B541EFCF8714058D2CD6B4D20182829E9439DE9E953
              790643B4BDBA1A445A2D88BABB016DDE1D1CEC9B899F9BAF2F9B40D093913189
              DA55AA49755FDFDBCE9C1C10F5F4F83490A7A08989DE288B8B5E85DF2E83DECC
              CC09D4565CDCA7BE79334D84DD035BC6865FF01D48D66BA02F2DD5A14E5CE4CC
              4B974E4AB69C253EF7728B200B7111E98DCD8527EE427ABD8F8BC6AE5C694243
              3ADDF94895EADBD0AD04C9C9C0B6B70384867A6B8E01D9BB778153A9DC1BB1A4
              F4A95340613390E50477A3915E60FAFB3FE71B6D10375AF28B179237B6922895
              C0DEBBC74F4E2FB710771112DC887451115077EE78CE580502236EB463A4D1C8
              A8788CDBFA9DB2B24FC85DE96D48000F3C1FB76077B9C81879F0C0F3898CC179
              21E66EDCF831B7A4E433CFB07B949ADA9DFAFC79D49BF8470A5E7FB904ED1770
              0CA5A4CC14EA7459344D9B3CE3DA6236BF6F52287E8D5D5A0A0826FABD06B803
              DCEF0119D713785CCB0D86B2B7F6ECB9C5D76EF383830B7EC1AA5657CB300979
              2789B3D00EE0AFC06D4B423085C123BBBABE48484BFB6EFD779F27736672F203
              BD46F34302968B143D40C88616C838C129446FD2B16B028111CB92DED2720EDF
              FCF666BC6D1F7D87C3B1B757AB3D6FABACFC48363313440844426DD6091C02C1
              5458D82B516DEDADECB2B2AFC803B3150BFDDFBF2DACDD1E376230BC67BA7F3F
              C7F6EC99CC3E321241BE8BE5F2D900A572626F41C1E358A5528B8147FD61FC07
              1C8D4833F85336EC0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000006814944415478DAAD97694C146718C79F777656761594A855148F6A41
              39BA5D15AD473128120E371EC807A0D6BB47AC4682951AEB8726FD6052255831
              35A92768231A5345773D6AA2A095432A05E472AD78E281C68A202CCAEC4C9F67
              76765DD85D5C8D6FF264373BB3EFFFF79CF30E932409BC59789FFF8DDADACF1F
              5DB932A1BDB2F223B87E3D4078F4A8AF88D75401012D5248C8435FBDBE61E8C4
              89E5416161798CB1E7DEECCBDE042008C287C5070FFEC06566CE89ACAE0EF074
              9F15ED15DA4BB4629DEE51EF8C8C1391A9A99B789EBFF34E00F8BBB6CC64FA89
              6DDCF8C5A776618E03098DA954003C0F928D1058672780283AFE6B41EB402B0B
              0F6FEABB7973EE9484841F31221D5E0358ADD6A17F6ED8909BB0654B0C231812
              D36A810D180092AF2F30B51AC457AF807574C806EDEDC05A5B651847E4D0DAD1
              5EA095A4A515CECBCC5CEA2E1A2E002F5A5BA75E4E4EDE3BEBF4E910F216FCFC
              401C3E1CA4C1838123CFD15B4911E5D0A0AD0DFFF402187D5AAD5DA3A800C810
              06437D6C5EDE725F3FBF528F0098EF111793924E459F38110EBD7A81346C1888
              212120A2D7B4B9EC318991B7CF9E016B6EB609530A3CA512AD55B1A2B83873A2
              D1385BAD56DF7401A09C9F5EB7CE343B2B2B1A30DCD6E060B0EA74001A0D300A
              378A72280A8D8DC09A9A6CA1770A798F85AC00B4502456ADFA2B79FBF6587B4D
              38004A8DC62D93E7CE5DC7D0736B783858274D0289C42D1660E4EDAD5BC03534
              008711F056D87991DA73C59A8E1EFD657A6262BA03805AEDEF09134AA6D6D606
              88A3478310130352BF7E58CE16E01E3F06D5D5ABC021801414640B3D46C1EDF2
              F10171DA34E00A0ADC5EB603548F18D11C7DED5A9456ABBD2A0314EEDFBF73C6
              92255F91A860300041505E198997940077135336762C749E392347849F350BD8
              9D3B2EE2427E3E885151C0AF58015C5E9EC72834A3DDDBB76F4FCCD2A55F3251
              14FD0BF5FAFA99F5F501A25E0F427CBC1C7A78F204F8D252505557CB9E93380C
              1C682B9C7BF7808F8E0676F76E5771FC4D5E58B0EE20442780521C5629555521
              EC7A75F5B7C375BA5F7DB0D285C44410C3C2E4D6E25098A750F6EF0F9DC5C500
              3803BAB4CFEDDBB64820A870EC1888F8BDCB2288050B802370A7D5AA00603983
              B6A66625BB9093B36B1A86821F32045E2E5F0EE0EF2F7BC61B8D72986910097B
              F680B870A14B480982EEA1B0BB5CABA800755C1CBADCF59160710268CACDDDC5
              4C6BD614C6EFD81105E8B9909222F7335F5808AA4B97E42274B4D2EEDD202E5A
              E455C5B3AA2A9B38B56DB7F54A1127AB5BBBF63C3B1E1B6B3614148C016C3B61
              CE1CC46A825E478E00BB7FDFE5CFDE40F4244EAB5311A72894E16062F911118D
              86AAAA40C002146363819595015F54641B342EBB338FE9902F5756821AF7E81E
              F6EE00F6145C9E38B1911D2580F2F240F5A041609D32456E3B2A2CB7AB7BB577
              07A0EE70D7A21E226003C014C49F3D3B468BDED1D89568F2BD83B8B7102F9D00
              AE500A8EA7A515CCDCB66D865F4FBB7A29EE0D44BB530ACC548405B9B93B7538
              05FDF107D55B8A53AB31B31944EA9EEED7505C1E56DDC6768B0240F694DA1007
              D1CA3E3ADD0E02E8ED465FC2F9205CB800D2C8912EE272B5B7B4B82F4C3C2BA8
              B1AB18B5B3B20445F8B902E25757F70D8DE2BE26BDDE4CE7BDBE1EA220050682
              70EE1C48A346D9C4DDB45A971645717EFE7CE010DC79B5C1EB0752A55EFF30A5
              A222447E189D3F70E0B790C58BBFC6E71FF4F190573B0479ECA9CF6588A424B7
              E282E2B5DDFB273939BB6330F58EC7F19971E34A27D7D60EA662D4F400412722
              8F7D8E9D24858602ABAB73B9643F90D0A7191FC77166F3748D4653E33890949C
              3AF5F31083E17B4A031EC0A017BCBFD50EAF8F65646DF9F95991F3E67D27333B
              1DC97CF2D7AF3745E049D8574985CF7B12A7DCDB0FA735AB575F4CC9CE8E7339
              92C9791284917F24269E9A6A328551479069E9A67710161571BBC9E23878928C
              C6043C94DE7264CDCDB17CCA99D4D4BD11274F866A15008D92126F4068379A76
              E49E453102A8C563B9E1D0A165BEBEBE979DEFF7F862723823E3F788AD5B673A
              03A8155329304C11241314EB5400EC10247E233DBD60FEE6CDCBBC7A31717822
              499A4BC78F6F6A4D4B5B167CF7AEBF1D80F700607502B0BF233660B50FC8CEDE
              FBD9DCB91BDFEAD5CC79592C964F8A0E1F5EF3342BCBF0310E2B82E014F30450
              8743E683F4F49391C9C9DBA8D57ADAFF8D004E11E977A3AE2EB5B1BC7CC27F95
              95411DF47AFEE001CD2EE003039B7D82839B068E1FFF6F6044C43F41A1A107D1
              E3566FF6FD1F204766B6D985F5360000000049454E44AE426082}
          end>
      end
      item
        Name = 'Browser\PageSetup'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001004944415478DA63FCFFFF3FC3E3C78F234F9C38D1C7CCCCFCFFFBF7
              EF4C9F3E7D62161717FFCD80043E7CF8C0A2A8A8B8D6D1D13113599C1164C0FD
              FBF7E3F9F9F917B0B2B232BC7FFF9EE1C58B170C9A9A9AC8EA189E3E7DCA00B2
              E0DBB76F75FAFAFA2D2806DCBC7933E3EEDDBBD3810A18BE7CF9C2F0EEDD3B06
              3939391403BE7EFDCA606F6F0F72ED1F3E3EBE0C2525A5B924B900A891819191
              91819D9D9DE1D4A9535F0202022C383838AE126D00081C3B768CC1CACA8AE1F7
              EFDF0C1F3F7E4C0086C9C25103460D801B404C5206816BD7AE3168696931FCFD
              FB9741595939535D5D7D06755C404C760601A0456C404DBF802E60B4B0B02892
              95955D0E00ECB26EF087EE38060000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000012D4944415478DA63FCFFFF3F03081C3F7EBC76D7AE5D192C2C2C1001
              20B873E70E978A8ACA37063CE0EDDBB77F7A7B7BB5191919BF629367845970E0
              C081766D6DED0A0E0E0EB8E4CA952B19C2C3C3F199CF70E1C20590E31E949696
              AA022DF943750BAE5EBDCA202B2BCBB062C58A8BC5C5C506382D58BF7EFDF417
              2F5E64B0B2B2C2254F9D3AC560666686D70229292906737373860F1F3E801CB9
              333939D983AA3EB872E50A839A9A1A98FDFAF5EBFF376EDC581C1010104F350B
              8009830164869C9C1C03C8F767CF9EFDE5EAEA9A2C2F2FBF842A16A0ABFBF1E3
              07285E3A1C1C1C2A472D18B560D482510B069D05DBB76FEFE7E2E22A60676787
              6B0495946E6E6E042D4056F7F3E74F866FDFBE4DF0F4F42CA4AF05433F0EC86D
              B6A0ABFBF3E70F2330B866585A5A3683F80007BDDFE0D6345F7B000000004945
              4E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000001B44944415478DA63FCFFFF3F030C2C5FBE7CCED1A3479D9899991182
              40F0EDDB37D64F9F3EB14948487C652002BC7CF992BFAAAAAA454F4F6F0221B5
              8CC80E983973E6EAC0C0C0104E4E4E144577EFDE65B87CF9324340400031F633
              9C3C7992E1F1E3C7BFDDDDDD23A5A4A4D60E88030C0D0D41E6FDC8CFCFB7E5E6
              E63E33200E00818913277EADABABD36061617942D00193274FDE606D6DEDCFC1
              C181A2E8E1C3870C376FDE6470737323CA01D7AF5F6770707000B3FFFDFBC7D0
              D6D6F6AEAFAF4F899191F1235E070015EE121414746565654551F4FAF56B509C
              3218191911E5802F5FBE30C4C6C6C2F93F7EFC60E8EEEE7ED6DFDF0F72C44FBA
              46010C7CFCF81164FE8D8E8E0E6DA023FED1D401274E9C6000C639C383070F18
              80210A16E3E7E767B872E5CA5F3939B919FEFEFE393475000C00E39DA1A8A808
              CEFFFEFD3BC3FAF5EBD7A4A7A7878E3A60D401A30E1875C0A803461D30EA8051
              070C4E07D4D7D7EFFFFDFBB703A839850C3E7CF8006E98AAAAAA92E480D3A74F
              33989A9AC2F97FFEFC610036780F3436363A0ECE101875C0803B805ADD7318B8
              7DFBB60030E17E80F1FFFEFDCB08ECFAED8B8C8C4C81890100AB99DFDF8BDA6B
              040000000049454E44AE426082}
          end>
      end
      item
        Name = 'Browser\Preview'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002A64944415478DA6DD37B4853511C07F0EFB5B9A5DB9CE66B495AE69C
              25AE5177E98C4C104A4A4421927405D1E39F8A820883A07FA23FFCA3200A82FE
              D24CE801D1C57F8228A4D202B1F081CC198D0D137177E0BBEDEE3E4EF71EA76E
              D681C38FC3B9DFCF39DC730E430801CFF3CDB3B3B35D509B3A840C0259AD8A02
              48F1CACFCDA558338D5D6E177B0D098DD1807038DC140A853A24491A8C08825E
              4318752AD5605096FF44CA6C0ED6E5F5F9402CF928247337CB4A6D0FFE0102C1
              A0C7C5B22D89BA2008E583635E6EA7BDA2D43BE943BA75379617E6099BA73B99
              979BFB3609F007829E4AD706A08587C67FF65A8BED256018F87C935814153A17
              599817CFD65797E874BAA9FF025141A8B8D8DDF34D9F93634A376C857FFA378E
              17EFC1917D07212944ED40B93976D86C360F5080D7007FD05355C9B688A2686B
              EBEC1EBD74F4445AA12513A222212AC6F07470006E4B011C7607459C163119F8
              E50F78D8FDCEDB1FBF8FBEFB1289D8CE38591A16E5D5BE224471EFF52BDCF55C
              A5802B4B4A0646C6276EE8D3CDD640CA16BBC010B88B76AD87636A97898C8E5E
              0EED8D17E811BBB313013EDCB4ACCFE464550EADAC80F3FE40ABF30062924801
              5D8A7A1788825BDDCF71A7F53ADD414D9E9C0CCCEB2C9CF673645941E79BC770
              D73662BB3983064DA93ADCEFFF0C676A0E2A1C5514A8B32AC900CF64A83B50B0
              EDEB23648DBCC039732B98FC4218D20C0806A7E0CEDD899AEA63EBA7505FB009
              98514C5C76FF43640FF7A0AFEC328CB5E721A9BB595A5A84DE98414334AC5D75
              B536EC20C980D8F784B30CBFC427673B8C874ED38FE584D0DACA727CDC5CB409
              981E7ACFCD4418A4B99A573F24646355FAA81240B59E2A463230113571124958
              251E5A43E44DE3B612660308F17CD387B1A967DAF9CAF195E8738E8FB5AA8595
              38A2BDD62B757B1B34E02FB537F4F0FF4CADC60000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000048E4944415478DA95967B4C5B551CC7BF7DD1C75A36588714365E0141
              E75A68651B7530EB746306342E1B01139C9AE834C13F489C8999844A343159D2
              458D616E267364FC216CD3F998C1002B6070660B436DD6D1B20DA3C3F1684B0B
              2D4DE9BDD773FB86DECE789AD39E7BEEEF7C3FE7F73BE7774E790CC3802D56AB
              F5A8D3E97C0F09852695215FAC49A84D3E14137EA622E316967CD38D757BB7F1
              78BC20380A2F0AB0DBEDAD25252526B61D15A043BF449C090BD37472FFCFE356
              509EB9C9C6FD86870984792080CFE77F1008043A288A4A23552C96487CEC3BBF
              DF2F0B0683129A6644C45E404B1487F30A8BE52CE4EAEF56A80A8A316719FDE5
              40ED1EFD03015E9FCFA851AB9564262B5CEE125BC5C0D0484F994E5F1BF5E2BA
              6502D9F945249434966F8F9DDF67A83E9412B0E8F5192B34DC0056BCAFDFDCFB
              D8CEEA7D89E11AB3DAA0CA2B0AD904570290BBEE1EAFD496BFC309702FF98CBA
              F26400B149EF1B1C3A5FAAD33FE30904A0481347D600B8FCC37790C81461313E
              0F42A1906A343CBE4B2E975F4D02B8167DC6CA8AD50076E69D172E0E7C363555
              99A5546283548ABF9D0E30EE45B4D51E40516E5E64D1238B4F3CCBE3B91A54AA
              ECDE24C0BCC767DCA98D03C83BF9B1B35DD77E138ACA5AF5BBB139632311A0B0
              420571E3DE9F383E3488238F6851ADD911F228BAB30AF90BC9001B01CCB97D46
              BD2E0C60C5BBBEFDFEC7B34EE7AE8FEB0F42241084C4D91A24C9417619FE5A70
              E0E8371770F28557A048CF087B413C2816A600DC777B8DD53A8D923C0AFBAF0C
              F77C649BA87BB3E6296CCBD91216A62200164420EC73AF651CB31377D072F0D5
              9807A5223737E01F97D75853A9D9DC6F1EEE2ED13E515FFBC527F8FAE523602D
              D8D905A31ED0C19038EB897D6E06277A7AD1D9D21E4E48E2C156710AC0B4CBD7
              B1EC7698CBB6D7D491A4C2A1EED338F7E26132888E08D3310FD82AE4F130E998
              4747F7399C6E793F96E16A490A8038ABD09478343C77EA044E3735235D2A8B0B
              474223E001D9EB64F8CAF2072E5DEEC3876F1C8B8DAB907A3800367BAB30ABC0
              14DDDF14F9EA34F741B65E8606B536B6062C844C1CF9E90A4CB917601C1CC00E
              C8B1BBE6D9586EE8D6791A72D602260880BF8900E8B807698EDB68BFF4256A0C
              0D28CF8D2FB45226866BD98FAEF11BB83E3C4266DF06F085B145DE2E5FE406D0
              1B0B4C5171A9C386C22BED08CE4EA23EF83432D455D0A954A12CBEB7E881F9EE
              1DF86D9378B7E92D48C8164D3C69ABD253005632F34DACA16CDE8A82A17608A7
              6F61842A42E0A533B866B7A2FFE618DC5E1F322562184A2BA0D65481042C7E9C
              4732BA7AFD1237C0BF21DF249BB5A0C8DC0EC1CC040699AD609A4F4144CE9A55
              F700BD5A307A4444414F66A40008971DA6C2E10EF0EFDB30C02F079A4F422895
              C7F637BDF622E20291BA27330540F1EBE72695F5227E92EA216AFA14823429E7
              CD9638DB98381DB7D9ABF426036E11C0822CDBB432DA8D80B691888B530AFC57
              FFFE4D2900F3B22DA65471FD3FFD750FF9B8013352024811D7B5FD8909991842
              B6FD7C3607C072D3FAF6A87DA68D35607B58632632900122075E58341496F07D
              11FE4B13118E96D70D8FBE969BA3EA61DBFF027292E7FE115B63400000000049
              454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000068D4944415478DA9D977B4C53571CC7BFB7A5A5806D0505C6A0B48A45
              1E420B2D0F6518E3DCA6DBD4F98853B32D7BE81FDBF06EECF9C792B9257B4F37
              D9A27BC464265BF6C8A68B268E3FDC445AFAF231E744A608F8A0A2088802C596
              F6F6EEDCB6B750687BDD6E73726EF33BE77C3FE7F73BBF73CEA5589605FFB4B4
              B4EC6018662D263D7ED224D02CD4D6CFBD060A1B7C0FD88336AE1A1C768B97D4
              18B6DC9599F90B041E6A2280C3E168A8AAAAA2278BFB499B601D2A60C350D1EC
              CD162BE4992A6661DE8C4572B9BCE57F01B093C4A78845816343FF4D161B72E6
              19D1EFBCE879D830A758229174FE278083070FEED6EBF5874847F7D8D8D8349F
              CF9738E6F5CAC422B14F2A95DCF67ABDC93E86917ADCEE648A12F98FFD79F2D1
              07566F30F2306602905B620C8CD9DB7576784DAD5E4551D4AD3B02A8A8ACA23B
              3BCED769B5DA9D42F1237DA9C34DCD5FCE2E356E9624CA28DE132D2404EAD28A
              703BE799E3BDEBEFBF278740F804010C1595F485CE0E41004EBCD964FE4C5558
              F6BC3429891A0F030B8BD50A4D696544FBEE93E6D68DCBEF2F1104D01B2BE9CB
              5DC200569B7D7B665E71BD541612C7F87AB0118059BA48004EA7AFD5DAB466D9
              92C571014A0D95B4F3427C806673CBE7D97375CF4322A5066E8F4299980C8948
              145E98A6A63F30EAF182B81C7EC6078FC71DE8274B4A6257D6943DAD51E7EE89
              0A60270025E51574CFC5CE98002D56DB27BFF60FBEF853EB29AAD7CF402995E2
              96C78334AF1F4B3579A87F701524099209691999A2F2A18BF545057377C40428
              2AABA07B2F4507683CDCF4F5330EC766835A8D75453A14A4A723255186A1DB2E
              9CE9EBC5DEB656D8FF69C3AE151B51304B1B358595C397E20314E82AE8BEEEA9
              00878E1CD9B5C16279F6859A8558A72329461197130F307E3F19D81FA87D60B0
              EFF429341CFE1DDFAD7912B9D9EA299E481B890360B33B1AF209C00D672480C5
              66DBF6D6DF675ECACFC9A65E58B038B405B341710E821B3C54FB48CC771FB793
              B1ECD8436F9DB0A105CB4C9700C09C52233D78A5AB2E3F0460B1DAB6F93254F5
              8FFCF29DA8F1B14D489D262762E3B3E67E5CCD10006E965C3DE81AC686BD3FE3
              43DD02E8F4D511BB6486EB727D51611C80D92546FA564F10C0E138FA5E4A76DE
              6B073ADBC56D03BD7873F18364964C0400C3FA03FF999007F87AA7C382B18E4B
              78F5892D1121C81A8D036025009A79467AE46A57DDCD9B37B364199AD7E5D353
              13B636EE47D6CC543C6558109C2927889030EF0976DC131CC0BE33A7E1683663
              7BDDD6F0E1C595ECDB0200B9C506FAC00FDF586A97ADAE4E912BC51CF95B8DBF
              E2AEF41978DA303FC2D5CC244F7075B2448C21CF18F67200479AB16DCBDBE1C3
              8BD352B9398082D8003945067A72FE7E4B1655C7D075BCB1E881A010EB0F6700
              33014044C6D0A64DC7E9EB7DF8C26187BBED3C5E7EE695F11010BBC623009055
              C80144E66FFFA80B0F7DBB0BFBD63F8EB4147954B7738B51AD50C04BDE4FF6F4
              60D381FD785D5584CADAA58199F39399ED8D07607334641696D311978F50E7BA
              1F77234F958DE7AAEE09A7200F0252E72AE51091ADF75CFF00BE397902C74C66
              7CFAEC9B48484C8A4843AD2F0E808500A4CF2DA783B93B4E0DC68BF4A35F61E5
              A96BA8AD5C825585E45023623C408A3401620AB8363C82DFDACF618FD984B749
              BBE2B29A2913C967BAEB8BE301CCC82FA7F978711D28DF1854F61D98DE6DC2D0
              E00D2C1FAB06A53562657E01E6A4A541269160849C05ED037D38D07E1E6D642B
              DE52321FD5B5CB22C4F97016F8050052B565E110503E37726D1F43E9B403034E
              5C1A2627DDFC77D1383884DF2F9C439F548264990CA36E37D25C2EE8650A3CB9
              741D66DCADC6E4239A2FC5AC0080624E1040E475436DFB000AA723207E718482
              7DD176E418EE0D0F766BE03A7AAE5F8522691A941959104B6598BC7E22B76216
              2570C606682100F2BC329AF28D42637E1F8A2B47811B4E74B8A4F8F3BECF7177
              E98298B76436CAC28D768BD65102004A95969E657A07D3AE9E0888B7BB64F86B
              E94E64CDAB9E12CF78B7E458B7E872511C00330198D7BD9F4EBD6C0EB8BDD5AD
              C4D9E55F2253AB8F1ACF2930986A8F802176A3F84A1C00ABA361BA52416BBF5F
              1D70FBD9155F2343AB8B19CFA862886DE34A9544002041A3A325FD1DB8E64E80
              224BF39FBF8A26A6301BC53E5F1A17C0DE20CAD5D3771ACF685F4542F69AC49E
              D800260240A9F4F49DC633969BD938F6853201007F8E9E8E8C771C218178F347
              70784B87004093C9BCA36BC0B3363030F84FF0F1CE2CBFEA1134F0EFE136A1F8
              23BC06D8F018FCF34899FA2302F019FFFF5FBE3B3C1B9F55E082000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'Browser\Print'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002874944415478DAA5524B6B1A51143E77C6F8C2118D848A69F139D198
              A40689A5082E2C5270118ACDA6AB76A3D0D045BA095D75D37F514ABBA949E9A2
              181A301BA19817B550AC5251AAC1802158C79034D1188D8F9E19504868B3682F
              1CCE30E77EDF77CE770FE9F57AF03F87F4093A9D8EB65EAFDB8E8F8F0D18C65A
              AD66383F3F37188DC6573A9D2E7C2541A9547AD46AB55E572A15BAD1685CBE53
              F07ABD138490D65F093086363636BE994C263B92C0E1E1E1E08256AB058D46F3
              00F3FB2B47C866B3CF51FD05C330707272022323232097CB855A2E975BC67450
              AD56C771AC091C29E276BB9F5C2040E5FB48F0A15028804422018EE3BA983BA8
              4CABD56A4AA552099D211890041289C45B9FCFF79074BB5D3116EE6C6E6E3E46
              E5BB7ABD5E8AC611A55209588366B33908BEB3E1E161A1BB743ADD9B9E9EBE49
              C2E170697676F6BA542A153A2997CBD06EB7795F2E80FBA15028C0E17008778B
              C5E233B2BABADAB4582C623450F8C90353A9149C9D9D0D40FC371F3CB1D3E904
              8BE508687A1BE271679844A3D13682688FC70362B15820D8DFDF87ADAD2DE09F
              9437D56AB50A868E8E8E023E27823F22D9296C6FDF58229148A42D1289689BCD
              265CE877818E0B609E94F7229FCF432C1643D538B02C0B2B2B2B3D147F4A82C1
              60796E6EEE1A3A0D53535303025C2E585B5B13400CA30083D180F5C9363BC652
              0A46417115EED78CF3969FA069F742A1D0BB85850599CBE582643209EBEBEB70
              DAA8C3B8DDDE1DB3B2A05432D4E505E2FDD8C9EFEC097B807B7F7B7E7EFE0DAA
              B17EBFFF7B201088570FAA4EFBE4B88757EF9F4A8583DDE26E4D26917D9D9971
              2DA3F14B8345C2393598441445FDEC033299CC62E2CBE745113DB467369B3F61
              872FD1931F7F5CE57F3DBF01B0375E950B78F3A30000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000004654944415478DAB555DB4B6367109F9344B789A83D5E378959D7A8B9
              18839A144DBCD315248268AD450A2BC4D2671F7CF2B17F87ACCBB606EA8A8A2F
              CA162F68F16E44AD56B46BDC583589BA9AC66BE22576E6B009B1BA17173A7038
              DF39E7FB7E33F3FBCDCC61AEAFAFE1FF34E6531DF8FDFE388FC793131515B5CC
              E7F3B73FCB0182B087878739FBFBFBCAD3D3D344AFD7FB102F19BE7F24140A53
              53525284CBCBCBED252525DFDFDBC1D0D0504B7C7CBC59229184D133BDDFDCDC
              84F5F575383B3B038C1AAEAEAE402E975FAA54AACA98989857F772E0F3F93256
              5656261C0E47D4C1C101F0783C4067A05028203C3C9CDBB3BDBD0D7B7B7BE4AC
              ABB4B4B4F6DE148D8F8FB72A95CA1F68ED72B900E980A3A3A3007D20168B213B
              3B1B90C2538D46938E8E1CF77280A0DF868585750E0C0C804C2683B4B4348E9A
              BB0C3532272525598E8F8F0B5657574D76BBBD442010F82B2B2B6BF18CEB7D22
              C72D2C2CFC1D111121B45AAD5CD4643A9D0E121313B9F5DADA1AD86C36A2CD8B
              9AE12D9C47DF03BA8D8C8C1C1615153D118944D65B0EC830FAD76AB53A0D33E1
              9EE7E6E6C0ED76033AF5C7C5C5F112121280AE809D9C9CC0EEEE2EA755C04977
              77B7BBAEAE4E8719D96F38383F3F57777575F561FAC91431023152A914B0F683
              7BA8922E2F2FE1E2E282BB688D9A006613CC92F60C0E0E765655557DC760293E
              9D9898F81179D36209B258820C6D5A5C5CE434A06A0A057BDF1A8302A3D118CC
              A2A3A3E3D06C36B38CC562592D2B2B53B02C7B4BC8D9D9592EAA0F0107D6D42B
              151515C1B3FDFDFD505E5EAE67DADBDB77A2A3A313F2F2F288E7E0068A024703
              2C2D2D0176F1079D60B743646424984C263C474D390D1B1B4260D98C5A0605F1
              A0A051B1B1B1A0D7EB2154135A8F8D8D7100A1C0F48C5542CDC9012727270745
              1689B291561BF6CF4B2C0E8399E9E9E93943FEBFA08FB9B9B980D9049DD01D3B
              1B868787B9922560A292B4D16AB5C13DA17781E037DCAB06ABF52DA4A6A63E65
              3A3B3BBD0FD0E82345929E9E7E2B0B1A0FC4315514891E0A481543BD313D3D0D
              D843DCFBBEBE3ED8DADA228ABF661A1B1BDFA218B11421696030186E00FC374A
              A28946C8CCCC0CF50C576D72790A4751A63613323235D79B1B9BAEC7C98F7F2D
              2E2EFE89696E6EA68ED31716167200050505F02EA12030CDA5DEDE5EAA6D989F
              9F071CDBA052AB40A954800601A55209C3F0981B15B8B7B3FF47BE31FF1B0687
              99A1A6A6A61B23105399D130231D025D3A393909164B1BB0312C2808509301B2
              473260989B807799CBB1F386EB647452D8D4D4D48263415D5F5F0F4EA7134647
              47395EE97BEBF367175FE5EAC33E8A88E6F9C7839AACFB4F8E8E6D4663FE7326
              44AC876D6D6D2D0D0D0D9559595927D5D5D57F226D533897A6700C587FFEE5C5
              98B1C0101B1039603EDF39D8DFD8C1E9707AD82F63A6F0CC0B2C8657B8CF7D6B
              D8E19A873F1B13FEAD7E470A8E4281F05B746BEBB361499238DBE7F5C18E6BD7
              C717F0577439FA2E2CD9973855FFBA2BA34FFEE9BF73C2E0CCD1E3943C46C055
              0CE2A387EFE5E073EC5FAA25A356B1AB46BD0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000006404944415478DAD557694C5457143E8F7D51106411867D9861911DD9
              2C60A198A6D1184DFF55348498901A9AF48FD1C6586AABADD55F9AF0C3A68AD1
              B06830B608084D88344209E0C2A284DDA1303828FB22FBD0EFBCF69171184182
              89E94D5EEEBC977BEFF9CE39DFF9CE1D616969893EE410FE5700B45AADDDE8E8
              E88EE1E161F9F8F8B8DBC4C4849BBBBBFB5F3E3E3EBFBC37006C048787C188CF
              CCCC8C038CC8F02E5F5858F0363232F2D8BE7DBBB5A5A5E5F2FAFAFA7AD5EEDD
              BBE38C8D8D351B0280D9F8EAD5AB55010101A1DEDEDE966666662B160314B5B5
              B551575717CDCFCF9320081412124200941D1F1F9FB9E1083437379FD8B66DDB
              4FAF5FBFA6274F9E904AA5A2C5C54509206DDDBA95FCFCFC0821275353D3E57D
              4D4D4DAF121313C31105F58600E0B759515151BDBFBF7FC8D8D818797A7A9289
              89C98A4D3D3D3D0C960607070969A143870EF1B71F222323BFDD10001E8D8D8D
              27DDDCDCCE48EF1A8D86BFD1C0C080F80E2E904C26A3F0F0707272725ADE575D
              5D5DBB6FDFBED80D0340C89D7158BBADADADCDFDFBF7492E9753505010D9D8D8
              AC7A506F6FEF2CF813616E6EDEF25F346D5FBC78B1B7B3B333BEBFBF3F342525
              E59C838343D19A0078141717D7C5C5C54549EFA80CC2419C6B4215100CADD8C3
              5CA9ADADBD858AF10041BDC0257BA55269C6D1E201E26A51C2BFE2DC2F415EED
              AA00E0F90DB03BB5A1A181F801B9C8D7D79782838369D3A64DE21A94283D7AF4
              88E01D595B5B2FD9DBDB0B2E2E2E62C450BA1C118A888890B825CE232323F4EC
              D9B33C44E3E0AA001E3F7E7C06043CA9FB8D4BB0AEAE8EE6E6E608E9A12D5BB6
              90979717393A3AAED8CF3C6163FC70C474417019637C1F161696F556004F9F3E
              FD06D38F5C8A1C7E36060FC5284811D00F3F1BD59FFBFAFAC8C3C3839C9D9D97
              D7B23DE8CD54464646284AB94B04800D2E607972474747E2CB972F775A595929
              51EF66161616E22192176B193434731525272793AEA35CC648E16954D277424D
              4DCD057CF81A356CC268F5078843DDDDDDA45028DED9A8EEAC56AB69FFFEFDA2
              5EE882B873E74E456A6A6A8A50585858EAEAEAFA19722292CDD0602F38129CEF
              F502601E8074646767F7461AAE5FBFAE3E7AF4A89B909F9F5F8FBCEE60516110
              86066F8036885C586F1418003C25EE2DFF4660013D4443376E544E830776425E
              5E5EFBE6CD9B156C28303090A0826F1896C6D4D414555454889EAC65981F2E45
              762A262686A00930AA260B8BAF908A6AACFB8872730F6A21E1AE424141413FEA
              D8858D30CA848404315F86409497978B55A16F943BA3A4150C1424163543B761
              2191A82005F6CBB03E8D4A4B239720DDAECC8121C8A7BDB48C65970545BF3CF9
              1DD24A65656584F534393929B6638E083F0C5C6A5C7A0D4EFA05C75A01204014
              AF9B376FCE9D3A75CA8953308A14D84A1BB8E572A331140109043F5CDB20AFC1
              75D23B478B2B08BA22826D6F6FA7929212163A4A4F4FFFFBCA952B9E42565696
              3A2A2ACA553A8017EEDAB5EB8D8A3074B8A1DF9C126ED30F1F3EA4AAAA2AD110
              4B36F32A283808A5EC4BA525F7B8AF2C9E3F7FBEE2F0E1C39F0A20425F525292
              4C5752434343099D6B4DC3D3D3D3D4D2D2221A422312A59ABFB16A464547899C
              F0F3579242E94BD20D6B7A7A7681B4941B111E91CE4D49C8C9C9B97BE4C891BD
              2023B1F2F1608DE75B8F21EF5998C01BAAACAC143D65AF25833E721FDC9894E4
              AB9093EEBDD140592F4D8C4D95C444C77C21A0B9288E1D3BF6DBC58B1703AF5D
              BB267280F3CB25A9EFFDD0D010656767D3EDDB8594909848B83B92D24F2186D6
              CADA8AD63B460647FF147B01F21472FCF8F1824B972E056016A593CB884BEAC1
              8307A2B7FCB4B6B6D2CF17CED147F13B09C45DB741698C8D8D53675B07CDCECE
              372E774384D2E3F2E5CB39999999C95CC7CC01965F66320FEE82070E1C50F305
              23E9938F656EEEB27736C867F4A87A49F55CA59D9D9955C5C6C6E6E1AE700B29
              6FD6BF945A4172CFA6A5A565A04159EED9B36710046D8A8E8EAE47472C87D795
              20CE12EE88273ABB3BCF0604FA19BDCDE8E0AB41DC82BA49D3AF99707070AC41
              3FC8C51DA3182237ACBBCEE07D00B9DE8BEF46E043190CCE1932002DF8BCE8EE
              EFB931B131E666E6A690DE49EA79DEC3519BB3B4B06C888F4FC8474BBF07F6B7
              AD169D0DFD37E4B441584E0F0C68944141C11550D13F10A5BAB7817EEF00DEC7
              F8E000FE0132D51BE9D29ECD690000000049454E44AE426082}
          end>
      end
      item
        Name = 'Browser\Save'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001974944415478DA63FCF7EF1FC7945D53167CFDFF95A3C2A322808108
              D0B1A36303372BF78F1CA79C04C6EED5DD6B4B794B839A599B37D738D5F81163
              40CBBE964DB5BCB5BEDD8FBBD731CA35C97D7864F6881F66C081030756B3B3B3
              AB63D3F8F1E3C75F1E1E1E263003E4F6C87DC430E0DEBD7B39DFBF7F17C36600
              D0BB0CBABABA75780DB870E142D7AF5FBF64B119F0E3C78FFF76767651780D38
              77EE5C2FD000796C06FCFCF9F39FBDBD7D185E03480944EA1AA0C8A1F8DEE09B
              C153620CB8C07541FA3ED77D41140318A488D18A1CA20C0C18067031723144F1
              44E1D5B7ECCB32866FFFBF613740864586E19ACC3586F3BFCE33DCF97D87C189
              C38941985918C500AD275A0C4FFE3CC16F40F1DB6286D99F6733EC91DCC360C6
              6E46630396EF5C3E3DF6756C0683040383148B14C31EA93D0C2D1F5A18967E59
              CAB05262258321BB2103232323188380DD4B3B86A77F9F32FCFFF69F6115D3AA
              A98CFFFFFF6758B96FE5F48A5B15B1422C428C6D2A6D9C7F19FE32FC63FCC7C0
              CAC8CAC0CCC40C3700844B6F977E7FFBE7EDBF6ED5EE8521B6213900345C158E
              D9192C440000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000002404944415478DA63FCFFFF3F0310F335AE6B5CB9ECFD328B1B493754
              989898DE329001FEFDFB27AC314FE34E9468D4897ABFFA704646C64F8C4041FE
              DAE5B5AB5AA55ADDB87F72FFFEE4FA490A68C11B322D10E1DBCDF7ECABD457D6
              D2ABA5FB3AC23AC2188BA715EFEFE5EE7560906360A0A6050C7718188A5F151F
              60D4AED67E7555EDAA28BA05C78E1D9BF8E3C70F7D620CFEF0E1C39FA0A02017
              740BB4CF6BBFC669C1C18307D75B5A5A061063C1F1E3C7DFDADBDB8B9064C1E7
              CF9FADBE7DFB264E64E8FC171717DF4092054F9E3C89FDFAF5AB1C31A6FFFEFD
              9B514747A7657005D1DDBB77338191A74CA40F182C2C2C4A48B2808C54CA3002
              2C60E662FE97F034E1CC7FA6FFE498CFC0F88F916181F40293BF9C7F99B05AC0
              204C96B9D8C1805AC0C2C8C220CD2C4D92794FFF3E65F8F3FF0F7116E8B0E930
              1C933A46920556CFAC18AEFCBA42BA05133F4E6478FBEF2D8338B33843365F36
              F52DD07DA2CBF0F0CF43063556358633D26706A30555D82D382A7514CCD67BAA
              479905F337CC9F9FFA3035E1BF2C307309022518191834D9341936496C02AB73
              7AEEC4F0E8EF230615361586DD92BBC162C0CA1C4EC3B0FB0B7786ABBFAF82F5
              83E5EE02331DDF8239A04A9F71F98EE533D39EA7A5FC11F90396566553655820
              BA00AC71C1E7050C9F8050884988218E370EC370183BF27524C3CDDF37C11630
              FF61FE3F9361E6FC588FD8544650B3050496EC5832A7EE6A5D24882DC929C958
              AC52CCC1C8C088E2627457C35D0B64B7DF6EFFF1FCC7F37F207E8B66CBD22897
              A874101B006CF5E64243718C040000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000002F54944415478DA63FCFFFF3F0308FCFDFB57A27469E9CAC30C8755FB
              B5FBA7DB18DB343350111C397BA4B6F06A61A62DB3EDEDEE88EE706666E61720
              7146900380964BE7CCCD5933436B8605033F03C3E13F87EB6C0CA9EC80F3476A
              6D596C9B40ECE433C9A767C4CE0863616179C0F8FBF76FD9DCD9B92B6788CFB0
              641003CAD2C1010CAF1818926E279D9E913C238231B923F9D45CBEB9A60CE240
              093A3A80E10930245E269F66342933797646E18C242E079C3F7FBEE6C58B1766
              E458FAE9D32786B0B0307F4646C6FFD81C6072C3E4394107ECDFBF7F9EB5B575
              22390ED8B973E70B1F1F1F298A1CB06DDBB6F9BF7EFD7227C70177EEDCF95E5C
              5CAC4291036899068872C0870F1F9C5EBE7CA9498EA54C4C4C7F5555556750E4
              80014F0303EE80B76FDF7A901B05A052565B5BBB9FA228A0161875C0D07240D0
              C5A0AB721C726FA9E980473F1E09AFD35FA74D9403680E461D30641C60C86EC8
              3057642E457625BF496638FFF33C790EB0E6B066D82EB19D220778BEF06438FA
              E328E50E78F6E71943CEDB1CB85C226F22832F972FFD1C70E7F71D06A3A74670
              B936A136861CBE9C510750C101A5480E10C574C05689AD60F6DD3F77198C9F1A
              C3E55A855AC10E0036362873C094255356143C2908FF2F01EC230A431C00EB2F
              9A7398332C135B06B6E4C1DF070C4ECF9D809D3988195502550CE97CE9603648
              1EE610743AE45508C3B19FC72066828440F835907ACCC8304360C652C67FFFFE
              312E58BF607EEEB3DCD8DF62BF9918F8100E356237629825320B6CD8A3BF8F18
              025E06C01D502C50CC10C713876129BA03125E27309CFC7912A209AA97F939F3
              FFC9AC9397A506A7C6813BA740CC386FFDBC05456F8B627E0B011D01057A6C7A
              0C7DC27D60C3FE01E1B7FFDFE006B333B133B033B2C32D0599836E3908E4BECB
              6538F7F31CD872A04D0CCCBF98FF4F649CB834C53F251EA8EE1F232CB8418E58
              BC65F1DCFE4BFDFE203EB049FD5F92579225412581176E1A5274E30A76B843A0
              6AE7DD9CF7E9E9D7A77F60FA8BB48A36447944A5811AAA202100F814756771F4
              67540000000049454E44AE426082}
          end>
      end
      item
        Name = 'Browser\Home'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002F14944415478DA8D936B48936114C7CFF3BEFACEFBD43575EA9C9A77
              513145372FE19AF941C2204CA25022A3082CC80F254817FADC873E481412CA18
              2C334B2A0DE76543ACA5A599A2A2F392383775CECBE69CF375EFDB3B534B33EA
              C0C30387F3FCCEFF9CF31C44D3341CB4ADADADB0A6A6CE8A850513A7B050FCC4
              C787DD067F31741060302C9DEEEDD59501381D3518C86989C4FF6960A0DFF3FF
              02D8EDF660A954D74F925E9C98182DD4D7F70141ACACE4E70B5FA5A444CBD96C
              CF2E84D0C6A100E6C61B1B47540643501642344447CF824CD6C994B308E9E965
              80100618B6B11E1A36F7FA8438E13A8661CBFB00838393F7D56ACEBD6DE70E40
              2EEF068120097C7D237693D0803ABE4445723F88C5D937F70066F35AB65CBEA9
              04C0718713C328888AD28142A1839090B43DB9383E342D0805D7170DCD44D5A3
              BB42822046114551DE32D964BFD57A44B05717A380CD5E87A5259C81B9ECF84C
              6BC1C153149F1FEAD5FFAD57B56527CD174B8A0A905239241D1B0B2A867F988B
              CBE749912836DC6CB63926B5A168572C56565C2B457AFDDCF99E1E7DB9C1E017
              65B7BB7B1EF618C7A77542A15BA043CDEAEA3A732C60B1CCB770B9EC01343333
              7B49A3313EA32804261361351ADD318AF260FD7ABE49F178C3CB8989711CDDF8
              0818355F810ECA0016CBF6362F2FAB8001682F6B344BD5BBE1348D188938533F
              9BC9EC0E365327145EC880898E7AC07A6AC04C70614D58C9F4887A999B9B7916
              A95A9A6A49F02BC19D59E877D90E90A3892ED43024678A61A2A506580375B0EC
              E40FCB29B72020C04926168B8A1D53F0EA5377DD1EEA56DF088C9778E0CEC4FE
              FAADB3C08F4D86EFADB5E036DCB00D984F28074F574BDB99A25327F73E1203E2
              7E52B5568EF5F55DE127E5B9624E3F41C8A2055E642268DB6BC063EC0DE85100
              68BD85C663C7D3A4A21C49E51FCBC4EC03EF63FBFB3B534323A541091202B3EA
              8123888391E66ADAB66E9E119EBBFA303236FE31B313F643B771D748920C57BE
              6B78303F3E2A894815D5A566E554393B13E307E37E00803755DE0009CAAA0000
              000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000004E24944415478DAA5967B4C5B551CC77FB7053A5A0B6D29C5DB09C8AB
              B4850EC66088C82A2A263393B14C912C8E6464602499F1115CB644FF5043A6D9
              E22326DB2461138C10970C918846CA0685096E8516471FE13D1C6D81324AE9E3
              F6715B4FBBC118301ED92FB9B93DE777CEF7F3FB9DDF3DE714F3F97CB09121FF
              8EA92963D1E5CB7F5648A5929E8C0C71338341BF095B346C2300F285F5F569CE
              984C96C8FEFE117E5292D06034860B4E9C10EC7A6200413825F5F5E69BE1E10E
              0B93E9A5D86C1E8BC1E0B11D3CC83E1B1383D73D1100F5311A1A8615566B94D0
              DF160AEFC2F8380172790FE038732E2181A92A2CCCFF9AC361B56118E6DA3640
              2ED7D6EA74FC638101980F9293A7606C6C11BABA1428B331080BC361F7EE0AA0
              50081B8F67EA110818B2D8D82859686868DFA6008361B6A4A525A461A94DA1F8
              40209882A12133F4F428C1E99C80CCCC77814EE7AE9272B9525226BECBCDCDAA
              7A2CC0E3F1C4D4D54DAB3C9EA7D80F015E04D023800501FA80C3C12129E99535
              5953A8FD036DD75A24976ABEC8A6D3E98A3500F40E6A6AD2CA4DA69D392B2752
              A95E24A8079DCE0CFDFD3A484F3F84A0D455359B59A0D307BCF21B4AAA283956
              FDC1FBE5B9987F6D570294CAE1EA5BB778A756471614444262A201B45A33CCCD
              D181CB8D5F1B3DA5539F9D9DCA576B6E775E6DEE907E73F6E3623E1FBFB20CB0
              5816A58D8DEE767FBC6B8A8402E1722D688C0DAD3FEEEF59E557FF2712054747
              44E060365B5DBFFED6ACF7A19CBEAC3E29465910184992DCFAFAC901A793CD87
              6D9BD5111EDEEFC9CCCC62DAED2EB0DB09989C1CB979A5A96DEFE99365A7D2D3
              2467B0AE2ECD0F1A0DBF7CFBE2FEE5EBBD9393931C4BA58680CDE64480FBCFBF
              83B7145AED58F2C5F39F27635EAF97D5DAAA92CDCDB1F804C1C1B72E7F675A24
              72F0A2A3E3B025E125C8FCBC7154D6D11D75B8E8C5C600A0A36370DE3FC56209
              769BCD0CC2E90C636E24EDF3B94926B3775E2ACDE13A1CEE0791138F64515C9C
              CB46DA0C7F0D223A3BD5A69502365B284C4FD37D24C9C0D60310D62EDB6B8512
              068DC680857B16B83BD009EE2915109C3408E28903802347F6D151911D7E000F
              01A6D713B2DB7780D1180A24F93021B7C30862C90C885376C1A4EE36DC6BFA14
              58600DF8C6E34B818A6704B2292DCD0F42001243BB1797CB35FA8D96C4E1A0A1
              8C68E83060C1ACB101DEA92C41BFBD30AAFC1B68B2CF96C78DC69502C64B43E3
              1DEEB2B25743FC7DD8F5DFAF9E9FD52F947313F650611373B982C1BBA086CCFC
              82C0328C296F00BDA37AD93F147314205282F68BC37AFCF8FE40DAFE226346FD
              D49BAD3F7EFF0983FD6C6A94307763C8DC08A4BD908FEA44C0B8B21B98DD5F2D
              FBB4D147C1C71183DBED98AFA838C00900569C45D4B1615DB9ECE79A2A363F35
              3E32296B5D00611A06498E34F0C54C28BB80D57B6ED9A7E6BF0D245B0416C390
              BBA8E4A5F71204A20B6B8E6BD40E1E1F193AF657FD85D391F159B19C58C9237E
              A7690884597901C0A4520E1CC5B781FE45920A83B4BD240BC79579070A6BE305
              A21A5464CF63AF4CFF65AF1B5455B6FF74B18A2F963ECD8E49BD5FF0996110EC
              C97900B8061EE52F60F232ED712F1F6ECCDB7FE81C8D46D3ACD4C1B6F0AF82A9
              56292A3B1B6B3F7A26AD203298EA839D496218FE47E6B32DCEA89E7FFDADDA44
              614A2D8AD6BEDEFC4D014B86F64B94A2BBFD43D5F5D63744CFE5FF91B9AFE012
              BA58FA369BF73F27CB81D19466E6360000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000007334944415478DAB5977F50535716C7CF4BF2124C404882F941B2460D
              0954A12905F99940ED52B7033BB5BAD5EE5A67D5FD55AB5DEDECDAA95D777667
              3AD34E77DBA9D3DDB5539969B78896ED883F6A19108A826C09A21628160AA640
              223F430249CCEF8497B7370F7903423074DBF3CFBB73EE79E7FB79E7DE77DF79
              184992B01C0B06834ABDBEE3F98909BB48ABD55427260A3A592C563F8661CB4B
              74CFB06801501CD6D73770F8D6ADFE8C77DEA92D369B9DDCA347B73408048A60
              69E94347D96CFCF60F06E074BAB44EA747D1D3839D2208A78DC5725C2308CCDC
              D292B40DC703BE2347A4592C1673E80701088542A28A8A3B5FF9FD7C09971B82
              8404B78FC5C24893C9636232ED56AD965F959A9AFCEE77117F20009A635457F7
              5C1A1B933F31EB4B4D1D4650000D0D46181B1B84B56B45232A15F7A64E975529
              958ACFA0BD10FADE007A7B8DAF3437F3DFA483D13E4B4919A1C6D5D5BD303939
              06D3D313E0F38D405252013CFC70B1552E7736AA54FC06B95C548736A7E93B03
              783CDE8DA74FBB5B4812C7677D0C06096AF50CC0679F7D035353E3E0F70FA14A
              79203FFF55C071DEBC1CB1B1E65EA592FC5CA512D708040997A20640BED8CA4A
              C3972E97583DD7CF6492A052CD0054555D07B7DB4D01A8D59B4122C98AF8942B
              B8379B4B4B1E7D5D20E0D74705D0D8D8536130C876DDEF6732430860941A5FBC
              D80D36DB04F2052133732F5A1E6CF1126377462F5F2D976C485DDDFDE7A3BFCF
              4071C49200C3C3E65FD6D4C4942F968CC5222039798C1A5FB8F035381C56484F
              2F012E5710419C08050275E65B3D3D31533617FF8DD75EDC979CAC3C1111207C
              CA555458DAA7A7792B174B88E30428953300E7CF770187C38775EBF220B27598
              64B26985C7EB6B3E73EE72E1AAC478CBBFDEFDAB0A55C1B100005DF1B367BFF9
              EFD4942C27523A367B1A098EDFAB40371AEB908FBB682C494E3A399C36766EAE
              8EE3F7FB89AA739F0EF419865407F7EFF8BB569BF7CA02809B370D6FB5B78B0E
              2FF138D41E90486CA8E401E8ECF4038FB726622C86358D68346B65F1F142F49A
              0661D068E8F8F8939A0C369B19F8B0EC8D34369B6DA0016C36C7E6AAAA502D3A
              771810B585C1236DBCDE61A9D42E5FBFFE11241EA000FCFE205CFF52DFD6A2EF
              CAD9F253DDD9E7766E7B868A25084274F2A4A933101048A3175FCA3C7E26B339
              50585810876A360FC062191D3A7FB14EE4F30739278EFFE5093E9FDF80DDB8D1
              F7767BBBF88FDF8F787899DA4C6969AB1462B18C120E0384C567C7236386AB97
              EA5B8B34E9CACE3F1D3990492D815EDF553E34C4C971382429FF8F38860D5984
              42636276762EE6F74F2F0AE074395D2DADCDEE71B34DFCB7D70FFD9A02686ABA
              D64892BCC7BC5E3C64B7E376972B51B07C7122C46235DA74BA6C219B1D734F70
              2140F8AA52AF7C4D28E4F7793C9E847B006DCD24C9D5CD26F3F9D86863C6912E
              170F8B1680C1E8346ED8C05BA350289160601E80C7E5862081D1007979ABF729
              14ABA9038902686CBCF60500AFE0FEA47E3F0E562B177D98C2E752649680D742
              8A93BAC8C2C2C719810041018C9BFAC1D2A387D0481738BC41106F7E99DE8C85
              85CABD72B9EC231AE0CA956BAD18C6CB8D24100CB2D0A737167546B1E1675D30
              EF775543E9D33A743EACA444DACA0E43E2DD5E8865CD9C3126B60A62B4876880
              E2E2D49D52A9A4720E405B1B8671B31F54E63088CDC643DF80300893F2396C9D
              90AFE5A04665032D70EB1FBB410253F47D26B61AF0BC17E9F99292F49F8944AB
              CE5100A8E562D4D736DF66AF1028A35D6F826082DD1E87FA010E4C0C97C3FE97
              F6A1C624440BF4FC130130EC74BC11570333673F3DBF75EBA3A50281A0860268
              B8F09FB2EE2BB5BBD7E99E65F384F2681928438D3278278D905FBC69DE81D37B
              7C0F4899F4F70606F0546064FD8E7E1B76ECC8294E4888BF4C2F81DBE5CAB97C
              EEF41F8CEDAD5B95BA5FE05C4152D4101EEB20E46C9A0B1080DBEFED8124DC45
              C7F42300C8F80D0DB06B9756171717FB050D306B76DBD48F3F3F537E68B4BBA3
              24B9681793CB7FF0E9ECB60CC2C6A2A239EF7B00FA4F84013C74CCB7AC8720A4
              D94B033CA693BD9AA6D1BCB90060D62CE6F12DF5956507274DA64DC9853FC756
              24482257C03200195A1D5DFE30C060D91E90717C74CC6D0440A4ED068F7D021C
              03AD4145B2F8EAF6175E7E1EC7F18125BB62F3D8E8B6BA8AF75E725A27B54AED
              B318274EB8B00213FDA0C9D7D2005EAF1F863FDC0B499CC0CC7C10839EE91F41
              204E6C4DCFD75E2CF8C953EF7379BC1BB3F73FF0C724FC4B3672C7F45CFDA9E3
              07BCB6BBB9CAA29DC08915CC0348CBC9A7013C6E0F98CB7F0B04C684D1008764
              4A53BE2EDABEE7D47A4DE671D409B9EFCFBF9C7F4386B1DFF0ABDA7F1F3BC820
              58E96B0AB623103EB8CCDFC2FA8D791480FBAE03063B9A60F046AD47F3E48EB3
              054F6EF960657CC2D5A5F2460D300704EFEBEE3A50FFD1B1176262846A514A16
              889324D0DD74812499D0F948F1D3B599F945C7180C86359A7CCB069803C2FDEA
              BAFEB0FED393CFACCB28E8C87EBCE4638130B16EB979FE072B51F2E58048B58D
              0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Browser\Item10'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002E74944415478DA95D25D4853611807F067C36D0E3F56CB22BF46EADA
              9A6E9AE6C7B4662E09323266A24105C22E32F1A2341194EC032A082214BA08D2
              82206DB2B41C915DA466D495A1964CA7A6CDA6A6CB52CFA7673BE77476D0C1B4
              2EFAC37BF3BEEFF37B5F1E1E01CBB240792865CBEB966BDA58ADC3906AB809FF
              1141A3A5F159CF6C8FB66BB52B0968800E6347A329CF74592010301C1E3235E5
              2C5B5E46A2D3D2B40DBEBD2D40D1DDA2CF9D68A7CEBFC321EF0AFB6F4484ECA2
              C6C750B3441C9F80E31E90C9A79FE4E7679EE790B5008024494D415381AD97E8
              4DC860B297CAF6567C57471E8A974A22C3831C2F0016BF807BE771F8C9C642A8
              6CF2557189F12C87ACF8015F0F46ECF6AAE151A759A530EC93484283280C07F1
              D043F0E22BB02A55837CE41ED895B7C1CD2132F94CDFB9B223A78542E1A21F68
              69B6BEF47825BA9C9CC238866181760D4398AD045C079B6149C401938F812269
              180B2F0602F540F88EF9C1F2CAFC629128689A07DADADE583E7E583EA1D1885D
              99592755044E01CC0F83FCFD45C0C375C052180C45D6C22A29061CA500C33864
              FBE2784DDDD153EB40B7A5BFCF5BCA323895A0F44CA76798D404C621EE7188F9
              540D0E4535CC099380E09A892214F81E20C9156AF71EFB280FB4B6765BFADEAE
              95D25E06680F412BD5842339D994C8FF64D90911DF5A61487E0108AE78059B41
              65115F7F4445CB143A9D4EC403288A65DDB8DAD1EE5E902A680F030C4DB1712A
              CCAE561D4B5A2319FE45F7AFC10569C8ECDA81745D4C7676B6D0D7C089890996
              077C415154DF506FB52CB8240ADA4B836FC5AB91D1B06D528938F877B0D1981B
              A5D168028628005847B2EA6B2CED7333A2758481F2EA1836F7708E60F3047277
              C16AB52E05001B48EDA5A7EDAEA9201EA9BB950229FB13FDE7188681CD6643B4
              5A6D935EAFBFBE05F00541507D55C5230E11C75EB993CA035B0A0502DA3F487F
              8B0FA9343F787EC69C1A3537EFC4954AE57D83C150BF511830CAFF0A82203903
              0303A6BCBCBCBACD851BF90369E08E3C043C33820000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000004E84944415478DAAD957B5054551CC77FF7DEBDFB602FB01B2ACB1558
              58244A196C12B7A4D01A90875432E9148F1410819AD440C09AD08AD03F739A26
              2A99AC3F6A9C1A289A819AE18FC01C66E511C2B2B8C22AAF855C7CB0AE28ECEB
              3EBAF7B28B800BDA4C67E6CCB973CF3DDFCFF9FE7EBF732EC2B22CF08D1BFDCE
              779D3F2616895D1BA3365E542A946DF03F34646E6E2E4E2A955E397DEEF4B98A
              6B157BF997F148FC64FBFBEDA91289C4E86B11B7199C1B280441D84702324F65
              1A46674783F4983E04B007130968C278F3E1E682C54E689A0E1EBC62CAEBBB64
              D9A70C42F5E9BB76E473106A55404255C2980ED7A97D4D6A59ED84EE84EE398A
              A214BDBD970BCD63746E60C0932A978B02A79302B164F0B757776FDFCF41EEAD
              08304F9ADFD8FAF5D6B337F01BC4F2C9145786257F738949416CD212F210192F
              CA8BBB9CDE4E032232B665E72667A1287AD327804FF2C8F8C85BDA3AED9969F1
              B41FB80072C405E617C3D2DC5B62332250548AF162AC6D0202FA3E03C43A0C0E
              79348C451C8259462E005974B0ABA07067368EE3233E017CEBEAEB3AD6D0FA7B
              FE663291887D3A298CB30D0CC30A3BA5EEDB20B8390366C854B0CB2280B8F613
              60760BE8E37E80192640F886C5AE0E94BC939CC3158CE12100D745870F1D1F2C
              2A3E19C50B7B1BE5A60137FE0C7EC61F61D63F1226626BE643E37041CCC537E1
              1EB1090C64E542B8001FBBFAEE919773E57279F772005E74F0F3A94085DB9693
              53AAC17131FF4E5844CF4C81F28F6C60DC7618D9DE00765A268425D250050C45
              C3A5D0131EE87C5E10DC3279A4E2853C8542D1BA04F076C957161C8F08124B8C
              C37BF79668645202717A124AF321FA731F80F30E4C87670376771C94D317E0EF
              0D5F8095550940A79D068727F92876F376E907DA62952AB8710940844504310C
              037EF2E1E19D29BBD501FEA4C85B3194FD1E84B5178264BA076E8517C2D0DA3C
              98A3449C383DEFC04ECD038412A6394DEB6CD98771EF2D004A4A6A2D18C20168
              06F82E274646B5CF2791C1EB34123E54FC42B7C30911BDA520B119411F530B56
              085910B70BE2F33027972354DA6D96F95BB10780A22F2D08A83D0016782732F9
              A83936F6D9A070F53372AF13379FE0CB15C072F33AF29407C08587DB80C37E9F
              6125BAD1400515949E9EA698999961BC00B4E6D3BA8B1363C15ABE34BD2E0427
              0196C9E8E82842ADD62A84DDF17971B8C1ED7471CF20ECDE366B7101D66D0E54
              62E49E3DAFFB9124295450676727B3700EB8EB20FC54CDF7BF8C0D3F11BFD805
              FF2C965EBF15B941C5466976AC13C2E57173DB6AB4BAA1FFAE4AA508CBCACA12
              71E5B9E4902D0178206135D5677F1D3129E317BBE06112D9F49D508D9F5DA34E
              26FF996ABFEE725FA3639ED2ACCFCCCC44310C035FADA3A36329C0EBE464F577
              0D4303C4D6E54E50D19DFB6B43471DAF65A6AF494C4C7CD44DED1BE075F2F1F1
              BA46D380FF96C52EF831FF5030A4A4BEB4AA30AFD9D4D4C4DA6CB6BF7C02F8C6
              DDFDEB6B3EF9B65EDF29DEB6D8C5812321AB025A5A5AC06432E9CBCBCBB3B81F
              D6E08A002FA4FAA3BAFA5E1DBECDEBE260290929690F03DADADAC06834F69795
              95652FFE13AE0AF040C8EAE3671ABA2F6002A4B83C7409801736180C03478F1E
              CD95C964FDCBD73F12F000F24D7D472B9250C839D8F54A9220DCD3D333545959
              B99F2088AE95D63E16C00BA93A56DB38E79CD8B2669DFF20B7E303AB09FF6780
              3727DCF18F512A95AD8FBBE65FE91025AF68F328090000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000006FB4944415478DAB5970B50545518C7BFBB4F96051504966730A14944
              81654E546848200B81BB4A41BCA48744EF89B2D246A766CA514BE2E58052E124
              9A010AE15899F88811155FC8AE28A2E50B2481405ECBDE77E7EEEEBDEC8A3C64
              A63373E79C7BEFE1FBFFCEF7FDCFB90BC6B22C58B7B65B6D09188631B81157FA
              F9FAFD261289BAE17F6C9835405D43DD9AA89AA8B5B80C9770F7EB7CD6557FFA
              CAA74B11103B658589000ED61FFCC2C9D1A9D76034D8ABF7A8D70ED80DC88597
              24C6EE5EB03B4F13A9F990CBCA5841D022665CBA743975CE9CD945681E7D5F00
              691BD34E6D376C9F0724BA93DE63067ABE61D6863D2B97AF7C190527AC5FE138
              1E78F264E38A0BE77BB46EAE4FF8E2D4A9BD8989B149689E61D2004F7FF2F4B5
              638A63BEE3CE426B2A9B57B625253E258BBBEDEBEB7BEEF8F1C6577B7A94F12E
              4E0F4D27491A28920182A0C0606C3898B63CEE25E49D9E4901D4D4D67CAD3DA4
              CDA665B468BC897E037EBD7B33F67DD9D27253ADB40F5C686F3F534A113470E2
              6600AE67CCF7D4D9A3A9CBD52F4B2492B609013813EED9BF2737E168C27BAC84
              C5EE9E201D943229F2D7DAE77A2DC0E6CF5DE28D81C42C423016516B00330437
              36108D8DAFAF50A7CAE5F20B1302A00B2BAB292B4E3F959EC9FBC0ABD7673849
              95D1F18847E8B4C7E746BB709B4510422B77BCFC1328CF7F07E2C11B40C99C61
              C023022EFB66A3D5839089615CDFB2222B32C3C141D9302E00D7502FCEDF91BF
              E3FB13A56A8D6F4AF7BC8068373FDF471DF88914650ECAA57DBABE1014677260
              68F68B80CB7DC0EEF609905EFB056855189C092A019CC4844C188996ABAF672D
              78C3D9D9E9C0B8009D9D9D2FECDB776495BFFFC22767CC50D9EC076E0E270EFD
              1D20E9D481F3A12CB83D7F2374BA2D06DE805E7F1580937E3DDCF1CB8026EF55
              964C5932415CF927F3ADA7DE727757558D09D0D4A4FBA0A8686FF6A245B16E01
              0121329B4D409B03B91C790718FD4E104BE47045A303232BB731E0632793404C
              F6C09F41BFC08841512670CE1337EFBC9219F8F1ACD90F968C09F05D497B0E41
              9C6B7FE6D950A79090E7ECF949A4A5EE144182AA2E0B287D19404032B406E70A
              A5E1AEE0D349285B181C9D5D3A624ADC9C21DC14A3CB9096E9F759505040EE98
              00DCD888EB6E87840428C2C2964C6318D6243E62400A3C1BB241A42F05DA3706
              6ECCF91008460A3EAD39A0ECAE07DD4385D0217F4C00E056CFF5B8C51304D54B
              C42528F32322C256DE13C0B42B90284EB474CD09F010472C4A74E60D65BDE53C
              9BD6804C570C5299022476CED0E3100C7F7BBD099D9259A615935600B8F077E6
              E70469A0E312C4C5EA98F0F76C004AB6B6E570E2DCAA5906A50DBFDEE7FD808C
              8C89C970B1096AE9BD2FAE8769CD8560F48C86B3B336014E61B687126E1EE377
              1D565C8F0ACB864519768C009CD37DB075CBCD1C5EDCD4D328FDD4AD81996E83
              43F1716FBBF3FB9BB20AE8FD5721385FDC04A4F37C38E1BF1986699955FDD1F1
              3CEAA04217D3DA2B53B6F403669C29009C43005B8A6E5832C098CAC0D0E63149
              750F3BB976F5C4C5BEEB352A13C81F3E6D3F82FBC575605045429DE77AF37361
              F523D046FA6C072B6A651EF4F7F1484F4F17D5D7D7D302804E77FEFDC2BC2BC8
              9D180865A0CD99301991EA21150E57BBB49A8F3C298A150E251EC6A36B3FB42B
              E6C110ADB4723F771AE230481CB9C6C02DFBE0E020D7C4C4444C2C169B346B6B
              6B69EB9350B679F3B68AC653D3E201443665602C638A1AA2258AF3B7E3E3B23D
              8191DCE33B3062B481C15EA6DF70F03A26BEE3121DBDD8312C2C6CD429680360
              0D71FA84433CB09850061E86BB688A00A983BE2D2A22CB5B24B21B05D0DDDD6A
              E837D47748E594175AAD5D6060E0981FA203070ED802081005DB2A1A8E291084
              482803EF0D13084D83CC51DFF64C68A2A75CEE2AE2003A3ACFFC7BA7FFF4A08B
              ABA3677272B254A552C144ED9E001608BB6F734A769F39AE8C611911D81853D8
              A634C81D9BDBDCBD5DD861E34D89BFBF9F7B464606A6502826149E10C00221CF
              CFFBBEE2F861791CCB5A79C22A13DCF881872FD25FAD5B2346BF80262DCCB79D
              3B778E0DC097A320EF878ABA3FC41663DA8A73E3B8243924A768EF4BB8A9A909
              AAABAB87962D5BB6615C000122BFB4FCF0AFB04430A65549E293ED260DC00957
              5555196263630BC2C3C33F473F5E8D13028C64A2B4BC762FBDC4644CAB4C6852
              151302E8F57AA8ACAC3446464616454747AFE684F97793021020724BCBF75793
              3699D0A62180D4A5630A979797E3515151C56AB57A15121EBE7BCEA401AC32F1
              F36F15460D6FCCA5CB95A300EE125E3DDEFF09F705C043E4E7FEF0F3BE5DC31A
              CE98DA7405A4A62798DE353737C3AE5DBB26253C65001E62D3C6E2DDBF57922F
              2C4EC0E0F9A867B92D858786866ED36AB51F23E1FEC9C69A12000F91F3CDD6CA
              C387F687BF9699B65DA3D17035EEBBDF385306E021D06E70455FB7F6A9C6F80F
              B76A61A8252E9AAF0000000049454E44AE426082}
          end>
      end>
    Left = 32
    Top = 264
  end
  object icCodeImages: TImageCollection
    Images = <
      item
        Name = 'CodeImages\Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002C24944415478DA95925D4853611CC69FB333CF745FCEB94D5DBAE974
              7EAEAC919515F6415A8A46604A190666185E4464445D74D14574212614791145
              14064A7825215426442BBFA0DAA0F26BA8CC955367926EF39C9D73D6D13EB0D0
              A4075EDE07DEF7FDBD3CFFFF9F088542F825C14BBBBB5FD7C5C7278CD86C9D85
              0481506969E5758AA286B086889580E6E67B0FFBFB9A4ED2CC2C685A0CB59A83
              DB1D3FD1DAFACA401044685D404D4DB1DB92A5D2BFE81CC5817C139E3D1F4678
              F8305A5A3C4962B1786C5D407575FE6C64A42CCAEE98C28EED71E8EB75213E61
              1C4D4DA356A954FAEE9F804020905D5595D59B929C2E19713A91926CC2E72F03
              D0EB5921CA99277575574AD604381CEFCF35341437188D8458281C485238108C
              3C2E0D5A732DDC1E163A4A6DDFBBC57CDF9468BCF5078065D9D8A31506171708
              88853E2033530A3282843F5A0453EA59986373D1D8EEC5C2BC1F47366B272E1E
              DB9724C0D9DF80C1C14FA76FBC29BC4BCDF1201679204204521385E4980A68A8
              4D5029E3B0E06351DFF61187AD3193E539DAA70AA5A63B4E9F706719C0F3BCEA
              C4D5F449595A5042FCA466A92EA0C36E06C3D0082E2E22480BBBE08FEF32BA4F
              E5BA2882F713AAC45A8B4824F22CD740889150586674E63BD8B0C7121F8A2EDD
              866D5C0305C5A1D2AAF0EA54F299A57B3A7980C9503EDA088E8657DE5816ADD1
              B62D037C3EDFD68E4399FD3D2504E69248A4CB2EA3DD168D6BC5EAD1DDA69706
              12B324581F7E2C3FB8909AA33734ED145ADBB71421B2A7BEB1792C8C11F5FAFA
              5355117AEF9E6D656F3FCCE0E05EB368C112DD9A4D0765F437DA300B9E01781A
              5F39ABDD6C29285A9A4E8263D918866162843C0C25910CAC6C9173C8DE982CA9
              3F4F0795B497DBEF26281D199B9897263CA4579DC4BF353333550EEF839B1A74
              C57AB822D73C91339692919BB7E628AF268EE3F4D3D39E02A95436A15028BB84
              DFF9FF02ACA7EF603330F0B482F1EA0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000004E64944415478DAB5967D4C137718C7BF77D7A3BDB620B4C5561C142D
              546B459953086EA8D33998A233731A17CDD06C51E736898951B70433D16CD3F9
              C7884B70E21C5934115F8841A3B83775B8CD9845059D6E1329AF2AB4BCB55C4B
              7BD7BBFDAEB06524E00BC99EE6774FEFDA3E9FE7F93ECFEF9752B22C632823CF
              55C469298AF2F23C3FBDABAB2BD96030D4731C77833C93F084460D05A8A9B9B1
              B1B070D11E8E0BB11C37A591656F259B4C0CE5760B108439B5656547671348F7
              88012B57CE6B1835CA6FA5698ADCB9D0D2AC8686D32125C587EBD77DD8BEBD6A
              6B6666E69E1103162F9E1AFAA870217BF0500D9A9AEF63F7C74B70E0AB6BA8AB
              BB8DA953FB60366F2E2F2828583162C0EAD533C32A464B9FAB4E8723E902D29C
              A371AC2A0DA3B51558B54A447BFB929F76EFFE7CF68800E49ECECFCF145F9C33
              89DA5FEA42AA4DC6AB8BED3854D6043F5F87BC3C09F5F52F5C2F29393C6D4400
              A5C14545B9C52FCD9B8F4040C4B9AA5FF0C68AB970B93C6868BC0AA75383F3E7
              FD626565FDACE8E8E85F9F0A70F6ECE9BD6565EB0A9C4E46F5F0A10096A5101F
              CFA2B13188B83815626319A8583562E39271E777936FE7CE038BF47A7DCDA326
              2A025064D9B6EDBD6AAFF7E44CA39105450D7C483CC3F47B995C129F7D1DD6E4
              B771CD1540833B001D238B932DEA8719E30C5F4FB4A76C1F16505171FC8B8A8A
              0DEF2A996667C7A0B69627526849057424B8A8A5C08F66F0BC731F4CD11AB8BB
              59945475212C8A080B0236E5DAEE2D9B376322A9441C12B062537A17A76E8BED
              7509189FACC1D5AB3E6464E8416969040C3442441AA5A805CE52B87B8C707BFB
              D0E8F6E1DA9F9E0864FD5C5BCB9ABC2C05C00F0958BEC5C1C7A605B5745806E3
              974149A42F447F81008CFA44CCB06E452FAF814E79C5B08AA4E8E812505C7937
              02783FD7DE3AD7A9F38A412FCF7289E58949E3F60E02ECF874F3C5E68493B329
              A65FFC7FAE8A3C0B1C5F62FF7704268448300162A85F96C87BB2C282884D7993
              5A7327F7C0CC9E19DBD697D31C9FBCE8399AA6DDFF6DB2EA839D1B2ED7AB2A33
              39138B708F04FA41181D4102C829C7A95BBD1095A0212192717FE07E88924CC9
              DA8CBAB4F8EA04ADF49B168C199DEA2D4B0D4663C5A0319524292E27DFDE62AB
              F46897313ABC13742375A91DB39696E27C4D07C95C80DD1285B7B2CC0FE2A235
              FEC8A6244B4509D033F7556398A35648242399814BDEB16B9CCD5E3808D0DEDE
              BEF0D8BAE9672634D138F126104E20334F47615A5C298EFCE8211988D89F6F6B
              B28DAAB644510D5190C9892DF5912F06FB9734E05526B4719FAC315BC6940D02
              DCBA78A9A871FD9A0F4F6D0523C590C2C9EF036D0272520E88A5977A5509060D
              3E7BCD78C7AA3FEE80240C04EC1B1C9CF836AC6A31A5E667300CF3E05FC0CD1F
              2EECE2CB4FBC62DCB0F61B9E74E49EEBAE7DD2C4C9B5369BED92CFD7EBB872BB
              61972833A6F4B17CA7953BEC5002F9C4293EBF94E48592A01C8EACB0A40A0568
              C7095BAA73CBA029F2F5F4640BC1600CE90363B2582A87DBF6757F5CBE90A23B
              3847C9D61B767AFDE1F1DD2A561F25D0A6A02529CB39EC3E186872BCD237325E
              9EE1007FDDF9F95BBBA6787E441AA50A3ADB775F5CF2BD51D39A65B4BE9C4400
              C2634FD34799C7E35EDED779A5E819F6E40439D0042ACA826E5D41A7C7C71D49
              B14FD9F8D8D3F4498C546A6E6D6D5EEEF376A46B34D1AE84B189A7D56AF5CDE1
              FE083C35E069ED7F07FC0D41856BEFED7CD01D0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000074B4944415478DAED977B5054D71DC7BFF7B1AC2C2CFBE255960596D7
              064D793462E814A52551A26D2D125E05343132D2D83451C758B5B13A661A3226
              9AA871D04C44A704D268224AA71926339A4C7510D34163451C164128BB5979BF
              D65D76EFDD7B7BF6AE181CA9DA864EFFE999DDB97BEEE37C3FE777BFBFDF394B
              89A288FF65A31E04E07038D22A2B77EDB65ADBF51A4DC4F0BA751BDFDFB2A5AC
              5214FBD5DEEB1E8F76A2B878D3278585851B288AE2661DA0A868597B72726BA2
              42C180E74534374F20333308344D1EA400EFA3D5D57DD8B1E3C37D797979EB67
              15809CF7CFCA0A1B3325C6CB424335E8ECEC4148E810AE5C6110A9FF1E464686
              90FD9480DE5E17CE9F0FB3B5B45C8E215170CF1A80D3E94CCDCF7FE2726DCDAF
              50F7D145F01E06B535E751575781AEAE5BB8FC751FCE7ED1889C1C15F6EEFD06
              ADADFD8B542AD5B95903B0D96CBFD8FE5AFEA9AC4509D8B0D50A16A328CA5723
              77791256965FC66D870863643BCACA025053D38F8686F62283C1707CD6006EDC
              B8F1DCFE77D71E93FBCB70F44F11E026BFC19A522050C9E260B516BC7B04B1FA
              8B2825000D0DC3A8ADBD36BB004D4D4D5BABAB37BFB165730ECA2B4E6378D881
              B2927928294EC5CB1B3EC3D8981B2E9719CB97ABD1D8388AAAAAAFCA4D26D391
              5903D8BCF9A5731DE69399156B7F89F4F946BCB6FD24DA3B7AB0F7ED55888850
              E395F575506B7A101323475B9B134A65C1F90307DECB224614BE1300E9531F7F
              5C5775E2C42B6B535214D4850B1C31A40C26931B2121329C3DEB848CF5C3DC79
              020C06B9948EBE888D2337F7AD636BD694AFFE8F01388E8B5DBD3AEF338ABA68
              321AE5D23986B973E31DA52941E948BE51090B111EBD042A851E43030202FD35
              57342AE5DF62A30D470302029A1E1980A45DF233CF24FF3523C3A16259EA1EC1
              29D1E9306E3985AC9CB731C1A5C26C73E0CB6B36B8380E34194FE5CFA2607EC4
              507A74D07BF171B13B1F0AE00D7B6EEE4F6E1A8DE6686F28B3B355A4DA51309B
              9D4848F027C2D3605802ABA1E15433285C701A7E0C0FCE6343D5E773601D1887
              E0F1C0C3F3E4C8E3ADD2D4AECCB4B9090FF28504D0DDDD5DF2EABE1FD6BABB5C
              A4A0385050100C411089B36FA1BC3C0CFEFEB424CE0752B81DCC4220B3A76906
              2BD2EA71B3FF36A2747ED87DCA02D7A4DB0740C40502B1333FD9B2F44729F308
              C0F80301F61FDCF349ABF2E0B332A708F9000FD62E900BDED72242A1A0E109A0
              E0D231E002090811D629F550CAD4285F780C3DFD03606494378AB00DB9B0B3EE
              AA347B2FC4EB4569964529F1A52CCB4EC8E5F20E02629F11E08323876A9AD9DD
              65D49D4506449C21559D225110FDC8E00C3912C7C505A7232B613B3AFB784C3A
              0522228066A5D481200A181C718BF54DDDD45414DE2849B73E99A894D3BC9976
              F1BADB6E84BE633044BF731FC0E8E868D60B7B1EFF2238C9CFF7B2255F52D393
              132C2DC3F319A7B0EB531BDCC46C1E2222CD94F348E1BEDB97C2EF0378736586
              F507D10E84B11FE92170B0F1CFF7841B16A490488CDD9705A71B4EEEABBBBAFE
              374146999400E25D10DFF578450664B2DFA2F16ADFB782DC34617E1A0039E71D
              F7F08B59BDF1DA6E79307D32D40BC0B1F3B90979719156ABAB9FB10E582C96FC
              552F2D3D0CDD2D6DF02805633F8D7EBB1B9F3B1C58B1ECD7A0D30AD1D63BEC13
              71FB6649DC0A9DCACF3B90CFFD04C49BC64B5323C77E9A163C1229AB89A6B85E
              8A3C00B0E1E872BD58191B97B86D46009EE7237337C5DD749E1E62F7DA75F89A
              73619373109A543976BDB91F7FBE1607DBB09D84DD37CBC58FEBC4E205FA0185
              DCE3948C20DE1D9562280FA5C59930B9A7452E8993087873B8DDFDBB7AD36329
              793302B4B6B6AE3B52BDF460C50916CDB11E7C594283D1D052E1C98C5E854B6D
              8BF1F7CE0169E6B16181D8F9F3284BB8A245E74F5DF1271B34C220F884BC82E2
              9DA3D4BFF39B56C3CC6FAD4E347D7FCD8C0037DAAE5738172F3B742E8547CB73
              A4CEB3E2940791103C1F7EF68DA8BF609500D665C7F53F35979A0C977D18E5BB
              87BF5F50B8176272CE9249475079C98C1E18191CCCE97AB6F4A89985E78FA6AB
              112A13433324295C43A2E81E94BBC3F144E7AA95BF0FD9F6E9F510B26660CBF2
              246B462C4F1C5EABFF5782F79EF3C0AAD8D31911933E77FAD6CD9786C3C34F5F
              7A75DB6E8522C015FC42D9F1D894945AB236188827144AA5F23A4DD383DE9BED
              76FBC2EB9DBD55E32E411513A69913C8585D61F451FD94D018BD626452D03AA4
              DD2A04DFAE5532862872429023509BB051ADD6FCE5FE52DC6E2E8F4A88AFB798
              3BF2489F8E4E7AEC301ED2C87D4C5FEFC5AE70EAFDA8A9598EB1050440E79044
              C947A630CA352131710F2DC5536DA8AFEF678C4CE6546BB5671E0DA0B92B5C3C
              10754FC8C97016764707238CC8C882C6861A9778433EF14800FF6EFB47D7A5B6
              2854264198FCF6BD5373D045FFE12B3F86237F5E042A32F6C9C4078DF19D00C6
              C7C716DB87CD877462A381755D9289DC1028464916AF6C7E84FEF12D5940CCCB
              D31D3FEB005EBF08821066B1F4164F8C0D3C4DCA9D4E10A9E12075E819BD3EF2
              38C33036127EFEBF06301BEDFF00FF04BEB1EEEEA2FB0BD10000000049454E44
              AE426082}
          end>
      end
      item
        Name = 'CodeImages\Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000D94944415478DA63FCFFFF3F03258071981B0094635D767EF6321D7E
              E333FACAC69D24190014675E7A6EF6F2B83DE9A10A1292DFD7D96C6A36503669
              27CA0064CDFF7980027C0C0C8AFF818658020D51453504C30090B3179F9BB122
              614F56104C33C367A0C2AB0C0C8BC367AE89B64B0BC56BC0D273B356C7EE4E0F
              816BFE02547485816141E8B475B1F619118C8C8CBFF11A70F5FEC5A2A0A37E4D
              B7FE3FE2066966006A5E123A6B55947D4A0CBA669C6170F501D0906340438E3E
              E2C6A7196F2C5CBC73B6FCCAF3B3265136A951B834134C07C480616000005582
              8DE1774CC26C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000001254944415478DA63FCFFFF3F032D01E3A805A316608087CFEF25EE78
              B1343955BFDA8F8989E91D552D78F2F25174EED9F89E8D970F48743B969F2934
              69F3045AF2862A16C00CDF70F18004032750230F0343B721D01243FC96106501
              BAE10C5C400CB28405688902D01273DC9610B40014E67917123A365D382C866C
              380313103F06524F1818A645351D4DB7AAB521D902B0CBCF015D7E01D5E530C3
              191E3030786BD8BC9A1AB6B0425E4A693E491650C370BC16CC3CDF7C246B579D
              F53F7624C39981F81128DC1818FCB46D5F4D0A5A80D770BC1600C505269DAED9
              5174B9CDFC1F1BAAE1DEEA845D4E541C802D3907B4E436D09207A41B4ED002B8
              25A780962C6933F75426CD70A22C00817FFFFE09CD3ED9BAD9433E7A0E298613
              6D012560D48211600100924EDAD1736E18110000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F40000017F4944415478DA63FCFFFF3FC34002C651078C3A60D40143DE01FFFE
              FD139E7BBB7E8F2B6FFC34052995D9747500D072C169D76AF696EDEB360CD372
              7E55A331A1494546632A5D1C806CF9F7AF7F1818D81918E24DDC5FD52891E608
              B21C00B77C3FD0F22F10CB61385E07E80819E21D41B203F0590EC61C40474800
              1DA10074842C614790E400622C67780DC42F808E30023AC298B02388760028B5
              4FBF5EBDA7746F8F012CCE7159CEF08A8181F3070BC3C4C28A0BA9FACD86143B
              00EEF303409F7FC6623908BF05E2E710CB39BE33337465955CCC366D73616262
              7A43910388B6FC15C401A4584E940366DFAC3D9FBFABC3006B9C235B0E0C7ACE
              EF2C0CDD592517324D5A4196BF25266A093AE0E1F3BBC92D37F25BE79CDF2A8E
              CF728E6FA4F99C6807C01D7117E8881B488EA082E5443B00EE88C740473C003A
              824A9693E400B823EE011DB107E8082A584EB203E08E389BDFBA64F50E714A2D
              27CB0120F0E0E99DD45D6FE6E5A4E8B638119BDAA9EA006A8251078C3A60D401
              03EE000004D24BD0B9C904090000000049454E44AE426082}
          end>
      end
      item
        Name = 'CodeImages\Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000D94944415478DA63FCFFFF3F03258071981B0094633DB16EDD327175
              F5334A3A3A9D24190014673EB676EDF2C7CD2DA142D272DF553ADB9A957475DA
              893200AEB9A92594878995818F859D814952FCBB645B43B3B2AE6E3B5E0340CE
              3EB67AF58AC72D6D4130CD7F816ADEFDFDCEA0505FBBC638303014AF01C7D7AE
              5DFDB0B139045DB35C4DD53A93E0E0084646C6DF780D7870FD7AD19DF2AA26B6
              67AFB9FF01E5DEFCF906B27915D0E61874CD38C3E0E1F51B454F2AEB9A9EDFBF
              CD8D4F33DE58B877E54AF9FBDBB74D8C0202A2706926980E8801C3C00000A91E
              94E1A08BAC840000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000014B4944415478DA63FCFFFF3F032D01E3A805A3166080E78F1E253E3B
              7C38D93032D28F8989E91D552D78F9F469F48996969E1FC78E4BA8E5E69ED14F
              4AF4045AF2862A16C00CFF7AE4A80417132B03172B1B836466DA19EDC478BC96
              106501DCF0C340C399591938995918388196B00269EED4B8333A4909382D2168
              0128CC4FB4B6767C3F7A5C8C1368381713D07020CDC2C8CCF0E9CF0F862F0C7F
              18544A8A8E1A4447DB906C01C8E5C7812EFF06743936C33FFFFDC9C06765F14A
              AFAAAA42524E6E3E491650C370BC169C5DBAF4C8EDAE1E6B7606266078B33070
              410DFF0C34FC13D070411BAB57DA1515780DC76B01505CE0E282853B5E4D9B65
              CEF19F09C570625C4E541C802CB9BC70F18EEF3317987FF8F9151834A4194ED0
              02982597162EDC7167D214731E3313920C27CA0210F8F7EF9FD0E5E5CB378BD9
              DACE21C570A22DA0048C5A30022C000042DEEFD1DEBFDC6F0000000049454E44
              AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000001934944415478DA63FCFFFF3FC34002C651078C3A60D40143DE01FFFE
              FD13BEB67CF95E211B9BA952F2F2B3E9EA00A0E58217162FDE7B79D224430D37
              F757D2A9294D324A4A53E9E20064CB597FFD666067646650F4F57E25969C4492
              23C872008AE53F8196333103310B184B7AB9BD124C8A6F9226D211243B009FE5
              3036979B1DD01109448504490E80597E0968391B0ECBBFFDFD0DC4BF18448121
              2195924CD011443B0094DA2F2E5AB4E7C2A44906ECBFFEE0B5FCDBBFDF0C7FD9
              5818F48B8B2F6846461A52EC00527C0EB2FC0F2B33834E6EEE45EDB838172626
              A6371439006EF9C489866C587CCE06A4BF832CFF077100299613E5802BCB979F
              3FDDD565C0FEFB2F41CB41C1AE939777412B260664F95B62A296A0039E3F7A94
              FC64E6ACD6479BB78AA35BFE0368F957327D4E521A0039E2C5EC79AD6FB6EE00
              3B825A9613ED009823DECE5DD0FA65FB3E716A594E9203E08E98B3A0F5E1E62D
              E2D4B09C6407C01CF174D6ECD6DBDBB789536A39590E0081670F1FA67E3C762C
              473D3CDC89D8D44E550750138C3A60D401A30E1870070000C48573D070467701
              0000000049454E44AE426082}
          end>
      end
      item
        Name = 'CodeImages\Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001A84944415478DA63FCFFFF3F03258071681870FBFEEDC46F3FBEF1E8
              6BEA4F26C900A01CE3F623DB3BD22EA565FC63FEC7B8CC6859AF839943235106
              FCFBF74FAC7141E3FC76E676F7DF5CBF994162FC6FF97F2CD65C3CC5D7DEB714
              C3804F9F3ED9BD78F1421FC4666161F9C1C5C3F5C2B2C572E1038B07820CAC08
              83393F72FE9E2333676EA47B64162323E37FB801B76FDFCEBA73E78E2754DD47
              5333D3C5A21F4477284D537A77DFE0BEE07F8EFF8C3043D83FB2FF69666DDE58
              1A5F1A023600A831F3C993276630054C4C4C5F8C4D8C176A9ED5DCF3D8E231BF
              5A95DA9BBBBA7785FE72FE6562FCC1F85FF5A4EADB0AE78A4D898189C960039E
              3F7F1E0A74BE2ECC00A017BEE9EAEA765CB97D252BF87970E32DEB5B226A356A
              6F9E4A3CE593BC2AF9F92EFF5DA1FBF9F7A3E465E457C0BD70FEFCF97AA02BEC
              90C2F19DAFAF6FE8CDFB3793831E07B55DB3B926A690ABF0FE39CF73DE7EE3FE
              1519A11989C030F80337E0E3C78FF6EFDEBD5385E9666565FD262323B30CC47E
              F8E46164E0F5C09E8F9B3F72747A772E0D710FC92339253E7CFA30E2C8B9230E
              D1BED11924A7447C60E00D000087EBDCE11089188B0000000049454E44AE4260
              82}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000002C94944415478DA63FCFFFF3F032D01E3A80534B500A897999191F12F
              D52D00EAE1AE9B5EB7FAF9BBE782B3AB677B002DF948350BFEFCF92397DC96BC
              66A9D452E3BF2C7F99026F075E5ED3BCC6998989E935C516BC7EF3DA23BC277C
              CA7EBDFDCA0CAC50C11F0C0C8137022FAF6A5CE5CDC2C2F2986C0BD6EC5833A9
              FE687DF835BD6B6218923F19183C2F7BDED8D8B2D1879595F52E4E0B806C3620
              E647D3FE17E8FD77CA11CA6FBF2A7F657B69F092077BD83130385D70BAB3B571
              6B100707C765AC16DCB97327F5F4E9D3BDC8FADEBE7D7B3B2727C7387A4AF4D9
              ED1FB76BB2BC63F9F7DAE23537564BFE3130D89EB5BDBFB966731C3F1FFF110C
              0B3E7FFE6C79E2C48924643D40D77CB0B5B52D8D9C12796E45CE0A43C15EC1EF
              4C0F99FEBFB57DCB85D5126070492E97FCFC6CEB333E140B2E5EBC58F6EBD72F
              0C977171717DD0D6D6EE0FEB0FBBB0BA70B53E484C70BAE07796CB409F38007D
              C288A4F80B506E8BE0775371D3C73BA7ED5447B160CE9C3957D5D4D4B4D02D00
              06DBF9A4A424A3BB0FEEC685EE0E6D3F9F7A5E0A24CEBD88FB17D721AEDFAFDD
              209630BF64FEC7B79BEFE77B95F79C5D5A5D6B4B934B43502C78FFFEBDD3EFDF
              BF317CC0CECEFE899F9FFF2088FDF4F9D3E080750113CE649F9101F139D771FE
              E6DACCF5FB87CA0F16B6D36C7FDFEBBCE764FEC8FCEF7CD6F93C5D4DDDA91871
              F0ECD9B3B073E7CE05630B5A0B0B8B05222222DB5FBD7EE5EDBBCC77FAA9BC53
              B22097B3AE62FDCBB598EBF747938F1CDC1FB97FE5C8E41C6C2F6CF707E6EEEF
              18169C3D7BB606684136360B6C6C6C5A353535A780D81F3F7DB4F598EEB1E444
              E90939D6C3AC7FFFCDFCC728202AF0A34CA76C47694A6938D0F03FB8F20128CA
              9818B0837F408D70C55FBE7E31F399E2B3FC1AF7357196D32CFF26864D5C18EA
              1D9A8BAE89A2D2F4DBB76F460AC10A87B6B66F6D303530EDC1A686E2FA00A89F
              1DE8B39FB8E407778533322C00004FE368E099AD39440000000049454E44AE42
              6082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000003ED4944415478DA63FCFFFF3FC34002C651078C3A605839E0EFDFBFD2
              DFBF7F97E1E1E1394977077CFCF8D1D625DF65D997FF5FD877B7EFCE92919259
              4337079CBB74AE30BA3FBAF286D30D51061606068DBD1AAFD794AEA9D356D79E
              41530700F5B2F5CDEB5B3AE9D2249747D68F04181861120C0C2A7B54DE2E4A5B
              D4636962D941130700E35B2ABC347CFB2E815DEA9F353FB363BA8E814161BFC2
              FB393173A6385B3BD751D501CF9E3F0BF42AF59A7AC9E292C47FF1FF8C381582
              1C7144E17DAF77EFA220F7A002AA3960D9A665D3E37AE2D2FEC6FC65621024AC
              5EF6B8ECC75AB3DA0D29E129898C8C8C2816623800C8E79E3871E2360101813F
              E806B1B3B3BF8A04825D8777B5BADF73AF625FC0FEE767C84F160631C28E903C
              2FF939533AF3404D564D10D0117FF039807FDAB469372D2C2CC4D10D3977EEDC
              D1D4D4549B1D8776B47B72795630F0011D95CEFEE74FE01FA6BF92C0D0200084
              2F0B7F4B154C3DD256D0E60774C44F5C0E603B78F06033908961A08888C8531D
              1D9D09DB0F6EEFF0E2F62A6730010A3E003A2209E8085FA023640838E23B0303
              EB3CD6BFE7A69FCBD3D1D49986E20020CDF3FAF56B077CFA393838DEF3F1F11D
              DD76605BA7378F7719D80120F00CE88868F63FFF3CFE31FE56FACD8C55F37BA0
              9A15EC7FB8FE71FD7EBDF7B5123333F30B14077CFBF6CD68E1C28567E5E4E470
              3AE0DEBD7BA7727373CD3F7CFC606F5D6BBDEA5AFB3531066EA8E45B0606CE08
              CEDFFF6CFE31FED402A60B24C0FC84F91FD37AA6FFBF357F33DBFEB1BD7F68EE
              21258C34002CC30D5A5B5BB7080909FDC2E5807FFFFE3D2D2929B105B13F7CF8
              606F5769B7E272FB65090601A8822F40478472FE6630039AA7F39D1524C47915
              C83F00E49B02F9C0B0C9E3CD3B38B17AA2038603C8015FBE7E31B32BB75B7FBE
              F9BC143C3B02E3993398F337933ED37F86CF40475FFFC7F8DD126839D039AC8F
              59FFEE49D9D3686761D78CD3017FFEFC51D8B2654B032B2BEB6F5C160313E313
              7373F34610FBEBD7AF26B6C5B61BC18E108519028C6F5FF63F4CFF99FE832D07
              02DE17BC3FFD78FDAE2CEE5CEC0CCC011F713A0054ABEDDDBBF790BCBC3C4E9F
              5FBC78F1605252123C187FFEFCA9659D6BBDEB6CFD5969066988186F36EFCFCF
              CCC0225A988141F499E8D758C5D8933DE53DDE40CB7F209B85E100A0611ACB96
              2DEB03A6F89FB81C202828F8CCC3C3231B59ECF7EFDF8AAE45AE7B0E961E5462
              908344C377EDEFAC724FE53E543A556E4E8F4A4F005AFE0FDD2CAA364880D127
              E790EF70F068C15105F67CF63FD2C2D29F7A637B1706B80514E1D243F52619A8
              5564936173ECE49D93B257675ECDD054D39C854F3D4DDA84C0E8507AF8F8A19D
              8A92CA02426A8757A374D401A30E20070000B882EAD0786AD343000000004945
              4E44AE426082}
          end>
      end
      item
        Name = 'CodeImages\Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000019A4944415478DA63FCFFFF3F03258071681870EFDAB574A03A46656D
              ED1924190094633EB2766DFF8FBCBC847F4C4CFF25962DEBD1B7B36B26CA80BF
              7FFF4A6E686A9A23DBDAEAC1FDF72F1348EC85B0F037FE65CB2698B8B9556318
              F0E1C307A757AF5E6983D86C6C6C5F58FEFDFBBDDAD97992D38307822C4806BF
              1614FCCE366FDE0CAB80802214036EDCB85178FFFE7D27109B8585E58DB6BAFA
              0E2679F915AB5454DEDADDB923CC8A64C85B4ECEDF1FBABB57FB656747830DB8
              79F366DEF3E7CF0D610A802E78AFA5A1B1E5B196D646D5972F79A6010DB1B97F
              5F10E495EF8C8CFF772B2BBF336E6A5AE51619990536E0E9D3A79140E76B2219
              F0595B5BBBFBF6850B79DF03031BB480DE980A3444EDF973DE3392929FBF030D
              2B7EFEDC475854743BDC0B67CE9C6905BAC20266083333F34B2F2FAFA83B972F
              677F0A0A6A34007AA35541E1FDCFD7AFB9ADA74C59EC111F9FC6C8C8F80F6EC0
              FBF7EF5D8001A90033809D9DFDB39494D44A10FBE9FDFBD18F0303BB77BC7FCF
              E93871E27C7B68009294124189E9C9EDDB6A76FEFEC524A7447C60E00D0000BA
              22DDE1C450B5930000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000002B74944415478DA63FCFFFF3F032D01E3A80534B300A88F1148313132
              32FEA5BA05403DFC0B4B4AD6FDFDF3873969C2042FA025DFA866C1AF9F3F3566
              C7C6AE305EBD5AFF2F23E3FF4BF1F1A733E6CD73035AF291620B9E3D7A14BE21
              32B2D3E6D8317966A8D857203E1118783967CD1A672626A6D7645BB07DFEFCB9
              AFDBDAFC8DEFDC114697FB01C447819664AF5EEDC9CCCCFC14A70540363B10F3
              A1286064FC0DC41F8AA5A43E19FFFAC5ACFFF62D17D6A003E2439E9E37B2366E
              F4616565BD8BD582AB57AF165CBA74A90959E3E7CF9F2FA5A5A5D92CF6F7BFF2
              E9E851455E16967FC62F5FF260B3E40F101F7072BA93B67973182717D7790C0B
              3E7CF8E074FAF4E948644DBCBCBC2F2D2C2C6A16F9FA5E8DDBB2456BAEA0E0F7
              7F4C4CFFAD70F804949C96CBC97DE87BF85010C58273E7CE55FFFDFB970D5D03
              1F1FDF2B7575F5A9F3BCBCAE276DDFAE01125B222CFCED0D907645B3E4133055
              AD1510F821E8E070A77ADD3A3D140BA64D9B765F47474701DD82070F1E1C8B8B
              8BB3BE7DE54AD6D598989A808B172541E22B79797F3EE3E0F8E3F6FA353788FF
              9C93F3F716209FF3FD7B4EABD9B317F8A6A424A258F0F6ED5B37A00FD8D12DE0
              E4E47C070CA6A320F6E37BF7624E040474875EBE2C01E26FE1E6FE759D8BEBB7
              1C30F24F02E38607E8A36FECEC7F8A6EDF8E9692955D85110740D7C65FB972C5
              0B5BD8DADADA4EE3E7E73FF8ECE1C388438181BD11E7CF4B81C4D7030DDCC7C6
              F657E4F367F61FFCFC3FB44A4A76C554578780521F8605C78E1D6B03A6A4446C
              16B8BABA962B28282C02FBF6D52BAFADDEDE3363CF9C91390C0CAAB540C3B9C4
              C5BF983734ACF74F4F4F001AFE0F6B32851560D82C402FD43EBC7BE7BCC1D777
              EEDFDBB7456E0183CA6FFAF499D61E1E9518FA2829AE3F7FFC6853A9AABAA3E0
              E0C122154DCD59581D46697D00CAFD40DFFDC4253F782B9C9163010051B867E0
              2A27429C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000003C84944415478DA63FCFFFF3FC34002C651078C3A605839E0F7EFDFCA
              7FFEFCE1E1E4E4BC487707BC7EF1C2779287C74C863F7F988A0E1E8C111416DE
              43170700F5339EDCBDBB656F6E6E9AF7AD5B2220D3366869BD4ADFB123574A56
              76154D1D00D4CBB1B8A161ED8FD9B36DAD9E3FE765848AFF01E2CD1A1AAF6337
              6F2E9557515948130700E35AB63B2060A7E69E3DAAAA3F7FB2A0CBFF05E24D4A
              4AEF82D7AC69D234349C485507DCBF7D3B716E70709BC7E5CB1282F8420888B7
              001DE1BE70619FA18D4D2BD51CB0BCAD6DC3E186069FB8DFBF997909A80599BE
              5B41E1BDC9C489F36CFDFC4A083AE0EFDFBF5253A74E5DC9C7C7F70B5DB1A0A0
              E07D7F7FFF946D73E6CCFF9F9A9AB09995F56F0CD011F84201068E011D215E5E
              BEC5373D3D9E9191F13F4E0700F3B2D2E2C58B2FE9EBEB73A31B72F5EAD59D71
              71711E5B67CF5EA0979616FF052836999DFD4F04300D0813E1881392929F790B
              0B7785969484031DF117AB03807CFE03070ED400156018202D2D7D5B555575D6
              96193316E96766C6CA02C56E01712F3024C281F95F1C9825F139E03310CF06AA
              6D7FF830584C5272238A03FEFDFB27F4E6CD1B2B7C06707373BF02E253C80E00
              814740DC0E0C097FA02364FFFE65C2A6F71530D857B0B0FCFB2320F063CECB97
              A2400F7E4771C0B367CF82F7ECD9B346581877603E7DFA745F5A5A9AF3CB67CF
              02173B3BCFCCBF714394152AF712881B808EF0FAF78F5109982E90F5DD018A6F
              01D202C0A8E27477BFD9B4638706461A78F5EA95EFAC59B3A6707171FDC5E500
              0E0E8E5B5959591E600B9F3F0F98EBE232B3E8DA35310EA8FC0720AEE6E4FCED
              0A7404AC6C38CDCDFDEB28902FF6FD3B2BC860FD9E9E3591C5C5A1180E2007BC
              7FFBD66586BDFD9282AB57C539A1629F80B802E8080720FD9089E9FF3D202DF1
              F52B1BD88140C7E49E3D9BA5A4AE3E17A703BE7DFB66B877EFDE1C2626A67FB8
              2C969191B905CC25DD20F6C7F7EF1D27DBDB2F2B04164AB06CF30D888B81898D
              0118E712409F83C4BE0A0B7F938D8C3C93336992072CFEB13AE0C183077177EE
              DC5908CCF3387D7EE3C68DADD1D1D13E30FED7AF5F4D7B2C2D37E521958CA0A8
              60045A0EB2FDABB4F427DDECEC7D311515A140CBFF209B85E1804F9F3ED96EDA
              B4A994999919675A909494BCEDE0E050862CF61D1872DD76769B73CE9E951602
              F20B800EE0033AE00BB000726F6B5BE11E199985CD2CAA36487EFDFCA9D16667
              B72BE5D429D976A003D8803E4F5AB6AC43D7D4B407971EAA37C97EFFFAA552A5
              AB7BFCDDDDBB42AD0F1F8649484BAFC5A79E266D4250747C78FF5E499280E534
              73002960D401A30E18700700004DDCE7D0E9DABB850000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'CodeImages\Item6'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002474944415478DA63FCFFFF3F033200F2595E3DBF1B2D2EA5B2908108
              C0886C0048F3ED331DAB65A47ADD9EBE99B448553F2A9368038034F3EDB31D6B
              94341A7D58787EB2FC78C3FFE3F193498B550DE2D2D05CC8C4C8C8F80FC50088
              CD2DEB94B59BBD98B97E33C324DF5CD17EC9AB7AC6999D9DE32A88FFF5DB37E3
              F87DFBE64DB1B0A8951011D90437E0E6C9968D52A24DDE0CAC40CD200701F1E7
              37DA2F19C5D7D44ACA68CC0629FCFEFDBB41F0A953CBB7DBDB6B189F39F374A3
              9C5C9EB498D83AB001B72EAC99C4F13A23F9FFFFB75C20CD7FFF0A7FBFF826F6
              D2FD37B25F949595EFFCE1E0609EC5C969B7CBCE4E0DEA0F86E46DDB4ECCF1F6
              B68487C1ADF36B27FEBB9E99CCC8C6FFE39B64E316062ECD27274E9CB0F5F2F6
              5E9872EA5CDD4563077946A052C67FFF18DCAF9F3C3FD7D3D59F8585E5314A2C
              DCBEB0A18F4758E3BAA4ACC6EC4F9F3ED9EFDEBD3B2E383838F9CABD7B59F9A7
              1E357FE29612D2FF7AF3C28C104F90E64718D1880C900D00F1CFDFBA5D38E3F4
              A5E8A9E1FE81209BB1A6037C0680C0BBD72F7D9F9EDDEDA7E31E9D068CCAFF24
              19F0FECD2BAF77F35266C830BF92BAA992B442D72F350E941E701AF0F9F3679B
              828282397676765799DEDE95B07CBBC340459C890B1CA5BF19FEDC332C9BAEED
              149487D30050CA7CFFFEBDC7AB87B7CD38B654662AB0BE14054BFCFBCF7085DD
              E8B246DE7C7F1656D6FB380D80810F6F5F7BBE9D183243F9F34539500ABB22E1
              7E59A368295833DE30400620FFBFEDF39FFE835BF2A346D94AB866A20D00818F
              EFDFBA70F3F0DD45D60C02007A133EF06A2648980000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F80000038C4944415478DAAD947D4813611CC79F73B7EDE676DB5D9BEEA5A681
              4A8C327B315B4568F646965804698AB388B27F2AC8227AFB232282FEB1065254
              1041F587D6F28FA5399B66F9421A6488DACBD6AE5DA71B9845CEC439EFDA9DDB
              3A9742E5BE7070CFF31C9FCFEFF93D7707310C03664B700DA1691A1308041EF0
              9F816613B0F04FEDE54D52D0335F99F5225728143A632608CE899DED476D4969
              778C227C5434D8B58E50AD6ADA2C148A1C73164C83278E8AB8C90900FE57324D
              C0B5A5EDB05DBFE87EA62821040FC70F80A7CBE856AD7E9103C3425734C84192
              FBF489899D62B1B86F4641B8E7498BEE650A5563C23F4AA101187A9DEE912C6E
              DD2D95CADBF94B6F1D8E23C504710E1F1EFE692F28D8CE977002B62D8EB6A336
              8DE68E11968F8A382533753121B88F48F72006FB7E059EF0940FEF73B9CAF738
              9D177A376D5283C949B0DE6271D9F2F30B1004E9890886BCEE42E8C3DA9BB48C
              924FED2624E01E0020E04BF748973797CA15CA67D19597B8DD677B7373D591C9
              4000ACB35808FBCE9DDBC422D1BB488B1C6F1F5722DEE36513E304CEF0E06258
              1678D49DEF7DE344641289644C2A958E7045180C23D6850BF5EF366C4898DE4A
              0664D53E269F6DD95288CA641DD30E9995C0AEE365A35F091C82005068927F78
              30734D3C9EF6C16AB51E4A494971180C86A65682305E9B8CCBA156ACC5215E31
              80A181A1CD465AF3B696627279CB8CAFA9A3DB7215262A4C3022F5A3C6E72605
              A6B2B1F366B3B9293B3BDB9E919171C9455125A696CE2B9ED4D53A2874604110
              D0BE6F256B77E545E0330AD87CEAB55D522D58D9CCEF395FC08EDD8383856575
              2F2BC7B1A55A96817E7BE3AE2EDC61E2C36715CC9468011B27F9C574B8A1E332
              43FB271F16E59746C3E72C60F37960602F86A2241A1FFF314E20F0C65CC0A6A7
              FEDE4DBAE57A5ECA99BA22995CD11A53414FC3831BCA57E662AD8241FB865132
              B9E2514950F2322682305C8701941DB3AC68C95F0BAAAAAA1A8359A156ABC7D9
              718EBF5391AB1D93A83518C47F8EA169D04DC551CBAE7424411044FFB5C0E7F3
              ADF17ABDCBD8FB8186DBC5A99435538B0B90DF3F2C6E0BA07F641EA539565D8E
              AB129FFCD30EC2F9F17D78A3F7E2FA9A34C9081E21873EB63E3A99D29DA83D88
              2913EAFFF90CA20EF6BAB2EE74898E1942C3867EF1124A73CA1AA97C4E024E52
              7FFF86D27AB258E71F44FBB1954178DD1FF0390938C993BBB74063E576FDF9C6
              03FCB6C44CC0261008E8611826675BFF054EC3F2E04D2C6CF50000000049454E
              44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000004884944415478DAC5967F4C1B551CC0DFDD75FD7D702D2D832188B4C7
              E00F464DDA081DB882FB11B2B868A7D3E8F48FB138C5E886C63F8CDB2C994B74
              D98CA6864C345942BA385996B82DCE40602C640E96B51BC6D15816DB126105BA
              B41D14D7B570E73BC295FEB802628BDFB477C9CBBDEFE7F3BEEFBD7B87D0340D
              FECF405622009FC91EE97BB96F56F65AAF7A93F1D09A0A30F0BFFA5EF8757D61
              AF3AEC938627F8E633EA4DBB0FAE89402C5C50322D0014005383055313E8490B
              A979F5DD8C0A24C1D948B304A7404AF82A24602E14DEF80882845624C0C2F30A
              7B487EC90C3F656628316353CEF8E43F9B0A55BA9329E058CBA54B17AF4D4C94
              5FDDBFBF0A4551EF9202D19117C1913FC531F2F987166E0F61157E2FF60B74F6
              3AA150F41B17FCD8E5CB3F7D83E375BE9212514D67A7BBA7B171338661E39C02
              2C3C47D1ABC6F2219C5E04D20970F46F0042234501BECEBE4D24925897827BEB
              EA244C1B36324271494405DCD7F60CCAA557CAE7B260D9E97860EC2CF122B0FA
              FE62BFF0197B3D1CF9E04AE06C70494405FEBC7BE50BF983B7DE0EAD1BCBA263
              E0D10AC00BB320F8747E30A8BAFE01C61387C462F1682C00C7F1DB70CECFB6CA
              E5066F6DAD047004E272D186AE2EE7D50307D4496BC079F797CF8989C6A68741
              0F4E51B1A30240B80E0029FCBF78440AEEDCC39233A328D0984C93F68A0A8977
              8B41B258411A42D8854B81C28E73810B5AED67DAB2B2534902AC44F6FDC6A6C9
              B14509B108809C3C79C8CE6FB708241B3C2E974BDFD6D6F65C7373F385DCDC5C
              3BCC811CB75A5FB95E505C14D26D8EAE1F842D1DFC213099ACEB62A0A35A1785
              730A244A08E15E90E5150504D58B0B6E7878F8238D4673C2ED76EF8102E7C3E1
              70E9C6D6D337C3062381F104D1A943164C680897DEEAF2FDB0456FAA24D566CE
              5D9018CC9A203CFB9A504C1411E9E3175CA200D3F6C0E7DF61B0FC68A1CAEB15
              F312302F320FA7C1DC706FA0BD5E1F37F26505981875DD39A4C82FEB4DDCE75C
              02AC447DFB398B284FAFE061CC66A2C1B4E746E0CCF61A4EF8B202A9229540AC
              84607D9522E4B52D09CF8800135E9FAFA1A2EDBBF39DBB8D1F5792A479A95C19
              11608279B33207D1BD9B3D1F96566D6D818751644D05288A92DF363D7F431974
              3C39A9DE35A07DE7D4762E898C08C09C84D5B4AB9F445C242197621E6FE8D1A8
              B2C6A67DCFBC154A3CCEA840229C6D4F25B16A81CACACA1303030366A552798B
              6D9F8D84C5DED6378EAA84BE3C22074713FB79BC8FA0446D9CC4AA049C4EE7FB
              2A95EAEBD8369C0740C70E14543DAD068452C6D98F61D91D93D34F1CB71AB365
              39DDAB16807DF0A1A1A183914864FE8B69361C12D2679BF6956605E5841C4792
              3E2098E380A680639CF2232F7DD9BAB17ADBE1FF3405093252DBA73BADEAC77F
              A80829C68BF97C01EC41C4301C3E7E00DB7BFA2B52676889ED9F0E01DC76A4C1
              AA0E0E9610029A170B66CBEE086507B037BF4F82A745600142588FEEEC27BD7D
              24C19BC3580926B3036CF023AF7F1B57F6B40BC449DCEF26093482CDC3F9C57E
              646F5B4A785A05E224C6BAC97151C1D472F0B40B30C1BC826D871BFA09E32716
              52FBECB1E59E4FBBC0BF8D7F005888F7DFAF5695E70000000049454E44AE4260
              82}
          end>
      end
      item
        Name = 'CodeImages\Item7'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001D74944415478DA63FCFFFF3F032580119B014031E68F1F3FB97CFBF6
              434252526CE5E3C7CF13444404CE7171719D2268C0BF7FFFC4962D7BB0B9BE5E
              C1A4B4F4D195B43439E79C9CCFB7AE5C61E3686E7EBCC6DE5E2D0EAF01D7AF3F
              A8D3D252685CBDFAF67E0707E129C2C2829B812E48993BF75FFEFDFBBC620B16
              F0AA333131BDC669C0D9B377FB4C4C940B6FDC7858ABAE2EDF0213DFB4E9D686
              EA6A35FF33677EEAB3B3B35F226800C8251A1A0ACD30F1CD9B6FAEABAA520FC4
              6B0090CDB877EFED15DEDE6A61376E3C29545494990093DBB1E3D6CAB838B5B0
              F3E75F244B4B4BCCC330E0C58BD7D1CB967DAF3E7448582E26E6F9B9E060655F
              4646C68F3085EFDE7DF09E37EF73CFD5ABBC62A1A16FAE7979A9D8623560DF3E
              11F9D8D8E717C2C294BC81067C8019F0E1C327D7B9733F4DBE7489572C3CFCF5
              550C03A05E60DAB307EC85D09B3751BDB07BF7CDA5D1D1EA51E7CE3D4F939191
              9C4D9B40C46700D1D178F3E6C36A4D4DF996356B6EEFB3B3139E0A4A488F1E3D
              4B5DB0E05FEEB56B7C92CB96F1683233333FC797948556ACB8BFB5BE5ED1ACB8
              F8F1D5B43459A7DCDCCFB7AE5F6765AFAB7BBCC1C1413D1A6F528605E6A74F9F
              9D80994952424274D593272FE28199E92C2727E75974B58C946667000FF22DF0
              CA1C04F30000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000003454944415478DA63FCFFFF3F032D01E3805B0094677FF9F275D8DBB7
              9F755958987EAAAB2BD63E7BF622F9EDDB2FBA02029CB7A4A525D7303131BD22
              CB82D7AFDF841D3DFAA369F26471A58B175959172EBC77CADB5BC9FCD8B15B1B
              3D3CD4FCE4E519188A8B9F3F747161689391919C45920520976FD8F0F4725090
              8CAA8000034353D3AB6766669FEE9A9BABD85DB8706BFED9B37C4E353512722F
              5E3030AC59F3E4565090B4362323E31FA22DF8F1E3874141C1EF433367F2F26E
              DDFAE0A2BBBBAC073333F30B2407F0EFDD7B7FB7ABAB926961E1C7F71D1D9CD6
              6C6C6CD789B6E0EBD7AF96A9A9CCFB962FE7E0387EFCD63A0B0BB560743567CE
              DC5A6E6AAA16E1EAFAFFFFDAB55F5C787979F79165C1891337D7989BAB87A2AB
              397BF6E6521313F52890056BD67C76E5E3E3DB3B782C78F5EA75545090F0E2A3
              4799984E9DBAB502181491E86ACE9FBF350F1874899A9A0C0CDBB6BD48919292
              984BD082F7EFDFFBBE7EFDD1F8CA159688901019750F8F1F3F66CFFE902B2D2D
              3107D3116F22B2B3B9E6AE5DCBC5B57EFDE31BDADAFF968889099E40F6098605
              5BB6BC7A9A9B2B26F5F7EFFFFF0D0D2F1FEBEB7F3A6064A49A004C8258BD0AF4
              C59C8B17795D5A5AC46519189898A64C7972CBC343461DA7059B37BF7A9A9525
              26C5C4F4FF7F4DCDAB2786861F8F181BABC6002DF887CD820B176E4F3F7F9EC7
              B3AD4D5CF6CF1F26A669D39EDCF2F4C463C1DBB7EF025FBFFE607AF9325B7078
              B88C9A8BCBCF9F73E7BE2D9095959A816EF8870F1F3C4B4A9857CE9BC7CB0BCC
              6C377574FEAC1017173CC1CFCFBF836024BF7CF92A26284864E1B163B823F9D2
              A5DB536D6C54B314154191FC3C0D582ECD263A150DFD7C406A5141B2057FFEFC
              916F69F970AAB151446CFBF60717DCDCE4DC80E5FE6B983C501FDFBE7DF777BB
              B828998585FDFC396FDE1F276E6EEE63445B001467DCB2E5F1553F3F394D2121
              5071FDF2A9B1F1E70716162A36C0B4BFF0DC395EFB860609B9274F1819972D7B
              762F2242521F9894BF106D01088092EC8103DFBBFAFB25956EDF66629A37EFDE
              6960856306AA70BCBCD4FCD4D5FFFD2B28787EDFC686B117988CA7633383982A
              931758B3F982AA4C6666C65F6A6A8AF5A02AF3DDBBAF5A4242DCD72425C55762
              7339D116500A686E010028DE08EF069A37220000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000004894944415478DAED976B4C936714C79FD20B145E7A2FB5405B50AC23
              D8D2541851BA459620449305B258322ECBF8C230311AF9409618F832B6004BA8
              2E8B386F64105756151DD4A182014614126940E01D9776584B79A12D965BC116
              7AD9F3C63997C50252925DE2F9F4E43CE7BCF9BDCFE59CFF43F0F97CE09F34C2
              5B803701F0783CFCA9A969A5DDBE92E4761322A954F05B42C2EE22E817A0A8E9
              A7B535CF0A99EC1D1789781A1A8DD64620103CDB0630363651896148416D2D9B
              373848246218001D1D4F87F7ED1349565757DF51AB173B8B8A38BC3D7B00C8CD
              5D5ACAC8983349A551E94422713A6000F8D79FDDB811567DF2248DF6D2C7E100
              70F7AE0995CB857B7180E66647776E2E8BB3B6F6625E2C0640AD9EE893CB7726
              070CA0D39986535284091EB8A0717100343458CC14CAE2189B1D725B2412A8BC
              5E2F1B450D2AB73B44AED5D223CBCBE94C3CEFDAB5D999AC2C462A89449A0808
              A0A767DA78E0005F848F3B3B2D668582F3AEBFA5EDEF1FBF9D93233EACD70370
              EA94D35956F6FC2893C9D40604F0E081C5A450F004F8F8D123139A9424DCEB2F
              16C3B0E2E3C7236B6FDE04203FDFE7ABA999CDE372B9EA6D03E8EB7B71F0FCC5
              5A2C964F4F9CE0D569347F02E443801FDE026C1900CE236D6DD6D18C0C5ED41F
              00431040EA2FDE6AB5E6979470EBAF5E2510323301A8AD3595C6C408BFDE1200
              9C238F8C18BE51A9A23EB9742934340A2234378FFF2C978B8FF8CB595E5E4EBD
              70C1DD52524267B25800B4B79B50994CA08055717ED3007AFD932F1C0E62B6CB
              45461A1BC3D967CF2208990C4053937D362D2DF8C3B0B0B09E75A04903039383
              4AA530DE600040A170BB2B2A9ECD20886B8942710E4924E29C0D01743AC32F07
              0FC6BDE770008020009C39B3302797AF2C4647934BB95C8E066C60B037443F7E
              6CFCD16AA5C5565531B99D9D2412EEEFED35EB5352A2C56F0CA052E100CF9722
              2309E53B76F0BEDF080056C688818189269B8DBEB3B2720B00E3E3135F391CA4
              2CA7931C7AE50A23E2F2652A9548C44BEBDCB3F47472368220DDEB01F4F71BFB
              0B0A4489284A2024277B3C9595B33374BA6B914C768E48A5E28F3604F8CB7E06
              A3A8FEBBEA6A81B2A1814AE5F301D06AF5AD72F9EEC3FE725C2E9754AD76B417
              16B2B90C0600F7EF4F8ECA6451EF070505D95E17BF996B48BB77CFFA6B66E6E6
              AEA1DD6ECF2E2B431ACF9DA3500E1D02E0FC79D3E7B1B1C22A7FF1FFFE42F416
              E0EF005B68C78103F4F64E3FD9BF9F1F838FBBBA2C930A0537199E6ACBEB62A1
              20D1424172041724C78EADAE5654383E66B1584D0101E874C681D4D49844970B
              80F8789FAFAECE6A26911646A1246BC31B0D2C3CDCE161438DDB4D91DCB9C310
              9C3ECD60E179F5F5B356A532FC83E0E0603420008BC59677EB16E5DBE2623AE3
              A52F220280D6D657A2B4A5C5D19D97C7E2E090B8EDDA05C0F5EBC641992C2671
              BD6F6F5A96C3EAF8258685175EBCC8E43E7C482299CDF8D6BC92E51ACD625769
              2927420495A352B9309F96366F9648A233A1769CDA1600DCF08709864D1FB5D9
              56D260D3E35328EE11D8DD0AF187C9D090B1D1EB755B4242884F05025E0B2CD7
              1DB0056FF8F1FFD6D3EC7F09F03B2222D8DF765C62200000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'CodeImages\Item8'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000E64944415478DA63FCFFFF3F03258011D9808F1F3FB9BC7CF9DE1CC4
              666262FCA3AC2CDB0B64FEBF7BF771C9BF7FFF5940E2E2E28227F9F9F9F66035
              E0D0A15B731D1CD492545418182222DEBF6D6C1450040AFF6D6E7EFF70C91221
              913B7718180E1CB835CFCE4E2D19A70141416A49C78F3F68909212DDCDC5C575
              0224FEEDDB378B172FDE38585BCBB7AE5A45840157AFBE8E1217175D8EECD737
              6FDE05E9E808AD1D3560E41870ECD8FD262929B1DDDCDC5CC740E25FBF7EB302
              25241B1BF96682068092B29212034364E4FBB74D4D90A4DCD4F4FEE1E2C54222
              F7EE1148CA146726720000B36F1FF0ECA73DF90000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000000E34944415478DA63FCFFFF3F032D01E3805870F5EADDBEAF5FFFC822
              8BC9CA0A6C9594145F0062BF78F12AF6D1A3F77EC8F2DCDC2C8FB5B5958B88B2
              60D3A6FBE7FCFD150D91C54E9CB8B5CEDC5C2D18C43E75EAD62A203B14597EE3
              C6FBE7FDFC148D48B2A0B5F5CD4B03834F0F41620A0AFF0F6A69299781D8376E
              DC6BBB778FC119C4BE7C994FAEA24244822C0B56AF7E742D24444E1B5F18AF5B
              F7F07270B0BCCEA805A3168C5A306AC1A805B82C686E7EF3D2D0F0D3239018B0
              B83E04AC504A40ECEBD7EF75008B6B27101B585CCB565692595C238B51B5C2A1
              7995494D40730B0034AD97E087AF184D0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000001654944415478DA63FCFFFF3FC34002C65107E072C0DFBF7FA5AF5E7D
              B4F9CF1F2616743956D67FBFB5B5E55D999898DE81F8403378AF5C7970E0F76F
              265674B52C2CFFFE686BCBF93233333F25C901BF7FFF565DB5EAE3E198181171
              74B9F5EB5F3FF7F51532061AFA1CC4FFF7EF9FD08E1D6F2E7B7B8B49A1AB5DB2
              E4CDCBB0307E5B5656D6DB643B808B8B81C1C101215759F9E19DA525AF0EB203
              4E9DFA74BBB9594008A6E6D02106862F5FA8E40075750686356BEE9FE5E56559
              0E92636262FC2B23233D8B9191F11B340AD89F3C799AF9EFDF7F6610FFF3E75F
              21D1D1CA16972E51D1012B57DE5AA2AFAF16CB4004B872E5F6ACE868D5D45107
              8C3A60D401A30E1875C0A803461D30EA80E1E5006093EC3C3737F34A901CA849
              262727330DB949F6E8D1935C5893ECEBD73F41D1D14A66547300272703839D1D
              42AEBAFAC33B2B2BD446E9C9939F6E3736221AA5478E801C422507A0CBD1A559
              3EE01D137A8151070000B409C7DF60BB2D430000000049454E44AE426082}
          end>
      end
      item
        Name = 'CodeImages\Item9'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000002744944415478DA8D935F4853511CC7BF77BBDEFDBBB68D58690EDA32
              F049437CA885DA7A101492FE3C053E186952505082CA5E6CB06CB07C309592C0
              1423F4C1B2127BF2C59990228A4213C9114B0D7392E664D76D67F776A6736C34
              B2038773F9DEEFFD9CDFF99EDF65244942BA417579302814D24746AD56CD310C
              134EE763D201C2E1C8E996960F4FCBCBF3C659564E46463C569BED52A342A1F0
              FC17C0E97C3FD4D454794B26936DC4ABD1391C43AF9A9BAF551E0A104551DFDF
              EF765655596F27EBC3C3934F2A2A8A3A5996F5A500666717EF7775CD35180CCA
              BD33122232BBBB44CEF31C393091902093DE7518C8A942417EC6BA2D8A123636
              04AEBEFE9C93999E5E6800B4AE82826369C35CF378E0BE7903F93333F0AB3508
              DE7D80BC9A3B989FF7C364E21AFF09F8FCB21BFE470EE4ADAC24B475A50AEBD5
              B5C0C56AE4E6AA5301E3E36E1C64C2711C76FAFAB0F46610A59B9B6093C05B19
              1C26AD5750FAD89E0A989898A000310E502034D08FB39D1DE8D6EA50FA7B0B19
              49100F7DBF56FF703401F07A3F81364C52230147BF7DC7095B03B282413C8B43
              628EAF1A0DDC4484B5EFEDF30440927ED01B20F1AB94A052A961349A30DBD606
              737B2BCC3B0174661E012F8AF046A30869B4B8FE71D4967284DEDE1EF03C1FCF
              4102F5A1A4A40C0BDD2F606A77E1E4F636EC344456AB47E6D57B28ABB99C9A81
              CFE78320088844A2B49DA3505233CFEB100A45B0343800D2EAC0984289F3F636
              F8A56C582C59A900DA85D44CF63E08870FD64842FBF2BA07BA220B94861C2C2E
              FE427171CE3E606A2AE0329BF57467427388EE55B03F49428B5514D36239C5D6
              E5E500EAEAF21B192A64AFAEFEBCF0F7EF8C4387D1787CEC0FD4D754030BE1F1
              E80000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000003EF4944415478DAB5967B4C5B7514C7BF5DCBBD75CD287B62C318E351
              2171218099C60713B36E2C86253A4C966150B6C50499B86816EB125192A29DC8
              A284D97FD81CD3099D64318C9751684DD9A3760F640DC2E6B2505B985BD75148
              D7D2F6F65E7FF7CE21A59704583CC9496EBFA73D9FDFEF77CEF9DD4A388EC3FF
              6992F9007CBEFB4F1B0CBDDAE6E63F9F0D0623B292920C6B65A5A62E2121DEF2
              C8809B375DA53B769CFADC682CFE58AD4EEE2212E770DCD2ECDAD5FA69636351
              6D76B6FAC8A201E170382D27A7FE427FFFBBF914455D9F19631826392FAFDE66
              B3EDDB2A97CBED8B02F4F45CD6310C2BDBB66DE341B1B8D53AA875383CC93B77
              6E7A675100BDBEEDC7B2B2E79A54AAD56D6271AF7772535555BBAEA1E1F517E7
              04304C2451A9FCCC999ABA2C303BE8F733529A96B252A9246615FCBA567A7EA3
              56DDEBA7AE67ECF5B190F1EA747C6CCC4F7B3C075502A0B0F0E86077F7DE9598
              A7B12C8B33070E8039F91DD679C7713EF729BCDA7C1A52FA31529B88E0E5E567
              3C2653F9930B06042627F1ED6BC5C83A7716AA7058D07CC47FDD908D979B8C88
              53C40B808A8A0E8FD9BC4080E3CA15B4EDD98DFC3F06A1987D9CC47B3232B1F9
              580B68E57254567612C0DB0B0354A7AD47B6DB8D0DC1A0689C577F4A4DC7F347
              9A507568401CE074FE858E8E762895CAE91F3A9D2E68B51FC2A8D98C898B3624
              901AE4CE01E10FAD3B29193FAB767B7BAC9F64C50046475DE8ECEC447CFCB219
              1D3186FDFBDFC70F5B3478E3AC05065A8EA544DF189C128530C40D4B14DC573E
              775E146078781893A4886296939387D6420DCA4871793B4ED3084996E085A9E8
              EEE61BD542D1B08539EE68C8BF2E0A7092B45D5A5A6A4C72BBDD8ED2D23DB07E
              DD00D4D5A270FC9EA0B750146E13C8967F771221DE1647619C0C894B9EC2B478
              AFAD8D02984C26325CF7A392B32C8740C08FA2A257C8DD14C1A5E3C7C07CA1C7
              F6BB6E21DE1E178761890405A1105A497296096329015C5DFD4CF0F4ADF32922
              45766260E0F7E90D47229C00E1872B3131096A7526AE1ABF47E0900EC59EBBC2
              B7BA64327490B8927D30CFDEC7936057BDE9EDBDA88B2DB2D9DC0BA9540A994C
              16735423232E14146C157672ADAB1D13351FA1C47D07FC81E9283914A1294CAC
              4D417EF597A8FF66D463B154C4B6295FE4A1A12172D7B06422F91D4488B382AF
              59A3C28A15AB04003FAD237D66DCA9D622F7F6DF68840454FA1378496F804CA1
              444DCD054F5F5F85F8A0F137ECC324BC3F7C16D39CD673F8E5BDB700B2F2A2FA
              13808C12627ABD9500F68903E64A3697E61AB88CE5E9596039C9B4565B6B1307
              F047B1500079BBC568870F5F7A0020DDA1CCCCAC1BA1696AC6A8CCB6686DE64B
              EABF472EEA7330C8E0C68D0FD6CFEB5FC5A3D83F8C37B3EF82D6222C00000000
              49454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000005A44944415478DAC5977B4CD35714C7BF2D6F8A02C531370504142805
              5FE0607F60D4311401996C261AD40C89248A8B0C512368228B64C6A964099081
              7B9929D16C3CC6E28648544493A11B1D0A182A030B9487D08214B0A5EDAF3BBF
              CA4B680B41B6DDE470EFEF9EF33BF773CF39F7FE0A47A7D3E1FF6C9CD900300C
              E32416B7C4D4D6B6F96BB50C57285C5427102C2931333393FEAB00646B76E7CE
              C3B4E8E89263B6B666EAA8A8C58FACADCDD54545929572B9DAE6F2E5F7B3B66C
              094EE57038AA3907203B4E6E6ED9C5D4D4071F95976F4B5BB5CA2B97161A1AD1
              59363448768786E69F8B8FF7BE7BF2644C34E934730A20164BF6F8F97D9FD7D1
              717093939343B9219BC1C1A14037B7F315D7AEC5A40705F99E99330076F76161
              D90DF1F1CB6F6CDFBE36D194EDAD5B7F9D4C48B87E402C3EF21645413D2700C3
              C3C3CBACAC3E172B1429C17676BC2A53B66AB5C6DDD232A3A9AF2F79ADBDFDBC
              CA390190C9FA42172CF8F286567B82CFE5727BA78B96A3E3A9A1EAEAB8BD1E1E
              8B2FCD082036F61B517E7EDBCA694966D02CA0C07A14E00FBC07395C8CDA2525
              79556466EE58A707D8B6EDEBBAC0401FDFAD5B7D66BD30C3006D0F45F835290E
              EB3BA4A870E0E3DD5359787B7530B45A1D0943365A68343A949636E1E953697D
              41C15EE11840787880EFAE5DCB67B5389BC5BBDF7D8BF2B4547C20EB018FE686
              497EE43B2124E30B083645E801341A867A2D8A8A9EA0BAFAEFB90160C861FEBE
              7DE829FC099BFAFBC19DA0632F829F1DF9589E960EDFA8183D042BC5C54F2012
              35BD3E804226437644383CEBEAB142A53468A36521E6DBC3737F3256EC8CD347
              A0B8B8113535CDAF0FB0DFD11E518383F09C2E4524BFCC9B0FE7DD7BB13A3E91
              52D088BABAA7A601E472399E3FEF33E8D0D9F94DF0783C7CCA7700333484DD54
              81F36600516647566191E8F5FD18F5F512D300B9B95FC1DE7EFE14474AA5127C
              3E1F9191D138EBB904E15229F2381CEC243F8E3388DA3D9E1D6ADCD7A0D36B3B
              01241807C8C9C986A5A5C514072A950A2E2EAED8BC3912E7977A20AD5D8A5A9A
              CF228858F2C59F01C46D736B543A2C1DBAD2FD88671480DD292B869A8D8D2D38
              1C2E32BD3C70BCBD5D3FD740728E2076903F67138BB31F874BE0A0C3CA417359
              29B7980270F5EA552814CF8D3A60CF72404000FCFD5720835290D1FD0C6623BA
              16925304F121F95C6CE05D05C90FA45793BE6749685F61F30DC72900172EE422
              2828C828C010155D7B7B27366E8CC0FDFC4B101D3F86E45E394693D54D729CA2
              13A563E03EE1BD6724F9B4733B2A4735D71CD2E003AD45F7325DA700141616E8
              4F80A1C6DA6A345A0406AE818F8F507FA93C2C29C6EF294938D4D70BEB113BF6
              EC1CE372B191FA6574421A69D725B4F80282B2A53989AD3DFAD69D10175E3BE4
              3DE37B806174FA4B64FC4A7D79ADB2E3FAB2DF509572102972D918443FC95182
              70A5BE916421815852FF8244EAB01083EBD3EB0B0B4D9C82B367CFC0DADA7A64
              E7FABF63635694CA17F0F616223838440FD258791B7792F6E32841F0264024D1
              CEDD46DE1D64E716B9C22AE233B476AB4C03E4E464C1CFCFCF6844140A050606
              540809D93012152D5A447FA222311E8765DD606F903692D3E6E67843A341BF99
              39863D9621EC741E6E56CA20917499061089AAD1D5D5A5CF3B1B7EFA29AEFFAC
              B2639DEEE55820F083237D6C26A6455A5B839B897B708420C4E4E72215A49585
              05B8027F846564434705585ADA82D6D667A60146C3FD6ADEB5635FB489CF93C7
              5D0DF5B8F5491C5CA8981F7000B70D9BB1F6703A28487ABBEBD75BD1D6D63D3D
              C0F862E30B4F7E36A6EB696EC2959D9110C6C4223021F915C8B2B25648A53DA6
              014C55FDD4B1619DEA85121CCAFD645D59591B3A3A64C601D8B9E9423D5B1D3B
              2E2F97A2B37312C0C0808DAF50E83CB6FB97C5C78C15215B0F139FC76D0CF5CC
              8477C6FD8CEA2512053C3C2CC7011E3F6E4EA8AA6A7E07FF610B0A72BF2F10B8
              E7CDEABFE3B96CFF00F16B63FD977CD4380000000049454E44AE426082}
          end>
      end
      item
        Name = 'CodeImages\Item10'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000001814944415478DA63FCFFFF3F032580912E06FCFBF74F849191F12310
              FF26C9805FBF7EA97554446EB217DEAC2E26CCF0FF055FCA7187C8A9EE4083BE
              10340028CE5C9EE177B3DD7E8B32333B03C383DFCAEFFEFF7C28F84930E892BE
              DF4A038206FCFCF953734389C0B570EB1F0C0CAC0C0C0FF5CF152C9A905A5D1B
              7456F48FDD2F251616D6FB780DF8F0E183C3C56EB1FDF6BABFC106FC61010A02
              310B27D070ABEF7AEC1C1C97711AF0F7EF5FA9E9D58107621537ABF2F333800D
              606083D00F7E1B3E93F7392B0F0C873F580D0086B8D8EAC9456B7979397EEA7D
              EE729611FF0F37E0FA77D327CA0187DDD8D8D8AF630D03A066A1D5938A368A49
              4ABE15BD5DEB2722F4FFFF5776994F420127A3F905454E333131BDC6198D409A
              6F657FE1560969E9D722D7AA0358A4F49F29271E75626563BB4554429AD15BBF
              CB80F7392FCFC305E6DCE2321F2452AFB87072729D2394C8E006ECDEB1B5976B
              7B48818C38E33FC6D0E34572AAFA93096946F702882D000CDDEF40FC8318CD78
              D301DD0C0000928ABBE1E359502C0000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
              F8000002784944415478DA63FCFFFF3F032D01E3A80554B7E0D7AF5F6AFBF7EF
              4BB973F980A9101FC74F4B97A8D50A4A6A73A962C1FDFBF763AB53ACE67584BE
              64911303EA636100E38BEF745E69859FB5666565BB43B605DFBF7F378872553E
              BD2AE3390B2B3B50801962F86F46C6FFAC6CFF199FFD92FB20E17D5F958989E9
              0D59162C5CB87081D6E3947853E53F1097032DB82DB364A69476C09C4DAD22A7
              239D7F30DC13DFD6A9A4E35941960589B111D7E7B9AED4606465805B704366F5
              E4BFDC9A370ECD329B9AE9F78DE1DE7FBFAB4A2E1B75C8B2203D29F2DA4CC715
              9AB0700759F01F881F7D6064909780C4C7E3BFB6F765DD0E29916CC1FBF7EF1D
              EBE375774C0A7DCA866C013AFB2657DF2A75CBC270922CF8FCF9B3D5D4DAB025
              015207E434247F30E3B2E02F13F3BF9F56EFADB9B8794F106DC1D7AF5F4D2696
              07AC09B49579D9BBF888F69CF03BDCD82CF8FD9FF9DF1BBD2B1992B21AB3D1CD
              C069013059EAF597F86D0AB49179B9FCC40F9E5C95155AA202100341617FE8AD
              CD236676EE9FDC4A812774AC139B8179E0363673B05AF0F3E74F8DDE429F1D01
              5652AFD65D62624D975A60C0CFC3F8FFD627815F4C5C82DFF85CD734C9281B4E
              2026716058F0FBF76FE5CE2CB703C176722FB6DFE3FF1DCB3DD952900F984204
              02AE2886ADD725C6509C16FCF9F347AE2BD7FB60A099C8BB1D8F44BF47734CB2
              16E3FFCF704B22E790AAFF24474646C67F14593075EAD455815F3ADDF6FE72B8
              EFC1BCD84014E8F24B32F5DB74FDEB2380867F26D5700C0B0E1C38D0FC704154
              49B4F6730E163660700193DE97C827818222129BC8311CC302209BF1FCF9F345
              4FAF1CB4646565FA67E39330898797FF08B9866358400B306A0141000029DC2A
              E0FAF409100000000049454E44AE426082}
          end
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
              F4000003854944415478DA63FCFFFF3FC34002C651078C3A801A0E009AC1F8EB
              D72FCDAF5FBF4A727171BD606767BFCEC8C8F88FE60E00597CFDFAF58CB2E2BC
              6A358ED3D2761A3F198405FE317CFE2BFC9D53A772B3836F6E0AD0219F69E200
              A03EA679F3E62DDC30A72C6656E27B064921A039CC40092606080DC42FBE0BFC
              E076B9EFC9CB277080EA0E387DFA7479558667C7C6DCB70C5C9C084B911D00C2
              DFFFB2FF61B07F6BC9C9C57D866A0E00EAE13131317E323FF412BF9EDC5F140B
              3F324A7FFA62B8B37CEDECC2F224B3DD0A3C5C0C0CF7FFFB5D5570D9A0872D5D
              90E580E7CF9F0726F9E9AEDB9EFF16C3C77715D6F72B1B0614EDDBB7AFF5C136
              9FAA248FEF60F1AFA61FADB979F88E51C501C0E02F5BD7E6DCD91EFC19C30137
              58E24E2BBACD4A888E0ADB5CE6B84DC94CE30F58FCB1FCD9225925A37EAA38E0
              ECD9B325DB3B1DBA6BFCBE603800C47FFC81918111C89611FB0F977F2875A856
              5EDDB6852A0ED8BA756BEF99051145F5FED81D808DFF5AF35E8CA884E2528A1D
              70E1C28582EE7CA7FEF9F1EF19D8D8188872C00F46DE9F6C8E1F64989898DE50
              E40050A133A52AA4B3CDE33EE7B527BF592DD5FF10E580FB52BB5B14B55C6AB1
              9949B403EEDEBD1BDF5BE835A5CDEFF3FF3DCC85FB5E1F6EF0CF74FA46D00137
              B93AD7A959978603B3E01FB21DF0F8F1E3F0D64CA7B91DBE9FFF3F345CDEDF57
              1551353BEA1533BE2878FA4BE52D83D1FA3A2979ED19F8EA05820E00E6F980C6
              14DBA51D3E5FFE3D355FD3D953165A3F33FC050BD872240BAF73E71C12302A9B
              CACCCCF4575048EC022B2BEB5D624216AF03DEBC79E3511D63B6AED3E7DBDF17
              B61B5BBACB429AA6073E610359FEF01BFFCFE73F047E32B3F3FDE0D3CFD9A166
              9B9A0EF4E90FA2E29318077CFAF4C9A624DC68678FE70786974EDBEB3B4BC29B
              A6FADEE76487FAFC81F9C13A056DBB66522D24CA0140CBADCBC28D7776BA7F64
              78EBB9A7B2A320A0738AD77D4E90CFFF3132FE7FEC7C25535A5E75170B0BEB7D
              AA3B00D8A830CD0F32DE37C5E511FB8B80D3F9EDF901DD93DDEE70832C7FF095
              F3CF6FCDFC7DAA3EEDEE945A8CD3016BD6AC99F67673717CB2CE73F6C2E39A9F
              7AAC2F09C283DD0E18EC3A94073B5E079C3871A26A7A6578DD6CF7C76C0CCCFF
              19D9582196FF60E2FCFD27F6990B0F9FC0219A3A00D6D23930B72426CDF82B38
              9B712BDBDC53895D95C92728B28B9A96637500CC1140CC0FE303CBF0F7D4B618
              AF03E809461D30EA00004D9BBBD0120E55810000000049454E44AE426082}
          end>
      end>
    Left = 112
    Top = 264
  end
end
