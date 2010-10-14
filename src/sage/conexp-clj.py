# Daniel Borchmann, 2010
# This code is a mixture from the sage interfaces of
#  - Lisp
#  - Octave
# with some specials for conexp-clj. The copyright is held by William
# Stein, see the corresponding files for more information.

from sage.interfaces.expect import Expect, ExpectElement, ExpectFunction, FunctionElement, gc_disabled
from sage.misc.sage_eval import sage_eval

###

class ConexpCLJ(Expect):
    def __init__(self,
                 maxread=100000,
                 script_subdirectory=None,
                 logfile=None,
                 server=None,
                 server_tmpdir=None):
        Expect.__init__(self,
                        name = 'conexp-clj',
                        prompt = 'user=> ', # fix this!
                        command = 'conexp-clj.sh',
                        maxread = maxread,
                        server = server,
                        server_tmpdir = server_tmpdir,
                        script_subdirectory = script_subdirectory,
                        restart_on_ctrlc = True,
                        verbose_start = False,
                        logfile = logfile,
                        eval_using_file_cutoff=1024)

    def _read_in_file_command(self, filename):
        return '(load-file "%s")'%filename

    def _start(self, *args, **kwds):
        Expect._start(self, *args, **kwds)

    def _quit_string(self):
        return "(System/exit 0)"

    def console(self):
        conexp_clj_console()

    def eval(self, code, strip=True, **kwds):
        with gc_disabled():
            code = str(code)
            out = self._eval_line(code)
            if out.find("Exception") != -1:
                raise TypeError, "Code:\n\t%s\nconexp-clj ERROR:\n\t%s"%(code,out)
            return out

    def _an_element_impl(self):
        return self(0)

    def set(self, var, value):
        cmd = "(def %s %s)"%(var,value)
        out = self.eval(cmd)

    def get(self, var):
        return self.eval('%s'%var)

    def clear(self, var):
        self.eval("(ns-unmap *ns* '%s)"%var)

    def _repr_(self):
        return "conexp-clj"

    def __reduce__(self):
        return reduce_load_conexp_clj, tuple([])

    def version(self):
        return conexp_clj_version()

    def _object_class(self):
        return ConexpCLJElement

    def _function_class(self):
        return ConexpCLJFunction

    def _function_element_class(self):
        return ConexpCLJFunctionElement

    def _true_symbol(self):
        return "true"

    def _false_symbol(self):
        return "false"

    def _equality_symbol(self):
        return "="

    def _inequality_symbol(self):
        return "not="

    def _assign_symbol(self):
        raise NotImplementedError

    def _left_list_delim(self):
        return "["

    def _right_list_delim(self):
        return "]"

    def help(self, command):
        print self.eval("(doc %s)"%str(command))

    #def _convert_args_kwds(self, args=None, kwds=None):
    #    pass

    def function_call(self, function, args=None, kwds=None):
        args, kwds = self._convert_args_kwds(args, kwds)
        function = function.replace("_","-")
        self._check_valid_function_name(function)
        return self.new("(%s %s)"%(function, " ".join([s.name() for s in args])))

    def _coerce_impl(self, x, **kwds):
        if x == None or x == False:
            return self("nil")
        elif x == True:
            return self("true")
        elif isinstance(x, set):
            return self("#{%s}"%(str(map(self, x))[1:-1]))
        elif isinstance(x, (list, tuple)):
            return self("[%s]"%(str(map(self, x))[1:-1]))
        elif isinstance(x, dict):
            string = "{"
            for key in x:
                string += str(key) + " " + str(x[key]) + ", "
            string += "}"
            return self(string)
        else:
            return self(str(x))


class ConexpCLJElement(ExpectElement):
    def _sage_doc_(self):
        M = self._obj.parent()
        return M.help(self._name)

    def _operation(self, operation, right):
        P = self._check_valid()
        try:
            return P.new("(%s %s %s)"%(operation, self._name, right._name))
        except Exception, msg:
            raise TypeError, msg

    def bool(self):
        P = self.parent()
        t = P._true_symbol()
        cmd = '(not (not %s))'%self._name
        return P.eval(cmd) == t

    def gen(self, n):
        P = self.parent()
        return P.new("(nth %s %s)"%(self._name, n))

    def _sage_repr(self):
        raise NotImplementedError

    def _sage_(self):
        P = self._check_valid()
        name = self._name
        if bool(P("(nil? %s)"%name)):
            return None
        elif bool(P("(set? %s)"%name)):
            return set([x._sage_() for x in self])
        elif bool(P("(sequential? %s)"%name)):
            return [x._sage_() for x in self]
        elif bool(P("(map? %s)"%name)):
            dit = {}
            for pair in [x._sage_() for x in self]:
                dit[pair[0]] = pair[1]
            return dit
        else:
            return sage_eval(str(self))

    def attribute(self, attrname):
        P = self._check_valid()
        return P("(%s %s)"%(attrname, self._name))

    def __pow__(self, n):
        P = self._check_valid()
        if not hasattr(n, 'parent') or P is not n.parent():
            n = P(n)
        return self._operation("expt", n)

    def _matrix_(self, n):
        raise NotImplementedError

    def _vector_(self, n):
        raise NotImplementedError

    def __len__(self):
        P = self._check_valid()
        return P("(count %s)"%self._name)

    def __iter__(self):
        self_seq = self.seq()
        for i in xrange(0, len(self)):
            yield self_seq.gen(i)

    def _contains(self, x):
        P = self._check_valid()
        if not hasattr(x, 'parent') or P is not x.parent():
            x = P(x)
        return P("(contains? %s %s)"%(self._name, x))

    def __getitem__(self, key):
        P = self._check_valid()
        return P("(get %s %s)"%(self._name, key))



class ConexpCLJFunction(ExpectFunction):
    def _sage_doc_(self):
        M = self._obj.parent()
        return M.help(self._name)


class ConexpCLJFunctionElement(FunctionElement):
    def _sage_doc_(self):
        M = self._obj.parent()
        return M.help(self._name)

###

conexp_clj = ConexpCLJ()

def reduce_load_conexp_clj():
    return conexp_clj

import os
def conexp_clj_console():
    os.system('conexp-clj.sh')

def conexp_clj_version():
    return str(conexp_clj('(conexp-version)')).strip()

###