/*
 * Copyright 2011,2015 Sven Verdoolaege. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY SVEN VERDOOLAEGE ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SVEN VERDOOLAEGE OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * The views and conclusions contained in the software and documentation
 * are those of the authors and should not be interpreted as
 * representing official policies, either expressed or implied, of
 * Sven Verdoolaege.
 */

#include "isl_config.h"

#include <stdarg.h>
#include <stdio.h>

#include <algorithm>
#include <iostream>
#include <map>
#include <vector>

#include "csharp.h"
#include "generator.h"

/* Argument format for Python methods with a fixed number of arguments.
 */
static const char *fixed_arg_fmt = "arg%d";
/* Argument format for Python methods with a variable number of arguments.
 */
static const char *var_arg_fmt = "args[%d]";

/* Drop the "isl_" initial part of the type name "name".
 */
static string type2csharp(string name) { return name.substr(4); }

/* Print the arguments of a method with "n_arg" arguments, starting at "first".
 */
void csharp_generator::print_method_arguments(int first, int n_arg) {
  for (int i = first; i < n_arg; ++i) {
    if (i > first)
      printf(", ");
    printf("arg%d", i);
  }
}

/* Print the start of a definition for method "name"
 * (without specifying the arguments).
 * If "is_static" is set, then mark the python method as static.
 *
 * If the method is called "from", then rename it to "convert_from"
 * because "from" is a python keyword.
 */
static void print_method_def(bool is_static, bool is_get, const string &name,
                             QualType returnType) {
  const char *s;
  printf("    public");
  if (is_static)
    printf(" static ");

  s = name.c_str();
  if (name == "from")
    s = "convert_from";

  printf(" %s %s", returnType->getTypeClassName(), s);
}

/* Print the header of the method "name" with "n_arg" arguments.
 * If "is_static" is set, then mark the python method as static.
 */
void csharp_generator::print_method_header(bool is_static, bool is_get,
                                           const string &name, int n_arg,
                                           QualType returnType) {
  print_method_def(is_static, is_get, name, returnType);
  printf("(");
  print_method_arguments(0, n_arg);
  printf(") {\n");
}

/* Print formatted output with the given indentation.
 */
static void print_indent(int indent, const char *format, ...) {
  va_list args;

  printf("%*s", indent, " ");
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
}

/* Print a check that the argument in position "pos" is of type "type"
 * with the given indentation.
 * If this fails and if "upcast" is set, then convert the first
 * argument to "super" and call the method "name" on it, passing
 * the remaining of the "n" arguments.
 * If the check fails and "upcast" is not set, then simply raise
 * an exception.
 * If "upcast" is not set, then the "super", "name" and "n" arguments
 * to this function are ignored.
 * "fmt" is the format for printing Python method arguments.
 */
void csharp_generator::print_type_check(int indent, const string &type,
                                        const char *fmt, int pos, bool upcast,
                                        const string &super, const string &name,
                                        int n) {
  print_indent(indent, "try:\n");
  print_indent(indent, "    if not ");
  printf(fmt, pos);
  printf(".__class__ is %s:\n", type.c_str());
  print_indent(indent, "        ");
  printf(fmt, pos);
  printf(" = %s(", type.c_str());
  printf(fmt, pos);
  printf(")\n");
  print_indent(indent, "except:\n");
  if (upcast) {
    print_indent(indent, "    return %s(", type2csharp(super).c_str());
    printf(fmt, 0);
    printf(").%s(", name.c_str());
    for (int i = 1; i < n; ++i) {
      if (i != 1)
        printf(", ");
      printf(fmt, i);
    }
    printf(")\n");
  } else
    print_indent(indent, "    raise\n");
}

/* For each of the "n" initial arguments of the function "method"
 * that refer to an isl structure,
 * including the object on which the method is called,
 * check if the corresponding actual argument is of the right type.
 * If not, try and convert it to the right type.
 * If that doesn't work and if "super" contains at least one element,
 * try and convert this to the type of the first superclass in "super" and
 * call the corresponding method.
 * If "first_is_ctx" is set, then the first argument is skipped.
 */
void csharp_generator::print_type_checks(const string &cname,
                                         FunctionDecl *method,
                                         bool first_is_ctx, int n,
                                         const vector<string> &super) {
  for (int i = first_is_ctx; i < n; ++i) {
    ParmVarDecl *param = method->getParamDecl(i);
    string type;

    if (!is_isl_type(param->getOriginalType()))
      continue;
    type = type2csharp(extract_type(param->getOriginalType()));
    if (!first_is_ctx && i > 0 && super.size() > 0)
      print_type_check(8, type, fixed_arg_fmt, i - first_is_ctx, true, super[0],
                       cname, n);
    else
      print_type_check(8, type, fixed_arg_fmt, i - first_is_ctx, false, "",
                       cname, -1);
  }
}

/* Print a call to the *_copy function corresponding to "type".
 */
void csharp_generator::print_copy(QualType type) {
  string type_s = extract_type(type);

  printf("Interop.%s_copy", type_s.c_str());
}

/* Construct a wrapper for callback argument "param" (at position "arg").
 * Assign the wrapper to "cb{arg}".
 *
 * The wrapper converts the arguments of the callback to python types,
 * taking a copy if the C callback does not take its arguments.
 * If any exception is thrown, the wrapper keeps track of it in exc_info[0]
 * and returns a value indicating an error.  Otherwise the wrapper
 * returns a value indicating success.
 * In case the C callback is expected to return an isl_stat,
 * the error value is -1 and the success value is 0.
 * In case the C callback is expected to return an isl_bool,
 * the error value is -1 and the success value is 1 or 0 depending
 * on the result of the Python callback.
 * Otherwise, None is returned to indicate an error and
 * a copy of the object in case of success.
 */
void csharp_generator::print_callback(ParmVarDecl *param, int arg) {
  QualType type = param->getOriginalType();
  const FunctionProtoType *fn = extract_prototype(type);
  QualType return_type = fn->getReturnType();
  unsigned n_arg = fn->getNumArgs();

  printf("        var exc_info = new Exception[1];\n");
  printf("        var fn = (");
  for (unsigned i = 0; i < n_arg - 1; ++i) {
    if (!is_isl_type(fn->getArgType(i)))
      die("Argument has non-isl type");
    if (i)
      printf(", ");
    printf("IntPtr cb_arg%d", i);
  }
  printf(", IntPtr user) => {\n");
  for (unsigned i = 0; i < n_arg - 1; ++i) {
    string arg_type;
    arg_type = type2csharp(extract_type(fn->getArgType(i)));
    printf("            var cb_cls%d = new %s(ctx: arg0.ctx, ptr: ", i,
           arg_type.c_str());
    if (!callback_takes_argument(param, i))
      print_copy(fn->getArgType(i));
    printf("(cb_arg%d));\n", i);
  }
  printf("            try {\n");
  if (is_isl_stat(return_type))
    printf("                arg%d(", arg);
  else
    printf("                var res = arg%d(", arg);
  for (unsigned i = 0; i < n_arg - 1; ++i) {
    if (i)
      printf(", ");
    printf("cb_cls%d", i);
  }
  printf(");\n");
  printf("            }\n");
  printf("            catch (Exception e) { \n");
  printf("                exc_info[0] = e;\n");
  if (is_isl_stat(return_type) || is_isl_bool(return_type))
    printf("                return -1;\n");
  else
    printf("                return IntPtr.Zero;\n");
  printf("             }\n");
  if (is_isl_stat(return_type)) {
    printf("            return 0;\n");
  } else if (is_isl_bool(return_type)) {
    printf("            return res;\n");
  } else {
    printf("            return ");
    print_copy(return_type);
    printf("(res.ptr);\n");
  }
  printf("    };\n");
  printf("        var cb%d = Marshal.GetFunctionPointerForDelegate(cb_func);\n",
         arg);
}

/* Print the argument at position "arg" in call to "fd".
 * "fmt" is the format for printing Python method arguments.
 * "skip" is the number of initial arguments of "fd" that are
 * skipped in the Python method.
 *
 * If the (first) argument is an isl_ctx, then print "ctx",
 * assuming that the caller has made the context available
 * in a "ctx" variable.
 * Otherwise, if the argument is a callback, then print a reference to
 * the corresponding callback wrapper.
 * Otherwise, if the argument is marked as consuming a reference,
 * then pass a copy of the pointer stored in the corresponding
 * argument passed to the Python method.
 * Otherwise, if the argument is a string, then the python string is first
 * encoded as a byte sequence, using 'ascii' as encoding.  This assumes
 * that all strings passed to isl can be converted to 'ascii'.
 * Otherwise, if the argument is a pointer, then pass this pointer itthis.
 * Otherwise, pass the argument directly.
 */
void csharp_generator::print_arg_in_call(FunctionDecl *fd, const char *fmt,
                                         int arg, int skip) {
  ParmVarDecl *param = fd->getParamDecl(arg);
  QualType type = param->getOriginalType();
  if (is_isl_ctx(type)) {
    printf("ctx");
  } else if (is_callback(type)) {
    printf("cb%d", arg - skip);
  } else if (takes(param)) {
    print_copy(type);
    printf("(");
    printf(fmt, arg - skip);
    printf(".ptr)");
  } else if (is_string(type)) {
    printf(fmt, arg - skip);
    printf(".encode('ascii')");
  } else if (type->isPointerType()) {
    printf(fmt, arg - skip);
    printf(".ptr");
  } else {
    printf(fmt, arg - skip);
  }
}

/* Generate code that raises the exception captured in "exc_info", if any,
 * with the given indentation.
 */
static void print_rethrow(int indent, const char *exc_info) {
  print_indent(indent, "if (%s is not null) {\n", exc_info);
  print_indent(indent, "    throw %s;\n", exc_info);
  print_indent(indent, "}\n");
}

/* Print code with the given indentation that checks
 * whether any of the persistent callbacks of "clazz"
 * is set and if it failed with an exception.  If so, the 'exc_info'
 * field contains the exception and is raised again.
 * The field is cleared because the callback and its data may get reused.
 * "fmt" is the format for printing Python method arguments.
 */
static void print_persistent_callback_failure_check(int indent,
                                                    const isl_class &clazz,
                                                    const char *fmt) {
  const set<FunctionDecl *> &callbacks = clazz.persistent_callbacks;
  set<FunctionDecl *>::const_iterator in;

  for (in = callbacks.begin(); in != callbacks.end(); ++in) {
    string callback_name = clazz.persistent_callback_name(*in);

    print_indent(indent, "if hasattr(");
    printf(fmt, 0);
    printf(", '%s') and ", callback_name.c_str());
    printf(fmt, 0);
    printf(".%s['exc_info'] != None:\n", callback_name.c_str());
    print_indent(indent, "    exc_info = ");
    printf(fmt, 0);
    printf(".%s['exc_info'][0]\n", callback_name.c_str());
    print_indent(indent, "    ");
    printf(fmt, 0);
    printf(".%s['exc_info'][0] = None\n", callback_name.c_str());
    print_rethrow(indent + 4, "exc_info");
  }
}

/* Print the return statement of the python method corresponding
 * to the C function "method" with the given indentation.
 * If the object on which the method was called
 * may have a persistent callback, then first check if any of those failed.
 * "fmt" is the format for printing Python method arguments.
 *
 * If the method returns a new instance of the same object type and
 * if the class has any persistent callbacks, then the data
 * for these callbacks are copied from the original to the new object.
 * If the method it itthis setting a persistent callback,
 * then keep track of the constructed C callback (such that it doesn't
 * get destroyed) and the data structure that holds the captured exception
 * (such that it can be raised again).
 * The callback appears in position 1 and the C callback is therefore
 * called "cb1".
 *
 * If the return type is a (const) char *, then convert the result
 * to a Python string, raising an error on NULL and freeing
 * the C string if needed.  For python 3 compatibility, the string returned
 * by isl is explicitly decoded as an 'ascii' string.  This is correct
 * as all strings returned by isl are expected to be 'ascii'.
 *
 * If the return type is isl_stat, isl_bool or isl_size, then
 * raise an error on isl_stat_error, isl_bool_error or isl_size_error.
 * In case of isl_bool, the result is converted to
 * a Python boolean.
 * In case of isl_size, the result is converted to a Python int.
 */
void csharp_generator::print_method_return(int indent, const isl_class &clazz,
                                           FunctionDecl *method,
                                           const char *fmt) {
  QualType return_type = method->getReturnType();

  if (!is_static(clazz, method))
    print_persistent_callback_failure_check(indent, clazz, fmt);

  if (is_isl_type(return_type)) {
    string type;

    type = type2csharp(extract_type(return_type));
    print_indent(indent, "var obj = new %s(ctx=ctx, ptr=res);\n", type.c_str());
    if (is_mutator(clazz, method) && clazz.has_persistent_callbacks())
      print_indent(indent, "obj.copy_callbacks(arg0);\n");
    if (clazz.persistent_callbacks.count(method)) {
      string callback_name;

      callback_name = clazz.persistent_callback_name(method);
      print_indent(indent, "obj.%s = new CallBackInfo(cb1, exc_info);\n",
                   callback_name.c_str());
    }
    print_indent(indent, "return obj;\n");
  } else if (is_string(return_type)) {
    print_indent(indent, "if (res == IntPtr.Zero) {\n");
    print_indent(indent, "    throw new InvalidOperationException();\n");
    print_indent(indent, "}\n");
    print_indent(indent, "var str = Marshal.PtrToStringAnsi(res);\n");

    if (gives(method))
      print_indent(indent, "Marshal.FreeHGlobal(res);\n");

    print_indent(indent, "return str;\n");
  } else if (is_isl_neg_error(return_type)) {
    print_indent(indent, "if (res < 0) {\n");
    print_indent(indent, "    throw new InvalidOperationException();\n");
    print_indent(indent, "}\n");
    if (is_isl_bool(return_type))
      print_indent(indent, "return res;\n");
    else if (is_isl_size(return_type))
      print_indent(indent, "return res;\n");
  } else {
    print_indent(indent, "return res;\n");
  }
  print_indent(indent - 4, "}\n");
}

/* Print a python "get" method corresponding to the C function "fd"
 * in class "clazz" using a name that includes the "get_" prefix.
 *
 * This method simply calls the variant without the "get_" prefix and
 * returns its result.
 * Note that static methods are not considered to be "get" methods.
 */
// void csharp_generator::print_get_method(const isl_class &clazz,
//                                         FunctionDecl *fd) {
//   string get_name = clazz.base_method_name(fd);
//   string name = clazz.method_name(fd);
//   int num_params = fd->getNumParams();
//   QualType type = fd->getReturnType();

//   print_method_header(false, get_name, num_params, type);
//   printf("        return arg0.%s(", name.c_str());
//   print_method_arguments(1, num_params);
//   printf(")\n");
// }

/* Print a call to "method", along with the corresponding
 * return statement, with the given indentation.
 * "drop_ctx" is set if the first argument is an isl_ctx.
 *
 * A "ctx" variable is first initialized as it may be needed
 * in the first call to print_arg_in_call and in print_method_return.
 *
 * If the method has any callback function, then any exception
 * thrown in any callback also need to be rethrown.
 */
void csharp_generator::print_method_call(int indent, const isl_class &clazz,
                                         FunctionDecl *method, const char *fmt,
                                         int drop_ctx) {
  string fullname = method->getName().str();
  int num_params = method->getNumParams();
  int drop_user = 0;

  if (drop_ctx) {
    print_indent(indent, "var ctx = Context.DefaultInstance;\n");
  } else {
    // print_indent(indent, "ctx = ");
    // printf(fmt, 0);
    // printf(".ctx\n");
  }
  print_indent(indent, "var res = Interop.%s(", fullname.c_str());
  for (int i = 0; i < num_params; ++i) {
    if (i > 0)
      printf(", ");
    print_arg_in_call(method, fmt, i, drop_ctx + drop_user);
    if (!is_callback_arg(method, i))
      continue;
    ++drop_user;
    ++i;
    printf(", IntPtr.Zerp");
  }
  printf(");\n");

  if (drop_user > 0)
    print_rethrow(indent, "exc_info[0]");

  print_method_return(indent, clazz, method, fmt);
}

/* Print a python method corresponding to the C function "method".
 * "super" contains the superclasses of the class to which the method belongs,
 * with the first element corresponding to the annotation that appears
 * closest to the annotated type.  This superclass is the least
 * general extension of the annotated type in the linearization
 * of the class hierarchy.
 *
 * If the first argument of "method" is something other than an instance
 * of the class, then mark the python method as static.
 * If, moreover, this first argument is an isl_ctx, then remove
 * it from the arguments of the Python method.
 *
 * If the function has any callback arguments, then it also has corresponding
 * "user" arguments.  Since Python has closures, there is no need for such
 * user arguments in the Python interface, so we simply drop them.
 * We also create a wrapper ("cb{arg}") for each callback.
 *
 * If the function consumes a reference, then we pass it a copy of
 * the actual argument.
 *
 * For methods that are identified as "get" methods, also
 * print a variant of the method using a name that includes
 * the "get_" prefix.
 */
void csharp_generator::print_method(const isl_class &clazz,
                                    FunctionDecl *method,
                                    vector<string> super) {
  string cname = clazz.method_name(method);
  int num_params = method->getNumParams();

  int drop_ctx = first_arg_is_isl_ctx(method);
  {
    print_method_def(clazz.is_static(method), clazz.is_get_method(method),
                     cname, method->getCallResultType());
    printf("(");
    int nargs = 0;
    for (int i = 0; i < num_params; ++i) {
      auto param = method->getParamDecl(0);
      if (i == 0 && is_isl_ctx(param->getOriginalType())) {
        continue;
      }
      if (is_callback_arg(method, i)) {
        continue;
      }
      if (nargs > 0) {
        printf(", ");
      }
      auto paramType = param->getOriginalType();
      printf("%s arg%d", paramType->getTypeClassName(), nargs);
      nargs++;
    }
    printf(") {\n");
  }

  // print_type_checks(cname, method, drop_ctx, num_params, super);
  int drop_user = 0;
  for (int i = 1; i < num_params; ++i) {
    ParmVarDecl *param = method->getParamDecl(i);
    QualType type = param->getOriginalType();
    if (!is_callback(type))
      continue;
    print_callback(param, i - drop_ctx - drop_user);
    drop_user += 1;
  }
  print_method_call(8, clazz, method, fixed_arg_fmt, drop_ctx);

  // NOTE no need add get method.
  // if (clazz.is_get_method(method))
  // 	print_get_method(clazz, method);
}

/* Print a condition that checks whether Python method argument "i"
 * corresponds to the C function argument type "type".
 */
static void print_argument_check(QualType type, int i) {
  if (generator::is_isl_type(type)) {
    string type_str;
    type_str = generator::extract_type(type);
    type_str = type2csharp(type_str);
    printf("%s arg_%d", type_str.c_str(), i);
  } else if (type->isPointerType()) {
    printf("string arg_%d", i);
  } else {
    printf("int args_%d", i);
  }
}

/* Is any element of "vector" set?
 */
static bool any(const std::vector<bool> &vector) {
  return std::find(vector.begin(), vector.end(), true) != vector.end();
}

/* Print a test that checks whether the arguments passed
 * to the Python method correspond to the arguments
 * expected by "fd" and
 * check if the object on which the method is called, if any,
 * is of the right type.
 * "drop_ctx" is set if the first argument of "fd" is an isl_ctx,
 * which does not appear as an argument to the Python method.
 *
 * If an automatic conversion function is available for any
 * of the argument types, then also allow the argument
 * to be of the type as prescribed by the second input argument
 * of the conversion function.
 * The corresponding arguments are then converted to the expected types
 * if needed.
 * The object on which the method is called is also converted if needed.
 * The argument tuple first needs to be converted to a list
 * in order to be able to modify the entries.
 */
void csharp_generator::print_argument_checks(const isl_class &clazz,
                                             FunctionDecl *fd, int drop_ctx) {
  int num_params = fd->getNumParams();
  bool is_static = generator::is_static(clazz, fd);
  int first = is_static ? drop_ctx : 1;
  std::vector<bool> convert(num_params);
  // num_params - drop_ctx
  printf("(");
  for (int i = first; i < num_params; ++i) {
    ParmVarDecl *param = fd->getParamDecl(i);
    QualType type = param->getOriginalType();
    const Type *ptr = type.getTypePtr();

    if (conversions.count(ptr) == 0) {
      print_argument_check(type, i - drop_ctx);
    } else {
      QualType type2 = conversions.at(ptr)->getOriginalType();
      convert[i] = true;
      print_argument_check(type, i - drop_ctx);
      // note conversion use implict cast.
      // printf("(");
      // printf(" or ");
      // print_argument_check(type2, i - drop_ctx);
      // printf(")");
    }
  }
  printf(") { \n");

  if (is_static && !any(convert))
    return;
  // note no need for type check
  // print_indent(8, "args = list(args)\n");
  // first = is_static ? drop_ctx : 0;
  // for (int i = first; i < num_params; ++i) {
  //   bool is_this = !is_static && i == 0;
  //   ParmVarDecl *param = fd->getParamDecl(i);
  //   string type;

  //   if (!is_this && !convert[i])
  //     continue;
  //   type = type2csharp(extract_type(param->getOriginalType()));
  //   print_type_check(12, type, var_arg_fmt, i - drop_ctx, false, "", "", -1);
  // }
}

/* Print part of an overloaded python method corresponding to the C function
 * "method".
 * "drop_ctx" is set if the first argument of "method" is an isl_ctx.
 *
 * In particular, print code to test whether the arguments passed to
 * the python method correspond to the arguments expected by "method"
 * and to call "method" if they do.
 */
void csharp_generator::print_method_overload(const isl_class &clazz,
                                             FunctionDecl *method) {
  int drop_ctx = first_arg_is_isl_ctx(method);

  print_argument_checks(clazz, method, drop_ctx);
  print_method_call(12, clazz, method, var_arg_fmt, drop_ctx);
}

/* Print a python method with a name derived from "fullname"
 * corresponding to the C functions "methods".
 * "super" contains the superclasses of the class to which the method belongs.
 *
 * If "methods" consists of a single element that is not marked overloaded,
 * the use print_method to print the method.
 * Otherwise, print an overloaded method with pieces corresponding
 * to each function in "methods".
 */
void csharp_generator::print_method(const isl_class &clazz,
                                    const string &fullname,
                                    const function_set &methods,
                                    vector<string> super) {
  string cname;
  function_set::const_iterator it;
  FunctionDecl *any_method;

  any_method = *methods.begin();
  if (methods.size() == 1 && !is_overload(any_method)) {
    print_method(clazz, any_method, super);
    return;
  }

  cname = clazz.method_name(any_method);

  for (it = methods.begin(); it != methods.end(); ++it) {
    print_method_def(is_static(clazz, any_method),
                     clazz.is_get_method(any_method), cname,
                     any_method->getCallResultType());
    print_method_overload(clazz, *it);
  }
  // printf("        raise Error\n");
}

/* Print a python method "name" corresponding to "fd" setting
 * the enum value "value".
 * "super" contains the superclasses of the class to which the method belongs,
 * with the first element corresponding to the annotation that appears
 * closest to the annotated type.
 *
 * The last argument of the C function does not appear in the method call,
 * but is fixed to "value" instead.
 * Other than that, the method printed here is similar to one
 * printed by csharp_generator::print_method, except that
 * some of the special cases do not occur.
 */
void csharp_generator::print_set_enum(const isl_class &clazz, FunctionDecl *fd,
                                      int value, const string &name,
                                      const vector<string> &super) {
  string fullname = fd->getName().str();
  int num_params = fd->getNumParams();

  print_method_header(is_static(clazz, fd), clazz.is_get_method(fd), name,
                      num_params - 1, fd->getCallResultType());

  print_type_checks(name, fd, false, num_params - 1, super);
  printf("        ctx = arg0.ctx\n");
  printf("        res = isl.%s(", fullname.c_str());
  for (int i = 0; i < num_params - 1; ++i) {
    if (i)
      printf(", ");
    print_arg_in_call(fd, fixed_arg_fmt, i, 0);
  }
  printf(", %d", value);
  printf(")\n");
  print_method_return(8, clazz, fd, fixed_arg_fmt);
}

/* Print python methods corresponding to "fd", which sets an enum.
 * "super" contains the superclasses of the class to which the method belongs,
 * with the first element corresponding to the annotation that appears
 * closest to the annotated type.
 *
 * A method is generated for each value in the enum, setting
 * the enum to that value.
 */
void csharp_generator::print_set_enum(const isl_class &clazz, FunctionDecl *fd,
                                      const vector<string> &super) {
  vector<set_enum>::const_iterator it;
  const vector<set_enum> &set_enums = clazz.set_enums.at(fd);

  for (it = set_enums.begin(); it != set_enums.end(); ++it)
    print_set_enum(clazz, fd, it->value, it->method_name, super);
}

/* Print part of the constructor for this isl_class.
 *
 * In particular, check if the actual arguments correspond to the
 * formal arguments of "cons" and if so call "cons" and put the
 * result in this.ptr and a reference to the default context in this.ctx.
 */
void csharp_generator::print_constructor(const isl_class &clazz,
                                         FunctionDecl *cons) {
  string fullname = cons->getName().str();
  string cname = clazz.method_name(cons);
  int num_params = cons->getNumParams();
  int drop_ctx = first_arg_is_isl_ctx(cons);

  print_argument_checks(clazz, cons, drop_ctx);
  printf("        this.ctx = Context.DefaultInstance;\n");
  printf("        this.ptr = Interop.%s(", fullname.c_str());
  if (drop_ctx)
    printf("this.ctx");
  for (int i = drop_ctx; i < num_params; ++i) {
    if (i)
      printf(", ");
    print_arg_in_call(cons, var_arg_fmt, i, drop_ctx);
  }
  printf(")\n");
  printf("    }\n");
}

/* The definition of the part of constructor for the "id" class
 * that construct an object from a name and a user object,
 * without the initial newline.
 *
 * Just like the parts generated by csharp_generator::print_constructor,
 * the result of the isl_id_alloc call is stored in this.ptr and
 * a reference to the default context is stored in this.ctx.
 * Also, just like any other constructor or method with a string argument,
 * the python string is first encoded as a byte sequence,
 * using 'ascii' as encoding.
 *
 * Since the isl_id keeps a reference to the Python user object,
 * the reference count of the Python object needs to be incremented,
 * but only if the construction of the isl_id is successful.
 * The reference count of the Python object is decremented again
 * by Context.free_user when the reference count of the isl_id
 * drops to zero.
 */
static const char *const id_constructor_user = &R"(
        if len(args) == 2 and type(args[0]) == str:
            this.ctx = Context.DefaultInstance
            name = args[0].encode('ascii')
            this.ptr = isl.isl_id_alloc(this.ctx, name, args[1])
            this.ptr = isl.isl_id_set_free_user(this.ptr, Context.free_user)
            if this.ptr is not None:
                pythonapi.Py_IncRef(py_object(args[1]))
            return
)"[1];

/* Print any special constructor parts of this class that are not
 * automatically derived from the C interface.
 *
 * In particular, print a special constructor part for the "id" class.
 */
void csharp_generator::print_special_constructors(const isl_class &clazz) {
  if (clazz.name != "isl_id")
    return;

  printf("%s", id_constructor_user);
}

/* The definition of an "id" method
 * for retrieving the user object associated to the identifier,
 * without the initial newline.
 *
 * The isl_id needs to have been created by the constructor
 * in id_constructor_user.  That is, it needs to have a user pointer and
 * it needs to have its free_user callback set to Context.free_user.
 * The functions need to be cast to c_void_p to be able to compare
 * the addresses.
 *
 * Return None if any of the checks fail.
 * Note that isl_id_get_user returning NULL automatically results in None.
 */
static const char *const id_user = &R"(
    def user(this):
        free_user = cast(Context.free_user, c_void_p)
        id_free_user = cast(isl.isl_id_get_free_user(this.ptr), c_void_p)
        if id_free_user.value != free_user.value:
            return None
        return isl.isl_id_get_user(this.ptr)
)"[1];

/* Print any special methods of this class that are not
 * automatically derived from the C interface.
 *
 * In particular, print a special method for the "id" class.
 */
void csharp_generator::print_special_methods(const isl_class &clazz) {
  if (clazz.name != "isl_id")
    return;

  printf("%s", id_user);
}

/* If "clazz" has a type function describing subclasses,
 * then add constructors that allow each of these subclasses
 * to be treated as an object to the superclass.
 */
void csharp_generator::print_upcast_constructors(const isl_class &clazz) {
  map<int, string>::const_iterator i;

  if (!clazz.fn_type)
    return;

  for (i = clazz.type_subclasses.begin(); i != clazz.type_subclasses.end();
       ++i) {
    printf("    public %s (%s arg0) {\n", type2csharp(clazz.name).c_str(),
           type2csharp(i->second).c_str());
    printf("        this.ctx = arg0.ctx;\n");
    printf("        this.ptr = isl.%s_copy(arg0.ptr);\n", clazz.name.c_str());
    printf("    }\n");
  }
}

/* Print the header of the class "name" with superclasses "super".
 * The order of the superclasses is the opposite of the order
 * in which the corresponding annotations appear in the source code.
 * If "clazz" is a subclass derived from a type function,
 * then the immediate superclass is recorded in "clazz" itthis.
 */
void csharp_generator::print_class_header(const isl_class &clazz,
                                          const string &name,
                                          const vector<string> &super) {
  printf("public class %s", name.c_str());
  if (super.size() > 0) {
    for (unsigned i = 0; i < super.size(); ++i) {
      if (i > 0)
        printf(", ");
      printf("%s", type2csharp(super[i]).c_str());
    }
  } else if (clazz.is_type_subclass()) {
    printf(": %s", type2csharp(clazz.superclass_name).c_str());
  }
  printf("{\n");
}

/* Tell ctypes about the return type of "fd".
 * In particular, if "fd" returns a pointer to an isl object,
 * then tell ctypes it returns a "c_void_p".
 * If "fd" returns a char *, then simply tell ctypes.
 *
 * Nothing needs to be done for functions returning
 * isl_bool, isl_stat or isl_size since they are represented by an int and
 * ctypes assumes that a function returns int by default.
 */
void csharp_generator::print_restype(FunctionDecl *fd) {
  string fullname = fd->getName().str();
  QualType type = fd->getReturnType();
  printf("public static extern IntPtr %s", fullname.c_str());
  // if (is_isl_type(type))
  // else if (is_string(type))
  //   printf("public static extern IntPtr %s \n", fullname.c_str());
}

/* Tell ctypes about the types of the arguments of the function "fd".
 *
 * Any callback argument is followed by a user pointer argument.
 * Each such pair or arguments is handled together.
 */
void csharp_generator::print_argtypes(FunctionDecl *fd) {
  string fullname = fd->getName().str();
  int n = fd->getNumParams();

  printf("(");
  for (int i = 0; i < n; ++i) {
    ParmVarDecl *param = fd->getParamDecl(i);
    QualType type = param->getOriginalType();
    if (i)
      printf(", ");
    if (is_isl_ctx(type))
      printf("Context");
    else if (is_isl_type(type))
      printf("IntPtr");
    else if (is_callback(type))
      printf("[MarshalAs(UnmanagedType.FunctionPtr)] IntPtr arg%d, IntPtr", i);
    else if (is_string(type))
      printf("IntPtr");
    else if (is_long(type))
      printf("long");
    else
      printf("int");
    if (is_callback(type))
      ++i;
    printf(" arg%d", i);
  }
  printf(");\n");
}

/* Print type definitions for the method 'fd'.
 */
void csharp_generator::print_method_type(FunctionDecl *fd) {
  print_restype(fd);
  print_argtypes(fd);
}

/* If "clazz" has a type function describing subclasses or
 * if it is one of those type subclasses, then print a __new__ method.
 *
 * In the superclass, the __new__ method constructs an object
 * of the subclass type specified by the type function,
 * raising an error on an error type.
 * In the subclass, the __new__ method reverts to the original behavior.
 */
void csharp_generator::print_new(const isl_class &clazz,
                                 const string &python_name) {
  if (!clazz.fn_type && !clazz.is_type_subclass())
    return;

  printf("    def __new__(cls, *args, **keywords):\n");

  if (clazz.fn_type) {
    map<int, string>::const_iterator i;

    printf("        if \"ptr\" in keywords:\n");
    printf("            type = isl.%s(keywords[\"ptr\"])\n",
           clazz.fn_type->getNameAsString().c_str());

    for (i = clazz.type_subclasses.begin(); i != clazz.type_subclasses.end();
         ++i) {
      printf("            if type == %d:\n", i->first);
      printf("                return %s(**keywords)\n",
             type2csharp(i->second).c_str());
    }
    printf("            raise Error\n");
  }

  printf("        return super(%s, cls).__new__(cls)\n", python_name.c_str());
}

/* Print declarations for methods printing the class representation,
 * provided there is a corresponding *_to_str function.
 *
 * In particular, provide an implementation of __str__ and __repr__ methods to
 * override the default representation used by python. Python uses __str__ to
 * pretty print the class (e.g., when calling print(obj)) and uses __repr__
 * when printing a precise representation of an object (e.g., when dumping it
 * in the REPL console).
 *
 * Check the type of the argument before calling the *_to_str function
 * on it in case the method was called on an object from a subclass.
 *
 * The return value of the *_to_str function is decoded to a python string
 * assuming an 'ascii' encoding.  This is necessary for python 3 compatibility.
 */
void csharp_generator::print_representation(const isl_class &clazz,
                                            const string &python_name) {
  if (!clazz.fn_to_str)
    return;

  printf("    public override ToString() {\n");
  // print_type_check(8, python_name, fixed_arg_fmt, 0, false, "", "", -1);
  printf("        IntPtr char_p = isl.%s(ptr)\n",
         string(clazz.fn_to_str->getName()).c_str());
  printf("        string res = Marshal.PtrToStringAnsi(ptr);\n");
  printf("        Marshal.FreeHGlobal(ptr);\n");
  printf("        return res;\n");
  printf("    }\n");
}

/* If "clazz" has any persistent callbacks, then print the definition
 * of a "copy_callbacks" function that copies the persistent callbacks
 * from one object to another.
 */
void csharp_generator::print_copy_callbacks(const isl_class &clazz) {
  const set<FunctionDecl *> &callbacks = clazz.persistent_callbacks;
  set<FunctionDecl *>::const_iterator in;

  if (!clazz.has_persistent_callbacks())
    return;

  printf("    public void copy_callbacks(%s obj) {\n",
         type2csharp(clazz.name).c_str());
  for (in = callbacks.begin(); in != callbacks.end(); ++in) {
    string callback_name = clazz.persistent_callback_name(*in);

    printf("        if (obj.%s is not null) {\n", callback_name.c_str());
    printf("            this.%s = obj.%s;\n", callback_name.c_str(),
           callback_name.c_str());
    printf("        }\n");
  }
  printf("    }\n");
}

/* Print code to set method type signatures.
 *
 * To be able to call C functions it is necessary to explicitly set their
 * argument and result types.  Do this for all exported constructors and
 * methods (including those that set a persistent callback and
 * those that set an enum value),
 * as well as for the *_to_str and the type function, if they exist.
 * Assuming each exported class has a *_copy and a *_free method,
 * also unconditionally set the type of such methods.
 */
void csharp_generator::print_interop(const isl_class &clazz) {
  function_set::const_iterator in;
  map<string, function_set>::const_iterator it;
  map<FunctionDecl *, vector<set_enum>>::const_iterator ie;
  const set<FunctionDecl *> &callbacks = clazz.persistent_callbacks;
  printf("internal static partial class Interop {\n");
  for (in = clazz.constructors.begin(); in != clazz.constructors.end(); ++in)
    print_method_type(*in);

  for (in = callbacks.begin(); in != callbacks.end(); ++in)
    print_method_type(*in);
  for (it = clazz.methods.begin(); it != clazz.methods.end(); ++it)
    for (in = it->second.begin(); in != it->second.end(); ++in)
      print_method_type(*in);
  for (ie = clazz.set_enums.begin(); ie != clazz.set_enums.end(); ++ie)
    print_method_type(ie->first);

  print_method_type(clazz.fn_copy);
  print_method_type(clazz.fn_free);
  if (clazz.fn_to_str)
    print_method_type(clazz.fn_to_str);
  if (clazz.fn_type)
    print_method_type(clazz.fn_type);
  printf("}\n");
}

/* Print out the definition of this isl_class.
 *
 * We first check if this isl_class is a subclass of one or more other classes.
 * If it is, we make sure those superclasses are printed out first.
 *
 * Then we print a constructor with several cases, one for constructing
 * a Python object from a return value, one for each function that
 * was marked as a constructor, a class specific constructor, if any, and
 * one for each type based subclass.
 *
 * Next, we print out some common methods, class specific methods and
 * the methods corresponding
 * to functions that are not marked as constructors, including those
 * that set a persistent callback and those that set an enum value.
 *
 * Finally, we tell ctypes about the types of the arguments of the
 * constructor functions and the return types of those function returning
 * an isl object.
 */
void csharp_generator::print(const isl_class &clazz) {
  string p_name = type2csharp(clazz.subclass_name);
  vector<string> super = find_superclasses(clazz.type);
  const set<FunctionDecl *> &callbacks = clazz.persistent_callbacks;

  for (unsigned i = 0; i < super.size(); ++i)
    if (done.find(super[i]) == done.end())
      print(classes[super[i]]);
  if (clazz.is_type_subclass() && done.find(clazz.name) == done.end())
    print(classes[clazz.name]);
  done.insert(clazz.subclass_name);

  printf("\n");
  print_class_header(clazz, p_name, super);
  printf("    private Context ctx;\n");
  printf("    private IntPtr ptr;\n");
  printf("    public %s(isl_context? ctx = null, IntPtr? ptr = null) {\n",
         p_name.c_str());

  printf("        if (ptr is not null) { \n");
  printf("            this.ctx = ctx!;\n");
  printf("            this.ptr = ptr!;\n");
  printf("            }\n");
  printf("    }\n");

  for (const auto &cons : clazz.constructors) {
    printf("    public %s", p_name.c_str());
    print_constructor(clazz, cons);
  }
  print_special_constructors(clazz);
  print_upcast_constructors(clazz);
  // printf("        raise Error\n");
  printf("    public Dispose() {\n");
  printf("        if (ptr is not null) {\n");
  printf("            isl.%s_free(ptr);\n", clazz.name.c_str());
  printf("        }\n");
  printf("    }\n");

  // print_new(clazz, p_name);
  print_representation(clazz, p_name);
  print_copy_callbacks(clazz);

  print_special_methods(clazz);
  for (const auto &callback : callbacks)
    print_method(clazz, callback, super);
  for (const auto &kvp : clazz.methods)
    print_method(clazz, kvp.first, kvp.second, super);
  for (const auto &kvp : clazz.set_enums)
    print_set_enum(clazz, kvp.first, super);

  printf("}\n");

  print_interop(clazz);
}

/* Generate a python interface based on the extracted types and
 * functions.
 *
 * Print out each class in turn.  If one of these is a subclass of some
 * other class, make sure the superclass is printed out first.
 * functions.
 */
void csharp_generator::generate() {
  map<string, isl_class>::iterator ci;
  printf("using System.Runtime.InteropServices;\n");
  printf("namespace ISLSharp;\n");

  for (ci = classes.begin(); ci != classes.end(); ++ci) {
    if (done.find(ci->first) == done.end())
      print(ci->second);
  }
}
