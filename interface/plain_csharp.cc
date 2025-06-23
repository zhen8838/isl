/*
 * Copyright 2016, 2017 Tobias Grosser. All rights reserved.
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
 * THIS SOFTWARE IS PROVIDED BY TOBIAS GROSSER ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL TOBIAS GROSSER OR
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
 * Tobias Grosser.
 */

#include <cstdarg>
#include <cstdio>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "isl_config.h"
#include "plain_csharp.h"

/* Print string formatted according to "fmt" to ostream "os".
 *
 * This osprintf method allows us to use printf style formatting constructs when
 * writing to an ostream.
 */
static void osprintf(ostream &os, const char *format, va_list arguments) {
  va_list copy;
  char *string_pointer;
  size_t size;

  va_copy(copy, arguments);
  size = vsnprintf(NULL, 0, format, copy);
  string_pointer = new char[size + 1];
  va_end(copy);
  vsnprintf(string_pointer, size + 1, format, arguments);
  os << string_pointer;
  delete[] string_pointer;
}

/* Print string formatted according to "fmt" to ostream "os".
 *
 * This osprintf method allows us to use printf style formatting constructs when
 * writing to an ostream.
 */
static void osprintf(ostream &os, const char *format, ...) {
  va_list arguments;

  va_start(arguments, format);
  osprintf(os, format, arguments);
  va_end(arguments);
}

/* Print string formatted according to "fmt" to ostream "os"
 * with the given indentation.
 *
 * This osprintf method allows us to use printf style formatting constructs when
 * writing to an ostream.
 */
static void osprintf(ostream &os, int indent, const char *format, ...) {
  va_list arguments;

  osprintf(os, "%*s", indent, " ");
  va_start(arguments, format);
  osprintf(os, format, arguments);
  va_end(arguments);
}

/* Convert "l" to a string.
 */
static std::string to_string(long l) {
  std::ostringstream strm;
  strm << l;
  return strm.str();
}

/* Construct a generator for plain C++ bindings.
 *
 * "checked" is set if C++ bindings should be generated
 * that rely on the user to check for error conditions.
 */
plain_csharp_generator::plain_csharp_generator(
    SourceManager &SM, set<RecordDecl *> &exported_types,
    set<FunctionDecl *> exported_functions, set<FunctionDecl *> functions,
    bool checked)
    : csharp_generator(SM, exported_types, exported_functions, functions),
      checked(checked) {}

/* Generate a csharp interface based on the extracted types and functions.
 *
 * Print first a set of forward declarations for all isl wrapper
 * classes, then the declarations of the classes, and at the end all
 * implementations.
 *
 * If checked C++ bindings are being generated,
 * then wrap them in a namespace to avoid conflicts
 * with the default C++ bindings (with automatic checks using exceptions).
 */
void plain_csharp_generator::generate() {
  ostream &os = cout;

  osprintf(os, "using System;\n");
  osprintf(os, "using System.Runtime.InteropServices;\n");
  osprintf(os, "#pragma warning disable\n");
  osprintf(os, "namespace IntegerSetLibrary {\n\n");
  if (checked)
    osprintf(os, "namespace checked {\n\n");

  // print_forward_declarations(os);
  // osprintf(os, "\n");
  // print_declarations(os);
  // osprintf(os, "\n");
  print_implementations(os);

  if (checked)
    osprintf(os, "} // namespace checked\n");

  osprintf(os, "internal static partial class Interop {\n");
  print_interops(os);
  osprintf(os, "}\n");

  osprintf(os, "} // namespace isl\n");
  osprintf(os, "#pragma warning restore\n");
}

/* Print forward declarations for all classes to "os".
 */
void plain_csharp_generator::print_forward_declarations(ostream &os) {
  map<string, isl_class>::iterator ci;

  osprintf(os, "// forward declarations\n");

  for (ci = classes.begin(); ci != classes.end(); ++ci)
    print_class_forward_decl(os, ci->second);
}

/* Print all declarations to "os".
 */
void plain_csharp_generator::print_declarations(ostream &os) {
  map<string, isl_class>::iterator ci;
  bool first = true;

  for (ci = classes.begin(); ci != classes.end(); ++ci) {
    if (first)
      first = false;
    else
      osprintf(os, "\n");

    print_class(os, ci->second);
  }
}

void plain_csharp_generator::print_interops(ostream &os) {
  map<string, isl_class>::iterator ci;
  bool first = true;

  for (ci = classes.begin(); ci != classes.end(); ++ci) {
    if (first)
      first = false;
    else
      osprintf(os, "\n");

    print_class_interops(os, ci->second);
  }
}

/* Print all implementations to "os".
 */
void plain_csharp_generator::print_implementations(ostream &os) {
  map<string, isl_class>::iterator ci;
  bool first = true;

  for (ci = classes.begin(); ci != classes.end(); ++ci) {
    if (first)
      first = false;
    else
      osprintf(os, "\n");

    print_class_impl(os, ci->second);
  }
}

/* If the printed class is a subclass that is based on a type function,
 * then introduce a "type" field that holds the value of the type
 * corresponding to the subclass and make the fields of the class
 * accessible to the "isa" and "as" methods of the (immediate) superclass.
 * In particular, "isa" needs access to the type field itself,
 * while "as" needs access to the private constructor.
 * In case of the "isa" method, all instances are made friends
 * to avoid access right confusion.
 */
void plain_csharp_generator::decl_printer::print_subclass_type() {
  std::string super;
  const char *csharpname = csharpstring.c_str();
  const char *supername;

  if (!clazz.is_type_subclass())
    return;

  super = type2csharp(clazz.superclass_name);
  supername = super.c_str();
  osprintf(os, "  template <class T>\n");
  osprintf(os, "  friend %s %s::isa() const;\n",
           generator.isl_bool2csharp().c_str(), supername);
  osprintf(os, "  friend %s %s::as<%s>() const;\n", csharpname, supername,
           csharpname);
  osprintf(os, "  static const auto type = %s;\n", clazz.subclass_name.c_str());
}

/* Print declarations for class "clazz" to "os".
 *
 * If "clazz" is a subclass based on a type function,
 * then it is made to inherit from the (immediate) superclass and
 * a "type" attribute is added for use in the "as" and "isa"
 * methods of the superclass.
 *
 * Conversely, if "clazz" is a superclass with a type function,
 * then declare those "as" and "isa" methods.
 *
 * The pointer to the isl object is only added for classes that
 * are not subclasses, since subclasses refer to the same isl object.
 */
void plain_csharp_generator::print_class(ostream &os, const isl_class &clazz) {
  decl_printer printer(os, clazz, *this);
  const char *name = clazz.name.c_str();
  const char *csharpname = printer.csharpstring.c_str();

  osprintf(os, "// declarations for isl::%s\n", csharpname);

  printer.print_class_factory();
  osprintf(os, "\n");
  osprintf(os, "class %s ", csharpname);
  if (clazz.is_type_subclass())
    osprintf(os, ": public %s ", type2csharp(clazz.superclass_name).c_str());
  osprintf(os, "{\n");
  printer.print_subclass_type();
  printer.print_class_factory("  friend ");
  osprintf(os, "\n");
  osprintf(os, " protected:\n");
  if (!clazz.is_type_subclass()) {
    osprintf(os, "  %s *ptr = nullptr;\n", name);
    osprintf(os, "\n");
  }
  printer.print_protected_constructors();
  osprintf(os, "\n");
  osprintf(os, " public:\n");
  printer.print_public_methods();

  osprintf(os, "};\n");
}

void plain_csharp_generator::print_interop_restype(ostream &os,
                                                   FunctionDecl *fd) {
  string fullname = fd->getName().str();
  QualType type = fd->getReturnType();
  const char *keywords = "public static extern ";

  if (is_isl_type(type))
    osprintf(os, "%s IntPtr", keywords);
  else if (is_string(type)) {
    if (gives(fd)) {
      osprintf(os, "[return: MarshalAs(UnmanagedType.LPStr)]\n %s string",
               keywords);
    } else {
      osprintf(os, "%s IntPtr", keywords);
    }
  } else if (type->isVoidType())
    osprintf(os, "%s void", keywords);
  else if (is_isl_stat(type)) {
    osprintf(os, "%s isl_stat", keywords);
  } else if (is_isl_bool(type)) {
    osprintf(os, "%s isl_bool", keywords);
  } else if (is_isl_size(type)) {
    osprintf(os, "%s int", keywords);
  } else if (type->isEnumeralType()) {

    osprintf(os, "%s %s", keywords,
             type2csharp(type.getAsString().substr(5)).c_str());
  } else if (type->isIntegerType()) {
    if (is_long(type)) {
      osprintf(os, "%s long", keywords);
    } else {
      osprintf(os, "%s int", keywords);
    }
  } else
    die("not support?");
}

void plain_csharp_generator::print_interop_argtypes(ostream &os,
                                                    FunctionDecl *fd) {
  string fullname = fd->getName().str();
  int n = fd->getNumParams();

  printf("%s(", fullname.c_str());
  for (int i = 0; i < n; ++i) {
    ParmVarDecl *param = fd->getParamDecl(i);
    QualType type = param->getOriginalType();
    if (i)
      osprintf(os, ", ");
    if (is_isl_ctx(type))
      osprintf(os, "IntPtr");
    else if (is_isl_type(type))
      osprintf(os, "IntPtr");
    else if (is_callback(type)) {
      osprintf(os, "[MarshalAs(UnmanagedType.FunctionPtr)] ");
      osprintf(os, generate_callback_type(type, false).c_str());
    } else if (is_string(type))
      osprintf(os, "[MarshalAs(UnmanagedType.LPStr)] string");
    else if (is_long(type))
      osprintf(os, "long");
    else if (is_double(type))
      osprintf(os, "double");
    else if (type->isEnumeralType())
      osprintf(os, type2csharp(type.getAsString().substr(5)).c_str());
    else if (type->isVoidPointerType())
      osprintf(os, "IntPtr");
    else if (type->isUnsignedIntegerType())
      osprintf(os, "uint");
    else if (type->isSignedIntegerType())
      osprintf(os, "int");

    osprintf(os, " ");
    auto name = param->getNameAsString();
    if (name == "params")
      name += "_";
    osprintf(os, name.c_str());
  }
  osprintf(os, ");\n");
}

void plain_csharp_generator::print_method_interop_type(ostream &os,
                                                       FunctionDecl *fd) {
  osprintf(os, "[DllImport(LibraryName)]\n");
  print_interop_restype(os, fd);
  osprintf(os, " ");
  print_interop_argtypes(os, fd);
  osprintf(os, "\n");
}

void plain_csharp_generator::print_class_interops(ostream &os,
                                                  const isl_class &clazz) {
  for (auto &&ctor : clazz.constructors) {
    print_method_interop_type(os, ctor);
  }

  for (auto &&callback : clazz.persistent_callbacks) {
    print_method_interop_type(os, callback);
  }

  for (auto &&method : clazz.methods) {
    for (auto &&it : method.second) {
      if (clazz.copied_from.find(it) == clazz.copied_from.end()) {
        if (!clazz.is_try_get_method(it)) {
          print_method_interop_type(os, it);
        }
      }
    }
  }
  for (auto &&it : clazz.set_enums) {
    print_method_interop_type(os, it.first);
  }
  if (!clazz.is_type_subclass())
    print_method_interop_type(os, clazz.fn_copy);
  if (!clazz.is_type_subclass())
    print_method_interop_type(os, clazz.fn_free);
  if (clazz.fn_to_str && !clazz.is_type_subclass())
    print_method_interop_type(os, clazz.fn_to_str);
  if (clazz.fn_type)
    print_method_interop_type(os, clazz.fn_type);
}

/* Print forward declaration of class "clazz" to "os".
 */
void plain_csharp_generator::print_class_forward_decl(ostream &os,
                                                      const isl_class &clazz) {
  std::string csharpstring = type2csharp(clazz);
  const char *csharpname = csharpstring.c_str();

  osprintf(os, "class %s;\n", csharpname);
}

/* Print global factory functions.
 *
 * Each class has two global factory functions:
 *
 * 	set manage(__isl_take isl_set *ptr);
 * 	set manage_copy(__isl_keep isl_set *ptr);
 *
 * A user can construct isl C++ objects from a raw pointer and indicate whether
 * they intend to take the ownership of the object or not through these global
 * factory functions. This ensures isl object creation is very explicit and
 * pointers are not converted by accident. Thanks to overloading, manage() and
 * manage_copy() can be called on any isl raw pointer and the corresponding
 * object is automatically created, without the user having to choose the right
 * isl object type.
 *
 * For a subclass based on a type function, no factory functions
 * are introduced because they share the C object type with
 * the superclass.
 */
void plain_csharp_generator::decl_printer::print_class_factory(
    const std::string &prefix) {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();

  if (clazz.is_type_subclass())
    return;

  os << prefix;
  osprintf(os, "inline %s manage(__isl_take %s *ptr);\n", csharpname, name);
  os << prefix;
  osprintf(os, "inline %s manage_copy(__isl_keep %s *ptr);\n", csharpname,
           name);
}

/* Print declarations of protected constructors.
 *
 * Each class has currently one protected constructor:
 *
 * 	1) Constructor from a plain isl_* C pointer
 *
 * Example:
 *
 * 	set(__isl_take isl_set *ptr);
 *
 * The raw pointer constructor is kept protected. Object creation is only
 * possible through manage() or manage_copy().
 */
void plain_csharp_generator::decl_printer::print_protected_constructors() {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();

  osprintf(os, "  inline explicit %s(__isl_take %s *ptr);\n", csharpname, name);
}

/* Print declarations of public constructors.
 *
 * Each class currently has two public constructors:
 *
 * 	1) A default constructor
 * 	2) A copy constructor
 *
 * Example:
 *
 *	set();
 *	set(const set &set);
 */
void plain_csharp_generator::decl_printer::print_public_constructors() {
  const char *csharpname = csharpstring.c_str();
  osprintf(os, "  inline /* implicit */ %s();\n", csharpname);

  osprintf(os, "  inline /* implicit */ %s(const %s &obj);\n", csharpname,
           csharpname);
}

/* Print declarations for "method".
 */
void plain_csharp_generator::decl_printer::print_method(
    const CSharpConversionMethod &method) {
  print_full_method_header(method);
}

/* Print declarations for "method".
 */
void plain_csharp_generator::decl_printer::print_method(
    const CSharpMethod &method) {
  print_full_method_header(method);
}

/* Print a declaration for a constructor for the "id" class
 * that takes a user object.
 */
void plain_csharp_generator::decl_printer::print_id_constructor_user() {
  print_id_constructor_user_header();
}

/* Print a declaration for an "id" method
 * for retrieving the user object associated to the identifier.
 * If "optional" is set, the method returns a std::optional user object.
 */
void plain_csharp_generator::decl_printer::print_id_user(bool optional) {
  print_id_user_header(optional);
}

/* Print declarations of copy assignment operator.
 *
 * Each class has one assignment operator.
 *
 * 	isl:set &set::operator=(set obj)
 *
 */
void plain_csharp_generator::decl_printer::print_copy_assignment() {
  const char *csharpname = csharpstring.c_str();

  osprintf(os, "  inline %s &operator=(%s obj);\n", csharpname, csharpname);
}

/* Print declaration of destructor.
 *
 * No explicit destructor is needed for type based subclasses.
 */
void plain_csharp_generator::decl_printer::print_destructor() {
  const char *csharpname = csharpstring.c_str();

  if (clazz.is_type_subclass())
    return;

  osprintf(os, "  inline ~%s();\n", csharpname);
}

/* Print declaration of pointer functions.
 * Since type based subclasses share the pointer with their superclass,
 * they can also reuse these functions from the superclass.
 *
 * To obtain a raw pointer three functions are provided:
 *
 * 	1) __isl_give isl_set *copy()
 *
 * 	  Returns a pointer to a _copy_ of the internal object
 *
 * 	2) __isl_keep isl_set *get()
 *
 * 	  Returns a pointer to the internal object
 *
 * 	3) __isl_give isl_set *release()
 *
 * 	  Returns a pointer to the internal object and resets the
 * 	  internal pointer to nullptr.
 *
 * We also provide functionality to explicitly check if a pointer is
 * currently managed by this object.
 *
 * 	4) bool is_null()
 *
 * 	  Check if the current object is a null pointer.
 *
 * The functions get() and release() model the value_ptr proposed in
 * http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3339.pdf.
 * The copy() function is an extension to allow the user to explicitly
 * copy the underlying object.
 *
 * Also generate a declaration to delete copy() for r-values, for
 * r-values release() should be used to avoid unnecessary copies.
 */
void plain_csharp_generator::decl_printer::print_ptr() {
  const char *name = clazz.name.c_str();

  if (clazz.is_type_subclass())
    return;

  osprintf(os, "  inline __isl_give %s *copy() const &;\n", name);
  osprintf(os, "  inline __isl_give %s *copy() && = delete;\n", name);
  osprintf(os, "  inline __isl_keep %s *get() const;\n", name);
  osprintf(os, "  inline __isl_give %s *release();\n", name);
  osprintf(os, "  inline bool is_null() const;\n");
}

/* Print a template declaration with given indentation
 * for the "isa_type" method that ensures it is only enabled
 * when called with a template argument
 * that represents a type that is equal to that
 * of the return type of the type function of "super".
 * In particular, "isa_type" gets called from "isa"
 * with as template argument the type of the "type" field
 * of the subclass.
 * The check ensures that this subclass is in fact a direct subclass
 * of "super".
 */
void plain_csharp_generator::decl_printer::print_isa_type_template(
    int indent, const isl_class &super) {
  osprintf(os, indent, "template <typename T,\n");
  osprintf(os, indent,
           "        typename = typename std::enable_if<std::is_same<\n");
  osprintf(os, indent, "                const decltype(%s(NULL)),\n",
           super.fn_type->getNameAsString().c_str());
  osprintf(os, indent, "                const T>::value>::type>\n");
}

/* Print declarations for the "as" and "isa" methods, if the printed class
 * is a superclass with a type function.
 *
 * "isa" checks whether an object is of a given subclass type.
 * "isa_type" does the same, but gets passed the value of the type field
 * of the subclass as a function argument and the type of this field
 * as a template argument.
 * "as" tries to cast an object to a given subclass type, returning
 * an invalid object if the object is not of the given type.
 */
void plain_csharp_generator::decl_printer::print_downcast() {
  if (!clazz.fn_type)
    return;

  osprintf(os, " private:\n");
  print_isa_type_template(2, clazz);
  osprintf(os, "  inline %s isa_type(T subtype) const;\n",
           generator.isl_bool2csharp().c_str());
  osprintf(os, " public:\n");
  osprintf(os, "  template <class T> inline %s isa() const;\n",
           generator.isl_bool2csharp().c_str());
  osprintf(os, "  template <class T> inline T as() const;\n");
}

/* Print the declaration of the ctx method.
 */
void plain_csharp_generator::decl_printer::print_ctx() {
  std::string ns = generator.isl_namespace();

  osprintf(os, "  inline %sContext ctx() const;\n", ns.c_str());
}

/* Print a separator between groups of method declarations.
 */
void plain_csharp_generator::decl_printer::print_method_separator() {
  os << "\n";
}

/* Add a space to the return type "type" if needed,
 * i.e., if it is not the type of a pointer.
 */
static string add_space_to_return_type(const string &type) {
  if (type[type.size() - 1] == '*')
    return type;
  return type + " ";
}

/* Print the prototype of the static inline method that is used
 * as the C callback set by "method".
 */
void plain_csharp_generator::plain_printer::print_persistent_callback_prototype(
    FunctionDecl *method) {
  string callback_prototype_name, rettype, c_args;
  ParmVarDecl *param = persistent_callback_arg(method);
  const FunctionProtoType *callback;
  QualType ptype;
  string classname;

  ptype = param->getType();
  callback = extract_prototype(ptype);

  rettype = callback->getReturnType().getAsString();
  rettype = add_space_to_return_type(rettype);
  if (*rettype.rbegin() == '*') {
    rettype = "IntPtr";
  }
  callback_prototype_name =
      clazz.persistent_callback_name(method) + "_prototype";
  c_args = generator.generate_callback_args(ptype, false, true);

  // if (!declarations)
  //   classname = type2csharp(clazz) + "::";

  osprintf(os, "private %s%s %s(%s)", rettype.c_str(), classname.c_str(),
           callback_prototype_name.c_str(), c_args.c_str());
}

/* Print the prototype of the method for setting the callback function
 * set by "method".
 */
void plain_csharp_generator::plain_printer::
    print_persistent_callback_setter_prototype(FunctionDecl *method) {
  string classname, callback_name, csharptype;
  ParmVarDecl *param = persistent_callback_arg(method);

  // if (!declarations)
  //   classname = type2csharp(clazz) + "::";

  csharptype = generator.param2csharp(param->getOriginalType());
  callback_name = clazz.persistent_callback_name(method);
  osprintf(os, "public void %sset_%s_data(%s %s)", classname.c_str(),
           callback_name.c_str(), csharptype.c_str(),
           param->getName().str().c_str());
}

/* Given a method "method" for setting a persistent callback,
 * print the fields that are needed for marshalling the callback.
 *
 * In particular, print
 * - the declaration of a data structure for storing the C++ callback function
 * - a shared pointer to such a data structure
 * - the declaration of a static inline method
 *   for use as the C callback function
 * - the declaration of a private method for setting the callback function
 */
void plain_csharp_generator::decl_printer::print_persistent_callback_data(
    FunctionDecl *method) {
  string callback_name;
  ParmVarDecl *param = generator.persistent_callback_arg(method);

  callback_name = clazz.persistent_callback_name(method);
  print_callback_data_decl(param, callback_name);
  osprintf(os, "\n");
  osprintf(os, "  %s_tuple? %s_data = null;\n", callback_name.c_str(),
           callback_name.c_str());
  // osprintf(os, "  static inline ");
  // print_persistent_callback_prototype(method);
  // osprintf(os, ";\n");
  // osprintf(os, "  public ");
  // print_persistent_callback_setter_prototype(method);
  // osprintf(os, ";\n");
}

/* Print declarations needed for the persistent callbacks of the class.
 *
 * In particular, if there are any persistent callbacks, then
 * print a private method for copying callback data from
 * one object to another,
 * private data for keeping track of the persistent callbacks and
 * public methods for setting the persistent callbacks.
 */
void plain_csharp_generator::decl_printer::print_persistent_callbacks() {
  const char *csharpname = csharpstring.c_str();

  if (!clazz.has_persistent_callbacks())
    return;

  osprintf(os, " private:\n");
  osprintf(os, "  inline %s &copy_callbacks(const %s &obj);\n", csharpname,
           csharpname);
  for (const auto &callback : clazz.persistent_callbacks)
    print_persistent_callback_data(callback);

  osprintf(os, " public:\n");
  for (const auto &callback : clazz.persistent_callbacks)
    print_method(CSharpMethod(clazz, callback));
}

/* Print a declaration for the "get" method "fd",
 * using a name that includes the "get_" prefix.
 */
void plain_csharp_generator::decl_printer::print_get_method(FunctionDecl *fd) {
  string base = clazz.base_method_name(fd);

  print_method(CSharpMethod(clazz, fd, base));
}

/* Print implementations for class "clazz" to "os".
 */
void plain_csharp_generator::print_class_impl(ostream &os,
                                              const isl_class &clazz) {
  impl_printer impl_p(os, clazz, *this);
  decl_printer decl_p(os, clazz, *this);
  const char *name = clazz.name.c_str();
  const char *csharpname = impl_p.csharpstring.c_str();

  // osprintf(os, "// implementations for isl::%s", csharpname);
  // 1. class decl
  osprintf(os, "public class %s : ", csharpname); /* TODO sealed */
  if (clazz.is_type_subclass())
    osprintf(os, "%s {\n", type2csharp(clazz.superclass_name).c_str());
  else {
    osprintf(os, "IntrusiveHandle {\n");
  }
  // if (!clazz.is_type_subclass()) {
  //   osprintf(os, "  protected IntPtr ptr = IntPtr.Zero;");
  // }
  // impl_p.print_class_factory(); /* remove manage and manage copy */
  impl_p.print_protected_constructors();
  for (const auto &callback : clazz.persistent_callbacks)
    decl_p.print_persistent_callback_data(callback);
  impl_p.print_public_methods();
  impl_p.print_stream_insertion();
  osprintf(os, "}\n");
}

/* Print code for throwing an exception corresponding to the last error
 * that occurred on "saved_ctx".
 * This assumes that a valid isl::ctx is available in the "saved_ctx" variable,
 * e.g., through a prior call to print_save_ctx.
 */
static void print_throw_last_error(ostream &os) {
  osprintf(os, "    throw new InvalidOperationException();\n");
}

/* Print code with the given indentation
 * for throwing an exception_invalid with the given message.
 */
static void print_throw_invalid(ostream &os, int indent, const char *msg) {
  osprintf(os, indent, "throw new ArgumentNullException(\"%s\");\n", msg);
}

/* Print code for throwing an exception on NULL input.
 */
static void print_throw_NULL_input(ostream &os) {
  print_throw_invalid(os, 4, "NULL input");
}

/* Print code with the given indentation
 * for acting on an invalid error with message "msg".
 * In particular, throw an exception_invalid.
 * In the checked C++ bindings, isl_die is called instead with the code
 * in "checked_code".
 */
void plain_csharp_generator::print_invalid(ostream &os, int indent,
                                           const char *msg,
                                           const char *checked_code) {
  if (checked)
    osprintf(os, indent,
             "isl_die(ctx().get(), isl_error_invalid, "
             "\"%s\", %s);\n",
             msg, checked_code);
  else
    print_throw_invalid(os, indent, msg);
}

/* Print an operator for inserting objects of the class
 * into an output stream.
 *
 * Unless checked C++ bindings are being generated,
 * the operator requires its argument to be non-NULL.
 * An exception is thrown if anything went wrong during the printing.
 * During this printing, isl is made not to print any error message
 * because the error message is included in the exception.
 *
 * If checked C++ bindings are being generated and anything went wrong,
 * then record this failure in the output stream.
 */
void plain_csharp_generator::impl_printer::print_stream_insertion() {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();

  // the printer_to_str is not a method for to string.
  if (!clazz.fn_to_str || !is_string(clazz.fn_to_str->getReturnType()))
    return;

  osprintf(os, "\n");
  osprintf(os, "public override string ToString()");
  osprintf(os, "{\n");
  print_check_ptr_start("DangerousGetHandle()");
  osprintf(os, "  var str = Interop.%s_to_str(DangerousGetHandle());\n", name);
  // print_check_ptr_end("str");
  // if (generator.checked) {
  //   osprintf(os, "  if (!str) {\n");
  //   osprintf(os, "    os.setstate(std::ios_base::badbit);\n");
  //   osprintf(os, "    return os;\n");
  //   osprintf(os, "  }\n");
  // }
  // osprintf(os, "  os << str;\n");
  // osprintf(os, "  free(str);\n");
  // osprintf(os, "  return os;\n");
  osprintf(os, "  return str;\n");
  osprintf(os, "}\n");
}

/* Print code that checks that "ptr" is not NULL at input.
 *
 * Omit the check if checked C++ bindings are being generated.
 */
void plain_csharp_generator::impl_printer::print_check_ptr(const char *ptr) {
  if (generator.checked)
    return;

  osprintf(os, "  if (%s == IntPtr.Zero) {\n", ptr);
  print_throw_NULL_input(os);
  osprintf(os, "  }\n");
}

/* Print code that checks that "ptr" is not NULL at input and
 * that saves a copy of the isl_ctx of "ptr" for a later check.
 *
 * Omit the check if checked C++ bindings are being generated.
 */
void plain_csharp_generator::impl_printer::print_check_ptr_start(
    const char *ptr) {
  if (generator.checked)
    return;

  print_check_ptr(ptr);
  // print_save_ctx("Interop." + clazz.name + "_get_ctx(" + ptr + ")");
  // print_on_error_continue();
}

/* Print code that checks that "ptr" is not NULL at the end.
 * A copy of the isl_ctx is expected to have been saved by
 * code generated by print_check_ptr_start.
 *
 * Omit the check if checked C++ bindings are being generated.
 */
void plain_csharp_generator::impl_printer::print_check_ptr_end(
    const char *ptr) {
  if (generator.checked)
    return;

  osprintf(os, "  if (%s == IntPtr.Zero)\n", ptr);
  print_throw_last_error(os);
}

/* Print implementation of global factory functions.
 *
 * Each class has two global factory functions:
 *
 * 	set manage(__isl_take isl_set *ptr);
 * 	set manage_copy(__isl_keep isl_set *ptr);
 *
 * Unless checked C++ bindings are being generated,
 * both functions require the argument to be non-NULL.
 * An exception is thrown if anything went wrong during the copying
 * in manage_copy.
 * During the copying, isl is made not to print any error message
 * because the error message is included in the exception.
 *
 * For a subclass based on a type function, no factory functions
 * are introduced because they share the C object type with
 * the superclass.
 */
void plain_csharp_generator::impl_printer::print_class_factory() {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();

  if (clazz.is_type_subclass())
    return;

  osprintf(os, "\n");
  osprintf(os, "%s manage(__isl_take %s *ptr) {\n", csharpname, name);
  print_check_ptr("handle");
  osprintf(os, "  return %s(ptr);\n", csharpname);
  osprintf(os, "}\n");

  osprintf(os, "%s manage_copy(__isl_keep %s *ptr) {\n", csharpname, name);
  print_check_ptr_start("handle");
  osprintf(os, "  ptr = %s_copy(ptr);\n", name);
  print_check_ptr_end("ptr");
  osprintf(os, "  return %s(ptr);\n", csharpname);
  osprintf(os, "}\n");
}

/* Print implementations of protected constructors.
 *
 * The pointer to the isl object is either initialized directly or
 * through the (immediate) superclass.
 */
void plain_csharp_generator::impl_printer::print_protected_constructors() {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();
  bool subclass = clazz.is_type_subclass();

  osprintf(os, "\n");
  osprintf(os, "internal %s(/* __isl_take */ IntPtr handle)\n", csharpname);
  osprintf(os, "  : base(handle) {}\n");
  // if (subclass)
  //   osprintf(os, "    : base(ptr) {}\n");
  // else
  //   osprintf(os, " { this.ptr = ptr; }\n");
}

/* Print implementations of public constructors.
 *
 * The pointer to the isl object is either initialized directly or
 * through the (immediate) superclass.
 *
 * If the class has any persistent callbacks, then copy them
 * from the original object in the copy constructor.
 * If the class is a subclass, then the persistent callbacks
 * are assumed to be copied by the copy constructor of the superclass.
 *
 * Throw an exception from the copy constructor if anything went wrong
 * during the copying or if the input is NULL, if any copying is performed.
 * During the copying, isl is made not to print any error message
 * because the error message is included in the exception.
 * No exceptions are thrown if checked C++ bindings
 * are being generated,
 */
void plain_csharp_generator::impl_printer::print_public_constructors() {
  std::string super;
  const char *csharpname = csharpstring.c_str();
  bool subclass = clazz.is_type_subclass();

  osprintf(os, "\n");
  if (subclass)
    super = type2csharp(clazz.superclass_name);
  osprintf(os, "%s::%s()\n", csharpname, csharpname);
  if (subclass)
    osprintf(os, "    : %s() {}\n\n", "base");
  else
    osprintf(os, "    : ptr(nullptr) {}\n\n");
  osprintf(os, "%s::%s(const %s &obj)\n", csharpname, csharpname, csharpname);
  if (subclass)
    osprintf(os, "    : %s(obj)\n", super.c_str());
  else
    osprintf(os, "    : ptr(nullptr)\n");
  osprintf(os, "{\n");
  if (!subclass) {
    print_check_ptr_start("obj.ptr");
    osprintf(os, "  ptr = obj.copy();\n");
    if (clazz.has_persistent_callbacks())
      osprintf(os, "  copy_callbacks(obj);\n");
    print_check_ptr_end("ptr");
  }
  osprintf(os, "}\n");
}

/* Print definition for "method",
 * without any automatic type conversions.
 *
 * This method distinguishes three kinds of methods: member methods, static
 * methods, and constructors.
 *
 * Member methods and static methods return a newly managed
 * isl C++ object.
 *
 * Constructors create a new object from a given set of input parameters. They
 * do not return a value, but instead update the pointer stored inside the
 * newly created object.
 *
 * Unless checked C++ bindings are being generated,
 * the inputs of the method are first checked for being valid isl objects and
 * a copy of the associated isl::ctx is saved (if needed).
 * If any failure occurs, either during the check for the inputs or
 * during the isl function call, an exception is thrown.
 * During the function call, isl is made not to print any error message
 * because the error message is included in the exception.
 */
void plain_csharp_generator::impl_printer::print_method(
    const CSharpMethod &method) {
  string methodname = method.fd->getName().str();
  int num_params = method.c_num_params();

  osprintf(os, "\n");
  print_full_method_header(method);
  osprintf(os, "{\n");
  print_argument_validity_check(method);
  // print_save_ctx(method);
  // print_on_error_continue();

  for (const auto &callback : method.callbacks)
    print_callback_local(callback);

  if (!is_void(method.fd->getReturnType())) {
    osprintf(os, "  var res = ");
  }
  osprintf(os, "Interop.%s", methodname.c_str());

  method.print_fd_arg_list(os, 0, num_params, [&](int i, int arg) {
    method.print_param_use(os, i);
  });
  osprintf(os, ";\n");

  print_exceptional_execution_check(method);
  if (method.kind == CSharpMethod::Kind::constructor) {
    osprintf(os, "  SetHandle(res);\n");
  } else {
    print_method_return(method);
  }

  osprintf(os, "}\n");
}

/* Convert argument of type "src" to "dst", with a name specified by "dst".
 *
 * If "src" is the same as "dst", then no argument conversion is needed.
 *
 * Otherwise, call the conversion function
 * with as arguments the isl_ctx of the object and the argument name,
 * or simply the argument name if the source type is an isl type.
 * This means this isl_ctx should be available.
 */
void plain_csharp_generator::impl_printer::print_arg_conversion(
    ParmVarDecl *dst, ParmVarDecl *src) {
  std::string name = dst->getName().str();
  QualType type = dst->getOriginalType();
  string csharptype = generator.param2csharp(type);
  if (name == "params") {
    name += '_';
  }

  if (dst == src)
    os << name;
  else if (is_isl_type(src->getOriginalType()))
    os << "new " << csharptype << "(" << name << ")";
  else
    os << "new " << csharptype << "(ctx.Current, " << name << ")";
}

/* Print a definition for "method",
 * where "this" or at least one of the argument types needs to be converted.
 *
 * "method" is assumed to be a member method.
 *
 * The generated method performs the required conversion(s) and
 * calls the method generated without conversions.
 *
 * Perform a conversion from the argument in the method declaration
 * (as specified by Method::get_param) to the argument of the C function,
 * if needed.
 * Such a conversion may require the isl_ctx to be available.
 * In order to be able to use this isl_ctx, the current object needs
 * to valid.  The validity of other arguments is checked
 * by the called method.
 */
void plain_csharp_generator::impl_printer::print_method(
    const CSharpConversionMethod &method) {
  if (method.kind != CSharpMethod::Kind::member_method)
    die("Automatic conversion currently only supported "
        "for object methods");

  osprintf(os, "\n");
  print_full_method_header(method);
  osprintf(os, "{\n");
  print_check_ptr("handle");
  if (!is_isl_stat(method.fd->getReturnType())) {
    osprintf(os, "  return ");
  }
  method.print_call(os, generator.isl_namespace());
  method.print_csharp_arg_list(os, [&](int i, int arg) {
    ParmVarDecl *param = method.fd->getParamDecl(i);

    print_arg_conversion(param, method.get_param(i));
  });
  osprintf(os, ";\n");
  osprintf(os, "}\n");
}

/* Print a definition for a constructor for the "id" class
 * that takes a user object.
 *
 * The user object is taken as a std::any and copied into
 * a new std::any object on the heap.
 * A pointer to this heap object is stored in the isl_id and
 * is scheduled to be freed when the reference count of the isl_id
 * drops to zero.
 * If the allocation of the isl_id fails, then the heap object
 * will not be freed automatically, so it needs to be freed manually.
 *
 * Unless checked C++ bindings are being generated,
 * the ctx argument is copied into the save_ctx variable
 * for use by print_throw_last_error, which throws an exception
 * if the construction fails.
 * During the function call, isl is made not to print any error message
 * because the error message is included in the exception.
 */
void plain_csharp_generator::impl_printer::print_id_constructor_user() {
  print_id_constructor_user_header();
  os << "{\n";
  if (!generator.checked) {
    print_save_ctx("ctx");
    print_on_error_continue();
  }
  // os << "  std::any *p = new std::any(any);\n";
  os << "  var res = Interop.isl_id_alloc(ctx.get(), str, IntPtr.Zero);\n";
  os << "  res = Interop.isl_id_set_free_user(res, ctx.free_user);\n";
  os << "  if (res == IntPtr.Zero) {\n";
  // os << "    delete p;\n";
  if (!generator.checked)
    print_throw_last_error(os);
  os << "  }\n";
  os << "  ptr = res;\n";
  os << "}\n";
}

/* Print a definition for an "id" method
 * for retrieving the user object associated to the identifier.
 * If "optional" is set, the method returns a std::optional user object.
 * The returned object is of a type specified by template parameter T.
 *
 * The isl_id needs to have been created by the constructor generated
 * by print_id_constructor_user.  That is, it needs to have a user pointer and
 * it needs to have its free_user callback set to &ctx::free_user.
 * The object stored in the std::any also needs to be of the required type.
 *
 * If "optional" is set, return a std::nullopt if any of the checks fail.
 * Otherwise, throw an exception_invalid (or call isl_die and
 * return a default T in the checked C++ bindings).
 */
void plain_csharp_generator::impl_printer::print_id_user(bool optional) {
  auto fail = [&](const char *msg) {
    if (optional)
      os << "    return std::nullopt;\n";
    else
      generator.print_invalid(os, 4, msg, "return T()");
  };
  os << "\n";
  print_id_user_header(optional);
  os << "{\n";
  print_check_ptr("handle");
  os << "  std::any *p = (std::any *) isl_id_get_user(handle);\n";
  os << "  if (!p)\n";
  fail("no user pointer");
  os << "  if (isl_id_get_free_user(handle) != &ctx::free_user)\n";
  fail("user pointer not attached by C++ interface");
  os << "  T *res = std::any_cast<T>(p);\n";
  os << "  if (!res)\n";
  fail("user pointer not of given type");
  os << "  return *res;\n";
  os << "}\n";
}

/* Print implementation of copy assignment operator.
 *
 * If the class has any persistent callbacks, then copy them
 * from the original object.
 */
void plain_csharp_generator::impl_printer::print_copy_assignment() {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();

  osprintf(os, "\n");
  osprintf(os, "%s &%s::operator=(%s obj) {\n", csharpname, csharpname,
           csharpname);
  osprintf(os, "  std::swap(this->ptr, obj.ptr);\n", name);
  if (clazz.has_persistent_callbacks())
    osprintf(os, "  copy_callbacks(obj);\n");
  osprintf(os, "  return *this;\n");
  osprintf(os, "}\n");
}

/* Print implementation of destructor.
 *
 * No explicit destructor is needed for type based subclasses.
 */
void plain_csharp_generator::impl_printer::print_destructor() {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();

  if (clazz.is_type_subclass())
    return;

  osprintf(os, "\n");
  osprintf(os, "protected override bool ReleaseHandle() {\n");
  osprintf(os, "  Interop.%s_free(handle);\n", name);
  osprintf(os, "  return true;\n");
  osprintf(os, "}\n");
}

/* Print a check that the persistent callback corresponding to "fd"
 * is not set, throwing an exception (or printing an error message
 * and returning nullptr) if it is set.
 */
void plain_csharp_generator::print_check_no_persistent_callback(
    ostream &os, const isl_class &clazz, FunctionDecl *fd) {
  string callback_name = clazz.persistent_callback_name(fd);

  osprintf(os, "  if (%s_data is not null)\n", callback_name.c_str());
  print_invalid(os, 4, "cannot release object with persistent callbacks",
                "return nullptr");
}

/* Print implementation of ptr() functions.
 * Since type based subclasses share the pointer with their superclass,
 * they can also reuse these functions from the superclass.
 *
 * If an object has persistent callbacks set, then the underlying
 * C object pointer cannot be released because it references data
 * in the C++ object.
 */
void plain_csharp_generator::impl_printer::print_ptr() {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();
  set<FunctionDecl *>::const_iterator in;
  const set<FunctionDecl *> &callbacks = clazz.persistent_callbacks;

  if (clazz.is_type_subclass())
    return;

  osprintf(os, "\n");
  osprintf(os, "internal override IntPtr IncreaseReference() {\n");
  print_check_ptr("handle");
  osprintf(os, "  return Interop.%s_copy(handle);\n", name);
  osprintf(os, "}\n\n");
}

/* Print implementations for the "as" and "isa" methods, if the printed class
 * is a superclass with a type function.
 *
 * "isa" checks whether an object is of a given subclass type.
 * "isa_type" does the same, but gets passed the value of the type field
 * of the subclass as a function argument and the type of this field
 * as a template argument.
 * "as" casts an object to a given subclass type, erroring out
 * if the object is not of the given type.
 *
 * If the input is an invalid object, then these methods raise
 * an exception.
 * If checked bindings are being generated,
 * then an invalid boolean or object is returned instead.
 */
void plain_csharp_generator::impl_printer::print_downcast() {
  const char *csharpname = csharpstring.c_str();

  if (!clazz.fn_type)
    return;

  auto fn_name = clazz.fn_type->getNameAsString();
  auto enum_type = generator.type_printer()->isl_enum_type(
      -1, clazz.fn_type->getReturnType());

  osprintf(os, "public bool isa(%s subtype) {\n", enum_type.c_str());
  print_check_ptr("DangerousGetHandle()");
  osprintf(os, "  return Interop.%s(DangerousGetHandle()) == subtype;\n", fn_name.c_str());
  osprintf(os, "}\n");
}

/* Print the implementation of the ctx method.
 */
void plain_csharp_generator::impl_printer::print_ctx() {
  const char *name = clazz.name.c_str();
  const char *csharpname = csharpstring.c_str();
  std::string ns = generator.isl_namespace();

  osprintf(os, "\n");
  osprintf(os, "public ctx ctx() {\n");
  osprintf(os, "  return new ctx(Interop.%s_get_ctx(ptr));\n", name);
  osprintf(os, "}\n");
}

/* Print a separator between groups of method definitions.
 *
 * No additional separator is required between method definitions.
 */
void plain_csharp_generator::impl_printer::print_method_separator() {}

/* Print the implementations of the methods needed for the persistent callbacks
 * of the class.
 */
void plain_csharp_generator::impl_printer::print_persistent_callbacks() {
  const char *csharpname = csharpstring.c_str();
  string classname = type2csharp(clazz);

  if (!clazz.has_persistent_callbacks())
    return;

  osprintf(os, "\n");
  osprintf(os, "public %s copy_callbacks(%s obj)\n", csharpname, csharpname);
  osprintf(os, "{\n");
  for (const auto &callback : clazz.persistent_callbacks) {
    string callback_name = clazz.persistent_callback_name(callback);

    osprintf(os, "  %s_data = obj.%s_data;\n", callback_name.c_str(),
             callback_name.c_str());
  }
  osprintf(os, "  return this;\n");
  osprintf(os, "}\n");

  for (const auto &callback : clazz.persistent_callbacks)
    print_set_persistent_callback(CSharpMethod(clazz, callback));
}

/* Print a definition for the "get" method "fd" in class "clazz",
 * using a name that includes the "get_" prefix, to "os".
 *
 * This definition simply calls the variant without the "get_" prefix and
 * returns its result.
 * Note that static methods are not considered to be "get" methods.
 */
void plain_csharp_generator::impl_printer::print_get_method(FunctionDecl *fd) {
  string get_name = clazz.base_method_name(fd);
  string name = clazz.method_name(fd);
  int num_params = fd->getNumParams();

  osprintf(os, "\n");
  print_full_method_header(CSharpMethod(clazz, fd, get_name));
  osprintf(os, "{\n");
  osprintf(os, "  return %s(", name.c_str());
  for (int i = 1; i < num_params; ++i) {
    ParmVarDecl *param = fd->getParamDecl(i);

    if (i != 1)
      osprintf(os, ", ");
    osprintf(os, "%s", param->getName().str().c_str());
  }
  osprintf(os, ");\n");
  osprintf(os, "}\n");
}

/* Print code that checks that all isl object arguments to "method" are valid
 * (not NULL) and throws an exception if they are not.
 *
 * If checked bindings are being generated,
 * then no such check is performed.
 */
void plain_csharp_generator::impl_printer::print_argument_validity_check(
    const CSharpMethod &method) {
  int n;
  bool first = true;

  if (generator.checked)
    return;

  n = method.num_params();
  for (int i = 0; i < n; ++i) {
    bool is_this;
    ParmVarDecl *param = method.fd->getParamDecl(i);
    string name = param->getNameAsString();
    if (name == "params") {
      name += "_";
    }
    const char *name_str = name.c_str();
    QualType type = param->getOriginalType();

    is_this = i == 0 && method.kind == CSharpMethod::Kind::member_method;
    if (!is_this && (is_isl_ctx(type) || !is_isl_type(type)))
      continue;

    if (first)
      osprintf(os, "  if (");
    else
      osprintf(os, " || ");

    if (is_this)
      osprintf(os, "IsInvalid");
    else
      osprintf(os, "%s.IsInvalid", name_str);

    first = false;
  }
  if (first)
    return;
  osprintf(os, ") {\n");
  print_throw_NULL_input(os);
  osprintf(os, "\n  }\n");
}

/* Print code for saving a copy of "ctx" in a "saved_ctx" variable.
 */
void plain_csharp_generator::impl_printer::print_save_ctx(
    const std::string &ctx) {
  os << "  var saved_ctx = " << ctx << ";\n";
}

/* Print code for saving a copy of the isl::ctx available at the start
 * of the method "method" in a "saved_ctx" variable,
 * for use in exception handling.
 *
 * If checked bindings are being generated,
 * then the "saved_ctx" variable is not needed.
 * If "method" is a member function, then obtain the isl_ctx from
 * the "this" object.
 * If the first argument of the method is an isl::ctx, then use that one.
 * Otherwise, save a copy of the isl::ctx associated to the first argument
 * of isl object type.
 */
void plain_csharp_generator::impl_printer::print_save_ctx(
    const CSharpMethod &method) {
  int n;
  ParmVarDecl *param = method.fd->getParamDecl(0);
  QualType type = param->getOriginalType();

  if (generator.checked)
    return;
  if (method.kind == CSharpMethod::Kind::member_method)
    return print_save_ctx("ctx()");
  if (is_isl_ctx(type))
    return print_save_ctx(param->getName().str());
  n = method.num_params();
  for (int i = 0; i < n; ++i) {
    ParmVarDecl *param = method.fd->getParamDecl(i);
    QualType type = param->getOriginalType();

    if (!is_isl_type(type))
      continue;
    print_save_ctx(param->getName().str() + ".ctx()");
    return;
  }
}

/* Print code to make isl not print an error message when an error occurs
 * within the current scope (if exceptions are available),
 * since the error message will be included in the exception.
 * If exceptions are not available, then exception::on_error
 * is set to ISL_ON_ERROR_ABORT and isl is therefore made to abort instead.
 *
 * If checked bindings are being generated,
 * then leave it to the user to decide what isl should do on error.
 * Otherwise, assume that a valid isl::ctx is available
 * in the "saved_ctx" variable,
 * e.g., through a prior call to print_save_ctx.
 */
void plain_csharp_generator::impl_printer::print_on_error_continue() {
  if (generator.checked)
    return;
  osprintf(os,
           "  var saved_on_error = new options_scoped_set_on_error(saved_ctx, "
           "OnError.CONTINUE);\n");
}

/* Print code to "os" that checks whether any of the persistent callbacks
 * of the class of "method" is set and if it failed with an exception.
 * If so, the "eptr" in the corresponding data structure contains the exception
 * that was caught and that needs to be rethrown.
 * This field is cleared because the callback and its data may get reused.
 *
 * The check only needs to be generated for member methods since
 * an object is needed for any of the persistent callbacks to be set.
 */
static void print_persistent_callback_exceptional_execution_check(
    ostream &os, const CSharpMethod &method) {
  if (method.kind != CSharpMethod::Kind::member_method)
    return;

  for (const auto &pcb : method.clazz.persistent_callbacks) {
    auto callback_name = method.clazz.persistent_callback_name(pcb);

    osprintf(os, "  if (%s_data is not null && %s_data.eptr is not null) {\n",
             callback_name.c_str(), callback_name.c_str());
    osprintf(os, "    var eptr = %s_data.eptr!;\n", callback_name.c_str());
    osprintf(os, "    %s_data.eptr = null;\n", callback_name.c_str());
    osprintf(os, "    throw eptr;\n");
    osprintf(os, "  }\n");
  }
}

/* Print code that checks whether the execution of the core of "method"
 * was successful.
 *
 * If checked bindings are being generated,
 * then no checks are performed.
 *
 * Otherwise, first check if any of the callbacks failed with
 * an exception.  If so, the "eptr" in the corresponding data structure
 * contains the exception that was caught and that needs to be rethrown.
 * Then check if the function call failed in any other way and throw
 * the appropriate exception.
 * In particular, if the return type is isl_stat, isl_bool or isl_size,
 * then a negative value indicates a failure.  If the return type
 * is an isl type, then a NULL value indicates a failure.
 * Assume print_save_ctx has made sure that a valid isl::ctx
 * is available in the "ctx" variable.
 */
void plain_csharp_generator::impl_printer::print_exceptional_execution_check(
    const CSharpMethod &method) {
  bool check_null, check_neg;
  QualType return_type = method.fd->getReturnType();

  if (generator.checked)
    return;

  print_persistent_callback_exceptional_execution_check(os, method);

  for (const auto &callback : method.callbacks) {
    std::string name;

    name = callback->getName().str();
    osprintf(os, "  if (%s_data.eptr is not null) {\n", name.c_str());
    osprintf(os, "    throw %s_data.eptr;\n", name.c_str());
    osprintf(os, "  }\n");
  }

  check_neg = is_isl_neg_error(return_type);
  check_null = is_isl_type(return_type);
  if (!check_null && !check_neg)
    return;

  if (check_neg)
    osprintf(os, "  if (res < 0) {\n");
  else
    osprintf(os, "  if (res == IntPtr.Zero) {\n");
  print_throw_last_error(os);
  osprintf(os, "  }\n");
}

/* Return a pointer to the appropriate type printer,
 * i.e., the regular type printer or the checked type printer
 * depending on the setting of this->checked.
 */
std::unique_ptr<csharp_type_printer> plain_csharp_generator::type_printer() {
  csharp_type_printer *printer;

  if (checked)
    printer = new checked_csharp_type_printer();
  else
    printer = new csharp_type_printer();

  return std::unique_ptr<csharp_type_printer>(printer);
}

/* Return the C++ return type of the method "method".
 *
 * Use the appropriate type printer.
 */
std::string
plain_csharp_generator::get_return_type(const CSharpMethod &method) {
  return type_printer()->return_type(method);
}

/* Given a method "method" for setting a persistent callback of its class,
 * print the implementations of the methods needed for that callback.
 *
 * In particular, print
 * - the implementation of a static inline method
 *   for use as the C callback function
 * - the definition of a private method for setting the callback function
 * - the public method for constructing a new object with the callback set.
 */
void plain_csharp_generator::impl_printer::print_set_persistent_callback(
    const CSharpMethod &method) {
  string fullname = method.fd->getName().str();
  ParmVarDecl *param = persistent_callback_arg(method.fd);
  string pname;
  string callback_name = clazz.persistent_callback_name(method.fd);
  string callback_prototype_name = callback_name + "_prototype";

  osprintf(os, "\n");
  print_persistent_callback_prototype(method.fd);
  osprintf(os, "\n");
  osprintf(os, "{\n");
  print_callback_body(2, param, callback_name);
  osprintf(os, "}\n\n");

  pname = param->getName().str();
  print_persistent_callback_setter_prototype(method.fd);
  osprintf(os, "\n");
  osprintf(os, "{\n");
  print_check_ptr_start("handle");
  osprintf(os, "  %s_data = new();\n", callback_name.c_str());
  osprintf(os, "  %s_data.func = %s;\n", callback_name.c_str(), pname.c_str());
  osprintf(os, "  handle = Interop.%s(handle, %s, IntPtr.Zero);\n",
           fullname.c_str(), callback_prototype_name.c_str(),
           callback_name.c_str());
  print_check_ptr_end("handle");
  osprintf(os, "}\n\n");

  print_full_method_header(method);
  osprintf(os, "{\n");
  osprintf(os, "  this.set_%s_data(%s);\n", callback_name.c_str(),
           pname.c_str());
  osprintf(os, "  return this;\n");
  osprintf(os, "}\n");
}

/* Print the return statement of the C++ method "method".
 *
 * The result of the corresponding isl function is returned as a new
 * object if the underlying isl function returns an isl_* ptr, as a bool
 * if the isl function returns an isl_bool, as void if the isl functions
 * returns an isl_stat,
 * as std::string if the isl function returns 'const char *', and as
 * unmodified return value otherwise.
 * If checked C++ bindings are being generated,
 * then an isl_bool return type is transformed into a boolean and
 * an isl_stat into a stat since no exceptions can be generated
 * on negative results from the isl function.
 * If the method returns a new instance of the same object type and
 * if the class has any persistent callbacks, then the data
 * for these callbacks are copied from the original to the new object.
 * If "clazz" is a subclass that is based on a type function and
 * if the return type corresponds to the superclass data type,
 * then it is replaced by the subclass data type.
 */
void plain_csharp_generator::impl_printer::print_method_return(
    const CSharpMethod &method) {
  QualType return_type = method.fd->getReturnType();
  string rettype_str = generator.get_return_type(method);
  bool returns_super = method.is_subclass_mutator();

  if (is_isl_type(return_type) ||
      (generator.checked && is_isl_neg_error(return_type))) {
    osprintf(os, "return new %s(res)", rettype_str.c_str());
    if (is_mutator(clazz, method.fd) && clazz.has_persistent_callbacks())
      osprintf(os, ".copy_callbacks(this)");
    osprintf(os, ";\n");
  } else if (is_isl_stat(return_type)) {
    osprintf(os, "  return;\n");
  } else if (is_isl_bool(return_type)) {
    osprintf(os, "  return res == isl_bool.True;\n");
  } else if (is_string(return_type)) {
    if (gives(method.fd)) {
      osprintf(os, "  return res;\n");
    } else {
      osprintf(os, "  return Marshal.PtrToStringAnsi(res);\n");
    }
  } else if (is_void(return_type)) {
  } else {
    osprintf(os, "  return res;\n");
  }
}

/* Print the header for "method", including the terminating semicolon
 * in case of a declaration and a newline.
 *
 * Use the appropriate type printer to print argument and return types.
 */
void plain_csharp_generator::plain_printer::print_full_method_header(
    const CSharpMethod &method) {
  auto type_printer = generator.type_printer();

  print_method_header(method, *type_printer);

  if (declarations)
    osprintf(os, ";");
  osprintf(os, "\n");
}

/* Generate the list of argument types for a callback function of
 * type "type".  If "csharp" is set, then generate the C++ type list, otherwise
 * the C type list.
 *
 * Use the appropriate type printer.
 * For the plain C++ interface, the argument position is irrelevant,
 * so simply pass in -1.
 */
string plain_csharp_generator::generate_callback_args(QualType type,
                                                      bool csharp,
                                                      bool with_arg) {
  return type_printer()->generate_callback_args(-1, type, csharp, with_arg);
}

/* Generate the full csharp type of a callback function of type "type".
 *
 * Use the appropriate type printer.
 * For the plain C++ interface, the argument position is irrelevant,
 * so simply pass in -1.
 */
string plain_csharp_generator::generate_callback_type(QualType type,
                                                      bool csharp) {
  return type_printer()->generate_callback_type(-1, type, csharp);
}

/* Print the call to the C++ callback function "call",
 * with the given indentation, wrapped
 * for use inside the lambda function that is used as the C callback function,
 * in the case where checked C++ bindings are being generated.
 *
 * In particular, print
 *
 *        auto ret = @call@;
 *        return ret.release();
 */
void plain_csharp_generator::impl_printer::print_wrapped_call_checked(
    int indent, const string &call) {
  osprintf(os, indent, "auto ret = %s;\n", call.c_str());
  osprintf(os, indent, "return ret.release();\n");
}

/* Print the call to the C++ callback function "call",
 * with the given indentation and with return type "rtype", wrapped
 * for use inside the lambda function that is used as the C callback function.
 *
 * In particular, print
 *
 *        ISL_csharp_TRY {
 *          @call@;
 *          return isl_stat_ok;
 *        } ISL_csharp_CATCH_ALL {
 *          data->eptr = std::current_exception();
 *          return isl_stat_error;
 *        }
 * or
 *        ISL_csharp_TRY {
 *          auto ret = @call@;
 *          return ret ? isl_bool_true : isl_bool_false;
 *        } ISL_csharp_CATCH_ALL {
 *          data->eptr = std::current_exception();
 *          return isl_bool_error;
 *        }
 * or
 *        ISL_csharp_TRY {
 *          auto ret = @call@;
 *          return ret.release();
 *        } ISL_csharp_CATCH_ALL {
 *          data->eptr = std::current_exception();
 *          return NULL;
 *        }
 *
 * depending on the return type.
 *
 * where ISL_csharp_TRY is defined to "try" and ISL_csharp_CATCH_ALL to "catch
 * (...)" (if exceptions are available).
 *
 * If checked C++ bindings are being generated, then
 * the call is wrapped differently.
 */
void plain_csharp_generator::impl_printer::print_wrapped_call(
    int indent, const string &call, QualType rtype) {
  if (generator.checked)
    return print_wrapped_call_checked(indent, call);

  osprintf(os, indent, "try {\n");
  if (is_isl_stat(rtype))
    osprintf(os, indent, "  %s;\n", call.c_str());
  else
    osprintf(os, indent, "  var ret = %s;\n", call.c_str());
  if (is_isl_stat(rtype))
    osprintf(os, indent, "  return isl_stat.Ok;\n");
  else if (is_isl_bool(rtype))
    osprintf(os, indent, "  return ret ? isl_bool.True : isl_bool.False;\n");
  else
    osprintf(os, indent, "  return ret.DangerousGetHandle();\n");
  osprintf(os, indent, "} catch (Exception e) {\n");
  osprintf(os, indent, "  throw e;\n");
  if (is_isl_stat(rtype))
    osprintf(os, indent, "  return isl_stat.Error;\n");
  else if (is_isl_bool(rtype))
    osprintf(os, indent, "  return isl_bool.Error;\n");
  else
    osprintf(os, indent, "  return IntPtr.Zero;\n");
  osprintf(os, indent, "}\n");
}

/* Print the declaration for a "prefix"_data data structure
 * that can be used for passing to a C callback function
 * containing a copy of the C++ callback function "param",
 * along with an std::exception_ptr that is used to store any
 * exceptions thrown in the C++ callback.
 *
 * If the C callback is of the form
 *
 *      isl_stat (*fn)(__isl_take isl_map *map, void *user)
 *
 * then the following declaration is printed:
 *
 *      struct <prefix>_data {
 *        std::function<stat(map)> func;
 *        std::exception_ptr eptr;
 *      }
 *
 * (without a newline or a semicolon).
 *
 * The std::exception_ptr object is not added to "prefix"_data
 * if checked C++ bindings are being generated.
 */
void plain_csharp_generator::plain_printer::print_callback_data_decl(
    ParmVarDecl *param, const string &prefix) {
  string csharp_args;

  csharp_args = generator.generate_callback_type(param->getType(), true);

  osprintf(os, " class %s_tuple {\n", prefix.c_str());
  osprintf(os, " public %s func;\n", csharp_args.c_str());
  if (!generator.checked)
    osprintf(os, " public Exception? eptr;\n");
  osprintf(os, "}");
}

/* Given a group of methods with the same name,
 * should extra methods be added that take as arguments
 * those types that can be converted to the original argument type
 * through a unary constructor?
 *
 * Note that even if this method returns true,
 * the extra methods are only printed by the caller
 * if exactly one of the methods in the group was originally defined
 * in the printed class.
 * Signal that they should be printed if the group contains
 * both methods originally defined in the printed class and
 * methods that have been copied from an ancestor
 * by checking whether there are at least two methods in the group.
 */
bool plain_csharp_generator::plain_printer::want_descendent_overloads(
    const function_set &methods) {
  return methods.size() > 1;
}

/* Print the header of the constructor for the "id" class
 * that takes a user object.
 *
 * The user object is taken as a std::any.
 */
void plain_csharp_generator::plain_printer::print_id_constructor_user_header() {
  if (declarations)
    os << "  inline explicit ";
  else
    os << "public ";
  os << "id("
     << "ctx ctx, "
     << "string str)";
  if (declarations)
    os << ";";
  os << "\n";
}

/* Print the header of the "id" method
 * for retrieving the user object associated to the identifier.
 * If "optional" is set, the method returns a std::optional user object.
 * The returned object is of a type specified by template parameter T.
 */
void plain_csharp_generator::plain_printer::print_id_user_header(
    bool optional) {
  auto indent = declarations ? "  " : "";
  os << indent << "template <class T>\n";
  os << indent << (optional ? "std::optional<T> " : "T ");
  if (!declarations)
    os << "id::";
  os << (optional ? "try_" : "");
  os << "user() const";
  if (declarations)
    os << ";";
  os << "\n";
}

/* Perform printing by "fn" in a context that only gets compiled
 * by C++17 compilers.
 */
static void on_cplusplus17(ostream &os, const std::function<void(void)> &fn) {
  os << "#if __cplusplus >= 201703L\n";
  fn();
  os << "#endif\n";
}

/* Print declarations or definitions of the special methods of the "id" class
 * that are not automatically derived from the C interface.
 *
 * In particular, print a constructor that takes a user pointer
 * as well as methods for retrieving this user pointer.
 *
 * These methods require C++17 features.
 */
void plain_csharp_generator::plain_printer::print_special_id() {
  os << "\n";
  // on_cplusplus17(os, [this]() {
  // print_id_constructor_user();
  // print_id_user(true);
  // print_id_user(false);
  // });
}

/* Print declarations or definitions of any special methods of this class
 * not automatically derived from the C interface.
 *
 * In particular, print special methods for the "id" class.
 */
void plain_csharp_generator::plain_printer::print_special() {
  if (clazz.name == "isl_id")
    print_special_id();
}

/* Print declarations or definitions of the public methods.
 */
void plain_csharp_generator::plain_printer::print_public_methods() {
  // NOTE not allow empty ctor and copy ctor
  // print_public_constructors();
  print_constructors();
  // print_copy_assignment();
  print_destructor();
  print_ptr();
  print_downcast();
  // print_ctx();
  print_method_separator();
  print_persistent_callbacks();
  print_methods();
  print_set_enums();
  print_special();
}

/* Print the body of C function callback with the given indentation
 * that can be use as an argument to "param" for marshalling
 * the corresponding C++ callback.
 * The data structure that contains the C++ callback is of type
 * "prefix"_data.
 *
 * For a callback of the form
 *
 *      isl_stat (*fn)(__isl_take isl_map *map, void *user)
 *
 * the following code is generated:
 *
 *        auto *data = static_cast<struct <prefix>_data *>(arg_1);
 *        ISL_csharp_TRY {
 *          stat ret = (data->func)(manage(arg_0));
 *          return isl_stat_ok;
 *        } ISL_csharp_CATCH_ALL {
 *          data->eptr = std::current_exception();
 *          return isl_stat_error;
 *        }
 *
 * If checked C++ bindings are being generated, then
 * generate the following code:
 *
 *        auto *data = static_cast<struct <prefix>_data *>(arg_1);
 *        stat ret = (data->func)(manage(arg_0));
 *        return isl_stat(ret);
 */
void plain_csharp_generator::impl_printer::print_callback_body(
    int indent, ParmVarDecl *param, const string &prefix) {
  QualType ptype, rtype;
  string call, last_idx;
  const FunctionProtoType *callback;
  int num_params;

  ptype = param->getType();

  callback = extract_prototype(ptype);
  rtype = callback->getReturnType();
  num_params = callback->getNumArgs();

  last_idx = ::to_string(num_params - 1);

  std::stringstream callos;
  osprintf(callos, "%s_data.func(", prefix.c_str());
  for (long i = 0; i < num_params - 1; i++) {
    if (!generator.callback_takes_argument(param, i))
      osprintf(callos, "");
    if (is_isl_type(callback->getParamType(i))) {
      callos << "new ";
    }
    osprintf(callos, ("(arg_" + ::to_string(i) + ")").c_str());
    if (i != num_params - 2)
      osprintf(callos, ", ");
  }
  osprintf(callos, ")");

  // osprintf(os, indent, "auto *data = static_cast<struct %s_data
  // *>(arg_%s);\n",
  //          prefix.c_str(), last_idx.c_str());
  print_wrapped_call(indent, callos.str(), rtype);
}

/* Print the local variables that are needed for a callback argument,
 * in particular, print a lambda function that wraps the callback and
 * a pointer to the actual C++ callback function.
 *
 * For a callback of the form
 *
 *      isl_stat (*fn)(__isl_take isl_map *map, void *user)
 *
 * the following lambda function is generated:
 *
 *      auto fn_lambda = [](isl_map *arg_0, void *arg_1) -> isl_stat {
 *        auto *data = static_cast<struct fn_data *>(arg_1);
 *        try {
 *          stat ret = (data->func)(manage(arg_0));
 *          return isl_stat_ok;
 *        } catch (...) {
 *          data->eptr = std::current_exception();
 *          return isl_stat_error;
 *        }
 *      };
 *
 * A copy of the std::function C++ callback function is stored in
 * a fn_data data structure for passing to the C callback function,
 * along with an std::exception_ptr that is used to store any
 * exceptions thrown in the C++ callback.
 *
 *      struct fn_data {
 *        std::function<stat(map)> func;
 *        std::exception_ptr eptr;
 *      } fn_data = { fn };
 *
 * This std::function object represents the actual user
 * callback function together with the locally captured state at the caller.
 *
 * The lambda function is expected to be used as a C callback function
 * where the lambda itself is provided as the function pointer and
 * where the user void pointer is a pointer to fn_data.
 * The std::function object is extracted from the pointer to fn_data
 * inside the lambda function.
 *
 * The std::exception_ptr object is not added to fn_data
 * if checked C++ bindings are being generated.
 * The body of the generated lambda function then is as follows:
 *
 *        stat ret = (data->func)(manage(arg_0));
 *        return isl_stat(ret);
 *
 * If the C callback does not take its arguments, then
 * manage_copy is used instead of manage.
 */
void plain_csharp_generator::impl_printer::print_callback_local(
    ParmVarDecl *param) {
  string pname;
  QualType ptype, rtype;
  string c_args, csharp_args, rettype;
  const FunctionProtoType *callback;

  pname = param->getName().str();
  ptype = param->getType();

  c_args = generator.generate_callback_args(ptype, false, true);

  callback = extract_prototype(ptype);
  rtype = callback->getReturnType();
  rettype = rtype.getAsString();
  string c_type = generator.generate_callback_type(ptype, false);

  osprintf(os, " (%s func, ",
           generator.generate_callback_type(ptype, true).c_str());
  if (!generator.checked)
    osprintf(os, " Exception? eptr");
  osprintf(os, ")");
  // print_callback_data_decl(param, pname);
  osprintf(os, " %s_data = (func: %s, eptr: null);\n", pname.c_str(),
           pname.c_str(), pname.c_str());
  osprintf(os, "%s %s_lambda = (%s) => {\n", c_type.c_str(), pname.c_str(),
           c_args.c_str());
  print_callback_body(4, param, pname);
  osprintf(os, "  };\n");
}

/* Return the C++ counterpart to the isl_bool type.
 *
 * For the checked C++ bindings this is "boolean".
 */
std::string checked_csharp_type_printer::isl_bool() const { return "boolean"; }

/* Return the C++ counterpart to the isl_bool type.
 *
 * Use the appropriate type printer.
 */
string plain_csharp_generator::isl_bool2csharp() {
  return type_printer()->isl_bool();
}

/* Return the C++ counterpart to the isl_stat type.
 *
 * For the checked C++ bindings this is "stat".
 */
string checked_csharp_type_printer::isl_stat() const { return "stat"; }

/* Return the C++ counterpart to the isl_size type.
 *
 * For the checked C++ bindings this is "class size".
 */
string checked_csharp_type_printer::isl_size() const { return "class size"; }

/* Return the namespace of the generated C++ bindings.
 *
 * For the checked C++ bindings this is "isl::checked::".
 */
std::string checked_csharp_type_printer::isl_namespace() const {
  return "isl::checked::";
}

/* Return the namespace of the generated C++ bindings.
 *
 * Use the appropriate type printer.
 */
string plain_csharp_generator::isl_namespace() {
  return type_printer()->isl_namespace();
}

/* Translate parameter or return type "type" to its C++ name counterpart.
 *
 * Use the appropriate type printer.
 * For the plain C++ interface, the argument position is irrelevant,
 * so simply pass in -1.
 */
string plain_csharp_generator::param2csharp(QualType type) {
  return type_printer()->param(-1, type);
}
