#ifndef ISL_INTERFACE_csharp_H
#define ISL_INTERFACE_csharp_H

#include <iostream>
#include <string>
#include <vector>

#include "generator.h"

/* A generated C++ method derived from an isl function.
 *
 * "clazz" is the class to which the method belongs.
 * "fd" is the original isl function.
 * "name" is the name of the method, which may be different
 * from the default name derived from "fd".
 * "kind" is the type of the method.
 * "callbacks" stores the callback arguments.
 */
struct CSharpMethod {
	enum Kind {
		static_method,
		member_method,
		constructor,
	};

	struct csharp_list_combiner;
	static csharp_list_combiner print_combiner(std::ostream &os);
	static csharp_list_combiner empty_combiner();

	CSharpMethod(const isl_class &clazz, FunctionDecl *fd,
		const std::string &name);
	CSharpMethod(const isl_class &clazz, FunctionDecl *fd);

	int c_num_params() const;
	virtual int num_params() const;
	virtual bool param_needs_copy(int pos) const;
	virtual clang::ParmVarDecl *get_param(int pos) const;
	virtual void print_param_use(ostream &os, int pos) const;
	bool is_subclass_mutator() const;
	static void on_arg_list(int start, int end,
		const csharp_list_combiner &combiner,
		const std::function<bool(int i)> &on_arg_skip_next);
	static void print_arg_list(std::ostream &os, int start, int end,
		const std::function<bool(int i)> &print_arg_skip_next);
	void on_fd_arg_list(int start, int end,
		const csharp_list_combiner &combiner,
		const std::function<void(int i, int arg)> &on_arg) const;
	void print_fd_arg_list(std::ostream &os, int start, int end,
		const std::function<void(int i, int arg)> &print_arg) const;
	void on_csharp_arg_list(const csharp_list_combiner &combiner,
		const std::function<void(int i, int arg)> &on_arg) const;
	void on_csharp_arg_list(
		const std::function<void(int i, int arg)> &on_arg) const;
	void print_csharp_arg_list(std::ostream &os,
		const std::function<void(int i, int arg)> &print_arg) const;

	const isl_class &clazz;
	FunctionDecl *const fd;
	const std::string name;
	const enum Kind kind;
	const std::vector<ParmVarDecl *> callbacks;
};

/* A data structure expressing how Method::on_arg_list should combine
 * the arguments.
 *
 * In particular, "before" is called before any argument is handled;
 * "between" is called between two arguments and
 * "after" is called after all arguments have been handled.
 */
struct CSharpMethod::csharp_list_combiner {
	const std::function<void()> before;
	const std::function<void()> between;
	const std::function<void()> after;
};

/* A method that does not require its isl type parameters to be a copy.
 */
struct CSharpNoCopyMethod : CSharpMethod {
	CSharpNoCopyMethod(const CSharpMethod &method) : CSharpMethod(method) {}

	virtual bool param_needs_copy(int pos) const override;
};

/* A generated method that performs one or more argument conversions and
 * then calls the original method.
 *
 * A ConversionMethod inherits from a NoCopyMethod, because
 * unlike methods that call an isl C function,
 * a conversion method never calls release() on an isl type argument,
 * so they can all be passed as const references.
 *
 * "this_type" is the name of the type to which "this" should be converted
 * (if different from clazz.name).
 * "get_param_fn" returns the method argument at position "pos".
 */
struct CSharpConversionMethod : CSharpNoCopyMethod {
	CSharpConversionMethod(const CSharpMethod &method, const std::string &this_type,
		const std::function<clang::ParmVarDecl *(int pos)> &get_param);
	CSharpConversionMethod(const CSharpMethod &method, const std::string &this_type);
	CSharpConversionMethod(const CSharpMethod &method,
		const std::function<clang::ParmVarDecl *(int pos)> &get_param);
	virtual clang::ParmVarDecl *get_param(int pos) const override;

	void print_call(std::ostream &os, const std::string &ns) const;

	const std::string this_type;
	const std::function<clang::ParmVarDecl *(int pos)> get_param_fn;
};

/* A specialized generated C++ method for setting an enum.
 *
 * "enum_name" is a string representation of the enum value
 * set by this method.
 */
struct CSharpEnumMethod : public CSharpMethod {
	CSharpEnumMethod(const isl_class &clazz, FunctionDecl *fd,
		const std::string &method_name, const std::string &enum_name);

	virtual int num_params() const override;
	virtual void print_param_use(ostream &os, int pos) const override;

	std::string enum_name;
};

/* A type printer for converting argument and return types,
 * as well as the class type,
 * to string representations of the corresponding types
 * in the C++ interface.
 */
struct csharp_type_printer {
	csharp_type_printer() {}

	virtual std::string isl_bool() const;
	virtual std::string isl_stat() const;
	virtual std::string isl_size() const;
	virtual std::string isl_namespace() const;
	virtual std::string class_type(const std::string &csharp_name) const;
	virtual std::string qualified(int arg, const std::string &csharp_type)
		const;
  std::string isl_enum_type(int arg, QualType type) const;
	std::string isl_type(int arg, QualType type) const;
	std::string generate_callback_args(int arg, QualType type, bool csharp, bool with_arg)
		const;
	std::string generate_callback_type(int arg, QualType type, bool csharp) const;
	std::string param(int arg, QualType type, bool csharp = true) const;
	std::string return_type(const CSharpMethod &method) const;
};

/* Generator for C++ bindings.
 */
class csharp_generator : public generator {
protected:
	struct class_printer;
public:
	csharp_generator(SourceManager &SM, set<RecordDecl *> &exported_types,
		set<FunctionDecl *> exported_functions,
		set<FunctionDecl *> functions);
	bool is_implicit_conversion(const CSharpMethod &cons);
private:
	void set_class_construction_types(isl_class &clazz);
	void set_construction_types();
	void copy_methods(isl_class &clazz, const std::string &name,
		const isl_class &super, const function_set &methods);
	void copy_super_methods(isl_class &clazz, const isl_class &super);
	void copy_super_methods(isl_class &clazz, set<string> &done);
	void copy_super_methods();
	bool is_subclass(QualType subclass_type, const isl_class &class_type);
public:
	static string type2csharp(const isl_class &clazz);
	static string type2csharp(string type_string);
};

/* A helper class for printing method declarations and definitions
 * of a class.
 *
 * "os" is the stream onto which the methods are printed.
 * "clazz" describes the methods of the class.
 * "csharpstring" is the C++ name of the class.
 * "generator" is the C++ interface generator printing the classes.
 * "declarations" is set if this object is used to print declarations.
 */
struct csharp_generator::class_printer {
	std::ostream &os;
	const isl_class &clazz;
	const std::string csharpstring;
	csharp_generator &generator;
	const bool declarations;

	class_printer(std::ostream &os, const isl_class &clazz,
			csharp_generator &generator, bool declarations);

	void print_constructors();
	void print_methods();
	bool next_variant(FunctionDecl *fd, std::vector<bool> &convert);
	void print_method_variants(FunctionDecl *fd, const std::string &name);
	virtual bool want_descendent_overloads(const function_set &methods) = 0;
	void print_descendent_overloads(FunctionDecl *fd,
		const std::string &name);
	void print_method_group(const function_set &methods,
		const std::string &name);
	virtual void print_method(const CSharpMethod &method) = 0;
	virtual void print_method(const CSharpConversionMethod &method) = 0;
	virtual void print_get_method(FunctionDecl *fd) = 0;
	void print_set_enums(FunctionDecl *fd);
	void print_set_enums();
	ParmVarDecl *get_param(FunctionDecl *fd, int pos,
		const std::vector<bool> &convert);
	void print_method_header(const CSharpMethod &method,
		const csharp_type_printer &type_printer);
};

#endif
