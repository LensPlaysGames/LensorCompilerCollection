#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendPluginRegistry.h>
#include <clang/Sema/Sema.h>
#include <iostream>
#include <mutex>
#include <ranges>

#define EXT_FORMAT "ext_format"

namespace match = clang::ast_matchers;

/// All functions to be typechecked.
struct FormatInfo {
  size_t format_str_index;
  size_t format_args_index;
};

/// All format functions.
std::mutex format_funcs_mutex;
std::map<const clang::FunctionDecl*, FormatInfo> format_functions;

/// Match callbacks.
struct FormatCheckCallback : public match::MatchFinder::MatchCallback {
  clang::CompilerInstance &ci;
  explicit FormatCheckCallback(clang::CompilerInstance &ci_) : ci(ci_) {}

  void run(const match::MatchFinder::MatchResult &result) override {
    auto func = result.Nodes.getNodeAs<clang::FunctionDecl>("func");
    auto call = result.Nodes.getNodeAs<clang::CallExpr>("call");
    auto& diags = ci.getDiagnostics();

    /// Ignore calls to functions not annotated with the EXT_FORMAT attribute.
    size_t format_string_index;
    size_t argument_index;
    {
      std::unique_lock lock{format_funcs_mutex};
      if (not format_functions.contains(func)) return;
      auto& format_info = format_functions.at(func);
      format_string_index = format_info.format_str_index;
      argument_index = format_info.format_args_index;
    }

    /// Get the format string.
    auto format = call->getArg(format_string_index)->IgnoreParenCasts();

    /// If the argument is a [const] char* rather than a string literal,
    /// issue a warning.
    auto lit = clang::dyn_cast<clang::StringLiteral>(format);
    if (not lit and format->getType()->isPointerType() and format->getType()->getPointeeType()->isCharType()) {
      diags.Report(format->getExprLoc(),
        diags.getCustomDiagID(clang::DiagnosticsEngine::Warning,
          "Format string is not a constant expression"));
      return;
    }

    /// Ignore arguments that are not string literals.
    if (not lit) return;

    /// Get the value of the format string.
    auto fmt = lit->getString();
    typecheck_format_string(fmt, call, argument_index);
  };

  /// Report an invalid format arg.
  void report(const clang::Expr* arg, std::string_view specifier, std::string_view note = "") {
    auto& diags = ci.getDiagnostics();

    /// Pretty print the argument name.
    std::string name = "'" + arg->getType().getAsString() + "'";
    if (arg->getType()->isTypedefNameType()) {
      name += " [aka '";
      name += arg->getType().getCanonicalType().getAsString();
      name += "']";
    }

    /// Report the error.
    diags.Report(arg->getExprLoc(),
      diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
        "Invalid argument type %0 for format specifier '%1'%2"))
          << std::vector{arg->getSourceRange()} << name << specifier << note;
  }

  /// Typecheck the format string.
  ///
  /// This only reports at most one error per format string, because once
  /// there is one error, we end up running into a lot of bogus errors.
  ///
  /// Supported format specifiers:
  ///   - %c: char
  ///   - %s: const char *, char *
  ///   - %S: span, string
  ///   - %C: A colour string. Only printed if colours are enabled.
  ///   - %d: int
  ///   - %i: int
  ///   - %u: unsigned
  ///   - %D: int64_t
  ///   - %I: int64_t
  ///   - %U: uint64_t
  ///   - %Z: size_t
  ///   - %zu: size_t
  ///   - %x: hexadecimal (32-bit)
  ///   - %X: hexadecimal (64-bit)
  ///   - %p: void *
  ///   - %b: bool
  ///   - %T: Type *
  ///   - %F: Another format string + va_list. This format spec therefore takes *two* arguments.
  ///   - %%: A '%' character.
  ///   - %m: Reset colours if colours are enabled.
  ///   - %X, where X is ESCAPE: A literal escape character if you need that for some ungodly reason.
  ///   - %XX, where X is in [0-9]: "\033[XXm" if colours are enabled, and "" otherwise.
  ///   - %BXX, where X is in [0-9]: "\033[1;XXm" if colours are enabled, and "" otherwise.
  void typecheck_format_string(std::string_view fmt, const clang::CallExpr* call, size_t arg_index) {
    auto& diags = ci.getDiagnostics();
    auto& ctx = ci.getASTContext();

    /// Main formatting loop.
    for (;;) {
      /// Find the start of the next format specifier or ANSI escape code.
      auto pos = fmt.find_first_of("%\033");

      /// No more format specifiers or escape codes. Check for excess arguments.
      if (pos == std::string_view::npos) {
        if (arg_index < call->getNumArgs()) {
          diags.Report(call->getArg(arg_index)->getBeginLoc(),
            diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
              "Excess arguments to format string"))
               << std::vector{clang::SourceRange{
                    call->getArg(arg_index)->getSourceRange().getBegin(),
                    call->getArg(call->getNumArgs() - 1)->getSourceRange().getEnd()
                  }};
        }
        return;
      }

      /// Skip everything up to the next format specifier or escape code.
      fmt.remove_prefix(pos);

      /// Skip ANSI escape codes.
      if (fmt[0] == '\033') {
        /// Find the end of the escape code.
        auto end = fmt.find('m');
        bool found = end != std::string_view::npos;
        if (!found) {
          diags.Report(call->getBeginLoc(),
            diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
              "Unterminated ANSI escape code in format string"));
          return;
        }

        /// Move on to the next format specifier or just skip the \033
        /// if we couldn’t find a matching 'm'.
        fmt.remove_prefix(found ? end + 1 : 1);
        continue;
      }

      /// Check if we still have enough arguments and extract the next argument.
#define GET_ARG(type)                                          \
  if (arg_index >= call->getNumArgs()) {                       \
    diags.Report(call->getBeginLoc(),                          \
      diags.getCustomDiagID(clang::DiagnosticsEngine::Error,   \
        "Not enough arguments to format string"));             \
    return;                                                    \
  }                                                            \
  auto arg = call->getArg(arg_index++);                        \
  auto type = arg->getType()

      /// Yeet '%'.
      fmt.remove_prefix(1);

      /// Handle the format specifier.
      bool bold = false;
      switch (auto spec = fmt[0]; fmt.remove_prefix(1), spec) {
        case 'c': {
          GET_ARG(type);
          if (!type->isCharType()) return report(arg, "%c");
        } break;

        case 's': {
          GET_ARG(type);
          if (!type->isPointerType() || !type->getPointeeType()->isCharType()) return report(arg, "%s");
        } break;

        case 'S': {
          GET_ARG(qtype);
          auto type = qtype.IgnoreParens()->getUnqualifiedDesugaredType();

          /// Argument must be a struct containing a pointer to a char and a size_t.
          /// Make sure it’s a record.
          if (!type->isRecordType()) return report(arg, "%S");

          /// Make sure it’s a struct.
          auto struct_decl = type->getAsRecordDecl();
          if (!struct_decl->isStruct()) return report(arg, "%S");

          /// Make sure it has two fields.
          auto fields = struct_decl->fields();
          auto count = std::distance(fields.begin(), fields.end());
          if (count != 2) return report(arg, "%S");

          /// Typecheck the fields.
          auto data_field = fields.begin()->getType();
          if (!data_field->isPointerType() || !data_field->getPointeeType()->isCharType())
            return report(arg, "%S");

          auto size_field = std::next(fields.begin())->getType();
          if (!size_field->isUnsignedIntegerType() || ci.getASTContext().getTypeSize(size_field) != sizeof(size_t) * 8)
            return report(arg, "%S");
        } break;

        case 'C': {
          GET_ARG(type);
          if (!type->isPointerType() || !type->getPointeeType()->isCharType()) return report(arg, "%C");
        } break;

        case 'd':
        case 'i': {
          GET_ARG(type);
          if (!type->isSignedIntegerType() || ci.getASTContext().getTypeSize(type) > 32)
            return report(arg, std::string{"%"} + spec);
        } break;

        case 'u': {
          GET_ARG(type);
          if (!type->isUnsignedIntegerType() || ci.getASTContext().getTypeSize(type) > 32)
            return report(arg, std::string{"%"} + spec);
        } break;

        case 'D':
        case 'I': {
          GET_ARG(type);
          if (!type->isSignedIntegerType() || ci.getASTContext().getTypeSize(type) != 64)
            return report(arg, std::string{"%"} + spec);
        } break;

        case 'U': {
          GET_ARG(type);
          if (!type->isUnsignedIntegerType() || ci.getASTContext().getTypeSize(type) != 64)
            return report(arg, std::string{"%"} + spec);
        } break;

        case 'z': {
          GET_ARG(type);

          /// Backwards compatibility with '%zu' because we were using that all over the place.
          if (fmt[0] != 'u') {
            diags.Report(arg->getExprLoc(),
              diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                "Invalid format specifier '%0' in format string"))
                  << std::string{"%z"} + (spec == 0 ? std::string{""} : std::string{spec});
            return;
          }

          fmt.remove_prefix(1);
          if (!type->isUnsignedIntegerType() || ci.getASTContext().getTypeSize(type) != sizeof(size_t) * CHAR_BIT)
            return report(arg, "%zu");
        } break;

        case 'Z': {
          GET_ARG(type);
          if (!type->isUnsignedIntegerType() || ci.getASTContext().getTypeSize(type) != sizeof(size_t) * CHAR_BIT)
            return report(arg, "%Z");
        } break;

        case 'x': {
          GET_ARG(type);
          if (!type->isIntegerType() || ci.getASTContext().getTypeSize(type) > sizeof(int32_t) * 8)
            return report(arg, "%x");
        } break;

        case 'X': {
          GET_ARG(type);
          if (!type->isIntegerType() || ci.getASTContext().getTypeSize(type) != sizeof(int64_t) * 8)
            return report(arg, "%X", " (Hint: %X is for 64-bit integers)");
        } break;

        case 'p': {
          GET_ARG(type);

          /// Make sure the argument is a pointer.
          if (!type->isPointerType()) return report(arg, "%p");
        } break;

        case 'b': {
          GET_ARG(type);
          /// Booleans are implicitly promoted to 'int' if they happen
          /// to be variadic arguments, so we need to remove any such
          /// conversions.
          auto base = arg->IgnoreParenImpCasts();
          if (!base->getType()->isBooleanType()) { return report(arg, "%b"); }
        } break;

        case 'T': {
          GET_ARG(type);

          /// Argument must be a pointer to a type whose name is 'Type'.
          if (!type->isPointerType() || type->getPointeeType().getUnqualifiedType().getAsString() != "Type")
            return report(arg, "%T");
        } break;

        case 'F': {
          GET_ARG(type);

          /// The first argument must be a '[const] char *'.
          if (!type->isPointerType() || !type->getPointeeType()->isCharType())
            return report(arg, "%F");

          /// After that, we need a 'va_list' argument.
          if (arg_index >= call->getNumArgs()) {
            diags.Report(call->getBeginLoc(),
              diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                "Not enough arguments to format string. Hint: %F requires two arguments "
                "(a '[const] char*' and a 'va_list')"));
            return;
          }

          /// The second argument must be a 'va_list'. The absurd amount of conversions below
          /// are due to the fact that a 'va_list' is typically something like '__va_list_tag[1]',
          /// but as an argument, this has already been decayed to a pointer, so we need to take
          /// that into account.
          auto va_arg = call->getArg(arg_index++);
          auto va_type = va_arg->getType()->getUnqualifiedDesugaredType();
          if (va_type->isPointerType() || va_type->isArrayType()) va_type = va_type->getPointeeOrArrayElementType();
          auto va_list_t = ci.getASTContext().getBuiltinVaListType()->getUnqualifiedDesugaredType();
          if (va_list_t->isPointerType() || va_list_t->isArrayType()) va_list_t = va_list_t->getPointeeOrArrayElementType();
          if (va_type != va_list_t) return report(va_arg, "%F");
        } break;

        case '%':
        case '\033':
        case 'm':
          break;

        case 'B':
          /// Next character must be a digit.
          bold = true;
          if (fmt[0] < '0' || fmt[0] > '9') {
            diags.Report(call->getBeginLoc(),
              diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                "Invalid format specifier '%0' in format string"))
                  << std::string{"%"} + (spec == 0 ? std::string{""} : std::string{spec});
            return;
          }
          fmt.remove_prefix(1);
          [[fallthrough]];

        case '0' ... '9':
          /// Colour codes are always 2 digits.
          if (fmt[0] < '0' || fmt[0] > '9') {
            diags.Report(call->getBeginLoc(),
              diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
                "Invalid format specifier '%0%1%2%3' in format string"))
                  << "%" << (bold ? "B" : "") << fmt[-1] << fmt[0];
            return;
          }

          /// Ignore this if colours are disabled.
          fmt.remove_prefix(1);
          break;

        default:
          diags.Report(call->getBeginLoc(),
            diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
              "Invalid format specifier '%0' in format string"))
                << std::string{"%"} + (spec == 0 ? std::string{""} : std::string{spec});
          return;
      }
    }
  }
};

/// Consumer.
struct FmtCheckConsumer : public clang::ASTConsumer {
  clang::CompilerInstance &CI;
  clang::ASTContext &ctx;
  explicit FmtCheckConsumer(clang::CompilerInstance &CI, clang::ASTContext& ctx2) : CI(CI), ctx(ctx2) {}

  void HandleTranslationUnit(clang::ASTContext &Context) override {
    /// Create a matcher.
    match::MatchFinder finder;

    /// Create a callback.
    FormatCheckCallback callback(CI);

    /// Add the callback to the matcher.
    finder.addMatcher(
      match::callExpr(
        match::callee(
          match::functionDecl().bind("func"))
      ).bind("call"),
    &callback);

    /// Run the matcher.
    finder.matchAST(Context);
  }
};

/// Register a custom attribute for our format functions.
struct ExtFormatAttrInfo : public clang::ParsedAttrInfo {
  ExtFormatAttrInfo() {
    static constexpr Spelling spellings[]{
      {clang::ParsedAttr::AS_GNU, EXT_FORMAT},   /// __attribute__((ext_format))
      {clang::ParsedAttr::AS_CXX11, EXT_FORMAT}, /// [[ext_format]]
    };
    Spellings = spellings;
    NumArgs = 2;
  }

  bool diagAppertainsToDecl(clang::Sema &S, const clang::ParsedAttr &Attr, const clang::Decl *D) const override {
    if (!clang::isa<clang::FunctionDecl>(D)) {
      S.Diag(Attr.getLoc(), S.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
        EXT_FORMAT " attribute can only be applied to functions"));
      return false;
    }
    return true;
  }

  AttrHandling handleDeclAttribute(clang::Sema& S, clang::Decl *D, const clang::ParsedAttr &Attr) const override {
    /// Both arguments must be numbers.
    auto format_index = Attr.getArgAsExpr(0);
    auto args_index = Attr.getArgAsExpr(1);
    clang::Expr::EvalResult format_index_val, args_index_val;
    if (!format_index->EvaluateAsInt(format_index_val, S.Context)) {
      S.Diag(Attr.getLoc(), S.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
        "The first argument of '" EXT_FORMAT "' must be an integral constant expression"));
      return AttributeNotApplied;
    }
    if (!args_index->EvaluateAsInt(args_index_val, S.Context)) {
      S.Diag(Attr.getLoc(), S.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
        "The second argument of '" EXT_FORMAT "' must be an integral constant expression"));
      return AttributeNotApplied;
    }

    /// Get the declaration and the format and args index. The index is still 1-based here.
    auto func = clang::cast<clang::FunctionDecl>(D);
    auto format_index_int = format_index_val.Val.getInt().getZExtValue();
    auto args_index_int = args_index_val.Val.getInt().getZExtValue();

    /// The arg index must be in range if the function isn’t variadic.
    if (!func->isVariadic() && args_index_int > func->getNumParams()) {
      S.Diag(Attr.getLoc(), S.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
        EXT_FORMAT " attribute args index %0 is out of bounds"))
          << args_index_int;
      return AttributeNotApplied;
    }

    /// The format index must be a string.
    if (format_index_int > func->getNumParams()) {
      S.Diag(Attr.getLoc(), S.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
        EXT_FORMAT " attribute format index %0 is out of bounds"))
          << format_index_int;
      return AttributeNotApplied;
    }
    auto format_index_param = func->getParamDecl(format_index_int - 1)->getType();
    if (!format_index_param->isPointerType() || !format_index_param->getPointeeType()->isCharType()) {
      auto range = func->getParamDecl(format_index_int - 1)->getSourceRange();
      S.Diag(range.getBegin(), S.Diags.getCustomDiagID(clang::DiagnosticsEngine::Error,
        EXT_FORMAT " attribute format index %0 must be a '[const] char *'"))
          << range << format_index_int;
      return AttributeNotApplied;
    }

    /// Add the function to the list of format functions.
    std::unique_lock lock {format_funcs_mutex};
    format_functions[cast<clang::FunctionDecl>(D)] = {format_index_int - 1, args_index_int - 1};
    return AttributeApplied;
  }
};

/// Plugin class.
struct FmtCheckPlugin : public clang::PluginASTAction {
  /// We don’t accept any arguments, but this is required by the interface.
  bool ParseArgs(const clang::CompilerInstance &, const std::vector<std::string> &) override {
    return true;
  }

  /// Create a new plugin instance.
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef) override {
    /// Define the __EXT_FORMAT__ macro to expand to 1.
    /// First, allocate an identifier info for '__EXT_FORMAT__'.
    auto& id_info = CI.getASTContext().Idents.get("__EXT_FORMAT__");

    /// Allocate a macro info.
    auto* macro_info = CI.getPreprocessor().AllocateMacroInfo(clang::SourceLocation{});
    macro_info->setIsUsed(true);

    /// Set the macro info to expand to 1.
    auto toks = macro_info->allocateTokens(1, CI.getPreprocessor().getPreprocessorAllocator());
    toks[0].startToken();
    toks[0].setKind(clang::tok::numeric_constant);
    toks[0].setLength(1);
    toks[0].setLocation(clang::SourceLocation{});
    toks[0].setLiteralData("1");

    /// Define the macro.
    CI.getPreprocessor().appendDefMacroDirective(&id_info, macro_info);

    /// Create the AST consumer for our plugin.
    return std::make_unique<FmtCheckConsumer>(CI, CI.getASTContext());
  }

  /// Run the plugin after the main action.
  PluginASTAction::ActionType getActionType() override {
    return AddAfterMainAction;
  }
};


static clang::FrontendPluginRegistry::Add<FmtCheckPlugin> X1{"fmt-check-plugin", "Format checker"};
static clang::ParsedAttrInfoRegistry::Add<ExtFormatAttrInfo> X2{EXT_FORMAT, ""};