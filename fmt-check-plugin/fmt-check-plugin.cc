#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendPluginRegistry.h>
#include <iostream>

namespace match = clang::ast_matchers;
/// TODO: Pragma handler?
static std::vector<llvm::StringRef> format_funcs = {
  "format",
  "print",
  "eprint",
};

/// Match callbacks.
struct FormatCheckCallback : public match::MatchFinder::MatchCallback {
  clang::CompilerInstance &ci;
  explicit FormatCheckCallback(clang::CompilerInstance &ci_) : ci(ci_) {}

  void run(const match::MatchFinder::MatchResult &result) override {
    /*auto call = result.Nodes.getNodeAs<clang::CallExpr>("call");
    call->dump();*/

    /*/// The first argument is the format string.
    auto format = call->getArg(0);

    /// Get the value of the format string.
    auto format_str = clang::Lexer::getSourceText(
        clang::CharSourceRange::getTokenRange(format->getSourceRange()),
        ci.getSourceManager(),
        ci.getLangOpts()
    );

    std::cerr << "STRING: " <<  format_str.data() << "\n";*/
  };
};

/// Consumer.
struct FmtCheckConsumer : public clang::ASTConsumer {
  clang::CompilerInstance &CI;
  clang::ASTContext &ctx;
  explicit FmtCheckConsumer(clang::CompilerInstance &CI, clang::ASTContext& ctx2) : CI(CI), ctx(ctx2) {}

  void HandleTranslationUnit(clang::ASTContext &Context) override {
    Context.getTranslationUnitDecl();
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
    return std::make_unique<FmtCheckConsumer>(CI, CI.getASTContext());
  }

  /// Run the plugin after the main action.
  PluginASTAction::ActionType getActionType() override {
    return AddAfterMainAction;
  }
};

static clang::FrontendPluginRegistry::Add<FmtCheckPlugin> X{"fmt-check-plugin", "Format checker"};
