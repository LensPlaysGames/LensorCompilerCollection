#ifndef LCC_MODULE_HH
#define LCC_MODULE_HH

#include <lcc/diags.hh>
#include <lcc/forward.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils.hh>
#include <lcc/utils/result.hh>
#include <object/generic.hh>

#include <algorithm>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace lcc {
/// An LCC IR module.
class Module {
    Context* _ctx{};
    std::string _name;

    std::vector<Function*> _code;
    std::vector<GlobalVariable*> _vars;
    std::vector<Section> _extra_sections;

    usz _virtual_register{0x420};

public:
    Module(Module&) = delete;
    Module(Module&&) = delete;
    auto operator=(Module&) -> Module& = delete;
    auto operator=(Module&&) -> Module& = delete;

    /// Create a new LCC module.
    ///
    /// It is recommended to allocate these on the heap since
    /// they can’t be moved.
    explicit Module(
        Context* ctx,
        std::string name = "<Peanut Butter Banana Pants Module (Unnamed)>"
    ) : _ctx(ctx), _name(std::move(name)) {}

    /// Get the context that owns the module.
    [[nodiscard]]
    auto context() const -> Context* { return _ctx; }

    /// Get the name of this module.
    [[nodiscard]]
    auto name() const -> const std::string& { return _name; }

    /// IR -> MIR
    [[nodiscard]]
    auto mir() -> std::vector<MFunction>;

    /// Emit the module as Textual LLVM IR.
    /// NOTE: LLVM's Textual IR Representation is not guaranteed, so your
    /// particular version of LLVM may not be happy compiling our particular
    /// dialect of their textual representation.
    [[nodiscard]]
    auto as_llvm_ir() -> std::string;

    /// Get the textual LCC IR of this module.
    [[nodiscard]]
    auto as_lcc_ir(bool use_colour) -> std::string;

    [[nodiscard]]
    auto code() -> std::vector<Function*>& { return _code; }
    [[nodiscard]]
    auto code() const -> std::vector<Function*> { return _code; }
    [[nodiscard]]
    auto vars() -> std::vector<GlobalVariable*>& { return _vars; }
    [[nodiscard]]
    auto vars() const -> std::vector<GlobalVariable*> { return _vars; }

    [[nodiscard]]
    auto extra_sections() -> std::vector<Section>& {
        return _extra_sections;
    }
    [[nodiscard]]
    auto extra_sections() const -> std::vector<Section> {
        return _extra_sections;
    }

    void add_function(Function* func) { _code.push_back(func); }
    void add_var(GlobalVariable* var) { _vars.push_back(var); }

    void add_extra_section(const Section& section) {
        _extra_sections.push_back(section);
    }

    /// Helper for lowering a store to a memcpy in x86_64
    /// \see Module::lower()
    void _x86_64_lower_store(StoreInst*, Function*);
    /// NOTE: May miss out on key architecture-common lowering if used alone.
    /// \see Module::lower()
    void _x86_64_sysv_lower_parameters();
    /// NOTE: May miss out on key architecture-common lowering if used alone.
    /// \see Module::lower()
    void _x86_64_sysv_lower_overlarge();
    /// NOTE: May miss out on key architecture-common lowering if used alone.
    /// \see Module::lower()
    void _x86_64_msx64_lower_parameters();
    /// NOTE: May miss out on key architecture-common lowering if used alone.
    /// \see Module::lower()
    void _x86_64_msx64_lower_overlarge();

    void lower();
    void emit(std::filesystem::path output_file_path);

    [[nodiscard]]
    auto next_vreg() -> usz {
        return _virtual_register++;
    }

    [[nodiscard]]
    auto function_by_name(std::string_view function_name) -> Result<Function*> {
        for (auto* f : code()) {
            if (f->has_name(function_name)) {
                return f;
            }
        }
        return Diag::Note("not found");
    }

    [[nodiscard]]
    auto function_by_one_of_names(const std::vector<IRName>& function_names) -> Result<Function*> {
        for (auto* f : code()) {
            if (f->has_one_of_names(function_names))
                return f;
        }
        return Diag::Note("not found");
    }

    /// Parse a module from a source span.
    [[nodiscard]]
    static auto Parse(Context* ctx, std::string_view source) -> std::unique_ptr<Module>;
    /// Parse a module from a file.
    [[nodiscard]]
    static auto Parse(Context* ctx, File& file) -> std::unique_ptr<Module>;
};
} // namespace lcc

#endif // LCC_MODULE_HH
