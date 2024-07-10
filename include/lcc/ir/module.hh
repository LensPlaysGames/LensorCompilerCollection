#ifndef LCC_MODULE_HH
#define LCC_MODULE_HH

#include <lcc/forward.hh>
#include <lcc/ir/ir.hh>
#include <lcc/utils.hh>
#include <object/generic.hh>

namespace lcc {
/// An LCC IR module.
class Module {
    Context* _ctx;
    std::string _name;

    std::vector<Function*> _code;
    std::vector<GlobalVariable*> _vars;
    std::vector<Section> _extra_sections;

    usz _virtual_register{0x420};

    Module(Module&) = delete;
    Module(Module&&) = delete;
    Module& operator=(Module&) = delete;
    Module& operator=(Module&&) = delete;

public:
    /// Create a new LCC module.
    ///
    /// It is recommended to allocate these on the heap since
    /// they can’t be moved.
    Module(Context* ctx, std::string name = "<unnamed>") : _ctx(ctx), _name(std::move(name)) {}

    /// Get the context that owns the module.
    auto context() const -> Context* { return _ctx; }

    /// Get the name of this module.
    auto name() const -> const std::string& { return _name; }

    /// IR -> MIR
    auto mir() -> std::vector<MFunction>;

    /// Emit the module as LLVM IR.
    auto llvm() -> std::string;

    /// Print the IR of this module.
    // TODO: return string
    void print_ir(bool use_colour);

    auto code() -> std::vector<Function*>& { return _code; }
    auto code() const -> std::vector<Function*> { return _code; }
    auto vars() -> std::vector<GlobalVariable*>& { return _vars; }
    auto vars() const -> std::vector<GlobalVariable*> { return _vars; }
    auto extra_sections() -> std::vector<Section>& { return _extra_sections; }
    auto extra_sections() const -> std::vector<Section> { return _extra_sections; }

    void add_function(Function* func) { _code.push_back(func); }
    void add_var(GlobalVariable* var) { _vars.push_back(var); }

    void add_extra_section(const Section& section) {
        _extra_sections.push_back(section);
    }

    void lower();
    void emit(std::filesystem::path output_file_path);

    usz next_vreg() {
        return _virtual_register++;
    }

    /// Parse a module from a file.
    static auto Parse(Context* ctx, File& file) -> std::unique_ptr<Module>;
};
} // namespace lcc

#endif // LCC_MODULE_HH
