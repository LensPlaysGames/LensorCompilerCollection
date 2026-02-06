#include <lcc/calling_convention.hh>

#include <lcc/calling_conventions/ms_x64.hh>
#include <lcc/calling_conventions/sysv_x86_64.hh>
#include <lcc/codegen/register_allocation.hh>
#include <lcc/codegen/x86_64/x86_64.hh>
#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/target.hh>

#include <algorithm>
#include <iterator>
#include <vector>

namespace lcc::cconv {

auto machine_description(Context* context) -> MachineDescription {
    MachineDescription desc{};

    // TODO: static assert for target handling
    if (context->target()->is_arch_x86_64()) {
        desc.return_register_to_replace = +x86_64::RegisterId::RETURN;
        std::vector<usz> jeep_registers{};
        std::vector<usz> scalar_registers{};
        std::vector<usz> volatile_registers{};
        if (context->target()->is_cconv_ms()) {
            desc.return_registers.emplace_back(
                MachineDescription::RegistersPerCategory{
                    +Register::Category::DEFAULT,
                    {+x86_64::RegisterId::RAX}
                }
            );
            desc.return_registers.emplace_back(
                MachineDescription::RegistersPerCategory{
                    +Register::Category::FLOAT,
                    {+x86_64::RegisterId::XMM0}
                }
            );
            // Just the volatile registers
            rgs::transform(
                cconv::msx64::volatile_regs,
                std::back_inserter(jeep_registers),
                [](auto r) { return +r; }
            );

            lcc::rgs::transform(
                lcc::cconv::msx64::volatile_float_regs,
                std::back_inserter(scalar_registers),
                [](auto r) { return lcc::operator+(r); }
            );

            // All volatile registers
            lcc::rgs::transform(
                lcc::cconv::msx64::volatile_regs,
                std::back_inserter(volatile_registers),
                [](auto r) { return lcc::operator+(r); }
            );
            lcc::rgs::transform(
                lcc::cconv::msx64::volatile_float_regs,
                std::back_inserter(volatile_registers),
                [](auto r) { return lcc::operator+(r); }
            );
        } else if (context->target()->is_cconv_sysv()) {
            desc.return_registers.emplace_back(
                MachineDescription::RegistersPerCategory{
                    +Register::Category::DEFAULT,
                    {+x86_64::RegisterId::RAX, +x86_64::RegisterId::RDX}
                }
            );
            desc.return_registers.emplace_back(
                MachineDescription::RegistersPerCategory{
                    +Register::Category::FLOAT,
                    {+x86_64::RegisterId::XMM0}
                }
            );
            // Just the volatile registers
            rgs::transform(
                cconv::sysv::volatile_regs,
                std::back_inserter(jeep_registers),
                [](auto r) { return +r; }
            );
            lcc::rgs::transform(
                lcc::cconv::sysv::scalar_regs,
                std::back_inserter(scalar_registers),
                [](auto r) { return lcc::operator+(r); }
            );
            // All volatile registers
            lcc::rgs::transform(
                lcc::cconv::sysv::all_volatile_regs,
                std::back_inserter(volatile_registers),
                [](auto r) { return lcc::operator+(r); }
            );
        } else Diag::ICE("Sorry, unhandled x86_64 calling convention");

        LCC_ASSERT(
            jeep_registers.size(),
            "Must populate general purpose register list"
        );
        desc.registers.emplace_back(
            +Register::Category::DEFAULT,
            std::move(jeep_registers)
        );
        desc.registers.emplace_back(
            +Register::Category::FLOAT,
            std::move(scalar_registers)
        );

        desc.volatile_registers = std::move(volatile_registers);
        desc.preserve_volatile_opcodes.emplace_back(
            +x86_64::Opcode::Call
        );

    } else Diag::ICE("Sorry, unhandled target architecture");

    return desc;
}

} // namespace lcc::cconv
