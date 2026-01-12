#include <sarif.hh>
#include <version.hh>

#include <lcc/context.hh>
#include <lcc/diags.hh>
#include <lcc/utils.hh>

#include <fmt/format.h>

#include <filesystem>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

void SARIFObject::add_property(std::string key, int value) {
    properties.emplace_back(key, SARIFItem{value});
}
void SARIFObject::add_property(std::string key, std::string value) {
    properties.emplace_back(key, SARIFItem{value});
}
void SARIFObject::add_property(std::string key, SARIFObject value) {
    properties.emplace_back(key, SARIFItem{value});
}
void SARIFObject::add_property(std::string key, SARIFArray value) {
    properties.emplace_back(key, SARIFItem{value});
}
void SARIFArray::add_element(int value) {
    elements.emplace_back(SARIFItem{value});
}
void SARIFArray::add_element(std::string value) {
    elements.emplace_back(SARIFItem{value});
}
void SARIFArray::add_element(SARIFArray value) {
    elements.emplace_back(SARIFItem{value});
}
void SARIFArray::add_element(SARIFObject value) {
    elements.emplace_back(SARIFItem{value});
}

std::string SARIFItem::emit() {
    if (std::holds_alternative<std::string>(value)) {
        std::string s{};
        // Cleanse string
        // Escape control characters, double quotes, and backslash.
        // (JSON does not allow un-escaped control characters).
        for (auto c : std::get<std::string>(value)) {
            if (c == '\n') s += "\n";
            if (c == '\t') s += "\t";
            else if (c < ' ' or c > '~')
                s += fmt::format("\\u{:04x}", (lcc::u32) c);
            else s += c;
        }

        return fmt::format("\"{}\"", s);
    }

    if (std::holds_alternative<int>(value))
        return fmt::format("{}", std::get<int>(value));

    if (std::holds_alternative<SARIFArray>(value))
        return std::get<SARIFArray>(value).emit();

    if (std::holds_alternative<SARIFObject>(value))
        return std::get<SARIFObject>(value).emit();

    LCC_UNREACHABLE();
}

std::string SARIFProperty::emit() {
    return fmt::format("\"{}\": {}", key, value.emit());
}

std::string SARIFObject::emit() {
    return fmt::format(
        "{{{}}}",
        fmt::join(
            lcc::vws::transform(
                properties,
                [](SARIFProperty& p) { return p.emit(); }
            ),
            ",\n"
        )
    );
}

std::string SARIFArray::emit() {
    return fmt::format(
        "[{}]",
        fmt::join(
            lcc::vws::transform(
                elements,
                [](SARIFItem& s) { return s.emit(); }
            ),
            ",\n"
        )
    );
}

std::string as_sarif(lcc::Context& ctx, std::string_view command_line) {
    const auto escape = [](std::string_view s) {
        std::string escaped{};
        constexpr std::string_view basic_escapes{"\\\""};
        for (auto c : s) {
            if (basic_escapes.contains(c)) {
                escaped += '\\';
                escaped += c;
            } else if (c == '\n') {
                escaped += "\\n";
            } else if (c == '\r') {
                escaped += "\\r";
            } else escaped += c;
        }
        return escaped;
    };

    SARIFObject sarif{};
    sarif.add_property("$schema", "https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json");
    sarif.add_property("version", "2.1.0");
    SARIFArray runs{};
    SARIFObject run{};

    SARIFObject tool{};
    SARIFObject driver{};
    driver.add_property("name", "lcc");
    driver.add_property("semanticVersion", LCC_VERSION_STRING);
    driver.add_property(
        "informationUri",
        "https://github.com/LensPlaysGames/LensorCompilerCollection"
    );
    driver.add_property("rules", SARIFArray{});
    tool.add_property("driver", driver);
    run.add_property("tool", tool);

    SARIFObject baseIds{};
    SARIFObject pwd{};
    const auto pwd_path = std::filesystem::absolute(std::filesystem::current_path());
    pwd.add_property(
        "uri",
        fmt::format(
            "file://{}/",
            pwd_path.string()
        )
    );
    baseIds.add_property("PWD", pwd);
    run.add_property("originalUriBaseIds", baseIds);

    // run artifacts
    SARIFArray artifacts{};
    // Populate with VALID, SEEKABLE artifacts
    for (auto& f : ctx.files()) {
        if (lcc::fs::exists(f->path())) {
            SARIFObject artifact{};

            SARIFObject location{};
            location.add_property("uri", lcc::fs::absolute(f->path()).lexically_relative(pwd_path));
            location.add_property("uriBaseId", "PWD");
            artifact.add_property("location", location);

            SARIFObject contents{};
            contents.add_property(
                "text",
                escape(std::string_view{f->data(), f->size()})
            );
            artifact.add_property("contents", contents);

            // TODO: IR, other languages, etc. Get this from context (or something).
            artifact.add_property("sourceLanguage", "Glint");

            artifacts.add_element(artifact);
        }
    }
    run.add_property("artifacts", artifacts);

    // SARIF run object invocations array property
    SARIFArray invocations{};

    // SARIF invocation object
    SARIFObject invocation{};
    // Add "commandLine" property
    invocation.add_property(
        "commandLine",
        std::string(command_line)
    );
    invocations.add_element(invocation);

    run.add_property("invocations", invocations);

    // run results
    // A SARIF "result" is pretty much equivalent to an LCC "diagnostic".
    SARIFArray results{};

    const auto level_string = [](lcc::Diag::Kind k) {
        switch (k) {
            case lcc::Diag::Kind::None: return "none";
            case lcc::Diag::Kind::Note: return "note";
            case lcc::Diag::Kind::Warning: return "warning";
            case lcc::Diag::Kind::Error: return "error";

            case lcc::Diag::Kind::FError:
            case lcc::Diag::Kind::ICError:
                LCC_UNREACHABLE();
        }
        LCC_UNREACHABLE();
    };

    for (auto& d : ctx.diagnostics()) {
        // SARIF result object
        // https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790888
        SARIFObject result{};

        // SARIF result object, ruleId property
        // https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790893
        // MS wants this to be a couple identifying letters followed by a rule
        // number.
        // GCC just sets this to "error", as far as I can tell.
        result.add_property("ruleId", "error");

        // SARIF result object, level property
        // https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790898
        result.add_property("level", level_string(d.kind));

        // SARIF result object, message property
        // https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790899
        SARIFObject message{};
        message.add_property("text", escape(d.message));
        result.add_property("message", message);

        SARIFArray locations{};
        if (d.where.seekable(&ctx)) {
            auto info = d.where.seek(&ctx);

            SARIFObject location{};
            SARIFObject physical_location{};

            SARIFObject artifact_location{};
            const auto path = ctx.files().at(d.where.file_id)->path();
            artifact_location.add_property("uri", path.string());
            artifact_location.add_property("uriBaseId", "PWD");
            physical_location.add_property("artifactLocation", artifact_location);

            SARIFObject region{};
            region.add_property("startLine", (int) info.line);
            region.add_property("startColumn", 1 + (int) info.col);
            region.add_property("endColumn", 1 + (int) info.col + d.where.len);
            physical_location.add_property("region", region);

            SARIFObject context_region{};
            context_region.add_property("startLine", (int) info.line);

            SARIFObject context_snippet{};
            std::string context_snippet_string{};
            for (auto it = info.line_start; it != info.line_end; ++it)
                context_snippet_string += *it;
            context_snippet.add_property("text", context_snippet_string);
            context_region.add_property("snippet", context_snippet);

            physical_location.add_property("contextRegion", context_region);

            location.add_property("physicalLocation", physical_location);
            locations.add_element(location);
        }
        result.add_property("locations", locations);

        // SARIF fixes
        SARIFArray fixes{};
        // TODO: Include Diag::fixes in Context::DiagnosticReport.
        // SARIF fix object
        result.add_property("fixes", fixes);

        results.add_element(result);
    }
    run.add_property("results", results);

    runs.add_element(run);
    sarif.add_property("runs", runs);

    return sarif.emit();
};
