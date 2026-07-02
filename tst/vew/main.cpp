// vew
// Display results of SARIF in a reasonable and uniform way.

#include <lccjson/lccjson.hh>

#include <lccbase/file.hh>

#include <fmt/format.h>

#include <algorithm>
#include <cstdio>
#include <iterator>
#include <ranges>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

// TODO: String cache for deduplication (I have a feeling we'll be dealing
// with sometimes hundreds of duplicate strings).
struct SARIFResult {
    // NOTE: For any other value, level must be "none".
    std::string kind{"fail"};
    std::string rule_identifier{};
    // One of error, warning, note, or none.
    std::string level{};
    std::string message{};
    // TODO: artifact + region location information
    // TODO: fixes
};

struct TextualResultsDisplay {
    std::vector<SARIFResult>& results;

    static auto display_result(const SARIFResult& result) -> std::string {
        std::string out{};

        if (result.kind == "pass") {
            fmt::format_to(
                std::back_inserter(out),
                "{}O{}",
                "\033[32;1m",
                "\033[0m"
            );
        } else {
            // If kind is "fail" (the default), level should be taken into account.
            std::string downcased_level{};
            std::ranges::transform(
                result.level,
                std::back_inserter(downcased_level),
                tolower
            );
            if (downcased_level == "error") {
                fmt::format_to(
                    std::back_inserter(out),
                    "{}X{}",
                    "\033[31;1m",
                    "\033[0m"
                );
            } else if (downcased_level == "warning") {
                fmt::format_to(
                    std::back_inserter(out),
                    "{}O{}",
                    "\033[33;1m",
                    "\033[0m"
                );
            } else if (downcased_level == "note") {
                out += '|';
            } else {
                fmt::format_to(std::back_inserter(out), "[{}]:", result.level);
            }
        }

        // Indent long or complex messages.
        bool long_or_complex = result.message.contains('\n')
                            or result.message.length() > 384;
        if (long_or_complex)
            out += "\n\t";
        else out += ' ';
        out += result.message;

        // Rule ID
        if (long_or_complex)
            out += '\n';
        else out += ' ';
        fmt::format_to(
            std::back_inserter(out),
            "{}[{}]{}",
            "\033[37m",
            result.rule_identifier,
            "\033[0m"
        );

        return out;
    }

    auto display() -> std::string {
        std::string out{};
        auto it = std::back_inserter(out);
        std::vector<SARIFResult*> failures{};
        for (auto& r : results) {
            fmt::format_to(
                it,
                "{}\n",
                display_result(r)
            );
            if (r.kind != "pass")
                failures.emplace_back(&r);
        }

        fmt::format_to(
            it,
            "{} RESULTS\n"
            "{} {}PASSING{}\n",
            results.size(),
            results.size() - failures.size(),
            "\033[32;1m",
            "\033[0m"
        );
        if (not failures.empty()) {
            fmt::format_to(
                it,
                "{} {}FAILING{}\n",
                failures.size(),
                "\033[31;1m",
                "\033[0m"
            );
            for (
                const auto& r : std::ranges::views::transform(
                    failures,
                    [](auto x) { return *x; }
                )
            ) fmt::format_to(it, "  {}\n", display_result(r));
        }

        return out;
    }
};

// @returns pointer to prevent copying, allow you to modify in place, etc.
auto json_property(JSONObject& object, std::string_view key) -> JSONProperty* {
    for (auto& property : object.properties) {
        if (property.key == key)
            return &property;
    }
    return nullptr;
}

auto json_drill(JSONObject& object, std::string_view keys) -> JSONProperty* {
    auto position_of_first_dot = keys.find_first_of('.');
    auto name = keys.substr(0, position_of_first_dot);

    auto property = json_property(object, name);
    if (not property)
        return nullptr;

    // If this wasn't the last name in the keys we are drilling, we have to
    // keep going.
    if (keys.contains('.')) {
        auto& next_object = std::get<JSONObject>(property->value.value);
        return json_drill(
            next_object,
            keys.substr(position_of_first_dot)
        );
    }
    return property;
}

int main(int argc, const char** argv) {
    std::vector<SARIFResult> results{};

    // Get SARIF files from command line arguments
    for (auto i = 1; i < argc; ++i) {
        auto contents = lcc::File::Read(argv[i]);
        // Parse contents as JSON/SARIF.
        auto data = json_read(std::string_view{contents});

        // SARIF validation + data collection
        // TODO: For index-references (numeric ruleId, formatted message
        // placeholders, etc), we need to resolve those *file-wide* before merging
        // the results into the pool of all collected results.
        if (auto o = std::get_if<JSONObject>(&data.value)) {
            auto runs_data = json_property(*o, "runs");
            if (not runs_data) {
                fmt::print(
                    stderr,
                    "Argument {}: invalid SARIF in file at `{}`: unable to parse `runs` property",
                    i,
                    argv[i]
                );
                continue;
            }
            auto& runs_array = std::get<JSONArray>(runs_data->value.value);
            for (auto& run : runs_array.elements) {
                auto results_data = json_property(
                    std::get<JSONObject>(run.value),
                    "results"
                );
                if (not results_data) {
                    fmt::print(
                        stderr,
                        "Argument {}: invalid SARIF in file at `{}`: unable to parse `results` property",
                        i,
                        argv[i]
                    );
                    continue;
                }
                auto& results_array = std::get<JSONArray>(results_data->value.value);
                for (auto& element : results_array.elements) {
                    // An element of the results array is a result!
                    auto& result_object = std::get<JSONObject>(element.value);

                    // TODO: More comprehensive result parsing

                    SARIFResult sarif_result{};
                    auto prop_rule_id = json_property(
                        result_object,
                        "ruleId"
                    );
                    // if (not prop_rule_id) {
                    //     fmt::print("Invalid SARIF: Missing ruleId property on run result\n");
                    // }
                    sarif_result.rule_identifier = std::get<std::string>(
                        prop_rule_id->value.value
                    );

                    auto prop_level = json_property(
                        result_object,
                        "level"
                    );
                    if (prop_level) {
                        sarif_result.level = std::get<std::string>(
                            prop_level->value.value
                        );
                    }

                    auto prop_kind = json_property(
                        result_object,
                        "kind"
                    );
                    if (prop_kind) {
                        sarif_result.kind = std::get<std::string>(
                            prop_kind->value.value
                        );
                    }

                    auto prop_message = json_property(
                        result_object,
                        "message"
                    );
                    if (prop_message) {
                        auto& message_object = std::get<JSONObject>(
                            prop_message->value.value
                        );
                        auto prop_message_text = json_property(
                            message_object,
                            "text"
                        );
                        sarif_result.message = std::get<std::string>(
                            prop_message_text->value.value
                        );
                    }

                    results.push_back(sarif_result);
                }
            }
        }
    }

    {
        TextualResultsDisplay d{results};
        fmt::print("{}", d.display());
    }
    // TODO: Emit website (HTML) that may be viewed in a browser.

    return 0;
}
