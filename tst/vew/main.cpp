#include <fmt/format.h>
#include <lcc/file.hh>
#include <lccjson/lccjson.hh>

#include <algorithm>
#include <iterator>
#include <variant>

// TODO: String cache for deduplication (I have a feeling we'll be dealing
// with sometimes hundreds of duplicate strings).

struct SARIFResult {
    std::string rule_identifier{};
    std::string level{};
    std::string message{};
    // TODO: artifact + region location information
    // TODO: fixes
};

struct TextualResultsDisplay {
    std::vector<SARIFResult>& results;

    static auto display_result(SARIFResult& result) -> std::string {
        std::string out{};

        std::string downcased_level{};
        std::ranges::transform(
            result.level,
            std::back_inserter(downcased_level),
            tolower
        );
        if (downcased_level == "error") {
            fmt::format_to(
                std::back_inserter(out),
                "{}error{}:",
                "\033[31;1m",
                "\033[0m"
            );
        } else if (downcased_level == "warning") {
            fmt::format_to(
                std::back_inserter(out),
                "{}warning{}:",
                "\033[33;1m",
                "\033[0m"
            );
        } else if (downcased_level == "note") {
            fmt::format_to(
                std::back_inserter(out),
                "{}note{}:",
                "\033[32:1m",
                "\033[0m"
            );
        } else {
            fmt::format_to(std::back_inserter(out), "[{}]:", result.level);
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
        for (auto& r : results) {
            fmt::format_to(
                std::back_inserter(out),
                "{}\n",
                display_result(r)
            );
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
    // Get SARIF files from command line arguments
    for (auto i = 1; i < argc; ++i) {
        auto contents = lcc::File::Read(argv[i]);
        // Parse contents as JSON/SARIF.
        auto data = json_read(std::string_view{contents});

        // Display results of SARIF in a reasonable and uniform way.
        std::vector<SARIFResult> results{};

        // SARIF validation + data collection
        if (auto o = std::get_if<JSONObject>(&data.value)) {
            auto runs_data = json_property(*o, "runs");
            if (not runs_data)
                continue;
            auto& runs_array = std::get<JSONArray>(runs_data->value.value);
            for (auto& run : runs_array.elements) {
                auto results_data = json_property(
                    std::get<JSONObject>(run.value),
                    "results"
                );
                if (not results_data)
                    continue;
                auto& results_array = std::get<JSONArray>(results_data->value.value);
                for (auto& element : results_array.elements) {
                    // An element of the results array is a result!
                    auto& result_object = std::get<JSONObject>(element.value);

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
                    sarif_result.level = std::get<std::string>(
                        prop_level->value.value
                    );

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

        TextualResultsDisplay d{results};

        fmt::print("{}", d.display());
    }

    // TODO: Display results of all SARIF inputs in a reasonable and uniform
    // way (sorting results?).

    // TODO: Emit website (HTML) that may be viewed in a browser.

    return 0;
}
