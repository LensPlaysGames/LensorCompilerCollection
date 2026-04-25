#include <cctype>
#include <iterator>
#include <lccjson/lccjson.hh>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <ranges>
#include <string>
#include <utility>
#include <vector>

void JSONObject::add_property(std::string key, int value) {
    properties.emplace_back(key, JSONItem{value});
}
void JSONObject::add_property(std::string key, std::string value) {
    properties.emplace_back(key, JSONItem{value});
}
void JSONObject::add_property(std::string key, JSONObject value) {
    properties.emplace_back(key, JSONItem{value});
}
void JSONObject::add_property(std::string key, JSONArray value) {
    properties.emplace_back(key, JSONItem{value});
}
void JSONObject::add_property(std::string key, JSONItem value) {
    properties.emplace_back(key, value);
}
void JSONArray::add_element(int value) {
    elements.emplace_back(JSONItem{value});
}
void JSONArray::add_element(std::string value) {
    elements.emplace_back(JSONItem{value});
}
void JSONArray::add_element(JSONArray value) {
    elements.emplace_back(JSONItem{value});
}
void JSONArray::add_element(JSONObject value) {
    elements.emplace_back(JSONItem{value});
}
void JSONArray::add_element(JSONItem value) {
    elements.emplace_back(value);
}

std::string JSONItem::emit() {
    if (std::holds_alternative<std::string>(value)) {
        std::string s{};
        // Cleanse string
        // Escape control characters, double quotes, and backslash.
        // (JSON does not allow un-escaped control characters).
        for (auto c : std::get<std::string>(value)) {
            if (c == '\n') s += "\n";
            if (c == '\t') s += "\t";
            else if (c < ' ' or c > '~')
                s += fmt::format("\\u{:04x}", (uint32_t) c);
            else s += c;
        }

        return fmt::format("\"{}\"", s);
    }

    if (std::holds_alternative<int>(value))
        return fmt::format("{}", std::get<int>(value));

    if (std::holds_alternative<JSONArray>(value))
        return std::get<JSONArray>(value).emit();

    if (std::holds_alternative<JSONObject>(value))
        return std::get<JSONObject>(value).emit();

    std::unreachable();
}

std::string JSONProperty::emit() {
    return fmt::format(
        "\"{}\": {}",
        key,
        value.emit()
    );
}

std::string JSONObject::emit() {
    return fmt::format(
        "{{{}}}",
        fmt::join(
            std::ranges::views::transform(
                properties,
                [](JSONProperty& property) {
                    return property.emit();
                }
            ),
            ",\n"
        )
    );
}

std::string JSONArray::emit() {
    return fmt::format(
        "[{}]",
        fmt::join(
            std::ranges::views::transform(
                elements,
                [](JSONItem& item) {
                    return item.emit();
                }
            ),
            ",\n"
        )
    );
}

namespace {

struct whitespace_iterator {
    std::string_view& characters;
    decltype(characters.begin()) iterator;

    whitespace_iterator(decltype(characters)&& in)
        : characters(in) {
        iterator = characters.begin();
    }

    operator bool() {
        return iterator != characters.end();
    }

    auto operator*() {
        if (iterator != characters.end())
            return *iterator;
        return '\0';
    }

    auto operator++() {
        // Move forward by one no matter what.
        ++iterator;

        // Keep moving iterator forward until non-space character is encountered,
        // or the end of the input is reached.
        while (
            iterator != characters.end()
            and std::isspace(*iterator)
        ) ++iterator;

        return iterator;
    }
};

struct JSONParseContext {
    whitespace_iterator& c;

    std::optional<JSONItem> read_string() {
        // Expect Opening Quote
        if (*c != '"')
            return {};
        ++c; // Yeet Opening Quote

        // Collect Contents Until Closing Quote
        auto begin = c.iterator;
        // TODO: Handle escape characters including quotes
        while (c and *c != '"')
            ++c;

        auto text = std::string{
            begin,
            c.iterator
        };

        // Expect Closing Quote
        // If not at closing quote, we have an unterminated string literal.
        if (*c != '"')
            return {};
        ++c; // Yeet Closing Quote

        return JSONItem{std::move(text)};
    }

    std::optional<JSONItem> read_number() {
        // Expect Digit
        if (not std::isdigit(*c))
            return {};

        // TODO: Parse 0x/0b/0o base prefix

        size_t length{};
        auto n = std::stoi(c.iterator, &length, 10);
        if (length == 0)
            return {};

        // Advance past parsed number.
        while (length--) ++c;

        return JSONItem{int(n)};
    }

    std::optional<JSONArray> read_array() {
        // Expect Opening Bracket
        if (*c != '[')
            return {};

        ++c; // Yeet Opening Bracket
        // Collect Contents Until Closing Bracket
        JSONArray array{};

        while (c and *c != ']') {
            auto element = read_item();
            // Expect element item
            if (not element)
                return {};

            array.add_element(element.value());

            // Advance past separator, if present.
            if (*c == ',') ++c;
        }

        if (*c != ']')
            return {};
        ++c; // Yeet Closing Bracket

        return array;
    }

    std::optional<JSONObject> read_object() {
        // Expect Opening Brace
        if (*c != '{')
            return {};
        ++c; // Yeet Opening Brace

        // Collect Contents Until Closing Brace
        JSONObject object{};

        while (c and *c != '}') {
            // Expect property name
            auto name = read_string();
            if (not name) fmt::print("object: no property name\n");
            if (not name)
                return {};

            // Expect Colon
            if (not c or *c != ':')
                return {};
            ++c; // Yeet Colon

            // Expect property value
            auto value = read_item();
            if (not value) fmt::print("object: no property value\n");
            if (not value)
                return {};

            // Advance past separator, if present.
            if (*c == ',') ++c;

            object.add_property(
                std::get<std::string>(name.value().value),
                value.value()
            );
        }

        // Expect Closing Brace
        if (*c != '}') fmt::print("object: no closing brace\n");
        if (*c != '}')
            return {};
        ++c; // Yeet Closing Brace

        return object;
    }

    std::optional<JSONItem> read_item() {
        switch (*c) {
            default:
                return {};

            // number
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': {
                auto n = read_number();
                if (n)
                    return n.value();
                return {};
            } break;

            // string
            case '"': {
                auto s = read_string();
                if (s)
                    return s.value();
                return {};
            } break;

            // object
            case '{': {
                auto o = read_object();
                if (o)
                    return JSONItem{o.value()};
                return {};
            } break;

            // array
            case '[': {
                auto a = read_array();
                if (a)
                    return JSONItem{a.value()};
                return {};
            } break;
        }
    }
};

} // namespace

JSONItem json_read(std::string_view text) {
    if (text.empty())
        return {};

    whitespace_iterator c{text};
    JSONParseContext p{c};
    auto i = p.read_item();
    if (not i)
        return {};
    return i.value();
}
