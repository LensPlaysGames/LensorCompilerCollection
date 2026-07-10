#ifndef LCCJSON_HH
#define LCCJSON_HH

#include <string>
#include <string_view>
#include <variant>
#include <vector>

struct JSONProperty; /** (!) Forward Declarations **/
struct JSONObject;
struct JSONArray;
struct JSONItem;

struct JSONObject {
    std::vector<JSONProperty> properties;

    void add_property(std::string key, int value);
    void add_property(std::string key, std::string value);
    void add_property(std::string key, JSONArray value);
    void add_property(std::string key, JSONObject value);
    void add_property(std::string key, JSONItem value);

    std::string emit();
};

struct JSONArray {
    std::vector<JSONItem> elements;

    void add_element(int value);
    void add_element(std::string value);
    void add_element(JSONArray value);
    void add_element(JSONObject value);
    void add_element(JSONItem value);

    std::string emit();
};

struct JSONItem {
    std::variant<JSONObject, JSONArray, std::string, int> value;

    std::string emit();
};

struct JSONProperty {
    std::string key{};
    JSONItem value{};

    std::string emit();
};

// Prefer simplicity: invalid JSON means you get an empty item back.
JSONItem json_read(std::string_view text);

#endif /* LCCJSON_HH */
