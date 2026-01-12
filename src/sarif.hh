#ifndef LCC_DRIVER_SARIF_HH
#define LCC_DRIVER_SARIF_HH

#include <lcc/context.hh>

#include <string>
#include <variant>
#include <vector>

struct SARIFProperty; // forward declaration
struct SARIFObject;   // forward declaration
struct SARIFArray;    // forward declaration
struct SARIFItem;     // forward declaration

struct SARIFObject {
    std::vector<SARIFProperty> properties;

    void add_property(std::string key, int value);
    void add_property(std::string key, std::string value);
    void add_property(std::string key, SARIFArray value);
    void add_property(std::string key, SARIFObject value);
    std::string emit();
};

struct SARIFArray {
    std::vector<SARIFItem> elements;

    void add_element(int value);
    void add_element(std::string value);
    void add_element(SARIFArray value);
    void add_element(SARIFObject value);
    std::string emit();
};

struct SARIFItem {
    std::variant<SARIFObject, SARIFArray, std::string, int> value;

    std::string emit();
};

struct SARIFProperty {
    std::string key{};
    SARIFItem value{};

    std::string emit();
};

std::string as_sarif(lcc::Context& ctx, std::string_view command_line);

#endif /* LCC_DRIVER_SARIF_HH */
