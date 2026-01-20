#ifndef LCC_DEPENDENCY_GRAPH_HH
#define LCC_DEPENDENCY_GRAPH_HH

#include <vector>
#include <lcc/utils.hh>

namespace lcc {

template <typename Entity>
class DependencyGraph {
    struct Node {
        Entity* entity;
        std::vector<Entity*> dependencies{};

        Node(Entity* entity) : entity(entity) {}
    };

    std::vector<Node*> nodes;

public:
    struct Result {
        enum class Kind {
            Ok,
            Cycle,
        };

        Kind kind;
        std::vector<Entity*> order{};
        Entity* from{nullptr};
        Entity* to{nullptr};
    };

    DependencyGraph() {}

    auto add_dependency(Entity* entity, Entity* dependency) {
        Node* node;

        auto it = rgs::find_if(nodes, [&](Node* node) { return node->entity == entity; });
        if (it != nodes.end()) {
            node = *it;
        } else {
            node = new Node(entity);
            nodes.push_back(node);
        }

        LCC_ASSERT(node);
        if (dependency) node->dependencies.push_back(dependency);
    }

    auto ensure_tracked(Entity* entity) { add_dependency(entity, nullptr); }

    Result get_resolved_order() {
        std::vector<Entity*> resolved{};
        std::vector<Entity*> seen{};

        std::function<Result(Entity*)> ResolveDependencies;
        ResolveDependencies = [&](Entity* entity) -> Result {
            auto resolved_it = rgs::find(resolved, entity);
            if (resolved_it != resolved.end())
                return Result{Result::Kind::Ok};

            seen.push_back(entity);

            auto it = rgs::find_if(nodes, [&](Node* node) { return node->entity == entity; });
            bool is_resolved = it == nodes.end() or (*it)->dependencies.empty();

            if (not is_resolved) {
                const std::vector<Entity*>& dependencies = (*it)->dependencies;
                for (Entity* dep : dependencies) {
                    auto dep_resolved_it = rgs::find(resolved, dep);
                    if (dep_resolved_it != resolved.end())
                        return Result{Result::Kind::Ok};

                    auto dep_seen_it = rgs::find(seen, dep);
                    if (dep_seen_it != seen.end())
                        return Result{Result::Kind::Cycle, {}, entity, *dep_seen_it};

                    Result result = ResolveDependencies(dep);
                    if (result.kind != Result::Kind::Ok) {
                        return result;
                    }
                }
            }

            resolved.push_back(entity);
            seen.erase(rgs::find(seen, entity));

            return Result{Result::Kind::Ok};
        };

        for (Node* node : nodes) {
            Result result = ResolveDependencies(node->entity);
            if (result.kind != Result::Kind::Ok) {
                return result;
            }
        }

        return Result{Result::Kind::Ok, std::move(resolved)};
    }
};

}; // namespace lcc

#endif // LCC_DEPENDENCY_GRAPH_HH
