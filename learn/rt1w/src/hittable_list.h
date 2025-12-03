#ifndef SRC_HITTABLE_LIST_H
#define SRC_HITTABLE_LIST_H

#include <memory>
#include <vector>

#include "hittable.h"


class hittable_list : public hittable
{
public:
    hittable_list() = default;

    explicit hittable_list(const std::shared_ptr<hittable>& object)
    {
        add(object);
    }

    void clear()
    {
        objects.clear();
    }

    void add(const std::shared_ptr<hittable>& object)
    {
        objects.push_back(object);
    }

    bool hit(const ray& r, double t_min, double t_max, hit_record& rec) const override;

private:
    std::vector<std::shared_ptr<hittable>> objects;
};



#endif //SRC_HITTABLE_LIST_H
