#include "hittable_list.h"

bool hittable_list::hit(const ray& r, double t_min, double t_max, hit_record& rec) const
{
    hit_record record;
    bool hit_anything = false;
    double closest_so_far = t_max;

    for (const auto& object : objects) {
        if (object->hit(r, t_min, closest_so_far, record)) {
            hit_anything = true;
            closest_so_far = record.t;
            rec = record; // Update the hit record with the closest hit
        }
    }

    return hit_anything;
}
