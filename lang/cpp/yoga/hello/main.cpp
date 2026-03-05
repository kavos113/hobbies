#include <yoga/Yoga.h>

#include <iostream>

struct Rect {
    float x, y, width, height;
};

Rect getNodeLayout(YGNodeRef node) {
    return {
        YGNodeLayoutGetLeft(node),
        YGNodeLayoutGetTop(node),
        YGNodeLayoutGetWidth(node),
        YGNodeLayoutGetHeight(node)
    };
}

template<typename CharT, typename Traits>
std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const Rect& rect) {
    return os << "Rect(x: " << rect.x << ", y: " << rect.y << ", width: " << rect.width << ", height: " << rect.height << ")";
}

int main()
{
    YGNodeRef root = YGNodeNew();
    YGNodeStyleSetWidth(root, 500);
    YGNodeStyleSetHeight(root, 500);

    YGNodeRef child1 = YGNodeNew();
    YGNodeStyleSetWidth(child1, 100);
    YGNodeStyleSetHeight(child1, 100);
    YGNodeInsertChild(root, child1, 0);

    YGNodeRef child2 = YGNodeNew();
    YGNodeStyleSetWidth(child2, 100);
    YGNodeStyleSetHeight(child2, 100);
    YGNodeInsertChild(root, child2, 1);

    YGNodeCalculateLayout(root, YGUndefined, YGUndefined, YGDirectionLTR);

    std::cout << "Root layout: " << getNodeLayout(root) << std::endl;
    std::cout << "Child 1 layout: " << getNodeLayout(child1) << std::endl;
    std::cout << "Child 2 layout: " << getNodeLayout(child2) << std::endl;

    YGNodeFreeRecursive(root);
    return 0;
}