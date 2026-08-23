#include <yoga/Yoga.h>

#include <iostream>

struct Rect {
    float x, y, width, height;
};

template<typename CharT, typename Traits>
std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os, const Rect& rect) {
    return os << "Rect(x: " << rect.x << ", y: " << rect.y << ", width: " << rect.width << ", height: " << rect.height << ")";
}

class Child
{
public:
    explicit Child(float width, float height)
    {
        m_node = YGNodeNew();
        YGNodeStyleSetWidth(m_node, width);
        YGNodeStyleSetHeight(m_node, height);

        m_width = width;
        m_height = height;
    }

    Rect layout() const
    {
        return {
            YGNodeLayoutGetLeft(m_node),
            YGNodeLayoutGetTop(m_node),
            YGNodeLayoutGetWidth(m_node),
            YGNodeLayoutGetHeight(m_node)
        };
    }

    float m_width;
    float m_height;

    YGNodeRef m_node;
};

class Parent
{
public:
    explicit Parent(float width, float height)
    {
        m_node = YGNodeNew();
        YGNodeStyleSetWidth(m_node, width);
        YGNodeStyleSetHeight(m_node, height);
    }

    Rect layout() const
    {
        return {
            YGNodeLayoutGetLeft(m_node),
            YGNodeLayoutGetTop(m_node),
            YGNodeLayoutGetWidth(m_node),
            YGNodeLayoutGetHeight(m_node)
        };
    }

    void free()
    {
        YGNodeFreeRecursive(m_node);
        m_node = nullptr;
    }

    void calc() const
    {
        YGNodeCalculateLayout(m_node, YGUndefined, YGUndefined, YGDirectionLTR);
    }

    YGNodeRef m_node;

    void add(const Child& child)
    {
        YGNodeInsertChild(m_node, child.m_node, m_childCount);
        m_childCount++;
    }

private:
    int m_childCount = 0;
};

static YGSize staticMeasureFunc(
    YGNodeRef node,
    float width,
    YGMeasureMode widthMode,
    float height,
    YGMeasureMode heightMode
)
{
    std::cout << "requested size: " << width << ", " << height << std::endl;
    auto *child = static_cast<Child*>(YGNodeGetContext(node));
    return {child->m_width, child->m_height};
}

int main()
{
    Parent root(200, 200);
    Child child(300, 100);

    YGNodeStyleSetFlexDirection(root.m_node, YGFlexDirectionColumn);
    YGNodeSetContext(child.m_node, &child);
    YGNodeSetMeasureFunc(child.m_node, reinterpret_cast<YGMeasureFunc>(staticMeasureFunc));
    YGNodeStyleSetFlexShrink(child.m_node, 1);
    YGNodeMarkDirty(child.m_node);

    root.add(child);
    root.calc();

    std::cout << "Root layout:  " << root.layout() << std::endl;
    std::cout << "Child layout: " << child.layout() << std::endl;

    root.free();
    return 0;
}