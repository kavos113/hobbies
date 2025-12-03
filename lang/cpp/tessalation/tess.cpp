#include <vector>
#include <list>
#include <iostream>
#include <algorithm>

struct Point
{
    float x, y;
};

// det(b-a, c-a)
float det(const Point &a, const Point &b, const Point &c)
{
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

bool isConvex(const Point &a, const Point &b, const Point &c)
{
    return det(a, b, c) > 0;
}

bool isPointInTriangle(const Point &p, const Point &a, const Point &b, const Point &c)
{
    float p_ab = det(a, b, p);
    float p_bc = det(b, c, p);
    float p_ca = det(c, a, p);

    return (p_ab > 0 && p_bc > 0 && p_ca > 0) || (p_ab < 0 && p_bc < 0 && p_ca < 0);
}

std::vector<unsigned int> triangulate(const std::vector<Point> &polygon)
{
    std::vector<unsigned int> triangleIndices;

    if (polygon.size() < 3)
    {
        return triangleIndices;
    }

    if (polygon.size() == 3)
    {
        triangleIndices = {0, 1, 2};
        return triangleIndices;
    }

    std::list<unsigned int> indices;
    for (int i = 0; i < polygon.size(); i++)
    {
        indices.push_back(i);
    }

    // ensure counter clockwise order
    float singnedArea = 0;
    for (size_t i = 0; i < polygon.size(); i++)
    {
        size_t next = (i + 1) % polygon.size();
        singnedArea += det({0, 0}, polygon[i], polygon[next]);
    }
    if (singnedArea < 0)
    {
        std::reverse(indices.begin(), indices.end());
    }

    while (indices.size() > 3)
    {
        for (int i = 0; i < indices.size(); i++)
        {
            int prev = (i + indices.size() - 1) % indices.size();
            int curr = i;
            int next = (i + 1) % indices.size();

            unsigned int i_prev = *std::next(indices.begin(), prev);
            unsigned int i_curr = *std::next(indices.begin(), curr);
            unsigned int i_next = *std::next(indices.begin(), next);

            const Point &p_p = polygon[i_prev];
            const Point &p_c = polygon[i_curr];
            const Point &p_n = polygon[i_next];

            if (!isConvex(p_p, p_c, p_n))
            {
                continue;
            }

            for (int index : indices)
            {
                if (index == i_prev || index == i_curr || index == i_next)
                {
                    continue;
                }

                if (isPointInTriangle(polygon[index], p_p, p_c, p_n))
                {
                    continue;
                }
            }

            triangleIndices.push_back(i_prev);
            triangleIndices.push_back(i_curr);
            triangleIndices.push_back(i_next);
            indices.erase(std::next(indices.begin(), curr));
            break;
        }
    }

    if (indices.size() == 3)
    {
        triangleIndices.push_back(*indices.begin());
        triangleIndices.push_back(*std::next(indices.begin(), 1));
        triangleIndices.push_back(*std::next(indices.begin(), 2));
    }

    return triangleIndices;
}

int main()
{
    std::vector<Point> polygon = {
        {0, 0}, {0, 1}, {1, 1}, {1, 0}, {0.5, 0.5}};

    std::vector<unsigned int> triangles = triangulate(polygon);
    for (size_t i = 0; i < triangles.size(); i += 3)
    {
        std::cout << "Triangle: "
                  << polygon[triangles[i]].x << ", " << polygon[triangles[i]].y << " | "
                  << polygon[triangles[i + 1]].x << ", " << polygon[triangles[i + 1]].y << " | "
                  << polygon[triangles[i + 2]].x << ", " << polygon[triangles[i + 2]].y << "\n";
    }

    // clockwise
    std::vector<Point> polygonClockwise = {
        {0, 0}, {0.5, 0.5}, {1, 0}, {1, 1}, {0, 1}
    };
    std::vector<unsigned int> trianglesClockwise = triangulate(polygonClockwise);
    for (size_t i = 0; i < trianglesClockwise.size(); i += 3)
    {
        std::cout << "Triangle: "
                  << polygonClockwise[trianglesClockwise[i]].x << ", " << polygonClockwise[trianglesClockwise[i]].y << " | "
                  << polygonClockwise[trianglesClockwise[i + 1]].x << ", " << polygonClockwise[trianglesClockwise[i + 1]].y << " | "
                  << polygonClockwise[trianglesClockwise[i + 2]].x << ", " << polygonClockwise[trianglesClockwise[i + 2]].y << "\n";
    }  

    return 0;
}