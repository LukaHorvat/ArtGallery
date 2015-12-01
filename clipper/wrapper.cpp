#include "clipper.hpp"
#include <vector>
#include <iostream>

#define sll signed long long
using namespace std;
using namespace ClipperLib;

extern "C" double UnionArea(sll* buffer, int* sizes, int sizesSize)
{
    Clipper clipper;
    int counter = 0;
    for (int polyIdx = 0; polyIdx < sizesSize; ++polyIdx)
    {
        vector<IntPoint> poly;
        for (int i = 0; i < sizes[polyIdx]; ++i, ++counter)
        {
            poly.push_back(IntPoint(buffer[counter * 2], buffer[counter * 2 + 1]));
        }
        clipper.AddPath(poly, ptSubject, true);
    }
    Paths sol;
    clipper.Execute(ctUnion, sol, pftPositive, pftPositive);
    double sum = 0;
    for (int i = 0; i < sol.size(); ++i)
    {
        sum += Area(sol[i]);
    }
    return sum;
}

extern "C" int PointInPoly(sll* buffer, int size, sll x, sll y)
{
    vector<IntPoint> poly;
    for (int i = 0; i < size; ++i)
    {
        poly.push_back(IntPoint(buffer[i * 2], buffer[i * 2 + 1]));
    }
    return PointInPolygon(IntPoint(x, y), poly);
}

// int main()
// {
//     int buffer[] = {0, 0, 3, 0, 3, 1, 0, 1,
//                     0, 0, 1, 0, 1, 2, 2, 2, 2, 0, 3, 0, 3, 3, 0, 3};
//     int sizes[]  = {4, 8};
//     cout << UnionArea(buffer, sizes, 2) << endl;
//     return 0;
// }
