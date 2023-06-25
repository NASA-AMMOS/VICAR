/* Lightweight C++ implementation of a PigPoint2D */

#ifndef _PIGPOINT2D_C_H_
#define _PIGPOINT2D_C_H_

class PigPoint2D {
    private:
    double _x, _y;

    public:
    PigPoint2D () { _x = 0.0; _y = 0.0; }
    PigPoint2D (double x, double y) { _x = x; _y = y; }
    double getX() { return _x; }
    double getY() { return _y; }

    double getSample() { return _x; }
    double getLine() { return _y; }

};

#endif
