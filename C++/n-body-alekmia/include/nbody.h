#pragma once

#include <cstdio>
#include <iostream>
#include <map>
#include <memory>
#include <string_view>
#include <tuple>
#include <vector>

struct Cartesian
{
    double x = 0;
    double y = 0;
    Cartesian() = default;
    Cartesian(double x, double y)
        : x(x)
        , y(y)
    {
    }

    Cartesian & operator+=(const Cartesian right)
    {
        *this = {x + right.x, y + right.y};
        return *this;
    }

    Cartesian operator/(double number) const
    {
        return {x / number, y / number};
    }
};

inline Cartesian operator*(double number, Cartesian pair)
{
    return {pair.x * number, pair.y * number};
}

using planet = std::tuple<Cartesian, Cartesian, double, std::string>;
using Track = std::vector<Cartesian>;

class Body;
// Quadrant representation, required for Problem 2
class Quadrant
{
    Cartesian center;
    double len;

public:
    // Create quadrant with center (x, y) and size 'lenth'
    Quadrant(Cartesian center, double length)
        : center(center)
        , len(length)
    {
    }

    // Test if point (x, y) is in the quadrant
    bool contains(Cartesian p) const
    {
        return (p.x >= center.x - len / 2 && p.x <= center.x + len / 2) &&
                (p.y >= center.y - len / 2 && p.y <= center.y + len / 2);
    }
    double length() const { return len; }

    // The four methods below construct new Quadrant representing sub-quadrant of the invoking quadrant
    Quadrant nw()
    {
        return Quadrant(Cartesian(center.x - len / 4, center.y - len / 4), len / 2);
    }
    Quadrant ne()
    {
        return Quadrant(Cartesian(center.x + len / 4, center.y - len / 4), len / 2);
    }
    Quadrant sw()
    {
        return Quadrant(Cartesian(center.x - len / 4, center.y + len / 4), len / 2);
    }
    Quadrant se()
    {
        return Quadrant(Cartesian(center.x + len / 4, center.y + len / 4), len / 2);
    }

    friend std::ostream & operator<<(std::ostream & out, const Quadrant & quad)
    {
        return out << "Quadrant with center {" << quad.center.x << ", " << quad.center.y << "} and length " << quad.len;
    }
};

// Single body representation, required for Problem 1 and Problem 2
class Body
{
private:
    Cartesian coords = Cartesian(0, 0);
    Cartesian a = Cartesian(0, 0);
    Cartesian force = Cartesian(0, 0);
    Cartesian v = Cartesian(0, 0);
    double m = 0;
    friend class BasicPositionTracker;
    friend class FastPositionTracker;
    std::string name;

public:
    Body(planet a)
        : coords({std::get<0>(a).x, std::get<0>(a).y})
        , v({std::get<1>(a).x, std::get<1>(a).y})
        , m(std::get<2>(a))
        , name(std::get<3>(a))
    {
    }

    double distance(const Body & b);
    double distanceX(const Body & b) const;
    double distanceY(const Body & b) const;

    // calculate the force-on current body by the 'b' and add the value to accumulated force value
    void add_force(const Body & b);
    // reset accumulated force value
    void reset_force() { force.x = 0, force.y = 0; }

    // update body's velocity and position
    void update(double delta_t);

    ////    friend std::ostream & operator<<(std::ostream &, const Body &);

    // The methods below to be done for Burnes-Hut algorithm only
    // Test if body is in quadrant
    bool in(const Quadrant q)
    {
        return q.contains(this->coords);
    }
    // Create new body representing center-of-mass of the invoking body and 'b'
    Body plus(const Body & b)
    {
        planet middle{{(coords.x * m + b.coords.x * b.m) / (m + b.m),
                       (coords.y * m + b.coords.y * b.m) / (m + b.m)},
                      {0, 0},
                      b.m + m,
                      "MiddleOf" + name + "And" + b.name};
        return Body(middle);
    }
};

// Burnes-Hut tree representation, required for Problem 2
class BHTreeNode
{
private:
    friend class FastPositionTracker;
    double mass = 0;
    Cartesian massCenter = {0, 0};
    std::vector<BHTreeNode> children;
    Quadrant quad = Quadrant({0, 0}, 0);
    std::shared_ptr<Body> body;

    bool free = true;
    bool isLeaf = true;

public:
    BHTreeNode() {}

    BHTreeNode(Quadrant quad)
        : quad(std::move(quad))
    {
        children.resize(4);
    }

    BHTreeNode(Quadrant quad, double mass)
        : mass(mass)
        , quad(std::move(quad))
    {
        children.resize(4);
    }

    BHTreeNode(Quadrant quad, double mass, Cartesian massCenter)
        : mass(mass)
        , massCenter(massCenter)
        , quad(std::move(quad))
    {
        children.resize(4);
    }

    void insert(Body b)
    {
        body = std::make_shared<Body>(std::move(b));
        free = false;
    }
    // Update net acting force-on 'b'
    void update_force(const Body & b);
};

class PositionTracker
{
protected:
    PositionTracker(const std::string & filename);
    std::vector<Body> input;
    double universeSize;

public:
    virtual ~PositionTracker() = default;
    virtual Track track(const std::string & body_name, size_t end_time, size_t time_step) = 0;
};

class BasicPositionTracker : public PositionTracker
{
public:
    ~BasicPositionTracker() = default;
    BasicPositionTracker(const std::string & filename);
    Track track(const std::string & body_name, size_t end_time, size_t time_step) override;
};

class FastPositionTracker : public PositionTracker
{
private:
    BHTreeNode buildTree(const std::vector<Body> & input);
    void insert(const Body & insertable, BHTreeNode & root);
    void dfs(Body & working, const BHTreeNode & root);
    int which(const Body & insertable, const BHTreeNode & current);

public:
    ~FastPositionTracker() = default;
    FastPositionTracker(const std::string & filename);
    Track track(const std::string & body_name, size_t end_time, size_t time_step) override;
};