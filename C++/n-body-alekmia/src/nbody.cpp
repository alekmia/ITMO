#include "nbody.h"

#include <cmath>
#include <fstream>
#include <memory>
#include <tuple>

constexpr double G = 0.0000000000667;
constexpr double THETA = 0.5;
using Track = std::vector<Cartesian>;

using planet = std::tuple<Cartesian, Cartesian, double, std::string>;

std::istream & operator>>(std::istream & reader, planet & elem)
{
    reader >> std::get<0>(elem).x >> std::get<0>(elem).y >> std::get<1>(elem).x >> std::get<1>(elem).y >> std::get<2>(elem) >> std::get<3>(elem);
    return reader;
}

std::ostream & operator<<(std::ostream & writer, const planet & elem)
{
    writer << std::get<0>(elem).x << " " << std::get<0>(elem).y << " " << std::get<1>(elem).x << " " << std::get<1>(elem).y << " " << std::get<2>(elem) << " " << std::get<3>(elem);
    return writer;
}

inline int FastPositionTracker::which(const Body & insertable, const BHTreeNode & current)
{
    for (int i = 0; i < 4; i++) {
        if (current.children[i].quad.contains(insertable.coords)) {
            return i;
        }
    }
    return 0;
}

inline void FastPositionTracker::insert(const Body & insertable, BHTreeNode & root) // NOLINT
{
    BHTreeNode * current = &root;
    while (true) {
        if (current->free) {
            current->massCenter = insertable.coords;
            current->mass = insertable.m;
            current->insert(insertable);
            return;
        }
        if (!current->isLeaf) {
            current->update_force(insertable);
            current->massCenter = current->body->coords;
            current->mass = current->body->m;
            current = &current->children[which(insertable, *current)];
        }
        else {
            current->children[0] = current->quad.nw();
            current->children[1] = current->quad.ne();
            current->children[2] = current->quad.se();
            current->children[3] = current->quad.sw();
            current->isLeaf = false;
            BHTreeNode * currentTmp = &current->children[which(*current->body, *current)];
            insert(*current->body, *currentTmp);
        }
    }
}

BHTreeNode FastPositionTracker::buildTree(const std::vector<Body> & input)
{
    BHTreeNode root(Quadrant({0, 0}, universeSize), 0, {0, 0});
    for (const auto & body : input) {
        insert(body, root);
    }
    return root;
}

void BHTreeNode::update_force(const Body & b)
{
    this->body->plus(b);
}

FastPositionTracker::FastPositionTracker(const std::string & filename)
    : PositionTracker(filename)
{
}

void FastPositionTracker::dfs(Body & working, const BHTreeNode & root)
{
    if (root.free) {
        return;
    }
    else {
        if (root.isLeaf) {
            working.add_force(*root.body);
        }
        else if (working.distance(*root.body) != 0 && root.quad.length() / working.distance(*root.body) < THETA) {
            working.add_force(*root.body);
        }
        else {
            for (int i = 0; i < 4; i++) {
                dfs(working, root.children[i]);
            }
        }
    }
    return;
}

Track FastPositionTracker::track(const std::string & body_name, size_t end_time, size_t time_step)
{
    Track trackingDevice;
    int stepCount = end_time / time_step;
    std::size_t workingIndex = 0;
    for (std::size_t i = 0; i < input.size(); i++) {
        if (input[i].name == body_name) {
            workingIndex = i;
            break;
        }
    }
    Body & workingPlanet = input[workingIndex];

    BHTreeNode root;
    for (int i = 0; i < stepCount; i++) {
        root = buildTree(input);
        for (auto & body : input) {
            body.reset_force();
            dfs(body, root);
            body.update(time_step);
        }
        Cartesian now(workingPlanet.coords.x, workingPlanet.coords.y);
        trackingDevice.push_back(now);
    }

    return trackingDevice;
}

BasicPositionTracker::BasicPositionTracker(const std::string & filename)
    : PositionTracker(filename)
{
}
Track BasicPositionTracker::track(const std::string & body_name, size_t end_time, size_t time_step)
{
    Track trackingDevice;
    int stepCount = end_time / time_step;
    std::size_t workingIndex = 0;
    for (size_t i = 0; i < input.size(); i++) {
        if (input[i].name == body_name) {
            workingIndex = i;
            break;
        }
    }
    Body & workingPlanet = input[workingIndex];

    for (int i = 0; i < stepCount; i++) {
        for (auto & body : input) {
            body.reset_force();
            for (const auto & passiveBody : input) {
                body.add_force(passiveBody);
            }
            body.update(time_step);
        }
        Cartesian now(workingPlanet.coords.x, workingPlanet.coords.y);
        trackingDevice.emplace_back(now);
    }
    return trackingDevice;
}

PositionTracker::PositionTracker(const std::string & filename)
{
    std::ifstream fin(filename);
    fin >> universeSize;
    for (int i = 0; fin.good(); i++) {
        planet tmp;
        fin >> tmp;
        Body body(tmp);
        input.push_back(body);
    }
    if (!input.empty()) {
        input.pop_back();
    }
}

double Body::distanceX(const Body & b) const
{
    return b.coords.x - coords.x;
}
double Body::distanceY(const Body & b) const
{
    return b.coords.y - coords.y;
}

double Body::distance(const Body & b) // NOLINT
{
    return std::sqrt((distanceX(b) * distanceX(b)) + (distanceY(b) * distanceY(b)));
}
void Body::add_force(const Body & b)
{
    if (distance(b) != 0) {
        double Force = (G * m * b.m) / (distance(b) * distance(b));
        force.x += (Force * distanceX(b)) / distance(b);
        force.y += (Force * distanceY(b)) / distance(b);
    }
}

void Body::update(double delta_t)
{
    a = force / m;
    v += delta_t * a;
    coords += delta_t * v;
}
