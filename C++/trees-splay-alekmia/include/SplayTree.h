#pragma once

#include <vector>

class SplayTree
{
private:
    struct Node;
    mutable Node * root = nullptr;
    std::size_t treeSize = 0;

    static void splay(Node * node);
    static void rotate(Node * node);

    Node * findValue(int value) const;
    Node * generalizedFindValue(Node * node, int value) const;
    static void recursiveValues(std::vector<int> & ans, const Node * node);
    void merge(Node * node1, Node * node2) const;

public:
    bool contains(int value) const;
    bool insert(int value);
    bool remove(int value);

    std::size_t size() const;
    bool empty() const;

    std::vector<int> values() const;

    ~SplayTree();
};
