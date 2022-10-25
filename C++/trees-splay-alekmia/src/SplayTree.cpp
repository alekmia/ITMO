#include "SplayTree.h"

#include <iostream>
#include <limits>
#include <utility>

struct SplayTree::Node
{
    int data;
    Node * left = nullptr;
    Node * right = nullptr;
    Node * parent = nullptr;

public:
    explicit Node(int data)
        : data(data)
    {
    }

    ~Node()
    {
        delete left;
        delete right;
    }
};

bool SplayTree::contains(int value) const
{
    Node * node = findValue(value);

    return node != nullptr && node->data == value;
}

bool SplayTree::insert(int value)
{
    if (root == nullptr) {
        root = new Node(value);
        treeSize++;
        return true;
    }

    if (contains(value)) {
        return false;
    }

    Node * node = findValue(value);

    Node * newRoot = new Node(value);
    treeSize++;

    if (node == nullptr) {
        newRoot->right = root;
        root->parent = newRoot;
        root = newRoot;
        return true;
    }

    newRoot->left = node;
    newRoot->right = node->right;
    if (node->right != nullptr) {
        newRoot->right->parent = newRoot;
    }
    node->parent = newRoot;
    node->right = nullptr;
    root = newRoot;
    return true;
}

bool SplayTree::remove(int value)
{
    if (!contains(value)) {
        return false;
    }

    Node * newRoot = root;
    treeSize--;

    if (newRoot->left != nullptr) {
        newRoot->left->parent = nullptr;
    }
    if (newRoot->right != nullptr) {
        newRoot->right->parent = nullptr;
    }
    Node * leftNode = newRoot->left;
    Node * rightNode = newRoot->right;
    newRoot->left = nullptr;
    newRoot->right = nullptr;
    merge(leftNode, rightNode);
    delete newRoot;
    return true;
}

void SplayTree::merge(Node * leftNode, Node * rightNode) const
{
    if (rightNode == nullptr) {
        root = leftNode;
        return;
    }
    if (leftNode == nullptr) {
        root = rightNode;
        return;
    }
    Node * maxNode = generalizedFindValue(leftNode, std::numeric_limits<int>::max());
    splay(maxNode);
    maxNode->right = rightNode;
    rightNode->parent = maxNode;
    root = maxNode;
}

std::size_t SplayTree::size() const
{
    return treeSize;
}

bool SplayTree::empty() const
{
    return treeSize == 0;
}

std::vector<int> SplayTree::values() const
{
    std::vector<int> ans;
    ans.reserve(size());
    recursiveValues(ans, this->root);
    return ans;
}

void SplayTree::recursiveValues(std::vector<int> & ans, const Node * node)
{
    if (node != nullptr) {
        recursiveValues(ans, node->left);
        ans.push_back(node->data);
        recursiveValues(ans, node->right);
    }
}

SplayTree::~SplayTree()
{
    delete root;
}

void SplayTree::splay(Node * v)
{
    while (v->parent != nullptr) {
        if (v->parent->parent != nullptr) {
            if (v->parent == v->parent->parent->left) {
                rotate(v->parent);
            }
            else {
                rotate(v);
            }
        }
        rotate(v);
    }
}

void SplayTree::rotate(Node * node)
{
    Node * dad = node->parent;
    Node * grandDad = (dad == nullptr ? nullptr : dad->parent);
    if (grandDad != nullptr) {
        if (grandDad->left == dad) {
            grandDad->left = node;
        }
        else {
            grandDad->right = node;
        }
    }
    if (node == dad->left) {
        Node * son = node->right;
        node->right = dad;
        dad->left = son;
        if (dad->left != nullptr) {
            dad->left->parent = dad;
        }
    }
    else {
        Node * son = node->left;
        node->left = dad;
        dad->right = son;
        if (dad->right != nullptr) {
            dad->right->parent = dad;
        }
    }
    dad->parent = node;
    node->parent = grandDad;
}

SplayTree::Node * SplayTree::findValue(int value) const
{
    return generalizedFindValue(root, value);
}

SplayTree::Node * SplayTree::generalizedFindValue(Node * node, int value) const
{
    Node * ans = nullptr;
    while (node != nullptr && value != node->data) {
        if (value <= node->data) {
            if (node->left == nullptr) {
                break;
            }
            node = node->left;
        }
        else {
            ans = node;
            if (node->right == nullptr) {
                break;
            }
            node = node->right;
        }
    }
    if (node != nullptr && value == node->data) {
        ans = node;
    }
    if (ans) {
        splay(ans);
        root = ans;
    }
    return ans;
}
