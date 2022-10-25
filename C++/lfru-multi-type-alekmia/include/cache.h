#pragma once

#include <algorithm>
#include <cstddef>
#include <list>
#include <new>
#include <ostream>

template <class Key, class KeyProvider, class Allocator>
class Cache
{
public:
    template <class... AllocArgs>
    Cache(const std::size_t cache_size, AllocArgs &&... alloc_args)
        : m_max_top_size(cache_size)
        , m_max_low_size(cache_size)
        , m_alloc(std::forward<AllocArgs>(alloc_args)...)
    {
    }

    std::size_t size() const
    {
        return privilege_storage.size() + not_privilege_storage.size();
    }

    bool empty() const
    {
        return size() == 0;
    }

    template <class T>
    T & get(const Key & key);

    std::ostream & print(std::ostream & strm) const;

    friend std::ostream & operator<<(std::ostream & strm, const Cache & cache)
    {
        return cache.print(strm);
    }

private:
    const std::size_t m_max_top_size;
    const std::size_t m_max_low_size;
    Allocator m_alloc;
    std::list<KeyProvider *> privilege_storage;
    std::list<KeyProvider *> not_privilege_storage;
};

template <class Key, class KeyProvider, class Allocator>
template <class T>
inline T & Cache<Key, KeyProvider, Allocator>::get(const Key & key)
{
    auto it = std::find_if(privilege_storage.begin(), privilege_storage.end(), [&key](const KeyProvider * ptr) {
        return *ptr == key;
    });

    if (it != privilege_storage.end()) {
        privilege_storage.splice(privilege_storage.begin(), privilege_storage, it);
        return *static_cast<T *>(privilege_storage.front());
    }

    it = std::find_if(not_privilege_storage.begin(), not_privilege_storage.end(), [&key](const KeyProvider * ptr) {
        return *ptr == key;
    });

    if (it != not_privilege_storage.end()) {
        if (privilege_storage.size() == m_max_top_size) {
            not_privilege_storage.push_front(privilege_storage.back()); // <--
            privilege_storage.pop_back();
        }
        privilege_storage.splice(privilege_storage.begin(), not_privilege_storage, it);
        return *static_cast<T *>(privilege_storage.front());
    }
    else if (m_max_low_size == not_privilege_storage.size()) {
        m_alloc.template destroy<KeyProvider>(not_privilege_storage.back());
        not_privilege_storage.pop_back();
    }
    not_privilege_storage.push_front(m_alloc.template create<T>(key));
    return *static_cast<T *>(not_privilege_storage.front());
}

template <class Key, class KeyProvider, class Allocator>
inline std::ostream & Cache<Key, KeyProvider, Allocator>::print(std::ostream & strm) const
{
    return strm << "Priority: <empty>"
                << "\nRegular: <empty>"
                << "\n";
}
