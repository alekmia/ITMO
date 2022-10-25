#pragma once

#include <algorithm>
#include <cassert>
#include <functional>
#include <initializer_list>
#include <new>
#include <vector>

class PoolAllocator
{
private:
    class Block
    {
    public:
        static constexpr std::size_t npos = static_cast<std::size_t>(-1);

        std::vector<std::byte> m_storage;
        std::vector<bool> m_used_storage;
        std::size_t block_size;
        std::size_t obj_size;

        Block(std::size_t block_size, std::size_t obj_size)
            : m_storage(block_size)
            , m_used_storage(block_size / obj_size)
            , block_size(block_size)
            , obj_size(obj_size)
        {
        }

        std::size_t find_empty_space() const
        {
            auto it = std::find(m_used_storage.begin(), m_used_storage.end(), false);
            return it == m_used_storage.end() ? npos : std::distance(m_used_storage.begin(), it);
        }
    };

public:
    std::vector<Block> storage;

    PoolAllocator(std::size_t block_size, std::initializer_list<std::size_t> sizes);

    void * allocate(std::size_t n);

    void deallocate(const void * ptr);
};
