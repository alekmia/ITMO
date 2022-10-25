#include "pool.h"

void PoolAllocator::deallocate(const void * ptr)
{
    auto b_ptr = static_cast<const std::byte *>(ptr);
    for (std::size_t i = 0; i < storage.size(); i++) {
        const std::byte * begin = &storage[i].m_storage[0];
        std::less_equal<const std::byte *> cmp;
        if (cmp(begin, b_ptr) && cmp(b_ptr, &storage[i].m_storage.back())) {
            const std::size_t offset = (b_ptr - begin) / storage[i].obj_size;
            assert(((b_ptr - begin) % storage[i].obj_size) == 0);
            if (offset < storage[i].m_used_storage.size()) {
                storage[i].m_used_storage[offset] = false;
            }
        }
    }
}

void * PoolAllocator::allocate(const std::size_t n)
{
    for (std::size_t i = 0; i < storage.size(); i++) {
        if (n == storage[i].obj_size) {
            const auto pos = storage[i].find_empty_space();
            if (pos != Block::npos) {
                storage[i].m_used_storage[pos] = true;
                return &storage[i].m_storage[pos * storage[i].obj_size];
            }
        }
    }
    throw std::bad_alloc{};
}
PoolAllocator::PoolAllocator(const std::size_t block_size, std::initializer_list<std::size_t> sizes)
{
    storage.reserve(sizes.size());
    for (auto it : sizes) {
        storage.emplace_back(block_size, it);
    }
}
