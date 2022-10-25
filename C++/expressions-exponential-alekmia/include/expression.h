#pragma once

#include "exponential.h"

#include <map>
#include <ostream>
#include <string>

struct Expression
{
    virtual Exponential eval(std::map<std::string, Exponential> const & values = {}) const = 0;

    virtual Expression * clone() const = 0;

    friend std::ostream & operator<<(std::ostream & out, const Expression & expression);
};
