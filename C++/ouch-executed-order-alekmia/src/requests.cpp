#include "requests.h"

#include <algorithm>
#include <bitset>
#include <cstring>

namespace {
unsigned get_int(const std::vector<unsigned char> & message, size_t & start)
{
    unsigned value = 0;
    for (size_t i = start; i < start + 4; i++) {
        value = (value << 8) | message[i];
    }
    start += 4;
    return value;
}

void fill_mmt(const std::vector<unsigned char> & message, char * mmt, size_t & point, size_t mmt_length)
{
    memset(mmt, '-', 15);
    mmt[0] = '1';
    mmt[9] = 'P';
    mmt[14] = '\0';
    mmt[1] = message[point];
    mmt[2] = message[point + 1];
    mmt[10] = message[point + 2];
    point += mmt_length;
}

void fill_id(const std::vector<unsigned char> & message, std::string & cl_ord_id, size_t & point, size_t length)
{
    size_t i = point;
    while (message[i] != ' ' && i < point + length)
        i++;
    cl_ord_id = std::string(message.begin() + point, message.begin() + i);
    point += length;
}

BreakReason fill_break_reason(char i, size_t & offset, size_t length)
{
    offset += length;
    switch (i) {
    case 'C':
        return BreakReason::Consent;
    case 'X':
        return BreakReason::External;
    case 'E':
        return BreakReason::Erroneous;
    case 'S':
        return BreakReason::Supervisory;
    case 'U':
        return BreakReason::Unknown;
    }
    return BreakReason::Unknown;
}

std::string get_counterpart(const std::vector<unsigned char> & message, size_t & offset, size_t length)
{
    std::string ret = std::string(message.begin() + offset, message.begin() + offset + length);
    offset += 4;
    return ret;
}

} // namespace

ExecutionDetails decode_executed_order(const std::vector<unsigned char> & message)
{
    ExecutionDetails exec_details;
    // fill exec_details fields
    size_t id_length = 14;
    size_t mmt_length = 3;
    size_t counterpart_length = 4;
    size_t offset = 9;

    fill_id(message, exec_details.cl_ord_id, offset, id_length);

    exec_details.filled_volume = get_int(message, offset);

    exec_details.price = get_int(message, offset) / 1e4;
    offset += 1; // Continuous market trade

    exec_details.match_number = get_int(message, offset);

    exec_details.counterpart = get_counterpart(message, offset, counterpart_length);

    fill_mmt(message, exec_details.mmt, offset, mmt_length);

    std::bitset<8> bs(message[offset]);
    exec_details.self_trade = bs[7];
    exec_details.internalized = bs[5];

    int first = (bs[4]) ? 1 : 0;
    int second = (bs[3]) ? 1 : 0;
    exec_details.liquidity_indicator = LiquidityIndicator(!first ? (first << 1) + second + 1 : 0);

    return exec_details;
}

BrokenTradeDetails decode_broken_trade(const std::vector<unsigned char> & message)
{
    BrokenTradeDetails break_details;
    // fill break_details fields
    size_t id_length = 14;
    size_t reason_length = 1;
    size_t mmt_length = 3;
    size_t offset = 9;

    fill_id(message, break_details.cl_ord_id, offset, id_length);

    break_details.match_number = get_int(message, offset);

    break_details.reason = fill_break_reason(message[offset], offset, reason_length);

    fill_mmt(message, break_details.mmt, offset, mmt_length);

    return break_details;
}
