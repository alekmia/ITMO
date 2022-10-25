#include <cstring>
#include <fstream>
#include <functional>
#include <iostream>
#include <random>
#include <string>

std::pair<int, int> parse_i_args(std::string const & line, size_t offset)
{
    size_t first, second;
    auto pos = std::find(line.begin() + offset, line.end(), '-');
    first = std::stoi(line.substr(offset, pos - line.begin()));
    second = std::stoi(line.substr(pos - line.begin() + 1, line.length()));
    return {first, second};
}

size_t parse_h_args(std::string const & line, size_t offset)
{
    return std::stoi(line.substr(offset, line.length()));
}

int main(int argc, char * argv[])
{
    bool input_range = false;
    const std::string input_range_string = "--input-range=";
    const size_t i_offset = input_range_string.length();
    std::pair<int, int> input_ints = {0, 0};

    bool head_count = false;
    const std::string head_count_string = "--head-count=";
    const size_t h_offset = head_count_string.length();
    int head_int = 0;

    bool repeat = false;
    const std::string repeat_string = "--repeat";
    const size_t repeat_offset = repeat_string.length();

    const char * input_name = nullptr;

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (argv[i][1] != '-') {
                const size_t len = std::strlen(argv[i]);
                for (size_t j = 1; j < len; ++j) {
                    switch (argv[i][j]) {
                    case 'r':
                        repeat = true;
                        break;
                    case 'n':
                        head_count = true;
                        head_int = std::stoi(argv[++i]);
                        break;
                    case 'i':
                        input_ints = parse_i_args(argv[i + 1], 0);
                        i += 2;
                        input_range = true;
                        break;
                    }
                }
            }
            else {
                std::string line = argv[i];
                if (line.length() >= i_offset && (line.substr(0, i_offset) == input_range_string)) {
                    input_range = true;
                    input_ints = parse_i_args(line, i_offset);
                }
                else if (line.length() >= h_offset && (line.substr(0, h_offset) == head_count_string)) {
                    head_count = true;
                    head_int = parse_h_args(line, h_offset);
                }
                else if (line.length() >= repeat_offset && (line.substr(0, repeat_offset) == repeat_string)) {
                    repeat = true;
                }
            }
        }
        else {
            input_name = argv[i];
        }
    }

    int output_lines;

    if (input_name != nullptr) {
        std::ifstream fin(input_name);
        std::vector<std::string> lines;
        std::string line;
        while (getline(fin, line)) {
            lines.push_back(line);
        }
        output_lines = lines.size();
        std::random_shuffle(lines.begin(), lines.end());
        if (head_count) {
            output_lines = std::min(output_lines, head_int);
        }
        if (repeat) {
            std::random_device rnd_device;
            std::mt19937 rand_engine{rnd_device()};
            std::uniform_int_distribution<int> dist{0, output_lines - 1};

            auto gen = [&dist, &rand_engine]() {
                return dist(rand_engine);
            };
            while (!head_count) {
                std::vector<int> vec(output_lines);
                generate(begin(vec), end(vec), gen);
                std::cout << lines[vec[0]] << std::endl;
            }
            std::vector<int> vec(output_lines);
            generate(begin(vec), end(vec), gen);

            for (int i = 0; i < output_lines; i++) {
                std::cout << lines[vec[i]] << std::endl;
            }
        }
        else {
            for (int i = 0; i < output_lines; i++) {
                std::cout << lines[i] << std::endl;
            }
        }
    }
    else if (input_range && input_ints.first <= input_ints.second) {
        std::vector<int> ints;
        output_lines = input_ints.second - input_ints.first + 1;
        if (head_count) {
            output_lines = std::min(output_lines, head_int);
        }
        ints.resize(output_lines);
        std::iota(ints.begin(), ints.end(), input_ints.first);
        std::random_shuffle(ints.begin(), ints.end());
        for (int i = 0; i < output_lines; i++) {
            std::cout << ints[i] << std::endl;
        }
    }
}
