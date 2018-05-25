// make_single_header.cpp
//
// Copyright (c) 2018 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <deque>
#include <iostream>
#include <fstream>
#include <regex>
#include <string>
#include <functional>
#include <filesystem>

namespace fs = std::filesystem;

namespace {

constexpr auto& include_regex = R"(#include <(nanorange(?:/\w*)+.hpp)>)";

template <typename Rng, typename Value>
bool contains(const Rng& range, const Value& val)
{
    return std::find(range.begin(), range.end(), val) !=
           range.end();
}

struct include_processor {
    include_processor()
        : regex_(include_regex)
    {}

    std::string run(const fs::path& start_path)
    {
        start_path_ = start_path;
        start_path_.remove_filename();
        auto str = process_one(start_path);

        return str;
    }

private:
    struct replacement {
        std::ptrdiff_t pos;
        std::ptrdiff_t len;
        std::string text;
    };

    std::string process_one(const fs::path& path)
    {
        std::cout << "Processing path " << path << '\n';

        std::ifstream infile(path);
        std::string text;
        std::copy(std::istreambuf_iterator<char>{infile},
                  std::istreambuf_iterator<char>{},
                  std::back_inserter(text));
        text.shrink_to_fit();

        std::sregex_iterator iter(text.begin(), text.end(), regex_);
        std::sregex_iterator end;

        std::deque<replacement> replacements;

        for (; iter != end; ++iter) {
            const auto match = *iter;

            auto rep = replacement{match.position(), match.length()};

            auto new_path = start_path_;
            new_path = fs::canonical(new_path.append(match.str(1)));

            if (!contains(processed_paths_, new_path)) {
                rep.text = process_one(new_path);
            }

            replacements.push_back(rep);
        }

        process_replacements(text, replacements);
        processed_paths_.push_back(path);
        return text;
    }

    static void process_replacements(std::string& str, std::deque<replacement>& replacements)
    {
        std::sort(replacements.begin(), replacements.end(), [] (const auto& rep1, const auto& rep2) {
            return rep1.pos < rep2.pos;
        });

        while (!replacements.empty()) {
            auto rep = std::move(replacements[0]);
            replacements.pop_front();
            str.replace(rep.pos, rep.len, rep.text);

            for (auto& r : replacements) {
                r.pos = r.pos - rep.len + rep.text.length();
            }
        }
    }

    std::regex regex_;
    fs::path start_path_;
    std::vector<fs::path> processed_paths_;
};

}

int main(int argc, char** argv) try
{
    if (argc != 3) {
        std::cerr << "Usage: make_single_header IN_FILE.hpp OUT_FILE.hpp\n";
        return 1;
    }

    const auto infile_path = fs::canonical(fs::path(argv[1]));
    const auto outfile_path = fs::path(argv[2]);

    std::regex regex(include_regex);

    const auto out_str =  include_processor{}.run(infile_path);

    std::ofstream outfile(outfile_path);
    std::copy(out_str.begin(), out_str.end(),
              std::ostreambuf_iterator<char>(outfile));

} catch (const std::exception& ex) {
    std::cout << ex.what() << '\n';
}