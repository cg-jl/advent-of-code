#include <iostream>
#include <string>
#include <fstream>
#include <functional>
#include <optional>
#include <string_view>


namespace util {
#ifdef UTIL_INPUT_READER
template <typename T>
class InputReader {
    std::ifstream input;
    std::function<T(std::string_view&&)> m_line_parser;
    std::string linebuf;

   public:
    class iterator {
        InputReader<T>& m_reader;
        bool m_is_end = false;
        std::string m_line = "";

       public:
        iterator(InputReader<T>& reader, std::string_view line, bool is_end = false)
            : m_reader(reader), m_is_end(is_end), m_line(line) {}
        iterator& operator++() {
            if (m_is_end) return *this;  // do not try to advance
            const auto maybe_next_line = m_reader.current_line();
            if (!maybe_next_line)
                m_is_end = true;
            else {
                m_line = *maybe_next_line;
            }
            return *this;
        }
        /// Parses the current line with the given function
        T operator*() { return m_reader.parse(m_line); }
        bool operator!=(const iterator& other) {
            // this operator is only going to be used in order to check
            return !(m_is_end ? other.m_is_end : false);
        }
        bool operator!=(const iterator& other) { return !(*this == other);}
    };

    InputReader(std::string_view filename,
                std::function<T(std::string_view)> line_parser)
        : input(filename.data()), m_line_parser(line_parser) {}

    InputReader(std::ifstream&& stream,
                std::function<T(std::string_view)> line_parser)
        : m_input(stream), m_line_parser(line_parser) {}
    T parse(std::string_view line) { return m_line_parser(line); }
    std::optional<std::string_view> current_line() {
        if (std::getline(input, linebuf)) {
            return {std::string_view(linebuf)};
        } else {
            return {};
        }
    }

    iterator begin() { 
        const auto maybe_line = current_line();
        if (!maybe_line) {
            return end(); // already consumed
        } else {
            return iterator(*this, *maybe_line);
        }
     }
    iterator end() { return iterator(*this, "", true); }
};
#endif
#ifdef UTIL_FILE_TO_STRING
auto load_file_to_string(std::string_view filename) -> std::string {
    std::ifstream stream(filename.data());
    std::string total;
    std::string line;
    while (std::getline(stream, line)) {
        if (!total.empty()) total.push_back('\n');
        total += line;
    }
    return total;
}
#endif
}  // namespace util
