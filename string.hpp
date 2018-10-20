#ifndef STRING_HPP
#define STRING_HPP

#include <utility>
#include <type_traits>

namespace utility
{
  template <std::size_t N>
  class string
  {
  private:
    char elems[N] = {};
  public:
    template <std::size_t... indexes>
    constexpr string(const char (&arr)[N], const std::index_sequence<indexes...>) noexcept : elems{arr[indexes]...} {}
    constexpr string(const char (&arr)[N]) noexcept : string(arr, std::make_index_sequence<N>{}) {}
    constexpr string() noexcept {}
  public:
    constexpr const char* c_str() const { return elems; }
    constexpr char& operator[](std::size_t n) { return elems[n]; }
    constexpr const char& operator[](std::size_t n) const { return elems[n]; }
    template <std::size_t M>
    constexpr bool operator==(const string<M>& other) const noexcept
    {
      if(N != M) return false;
      else {
        for(std::size_t i = 0; i < N; ++i) {
          if(elems[i] != other.elems[i])return false;
        }
        return true;
      }
    }
    template <std::size_t M>
    constexpr bool operator==(const char (&arr)[M]) const noexcept
    {
      if(N != M) return false;
      else {
        for(std::size_t i = 0; i < N; ++i) {
          if(elems[i] != arr[i])return false;
        }
        return true;
      }
    }
  };

  template <std::size_t N, std::size_t M>
  constexpr bool operator==(const char(&arr)[N], const string<M>& str) noexcept
  {
    return str == arr;
  }

  template <std::size_t N>
  constexpr auto to_string(const char(&arr)[N]) noexcept { return string<N>(arr); }
}

#endif
