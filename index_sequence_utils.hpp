#ifndef INDEX_SEQUENCE_UTILS
#define INDEX_SEQUENCE_UTILS

#include <utility>
#include "type_traits.hpp"

namespace index_seq_util
{
  template <typename IndexSeq1, typename IndexSeq2>
  struct concat;
  template <std::size_t...Is, std::size_t...Js>
  struct concat<std::index_sequence<Is...>, std::index_sequence<Js...>> { using type = std::index_sequence<Is..., Js...>; };
  template <typename IndexSeq1, typename IndexSeq2>
  using concat_t = typename concat<IndexSeq1, IndexSeq2>::type;

  template <typename IndexSeq, template <std::size_t> class Fn>
  struct for_each;
  template <std::size_t... Is, template <std::size_t> class Fn>
  struct for_each<std::index_sequence<Is...>, Fn> { using type = std::index_sequence<Fn<Is>::value...>; };
  template <typename IndexSeq, template <std::size_t> class Fn>
  using for_each_t = typename for_each<IndexSeq, Fn>::type;

  template <std::size_t I>
  struct add_meta
  {
    template <std::size_t J>
    struct fn { static constexpr std::size_t value = I + J; };
  };

  namespace detail
  {
    template <typename IndexSeq, std::size_t I>
    struct exist_impl;
    template <std::size_t...Is, std::size_t I, std::size_t J>
    struct exist_impl<std::index_sequence<I, Is...>, J>
      : std::conditional_t<I == J, type_traits::identity<std::true_type>, exist_impl<std::index_sequence<Is...>, J>>::type {};
    template <std::size_t J>
    struct exist_impl<std::index_sequence<>, J>: std::false_type {};
  }

  template <typename IndexSeq, std::size_t I>
  struct exist: detail::exist_impl<IndexSeq, I> {};

  template <typename IndexSeq, std::size_t>
  struct push_front;
  template <std::size_t...Is, std::size_t I>
  struct push_front<std::index_sequence<Is...>, I> { using type = std::index_sequence<I, Is...>; };
  template <typename IndexSeq, std::size_t I>
  using push_front_t = typename push_front<IndexSeq, I>::type;

  template <typename IndexSeq, std::size_t I>
  struct push_back;
  template <std::size_t...Is, std::size_t I>
  struct push_back<std::index_sequence<Is...>, I> { using type = std::index_sequence<Is..., I>; };
  template <typename IndexSeq, std::size_t I>
  using push_back_t = typename push_back<IndexSeq, I>::type;

  template <typename IndexSeq>
  struct head;
  template <std::size_t I, std::size_t...Is>
  struct head<std::index_sequence<I, Is...>> { static constexpr auto value = I; };
  template <>
  struct head<std::index_sequence<>> {};
  template <typename IndexSeq>
  constexpr std::size_t head_v = head<IndexSeq>::value;

  template <typename IndexSeq>
  struct tail;
  template <std::size_t I, std::size_t...Is>
  struct tail<std::index_sequence<I, Is...>> { using type = std::index_sequence<Is...>; };
  template <>
  struct tail<std::index_sequence<>> {};
  template <typename IndexSeq>
  using tail_t = typename tail<IndexSeq>::type;

  template <typename IndexSeq>
  struct size;
  template <std::size_t...Is>
  struct size<std::index_sequence<Is...>> { static constexpr auto value = sizeof...(Is); };
  template <typename IndexSeq>
  constexpr std::size_t size_v = size<IndexSeq>::value;
}

#endif
