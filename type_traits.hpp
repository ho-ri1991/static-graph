#ifndef TYPE_TRAITS_HPP
#define TYPE_TRAITS_HPP

namespace type_traits
{
  template <typename T>
  struct identity { using type = T; };

  template <typename Tpl, typename... Tpls>
  struct concat { using type = typename concat<Tpl, typename concat<Tpls...>::type>::type; };
  template <template <typename...> class Tpl, typename... Ts>
  struct concat<Tpl<Ts...>> { using type = Tpl<Ts...>; };
  template <template <typename...> class Tpl, typename... Ts, typename... Us>
  struct concat<Tpl<Ts...>, Tpl<Us...>> { using type = Tpl<Ts..., Us...>; };
  template <typename... Tpls>
  using concat_t = typename concat<Tpls...>::type;

  template <typename Tpl>
  struct head;
  template <template <typename...> class Tpl, typename T, typename...Ts>
  struct head<Tpl<T, Ts...>> { using type = T; };
  template <template <typename...> class Tpl>
  struct head<Tpl<>> {};
  template <typename Tpl>
  using head_t = typename head<Tpl>::type;

  template <typename Tpl>
  struct tail;
  template <template <typename...> class Tpl, typename T, typename...Ts>
  struct tail<Tpl<T, Ts...>> { using type = Tpl<Ts...>; };
  template <template <typename...> class Tpl>
  struct tail<Tpl<>> {};
  template <typename Tpl>
  using tail_t = typename tail<Tpl>::type;

  template <typename Tpl>
  struct last: last<tail_t<Tpl>> {};
  template <template <typename...> class Tpl, typename T>
  struct last<Tpl<T>> { using type = T; };
  template <typename Tpl>
  using last_t = typename last<Tpl>::type;

  template <template <typename...> class Class, typename T>
  struct is_match_template: std::false_type {};
  template <template <typename...> class Class, typename... Ts>
  struct is_match_template<Class, Class<Ts...>>: std::true_type {};
  template <template <typename...> class Class, typename T>
  constexpr bool is_match_template_v = is_match_template<Class, T>::value;

  template <typename Tpl>
  struct size;
  template <template <typename...> class Tpl, typename...Ts>
  struct size<Tpl<Ts...>> { static constexpr std::size_t value = sizeof...(Ts); };
  template <typename Tpl>
  constexpr std::size_t size_v = size<Tpl>::value;

  template <typename Tpl, typename T>
  struct push_back;
  template <template <typename...> class Tpl, typename... Ts, typename T>
  struct push_back<Tpl<Ts...>, T> { using type = Tpl<Ts..., T>; };
  template <typename Tpl, typename T>
  using push_back_t = typename push_back<Tpl, T>::type;

  template <typename Tpl, typename T>
  struct push_front;
  template <template <typename...> class Tpl, typename... Ts, typename T>
  struct push_front<Tpl<Ts...>, T> { using type = Tpl<T, Ts...>; };
  template <typename Tpl, typename T>
  using push_front_t = typename push_front<Tpl, T>::type;

  template <typename Tpl, template <typename> class Fn>
  struct for_each;
  template <template <typename...> class Tpl, typename... Ts, template <typename> class Fn>
  struct for_each<Tpl<Ts...>, Fn> { using type = Tpl<typename Fn<Ts>::type...>; };
  template <typename Tpl, template <typename> class Fn>
  using for_each_t = typename for_each<Tpl, Fn>::type;

}

#endif
