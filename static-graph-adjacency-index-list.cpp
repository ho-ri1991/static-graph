#include <type_traits>
#include <tuple>
#include <array>
#include "type_traits.hpp"
#include "string.hpp"
#include "index_sequence_utils.hpp"

namespace static_graph
{
  template <typename Edges, typename HeadIndexSeq, typename LastIndexSeq, typename NodeTpl>
  struct graph;

  template <typename... EdgeIndexSeqs, std::size_t... HeadIndexes, std::size_t... LastIndexes, typename... Nodes>
  struct graph<
    std::tuple<EdgeIndexSeqs...>,
    std::index_sequence<HeadIndexes...>,
    std::index_sequence<LastIndexes...>,
    std::tuple<Nodes...>
  >
  {
    static_assert(sizeof...(EdgeIndexSeqs) == sizeof...(Nodes));
    using edges_t = std::tuple<EdgeIndexSeqs...>;
    using heads_t = std::index_sequence<HeadIndexes...>;
    using lasts_t = std::index_sequence<LastIndexes...>;
    using nodes_t = std::tuple<Nodes...>;
    static constexpr auto size = std::tuple_size_v<nodes_t>;
    nodes_t nodes;
  };

  template <typename Graph>
  struct is_graph: std::conditional_t<type_traits::is_match_template_v<graph, Graph>, std::true_type, std::false_type> {};
  template <typename Graph>
  constexpr bool is_graph_v = is_graph<Graph>::value;

  namespace detail
  {
    template <std::size_t I, typename Tpl, typename IndexSeq, template <typename> class Fn>
    struct apply_only_impl;
    template <std::size_t I, template <typename...> class Tpl, typename T, typename...Ts, std::size_t...Is, template <typename> class Fn>
    struct apply_only_impl<I, Tpl<T, Ts...>, std::index_sequence<Is...>, Fn>
    {
      using filter_index_seq = std::index_sequence<Is...>;
      using result = typename std::conditional_t<index_seq_util::exist<filter_index_seq, I>::value, Fn<T>, type_traits::identity<T>>::type;
      using type = type_traits::push_front_t<typename apply_only_impl<I + 1, Tpl<Ts...>, filter_index_seq, Fn>::type, result>;
    };
    template <std::size_t I, template <typename...> class Tpl, std::size_t...Is, template <typename> class Fn>
    struct apply_only_impl<I, Tpl<>, std::index_sequence<Is...>, Fn> { using type = Tpl<>; };
  }

  template <typename Tpl, typename FilterIndexSeq, template <typename> class Fn>
  struct apply_only: detail::apply_only_impl<0, Tpl, FilterIndexSeq, Fn> {};
  template <typename Tpl, typename FilterIndexSeq, template <typename> class Fn>
  using apply_only_t = typename apply_only<Tpl, FilterIndexSeq, Fn>::type;

  template <typename IndexSeq1>
  struct concat_meta 
  {
    template <typename IndexSeq2>
    struct fn { using type = index_seq_util::concat_t<IndexSeq2, IndexSeq1>; };
  };

  template <std::size_t I>
  struct add_all_meta
  {
    template <typename IndexSeq>
    struct fn { using type = index_seq_util::for_each_t<IndexSeq, index_seq_util::add_meta<I>::template fn>; };
  };

  template <typename Graph1, typename Graph2>
  constexpr auto operator+(Graph1&& graph1, Graph2&& graph2)
  {
    using graph1_t = std::decay_t<Graph1>;
    using graph2_t = std::decay_t<Graph2>;
    if constexpr (is_graph_v<graph1_t> && is_graph_v<graph2_t>)
    {
      using graph1_nodes = typename graph1_t::nodes_t;
      using graph2_nodes = typename graph2_t::nodes_t;
      using graph1_edges = typename graph1_t::edges_t;
      using graph2_edges = typename graph2_t::edges_t;
      using graph2_heads = typename graph2_t::heads_t;
      using graph1_lasts = typename graph1_t::lasts_t;
      using nodes_t = type_traits::concat_t<graph1_nodes, graph2_nodes>;
      using heads_t = typename graph1_t::heads_t;
      using lasts_t = index_seq_util::for_each_t<
          typename graph2_t::lasts_t, 
          index_seq_util::add_meta<type_traits::size_v<graph1_nodes>>::template fn
        >;
      using graph2_shifted_heads = index_seq_util::for_each_t<graph2_heads, index_seq_util::add_meta<type_traits::size_v<graph1_nodes>>::template fn>;
      using edges_t = type_traits::concat_t<
          apply_only_t<graph1_edges, graph1_lasts, concat_meta<graph2_shifted_heads>::template fn>,
          type_traits::for_each_t<graph2_edges, add_all_meta<type_traits::size_v<graph1_nodes>>::template fn>
        >;
      return graph<edges_t, heads_t, lasts_t, nodes_t>{
        std::tuple_cat(std::forward<Graph1>(graph1).nodes, std::forward<Graph2>(graph2).nodes)
      };
    }
    else if constexpr (!is_graph_v<graph1_t> && is_graph_v<graph2_t>)
    {
      using graph2_nodes = typename graph2_t::nodes_t;
      using graph2_heads = typename graph2_t::heads_t;
      using graph2_lasts = typename graph2_t::lasts_t;
      using graph2_edges = typename graph2_t::edges_t;
      using nodes_t = type_traits::push_front_t<graph2_nodes, graph1_t>;
      using heads_t = std::index_sequence<0>;
      using lasts_t = index_seq_util::for_each_t<graph2_lasts, index_seq_util::add_meta<1>::template fn>;
      using edges_t = type_traits::push_front_t<
          type_traits::for_each_t<graph2_edges, add_all_meta<1>::fn>,
          index_seq_util::for_each_t<graph2_heads, index_seq_util::add_meta<1>::fn>
        >;
      return graph<edges_t, heads_t, lasts_t, nodes_t>{
        std::tuple_cat(std::make_tuple(std::forward<Graph1>(graph1)), std::forward<Graph2>(graph2).nodes)
      };
    }
    else if constexpr (is_graph_v<graph1_t> && !is_graph_v<graph2_t>)
    {
      using graph1_nodes = typename graph1_t::nodes_t;
      using graph1_edges = typename graph1_t::edges_t;
      using graph1_lasts = typename graph1_t::lasts_t;
      using nodes_t = type_traits::push_back_t<graph1_nodes, graph2_t>;
      using heads_t = typename graph1_t::heads_t;
      using lasts_t = std::index_sequence<type_traits::size_v<graph1_nodes>>;
      using edges_t = type_traits::push_back_t<
          apply_only_t<graph1_edges, graph1_lasts, concat_meta<std::index_sequence<type_traits::size_v<graph1_nodes>>>::template fn>,
          std::index_sequence<>
        >;
      return graph<edges_t, heads_t, lasts_t, nodes_t>{
        std::tuple_cat(std::forward<Graph1>(graph1).nodes, std::make_tuple(std::forward<Graph2>(graph2)))
      };
    }
    else
    {
      using edges_t = std::tuple<std::index_sequence<1>, std::index_sequence<>>;
      using heads_t = std::index_sequence<0>;
      using lasts_t = std::index_sequence<1>;
      using nodes_t = std::tuple<graph1_t, graph2_t>;
      return graph<edges_t, heads_t, lasts_t, nodes_t>{std::make_tuple(std::forward<Graph1>(graph1), std::forward<Graph2>(graph2))};
    }
  }

  template <typename Graph1, typename Graph2>
  constexpr auto operator|(Graph1&& graph1, Graph2&& graph2)
  {
    using graph1_t = std::decay_t<Graph1>;
    using graph2_t = std::decay_t<Graph2>;
    if constexpr (is_graph_v<graph1_t> && is_graph_v<graph2_t>)
    {
      using graph1_edges = typename graph1_t::edges_t;
      using graph2_edges = typename graph2_t::edges_t;
      using graph1_nodes = typename graph1_t::nodes_t;
      using graph2_nodes = typename graph2_t::nodes_t;
      using graph1_heads = typename graph1_t::heads_t;
      using graph2_heads = typename graph2_t::heads_t;
      using graph1_lasts = typename graph1_t::lasts_t;
      using graph2_lasts = typename graph2_t::lasts_t;
      constexpr auto graph1_size = type_traits::size_v<graph1_nodes>;
      using edges_t = type_traits::concat_t<
          graph1_edges,
          type_traits::for_each_t<graph2_edges, add_all_meta<graph1_size>::template fn>
        >;
      using heads_t = index_seq_util::concat_t<
          graph1_heads,
          index_seq_util::for_each_t<graph2_heads, index_seq_util::add_meta<graph1_size>::template fn>
        >;
      using lasts_t = index_seq_util::concat_t<
          graph1_lasts,
          index_seq_util::for_each_t<graph2_lasts, index_seq_util::add_meta<graph1_size>::template fn>
        >;
      using nodes_t = type_traits::concat_t<graph1_nodes, graph2_nodes>;
      return graph<edges_t, heads_t, lasts_t, nodes_t>{std::tuple_cat(std::forward<Graph1>(graph1).nodes, std::forward<Graph2>(graph2).nodes)};
    }
    else if constexpr (is_graph_v<graph1_t> && !is_graph_v<graph2_t>)
    {
      using graph1_edges = typename graph1_t::edges_t;
      using graph1_nodes = typename graph1_t::nodes_t;
      using graph1_heads = typename graph1_t::heads_t;
      using graph1_lasts = typename graph1_t::lasts_t;
      constexpr auto graph1_size = type_traits::size_v<graph1_nodes>;
      using edges_t = type_traits::push_back_t<graph1_edges, std::index_sequence<>>;
      using heads_t = index_seq_util::push_back_t<graph1_heads, graph1_size>;
      using lasts_t = index_seq_util::push_back_t<graph1_lasts, graph1_size>;
      using nodes_t = type_traits::push_back_t<graph1_nodes, graph2_t>;
      return graph<edges_t, heads_t, lasts_t, nodes_t>{std::tuple_cat(std::forward<Graph1>(graph1).nodes, std::make_tuple(std::forward<Graph2>(graph2)))};
    }
    else if constexpr (!is_graph_v<graph1_t> && is_graph_v<graph2_t>)
    {
      using graph2_edges = typename graph2_t::edges_t;
      using graph2_nodes = typename graph2_t::nodes_t;
      using graph2_heads = typename graph2_t::heads_t;
      using graph2_lasts = typename graph2_t::lasts_t;
      using edges_t = type_traits::push_front_t<
          type_traits::for_each_t<graph2_edges, add_all_meta<1>::template fn>,
          std::index_sequence<>
        >;
      using heads_t = index_seq_util::push_front_t<
          index_seq_util::for_each_t<graph2_heads, index_seq_util::add_meta<1>::template fn>, 0
        >;
      using lasts_t = index_seq_util::push_front_t<
          index_seq_util::for_each_t<graph2_lasts, index_seq_util::add_meta<1>::template fn>, 0
        >;
      using nodes_t = type_traits::push_front_t<graph2_nodes, graph1_t>;
      return graph<edges_t, heads_t, lasts_t, nodes_t>{std::tuple_cat(std::make_tuple(std::forward<Graph1>(graph1)), std::forward<Graph2>(graph2).nodes)};
    }
    else
    {
      using edges_t = std::tuple<std::index_sequence<>, std::index_sequence<>>;
      using heads_t = std::index_sequence<0, 1>;
      using lasts_t = std::index_sequence<0, 1>;
      using nodes_t = std::tuple<graph1_t, graph2_t>;
      return graph<edges_t, heads_t, lasts_t, nodes_t>{std::make_tuple(std::forward<Graph1>(graph1), std::forward<Graph2>(graph2))};
    }
  }

  struct single_node {};
}

template <char c>
struct node: static_graph::single_node { static constexpr char value = c; };

template <std::size_t I, typename Graph, std::size_t N>
constexpr std::size_t constexpr_dfs_impl(Graph&& graph, std::array<bool, std::decay_t<Graph>::size>& is_visit, utility::string<N>& str, std::size_t index);

template <typename IndexSeq, typename Graph, std::size_t N>
constexpr std::size_t constexpr_dfs_impl_helper(Graph&& graph, std::array<bool, std::decay_t<Graph>::size>& is_visit, utility::string<N>& str, std::size_t index)
{
  static_assert(std::decay_t<Graph>::size <= N);
  if constexpr (index_seq_util::size_v<IndexSeq> > 0)
  {
    auto i = constexpr_dfs_impl<index_seq_util::head_v<IndexSeq>>(graph, is_visit, str, index);
    auto res = constexpr_dfs_impl_helper<index_seq_util::tail_t<IndexSeq>>(graph, is_visit, str, i);
    return res;
  }
  return index;
}

template <std::size_t I, typename Graph, std::size_t N>
constexpr std::size_t constexpr_dfs_impl(Graph&& graph, std::array<bool, std::decay_t<Graph>::size>& is_visit, utility::string<N>& str, std::size_t index)
{
  using graph_t = std::decay_t<Graph>;
  static_assert(graph_t::size <= N);
  using nexts_t = std::tuple_element_t<I, typename graph_t::edges_t>;
  if (is_visit[I])
    return index;
  str[index] = std::get<I>(graph.nodes).value;
  is_visit[I] = true;
  auto res = constexpr_dfs_impl_helper<nexts_t>(graph, is_visit, str, index + 1);
  return res;
}

template <typename Graph>
constexpr auto constexpr_dfs(Graph&& graph)
{
  using graph_t = std::decay_t<Graph>;
  utility::string<graph_t::size + 1> str{};
  std::array<bool, graph_t::size> is_visit{};
  constexpr_dfs_impl_helper<typename graph_t::heads_t>(graph, is_visit, str, 0);
  return str;
}

template <typename IndexQueue, typename Graph, std::size_t N>
constexpr auto constexpr_bfs_helper(Graph&& graph, std::array<bool, std::decay_t<Graph>::size>& is_visit, utility::string<N>& str, std::size_t index)
{
  using graph_t = std::decay_t<Graph>;
  if constexpr (index_seq_util::size_v<IndexQueue> == 0)
    return index;
  else
  {
    constexpr auto I = index_seq_util::head_v<IndexQueue>;
    using queue_tail = index_seq_util::tail_t<IndexQueue>;
    if (is_visit[I])
    {
      return constexpr_bfs_helper<queue_tail>(graph, is_visit, str, index);
    }
    else
    {
      is_visit[I] = true;
      str[index] = std::get<I>(graph.nodes).value;
      using queue_next = index_seq_util::concat_t<queue_tail, std::tuple_element_t<I, typename graph_t::edges_t>>;
      return constexpr_bfs_helper<queue_next>(graph, is_visit, str, index + 1);
    }
  }
}

template <typename HeadIndexSeq, typename Graph, std::size_t N>
constexpr void constexpr_bfs_init(Graph&& graph, std::array<bool, std::decay_t<Graph>::size>& is_visit, utility::string<N>& str, std::size_t index = 0)
{
  if constexpr (index_seq_util::size_v<HeadIndexSeq> > 0)
  {
    auto i = constexpr_bfs_helper<std::index_sequence<index_seq_util::head_v<HeadIndexSeq>>>(graph, is_visit, str, index);
    constexpr_bfs_init<index_seq_util::tail_t<HeadIndexSeq>>(graph, is_visit, str, i);
  }
}

template <typename Graph>
constexpr auto constexpr_bfs(Graph&& graph)
{
  using graph_t = std::decay_t<Graph>;
  utility::string<graph_t::size + 1> str{};
  std::array<bool, graph_t::size> is_visit{};
  constexpr_bfs_init<typename graph_t::heads_t>(graph, is_visit, str);
  return str;
}

template <char c>
struct [[deprecated]] PRINT_CHAR {};

template <typename T>
struct PRINT_TYPE;

int main()
{
  constexpr auto n1 = node<'a'>{} + node<'b'>{} + node<'c'>{};
  constexpr auto n2 = node<'d'>{} + node<'e'>{} + (node<'f'>{} | node<'g'>{});
  constexpr auto n3 = (node<'h'>{} | node<'i'>{}) + node<'j'>{};
  constexpr auto n4 = n1 + (n2 | node<'k'>{} | node<'l'>{}) + n3;
  constexpr auto ans = constexpr_dfs(n4);
//  constexpr auto ans = constexpr_bfs(n4);
  PRINT_CHAR<ans[0]> p0;
  PRINT_CHAR<ans[1]> p1;
  PRINT_CHAR<ans[2]> p2;
  PRINT_CHAR<ans[3]> p3;
  PRINT_CHAR<ans[4]> p4;
  PRINT_CHAR<ans[5]> p5;
  PRINT_CHAR<ans[6]> p6;
  PRINT_CHAR<ans[7]> p7;
  PRINT_CHAR<ans[8]> p8;
  PRINT_CHAR<ans[9]> p9;
  PRINT_CHAR<ans[10]> p10;
  PRINT_CHAR<ans[11]> p11;
  static_assert(ans == "abcdefhjigkl");
//  static_assert(ans == "abcdklehifgj");
  return 0;
}

