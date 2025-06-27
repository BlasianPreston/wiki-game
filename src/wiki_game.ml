open! Core

module Article = struct
  type t =
    { name : String.t
    ; url : String.t
    }
  [@@deriving compare, sexp, hash, equal]

  let vertex_name t = t.name
  let create name = { name; url = "https://en.wikipedia.org/" ^ name }
end

(* We separate out the [Network] module to represent our social network in OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where a connection
     represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = Article.t * Article.t [@@deriving compare, sexp, hash, equal]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  (* Make a function that gets all the nearest neighbors to the specified node *)
  (* Continue through each value in the set and add visited nodes to the visited set *)
  (* Run the nearest neighbors on the nodes that are connected to the source node *)
  (* Continue this until the queue is empty which will signal that all neighbors have been found*)
end

module G = Graph.Imperative.Graph.Concrete (Article)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None

    let vertex_attributes v =
      [ `Shape `Box; `Label (Article.vertex_name v); `Fillcolor 1000 ]
    ;;

    let vertex_name v = Article.vertex_name v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)

let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.map ~f:(fun link -> R.attribute "href" link |> String.strip)
    (* Get all of the links from the href attributes *)
  |> List.filter ~f:(fun link -> String.is_prefix ~prefix:"/wiki/" link)
  |> List.map ~f:(fun str -> String.substr_replace_first str ~pattern:"/wiki/" ~with_:"wiki/")
  |> List.filter ~f:(fun link ->
    Option.is_none (Wikipedia_namespace.namespace link))
    (* Gets rid of all the bad namespaces*)
  |> List.dedup_and_sort ~compare:String.compare
;;


let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let article = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
  let graph = G.create () in
  let visited = String.Hash_set.create () in
  let to_visit = Stack.create () in
  Stack.push to_visit article;
  let rec traverse depth () =
    match Stack.pop to_visit with
    | None -> ()
    | Some current_node ->
      if max_depth < 1 then () else if not (Hash_set.mem visited current_node)
      then (
        Hash_set.add visited current_node;
        let adjacent_nodes = get_linked_articles current_node in
        List.iter adjacent_nodes ~f:(fun next_node ->
          Stack.push to_visit next_node;
          G.add_edge graph (Article.create current_node) (Article.create next_node)));
      traverse (depth - 1) ()
  in
  traverse max_depth ();
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
