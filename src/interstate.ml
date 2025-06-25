open! Core
module State = String

module Highway_name = struct
  include String

  let default = ""
end

(* We separate out the [Network] module to represent our social network in OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where a connection
     represents a friendship between two people. *)
  module Highway = struct
    module T = struct
      type t =
        { name : Highway_name.t
        ; cities : State.t * State.t
        }
      [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include T
    include Comparable.Make (T)

    let remove_bad_chars str =
      String.substr_replace_all ~pattern:" " ~with_:"_" str
      |> String.substr_replace_all ~pattern:"." ~with_:"" (* Input doesn't read correctly with spaces or periods so you have to replace them *)
    ;;

    let rec create_pairings highway cities =
      match cities with
      | h :: t ->
        List.map
          ~f:(fun city ->
            { name = highway
            ; cities = remove_bad_chars h, remove_bad_chars city (* Essentially makes the cartesian product without the reverse order pairs *)
            })
          t
        @ create_pairings highway t
      | [] -> []
    ;;

    let of_string s =
      match String.split s ~on:',' with
      | highway :: cities -> Some (create_pairings highway cities)
      | _ -> None
    ;;
  end

  type t = Highway.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
        match Highway.of_string s with
        | Some lst -> lst
        (* Friendships are mutual; a connection between a and b means we should also
           consider the connection between b and a. *)
        | None ->
          printf
            "ERROR: Could not parse line as connection; dropping. %s\n"
            s;
          [])
    in
    Highway.Set.of_list connections
  ;;
end

module G = Graph.Imperative.Graph.ConcreteLabeled (State) (Highway_name)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes e = [ `Dir `None; `Label (E.label e) ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values (whose types
           have [sexp_of_t] implemented). *)
        printf !"%{sexp: Network.t}\n" network]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun highway ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
             they don't already exist. *)
          let city1, city2 = highway.cities in
          let name = highway.name in
          G.add_edge_e graph (city1, name, city2)); (* Created a vertex as a tuple to add the edge names *)
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
