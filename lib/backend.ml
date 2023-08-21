open Lwt.Syntax
open Lwt.Infix

module Range = struct
  type t = int * int

  let is_within (start, finish) x =
    x > start && x < finish
end

module Pessoa = struct
  type create =
    { apelido : string
    ; nome : string
    ; nascimento : string
    ; stack : string list option }
  [@@deriving yojson]

  type t =
    { id : string
    ; apelido : string
    ; nome : string
    ; nascimento : string
    ; stack : string list option }
  [@@deriving yojson]

  [@@inline]
  let of_object (row: < id: string; apelido: string; nome: string; nascimento: string; stack: PGOCaml.string_array option >) =
    { id = row#id
    ; apelido = row#apelido
    ; nome = row#nome
    ; nascimento = row#nascimento
    ; stack = Option.map (List.map Option.get) row#stack }

  let nascimento_regexp = Str.regexp "^\\d{4}-\\d{2}-\\d{2}$"

  let is_valid (create: create) =
    Range.is_within (0, 32) (String.length create.apelido) &&
    Range.is_within (0, 100) (String.length create.nome) &&
    Str.(string_match nascimento_regexp create.nascimento 0) &&
    List.for_all (fun s -> Range.is_within (0, 32) @@ String.length s)
      @@ Option.value ~default:[] create.stack

  let create ~connection req =
    let* create =
      Dream.body req
      >|= Yojson.Safe.from_string
      >|= create_of_yojson
    in
    match create with
    | Ok create when is_valid create ->
      let id =
        let stack = Option.map (List.map Option.some) create.stack in
        List.hd [%pgsql connection
          "INSERT INTO pessoas (id, apelido, nome, nascimento, stack)
           VALUES (gen_random_uuid(), ${create.apelido}, ${create.nome}, ${create.nascimento}, $?{stack})
           RETURNING id"]
      in
      Dream.empty ~headers:[ "Location", "/pessoas/" ^ id ] `Created
    | Ok _ | Error _ ->
      Dream.(empty (`Status 422))

  let detail ~connection req =
    let id = Dream.param req "id" in
    match [%pgsql.object connection "SELECT * FROM pessoas WHERE id = $id"] with
    | [ pessoa ] ->
      pessoa
      |> of_object
      |> to_yojson
      |> Yojson.Safe.to_string
      |> Dream.json
    | [] ->
      Dream.empty (`Status 404)
    | _ -> assert false

  let search ~connection req =
    match Dream.query req "t" with
    | Some term ->
      [%pgsql.object connection
          "SELECT * FROM pessoas WHERE to_tsvector(apelido || ' ' || nome || ' ' || coalesce(array_to_string(stack, ' '), '')) @@ to_tsquery($term) limit 50"]
      |> List.map of_object
      |> [%to_yojson: t list]
      |> Yojson.Safe.to_string
      |> Dream.json
    | None ->
      Dream.empty (`Status 400)

  let count ~connection _req =
    [%pgsql connection "SELECT COUNT(*) FROM pessoas"]
    |> List.hd
    |> Option.get
    |> Int64.to_string
    |> Dream.respond ~status:`OK
end

let main () =
  let connection = PGOCaml.connect ~host:"localhost" ~user:"postgres" () in
  Dream.serve
    @@ Dream.logger
    @@ Dream.router
      [ Dream.post "/pessoas" (Pessoa.create ~connection)
      ; Dream.get "/pessoas/:id" (Pessoa.detail ~connection)
      ; Dream.get "/pessoas" (Pessoa.search ~connection)
      ; Dream.get "/contagem-pessoas" (Pessoa.count ~connection) ]