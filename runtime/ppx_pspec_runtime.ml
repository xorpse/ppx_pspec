open Base
open Base.Continue_or_stop

type byte = char

module Bitvec = struct
  include Bitvec

  let of_bits = concat 1

  let of_bools =
    Fn.compose of_bits (List.map ~f:bool)

end

module List = struct
  include List

  let rec append_n n v vs =
    if Int.(n > 0) then append_n Int.(pred n) v (v :: vs) else vs
end

module Modifier = struct
  type t = Prefix | Suffix of string * int
  [@@deriving compare, equal]
end

module Length = struct
  type t = Variable | Fixed of int
  [@@deriving compare, equal]
end

module Order = struct
  type t = Lsb | Msb
  [@@deriving compare, equal]

  let is_lsb = equal Lsb
  let is_msb = equal Msb
end

module Fixed = struct
  type t = Skip | Bit of bool
  [@@deriving compare, equal]

  let bit b = Bit b
  let bits c order =
    let rec go n a i =
      if n <= 0 then a
      else go (Int.pred n) (bit (i land 1 = 1) :: a) (i lsr 1)
    in
    let bits = go 8 [] (Char.to_int c) in
    if Order.(equal order Lsb) then List.rev bits else bits
end

module Directive = struct
  type t =
    | Directive of {
        stop     : bool;
        symbol   : string;
        location : Length.t;
    }
    | Fixed of Fixed.t
  [@@deriving compare, equal]

  let length = function
    | Fixed _ -> 1
    | Directive { location = Length.Fixed v ; _ } -> v
    | _ -> 0
end

module Extractor = struct
  type t = {
    start : int;
    last  : int option;
  }
  [@@deriving compare, equal]

  let make ?last start = {
    start; last
  }
end

module Spec = struct
  type t = {
    length     : Length.t;
    order      : Order.t;
    directives : Directive.t list;
    modifier   : Modifier.t option;
    mask       : Bitvec.t;
    fix        : Bitvec.t;
    modulus    : int;
    extractors : Extractor.t Map.M(String).t;
  }
  [@@deriving compare, equal]

  let make ?modifier ~length ~order ~directives =
    let fmt = if Order.is_msb order then
        List.rev directives
      else
        directives
    in
    let _, modulus = match length with
      | Length.Fixed size -> true, size
      | Length.Variable ->
        false,
        List.fold_until
          ~init:0
          ~f:(fun acc v ->
              let len = Directive.length v in
              if len > 0 then Continue (len + acc) else Stop acc)
          ~finish:Fn.id
          directives
    in
    let (fix, mask, _, _, extractors) = List.fold
        fmt
        ~init:([], [], 0, 0, Map.empty (module String))
        ~f:(fun (fix, mask, count, i, extr) -> function
            | Directive.Fixed Fixed.Skip ->
              (Bitvec.zero :: fix, Bitvec.zero :: mask, count + 1, i + 1, extr)
            | Directive.Fixed (Fixed.Bit b) ->
              (Bitvec.bool b :: fix, Bitvec.one :: mask, count + 1, i + 1, extr)
            | Directive.Directive { symbol; location = Length.Variable; _ } ->
              let fix, mask, count =
                if count < modulus then
                  let diff = modulus - count in
                  List.append_n diff Bitvec.zero fix,
                  List.append_n diff Bitvec.zero mask,
                  modulus
                else
                  fix, mask, count
              in
              (fix, mask, count, i, Map.add_exn extr ~key:symbol ~data:(Extractor.make i))
            | Directive.Directive { stop; symbol; location = Length.Fixed loc } ->
              let i = if stop && Order.is_lsb order then i - loc else i in
              let sta = i in
              let sto = i + loc in
              let fix, mask, count =
                if not stop then
                  List.append_n loc Bitvec.zero fix,
                  List.append_n loc Bitvec.zero mask,
                  count + loc
                else
                  fix, mask, count
              in
              let i = sto in
              let i = if stop && Order.is_msb order then i - loc else i in
              let extr = Map.add_exn extr ~key:symbol ~data:(Extractor.make sta ~last:sto) in
              (fix, mask, count, i, extr)
          )
    in
    let fix = Bitvec.concat 1 @@ List.rev fix in
    let mask = Bitvec.concat 1 @@ List.rev mask in
    { length; order; directives; modifier; fix; mask; modulus; extractors }
end

module Parser = struct
  open! Angstrom

  let number =
    Int.of_string <$> take_while1 (function '0' .. '9' -> true | _ -> false)

  let alpha_ =
    satisfy (function 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false)

  let alphanum_ =
    satisfy (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)

  let identifier =
    consumed (alpha_ *> skip_many alphanum_)

  let multispace0 = skip_many @@ satisfy Char.is_whitespace
  let multispace1 = skip_many1 @@ satisfy Char.is_whitespace

  let length =
    (Fn.const Length.Variable <$> char '*') <|>
    ((fun v -> Length.Fixed v) <$> number)

  let order =
    let p = (Fn.const Order.Msb <$> char '<')
            <|>
            (Fn.const Order.Lsb <$> char '>')
    in
    option Order.Msb p

  let symbol = identifier

  let stop =
    option false (Fn.const true <$> char '=')

  let location =
    option (Length.Fixed 1) @@ char '(' *> length <* char ')'

  let directive =
    multispace0 *>
    stop >>= fun stop ->
    multispace0 *>
    symbol >>= fun symbol ->
    multispace0 *>
    location >>| fun location ->
    Directive.Directive { stop; symbol; location }

  let fixed_byte =
    char '{' *> begin
      multispace0 *>
      take_while1 (function '0'..'9' | 'a'..'f' | 'A'..'F' -> true | _ -> false)
      <* multispace0
    end <* char '}'
    >>| fun v -> Int.of_string ("0x" ^ v) |> Char.of_int_exn

  let fixed order =
    multispace0 *>
    begin
      (Fn.const [Fixed.Skip] <$> char '-') <|>
      (Fn.const [Fixed.Bit false] <$> char '0') <|>
      (Fn.const [Fixed.Bit true] <$> char '1') <|>
      ((fun v -> Fixed.bits v order) <$> fixed_byte)
    end
    >>| List.map ~f:(fun v -> Directive.Fixed v)

  let directives order =
    char '[' *>
    multispace0 *>
    many ((List.return <$> directive) <|> fixed order)
    <* multispace0
    <* char ']'
    >>| List.concat

  let modifier =
    (Fn.const Modifier.Prefix <$> char '+')
    <|>
    begin
      char '&' *>
      multispace0 *>
      symbol >>= fun symbol ->
      multispace0 *>
      char '(' *>
      multispace0 *>
      number >>= fun number ->
      multispace0 *>
      char ')' *>
      return (Modifier.Suffix (symbol, number))
    end

  let maybe p =
    option None (Option.some <$> p)

  let spec =
    multispace0 *>
    length >>= fun length ->
    multispace0 *>
    order >>= fun order ->
    multispace0 *>
    directives order >>= fun directives ->
    multispace0 *>
    maybe modifier >>| fun modifier ->
    Spec.make ?modifier ~length ~order ~directives

  let parse_string =
    parse_string ~consume:All spec

  let parse_bigstring =
    parse_bigstring ~consume:All spec
end
