(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(* Gavin Mack gmmack@ucsc.edu
   Timmy Kwan tmakwan@ucsc.edu*)
open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

type comparison = LT | EQ | GT | UNINIT
(*Compares two numbers, ret 0 if same,
  ret 1 if l1>l2, ret -1 if l1<l2*)
    let rec cmp list1 list2 cmpval = match (list1, list2, cmpval) with
(*        | [], [], cmpval     -> EQ
        | [], [], UNINIT     -> EQ*)
        | [], [], GT         -> GT
        | [], [], LT         -> LT
        | list1, [], cmpval  -> GT
        | [], list2, cmpval  -> LT
        | car1::cdr1, car2::cdr2, comparison ->
          if car1 > car2
              then cmp cdr1 cdr2 GT
          else if car1 < car2
              then cmp cdr1 cdr2 LT
          else cmp cdr1 cdr2 EQ
        | _,_,_ -> printf "error"; GT

    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                 let cdr' = trimzeros' cdr
                 in  match car, cdr' with
                     | 0, [] -> []
                     | car, cdr' -> car::cdr'
        in trimzeros' list

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 borrow = match (list1, list2, borrow) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], borrow  -> sub' list1 [borrow] 0
        | [], list2, borrow  -> sub' [borrow] list2 0
        | car1::cdr1, car2::cdr2, borrow ->
          let digit = car1 - car2 - borrow + 10
          in  digit mod radix :: sub' cdr1 cdr2 (1 - digit / radix)

    let double number = add' number number 0

    let rec mul' (multiplier, powof2, multiplicand') =
        if cmp powof2 multiplier UNINIT = GT
        then multiplier, [0]
        else let remainder, product = 
                mul' (multiplier, double powof2, double multiplicand')
            in  if cmp remainder powof2 UNINIT = LT
                then remainder, product
                else trimzeros (sub' remainder powof2 0),
                                add' product multiplicand' 0

    let rec divrem' (dividend, powof2, divisor') =
        if cmp divisor' dividend UNINIT = GT
        then [], dividend
        else let quotient, remainder = 
                divrem' (dividend, double powof2, double divisor')
            in  if cmp remainder divisor' UNINIT = LT
                then quotient, remainder
                else ((add' quotient powof2 0),
                      (sub' remainder divisor' 0))

    let divrem (dividend, divisor') = divrem' (dividend, [1], divisor')

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
             match (neg1, neg2) with
        | Pos, Pos   -> Bigint (neg1, add' value1 value2 0)
        | Neg, Neg   -> Bigint (neg1, add' value1 value2 0)
        | Pos, Neg   -> if cmp value1 value2 UNINIT = GT
                        then Bigint (Pos, trimzeros 
                                    (sub' value1 value2 0))
                        else if cmp value1 value2 UNINIT = LT
                        then Bigint (Neg, trimzeros 
                                    (sub' value2 value1 0))
                        else zero
        | Neg, Pos   -> if cmp value1 value2 UNINIT = GT
                        then Bigint (Neg, trimzeros 
                                    (sub' value1 value2 0))
                        else if cmp value1 value2 UNINIT = LT
                        then Bigint (Pos, trimzeros 
                                    (sub' value2 value1 0))
                        else zero

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
             match (neg1, neg2) with
        | Pos, Neg   -> Bigint (neg1, add' value1 value2 0)
        | Neg, Pos   -> Bigint (neg1, add' value1 value2 0)
        | Pos, Pos   -> if cmp value1 value2 UNINIT = GT
                        then Bigint (Pos, trimzeros 
                                    (sub' value1 value2 0))
                        else if cmp value1 value2 UNINIT = LT
                        then Bigint (Neg, trimzeros 
                                    (sub' value2 value1 0))
                        else zero
        | Neg, Neg   -> if cmp value1 value2 UNINIT = GT
                        then Bigint (Neg, trimzeros 
                                    (sub' value1 value2 0))
                        else if cmp value1 value2 UNINIT = LT
                        then Bigint (Pos, trimzeros 
                                    (sub' value2 value1 0))
                        else zero

    let mul (Bigint (neg1, multiplier)) (Bigint (neg2, multiplicand)) =
        let _, product = mul' (multiplier, [1], multiplicand)
        in  Bigint (neg1, product)

    let div = add
(*    let div (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) = 
        let quotient, _ = divrem (dividend, divisor)
        in quotient*)

    let rem = add

    let pow = add

end
