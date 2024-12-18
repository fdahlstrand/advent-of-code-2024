module PrioQueue = struct
  type prio = int

  type 'a queue = Empty | Node of prio * 'a * 'a queue * 'a queue

  let empty = Empty

  exception Queue_empty

  let rec insert prio elt queue =
    match queue with
    | Empty ->
        Node (prio, elt, Empty, Empty)
    | Node (p, e, left, right) ->
        if prio <= p then Node (prio, elt, insert p e right, left)
        else Node (p, e, insert prio elt right, left)

  let rec remove_top = function
    | Empty ->
        raise Queue_empty
    | Node (_, _, left, Empty) ->
        left
    | Node (_, _, Empty, right) ->
        right
    | Node
        ( _
        , _
        , (Node (lprio, lelt, _, _) as left)
        , (Node (rprio, relt, _, _) as right) ) ->
        if lprio <= rprio then Node (lprio, lelt, remove_top left, right)
        else Node (rprio, relt, left, remove_top right)

  let extract_min = function
    | Empty ->
        raise Queue_empty
    | Node (prio, elt, _, _) as queue ->
        (prio, elt, remove_top queue)
end
