module MultiProducerClone where

-- Two-message protocol. Both cloned senders send one int and drop; the
-- reader receives both. Order forces 10 then 20 here because both
-- send/drop calls are sequential (no fork).
--
-- Migration:
--   * 'newA' returns its tuple as (receiver, sender) per the Prelude
--     signature `forall (s : 1S) . () -> (**?(Dual s), **!s)` — so
--     we bind 'r' first, 's' second.
--   * The reader's terminal arm binds the linear receiver continuation
--     '**?Wait' as 'r'' and terminates via 'waitA r''.
type Two : 1S
type Two = !Int ; !Int ; Close

collectAll : **?(?Int ; ?Int ; Wait) -1-> ()
collectAll r =
  case receiveA r of
    NothingL -> ()
    JustL (x, r') ->
      print x;
      case receiveA r' of
        NothingL -> ()
        JustL (y, r'') ->
          let () = waitA r'' in
          print y

main : ()
main =
  -- (receiver, sender) per Prelude's newA signature.
  let (r, s)  = newA @Two () in
  -- 'cloneAS' is linear because 's' is linear.
  let (s1, s2) = cloneAS s in
  drop (sendA 10 s1);
  drop (sendA 20 s2);
  collectAll r
