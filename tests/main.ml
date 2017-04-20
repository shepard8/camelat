module type TEST = sig
  val count : int
  val tests : unit -> unit
end

let modules = [
  (module Count : TEST);
]

let count = List.fold_left (fun acc m ->
  let module M = (val m : TEST) in
  acc + M.count
) 0 modules

let () =
  TestSimple.plan count;
  List.iter (fun m ->
    let module M = (val m : TEST) in
    M.tests ()
  ) modules

