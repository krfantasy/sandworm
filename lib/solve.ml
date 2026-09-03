(* Dense linear solvers with partial pivoting, real and complex.
   Small-circuit MNA systems only; no external dependencies. *)

exception Singular of string

(* Solve A x = b (real). A is n x n, b has length n. Neither is mutated. *)
let solve_real (a : float array array) (b : float array) : float array =
  let n = Array.length b in
  if n = 0 then [||]
  else begin
    if Array.length a <> n then raise (Singular "non-square matrix");
    let m = Array.init n (fun i ->
        if Array.length a.(i) <> n then raise (Singular "non-square matrix");
        Array.append (Array.copy a.(i)) [| b.(i) |])
    in
    for col = 0 to n - 1 do
      (* partial pivot *)
      let piv = ref col in
      for row = col + 1 to n - 1 do
        if Float.abs m.(row).(col) > Float.abs m.(!piv).(col) then piv := row
      done;
      if m.(!piv).(col) = 0.0 then raise (Singular "singular matrix (zero pivot)");
      if !piv <> col then begin
        let tmp = m.(col) in
        m.(col) <- m.(!piv);
        m.(!piv) <- tmp
      end;
      let p = m.(col).(col) in
      for row = col + 1 to n - 1 do
        let f = m.(row).(col) /. p in
        if f <> 0.0 then begin
          for k = col to n do
            m.(row).(k) <- m.(row).(k) -. f *. m.(col).(k)
          done
        end
      done
    done;
    let x = Array.make n 0.0 in
    for i = n - 1 downto 0 do
      let s = ref m.(i).(n) in
      for j = i + 1 to n - 1 do
        s := !s -. m.(i).(j) *. x.(j)
      done;
      x.(i) <- !s /. m.(i).(i)
    done;
    x
  end

(* Invert a small real matrix (for coupled-inductor L matrices). *)
let invert_real (a : float array array) : float array array =
  let n = Array.length a in
  if n = 0 then [||]
  else begin
    let cols =
      Array.init n (fun j ->
        let b = Array.init n (fun i -> if i = j then 1.0 else 0.0) in
        solve_real a b)
    in
    Array.init n (fun i -> Array.init n (fun j -> cols.(j).(i)))
  end

module C = struct
  type t = Complex.t

  let zero = Complex.zero
  let one = Complex.one

  let norm2 (z : t) =
    let open Float in
    z.re *. z.re +. z.im *. z.im
end

(* Solve A x = b (complex). *)
let solve_complex (a : Complex.t array array) (b : Complex.t array) : Complex.t array =
  let n = Array.length b in
  if n = 0 then [||]
  else begin
    if Array.length a <> n then raise (Singular "non-square matrix");
    let m = Array.init n (fun i ->
        if Array.length a.(i) <> n then raise (Singular "non-square matrix");
        Array.append (Array.copy a.(i)) [| b.(i) |])
    in
    for col = 0 to n - 1 do
      let piv = ref col in
      for row = col + 1 to n - 1 do
        if C.norm2 m.(row).(col) > C.norm2 m.(!piv).(col) then piv := row
      done;
      if m.(!piv).(col) = Complex.zero then
        raise (Singular "singular matrix (zero pivot)");
      if !piv <> col then begin
        let tmp = m.(col) in
        m.(col) <- m.(!piv);
        m.(!piv) <- tmp
      end;
      let p = m.(col).(col) in
      for row = col + 1 to n - 1 do
        let f = Complex.div m.(row).(col) p in
        if f <> Complex.zero then begin
          for k = col to n do
            m.(row).(k) <- Complex.sub m.(row).(k) (Complex.mul f m.(col).(k))
          done
        end
      done
    done;
    let x = Array.make n Complex.zero in
    for i = n - 1 downto 0 do
      let s = ref m.(i).(n) in
      for j = i + 1 to n - 1 do
        s := Complex.sub !s (Complex.mul m.(i).(j) x.(j))
      done;
      x.(i) <- Complex.div !s m.(i).(i)
    done;
    x
  end
