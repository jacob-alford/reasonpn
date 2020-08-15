open Rationale;
open Rationale.Option.Infix;

type rpnState = {
  stack: list(float),
  tape: list(string),
};

type op1 =
  | Exp
  | Sin
  | Cos
  | Tan
  | Asin
  | Acos
  | Atan;

type op2 =
  | Add
  | Sub
  | Mul
  | Div
  | Xy
  | XrtY
  | LogXY;

type rpnAction =
  | Enter(float)
  | Drop
  | Act1(op1)
  | Act2(op2);

let rpnInit: rpnState = {stack: [], tape: []};

let str = Js.Float.toString;
let def = Option.default;
let hd = RList.head;
let tail = RList.tail;

let fromNaN = (n: float) =>
  Js.Float.isNaN(n) ? Option.none() : Option.some(n);

let rpnReducer = ({stack, tape}: rpnState, action: rpnAction): rpnState =>
  switch (stack) {
  | [] =>
    switch (action) {
    | Enter(a) => {stack: [a], tape: ["Entered " ++ str(a), ...tape]}
    | Drop => {stack: [], tape}
    | Act1(_) => {stack: [], tape}
    | Act2(_) => {stack: [], tape}
    }
  | [x, ...xs] =>
    switch (action) {
    | Enter(a) => {
        stack: [a, x, ...xs],
        tape: ["Entered " ++ str(a), ...tape],
      }
    | Drop => {stack: xs, tape: ["Dropped " ++ str(x), ...tape]}
    | Act1(op) =>
      switch (op) {
      | Exp => {
          stack: [exp(x), ...xs],
          tape: ["exp(" ++ str(x) ++ ")", ...tape],
        }
      | Sin => {
          stack: [sin(x), ...xs],
          tape: ["sin(" ++ str(x) ++ ")", ...tape],
        }
      | Cos => {
          stack: [cos(x), ...xs],
          tape: ["cos(" ++ str(x) ++ ")", ...tape],
        }
      | Tan => {
          stack: [tan(x), ...xs],
          tape: ["tan(" ++ str(x) ++ ")", ...tape],
        }
      | Asin => {
          stack: asin(x) |> fromNaN <$> (n => [n, ...xs]) |> def(xs),
          tape:
            asin(x)
            |> fromNaN
            <$> (_ => ["asin(" ++ str(x) ++ ")", ...tape])
            |> def(tape),
        }
      | Acos => {
          stack: acos(x) |> fromNaN <$> (n => [n, ...xs]) |> def(xs),
          tape:
            acos(x)
            |> fromNaN
            <$> (_ => ["acos(" ++ str(x) ++ ")", ...tape])
            |> def(tape),
        }
      | Atan => {
          stack: [atan(x), ...xs],
          tape: ["atan(" ++ str(x) ++ ")", ...tape],
        }
      }
    | Act2(op) =>
      switch (op) {
      | Add => {
          stack: [
            hd(xs) <$> (y => x +. y) |> def(x),
            ...tail(xs) |> def([]),
          ],
          tape:
            hd(xs)
            <$> (y => [str(x) ++ "+" ++ str(y), ...tape])
            |> def(tape),
        }
      | Sub => {
          stack: [
            hd(xs) <$> (y => x -. y) |> def(x),
            ...tail(xs) |> def([]),
          ],
          tape:
            hd(xs)
            <$> (y => [str(x) ++ "-" ++ str(y), ...tape])
            |> def(tape),
        }
      | Mul => {
          stack: [
            hd(xs) <$> (y => x *. y) |> def(x),
            ...tail(xs) |> def([]),
          ],
          tape:
            hd(xs)
            <$> (y => [str(x) ++ "*" ++ str(y), ...tape])
            |> def(tape),
        }
      | Div => {
          stack: [
            hd(xs) <$> (y => y /. x) |> def(x),
            ...tail(xs) |> def([]),
          ],
          tape:
            hd(xs)
            <$> (y => [str(y) ++ "÷" ++ str(x), ...tape])
            |> def(tape),
        }
      | Xy => {
          stack: [
            hd(xs) <$> (y => x ** y) |> def(x),
            ...tail(xs) |> def([]),
          ],
          tape:
            hd(xs)
            <$> (y => [str(x) ++ "^" ++ str(y), ...tape])
            |> def(tape),
        }
      | XrtY => {
          stack: [
            hd(xs) <$> (y => y ** (1.0 /. x)) |> def(x),
            ...tail(xs) |> def([]),
          ],
          tape:
            hd(xs)
            <$> (y => [str(y) ++ "^(1/" ++ str(x) ++ ")", ...tape])
            |> def(tape),
        }
      | LogXY => {
          stack: [
            hd(xs)
            <$> (y => log(y) /. log(x) |> fromNaN |> def(0.0))
            |> def(x),
            ...tail(xs) |> def([]),
          ],
          tape:
            hd(xs)
            <$> (y => ["log_" ++ str(x) ++ "^" ++ str(y), ...tape])
            |> def(tape),
        }
      }
    }
  };
