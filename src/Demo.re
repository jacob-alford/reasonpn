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
  | Xy;

type rpnAction =
  | Enter(float)
  | Drop
  | Act1(op1)
  | Act2(op2);

let rpnInit: rpnState = {stack: [], tape: []};

let rpnReducer = ({stack, tape}: rpnState, action: rpnAction): rpnState =>
  switch (stack) {
  | [] =>
    switch (action) {
    | Enter(a) => {stack: [a], tape: []}
    | Drop => {stack: [], tape: []}
    | Act1(_) => {stack: [], tape: []}
    | Act2(_) => {stack: [], tape: []}
    }
  | [x, ...xs] =>
    switch (action) {
    | Enter(a) => {stack: [a, x, ...xs], tape: []}
    | Drop => {stack: xs, tape: []}
    | Act1(op) =>
      switch (op) {
      | Exp => {
          stack: [exp(x), ...xs],
          tape: ["exp(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Sin => {
          stack: [sin(x), ...xs],
          tape: ["sin(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Cos => {
          stack: [cos(x), ...xs],
          tape: ["cos(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Tan => {
          stack: [tan(x), ...xs],
          tape: ["tan(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Asin => {
          stack: [asin(x), ...xs],
          tape: ["asin(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Acos => {
          stack: [acos(x), ...xs],
          tape: ["acos(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Atan => {
          stack: [atan(x), ...xs],
          tape: ["atan(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      }
    // TODO Act II VVV
    | Act2(op) => {stack: [], tape: []}
    }
  };
