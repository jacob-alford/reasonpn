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
  | [x] =>
    switch (action) {
    | Enter(a) => {stack: [a, x], tape: []}
    | Drop => {stack: [], tape: []}
    | Act1(op) =>
      switch (op) {
      | Exp => {
          stack: [exp(x)],
          tape: ["exp(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Sin => {
          stack: [sin(x)],
          tape: ["sin(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Cos => {
          stack: [cos(x)],
          tape: ["cos(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Tan => {
          stack: [tan(x)],
          tape: ["tan(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Asin => {
          stack: [asin(x)],
          tape: ["asin(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Acos => {
          stack: [acos(x)],
          tape: ["acos(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Atan => {
          stack: [atan(x)],
          tape: ["atan(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      }
    // TODO Act II VVV
    | Act2(_) => {stack, tape}
    }
  | [x, y, ...xs] =>
    switch (action) {
    | Enter(a) => {stack: [a, x, y, ...xs], tape: []}
    | Drop => {stack: xs, tape: []}
    | Act1(op) =>
      switch (op) {
      | Exp => {
          stack: [exp(x), y, ...xs],
          tape: ["exp(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Sin => {
          stack: [sin(x), y, ...xs],
          tape: ["sin(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Cos => {
          stack: [cos(x), y, ...xs],
          tape: ["cos(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Tan => {
          stack: [tan(x), y, ...xs],
          tape: ["tan(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Asin => {
          stack: [asin(x), y, ...xs],
          tape: ["asin(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Acos => {
          stack: [acos(x), y, ...xs],
          tape: ["acos(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      | Atan => {
          stack: [atan(x), y, ...xs],
          tape: ["atan(" ++ Js.Float.toString(x) ++ ")", ...tape],
        }
      }
    // TODO Act II VVV
    | Act2(op) =>
      switch (op) {
      | Add => {
          stack: [x +. y, ...xs],
          tape: [
            Js.Float.toString(x) ++ "+" ++ Js.Float.toString(y),
            ...tape,
          ],
        }
      | Sub => {
          stack: [x -. y, ...xs],
          tape: [
            Js.Float.toString(x) ++ "-" ++ Js.Float.toString(y),
            ...tape,
          ],
        }
      | Mul => {
          stack: [x *. y, ...xs],
          tape: [
            Js.Float.toString(x) ++ "×" ++ Js.Float.toString(y),
            ...tape,
          ],
        }
      | Div => {
          stack: [x /. y, ...xs],
          tape: [
            Js.Float.toString(x) ++ "÷" ++ Js.Float.toString(y),
            ...tape,
          ],
        }
      | Xy => {
          stack: [x ** y, ...xs],
          tape: [
            Js.Float.toString(x) ++ "^" ++ Js.Float.toString(y),
            ...tape,
          ],
        }
      }
    }
  };
