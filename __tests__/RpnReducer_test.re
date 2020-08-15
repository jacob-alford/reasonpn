open Jest;
open RpnReducer;

describe("rpnReducer() > Empty", () => {
  Expect.(
    test("Enter", () => {
      expect(rpnReducer({stack: [], tape: []}, Enter(5.0)))
      |> toEqual({stack: [5.0], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Drop", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Drop))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act1", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act1(Exp)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act2", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act2(Add)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
});

describe("rpnReducer() > Nonempty", () => {
  Expect.(
    test("Enter", () => {
      expect(rpnReducer({stack: [6.0], tape: ["Entered 6"]}, Enter(5.0)))
      |> toEqual({stack: [5.0, 6.0], tape: ["Entered 5", "Entered 6"]})
    })
  );
  Expect.(
    test("Drop", () => {
      expect(rpnReducer({stack: [69.9], tape: ["Entered 5"]}, Drop))
      |> toEqual({stack: [], tape: ["Dropped 69.9", "Entered 5"]})
    })
  );
  Expect.(
    test("Act1 > Exp", () => {
      expect(rpnReducer({stack: [1.0], tape: ["Entered 5"]}, Act1(Exp)))
      |> toEqual({
           stack: [2.718281828459045],
           tape: ["exp(1)", "Entered 5"],
         })
    })
  );
  Expect.(
    test("Act1 > Sin", () => {
      expect(
        rpnReducer(
          {stack: [1.57079632698], tape: ["Entered 5"]},
          Act1(Sin),
        ),
      )
      |> toEqual({stack: [1.0], tape: ["sin(1.57079632698)", "Entered 5"]})
    })
  );
  Expect.(
    test("Act1 > Cos", () => {
      expect(
        rpnReducer(
          {stack: [3.141592653], tape: ["Entered 5"]},
          Act1(Cos),
        ),
      )
      |> toEqual({stack: [(-1.0)], tape: ["cos(3.141592653)", "Entered 5"]})
    })
  );
  Expect.(
    test("Act1 > Tan", () => {
      expect(
        rpnReducer(
          {stack: [3.141592653], tape: ["Entered 5"]},
          Act1(Tan),
        ),
      )
      |> toEqual({
           stack: [(-5.897932257097086e-10)],
           tape: ["tan(3.141592653)", "Entered 5"],
         })
    })
  );

  Expect.(
    test("Act1 > Asin > NaN", () => {
      expect(
        rpnReducer({stack: [30.0], tape: ["Entered 5"]}, Act1(Asin)),
      )
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act1 > Asin", () => {
      expect(rpnReducer({stack: [0.5], tape: ["Entered 5"]}, Act1(Asin)))
      |> toEqual({
           stack: [0.5235987755982989],
           tape: ["asin(0.5)", "Entered 5"],
         })
    })
  );
  Expect.(
    test("Act1 > Acos > NaN", () => {
      expect(
        rpnReducer({stack: [30.0], tape: ["Entered 5"]}, Act1(Acos)),
      )
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act1 > Acos", () => {
      expect(rpnReducer({stack: [0.5], tape: ["Entered 5"]}, Act1(Acos)))
      |> toEqual({
           stack: [1.0471975511965979],
           tape: ["acos(0.5)", "Entered 5"],
         })
    })
  );
  Expect.(
    test("Act1 > Atan", () => {
      expect(rpnReducer({stack: [0.5], tape: ["Entered 5"]}, Act1(Atan)))
      |> toEqual({
           stack: [0.4636476090008061],
           tape: ["atan(0.5)", "Entered 5"],
         })
    })
  );

  Expect.(
    test("Act2 > Add > Empty", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act2(Add)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Add > Nonempty", () => {
      expect(
        rpnReducer({stack: [1.0, 2.0], tape: ["Entered 5"]}, Act2(Add)),
      )
      |> toEqual({stack: [3.0], tape: ["1+2", "Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Sub > Empty", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act2(Sub)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Sub > Nonempty", () => {
      expect(
        rpnReducer({stack: [1.0, 2.0], tape: ["Entered 5"]}, Act2(Sub)),
      )
      |> toEqual({stack: [(-1.0)], tape: ["1-2", "Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Mul > Empty", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act2(Mul)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Mul > Nonempty", () => {
      expect(
        rpnReducer({stack: [1.0, 2.0], tape: ["Entered 5"]}, Act2(Mul)),
      )
      |> toEqual({stack: [2.0], tape: ["1*2", "Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Div > Empty", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act2(Div)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Div > Nonempty", () => {
      expect(
        rpnReducer({stack: [2.0, 1.0], tape: ["Entered 5"]}, Act2(Div)),
      )
      |> toEqual({stack: [0.5], tape: ["1÷2", "Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Xy > Empty", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act2(Xy)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > Xy > Nonempty", () => {
      expect(
        rpnReducer({stack: [2.0, 2.0], tape: ["Entered 5"]}, Act2(Xy)),
      )
      |> toEqual({stack: [4.0], tape: ["2^2", "Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > XrtY > Empty", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act2(XrtY)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > XrtY > Nonempty", () => {
      expect(
        rpnReducer(
          {stack: [2.0, 100.0], tape: ["Entered 5"]},
          Act2(XrtY),
        ),
      )
      |> toEqual({stack: [10.0], tape: ["100^(1/2)", "Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > LogXY > Empty", () => {
      expect(rpnReducer({stack: [], tape: ["Entered 5"]}, Act2(LogXY)))
      |> toEqual({stack: [], tape: ["Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > LogXY > Nonempty", () => {
      expect(
        rpnReducer(
          {stack: [10.0, 10.0], tape: ["Entered 5"]},
          Act2(LogXY),
        ),
      )
      |> toEqual({stack: [1.0], tape: ["log_10^10", "Entered 5"]})
    })
  );
  Expect.(
    test("Act2 > LogXY > NaN", () => {
      expect(
        rpnReducer(
          {stack: [(-1.0), 10.0], tape: ["Entered 5"]},
          Act2(LogXY),
        ),
      )
      |> toEqual({stack: [0.0], tape: ["log_-1^10", "Entered 5"]})
    })
  );
});
