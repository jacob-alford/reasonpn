open RpnReducer;

type rpnHistory = {
  history: list(rpnState),
  stash: list(rpnState),
};

type historyAction =
  | Push(rpnState)
  | Stash
  | Pop;

let historyInit: rpnHistory = {history: [], stash: []};

let historyReducer =
    ({history, stash}: rpnHistory, action: historyAction): rpnHistory =>
  switch (action) {
  | Push(state) => {history: [state, ...history], stash}
  | Stash =>
    switch (history) {
    | [] => {history: [], stash}
    | [x, ...xs] => {history: xs, stash: [x, ...stash]}
    }
  | Pop =>
    switch (stash) {
    | [] => {history, stash: []}
    | [x, ...xs] => {history: [x, ...history], stash: xs}
    }
  };
