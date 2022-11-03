%%raw(`import './App.css';`)
@val external document: 'a = "document"

type state = {
  cells: array<Cell.t>,
  apple: int,
  nextDir: Cell.direction,
  snakeLength: int,
  isGameOver: bool,
  hasGameStarted: bool,
}

let initAppleLocation = Js.Math.random_int(0, Board.area - 1)
let initState = {
  cells: Cell.initCells(initAppleLocation),
  apple: initAppleLocation,
  nextDir: Cell.Right,
  snakeLength: Board.initSnakeSize,
  isGameOver: false,
  hasGameStarted: false,
}

type action = Tick | ChangeDirection(Cell.direction) | SnakeGrows | Retry

let reducer = (state, action) => {
  switch action {
  | Tick =>
    if state.isGameOver {
      state
    } else {
      let result = Cell.handleTick(state.cells, state.nextDir, state.apple)
      switch result {
      | Error() => {
          ...state,
          isGameOver: true,
        }
      | Ok(r) => {...state, cells: r.newCells, apple: r.appleLocation}
      }
    }
  | ChangeDirection(dir) => {...state, nextDir: dir}
  | SnakeGrows =>
    if state.hasGameStarted {
      {...state, snakeLength: state.snakeLength + 1}
    } else {
      {...state, hasGameStarted: true}
    }
  | Retry => initState
  }
}

let intervalId = ref(None)
@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initState)

  let startInterval = () => {
    intervalId := Js.Global.setInterval(() => {
        dispatch(Tick)
      }, Board.tickInterval)->Some
  }

  React.useMemo1(() => {
    dispatch(SnakeGrows)
  }, [state.apple])

  React.useEffect0(() => {
    document["addEventListener"]("keydown", event => {
      switch event["key"] {
      | "w" | "ArrowUp" => dispatch(ChangeDirection(Cell.Up))
      | "s" | "ArrowDown" => dispatch(ChangeDirection(Cell.Down))
      | "a" | "ArrowLeft" => dispatch(ChangeDirection(Cell.Left))
      | "d" | "ArrowRight" => dispatch(ChangeDirection(Cell.Right))
      | _ => ()
      }
    })
    startInterval()
    Some(
      () => {
        let _ = intervalId.contents->Belt.Option.map(Js.Global.clearInterval)
      },
    )
  })

  <div className="App">
    <h1> {"Snake Game"->React.string} </h1>
    <h3> {`Score ${Belt.Int.toString(state.snakeLength - Board.initSnakeSize)}`->React.string} </h3>
    <div style={ReactDOM.Style.make(~display="flex", ~justifyContent="center", ())}>
      <div
        style={ReactDOM.Style.make(
          ~display="flex",
          ~flexWrap="wrap",
          ~width=Board.rowWidthPx,
          ~border="1px solid black",
          (),
        )}>
        {state.cells
        ->Belt.Array.mapWithIndex((i, cell) => {
          <Cell cell key={i->Belt.Int.toString} />
        })
        ->React.array}
      </div>
    </div>
    {if state.isGameOver {
      <>
        <h1 style={ReactDOM.Style.make(~color="red", ())}> {"Game Over!"->React.string} </h1>
        <button
          style={ReactDOM.Style.make(
            ~padding="1rem",
            ~border="0",
            ~background="lightblue",
            ~fontWeight="bold",
            (),
          )}
          onClick={_ => dispatch(Retry)}>
          {"RETRY?"->React.string}
        </button>
      </>
    } else {
      <> </>
    }}
  </div>
}
