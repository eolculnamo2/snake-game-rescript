%%raw(`import './App.css';`)
@val external document: 'a = "document"

type gameState = Playing | Paused | GameOver
type state = {
  cells: array<Cell.t>,
  apple: int,
  nextDir: Cell.direction,
  snakeLength: int,
  gameState: gameState,
  hasGameStarted: bool,
}

let initAppleLocation = Js.Math.random_int(0, Board.area - 1)
let initState = {
  cells: Cell.initCells(initAppleLocation),
  apple: initAppleLocation,
  nextDir: Cell.Right,
  snakeLength: Board.initSnakeSize,
  gameState: Playing,
  hasGameStarted: false,
}

type action = Tick | ChangeDirection(Cell.direction) | SnakeGrows | Retry | TogglePause

let reducer = (state, action) => {
  switch action {
  | Tick =>
    if state.gameState != Playing {
      state
    } else {
      let result = Cell.handleTick(state.cells, state.nextDir, state.apple)
      switch result {
      | Error(errorType) => {
          ...state,
          gameState: errorType == Cell.GameEnding ? GameOver : state.gameState,
        }
      | Ok(r) => {...state, cells: r.newCells, apple: r.appleLocation}
      }
    }
  | ChangeDirection(dir) => {
      ...state,
      nextDir: switch Cell.validateDirectionChange(dir, state.cells) {
      | Error(_) => state.nextDir // doesn't change if invalid direction
      | Ok() => dir
      },
    }
  | SnakeGrows =>
    if state.hasGameStarted {
      {...state, snakeLength: state.snakeLength + 1}
    } else {
      {...state, hasGameStarted: true}
    }
  | Retry => initState
  | TogglePause => {
      ...state,
      gameState: switch state.gameState {
      | Playing => Paused
      | Paused => Playing
      | GameOver => GameOver
      },
    }
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
      | "w" | "ArrowUp" | "k" => dispatch(ChangeDirection(Cell.Up))
      | "s" | "ArrowDown" | "j" => dispatch(ChangeDirection(Cell.Down))
      | "a" | "ArrowLeft" | "h" => dispatch(ChangeDirection(Cell.Left))
      | "d" | "ArrowRight" | "l" => dispatch(ChangeDirection(Cell.Right))
      | "p" | " " => dispatch(TogglePause)
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
    {if state.gameState == GameOver {
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
