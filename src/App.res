%%raw(`import './App.css';`)
@val external document: 'a = "document"

type state = {cells: array<Cell.t>, apple: int, nextDir: Cell.direction, snakeLength: int}

let initAppleLocation = Js.Math.random_int(0, Board.area - 1)
let initState = {
  cells: Cell.initCells(initAppleLocation),
  apple: initAppleLocation,
  nextDir: Cell.Right,
  snakeLength: Board.initSnakeSize,
}

type action = Tick | ChangeDirection(Cell.direction) | SnakeGrows

let reducer = (state, action) => {
  switch action {
  | Tick => {
      let result = Cell.handleTick(state.cells, state.nextDir, state.apple)

      {...state, cells: result.newCells, apple: result.appleLocation}
    }
  | ChangeDirection(dir) => {...state, nextDir: dir}
  | SnakeGrows => {...state, snakeLength: state.snakeLength + 1}
  }
}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initState)
  let mounted = React.useRef(false)

  React.useMemo1(() => {
    if mounted.current == true {
      dispatch(SnakeGrows)
    } else {
      mounted.current = true
    }
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
    let intervalId = Js.Global.setInterval(() => {
      dispatch(Tick)
    }, Board.tickInterval)

    Some(
      () => {
        Js.Global.clearInterval(intervalId)
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
  </div>
}
