exception InvalidDifference(int)
exception InvalidHeadPosition
exception BadArgument(string)
exception InvalidLengthOrId
exception TailNextNotFound(string)
exception ExcessiveTailRecursion

type direction = Left | Right | Up | Down
type gameEnding = EdgeCollision | SelfCollision
type collisionError = GameEnding | PreventAction
type cellId = int

type rec t = {
  next: option<cellId>,
  prev: option<cellId>,
  isHead: bool,
  isApple: bool,
  location: cellId,
}

let findSnakeHead = (cells: array<t>): t => {
  cells->Js.Array2.find(c => c.isHead == true)->Belt.Option.getExn
}

let max = ref(0)
let rec findTail = (cell: t, cells: array<t>): cellId => {
  max := max.contents + 1
  switch cell.prev {
  | Some(_) if max.contents > 1000 => raise(ExcessiveTailRecursion)
  | Some(c) =>
    cells->Js.Array2.find(cell => cell.location == c)->Belt.Option.getExn->findTail(cells)
  | None => {
      max := 0
      cell.location
    }
  }
}

let checkBorderCollision = (newTailLocation: cellId, nextPos: direction) => {
  if newTailLocation > Board.area {
    Error(GameEnding)
  } else if newTailLocation < 0 {
    Error(GameEnding)
  } else if mod(newTailLocation, Board.width) == Board.width - 1 && nextPos == Left {
    Error(GameEnding)
  } else if mod(newTailLocation, Board.width) == 0 && nextPos == Right {
    Error(GameEnding)
  } else {
    Ok()
  }
}

// prevent 180 degree turn
let checkInvalidTurn = (newTailLocation, head) => {
  let headPrev = head.prev->Belt.Option.getExn
  if headPrev == newTailLocation {
    Error(PreventAction)
  } else {
    Ok()
  }
}

let rec checkSelfCollision = (newTailLocation: cellId, head: t, cells: array<t>) => {
  switch head.prev {
  | Some(id) =>
    if id == newTailLocation {
      Error(GameEnding)
    } else {
      let childCell = cells->Js.Array2.find(cell => cell.location == id)->Belt.Option.getExn
      checkSelfCollision(newTailLocation, childCell, cells)
    }
  | None => Ok()
  }
}

// move tail to in front of head
// make head isHead = false and next point to previously tail
// create new tail by remove prev
let moveTailToHead = (cells: array<t>, head: t, newTailLocation) => {
  let tailId = findTail(head, cells)
  let tail = cells->Js.Array2.find(c => c.location == tailId)->Belt.Option.getExn
  let tailNext = switch tail.next {
  | Some(id) => cells->Js.Array2.find(c => c.location == id)->Belt.Option.getExn
  | None => {
      Js.log(cells)
      raise(TailNextNotFound(tail.location->Belt.Int.toString))
    }
  }

  let oldHead = {...head, isHead: false, next: Some(newTailLocation)}
  let newHead = {
    isApple: false,
    isHead: true,
    prev: Some(head.location),
    next: None,
    location: newTailLocation,
  }
  let newTail = {...tailNext, prev: None}

  cells->Belt.Array.mapWithIndex((i, cell) => {
    if oldHead.location == i {
      oldHead
    } else if newHead.location == i {
      newHead
    } else if newTail.location == i {
      newTail
    } else if tail.location == i {
      {...tail, next: None, prev: None, isHead: false}
    } else {
      cell
    }
  })
}

let validateLength = (id, length) => length > id ? raise(InvalidLengthOrId) : ()
let rec appendToSnake = (cells: array<t>, id, length, ~nextDir) => {
  validateLength(id, length)

  let nextId = switch nextDir {
  | Left => id - 1
  | Right => id + 1
  | Up => id - Board.width
  | Down => id + Board.width
  }

  switch length {
  | 0 => cells
  | _ => {
      let cell = cells->Belt.Array.get(id)->Belt.Option.getExn
      let next = switch cells->Belt.Array.get(nextId) {
      | Some(c) => Some(c.location)
      | None => None
      }
      // id - 1 assumes always right direction, may have to change later
      let prev = switch cells->Belt.Array.get(id - 1) {
      | Some(c) if length > 1 => Some(c.location)
      | _ => None
      }
      cells
      ->Belt.Array.map(c => {
        if c.location == cell.location {
          {...c, next, prev}
        } else {
          c
        }
      })
      ->appendToSnake(id - 1, length - 1, ~nextDir)
    }
  }
}

let inferDirectionFromNext = (cell: t): direction => {
  let nextId = switch cell.next {
  | None => raise(BadArgument("Cannot infer direction from head. Use nextDir state instead"))
  | Some(id) => id
  }

  let difference = nextId - cell.location
  switch difference {
  | d if d == 1 => Right
  | d if d == -1 => Left
  | d if d == Board.width => Down
  | d if d == Board.width * -1 => Up
  | _ => raise(InvalidDifference(difference))
  }
}

// guarantee never same location twice to prevent messing up snake length count
let rec generateNewAppleLocation = (currentLocation, newLocation) => {
  if currentLocation == newLocation {
    generateNewAppleLocation(currentLocation, Js.Math.random_int(0, Board.area - 1))
  } else {
    newLocation
  }
}

let handleApple = (cells: array<t>, appleLocation: cellId, head: t): (array<t>, int) => {
  if head.location == appleLocation {
    let tailId = findTail(head, cells)
    let tail = cells->Js.Array2.find(c => c.location == tailId)->Belt.Option.getExn
    let inferredDirection = inferDirectionFromNext(tail)
    let newAppleLocation = generateNewAppleLocation(
      appleLocation,
      Js.Math.random_int(0, Board.area - 1),
    )

    // THIS MIGHT BE WRONG
    let newPrevOffset = switch inferredDirection {
    | Right => -1
    | Left => 1
    | Up => Board.width
    | Down => Board.width * -1
    }
    let prevTail = {...tail, prev: Some(tail.location + newPrevOffset)}
    let c =
      cells
      ->appendToSnake(tail.location + newPrevOffset, 1, ~nextDir=inferredDirection)
      ->Belt.Array.mapWithIndex((i, cell) => {
        if i == prevTail.location {
          prevTail
        } else if i == newAppleLocation {
          {...cell, isApple: true}
        } else {
          cell
        }
      })
    (c, newAppleLocation)
  } else {
    (cells, appleLocation)
  }
}

let getNewTailLocation = (nextPos, head) => {
  switch nextPos {
  | Right => head.location + 1
  | Left => head.location - 1
  | Up => head.location - Board.width
  | Down => head.location + Board.width
  }
}

let validateDirectionChange = (nextPos, cells) => {
  let head = findSnakeHead(cells)
  getNewTailLocation(nextPos, head)->checkInvalidTurn(head)
}

type handleTickReturn = {
  newCells: array<t>,
  appleLocation: int,
}
let handleTick = (cells: array<t>, nextPos: direction, appleLocation: cellId): result<
  handleTickReturn,
  collisionError,
> => {
  let head = findSnakeHead(cells)
  let (c, appleLocation) = cells->handleApple(appleLocation, head)

  // this is the previous tail which will become the head at this new location
  let newTailLocation = getNewTailLocation(nextPos, head)
  checkInvalidTurn(newTailLocation, head)
  ->Belt.Result.flatMap(() => checkSelfCollision(newTailLocation, head, cells))
  ->Belt.Result.flatMap(() => checkBorderCollision(newTailLocation, nextPos))
  ->Belt.Result.flatMap(() => {
    Ok({
      newCells: c->moveTailToHead(head, newTailLocation),
      appleLocation,
    })
  })
}

let initSnake = (cells: array<t>): array<t> => {
  let cell = switch cells->Belt.Array.get(Board.initSnakeHeadLocation) {
  | Some(c) => c
  | None => raise(InvalidHeadPosition)
  }
  let headCell = {
    ...cell,
    isHead: true,
    prev: switch cells->Belt.Array.get(cell.location - 1) {
    | Some(c) => Some(c.location)
    | None => None
    },
  }
  cells
  ->Belt.Array.map(c => {
    if headCell.location == c.location {
      headCell
    } else {
      c
    }
  })
  ->appendToSnake(headCell.location - 1, Board.initSnakeSize - 1, ~nextDir=Right)
}

let initCells = (appleLocation: int) => {
  let cells = []
  for i in 0 to Board.area - 1 {
    let _ = cells->Js.Array2.push({
      next: None,
      prev: None,
      isHead: false,
      isApple: appleLocation == i,
      location: i,
    })
  }
  initSnake(cells)
}

let isCellSnake = (cell: t) =>
  Belt.Option.isSome(cell.next) || Belt.Option.isSome(cell.prev) || cell.isHead == true

@react.component
let make = React.memo((~cell: t) => {
  let isSnake = isCellSnake(cell)

  <div
    style={ReactDOM.Style.make(
      ~width=Board.cellSizePx,
      ~height=Board.cellSizePx,
      ~backgroundColor=switch (isSnake, cell.isApple) {
      | (false, true) => "red"
      | (true, true | false) => "green" // show occupied cell over apple
      | (false, false) => "gray"
      },
      (),
    )}
  />
})
