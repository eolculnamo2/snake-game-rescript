exception InvalidHeadPosition
exception InvalidLengthOrId
exception TailPrevNotFound(string)
exception ExcessiveTailRecursion
type direction = Left | Right | Up | Down
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
  | Some(_) if max.contents > 1000 => {
      raise(ExcessiveTailRecursion)
    }
  | Some(c) =>
    cells->Js.Array2.find(cell => cell.location == c)->Belt.Option.getExn->findTail(cells)
  | None => {
      max := 0
      cell.location
    }
  }
}

// move tail to in front of head
// make head isHead = false and next point to previously tail
// create new tail by remove prev
let moveTailToHead = (cells: array<t>, nextPos: direction, head: t) => {
  let tailId = findTail(head, cells)
  let tail = cells->Js.Array2.find(c => c.location == tailId)->Belt.Option.getExn
  let tailNext = switch tail.next {
  | Some(id) => cells->Js.Array2.find(c => c.location == id)->Belt.Option.getExn
  | None => raise(TailPrevNotFound(`no tail for ${tail.location->Belt.Int.toString}`))
  }
  let newTailLocation = switch nextPos {
  | Right => head.location + 1
  | Left => head.location - 1
  | Up => head.location - Board.width
  | Down => head.location + Board.width
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
let rec appendToSnake = (cells: array<t>, id, length) => {
  validateLength(id, length)
  switch length {
  | 0 => cells
  | _ => {
      let cell = cells->Belt.Array.get(id)->Belt.Option.getExn
      let next = switch cells->Belt.Array.get(id + 1) {
      | Some(c) => Some(c.location)
      | None => None
      }
      let prev = switch cells->Belt.Array.get(id - 1) {
      | Some(c) if length > 1 => Some(c.location)
      | _ => None
      }
      cells
      ->Belt.Array.map(c => {
        if c.location == cell.location {
          {...c, next: next, prev: prev}
        } else {
          c
        }
      })
      ->appendToSnake(id - 1, length - 1)
    }
  }
}

let handleApple = (cells: array<t>, appleLocation: cellId, head: t): (array<t>, int) => {
  if head.location == appleLocation {
    let tailId = findTail(head, cells)
    let tail = cells->Js.Array2.find(c => c.location == tailId)->Belt.Option.getExn
    let newAppleLocation = Js.Math.random_int(0, Board.area - 1)
    let prevTail = {...tail, prev: Some(tail.location - 1)}
    let c =
      cells
      ->appendToSnake(tail.location - 1, 1)
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

type handleTickReturn = {
  newCells: array<t>,
  appleLocation: int,
}
let handleTick = (cells: array<t>, nextPos: direction, appleLocation: cellId): handleTickReturn => {
  let head = findSnakeHead(cells)
  let (c, appleLocation) = cells->handleApple(appleLocation, head)

  let newCells = c->moveTailToHead(nextPos, head)
  {
    newCells: newCells,
    appleLocation: appleLocation
  }
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
  ->appendToSnake(headCell.location - 1, Board.initSnakeSize - 1)
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
  let cells = initSnake(cells)
  Js.log(cells)
  cells
}

let isCellSnake = (cell: t) =>
  Belt.Option.isSome(cell.next) || Belt.Option.isSome(cell.prev) || cell.isHead == true

@react.component
let make = (~cell: t) => {
  let isSnake = isCellSnake(cell)

  <div
    style={ReactDOM.Style.make(
      ~width=Board.cellSizePx,
      ~height=Board.cellSizePx,
      ~backgroundColor=switch (isSnake, cell.isApple) {
      | (true, true) => "green" // show snake over apple
      | (false, true) => "red"
      | (true, false) => "green"
      | (false, false) => "gray"
      },
      (),
    )}
  />
}
