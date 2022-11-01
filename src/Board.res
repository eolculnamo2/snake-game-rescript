let tickInterval = 200// ms
// in cells
let width = 20
let height = 20
let area = width * height

let cellSize = 20 // px
let cellSizePx = cellSize -> Belt.Int.toString ++ "px"
let rowWidth = width * cellSize
let rowWidthPx = rowWidth -> Belt.Int.toString ++ "px"


let initSnakeSize = 4
let initSnakeHeadLocation = width / 2 * height + width / 2
