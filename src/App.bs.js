// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Cell from "./components/Cell.bs.js";
import * as Board from "./Board.bs.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as React from "react";
import * as Js_math from "rescript/lib/es6/js_math.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

import './App.css';
;

var initAppleLocation = Js_math.random_int(0, Board.area - 1 | 0);

var initState_cells = Cell.initCells(initAppleLocation);

var initState = {
  cells: initState_cells,
  apple: initAppleLocation,
  nextDir: /* Right */1,
  snakeLength: Board.initSnakeSize,
  isGameOver: false,
  hasGameStarted: false
};

function reducer(state, action) {
  if (typeof action !== "number") {
    return {
            cells: state.cells,
            apple: state.apple,
            nextDir: action._0,
            snakeLength: state.snakeLength,
            isGameOver: state.isGameOver,
            hasGameStarted: state.hasGameStarted
          };
  }
  switch (action) {
    case /* Tick */0 :
        if (state.isGameOver) {
          return state;
        }
        var result = Cell.handleTick(state.cells, state.nextDir, state.apple);
        if (result.TAG !== /* Ok */0) {
          return {
                  cells: state.cells,
                  apple: state.apple,
                  nextDir: state.nextDir,
                  snakeLength: state.snakeLength,
                  isGameOver: true,
                  hasGameStarted: state.hasGameStarted
                };
        }
        var r = result._0;
        return {
                cells: r.newCells,
                apple: r.appleLocation,
                nextDir: state.nextDir,
                snakeLength: state.snakeLength,
                isGameOver: state.isGameOver,
                hasGameStarted: state.hasGameStarted
              };
    case /* SnakeGrows */1 :
        if (state.hasGameStarted) {
          return {
                  cells: state.cells,
                  apple: state.apple,
                  nextDir: state.nextDir,
                  snakeLength: state.snakeLength + 1 | 0,
                  isGameOver: state.isGameOver,
                  hasGameStarted: state.hasGameStarted
                };
        } else {
          return {
                  cells: state.cells,
                  apple: state.apple,
                  nextDir: state.nextDir,
                  snakeLength: state.snakeLength,
                  isGameOver: state.isGameOver,
                  hasGameStarted: true
                };
        }
    case /* Retry */2 :
        return initState;
    
  }
}

var intervalId = {
  contents: undefined
};

function App(Props) {
  var match = React.useReducer(reducer, initState);
  var dispatch = match[1];
  var state = match[0];
  var startInterval = function (param) {
    intervalId.contents = Caml_option.some(setInterval((function (param) {
                return Curry._1(dispatch, /* Tick */0);
              }), Board.tickInterval));
    
  };
  React.useMemo((function () {
          return Curry._1(dispatch, /* SnakeGrows */1);
        }), [state.apple]);
  React.useEffect((function () {
          Curry._2(document.addEventListener, "keydown", (function ($$event) {
                  var match = $$event.key;
                  switch (match) {
                    case "ArrowLeft" :
                    case "a" :
                        return Curry._1(dispatch, /* ChangeDirection */{
                                    _0: /* Left */0
                                  });
                    case "ArrowRight" :
                    case "d" :
                        return Curry._1(dispatch, /* ChangeDirection */{
                                    _0: /* Right */1
                                  });
                    case "ArrowDown" :
                    case "s" :
                        return Curry._1(dispatch, /* ChangeDirection */{
                                    _0: /* Down */3
                                  });
                    case "ArrowUp" :
                    case "w" :
                        return Curry._1(dispatch, /* ChangeDirection */{
                                    _0: /* Up */2
                                  });
                    default:
                      return ;
                  }
                }));
          startInterval(undefined);
          return (function (param) {
                    Belt_Option.map(intervalId.contents, (function (prim) {
                            clearInterval(prim);
                            
                          }));
                    
                  });
        }), []);
  return React.createElement("div", {
              className: "App"
            }, React.createElement("h1", undefined, "Snake Game"), React.createElement("h3", undefined, "Score " + String(state.snakeLength - Board.initSnakeSize | 0)), React.createElement("div", {
                  style: {
                    display: "flex",
                    justifyContent: "center"
                  }
                }, React.createElement("div", {
                      style: {
                        border: "1px solid black",
                        display: "flex",
                        width: Board.rowWidthPx,
                        flexWrap: "wrap"
                      }
                    }, Belt_Array.mapWithIndex(state.cells, (function (i, cell) {
                            return React.createElement(Cell.make, {
                                        cell: cell,
                                        key: String(i)
                                      });
                          })))), state.isGameOver ? React.createElement(React.Fragment, undefined, React.createElement("h1", {
                        style: {
                          color: "red"
                        }
                      }, "Game Over!"), React.createElement("button", {
                        style: {
                          background: "lightblue",
                          border: "0",
                          fontWeight: "bold",
                          padding: "1rem"
                        },
                        onClick: (function (param) {
                            return Curry._1(dispatch, /* Retry */2);
                          })
                      }, "RETRY?")) : React.createElement(React.Fragment, undefined));
}

var make = App;

export {
  initAppleLocation ,
  initState ,
  reducer ,
  intervalId ,
  make ,
  
}
/*  Not a pure module */
