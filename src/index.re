open Reprocessing;

/* Model */
let g = (-10.0);

let runSpeed = 50.0;

let jumpSpeed = 60.0;

type obstacle = {x: float};

type state = {
  x: float,
  y: float,
  vy: float,
  obstacles: list(obstacle)
};

let initialState = {
  x: 0.0,
  y: 0.0,
  vy: 0.0,
  obstacles: [{x: 5.0}, {x: 10.0}, {x: 15.0}, {x: 20.0}]
};

let setup = env : state => {
  Env.size(~width=600, ~height=600, env);
  initialState;
};

type action =
  | Nothing
  | Jump
  | Tick(float);

let move = (state, timeStep) => {
  ...state,
  x: state.x +. runSpeed *. timeStep,
  y: max(0.0, state.y +. state.vy *. timeStep),
  vy: state.vy < 0.0 && state.y == 0.0 ? 0.0 : state.vy +. g *. timeStep
};

let reduce = (state: state, action: action) : state =>
  switch action {
  | Nothing => state
  | Jump => state.y != 0.0 ? state : {...state, vy: jumpSpeed}
  | Tick(timeStep) => move(state, timeStep)
  };

/* View */
let tileSize = 50;

let draw = (state: state, env) : state => {
  let timeStep = Env.deltaTime(env);
  Draw.background(Constants.black, env);
  Draw.fill(Constants.red, env);
  let height = 2 * tileSize;
  let x = tileSize + int_of_float(state.x) * 3;
  let y = 600 - int_of_float(state.y) - height;
  Draw.pushMatrix(env);
  Draw.translate(~x=float_of_int(- x), ~y=0.0, env);
  Draw.rect(~pos=(x, y), ~width=50, ~height, env);
  Draw.fill(Constants.green, env);
  Draw.rect(~pos=(200, 550), ~width=50, ~height=50, env);
  Draw.rect(~pos=(600, 550), ~width=50, ~height=50, env);
  Draw.fill(Constants.blue, env);
  Draw.rect(~pos=(400, 550), ~width=50, ~height=50, env);
  Draw.rect(~pos=(800, 550), ~width=50, ~height=50, env);
  Draw.popMatrix(env);
  reduce(state, Tick(timeStep *. 15.0));
};

let keyPressed = (state, env) =>
  Events.(
    switch (Env.keyCode(env)) {
    | Space => reduce(state, Jump)
    | _ => state
    }
  );

run(~setup, ~draw, ~keyPressed, ());
