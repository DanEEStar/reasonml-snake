open Reprocessing;

type boardPositionT = {
  x: int,
  y: int,
};

type directionT =
  | North
  | East
  | South
  | West;

type snakeT = {
  dir: directionT,
  parts: list(boardPositionT),
};

type msgT =
  | ChangeDirection(directionT)
  | UpdateGameState
  | ResetGameState;

type stateT = {snake: snakeT};

let createNewSnakeHead = snake => {
  let oldHead = List.hd(snake.parts);
  switch (snake.dir) {
  | North => {x: oldHead.x, y: oldHead.y - 1}
  | East => {x: oldHead.x - 1, y: oldHead.y}
  | South => {x: oldHead.x, y: oldHead.y + 1}
  | West => {x: oldHead.x + 1, y: oldHead.y}
  };
};

let updateSnake = (dropLast, snake) => {
  let newHead = createNewSnakeHead(snake);
  let rest =
    if (dropLast) {
      snake.parts->List.rev->List.tl->List.rev;
    } else {
      snake.parts;
    };

  {...snake, parts: [newHead] @ rest};
};

let initSnake = () => {
  dir: North,
  parts: [{x: 9, y: 10}, {x: 10, y: 10}, {x: 11, y: 10}],
};

let drawSnake = (snake, env) => {
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  List.iter(
    part =>
      Draw.rect(~pos=(part.x * 20, part.y * 20), ~width=20, ~height=20, env),
    snake.parts,
  );
};

let inputMap = [
  (Reprocessing_Events.Up, ChangeDirection(North)),
  (Reprocessing_Events.Down, ChangeDirection(South)),
  (Reprocessing_Events.Left, ChangeDirection(East)),
  (Reprocessing_Events.Right, ChangeDirection(West)),
  (Reprocessing_Events.A, ResetGameState),
  (Reprocessing_Events.S, UpdateGameState),
];

let handleInput = env =>
  List.filter(input => Env.keyPressed(fst(input), env), inputMap)
  |> List.map(snd);

let updateGame = (state: stateT, messages: list(msgT)) =>
  List.fold_left(
    (state, message) =>
      switch (message) {
      | ChangeDirection(direction) => {
          ...state,
          snake: {
            ...state.snake,
            dir: direction,
          },
        }
      | UpdateGameState => {...state, snake: updateSnake(true, state.snake)}
      | ResetGameState => {...state, snake: initSnake()}
      },
    state,
    messages,
  );

let setup = env => {
  Env.size(~width=400, ~height=400, env);
  {snake: initSnake()};
};

let draw = (state: stateT, env) => {
  let messages = handleInput(env);
  let state = updateGame(state, messages);

  Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);
  drawSnake(state.snake, env);
  state;
};

run(~setup, ~draw, ());