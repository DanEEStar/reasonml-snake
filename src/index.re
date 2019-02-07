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
  | AddNewFruit(boardPositionT)
  | UpdateGameState
  | ResetGameState
  | TogglePause;

type stateT = {
  paused: bool,
  fruit: boardPositionT,
  snake: snakeT,
};

let createNewSnakeHead = snake => {
  let {x, y} = List.hd(snake.parts);
  switch (snake.dir) {
  | North => {x, y: y - 1}
  | East => {x: x - 1, y}
  | South => {x, y: y + 1}
  | West => {x: x + 1, y}
  };
};

let updateSnakeDirection = (state: stateT, newDirection: directionT) => {
  ...state,
  snake: {
    ...state.snake,
    dir: newDirection,
  },
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

let resetGameState = () => {
  paused: true,
  fruit: {
    x: 10,
    y: 5,
  },
  snake: {
    dir: North,
    parts: [{x: 10, y: 9}, {x: 10, y: 10}, {x: 10, y: 11}],
  },
};

let inputMap = [
  (Reprocessing_Events.Up, ChangeDirection(North)),
  (Reprocessing_Events.Down, ChangeDirection(South)),
  (Reprocessing_Events.Left, ChangeDirection(East)),
  (Reprocessing_Events.Right, ChangeDirection(West)),
  (Reprocessing_Events.J, ChangeDirection(North)),
  (Reprocessing_Events.K, ChangeDirection(South)),
  (Reprocessing_Events.H, ChangeDirection(East)),
  (Reprocessing_Events.L, ChangeDirection(West)),
  (Reprocessing_Events.W, ChangeDirection(North)),
  (Reprocessing_Events.S, ChangeDirection(South)),
  (Reprocessing_Events.A, ChangeDirection(East)),
  (Reprocessing_Events.D, ChangeDirection(West)),
  (Reprocessing_Events.R, ResetGameState),
  (Reprocessing_Events.U, UpdateGameState),
  (Reprocessing_Events.Space, TogglePause),
];

let handleInput = env =>
  List.filter(input => Env.keyPressed(fst(input), env), inputMap)
  |> List.map(snd);

let updateGameState = state => {
  let newHead = createNewSnakeHead(state.snake);
  if (newHead == state.fruit) {
    (
      {...state, snake: updateSnake(false, state.snake)},
      [AddNewFruit({x: Random.int(20), y: Random.int(20)})],
    );
  } else {
    ({...state, snake: updateSnake(true, state.snake)}, []);
  };
};

let updateGame = (state: stateT, messages: list(msgT)) => {
  let rec loop = (state: stateT, messages: list(msgT)) => {
    let (newState, newMessages) =
      List.fold_left(
        ((state, _messages), message) =>
          switch (message) {
          | ChangeDirection(direction) => (
              updateSnakeDirection(state, direction),
              [],
            )
          | AddNewFruit(fruitPos) => ({...state, fruit: fruitPos}, [])
          | UpdateGameState => updateGameState(state)
          | ResetGameState => (resetGameState(), [])
          | TogglePause => ({...state, paused: !state.paused}, [])
          },
        (state, []),
        messages,
      );
    switch (newMessages) {
    | [] => newState
    | _ => loop(newState, newMessages)
    };
  };

  loop(state, messages);
};

let setup = env => {
  Env.size(~width=400, ~height=400, env);
  resetGameState();
};

let drawPart = (env, part) =>
  Draw.rect(~pos=(part.x * 20, part.y * 20), ~width=20, ~height=20, env);

let drawSnake = (snake, env) => {
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  List.iter(drawPart(env), snake.parts);
};

let drawFruit = (fruit, env) => {
  Draw.fill(Utils.color(~r=255, ~g=80, ~b=41, ~a=255), env);
  drawPart(env, fruit);
};

let draw = (state: stateT, env) => {
  let messages = handleInput(env);

  let messages =
    if (Env.frameCount(env) mod 7 == 0 && !state.paused) {
      [UpdateGameState, ...messages];
    } else {
      messages;
    };
  let state = updateGame(state, messages);

  Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);
  drawSnake(state.snake, env);
  drawFruit(state.fruit, env);

  state;
};

run(~setup, ~draw, ());