open Reprocessing;

let segmentSize = 20;
let boardSizeX = 20;
let boardSizeY = 20;
let snakeColor = Utils.color(~r=41, ~g=166, ~b=244, ~a=255);
let fruitColor = Utils.color(~r=255, ~g=80, ~b=41, ~a=255);

type segmentPositionT = {
  x: int,
  y: int,
};

let drawSegment =
    (
      ~color,
      ~boardPosition,
      ~width=float_of_int(segmentSize),
      ~height=float_of_int(segmentSize),
      env,
    ) => {
  let x = float_of_int(boardPosition.x * segmentSize + segmentSize / 2) -. width /. 2.0;
  let y = float_of_int(boardPosition.y * segmentSize + segmentSize / 2) -. height /. 2.0;
  Draw.fill(color, env);
  Draw.rectf(~pos=(x, y), ~width, ~height, env);
};

module SegmentAnimation = {
  type segmentAnimationT = {
    pos: segmentPositionT,
    color: Reprocessing.colorT,
    runningTime: float,
    elapsedTime: float,
  };

  type t = segmentAnimationT;

  let make = (pos, color, runningTime) => {
    pos,
    color,
    runningTime,
    elapsedTime: 0.0,
  };

  let update = (sa: segmentAnimationT) => {
    ...sa,
    elapsedTime: sa.elapsedTime +. 1.0,
    color: {
      ...sa.color,
      a: 1.0 -. sa.elapsedTime /. sa.runningTime,
    },
  };

  let draw = (env, segmentAnimation: t) => {
    let size = float_of_int(segmentSize) +. segmentAnimation.elapsedTime /. 2.0;
    drawSegment(
      ~color=segmentAnimation.color,
      ~boardPosition=segmentAnimation.pos,
      ~height=size,
      ~width=size,
      env,
    );
  };

  let updateAll = (segmentAnimations: list(t)) =>
    segmentAnimations
    |> List.map(update)
    |> List.filter(sa => sa.elapsedTime < sa.runningTime);
};

type directionT =
  | North
  | East
  | South
  | West;

type snakeT = {
  dir: directionT,
  segments: list(segmentPositionT),
};

type msgT =
  | ChangeDirection(directionT)
  | AddNewFruit(segmentPositionT)
  | UpdateGameState
  | ResetGameState
  | TogglePause;

type stateT = {
  paused: bool,
  fruit: segmentPositionT,
  snake: snakeT,
  segmentAnimations: list(SegmentAnimation.t),
};

let createNewSnakeHead = snake => {
  let {x, y} = List.hd(snake.segments);
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
      snake.segments->List.rev->List.tl->List.rev;
    } else {
      snake.segments;
    };

  {...snake, segments: [newHead] @ rest};
};

let resetGameState = () => {
  paused: true,
  fruit: {
    x: 10,
    y: 5,
  },
  snake: {
    dir: North,
    segments: [{x: 10, y: 9}, {x: 10, y: 10}, {x: 10, y: 11}],
  },
  segmentAnimations: [],
};

let inputMap = [
  (Reprocessing_Events.Up, ChangeDirection(North)),
  (Reprocessing_Events.Down, ChangeDirection(South)),
  (Reprocessing_Events.Left, ChangeDirection(East)),
  (Reprocessing_Events.Right, ChangeDirection(West)),
  (Reprocessing_Events.J, ChangeDirection(South)),
  (Reprocessing_Events.K, ChangeDirection(North)),
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
      {
        ...state,
        snake: updateSnake(false, state.snake),
        segmentAnimations: [
          SegmentAnimation.make(state.fruit, fruitColor, 60.0),
          ...state.segmentAnimations,
        ],
      },
      [
        AddNewFruit({x: Random.int(boardSizeX), y: Random.int(boardSizeY)}),
      ],
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

  let state = loop(state, messages);
  {
    ...state,
    segmentAnimations: SegmentAnimation.updateAll(state.segmentAnimations),
  };
};

let setup = env => {
  Env.size(~width=400, ~height=400, env);
  resetGameState();
};

let drawSnake = (snake, env) =>
  List.iter(
    p => drawSegment(~color=snakeColor, ~boardPosition=p, env),
    snake.segments,
  );

let drawFruit = (fruit, env) =>
  drawSegment(~color=fruitColor, ~boardPosition=fruit, env);

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
  List.iter(s => SegmentAnimation.draw(env, s), state.segmentAnimations);

  state;
};

run(~setup, ~draw, ());