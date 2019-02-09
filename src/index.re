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

let drawSegment = (~color, ~pos, ~sizeMultiplier=1.0, env) => {
  let width = float_of_int(segmentSize) *. sizeMultiplier;
  let height = float_of_int(segmentSize) *. sizeMultiplier;
  let x = float_of_int(pos.x * segmentSize + segmentSize / 2) -. width /. 2.0;
  let y =
    float_of_int(pos.y * segmentSize + segmentSize / 2) -. height /. 2.0;
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
    let size = 1.0 +. segmentAnimation.elapsedTime /. 50.0;
    drawSegment(
      ~color=segmentAnimation.color,
      ~pos=segmentAnimation.pos,
      ~sizeMultiplier=size,
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

type visualStateT = {
  segmentAnimations: list(SegmentAnimation.t),
  drawBigSnakeFruitSegment: segmentPositionT,
};

type stateT = {
  paused: bool,
  fruit: segmentPositionT,
  snake: snakeT,
  visualState: visualStateT,
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
  visualState: {
    segmentAnimations: [],
    drawBigSnakeFruitSegment: {
      x: (-1),
      y: (-1),
    },
  },
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

let visualHandleSnakeFruitAnimation = (state: stateT, pos: segmentPositionT) => {
  ...state,
  visualState: {
    segmentAnimations: [
      SegmentAnimation.make(pos, fruitColor, 60.0),
      ...state.visualState.segmentAnimations,
    ],
    drawBigSnakeFruitSegment: pos,
  },
};

let updateGameState = state => {
  let newHead = createNewSnakeHead(state.snake);
  if (newHead == state.fruit) {
    let state = {...state, snake: updateSnake(false, state.snake)};
    let state = visualHandleSnakeFruitAnimation(state, state.fruit);
    (
      state,
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

  loop(state, messages);
};

let updateVisualState = (state: stateT) => {
  segmentAnimations:
    SegmentAnimation.updateAll(state.visualState.segmentAnimations),
  drawBigSnakeFruitSegment:
    if (List.mem(
          state.visualState.drawBigSnakeFruitSegment,
          state.snake.segments,
        )) {
      state.visualState.drawBigSnakeFruitSegment;
    } else {
      {x: (-1), y: (-1)};
    },
};

let setup = env => {
  Env.size(~width=400, ~height=400, env);
  resetGameState();
};

let drawSnake = (state, env) =>
  List.iteri(
    (index, p) => drawSegment(~color=snakeColor, ~pos=p, env),
    state.snake.segments,
  );

let drawFruit = (state, env) =>
  drawSegment(~color=fruitColor, ~pos=state.fruit, env);

let draw = (state: stateT, env) => {
  let messages = handleInput(env);

  let messages =
    if (Env.frameCount(env) mod 7 == 0 && !state.paused) {
      [UpdateGameState, ...messages];
    } else {
      messages;
    };
  let state = updateGame(state, messages);
  let state = {...state, visualState: updateVisualState(state)};

  Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);
  drawSnake(state, env);
  drawFruit(state, env);
  List.iter(
    s => SegmentAnimation.draw(env, s),
    state.visualState.segmentAnimations,
  );
  if (List.mem(
        state.visualState.drawBigSnakeFruitSegment,
        state.snake.segments,
      )) {
    drawSegment(
      ~color={...fruitColor, a: 0.7},
      ~pos=state.visualState.drawBigSnakeFruitSegment,
      env,
    );
  };

  state;
};

run(~setup, ~draw, ());