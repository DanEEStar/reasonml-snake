open Reprocessing;

let segmentSize = 20;
let boardSizeX = 20;
let boardSizeY = 20;
let snakeColor = Utils.color(~r=41, ~g=166, ~b=244, ~a=255);
let snakeHeadColor = Utils.color(~r=66, ~g=122, ~b=244, ~a=255);
let fruitColor = Utils.color(~r=255, ~g=80, ~b=41, ~a=255);

type segmentPositionT = {
  x: int,
  y: int,
};

type directionT =
  | North
  | East
  | South
  | West;

let directionFromNeighbour = (pos1, pos2) =>
  if (pos1.x > pos2.x) {
    West;
  } else if (pos1.x < pos2.x) {
    East;
  } else if (pos1.y > pos2.y) {
    South;
  } else {
    North;
  };

let drawSegment =
    (
      ~color,
      ~pos,
      ~sizeMultiplier=1.0,
      ~width=float_of_int(segmentSize) *. sizeMultiplier,
      ~height=float_of_int(segmentSize) *. sizeMultiplier,
      ~xOffset=0.0,
      ~yOffset=0.0,
      env,
    ) => {
  let x = float_of_int(pos.x * segmentSize + segmentSize / 2) -. width /. 2.0 -. xOffset;
  let y = float_of_int(pos.y * segmentSize + segmentSize / 2) -. height /. 2.0 -. yOffset;
  Draw.fill(color, env);
  Draw.rectf(~pos=(x, y), ~width, ~height, env);
};

let drawSegmentPercentage = (~color, ~pos, ~percentage, ~dir, env) =>
  switch (dir) {
  | North =>
    let height = float_of_int(segmentSize) *. percentage;
    let yOffset = float_of_int(- segmentSize) *. (1.0 -. percentage) /. 2.0;
    drawSegment(~color, ~pos, ~height, ~yOffset, env);
  | South =>
    let height = float_of_int(segmentSize) *. percentage;
    let yOffset = float_of_int(segmentSize) *. (1.0 -. percentage) /. 2.0;
    drawSegment(~color, ~pos, ~height, ~yOffset, env);
  | East =>
    let width = float_of_int(segmentSize) *. percentage;
    let xOffset = float_of_int(- segmentSize) *. (1.0 -. percentage) /. 2.0;
    drawSegment(~color, ~pos, ~width, ~xOffset, env);
  | West =>
    let width = float_of_int(segmentSize) *. percentage;
    let xOffset = float_of_int(segmentSize) *. (1.0 -. percentage) /. 2.0;
    drawSegment(~color, ~pos, ~width, ~xOffset, env);
  };

module SegmentAnimation = {
  type segmentAnimationT = {
    pos: segmentPositionT,
    color: Reprocessing.colorT,
    runningTime: float,
    elapsedTime: float,
  };

  type t = segmentAnimationT;

  let make = (pos, color, runningTime) => {pos, color, runningTime, elapsedTime: 0.0};

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
    drawSegment(~color=segmentAnimation.color, ~pos=segmentAnimation.pos, ~sizeMultiplier=size, env);
  };

  let updateAll = (segmentAnimations: list(t)) =>
    segmentAnimations |> List.map(update) |> List.filter(sa => sa.elapsedTime < sa.runningTime);
};

module SnakeGame = {
  type snakeT = {
    dir: directionT,
    segments: list(segmentPositionT),
  };
  type visualStateT = {
    segmentAnimations: list(SegmentAnimation.t),
    drawBigSnakeFruitSegment: segmentPositionT,
    mutable drawPercentage: float,
    mutable continuousDrawing: bool,
  };

  type stateT = {
    paused: bool,
    fruit: segmentPositionT,
    snake: snakeT,
    visualState: visualStateT,
  };

  type t = stateT;

  type msgT =
    | ChangeDirection(directionT)
    | AddNewFruit(segmentPositionT)
    | TickGameState
    | ResetGameState
    | TogglePause
    | ToggleContinuousDrawing;

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
      segments: [{x: 10, y: 9}, {x: 10, y: 10}, {x: 10, y: 11}, {x: 10, y: 12}],
    },
    visualState: {
      segmentAnimations: [],
      drawBigSnakeFruitSegment: {
        x: (-1),
        y: (-1),
      },
      drawPercentage: 0.0,
      continuousDrawing: true,
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
    (Reprocessing_Events.U, TickGameState),
    (Reprocessing_Events.C, ToggleContinuousDrawing),
    (Reprocessing_Events.Space, TogglePause),
  ];

  let handleInput = env => List.filter(input => Env.keyPressed(fst(input), env), inputMap) |> List.map(snd);

  let visualHandleSnakeFruitAnimation = (state: stateT, pos: segmentPositionT) => {
    ...state,
    visualState: {
      ...state.visualState,
      segmentAnimations: [SegmentAnimation.make(pos, fruitColor, 60.0), ...state.visualState.segmentAnimations],
      drawBigSnakeFruitSegment: pos,
    },
  };

  let tickGameState = state => {
    let newHead = createNewSnakeHead(state.snake);
    let result =
      if (newHead == state.fruit) {
        let state = {...state, snake: updateSnake(false, state.snake)};
        let state = visualHandleSnakeFruitAnimation(state, state.fruit);
        (state, [AddNewFruit({x: Random.int(boardSizeX), y: Random.int(boardSizeY)})]);
      } else {
        ({...state, snake: updateSnake(true, state.snake)}, []);
      };
    fst(result).visualState.drawPercentage = 0.0;
    result;
  };

  let updatePlayState = (state: stateT, messages: list(msgT)) => {
    let rec loop = (state: stateT, messages: list(msgT)) => {
      let (newState, newMessages) =
        List.fold_left(
          ((state, _messages), message) =>
            switch (message) {
            | ChangeDirection(direction) => (updateSnakeDirection(state, direction), [])
            | AddNewFruit(fruitPos) => ({...state, fruit: fruitPos}, [])
            | TickGameState => tickGameState(state)
            | ResetGameState => (resetGameState(), [])
            | TogglePause => ({...state, paused: !state.paused}, [])
            | ToggleContinuousDrawing =>
              state.visualState.continuousDrawing = !state.visualState.continuousDrawing;
              (state, []);
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
    ...state.visualState,
    segmentAnimations: SegmentAnimation.updateAll(state.visualState.segmentAnimations),
    drawBigSnakeFruitSegment:
      if (List.mem(state.visualState.drawBigSnakeFruitSegment, state.snake.segments)) {
        state.visualState.drawBigSnakeFruitSegment;
      } else {
        {x: (-1), y: (-1)};
      },
    drawPercentage:
      if (state.paused) {
        state.visualState.drawPercentage;
      } else {
        state.visualState.drawPercentage +. 0.1;
      },
  };

  let drawSnake = (state, env) =>
    if (state.visualState.continuousDrawing) {
      let revSegments = List.rev(state.snake.segments);
      let previousSegment = ref(List.hd(revSegments));

      List.iteri(
        (index, pos) => {
          if (index == 0) {
            drawSegmentPercentage(
              ~color=snakeColor,
              ~pos,
              ~percentage=1.0 -. state.visualState.drawPercentage,
              ~dir=directionFromNeighbour(pos, revSegments |> List.tl |> List.hd),
              env,
            );
          } else if (List.length(state.snake.segments) == index + 1) {
            let (xOffset, yOffset) =
              switch (directionFromNeighbour(pos, previousSegment^)) {
              | North => (
                  0.0,
                  float_of_int(segmentSize) *. state.visualState.drawPercentage -. float_of_int(segmentSize),
                )
              | South => (
                  0.0,
                  float_of_int(- segmentSize) *. state.visualState.drawPercentage +. float_of_int(segmentSize),
                )
              | East => (
                  float_of_int(segmentSize) *. state.visualState.drawPercentage -. float_of_int(segmentSize),
                  0.0,
                )
              | West => (
                  float_of_int(- segmentSize) *. state.visualState.drawPercentage +. float_of_int(segmentSize),
                  0.0,
                )
              };
            drawSegment(~color=snakeHeadColor, ~pos, ~xOffset, ~yOffset, env);
          } else {
            drawSegment(~color=snakeColor, ~pos, env);
          };
          previousSegment := pos;
        },
        revSegments,
      );
    } else {
      List.iter(pos => drawSegment(~color=snakeColor, ~pos, env), state.snake.segments);
    };

  let drawFruit = (state, sizeMultiplier, env) =>
    drawSegment(~color=fruitColor, ~pos=state.fruit, ~sizeMultiplier, env);

  let update = (state: stateT, inputMessages, frameCount) => {
    let messages =
      if (frameCount mod 10 == 0 && !state.paused) {
        [TickGameState, ...inputMessages];
      } else {
        inputMessages;
      };
    let state = updatePlayState(state, messages);
    {...state, visualState: updateVisualState(state)};
  };

  let draw = (state: stateT, env) => {
    let messages = handleInput(env);

    let state = update(state, messages, Env.frameCount(env));

    Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);
    drawSnake(state, env);
    drawFruit(state, 1.0 +. sin(float_of_int(Env.frameCount(env)) /. 20.0) /. 5.0, env);
    List.iter(s => SegmentAnimation.draw(env, s), state.visualState.segmentAnimations);
    if (List.mem(state.visualState.drawBigSnakeFruitSegment, state.snake.segments)) {
      drawSegment(~color={...fruitColor, a: 0.7}, ~pos=state.visualState.drawBigSnakeFruitSegment, env);
    };

    state;
  };
};

type menuStateT =
  | StartMenu
  | GameInProgress
  | GameOver;

type stateT = {
  menuState: menuStateT,
  playState: SnakeGame.t,
};

let inputMap = [];

let handleInput = env => List.filter(input => Env.keyPressed(fst(input), env), inputMap) |> List.map(snd);

let setup = env => {
  Env.size(~width=400, ~height=400, env);
  {menuState: GameInProgress, playState: SnakeGame.resetGameState()};
};

let draw = (state: stateT, env) => {...state, playState: SnakeGame.draw(state.playState, env)};

run(~setup, ~draw, ());