open Reprocessing;

let segmentSize = 20;
let boardSizeX = 20;
let boardSizeY = 20;
let snakeColor = Utils.color(~r=41, ~g=166, ~b=244, ~a=255);
let snakeHeadColor = Utils.color(~r=66, ~g=122, ~b=244, ~a=255);
let fruitColor = Utils.color(~r=255, ~g=80, ~b=41, ~a=255);
let tickCount = 10;
let fruitDigestSpeed = 5.0;

let messageReduce = (state: 'a, messages: list('b), messageHandler) => {
  let rec loop = (state: 'a, messages: list('b)) => {
    let (newState, newMessages) = List.fold_left(messageHandler, (state, []), messages);
    switch (newMessages) {
    | [] => newState
    | _ => loop(newState, newMessages)
    };
  };
  loop(state, messages);
};

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

let offsetFromNeighbour = (pos1, pos2, drawPercentage) =>
  switch (directionFromNeighbour(pos1, pos2)) {
  | North => (0.0, float_of_int(segmentSize) *. drawPercentage -. float_of_int(segmentSize))
  | South => (0.0, float_of_int(- segmentSize) *. drawPercentage +. float_of_int(segmentSize))
  | East => (float_of_int(segmentSize) *. drawPercentage -. float_of_int(segmentSize), 0.0)
  | West => (float_of_int(- segmentSize) *. drawPercentage +. float_of_int(segmentSize), 0.0)
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
    mutable headOffset: (float, float),
    mutable fruitOffsets: list(float),
    mutable drawPercentage: float,
    mutable continuousDrawing: bool,
  };

  type gameStateT =
    | Playing
    | GameOverWaiting
    | GameOver;

  type stateT = {
    paused: bool,
    fruit: segmentPositionT,
    snake: snakeT,
    gameState: gameStateT,
    visualState: visualStateT,
  };

  type t = stateT;

  type msgT =
    | ChangeDirection(directionT)
    | AddNewFruit(segmentPositionT)
    | TickGameState
    | ResetGameState
    | TogglePause
    | ToggleContinuousDrawing
    | SetGameOver;

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
    paused: false,
    fruit: {
      x: 10,
      y: 5,
    },
    snake: {
      dir: North,
      segments: [
        {x: 10, y: 9}, {x: 10, y: 10}, {x: 10, y: 11}, {x: 10, y: 12}
      ]
    },
    visualState: {
      segmentAnimations: [],
      headOffset: (0.0, 0.0),
      fruitOffsets: [],
      drawPercentage: 0.0,
      continuousDrawing: true,
    },
    gameState: Playing,
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
    (Reprocessing_Events.M, SetGameOver),
  ];

  let handleInput = env => List.filter(input => Env.keyPressed(fst(input), env), inputMap) |> List.map(snd);

  let visualHandleSnakeFruitAnimation = (state: stateT, pos: segmentPositionT) => {
    ...state,
    visualState: {
      ...state.visualState,
      segmentAnimations: [SegmentAnimation.make(pos, fruitColor, 60.0), ...state.visualState.segmentAnimations],
      fruitOffsets: [0.0],
    },
  };

  let tickGameState = state =>
    if (state.gameState == Playing) {
      let newHead = createNewSnakeHead(state.snake);
      let result =
        if (newHead == state.fruit) {
          let state = {...state, snake: updateSnake(false, state.snake)};
          let state = visualHandleSnakeFruitAnimation(state, state.fruit);
          (state, [AddNewFruit({x: Random.int(boardSizeX), y: Random.int(boardSizeY)})]);
        } else if (List.mem(newHead, state.snake.segments)) {
          (state, [SetGameOver]);
        } else {
          ({...state, snake: updateSnake(true, state.snake)}, []);
        };
      fst(result).visualState.drawPercentage = 0.0;
      result;
    } else {
      (state, []);
    };

  let playStateMessageHandler = ((state, _), message) =>
    switch (message) {
    | ChangeDirection(direction) => (updateSnakeDirection(state, direction), [])
    | AddNewFruit(fruitPos) => ({...state, fruit: fruitPos}, [])
    | TickGameState => tickGameState(state)
    | SetGameOver => ({...state, gameState: GameOver}, [])
    | ResetGameState => (resetGameState(), [])
    | TogglePause => ({...state, paused: !state.paused}, [])
    | ToggleContinuousDrawing =>
      state.visualState.continuousDrawing = !state.visualState.continuousDrawing;
      (state, []);
    };

  let updateVisualState = (state: stateT) => {
    let visualState = {
      ...state.visualState,
      segmentAnimations: SegmentAnimation.updateAll(state.visualState.segmentAnimations),
      fruitOffsets: List.map(o => o +. fruitDigestSpeed, state.visualState.fruitOffsets),
      drawPercentage:
        if (state.paused) {
          state.visualState.drawPercentage;
        } else {
          state.visualState.drawPercentage +. 1.0 /. float_of_int(tickCount);
        },
    };

    visualState;
  };

  let drawSnake = (state, env) =>
    if (state.visualState.continuousDrawing) {
      let revSegments = List.rev(state.snake.segments);
      let previousSegment = ref(List.hd(revSegments));

      List.iteri(
        (index, pos) => {
          if (index == 0) {
            /* last segment */
            drawSegmentPercentage(
              ~color=snakeColor,
              ~pos,
              ~percentage=1.0 -. state.visualState.drawPercentage,
              ~dir=directionFromNeighbour(pos, revSegments |> List.tl |> List.hd),
              env,
            );
          } else if (List.length(state.snake.segments) == index + 1) {
            /* first segment */
            let (xOffset, yOffset) = offsetFromNeighbour(pos, previousSegment^, state.visualState.drawPercentage);
            state.visualState.headOffset = (xOffset, yOffset);
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
      if (frameCount mod tickCount == 0 && !state.paused) {
        [TickGameState, ...inputMessages];
      } else {
        inputMessages;
      };
    let state = messageReduce(state, messages, playStateMessageHandler);

    if (state.gameState == Playing) {
      {...state, visualState: updateVisualState(state)};
    } else {
      state;
    };
  };

  let draw = (state, env) => {
    Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);
    drawSnake(state, env);
    drawFruit(state, 1.0 +. sin(float_of_int(Env.frameCount(env)) /. 20.0) /. 5.0, env);
    /*List.iter(s => SegmentAnimation.draw(env, s), state.visualState.segmentAnimations);*/

    if (List.length(state.visualState.fruitOffsets) > 0) {
      let offset = List.hd(state.visualState.fruitOffsets);
      let drawFruitPosIndex = int_of_float((offset) /. float_of_int(segmentSize));

      if (drawFruitPosIndex < List.length(state.snake.segments) - 2) {
        let pos = List.nth(state.snake.segments, drawFruitPosIndex);
        let posBefore = List.nth(state.snake.segments, drawFruitPosIndex + 1)
        let drawFruitPosIndex2 = int_of_float((offset +. 1.0 +. (1.0 -. state.visualState.drawPercentage) *. float_of_int(segmentSize)) /. float_of_int(segmentSize));
        let (xOffsetHead, yOffsetHead) = offsetFromNeighbour(pos, posBefore, state.visualState.drawPercentage);
        /*let (xOffsetHead, yOffsetHead) = offsetFromNeighbour(pos, posBefore, 1.0);*/
        let pos1 = List.nth(state.snake.segments, drawFruitPosIndex2);
        let pos2 = List.nth(state.snake.segments, drawFruitPosIndex2 + 1)

        /*
        print_int(pos.x);
        print_string(", ")
        print_int(pos.y);
        print_string(" -> ")
        print_int(posBefore.x);
        print_string(", ")
        print_int(posBefore.y);
        print_string(" -> ")
        print_int(pos1.x);
        print_string(", ")
        print_int(pos1.y);
        print_string(" -> ")
        print_int(pos2.x);
        print_string(", ")
        print_int(pos2.y);
        print_string(" -> ")
        print_int(drawFruitPosIndex);
        print_string(" -> ")
        print_float(xOffsetHead);
        print_string(", ")
        print_float(yOffsetHead);
        print_string(" -- ")
        print_float(offset);
        print_endline("");
        */

        let blockOffset = float_of_int(drawFruitPosIndex) *. float_of_int(segmentSize);
        let percentageSegmentOffset = state.visualState.drawPercentage *. float_of_int(segmentSize);
        let drawOffset = offset -. blockOffset -. percentageSegmentOffset;

        print_float(drawOffset);
        print_endline("");

        if(pos.y < posBefore.y) {
          if(pos1.y < pos2.y) {
            let yOffset = -.mod_float(offset, float_of_int(segmentSize)) +. yOffsetHead;
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos, ~yOffset, env);
          } else if (pos1.x > pos2.x) {
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos1, ~xOffset=drawOffset, env);
          } else {
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos1, ~xOffset=-.drawOffset, env);
          }
        } else if(pos.y > posBefore.y) {
          if(pos1.y > pos2.y) {
            let yOffset = +.mod_float(offset, float_of_int(segmentSize)) +. yOffsetHead;
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos, ~yOffset, env);
          } else if (pos1.x > pos2.x) {
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos1, ~xOffset=drawOffset, env);
          } else {
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos1, ~xOffset=-.drawOffset, env);
          }
        } else if(pos.x < posBefore.x) {
          if(pos1.x < pos2.x) {
            let xOffset = -.mod_float(offset, float_of_int(segmentSize)) +. xOffsetHead;
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos, ~xOffset, env);
          } else if (pos1.y > pos2.y ) {
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos1, ~yOffset=drawOffset, env);
          } else {
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos1, ~yOffset=-.drawOffset, env);
          }
        } else if(pos.x > posBefore.x) {
          if(pos1.x > pos2.x) {
            let xOffset = +.mod_float(offset, float_of_int(segmentSize)) +. xOffsetHead;
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos, ~xOffset, env);
          } else if (pos1.y > pos2.y ) {
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos1, ~yOffset=drawOffset, env);
          } else {
            drawSegment(~color={...fruitColor, a: 0.7}, ~pos=pos1, ~yOffset=-.drawOffset, env);
          }
        }
      } else {
        ();
      }
    } else {
      ();
    };
  };

  let drawAndUpdate = (state: stateT, env) => {
    let messages = handleInput(env);
    let state = update(state, messages, Env.frameCount(env));
    draw(state, env);
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

type msgT =
  | StartGame
  | Reset
  | FruitOffset;

let setup = env => {
  Env.size(~width=400, ~height=400, env);
  {menuState: GameInProgress, playState: SnakeGame.resetGameState()};
};

let drawAndUpdateMenu = (state, env) => {
  let inputMap = [
    (Reprocessing_Events.C, StartGame),
    (Reprocessing_Events.R, Reset),
    (Reprocessing_Events.Space, FruitOffset)
  ];

  let handleInput = env => List.filter(input => Env.keyPressed(fst(input), env), inputMap) |> List.map(snd);

  let messageHandler = ((state, _), message) =>
    switch (message) {
    | Reset => ({menuState: StartMenu, playState: SnakeGame.resetGameState()}, [])
    | StartGame => ({...state, menuState: GameInProgress, playState: SnakeGame.resetGameState()}, [])
    | FruitOffset => ({...state, playState: {...state.playState, visualState: {...state.playState.visualState, fruitOffsets: List.map((f) => f +. 4.0, state.playState.visualState.fruitOffsets)}}}, [])
    };

  let state = messageReduce(state, handleInput(env), messageHandler);

  SnakeGame.draw(state.playState, env);

  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=100), env);
  Draw.rect(~pos=(0, 0), ~width=400, ~height=400, env);

  state;
};

let draw = (state: stateT, env) =>
  switch (state.menuState) {
  | StartMenu => drawAndUpdateMenu(state, env)
  | GameInProgress =>
    if (state.playState.gameState == SnakeGame.GameOver) {
      {...state, menuState: StartMenu};
    } else {
      {...state, playState: SnakeGame.drawAndUpdate(state.playState, env)};
    }
  | GameOver => drawAndUpdateMenu(state, env)
  };

 run(~setup, ~draw, ());