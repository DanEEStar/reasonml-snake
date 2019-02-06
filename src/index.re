open Reprocessing;

type boardPosition = {
  x: int,
  y: int,
};

type direction =
  | North
  | East
  | South
  | West;

type snakeT = {
  dir: direction,
  parts: list(boardPosition),
};

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
  {...snake, parts: [newHead] @ snake.parts};
};

/*
 updateSnakeSegments : Bool -> BoardPosition -> Snake -> Snake
 updateSnakeSegments drop newHead snake =
     let
         newParts =
             if drop then
                 newHead ::: dropLast snake.parts
             else
                 newHead ::: snake.parts
     in
     { snake | parts = newParts }
 */

type state = {snake: snakeT};

let initSnake = () => {
  dir: North,
  parts: [{x: 9, y: 10}, {x: 10, y: 10}, {x: 11, y: 10}],
};

let setup = env => {
  Env.size(~width=400, ~height=400, env);
  initSnake();
};

let drawSnake = (snake, env) => {
  Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
  List.iter(
    part =>
      Draw.rect(~pos=(part.x * 20, part.y * 20), ~width=20, ~height=20, env),
    snake.parts,
  );
};

let draw = (snake, env) => {
  /* let newSnake = {...snake, parts: updateSnakeSegments(false, snake)} */

  let snake =
    if (Env.keyPressed(A, env)) {
      initSnake();
    } else {
      snake;
    };

  let snake =
    if (Env.keyPressed(Up, env)) {
      updateSnake(false, {...snake, dir: North});
    } else {
      snake;
    };

  let snake =
    if (Env.keyPressed(Left, env)) {
      updateSnake(false, {...snake, dir: East});
    } else {
      snake;
    };

  let snake =
    if (Env.keyPressed(Right, env)) {
      updateSnake(false, {...snake, dir: West});
    } else {
      snake;
    };

  let snake =
    if (Env.keyPressed(Down, env)) {
      updateSnake(false, {...snake, dir: South});
    } else {
      snake;
    };

  Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);
  drawSnake(snake, env);
  snake;
};

run(~setup, ~draw, ());