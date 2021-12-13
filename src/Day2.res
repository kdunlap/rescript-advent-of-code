open ReScriptJs.Js

type instruction = Forward(int) | Up(int) | Down(int) | Idle

let directions = Node.Fs.readFileSync("src/assets/day2-movements.txt", #ascii)
    ->String.split("\n")
    ->Array.map(line =>
      switch line->String.splitByRegExp(RegExp.fromString("\s+")) {
      | [Some(dir), Some(dis)] => 
        let distance = dis->Int.fromString->Belt.Option.getWithDefault(0)
        switch dir {
        | "forward" => Some(Forward(distance))
        | "up" => Some(Up(distance))
        | "down" => Some(Down(distance))
        | _ => Some(Idle)
        } 
      | _ => None
      }
    )

module Part1 = {

  type position = {
    horizontal: int,
    depth: int
  }

  let initialPosition = { horizontal: 0, depth: 0 }  

  // forward X increases the horizontal position by X units.
  // down X increases the depth by X units.
  // up X decreases the depth by X units.
  let position = directions->Array.reduce((position, instruction) => 
    switch instruction {
    | Some(Forward(distance)) => ({
      horizontal: position.horizontal + distance,
      depth: position.depth
    })
    | Some(Up(distance)) => ({
      horizontal: position.horizontal,
      depth: position.depth - distance
    })
    | Some(Down(distance)) => ({
      horizontal: position.horizontal,
      depth: position.depth + distance
    })
    | _ => position
    }
  , initialPosition)
}

module Part2 = {

  type position = {
    aim: int,
    horizontal: int,
    depth: int
  }

  let initialPosition = { aim: 0, horizontal: 0, depth: 0 }  

  // down X increases your aim by X units.
  // up X decreases your aim by X units.
  // forward X does two things:
  // It increases your horizontal position by X units.
  // It increases your depth by your aim multiplied by X.
  let position = directions->Array.reduce((position, instruction) => {
    switch instruction {
    | Some(Forward(distance)) => ({
        aim: position.aim,
        horizontal: position.horizontal + distance,
        depth: position.depth + (position.aim * distance)
    })
    | Some(Up(distance)) => ({
        aim: position.aim - distance,
        horizontal: position.horizontal,
        depth: position.depth
    })
    | Some(Down(distance)) => ({
        aim: position.aim + distance,
        horizontal: position.horizontal,
        depth: position.depth
    })
    | _ => position
    }
  }, initialPosition)
}

Js.Console.log2(Part1.position.horizontal * Part1.position.depth, Part1.position)

Js.Console.log2(Part2.position.horizontal * Part2.position.depth, Part2.position)