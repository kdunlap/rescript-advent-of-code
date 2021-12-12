open ReScriptJs.Js

type depthResult = {
  previous: option<int>,
  increased: bool
}  

let lines = Node.Fs.readFileSync("src/assets/day1-report.txt", #ascii)
  ->String.split("\n")
  ->Array.map(l => l->Int.fromString->Belt.Option.getWithDefault(0))

let countPreviousIncreases = (arr: array<int>) =>
    arr->Array.mapWithIndex((value, index) => 
      switch arr->Array.get(index - 1) {
      | None => ({ 
          previous: None, 
          increased: false
        })
      | Some(previous) => ({ 
          previous: Some(previous),
          increased: value > previous
        })
      })
  ->Array.reduce((sum, result) => result.increased ? sum + 1 : sum, 0)


module Part1 = {
  let total = lines->countPreviousIncreases
}

module Part2 = {
  let total = lines->Array.sliceToEnd(~from=2)
    ->Array.mapWithIndex((_value, index) => 
      lines->Array.slice(~from=index, ~end=index + 3)
        ->Array.reduce((total, value) => total + value, 0)
    )
    ->countPreviousIncreases
}


Js.Console.log2("Part 1: ", Part1.total)
Js.Console.log2("Part 2: ", Part2.total)
