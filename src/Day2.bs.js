// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Js__Int = require("rescript-js/src/Js__Int.bs.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

var directions = Fs.readFileSync("src/assets/day2-movements.txt", "ascii").split("\n").map(function (line) {
      var match = line.split(new RegExp("\\s+"));
      if (match.length !== 2) {
        return ;
      }
      var dir = match[0];
      if (dir === undefined) {
        return ;
      }
      var dis = match[1];
      if (dis === undefined) {
        return ;
      }
      var distance = Belt_Option.getWithDefault(Js__Int.fromString(dis), 0);
      switch (dir) {
        case "down" :
            return {
                    TAG: /* Down */2,
                    _0: distance
                  };
        case "forward" :
            return {
                    TAG: /* Forward */0,
                    _0: distance
                  };
        case "up" :
            return {
                    TAG: /* Up */1,
                    _0: distance
                  };
        default:
          return /* Idle */0;
      }
    });

var initialPosition = {
  horizontal: 0,
  depth: 0
};

var position = directions.reduce((function (position, instruction) {
        if (instruction === undefined) {
          return position;
        }
        if (typeof instruction === "number") {
          return position;
        }
        switch (instruction.TAG | 0) {
          case /* Forward */0 :
              return {
                      horizontal: position.horizontal + instruction._0 | 0,
                      depth: position.depth
                    };
          case /* Up */1 :
              return {
                      horizontal: position.horizontal,
                      depth: position.depth - instruction._0 | 0
                    };
          case /* Down */2 :
              return {
                      horizontal: position.horizontal,
                      depth: position.depth + instruction._0 | 0
                    };
          
        }
      }), initialPosition);

var Part1 = {
  initialPosition: initialPosition,
  position: position
};

var initialPosition$1 = {
  aim: 0,
  horizontal: 0,
  depth: 0
};

var position$1 = directions.reduce((function (position, instruction) {
        if (instruction === undefined) {
          return position;
        }
        if (typeof instruction === "number") {
          return position;
        }
        switch (instruction.TAG | 0) {
          case /* Forward */0 :
              var distance = instruction._0;
              return {
                      aim: position.aim,
                      horizontal: position.horizontal + distance | 0,
                      depth: position.depth + Math.imul(position.aim, distance) | 0
                    };
          case /* Up */1 :
              return {
                      aim: position.aim - instruction._0 | 0,
                      horizontal: position.horizontal,
                      depth: position.depth
                    };
          case /* Down */2 :
              return {
                      aim: position.aim + instruction._0 | 0,
                      horizontal: position.horizontal,
                      depth: position.depth
                    };
          
        }
      }), initialPosition$1);

var Part2 = {
  initialPosition: initialPosition$1,
  position: position$1
};

console.log(Math.imul(position.horizontal, position.depth), position);

console.log(Math.imul(position$1.horizontal, position$1.depth), position$1);

exports.directions = directions;
exports.Part1 = Part1;
exports.Part2 = Part2;
/* directions Not a pure module */