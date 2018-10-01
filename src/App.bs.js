// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var HomeScreen = require("./HomeScreen.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var SetupScreen = require("./SetupScreen.bs.js");

var component = ReasonReact.reducerComponent("App");

function toString(prim) {
  return prim;
}

function make() {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              var watchUrl = function (url) {
                var match = url[/* path */0];
                var exit = 0;
                if (match && match[0] === "setup" && !match[1]) {
                  return Curry._1(self[/* send */3], /* RenderComponent */[ReasonReact.element(undefined, undefined, SetupScreen.make(/* array */[]))]);
                } else {
                  exit = 1;
                }
                if (exit === 1) {
                  return Curry._1(self[/* send */3], /* RenderComponent */[ReasonReact.element(undefined, undefined, HomeScreen.make(/* array */[]))]);
                }
                
              };
              var watcherID = ReasonReact.Router[/* watchUrl */1](watchUrl);
              watchUrl(ReasonReact.Router[/* dangerouslyGetInitialUrl */3](/* () */0));
              return Curry._1(self[/* onUnmount */4], (function () {
                            return ReasonReact.Router[/* unwatchUrl */2](watcherID);
                          }));
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return self[/* state */1][/* component */0];
            }),
          /* initialState */(function () {
              return /* record */[/* component */null];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, _) {
              return /* Update */Block.__(0, [/* record */[/* component */action[0]]]);
            }),
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

exports.component = component;
exports.toString = toString;
exports.make = make;
/* component Not a pure module */
