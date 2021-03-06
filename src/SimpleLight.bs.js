// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Hue = require("./Hue.bs.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Debouncer = require("re-debouncer/src/Debouncer.bs.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

var component = ReasonReact.reducerComponent("SimpleLight");

function updateLight(oldLight, newLight, self) {
  Hue.updateLight(newLight).then((function () {
            return Promise.resolve(Curry._1(self[/* send */3], /* UpdateLightState */Block.__(1, [newLight])));
          })).catch((function () {
          return Promise.resolve((alert("Could not update light."), Curry._1(self[/* send */3], /* UpdateLightState */Block.__(1, [oldLight]))));
        }));
  return /* () */0;
}

var updateBrightness = Debouncer.make(500, (function (self) {
        return updateLight(self[/* state */1][/* oldLight */1], self[/* state */1][/* light */0], self);
      }));

function make(light, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              var light = self[/* state */1][/* light */0];
              var match = light[/* state */1][/* on */0];
              return React.createElement("tr", {
                          className: match ? "bg-success" : "bg-danger"
                        }, React.createElement("td", undefined, light[/* id */0]), React.createElement("td", undefined, light[/* name */2]), React.createElement("td", undefined, React.createElement("input", {
                                  max: "254",
                                  min: 0,
                                  type: "range",
                                  value: String(light[/* state */1][/* brightness */1]),
                                  onChange: (function ($$event) {
                                      return Curry._1(self[/* send */3], /* SetBrightness */Block.__(0, [Caml_format.caml_int_of_string($$event.target.value)]));
                                    })
                                }), React.createElement("label", undefined, String(light[/* state */1][/* brightness */1]))), React.createElement("td", undefined, React.createElement("button", {
                                  onClick: (function () {
                                      return Curry._1(self[/* send */3], /* Toggle */0);
                                    })
                                }, "Toggle")));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* light */light,
                      /* oldLight */light
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (typeof action === "number") {
                var init = state[/* light */0];
                var init$1 = state[/* light */0][/* state */1];
                var newLight_000 = /* id */init[/* id */0];
                var newLight_001 = /* state : record */[
                  /* on */!state[/* light */0][/* state */1][/* on */0],
                  /* brightness */init$1[/* brightness */1],
                  /* hue */init$1[/* hue */2],
                  /* sat */init$1[/* sat */3]
                ];
                var newLight_002 = /* name */init[/* name */2];
                var newLight = /* record */[
                  newLight_000,
                  newLight_001,
                  newLight_002
                ];
                var partial_arg = state[/* light */0];
                return /* SideEffects */Block.__(1, [(function (param) {
                              return updateLight(partial_arg, newLight, param);
                            })]);
              } else if (action.tag) {
                return /* Update */Block.__(0, [/* record */[
                            /* light */action[0],
                            /* oldLight */state[/* oldLight */1]
                          ]]);
              } else {
                var oldLight = state[/* light */0];
                var init$2 = state[/* light */0];
                var init$3 = state[/* light */0][/* state */1];
                var newLight_000$1 = /* id */init$2[/* id */0];
                var newLight_001$1 = /* state : record */[
                  /* on */true,
                  /* brightness */action[0],
                  /* hue */init$3[/* hue */2],
                  /* sat */init$3[/* sat */3]
                ];
                var newLight_002$1 = /* name */init$2[/* name */2];
                var newLight$1 = /* record */[
                  newLight_000$1,
                  newLight_001$1,
                  newLight_002$1
                ];
                return /* UpdateWithSideEffects */Block.__(2, [
                          /* record */[
                            /* light */newLight$1,
                            /* oldLight */oldLight
                          ],
                          updateBrightness
                        ]);
              }
            }),
          /* jsElementWrapped */component[/* jsElementWrapped */13]
        ];
}

exports.component = component;
exports.updateLight = updateLight;
exports.updateBrightness = updateBrightness;
exports.make = make;
/* component Not a pure module */
