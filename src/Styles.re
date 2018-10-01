open Css;

let discoveryWrap = [display(`flex)]->style;

let box =
  [
    color(red),
    height(vh(10.0)),
    width(vw(10.0)),
    backgroundColor(grey),
    fontWeight(500),
  ]
  ->style;