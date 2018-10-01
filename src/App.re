type state = {component: ReasonReact.reactElement};

type action =
  | RenderComponent(ReasonReact.reactElement);

let component = ReasonReact.reducerComponent("App");

/* Simple Helper */
let toString = ReasonReact.string;

let make = _children => {
  ...component,
  initialState: () => {component: ReasonReact.null},
  reducer: (action, _state) =>
    switch (action) {
    | RenderComponent(newComponent) =>
      ReasonReact.Update({component: newComponent})
    },
  didMount: self => {
    let watchUrl = (url: ReasonReact.Router.url) =>
      switch (url.path) {
      | ["setup"] => self.send(RenderComponent(<SetupScreen />))

      | _ => self.send(RenderComponent(<HomeScreen />))
      };

    let watcherID = ReasonReact.Router.watchUrl(watchUrl);

    watchUrl(ReasonReact.Router.dangerouslyGetInitialUrl());

    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
  },
  render: self => self.state.component,
};