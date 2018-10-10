type routes =
  | Home
  | Setup
  | Lights
  | NotFound;

type state = {component: ReasonReact.reactElement};

type action =
  | SetComponent(ReasonReact.reactElement);

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {component: ReasonReact.null},
  reducer: (action, _state) =>
    switch (action) {
    | SetComponent(component) => ReasonReact.Update({component: component})
    },
  didMount: self => {
    let watchUrl = (url: ReasonReact.Router.url) =>
      /* I'm routing like this because there are only two possible states and screens, you either setup or you control the lights directly. */
      self.send(
        SetComponent(
          switch (url.path) {
          | [] =>
            /* Set route to lights automatically if there's a basestation configured */
            switch (Hue.getLinkedStation()) {
            | Some(station) => <LightsScreen station />
            | None => <SetupScreen />
            }

          | ["setup"] => <SetupScreen />

          | _ => ReasonReact.string("Not Found.")
          },
        ),
      );

    let watcherID = ReasonReact.Router.watchUrl(watchUrl);

    watchUrl(ReasonReact.Router.dangerouslyGetInitialUrl());

    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
  },
  render: self => self.state.component,
};