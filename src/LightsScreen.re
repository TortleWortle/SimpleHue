open Belt;

type status =
  | Loading
  | Failed
  | Loaded;

type state = {
  lights: list(Hue.light),
  status,
};

type action =
  | LoadLights
  | RefreshLights
  | LightsLoaded(list(Hue.light))
  | LightsFailed;

let component = ReasonReact.reducerComponent("LightsScreen");

let make = (~station, _children) => {
  ...component,
  initialState: () => {lights: [], status: Loading},
  reducer: (action, state) =>
    switch (action) {
    | LoadLights =>
      ReasonReact.UpdateWithSideEffects(
        {...state, status: Loading},
        (
          self =>
            Js.Promise.(
              Hue.getLights(station)
              |> then_(lights => self.send(LightsLoaded(lights)) |> resolve)
              |> catch(_err => self.send(LightsFailed) |> resolve)
              |> ignore
            )
        ),
      )
    | RefreshLights =>
      ReasonReact.SideEffects(
        (
          self =>
            Js.Promise.(
              Hue.getLights(station)
              |> then_(lights => self.send(LightsLoaded(lights)) |> resolve)
              |> catch(_err => self.send(LightsFailed) |> resolve)
              |> ignore
            )
        ),
      )
    | LightsLoaded(lights) => ReasonReact.Update({lights, status: Loaded})
    | LightsFailed => ReasonReact.Update({...state, status: Failed})
    },
  didMount: self => {
    let intervalId =
      Js.Global.setInterval(() => self.send(LoadLights), 5000);
    self.onUnmount(() => Js.Global.clearInterval(intervalId));
    self.send(LoadLights);
  },
  render: self =>
    <div className="container">
      <button onClick={_event => self.send(LoadLights)}>
        {ReasonReact.string("Refresh")}
      </button>
      {
        switch (self.state.status) {
        | Loading => ReasonReact.string("Loading lights...")
        | Failed => ReasonReact.string("Failed to load lights.")
        | Loaded => ReasonReact.string("Lights loaded.")
        }
      }
      <table className="table">
        <thead>
          <tr>
            <th> {ReasonReact.string("Id")} </th>
            <th> {ReasonReact.string("Name")} </th>
            <th> {ReasonReact.string("Brightness")} </th>
            <th> {ReasonReact.string("Toggle")} </th>
          </tr>
        </thead>
        <tbody>
          {
            self.state.lights
            |> List.map(_, light =>
                 <SimpleLight
                   light
                   key={light.id ++ string_of_bool(light.state.on)}
                 />
               )
            |> List.toArray
            |> ReasonReact.array
          }
        </tbody>
      </table>
    </div>,
};