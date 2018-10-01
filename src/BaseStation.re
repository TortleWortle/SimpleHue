type stationStatus =
  | Linked
  | Linking
  | Success
  | Loading
  | Failed;

type state = {
  station: option(Hue.baseStation),
  status: stationStatus,
};

type action =
  | LoadingFinished(option(Hue.baseStation))
  | StartLoading
  | LoadingError;

let component = ReasonReact.reducerComponent("BaseStation");

let make = (~discoveredStation, _children) => {
  ...component,
  initialState: () => {station: None, status: Loading},
  reducer: (action, state) =>
    switch (action) {
    | LoadingFinished(station) =>
      let status =
        switch (station) {
        | Some(_station) => Success
        | None => Failed
        };
      ReasonReact.Update({station, status});
    | StartLoading =>
      ReasonReact.UpdateWithSideEffects(
        {...state, status: Loading},
        (
          self =>
            Js.Promise.(
              Hue.getInfo(discoveredStation)
              |> then_(station =>
                   self.send(LoadingFinished(Some(station))) |> resolve
                 )
              |> catch(_error => self.send(LoadingError) |> resolve)
              |> ignore
            )
        ),
      )
    | LoadingError => ReasonReact.Update({...state, status: Failed})
    },
  didMount: self => self.send(StartLoading),
  render: self =>
    switch (self.state.status) {
    | Success =>
      switch (self.state.station) {
      | Some(station) =>
        <div>
          <div> {ReasonReact.string("Name: " ++ station.name)} </div>
          <div> {ReasonReact.string("ModelId: " ++ station.modelid)} </div>
          <button onClick=(_event => Js.log("Connect"))>
            {ReasonReact.string("Connect")}
          </button>
        </div>
      /* None should never occur as we do Failed checking in the LoadingFinished action */
      /* We do it in there since ReasonML forces me to use Some/None for nullable types */
      /* It's only nullable because it's fetched from an api when the component is loaded. */
      | None => ReasonReact.string("This should never happen.")
      }
    | Loading => ReasonReact.string("Loading Station Info...")
    | Failed => ReasonReact.string("Could not load station info.")
    | _ => ReasonReact.string("TODO not implemented yet")
    },
};