open Belt;
type stationStatus =
  | Linked
  | Linking
  | Success
  | Loading
  | Failed;

type linkStatus =
  | Linking
  | Linked
  | Unlinked
  | Failed;

type state = {
  station: option(Hue.baseStation),
  status: stationStatus,
  linkStatus,
};

type action =
  | LoadingFinished(option(Hue.baseStation))
  | StartLoading
  | LoadingError
  | StartLinking
  | LinkResponses(list(Hue.linkResponse))
  | LinkingFailed
  | LinkingSuccess(string)
  | Unlink;

let component = ReasonReact.reducerComponent("BaseStation");

let make = (~discoveredStation: Hue.discoveredStation, _children) => {
  ...component,
  initialState: () => {station: None, status: Loading, linkStatus: Unlinked},
  reducer: (action, state) =>
    switch (action) {
    | LoadingFinished(station) =>
      let status =
        switch (station) {
        | Some(_station) => Success
        | None => Failed
        };
      let linkStatus =
        if (Hue.isLinked(discoveredStation.id)) {
          Linked;
        } else {
          state.linkStatus;
        };

      ReasonReact.Update({station, status, linkStatus});
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
    | StartLinking =>
      ReasonReact.UpdateWithSideEffects(
        {...state, linkStatus: Linking},
        (
          self =>
            Js.Promise.(
              Hue.linkWithStation(discoveredStation)
              |> then_(responses =>
                   self.send(LinkResponses(responses)) |> resolve
                 )
              |> catch(_error => self.send(LinkingFailed) |> resolve)
              |> ignore
            )
        ),
      )
    | LinkResponses(responses) =>
      /* Refactor this, there has to be a better way. */
      let firstResponse = responses |> List.getExn(_, 0);

      switch (firstResponse.success) {
      | Some(succ) =>
        ReasonReact.SideEffects(
          (self => self.send(LinkingSuccess(succ.username))),
        )
      | None => ReasonReact.SideEffects((self => self.send(LinkingFailed)))
      };
    | LinkingFailed => ReasonReact.Update({...state, linkStatus: Failed})
    | LinkingSuccess(username) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, linkStatus: Linked},
        (
          _self => {
            Hue.setLinkedStation({
              id: discoveredStation.id,
              ip: discoveredStation.internalipaddress,
              username,
              timestamp: string_of_float(Js.Date.now()),
            });
            /* And start the app a new since we finished setup. */
            ReasonReact.Router.push("/");
            Js.log(username);
          }
        ),
      )
    | Unlink =>
      ReasonReact.UpdateWithSideEffects(
        {...state, linkStatus: Unlinked},
        (_s => Hue.unlinkStation(discoveredStation.id)),
      )
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
          <div>
            {
              ReasonReact.string("Ip: " ++ discoveredStation.internalipaddress)
            }
          </div>
          {
            switch (self.state.linkStatus) {
            | Unlinked =>
              <button
                className="btn btn-primary"
                onClick=(_event => self.send(StartLinking))>
                {ReasonReact.string("Link to Station")}
              </button>
            | Linking =>
              <button className="btn btn-warning" disabled=true>
                {ReasonReact.string("Linking...")}
              </button>
            | Failed =>
              <button
                className="btn btn-danger"
                onClick=(_event => self.send(StartLinking))>
                {ReasonReact.string("Failed, Retry?")}
              </button>
            | Linked =>
              <span>
                <button className="btn btn-success" disabled=true>
                  {ReasonReact.string("Linked")}
                </button>
                <button
                  className="btn btn-danger"
                  onClick=(_event => self.send(Unlink))>
                  {ReasonReact.string("Unlink station.")}
                </button>
              </span>
            }
          }
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