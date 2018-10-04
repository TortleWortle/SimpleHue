open Belt;
/* Setup types */
type discoveryStatus =
  | Success
  | Loading
  | Failed;

/* Setup state */
type state = {
  discoveredStations: list(Hue.discoveredStation),
  stations: list(Hue.baseStation),
  status: discoveryStatus,
};

/* Setup actions */
type action =
  | BaseStationsDiscovered(list(Hue.discoveredStation))
  | DiscoveryError
  | StartDiscovering;

let component = ReasonReact.reducerComponent("SetupScreen");

/* Gets called by the retry button */
let discoverStationsEv = (_event, self) =>
  ReasonReact.(self.send(StartDiscovering));

let make = _children => {
  ...component,
  /* Setup reducers */
  reducer: (action, state) =>
    switch (action) {
    | BaseStationsDiscovered(stations) =>
      ReasonReact.Update({
        ...state,
        discoveredStations: stations,
        status: Success,
      })
    | DiscoveryError => ReasonReact.Update({...state, status: Failed})
    | StartDiscovering =>
      ReasonReact.UpdateWithSideEffects(
        {...state, status: Loading},
        (
          self =>
            Js.Promise.(
              Hue.discover()
              |> then_(stations =>
                   self.send(BaseStationsDiscovered(stations)) |> resolve
                 )
              |> catch(_error => self.send(DiscoveryError) |> resolve)
              |> ignore
            )
        ),
      )
    },
  /* Setup initial state */
  initialState: () => {discoveredStations: [], stations: [], status: Loading},
  didMount: self =>
    /* Discover base stations from discovery.meethue.com */
    self.send(StartDiscovering),
  render: self =>
    <div className="container">
      <a onClick={_e => ReasonReact.Router.push("/")} href="/">
        {ReasonReact.string("Back to Home.")}
      </a>
      <h1> {ReasonReact.string("Discovered BaseStations")} </h1>
      <div>
        {
          switch (self.state.status) {
          | Success =>
            self.state.discoveredStations
            |> List.map(_, station =>
                 <BaseStation discoveredStation=station key={station.id} />
               )
            |> List.toArray
            |> ReasonReact.array
          | Loading => ReasonReact.string("Discovering stations...")
          | Failed =>
            <div>
              {ReasonReact.string("Failed to discover base stations.")}
              <button onClick={self.handle(discoverStationsEv)}>
                {ReasonReact.string("Try again")}
              </button>
            </div>
          }
        }
      </div>
    </div>,
};