type state = {
  light: Hue.light,
  oldLight: Hue.light,
};
type action =
  | Toggle
  | SetBrightness(int)
  | UpdateLightState(Hue.light);

/* Alert because I have no time to implement proper error displaying. */
[@bs.val] external alert: string => unit = "alert";

let component = ReasonReact.reducerComponent("SimpleLight");

let updateLight = (oldLight, newLight, self) =>
  Js.Promise.(
    Hue.updateLight(newLight)
    |> then_(_r =>
         ReasonReact.(self.send(UpdateLightState(newLight))) |> resolve
       )
    |> catch(_err =>
         {
           alert("Could not update light.");
           ReasonReact.(self.send(UpdateLightState(oldLight)));
         }
         |> resolve
       )
    |> ignore
  );

let updateBrightness =
  Debouncer.make(~wait=500, self =>
    ReasonReact.(updateLight(self.state.oldLight, self.state.light, self))
  );

let make = (~light: Hue.light, _children) => {
  ...component,
  initialState: () => {light, oldLight: light},
  reducer: (action, state) =>
    switch (action) {
    | Toggle =>
      let newLight = {
        ...state.light,
        state: {
          ...state.light.state,
          on: !state.light.state.on,
        },
      };
      ReasonReact.SideEffects(updateLight(state.light, newLight));
    | SetBrightness(brightness) =>
      let oldLight = state.light;

      let newLight = {
        ...state.light,
        state: {
          ...state.light.state,
          brightness,
          on: true,
        },
      };

      ReasonReact.UpdateWithSideEffects(
        {light: newLight, oldLight},
        updateBrightness,
      );
    | UpdateLightState(light) => ReasonReact.Update({...state, light})
    },
  render: self => {
    let light = self.state.light;
    <tr className={light.state.on ? "bg-success" : "bg-danger"}>
      <td> {ReasonReact.string(light.id)} </td>
      <td> {ReasonReact.string(light.name)} </td>
      <td>
        <input
          type_="range"
          min=0
          max="254"
          onChange={
            event =>
              self.send(
                SetBrightness(
                  int_of_string(ReactEvent.Form.target(event)##value),
                ),
              )
          }
          value={string_of_int(light.state.brightness)}
        />
        <label>
          {ReasonReact.string(string_of_int(light.state.brightness))}
        </label>
      </td>
      <td>
        <button onClick={_event => self.send(Toggle)}>
          {ReasonReact.string("Toggle")}
        </button>
      </td>
    </tr>;
  },
};