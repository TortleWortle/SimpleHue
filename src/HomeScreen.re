let component = ReasonReact.statelessComponent("HomeScreen");
let toString = ReasonReact.string;

let make = _children => {
  ...component,
  render: _self =>
    <div>
      <a
        onClick={
          event => {
            ReactEvent.Mouse.preventDefault(event);
            ReasonReact.Router.push("/setup");
          }
        }
        href="/setup">
        "Setup!"->toString
      </a>
    </div>,
};