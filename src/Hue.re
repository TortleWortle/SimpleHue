type discoveredStation = {
  id: string,
  internalipaddress: string,
};

type baseStation = {
  name: string,
  datastoreversion: string,
  swversion: string,
  apiversion: string,
  mac: string,
  bridgeid: string,
  factorynew: bool,
  replacesbridgeid: option(string),
  modelid: string,
  starterkitid: string,
};

module Decode = {
  let discoveredStation = json =>
    Json.Decode.{
      id: json |> field("id", string),
      internalipaddress: json |> field("internalipaddress", string),
    };
  let discoveredStations = json: list(discoveredStation) =>
    Json.Decode.list(discoveredStation, json);

  let baseStation = json: baseStation =>
    Json.Decode.{
      name: json |> field("name", string),
      datastoreversion: json |> field("datastoreversion", string),
      swversion: json |> field("swversion", string),
      apiversion: json |> field("apiversion", string),
      mac: json |> field("mac", string),
      bridgeid: json |> field("swversion", string),
      factorynew: json |> field("factorynew", bool),
      replacesbridgeid: json |> field("replacesbridgeid", optional(string)),
      modelid: json |> field("modelid", string),
      starterkitid: json |> field("starterkitid", string),
    };
};

/* Discover base stations */
let discover = () =>
  Js.Promise.(
    Fetch.fetch("https://discovery.meethue.com")
    |> then_(Fetch.Response.json)
    |> then_(json => json |> Decode.discoveredStations |> resolve)
  );

let getInfo = (station: discoveredStation) =>
  Js.Promise.(
    Fetch.fetch("http://" ++ station.internalipaddress ++ "/api/config")
    |> then_(Fetch.Response.json)
    |> then_(json => json |> Decode.baseStation |> resolve)
  );