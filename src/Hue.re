exception NoBaseStation;

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

type linkedStation = {
  id: string, /* Id for later */
  ip: string, /* The internal ip address */
  username: string, /* Actually more like auth token */
  timestamp: string /* Timestamp for later */
};

/* type apiError = {
     errorType: int,
     address: option(string),
     description: string,
   }; */

type linkSuccess = {username: string};

type linkResponse = {
  /* error: option(apiError), */
  success: option(linkSuccess),
};

module Decode = {
  let decodeDiscoveredStation = json =>
    Json.Decode.{
      id: json |> field("id", string),
      internalipaddress: json |> field("internalipaddress", string),
    };
  let discoveredStations = json: list(discoveredStation) =>
    Json.Decode.list(decodeDiscoveredStation, json);

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

  /* let errorResponse = json =>
     Json.Decode.{
       errorType: json |> field("type", int),
       address: json |> field("address", optional(string)),
       description: json |> field("description", string),
     }; */

  let linkSuccess = json =>
    Json.Decode.{username: json |> field("username", string)};

  let linkResponse = json =>
    Json.Decode.{
      /* error: json |> field("error", optional(errorResponse)), */
      success: json |> field("success", optional(linkSuccess)),
    };

  let linkResponses = json: list(linkResponse) =>
    Json.Decode.list(linkResponse, json);
};

/* Discover base stations */
let discover = () =>
  Js.Promise.(
    Fetch.fetch("https://discovery.meethue.com")
    |> then_(Fetch.Response.json)
    |> then_(json => json |> Decode.discoveredStations |> resolve)
  );

/* Get info from a base station */
let getInfo = (station: discoveredStation) =>
  Js.Promise.(
    Fetch.fetch("http://" ++ station.internalipaddress ++ "/api/config")
    |> then_(Fetch.Response.json)
    |> then_(json => json |> Decode.baseStation |> resolve)
  );

/* Link with a base station */
let linkWithStation = (station: discoveredStation) => {
  let payload = Js.Dict.empty();
  Js.Dict.set(payload, "devicetype", Js.Json.string("SimpleHue#WebClient"));
  Js.Promise.(
    Fetch.fetchWithInit(
      "http://" ++ station.internalipaddress ++ "/api",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=
          Fetch.BodyInit.make(Js.Json.stringify(Js.Json.object_(payload))),
        ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
    |> then_(json => Decode.linkResponses(json) |> resolve)
  );
};

/* LocalStorage decoder */
module LSDecode = {
  let linkedStation = json: linkedStation =>
    Json.Decode.{
      id: json |> field("id", string),
      ip: json |> field("ip", string),
      username: json |> field("username", string),
      timestamp: json |> field("timestamp", string),
    };
};
/* LocalStorage encoder. */
module LSEncode = {
  let linkedStation = (station: linkedStation) =>
    Json.Encode.(
      object_([
        ("id", string(station.id)),
        ("ip", string(station.ip)),
        ("username", string(station.username)),
        ("timestamp", string(station.timestamp)),
      ])
    );
};
/* Currently some of the api is designed to support multiple linked stations in the future if needed, however they have not been implemented yet. */

/* Return the currently linked station (for when multiple stations are linked it should return the selected one.) */
let getLinkedStation = (): option(linkedStation) => {
  let stationString = Dom.Storage.(localStorage |> getItem("linkedStation"));
  switch (stationString) {
  | Some(station) =>
    Some(station |> Json.parseOrRaise |> LSDecode.linkedStation)
  | None => None
  };
};

/* Check whether a station is already linked with or not. */
let isLinked = id: bool =>
  switch (getLinkedStation()) {
  | Some(station) => station.id == id
  | None => false
  };

/* Store linked station in localstorage */
let setLinkedStation = (station: linkedStation) =>
  Dom.Storage.(
    localStorage
    |> setItem(
         "linkedStation",
         LSEncode.linkedStation(station) |> Json.stringify,
       )
  );

/* _id unused. */
let unlinkStation = _id =>
  Dom.Storage.(localStorage |> removeItem("linkedStation"));

/* LIGHTS */

type lightState = {
  on: bool,
  brightness: int,
  hue: option(int),
  sat: option(int),
};

type light = {
  id: string,
  state: lightState,
  name: string,
};

module LDecode = {
  let lightState = json: lightState =>
    Json.Decode.{
      on: json |> field("on", bool),
      brightness: json |> field("bri", int),
      hue: json |> optional(field("hue", int)),
      sat: json |> optional(field("sat", int)),
    };
  let light = (json, id): light =>
    Json.Decode.{
      id,
      state: json |> field("state", lightState),
      name: json |> field("name", string),
    };
  let lightsResponse = json: list(light) =>
    Js.Json.decodeObject(json)
    |> Belt.Option.getExn
    |> Js.Dict.entries
    |> Belt.Array.map(_, ((k, v)) => light(v, k))
    |> Belt.List.fromArray;
};

let getLights = (station: linkedStation) =>
  Js.Promise.(
    Fetch.fetch(
      "http://" ++ station.ip ++ "/api/" ++ station.username ++ "/lights",
    )
    |> then_(Fetch.Response.json)
    |> then_(json => LDecode.lightsResponse(json) |> resolve)
  );

let updateLight = (light: light) => {
  let station =
    switch (getLinkedStation()) {
    | Some(station) => station
    | None => raise_notrace(NoBaseStation)
    };

  Js.Promise.(
    Fetch.fetchWithInit(
      "http://"
      ++ station.ip
      ++ "/api/"
      ++ station.username
      ++ "/lights/"
      ++ light.id
      ++ "/state",
      Fetch.RequestInit.make(
        ~method_=Put,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Json.Encode.(
                object_([
                  ("on", bool(light.state.on)),
                  ("bri", int(light.state.brightness)),
                ])
              ),
            ),
          ),
        ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
  );
};