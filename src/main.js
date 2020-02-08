import { Elm } from "./elm/Main.elm";
import * as ElmCanvas from "elm-canvas";

const app = Elm.Main.init({
  node: document.getElementById("elm-main"),
  flags: { w: 600.0, h: 600.0 }
});
