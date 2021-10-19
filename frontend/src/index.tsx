import * as React from "react"
import * as ReactDOM from "react-dom"

import { Create } from "./components/Create"
;(async () => {
  const apiConfig = await fetch("/v1/config").then((_) => _.json())

  const container = document.querySelector("#create")
  ReactDOM.render(
    <Create
      initialView={apiConfig.uploadsEnabled ? "submit" : "submissions-disabled"}
    />,
    container
  )
})()
