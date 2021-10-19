import * as React from "react"
import * as ReactDOM from "react-dom"

import { Create } from "./components/Create"
;(async () => {
  let apiConfig
  try {
    apiConfig = await fetch("/v1/config").then((_) => _.json())
  } catch (error) {
    apiConfig = { uploadsEnabled: false }
  }

  const container = document.querySelector("#create")
  ReactDOM.render(
    <Create
      initialView={apiConfig.uploadsEnabled ? "submit" : "submissions-disabled"}
    />,
    container
  )
})()
