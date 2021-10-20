// IMPORTANT: Required to make `__SNOWPACK_ENV__` available.
// See: https://github.com/snowpackjs/snowpack/issues/3621#issuecomment-907731004
import.meta.hot

import * as React from "react"
import * as ReactDOM from "react-dom"

import { Create } from "./components/Create"
;(async () => {
  let apiConfig
  try {
    apiConfig = await fetch(
      `${__SNOWPACK_ENV__.SNOWPACK_PUBLIC_API_BASE_URI}/v1/config`
    ).then((_) => _.json())
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
