/// <reference types="vite/client" />

import * as React from "react"
import * as ReactDOM from "react-dom"

import { Create } from "./components/Create"

;(async () => {

  let viewer
  ;(() => {
    let heroMode = "animated" // "animated" | "interactive"
    const heroModeSelector = document.querySelector(
      "#hero-mode-toggle"
    ) as HTMLButtonElement
    const createContainer = document.querySelector("#create-container")
    const image = {
      id: "0w5YD",
      width: 8192,
      height: 8192,
      tileSize: 254,
      tileOverlap: 1,
      tileFormat: "jpg"
    }

    viewer = OpenSeadragon({
      id: "hero-image",
      toolbar: "hero-toolbar",
      // navigation control images
      prefixUrl:
        "https://static.zoomhub.net/scripts/openseadragon/3.0.0-zoomhub/images/",
      // image source
      tileSources: {
        Image: {
          xmlns: "http://schemas.microsoft.com/deepzoom/2008",
          Size: { Width: image.width, Height: image.height },
          Format: image.tileFormat,
          Overlap: image.tileOverlap,
          Url: `https://cache.zoomhub.net/content/${image.id}_files/`,
          TileSize: image.tileSize,
        },
      },
    })
    viewer.setControlsEnabled(false)

    let animationRequestId
    const enableInteractivity = () => {
      viewer.setControlsEnabled(true)
      viewer.setMouseNavEnabled(true)
      document.querySelector("#hero-toolbar").classList.remove("hidden")

      createContainer.classList.add("hidden")

      cancelAnimationFrame(animationRequestId)
    }
    const disableInteractivity = () => {
      viewer.setControlsEnabled(false)
      viewer.setMouseNavEnabled(false)

      createContainer.classList.remove("hidden")

      document.querySelector("#hero-toolbar").classList.add("hidden")
    }

    heroModeSelector.addEventListener("click", () => {
      switch (heroMode) {
        case "animated":
          heroModeSelector.innerText = "Exit demo"
          enableInteractivity()
          heroMode = "interactive"
          return
        case "interactive":
          heroModeSelector.innerText = "Try demo"
          disableInteractivity()
          heroMode = "animated"
          return
        default:
          throw new Error(`Invalid hero mode: ${heroMode}`)
      }
    })

    viewer.addHandler("open", () => {
      viewer.setControlsEnabled(false)

      const targetWidth = 0.1
      const endBounds = viewer.viewport.getHomeBounds()
      const startBounds = new OpenSeadragon.Rect(
        0.1 + (1 - targetWidth) / 2, // x
        0.075, // y
        targetWidth, // width
        0.1 // height
      )
      viewer.viewport.fitBounds(startBounds, true)

      const animationDuration = 5 * 60 * 1000 // ms
      const animationStep = (timestamp) => {
        const delta = (timestamp - animationStartTime) / animationDuration
        const currentZoom = viewer.viewport.getZoom()
        if (delta >= 1.0) {
          enableInteractivity()
          return
        }
        const currentBounds = new OpenSeadragon.Rect(
          startBounds.x + (endBounds.x - startBounds.x) * delta,
          startBounds.y + (endBounds.y - startBounds.y) * delta,
          startBounds.width + (endBounds.width - startBounds.width) * delta,
          startBounds.height + (endBounds.height - startBounds.height) * delta
        )

        viewer.viewport.fitBounds(currentBounds)
        animationRequestId = requestAnimationFrame(animationStep)
      }

      let animationStartTime = performance.now()
      animationRequestId = requestAnimationFrame(animationStep)
    })
  })()

  // Create
  let apiConfig
  try {
    apiConfig = await fetch(
      `${import.meta.env.VITE_API_BASE_URI}/internal/config`
    ).then((_) => _.json())
  } catch (error) {
    console.error("Error fetching API configuration:", error)
    apiConfig = { uploadsEnabled: false }
  }

  const container = document.querySelector("#create-container")
  ReactDOM.render(
    <Create
      initialView={apiConfig.uploadsEnabled ? "submit" : "submissions-disabled"}
    />,
    container
  )
})()
