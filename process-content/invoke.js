const processContent = require("./index").handler

;(async () => {
  const result = await processContent({
    contentURL: "http://api.zoomhub.net/v1/content/h",
  })
  console.log("Lambda invoke response:", result)
})()
