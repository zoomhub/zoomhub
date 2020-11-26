const processContent = require("./index").handler

;(async () => {
  const result = await processContent({
    url: "http://api.zoomhub.net/v1/content/h",
  })
  console.log("result:", result)
})()
