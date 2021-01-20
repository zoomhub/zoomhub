const processContent = require("./index").handler

;(async () => {
  const result = await processContent({
    contentURL: `${process.env.BASE_URI}/v1/content/Xar`,
  })
  console.log("Lambda invoke response:", result)
})()
