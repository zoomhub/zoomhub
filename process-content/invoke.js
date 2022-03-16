const processContent = require("./index").handler

;(async () => {
  const result = await processContent(
    {
      contentURL: `${process.env.BASE_URI}/v1/content/${
        process.env.CONTENT_ID || "Xar"
      }`,
    },
    {}
  )
  console.log("Lambda invoke response:", result)
})()
