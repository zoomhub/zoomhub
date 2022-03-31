const processContent = require("./index").handler

if (!process.env.BASE_URI) {
  throw new Error("Please set 'BASE_URI' environment variable")
}

if (!process.env.CONTENT_ID) {
  throw new Error("Please set 'CONTENT_ID' environment variable")
}

;(async () => {
  const result = await processContent(
    {
      contentURL: `${process.env.BASE_URI}/v1/content/${process.env.CONTENT_ID}`,
    },
    {}
  )
  console.log("Lambda invoke response:", result)
})()
