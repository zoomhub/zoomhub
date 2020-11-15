const AWS = require("aws-sdk")
const fs = require("fs").promises
const sharp = require("sharp")

const s3 = new AWS.S3({ apiVersion: "2006-03-01" })

exports.handler = async (event, context) => {
  if (!event.bucket) {
    return "Please specify a URL to process"
  }
  if (!event.key) {
    return "Please specify a URL to process"
  }

  const result = await s3
    .getObject({ Bucket: event.bucket, Key: event.key })
    .promise()

  await fs.writeFile("/tmp/download.jpg", result.Body)
  await sharp("/tmp/download.jpg")
    .jpeg()
    .tile({
      depth: "onepixel",
      layout: "dz",
      overlap: 1,
      size: 254,
    })
    .toFile("/tmp/output.dz")
  const dziXML = await fs.readFile("/tmp/output.dzi", "utf8")

  // output.dzi is the Deep Zoom XML definition
  // output_files contains 512x512 tiles grouped by zoom level
  console.log(dziXML)
}
