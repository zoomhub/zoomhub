// IMPORTANT: Required to make `__SNOWPACK_ENV__` available.
// See: https://github.com/snowpackjs/snowpack/issues/3621#issuecomment-907731004
import.meta.hot

import { PresignedPOSTData } from "../types/PresignedPOSTData"
import { UploadError } from "./Error"

export const submitFile = async ({ email, file }) => {
  // TODO: Handle errors
  const presignedPostData: PresignedPOSTData = await fetch(
    `${__SNOWPACK_ENV__.SNOWPACK_PUBLIC_API_BASE_URI}/v1/content/upload?email=${email}`
  ).then((response) => response.json())

  const formData = new FormData()
  Object.entries(presignedPostData)
    .filter(([key, _]) => key !== "url")
    .forEach(([key, value]) => {
      formData.append(key, value)
    })
  formData.append("file", file)

  let uploadResponse
  try {
    uploadResponse = await fetch(presignedPostData.url, {
      method: "POST",
      body: formData,
    })
  } catch (innerError) {
    throw new UploadError({ message: "Upload request failed", innerError })
  }

  if (uploadResponse.status !== 200) {
    throw new UploadError({
      message: `Unexpected upload response status code`,
      response: uploadResponse,
    })
  }

  return uploadResponse
}
