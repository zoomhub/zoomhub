// IMPORTANT: Required to make `__SNOWPACK_ENV__` available.
// See: https://github.com/snowpackjs/snowpack/issues/3621#issuecomment-907731004
import.meta.hot

import { PresignedPOSTData } from "../types/PresignedPOSTData"
import { UploadError } from "./Error"

export const submitFile: ({
  email,
  file,
  onProgress,
}: {
  email: string
  file: File
  onProgress?: (progress: number) => void
}) => Promise<Response> = async ({ email, file }) => {
  let presignedPOSTData: PresignedPOSTData
  try {
    presignedPOSTData = await fetch(
      `${__SNOWPACK_ENV__.SNOWPACK_PUBLIC_API_BASE_URI}/v1/content/upload?email=${email}`
    ).then((response) => response.json())
  } catch (innerError) {
    throw new UploadError({
      message: "Failed to fetch presigned POST data",
      innerError,
    })
  }

  const formData = new FormData()
  Object.entries(presignedPOSTData)
    .filter(([key, _]) => key !== "url")
    .forEach(([key, value]) => {
      formData.append(key, value)
    })
  formData.append("file", file)

  let uploadResponse
  try {
    uploadResponse = await fetch(presignedPOSTData.url, {
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
