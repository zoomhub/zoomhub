/// <reference types="vite/client" />

import axios from "axios"
import { PresignedPOSTData } from "../types/PresignedPOSTData"
import { UploadError } from "./Error"

export const submitFile: ({
  email,
  file,
  onProgress,
}: {
  email: string
  file: File
  onProgress?: (progressEvent: ProgressEvent) => void
}) => Promise<Response> = async ({ email, file, onProgress }) => {
  let presignedPOSTData: PresignedPOSTData
  try {
    presignedPOSTData = await fetch(
      `${import.meta.env.VITE_API_BASE_URI}/v1/content/upload?email=${email}`
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
    uploadResponse = await axios.request({
      method: "POST",
      url: presignedPOSTData.url,
      data: formData,
      headers: {
        "Content-Type": "multipart/form-data",
      },
      onUploadProgress: onProgress,
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
