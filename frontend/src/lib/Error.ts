export class UploadError extends Error {
  innerError: Error
  response: Response

  constructor({
    message,
    innerError,
    response,
  }: {
    message: string
    innerError?: Error
    response?: Response
  }) {
    super(message)

    this.innerError = innerError
    this.response = response

    // Workaround for custom error types: Set the prototype explicitly.
    // https://github.com/Microsoft/TypeScript-wiki/blob/main/Breaking-Changes.md#extending-built-ins-like-error-array-and-map-may-no-longer-work
    Object.setPrototypeOf(this, UploadError.prototype)
  }
}
