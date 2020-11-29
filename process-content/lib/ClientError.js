module.exports = class ClientError extends Error {
  constructor(message, { status } = {}) {
    super(message)
    this.name = this.constructor.name
    Error.captureStackTrace(this, this.constructor)

    // custom properties
    this.status = status || 400
  }
}
