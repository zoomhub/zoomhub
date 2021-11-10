// IMPORTANT: Required to make `__SNOWPACK_ENV__` available.
// See: https://github.com/snowpackjs/snowpack/issues/3621#issuecomment-907731004
import.meta.hot

import * as React from "react"
import { useRef, useState } from "react"

import axios from "axios"
import {
  ChevronDoubleRightIcon,
  LinkIcon,
  MailIcon,
  UploadIcon,
  XIcon,
} from "@heroicons/react/solid"
import { submitFile } from "../lib/submitFile"

export const Create = ({ initialView }) => {
  const [view, setView] = useState(initialView)

  return (
    <div
      className="
        w-full
        sm:w-auto
        rounded-lg
        bg-gray-800/50
        py-8
        px-6
        mt-4
        shadow-xl
        backdrop-filter
        backdrop-blur-md
        pointer-events-auto
      "
    >
      {(() => {
        switch (view) {
          case "submissions-disabled":
            return <SubmissionsDisabled />
          case "submit":
            return <Submit onViewChange={setView} />
          case "verify-email":
            return <VerifyEmail onViewChange={setView} />
          case "error":
            return <Oops onViewChange={setView} />
          default:
            throw new Error(`Invalid view: ${view}`)
        }
      })()}
    </div>
  )
}

const SubmissionsDisabled = () => (
  <div className="text-center grid gap-6 lg:gap-10">
    <Tagline />
    <hr className="border-gray-600/40 hidden lg:block" />
    <SectionTitle>
      Make your own… <span className="text-orange-500">coming soon.</span>
    </SectionTitle>
  </div>
)

const Submit = ({ onViewChange }) => {
  const formRef = useRef(null)
  const [imageURL, setImageURL] = useState("")
  const [email, setEmail] = useState("")
  const [isSubmissionPending, setIsSubmissionPending] = useState(false)
  const [view, setView] = useState("source-type-selector")
  const [file, setFile] = useState(null)

  return (
    <div className="grid gap-6 lg:gap-10">
      <Tagline />
      <hr className="border-gray-600/40 hidden lg:block" />
      <form className="grid gap-7" ref={formRef}>
        <SectionTitle className="hidden lg:block">
          Try it with your own image
        </SectionTitle>
        <SectionTitle className="lg:hidden">
          Create your own zoomable image
        </SectionTitle>
        {(() => {
          switch (view) {
            case "source-type-selector":
              return (
                <div className="w-full">
                  <div className="flex flex-col md:flex-row w-full items-center">
                    <label className="w-full md:w-auto md:flex-grow btn btn-secondary">
                      <UploadIcon className="h-5 w-5 mr-1" />
                      I have an image file…
                      <input
                        type="file"
                        className="hidden"
                        onChange={(event) => {
                          setFile(event.target.files[0])
                          setView("source-type-file")
                        }}
                      />
                    </label>
                    <div className="w-full text-center my-2 md:my-0 md:inline-block md:w-auto text-gray-600 mx-2 text-sm font-semibold uppercase">
                      or
                    </div>
                    <button
                      type="button"
                      className="w-full md:w-auto md:flex-grow btn btn-secondary"
                      onClick={() => setView("source-type-url")}
                    >
                      <LinkIcon className="w-5 h-5 mr-1" />I have an image link…
                    </button>
                  </div>
                </div>
              )
            case "source-type-url":
              return (
                <div>
                  <label className="text-gray-50 inline-flex items-center text-sm">
                    <LinkIcon className="h-5 w-5 mr-1" />
                    Link to an image on the web
                  </label>
                  <div className="flex flex-column">
                    <input
                      type="url"
                      className="flex-grow text-input"
                      placeholder="https://www.example.com/image.jpg"
                      value={imageURL}
                      onChange={(event) => setImageURL(event.target.value)}
                      required
                    />
                    <CancelButton
                      onClick={() => {
                        setImageURL(null)
                        setView("source-type-selector")
                      }}
                    />
                  </div>
                </div>
              )
            case "source-type-file":
              return (
                <div className="flex align-items-center">
                  <input
                    disabled
                    className="text-input text-left flex-grow"
                    value={file.name || ""}
                  />
                  <CancelButton
                    onClick={() => {
                      setFile(null)
                      setView("source-type-selector")
                    }}
                  />
                </div>
              )
              break
            default:
              throw new Error(`Invalid view: ${view}`)
          }
        })()}
        <div className="grid gap-1">
          <label className="text-gray-50 inline-flex items-center text-sm">
            <MailIcon className="h-5 w-5 mr-1" />
            Email{" "}
            <span className="ml-1 text-gray-400">
              (where you want to be notified when your image is ready
              <sup>★</sup>)
            </span>
          </label>
          <input
            type="email"
            className="w-full text-input"
            placeholder="you@example.com"
            value={email}
            onChange={(event) => setEmail(event.target.value)}
            required
          />
        </div>
        <div className="text-center">
          <button
            type="submit"
            className="w-full btn btn-primary"
            disabled={isSubmissionPending}
            onClick={async (event) => {
              event.preventDefault()

              if (!formRef.current.reportValidity()) {
                return
              }

              let response
              try {
                setIsSubmissionPending(true)
                response = await (async () => {
                  switch (view) {
                    case "source-type-url":
                      return submitURL({ url: imageURL, email })
                    case "source-type-file":
                      return submitFile({ file, email })
                    default:
                      throw new Error(`Invalid view: ${view}`)
                  }
                })()
              } catch (error) {
                console.error("error", { error, response })
                onViewChange("error")
                return
              } finally {
                setIsSubmissionPending(false)
              }

              onViewChange("verify-email")
            }}
          >
            Create
            <ChevronDoubleRightIcon className="h-5 w-5 ml-1" />
          </button>

          <div className="text-gray-600 text-sm mt-4">
            <sup>★</sup> We won’t spam you or share your email — promise!
          </div>
        </div>
      </form>
    </div>
  )
}

const VerifyEmail = ({ onViewChange }) => (
  <div className="text-center">
    <h1 className="text-4xl text-white tracking-tighter font-semibold">
      🎉 Success
    </h1>
    <div className="text-gray-200 text-2xl tracking-tight font-medium mt-5">
      📩 Check your email
    </div>
    <div className="text-gray-200 text-xl tracking-tight font-medium">
      We’ve just sent you the link to view your upload.
    </div>

    <button
      type="button"
      className="btn btn-primary mt-6"
      onClick={() => onViewChange("submit")}
    >
      Create another one
      <ChevronDoubleRightIcon className="h-5 w-5 ml-1" />
    </button>
  </div>
)

const Oops = ({ onViewChange }) => (
  <div className="text-center">
    <h1 className="text-4xl text-white tracking-tighter font-semibold">
      😕 Oops…
    </h1>
    <div className="text-gray-200 text-2xl tracking-tight font-medium mt-5">
      Something went wrong.
    </div>

    <button
      type="button"
      className="btn btn-primary mt-6"
      onClick={() => onViewChange("submit")}
    >
      Try again
      <ChevronDoubleRightIcon className="h-5 w-5 ml-1" />
    </button>
  </div>
)

const SectionTitle: React.FC<{ className: String }> = ({
  children,
  className,
}) => (
  <h2
    className={`text-3xl lg:text-4xl text-gray-200 text-center font-semibold tracking-tighter ${className}`}
  >
    {children}
  </h2>
)

const Tagline = () => (
  <div className="lg:grid place-content-center place-items-center text-center hidden">
    <h1 className="text-4xl lg:text-5xl text-white font-semibold tracking-tighter">
      Stunning zoomable images
    </h1>
    <h2 className="text-lg lg:text-xl text-gray-400 font-semibold tracking-tighter">
      Play around with the background: tap, pinch, drag, scroll.
    </h2>
  </div>
)

const CancelButton = ({ onClick }) => (
  <button className="btn btn-secondary ml-1" onClick={onClick}>
    <XIcon className="h-5 w-5" />
  </button>
)

const submitURL = async ({ url, email }) =>
  await axios.get(
    `${__SNOWPACK_ENV__.SNOWPACK_PUBLIC_API_BASE_URI}/v1/content`,
    { params: { url, email } }
  )
