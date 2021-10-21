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
} from "@heroicons/react/solid"

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
            return <Error onViewChange={setView} />
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
        <div className="w-full">
          <label className="text-gray-50 inline-flex items-center text-sm">
            <LinkIcon className="h-5 w-5 mr-1" />
            Link to an image on the web
          </label>
          <input
            type="url"
            className="w-full text-input"
            placeholder="https://www.example.com/image.jpg"
            value={imageURL}
            onChange={(event) => setImageURL(event.target.value)}
            required
          />
        </div>
        <div className="grid gap-1">
          <label className="text-gray-50 inline-flex items-center text-sm">
            <MailIcon className="h-5 w-5 mr-1" />
            Email{" "}
            <span className="ml-1 text-gray-400">
              (we’ll notify you when your image is ready<sup>★</sup>)
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
                response = await submitURL({ url: imageURL, email })
              } catch (error) {
                console.error("response:", response)
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
      className="btn-primary mt-6"
      onClick={() => onViewChange("submit")}
    >
      Create another one
      <ChevronDoubleRightIcon className="h-5 w-5 ml-1" />
    </button>
  </div>
)

const Error = ({ onViewChange }) => (
  <div className="text-center">
    <h1 className="text-4xl text-white tracking-tighter font-semibold">
      😕 Oops…
    </h1>
    <div className="text-gray-200 text-2xl tracking-tight font-medium mt-5">
      Something went wrong.
    </div>

    <button
      type="button"
      className="btn-primary mt-6"
      onClick={() => onViewChange("submit")}
    >
      Try again
      <ChevronDoubleRightIcon className="h-5 w-5 ml-1" />
    </button>
  </div>
)

const SectionTitle = ({ children, className }) => (
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

const submitURL = async ({ url, email }) =>
  await axios.get(
    `${__SNOWPACK_ENV__.SNOWPACK_PUBLIC_API_BASE_URI}/v1/content`,
    { params: { url, email } }
  )
