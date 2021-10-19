import * as React from "react"
import { useState } from "react"

import {
  ChevronDoubleRightIcon,
  LinkIcon,
  MailIcon,
} from "@heroicons/react/solid"

export const Create = () => {
  const [imageURL, setImageURL] = useState("")

  return (
    <div className="grid gap-10 rounded-lg bg-gray-800 bg-opacity-50 py-8 px-6 mt-4 shadow-xl backdrop-filter backdrop-blur-md">
      <div className="grid place-content-center place-items-center">
        <h1 className="text-5xl text-white font-semibold tracking-tighter">
          Instant zoomable images
        </h1>
        <h2 className="text-lg text-gray-400 font-semibold tracking-tighter">
          Play around with the background: tap, pinch, drag, scroll.
        </h2>
      </div>
      <hr className="border-gray-600/40" />
      <form className="grid gap-7">
        <h2 className="text-gray-200 text-center text-3xl font-semibold tracking-tighter">
          Try it with your own image
        </h2>
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
            Email where we can notify you when your image is ready<sup>★</sup>
          </label>
          <input
            type="email"
            className="w-full text-input"
            placeholder="you@example.com"
            required
          />
        </div>
        <div className="text-center">
          <button type="submit" className=" w-full btn-primary">
            Create
            <ChevronDoubleRightIcon className="h-5 w-5 ml-1"></ChevronDoubleRightIcon>
          </button>

          <div className="text-gray-600 text-sm mt-4">
            <sup>★</sup> We won’t spam you or share your email — promise!
          </div>
        </div>
      </form>
    </div>
  )
}
