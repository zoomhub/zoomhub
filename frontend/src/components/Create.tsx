import * as React from "react"

import {
  ChevronDoubleRightIcon,
  LinkIcon,
  MailIcon,
  UploadIcon,
} from "@heroicons/react/solid"

export const Create = () => (
  <div className="rounded-lg bg-gray-800 bg-opacity-50 p-8 mt-4 shadow-xl backdrop-filter backdrop-blur-md">
    <form className="grid gap-7">
      <h2 className="text-gray-200 text-center text-3xl font-semibold tracking-tighter">
        Make your images zoomable
      </h2>
      <div className="flex w-full items-center">
        <button type="button" className="flex-grow btn-secondary">
          <LinkIcon className="w-5 h-5 mr-1" />
          Post image link
        </button>
        <span className="text-gray-600 mx-2 text-sm font-semibold uppercase">
          or
        </span>
        <label className="flex-grow btn-secondary">
          <UploadIcon className="h-5 w-5 mr-1" />
          Upload image
          <input type="file" className="hidden" />
        </label>
      </div>

      <div className="grid gap-1">
        <label className="text-gray-50 inline-flex items-center text-sm">
          <MailIcon className="h-5 w-5 mr-1" />
          Email where we can notify you when your image is ready<sup>★</sup>
        </label>
        <input
          type="text"
          className="w-full text-input"
          placeholder="you@example.com"
        ></input>
      </div>

      <div className="text-center">
        <button type="button" className=" w-full btn-primary">
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
