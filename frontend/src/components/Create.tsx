import * as React from "react"

import {
  ChevronDoubleRightIcon,
  LinkIcon,
  MailIcon,
  UploadIcon,
} from "@heroicons/react/solid"

export const Create = () => (
  <div className="rounded-lg bg-gray-800 p-8 mt-4 shadow-md">
    <form className="grid gap-7">
      <h2 className="text-gray-200 text-center text-3xl font-semibold">
        Create your own zoomable image
      </h2>
      <div className="inline-grid w-full grid-cols-2 gap-2">
        <button
          type="button"
          className="
            font-semibold
            inline-flex
            items-center
            justify-center
            px-4
            py-3
            uppercase
            bg-gray-400
            hover:bg-gray-600
            text-sm
            rounded-lg
            shadow-lg
            cursor-pointer
            text-gray-800
            hover:text-white

            outline-none
            focus:ring-4
            focus:ring-gray-300
            focus:ring-opacity-90
          "
        >
          <LinkIcon className="w-5 h-5 mr-1" />
          Post image link
        </button>

        <label
          className="
            font-semibold
            inline-flex
            items-center
            justify-center
            px-4
            py-3
            uppercase
            bg-gray-400
            hover:bg-gray-600
            text-sm
            rounded-lg
            shadow-lg
            cursor-pointer
            text-gray-800
            hover:text-white

            outline-none
            focus:ring-4
            focus:ring-gray-300
            focus:ring-opacity-90
          "
        >
          <UploadIcon className="h-5 w-5 mr-1" />
          Upload image
          <input type="file" className="hidden" />
        </label>
      </div>

      <div className="grid gap-1">
        <label className="text-gray-50 inline-flex items-center text-sm">
          <MailIcon className="h-5 w-5 mr-1" />
          Give us your email and we’ll let you know when your image is ready{" "}
          <sup>★</sup>
        </label>
        <input
          type="text"
          className="
            w-full
            px-4
            py-3
            rounded
            placeholder-gray-400
            text-center
            outline-none
            focus:ring-4
            focus:ring-orange-500
            focus:ring-opacity-90
            text-lg
            font-medium
          "
          placeholder="you@example.com"
        ></input>
      </div>

      <div>
        <button
          type="button"
          className="
            w-full
            font-semibold
            inline-flex
            items-center
            justify-center
            px-4
            py-3
            uppercase
            bg-orange-500
            hover:bg-orange-600
            rounded-lg
            shadow-lg
            cursor-pointer
            text-gray-800
            hover:text-white
            text-lg

            outline-none
            focus:ring-4
            focus:ring-orange-500
            focus:ring-opacity-90
          "
        >
          Create
          <ChevronDoubleRightIcon className="h-5 w-5 ml-1"></ChevronDoubleRightIcon>
        </button>

        <div className="text-gray-500 text-sm mt-4">
          <sup>★</sup> We won’t spam you or share your email — promise!
        </div>
      </div>
    </form>
  </div>
)
