#!/usr/bin/python3
import sys

if __name__ == "__main__":
  path = sys.argv[1]

  with open(path, 'r') as f:
    mank_js = f.read()

  # Fix a few issues to allow use with create react app
  mank_js = mank_js.replace("mankc_web.wasm", "/mankc_web.wasm")
  mank_js = mank_js.replace("wasmBinaryFile=locateFile(wasmBinaryFile)", "/*nop*/")
  mank_js = "/* eslint-disable */\n" + mank_js

  outpath = path.replace(".js", ".cra.js")
  with open(outpath, 'w') as f:
    f.write(mank_js)

  print("[Fixup] Created CRA compatible js")
