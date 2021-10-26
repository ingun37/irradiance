#!/usr/bin/env bash

mkdir wasm-out
ahc-link --input-hs Wasm.hs --browser --export-function=rotatedHemisphere --output-directory wasm-out --ghc-option -isrc