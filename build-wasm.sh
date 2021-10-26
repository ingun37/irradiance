#!/usr/bin/env zsh
python3 gen.py
docker run -it --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius ./build-wasm.sh