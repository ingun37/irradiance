import re
def changeModuleName(str):
    return re.sub(r'^module Wasm', r'module Main', str)

def importAsterius(str):
    return re.sub(r'^import(.+)$', r'import Asterius.Types\nimport Asterius.ByteString\nimport\1', str, 1, re.MULTILINE)
def uncomment(str):
    return re.sub(r'^-- ', '', str, 0, re.MULTILINE)
with open("../src/Wasm.hs", "r") as src_file:
    src = src_file.read()
    modified = uncomment(src)
    with open("Wasm.hs", "w") as dst_file:
        dst_file.write(modified)