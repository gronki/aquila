function aqstack {
    podman run -it -v "$(pwd)":"$(pwd)" --workdir "$(pwd)" --entrypoint aqstack aquila
}
export -f aqstack

function aqlrgb {
    podman run -it -v "$(pwd)":"$(pwd)" --workdir "$(pwd)" --entrypoint aqlrgb aquila
}
export -f aqlrgb

function aqconvol {
    podman run -it -v "$(pwd)":"$(pwd)" --workdir "$(pwd)" --entrypoint aqconvol aquila
}
export -f aqconvol
