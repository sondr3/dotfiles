alias b := build
alias r := run
alias l := lock

build:
    deno compile -A --unstable --import-map import_map.json -o dots main.ts

run COMMAND *FLAGS:
    deno run -A --unstable --import-map import_map.json main.ts {{COMMAND}} {{FLAGS}}

lock:
    deno cache --import-map=import_map.json --lock lock.json --lock-write --unstable main.ts