alias b := build
alias r := run
alias l := lock

flags := "--lock lock.json --unstable --import-map import_map.json"

build:
    deno compile {{flags}} -A -o dots main.ts

run COMMAND *FLAGS:
    deno run {{flags}} -A main.ts {{COMMAND}} {{FLAGS}}

lock:
    deno cache {{flags}} --lock-write main.ts

fmt:
    deno fmt **/*.ts

lint:
    deno lint

test:
    deno test -A --unstable --import-map import_map.json