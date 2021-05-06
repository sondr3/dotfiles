alias b := build
alias r := run

build:
    deno compile -A --unstable --import-map import_map.json -o dots main.ts

run COMMAND *FLAGS:
    deno run -A --unstable --import-map import_map.json main.ts {{COMMAND}} {{FLAGS}}
